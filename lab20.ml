(* images are lists of lists of floats between 0. (white) and 1. (black) *)
type image = float list list ;;
type size = int * int ;;

let image_map (f : float -> float) (img : image) : image =
  List.map (List.map f) img ;;

let image_iteri (f : int -> float -> (int -> unit)) (img : image) : unit =
  List.iteri (fun r -> List.iteri (fun c p -> (f c p) r)) img;;

(* threshold thershold image -- image where pixels above the threshold
   value are black *)
let threshold img thresh =
  image_map (fun v -> if v <= thresh then 0. else 1.) img

(* dither max image -- dithered image *)
let dither img =
  image_map (fun v -> if v > Random.float 1. then 1. else 0.) img

(* show the image *)
let depict img =
  let open Graphics in
  let (x, y) : size = List.length (List.hd img), List.length img in

  open_graph "";
  clear_graph ();
  resize_window x y;

  let depict_pix col pix row =
    let clr = int_of_float (255. *. (1. -. pix)) in
      set_color (rgb clr clr clr);
      plot col (y - row) in
  image_iteri depict_pix img;

  Unix.sleep 2; close_graph () ;;

(* run *)
let _ =
  let mona = Monalisa.image in
  depict mona;
  depict (threshold mona 0.75);
  depict (dither mona) ;;

