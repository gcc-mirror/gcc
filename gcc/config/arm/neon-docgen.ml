(* ARM NEON documentation generator.

   Copyright (C) 2006, 2007 Free Software Foundation, Inc.
   Contributed by CodeSourcery.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3, or (at your option) any later
   version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.

   This is an O'Caml program.  The O'Caml compiler is available from:

     http://caml.inria.fr/

   Or from your favourite OS's friendly packaging system. Tested with version
   3.09.2, though other versions will probably work too.

   Compile with:
     ocamlc -c neon.ml
     ocamlc -o neon-docgen neon.cmo neon-docgen.ml

   Run with:
     /path/to/neon-docgen /path/to/gcc/doc/arm-neon-intrinsics.texi
*)

open Neon

(* The combined "ops" and "reinterp" table.  *)
let ops_reinterp = reinterp @ ops

(* Helper functions for extracting things from the "ops" table.  *)
let single_opcode desired_opcode () =
  List.fold_left (fun got_so_far ->
                  fun row ->
                    match row with
                      (opcode, _, _, _, _, _) ->
                        if opcode = desired_opcode then row :: got_so_far
                                                   else got_so_far
                 ) [] ops_reinterp

let multiple_opcodes desired_opcodes () =
  List.fold_left (fun got_so_far ->
                  fun desired_opcode ->
                    (single_opcode desired_opcode ()) @ got_so_far)
                 [] desired_opcodes

let ldx_opcode number () =
  List.fold_left (fun got_so_far ->
                  fun row ->
                    match row with
                      (opcode, _, _, _, _, _) ->
                        match opcode with
                          Vldx n | Vldx_lane n | Vldx_dup n when n = number ->
                            row :: got_so_far
                          | _ -> got_so_far
                 ) [] ops_reinterp

let stx_opcode number () =
  List.fold_left (fun got_so_far ->
                  fun row ->
                    match row with
                      (opcode, _, _, _, _, _) ->
                        match opcode with
                          Vstx n | Vstx_lane n when n = number ->
                            row :: got_so_far
                          | _ -> got_so_far
                 ) [] ops_reinterp

let tbl_opcode () =
  List.fold_left (fun got_so_far ->
                  fun row ->
                    match row with
                      (opcode, _, _, _, _, _) ->
                        match opcode with
                          Vtbl _ -> row :: got_so_far
                          | _ -> got_so_far
                 ) [] ops_reinterp

let tbx_opcode () =
  List.fold_left (fun got_so_far ->
                  fun row ->
                    match row with
                      (opcode, _, _, _, _, _) ->
                        match opcode with
                          Vtbx _ -> row :: got_so_far
                          | _ -> got_so_far
                 ) [] ops_reinterp

(* The groups of intrinsics.  *)
let intrinsic_groups =
  [ "Addition", single_opcode Vadd;
    "Multiplication", single_opcode Vmul;
    "Multiply-accumulate", single_opcode Vmla;
    "Multiply-subtract", single_opcode Vmls;
    "Subtraction", single_opcode Vsub;
    "Comparison (equal-to)", single_opcode Vceq;
    "Comparison (greater-than-or-equal-to)", single_opcode Vcge;
    "Comparison (less-than-or-equal-to)", single_opcode Vcle;
    "Comparison (greater-than)", single_opcode Vcgt;
    "Comparison (less-than)", single_opcode Vclt;
    "Comparison (absolute greater-than-or-equal-to)", single_opcode Vcage;
    "Comparison (absolute less-than-or-equal-to)", single_opcode Vcale;
    "Comparison (absolute greater-than)", single_opcode Vcagt;
    "Comparison (absolute less-than)", single_opcode Vcalt;
    "Test bits", single_opcode Vtst;
    "Absolute difference", single_opcode Vabd;
    "Absolute difference and accumulate", single_opcode Vaba;
    "Maximum", single_opcode Vmax;
    "Minimum", single_opcode Vmin;
    "Pairwise add", single_opcode Vpadd;
    "Pairwise add, single_opcode widen and accumulate", single_opcode Vpada;
    "Folding maximum", single_opcode Vpmax;
    "Folding minimum", single_opcode Vpmin;
    "Reciprocal step", multiple_opcodes [Vrecps; Vrsqrts];
    "Vector shift left", single_opcode Vshl;
    "Vector shift left by constant", single_opcode Vshl_n;
    "Vector shift right by constant", single_opcode Vshr_n;
    "Vector shift right by constant and accumulate", single_opcode Vsra_n;
    "Vector shift right and insert", single_opcode Vsri;
    "Vector shift left and insert", single_opcode Vsli;
    "Absolute value", single_opcode Vabs;
    "Negation", single_opcode Vneg;
    "Bitwise not", single_opcode Vmvn;
    "Count leading sign bits", single_opcode Vcls;
    "Count leading zeros", single_opcode Vclz;
    "Count number of set bits", single_opcode Vcnt;
    "Reciprocal estimate", single_opcode Vrecpe;
    "Reciprocal square-root estimate", single_opcode Vrsqrte;
    "Get lanes from a vector", single_opcode Vget_lane;
    "Set lanes in a vector", single_opcode Vset_lane;
    "Create vector from literal bit pattern", single_opcode Vcreate;
    "Set all lanes to the same value",
      multiple_opcodes [Vdup_n; Vmov_n; Vdup_lane];
    "Combining vectors", single_opcode Vcombine;
    "Splitting vectors", multiple_opcodes [Vget_high; Vget_low];
    "Conversions", multiple_opcodes [Vcvt; Vcvt_n];
    "Move, single_opcode narrowing", single_opcode Vmovn;
    "Move, single_opcode long", single_opcode Vmovl;
    "Table lookup", tbl_opcode;
    "Extended table lookup", tbx_opcode;
    "Multiply, lane", single_opcode Vmul_lane;
    "Long multiply, lane", single_opcode Vmull_lane;
    "Saturating doubling long multiply, lane", single_opcode Vqdmull_lane;
    "Saturating doubling multiply high, lane", single_opcode Vqdmulh_lane;
    "Multiply-accumulate, lane", single_opcode Vmla_lane;
    "Multiply-subtract, lane", single_opcode Vmls_lane;
    "Vector multiply by scalar", single_opcode Vmul_n;
    "Vector long multiply by scalar", single_opcode Vmull_n;
    "Vector saturating doubling long multiply by scalar",
      single_opcode Vqdmull_n;
    "Vector saturating doubling multiply high by scalar",
      single_opcode Vqdmulh_n;
    "Vector multiply-accumulate by scalar", single_opcode Vmla_n;
    "Vector multiply-subtract by scalar", single_opcode Vmls_n;
    "Vector extract", single_opcode Vext;
    "Reverse elements", multiple_opcodes [Vrev64; Vrev32; Vrev16];
    "Bit selection", single_opcode Vbsl;
    "Transpose elements", single_opcode Vtrn;
    "Zip elements", single_opcode Vzip;
    "Unzip elements", single_opcode Vuzp;
    "Element/structure loads, VLD1 variants", ldx_opcode 1;
    "Element/structure stores, VST1 variants", stx_opcode 1;
    "Element/structure loads, VLD2 variants", ldx_opcode 2;
    "Element/structure stores, VST2 variants", stx_opcode 2;
    "Element/structure loads, VLD3 variants", ldx_opcode 3;
    "Element/structure stores, VST3 variants", stx_opcode 3;
    "Element/structure loads, VLD4 variants", ldx_opcode 4;
    "Element/structure stores, VST4 variants", stx_opcode 4;
    "Logical operations (AND)", single_opcode Vand;
    "Logical operations (OR)", single_opcode Vorr;
    "Logical operations (exclusive OR)", single_opcode Veor;
    "Logical operations (AND-NOT)", single_opcode Vbic;
    "Logical operations (OR-NOT)", single_opcode Vorn;
    "Reinterpret casts", single_opcode Vreinterp ]

(* Given an intrinsic shape, produce a string to document the corresponding
   operand shapes.  *)
let rec analyze_shape shape =
  let rec n_things n thing =
    match n with
      0 -> []
    | n -> thing :: (n_things (n - 1) thing)
  in
  let rec analyze_shape_elt reg_no elt =
    match elt with
      Dreg -> "@var{d" ^ (string_of_int reg_no) ^ "}"
    | Qreg -> "@var{q" ^ (string_of_int reg_no) ^ "}"
    | Corereg -> "@var{r" ^ (string_of_int reg_no) ^ "}"
    | Immed -> "#@var{0}"
    | VecArray (1, elt) ->
        let elt_regexp = analyze_shape_elt 0 elt in
          "@{" ^ elt_regexp ^ "@}"
    | VecArray (n, elt) ->
      let rec f m =
        match m with
          0 -> []
        | m -> (analyze_shape_elt (m - 1) elt) :: (f (m - 1))
      in
      let ops = List.rev (f n) in
        "@{" ^ (commas (fun x -> x) ops "") ^ "@}"
    | (PtrTo elt | CstPtrTo elt) ->
      "[" ^ (analyze_shape_elt reg_no elt) ^ "]"
    | Element_of_dreg -> (analyze_shape_elt reg_no Dreg) ^ "[@var{0}]"
    | Element_of_qreg -> (analyze_shape_elt reg_no Qreg) ^ "[@var{0}]"
    | All_elements_of_dreg -> (analyze_shape_elt reg_no Dreg) ^ "[]"
  in
    match shape with
      All (n, elt) -> commas (analyze_shape_elt 0) (n_things n elt) ""
    | Long -> (analyze_shape_elt 0 Qreg) ^ ", " ^ (analyze_shape_elt 0 Dreg) ^
              ", " ^ (analyze_shape_elt 0 Dreg)
    | Long_noreg elt -> (analyze_shape_elt 0 elt) ^ ", " ^
              (analyze_shape_elt 0 elt)
    | Wide -> (analyze_shape_elt 0 Qreg) ^ ", " ^ (analyze_shape_elt 0 Qreg) ^
              ", " ^ (analyze_shape_elt 0 Dreg)
    | Wide_noreg elt -> analyze_shape (Long_noreg elt)
    | Narrow -> (analyze_shape_elt 0 Dreg) ^ ", " ^ (analyze_shape_elt 0 Qreg) ^
                ", " ^ (analyze_shape_elt 0 Qreg)
    | Use_operands elts -> commas (analyze_shape_elt 0) (Array.to_list elts) ""
    | By_scalar Dreg ->
        analyze_shape (Use_operands [| Dreg; Dreg; Element_of_dreg |])
    | By_scalar Qreg ->
        analyze_shape (Use_operands [| Qreg; Qreg; Element_of_dreg |])
    | By_scalar _ -> assert false
    | Wide_lane ->
        analyze_shape (Use_operands [| Qreg; Dreg; Element_of_dreg |])
    | Wide_scalar ->
        analyze_shape (Use_operands [| Qreg; Dreg; Element_of_dreg |])
    | Pair_result elt ->
      let elt_regexp = analyze_shape_elt 0 elt in
      let elt_regexp' = analyze_shape_elt 1 elt in
        elt_regexp ^ ", " ^ elt_regexp'
    | Unary_scalar _ -> "FIXME Unary_scalar"
    | Binary_imm elt -> analyze_shape (Use_operands [| elt; elt; Immed |])
    | Narrow_imm -> analyze_shape (Use_operands [| Dreg; Qreg; Immed |])
    | Long_imm -> analyze_shape (Use_operands [| Qreg; Dreg; Immed |])

(* Document a single intrinsic.  *)
let describe_intrinsic first chan
                       (elt_ty, (_, features, shape, name, munge, _)) =
  let c_arity, new_elt_ty = munge shape elt_ty in
  let c_types = strings_of_arity c_arity in
  Printf.fprintf chan "@itemize @bullet\n";
  let item_code = if first then "@item" else "@itemx" in
    Printf.fprintf chan "%s %s %s_%s (" item_code (List.hd c_types)
                   (intrinsic_name name) (string_of_elt elt_ty);
    Printf.fprintf chan "%s)\n" (commas (fun ty -> ty) (List.tl c_types) "");
    if not (List.exists (fun feature -> feature = No_op) features) then
    begin
      let print_one_insn name =
        Printf.fprintf chan "@code{";
        let no_suffix = (new_elt_ty = NoElts) in
        let name_with_suffix =
          if no_suffix then name
          else name ^ "." ^ (string_of_elt_dots new_elt_ty)
        in
        let possible_operands = analyze_all_shapes features shape
                                                   analyze_shape
        in
	let rec print_one_possible_operand op =
	  Printf.fprintf chan "%s %s}" name_with_suffix op
        in
          (* If the intrinsic expands to multiple instructions, we assume
             they are all of the same form.  *)
          print_one_possible_operand (List.hd possible_operands)
      in
      let rec print_insns names =
        match names with
          [] -> ()
        | [name] -> print_one_insn name
        | name::names -> (print_one_insn name;
                          Printf.fprintf chan " @emph{or} ";
                          print_insns names)
      in
      let insn_names = get_insn_names features name in
        Printf.fprintf chan "@*@emph{Form of expected instruction(s):} ";
        print_insns insn_names;
        Printf.fprintf chan "\n"
    end;
    Printf.fprintf chan "@end itemize\n";
    Printf.fprintf chan "\n\n"

(* Document a group of intrinsics.  *)
let document_group chan (group_title, group_extractor) =
  (* Extract the rows in question from the ops table and then turn them
     into a list of intrinsics.  *)
  let intrinsics =
    List.fold_left (fun got_so_far ->
                    fun row ->
                      match row with
                        (_, _, _, _, _, elt_tys) ->
                          List.fold_left (fun got_so_far' ->
                                          fun elt_ty ->
                                            (elt_ty, row) :: got_so_far')
                                         got_so_far elt_tys
                   ) [] (group_extractor ())
  in
    (* Emit the title for this group.  *)
    Printf.fprintf chan "@subsubsection %s\n\n" group_title;
    (* Emit a description of each intrinsic.  *)
    List.iter (describe_intrinsic true chan) intrinsics;
    (* Close this group.  *)
    Printf.fprintf chan "\n\n"

let gnu_header chan =
  List.iter (fun s -> Printf.fprintf chan "%s\n" s) [
  "@c Copyright (C) 2006 Free Software Foundation, Inc.";
  "@c This is part of the GCC manual.";
  "@c For copying conditions, see the file gcc.texi.";
  "";
  "@c This file is generated automatically using gcc/config/arm/neon-docgen.ml";
  "@c Please do not edit manually."]

(* Program entry point.  *)
let _ =
  if Array.length Sys.argv <> 2 then
    failwith "Usage: neon-docgen <output filename>"
  else
  let file = Sys.argv.(1) in
    try
      let chan = open_out file in
        gnu_header chan;
        List.iter (document_group chan) intrinsic_groups;
        close_out chan
    with Sys_error sys ->
      failwith ("Could not create output file " ^ file ^ ": " ^ sys)
