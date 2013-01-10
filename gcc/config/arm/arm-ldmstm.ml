(* Auto-generate ARM ldm/stm patterns
   Copyright (C) 2010-2013 Free Software Foundation, Inc.
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

   Run with:
     ocaml arm-ldmstm.ml >/path/to/gcc/config/arm/ldmstm.md
*)

type amode = IA | IB | DA | DB

type optype = IN | OUT | INOUT

let rec string_of_addrmode addrmode =
  match addrmode with
    IA -> "ia" | IB -> "ib" | DA -> "da" | DB -> "db"

let rec initial_offset addrmode nregs =
  match addrmode with
    IA -> 0
  | IB -> 4
  | DA -> -4 * nregs + 4
  | DB -> -4 * nregs

let rec final_offset addrmode nregs =
  match addrmode with
    IA -> nregs * 4
  | IB -> nregs * 4
  | DA -> -4 * nregs
  | DB -> -4 * nregs

let constr thumb =
  if thumb then "l" else "rk"

let inout_constr op_type =
  match op_type with
  OUT -> "=&"
  | INOUT -> "+&"
  | IN -> ""

let destreg nregs first op_type thumb =
  if not first then
    Printf.sprintf "(match_dup %d)" (nregs + 1)
  else
    Printf.sprintf ("(match_operand:SI %d \"s_register_operand\" \"%s%s\")")
      (nregs + 1) (inout_constr op_type) (constr thumb)

let write_ldm_set thumb nregs offset opnr first =
  let indent = "     " in
  Printf.printf "%s" (if first then "    [" else indent);
  Printf.printf "(set (match_operand:SI %d \"arm_hard_register_operand\" \"\")\n" opnr;
  Printf.printf "%s     (mem:SI " indent;
  begin if offset != 0 then Printf.printf "(plus:SI " end;
  Printf.printf "%s" (destreg nregs first IN thumb);
  begin if offset != 0 then Printf.printf "\n%s             (const_int %d))" indent offset end;
  Printf.printf "))"

let write_stm_set thumb nregs offset opnr first =
  let indent = "     " in
  Printf.printf "%s" (if first then "    [" else indent);
  Printf.printf "(set (mem:SI ";
  begin if offset != 0 then Printf.printf "(plus:SI " end;
  Printf.printf "%s" (destreg nregs first IN thumb);
  begin if offset != 0 then Printf.printf " (const_int %d))" offset end;
  Printf.printf ")\n%s     (match_operand:SI %d \"arm_hard_register_operand\" \"\"))" indent opnr 

let write_ldm_peep_set extra_indent nregs opnr first =
  let indent = "   " ^ extra_indent in
  Printf.printf "%s" (if first then extra_indent ^ "  [" else indent);
  Printf.printf "(set (match_operand:SI %d \"s_register_operand\" \"\")\n" opnr;
  Printf.printf "%s     (match_operand:SI %d \"memory_operand\" \"\"))" indent (nregs + opnr)

let write_stm_peep_set extra_indent nregs opnr first =
  let indent = "   " ^ extra_indent in
  Printf.printf "%s" (if first then extra_indent ^ "  [" else indent);
  Printf.printf "(set (match_operand:SI %d \"memory_operand\" \"\")\n" (nregs + opnr);
  Printf.printf "%s     (match_operand:SI %d \"s_register_operand\" \"\"))" indent opnr

let write_any_load optype nregs opnr first =
  let indent = "   " in
  Printf.printf "%s" (if first then "  [" else indent);
  Printf.printf "(set (match_operand:SI %d \"s_register_operand\" \"\")\n" opnr;
  Printf.printf "%s     (match_operand:SI %d \"%s\" \"\"))" indent (nregs * 2 + opnr) optype

let write_const_store nregs opnr first =
  let indent = "   " in
  Printf.printf "%s(set (match_operand:SI %d \"memory_operand\" \"\")\n" indent (nregs + opnr);
  Printf.printf "%s     (match_dup %d))" indent opnr

let write_const_stm_peep_set nregs opnr first =
  write_any_load "const_int_operand" nregs opnr first;
  Printf.printf "\n";
  write_const_store nregs opnr false

  
let rec write_pat_sets func opnr offset first n_left =
  func offset opnr first;
  begin
    if n_left > 1 then begin
      Printf.printf "\n";
      write_pat_sets func (opnr + 1) (offset + 4) false (n_left - 1);
    end else
      Printf.printf "]"
  end

let rec write_peep_sets func opnr first n_left =
  func opnr first;
  begin
    if n_left > 1 then begin
      Printf.printf "\n";
      write_peep_sets func (opnr + 1) false (n_left - 1);
    end
  end
    
let can_thumb addrmode update is_store =
  match addrmode, update, is_store with
    (* Thumb1 mode only supports IA with update.  However, for LDMIA,
       if the address register also appears in the list of loaded
       registers, the loaded value is stored, hence the RTL pattern
       to describe such an insn does not have an update.  We check
       in the match_parallel predicate that the condition described
       above is met.  *)
    IA, _, false -> true
  | IA, true, true -> true
  | _ -> false

let target addrmode thumb =
  match addrmode, thumb with
    IA, true -> "TARGET_THUMB1"
  | IA, false -> "TARGET_32BIT"
  | DB, false -> "TARGET_32BIT"
  | _, false -> "TARGET_ARM"

let write_pattern_1 name ls addrmode nregs write_set_fn update thumb =
  let astr = string_of_addrmode addrmode in
  Printf.printf "(define_insn \"*%s%s%d_%s%s\"\n"
    (if thumb then "thumb_" else "") name nregs astr
    (if update then "_update" else "");
  Printf.printf "  [(match_parallel 0 \"%s_multiple_operation\"\n" ls;
  begin
    if update then begin
      Printf.printf "    [(set %s\n          (plus:SI %s"
	(destreg nregs true INOUT thumb) (destreg nregs false IN thumb);
      Printf.printf " (const_int %d)))\n"
	(final_offset addrmode nregs)
    end
  end;
  write_pat_sets
    (write_set_fn thumb nregs) 1
    (initial_offset addrmode nregs)
    (not update) nregs;
  Printf.printf ")]\n  \"%s && XVECLEN (operands[0], 0) == %d\"\n"
    (target addrmode thumb)
    (if update then nregs + 1 else nregs);
  Printf.printf "  \"%s%%(%s%%)\\t%%%d%s, {"
    name astr (nregs + 1) (if update then "!" else "");
  for n = 1 to nregs; do
    Printf.printf "%%%d%s" n (if n < nregs then ", " else "")
  done;
  Printf.printf "}\"\n";
  Printf.printf "  [(set_attr \"type\" \"%s%d\")" ls nregs;
  begin if not thumb then
    Printf.printf "\n   (set_attr \"predicable\" \"yes\")";
  end;
  Printf.printf "])\n\n"

let write_ldm_pattern addrmode nregs update =
  write_pattern_1 "ldm" "load" addrmode nregs write_ldm_set update false;
  begin if can_thumb addrmode update false then
    write_pattern_1 "ldm" "load" addrmode nregs write_ldm_set update true;
  end

let write_stm_pattern addrmode nregs update =
  write_pattern_1 "stm" "store" addrmode nregs write_stm_set update false;
  begin if can_thumb addrmode update true then
    write_pattern_1 "stm" "store" addrmode nregs write_stm_set update true;
  end

let write_ldm_commutative_peephole thumb =
  let nregs = 2 in
  Printf.printf "(define_peephole2\n";
  write_peep_sets (write_ldm_peep_set "" nregs) 0 true nregs;
  let indent = "   " in
  if thumb then begin
    Printf.printf "\n%s(set (match_operand:SI %d \"s_register_operand\" \"\")\n" indent (nregs * 2);
    Printf.printf "%s     (match_operator:SI %d \"commutative_binary_operator\"\n" indent (nregs * 2 + 1);
    Printf.printf "%s      [(match_operand:SI %d \"s_register_operand\" \"\")\n" indent (nregs * 2 + 2);
    Printf.printf "%s       (match_operand:SI %d \"s_register_operand\" \"\")]))]\n" indent (nregs * 2 + 3)
  end else begin
    Printf.printf "\n%s(parallel\n" indent;
    Printf.printf "%s  [(set (match_operand:SI %d \"s_register_operand\" \"\")\n" indent (nregs * 2);
    Printf.printf "%s        (match_operator:SI %d \"commutative_binary_operator\"\n" indent (nregs * 2 + 1);
    Printf.printf "%s         [(match_operand:SI %d \"s_register_operand\" \"\")\n" indent (nregs * 2 + 2);
    Printf.printf "%s          (match_operand:SI %d \"s_register_operand\" \"\")]))\n" indent (nregs * 2 + 3);
    Printf.printf "%s   (clobber (reg:CC CC_REGNUM))])]\n" indent
  end;
  Printf.printf "  \"((((REGNO (operands[%d]) == REGNO (operands[0]))\n" (nregs * 2 + 2);
  Printf.printf "         && (REGNO (operands[%d]) == REGNO (operands[1])))\n"  (nregs * 2 + 3);
  Printf.printf "      || ((REGNO (operands[%d]) == REGNO (operands[0]))\n" (nregs * 2 + 3);
  Printf.printf "         && (REGNO (operands[%d]) == REGNO (operands[1]))))\n" (nregs * 2 + 2);
  Printf.printf "    && (peep2_regno_dead_p (%d, REGNO (operands[0]))\n" (nregs + 1);
  Printf.printf "      || (REGNO (operands[0]) == REGNO (operands[%d])))\n"  (nregs * 2);
  Printf.printf "    && (peep2_regno_dead_p (%d, REGNO (operands[1]))\n" (nregs + 1);
  Printf.printf "      || (REGNO (operands[1]) == REGNO (operands[%d]))))\"\n" (nregs * 2);
  begin
    if thumb then
      Printf.printf "  [(set (match_dup %d) (match_op_dup %d [(match_dup %d) (match_dup %d)]))]\n"
	(nregs * 2) (nregs * 2 + 1) (nregs * 2 + 2) (nregs * 2 + 3)
    else begin
      Printf.printf "  [(parallel\n";
      Printf.printf "    [(set (match_dup %d) (match_op_dup %d [(match_dup %d) (match_dup %d)]))\n"
	(nregs * 2) (nregs * 2 + 1) (nregs * 2 + 2) (nregs * 2 + 3);
      Printf.printf "     (clobber (reg:CC CC_REGNUM))])]\n"
    end
  end;
  Printf.printf "{\n  if (!gen_ldm_seq (operands, %d, true))\n    FAIL;\n" nregs;
  Printf.printf "})\n\n"

let write_ldm_peephole nregs =
  Printf.printf "(define_peephole2\n";
  write_peep_sets (write_ldm_peep_set "" nregs) 0 true nregs;
  Printf.printf "]\n  \"\"\n  [(const_int 0)]\n{\n";
  Printf.printf "  if (gen_ldm_seq (operands, %d, false))\n    DONE;\n  else\n    FAIL;\n})\n\n" nregs

let write_ldm_peephole_b nregs =
  if nregs > 2 then begin
    Printf.printf "(define_peephole2\n";
    write_ldm_peep_set "" nregs 0 true;
    Printf.printf "\n   (parallel\n";
    write_peep_sets (write_ldm_peep_set "  " nregs) 1 true (nregs - 1);
    Printf.printf "])]\n  \"\"\n  [(const_int 0)]\n{\n";
    Printf.printf "  if (gen_ldm_seq (operands, %d, false))\n    DONE;\n  else\n    FAIL;\n})\n\n" nregs
  end

let write_stm_peephole nregs =
  Printf.printf "(define_peephole2\n";
  write_peep_sets (write_stm_peep_set "" nregs) 0 true nregs;
  Printf.printf "]\n  \"\"\n  [(const_int 0)]\n{\n";
  Printf.printf "  if (gen_stm_seq (operands, %d))\n    DONE;\n  else\n    FAIL;\n})\n\n" nregs

let write_stm_peephole_b nregs =
  if nregs > 2 then begin
    Printf.printf "(define_peephole2\n";
    write_stm_peep_set "" nregs 0 true;
    Printf.printf "\n   (parallel\n";
    write_peep_sets (write_stm_peep_set "" nregs) 1 true (nregs - 1);
    Printf.printf "]\n  \"\"\n  [(const_int 0)]\n{\n";
    Printf.printf "  if (gen_stm_seq (operands, %d))\n    DONE;\n  else\n    FAIL;\n})\n\n" nregs
  end

let write_const_stm_peephole_a nregs =
  Printf.printf "(define_peephole2\n";
  write_peep_sets (write_const_stm_peep_set nregs) 0 true nregs;
  Printf.printf "]\n  \"\"\n  [(const_int 0)]\n{\n";
  Printf.printf "  if (gen_const_stm_seq (operands, %d))\n    DONE;\n  else\n    FAIL;\n})\n\n" nregs

let write_const_stm_peephole_b nregs =
  Printf.printf "(define_peephole2\n";
  write_peep_sets (write_any_load "const_int_operand" nregs) 0 true nregs;
  Printf.printf "\n";
  write_peep_sets (write_const_store nregs) 0 false nregs;
  Printf.printf "]\n  \"\"\n  [(const_int 0)]\n{\n";
  Printf.printf "  if (gen_const_stm_seq (operands, %d))\n    DONE;\n  else\n    FAIL;\n})\n\n" nregs

let patterns () =
  let addrmodes = [ IA; IB; DA; DB ]  in
  let sizes = [ 4; 3; 2] in
  List.iter
    (fun n ->
      List.iter
	(fun addrmode ->
	  write_ldm_pattern addrmode n false;
	  write_ldm_pattern addrmode n true;
	  write_stm_pattern addrmode n false;
	  write_stm_pattern addrmode n true)
	addrmodes;
      write_ldm_peephole n;
      write_ldm_peephole_b n;
      write_const_stm_peephole_a n;
      write_const_stm_peephole_b n;
      write_stm_peephole n;)
    sizes;
  write_ldm_commutative_peephole false;
  write_ldm_commutative_peephole true

let print_lines = List.iter (fun s -> Format.printf "%s@\n" s)

(* Do it.  *)

let _ = 
  print_lines [
"/* ARM ldm/stm instruction patterns.  This file was automatically generated";
"   using arm-ldmstm.ml.  Please do not edit manually.";
"";
"   Copyright (C) 2010-2013 Free Software Foundation, Inc.";
"   Contributed by CodeSourcery.";
"";
"   This file is part of GCC.";
"";
"   GCC is free software; you can redistribute it and/or modify it";
"   under the terms of the GNU General Public License as published";
"   by the Free Software Foundation; either version 3, or (at your";
"   option) any later version.";
"";
"   GCC is distributed in the hope that it will be useful, but WITHOUT";
"   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY";
"   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public";
"   License for more details.";
"";
"   You should have received a copy of the GNU General Public License and";
"   a copy of the GCC Runtime Library Exception along with this program;";
"   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see";
"   <http://www.gnu.org/licenses/>.  */";
""];
  patterns ();
