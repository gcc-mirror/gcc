(* Auto-generate Nios II R2 CDX ldwm/stwm/push.n/pop.n patterns
   Copyright (C) 2014-2019 Free Software Foundation, Inc.
   Contributed by Mentor Graphics.

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

   This is a Standard ML program.  There are multiple Standard ML
   implementations widely available.  We recommend the MLton optimizing
   SML compiler, due to its ease of creating a standalone executable.

     http://www.mlton.org/

   Or from your favourite OS's friendly packaging system. Tested with
   MLton Release 20130715, though other versions will probably work too.

   Run with:
     mlton -output a.out /path/to/gcc/config/nios2/nios2-ldstwm.sml
     ./a.out >/path/to/gcc/config/nios2/ldstwm.md
*)

datatype ld_st = ld | st;    
datatype push_pop = push | pop;
datatype inc_dec = inc | dec;

fun for ls f = map f ls;
fun conds cond str = if cond then str else "";
fun ints n = if n>=0 then (Int.toString n) else ("-" ^ (Int.toString (~n)));

fun pushpop_pattern pptype n fp =
    let 
	val sp_reg = "(reg:SI SP_REGNO)";
	val ra_reg = "(reg:SI RA_REGNO)";
	val fp_reg = "(reg:SI FP_REGNO)";

	fun sets lhs rhs = "(set " ^ lhs ^
			   (if pptype=push then " "
			    else " ") ^ rhs ^ ")";
	val sp_adj =
	    "(set " ^ sp_reg ^ "\n          " ^
	    "(plus:SI " ^ sp_reg ^
	    " (match_operand 1 \"const_int_operand\" \"\")))";

	fun reg i regi = "(reg:SI " ^ (ints regi) ^ ")";
	fun mem i opndi =
	    if pptype=push then
		"(mem:SI (plus:SI (reg:SI SP_REGNO) (const_int " ^ (ints (~4*i)) ^ ")))"
	    else
		"(match_operand:SI " ^
		(ints opndi) ^ " \"stack_memory_operand\" \"\")";

	val start = 1 + (if fp then 2 else 1);
	val lim = n + (if fp then 2 else 1);
	fun set_elt i regi opndi =
	    if pptype=push then (sets (mem i opndi) (reg i regi))
	    else (sets (reg i regi) (mem i opndi));
	fun get_elt_list (i, regi, opndi) =
	    if i > lim then []
	    else (set_elt i regi opndi) :: get_elt_list (i+1, regi-1, opndi+1);

	val set_elements = get_elt_list (start, 16+n-1, start+1);

	val ra_set = if pptype=push then sets (mem 1 2) ra_reg
		     else sets ra_reg (mem 1 2);
	val fp_set = (conds fp (if pptype=push then sets (mem 2 3) fp_reg
				else sets fp_reg (mem 2 3)));
	val ret = (conds (pptype=pop) "(return)");
	val element_list =
	    List.filter (fn x => x<>"")
			([ret, sp_adj, ra_set, fp_set] @ set_elements);

	fun reg_index i = 16 + n - i;
	fun pop_opnds 0 spl = (conds fp ("fp" ^ spl)) ^ "ra"
	  | pop_opnds n spl = "r" ^ (ints (reg_index n)) ^ spl ^ (pop_opnds (n-1) spl);
	fun push_opnds 0 spl = "ra" ^ (conds fp (spl ^ "fp"))
	  | push_opnds n spl = (push_opnds (n-1) spl) ^ spl ^ "r" ^ (ints (reg_index n));

	val spadj_opnd = if pptype=push then 2 else (start+n);
	val spadj = ints spadj_opnd;
	val regsave_num = n + (if fp then 2 else 1);

	val ppname = if pptype=push then "push" else "pop";
	val name = if pptype=push then "push" ^ "_" ^ (push_opnds n "_")
		   else "pop" ^ "_" ^ (pop_opnds n "_");
    in
	"(define_insn \"*cdx_" ^ name ^ "\"\n" ^
	"  [(match_parallel 0 \"" ^
	(conds (pptype=pop) "pop_operation") ^ "\"\n" ^
	"    [" ^ (String.concatWith ("\n     ") element_list) ^ "])]\n" ^
	"   \"TARGET_HAS_CDX && XVECLEN (operands[0], 0) == " ^
	(ints (length element_list)) ^
	(conds (pptype=push)
	       ("\n    && (-INTVAL (operands[1]) & 3) == 0\n" ^
		"    && (-INTVAL (operands[1]) - " ^
		(ints (4*regsave_num)) ^ ") <= 60")) ^
	"\"\n" ^
	(if pptype=pop then
	     "{\n" ^
	     "  rtx x = XEXP (operands[" ^ spadj ^ "], 0);\n" ^
	     "  operands[" ^ spadj ^ "] = REG_P (x) ? const0_rtx : XEXP (x, 1);\n" ^
	     "  return \"pop.n\\\\t{" ^ (pop_opnds n ", ") ^ "}, %" ^ spadj ^ "\";\n" ^
	     "}\n"
	 else
	     "{\n" ^
	     "  operands[" ^ spadj ^ "] = " ^
	     "GEN_INT (-INTVAL (operands[1]) - " ^ (ints (4*regsave_num)) ^ ");\n" ^
	     "  return \"push.n\\\\t{" ^ (push_opnds n ", ") ^ "}, %" ^ spadj ^ "\";\n" ^
	     "}\n") ^
	"  [(set_attr \"type\" \"" ^ ppname ^ "\")])\n\n"
    end;

fun ldstwm_pattern ldst n id wb pc =
    let
	val ldstwm = (if ldst=ld then "ldwm" else "stwm");
	val name = "*cdx_" ^ ldstwm ^ (Int.toString n) ^
		   (if id=inc then "_inc" else "_dec") ^
		   (conds wb "_wb") ^ (conds pc "_ret");
	val base_reg_referenced_p = ref false;
	val base_regno = ints (n+1);
	fun plus_addr base offset =
	    "(plus:SI " ^ base ^ " (const_int " ^ (ints offset) ^ "))";
	fun base_reg () =
	    if !base_reg_referenced_p then
		"(match_dup " ^ base_regno ^ ")"
	    else (base_reg_referenced_p := true;
		  "(match_operand:SI " ^ base_regno ^
		  " \"register_operand\" \"" ^ (conds wb "+&") ^ "r\")");
	fun reg i = "(match_operand:SI " ^ (ints i) ^
		    " \"nios2_hard_register_operand\" \"" ^
		    (conds (ldst=ld) "") ^ "\")";

	fun addr 1 = if id=inc then base_reg ()
		     else plus_addr (base_reg ()) (~4)
	  | addr i = let val offset = if id=inc then (i-1)*4 else (~i*4)
		     in plus_addr (base_reg ()) offset end;

	fun mem i = "(mem:SI " ^ (addr i) ^ ")";
	fun lhs i = if ldst=ld then reg i else mem i;
	fun rhs i = if ldst=st then reg i else mem i;
	fun sets lhs rhs = "(set " ^ lhs ^ "\n          " ^ rhs ^ ")";
	fun set_elements i =
	    if i > n then []
	    else (sets (lhs i) (rhs i)) :: (set_elements (i+1));

	fun opnds 1 = "%1"
	  | opnds n = opnds(n-1) ^ ", %" ^ (Int.toString n);

	val asm_template = ldstwm ^ "\\\\t{" ^ (opnds n) ^ "}" ^
			   (if id=inc
			    then ", (%" ^ base_regno ^ ")++"
			    else ", --(%" ^ base_regno ^ ")") ^
			   (conds wb ", writeback") ^
			   (conds pc ", ret");
	val wbtmp =
	    if wb then
		(sets (base_reg ())
		      (plus_addr (base_reg ())
				 ((if id=inc then n else ~n)*4)))
	    else "";
	val pctmp = conds pc "(return)";
	val set_list = List.filter (fn x => x<>"")
				   ([pctmp, wbtmp] @ (set_elements 1));
    in
	if ldst=st andalso pc then ""
	else
	    "(define_insn \"" ^ name ^ "\"\n" ^
	    "  [(match_parallel 0 \"" ^ ldstwm ^  "_operation\"\n" ^
	    "    [" ^ (String.concatWith ("\n     ") set_list) ^ "])]\n" ^
	    "   \"TARGET_HAS_CDX && XVECLEN (operands[0], 0) == " ^
	    (ints (length set_list)) ^ "\"\n" ^
	    "   \"" ^ asm_template ^ "\"\n" ^
	    "  [(set_attr \"type\" \"" ^ ldstwm ^ "\")])\n\n"
    end;

fun peephole_pattern ldst n scratch_p =
    let
	fun sets lhs rhs = "(set " ^ lhs ^ "\n        " ^ rhs ^ ")";
	fun single_set i indent =
	    let val reg = "(match_operand:SI " ^ (ints i) ^
			  " \"register_operand\" \"\")";
		val mem = "(match_operand:SI " ^ (ints (i+n)) ^
			  " \"memory_operand\" \"\")";
	    in
		if ldst=ld then sets reg mem
		else sets mem reg
	    end;

	fun single_sets i =
	    if i=n then []
	    else (single_set i "   ") :: (single_sets (i+1));

	val scratch = ints (2*n);
	val peephole_elements =
	    let val tmp = single_sets 0 in
		if scratch_p
		then (["(match_scratch:SI " ^ scratch ^ " \"r\")"] @
		      tmp @
		      ["(match_dup " ^ scratch ^ ")"])
		else tmp
	    end;
    in
	"(define_peephole2\n" ^
	"  [" ^ (String.concatWith ("\n   ") peephole_elements) ^ "]\n" ^
	"  \"TARGET_HAS_CDX\"\n" ^
	"  [(const_int 0)]\n" ^
	"{\n" ^
	"  if (gen_ldstwm_peep (" ^
	(if ldst=st then "false" else "true") ^ ", " ^ (ints n) ^ ", " ^ 
	(if scratch_p then ("operands[" ^ scratch ^ "]") else "NULL_RTX") ^
	", operands))\n" ^
	"    DONE;\n" ^
	"  else\n" ^
	"    FAIL;\n" ^
	"})\n\n"
    end;


print
("/* Nios II R2 CDX ldwm/stwm/push.h/pop.n instruction patterns.\n" ^
 "   This file was automatically generated using nios2-ldstwm.sml.\n" ^
 "   Please do not edit manually.\n" ^
 "\n" ^
 "   Copyright (C) 2014-2019 Free Software Foundation, Inc.\n" ^
 "   Contributed by Mentor Graphics.\n" ^
 "\n" ^
 "   This file is part of GCC.\n" ^
 "\n" ^
 "   GCC is free software; you can redistribute it and/or modify it\n" ^
 "   under the terms of the GNU General Public License as published\n" ^
 "   by the Free Software Foundation; either version 3, or (at your\n" ^
 "   option) any later version.\n" ^
 "\n" ^
 "   GCC is distributed in the hope that it will be useful, but WITHOUT\n" ^
 "   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY\n" ^
 "   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public\n" ^
 "   License for more details.\n" ^
 "\n" ^
 "   You should have received a copy of the GNU General Public License and\n" ^
 "   a copy of the GCC Runtime Library Exception along with this program;\n" ^
 "   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see\n" ^
 "   <http://www.gnu.org/licenses/>.  */\n\n");

fun seq a b = if a=b then [b]
	      else a :: (seq (if a<b then a+1 else a-1) b);

(* push/pop patterns *)
for (seq 0 8) (fn n =>
  for [push, pop] (fn p =>
    for [true, false] (fn fp =>
       print (pushpop_pattern p n fp))));

(* ldwm/stwm patterns *)
for [ld, st] (fn l =>
  for (seq 1 12) (fn n =>
    for [inc, dec] (fn id =>
      for [true, false] (fn wb =>
        for [true, false] (fn pc =>
          print (ldstwm_pattern l n id wb pc))))));

(* peephole patterns *)
for [ld, st] (fn l =>
  for (seq 12 2) (fn n =>
    print (peephole_pattern l n true)));

