;;- Machine description for SPARC chip for GNU C compiler
;;   Copyright (C) 1987, 1988, 1989, 1992 Free Software Foundation, Inc.
;;   Contributed by Michael Tiemann (tiemann@cygnus.com)

;; This file is part of GNU CC.

;; GNU CC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU CC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU CC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.

;; Insn type.  Used to default other attribute values.

;; type "unary" insns have one input operand (1) and one output operand (0)
;; type "binary" insns have two input operands (1,2) and one output (0)
;; type "compare" insns have one or two input operands (0,1) and no output
;; type "call_no_delay_slot" is a call followed by an unimp instruction.

(define_attr "type"
  "move,unary,binary,compare,load,store,uncond_branch,branch,call,call_no_delay_slot,address,fpload,fpstore,fp,fpcmp,fpmul,fpdiv,fpsqrt,multi,misc"
  (const_string "binary"))

;; Set true if insn uses call-clobbered intermediate register.
(define_attr "use_clobbered" "false,true"
  (if_then_else (and (eq_attr "type" "address")
		     (match_operand 0 "clobbered_register" ""))
	 	(const_string "true")
		(const_string "false")))

;; Length (in # of insns).
(define_attr "length" ""
  (cond [(eq_attr "type" "load,fpload")
	 (if_then_else (match_operand 1 "symbolic_memory_operand" "")
		       (const_int 2) (const_int 1))

	 (eq_attr "type" "store,fpstore")
	 (if_then_else (match_operand 0 "symbolic_memory_operand" "")
		       (const_int 2) (const_int 1))

	 (eq_attr "type" "address") (const_int 2)

	 (eq_attr "type" "binary")
	 (if_then_else (ior (match_operand 2 "arith_operand" "")
			    (match_operand 2 "arith_double_operand" ""))
		       (const_int 1) (const_int 3))

	 (eq_attr "type" "multi") (const_int 2)

	 (eq_attr "type" "move,unary")
	 (if_then_else (ior (match_operand 1 "arith_operand" "")
			    (match_operand 1 "arith_double_operand" ""))
		       (const_int 1) (const_int 2))]

	(const_int 1)))

(define_asm_attributes
  [(set_attr "length" "1")
   (set_attr "type" "multi")])

;; Attributes for instruction and branch scheduling

(define_attr "in_call_delay" "false,true"
  (cond [(eq_attr "type" "uncond_branch,branch,call,call_no_delay_slot,multi")
	 	(const_string "false")
	 (eq_attr "type" "load,fpload,store,fpstore")
	 	(if_then_else (eq_attr "length" "1")
			      (const_string "true")
			      (const_string "false"))
	 (eq_attr "type" "address")
	 	(if_then_else (eq_attr "use_clobbered" "false")
			      (const_string "true")
			      (const_string "false"))]
	(if_then_else (eq_attr "length" "1")
		      (const_string "true")
		      (const_string "false"))))

(define_delay (eq_attr "type" "call")
  [(eq_attr "in_call_delay" "true") (nil) (nil)])

;; ??? Should implement the notion of predelay slots for floating point
;; branches.  This would allow us to remove the nop always inserted before
;; a floating point branch.

;; ??? It is OK for fill_simple_delay_slots to put load/store instructions
;; in a delay slot, but it is not OK for fill_eager_delay_slots to do so.
;; This is because doing so will add several pipeline stalls to the path
;; that the load/store did not come from.  Unfortunately, there is no way
;; to prevent fill_eager_delay_slots from using load/store without completely
;; disabling them.  For the SPEC benchmark set, this is a serious lose,
;; because it prevents us from moving back the final store of inner loops.

(define_attr "in_branch_delay" "false,true"
  (if_then_else (and (eq_attr "type" "!uncond_branch,branch,call,call_no_delay_slot,multi")
		     (eq_attr "length" "1"))
		(const_string "true")
		(const_string "false")))

(define_attr "in_uncond_branch_delay" "false,true"
  (if_then_else (and (eq_attr "type" "!uncond_branch,branch,call,call_no_delay_slot,multi")
		     (eq_attr "length" "1"))
		(const_string "true")
		(const_string "false")))

(define_attr "in_annul_branch_delay" "false,true"
  (if_then_else (and (eq_attr "type" "!uncond_branch,branch,call,call_no_delay_slot,multi")
		     (eq_attr "length" "1"))
		(const_string "true")
		(const_string "false")))

(define_delay (eq_attr "type" "branch")
  [(eq_attr "in_branch_delay" "true")
   (nil) (eq_attr "in_annul_branch_delay" "true")])

(define_delay (eq_attr "type" "uncond_branch")
  [(eq_attr "in_uncond_branch_delay" "true")
   (nil) (nil)])
   
;; Function units of the SPARC

;; (define_function_unit {name} {num-units} {n-users} {test}
;;                       {ready-delay} {issue-delay} [{conflict-list}])

;; The integer ALU.
;; (Noted only for documentation; units that take one cycle do not need to
;; be specified.)

;; (define_function_unit "alu" 1 0
;;  (eq_attr "type" "unary,binary,move,address") 1 0)

;; Memory with load-delay of 1 (i.e., 2 cycle load).
(define_function_unit "memory" 1 1 (eq_attr "type" "load,fpload") 2 0)

;; SPARC has two floating-point units: the FP ALU,
;; and the FP MUL/DIV/SQRT unit.
;; Instruction timings on the CY7C602 are as follows
;; FABSs	4
;; FADDs/d	5/5
;; FCMPs/d	4/4
;; FDIVs/d	23/37
;; FMOVs	4
;; FMULs/d	5/7
;; FNEGs	4
;; FSQRTs/d	34/63
;; FSUBs/d	5/5
;; FdTOi/s	5/5
;; FsTOi/d	5/5
;; FiTOs/d	9/5

;; The CY7C602 can only support 2 fp isnsn simultaneously.
;; More insns cause the chip to stall.

(define_function_unit "fp_alu" 1 1 (eq_attr "type" "fp") 5 0)
(define_function_unit "fp_mds" 1 1 (eq_attr "type" "fpmul") 7 0)
(define_function_unit "fp_mds" 1 1 (eq_attr "type" "fpdiv") 37 0)
(define_function_unit "fp_mds" 1 1 (eq_attr "type" "fpsqrt") 63 0)

;; Compare instructions.
;; This controls RTL generation and register allocation.

;; We generate RTL for comparisons and branches by having the cmpxx 
;; patterns store away the operands.  Then, the scc and bcc patterns
;; emit RTL for both the compare and the branch.
;;
;; We do this because we want to generate different code for an sne and
;; seq insn.  In those cases, if the second operand of the compare is not
;; const0_rtx, we want to compute the xor of the two operands and test
;; it against zero.
;;
;; We start with the DEFINE_EXPANDs, then then DEFINE_INSNs to match
;; the patterns.  Finally, we have the DEFINE_SPLITs for some of the scc
;; insns that actually require more than one machine instruction.

;; Put cmpsi first among compare insns so it matches two CONST_INT operands.

(define_expand "cmpsi"
  [(set (reg:CC 0)
	(compare:CC (match_operand:SI 0 "register_operand" "")
		    (match_operand:SI 1 "arith_operand" "")))]
  ""
  "
{
  sparc_compare_op0 = operands[0];
  sparc_compare_op1 = operands[1];
  DONE;
}")

(define_expand "cmpsf"
  [(set (reg:CCFP 0)
	(compare:CCFP (match_operand:SF 0 "register_operand" "")
		      (match_operand:SF 1 "register_operand" "")))]
  ""
  "
{
  sparc_compare_op0 = operands[0];
  sparc_compare_op1 = operands[1];
  DONE;
}")

(define_expand "cmpdf"
  [(set (reg:CCFP 0)
	(compare:CCFP (match_operand:DF 0 "register_operand" "")
		      (match_operand:DF 1 "register_operand" "")))]
  ""
  "
{
  sparc_compare_op0 = operands[0];
  sparc_compare_op1 = operands[1];
  DONE;
}")

(define_expand "cmptf"
  [(set (reg:CCFP 0)
	(compare:CCFP (match_operand:TF 0 "register_operand" "")
		      (match_operand:TF 1 "register_operand" "")))]
  ""
  "
{
  sparc_compare_op0 = operands[0];
  sparc_compare_op1 = operands[1];
  DONE;
}")

;; Next come the scc insns.  For seq, sne, sgeu, and sltu, we can do this
;; without jumps using the addx/subx instructions.  For the rest, we do
;; branches.  Seq_special and sne_special clobber the CC reg, because they
;; generate addcc/subcc instructions.

(define_expand "seq_special"
  [(set (match_dup 3) (xor:SI (match_operand:SI 1 "register_operand" "")
			      (match_operand:SI 2 "register_operand" "")))
   (parallel [(set (match_operand:SI 0 "register_operand" "")
		   (eq:SI (match_dup 3) (const_int 0)))
	      (clobber (reg:CC 0))])]
	     
  ""
  "{ operands[3] = gen_reg_rtx (SImode); }")

(define_expand "sne_special"
  [(set (match_dup 3) (xor:SI (match_operand:SI 1 "register_operand" "")
			      (match_operand:SI 2 "register_operand" "")))
   (parallel [(set (match_operand:SI 0 "register_operand" "")
		   (ne:SI (match_dup 3) (const_int 0)))
	      (clobber (reg:CC 0))])]
  ""
  "{ operands[3] = gen_reg_rtx (SImode); }")

(define_expand "seq"
  [(set (match_operand:SI 0 "register_operand" "")
	(eq:SI (match_dup 1) (const_int 0)))]
  ""
  "
{ if (GET_MODE (sparc_compare_op0) == SImode)
    {
      emit_insn (gen_seq_special (operands[0], sparc_compare_op0,
				  sparc_compare_op1));
      DONE;
    }
  else
    operands[1] = gen_compare_reg (EQ, sparc_compare_op0, sparc_compare_op1);
}")

(define_expand "sne"
  [(set (match_operand:SI 0 "register_operand" "")
	(ne:SI (match_dup 1) (const_int 0)))]
  ""
  "
{ if (GET_MODE (sparc_compare_op0) == SImode)
    {
      emit_insn (gen_sne_special (operands[0], sparc_compare_op0,
				  sparc_compare_op1));
      DONE;
    }
  else
    operands[1] = gen_compare_reg (NE, sparc_compare_op0, sparc_compare_op1);
}")

(define_expand "sgt"
  [(set (match_operand:SI 0 "register_operand" "")
	(gt:SI (match_dup 1) (const_int 0)))]
  ""
  "
{ operands[1] = gen_compare_reg (GT, sparc_compare_op0, sparc_compare_op1); }")

(define_expand "slt"
  [(set (match_operand:SI 0 "register_operand" "")
	(lt:SI (match_dup 1) (const_int 0)))]
  ""
  "
{ operands[1] = gen_compare_reg (LT, sparc_compare_op0, sparc_compare_op1); }")

(define_expand "sge"
  [(set (match_operand:SI 0 "register_operand" "")
	(ge:SI (match_dup 1) (const_int 0)))]
  ""
  "
{ operands[1] = gen_compare_reg (GE, sparc_compare_op0, sparc_compare_op1); }")

(define_expand "sle"
  [(set (match_operand:SI 0 "register_operand" "")
	(le:SI (match_dup 1) (const_int 0)))]
  ""
  "
{ operands[1] = gen_compare_reg (LE, sparc_compare_op0, sparc_compare_op1); }")

(define_expand "sgtu"
  [(set (match_operand:SI 0 "register_operand" "")
	(gtu:SI (match_dup 1) (const_int 0)))]
  ""
  "
{
  rtx tem;

  /* We can do ltu easily, so if both operands are registers, swap them and
     do a LTU.  */
  if ((GET_CODE (sparc_compare_op0) == REG
       || GET_CODE (sparc_compare_op0) == SUBREG)
      && (GET_CODE (sparc_compare_op1) == REG
	  || GET_CODE (sparc_compare_op1) == SUBREG))
    {
      tem = sparc_compare_op0;
      sparc_compare_op0 = sparc_compare_op1;
      sparc_compare_op1 = tem;
      emit_insn (gen_sltu (operands[0]));
      DONE;
    }

  operands[1] = gen_compare_reg (LEU, sparc_compare_op0, sparc_compare_op1);
}")

(define_expand "sltu"
  [(set (match_operand:SI 0 "register_operand" "")
	(ltu:SI (match_dup 1) (const_int 0)))]
  ""
  "
{ operands[1] = gen_compare_reg (LTU, sparc_compare_op0, sparc_compare_op1);
}")

(define_expand "sgeu"
  [(set (match_operand:SI 0 "register_operand" "")
	(geu:SI (match_dup 1) (const_int 0)))]
  ""
  "
{ operands[1] = gen_compare_reg (GEU, sparc_compare_op0, sparc_compare_op1);
}")

(define_expand "sleu"
  [(set (match_operand:SI 0 "register_operand" "")
	(leu:SI (match_dup 1) (const_int 0)))]
  ""
  "
{
  rtx tem;

  /* We can do geu easily, so if both operands are registers, swap them and
     do a GEU.  */
  if ((GET_CODE (sparc_compare_op0) == REG
       || GET_CODE (sparc_compare_op0) == SUBREG)
      && (GET_CODE (sparc_compare_op1) == REG
	  || GET_CODE (sparc_compare_op1) == SUBREG))
    {
      tem = sparc_compare_op0;
      sparc_compare_op0 = sparc_compare_op1;
      sparc_compare_op1 = tem;
      emit_insn (gen_sgeu (operands[0]));
      DONE;
    }

  operands[1] = gen_compare_reg (LEU, sparc_compare_op0, sparc_compare_op1);
}")

;; Now the DEFINE_INSNs for the compare and scc cases.  First the compares.

(define_insn ""
  [(set (reg:CC 0)
	(compare:CC (match_operand:SI 0 "register_operand" "r")
		    (match_operand:SI 1 "arith_operand" "rI")))]
  ""
  "cmp %r0,%1"
  [(set_attr "type" "compare")])

(define_insn ""
  [(set (reg:CCFPE 0)
	(compare:CCFPE (match_operand:DF 0 "register_operand" "f")
		       (match_operand:DF 1 "register_operand" "f")))]
  ""
  "fcmped %0,%1"
  [(set_attr "type" "fpcmp")])

(define_insn ""
  [(set (reg:CCFPE 0)
	(compare:CCFPE (match_operand:SF 0 "register_operand" "f")
		       (match_operand:SF 1 "register_operand" "f")))]
  ""
  "fcmpes %0,%1"
  [(set_attr "type" "fpcmp")])

(define_insn ""
  [(set (reg:CCFPE 0)
	(compare:CCFPE (match_operand:TF 0 "register_operand" "f")
		       (match_operand:TF 1 "register_operand" "f")))]
  ""
  "fcmpeq %0,%1"
  [(set_attr "type" "fpcmp")])

(define_insn ""
  [(set (reg:CCFP 0)
	(compare:CCFP (match_operand:DF 0 "register_operand" "f")
		      (match_operand:DF 1 "register_operand" "f")))]
  ""
  "fcmpd %0,%1"
  [(set_attr "type" "fpcmp")])

(define_insn ""
  [(set (reg:CCFP 0)
	(compare:CCFP (match_operand:SF 0 "register_operand" "f")
		      (match_operand:SF 1 "register_operand" "f")))]
  ""
  "fcmps %0,%1"
  [(set_attr "type" "fpcmp")])

(define_insn ""
  [(set (reg:CCFP 0)
	(compare:CCFP (match_operand:TF 0 "register_operand" "f")
		      (match_operand:TF 1 "register_operand" "f")))]
  ""
  "fcmpq %0,%1"
  [(set_attr "type" "fpcmp")])

;; The SEQ and SNE patterns are special because they can be done
;; without any branching and do not involve a COMPARE.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ne:SI (match_operand:SI 1 "register_operand" "r") (const_int 0)))
   (clobber (reg:CC 0))]
  ""
  "subcc %%g0,%1,%%g0\;addx %%g0,0,%0"
  [(set_attr "type" "unary")
   (set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (ne:SI (match_operand:SI 1 "register_operand" "r")
		       (const_int 0))))
   (clobber (reg:CC 0))]
  ""
  "subcc %%g0,%1,%%g0\;subx %%g0,0,%0"
  [(set_attr "type" "unary")
   (set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(eq:SI (match_operand:SI 1 "register_operand" "r") (const_int 0)))
   (clobber (reg:CC 0))]
  ""
  "subcc %%g0,%1,%%g0\;subx %%g0,-1,%0"
  [(set_attr "type" "unary")
   (set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (eq:SI (match_operand:SI 1 "register_operand" "r")
		       (const_int 0))))
   (clobber (reg:CC 0))]
  ""
  "subcc %%g0,%1,%%g0\;addx %%g0,-1,%0"
  [(set_attr "type" "unary")
   (set_attr "length" "2")])

;; We can also do (x + (i == 0)) and related, so put them in.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (ne:SI (match_operand:SI 1 "register_operand" "r")
			(const_int 0))
		 (match_operand:SI 2 "register_operand" "r")))
   (clobber (reg:CC 0))]
  ""
  "subcc %%g0,%1,%%g0\;addx %2,0,%0"
  [(set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 2 "register_operand" "r")
		  (ne:SI (match_operand:SI 1 "register_operand" "r")
			 (const_int 0))))
   (clobber (reg:CC 0))]
  ""
  "subcc %%g0,%1,%%g0\;subx %2,0,%0"
  [(set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (eq:SI (match_operand:SI 1 "register_operand" "r")
			(const_int 0))
		 (match_operand:SI 2 "register_operand" "r")))
   (clobber (reg:CC 0))]
  ""
  "subcc %%g0,%1,%%g0\;subx %2,-1,%0"
  [(set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 2 "register_operand" "r")
		  (eq:SI (match_operand:SI 1 "register_operand" "r")
			 (const_int 0))))
   (clobber (reg:CC 0))]
  ""
  "subcc %%g0,%1,%%g0\;addx %2,-1,%0"
  [(set_attr "length" "2")])

;; We can also do GEU and LTU directly, but these operate after a
;; compare.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ltu:SI (reg:CC 0) (const_int 0)))]
  ""
  "addx %%g0,0,%0"
  [(set_attr "type" "misc")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (ltu:SI (reg:CC 0) (const_int 0))))]
  ""
  "subx %%g0,0,%0"
  [(set_attr "type" "misc")])

;; ??? Combine should canonicalize these next two to the same pattern.
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (neg:SI (ltu:SI (reg:CC 0) (const_int 0)))
		  (match_operand:SI 1 "arith_operand" "rI")))]
  ""
  "subx %%g0,%1,%0"
  [(set_attr "type" "unary")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (plus:SI (ltu:SI (reg:CC 0) (const_int 0))
			 (match_operand:SI 1 "arith_operand" "rI"))))]
  ""
  "subx %%g0,%1,%0"
  [(set_attr "type" "unary")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(geu:SI (reg:CC 0) (const_int 0)))]
  ""
  "subx %%g0,-1,%0"
  [(set_attr "type" "misc")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (geu:SI (reg:CC 0) (const_int 0))))]
  ""
  "addx %%g0,-1,%0"
  [(set_attr "type" "misc")])

;; We can also do (x + ((unsigned) i >= 0)) and related, so put them in.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (ltu:SI (reg:CC 0) (const_int 0))
		 (match_operand:SI 1 "arith_operand" "rI")))]
  ""
  "addx %%g0,%1,%0"
  [(set_attr "type" "unary")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (ltu:SI (reg:CC 0) (const_int 0))
		 (plus:SI (match_operand:SI 1 "arith_operand" "%r")
			  (match_operand:SI 2 "arith_operand" "rI"))))]
  ""
  "addx %1,%2,%0")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 1 "register_operand" "r")
		  (ltu:SI (reg:CC 0) (const_int 0))))]
  ""
  "subx %1,0,%0"
  [(set_attr "type" "unary")])

;; ??? Combine should canonicalize these next two to the same pattern.
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (minus:SI (match_operand:SI 1 "register_operand" "r")
			    (match_operand:SI 2 "arith_operand" "rI"))
		  (ltu:SI (reg:CC 0) (const_int 0))))]
  ""
  "subx %1,%2,%0")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 1 "register_operand" "r")
		  (plus:SI (ltu:SI (reg:CC 0) (const_int 0))
			   (match_operand:SI 2 "arith_operand" "rI"))))]
  ""
  "subx %1,%2,%0")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (geu:SI (reg:CC 0) (const_int 0))
		 (match_operand:SI 1 "register_operand" "r")))]
  ""
  "subx %1,-1,%0"
  [(set_attr "type" "unary")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 1 "register_operand" "r")
		  (geu:SI (reg:CC 0) (const_int 0))))]
  ""
  "addx %1,-1,%0"
  [(set_attr "type" "unary")])

;; Now we have the generic scc insns.  These will be done using a jump.
;; We have to exclude the cases above, since we will not want combine to
;; turn something that does not require a jump into something that does.
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operator:SI 1 "noov_compare_op" [(reg 0) (const_int 0)]))]
  ""
  "* return output_scc_insn (operands, insn); "
  [(set_attr "type" "multi")
   (set_attr "length" "3")])

;; These control RTL generation for conditional jump insns

(define_expand "beq"
  [(set (pc)
	(if_then_else (eq (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{ operands[1] = gen_compare_reg (EQ, sparc_compare_op0, sparc_compare_op1); }")

(define_expand "bne"
  [(set (pc)
	(if_then_else (ne (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{ operands[1] = gen_compare_reg (NE, sparc_compare_op0, sparc_compare_op1); }")

(define_expand "bgt"
  [(set (pc)
	(if_then_else (gt (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{ operands[1] = gen_compare_reg (GT, sparc_compare_op0, sparc_compare_op1); }")

(define_expand "bgtu"
  [(set (pc)
	(if_then_else (gtu (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{ operands[1] = gen_compare_reg (GTU, sparc_compare_op0, sparc_compare_op1);
}")

(define_expand "blt"
  [(set (pc)
	(if_then_else (lt (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{ operands[1] = gen_compare_reg (LT, sparc_compare_op0, sparc_compare_op1); }")

(define_expand "bltu"
  [(set (pc)
	(if_then_else (ltu (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{ operands[1] = gen_compare_reg (LTU, sparc_compare_op0, sparc_compare_op1);
}")

(define_expand "bge"
  [(set (pc)
	(if_then_else (ge (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{ operands[1] = gen_compare_reg (GE, sparc_compare_op0, sparc_compare_op1); }")

(define_expand "bgeu"
  [(set (pc)
	(if_then_else (geu (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{ operands[1] = gen_compare_reg (GEU, sparc_compare_op0, sparc_compare_op1);
}")

(define_expand "ble"
  [(set (pc)
	(if_then_else (le (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{ operands[1] = gen_compare_reg (LE, sparc_compare_op0, sparc_compare_op1); }")

(define_expand "bleu"
  [(set (pc)
	(if_then_else (leu (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{ operands[1] = gen_compare_reg (LEU, sparc_compare_op0, sparc_compare_op1);
}")

;; Now match both normal and inverted jump.

(define_insn ""
  [(set (pc)
	(if_then_else (match_operator 0 "noov_compare_op"
				      [(reg 0) (const_int 0)])
		      (label_ref (match_operand 1 "" ""))
		      (pc)))]
  ""
  "*
{
  return output_cbranch (operands[0], 1, 0,
			 final_sequence && INSN_ANNULLED_BRANCH_P (insn),
			 ! final_sequence);
}"
  [(set_attr "type" "branch")])

(define_insn ""
  [(set (pc)
	(if_then_else (match_operator 0 "noov_compare_op"
				      [(reg 0) (const_int 0)])
		      (pc)
		      (label_ref (match_operand 1 "" ""))))]
  ""
  "*
{
  return output_cbranch (operands[0], 1, 1,
			 final_sequence && INSN_ANNULLED_BRANCH_P (insn),
			 ! final_sequence);
}"
  [(set_attr "type" "branch")])

;; Move instructions

(define_expand "movsi"
  [(set (match_operand:SI 0 "general_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
  "
{
  if (emit_move_sequence (operands, SImode, 0))
    DONE;
}")

(define_expand "reload_insi"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operand:SI 1 "general_operand" ""))
   (clobber (match_operand:SI 2 "register_operand" "=&r"))]
  ""
  "
{
  if (emit_move_sequence (operands, SImode, operands[2]))
    DONE;

  /* We don't want the clobber emitted, so handle this ourselves.  */
  emit_insn (gen_rtx (SET, VOIDmode, operands[0], operands[1]));
  DONE;
}")

;; We must support both 'r' and 'f' registers here, because combine may
;; convert SFmode hard registers to SImode hard registers when simplifying
;; subreg sets.

;; We cannot combine the similar 'r' and 'f' constraints, because it causes
;; problems with register allocation.  Reload might try to put an integer
;; in an fp register, or an fp number is an integer register.

(define_insn ""
  [(set (match_operand:SI 0 "reg_or_nonsymb_mem_operand" "=r,r,r,f,Q,Q,rf")
	(match_operand:SI 1 "move_operand" "rI,K,Q,!Q,rJ,!f,!fr"))]
  "register_operand (operands[0], SImode)
   || register_operand (operands[1], SImode)
   || operands[1] == const0_rtx"
  "@
   mov %1,%0
   sethi %%hi(%a1),%0
   ld %1,%0
   ld %1,%0
   st %r1,%0
   st %r1,%0
   st %r1,[%%fp-4]\;ld [%%fp-4],%0"
  [(set_attr "type" "move,move,load,load,store,store,multi")
   (set_attr "length" "*,1,*,*,*,*,*")])

;; Special pic pattern, for loading the address of a label into a register.
;; It clobbers o7 because the call puts the return address (i.e. pc value)
;; there.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operand:SI 1 "move_pic_label" "i"))
   (set (reg:SI 15) (pc))]
  ""
  "\\n1:\;call 2f\;sethi %%hi(%l1-1b),%0\\n2:\\tor %0,%%lo(%l1-1b),%0\;add %0,%%o7,%0"
  [(set_attr "type" "multi")
   (set_attr "length" "4")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(high:DI (match_operand 1 "" "")))]
  "check_pic (1)"
  "*
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];

  if (GET_CODE (op1) == CONST_INT)
    {
      operands[0] = operand_subword (op0, 1, 0, DImode);
      output_asm_insn (\"sethi %%hi(%a1),%0\", operands);

      operands[0] = operand_subword (op0, 0, 0, DImode);
      if (INTVAL (op1) < 0)
	output_asm_insn (\"mov -1,%0\", operands);
      else
	output_asm_insn (\"mov 0,%0\", operands);
    }
  else if (GET_CODE (op1) == CONST_DOUBLE)
    {
      operands[0] = operand_subword (op0, 1, 0, DImode);
      operands[1] = gen_rtx (CONST_INT, VOIDmode, CONST_DOUBLE_LOW (op1));
      output_asm_insn (\"sethi %%hi(%a1),%0\", operands);

      operands[0] = operand_subword (op0, 0, 0, DImode);
      operands[1] = gen_rtx (CONST_INT, VOIDmode, CONST_DOUBLE_HIGH (op1));
      output_asm_insn (singlemove_string (operands), operands);
    }
  else
    abort ();
}"
  [(set_attr "type" "move")
   (set_attr "length" "2")])

;; For PIC, symbol_refs are put inside unspec so that the optimizer won't
;; confuse them with real addresses.
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(high:SI (unspec:SI [(match_operand 1 "" "")] 0)))]
  "check_pic (1)"
  "sethi %%hi(%a1),%0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(high:SI (match_operand 1 "" "")))]
  "check_pic (1)"
  "sethi %%hi(%a1),%0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r")
	(high:HI (match_operand 1 "" "")))]
  "check_pic (1)"
  "sethi %%hi(%a1),%0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(lo_sum:DI (match_operand:DI 1 "register_operand" "0")
		   (match_operand:DI 2 "immediate_operand" "in")))]
  ""
  "*
{
  /* Don't output a 64 bit constant, since we can't trust the assembler to
     handle it correctly.  */
  if (GET_CODE (operands[2]) == CONST_DOUBLE)
    operands[2] = gen_rtx (CONST_INT, VOIDmode, CONST_DOUBLE_LOW (operands[2]));
  return \"or %R1,%%lo(%a2),%R0\";
}"
  ;; Need to set length for this arith insn because operand2
  ;; is not an "arith_operand".
  [(set_attr "length" "1")])

;; For PIC, symbol_refs are put inside unspec so that the optimizer won't
;; confuse them with real addresses.
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lo_sum:SI (match_operand:SI 1 "register_operand" "r")
		   (unspec:SI [(match_operand:SI 2 "immediate_operand" "in")] 0)))]
  ""
  "or %1,%%lo(%a2),%0"
  ;; Need to set length for this arith insn because operand2
  ;; is not an "arith_operand".
  [(set_attr "length" "1")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lo_sum:SI (match_operand:SI 1 "register_operand" "r")
		   (match_operand:SI 2 "immediate_operand" "in")))]
  ""
  "or %1,%%lo(%a2),%0"
  ;; Need to set length for this arith insn because operand2
  ;; is not an "arith_operand".
  [(set_attr "length" "1")])

(define_insn ""
  [(set (mem:SI (match_operand:SI 0 "symbolic_operand" ""))
	(match_operand:SI 1 "reg_or_0_operand" "rJ"))
   (clobber (match_scratch:SI 2 "=&r"))]
  ""
  "sethi %%hi(%a0),%2\;st %r1,[%2+%%lo(%a0)]"
  [(set_attr "type" "store")
   (set_attr "length" "2")])

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  "
{
  if (emit_move_sequence (operands, HImode, 0))
    DONE;
}")

(define_insn ""
  [(set (match_operand:HI 0 "reg_or_nonsymb_mem_operand" "=r,r,r,Q")
	(match_operand:HI 1 "move_operand" "rI,K,Q,rJ"))]
  "register_operand (operands[0], HImode)
   || register_operand (operands[1], HImode)
   || operands[1] == const0_rtx"
  "@
   mov %1,%0
   sethi %%hi(%a1),%0
   lduh %1,%0
   sth %r1,%0"
  [(set_attr "type" "move,move,load,store")
   (set_attr "length" "*,1,*,1")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r")
	(lo_sum:HI (match_operand:HI 1 "register_operand" "r")
		   (match_operand 2 "immediate_operand" "in")))]
  ""
  "or %1,%%lo(%a2),%0"
  [(set_attr "length" "1")])

(define_insn ""
  [(set (mem:HI (match_operand:SI 0 "symbolic_operand" ""))
	(match_operand:HI 1 "reg_or_0_operand" "rJ"))
   (clobber (match_scratch:SI 2 "=&r"))]
  ""
  "sethi %%hi(%a0),%2\;sth %r1,[%2+%%lo(%a0)]"
  [(set_attr "type" "store")
   (set_attr "length" "2")])

(define_expand "movqi"
  [(set (match_operand:QI 0 "general_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
  "
{
  if (emit_move_sequence (operands, QImode, 0))
    DONE;
}")

(define_insn ""
  [(set (match_operand:QI 0 "reg_or_nonsymb_mem_operand" "=r,r,r,Q")
	(match_operand:QI 1 "move_operand" "rI,K,Q,rJ"))]
  "register_operand (operands[0], QImode)
   || register_operand (operands[1], QImode)
   || operands[1] == const0_rtx"
  "@
   mov %1,%0
   sethi %%hi(%a1),%0
   ldub %1,%0
   stb %r1,%0"
  [(set_attr "type" "move,move,load,store")
   (set_attr "length" "*,1,*,1")])

(define_insn ""
  [(set (match_operand:QI 0 "register_operand" "=r")
	(subreg:QI (lo_sum:SI (match_operand:QI 1 "register_operand" "r")
			      (match_operand 2 "immediate_operand" "in")) 0))]
  ""
  "or %1,%%lo(%a2),%0"
  [(set_attr "length" "1")])

(define_insn ""
  [(set (mem:QI (match_operand:SI 0 "symbolic_operand" ""))
	(match_operand:QI 1 "reg_or_0_operand" "rJ"))
   (clobber (match_scratch:SI 2 "=&r"))]
  ""
  "sethi %%hi(%a0),%2\;stb %r1,[%2+%%lo(%a0)]"
  [(set_attr "type" "store")
   (set_attr "length" "2")])

;; The definition of this insn does not really explain what it does,
;; but it should suffice
;; that anything generated as this insn will be recognized as one
;; and that it will not successfully combine with anything.
(define_expand "movstrsi"
  [(parallel [(set (mem:BLK (match_operand:BLK 0 "general_operand" ""))
		   (mem:BLK (match_operand:BLK 1 "general_operand" "")))
	      (use (match_operand:SI 2 "nonmemory_operand" ""))
	      (use (match_operand:SI 3 "immediate_operand" ""))
	      (clobber (match_dup 0))
	      (clobber (match_dup 1))
	      (clobber (match_scratch:SI 4 ""))
	      (clobber (reg:SI 0))
	      (clobber (reg:SI 1))])]
  ""
  "
{
  /* If the size isn't known, don't emit inline code.  output_block_move
     would output code that's much slower than the library function.
     Also don't output code for large blocks.  */
  if (GET_CODE (operands[2]) != CONST_INT
      || GET_CODE (operands[3]) != CONST_INT
      || INTVAL (operands[2]) / INTVAL (operands[3]) > 16)
    FAIL;

  operands[0] = copy_to_mode_reg (Pmode, XEXP (operands[0], 0));
  operands[1] = copy_to_mode_reg (Pmode, XEXP (operands[1], 0));
  operands[2] = force_not_mem (operands[2]);
}")

(define_insn ""
  [(set (mem:BLK (match_operand:SI 0 "register_operand" "+r"))
	(mem:BLK (match_operand:SI 1 "register_operand" "+r")))
   (use (match_operand:SI 2 "nonmemory_operand" "rn"))
   (use (match_operand:SI 3 "immediate_operand" "i"))
   (clobber (match_dup 0))
   (clobber (match_dup 1))
   (clobber (match_scratch:SI 4 "=&r"))
   (clobber (reg:SI 0))
   (clobber (reg:SI 1))]
  ""
  "* return output_block_move (operands);"
  [(set_attr "type" "multi")
   (set_attr "length" "6")])

;; Floating point move insns

;; This pattern forces (set (reg:TF ...) (const_double ...))
;; to be reloaded by putting the constant into memory.
;; It must come before the more general movtf pattern.
(define_insn ""
  [(set (match_operand:TF 0 "general_operand" "=?r,f,o")
	(match_operand:TF 1 "" "?E,m,G"))]
  "GET_CODE (operands[1]) == CONST_DOUBLE"
  "*
{
  switch (which_alternative)
    {
    case 0:
      return output_move_quad (operands);
    case 1:
      return output_fp_move_quad (operands);
    case 2:
      operands[1] = adj_offsettable_operand (operands[0], 4);
      operands[2] = adj_offsettable_operand (operands[0], 8);
      operands[3] = adj_offsettable_operand (operands[0], 12);
      return \"st %%g0,%0\;st %%g0,%1\;st %%g0,%2\;st %%g0,%3\";
    }
}"
  [(set_attr "type" "load,fpload,store")
   (set_attr "length" "5,5,5")])

(define_expand "movtf"
  [(set (match_operand:TF 0 "general_operand" "")
	(match_operand:TF 1 "general_operand" ""))]
  ""
  "
{
  if (emit_move_sequence (operands, TFmode, 0))
    DONE;
}")

(define_insn ""
  [(set (match_operand:TF 0 "reg_or_nonsymb_mem_operand" "=f,r,Q,Q,f,&r,?f,?r")
	(match_operand:TF 1 "reg_or_nonsymb_mem_operand" "f,r,f,r,Q,Q,r,f"))]
  "register_operand (operands[0], TFmode)
   || register_operand (operands[1], TFmode)"
  "*
{
  if (FP_REG_P (operands[0]) || FP_REG_P (operands[1]))
    return output_fp_move_quad (operands);
  return output_move_quad (operands);
}"
  [(set_attr "type" "fp,move,fpstore,store,fpload,load,multi,multi")
   (set_attr "length" "4,4,5,5,5,5,5,5")])

(define_insn ""
  [(set (mem:TF (match_operand:SI 0 "symbolic_operand" "i,i"))
	(match_operand:TF 1 "reg_or_0_operand" "rf,G"))
   (clobber (match_scratch:SI 2 "=&r,&r"))]
  ""
  "*
{
  output_asm_insn (\"sethi %%hi(%a0),%2\", operands);
  if (which_alternative == 0)
    return \"std %1,[%2+%%lo(%a0)]\;std %S1,[%2+%%lo(%a0+8)]\";
  else
    return \"st %%g0,[%2+%%lo(%a0)]\;st %%g0,[%2+%%lo(%a0+4)]\; st %%g0,[%2+%%lo(%a0+8)]\;st %%g0,[%2+%%lo(%a0+12)]\";
}"
  [(set_attr "type" "store")
   (set_attr "length" "5")])

;; This pattern forces (set (reg:DF ...) (const_double ...))
;; to be reloaded by putting the constant into memory.
;; It must come before the more general movdf pattern.
(define_insn ""
  [(set (match_operand:DF 0 "general_operand" "=?r,f,o")
	(match_operand:DF 1 "" "?E,m,G"))]
  "GET_CODE (operands[1]) == CONST_DOUBLE"
  "*
{
  switch (which_alternative)
    {
    case 0:
      return output_move_double (operands);
    case 1:
      return output_fp_move_double (operands);
    case 2:
      operands[1] = adj_offsettable_operand (operands[0], 4);
      return \"st %%g0,%0\;st %%g0,%1\";
    }
}"
  [(set_attr "type" "load,fpload,store")
   (set_attr "length" "3,3,3")])

(define_expand "movdf"
  [(set (match_operand:DF 0 "general_operand" "")
	(match_operand:DF 1 "general_operand" ""))]
  ""
  "
{
  if (emit_move_sequence (operands, DFmode, 0))
    DONE;
}")

(define_insn ""
  [(set (match_operand:DF 0 "reg_or_nonsymb_mem_operand" "=T,U,f,r,Q,Q,f,&r,?f,?r")
	(match_operand:DF 1 "reg_or_nonsymb_mem_operand" "U,T,f,r,f,r,Q,Q,r,f"))]
  "register_operand (operands[0], DFmode)
   || register_operand (operands[1], DFmode)"
  "*
{
  if (FP_REG_P (operands[0]) || FP_REG_P (operands[1]))
    return output_fp_move_double (operands);
  return output_move_double (operands);
}"
  [(set_attr "type" "fpstore,fpload,fp,move,fpstore,store,fpload,load,multi,multi")
   (set_attr "length" "1,1,2,2,3,3,3,3,3,3")])

(define_insn ""
  [(set (mem:DF (match_operand:SI 0 "symbolic_operand" "i,i"))
	(match_operand:DF 1 "reg_or_0_operand" "rf,G"))
   (clobber (match_scratch:SI 2 "=&r,&r"))]
  ""
  "*
{
  output_asm_insn (\"sethi %%hi(%a0),%2\", operands);
  if (which_alternative == 0)
    return \"std %1,[%2+%%lo(%a0)]\";
  else
    return \"st %%g0,[%2+%%lo(%a0)]\;st %%g0,[%2+%%lo(%a0+4)]\";
}"
  [(set_attr "type" "store")
   (set_attr "length" "3")])

;; Double-word move insns.

(define_expand "movdi"
  [(set (match_operand:DI 0 "reg_or_nonsymb_mem_operand" "")
	(match_operand:DI 1 "general_operand" ""))]
  ""
  "
{
  if (emit_move_sequence (operands, DImode, 0))
    DONE;
}")

(define_insn ""
  [(set (match_operand:DI 0 "reg_or_nonsymb_mem_operand" "=r,Q,&r,&r,?f,?f,?f,?r,?Q")
	(match_operand:DI 1 "general_operand" "r,r,Q,i,r,f,Q,f,f"))]
  "register_operand (operands[0], DImode)
   || register_operand (operands[1], DImode)
   || operands[1] == const0_rtx"
  "*
{
  if (FP_REG_P (operands[0]) || FP_REG_P (operands[1]))
    return output_fp_move_double (operands);
  return output_move_double (operands);
}"
  [(set_attr "type" "move,store,load,multi,multi,fp,fpload,multi,fpstore")
   (set_attr "length" "2,3,3,3,3,2,3,3,3")])

;; Floating-point move insns.

;; This pattern forces (set (reg:SF ...) (const_double ...))
;; to be reloaded by putting the constant into memory.
;; It must come before the more general movsf pattern.
(define_insn ""
  [(set (match_operand:SF 0 "general_operand" "=?r,f,m")
	(match_operand:SF 1 "" "?E,m,G"))]
  "GET_CODE (operands[1]) == CONST_DOUBLE"
  "*
{
  switch (which_alternative)
    {
    case 0:
      return singlemove_string (operands);
    case 1:
      return \"ld %1,%0\";
    case 2:
      return \"st %%g0,%0\";
    }
}"
  [(set_attr "type" "load,fpload,store")
   (set_attr "length" "2,1,1")])

(define_expand "movsf"
  [(set (match_operand:SF 0 "general_operand" "")
	(match_operand:SF 1 "general_operand" ""))]
  ""
  "
{
  if (emit_move_sequence (operands, SFmode, 0))
    DONE;
}")

(define_insn ""
  [(set (match_operand:SF 0 "reg_or_nonsymb_mem_operand" "=f,r,rf,f,r,Q,Q")
	(match_operand:SF 1 "reg_or_nonsymb_mem_operand" "f,r,!rf,Q,Q,f,r"))]
  "register_operand (operands[0], SFmode)
   || register_operand (operands[1], SFmode)"
  "@
   fmovs %1,%0
   mov %1,%0
   st %r1,[%%fp-4]\;ld [%%fp-4],%0
   ld %1,%0
   ld %1,%0
   st %r1,%0
   st %r1,%0"
  [(set_attr "type" "fp,move,multi,fpload,load,fpstore,store")])

(define_insn ""
  [(set (mem:SF (match_operand:SI 0 "symbolic_operand" "i"))
	(match_operand:SF 1 "reg_or_0_operand" "rfG"))
   (clobber (match_scratch:SI 2 "=&r"))]
  ""
  "sethi %%hi(%a0),%2\;st %r1,[%2+%%lo(%a0)]"
  [(set_attr "type" "store")
   (set_attr "length" "2")])

;;- zero extension instructions

;; These patterns originally accepted general_operands, however, slightly
;; better code is generated by only accepting register_operands, and then
;; letting combine generate the ldu[hb] insns.

(define_expand "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(zero_extend:SI (match_operand:HI 1 "register_operand" "")))]
  ""
  "
{
  rtx temp = gen_reg_rtx (SImode);
  rtx shift_16 = gen_rtx (CONST_INT, VOIDmode, 16);

  if (GET_CODE (operand1) == SUBREG)
    operand1 = XEXP (operand1, 0);

  emit_insn (gen_ashlsi3 (temp, gen_rtx (SUBREG, SImode, operand1, 0),
			  shift_16));
  emit_insn (gen_lshrsi3 (operand0, temp, shift_16));
  DONE;
}")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (match_operand:HI 1 "memory_operand" "m")))]
  ""
  "lduh %1,%0"
  [(set_attr "type" "load")])

(define_expand "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "")
	(zero_extend:HI (match_operand:QI 1 "register_operand" "")))]
  ""
  "")

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r,r,r")
	(zero_extend:HI (match_operand:QI 1 "sparc_operand" "r,I,Q")))]
  "GET_CODE (operands[1]) != CONST_INT"
  "@
   and %1,0xff,%0;
   mov (%1 & 0xff),%0
   ldub %1,%0"
  [(set_attr "type" "unary,move,load")
   (set_attr "length" "1")])

(define_expand "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(zero_extend:SI (match_operand:QI 1 "register_operand" "")))]
  ""
  "")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r,r,r")
	(zero_extend:SI (match_operand:QI 1 "sparc_operand" "r,I,Q")))]
  "GET_CODE (operands[1]) != CONST_INT"
  "@
   and %1,0xff,%0
   mov (%1 & 0xff),%0
   ldub %1,%0"
  [(set_attr "type" "unary,move,load")
   (set_attr "length" "1")])

(define_insn ""
  [(set (reg:CC 0)
	(compare:CC (zero_extend:SI (match_operand:QI 0 "register_operand" "r"))
		    (const_int 0)))]
  ""
  "andcc %0,0xff,%%g0"
  [(set_attr "type" "compare")])

(define_insn ""
  [(set (reg:CC 0)
	(compare:CC (zero_extend:SI (match_operand:QI 1 "register_operand" "r"))
		    (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (match_dup 1)))]
  ""
  "andcc %1,0xff,%0"
  [(set_attr "type" "unary")])

;;- sign extension instructions

;; These patterns originally accepted general_operands, however, slightly
;; better code is generated by only accepting register_operands, and then
;; letting combine generate the lds[hb] insns.

(define_expand "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(sign_extend:SI (match_operand:HI 1 "register_operand" "")))]
  ""
  "
{
  rtx temp = gen_reg_rtx (SImode);
  rtx shift_16 = gen_rtx (CONST_INT, VOIDmode, 16);

  if (GET_CODE (operand1) == SUBREG)
    operand1 = XEXP (operand1, 0);

  emit_insn (gen_ashlsi3 (temp, gen_rtx (SUBREG, SImode, operand1, 0),
			  shift_16));
  emit_insn (gen_ashrsi3 (operand0, temp, shift_16));
  DONE;
}")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:HI 1 "memory_operand" "m")))]
  ""
  "ldsh %1,%0"
  [(set_attr "type" "load")])

(define_expand "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "")
	(sign_extend:HI (match_operand:QI 1 "register_operand" "")))]
  ""
  "
{
  rtx temp = gen_reg_rtx (SImode);
  rtx shift_24 = gen_rtx (CONST_INT, VOIDmode, 24);

  if (GET_CODE (operand1) == SUBREG)
    operand1 = XEXP (operand1, 0);
  if (GET_CODE (operand0) == SUBREG)
    operand0 = XEXP (operand0, 0);
  emit_insn (gen_ashlsi3 (temp, gen_rtx (SUBREG, SImode, operand1, 0),
			  shift_24));
  if (GET_MODE (operand0) != SImode)
    operand0 = gen_rtx (SUBREG, SImode, operand0, 0);
  emit_insn (gen_ashrsi3 (operand0, temp, shift_24));
  DONE;
}")

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r")
	(sign_extend:HI (match_operand:QI 1 "memory_operand" "m")))]
  ""
  "ldsb %1,%0"
  [(set_attr "type" "load")])

(define_expand "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(sign_extend:SI (match_operand:QI 1 "register_operand" "")))]
  ""
  "
{
  rtx temp = gen_reg_rtx (SImode);
  rtx shift_24 = gen_rtx (CONST_INT, VOIDmode, 24);

  if (GET_CODE (operand1) == SUBREG)
    operand1 = XEXP (operand1, 0);
  emit_insn (gen_ashlsi3 (temp, gen_rtx (SUBREG, SImode, operand1, 0),
			  shift_24));
  emit_insn (gen_ashrsi3 (operand0, temp, shift_24));
  DONE;
}")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:QI 1 "memory_operand" "m")))]
  ""
  "ldsb %1,%0"
  [(set_attr "type" "load")])

;; Special pattern for optimizing bit-field compares.  This is needed
;; because combine uses this as a canonical form.

(define_insn ""
  [(set (reg:CC 0)
	(compare:CC
	 (zero_extract:SI (match_operand:SI 0 "register_operand" "r")
			  (match_operand:SI 1 "small_int" "n")
			  (match_operand:SI 2 "small_int" "n"))
	 (const_int 0)))]
  "INTVAL (operands[2]) > 19"
  "*
{
  int len = INTVAL (operands[1]);
  int pos = 32 - INTVAL (operands[2]) - len;
  unsigned mask = ((1 << len) - 1) << pos;

  operands[1] = gen_rtx (CONST_INT, VOIDmode, mask);
  return \"andcc %0,%1,%%g0\";
}")

;; Conversions between float, double and long double.

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float_extend:DF
	 (match_operand:SF 1 "register_operand" "f")))]
  ""
  "fstod %1,%0"
  [(set_attr "type" "fp")])

(define_insn "extendsftf2"
  [(set (match_operand:TF 0 "register_operand" "=f")
	(float_extend:TF
	 (match_operand:SF 1 "register_operand" "f")))]
  ""
  "fstoq %1,%0"
  [(set_attr "type" "fp")])

(define_insn "extenddftf2"
  [(set (match_operand:TF 0 "register_operand" "=f")
	(float_extend:TF
	 (match_operand:DF 1 "register_operand" "f")))]
  ""
  "fdtoq %1,%0"
  [(set_attr "type" "fp")])

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float_truncate:SF
	 (match_operand:DF 1 "register_operand" "f")))]
  ""
  "fdtos %1,%0"
  [(set_attr "type" "fp")])

(define_insn "trunctfsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float_truncate:SF
	 (match_operand:TF 1 "register_operand" "f")))]
  ""
  "fqtos %1,%0"
  [(set_attr "type" "fp")])

(define_insn "trunctfdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float_truncate:DF
	 (match_operand:TF 1 "register_operand" "f")))]
  ""
  "fqtod %1,%0"
  [(set_attr "type" "fp")])

;; Conversion between fixed point and floating point.

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float:SF (match_operand:SI 1 "register_operand" "f")))]
  ""
  "fitos %1,%0"
  [(set_attr "type" "fp")])

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float:DF (match_operand:SI 1 "register_operand" "f")))]
  ""
  "fitod %1,%0"
  [(set_attr "type" "fp")])

(define_insn "floatsitf2"
  [(set (match_operand:TF 0 "register_operand" "=f")
	(float:TF (match_operand:SI 1 "register_operand" "f")))]
  ""
  "fitox %1,%0"
  [(set_attr "type" "fp")])

;; Convert a float to an actual integer.
;; Truncation is performed as part of the conversion.

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (fix:SF (match_operand:SF 1 "register_operand" "f"))))]
  ""
  "fstoi %1,%0"
  [(set_attr "type" "fp")])

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (fix:DF (match_operand:DF 1 "register_operand" "f"))))]
  ""
  "fdtoi %1,%0"
  [(set_attr "type" "fp")])

(define_insn "fix_trunctfsi2"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (fix:TF (match_operand:TF 1 "register_operand" "f"))))]
  ""
  "fqtoi %1,%0"
  [(set_attr "type" "fp")])

;;- arithmetic instructions

(define_insn "adddi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI (match_operand:DI 1 "arith_double_operand" "%r")
		 (match_operand:DI 2 "arith_double_operand" "rHI")))
   (clobber (reg:SI 0))]
  ""
  "*
{
  rtx op2 = operands[2];

  /* If constant is positive, upper bits zeroed, otherwise unchanged.
     Give the assembler a chance to pick the move instruction. */
  if (GET_CODE (op2) == CONST_INT)
    {
      int sign = INTVAL (op2);
      if (sign < 0)
	return \"addcc %R1,%2,%R0\;addx %1,-1,%0\";
      return \"addcc %R1,%2,%R0\;addx %1,0,%0\";
    }
  else if (GET_CODE (op2) == CONST_DOUBLE)
    {
      int sign = CONST_DOUBLE_HIGH (op2);
      operands[2] = gen_rtx (CONST_INT, VOIDmode,
			     CONST_DOUBLE_LOW (operands[1]));
      if (sign < 0)
        return \"addcc %R1,%2,%R0\;addx %1,-1,%0\";
      return \"addcc %R1,%2,%R0\;addx %1,0,%0\";
    }
  return \"addcc %R1,%R2,%R0\;addx %1,%2,%0\";
}"
  [(set_attr "length" "2")])

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_operand:SI 1 "arith_operand" "%r")
		 (match_operand:SI 2 "arith_operand" "rI")))]
  ""
  "add %1,%2,%0")

(define_insn ""
  [(set (reg:CC_NOOV 0)
	(compare:CC_NOOV (plus:SI (match_operand:SI 0 "arith_operand" "%r")
				  (match_operand:SI 1 "arith_operand" "rI"))
			 (const_int 0)))]
  ""
  "addcc %0,%1,%%g0"
  [(set_attr "type" "compare")])

(define_insn ""
  [(set (reg:CC_NOOV 0)
	(compare:CC_NOOV (plus:SI (match_operand:SI 1 "arith_operand" "%r")
				  (match_operand:SI 2 "arith_operand" "rI"))
			 (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "addcc %1,%2,%0")

(define_insn "subdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(minus:DI (match_operand:DI 1 "register_operand" "r")
		  (match_operand:DI 2 "arith_double_operand" "rHI")))
   (clobber (reg:SI 0))]
  ""
  "*
{
  rtx op2 = operands[2];

  /* If constant is positive, upper bits zeroed, otherwise unchanged.
     Give the assembler a chance to pick the move instruction. */
  if (GET_CODE (op2) == CONST_INT)
    {
      int sign = INTVAL (op2);
      if (sign < 0)
	return \"subcc %R1,%2,%R0\;subx %1,-1,%0\";
      return \"subcc %R1,%2,%R0\;subx %1,0,%0\";
    }
  else if (GET_CODE (op2) == CONST_DOUBLE)
    {
      int sign = CONST_DOUBLE_HIGH (op2);
      operands[2] = gen_rtx (CONST_INT, VOIDmode,
			     CONST_DOUBLE_LOW (operands[1]));
      if (sign < 0)
        return \"subcc %R1,%2,%R0\;subx %1,-1,%0\";
      return \"subcc %R1,%2,%R0\;subx %1,0,%0\";
    }
  return \"subcc %R1,%R2,%R0\;subx %1,%2,%0\";
}"
  [(set_attr "length" "2")])

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 1 "register_operand" "r")
		  (match_operand:SI 2 "arith_operand" "rI")))]
  ""
  "sub %1,%2,%0")

(define_insn ""
  [(set (reg:CC_NOOV 0)
	(compare:CC_NOOV (minus:SI (match_operand:SI 0 "register_operand" "r")
				   (match_operand:SI 1 "arith_operand" "rI"))
			 (const_int 0)))]
  ""
  "subcc %0,%1,%%g0"
  [(set_attr "type" "compare")])

(define_insn ""
  [(set (reg:CC_NOOV 0)
	(compare:CC_NOOV (minus:SI (match_operand:SI 1 "register_operand" "r")
				   (match_operand:SI 2 "arith_operand" "rI"))
			 (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_dup 1) (match_dup 2)))]
  ""
  "subcc %1,%2,%0")

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mult:SI (match_operand:SI 1 "arith_operand" "%r")
		 (match_operand:SI 2 "arith_operand" "rI")))]
  "TARGET_V8 || TARGET_SPARCLITE"
  "smul %1,%2,%0")

;; It is not known whether this will match.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mult:SI (match_operand:SI 1 "arith_operand" "%r")
		 (match_operand:SI 2 "arith_operand" "rI")))
   (set (reg:CC_NOOV 0)
	(compare:CC_NOOV (mult:SI (match_dup 1) (match_dup 2))
			 (const_int 0)))]
  "TARGET_V8 || TARGET_SPARCLITE"
  "smulcc %1,%2,%0")

(define_insn "mulsidi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "arith_operand" "%r"))
		 (sign_extend:DI (match_operand:SI 2 "arith_operand" "rI"))))]
  "TARGET_V8 || TARGET_SPARCLITE"
  "smul %1,%2,%R0\;rd %y,%0"
  [(set_attr "length" "2")])

(define_insn "umulsidi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "arith_operand" "%r"))
		 (zero_extend:DI (match_operand:SI 2 "arith_operand" "rI"))))]
  "TARGET_V8 || TARGET_SPARCLITE"
  "umul %1,%2,%R0\;rd %y,%0"
  [(set_attr "length" "2")])

;; The architecture specifies that there must be 3 instructions between
;; a y register write and a use of it for correct results.

(define_insn "divsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(div:SI (match_operand:SI 1 "register_operand" "r")
		(match_operand:SI 2 "arith_operand" "rI")))
   (clobber (match_scratch:SI 3 "=&r"))]
  "TARGET_V8"
  "sra %1,31,%3\;wr %%g0,%3,%%y\;nop\;nop\;nop\;sdiv %1,%2,%0"
  [(set_attr "length" "3")])

;; It is not known whether this will match.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(div:SI (match_operand:SI 1 "register_operand" "r")
		(match_operand:SI 2 "arith_operand" "rI")))
   (set (reg:CC 0)
	(compare:CC (div:SI (match_dup 1) (match_dup 2))
		    (const_int 0)))
   (clobber (match_scratch:SI 3 "=&r"))]
  "TARGET_V8"
  "sra %1,31,%3\;wr %%g0,%3,%%y\;nop\;nop\;nop\;sdivcc %1,%2,%0"
  [(set_attr "length" "3")])

(define_insn "udivsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(udiv:SI (match_operand:SI 1 "register_operand" "r")
		(match_operand:SI 2 "arith_operand" "rI")))]
  "TARGET_V8"
  "wr %%g0,%%g0,%%y\;nop\;nop\;nop\;udiv %1,%2,%0"
  [(set_attr "length" "2")])

;; It is not known whether this will match.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(udiv:SI (match_operand:SI 1 "register_operand" "r")
		(match_operand:SI 2 "arith_operand" "rI")))
   (set (reg:CC 0)
	(compare:CC (udiv:SI (match_dup 1) (match_dup 2))
		    (const_int 0)))]
  "TARGET_V8"
  "wr %%g0,%%g0,%%y\;nop\;nop\;nop\;udivcc %1,%2,%0"
  [(set_attr "length" "2")])

;;- and instructions
;; We define DImode `and` so with DImode `not` we can get
;; DImode `andn`.  Other combinations are possible.

(define_expand "anddi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(and:DI (match_operand:DI 1 "arith_double_operand" "")
		(match_operand:DI 2 "arith_double_operand" "")))]
  ""
  "")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(and:DI (match_operand:DI 1 "arith_double_operand" "%r")
		(match_operand:DI 2 "arith_double_operand" "rHI")))]
  ""
  "*
{
  rtx op2 = operands[2];

  /* If constant is positive, upper bits zeroed, otherwise unchanged.
     Give the assembler a chance to pick the move instruction. */
  if (GET_CODE (op2) == CONST_INT)
    {
      int sign = INTVAL (op2);
      if (sign < 0)
	return \"mov %1,%0\;and %R1,%2,%R0\";
      return \"mov 0,%0\;and %R1,%2,%R0\";
    }
  else if (GET_CODE (op2) == CONST_DOUBLE)
    {
      int sign = CONST_DOUBLE_HIGH (op2);
      operands[2] = gen_rtx (CONST_INT, VOIDmode,
			     CONST_DOUBLE_LOW (operands[1]));
      if (sign < 0)
	return \"mov %1,%0\;and %R1,%2,%R0\";
      return \"mov 0,%0\;and %R1,%2,%R0\";
    }
  return \"and %1,%2,%0\;and %R1,%R2,%R0\";
}"
  [(set_attr "length" "2")])

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (match_operand:SI 1 "arith_operand" "%r")
		(match_operand:SI 2 "arith_operand" "rI")))]
  ""
  "and %1,%2,%0")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(and:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "GET_CODE (operands[2]) == CONST_INT
   && !SMALL_INT (operands[2])
   && (INTVAL (operands[2]) & 0x3ff) == 0x3ff"
  [(set (match_dup 3) (match_dup 4))
   (set (match_dup 0) (and:SI (not:SI (match_dup 3)) (match_dup 1)))]
  "
{
  operands[4] = gen_rtx (CONST_INT, VOIDmode, ~INTVAL (operands[2]));
}")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(and:DI (not:DI (match_operand:DI 1 "register_operand" "r"))
		(match_operand:DI 2 "register_operand" "r")))]
  ""
  "andn %2,%1,%0\;andn %R2,%R1,%R0"
  [(set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (not:SI (match_operand:SI 1 "register_operand" "r"))
		(match_operand:SI 2 "register_operand" "r")))]
  ""
  "andn %2,%1,%0")

(define_expand "iordi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(ior:DI (match_operand:DI 1 "arith_double_operand" "")
		(match_operand:DI 2 "arith_double_operand" "")))]
  ""
  "")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ior:DI (match_operand:DI 1 "arith_double_operand" "%r")
		(match_operand:DI 2 "arith_double_operand" "rHI")))]
  ""
  "*
{
  rtx op2 = operands[2];

  /* If constant is positive, upper bits zeroed, otherwise unchanged.
     Give the assembler a chance to pick the move instruction. */
  if (GET_CODE (op2) == CONST_INT)
    {
      int sign = INTVAL (op2);
      if (sign < 0)
	return \"mov -1,%0\;or %R1,%2,%R0\";
      return \"mov %1,%0\;or %R1,%2,%R0\";
    }
  else if (GET_CODE (op2) == CONST_DOUBLE)
    {
      int sign = CONST_DOUBLE_HIGH (op2);
      operands[2] = gen_rtx (CONST_INT, VOIDmode,
			     CONST_DOUBLE_LOW (operands[1]));
      if (sign < 0)
	return \"mov -1,%0\;or %R1,%2,%R0\";
      return \"mov %1,%0\;or %R1,%2,%R0\";
    }
  return \"or %1,%2,%0\;or %R1,%R2,%R0\";
}"
  [(set_attr "length" "2")])

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (match_operand:SI 1 "arith_operand" "%r")
		(match_operand:SI 2 "arith_operand" "rI")))]
  ""
  "or %1,%2,%0")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(ior:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "GET_CODE (operands[2]) == CONST_INT
   && !SMALL_INT (operands[2])
   && (INTVAL (operands[2]) & 0x3ff) == 0x3ff"
  [(set (match_dup 3) (match_dup 4))
   (set (match_dup 0) (ior:SI (not:SI (match_dup 3)) (match_dup 1)))]
  "
{
  operands[4] = gen_rtx (CONST_INT, VOIDmode, ~INTVAL (operands[2]));
}")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ior:DI (not:DI (match_operand:DI 1 "register_operand" "r"))
		(match_operand:DI 2 "register_operand" "r")))]
  ""
  "orn %2,%1,%0\;orn %R2,%R1,%R0"
  [(set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (not:SI (match_operand:SI 1 "register_operand" "r"))
		(match_operand:SI 2 "register_operand" "r")))]
  ""
  "orn %2,%1,%0")

(define_expand "xordi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(xor:DI (match_operand:DI 1 "arith_double_operand" "")
		(match_operand:DI 2 "arith_double_operand" "")))]
  ""
  "")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(xor:DI (match_operand:DI 1 "arith_double_operand" "%r")
		(match_operand:DI 2 "arith_double_operand" "rHI")))]
  ""
  "*
{
  rtx op2 = operands[2];

  /* If constant is positive, upper bits zeroed, otherwise unchanged.
     Give the assembler a chance to pick the move instruction. */
  if (GET_CODE (op2) == CONST_INT)
    {
      int sign = INTVAL (op2);
      if (sign < 0)
	return \"xor %1,-1,%0\;xor %R1,%2,%R0\";
      return \"mov %1,%0\;xor %R1,%2,%R0\";
    }
  else if (GET_CODE (op2) == CONST_DOUBLE)
    {
      int sign = CONST_DOUBLE_HIGH (op2);
      operands[2] = gen_rtx (CONST_INT, VOIDmode,
			     CONST_DOUBLE_LOW (operands[1]));
      if (sign < 0)
	return \"xor %1,-1,%0\;xor %R1,%2,%R0\";
      return \"mov %1,%0\;xor %R1,%2,%R0\";
    }
  return \"xor %1,%2,%0\;xor %R1,%R2,%R0\";
}"
  [(set_attr "length" "2")])

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(xor:SI (match_operand:SI 1 "arith_operand" "%rJ")
		(match_operand:SI 2 "arith_operand" "rI")))]
  ""
  "xor %r1,%2,%0")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(xor:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "GET_CODE (operands[2]) == CONST_INT
   && !SMALL_INT (operands[2])
   && (INTVAL (operands[2]) & 0x3ff) == 0x3ff"
  [(set (match_dup 3) (match_dup 4))
   (set (match_dup 0) (not:SI (xor:SI (match_dup 3) (match_dup 1))))]
  "
{
  operands[4] = gen_rtx (CONST_INT, VOIDmode, ~INTVAL (operands[2]));
}")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(not:SI (xor:SI (match_operand:SI 1 "register_operand" "")
			(match_operand:SI 2 "" ""))))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "GET_CODE (operands[2]) == CONST_INT
   && !SMALL_INT (operands[2])
   && (INTVAL (operands[2]) & 0x3ff) == 0x3ff"
  [(set (match_dup 3) (match_dup 4))
   (set (match_dup 0) (xor:SI (match_dup 3) (match_dup 1)))]
  "
{
  operands[4] = gen_rtx (CONST_INT, VOIDmode, ~INTVAL (operands[2]));
}")

;; xnor patterns.  Note that (a ^ ~b) == (~a ^ b) == ~(a ^ b).
;; Combine now canonicalizes to the rightmost expression.
(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(not:DI (xor:DI (match_operand:DI 1 "register_operand" "r")
			(match_operand:DI 2 "register_operand" "r"))))]
  ""
  "xnor %1,%2,%0\;xnor %R1,%R2,%R0"
  [(set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(not:SI (xor:SI (match_operand:SI 1 "reg_or_0_operand" "rJ")
			(match_operand:SI 2 "arith_operand" "rI"))))]
  ""
  "xnor %r1,%2,%0")

;; These correspond to the above in the case where we also (or only)
;; want to set the condition code.  

(define_insn ""
  [(set (reg:CC 0)
	(compare:CC
	 (match_operator:SI 2 "cc_arithop"
			    [(match_operand:SI 0 "arith_operand" "%r")
			     (match_operand:SI 1 "arith_operand" "rI")])
	 (const_int 0)))]
  ""
  "%A2cc %0,%1,%%g0"
  [(set_attr "type" "compare")])

(define_insn ""
  [(set (reg:CC 0)
	(compare:CC
	 (match_operator:SI 3 "cc_arithop"
			    [(match_operand:SI 1 "arith_operand" "%r")
			     (match_operand:SI 2 "arith_operand" "rI")])
	 (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(match_dup 3))]
  ""
  "%A3cc %1,%2,%0")

(define_insn ""
  [(set (reg:CC 0)
	(compare:CC
	 (not:SI (xor:SI (match_operand:SI 0 "reg_or_0_operand" "%rJ")
			 (match_operand:SI 1 "arith_operand" "rI")))
	 (const_int 0)))]
  ""
  "xnorcc %r0,%1,%%g0"
  [(set_attr "type" "compare")])

(define_insn ""
  [(set (reg:CC 0)
	(compare:CC
	 (not:SI (xor:SI (match_operand:SI 1 "reg_or_0_operand" "%rJ")
			 (match_operand:SI 2 "arith_operand" "rI")))
	 (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(not:SI (xor:SI (match_dup 1) (match_dup 2))))]
  ""
  "xnorcc %r1,%2,%0")

(define_insn ""
  [(set (reg:CC 0)
	(compare:CC
	 (match_operator:SI 2 "cc_arithopn"
			    [(not:SI (match_operand:SI 0 "arith_operand" "rI"))
			     (match_operand:SI 1 "reg_or_0_operand" "rJ")])
	 (const_int 0)))]
  ""
  "%B2cc %r1,%0,%%g0"
  [(set_attr "type" "compare")])

(define_insn ""
  [(set (reg:CC 0)
	(compare:CC
	 (match_operator:SI 3 "cc_arithopn"
			    [(not:SI (match_operand:SI 1 "arith_operand" "rI"))
			     (match_operand:SI 2 "reg_or_0_operand" "rJ")])
	 (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(match_dup 3))]
  ""
  "%B3cc %r2,%1,%0")

;; We cannot use the "neg" pseudo insn because the Sun assembler
;; does not know how to make it work for constants.

(define_insn "negdi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(neg:DI (match_operand:DI 1 "register_operand" "r")))
   (clobber (reg:SI 0))]
  ""
  "subcc %%g0,%R1,%R0\;subx %%g0,%1,%0"
  [(set_attr "type" "unary")
   (set_attr "length" "2")])

(define_insn "negsi2"
  [(set (match_operand:SI 0 "general_operand" "=r")
	(neg:SI (match_operand:SI 1 "arith_operand" "rI")))]
  ""
  "sub %%g0,%1,%0"
  [(set_attr "type" "unary")])

(define_insn ""
  [(set (reg:CC_NOOV 0)
	(compare:CC_NOOV (neg:SI (match_operand:SI 0 "arith_operand" "rI"))
			 (const_int 0)))]
  ""
  "subcc %%g0,%0,%%g0"
  [(set_attr "type" "compare")])

(define_insn ""
  [(set (reg:CC_NOOV 0)
	(compare:CC_NOOV (neg:SI (match_operand:SI 1 "arith_operand" "rI"))
			 (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (match_dup 1)))]
  ""
  "subcc %%g0,%1,%0"
  [(set_attr "type" "unary")])

;; We cannot use the "not" pseudo insn because the Sun assembler
;; does not know how to make it work for constants.
(define_expand "one_cmpldi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(not:DI (match_operand:DI 1 "arith_double_operand" "rHI")))]
  ""
  "")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(not:DI (match_operand:DI 1 "arith_double_operand" "rHI")))]
  ""
  "*
{
  rtx op1 = operands[1];

  if (GET_CODE (op1) == CONST_INT)
    {
      int sign = INTVAL (op1);
      if (sign < 0)
	return \"xnor %%g0,%1,%R0\;xnor %%g0,-1,%0\";
      return \"xnor %%g0,%1,%R0\;xnor %%g0,0,%0\";
    }
  else if (GET_CODE (op1) == CONST_DOUBLE)
    {
      int sign = CONST_DOUBLE_HIGH (op1);
      operands[1] = gen_rtx (CONST_INT, VOIDmode,
			     CONST_DOUBLE_LOW (operands[1]));
      if (sign < 0)
	return \"xnor %%g0,%1,%R0\;xnor %%g0,-1,%0\";
      return \"xnor %%g0,%1,%R0\;xnor %%g0,0,%0\";
    }
  return \"xnor %%g0,%1,%0\;xnor %%g0,%R1,%R0\";
}"
  [(set_attr "type" "unary")
   (set_attr "length" "2")])

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(not:SI (match_operand:SI 1 "arith_operand" "rI")))]
  ""
  "xnor %%g0,%1,%0"
  [(set_attr "type" "unary")])

(define_insn ""
  [(set (reg:CC 0)
	(compare:CC (not:SI (match_operand:SI 0 "arith_operand" "rI"))
		    (const_int 0)))]
  ""
  "xnorcc %%g0,%0,%%g0"
  [(set_attr "type" "compare")])

(define_insn ""
  [(set (reg:CC 0)
	(compare:CC (not:SI (match_operand:SI 1 "arith_operand" "rI"))
		    (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(not:SI (match_dup 1)))]
  ""
  "xnorcc %%g0,%1,%0"
  [(set_attr "type" "unary")])

;; Floating point arithmetic instructions.

(define_insn "addtf3"
  [(set (match_operand:TF 0 "register_operand" "=f")
	(plus:TF (match_operand:TF 1 "register_operand" "f")
		 (match_operand:TF 2 "register_operand" "f")))]
  ""
  "faddq %1,%2,%0"
  [(set_attr "type" "fp")])

(define_insn "adddf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(plus:DF (match_operand:DF 1 "register_operand" "f")
		 (match_operand:DF 2 "register_operand" "f")))]
  ""
  "faddd %1,%2,%0"
  [(set_attr "type" "fp")])

(define_insn "addsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(plus:SF (match_operand:SF 1 "register_operand" "f")
		 (match_operand:SF 2 "register_operand" "f")))]
  ""
  "fadds %1,%2,%0"
  [(set_attr "type" "fp")])

(define_insn "subtf3"
  [(set (match_operand:TF 0 "register_operand" "=f")
	(minus:TF (match_operand:TF 1 "register_operand" "f")
		  (match_operand:TF 2 "register_operand" "f")))]
  ""
  "fsubq %1,%2,%0"
  [(set_attr "type" "fp")])

(define_insn "subdf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(minus:DF (match_operand:DF 1 "register_operand" "f")
		  (match_operand:DF 2 "register_operand" "f")))]
  ""
  "fsubd %1,%2,%0"
  [(set_attr "type" "fp")])

(define_insn "subsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(minus:SF (match_operand:SF 1 "register_operand" "f")
		  (match_operand:SF 2 "register_operand" "f")))]
  ""
  "fsubs %1,%2,%0"
  [(set_attr "type" "fp")])

(define_insn "multf3"
  [(set (match_operand:TF 0 "register_operand" "=f")
	(mult:TF (match_operand:TF 1 "register_operand" "f")
		 (match_operand:TF 2 "register_operand" "f")))]
  ""
  "fmulq %1,%2,%0"
  [(set_attr "type" "fpmul")])

(define_insn "muldf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(mult:DF (match_operand:DF 1 "register_operand" "f")
		 (match_operand:DF 2 "register_operand" "f")))]
  ""
  "fmuld %1,%2,%0"
  [(set_attr "type" "fpmul")])

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(mult:SF (match_operand:SF 1 "register_operand" "f")
		 (match_operand:SF 2 "register_operand" "f")))]
  ""
  "fmuls %1,%2,%0"
  [(set_attr "type" "fpmul")])

(define_insn "divtf3"
  [(set (match_operand:TF 0 "register_operand" "=f")
	(div:TF (match_operand:TF 1 "register_operand" "f")
		(match_operand:TF 2 "register_operand" "f")))]
  ""
  "fdivq %1,%2,%0"
  [(set_attr "type" "fpdiv")])

(define_insn "divdf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(div:DF (match_operand:DF 1 "register_operand" "f")
		(match_operand:DF 2 "register_operand" "f")))]
  ""
  "fdivd %1,%2,%0"
  [(set_attr "type" "fpdiv")])

(define_insn "divsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(div:SF (match_operand:SF 1 "register_operand" "f")
		(match_operand:SF 2 "register_operand" "f")))]
  ""
  "fdivs %1,%2,%0"
  [(set_attr "type" "fpdiv")])

(define_insn "negtf2"
  [(set (match_operand:TF 0 "register_operand" "=f,f")
	(neg:TF (match_operand:TF 1 "register_operand" "0,f")))]
  ""
  "@
   fnegs %0,%0
   fnegs %1,%0\;fmovs %R1,%R0\;fmovs %S1,%S0\;fmovs %T1,%T0"
  [(set_attr "type" "fp")
   (set_attr "length" "1,4")])

(define_insn "negdf2"
  [(set (match_operand:DF 0 "register_operand" "=f,f")
	(neg:DF (match_operand:DF 1 "register_operand" "0,f")))]
  ""
  "@
   fnegs %0,%0
   fnegs %1,%0\;fmovs %R1,%R0"
  [(set_attr "type" "fp")
   (set_attr "length" "1,2")])

(define_insn "negsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(neg:SF (match_operand:SF 1 "register_operand" "f")))]
  ""
  "fnegs %1,%0"
  [(set_attr "type" "fp")])

(define_insn "abstf2"
  [(set (match_operand:TF 0 "register_operand" "=f,f")
	(abs:TF (match_operand:TF 1 "register_operand" "0,f")))]
  ""
  "@
   fabss %0,%0
   fabss %1,%0\;fmovs %R1,%R0\;fmovs %S1,%S0\;fmovs %T1,%T0"
  [(set_attr "type" "fp")
   (set_attr "length" "1,4")])

(define_insn "absdf2"
  [(set (match_operand:DF 0 "register_operand" "=f,f")
	(abs:DF (match_operand:DF 1 "register_operand" "0,f")))]
  ""
  "@
   fabss %0,%0
   fabss %1,%0\;fmovs %R1,%R0"
  [(set_attr "type" "fp")
   (set_attr "length" "1,2")])

(define_insn "abssf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(abs:SF (match_operand:SF 1 "register_operand" "f")))]
  ""
  "fabss %1,%0"
  [(set_attr "type" "fp")])

(define_insn "sqrttf2"
  [(set (match_operand:TF 0 "register_operand" "=f")
	(sqrt:TF (match_operand:TF 1 "register_operand" "f")))]
  ""
  "fsqrtq %1,%0"
  [(set_attr "type" "fpsqrt")])

(define_insn "sqrtdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(sqrt:DF (match_operand:DF 1 "register_operand" "f")))]
  ""
  "fsqrtd %1,%0"
  [(set_attr "type" "fpsqrt")])

(define_insn "sqrtsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(sqrt:SF (match_operand:SF 1 "register_operand" "f")))]
  ""
  "fsqrts %1,%0"
  [(set_attr "type" "fpsqrt")])

;;- arithmetic shift instructions

;; We can trivially handle shifting the constant 1 by 64 bits.
;; For other shifts we use the library routine.
;; ??? Questionable, we can do better than this can't we?
(define_expand "ashldi3"
  [(parallel [(set (match_operand:DI 0 "register_operand" "")
		   (ashift:DI (match_operand:DI 1 "const_double_operand" "")
			      (match_operand:SI 2 "register_operand" "")))
	      (clobber (reg:SI 0))])]
  ""
  "
{
  if (GET_CODE (operands[1]) == CONST_DOUBLE
      && CONST_DOUBLE_HIGH (operands[1]) == 0
      && CONST_DOUBLE_LOW (operands[1]) == 1)
    operands[1] = const1_rtx;
  else if (operands[1] != const1_rtx)
    FAIL;
}")

;; ??? Questionable, we can do better than this can't we?
(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=&r")
	(ashift:DI (const_int 1)
		   (match_operand:SI 1 "register_operand" "r")))
   (clobber (reg:SI 0))]
  ""
  "subcc %1,32,%%g0\;addx %%g0,0,%R0\;xor %R0,1,%0\;sll %R0,%1,%R0\;sll %0,%1,%0"
  [(set_attr "type" "multi")
   (set_attr "length" "5")])

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashift:SI (match_operand:SI 1 "register_operand" "r")
		   (match_operand:SI 2 "arith_operand" "rI")))]
  ""
  "sll %1,%2,%0")

(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "r")
		     (match_operand:SI 2 "arith_operand" "rI")))]
  ""
  "sra %1,%2,%0")

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
		     (match_operand:SI 2 "arith_operand" "rI")))]
  ""
  "srl %1,%2,%0")

;; Unconditional and other jump instructions
;; On the Sparc, by setting the annul bit on an unconditional branch, the
;; following insn is never executed.  This saves us a nop.  Dbx does not
;; handle such branches though, so we only use them when optimizing.
(define_insn "jump"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  ""
  "b%* %l0%("
  [(set_attr "type" "uncond_branch")])

(define_expand "tablejump"
  [(parallel [(set (pc) (match_operand:SI 0 "register_operand" "r"))
	      (use (label_ref (match_operand 1 "" "")))])]
  ""
  "
{
  /* We need to use the PC value in %o7 that was set up when the address
     of the label was loaded into a register, so we need different RTL.  */
  if (flag_pic)
    {
      emit_insn (gen_pic_tablejump (operands[0], operands[1]));
      DONE;
    }
}")

(define_insn "pic_tablejump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))
   (use (label_ref (match_operand 1 "" "")))
   (use (reg:SI 15))]
  ""
  "jmp %%o7+%0%#"
  [(set_attr "type" "uncond_branch")])

(define_insn ""
  [(set (pc) (match_operand:SI 0 "address_operand" "p"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "jmp %a0%#"
  [(set_attr "type" "uncond_branch")])

(define_insn ""
  [(set (pc) (label_ref (match_operand 0 "" "")))
   (set (reg:SI 15) (label_ref (match_dup 0)))]
  ""
  "call %l0%#"
  [(set_attr "type" "uncond_branch")])

;; This pattern recognizes the "instruction" that appears in 
;; a function call that wants a structure value, 
;; to inform the called function if compiled with Sun CC.
;(define_insn ""
;  [(match_operand:SI 0 "immediate_operand" "")]
;  "GET_CODE (operands[0]) == CONST_INT && INTVAL (operands[0]) > 0"
;  "unimp %0"
;  [(set_attr "type" "marker")])

;;- jump to subroutine
(define_expand "call"
  ;; Note that this expression is not used for generating RTL.
  ;; All the RTL is generated explicitly below.
  [(call (match_operand:SI 0 "call_operand" "")
	 (match_operand 3 "" "i"))]
  ;; operands[2] is next_arg_register
  ;; operands[3] is struct_value_size_rtx.
  ""
  "
{
  rtx fn_rtx, nregs_rtx;

  if (GET_CODE (XEXP (operands[0], 0)) == LABEL_REF)
    {
      /* This is really a PIC sequence.  We want to represent
	 it as a funny jump so it's delay slots can be filled. 

	 ??? But if this really *is* a CALL, will not it clobber the
	 call-clobbered registers?  We lose this if it is a JUMP_INSN.
	 Why cannot we have delay slots filled if it were a CALL?  */

      if (INTVAL (operands[3]) > 0)
	emit_jump_insn (gen_rtx (PARALLEL, VOIDmode, gen_rtvec (3,
				 gen_rtx (SET, VOIDmode, pc_rtx,
					  XEXP (operands[0], 0)),
				 operands[3],
				 gen_rtx (CLOBBER, VOIDmode,
					  gen_rtx (REG, SImode, 15)))));
      else
	emit_jump_insn (gen_rtx (PARALLEL, VOIDmode, gen_rtvec (2,
				 gen_rtx (SET, VOIDmode, pc_rtx,
					  XEXP (operands[0], 0)),
				 gen_rtx (CLOBBER, VOIDmode,
					  gen_rtx (REG, SImode, 15)))));
      goto finish_call;
    }

  fn_rtx = operands[0];

  /* Count the number of parameter registers being used by this call.
     if that argument is NULL, it means we are using them all, which
     means 6 on the sparc.  */
#if 0
  if (operands[2])
    nregs_rtx = gen_rtx (CONST_INT, VOIDmode, REGNO (operands[2]) - 8);
  else
    nregs_rtx = gen_rtx (CONST_INT, VOIDmode, 6);
#else
  nregs_rtx = const0_rtx;
#endif

  if (INTVAL (operands[3]) > 0)
    emit_call_insn (gen_rtx (PARALLEL, VOIDmode, gen_rtvec (3,
			     gen_rtx (CALL, VOIDmode, fn_rtx, nregs_rtx),
			     operands[3],
			     gen_rtx (CLOBBER, VOIDmode,
					       gen_rtx (REG, SImode, 15)))));
  else
    emit_call_insn (gen_rtx (PARALLEL, VOIDmode, gen_rtvec (2,
			     gen_rtx (CALL, VOIDmode, fn_rtx, nregs_rtx),
			     gen_rtx (CLOBBER, VOIDmode,
					       gen_rtx (REG, SImode, 15)))));

 finish_call:
#if 0
  /* If this call wants a structure value,
     emit an unimp insn to let the called function know about this.  */
  if (INTVAL (operands[3]) > 0)
    {
      rtx insn = emit_insn (operands[3]);
      SCHED_GROUP_P (insn) = 1;
    }
#endif

  DONE;
}")

(define_insn ""
  [(call (mem:SI (match_operand:SI 0 "call_operand_address" "S,r"))
	 (match_operand 1 "" ""))
   (clobber (reg:SI 15))]
  ;;- Do not use operand 1 for most machines.
  ""
  "*
{
  return \"call %a0,%1%#\";
}"
  [(set_attr "type" "call")])

;; This is a call that wants a structure value.
(define_insn ""
  [(call (mem:SI (match_operand:SI 0 "call_operand_address" "S,r"))
	 (match_operand 1 "" ""))
   (match_operand 2 "immediate_operand" "")
   (clobber (reg:SI 15))]
  ;;- Do not use operand 1 for most machines.
  "GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) > 0"
  "*
{
  return \"call %a0,%1\;nop\;unimp %2\";
}"
  [(set_attr "type" "call_no_delay_slot")])

(define_expand "call_value"
  [(set (match_operand 0 "register_operand" "=rf")
	(call (match_operand:SI 1 "" "")
	      (match_operand 4 "" "")))]
  ;; operand 3 is next_arg_register
  ""
  "
{
  rtx fn_rtx, nregs_rtx;
  rtvec vec;

  fn_rtx = operands[1];

#if 0
  if (operands[3])
    nregs_rtx = gen_rtx (CONST_INT, VOIDmode, REGNO (operands[3]) - 8);
  else
    nregs_rtx = gen_rtx (CONST_INT, VOIDmode, 6);
#else
  nregs_rtx = const0_rtx;
#endif

  vec = gen_rtvec (2,
		   gen_rtx (SET, VOIDmode, operands[0],
			    gen_rtx (CALL, VOIDmode, fn_rtx, nregs_rtx)),
		   gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 15)));

  emit_call_insn (gen_rtx (PARALLEL, VOIDmode, vec));

  DONE;
}")

(define_insn ""
  [(set (match_operand 0 "" "=rf")
	(call (mem:SI (match_operand:SI 1 "call_operand_address" "rS"))
	      (match_operand 2 "" "")))
   (clobber (reg:SI 15))]
  ;;- Do not use operand 2 for most machines.
  ""
  "*
{
  return \"call %a1,%2%#\";
}"
  [(set_attr "type" "call")])

(define_insn "return"
  [(return)]
  "! TARGET_EPILOGUE"
  "* return output_return (operands);"
  [(set_attr "type" "multi")])

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop")

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "address_operand" "p"))]
  ""
 "jmp %a0%#"
 [(set_attr "type" "uncond_branch")])
 
(define_expand "nonlocal_goto"
  [(match_operand:SI 0 "general_operand" "")
   (match_operand:SI 1 "general_operand" "")
   (match_operand:SI 2 "general_operand" "")
   (match_operand:SI 3 "" "")]
  ""
  "
{
  /* Trap instruction to flush all the registers window.  */
  emit_insn (gen_rtx (UNSPEC_VOLATILE, VOIDmode,
		      gen_rtvec (1, const0_rtx), 0));
  /* Load the fp value for the containing fn into %fp.
     This is needed because operands[2] refers to %fp.
     Virtual register instantiation fails if the virtual %fp isn't set from a
     register.  Thus we must copy operands[0] into a register if it isn't
     already one.  */
  if (GET_CODE (operands[0]) != REG)
    operands[0] = force_reg (SImode, operands[0]);
  emit_move_insn (virtual_stack_vars_rtx, operands[0]);
  /* Find the containing function's current nonlocal goto handler,
     which will do any cleanups and then jump to the label.  */
  emit_move_insn (gen_rtx (REG, SImode, 8), operands[1]);
  /* Restore %fp from stack pointer value for containing function.
     The restore insn that follows will move this to %sp,
     and reload the appropriate value into %fp.  */
  emit_move_insn (frame_pointer_rtx, operands[2]);
  /* Put in the static chain register the nonlocal label address.  */
  emit_move_insn (static_chain_rtx, operands[3]);
  /* USE of frame_pointer_rtx added for consistency; not clear if
     really needed.  */
  emit_insn (gen_rtx (USE, VOIDmode, frame_pointer_rtx));
  emit_insn (gen_rtx (USE, VOIDmode, stack_pointer_rtx));
  emit_insn (gen_rtx (USE, VOIDmode, static_chain_rtx));
  emit_insn (gen_rtx (USE, VOIDmode, gen_rtx (REG, SImode, 8)));
  /* Return, restoring reg window and jumping to goto handler.  */
  emit_insn (gen_rtx (UNSPEC_VOLATILE, VOIDmode,
		      gen_rtvec (1, const0_rtx), 1));
  DONE;
}")

;; Special trap insn to flush register windows.
(define_insn ""
  [(unspec_volatile [(const_int 0)] 0)]
  ""
  "ta 3"
  [(set_attr "type" "misc")])

(define_insn ""
  [(unspec_volatile [(const_int 0)] 1)]
  ""
  "jmp %%o0+0\;restore"
  [(set_attr "type" "misc")
   (set_attr "length" "2")])

;; Split up troublesome insns for better scheduling.  */

;; The following patterns are straightforward.  They can be applied
;; either before or after register allocation.

(define_split
  [(set (match_operator 0 "memop" [(match_operand:SI 1 "symbolic_operand" "")])
	(match_operand 2 "reg_or_0_operand" ""))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "! flag_pic"
  [(set (match_dup 3) (high:SI (match_dup 1)))
   (set (match_op_dup 0 [(lo_sum:SI (match_dup 3) (match_dup 1))])
	(match_dup 2))]
  "")

(define_split
  [(set (match_operator 0 "memop"
			[(match_operand:SI 1 "immediate_operand" "")])
	(match_operand 2 "general_operand" ""))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "flag_pic"
  [(set (match_op_dup 0 [(match_dup 1)])
	(match_dup 2))]
  "
{
  operands[1] = legitimize_pic_address (operands[1], GET_MODE (operands[0]),
					operands[3], 0);
}")

(define_split
  [(set (match_operand 0 "register_operand" "")
	(match_operator 1 "memop"
			[(match_operand:SI 2 "immediate_operand" "")]))]
  "flag_pic"
  [(set (match_dup 0)
	(match_op_dup 1 [(match_dup 2)]))]
  "
{
  operands[2] = legitimize_pic_address (operands[2], GET_MODE (operands[1]),
					operands[0], 0);
}")

;; Sign- and Zero-extend operations can have symbolic memory operands.

(define_split
  [(set (match_operand 0 "register_operand" "")
	(match_operator 1 "extend_op"
			[(match_operator 2 "memop"
					 [(match_operand:SI 3 "immediate_operand" "")])]))]
  "flag_pic"
  [(set (match_dup 0)
	(match_op_dup 1 [(match_op_dup 2 [(match_dup 3)])]))]
  "
{
  operands[3] = legitimize_pic_address (operands[3], GET_MODE (operands[2]),
					operands[0], 0);
}")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "immediate_operand" ""))]
  "! flag_pic && (GET_CODE (operands[1]) == SYMBOL_REF
		  || GET_CODE (operands[1]) == CONST
		  || GET_CODE (operands[1]) == LABEL_REF)"
  [(set (match_dup 0) (high:SI (match_dup 1)))
   (set (match_dup 0)
	(lo_sum:SI (match_dup 0) (match_dup 1)))]
  "")

;; LABEL_REFs are not modified by `legitimize_pic_address`
;; so do not recurse infinitely in the PIC case.
(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "immediate_operand" ""))]
  "flag_pic && (GET_CODE (operands[1]) == SYMBOL_REF
		|| GET_CODE (operands[1]) == CONST)"
  [(set (match_dup 0) (match_dup 1))]
  "
{
  operands[1] = legitimize_pic_address (operands[1], Pmode, operands[0], 0);
}")

;; These split sne/seq insns.  The forms of the resulting insns are 
;; somewhat bogus, but they avoid extra patterns and show data dependency.
;; Nothing will look at these in detail after splitting has occurred.

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(ne:SI (match_operand:SI 1 "register_operand" "") (const_int 0)))
   (clobber (reg:CC 0))]
  ""
  [(set (reg:CC_NOOV 0) (compare:CC_NOOV (neg:SI (match_dup 1))
					 (const_int 0)))
   (set (match_dup 0) (ltu:SI (reg:CC 0) (const_int 0)))]
  "")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(neg:SI (ne:SI (match_operand:SI 1 "register_operand" "")
		       (const_int 0))))
   (clobber (reg:CC 0))]
  ""
  [(set (reg:CC_NOOV 0) (compare:CC_NOOV (neg:SI (match_dup 1))
					 (const_int 0)))
   (set (match_dup 0) (neg:SI (ltu:SI (reg:CC 0) (const_int 0))))]
  "")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(eq:SI (match_operand:SI 1 "register_operand" "") (const_int 0)))
   (clobber (reg:CC 0))]
  ""
  [(set (reg:CC_NOOV 0) (compare:CC_NOOV (neg:SI (match_dup 1))
					 (const_int 0)))
   (set (match_dup 0) (geu:SI (reg:CC 0) (const_int 0)))]
  "")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(neg:SI (eq:SI (match_operand:SI 1 "register_operand" "")
		       (const_int 0))))
   (clobber (reg:CC 0))]
  ""
  [(set (reg:CC_NOOV 0) (compare:CC_NOOV (neg:SI (match_dup 1))
					 (const_int 0)))
   (set (match_dup 0) (neg:SI (geu:SI (reg:CC 0) (const_int 0))))]
  "")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (ne:SI (match_operand:SI 1 "register_operand" "")
			(const_int 0))
		 (match_operand:SI 2 "register_operand" "")))
   (clobber (reg:CC 0))]
  ""
  [(set (reg:CC_NOOV 0) (compare:CC_NOOV (neg:SI (match_dup 1))
					 (const_int 0)))
   (set (match_dup 0) (plus:SI (ltu:SI (reg:CC 0) (const_int 0))
			       (match_dup 2)))]
  "")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(minus:SI (match_operand:SI 2 "register_operand" "")
		  (ne:SI (match_operand:SI 1 "register_operand" "")
			 (const_int 0))))
   (clobber (reg:CC 0))]
  ""
  [(set (reg:CC_NOOV 0) (compare:CC_NOOV (neg:SI (match_dup 1))
					 (const_int 0)))
   (set (match_dup 0) (minus:SI (match_dup 2)
				(ltu:SI (reg:CC 0) (const_int 0))))]
  "")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (eq:SI (match_operand:SI 1 "register_operand" "")
			(const_int 0))
		 (match_operand:SI 2 "register_operand" "")))
   (clobber (reg:CC 0))]
  ""
  [(set (reg:CC_NOOV 0) (compare:CC_NOOV (neg:SI (match_dup 1))
					 (const_int 0)))
   (set (match_dup 0) (plus:SI (geu:SI (reg:CC 0) (const_int 0))
			       (match_dup 2)))]
  "")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(minus:SI (match_operand:SI 2 "register_operand" "")
		  (eq:SI (match_operand:SI 1 "register_operand" "")
			 (const_int 0))))
   (clobber (reg:CC 0))]
  ""
  [(set (reg:CC_NOOV 0) (compare:CC_NOOV (neg:SI (match_dup 1))
					 (const_int 0)))
   (set (match_dup 0) (minus:SI (match_dup 2)
				(geu:SI (reg:CC 0) (const_int 0))))]
  "")

;; Peepholes go at the end.

;; Optimize consecutive loads or stores into ldd and std when possible.
;; The conditions in which we do this are very restricted and are 
;; explained in the code for {registers,memory}_ok_for_ldd functions.

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=rf")
        (match_operand:SI 1 "memory_operand" ""))
   (set (match_operand:SI 2 "register_operand" "=rf")
        (match_operand:SI 3 "memory_operand" ""))]
  "registers_ok_for_ldd_peep (operands[0], operands[2]) 
   && ! MEM_VOLATILE_P (operands[1]) && ! MEM_VOLATILE_P (operands[3])
   && addrs_ok_for_ldd_peep (XEXP (operands[1], 0), XEXP (operands[3], 0))" 
  "ldd %1,%0")

(define_peephole
  [(set (match_operand:SI 0 "memory_operand" "")
        (match_operand:SI 1 "register_operand" "rf"))
   (set (match_operand:SI 2 "memory_operand" "")
        (match_operand:SI 3 "register_operand" "rf"))]
  "registers_ok_for_ldd_peep (operands[1], operands[3]) 
   && ! MEM_VOLATILE_P (operands[0]) && ! MEM_VOLATILE_P (operands[2])
   && addrs_ok_for_ldd_peep (XEXP (operands[0], 0), XEXP (operands[2], 0))"
  "std %1,%0")
 
(define_peephole
  [(set (match_operand:SF 0 "register_operand" "=fr")
        (match_operand:SF 1 "memory_operand" ""))
   (set (match_operand:SF 2 "register_operand" "=fr")
        (match_operand:SF 3 "memory_operand" ""))]
  "registers_ok_for_ldd_peep (operands[0], operands[2]) 
   && ! MEM_VOLATILE_P (operands[1]) && ! MEM_VOLATILE_P (operands[3])
   && addrs_ok_for_ldd_peep (XEXP (operands[1], 0), XEXP (operands[3], 0))"
  "ldd %1,%0")

(define_peephole
  [(set (match_operand:SF 0 "memory_operand" "")
        (match_operand:SF 1 "register_operand" "fr"))
   (set (match_operand:SF 2 "memory_operand" "")
        (match_operand:SF 3 "register_operand" "fr"))]
  "registers_ok_for_ldd_peep (operands[1], operands[3]) 
   && ! MEM_VOLATILE_P (operands[0]) && ! MEM_VOLATILE_P (operands[2])
   && addrs_ok_for_ldd_peep (XEXP (operands[0], 0), XEXP (operands[2], 0))"
  "std %1,%0")

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=rf")
        (match_operand:SI 1 "memory_operand" ""))
   (set (match_operand:SI 2 "register_operand" "=rf")
        (match_operand:SI 3 "memory_operand" ""))]
  "registers_ok_for_ldd_peep (operands[2], operands[0]) 
   && ! MEM_VOLATILE_P (operands[3]) && ! MEM_VOLATILE_P (operands[1])
   && addrs_ok_for_ldd_peep (XEXP (operands[3], 0), XEXP (operands[1], 0))"
  "ldd %3,%2")

(define_peephole
  [(set (match_operand:SI 0 "memory_operand" "")
        (match_operand:SI 1 "register_operand" "rf"))
   (set (match_operand:SI 2 "memory_operand" "")
        (match_operand:SI 3 "register_operand" "rf"))]
  "registers_ok_for_ldd_peep (operands[3], operands[1]) 
   && ! MEM_VOLATILE_P (operands[2]) && ! MEM_VOLATILE_P (operands[0])
   && addrs_ok_for_ldd_peep (XEXP (operands[2], 0), XEXP (operands[0], 0))" 
  "std %3,%2")
 
(define_peephole
  [(set (match_operand:SF 0 "register_operand" "=fr")
        (match_operand:SF 1 "memory_operand" ""))
   (set (match_operand:SF 2 "register_operand" "=fr")
        (match_operand:SF 3 "memory_operand" ""))]
  "registers_ok_for_ldd_peep (operands[2], operands[0]) 
   && ! MEM_VOLATILE_P (operands[3]) && ! MEM_VOLATILE_P (operands[1])
   && addrs_ok_for_ldd_peep (XEXP (operands[3], 0), XEXP (operands[1], 0))"
  "ldd %3,%2")

(define_peephole
  [(set (match_operand:SF 0 "memory_operand" "")
        (match_operand:SF 1 "register_operand" "fr"))
   (set (match_operand:SF 2 "memory_operand" "")
        (match_operand:SF 3 "register_operand" "fr"))]
  "registers_ok_for_ldd_peep (operands[3], operands[1]) 
   && ! MEM_VOLATILE_P (operands[2]) && ! MEM_VOLATILE_P (operands[0])
   && addrs_ok_for_ldd_peep (XEXP (operands[2], 0), XEXP (operands[0], 0))"
  "std %3,%2")
 
;; Optimize the case of following a reg-reg move with a test
;; of reg just moved.  Don't allow floating point regs for operand 0 or 1.
;; This can result from a float to fix conversion.

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operand:SI 1 "register_operand" "r"))
   (set (reg:CC 0)
	(compare:CC (match_operand:SI 2 "register_operand" "r")
		    (const_int 0)))]
  "(rtx_equal_p (operands[2], operands[0])
    || rtx_equal_p (operands[2], operands[1]))
   && ! FP_REG_P (operands[0]) && ! FP_REG_P (operands[1])"
  "orcc %1,%%g0,%0")

;; Do {sign,zero}-extended compares somewhat more efficiently.
;; ??? Is this now the Right Way to do this?  Or will SCRATCH
;;     eventually have some impact here?

(define_peephole
  [(set (match_operand:HI 0 "register_operand" "")
	(match_operand:HI 1 "memory_operand" ""))
   (set (match_operand:SI 2 "register_operand" "")
	(sign_extend:SI (match_dup 0)))
   (set (reg:CC 0)
	(compare:CC (match_dup 2)
		    (const_int 0)))]
  ""
  "ldsh %1,%0\;orcc %0,%%g0,%2")

(define_peephole
  [(set (match_operand:QI 0 "register_operand" "")
	(match_operand:QI 1 "memory_operand" ""))
   (set (match_operand:SI 2 "register_operand" "")
	(sign_extend:SI (match_dup 0)))
   (set (reg:CC 0)
	(compare:CC (match_dup 2)
		    (const_int 0)))]
  ""
  "ldsb %1,%0\;orcc %0,%%g0,%2")

(define_peephole
  [(set (match_operand:HI 0 "register_operand" "")
	(match_operand:HI 1 "memory_operand" ""))
   (set (match_operand:SI 2 "register_operand" "")
	(sign_extend:SI (match_dup 0)))]
  "dead_or_set_p (insn, operands[0])"
  "*
{
  warning (\"bad peephole\");
  if (! MEM_VOLATILE_P (operands[1]))
    abort ();
  return \"ldsh %1,%2\";
}")

(define_peephole
  [(set (match_operand:QI 0 "register_operand" "")
	(match_operand:QI 1 "memory_operand" ""))
   (set (match_operand:SI 2 "register_operand" "")
	(sign_extend:SI (match_dup 0)))]
  "dead_or_set_p (insn, operands[0])"
  "*
{
  warning (\"bad peephole\");
  if (! MEM_VOLATILE_P (operands[1]))
    abort ();
  return \"ldsb %1,%2\";
}")

;; Floating-point move peepholes

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lo_sum:SI (match_dup 0)
		   (match_operand:SI 1 "immediate_operand" "i")))
   (set (match_operand:DF 2 "register_operand" "=fr")
	(mem:DF (match_dup 0)))]
  "RTX_UNCHANGING_P (operands[1]) && reg_unused_after (operands[0], insn)"
  "*
{
  /* Go by way of output_move_double in case the register in operand 2
     is not properly aligned for ldd.  */
  operands[1] = gen_rtx (MEM, DFmode,
			 gen_rtx (LO_SUM, SImode, operands[0], operands[1]));
  operands[0] = operands[2];
  return output_move_double (operands);
}")

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lo_sum:SI (match_dup 0)
		   (match_operand:SI 1 "immediate_operand" "i")))
   (set (match_operand:SF 2 "register_operand" "=fr")
	(mem:SF (match_dup 0)))]
  "RTX_UNCHANGING_P (operands[1]) && reg_unused_after (operands[0], insn)"
  "ld [%0+%%lo(%a1)],%2")

;; Return peepholes.  First the "normal" ones

;; ??? There are QImode, HImode, and SImode versions of this pattern.
;; It might be possible to write one more general pattern instead of three.

(define_insn ""
  [(set (match_operand:QI 0 "restore_operand" "")
	(match_operand:QI 1 "arith_operand" "rI"))
   (return)]
  "! TARGET_EPILOGUE"
  "*
{
  if (current_function_returns_struct)
    return \"jmp %%i7+12\;restore %%g0,%1,%Y0\";
  else
    return \"ret\;restore %%g0,%1,%Y0\";
}"
  [(set_attr "type" "multi")])

(define_insn ""
  [(set (match_operand:HI 0 "restore_operand" "")
	(match_operand:HI 1 "arith_operand" "rI"))
   (return)]
  "! TARGET_EPILOGUE"
  "*
{
  if (current_function_returns_struct)
    return \"jmp %%i7+12\;restore %%g0,%1,%Y0\";
  else
    return \"ret\;restore %%g0,%1,%Y0\";
}"
  [(set_attr "type" "multi")])

(define_insn ""
  [(set (match_operand:SI 0 "restore_operand" "")
	(match_operand:SI 1 "arith_operand" "rI"))
   (return)]
  "! TARGET_EPILOGUE"
  "*
{
  if (current_function_returns_struct)
    return \"jmp %%i7+12\;restore %%g0,%1,%Y0\";
  else
    return \"ret\;restore %%g0,%1,%Y0\";
}"
  [(set_attr "type" "multi")])

(define_insn ""
  [(set (match_operand:SI 0 "restore_operand" "")
	(plus:SI (match_operand:SI 1 "arith_operand" "%r")
		 (match_operand:SI 2 "arith_operand" "rI")))
   (return)]
  "! TARGET_EPILOGUE"
  "*
{
  if (current_function_returns_struct)
    return \"jmp %%i7+12\;restore %r1,%2,%Y0\";
  else
    return \"ret\;restore %r1,%2,%Y0\";
}"
  [(set_attr "type" "multi")])

;; Turned off because it should never match (subtracting a constant
;; is turned into addition) and because it would do the wrong thing
;; when operand 2 is -4096 (--4096 == 4096 is not a valid immediate).
;;(define_insn ""
;;  [(set (match_operand:SI 0 "restore_operand" "")
;;	(minus:SI (match_operand:SI 1 "register_operand" "r")
;;		  (match_operand:SI 2 "small_int" "I")))
;;   (return)]
;;  "! TARGET_EPILOGUE"
;;  "ret\;restore %1,-(%2),%Y0"
;;  [(set_attr "type" "multi")])

;; The following pattern is only generated by delayed-branch scheduling,
;; when the insn winds up in the epilogue.
(define_insn ""
  [(set (reg:SF 32)
	(match_operand:SF 0 "register_operand" "f"))
   (return)]
  "! TARGET_EPILOGUE"
  "ret\;fmovs %0,%%f0"
  [(set_attr "type" "multi")])

;; Now peepholes to go a call followed by a jump.

(define_peephole
  [(parallel [(set (match_operand 0 "" "")
		   (call (mem:SI (match_operand:SI 1 "call_operand_address" "S,r"))
			 (match_operand 2 "" "")))
	      (clobber (reg:SI 15))])
   (set (pc) (label_ref (match_operand 3 "" "")))]
  "short_branch (INSN_UID (insn), INSN_UID (operands[3]))"
  "*
{
  return \"call %a1,%2\;add %%o7,(%l3-.-4),%%o7\";
}")

(define_peephole
  [(parallel [(call (mem:SI (match_operand:SI 0 "call_operand_address" "S,r"))
		    (match_operand 1 "" ""))
	      (clobber (reg:SI 15))])
   (set (pc) (label_ref (match_operand 2 "" "")))]
  "short_branch (INSN_UID (insn), INSN_UID (operands[2]))"
  "*
{
  return \"call %a0,%1\;add %%o7,(%l2-.-4),%%o7\";
}")

(define_peephole
  [(parallel [(set (match_operand:SI 0 "register_operand" "=r")
		   (minus:SI (match_operand:SI 1 "reg_or_0_operand" "rJ")
			     (reg:SI 0)))
	      (clobber (reg:CC 0))])
   (set (reg:CC 0) (compare (match_dup 0) (const_int 0)))]
  ""
  "subxcc %r1,0,%0")

;;- Local variables:
;;- mode:emacs-lisp
;;- comment-start: ";;- "
;;- eval: (set-syntax-table (copy-sequence (syntax-table)))
;;- eval: (modify-syntax-entry ?[ "(]")
;;- eval: (modify-syntax-entry ?] ")[")
;;- eval: (modify-syntax-entry ?{ "(}")
;;- eval: (modify-syntax-entry ?} "){")
;;- End:
