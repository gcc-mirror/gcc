;;- Machine description for HP PA-RISC architecture for GNU C compiler
;;   Copyright (C) 1992 Free Software Foundation, Inc.
;;   Contributed by the Center for Software Science at the University
;;   of Utah.

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

;; This gcc Version 2 machine description is inspired by sparc.md and
;; mips.md.

;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.

;; Insn type.  Used to default other attribute values.

;; type "unary" insns have one input operand (1) and one output operand (0)
;; type "binary" insns have two input operands (1,2) and one output (0)

(define_attr "type"
  "move,unary,binary,compare,load,store,branch,cbranch,fbranch,call,dyncall,fpload,fpstore,fpalu,fpcc,fpmul,fpdivsgl,fpdivdbl,fpsqrtsgl,fpsqrtdbl,multi,misc,milli"
  (const_string "binary"))

;; Length (in # of insns).
(define_attr "length" ""
  (cond [(eq_attr "type" "load,fpload")
	 (if_then_else (match_operand 1 "symbolic_memory_operand" "")
		       (const_int 2) (const_int 1))

	 (eq_attr "type" "store,fpstore")
	 (if_then_else (match_operand 0 "symbolic_memory_operand" "")
		       (const_int 2) (const_int 1))

	 (eq_attr "type" "binary")
	 (if_then_else (match_operand 2 "arith_operand" "")
		       (const_int 1) (const_int 3))

	 (eq_attr "type" "move,unary")
	 (if_then_else (match_operand 1 "arith_operand" "")
		       (const_int 1) (const_int 2))]

	(const_int 1)))

(define_asm_attributes
  [(set_attr "length" "1")
   (set_attr "type" "multi")])

;; Attributes for instruction and branch scheduling

(define_attr "in_branch_delay" "false,true"
  (if_then_else (and (eq_attr "type" "!branch,cbranch,fbranch,call,dyncall,multi,milli")
		     (eq_attr "length" "1"))
		(const_string "true")
		(const_string "false")))

;; Unconditional branch, call, and millicode call delay slot description.
(define_delay (eq_attr "type" "branch,call,milli")
  [(eq_attr "in_branch_delay" "true") (nil) (nil)])

;; Floating point conditional branch delay slot description.
(define_delay (eq_attr "type" "fbranch")
  [(eq_attr "in_branch_delay" "true")
   (eq_attr "in_branch_delay" "true")
   (nil)])

;; Integer conditional branch delay slot description.
(define_delay (eq_attr "type" "cbranch")
  [(eq_attr "in_branch_delay" "true") (nil) (nil)])

;; Function units of the HPPA. The following data is for the "Snake"
;; (Mustang CPU + Timex FPU) because that's what I have the docs for.
;; Scheduling instructions for PA-83 machines according to the Snake
;; constraints shouldn't hurt.

;; (define_function_unit {name} {num-units} {n-users} {test}
;;                       {ready-delay} {issue-delay} [{conflict-list}])

;; The integer ALU.
;; (Noted only for documentation; units that take one cycle do not need to
;; be specified.)

;; (define_function_unit "alu" 1 0
;;  (eq_attr "type" "unary,binary,move,address") 1 0)


;; Memory. Disregarding Cache misses, the Mustang memory times are:
;; load: 2
;; store, fpstore: 3, no D-cache operations should be scheduled.
;; fpload: 3 (really 2 for flops, but I don't think we can specify that).

(define_function_unit "memory" 1 0 (eq_attr "type" "load") 2 0)
(define_function_unit "memory" 1 0 (eq_attr "type" "store,fpstore") 3 3)
(define_function_unit "memory" 1 0 (eq_attr "type" "fpload") 2 0)

;; The Timex has two floating-point units: ALU, and MUL/DIV/SQRT unit.
;; Timings:
;; Instruction	Time	Unit	Minimum Distance (unit contention)
;; fcpy		3	ALU	2
;; fabs		3	ALU	2
;; fadd		3	ALU	2
;; fsub		3	ALU	2
;; fcmp		3	ALU	2
;; fcnv		3	ALU	2
;; fmpyadd	3	ALU,MPY	2
;; fmpysub	3	ALU,MPY 2
;; fmpycfxt	3	ALU,MPY 2
;; fmpy		3	MPY	2
;; fmpyi	3	MPY	2
;; fdiv,sgl	10	MPY	10
;; fdiv,dbl	12	MPY	12
;; fsqrt,sgl	14	MPY	14
;; fsqrt,dbl	18	MPY	18

(define_function_unit "fp_alu" 1 0 (eq_attr "type" "fpcc") 4 2)
(define_function_unit "fp_alu" 1 0 (eq_attr "type" "fpalu") 3 2)
(define_function_unit "fp_mpy" 1 0 (eq_attr "type" "fpmul") 3 2)
(define_function_unit "fp_mpy" 1 0 (eq_attr "type" "fpdivsgl") 10 10)
(define_function_unit "fp_mpy" 1 0 (eq_attr "type" "fpdivdbl") 12 12)
(define_function_unit "fp_mpy" 1 0 (eq_attr "type" "fpsqrtsgl") 14 14)
(define_function_unit "fp_mpy" 1 0 (eq_attr "type" "fpsqrtdbl") 18 18)

;; Compare instructions.
;; This controls RTL generation and register allocation.

;; We generate RTL for comparisons and branches by having the cmpxx
;; patterns store away the operands.  Then, the scc and bcc patterns
;; emit RTL for both the compare and the branch.
;;

(define_expand "cmpsi"
  [(set (reg:CC 0)
	(compare:CC (match_operand:SI 0 "reg_or_0_operand" "")
		    (match_operand:SI 1 "arith5_operand" "")))]
  ""
  "
{
 hppa_compare_op0 = operands[0];
 hppa_compare_op1 = operands[1];
 hppa_branch_type = CMP_SI;
 DONE;
}")

(define_expand "cmpsf"
  [(set (reg:CCFP 0)
	(compare:CCFP (match_operand:SF 0 "reg_or_0_operand" "")
		      (match_operand:SF 1 "reg_or_0_operand" "")))]
  ""
  "
{
  hppa_compare_op0 = operands[0];
  hppa_compare_op1 = operands[1];
  hppa_branch_type = CMP_SF;
  DONE;
}")

(define_expand "cmpdf"
  [(set (reg:CCFP 0)
      (compare:CCFP (match_operand:DF 0 "reg_or_0_operand" "")
                    (match_operand:DF 1 "reg_or_0_operand" "")))]
  ""
  "
{
  hppa_compare_op0 = operands[0];
  hppa_compare_op1 = operands[1];
  hppa_branch_type = CMP_DF;
  DONE;
}")

(define_insn ""
 [(set (reg:CCFP 0)
       (match_operator:CCFP 2 "comparison_operator"
			    [(match_operand:SF 0 "reg_or_0_operand" "fxG")
			     (match_operand:SF 1 "reg_or_0_operand" "fxG")]))]
 ""
 "fcmp,sgl,%Y2 %r0,%r1"
 [(set_attr "type" "fpcc")])

(define_insn ""
 [(set (reg:CCFP 0)
       (match_operator:CCFP 2 "comparison_operator"
			    [(match_operand:DF 0 "reg_or_0_operand" "fxG")
			     (match_operand:DF 1 "reg_or_0_operand" "fxG")]))]
 ""
 "fcmp,dbl,%Y2 %r0,%r1"
 [(set_attr "type" "fpcc")])

;; scc insns.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operator:CCFP 1 "comparison_operator" [(reg:CCFP 0)
						      (const_int 0)]))]
  ""
  "copy 0,%0\;ftest\;ldi 1,%0"
  [(set_attr "type" "unary")
   (set_attr "length" "3")])

(define_expand "seq"
  [(set (match_operand:SI 0 "register_operand" "")
	(eq:SI (match_dup 1)
	       (match_dup 2)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    {
      emit_insn (gen_cmp_fp (EQ, hppa_compare_op0, hppa_compare_op1));
      emit_insn (gen_scond_fp (EQ, operands[0]));
      DONE;
    }
  /* set up operands from compare.  */
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
  /* fall through and generate default code */
}")

(define_expand "sne"
  [(set (match_operand:SI 0 "register_operand" "")
	(ne:SI (match_dup 1)
	       (match_dup 2)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    {
      emit_insn (gen_cmp_fp (NE, hppa_compare_op0, hppa_compare_op1));
      emit_insn (gen_scond_fp (NE, operands[0]));
      DONE;
    }
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "slt"
  [(set (match_operand:SI 0 "register_operand" "")
	(lt:SI (match_dup 1)
	       (match_dup 2)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    {
      emit_insn (gen_cmp_fp (LT, hppa_compare_op0, hppa_compare_op1));
      emit_insn (gen_scond_fp (LT, operands[0]));
      DONE;
    }
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "sgt"
  [(set (match_operand:SI 0 "register_operand" "")
	(gt:SI (match_dup 1)
	       (match_dup 2)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    {
      emit_insn (gen_cmp_fp (GT, hppa_compare_op0, hppa_compare_op1));
      emit_insn (gen_scond_fp (GT, operands[0]));
      DONE;
    }
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "sle"
  [(set (match_operand:SI 0 "register_operand" "")
	(le:SI (match_dup 1)
	       (match_dup 2)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    {
      emit_insn (gen_cmp_fp (LE, hppa_compare_op0, hppa_compare_op1));
      emit_insn (gen_scond_fp (LE, operands[0]));
      DONE;
    }
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "sge"
  [(set (match_operand:SI 0 "register_operand" "")
	(ge:SI (match_dup 1)
	       (match_dup 2)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    {
      emit_insn (gen_cmp_fp (GE, hppa_compare_op0, hppa_compare_op1));
      emit_insn (gen_scond_fp (GE, operands[0]));
      DONE;
    }
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "sltu"
  [(set (match_operand:SI 0 "register_operand" "")
	(ltu:SI (match_dup 1)
	        (match_dup 2)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    FAIL;
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "sgtu"
  [(set (match_operand:SI 0 "register_operand" "")
	(gtu:SI (match_dup 1)
	        (match_dup 2)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    FAIL;
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "sleu"
  [(set (match_operand:SI 0 "register_operand" "")
	(leu:SI (match_dup 1)
	        (match_dup 2)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    FAIL;
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "sgeu"
  [(set (match_operand:SI 0 "register_operand" "")
	(geu:SI (match_dup 1)
	        (match_dup 2)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    FAIL;
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

;; Instruction canonicalization puts immediate operands second, which
;; is the reverse of what we want.

(define_insn "scc"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operator:SI 3 "comparison_operator"
			   [(match_operand:SI 1 "register_operand" "r")
			    (match_operand:SI 2  "arith11_operand" "rI")]))]
  ""
  "com%I2clr,%B3 %2,%1,%0\;ldi 1,%0"
  [(set_attr "type" "binary")
   (set_attr "length" "2")])

;; Combiner patterns for common operations performed with the output
;; from an scc insn (negscc and incscc).  
(define_insn "negscc"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg (match_operator:SI 3 "comparison_operator"
	       [(match_operand:SI 1 "register_operand" "r")
		(match_operand:SI 2  "arith11_operand" "rI")])))]
  ""
  "com%I2clr,%B3 %2,%1,%0\;ldi -1,%0"
  [(set_attr "type" "binary")
   (set_attr "length" "2")])

;; Patterns for adding/subtracting the result of a boolean expression from
;; a register.  First we have special patterns that make use of the carry
;; bit, and output only two instructions.  For the cases we can't in
;; general do in two instructions, the incscc pattern at the end outputs
;; two or three instructions.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (leu:SI (match_operand:SI 2 "register_operand" "r")
			 (match_operand:SI 3 "arith11_operand" "rI"))
		 (match_operand:SI 1 "register_operand" "r")))]
  ""
  "sub%I3 %3,%2,0\;addc 0,%1,%0"
  [(set_attr "type" "binary")
   (set_attr "length" "2")])

; This need only accept registers for op3, since canonicalization
; replaces geu with gtu when op3 is an integer.
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (geu:SI (match_operand:SI 2 "register_operand" "r")
			 (match_operand:SI 3 "register_operand" "r"))
		 (match_operand:SI 1 "register_operand" "r")))]
  ""
  "sub %2,%3,0\;addc 0,%1,%0"
  [(set_attr "type" "binary")
   (set_attr "length" "2")])

; Match only integers for op3 here.  This is used as canonical form of the
; geu pattern when op3 is an integer.  Don't match registers since we can't
; make better code than the general incscc pattern.
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (gtu:SI (match_operand:SI 2 "register_operand" "r")
			 (match_operand:SI 3 "int11_operand" "I"))
		 (match_operand:SI 1 "register_operand" "r")))]
  ""
  "addi %k3,%2,0\;addc 0,%1,%0"
  [(set_attr "type" "binary")
   (set_attr "length" "2")])

(define_insn "incscc"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
 	(plus:SI (match_operator:SI 4 "comparison_operator"
		    [(match_operand:SI 2 "register_operand" "r,r")
		     (match_operand:SI 3 "arith11_operand" "rI,rI")])
		 (match_operand:SI 1 "register_operand" "0,?r")))]
  ""
  "@
   com%I3clr,%B4 %3,%2,0\;addi 1,%0,%0
   com%I3clr,%B4 %3,%2,0\;addi,tr 1,%1,%0\;copy %1,%0"
  [(set_attr "type" "binary,binary")
   (set_attr "length" "2,3")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 1 "register_operand" "r")
		  (gtu:SI (match_operand:SI 2 "register_operand" "r")
			  (match_operand:SI 3 "arith11_operand" "rI"))))]
  ""
  "sub%I3 %3,%2,0\;subb %1,0,%0"
  [(set_attr "type" "binary")
   (set_attr "length" "2")])

; This need only accept registers for op3, since canonicalization
; replaces ltu with leu when op3 is an integer.
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 1 "register_operand" "r")
		  (ltu:SI (match_operand:SI 2 "register_operand" "r")
			  (match_operand:SI 3 "register_operand" "r"))))]
  ""
  "sub %2,%3,0\;subb %1,0,%0"
  [(set_attr "type" "binary")
   (set_attr "length" "2")])

; Match only integers for op3 here.  This is used as canonical form of the
; ltu pattern when op3 is an integer.  Don't match registers since we can't
; make better code than the general incscc pattern.
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 1 "register_operand" "r")
		  (leu:SI (match_operand:SI 2 "register_operand" "r")
			  (match_operand:SI 3 "int11_operand" "I"))))]
  ""
  "addi %k3,%2,0\;subb %1,0,%0"
  [(set_attr "type" "binary")
   (set_attr "length" "2")])

(define_insn "decscc"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(minus:SI (match_operand:SI 1 "register_operand" "0,?r")
		  (match_operator:SI 4 "comparison_operator"
		     [(match_operand:SI 2 "register_operand" "r,r")
		      (match_operand:SI 3 "arith11_operand" "rI,rI")])))]
  ""
  "@
   com%I3clr,%B4 %3,%2,0\;addi -1,%0,%0
   com%I3clr,%B4 %3,%2,0\;addi,tr -1,%1,%0\;copy %1,%0"
  [(set_attr "type" "binary,binary")
   (set_attr "length" "2,3")])

;;; Experimental conditional move

(define_insn "cmov"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r")
	(if_then_else:SI
	 (match_operator 5 "comparison_operator"
			 [(match_operand:SI 3 "register_operand" "r,r,r,r")
			  (match_operand:SI 4 "arith5_operand" "rL,rL,rL,rL")])
	 (match_operand:SI 1 "arith11_operand" "0,0,r,I")
	 (match_operand:SI 2 "arith11_operand" "r,I,0,0")))]
  ""
  "@
   com%I4clr,%S5 %4,%3,0\;copy %2,%0
   com%I4clr,%S5 %4,%3,0\;ldi %2,%0
   com%I4clr,%S5 %4,%3,0\;copy %1,%0
   com%I4clr,%S5 %4,%3,0\;ldi %1,%0"
  [(set_attr "type" "multi,multi,multi,multi")
   (set_attr "length" "2,2,2,2")])

;; Conditional Branches

(define_expand "beq"
  [(set (pc)
	(if_then_else (eq (match_dup 1) (match_dup 2))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    {
      emit_insn (gen_cmp_fp (EQ, hppa_compare_op0, hppa_compare_op1));
      emit_bcond_fp (NE, operands[0]);
      DONE;
    }
  /* set up operands from compare.  */
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
  /* fall through and generate default code */
}")

(define_expand "bne"
  [(set (pc)
	(if_then_else (ne (match_dup 1) (match_dup 2))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    {
      emit_insn (gen_cmp_fp (NE, hppa_compare_op0, hppa_compare_op1));
      emit_bcond_fp (NE, operands[0]);
      DONE;
    }
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "bgt"
  [(set (pc)
	(if_then_else (gt (match_dup 1) (match_dup 2))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    {
      emit_insn (gen_cmp_fp (GT, hppa_compare_op0, hppa_compare_op1));
      emit_bcond_fp (NE, operands[0]);
      DONE;
    }
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "blt"
  [(set (pc)
	(if_then_else (lt (match_dup 1) (match_dup 2))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    {
      emit_insn (gen_cmp_fp (LT, hppa_compare_op0, hppa_compare_op1));
      emit_bcond_fp (NE, operands[0]);
      DONE;
    }
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "bge"
  [(set (pc)
	(if_then_else (ge (match_dup 1) (match_dup 2))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    {
      emit_insn (gen_cmp_fp (GE, hppa_compare_op0, hppa_compare_op1));
      emit_bcond_fp (NE, operands[0]);
      DONE;
    }
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "ble"
  [(set (pc)
	(if_then_else (le (match_dup 1) (match_dup 2))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    {
      emit_insn (gen_cmp_fp (LE, hppa_compare_op0, hppa_compare_op1));
      emit_bcond_fp (NE, operands[0]);
      DONE;
    }
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "bgtu"
  [(set (pc)
	(if_then_else (gtu (match_dup 1) (match_dup 2))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    FAIL;
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "bltu"
  [(set (pc)
	(if_then_else (ltu (match_dup 1) (match_dup 2))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    FAIL;
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "bgeu"
  [(set (pc)
	(if_then_else (geu (match_dup 1) (match_dup 2))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    FAIL;
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

(define_expand "bleu"
  [(set (pc)
	(if_then_else (leu (match_dup 1) (match_dup 2))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (hppa_branch_type != CMP_SI)
    FAIL;
  operands[1] = hppa_compare_op0;
  operands[2] = hppa_compare_op1;
}")

;; Match the branch patterns.

(define_insn ""
  [(set (pc)
	(if_then_else
	 (match_operator 3 "comparison_operator"
			 [(match_operand:SI 1 "register_operand" "r,r")
			  (match_operand:SI 2 "arith5_operand" "r,L")])
	 (label_ref (match_operand 0 "" ""))
	 (pc)))]
  ""
  "*
{
  return (get_attr_length (insn) == 1
	  ? \"com%I2b,%S3 %2,%1,%0%#\" : \"com%I2clr,%B3 %2,%1,0\;bl %0,0%#\");
}"
  [(set_attr "type" "cbranch")
   (set (attr "length") (if_then_else (lt (abs (minus (match_dup 0)
						      (plus (pc) (const_int 2))))
					  (const_int 1023))
				      (const_int 1)
				      (const_int 2)))])

;; Match the negated branch.

(define_insn ""
  [(set (pc)
	(if_then_else
	 (match_operator 3 "comparison_operator"
			 [(match_operand:SI 1 "register_operand" "r,r")
			  (match_operand:SI 2 "arith5_operand" "r,L")])
	 (pc)
	 (label_ref (match_operand 0 "" ""))))]
  ""
  "*
{
  return (get_attr_length (insn) == 1
	  ? \"com%I2b,%B3 %2,%1,%0%#\" : \"com%I2clr,%S3 %2,%1,0%#\;bl %0,0%#\");
}"
  [(set_attr "type" "cbranch")
   (set (attr "length") (if_then_else (lt (abs (minus (match_dup 0)
						      (plus (pc) (const_int 2))))
					  (const_int 1023))
				      (const_int 1)
				      (const_int 2)))])

;; Floating point branches

(define_insn ""
  [(set (pc) (if_then_else (ne (reg:CCFP 0) (const_int 0))
			   (label_ref (match_operand 0 "" ""))
			   (pc)))]
  ""
  "*
{
  if (INSN_ANNULLED_BRANCH_P (insn))
    return \"ftest\;bl,n %0,0\";
  else
    return \"ftest\;bl%* %0,0\";
}"
  [(set_attr "type" "fbranch")
   (set_attr "length" "2")])

(define_insn ""
  [(set (pc) (if_then_else (ne (reg:CCFP 0) (const_int 0))
			   (pc)
			   (label_ref (match_operand 0 "" ""))))]
  ""
  "*
{
  if (INSN_ANNULLED_BRANCH_P (insn))
    return \"ftest\;add,tr 0,0,0\;bl,n %0,0\";
  else
    return \"ftest\;add,tr 0,0,0\;bl%* %0,0\";
}"
  [(set_attr "type" "fbranch")
   (set_attr "length" "3")])

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

;; Reloading an SImode or DImode value requires a scratch register if
;; going in to or out of float point registers.

(define_expand "reload_insi"
  [(set (match_operand:SI 0 "register_operand" "=Z")
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

(define_expand "reload_outsi"
  [(set (match_operand:SI 0 "general_operand" "")
	(match_operand:SI 1  "register_operand" "Z"))
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

(define_insn ""
  [(set (match_operand:SI 0 "fp_reg_operand" "=fx")
	(match_operand:SI 1 "short_memory_operand" "T"))]
  ""
  "fldws%F1 %1,%0"
  [(set_attr "type" "fpload")
   (set_attr "length" "1")])

(define_insn ""
  [(set (match_operand:SI 0 "short_memory_operand" "=T")
	(match_operand:SI 1 "fp_reg_operand" "fx"))]
  ""
  "fstws%F0 %1,%0"
  [(set_attr "type" "fpstore")
   (set_attr "length" "1")])

;;; pic symbol references

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mem:SI (plus:SI (match_operand:SI 1 "register_operand" "r")
			 (match_operand:SI 2 "symbolic_operand" ""))))]
  "flag_pic && operands[1] == pic_offset_table_rtx"
  "ldw T'%2(%1),%0"
  [(set_attr "type" "load")
   (set_attr "length" "1")])

(define_insn ""
  [(set (match_operand:SI 0 "reg_or_nonsymb_mem_operand" "=r,r,r,r,r,Q,*q,!*r,!fx,!fx")
	(match_operand:SI 1 "move_operand" "rM,J,N,K,Q,rM,rM,!fx,!*r,!fxM"))]
  "register_operand (operands[0], SImode)
   || reg_or_0_operand (operands[1], SImode)"
  "@
   copy %r1,%0
   ldi %1,%0
   ldil L'%1,%0
   zdepi %Z1,%0
   ldw%M1 %1,%0
   stw%M0 %r1,%0
   mtsar %r1
   fstws %1,-16(0,%%r30)\;ldw -16(0,%%r30),%0
   stw %1,-16(0,%%r30)\;fldws -16(0,%%r30),%0
   fcpy,sgl %r1,%0"
  [(set_attr "type" "move,move,move,move,load,store,move,load,fpload,fpalu")
   (set_attr "length" "1,1,1,1,1,1,1,2,2,1")])

;; Load indexed.  We don't use unscaled modes since they can't be used
;; unless we can tell which of the registers is the base and which is
;; the index, due to PA's idea of segment selection using the top bits
;; of the base register.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mem:SI (plus:SI (mult:SI (match_operand:SI 1 "register_operand" "r")
				  (const_int 4))
			 (match_operand:SI 2 "register_operand" "r"))))]
  "! TARGET_DISABLE_INDEXING"
  "ldwx,s %1(0,%2),%0"
  [(set_attr "type" "load")
   (set_attr "length" "1")])

;; Load or store with base-register modification.

(define_insn ""
  [(set (match_operand:SI 3 "register_operand" "=r")
	(mem:SI (plus:SI (match_operand:SI 1 "register_operand" "0")
			 (match_operand:SI 2 "pre_cint_operand" ""))))
   (set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "*
{
  if (INTVAL (operands[2]) < 0)
    return \"ldwm %2(0,%0),%3\";
  return \"ldws,mb %2(0,%0),%3\";
}"
  [(set_attr "type" "load")
   (set_attr "length" "1")])

(define_insn ""
  [(set (mem:SI (plus:SI (match_operand:SI 1 "register_operand" "0")
			 (match_operand:SI 2 "pre_cint_operand" "")))
	(match_operand:SI 3 "reg_or_0_operand" "rM"))
   (set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "*
{
  if (INTVAL (operands[2]) < 0)
    return \"stwm %r3,%2(0,%0)\";
  return \"stws,mb %r3,%2(0,%0)\";
}"
  [(set_attr "type" "store")
   (set_attr "length" "1")])

;; For pic
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operand:SI 1 "pic_operand" "i"))
   (clobber (match_scratch:SI 2 "=a"))]
  ""
  "*
{
  rtx label_rtx = gen_label_rtx ();
  rtx xoperands[3];
  extern FILE *asm_out_file;

  xoperands[0] = operands[0];
  xoperands[1] = operands[1];
  xoperands[2] = label_rtx;
  output_asm_insn (\"bl .+8,%0\;addil L'%1-%2,%0\", xoperands);
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, \"L\", CODE_LABEL_NUMBER (label_rtx));
  output_asm_insn (\"ldo R'%1-%2(1),%0\", xoperands);
  return \"\";
  }
"
  [(set_attr "type" "multi")
   (set_attr "length" "3")])

;; For kernel code always use addil; else we can lose due to a linker
;; bug involving absolute symbols and "ldil;add" style relocations
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=a")
	(high:SI (match_operand 1 "" "")))]
  "TARGET_KERNEL && symbolic_operand(operands[1], Pmode)
   && ! function_label_operand (operands[1])
   && ! read_only_operand (operands[1])"
  "@
   addil L'%G1,%%r27"
  [(set_attr "type" "binary")
   (set_attr "length" "1")])

;; For all symbolic operands *except* function addresses and read-only
;; operands (which live in TEXT space and do not require relocation).  
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=a,!*r")
	(high:SI (match_operand 1 "" "")))]
  "! TARGET_KERNEL && symbolic_operand(operands[1], Pmode)
   && ! function_label_operand (operands[1])
   && ! read_only_operand (operands[1])"
  "@
   addil L'%G1,%%r27
   ldil L'%G1,%0\;add %0,%%r27,%0"
  [(set_attr "type" "binary,binary")
   (set_attr "length" "1,2")])

;; For function addresses when TARGET_SHARED_LIBS
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(high:SI (match_operand:SI 1 "function_label_operand" "")))]
  "TARGET_SHARED_LIBS"
  "ldil LP'%G1,%0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(high:SI (match_operand 1 "" "")))]
  "check_pic (1)"
  "ldil L'%G1,%0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

;; lo_sum of a function address when TARGET_SHARED_LIBS
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lo_sum:SI (match_operand:SI 1 "register_operand" "r")
		   (match_operand:SI 2 "function_label_operand" "")))
   (clobber (match_operand:SI 3 "register_operand" "=r"))]
  "TARGET_SHARED_LIBS"
  "ldo RP'%G2(%1),%0\;extru,= %0,31,1,%3\;ldw -4(0,%%r27),%3\;add %0,%3,%0"
  [(set_attr "type" "multi")
   (set_attr "length" "4")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lo_sum:SI (match_operand:SI 1 "register_operand" "r")
		   (match_operand:SI 2 "immediate_operand" "i")))]
  ""
  "ldo R'%G2(%1),%0"
  [(set_attr "length" "1")])

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
  [(set (match_operand:HI 0 "reg_or_nonsymb_mem_operand" "=r,r,r,r,r,Q,*q,!*r,!fx,!fx")
	(match_operand:HI 1 "move_operand" "rM,J,N,K,Q,rM,rM,!fx,!*r,!fxM"))]
  "register_operand (operands[0], HImode)
   || reg_or_0_operand (operands[1], HImode)"
  "@
   copy %r1,%0
   ldi %1,%0
   ldil L'%1,%0
   zdepi %Z1,%0
   ldh%M1 %1,%0
   sth%M0 %r1,%0
   mtsar %r1
   fstws %1,-16(0,%%r30)\;ldw -16(0,%%r30),%0
   stw %1,-16(0,%%r30)\;fldws -16(0,%%r30),%0
   fcpy,sgl %r1,%0"
  [(set_attr "type" "move,move,move,move,load,store,move,load,fpload,fpalu")
   (set_attr "length" "1,1,1,1,1,1,1,2,2,1")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r")
	(mem:HI (plus:SI (mult:SI (match_operand:SI 2 "register_operand" "r")
				  (const_int 2))
			 (match_operand:SI 1 "register_operand" "r"))))]
  "! TARGET_DISABLE_INDEXING"
  "ldhx,s %2(0,%1),%0"
  [(set_attr "type" "load")
   (set_attr "length" "1")])

(define_insn ""
  [(set (match_operand:HI 3 "register_operand" "=r")
	(mem:HI (plus:SI (match_operand:SI 1 "register_operand" "0")
			 (match_operand:SI 2 "int5_operand" "L"))))
   (set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "ldhs,mb %2(0,%0),%3"
  [(set_attr "type" "load")
   (set_attr "length" "1")])

(define_insn ""
  [(set (mem:HI (plus:SI (match_operand:SI 1 "register_operand" "0")
			 (match_operand:SI 2 "int5_operand" "L")))
	(match_operand:HI 3 "reg_or_0_operand" "rM"))
   (set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "sths,mb %r3,%2(0,%0)"
  [(set_attr "type" "store")
   (set_attr "length" "1")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r")
	(high:HI (match_operand 1 "" "")))]
  "check_pic (1)"
  "ldil L'%G1,%0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r")
	(lo_sum:HI (match_operand:HI 1 "register_operand" "r")
		   (match_operand 2 "immediate_operand" "i")))]
  ""
  "ldo R'%G2(%1),%0"
  [(set_attr "length" "1")])

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
  [(set (match_operand:QI 0 "reg_or_nonsymb_mem_operand" "=r,r,r,r,r,Q,*q,!*r,!fx,!fx")
	(match_operand:QI 1 "move_operand" "rM,J,N,K,Q,rM,rM,!fx,!*r,!fxM"))]
  "register_operand (operands[0], QImode)
   || reg_or_0_operand (operands[1], QImode)"
  "@
   copy %r1,%0
   ldi %1,%0
   ldil L'%1,%0
   zdepi %Z1,%0
   ldb%M1 %1,%0
   stb%M0 %r1,%0
   mtsar %r1
   fstws %1,-16(0,%%r30)\;ldw -16(0,%%r30),%0
   stw %1,-16(0,%%r30)\;fldws -16(0,%%r30),%0
   fcpy,sgl %r1,%0"
  [(set_attr "type" "move,move,move,move,load,store,move,load,fpload,fpalu")
   (set_attr "length" "1,1,1,1,1,1,1,2,2,1")])

(define_insn ""
  [(set (match_operand:QI 3 "register_operand" "=r")
	(mem:QI (plus:SI (match_operand:SI 1 "register_operand" "0")
			 (match_operand:SI 2 "int5_operand" "L"))))
   (set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "ldbs,mb %2(0,%0),%3"
  [(set_attr "type" "load")
   (set_attr "length" "1")])

(define_insn ""
  [(set (mem:QI (plus:SI (match_operand:SI 1 "register_operand" "0")
			 (match_operand:SI 2 "int5_operand" "L")))
	(match_operand:QI 3 "reg_or_0_operand" "rM"))
   (set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "stbs,mb %r3,%2(0,%0)"
  [(set_attr "type" "store")
   (set_attr "length" "1")])

;; The definition of this insn does not really explain what it does,
;; but it should suffice
;; that anything generated as this insn will be recognized as one
;; and that it will not successfully combine with anything.
(define_expand "movstrsi"
  [(parallel [(set (mem:BLK (match_operand:BLK 0 "general_operand" ""))
		   (mem:BLK (match_operand:BLK 1 "general_operand" "")))
	      (clobber (match_dup 0))
	      (clobber (match_dup 1))
	      (clobber (match_dup 4))
	      (clobber (match_dup 5))
	      (use (match_operand:SI 2 "arith_operand" ""))
	      (use (match_operand:SI 3 "const_int_operand" ""))])]
  ""
  "
{
  /* If the blocks are not at least word-aligned and rather big (>16 items),
     or the size is indeterminate, don't inline the copy code.  A
     procedure call is better since it can check the alignment at
     runtime and make the optimal decisions.  */
     if (INTVAL (operands[3]) < 4
	 && (GET_CODE (operands[2]) != CONST_INT
	     || (INTVAL (operands[2]) / INTVAL (operands[3]) > 16)))
       FAIL;

  operands[0] = copy_to_mode_reg (SImode, XEXP (operands[0], 0));
  operands[1] = copy_to_mode_reg (SImode, XEXP (operands[1], 0));
  operands[4] = gen_reg_rtx (SImode);
  operands[5] = gen_reg_rtx (SImode);
}")

;; The operand constraints are written like this to support both compile-time
;; and run-time determined byte count.  If the count is run-time determined,
;; the register with the byte count is clobbered by the copying code, and
;; therefore it is forced to operand 2.  If the count is compile-time
;; determined, we need two scratch registers for the unrolled code.
(define_insn ""
  [(set (mem:BLK (match_operand:SI 0 "register_operand" "+r,r"))
	(mem:BLK (match_operand:SI 1 "register_operand" "+r,r")))
   (clobber (match_dup 0))
   (clobber (match_dup 1))
   (clobber (match_operand:SI 2 "register_operand" "=r,r"))	;loop cnt/tmp
   (clobber (match_operand:SI 3 "register_operand" "=&r,&r"))	;item tmp
   (use (match_operand:SI 4 "arith_operand" "J,2"))	 ;byte count
   (use (match_operand:SI 5 "const_int_operand" "n,n"))] ;alignment
  ""
  "* return output_block_move (operands, !which_alternative);"
  [(set_attr "type" "multi,multi")])

;; Floating point move insns

;; This pattern forces (set (reg:DF ...) (const_double ...))
;; to be reloaded by putting the constant into memory.
;; It must come before the more general movdf pattern.
;; In the 3rd alternative case -- we know we will not be using a 
;; general register, so we can be sure length is just 1.
(define_insn ""
  [(set (match_operand:DF 0 "general_operand" "=?r,r,fx")
	(match_operand:DF 1 "" "?E,G,m"))]
  "GET_CODE (operands[1]) == CONST_DOUBLE
   && operands[1] != CONST0_RTX (DFmode)"
  "*
{
  switch (which_alternative)
    {
    case 0:
      return output_move_double (operands);
    case 1:
      return \"copy 0,%0\;copy 0,%R0\";
    case 2:
      return output_fp_move_double (operands);
    }
}"
  [(set_attr "type" "load,move,fpload")
   (set_attr "length" "3,2,1")])

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
  [(set (match_operand:DF 0 "reg_or_nonsymb_mem_operand"
			  "=fx,*r,Q,?Q,fx,*&r,?fx,?*r")
	(match_operand:DF 1 "reg_or_0_or_nonsymb_mem_operand"
			  "fxG,*rG,fx,*r,Q,Q,*r,fx"))]
  "register_operand (operands[0], DFmode)
   || reg_or_0_operand (operands[1], DFmode)"
  "*
{
  if (FP_REG_P (operands[0]) || FP_REG_P (operands[1]) 
      || operands[1] == CONST0_RTX (DFmode))
    return output_fp_move_double (operands);
  return output_move_double (operands);
}"
  [(set_attr "type" "fpalu,move,fpstore,store,fpload,load,fpload,load")
   (set_attr "length" "1,2,1,2,1,2,3,3")])

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=fx")
	(mem:DF (plus:SI (mult:SI (match_operand:SI 1 "register_operand" "r")
				  (const_int 8))
			 (match_operand:SI 2 "register_operand" "r"))))]
  "! TARGET_DISABLE_INDEXING"
  "flddx,s %1(0,%2),%0"
  [(set_attr "type" "fpload")
   (set_attr "length" "1")])

(define_insn ""
  [(set (mem:DF (plus:SI (mult:SI (match_operand:SI 1 "register_operand" "r")
				  (const_int 8))
			 (match_operand:SI 2 "register_operand" "r")))
	(match_operand:DF 0 "register_operand" "fx"))]
  "! TARGET_DISABLE_INDEXING"
  "fstdx,s %0,%1(0,%2)"
  [(set_attr "type" "fpstore")
   (set_attr "length" "1")])

(define_expand "movdi"
  [(set (match_operand:DI 0 "reg_or_nonsymb_mem_operand" "")
	(match_operand:DI 1 "general_operand" ""))]
  ""
  "
{
  if (emit_move_sequence (operands, DImode, 0))
    DONE;
}")

(define_expand "reload_indi"
  [(set (match_operand:DI 0 "register_operand" "=z")
	(match_operand:DI 1 "general_operand" ""))
   (clobber (match_operand:SI 2 "register_operand" "=&r"))]
  ""
  "
{
  if (emit_move_sequence (operands, DImode, operands[2]))
    DONE;

  /* We don't want the clobber emitted, so handle this ourselves.  */
  emit_insn (gen_rtx (SET, VOIDmode, operands[0], operands[1]));
  DONE;
}")

(define_expand "reload_outdi"
  [(set (match_operand:DI 0 "general_operand" "")
	(match_operand:DI 1 "register_operand" "z"))
   (clobber (match_operand:SI 2 "register_operand" "=&r"))]
  ""
  "
{
  if (emit_move_sequence (operands, DImode, operands[2]))
    DONE;

  /* We don't want the clobber emitted, so handle this ourselves.  */
  emit_insn (gen_rtx (SET, VOIDmode, operands[0], operands[1]));
  DONE;
}")

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
      output_asm_insn (\"ldil L'%1,%0\", operands);

      operands[0] = operand_subword (op0, 0, 0, DImode);
      if (INTVAL (op1) < 0)
	output_asm_insn (\"ldi -1,%0\", operands);
      else
	output_asm_insn (\"ldi 0,%0\", operands);
      return \"\";
    }
  else if (GET_CODE (op1) == CONST_DOUBLE)
    {
      operands[0] = operand_subword (op0, 1, 0, DImode);
      operands[1] = gen_rtx (CONST_INT, VOIDmode, CONST_DOUBLE_LOW (op1));
      output_asm_insn (\"ldil L'%1,%0\", operands);

      operands[0] = operand_subword (op0, 0, 0, DImode);
      operands[1] = gen_rtx (CONST_INT, VOIDmode, CONST_DOUBLE_HIGH (op1));
      output_asm_insn (singlemove_string (operands), operands);
      return \"\";
    }
  else
    abort ();
}"
  [(set_attr "type" "move")
   (set_attr "length" "2")])

;;; Experimental

(define_insn ""
  [(set (match_operand:DI 0 "fp_reg_operand" "=fx")
	(match_operand:DI 1 "short_memory_operand" "T"))]
  ""
  "fldds%F1 %1,%0"
  [(set_attr "type" "fpload")
   (set_attr "length" "1")])

(define_insn ""
  [(set (match_operand:DI 0 "short_memory_operand" "=T")
	(match_operand:DI 1 "fp_reg_operand" "fx"))]
  ""
  "fstds%F0 %1,%0"
  [(set_attr "type" "fpstore")
   (set_attr "length" "1")])

(define_insn ""
  [(set (match_operand:DI 0 "reg_or_nonsymb_mem_operand"
			  "=r,Q,&r,&r,fx,fx,*r")
	(match_operand:DI 1 "general_operand"
			  "rM,r,Q,i,*r,fxM,fx"))]
  "register_operand (operands[0], DImode)
   || reg_or_0_operand (operands[1], DImode)"
  "*
{
  if (FP_REG_P (operands[0]) || FP_REG_P (operands[1])
      || (operands[1] == CONST0_RTX (DImode)))
    return output_fp_move_double (operands);
  return output_move_double (operands);
}"
;; Use move in the last type..  This case happens often with xmpyu
;; and in nearly all cases we only access the data from the first 
;; of the two loads generated, and that can't stall on a data conflict
;; because of the second load.
  [(set_attr "type" "move,store,load,misc,fpload,fpalu,move")
   (set_attr "length" "2,3,3,3,3,1,3")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(lo_sum:DI (match_operand:DI 1 "register_operand" "0,r")
		   (match_operand:DI 2 "immediate_operand" "i,i")))]
  ""
  "*
{
  /* Don't output a 64 bit constant, since we can't trust the assembler to
     handle it correctly.  */
  if (GET_CODE (operands[2]) == CONST_DOUBLE)
    operands[2] = gen_rtx (CONST_INT, VOIDmode, CONST_DOUBLE_LOW (operands[2]));
  if (which_alternative == 1)
    output_asm_insn (\"copy %1,%0\", operands);
  return \"ldo R'%G2(%R1),%R0\";
}"
  ;; Need to set length for this arith insn because operand2
  ;; is not an "arith_operand".
  [(set_attr "length" "1,2")])

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
  [(set (match_operand:SF 0 "reg_or_nonsymb_mem_operand"
			  "=fx,r,*r,fx,fx,r,Q,Q")
	(match_operand:SF 1 "reg_or_0_or_nonsymb_mem_operand"
			  "fxG,rG,!fx,!*r,Q,Q,fx,rG"))]
  "register_operand (operands[0], SFmode)
   || reg_or_0_operand (operands[1], SFmode)"
  "@
   fcpy,sgl %r1,%0
   copy %r1,%0
   fstws %1,-16(0,%%r30)\;ldw -16(0,%%r30),%0
   stw %r1,-16(0,%%r30)\;fldws -16(0,%%r30),%0
   fldws%F1 %1,%0
   ldw%M1 %1,%0
   fstws%F0 %r1,%0
   stw%M0 %r1,%0"
  [(set_attr "type" "fpalu,move,load,fpload,fpload,load,fpstore,store")
   (set_attr "length" "1,1,2,2,1,1,1,1")])

(define_insn ""
  [(set (match_operand:SF 0 "register_operand" "=fx")
	(mem:SF (plus:SI (mult:SI (match_operand:SI 1 "register_operand" "r")
				  (const_int 4))
			 (match_operand:SI 2 "register_operand" "r"))))]
  "! TARGET_DISABLE_INDEXING"
  "fldwx,s %1(0,%2),%0"
  [(set_attr "type" "fpload")
   (set_attr "length" "1")])

(define_insn ""
  [(set (mem:SF (plus:SI (mult:SI (match_operand:SI 1 "register_operand" "r")
				  (const_int 4))
			 (match_operand:SI 2 "register_operand" "r")))
	(match_operand:SF 0 "register_operand" "fx"))]
  "! TARGET_DISABLE_INDEXING"
  "fstwx,s %0,%1(0,%2)"
  [(set_attr "type" "fpstore")
   (set_attr "length" "1")])

;;- zero extension instructions

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extend:SI
	 (match_operand:HI 1 "reg_or_nonsymb_mem_operand" "r,Q")))]
  ""
  "@
   extru %1,31,16,%0
   ldh%M1 %1,%0"
  [(set_attr "type" "unary,load")])

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(zero_extend:HI
	 (match_operand:QI 1 "reg_or_nonsymb_mem_operand" "r,Q")))]
  ""
  "@
   extru %1,31,8,%0
   ldb%M1 %1,%0"
  [(set_attr "type" "unary,load")
   (set_attr "length" "1,1")])

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extend:SI
	 (match_operand:QI 1 "reg_or_nonsymb_mem_operand" "r,Q")))]
  ""
  "@
   extru %1,31,8,%0
   ldb%M1 %1,%0"
  [(set_attr "type" "unary,load")
   (set_attr "length" "1,1")])

;;- sign extension instructions

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:HI 1 "register_operand" "r")))]
  ""
  "extrs %1,31,16,%0"
  [(set_attr "type" "unary")])

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(sign_extend:HI (match_operand:QI 1 "register_operand" "r")))]
  ""
  "extrs %1,31,8,%0"
  [(set_attr "type" "unary")])

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:QI 1 "register_operand" "r")))]
  ""
  "extrs %1,31,8,%0"
  [(set_attr "type" "unary")])

;; Conversions between float and double.

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=fx")
	(float_extend:DF
	 (match_operand:SF 1 "register_operand" "fx")))]
  ""
  "fcnvff,sgl,dbl %1,%0"
  [(set_attr "type" "fpalu")])

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "register_operand" "=fx")
	(float_truncate:SF
	 (match_operand:DF 1 "register_operand" "fx")))]
  ""
  "fcnvff,dbl,sgl %1,%0"
  [(set_attr "type" "fpalu")])

;; Conversion between fixed point and floating point.
;; Note that among the fix-to-float insns
;; the ones that start with SImode come first.
;; That is so that an operand that is a CONST_INT
;; (and therefore lacks a specific machine mode).
;; will be recognized as SImode (which is always valid)
;; rather than as QImode or HImode.

;; This pattern forces (set (reg:SF ...) (float:SF (const_int ...)))
;; to be reloaded by putting the constant into memory.
;; It must come before the more general floatsisf2 pattern.
;; Note length will always be 2 since we know we are loading
;; operand 1 from memory and the target is a FP register.
(define_insn ""
  [(set (match_operand:SF 0 "general_operand" "=fx")
	(float:SF (match_operand:SI 1 "const_int_operand" "m")))]
  ""
  "fldws %1,%0\;fcnvxf,sgl,sgl %0,%0"
  [(set_attr "type" "fpalu")
   (set_attr "length" "2")])

;; Note length will always be 1 since we only allow FP registers
;; for the source and target.
(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "general_operand" "=fx")
	(float:SF (match_operand:SI 1 "register_operand" "fx")))]
  ""
  "fcnvxf,sgl,sgl %1,%0"
  [(set_attr "type" "fpalu")
   (set_attr "length" "1")])

;; This pattern forces (set (reg:DF ...) (float:DF (const_int ...)))
;; to be reloaded by putting the constant into memory.
;; It must come before the more general floatsidf2 pattern.
;; Note length will always be 2 since we know we are loading
;; operand 1 from memory and the target is a FP register.
(define_insn ""
  [(set (match_operand:DF 0 "general_operand" "=fx")
	(float:DF (match_operand:SI 1 "const_int_operand" "m")))]
  ""
  "fldws %1,%0\;fcnvxf,sgl,dbl %0,%0"
  [(set_attr "type" "fpalu")
   (set_attr "length" "2")])

;; Note length will always be 1 since we only allow FP registers
;; for the source and target.
(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "general_operand" "=fx")
	(float:DF (match_operand:SI 1 "register_operand" "fx")))]
  ""
  "fcnvxf,sgl,dbl %1,%0"
  [(set_attr "type" "fpalu")
   (set_attr "length" "1")])

(define_expand "floatunssisf2"
  [(set (subreg:SI (match_dup 2) 1)
	(match_operand:SI 1 "register_operand" ""))
   (set (subreg:SI (match_dup 2) 0)
	(const_int 0))
   (set (match_operand:SF 0 "general_operand" "")
	(float:SF (match_dup 2)))]
  ""
  "operands[2] = gen_reg_rtx (DImode);")

(define_expand "floatunssidf2"
  [(set (subreg:SI (match_dup 2) 1)
	(match_operand:SI 1 "register_operand" ""))
   (set (subreg:SI (match_dup 2) 0)
	(const_int 0))
   (set (match_operand:DF 0 "general_operand" "")
	(float:DF (match_dup 2)))]
  ""
  "operands[2] = gen_reg_rtx (DImode);")

(define_insn "floatdisf2"
  [(set (match_operand:SF 0 "general_operand" "=fx")
	(float:SF (match_operand:DI 1 "register_operand" "fx")))]
  ""
  "fcnvxf,dbl,sgl %1,%0"
  [(set_attr "type" "fpalu")
   (set_attr "length" "1")])

(define_insn "floatdidf2"
  [(set (match_operand:DF 0 "general_operand" "=fx")
	(float:DF (match_operand:DI 1 "register_operand" "fx")))]
  ""
  "fcnvxf,dbl,dbl %1,%0"
  [(set_attr "type" "fpalu")
   (set_attr "length" "1")])

;; Convert a float to an actual integer.
;; Truncation is performed as part of the conversion.

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "=r,fx")
	(fix:SI (fix:SF (match_operand:SF 1 "register_operand" "fx,fx"))))
   (clobber (match_scratch:SI 2 "=&fx,X"))]
  ""
  "@
   fcnvfxt,sgl,sgl %1,%2\;fstws %2,-16(0,%%r30)\;ldw -16(0,%%r30),%0
   fcnvfxt,sgl,sgl %1,%0"
  [(set_attr "type" "fpalu,fpalu")
   (set_attr "length" "3,1")])

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "register_operand" "=r,fx")
	(fix:SI (fix:DF (match_operand:DF 1 "register_operand" "fx,fx"))))
   (clobber (match_scratch:SI 2 "=&fx,X"))]
  ""
  "@
   fcnvfxt,dbl,sgl %1,%2\;fstws %2,-16(0,%%r30)\;ldw -16(0,%%r30),%0
   fcnvfxt,dbl,sgl %1,%0"
  [(set_attr "type" "fpalu,fpalu")
   (set_attr "length" "3,1")])

(define_insn "fix_truncsfdi2"
  [(set (match_operand:DI 0 "register_operand" "=fx")
	(fix:DI (fix:SF (match_operand:SF 1 "register_operand" "fx"))))]
  ""
  "fcnvfxt,sgl,dbl %1,%0"
  [(set_attr "type" "fpalu")
   (set_attr "length" "1")])

(define_insn "fix_truncdfdi2"
  [(set (match_operand:DI 0 "register_operand" "=fx")
	(fix:DI (fix:DF (match_operand:DF 1 "register_operand" "fx"))))]
  ""
  "fcnvfxt,dbl,dbl %1,%0"
  [(set_attr "type" "fpalu")
   (set_attr "length" "1")])

;;- arithmetic instructions

(define_insn "adddi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI (match_operand:DI 1 "register_operand" "%r")
		 (match_operand:DI 2 "arith11_operand" "rI")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      if (INTVAL (operands[2]) >= 0)
	return \"addi %2,%R1,%R0\;addc %1,0,%0\";
      else
	return \"addi %2,%R1,%R0\;subb %1,0,%0\";
    }
  else
    return \"add %R2,%R1,%R0\;addc %2,%1,%0\";
}"
  [(set_attr "length" "2")])

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(plus:SI (match_operand:SI 1 "register_operand" "%r,r")
		 (match_operand:SI 2 "arith_operand" "r,J")))]
  ""
  "@
   add %1,%2,%0
   ldo %2(%1),%0")

(define_insn "subdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(minus:DI (match_operand:DI 1 "register_operand" "r")
		  (match_operand:DI 2 "register_operand" "r")))]
  ""
  "sub %R1,%R2,%R0\;subb %1,%2,%0"
  [(set_attr "length" "2")])

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(minus:SI (match_operand:SI 1 "arith11_operand" "r,I")
		  (match_operand:SI 2 "register_operand" "r,r")))]
  ""
  "@
   sub %1,%2,%0
   subi %1,%2,%0")

;; Clobbering a "register_operand" instead of a match_scratch
;; in operand3 of millicode calls avoids spilling %r1 and
;; produces better code.

;; The mulsi3 insns set up registers for the millicode call.
(define_expand "mulsi3"
  [(set (reg:SI 26) (match_operand:SI 1 "move_operand" ""))
   (set (reg:SI 25) (match_operand:SI 2 "move_operand" ""))
   (parallel [(set (reg:SI 29) (mult:SI (reg:SI 26) (reg:SI 25)))
	      (clobber (match_operand:SI 3 "register_operand" ""))
	      (clobber (reg:SI 26))
	      (clobber (reg:SI 25))
	      (clobber (reg:SI 31))])
   (set (match_operand:SI 0 "general_operand" "") (reg:SI 29))]
  ""
  "
{
  if (TARGET_SNAKE && ! TARGET_DISABLE_FPREGS)
    {
      rtx scratch = gen_reg_rtx (DImode);
      operands[1] = force_reg (SImode, operands[1]);
      operands[2] = force_reg (SImode, operands[2]);
      emit_insn (gen_umulsidi3 (scratch, operands[1], operands[2]));
      emit_insn (gen_rtx (SET, VOIDmode,
			  operands[0],
			  gen_rtx (SUBREG, SImode, scratch, 1)));
      DONE;
    }
  operands[3] = gen_reg_rtx(SImode);
}")

(define_insn "umulsidi3"
  [(set (match_operand:DI 0 "register_operand" "=x")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "x"))
		 (zero_extend:DI (match_operand:SI 2 "register_operand" "x"))))]
  "TARGET_SNAKE && ! TARGET_DISABLE_FPREGS"
  "xmpyu %1,%2,%0"
  [(set_attr "type" "fpmul")])

(define_insn ""
  [(set (reg:SI 29) (mult:SI (reg:SI 26) (reg:SI 25)))
   (clobber (match_operand:SI 0 "register_operand" "=a"))
   (clobber (reg:SI 26))
   (clobber (reg:SI 25))
   (clobber (reg:SI 31))]
  ""
  "* return output_mul_insn (0);"
  [(set_attr "type" "milli")])

;;; Division and mod.
(define_expand "divsi3"
  [(set (reg:SI 26) (match_operand:SI 1 "move_operand" ""))
   (set (reg:SI 25) (match_operand:SI 2 "move_operand" ""))
   (parallel [(set (reg:SI 29) (div:SI (reg:SI 26) (reg:SI 25)))
	      (clobber (match_operand:SI 3 "register_operand" ""))
	      (clobber (reg:SI 26))
	      (clobber (reg:SI 25))
	      (clobber (reg:SI 31))])
   (set (match_operand:SI 0 "general_operand" "") (reg:SI 29))]
  ""
  "
{
  operands[3] = gen_reg_rtx(SImode);
  if (!(GET_CODE (operands[2]) == CONST_INT && emit_hpdiv_const(operands, 0)))
    {
      emit_move_insn (gen_rtx (REG, SImode, 26), operands[1]);
      emit_move_insn (gen_rtx (REG, SImode, 25), operands[2]);
      emit
	(gen_rtx
	 (PARALLEL, VOIDmode,
	  gen_rtvec (5, gen_rtx (SET, VOIDmode, gen_rtx (REG, SImode, 29),
				 gen_rtx (DIV, SImode,
					  gen_rtx (REG, SImode, 26),
					  gen_rtx (REG, SImode, 25))),
		     gen_rtx (CLOBBER, VOIDmode, operands[3]),
		     gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 26)),
		     gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 25)),
		     gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 31)))));
      emit_move_insn (operands[0], gen_rtx (REG, SImode, 29));
    }
  DONE;
}")

(define_insn ""
  [(set (reg:SI 29)
    (div:SI (reg:SI 26) (match_operand:SI 0 "div_operand" "")))
   (clobber (match_operand:SI 1 "register_operand" "=a"))
   (clobber (reg:SI 26))
   (clobber (reg:SI 25))
   (clobber (reg:SI 31))]
 ""
 "*
 return output_div_insn (operands, 0);"
 [(set_attr "type" "milli")])

(define_expand "udivsi3"
  [(set (reg:SI 26) (match_operand:SI 1 "move_operand" ""))
   (set (reg:SI 25) (match_operand:SI 2 "move_operand" ""))
   (parallel [(set (reg:SI 29) (udiv:SI (reg:SI 26) (reg:SI 25)))
	      (clobber (match_operand:SI 3 "register_operand" ""))
	      (clobber (reg:SI 26))
	      (clobber (reg:SI 25))
	      (clobber (reg:SI 31))])
   (set (match_operand:SI 0 "general_operand" "") (reg:SI 29))]
  ""
  "
{
  operands[3] = gen_reg_rtx(SImode);
  if (!(GET_CODE (operands[2]) == CONST_INT && emit_hpdiv_const(operands, 1)))
    {
      emit_move_insn (gen_rtx (REG, SImode, 26), operands[1]);
      emit_move_insn (gen_rtx (REG, SImode, 25), operands[2]);
      emit
	(gen_rtx
	 (PARALLEL, VOIDmode,
	  gen_rtvec (5, gen_rtx (SET, VOIDmode, gen_rtx (REG, SImode, 29),
				 gen_rtx (UDIV, SImode,
					  gen_rtx (REG, SImode, 26),
					  gen_rtx (REG, SImode, 25))),
		     gen_rtx (CLOBBER, VOIDmode, operands[3]), 
		     gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 26)),
		     gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 25)),
		     gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 31)))));
      emit_move_insn (operands[0], gen_rtx (REG, SImode, 29));
    }
  DONE;
}")

(define_insn ""
  [(set (reg:SI 29)
    (udiv:SI (reg:SI 26) (match_operand:SI 0 "div_operand" "")))
   (clobber (match_operand:SI 1 "register_operand" "=a"))
   (clobber (reg:SI 26))
   (clobber (reg:SI 25))
   (clobber (reg:SI 31))]
 ""
 "*
 return output_div_insn (operands, 1);"
 [(set_attr "type" "milli")])

(define_expand "modsi3"
  [(set (reg:SI 26) (match_operand:SI 1 "move_operand" ""))
   (set (reg:SI 25) (match_operand:SI 2 "move_operand" ""))
   (parallel [(set (reg:SI 29) (mod:SI (reg:SI 26) (reg:SI 25)))
	      (clobber (match_operand:SI 3 "register_operand" ""))
	      (clobber (reg:SI 26))
	      (clobber (reg:SI 25))
	      (clobber (reg:SI 31))])
   (set (match_operand:SI 0 "general_operand" "") (reg:SI 29))]
  ""
  "
{
  operands[3] = gen_reg_rtx(SImode);
  emit_move_insn (gen_rtx (REG, SImode, 26), operands[1]);
  emit_move_insn (gen_rtx (REG, SImode, 25), operands[2]);
  emit
    (gen_rtx
     (PARALLEL, VOIDmode,
      gen_rtvec (5, gen_rtx (SET, VOIDmode, gen_rtx (REG, SImode, 29),
			     gen_rtx (MOD, SImode,
				      gen_rtx (REG, SImode, 26),
				      gen_rtx (REG, SImode, 25))),
		 gen_rtx (CLOBBER, VOIDmode, operands[3]), 
		 gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 26)),
		 gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 25)),
		 gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 31)))));
  emit_move_insn (operands[0], gen_rtx (REG, SImode, 29));
  DONE;
}")

(define_insn ""
  [(set (reg:SI 29) (mod:SI (reg:SI 26) (reg:SI 25)))
   (clobber (match_operand:SI 0 "register_operand" "=a"))
   (clobber (reg:SI 26))
   (clobber (reg:SI 25))
   (clobber (reg:SI 31))]
  ""
  "*
  return output_mod_insn (0);"
  [(set_attr "type" "milli")])

(define_expand "umodsi3"
  [(set (reg:SI 26) (match_operand:SI 1 "move_operand" ""))
   (set (reg:SI 25) (match_operand:SI 2 "move_operand" ""))
   (parallel [(set (reg:SI 29) (umod:SI (reg:SI 26) (reg:SI 25)))
	      (clobber (match_operand:SI 3 "register_operand" ""))
	      (clobber (reg:SI 26))
	      (clobber (reg:SI 25))
	      (clobber (reg:SI 31))])
   (set (match_operand:SI 0 "general_operand" "") (reg:SI 29))]
  ""
  "
{
  operands[3] = gen_reg_rtx(SImode);
  emit_move_insn (gen_rtx (REG, SImode, 26), operands[1]);
  emit_move_insn (gen_rtx (REG, SImode, 25), operands[2]);
  emit
    (gen_rtx
     (PARALLEL, VOIDmode,
      gen_rtvec (5, gen_rtx (SET, VOIDmode, gen_rtx (REG, SImode, 29),
			     gen_rtx (UMOD, SImode,
				      gen_rtx (REG, SImode, 26),
				      gen_rtx (REG, SImode, 25))),
		 gen_rtx (CLOBBER, VOIDmode, operands[3]), 
		 gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 26)),
		 gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 25)),
		 gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 31)))));
  emit_move_insn (operands[0], gen_rtx (REG, SImode, 29));
  DONE;
}")

(define_insn ""
  [(set (reg:SI 29) (umod:SI (reg:SI 26) (reg:SI 25)))
   (clobber (match_operand:SI 0 "register_operand" "=a"))
   (clobber (reg:SI 26))
   (clobber (reg:SI 25))
   (clobber (reg:SI 31))]
  ""
  "*
  return output_mod_insn (1);"
  [(set_attr "type" "milli")])

;;- and instructions
;; We define DImode `and` so with DImode `not` we can get
;; DImode `andn`.  Other combinations are possible.

(define_expand "anddi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(and:DI (match_operand:DI 1 "arith_double_operand" "")
		(match_operand:DI 2 "arith_double_operand" "")))]
  ""
  "
{
  if (! register_operand (operands[1], DImode)
      || ! register_operand (operands[2], DImode))
    /* Let GCC break this into word-at-a-time operations.  */
    FAIL;
}")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(and:DI (match_operand:DI 1 "register_operand" "%r")
		(match_operand:DI 2 "register_operand" "r")))]
  ""
  "and %1,%2,%0\;and %R1,%R2,%R0"
  [(set_attr "length" "2")])

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(and:SI (match_operand:SI 1 "register_operand" "%r,0")
		(match_operand:SI 2 "and_operand" "rO,P")))]
  ""
  "* return output_and (operands); ")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(and:DI (not:DI (match_operand:DI 1 "register_operand" "r"))
		(match_operand:DI 2 "register_operand" "r")))]
  ""
  "andcm %2,%1,%0\;andcm %R2,%R1,%R0"
  [(set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (not:SI (match_operand:SI 1 "register_operand" "r"))
		(match_operand:SI 2 "register_operand" "r")))]
  ""
  "andcm %2,%1,%0")

(define_expand "iordi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(ior:DI (match_operand:DI 1 "arith_double_operand" "")
		(match_operand:DI 2 "arith_double_operand" "")))]
  ""
  "
{
  if (! register_operand (operands[1], DImode)
      || ! register_operand (operands[2], DImode))
    /* Let GCC break this into word-at-a-time operations.  */
    FAIL;
}")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ior:DI (match_operand:DI 1 "register_operand" "%r")
		(match_operand:DI 2 "register_operand" "r")))]
  ""
  "or %1,%2,%0\;or %R1,%R2,%R0"
  [(set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (match_operand:SI 1 "register_operand" "0")
		(match_operand:SI 2 "ior_operand" "")))]
  ""
  "* return output_ior (operands); ")

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (match_operand:SI 1 "register_operand" "%r")
		(match_operand:SI 2 "register_operand" "r")))]
  ""
  "* return output_ior (operands); ")

(define_expand "xordi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(xor:DI (match_operand:DI 1 "arith_double_operand" "")
		(match_operand:DI 2 "arith_double_operand" "")))]
  ""
  "
{
  if (! register_operand (operands[1], DImode)
      || ! register_operand (operands[2], DImode))
    /* Let GCC break this into word-at-a-time operations.  */
    FAIL;
}")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(xor:DI (match_operand:DI 1 "register_operand" "%r")
		(match_operand:DI 2 "register_operand" "r")))]
  ""
  "xor %1,%2,%0\;xor %R1,%R2,%R0"
  [(set_attr "length" "2")])

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(xor:SI (match_operand:SI 1 "register_operand" "%r")
		(match_operand:SI 2 "register_operand" "r")))]
  ""
  "xor %1,%2,%0")

(define_insn "negdi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(neg:DI (match_operand:DI 1 "register_operand" "r")))]
  ""
  "sub 0,%R1,%R0\;subb 0,%1,%0"
  [(set_attr "type" "unary")
   (set_attr "length" "2")])

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "sub 0,%1,%0"
  [(set_attr "type" "unary")])

(define_expand "one_cmpldi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(not:DI (match_operand:DI 1 "arith_double_operand" "")))]
  ""
  "
{
  if (! register_operand (operands[1], DImode))
    FAIL;
}")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(not:DI (match_operand:DI 1 "register_operand" "r")))]
  ""
  "uaddcm 0,%1,%0\;uaddcm 0,%R1,%R0"
  [(set_attr "type" "unary")
   (set_attr "length" "2")])

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(not:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "uaddcm 0,%1,%0"
  [(set_attr "type" "unary")])

;; Floating point arithmetic instructions.

(define_insn "adddf3"
  [(set (match_operand:DF 0 "register_operand" "=fx")
	(plus:DF (match_operand:DF 1 "register_operand" "fx")
		 (match_operand:DF 2 "register_operand" "fx")))]
  ""
  "fadd,dbl %1,%2,%0"
  [(set_attr "type" "fpalu")])

(define_insn "addsf3"
  [(set (match_operand:SF 0 "register_operand" "=fx")
	(plus:SF (match_operand:SF 1 "register_operand" "fx")
		 (match_operand:SF 2 "register_operand" "fx")))]
  ""
  "fadd,sgl %1,%2,%0"
  [(set_attr "type" "fpalu")])

(define_insn "subdf3"
  [(set (match_operand:DF 0 "register_operand" "=fx")
	(minus:DF (match_operand:DF 1 "register_operand" "fx")
		  (match_operand:DF 2 "register_operand" "fx")))]
  ""
  "fsub,dbl %1,%2,%0"
  [(set_attr "type" "fpalu")])

(define_insn "subsf3"
  [(set (match_operand:SF 0 "register_operand" "=fx")
	(minus:SF (match_operand:SF 1 "register_operand" "fx")
		  (match_operand:SF 2 "register_operand" "fx")))]
  ""
  "fsub,sgl %1,%2,%0"
  [(set_attr "type" "fpalu")])

(define_insn "muldf3"
  [(set (match_operand:DF 0 "register_operand" "=fx")
	(mult:DF (match_operand:DF 1 "register_operand" "fx")
		 (match_operand:DF 2 "register_operand" "fx")))]
  ""
  "fmpy,dbl %1,%2,%0"
  [(set_attr "type" "fpmul")])

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "=fx")
	(mult:SF (match_operand:SF 1 "register_operand" "fx")
		 (match_operand:SF 2 "register_operand" "fx")))]
  ""
  "fmpy,sgl %1,%2,%0"
  [(set_attr "type" "fpmul")])

(define_insn "divdf3"
  [(set (match_operand:DF 0 "register_operand" "=fx")
	(div:DF (match_operand:DF 1 "register_operand" "fx")
		(match_operand:DF 2 "register_operand" "fx")))]
  ""
  "fdiv,dbl %1,%2,%0"
  [(set_attr "type" "fpdivdbl")])

(define_insn "divsf3"
  [(set (match_operand:SF 0 "register_operand" "=fx")
	(div:SF (match_operand:SF 1 "register_operand" "fx")
		(match_operand:SF 2 "register_operand" "fx")))]
  ""
  "fdiv,sgl %1,%2,%0"
  [(set_attr "type" "fpdivsgl")])

(define_insn "negdf2"
  [(set (match_operand:DF 0 "register_operand" "=fx")
	(neg:DF (match_operand:DF 1 "register_operand" "fx")))]
  ""
  "fsub,dbl 0,%1,%0"
  [(set_attr "type" "fpalu")])

(define_insn "negsf2"
  [(set (match_operand:SF 0 "register_operand" "=fx")
	(neg:SF (match_operand:SF 1 "register_operand" "fx")))]
  ""
  "fsub,sgl 0,%1,%0"
  [(set_attr "type" "fpalu")])

(define_insn "absdf2"
  [(set (match_operand:DF 0 "register_operand" "=fx")
	(abs:DF (match_operand:DF 1 "register_operand" "fx")))]
  ""
  "fabs,dbl %1,%0"
  [(set_attr "type" "fpalu")])

(define_insn "abssf2"
  [(set (match_operand:SF 0 "register_operand" "=fx")
	(abs:SF (match_operand:SF 1 "register_operand" "fx")))]
  ""
  "fabs,sgl %1,%0"
  [(set_attr "type" "fpalu")])

(define_insn "sqrtdf2"
  [(set (match_operand:DF 0 "register_operand" "=fx")
	(sqrt:DF (match_operand:DF 1 "register_operand" "fx")))]
  ""
  "fsqrt,dbl %1,%0"
  [(set_attr "type" "fpsqrtdbl")])

(define_insn "sqrtsf2"
  [(set (match_operand:SF 0 "register_operand" "=fx")
	(sqrt:SF (match_operand:SF 1 "register_operand" "fx")))]
  ""
  "fsqrt,sgl %1,%0"
  [(set_attr "type" "fpsqrtsgl")])

;;- Shift instructions

;; Optimized special case of shifting.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lshiftrt:SI (match_operand:SI 1 "memory_operand" "m")
		     (const_int 24)))]
  ""
  "ldb%M1 %1,%0"
  [(set_attr "type" "load")
   (set_attr "length" "1")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lshiftrt:SI (match_operand:SI 1 "memory_operand" "m")
		     (const_int 16)))]
  ""
  "ldh%M1 %1,%0"
  [(set_attr "type" "load")
   (set_attr "length" "1")])

;; Using shadd_operand works around a bug in reload.  For 2.4 fix
;; reload and use register_operand instead.
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (mult:SI (match_operand:SI 2 "register_operand" "r")
			  (const_int 2))
		 (match_operand:SI 1 "shadd_operand" "r")))]
  ""
  "sh1add %2,%1,%0")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (mult:SI (match_operand:SI 2 "register_operand" "r")
			  (const_int 4))
		 (match_operand:SI 1 "shadd_operand" "r")))]
  ""
  "sh2add %2,%1,%0")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (mult:SI (match_operand:SI 2 "register_operand" "r")
			  (const_int 8))
		 (match_operand:SI 1 "shadd_operand" "r")))]
  ""
  "sh3add %2,%1,%0")

(define_expand "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(ashift:SI (match_operand:SI 1 "arith5_operand" "")
		   (match_operand:SI 2 "arith32_operand" "")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    {
      rtx temp = gen_reg_rtx (SImode);
      emit_insn (gen_subsi3 (temp, GEN_INT (31), operands[2]));
      emit_insn (gen_zvdep32 (operands[0], operands[1], temp));
      DONE;
    }
  /* Make sure both inputs are not constants, 
     the recognizer can't handle that.  */
  operands[1] = force_reg (SImode, operands[1]);
}")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashift:SI (match_operand:SI 1 "register_operand" "r")
		   (match_operand:SI 2 "const_int_operand" "n")))]
  ""
  "*
{
  rtx xoperands[4];
  xoperands[0] = operands[0];
  xoperands[1] = operands[1];
  xoperands[2] = GEN_INT (31 - (INTVAL (operands[2]) & 31));
  xoperands[3] = GEN_INT (32 - (INTVAL (operands[2]) & 31));
  output_asm_insn (\"zdep %1,%2,%3,%0\", xoperands);
  return \"\";
}")

(define_insn "zvdep32"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(ashift:SI (match_operand:SI 1 "arith5_operand" "r,L")
		   (minus:SI (const_int 31)
			     (match_operand:SI 2 "register_operand" "q,q"))))]
  ""
  "@
   zvdep %1,32,%0
   zvdepi %1,32,%0")

(define_expand "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "")
		     (match_operand:SI 2 "arith32_operand" "")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    {
      rtx temp = gen_reg_rtx (SImode);
      emit_insn (gen_subsi3 (temp, GEN_INT (31), operands[2]));
      emit_insn (gen_vextrs32 (operands[0], operands[1], temp));
      DONE;
    }
}")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "r")
		     (match_operand:SI 2 "const_int_operand" "n")))]
  ""
  "*
{
  rtx xoperands[4];
  xoperands[0] = operands[0];
  xoperands[1] = operands[1];
  xoperands[2] = GEN_INT (31 - (INTVAL (operands[2]) & 31));
  xoperands[3] = GEN_INT (32 - (INTVAL (operands[2]) & 31));
  output_asm_insn (\"extrs %1,%2,%3,%0\", xoperands);
  return \"\";
}")

(define_insn "vextrs32"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "r")
		     (minus:SI (const_int 31)
			       (match_operand:SI 2 "register_operand" "q"))))]
  ""
  "vextrs %1,32,%0")

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
		     (match_operand:SI 2 "arith32_operand" "qn")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      operands[3] = GEN_INT (32 - (INTVAL (operands[2]) & 31));
      operands[2] = GEN_INT (31 - (INTVAL (operands[2]) & 31));
      return \"extru %1,%2,%3,%0\";
    }
  else
    return \"vshd 0,%1,%0\";
}")

(define_insn "rotrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(rotatert:SI (match_operand:SI 1 "register_operand" "r")
		     (match_operand:SI 2 "arith32_operand" "qn")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT)
    return \"shd %1,%1,%2,%0\";
  else
    return \"vshd %1,%1,%0\";
}")

(define_insn "rotlsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(rotate:SI (match_operand:SI 1 "register_operand" "r")
		   (match_operand:SI 2 "const_int_operand" "n")))]
  ""
  "*
{
  operands[2] = GEN_INT ((32 - INTVAL (operands[2])) & 31);
  return \"shd %1,%1,%2,%0\";
}")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operator:SI 5 "plus_xor_ior_operator"
	  [(ashift:SI (match_operand:SI 1 "register_operand" "r")
		      (match_operand:SI 3 "const_int_operand" "n"))
	   (lshiftrt:SI (match_operand:SI 2 "register_operand" "r")
			(match_operand:SI 4 "const_int_operand" "n"))]))]
  "INTVAL (operands[3]) + INTVAL (operands[4]) == 32"
  "shd %1,%2,%4,%0"
  [(set_attr "type" "binary")
   (set_attr "length" "1")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operator:SI 5 "plus_xor_ior_operator"
	  [(lshiftrt:SI (match_operand:SI 2 "register_operand" "r")
			(match_operand:SI 4 "const_int_operand" "n"))
	   (ashift:SI (match_operand:SI 1 "register_operand" "r")
		      (match_operand:SI 3 "const_int_operand" "n"))]))]
  "INTVAL (operands[3]) + INTVAL (operands[4]) == 32"
  "shd %1,%2,%4,%0"
  [(set_attr "type" "binary")
   (set_attr "length" "1")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (ashift:SI (match_operand:SI 1 "register_operand" "r")
			   (match_operand:SI 2 "const_int_operand" ""))
		(match_operand:SI 3 "const_int_operand" "")))]
  "exact_log2 (1 + (INTVAL (operands[3]) >> (INTVAL (operands[2]) & 31))) >= 0"
  "*
{
  int cnt = INTVAL (operands[2]) & 31;
  operands[3] = GEN_INT (exact_log2 (1 + (INTVAL (operands[3]) >> cnt)));
  operands[2] = GEN_INT (31 - cnt);
  return \"zdep %1,%2,%3,%0\";
}")


;; Unconditional and other jump instructions.

(define_insn "jump"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  ""
  "bl%* %l0,0"
  [(set_attr "type" "branch")])

;; Subroutines of "casesi".
;; operand 0 is index
;; operand 1 is the minimum bound
;; operand 2 is the maximum bound - minimum bound + 1
;; operand 3 is CODE_LABEL for the table;
;; operand 4 is the CODE_LABEL to go to if index out of range.

(define_expand "casesi"
  [(match_operand:SI 0 "general_operand" "")
   (match_operand:SI 1 "const_int_operand" "")
   (match_operand:SI 2 "const_int_operand" "")
   (match_operand 3 "" "")
   (match_operand 4 "" "")]
  ""
  "
{
  if (GET_CODE (operands[0]) != REG)
    operands[0] = force_reg (SImode, operands[0]);

  if (operands[1] != const0_rtx)
    {
      rtx reg = gen_reg_rtx (SImode);

      operands[1] = GEN_INT (-INTVAL (operands[1]));
      if (!INT_14_BITS (operands[1]))
	operands[1] = force_reg (SImode, operands[1]);
      emit_insn (gen_addsi3 (reg, operands[0], operands[1]));

      operands[0] = reg;
    }

  if (!INT_11_BITS (operands[2]))
    operands[2] = force_reg (SImode, operands[2]);

  emit_jump_insn (gen_casesi0 (operands[0], operands[2],
			       operands[3], operands[4]));
  DONE;
}")

(define_insn "casesi0"
  [(set (pc)
	(if_then_else (leu (match_operand:SI 0 "register_operand" "r")
			   (match_operand:SI 1 "arith11_operand" "rI"))
		      (plus:SI (mem:SI (plus:SI (pc) (match_dup 0)))
			       (label_ref (match_operand 2 "" "")))
		      (pc)))
   (use (label_ref (match_operand 3 "" "")))]
  ""
  "*
{
  if (GET_CODE (operands[1]) == CONST_INT)
    {
      operands[1] = GEN_INT (~INTVAL (operands[1]));
      return \"addi,uv %1,%0,0\;blr,n %0,0\;b,n %l3\";
    }
  else
    {
      return \"sub,>> %0,%1,0\;blr,n %0,0\;b,n %l3\";
    }
}"
 [(set_attr "length" "3")])


;; Need nops for the calls because execution is supposed to continue
;; past; we don't want to nullify an instruction that we need.
;;- jump to subroutine

(define_expand "call"
 [(parallel [(call (match_operand:SI 0 "" "")
		   (match_operand 1 "" ""))
	     (clobber (reg:SI 31))
	     (clobber (reg:SI 2))])]
 ""
 "
{
  rtx op;
  
  if (TARGET_LONG_CALLS) 
    op = force_reg (SImode, XEXP (operands[0], 0));
  else
    op = XEXP (operands[0], 0);
  emit_call_insn (gen_call_internal (op, operands[1]));
  if (flag_pic)
    {
      if (!hppa_save_pic_table_rtx)
	hppa_save_pic_table_rtx = gen_reg_rtx (Pmode);
      emit_insn (gen_rtx (SET, VOIDmode,
			  gen_rtx (REG, Pmode, 19), hppa_save_pic_table_rtx));
    }
  DONE;
}")

(define_insn "call_internal"
 [(call (mem:SI (match_operand:SI 0 "call_operand_address" "r,S"))
	(match_operand 1 "" "i,i"))
  (clobber (reg:SI 31))
  (clobber (reg:SI 2))]
 ""
 "*
{
  if (which_alternative == 0)
    return \"copy %0,22\;.CALL\\tARGW0=GR\;bl $$dyncall,31\;copy 31,2\";
  else
    {
      output_arg_descriptor (insn);
      return \"bl %0,2%#\";
    }
}"
 [(set_attr "type" "dyncall,call")
  (set_attr "length" "3,1")])

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "" "")
		   (call (match_operand:SI 1 "" "")
			 (match_operand 2 "" "")))
	      (clobber (reg:SI 31))
	      (clobber (reg:SI 2))])]
  ;;- Don't use operand 1 for most machines.
  ""
  "
{
  rtx op;
  
  if (TARGET_LONG_CALLS) 
    op = force_reg (SImode, XEXP (operands[1], 0));
  else
    op = XEXP (operands[1], 0);
  emit_call_insn (gen_call_value_internal (operands[0], op, operands[2]));
  if (flag_pic)
    {
      if (!hppa_save_pic_table_rtx)
	hppa_save_pic_table_rtx = gen_reg_rtx (Pmode);
      emit_insn (gen_rtx (SET, VOIDmode,
			  gen_rtx (REG, Pmode, 19), hppa_save_pic_table_rtx));
    }
  DONE;
}")

(define_insn "call_value_internal"
  [(set (match_operand 0 "" "=rfx,rfx")
	(call (mem:SI (match_operand:SI 1 "call_operand_address" "r,S"))
	      (match_operand 2 "" "i,i")))
   (clobber (reg:SI 31))
   (clobber (reg:SI 2))]
  ;;- Don't use operand 1 for most machines.
  ""
  "*
{
  if (which_alternative == 0)
    return \"copy %1,22\;.CALL\\tARGW0=GR\;bl $$dyncall,31\;copy 31,2\";
  else
    {
      output_arg_descriptor (insn);
      return \"bl %1,2%#\";
    }
}"
 [(set_attr "type" "dyncall,call")
  (set_attr "length" "3,1")])

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop")

;;; Hope this is only within a function...
(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))]
  ""
 "bv%* 0(%0)"
 [(set_attr "type" "branch")])

(define_insn "extzv"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extract:SI (match_operand:SI 1 "register_operand" "r")
			 (match_operand:SI 2 "uint5_operand" "")
			 (match_operand:SI 3 "uint5_operand" "")))]
  ""
  "extru %1,%3+%2-1,%2,%0")

(define_insn "extv"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extract:SI (match_operand:SI 1 "register_operand" "r")
			 (match_operand:SI 2 "uint5_operand" "")
			 (match_operand:SI 3 "uint5_operand" "")))]
  ""
  "extrs %1,%3+%2-1,%2,%0")

(define_insn "insv"
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+r,r")
			 (match_operand:SI 1 "uint5_operand" "")
			 (match_operand:SI 2 "uint5_operand" ""))
	(match_operand:SI 3 "arith5_operand" "r,L"))]
  ""
  "@
   dep %3,%2+%1-1,%1,%0
   depi %3,%2+%1-1,%1,%0")

;; Optimize insertion of const_int values of type 1...1xxxx.
(define_insn ""
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+r")
			 (match_operand:SI 1 "uint5_operand" "")
			 (match_operand:SI 2 "uint5_operand" ""))
	(match_operand:SI 3 "const_int_operand" ""))]
  "(INTVAL (operands[3]) & 0x10) != 0 &&
   (~INTVAL (operands[3]) & (1L << INTVAL (operands[1])) - 1 & ~0xf) == 0"
  "*
{
  operands[3] = GEN_INT ((INTVAL (operands[3]) & 0xf) - 0x10);
  return \"depi %3,%2+%1-1,%1,%0\";
}")

;; This insn is used for some loop tests, typically loops reversed when
;; strength reduction is used.  It is actually created when the instruction
;; combination phase combines the special loop test.  Since this insn
;; is both a jump insn and has an output, it must deal with it's own
;; reloads, hence the `m' constraints.  The `!' constraints direct reload
;; to not choose the register alternatives in the event a reload is needed.

(define_insn "decrement_and_branch_until_zero"
  [(set (pc)
	(if_then_else
	  (ge (plus:SI (match_operand:SI 0 "register_operand" "+!r,m")
		       (const_int -1))
	      (const_int 0))
	  (label_ref (match_operand 1 "" ""))
	  (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int -1)))
   (clobber (match_scratch:SI 2 "=X,r"))]
  "find_reg_note (insn, REG_NONNEG, 0)"
"*
{
  if (which_alternative == 0)
    if (get_attr_length (insn) == 1)
      return \"addib,>= -1,%0,%1%#\";
    else
      return \"addi,< -1,%0,%0\;bl %1,0%#\";
  else
    {
      output_asm_insn (\"ldw %0,%2\;ldo -1(%2),%2\;stw %2,%0\", operands);
      if (get_attr_length (insn) == 4)
	return \"comb,> 0,%2,%1%#\";
      else
	return \"comclr,<= 0,%2,0\;bl %1,0%#\";
    }
}"
[(set_attr "type" "cbranch")
 (set (attr "length")
      (if_then_else (eq (symbol_ref "which_alternative") (const_int 0))
		    (if_then_else (lt (abs (minus (match_dup 1)
						  (plus (pc) (const_int 2))))
				      (const_int 1023))
				  (const_int 1)
				  (const_int 2))
		    (if_then_else (lt (match_dup 1)
				      (pc))
				  (if_then_else
				   (lt (abs (minus (match_dup 1)
						   (plus (pc)
							 (const_int 5))))
				       (const_int 1023))
				   (const_int 4)
				   (const_int 5))
				  (if_then_else
				   (lt (abs (minus (match_dup 1)
						   (plus (pc)
							 (const_int 2))))
				       (const_int 1023))
				   (const_int 4)
				   (const_int 5)))))])


;; The next four peepholes take advantage of the new 5 operand 
;; fmpy{add,sub} instructions available on 1.1 CPUS.  Basically
;; fmpyadd performs a multiply and add/sub of independent operands
;; at the same time.  Because the operands must be independent
;; combine will not try to combine such insns...  Thus we have
;; to use a peephole.
(define_peephole
  [(set (match_operand 0 "register_operand" "=fx")
	(mult (match_operand 1 "register_operand" "fx")
	      (match_operand 2 "register_operand" "fx")))
   (set (match_operand 3 "register_operand" "+fx")
	(plus (match_operand 4 "register_operand" "fx")
	      (match_operand 5 "register_operand" "fx")))]
  "TARGET_SNAKE && fmpyaddoperands (operands)"
  "*
{
  if (GET_MODE (operands[0]) == DFmode)
    {
      if (rtx_equal_p (operands[5], operands[3]))
	return \"fmpyadd,dbl %1,%2,%0,%4,%3\";
      else
	return \"fmpyadd,dbl %1,%2,%0,%5,%3\";
    }
  else
    {
      if (rtx_equal_p (operands[5], operands[3]))
	return \"fmpyadd,sgl %1,%2,%0,%4,%3\";
      else
	return \"fmpyadd,sgl %1,%2,%0,%5,%3\";
    }
}")


(define_peephole 
  [(set (match_operand 3 "register_operand" "+fx")
	(plus (match_operand 4 "register_operand" "fx")
	      (match_operand 5 "register_operand" "fx")))
   (set (match_operand 0 "register_operand" "=fx")
	(mult (match_operand 1 "register_operand" "fx")
	      (match_operand 2 "register_operand" "fx")))]
  "TARGET_SNAKE && fmpyaddoperands (operands)"
  "*
{
  if (GET_MODE (operands[0]) == DFmode)
    {
      if (rtx_equal_p (operands[3], operands[5]))
	return \"fmpyadd,dbl %1,%2,%0,%4,%3\";
      else
	return \"fmpyadd,dbl %1,%2,%0,%5,%3\";
    }
  else
    {
      if (rtx_equal_p (operands[3], operands[5]))
	return \"fmpyadd,sgl %1,%2,%0,%4,%3\";
      else
	return \"fmpyadd,sgl %1,%2,%0,%5,%3\";
    }
}")

;; Note fsub subtracts the second operand from the first while fmpysub
;; does the opposite for the subtraction operands!
(define_peephole
  [(set (match_operand 0 "register_operand" "=fx")
	(mult (match_operand 1 "register_operand" "fx")
	      (match_operand 2 "register_operand" "fx")))
   (set (match_operand 3 "register_operand" "+fx")
	(minus (match_operand 4 "register_operand" "fx")
	       (match_operand 5 "register_operand" "fx")))]
  "TARGET_SNAKE && fmpysuboperands (operands)"
  "*
{
  if (GET_MODE (operands[0]) == DFmode)
    return \"fmpysub,dbl %1,%2,%0,%5,%3\";
  else
    return \"fmpysub,sgl %1,%2,%0,%5,%3\";
}")

(define_peephole
  [(set (match_operand 3 "register_operand" "+fx")
	(minus (match_operand 4 "register_operand" "fx")
	       (match_operand 5 "register_operand" "fx")))
   (set (match_operand 0 "register_operand" "=fx")
	(mult (match_operand 1 "register_operand" "fx")
	      (match_operand 2 "register_operand" "fx")))]
  "TARGET_SNAKE && fmpysuboperands (operands)"
  "*
{
  if (GET_MODE (operands[0]) == DFmode)
    return \"fmpysub,dbl %1,%2,%0,%5,%3\";
  else
    return \"fmpysub,sgl %1,%2,%0,%5,%3\";
}")
