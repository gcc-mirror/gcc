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
  "move,unary,binary,compare,load,store,branch,cbranch,call,dyncall,address,fpload,fpstore,fpalu,fpcc,fpmul,fpdivsgl,fpdivdbl,fpsqrtsgl,fpsqrtdbl,multi,misc,milli"
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

(define_attr "in_call_delay" "false,true"
  (cond [(eq_attr "type" "branch,cbranch,call,dyncall,multi,milli")
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

(define_attr "in_milli_delay" "false,true"
  (cond [(eq_attr "length" "!1")
	 (const_string "false")

	 (eq_attr "type" "branch,cbranch,call,dyncall,milli")
	 (const_string "false")

	 (ne (symbol_ref "use_milli_regs (insn)") (const_int 0))
	 (const_string "false")]
	(const_string "true")))

(define_delay (eq_attr "type" "call")
  [(eq_attr "in_call_delay" "true") (nil) (nil)])

(define_attr "in_branch_delay" "false,true"
  (if_then_else (and (eq_attr "type" "!branch,cbranch,call,multi,milli")
		     (eq_attr "length" "1"))
		(const_string "true")
		(const_string "false")))

(define_delay (eq_attr "type" "branch")
  [(eq_attr "in_branch_delay" "true")
   (eq_attr "in_branch_delay" "true") (nil)])

(define_delay (eq_attr "type" "cbranch")
  [(eq_attr "in_branch_delay" "true") (nil) (nil)])

(define_delay (eq_attr "type" "milli")
  [(eq_attr "in_milli_delay" "true") (nil) (nil)])

;; Function units of the HPPA. The following data is for the "Snake"
;; (Mustang CPU + Timex FPU) because that's what I have the docs for.
;; Scheduling instructions for PA-83 machines according to the Snake
;; constraints shouldn't hurt.

;; (define_function_unit {name} {num-units} {n-users} {test}
;;                       {ready-delay} {busy-delay} [{conflict-list}])

;; The integer ALU.
;; (Noted only for documentation; units that take one cycle do not need to
;; be specified.)

;; (define_function_unit "alu" 1 0
;;  (eq_attr "type" "unary,binary,move,address") 1 0)


;; Memory. Disregarding Cache misses, the Mustang memory times are:
;; load: 1
;; store, fpstore: 3, no D-cache operations should be scheduled.
;; fpload: 3 (really 2 for flops, but I don't think we can specify that).

(define_function_unit "memory" 1 1 (eq_attr "type" "load") 1 0)
(define_function_unit "memory" 1 1 (eq_attr "type" "store,fpstore") 3 0)
(define_function_unit "memory" 1 1 (eq_attr "type" "fpload") 3 0)

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
	(compare:CCFP (match_operand:SF 0 "register_operand" "")
		      (match_operand:SF 1 "register_operand" "")))]
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
      (compare:CCFP (match_operand:DF 0 "register_operand" "")
                    (match_operand:DF 1 "register_operand" "")))]
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
			    [(match_operand:SF 0 "register_operand" "fx")
			     (match_operand:SF 1 "register_operand" "fx")]))]
 ""
 "fcmp,sgl,%Y2 %0,%1"
 [(set_attr "type" "fpcc")])

(define_insn ""
 [(set (reg:CCFP 0)
       (match_operator:CCFP 2 "comparison_operator"
			    [(match_operand:DF 0 "register_operand" "fx")
			     (match_operand:DF 1 "register_operand" "fx")]))]
 ""
 "fcmp,dbl,%Y2 %0,%1"
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
	(eq:CC (match_dup 1)
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
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ne:CC (match_dup 1)
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
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lt:CC (match_dup 1)
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
  [(set (match_operand:SI 0 "register_operand" "=r")
	(gt:CC (match_dup 1)
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
  [(set (match_operand:SI 0 "register_operand" "=r")
	(le:CC (match_dup 1)
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
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ge:CC (match_dup 1)
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
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ltu:CC (match_dup 1)
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
  [(set (match_operand:SI 0 "register_operand" "=r")
	(gtu:CC (match_dup 1)
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
  [(set (match_operand:SI 0 "register_operand" "=r")
	(leu:CC (match_dup 1)
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
  [(set (match_operand:SI 0 "register_operand" "=r")
	(geu:CC (match_dup 1)
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

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(match_operator:CC 3 "comparison_operator"
			   [(match_operand:SI 1 "register_operand" "r,r")
			    (match_operand:SI 2  "arith11_operand" "r,I")]))]
  ""
  "*
{
  if (which_alternative == 0)
    return \"comclr,%N3 %1,%2,%0\;ldi 1,%0\";
  else
    {
      if (!(GET_CODE (operands[3]) == EQ || GET_CODE (operands[3]) == NE))
	PUT_CODE (operands[3], reverse_relop (GET_CODE (operands[3])));
      output_asm_insn (\"comiclr,%N3 %2,%1,%0\;ldi 1,%0\", operands);
      return \"\";
    }
}"
  [(set_attr "type" "binary")
   (set_attr "length" "2")])

;; Conditionals

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
  if (which_alternative == 0)
    return (get_attr_length (insn) == 1
	    ? \"comb,%C3 %1,%2,%0%#\" : \"comclr,%N3 %1,%2,0\;bl %0,0%#\");
    {
      enum rtx_code comp_code = GET_CODE (operands[3]);
      if (!(comp_code == EQ || comp_code == NE))
	PUT_CODE (operands[3], reverse_relop (comp_code));
      if (get_attr_length (insn) == 1)
	output_asm_insn (\"comib,%C3 %2,%1,%0%#\", operands);
      else
	output_asm_insn (\"comiclr,%N3 %2,%1,0\;bl %0,0%#\", operands);
      return \"\";
    }
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
  if (which_alternative == 0)
    return (get_attr_length (insn) == 1
	    ? \"comb,%N3 %1,%2,%0%#\" : \"comclr,%C3 %1,%2,0\;bl %0,0%#\");
    {
      enum rtx_code comp_code = GET_CODE (operands[3]);
      if (!(comp_code == EQ || comp_code == NE))
	PUT_CODE (operands[3], reverse_relop (comp_code));
      if (get_attr_length (insn) == 1)
	output_asm_insn (\"comib,%N3 %2,%1,%0%#\", operands);
      else
	output_asm_insn (\"comiclr,%C3 %2,%1,0%#\;bl %0,0%#\", operands);
      return \"\";
    }
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
  "ftest\;bl%* %0,0"
  [(set_attr "type" "cbranch")
   (set_attr "length" "2")])

(define_insn ""
  [(set (pc) (if_then_else (ne (reg:CCFP 0) (const_int 0))
			   (pc)
			   (label_ref (match_operand 0 "" ""))))]
  ""
  "ftest\;add,tr 0,0,0\;bl%* %0,0"
  [(set_attr "type" "cbranch")
   (set_attr "length" "3")])

;; Move instructions

(define_expand "movsi"
  [(set (match_operand:SI 0 "general_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
  "
{
  if (emit_move_sequence (operands, SImode))
    DONE;
}")

;; Moves to and from the shift register.

(define_insn ""
  [(set (reg:SI 112)
	(match_operand:SI 0 "register_operand" "r"))]
  ""
  "mtsar %0"
  [(set_attr "type" "move")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(reg:SI 112))]
  ""
  "mfctl 11,%0"
  [(set_attr "type" "move")])

(define_insn ""
  [(set (match_operand:SI 0 "reg_or_nonsymb_mem_operand"
			  "=r,r,Q,!r,!*f*x,!*f*x")
	(match_operand:SI 1 "move_operand" "rM,Q,rM,!*f*x*y,!r,!*f*x"))]
  ""
  "@
   copy %r1,%0
   ldw%M1 %1,%0
   stw%M0 %r1,%0
   fstws %1,-16(30)\;ldw -16(30),%0
   stw %1,-16(30)\;fldws -16(30),%0
   fcpy,sgl %1,%0"
  [(set_attr "type" "move,load,store,move,move,fpalu")
   (set_attr "length" "1,1,1,2,2,1")])

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
  output_asm_insn (\"ldo R'%1(1),%0\", xoperands);
  return \"\";
  }
"
  [(set_attr "type" "multi")
   (set_attr "length" "3")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operand:SI 1 "immediate_operand" "n"))]
  "(GET_CODE (operands[1]) == CONST_INT) &&
   (INT_14_BITS (operands[1]) || !(INTVAL (operands[1]) & 0x7ff))"
  "*
{
  if (INT_14_BITS (operands[1]))
    return \"ldo %1(0),%0\";
  else
    return \"ldil L'%1,%0\";
}"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=a,?*r")
	(plus:SI (match_operand:SI 1 "register_operand" "r,r")
		 (high:SI (match_operand 2 "" ""))))]
  ""
  "@
   addil L'%G2,%1
   ldil L'%G2,%0\;add %0,%1,%0"
  [(set_attr "type" "binary")
   (set_attr "length" "1,2")])

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (match_operand:SI 1 "register_operand" "")
		 (high:SI (match_operand 2 "" ""))))]
  "reload_completed && REGNO (operands[0]) != 1"
  [(set (match_dup 0) (high:SI (match_dup 2)))
   (set (match_dup 0) (plus:SI (match_dup 0) (match_dup 1)))]
  "")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(high:SI (match_operand 1 "" "")))]
  "check_pic (1)"
  "ldil L'%G1,%0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r")
	(high:HI (match_operand 1 "" "")))]
  "check_pic (1)"
  "ldil L'%G1,%0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lo_sum:SI (match_operand:SI 1 "register_operand" "r")
		   (match_operand:SI 2 "immediate_operand" "in")))]
  ""
  "ldo R'%G2(%1),%0"
  ;; Need to set length for this arith insn because operand2
  ;; is not an "arith_operand".
  [(set_attr "length" "1")])

;;; Experimental

(define_insn ""
  [(set (match_operand:SI 0 "fp_reg_operand" "*f*x")
	(match_operand:SI 1 "short_memory_operand" "T"))]
  ""
  "fldws%F1 %1,%0"
  [(set_attr "type" "fpload")
   (set_attr "length" "1")])

(define_insn ""
  [(set (match_operand:SI 0 "short_memory_operand" "T")
	(match_operand:SI 1 "fp_reg_operand" "*f*x"))]
  ""
  "fstws%F0 %1,%0"
  [(set_attr "type" "fpstore")
   (set_attr "length" "1")])

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  "
{
  if (emit_move_sequence (operands, HImode))
    DONE;
}")

(define_insn ""
  [(set (match_operand:HI 0 "reg_or_nonsymb_mem_operand" "=r,r,Q")
	(match_operand:HI 1 "move_operand" "rM,Q,rM"))]
  ""
  "@
   copy %r1,%0
   ldh%M1 %1,%0
   sth%M0 %r1,%0"
  [(set_attr "type" "move,load,store")
   (set_attr "length" "1,1,1")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r")
	(match_operand:HI 1 "immediate_operand" "n"))]
  "(GET_CODE (operands[1]) == CONST_INT) &&
   (INT_14_BITS (operands[1]) || !(INTVAL (operands[1]) & 0x7ff))"
  "*
{
  if (INT_14_BITS (operands[1]))
    return \"ldo %1(0),%0\";
  else
    return \"ldil L'%1,%0\";
}"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r")
	(lo_sum:HI (match_operand:HI 1 "register_operand" "r")
		   (match_operand 2 "immediate_operand" "in")))]
  ""
  "ldo R'%G2(%1),%0"
  [(set_attr "length" "1")])

(define_expand "movqi"
  [(set (match_operand:QI 0 "general_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
  "
{
  if (emit_move_sequence (operands, QImode))
    DONE;
}")

(define_insn ""
  [(set (match_operand:QI 0 "reg_or_nonsymb_mem_operand" "=r,r,Q")
	(match_operand:QI 1 "move_operand" "rM,Q,rM"))]
  ""
  "@
   copy %r1,%0
   ldb%M1 %1,%0
   stb%M0 %r1,%0"
  [(set_attr "type" "move,load,store")
   (set_attr "length" "1,1,1")])

(define_insn ""
  [(set (match_operand:QI 0 "register_operand" "=r")
	(match_operand:QI 1 "immediate_operand" "J"))]
  ""
  "ldo %1(0),%0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn ""
  [(set (match_operand:QI 0 "register_operand" "=r")
	(subreg:QI (lo_sum:SI (match_operand:QI 1 "register_operand" "r")
			      (match_operand 2 "immediate_operand" "in")) 0))]
  ""
  "ldo R'%G2(%1),%0"
  [(set_attr "length" "1")])

;; The definition of this insn does not really explain what it does,
;; but it should suffice
;; that anything generated as this insn will be recognized as one
;; and that it will not successfully combine with anything.
(define_expand "movstrsi"
  [(parallel [(set (mem:BLK (match_operand:BLK 0 "general_operand" ""))
		   (mem:BLK (match_operand:BLK 1 "general_operand" "")))
	      (clobber (match_dup 0))
	      (clobber (match_dup 1))
	      (clobber (match_scratch:SI 4 ""))
	      (clobber (match_scratch:SI 5 ""))
	      (use (match_operand:SI 2 "arith_operand" ""))
	      (use (match_operand:SI 3 "const_int_operand" ""))])]
  ""
  "
{
  /* If the blocks are not word-aligned and rather big (>16 items),
     or the size is indeterminate, don't inline the copy code.  A
     procedure call is better since it can check the alignment at
     runtime and make the optimal decisions.  */
     if (INTVAL (operands[3]) != 4
	 && (GET_CODE (operands[2]) != CONST_INT
	     || (INTVAL (operands[2]) / INTVAL (operands[3]) > 16)))
       FAIL;

  operands[0] = copy_to_mode_reg (SImode, XEXP (operands[0], 0));
  operands[1] = copy_to_mode_reg (SImode, XEXP (operands[1], 0));
  operands[2] = force_not_mem (operands[2]);
}")

;; The operand constraints are written like this to support both compile-time
;; and run-time determined byte count.  If the count is run-time determined,
;; the register with the byte count is clobbered by the copying code, and
;; therefore it is forced to operand 2.  If the count is compile-time
;; determined, we need two scratch registers for the unrolled code.
(define_insn ""
  [(set (mem:BLK (match_operand:SI 0 "register_operand" "r,r"))
	(mem:BLK (match_operand:SI 1 "register_operand" "r,r")))
   (clobber (match_dup 0))
   (clobber (match_dup 1))
   (clobber (match_scratch:SI 2 "=r,r"))		 ;loop cnt/item tmp
   (clobber (match_scratch:SI 3 "=r,r"))		 ;item tmp
   (use (match_operand:SI 4 "arith_operand" "J,2"))	 ;byte count
   (use (match_operand:SI 5 "const_int_operand" "n,n"))] ;alignment
  ""
  "* return output_block_move (operands, !which_alternative);"
  [(set_attr "type" "multi")])

;; Floating point move insns

;; This pattern forces (set (reg:DF ...) (const_double ...))
;; to be reloaded by putting the constant into memory.
;; It must come before the more general movdf pattern.
(define_insn ""
  [(set (match_operand:DF 0 "general_operand" "=?r,r,fx")
	(match_operand:DF 1 "" "?E,G,m"))]
  "GET_CODE (operands[1]) == CONST_DOUBLE"
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
   (set_attr "length" "3,2,3")])

(define_expand "movdf"
  [(set (match_operand:DF 0 "general_operand" "")
	(match_operand:DF 1 "general_operand" ""))]
  ""
  "
{
  if (emit_move_sequence (operands, DFmode))
    DONE;
}")

(define_insn ""
  [(set (match_operand:DF 0 "reg_or_nonsymb_mem_operand"
			  "=fx,r,Q,Q,fx,&r,?fx,?r")
	(match_operand:DF 1 "reg_or_nonsymb_mem_operand"
			  "fx,r,fx,r,Q,Q,r,fx"))]
  ""
  "*
{
  if (FP_REG_P (operands[0]) || FP_REG_P (operands[1]))
    return output_fp_move_double (operands);
  return output_move_double (operands);
}"
  [(set_attr "type" "fpalu,move,fpstore,store,fpload,load,multi,multi")
   (set_attr "length" "1,2,1,2,1,2,3,3")])

(define_expand "movdi"
  [(set (match_operand:DI 0 "reg_or_nonsymb_mem_operand" "")
	(match_operand:DI 1 "general_operand" ""))]
  ""
  "
{
  if (emit_move_sequence (operands, DImode))
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
	output_asm_insn (\"ldo -1(0),%0\", operands);
      else
	output_asm_insn (\"ldo 0(0),%0\", operands);
    }
  else if (GET_CODE (op1) == CONST_DOUBLE)
    {
      operands[0] = operand_subword (op0, 1, 0, DImode);
      operands[1] = gen_rtx (CONST_INT, VOIDmode, CONST_DOUBLE_LOW (op1));
      output_asm_insn (\"ldil L'%1,%0\", operands);

      operands[0] = operand_subword (op0, 0, 0, DImode);
      operands[1] = gen_rtx (CONST_INT, VOIDmode, CONST_DOUBLE_HIGH (op1));
      output_asm_insn (singlemove_string (operands), operands);
    }
  else
    abort ();
}"
  [(set_attr "type" "move")
   (set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:DI 0 "reg_or_nonsymb_mem_operand"
			  "=r,Q,&r,&r,*f*x,*f*x,*f*x,r,Q")
	(match_operand:DI 1 "general_operand"
			  "r,r,Q,i,r,*f*x,Q,*f*x,*f*x"))]
  ""
  "*
{
  if (FP_REG_P (operands[0]) || FP_REG_P (operands[1]))
    return output_fp_move_double (operands);
  return output_move_double (operands);
}"
  [(set_attr "type" "move,store,load,misc,multi,fpalu,fpload,multi,fpstore")
   (set_attr "length" "2,3,3,3,3,2,3,3,3")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(lo_sum:DI (match_operand:DI 1 "register_operand" "0,r")
		   (match_operand:DI 2 "immediate_operand" "in,in")))]
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
  if (emit_move_sequence (operands, SFmode))
    DONE;
}")

(define_insn ""
  [(set (match_operand:SF 0 "reg_or_nonsymb_mem_operand"
			  "=fx,r,r,fx,fx,r,Q,Q")
	(match_operand:SF 1 "reg_or_nonsymb_mem_operand"
			  "fx,r,!fx,!r,Q,Q,fx,r"))]
  ""
  "@
   fcpy %1,%0
   copy %1,%0
   fstws %1,-16(0,30)\;ldw -16(0,30),%0
   stw %r1,-16(0,30)\;fldws -16(0,30),%0
   fldws%F1 %1,%0
   ldw%M1 %1,%0
   fstws%F0 %r1,%0
   stw%M0 %r1,%0"
  [(set_attr "type" "fpalu,move,multi,multi,fpload,load,fpstore,store")
   (set_attr "length" "1,1,2,2,1,1,1,1")])


;;- zero extension instructions

;; Note that the one starting from HImode comes before those for QImode
;; so that a constant operand will match HImode, not QImode.

(define_expand "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(zero_extend:SI
	 (match_operand:HI 1 "general_operand" "")))]
  ""
  "
{
  if (GET_CODE (operand1) == MEM
      && symbolic_operand (XEXP (operand1, 0), Pmode))
    {
      rtx temp = copy_to_mode_reg (Pmode, gen_rtx (HIGH, Pmode,
						   XEXP (operand1, 0)));
      operands[1] = gen_rtx (MEM, HImode,
			     gen_rtx (LO_SUM, Pmode,
				      temp, XEXP (operand1, 0)));
    }
}")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extend:SI
	 (match_operand:HI 1 "reg_or_nonsymb_mem_operand" "r,Q")))]
  ""
  "@
   extru %1,31,16,%0
   ldh%M1 %1,%0"
  [(set_attr "type" "unary,load")])

(define_expand "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "")
	(zero_extend:HI
	 (match_operand:QI 1 "general_operand" "")))]
  ""
  "
{
  if (GET_CODE (operand1) == MEM
      && symbolic_operand (XEXP (operand1, 0), Pmode))
    {
      rtx temp = copy_to_mode_reg (Pmode, gen_rtx (HIGH, Pmode,
						   XEXP (operand1, 0)));
      operands[1] = gen_rtx (MEM, QImode,
			     gen_rtx (LO_SUM, Pmode,
				      temp, XEXP (operand1, 0)));
    }
}")

(define_insn ""
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(zero_extend:HI
	 (match_operand:QI 1 "reg_or_nonsymb_mem_operand" "r,Q")))]
  ""
  "@
   extru %1,31,8,%0
   ldb%M1 %1,%0"
  [(set_attr "type" "unary,load")
   (set_attr "length" "1")])

(define_expand "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(zero_extend:SI
	 (match_operand:QI 1 "general_operand" "")))]
  ""
  "
{
  if (GET_CODE (operand1) == MEM
      && symbolic_operand (XEXP (operand1, 0), Pmode))
    {
      rtx temp = copy_to_mode_reg (Pmode, gen_rtx (HIGH, Pmode,
						   XEXP (operand1, 0)));
      operand1 = gen_rtx (MEM, QImode,
			  gen_rtx (LO_SUM, Pmode,
				   temp, XEXP (operand1, 0)));
      emit_insn (gen_rtx (SET, VOIDmode, operand0,
			  gen_rtx (ZERO_EXTEND, SImode, operand1)));
      DONE;
    }
}")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extend:SI
	 (match_operand:QI 1 "reg_or_nonsymb_mem_operand" "r,Q")))]
  ""
  "@
   extru %1,31,8,%0
   ldb%M1 %1,%0"
  [(set_attr "type" "unary,load")
   (set_attr "length" "1")])

;;- sign extension instructions
;; Note that the one starting from HImode comes before those for QImode
;; so that a constant operand will match HImode, not QImode.

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
(define_insn ""
  [(set (match_operand:SF 0 "general_operand" "=fx")
	(float:SF (match_operand:SI 1 "const_int_operand" "m")))]
  ""
  "* return output_floatsisf2 (operands);"
  [(set_attr "type" "fpalu")
   (set_attr "length" "3")])

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "general_operand" "=fx")
	(float:SF (match_operand:SI 1 "register_operand" "fxr")))]
  ""
  "* return output_floatsisf2 (operands);"
  [(set_attr "type" "fpalu")
   (set_attr "length" "3")])

;; This pattern forces (set (reg:DF ...) (float:DF (const_int ...)))
;; to be reloaded by putting the constant into memory.
;; It must come before the more general floatsidf2 pattern.
(define_insn ""
  [(set (match_operand:DF 0 "general_operand" "=fx")
	(float:DF (match_operand:SI 1 "const_int_operand" "m")))]
  ""
  "* return output_floatsidf2 (operands);"
  [(set_attr "type" "fpalu")
   (set_attr "length" "3")])

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "general_operand" "=fx")
	(float:DF (match_operand:SI 1 "register_operand" "fxr")))]
  ""
  "* return output_floatsidf2 (operands);"
  [(set_attr "type" "fpalu")
   (set_attr "length" "3")])

;; Convert a float to an actual integer.
;; Truncation is performed as part of the conversion.

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "=r,fx")
	(fix:SI (fix:SF (match_operand:SF 1 "register_operand" "fx,fx"))))
   (clobber (match_scratch:SI 2 "=&fx,X"))]
  ""
  "@
   fcnvfxt,sgl,sgl %1,%2\;fstws %2,-16(30)\;ldw -16(30),%0
   fcnvfxt,sgl,sgl %1,%0"
  [(set_attr "type" "fpalu,fpalu")
   (set_attr "length" "3,1")])

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "register_operand" "=r,fx")
	(fix:SI (fix:DF (match_operand:DF 1 "register_operand" "fx,fx"))))
   (clobber (match_scratch:SI 2 "=&fx,X"))]
  ""
  "@
   fcnvfxt,dbl,sgl %1,%2\;fstws %2,-16(30)\;ldw -16(30),%0
   fcnvfxt,dbl,sgl %1,%0"
  [(set_attr "type" "fpalu,fpalu")
   (set_attr "length" "3,1")])


;;- arithmetic instructions

(define_insn "adddi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI (match_operand:DI 1 "register_operand" "%r")
		 (match_operand:DI 2 "register_operand" "r")))]
  ""
  "add %R1,%R2,%R0\;addc %1,%2,%0"
  [(set_attr "type" "binary")
   (set_attr "length" "2")])

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

;; The mulsi3 insns set up registers for the millicode call.

(define_expand "mulsi3"
  [(set (reg:SI 26) (match_operand:SI 1 "srcsi_operand" ""))
   (set (reg:SI 25) (match_operand:SI 2 "srcsi_operand" ""))
   (parallel [(set (reg:SI 29) (mult:SI (reg:SI 26) (reg:SI 25)))
	      (clobber (match_scratch:SI 3 ""))
	      (clobber (reg:SI 26))
	      (clobber (reg:SI 25))
	      (clobber (reg:SI 31))])
   (set (match_operand:SI 0 "general_operand" "") (reg:SI 29))]
  ""
  "")

(define_insn ""
  [(set (reg:SI 29) (mult:SI (reg:SI 26) (reg:SI 25)))
   (clobber (match_scratch:SI 0 "=a"))
   (clobber (reg:SI 26))
   (clobber (reg:SI 25))
   (clobber (reg:SI 31))]
  ""
  "* return output_mul_insn (0);"
  [(set_attr "type" "milli")])

;;; Division and mod.

(define_expand "divsi3"
  [(set (reg:SI 26) (match_operand:SI 1 "srcsi_operand" ""))
   (set (reg:SI 25) (match_operand:SI 2 "srcsi_operand" ""))
   (parallel [(set (reg:SI 29) (div:SI (reg:SI 26) (reg:SI 25)))
	      (clobber (match_scratch:SI 3 ""))
	      (clobber (reg:SI 26))
	      (clobber (reg:SI 25))
	      (clobber (reg:SI 31))])
   (set (match_operand:SI 0 "general_operand" "") (reg:SI 29))]
  ""
  "
{
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
		     gen_rtx (CLOBBER, VOIDmode, gen_rtx (SCRATCH, SImode, 0)),
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
   (clobber (match_scratch:SI 1 "=a"))
   (clobber (reg:SI 26))
   (clobber (reg:SI 25))
   (clobber (reg:SI 31))]
 ""
 "*
 return output_div_insn (operands, 0);"
 [(set_attr "type" "milli")])

(define_expand "udivsi3"
  [(set (reg:SI 26) (match_operand:SI 1 "srcsi_operand" ""))
   (set (reg:SI 25) (match_operand:SI 2 "srcsi_operand" ""))
   (parallel [(set (reg:SI 29) (udiv:SI (reg:SI 26) (reg:SI 25)))
	      (clobber (match_scratch:SI 3 ""))
	      (clobber (reg:SI 26))
	      (clobber (reg:SI 25))
	      (clobber (reg:SI 31))])
   (set (match_operand:SI 0 "general_operand" "") (reg:SI 29))]
  ""
  "
{
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
		     gen_rtx (CLOBBER, VOIDmode, gen_rtx (SCRATCH, SImode, 0)),
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
   (clobber (match_scratch:SI 1 "=a"))
   (clobber (reg:SI 26))
   (clobber (reg:SI 25))
   (clobber (reg:SI 31))]
 ""
 "*
 return output_div_insn (operands, 1);"
 [(set_attr "type" "milli")])

(define_expand "modsi3"
  [(set (reg:SI 26) (match_operand:SI 1 "srcsi_operand" ""))
   (set (reg:SI 25) (match_operand:SI 2 "srcsi_operand" ""))
   (parallel [(set (reg:SI 29) (mod:SI (reg:SI 26) (reg:SI 25)))
	      (clobber (match_scratch:SI 3 ""))
	      (clobber (reg:SI 26))
	      (clobber (reg:SI 25))
	      (clobber (reg:SI 31))])
   (set (match_operand:SI 0 "general_operand" "") (reg:SI 29))]
  ""
  "
{
  emit_move_insn (gen_rtx (REG, SImode, 26), operands[1]);
  emit_move_insn (gen_rtx (REG, SImode, 25), operands[2]);
  emit
    (gen_rtx
     (PARALLEL, VOIDmode,
      gen_rtvec (5, gen_rtx (SET, VOIDmode, gen_rtx (REG, SImode, 29),
			     gen_rtx (MOD, SImode,
				      gen_rtx (REG, SImode, 26),
				      gen_rtx (REG, SImode, 25))),
		 gen_rtx (CLOBBER, VOIDmode, gen_rtx (SCRATCH, SImode, 0)),
		 gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 26)),
		 gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 25)),
		 gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 31)))));
  emit_move_insn (operands[0], gen_rtx (REG, SImode, 29));
  DONE;
}")
 
(define_insn ""
  [(set (reg:SI 29) (mod:SI (reg:SI 26) (reg:SI 25)))
   (clobber (match_scratch:SI 0 "=a"))
   (clobber (reg:SI 26))
   (clobber (reg:SI 25))
   (clobber (reg:SI 31))]
  ""
  "*
  return output_mod_insn (0);"
  [(set_attr "type" "milli")])

(define_expand "umodsi3"
  [(set (reg:SI 26) (match_operand:SI 1 "srcsi_operand" ""))
   (set (reg:SI 25) (match_operand:SI 2 "srcsi_operand" ""))
   (parallel [(set (reg:SI 29) (umod:SI (reg:SI 26) (reg:SI 25)))
	      (clobber (match_scratch:SI 3 ""))
	      (clobber (reg:SI 26))
	      (clobber (reg:SI 25))
	      (clobber (reg:SI 31))])
   (set (match_operand:SI 0 "general_operand" "") (reg:SI 29))]
  ""
  "
{
  emit_move_insn (gen_rtx (REG, SImode, 26), operands[1]);
  emit_move_insn (gen_rtx (REG, SImode, 25), operands[2]);
  emit
    (gen_rtx
     (PARALLEL, VOIDmode,
      gen_rtvec (5, gen_rtx (SET, VOIDmode, gen_rtx (REG, SImode, 29),
			     gen_rtx (UMOD, SImode,
				      gen_rtx (REG, SImode, 26),
				      gen_rtx (REG, SImode, 25))),
		 gen_rtx (CLOBBER, VOIDmode, gen_rtx (SCRATCH, SImode, 0)),
		 gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 26)),
		 gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 25)),
		 gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 31)))));
  emit_move_insn (operands[0], gen_rtx (REG, SImode, 29));
  DONE;
}")

(define_insn ""
  [(set (reg:SI 29) (umod:SI (reg:SI 26) (reg:SI 25)))
   (clobber (match_scratch:SI 0 "=a"))
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
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (match_operand:SI 1 "register_operand" "%r")
		(match_operand:SI 2 "register_operand" "r")))]
  ""
  "and %1,%2,%0")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(and:DI (match_operand:DI 1 "register_operand" "r")
		(not:DI (match_operand:DI 2 "register_operand" "r"))))]
  ""
  "andcm %2,%1,%0\;andcm %R2,%R1,%R0"
  [(set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (match_operand:SI 1 "register_operand" "%r")
		(not:SI (match_operand:SI 2 "register_operand" "r"))))]
  ""
  "andcm %1,%2,%0")


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

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (match_operand:SI 1 "register_operand" "%r")
		(match_operand:SI 2 "register_operand" "r")))]
  ""
  "or %1,%2,%0")

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
  "xor %r1,%2,%0")

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
  "@
   sub 0,%1,%0"
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
	(not:DI (match_operand:DI 1 "arith_double_operand" "r")))]
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
  "fsub,sgl 0, %1,%0"
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
  "ldb%M1 %1,%0")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (mult:SI (match_operand:SI 2 "register_operand" "r")
			  (const_int 2))
		 (match_operand:SI 1 "register_operand" "r")))]
  ""
  "sh1add %2,%1,%0")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (mult:SI (match_operand:SI 2 "register_operand" "r")
			  (const_int 4))
		 (match_operand:SI 1 "register_operand" "r")))]
  ""
  "sh2add %2,%1,%0")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (mult:SI (match_operand:SI 2 "register_operand" "r")
			  (const_int 8))
		 (match_operand:SI 1 "register_operand" "r")))]
  ""
  "sh3add %2,%1,%0")

(define_insn "sar_sub"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(if_then_else (gtu:SI (match_operand:SI 2 "register_operand" "r")
			      (match_operand:SI 1 "int11_operand" "I"))
		      (const_int 0)
		      (minus:SI (match_dup 1) (match_dup 2))))]
  ""
  "subi,>>= %1,%2,%0\;copy 0,%0"
  [(set_attr "length" "2" )])

(define_expand "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(ashift:SI (match_operand:SI 1 "register_operand" "")
		   (match_operand:SI 2 "arith5_operand" "")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    {
      rtx temp = gen_reg_rtx (SImode);
      emit_insn (gen_sar_sub (temp,
			      gen_rtx (CONST_INT, VOIDmode, 31),
			      operands[2]));
      emit_insn (gen_rtx (SET, VOIDmode, gen_rtx (REG, SImode, 112), temp));
      emit_insn (gen_rtx (SET, VOIDmode,
			  operands[0],
			  gen_rtx (ASHIFT, SImode,
				   operands[1],
				   gen_rtx (MINUS, SImode,
					    gen_rtx (CONST_INT, VOIDmode, 31),
					    gen_rtx (REG, SImode, 112)))));
      DONE;
    }
}")

(define_insn ""
 [(set (match_operand:SI 0 "register_operand" "=r")
       (ashift:SI (match_operand:SI 1 "register_operand" "r")
		  (match_operand:SI 2 "int5_operand" "L")))]
 ""
 "*
{
  rtx xoperands[4];
  xoperands[0] = operands[0];  xoperands[1] = operands[1];
  xoperands[2] = gen_rtx (CONST_INT, VOIDmode,
			  31 - INTVAL (operands[2]));
  xoperands[3] = gen_rtx (CONST_INT, VOIDmode,
			  32 - INTVAL (operands[2]));
  output_asm_insn (\"zdep %1,%2,%3,%0\", xoperands);
  return \"\";
}")

(define_insn ""
 [(set (match_operand:SI 0 "register_operand" "=r")
       (ashift:SI (match_operand:SI 1 "register_operand" "r")
		  (minus:SI (const_int 31)
			    (reg:SI 112))))]
 ""
 "zvdep %1,32,%0")

(define_expand "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "")
		     (match_operand:SI 2 "arith5_operand" "")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    {
      rtx temp = gen_reg_rtx (SImode);
      emit_insn (gen_sar_sub (temp,
			      gen_rtx (CONST_INT, VOIDmode, 31),
			      operands[2]));
      emit_insn (gen_rtx (SET, VOIDmode, gen_rtx (REG, SImode, 112), temp));
      emit_insn (gen_rtx (SET, VOIDmode,
			  operands[0],
			  gen_rtx (ASHIFTRT, SImode,
				   operands[1],
				   gen_rtx (MINUS, SImode,
					    gen_rtx (CONST_INT, VOIDmode, 31),
					    gen_rtx (REG, SImode, 112)))));
      DONE;
    }
}")

(define_insn ""
 [(set (match_operand:SI 0 "register_operand" "=r")
       (ashiftrt:SI (match_operand:SI 1 "register_operand" "r")
		    (match_operand:SI 2 "int5_operand" "L")))]
 ""
 "*
{
  rtx xoperands[4];
  xoperands[0] = operands[0];  xoperands[1] = operands[1];
  xoperands[2] = gen_rtx (CONST_INT, VOIDmode,
			  31 - INTVAL (operands[2]));
  xoperands[3] = gen_rtx (CONST_INT, VOIDmode,
			  32 - INTVAL (operands[2]));
  output_asm_insn (\"extrs %1,%2,%3,%0\", xoperands);
  return \"\";
}")


(define_insn ""
 [(set (match_operand:SI 0 "register_operand" "=r")
       (ashiftrt:SI (match_operand:SI 1 "register_operand" "r")
		  (minus:SI (const_int 31)
			    (reg:SI 112))))]
 ""
 "vextrs %1,32,%0")

(define_expand "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "")
		     (match_operand:SI 2 "arith5_operand" "")))]
  ""
  "
{
  if (GET_CODE (operands[2]) != CONST_INT)
    {
      rtx temp = gen_reg_rtx (SImode);
      emit_insn (gen_sar_sub (temp,
			      gen_rtx (CONST_INT, VOIDmode, 31),
			      operands[2]));
      emit_insn (gen_rtx (SET, VOIDmode, gen_rtx (REG, SImode, 112), temp));
      emit_insn (gen_rtx (SET, VOIDmode,
			  operands[0],
			  gen_rtx (LSHIFTRT, SImode,
				   operands[1],
				   gen_rtx (MINUS, SImode,
					    gen_rtx (CONST_INT, VOIDmode, 31),
					    gen_rtx (REG, SImode, 112)))));
      DONE;
    }
}")

(define_insn ""
 [(set (match_operand:SI 0 "register_operand" "=r")
       (lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
		    (match_operand:SI 2 "uint5_operand" "K")))]
 ""
 "*
{
  rtx xoperands[4];
  xoperands[0] = operands[0];  xoperands[1] = operands[1];
  xoperands[2] = gen_rtx (CONST_INT, VOIDmode,
			  31 - INTVAL (operands[2]));
  xoperands[3] = gen_rtx (CONST_INT, VOIDmode,
			  32 - INTVAL (operands[2]));
  output_asm_insn (\"extru %1,%2,%3,%0\", xoperands);
  return \"\";
}")

(define_insn ""
 [(set (match_operand:SI 0 "register_operand" "=r")
       (lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
		  (minus:SI (const_int 31)
			    (reg:SI 112))))]
 ""
 "vextru %1,32,%0")

;; Unconditional and other jump instructions.

(define_insn "jump"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  ""
  "bl%* %l0,0"
  [(set_attr "type" "branch")])

(define_insn "casesi"
  [(set (pc)
	(if_then_else (leu (minus:SI
			    (match_operand:SI 0 "general_operand" "r")
			    (match_operand:SI 1 "general_operand" "rI"))
			   (match_operand:SI 2 "general_operand" "rI"))
		      (plus:SI (mem:SI (plus:SI (pc)
						(minus:SI (match_dup 0)
							  (match_dup 1))))
			       (label_ref (match_operand 3 "" "")))
		      (pc)))
   (use (label_ref (match_operand 4 "" "")))
   (clobber (match_scratch:SI 5 "=r"))]
  ""
  "*
{
  if (GET_CODE (operands[1]) == CONST_INT)
    {
      if (GET_CODE (operands[2]) == CONST_INT)
	{
	  operands[2] = gen_rtx (CONST_INT, VOIDmode,
				 INTVAL (operands[1]) + INTVAL (operands[2]));
	  if (!INT_11_BITS (operands[2]))
	    {
	      output_asm_insn (\"ldo %2(0),%5\", operands);
	      operands[2] = operands[5];
	    }
	}
      else
	output_asm_insn (\"ldo %1(%2),%2\", operands);
      output_asm_insn (\"addi,< %n1,%0,0\", operands);
    }
  else
    {
      if (GET_CODE (operands[2]) == CONST_INT)
	{
	  output_asm_insn (\"ldo %2(%1),%5\", operands);
	  operands[2] = operands[5];
	}
      output_asm_insn (\"sub,< %0,%1,0\", operands);
    }
  if (GET_CODE (operands[2]) == CONST_INT)
    output_asm_insn (\"addi,<= %n2,%0,0\", operands);
  else
    output_asm_insn (\"sub,<= %0,%2,0\", operands);
  output_asm_insn (\"b,n %l4\", operands);
  if (GET_CODE (operands[1]) == CONST_INT)
    output_asm_insn (\"ldo %n1(%0),%5\", operands);
  else output_asm_insn (\"sub %0,%1,%5\", operands);
  return \"blr %5,0\;nop\";
}"
[(set_attr "length" "7")])
  
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
  operands[0] = gen_rtx (MEM, SImode, XEXP (operands[0], 0));
}")

(define_insn ""
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
  operands[1] = gen_rtx (MEM, SImode, XEXP (operands[1], 0));
}")

(define_insn ""
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
      return \"bl %1,2\;nop\";
    }
}"
 [(set_attr "type" "dyncall")
  (set_attr "length" "3,2")])

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop")

;;; Hope this is only within a function...
(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))]
  ""
 "bv 0(%0)%#"
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
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "=r")
			 (match_operand:SI 1 "uint5_operand" "")
			 (match_operand:SI 2 "uint5_operand" ""))
	(match_operand:SI 3 "register_operand" "r"))]
  ""
  "dep %3,%2+%1-1,%1,%0")

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



;;- Local variables:
;;- mode:emacs-lisp
;;- comment-start: ";;- "
;;- eval: (set-syntax-table (copy-sequence (syntax-table)))
;;- eval: (modify-syntax-entry ?[ "(]")
;;- eval: (modify-syntax-entry ?] ")[")
;;- eval: (modify-syntax-entry ?{ "(}")
;;- eval: (modify-syntax-entry ?} "){")
;;- End:
