;;- Machine description for SPARC chip for GNU C compiler
;;  Copyright (C) 1987, 88, 89, 92-96, 1997 Free Software Foundation, Inc.
;;  Contributed by Michael Tiemann (tiemann@cygnus.com)
;;  64 bit SPARC V9 support by Michael Tiemann, Jim Wilson, and Doug Evans,
;;  at Cygnus Support.

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
;; the Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.

;; The upper 32 fp regs on the v9 can't hold SFmode values.  To deal with this
;; a second register class, EXTRA_FP_REGS, exists for the v9 chip.  The name
;; is a bit of a misnomer as it covers all 64 fp regs.  The corresponding
;; constraint letter is 'e'.  To avoid any confusion, 'e' is used instead of
;; 'f' for all DF/TFmode values, including those that are specific to the v8.
;;
;; -mlive-g0 is *not* supported for TARGET_ARCH64, so we don't bother to
;; test TARGET_LIVE_G0 if we have TARGET_ARCH64.

;; Attribute for cpu type.
;; These must match the values for enum processor_type in sparc.h.
(define_attr "cpu" "v7,cypress,v8,supersparc,sparclite,f930,f934,sparclet,tsc701,v9,ultrasparc"
  (const (symbol_ref "sparc_cpu_attr")))

;; Attribute for the instruction set.
;; At present we only need to distinguish v9/!v9, but for clarity we
;; test TARGET_V8 too.
(define_attr "isa" "v6,v8,v9,sparclet"
 (const
  (cond [(symbol_ref "TARGET_V9") (const_string "v9")
	 (symbol_ref "TARGET_V8") (const_string "v8")
	 (symbol_ref "TARGET_SPARCLET") (const_string "sparclet")]
	(const_string "v6"))))

;; Architecture size.
(define_attr "arch" "arch32bit,arch64bit"
 (const
  (cond [(symbol_ref "TARGET_ARCH64") (const_string "arch64bit")]
	(const_string "arch32bit"))))

;; Whether -mlive-g0 is in effect.
(define_attr "live_g0" "no,yes"
 (const
  (cond [(symbol_ref "TARGET_LIVE_G0") (const_string "yes")]
	(const_string "no"))))

;; Insn type.  Used to default other attribute values.

;; type "unary" insns have one input operand (1) and one output operand (0)
;; type "binary" insns have two input operands (1,2) and one output (0)
;; type "compare" insns have one or two input operands (0,1) and no output
;; type "call_no_delay_slot" is a call followed by an unimp instruction.

(define_attr "type"
  "move,unary,binary,compare,load,sload,store,ialu,shift,uncond_branch,branch,call,call_no_delay_slot,return,address,imul,fpload,fpstore,fp,fpmove,fpcmove,fpcmp,fpmul,fpdivs,fpdivd,fpsqrt,cmove,multi,misc"
  (const_string "binary"))

;; Set true if insn uses call-clobbered intermediate register.
(define_attr "use_clobbered" "false,true"
  (if_then_else (and (eq_attr "type" "address")
		     (match_operand 0 "clobbered_register" ""))
	 	(const_string "true")
		(const_string "false")))

;; Length (in # of insns).
(define_attr "length" ""
  (cond [(eq_attr "type" "load,sload,fpload")
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
  (cond [(eq_attr "type" "uncond_branch,branch,call,call_no_delay_slot,return,multi")
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

(define_attr "leaf_function" "false,true"
  (const (symbol_ref "leaf_function")))


(define_attr "in_return_delay" "false,true"
  (if_then_else (and (and (and (eq_attr "type" "move,load,sload,store,binary,ialu")
			       (eq_attr "length" "1"))
			  (eq_attr "leaf_function" "false"))
		     (match_insn "eligible_for_return_delay"))
		(const_string "true")
		(const_string "false")))

(define_delay (and (eq_attr "type" "return")
		   (eq_attr "isa" "v9"))
  [(eq_attr "in_return_delay" "true") (nil) (nil)])

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

;; On the sparclite, integer multiply takes 1, 3, or 5 cycles depending on
;; the inputs.

;; (define_function_unit "alu" 1 0
;;  (eq_attr "type" "unary,binary,move,address") 1 0)

;; ---- cypress CY7C602 scheduling:
;; Memory with load-delay of 1 (i.e., 2 cycle load).

(define_function_unit "memory" 1 0 
  (and (eq_attr "cpu" "cypress")
    (eq_attr "type" "load,sload,fpload"))
  2 2)

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

(define_function_unit "fp_alu" 1 0
  (and (eq_attr "cpu" "cypress")
    (eq_attr "type" "fp,fpmove"))
  5 5)

(define_function_unit "fp_mds" 1 0
  (and (eq_attr "cpu" "cypress")
    (eq_attr "type" "fpmul"))
  7 7)

(define_function_unit "fp_mds" 1 0
  (and (eq_attr "cpu" "cypress")
    (eq_attr "type" "fpdivs,fpdivd"))
  37 37)

(define_function_unit "fp_mds" 1 0
  (and (eq_attr "cpu" "cypress")
    (eq_attr "type" "fpsqrt"))
  63 63)

;; ----- The TMS390Z55 scheduling
;; The Supersparc can issue 1 - 3 insns per cycle: up to two integer,
;; one ld/st, one fp.
;; Memory delivers its result in one cycle to IU, zero cycles to FP

(define_function_unit "memory" 1 0
  (and (eq_attr "cpu" "supersparc")
    (eq_attr "type" "load,sload"))
  1 1)

(define_function_unit "memory" 1 0
  (and (eq_attr "cpu" "supersparc")
    (eq_attr "type" "fpload"))
  0 1)

(define_function_unit "memory" 1 0
  (and (eq_attr "cpu" "supersparc")
    (eq_attr "type" "store,fpstore"))
  1 1)

(define_function_unit "shift" 1 0
  (and (eq_attr "cpu" "supersparc")
    (eq_attr "type" "shift"))
  1 1)

;; There are only two write ports to the integer register file
;; A store also uses a write port

(define_function_unit "iwport" 2 0
  (and (eq_attr "cpu" "supersparc")
    (eq_attr "type" "load,sload,store,shift,ialu"))
  1 1)

;; Timings; throughput/latency
;; FADD     1/3    add/sub, format conv, compar, abs, neg
;; FMUL     1/3
;; FDIVs    4/6
;; FDIVd    7/9
;; FSQRTs   6/8
;; FSQRTd  10/12
;; IMUL     4/4

(define_function_unit "fp_alu" 1 0
  (and (eq_attr "cpu" "supersparc")
    (eq_attr "type" "fp,fpmove,fpcmp"))
  3 1)

(define_function_unit "fp_mds" 1 0
  (and (eq_attr "cpu" "supersparc")
    (eq_attr "type" "fpmul"))
  3 1)

(define_function_unit "fp_mds" 1 0
  (and (eq_attr "cpu" "supersparc")
    (eq_attr "type" "fpdivs"))
  6 4)

(define_function_unit "fp_mds" 1 0
  (and (eq_attr "cpu" "supersparc")
    (eq_attr "type" "fpdivd"))
  9 7)

(define_function_unit "fp_mds" 1 0
  (and (eq_attr "cpu" "supersparc")
    (eq_attr "type" "fpsqrt"))
  12 10)

(define_function_unit "fp_mds" 1 0
  (and (eq_attr "cpu" "supersparc")
    (eq_attr "type" "imul"))
  4 4)

;; ----- sparclet tsc701 scheduling
;; The tsc701 issues 1 insn per cycle.
;; Results may be written back out of order.

;; Loads take 2 extra cycles to complete and 4 can be buffered at a time.

(define_function_unit "tsc701_load" 4 1
  (and (eq_attr "cpu" "tsc701")
    (eq_attr "type" "load,sload"))
  3 1)

;; Stores take 2(?) extra cycles to complete.
;; It is desirable to not have any memory operation in the following 2 cycles.
;; (??? or 2 memory ops in the case of std).

(define_function_unit "tsc701_store" 1 0
  (and (eq_attr "cpu" "tsc701")
    (eq_attr "type" "store"))
  3 3
  [(eq_attr "type" "load,sload,store")])

;; The multiply unit has a latency of 5.
(define_function_unit "tsc701_mul" 1 0
  (and (eq_attr "cpu" "tsc701")
    (eq_attr "type" "imul"))
  5 5)

;; ----- The UltraSPARC-1 scheduling
;; UltraSPARC has two integer units.  Shift instructions can only execute
;; on IE0.  Condition code setting instructions, call, and jmpl (including
;; the ret and retl pseudo-instructions) can only execute on IE1.
;; Branch on register uses IE1, but branch on condition code does not.
;; Conditional moves take 2 cycles.  No other instruction can issue in the
;; same cycle as a conditional move.
;; Multiply and divide take many cycles during which no other instructions
;; can issue.
;; Memory delivers its result in two cycles (except for signed loads,
;; which take one cycle more).  One memory instruction can be issued per
;; cycle.

(define_function_unit "memory" 1 0
  (and (eq_attr "cpu" "ultrasparc")
    (eq_attr "type" "load,fpload"))
  2 1)

(define_function_unit "memory" 1 0
  (and (eq_attr "cpu" "ultrasparc")
    (eq_attr "type" "sload"))
  3 1)

(define_function_unit "memory" 1 0
  (and (eq_attr "cpu" "ultrasparc")
    (eq_attr "type" "store,fpstore"))
  1 1)

(define_function_unit "ieu" 1 0
  (and (eq_attr "cpu" "ultrasparc")
    (eq_attr "type" "ialu,binary,shift,compare,cmove,call"))
  1 1)

(define_function_unit "ieu_shift" 1 0
  (and (eq_attr "cpu" "ultrasparc")
    (eq_attr "type" "shift"))
  1 1)

(define_function_unit "ieu_shift" 1 0
  (and (eq_attr "cpu" "ultrasparc")
    (eq_attr "type" "cmove"))
  2 1)

;; Timings; throughput/latency
;; FMOV     1/1    fmov, fabs, fneg
;; FMOVcc   1/2
;; FADD     1/4    add/sub, format conv, compar
;; FMUL     1/4
;; FDIVs    12/12
;; FDIVd    22/22
;; FSQRTs   12/12
;; FSQRTd   22/22
;; FCMP takes 1 cycle to branch, 2 cycles to conditional move.

(define_function_unit "fadd" 1 0
  (and (eq_attr "cpu" "ultrasparc")
    (eq_attr "type" "fpmove"))
  1 1)

(define_function_unit "fadd" 1 0
  (and (eq_attr "cpu" "ultrasparc")
    (eq_attr "type" "fpcmove"))
  2 1)

(define_function_unit "fadd" 1 0
  (and (eq_attr "cpu" "ultrasparc")
    (eq_attr "type" "fp"))
  4 1)

(define_function_unit "fadd" 1 0
  (and (eq_attr "cpu" "ultrasparc")
    (eq_attr "type" "fpcmp"))
  2 1)

(define_function_unit "fmul" 1 0
  (and (eq_attr "cpu" "ultrasparc")
    (eq_attr "type" "fpmul"))
  4 1)

(define_function_unit "fadd" 1 0
  (and (eq_attr "cpu" "ultrasparc")
    (eq_attr "type" "fpcmove"))
  2 1)

(define_function_unit "fadd" 1 0
  (and (eq_attr "cpu" "ultrasparc")
    (eq_attr "type" "fpdivs"))
  12 12)

(define_function_unit "fadd" 1 0
  (and (eq_attr "cpu" "ultrasparc")
    (eq_attr "type" "fpdivd"))
  22 22)

(define_function_unit "fadd" 1 0
  (and (eq_attr "cpu" "ultrasparc")
    (eq_attr "type" "fpsqrt"))
  12 12)

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
;; We start with the DEFINE_EXPANDs, then the DEFINE_INSNs to match
;; the patterns.  Finally, we have the DEFINE_SPLITs for some of the scc
;; insns that actually require more than one machine instruction.

;; Put cmpsi first among compare insns so it matches two CONST_INT operands.

(define_expand "cmpsi"
  [(set (reg:CC 100)
	(compare:CC (match_operand:SI 0 "register_operand" "")
		    (match_operand:SI 1 "arith_operand" "")))]
  ""
  "
{
  sparc_compare_op0 = operands[0];
  sparc_compare_op1 = operands[1];
  DONE;
}")

(define_expand "cmpdi"
  [(set (reg:CCX 100)
	(compare:CCX (match_operand:DI 0 "register_operand" "")
		     (match_operand:DI 1 "arith_double_operand" "")))]
  "TARGET_ARCH64 || TARGET_V8PLUS"
  "
{
  sparc_compare_op0 = operands[0];
  sparc_compare_op1 = operands[1];
  DONE;
}")

(define_expand "cmpsf"
  ;; The 96 here isn't ever used by anyone.
  [(set (reg:CCFP 96)
	(compare:CCFP (match_operand:SF 0 "register_operand" "")
		      (match_operand:SF 1 "register_operand" "")))]
  "TARGET_FPU"
  "
{
  sparc_compare_op0 = operands[0];
  sparc_compare_op1 = operands[1];
  DONE;
}")

(define_expand "cmpdf"
  ;; The 96 here isn't ever used by anyone.
  [(set (reg:CCFP 96)
	(compare:CCFP (match_operand:DF 0 "register_operand" "")
		      (match_operand:DF 1 "register_operand" "")))]
  "TARGET_FPU"
  "
{
  sparc_compare_op0 = operands[0];
  sparc_compare_op1 = operands[1];
  DONE;
}")

(define_expand "cmptf"
  ;; The 96 here isn't ever used by anyone.
  [(set (reg:CCFP 96)
	(compare:CCFP (match_operand:TF 0 "register_operand" "")
		      (match_operand:TF 1 "register_operand" "")))]
  "TARGET_FPU"
  "
{
  sparc_compare_op0 = operands[0];
  sparc_compare_op1 = operands[1];
  DONE;
}")

;; Now the compare DEFINE_INSNs.

(define_insn "*cmpsi_insn"
  [(set (reg:CC 100)
	(compare:CC (match_operand:SI 0 "register_operand" "r")
		    (match_operand:SI 1 "arith_operand" "rI")))]
  ""
  "cmp %0,%1"
  [(set_attr "type" "compare")])

(define_insn "*cmpdi_sp64"
  [(set (reg:CCX 100)
	(compare:CCX (match_operand:DI 0 "register_operand" "r")
		     (match_operand:DI 1 "arith_double_operand" "rHI")))]
  "TARGET_ARCH64"
  "cmp %0,%1"
  [(set_attr "type" "compare")])

(define_insn "cmpdi_v8plus"
  [(set (reg:CCX 100)
	(compare:CCX (match_operand:DI 0 "register_operand" "r,r,r")
		     (match_operand:DI 1 "arith_double_operand" "J,I,r")))
   (clobber (match_scratch:SI 2 "=&h,&h,&h"))
   (clobber (match_scratch:SI 3 "=X,X,&h"))]
  "TARGET_V8PLUS"
  "*
{
  /* The srl can be omitted if the value in the %L0 or %L1 is already
     zero extended.  */

  output_asm_insn (\"sllx %H0,32,%2\", operands);

  if (sparc_check_64 (operands[0], insn) <= 0)
    output_asm_insn (\"srl %L0,0,%L0\", operands);

  switch (which_alternative)
    {
    case 0:
      return \"orcc %L0,%2,%%g0\";
    case 1:
      return \"or %L0,%2,%2\;cmp %2,%1\";
    case 2:
      if (sparc_check_64 (operands[1], insn) <= 0)
	output_asm_insn (\"srl %L1,0,%L1\", operands);
      return \"sllx %H1,32,%3\;or %L0,%2,%2\;or %L1,%3,%3\;cmp %2,%3\";
    default:
      abort();
    }
}"
  [(set_attr "length" "3,4,7")])

(define_insn "*cmpsf_fpe"
  [(set (match_operand:CCFPE 0 "fcc_reg_operand" "=c")
	(compare:CCFPE (match_operand:SF 1 "register_operand" "f")
		       (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_FPU"
  "*
{
  if (TARGET_V9)
    return \"fcmpes %0,%1,%2\";
  return \"fcmpes %1,%2\";
}"
  [(set_attr "type" "fpcmp")])

(define_insn "*cmpdf_fpe"
  [(set (match_operand:CCFPE 0 "fcc_reg_operand" "=c")
	(compare:CCFPE (match_operand:DF 1 "register_operand" "e")
		       (match_operand:DF 2 "register_operand" "e")))]
  "TARGET_FPU"
  "*
{
  if (TARGET_V9)
    return \"fcmped %0,%1,%2\";
  return \"fcmped %1,%2\";
}"
  [(set_attr "type" "fpcmp")])

(define_insn "*cmptf_fpe"
  [(set (match_operand:CCFPE 0 "fcc_reg_operand" "=c")
	(compare:CCFPE (match_operand:TF 1 "register_operand" "e")
		       (match_operand:TF 2 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "*
{
  if (TARGET_V9)
    return \"fcmpeq %0,%1,%2\";
  return \"fcmpeq %1,%2\";
}"
  [(set_attr "type" "fpcmp")])

(define_insn "*cmpsf_fp"
  [(set (match_operand:CCFP 0 "fcc_reg_operand" "=c")
	(compare:CCFP (match_operand:SF 1 "register_operand" "f")
		      (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_FPU"
  "*
{
  if (TARGET_V9)
    return \"fcmps %0,%1,%2\";
  return \"fcmps %1,%2\";
}"
  [(set_attr "type" "fpcmp")])

(define_insn "*cmpdf_fp"
  [(set (match_operand:CCFP 0 "fcc_reg_operand" "=c")
	(compare:CCFP (match_operand:DF 1 "register_operand" "e")
		      (match_operand:DF 2 "register_operand" "e")))]
  "TARGET_FPU"
  "*
{
  if (TARGET_V9)
    return \"fcmpd %0,%1,%2\";
  return \"fcmpd %1,%2\";
}"
  [(set_attr "type" "fpcmp")])

(define_insn "*cmptf_fp"
  [(set (match_operand:CCFP 0 "fcc_reg_operand" "=c")
	(compare:CCFP (match_operand:TF 1 "register_operand" "e")
		      (match_operand:TF 2 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "*
{
  if (TARGET_V9)
    return \"fcmpq %0,%1,%2\";
  return \"fcmpq %1,%2\";
}"
  [(set_attr "type" "fpcmp")])

;; Next come the scc insns.  For seq, sne, sgeu, and sltu, we can do this
;; without jumps using the addx/subx instructions.  For seq/sne on v9 we use
;; the same code as v8 (the addx/subx method has more applications).  The
;; exception to this is "reg != 0" which can be done in one instruction on v9
;; (so we do it).  For the rest, on v9 we use conditional moves; on v8, we do
;; branches.

;; Seq_special[_xxx] and sne_special[_xxx] clobber the CC reg, because they
;; generate addcc/subcc instructions.

(define_expand "seqsi_special"
  [(set (match_dup 3)
	(xor:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "register_operand" "")))
   (parallel [(set (match_operand:SI 0 "register_operand" "")
		   (eq:SI (match_dup 3) (const_int 0)))
	      (clobber (reg:CC 100))])]
  "! TARGET_LIVE_G0"
  "{ operands[3] = gen_reg_rtx (SImode); }")

(define_expand "seqdi_special"
  [(set (match_dup 3)
	(xor:DI (match_operand:DI 1 "register_operand" "")
		(match_operand:DI 2 "register_operand" "")))
   (set (match_operand:DI 0 "register_operand" "")
	(eq:DI (match_dup 3) (const_int 0)))]
  "TARGET_ARCH64"
  "{ operands[3] = gen_reg_rtx (DImode); }")

(define_expand "snesi_special"
  [(set (match_dup 3)
	(xor:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "register_operand" "")))
   (parallel [(set (match_operand:SI 0 "register_operand" "")
		   (ne:SI (match_dup 3) (const_int 0)))
	      (clobber (reg:CC 100))])]
  "! TARGET_LIVE_G0"
  "{ operands[3] = gen_reg_rtx (SImode); }")

(define_expand "snedi_special"
  [(set (match_dup 3)
	(xor:DI (match_operand:DI 1 "register_operand" "")
		(match_operand:DI 2 "register_operand" "")))
   (set (match_operand:DI 0 "register_operand" "")
	(ne:DI (match_dup 3) (const_int 0)))]
  "TARGET_ARCH64"
  "{ operands[3] = gen_reg_rtx (DImode); }")

(define_expand "seqdi_special_trunc"
  [(set (match_dup 3)
	(xor:DI (match_operand:DI 1 "register_operand" "")
		(match_operand:DI 2 "register_operand" "")))
   (set (match_operand:SI 0 "register_operand" "")
	(eq:DI (match_dup 3) (const_int 0)))]
  "TARGET_ARCH64"
  "{ operands[3] = gen_reg_rtx (DImode); }")

(define_expand "snedi_special_trunc"
  [(set (match_dup 3)
	(xor:DI (match_operand:DI 1 "register_operand" "")
		(match_operand:DI 2 "register_operand" "")))
   (set (match_operand:SI 0 "register_operand" "")
	(ne:DI (match_dup 3) (const_int 0)))]
  "TARGET_ARCH64"
  "{ operands[3] = gen_reg_rtx (DImode); }")

(define_expand "seqsi_special_extend"
  [(set (match_dup 3)
	(xor:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "register_operand" "")))
   (parallel [(set (match_operand:DI 0 "register_operand" "")
		   (eq:SI (match_dup 3) (const_int 0)))
	      (clobber (reg:CC 100))])]
  "TARGET_ARCH64"
  "{ operands[3] = gen_reg_rtx (SImode); }")

(define_expand "snesi_special_extend"
  [(set (match_dup 3)
	(xor:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "register_operand" "")))
   (parallel [(set (match_operand:DI 0 "register_operand" "")
		   (ne:SI (match_dup 3) (const_int 0)))
	      (clobber (reg:CC 100))])]
  "TARGET_ARCH64"
  "{ operands[3] = gen_reg_rtx (SImode); }")

;; ??? v9: Operand 0 needs a mode, so SImode was chosen.
;; However, the code handles both SImode and DImode.
(define_expand "seq"
  [(set (match_operand:SI 0 "intreg_operand" "")
	(eq:SI (match_dup 1) (const_int 0)))]
  "! TARGET_LIVE_G0"
  "
{
  if (GET_MODE (sparc_compare_op0) == SImode)
    {
      rtx pat;

      if (GET_MODE (operands[0]) == SImode)
	pat = gen_seqsi_special (operands[0], sparc_compare_op0,
				 sparc_compare_op1);
      else if (! TARGET_ARCH64)
	FAIL;
      else
	pat = gen_seqsi_special_extend (operands[0], sparc_compare_op0,
					sparc_compare_op1);
      emit_insn (pat);
      DONE;
    }
  else if (GET_MODE (sparc_compare_op0) == DImode)
    {
      rtx pat;

      if (! TARGET_ARCH64)
	FAIL;
      else if (GET_MODE (operands[0]) == SImode)
	pat = gen_seqdi_special_trunc (operands[0], sparc_compare_op0,
				       sparc_compare_op1);
      else
	pat = gen_seqdi_special (operands[0], sparc_compare_op0,
				 sparc_compare_op1);
      emit_insn (pat);
      DONE;
    }
  else if (GET_MODE (sparc_compare_op0) == TFmode && ! TARGET_HARD_QUAD)
    {
      emit_float_lib_cmp (sparc_compare_op0, sparc_compare_op1, EQ);
      emit_insn (gen_sne (operands[0]));
      DONE;
    }      
  else if (TARGET_V9)
    {
      if (gen_v9_scc (EQ, operands))
	DONE;
      /* fall through */
    }
  operands[1] = gen_compare_reg (EQ, sparc_compare_op0, sparc_compare_op1);
}")

;; ??? v9: Operand 0 needs a mode, so SImode was chosen.
;; However, the code handles both SImode and DImode.
(define_expand "sne"
  [(set (match_operand:SI 0 "intreg_operand" "")
	(ne:SI (match_dup 1) (const_int 0)))]
  "! TARGET_LIVE_G0"
  "
{
  if (GET_MODE (sparc_compare_op0) == SImode)
    {
      rtx pat;

      if (GET_MODE (operands[0]) == SImode)
	pat = gen_snesi_special (operands[0], sparc_compare_op0,
				 sparc_compare_op1);
      else if (! TARGET_ARCH64)
	FAIL;
      else
	pat = gen_snesi_special_extend (operands[0], sparc_compare_op0,
					sparc_compare_op1);
      emit_insn (pat);
      DONE;
    }
  else if (GET_MODE (sparc_compare_op0) == DImode)
    {
      rtx pat;

      if (! TARGET_ARCH64)
	FAIL;
      else if (GET_MODE (operands[0]) == SImode)
	pat = gen_snedi_special_trunc (operands[0], sparc_compare_op0,
				       sparc_compare_op1);
      else
	pat = gen_snedi_special (operands[0], sparc_compare_op0,
				 sparc_compare_op1);
      emit_insn (pat);
      DONE;
    }
  else if (GET_MODE (sparc_compare_op0) == TFmode && ! TARGET_HARD_QUAD)
    {
      emit_float_lib_cmp (sparc_compare_op0, sparc_compare_op1, NE);
      emit_insn (gen_sne (operands[0]));
      DONE;
    }      
  else if (TARGET_V9)
    {
      if (gen_v9_scc (NE, operands))
	DONE;
      /* fall through */
    }
  operands[1] = gen_compare_reg (NE, sparc_compare_op0, sparc_compare_op1);
}")

(define_expand "sgt"
  [(set (match_operand:SI 0 "intreg_operand" "")
	(gt:SI (match_dup 1) (const_int 0)))]
  "! TARGET_LIVE_G0"
  "
{
  if (GET_MODE (sparc_compare_op0) == TFmode && ! TARGET_HARD_QUAD)
    {
      emit_float_lib_cmp (sparc_compare_op0, sparc_compare_op1, GT);
      emit_insn (gen_sne (operands[0]));
      DONE;
    }
  else if (TARGET_V9)
    {
      if (gen_v9_scc (GT, operands))
	DONE;
      /* fall through */
    }
  operands[1] = gen_compare_reg (GT, sparc_compare_op0, sparc_compare_op1);
}")

(define_expand "slt"
  [(set (match_operand:SI 0 "intreg_operand" "")
	(lt:SI (match_dup 1) (const_int 0)))]
  "! TARGET_LIVE_G0"
  "
{
  if (GET_MODE (sparc_compare_op0) == TFmode && ! TARGET_HARD_QUAD)
    {
      emit_float_lib_cmp (sparc_compare_op0, sparc_compare_op1, LT);
      emit_insn (gen_sne (operands[0]));
      DONE;
    }
  else if (TARGET_V9)
    {
      if (gen_v9_scc (LT, operands))
	DONE;
      /* fall through */
    }
  operands[1] = gen_compare_reg (LT, sparc_compare_op0, sparc_compare_op1);
}")

(define_expand "sge"
  [(set (match_operand:SI 0 "intreg_operand" "")
	(ge:SI (match_dup 1) (const_int 0)))]
  "! TARGET_LIVE_G0"
  "
{
  if (GET_MODE (sparc_compare_op0) == TFmode && ! TARGET_HARD_QUAD)
    {
      emit_float_lib_cmp (sparc_compare_op0, sparc_compare_op1, GE);
      emit_insn (gen_sne (operands[0]));
      DONE;
    }
  else if (TARGET_V9)
    {
      if (gen_v9_scc (GE, operands))
	DONE;
      /* fall through */
    }
  operands[1] = gen_compare_reg (GE, sparc_compare_op0, sparc_compare_op1);
}")

(define_expand "sle"
  [(set (match_operand:SI 0 "intreg_operand" "")
	(le:SI (match_dup 1) (const_int 0)))]
  "! TARGET_LIVE_G0"
  "
{
  if (GET_MODE (sparc_compare_op0) == TFmode && ! TARGET_HARD_QUAD)
    {
      emit_float_lib_cmp (sparc_compare_op0, sparc_compare_op1, LE);
      emit_insn (gen_sne (operands[0]));
      DONE;
    }
  else if (TARGET_V9)
    {
      if (gen_v9_scc (LE, operands))
	DONE;
      /* fall through */
    }
  operands[1] = gen_compare_reg (LE, sparc_compare_op0, sparc_compare_op1);
}")

(define_expand "sgtu"
  [(set (match_operand:SI 0 "intreg_operand" "")
	(gtu:SI (match_dup 1) (const_int 0)))]
  "! TARGET_LIVE_G0"
  "
{
  if (! TARGET_V9)
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
    }
  else
    {
      if (gen_v9_scc (GTU, operands))
	DONE;
    }
  operands[1] = gen_compare_reg (GTU, sparc_compare_op0, sparc_compare_op1);
}")

(define_expand "sltu"
  [(set (match_operand:SI 0 "intreg_operand" "")
	(ltu:SI (match_dup 1) (const_int 0)))]
  "! TARGET_LIVE_G0"
  "
{
  if (TARGET_V9)
    {
      if (gen_v9_scc (LTU, operands))
	DONE;
    }
  operands[1] = gen_compare_reg (LTU, sparc_compare_op0, sparc_compare_op1);
}")

(define_expand "sgeu"
  [(set (match_operand:SI 0 "intreg_operand" "")
	(geu:SI (match_dup 1) (const_int 0)))]
  "! TARGET_LIVE_G0"
  "
{
  if (TARGET_V9)
    {
      if (gen_v9_scc (GEU, operands))
	DONE;
    }
  operands[1] = gen_compare_reg (GEU, sparc_compare_op0, sparc_compare_op1);
}")

(define_expand "sleu"
  [(set (match_operand:SI 0 "intreg_operand" "")
	(leu:SI (match_dup 1) (const_int 0)))]
  "! TARGET_LIVE_G0"
  "
{
  if (! TARGET_V9)
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
    }
  else
    {
      if (gen_v9_scc (LEU, operands))
	DONE;
    }
  operands[1] = gen_compare_reg (LEU, sparc_compare_op0, sparc_compare_op1);
}")

;; Now the DEFINE_INSNs for the scc cases.

;; The SEQ and SNE patterns are special because they can be done
;; without any branching and do not involve a COMPARE.

(define_insn "*snesi_zero"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ne:SI (match_operand:SI 1 "register_operand" "r")
	       (const_int 0)))
   (clobber (reg:CC 100))]
  "! TARGET_LIVE_G0"
  "subcc %%g0,%1,%%g0\;addx %%g0,0,%0"
  [(set_attr "type" "unary")
   (set_attr "length" "2")])

(define_insn "*neg_snesi_zero"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (ne:SI (match_operand:SI 1 "register_operand" "r")
		       (const_int 0))))
   (clobber (reg:CC 100))]
  "! TARGET_LIVE_G0"
  "subcc %%g0,%1,%%g0\;subx %%g0,0,%0"
  [(set_attr "type" "unary")
   (set_attr "length" "2")])

(define_insn "*snesi_zero_extend"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ne:SI (match_operand:SI 1 "register_operand" "r")
	       (const_int 0)))
   (clobber (reg:CC 100))]
  "TARGET_ARCH64"
  "subcc %%g0,%1,%%g0\;addx %%g0,0,%0"
  [(set_attr "type" "unary")
   (set_attr "length" "2")])

(define_insn "*snedi_zero"
  [(set (match_operand:DI 0 "register_operand" "=&r")
	(ne:DI (match_operand:DI 1 "register_operand" "r")
	       (const_int 0)))]
  "TARGET_ARCH64"
  "mov 0,%0\;movrnz %1,1,%0"
  [(set_attr "type" "cmove")
   (set_attr "length" "2")])

(define_insn "*neg_snedi_zero"
  [(set (match_operand:DI 0 "register_operand" "=&r")
	(neg:DI (ne:DI (match_operand:DI 1 "register_operand" "r")
		       (const_int 0))))]
  "TARGET_ARCH64"
  "mov 0,%0\;movrnz %1,-1,%0"
  [(set_attr "type" "cmove")
   (set_attr "length" "2")])

(define_insn "*snedi_zero_trunc"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(ne:DI (match_operand:DI 1 "register_operand" "r")
	       (const_int 0)))]
  "TARGET_ARCH64"
  "mov 0,%0\;movrnz %1,1,%0"
  [(set_attr "type" "cmove")
   (set_attr "length" "2")])

(define_insn "*seqsi_zero"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(eq:SI (match_operand:SI 1 "register_operand" "r")
	       (const_int 0)))
   (clobber (reg:CC 100))]
  "! TARGET_LIVE_G0"
  "subcc %%g0,%1,%%g0\;subx %%g0,-1,%0"
  [(set_attr "type" "unary")
   (set_attr "length" "2")])

(define_insn "*neg_seqsi_zero"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (eq:SI (match_operand:SI 1 "register_operand" "r")
		       (const_int 0))))
   (clobber (reg:CC 100))]
  "! TARGET_LIVE_G0"
  "subcc %%g0,%1,%%g0\;addx %%g0,-1,%0"
  [(set_attr "type" "unary")
   (set_attr "length" "2")])

(define_insn "*seqsi_zero_extend"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(eq:SI (match_operand:SI 1 "register_operand" "r")
	       (const_int 0)))
   (clobber (reg:CC 100))]
  "TARGET_ARCH64"
  "subcc %%g0,%1,%%g0\;subx %%g0,-1,%0"
  [(set_attr "type" "unary")
   (set_attr "length" "2")])

(define_insn "*seqdi_zero"
  [(set (match_operand:DI 0 "register_operand" "=&r")
	(eq:DI (match_operand:DI 1 "register_operand" "r")
	       (const_int 0)))]
  "TARGET_ARCH64"
  "mov 0,%0\;movrz %1,1,%0"
  [(set_attr "type" "cmove")
   (set_attr "length" "2")])

(define_insn "*neg_seqdi_zero"
  [(set (match_operand:DI 0 "register_operand" "=&r")
	(neg:DI (eq:DI (match_operand:DI 1 "register_operand" "r")
		       (const_int 0))))]
  "TARGET_ARCH64"
  "mov 0,%0\;movrz %1,-1,%0"
  [(set_attr "type" "cmove")
   (set_attr "length" "2")]) 

(define_insn "*seqdi_zero_trunc"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(eq:DI (match_operand:DI 1 "register_operand" "r")
	       (const_int 0)))]
  "TARGET_ARCH64"
  "mov 0,%0\;movrz %1,1,%0"
  [(set_attr "type" "cmove")
   (set_attr "length" "2")])

;; We can also do (x + (i == 0)) and related, so put them in.
;; ??? The addx/subx insns use the 32 bit carry flag so there are no DImode
;; versions for v9.

(define_insn "*x_plus_i_ne_0"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (ne:SI (match_operand:SI 1 "register_operand" "r")
			(const_int 0))
		 (match_operand:SI 2 "register_operand" "r")))
   (clobber (reg:CC 100))]
  "! TARGET_LIVE_G0"
  "subcc %%g0,%1,%%g0\;addx %2,0,%0"
  [(set_attr "length" "2")])

(define_insn "*x_minus_i_ne_0"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 2 "register_operand" "r")
		  (ne:SI (match_operand:SI 1 "register_operand" "r")
			 (const_int 0))))
   (clobber (reg:CC 100))]
  "! TARGET_LIVE_G0"
  "subcc %%g0,%1,%%g0\;subx %2,0,%0"
  [(set_attr "length" "2")])

(define_insn "*x_plus_i_eq_0"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (eq:SI (match_operand:SI 1 "register_operand" "r")
			(const_int 0))
		 (match_operand:SI 2 "register_operand" "r")))
   (clobber (reg:CC 100))]
  "! TARGET_LIVE_G0"
  "subcc %%g0,%1,%%g0\;subx %2,-1,%0"
  [(set_attr "length" "2")])

(define_insn "*x_minus_i_eq_0"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 2 "register_operand" "r")
		  (eq:SI (match_operand:SI 1 "register_operand" "r")
			 (const_int 0))))
   (clobber (reg:CC 100))]
  "! TARGET_LIVE_G0"
  "subcc %%g0,%1,%%g0\;addx %2,-1,%0"
  [(set_attr "length" "2")])

;; We can also do GEU and LTU directly, but these operate after a compare.
;; ??? The addx/subx insns use the 32 bit carry flag so there are no DImode
;; versions for v9.

(define_insn "*sltu_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ltu:SI (reg:CC 100) (const_int 0)))]
  "! TARGET_LIVE_G0"
  "addx %%g0,0,%0"
  [(set_attr "type" "misc")])

(define_insn "*neg_sltu_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (ltu:SI (reg:CC 100) (const_int 0))))]
  "! TARGET_LIVE_G0"
  "subx %%g0,0,%0"
  [(set_attr "type" "misc")])

;; ??? Combine should canonicalize these next two to the same pattern.
(define_insn "*neg_sltu_minus_x"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (neg:SI (ltu:SI (reg:CC 100) (const_int 0)))
		  (match_operand:SI 1 "arith_operand" "rI")))]
  "! TARGET_LIVE_G0"
  "subx %%g0,%1,%0"
  [(set_attr "type" "unary")])

(define_insn "*neg_sltu_plus_x"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (plus:SI (ltu:SI (reg:CC 100) (const_int 0))
			 (match_operand:SI 1 "arith_operand" "rI"))))]
  "! TARGET_LIVE_G0"
  "subx %%g0,%1,%0"
  [(set_attr "type" "unary")])

(define_insn "*sgeu_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(geu:SI (reg:CC 100) (const_int 0)))]
  "! TARGET_LIVE_G0"
  "subx %%g0,-1,%0"
  [(set_attr "type" "misc")])

(define_insn "*neg_sgeu_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (geu:SI (reg:CC 100) (const_int 0))))]
  "! TARGET_LIVE_G0"
  "addx %%g0,-1,%0"
  [(set_attr "type" "misc")])

;; We can also do (x + ((unsigned) i >= 0)) and related, so put them in.
;; ??? The addx/subx insns use the 32 bit carry flag so there are no DImode
;; versions for v9.

(define_insn "*sltu_plus_x"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (ltu:SI (reg:CC 100) (const_int 0))
		 (match_operand:SI 1 "arith_operand" "rI")))]
  "! TARGET_LIVE_G0"
  "addx %%g0,%1,%0"
  [(set_attr "type" "unary")])

(define_insn "*sltu_plus_x_plus_y"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (ltu:SI (reg:CC 100) (const_int 0))
		 (plus:SI (match_operand:SI 1 "arith_operand" "%r")
			  (match_operand:SI 2 "arith_operand" "rI"))))]
  ""
  "addx %1,%2,%0")

(define_insn "*x_minus_sltu"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 1 "register_operand" "r")
		  (ltu:SI (reg:CC 100) (const_int 0))))]
  ""
  "subx %1,0,%0"
  [(set_attr "type" "unary")])

;; ??? Combine should canonicalize these next two to the same pattern.
(define_insn "*x_minus_y_minus_sltu"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (minus:SI (match_operand:SI 1 "register_operand" "r")
			    (match_operand:SI 2 "arith_operand" "rI"))
		  (ltu:SI (reg:CC 100) (const_int 0))))]
  ""
  "subx %1,%2,%0")

(define_insn "*x_minus_sltu_plus_y"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 1 "register_operand" "r")
		  (plus:SI (ltu:SI (reg:CC 100) (const_int 0))
			   (match_operand:SI 2 "arith_operand" "rI"))))]
  ""
  "subx %1,%2,%0")

(define_insn "*sgeu_plus_x"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (geu:SI (reg:CC 100) (const_int 0))
		 (match_operand:SI 1 "register_operand" "r")))]
  ""
  "subx %1,-1,%0"
  [(set_attr "type" "unary")])

(define_insn "*x_minus_sgeu"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 1 "register_operand" "r")
		  (geu:SI (reg:CC 100) (const_int 0))))]
  ""
  "addx %1,-1,%0"
  [(set_attr "type" "unary")])

;; Now we have the generic scc insns.
;; !v9: These will be done using a jump.
;; v9: Use conditional moves which are defined elsewhere.
;; We have to exclude the cases above, since we will not want combine to
;; turn something that does not require a jump into something that does.

(define_insn "*scc_si"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operator:SI 2 "noov_compare_op"
			   [(match_operand 1 "icc_or_fcc_reg_operand" "")
			    (const_int 0)]))]
  ""
  "* return output_scc_insn (operands, insn); "
  [(set_attr "type" "multi")
   (set_attr "length" "3")])

(define_split
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operator:SI 2 "noov_compare_op"
			   [(match_operand 1 "icc_or_fcc_reg_operand" "")
			    (const_int 0)]))]
  ;; 32 bit LTU/GEU are better implemented using addx/subx
  "TARGET_V9 && REGNO (operands[1]) == SPARC_ICC_REG
   && (GET_MODE (operands[1]) == CCXmode
       || (GET_CODE (operands[2]) != LTU && GET_CODE (operands[2]) != GEU))"
  [(set (match_dup 0) (const_int 0))
   (set (match_dup 0)
	(if_then_else:SI (match_op_dup:SI 2 [(match_dup 1) (const_int 0)])
			 (const_int 1)
			 (match_dup 0)))]
  "")

(define_insn "*scc_di"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(match_operator:DI 2 "noov_compare_op"
			   [(match_operand 1 "icc_or_fcc_reg_operand" "")
			    (const_int 0)]))]
  "TARGET_ARCH64"
  "* return output_scc_insn (operands, insn); "
  [(set_attr "type" "multi")
   (set_attr "length" "3")])

;; These control RTL generation for conditional jump insns

;; The quad-word fp compare library routines all return nonzero to indicate
;; true, which is different from the equivalent libgcc routines, so we must
;; handle them specially here.

(define_expand "beq"
  [(set (pc)
	(if_then_else (eq (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (TARGET_ARCH64 && sparc_compare_op1 == const0_rtx
      && GET_CODE (sparc_compare_op0) == REG
      && GET_MODE (sparc_compare_op0) == DImode)
    {
      emit_v9_brxx_insn (EQ, sparc_compare_op0, operands[0]);
      DONE;
    }
  else if (GET_MODE (sparc_compare_op0) == TFmode && ! TARGET_HARD_QUAD)
    {
      emit_float_lib_cmp (sparc_compare_op0, sparc_compare_op1, EQ);
      emit_jump_insn (gen_bne (operands[0]));
      DONE;
    }      
  operands[1] = gen_compare_reg (EQ, sparc_compare_op0, sparc_compare_op1);
}")

(define_expand "bne"
  [(set (pc)
	(if_then_else (ne (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (TARGET_ARCH64 && sparc_compare_op1 == const0_rtx
      && GET_CODE (sparc_compare_op0) == REG
      && GET_MODE (sparc_compare_op0) == DImode)
    {
      emit_v9_brxx_insn (NE, sparc_compare_op0, operands[0]);
      DONE;
    }
  else if (GET_MODE (sparc_compare_op0) == TFmode && ! TARGET_HARD_QUAD)
    {
      emit_float_lib_cmp (sparc_compare_op0, sparc_compare_op1, NE);
      emit_jump_insn (gen_bne (operands[0]));
      DONE;
    }      
  operands[1] = gen_compare_reg (NE, sparc_compare_op0, sparc_compare_op1);
}")

(define_expand "bgt"
  [(set (pc)
	(if_then_else (gt (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "
{
  if (TARGET_ARCH64 && sparc_compare_op1 == const0_rtx
      && GET_CODE (sparc_compare_op0) == REG
      && GET_MODE (sparc_compare_op0) == DImode)
    {
      emit_v9_brxx_insn (GT, sparc_compare_op0, operands[0]);
      DONE;
    }
  else if (GET_MODE (sparc_compare_op0) == TFmode && ! TARGET_HARD_QUAD)
    {
      emit_float_lib_cmp (sparc_compare_op0, sparc_compare_op1, GT);
      emit_jump_insn (gen_bne (operands[0]));
      DONE;
    }      
  operands[1] = gen_compare_reg (GT, sparc_compare_op0, sparc_compare_op1);
}")

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
{
  if (TARGET_ARCH64 && sparc_compare_op1 == const0_rtx
      && GET_CODE (sparc_compare_op0) == REG
      && GET_MODE (sparc_compare_op0) == DImode)
    {
      emit_v9_brxx_insn (LT, sparc_compare_op0, operands[0]);
      DONE;
    }
  else if (GET_MODE (sparc_compare_op0) == TFmode && ! TARGET_HARD_QUAD)
    {
      emit_float_lib_cmp (sparc_compare_op0, sparc_compare_op1, LT);
      emit_jump_insn (gen_bne (operands[0]));
      DONE;
    }      
  operands[1] = gen_compare_reg (LT, sparc_compare_op0, sparc_compare_op1);
}")

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
{
  if (TARGET_ARCH64 && sparc_compare_op1 == const0_rtx
      && GET_CODE (sparc_compare_op0) == REG
      && GET_MODE (sparc_compare_op0) == DImode)
    {
      emit_v9_brxx_insn (GE, sparc_compare_op0, operands[0]);
      DONE;
    }
  else if (GET_MODE (sparc_compare_op0) == TFmode && ! TARGET_HARD_QUAD)
    {
      emit_float_lib_cmp (sparc_compare_op0, sparc_compare_op1, GE);
      emit_jump_insn (gen_bne (operands[0]));
      DONE;
    }      
  operands[1] = gen_compare_reg (GE, sparc_compare_op0, sparc_compare_op1);
}")

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
{
  if (TARGET_ARCH64 && sparc_compare_op1 == const0_rtx
      && GET_CODE (sparc_compare_op0) == REG
      && GET_MODE (sparc_compare_op0) == DImode)
    {
      emit_v9_brxx_insn (LE, sparc_compare_op0, operands[0]);
      DONE;
    }
  else if (GET_MODE (sparc_compare_op0) == TFmode && ! TARGET_HARD_QUAD)
    {
      emit_float_lib_cmp (sparc_compare_op0, sparc_compare_op1, LE);
      emit_jump_insn (gen_bne (operands[0]));
      DONE;
    }      
  operands[1] = gen_compare_reg (LE, sparc_compare_op0, sparc_compare_op1);
}")

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

(define_insn "*normal_branch"
  [(set (pc)
	(if_then_else (match_operator 0 "noov_compare_op"
				      [(reg 100) (const_int 0)])
		      (label_ref (match_operand 1 "" ""))
		      (pc)))]
  ""
  "*
{
  return output_cbranch (operands[0], 1, 0,
			 final_sequence && INSN_ANNULLED_BRANCH_P (insn),
			 ! final_sequence, insn);
}"
  [(set_attr "type" "branch")])

(define_insn "*inverted_branch"
  [(set (pc)
	(if_then_else (match_operator 0 "noov_compare_op"
				      [(reg 100) (const_int 0)])
		      (pc)
		      (label_ref (match_operand 1 "" ""))))]
  ""
  "*
{
  return output_cbranch (operands[0], 1, 1,
			 final_sequence && INSN_ANNULLED_BRANCH_P (insn),
			 ! final_sequence, insn);
}"
  [(set_attr "type" "branch")])

(define_insn "*normal_fp_branch"
  [(set (pc)
	(if_then_else (match_operator 1 "comparison_operator"
				      [(match_operand:CCFP 0 "fcc_reg_operand" "c")
				       (const_int 0)])
		      (label_ref (match_operand 2 "" ""))
		      (pc)))]
  ""
  "*
{
  return output_cbranch (operands[1], 2, 0,
			 final_sequence && INSN_ANNULLED_BRANCH_P (insn),
			 ! final_sequence, insn);
}"
  [(set_attr "type" "branch")])

(define_insn "*inverted_fp_branch"
  [(set (pc)
	(if_then_else (match_operator 1 "comparison_operator"
				      [(match_operand:CCFP 0 "fcc_reg_operand" "c")
				       (const_int 0)])
		      (pc)
		      (label_ref (match_operand 2 "" ""))))]
  ""
  "*
{
  return output_cbranch (operands[1], 2, 1,
			 final_sequence && INSN_ANNULLED_BRANCH_P (insn),
			 ! final_sequence, insn);
}"
  [(set_attr "type" "branch")])

(define_insn "*normal_fpe_branch"
  [(set (pc)
	(if_then_else (match_operator 1 "comparison_operator"
				      [(match_operand:CCFPE 0 "fcc_reg_operand" "c")
				       (const_int 0)])
		      (label_ref (match_operand 2 "" ""))
		      (pc)))]
  ""
  "*
{
  return output_cbranch (operands[1], 2, 0,
			 final_sequence && INSN_ANNULLED_BRANCH_P (insn),
			 ! final_sequence, insn);
}"
  [(set_attr "type" "branch")])

(define_insn "*inverted_fpe_branch"
  [(set (pc)
	(if_then_else (match_operator 1 "comparison_operator"
				      [(match_operand:CCFPE 0 "fcc_reg_operand" "c")
				       (const_int 0)])
		      (pc)
		      (label_ref (match_operand 2 "" ""))))]
  ""
  "*
{
  return output_cbranch (operands[1], 2, 1,
			 final_sequence && INSN_ANNULLED_BRANCH_P (insn),
			 ! final_sequence, insn);
}"
  [(set_attr "type" "branch")])

;; Sparc V9-specific jump insns.  None of these are guaranteed to be
;; in the architecture.

;; There are no 32 bit brreg insns.

(define_insn "*normal_int_branch_sp64"
  [(set (pc)
	(if_then_else (match_operator 0 "v9_regcmp_op"
				      [(match_operand:DI 1 "register_operand" "r")
				       (const_int 0)])
		      (label_ref (match_operand 2 "" ""))
		      (pc)))]
  "TARGET_ARCH64"
  "*
{
  return output_v9branch (operands[0], 1, 2, 0,
			  final_sequence && INSN_ANNULLED_BRANCH_P (insn),
			  ! final_sequence);
}"
  [(set_attr "type" "branch")])

(define_insn "*inverted_int_branch_sp64"
  [(set (pc)
	(if_then_else (match_operator 0 "v9_regcmp_op"
				      [(match_operand:DI 1 "register_operand" "r")
				       (const_int 0)])
		      (pc)
		      (label_ref (match_operand 2 "" ""))))]
  "TARGET_ARCH64"
  "*
{
  return output_v9branch (operands[0], 1, 2, 1,
			  final_sequence && INSN_ANNULLED_BRANCH_P (insn),
			  ! final_sequence);
}"
  [(set_attr "type" "branch")])

;; Esoteric move insns (lo_sum, high, pic).

(define_insn "*lo_sum_si"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lo_sum:SI (match_operand:SI 1 "register_operand" "r")
		   (match_operand:SI 2 "immediate_operand" "in")))]
  ""
  ;; V9 needs "add" because of the code models.  We still use "or" for v8
  ;; so we can compare the old compiler with the new.
  "* return TARGET_ARCH64 ? \"add %1,%%lo(%a2),%0\" : \"or %1,%%lo(%a2),%0\";"
  ;; Need to set length for this arith insn because operand2
  ;; is not an "arith_operand".
  [(set_attr "length" "1")])

;; For PIC, symbol_refs are put inside unspec so that the optimizer will not
;; confuse them with real addresses.
(define_insn "pic_lo_sum_si"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lo_sum:SI (match_operand:SI 1 "register_operand" "r")
		   (unspec:SI [(match_operand:SI 2 "immediate_operand" "in")] 0)))]
  "flag_pic"
  ;; V9 needs "add" because of the code models.  We still use "or" for v8
  ;; so we can compare the old compiler with the new.
  "* return TARGET_ARCH64 ? \"add %1,%%lo(%a2),%0\" : \"or %1,%%lo(%a2),%0\";"
  ;; Need to set length for this arith insn because operand2
  ;; is not an "arith_operand".
  [(set_attr "length" "1")])

;; The PIC version of sethi must appear before the non-pic case so that
;; the unspec will not be matched as part of the operand.
;; For PIC, symbol_refs are put inside unspec so that the optimizer will not
;; confuse them with real addresses.
(define_insn "pic_sethi_si"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(high:SI (unspec:SI [(match_operand 1 "" "")] 0)))]
  "flag_pic && check_pic (1)"
  "sethi %%hi(%a1),%0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn "pic_lo_sum_di"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (lo_sum:SI (match_operand:DI 1 "register_operand" "r")
                   (unspec:SI [(match_operand:DI 2 "immediate_operand" "in")] 0)))]
  "TARGET_ARCH64 && flag_pic"
  "add %1,%%lo(%a2),%0"
  [(set_attr "length" "1")])

(define_insn "pic_sethi_di"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (high:SI (unspec:SI [(match_operand 1 "" "")] 0)))]
  "TARGET_ARCH64 && flag_pic && check_pic (1)"
  "sethi %%hi(%a1),%0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn "get_pc"
  [(clobber (reg:SI 15))
   (set (match_operand 0 "register_operand" "=r")
	(unspec [(match_operand 1 "" "") (match_operand 2 "" "")] 2))]
  "flag_pic && REGNO (operands[0]) == 23"
  "sethi %%hi(%a1-4),%0\;call %a2\;add %0,%%lo(%a1+4),%0"
  [(set_attr "length" "3")])

(define_insn "get_pc_via_rdpc"
  [(set (match_operand 0 "register_operand" "=r") (pc))]
  "TARGET_V9"
  "rd %%pc,%0"
  [(set_attr "type" "move")])

(define_insn "*sethi_hi"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(high:HI (match_operand 1 "" "")))]
  "check_pic (1)"
  "sethi %%hi(%a1),%0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

;; This must appear after the PIC sethi so that the PIC unspec will not
;; be matched as part of the operand.
(define_insn "*sethi_si"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(high:SI (match_operand 1 "" "")))]
  "check_pic (1)"
  "sethi %%hi(%a1),%0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn "*lo_sum_di_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(lo_sum:DI (match_operand:DI 1 "register_operand" "0")
		   (match_operand:DI 2 "immediate_operand" "in")))]
  "! TARGET_ARCH64"
  "*
{
  /* Don't output a 64 bit constant, since we can't trust the assembler to
     handle it correctly.  */
  if (GET_CODE (operands[2]) == CONST_DOUBLE)
    operands[2] = GEN_INT (CONST_DOUBLE_LOW (operands[2]));
  else if (GET_CODE (operands[2]) == CONST_INT
	   && HOST_BITS_PER_WIDE_INT > 32
	   && INTVAL (operands[2]) > 0xffffffff)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0xffffffff);

  return \"or %L1,%%lo(%a2),%L0\";
}"
  ;; Need to set length for this arith insn because operand2
  ;; is not an "arith_operand".
  [(set_attr "length" "1")])

;; ??? Optimizer does not handle "or %o1,%lo(0),%o1". How about add?

(define_insn "*lo_sum_di_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(lo_sum:DI (match_operand:DI 1 "register_operand" "0")
		   (match_operand:DI 2 "immediate_operand" "in")))]
  "TARGET_ARCH64"
  "*
{
  /* Don't output a 64 bit constant, since we can't trust the assembler to
     handle it correctly.  */
  if (GET_CODE (operands[2]) == CONST_DOUBLE)
    operands[2] = GEN_INT (CONST_DOUBLE_LOW (operands[2]));
  else if (GET_CODE (operands[2]) == CONST_INT
	   && HOST_BITS_PER_WIDE_INT > 32
	   && INTVAL (operands[2]) > 0xffffffff)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0xffffffff);

  /* Note that we use add here.  This is important because Medium/Anywhere
     code model support depends on it.  */
  return \"add %1,%%lo(%a2),%0\";
}"
  ;; Need to set length for this arith insn because operand2
  ;; is not an "arith_operand".
  [(set_attr "length" "1")])

(define_insn "*sethi_di_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(high:DI (match_operand 1 "" "")))]
  "! TARGET_ARCH64 && check_pic (1)"
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
	return \"mov -1,%0\";
      else
	return \"mov 0,%0\";
    }
  else if (GET_CODE (op1) == CONST_DOUBLE)
    {
      operands[0] = operand_subword (op0, 1, 0, DImode);
      operands[1] = GEN_INT (CONST_DOUBLE_LOW (op1));
      output_asm_insn (\"sethi %%hi(%a1),%0\", operands);

      operands[0] = operand_subword (op0, 0, 0, DImode);
      operands[1] = GEN_INT (CONST_DOUBLE_HIGH (op1));
      return singlemove_string (operands);
    }
  else
    abort ();
  return \"\";
}"
  [(set_attr "type" "move")
   (set_attr "length" "2")])

;;; ??? This pattern originally clobbered a scratch register.  However, this
;;; is invalid, the movdi pattern may not use a temp register because it
;;; may be called from reload to reload a DImode value.  In that case, we
;;; end up with a scratch register that never gets allocated.  To avoid this,
;;; we use global register 1 which is never otherwise used by gcc as a temp.
;;; The correct solution here might be to force DImode constants to memory,
;;; e.g. by using a toc like the romp and rs6000 ports do for addresses, reg
;;; 1 will then no longer need to be considered a fixed reg.

(define_expand "sethi_di_sp64"
  [(parallel
     [(set (match_operand:DI 0 "register_operand" "")
	   (high:DI (match_operand 1 "general_operand" "")))
      (clobber (reg:DI 1))])]
  "TARGET_ARCH64"
  "")

(define_insn "*sethi_di_sp64_const"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(high:DI (match_operand 1 "const_double_operand" "")))
   (clobber (reg:DI 1))]
  "TARGET_ARCH64 && check_pic (1)"
  "*
{
#if HOST_BITS_PER_WIDE_INT == 32
  rtx high, low;
  
  split_double (operands[1], &high, &low);

  if (high == const0_rtx)
    {
      operands[1] = low;
      output_asm_insn (\"sethi %%hi(%a1),%0\", operands);
    }
  else
    {
      operands[1] = high;
      output_asm_insn (singlemove_string (operands), operands);

      operands[1] = low;
      output_asm_insn (\"sllx %0,32,%0\", operands);
      if (low != const0_rtx)
	output_asm_insn (\"sethi %%hi(%a1),%%g1; or %0,%%g1,%0\", operands);
    }
#else
  rtx op = operands[1];

  if (! SPARC_SETHI_P (INTVAL(op)))
    {
      operands[1] = GEN_INT (INTVAL (op) >> 32);
      output_asm_insn (singlemove_string (operands), operands);

      output_asm_insn (\"sllx %0,32,%0\", operands);
      if (INTVAL (op) & 0xffffffff)
	{
	  operands[1] = GEN_INT (INTVAL (op) & 0xffffffff);
	  output_asm_insn (\"sethi %%hi(%a1),%%g1; or %0,%%g1,%0\", operands);
	}
    }
  else
    {
      output_asm_insn (\"sethi %%hi(%a1),%0\", operands);
    }
#endif

  return \"\";
}"
  [(set_attr "type" "move")
   (set_attr "length" "5")])

;; Most of the required support for the various code models is here.
;; We can do this because sparcs need the high insn to load the address.  We
;; just need to get high to do the right thing for each code model.  Then each
;; uses the same "%X+%lo(...)" in the load/store insn, though in the case of
;; the medium/middle code model "%lo" is written "%l44".

;; When TARGET_CM_MEDLOW, assume that the upper 32 bits of symbol addresses are
;; always 0.
;; When TARGET_CM_MEDMID, the executable must be in the low 16 TB of memory.
;; This corresponds to the low 44 bits, and the %[hml]44 relocs are used.
;; ??? Not implemented yet.
;; When TARGET_CM_EMBMEDANY, the text and data segments have a maximum size of
;; 31 bits and may be located anywhere.  EMBMEDANY_BASE_REG contains the start
;; address of the data segment, currently %g4.
;; When TARGET_CM_MEDANY, the text and data segments have a maximum size of 31
;; bits and may be located anywhere.  The maximum offset from any instruction
;; to the label _GLOBAL_OFFSET_TABLE_ is 31 bits.

(define_insn "*sethi_di_medlow"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(high:DI (match_operand 1 "" "")))
  ;; The clobber is here because emit_move_sequence assumes the worst case.
   (clobber (reg:DI 1))]
  "TARGET_CM_MEDLOW && check_pic (1)"
  "sethi %%hi(%a1),%0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn "*sethi_di_medium_pic"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(high:DI (match_operand 1 "sp64_medium_pic_operand" "")))]
  "(TARGET_CM_MEDLOW || TARGET_CM_EMBMEDANY) && check_pic (1)"
  "sethi %%hi(%a1),%0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

;; WARNING: %0 gets %hi(%1)+%g4.
;;          You cannot OR in %lo(%1), it must be added in.

(define_insn "*sethi_di_embmedany_data"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(high:DI (match_operand 1 "data_segment_operand" "")))
  ;; The clobber is here because emit_move_sequence assumes the worst case.
   (clobber (reg:DI 1))]
  "TARGET_CM_EMBMEDANY && check_pic (1)"
  "sethi %%hi(%a1),%0; add %0,%_,%0"
  [(set_attr "type" "move")
   (set_attr "length" "2")])

(define_insn "*sethi_di_embmedany_text"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(high:DI (match_operand 1 "text_segment_operand" "")))
  ;; The clobber is here because emit_move_sequence assumes the worst case.
   (clobber (reg:DI 1))]
  "TARGET_CM_EMBMEDANY && check_pic (1)"
  "sethi %%uhi(%a1),%%g1; or %%g1,%%ulo(%a1),%%g1; sllx %%g1,32,%%g1; sethi %%hi(%a1),%0; or %0,%%g1,%0"
  [(set_attr "type" "move")
   (set_attr "length" "5")])

;; Move instructions

(define_expand "movqi"
  [(set (match_operand:QI 0 "general_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
  "
{
  if (emit_move_sequence (operands, QImode))
    DONE;
}")

(define_insn "*movqi_insn"
  [(set (match_operand:QI 0 "reg_or_nonsymb_mem_operand" "=r,r,r,Q")
	(match_operand:QI 1 "move_operand" "rI,K,Q,rJ"))]
  "! TARGET_LIVE_G0
   && (register_operand (operands[0], QImode)
       || register_operand (operands[1], QImode)
       || operands[1] == const0_rtx)"
  "@
   mov %1,%0
   sethi %%hi(%a1),%0
   ldub %1,%0
   stb %r1,%0"
  [(set_attr "type" "move,move,load,store")
   (set_attr "length" "1")])

(define_insn "*movqi_insn_liveg0"
  [(set (match_operand:QI 0 "reg_or_nonsymb_mem_operand" "=r,r,r,r,r,Q")
	(match_operand:QI 1 "move_operand" "r,J,I,K,Q,r"))]
  "TARGET_LIVE_G0
   && (register_operand (operands[0], QImode)
       || register_operand (operands[1], QImode))"
  "@
   mov %1,%0
   and %0,0,%0
   and %0,0,%0\;or %0,%1,%0
   sethi %%hi(%a1),%0
   ldub %1,%0
   stb %1,%0"
  [(set_attr "type" "move,move,move,move,load,store")
   (set_attr "length" "1,1,2,1,1,1")])

(define_insn "*lo_sum_qi"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(subreg:QI (lo_sum:SI (match_operand:QI 1 "register_operand" "r")
			      (match_operand 2 "immediate_operand" "in")) 0))]
  ""
  "or %1,%%lo(%a2),%0"
  [(set_attr "length" "1")])

(define_insn "*store_qi"
  [(set (mem:QI (match_operand:SI 0 "symbolic_operand" ""))
	(match_operand:QI 1 "reg_or_0_operand" "rJ"))
   (clobber (match_scratch:SI 2 "=&r"))]
  "(reload_completed || reload_in_progress)
   && ! TARGET_PTR64"
  "sethi %%hi(%a0),%2\;stb %r1,[%2+%%lo(%a0)]"
  [(set_attr "type" "store")
   (set_attr "length" "2")])

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  "
{
  if (emit_move_sequence (operands, HImode))
    DONE;
}")

(define_insn "*movhi_insn"
  [(set (match_operand:HI 0 "reg_or_nonsymb_mem_operand" "=r,r,r,Q")
	(match_operand:HI 1 "move_operand" "rI,K,Q,rJ"))]
  "! TARGET_LIVE_G0
   && (register_operand (operands[0], HImode)
       || register_operand (operands[1], HImode)
       || operands[1] == const0_rtx)"
  "@
   mov %1,%0
   sethi %%hi(%a1),%0
   lduh %1,%0
   sth %r1,%0"
  [(set_attr "type" "move,move,load,store")
   (set_attr "length" "1")])

(define_insn "*movhi_insn_liveg0"
  [(set (match_operand:HI 0 "reg_or_nonsymb_mem_operand" "=r,r,r,r,r,Q")
	(match_operand:HI 1 "move_operand" "r,J,I,K,Q,r"))]
  "TARGET_LIVE_G0
   && (register_operand (operands[0], HImode)
       || register_operand (operands[1], HImode))"
  "@
   mov %1,%0
   and %0,0,%0
   and %0,0,%0\;or %0,%1,%0
   sethi %%hi(%a1),%0
   lduh %1,%0
   sth %1,%0"
  [(set_attr "type" "move,move,move,move,load,store")
   (set_attr "length" "1,1,2,1,1,1")])

(define_insn "*lo_sum_hi"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(lo_sum:HI (match_operand:HI 1 "register_operand" "r")
		   (match_operand 2 "immediate_operand" "in")))]
  ""
  "or %1,%%lo(%a2),%0"
  [(set_attr "length" "1")])

(define_insn "*store_hi"
  [(set (mem:HI (match_operand:SI 0 "symbolic_operand" ""))
	(match_operand:HI 1 "reg_or_0_operand" "rJ"))
   (clobber (match_scratch:SI 2 "=&r"))]
  "(reload_completed || reload_in_progress)
   && ! TARGET_PTR64"
  "sethi %%hi(%a0),%2\;sth %r1,[%2+%%lo(%a0)]"
  [(set_attr "type" "store")
   (set_attr "length" "2")])

(define_expand "movsi"
  [(set (match_operand:SI 0 "general_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
  "
{
  if (emit_move_sequence (operands, SImode))
    DONE;
}")

;; We must support both 'r' and 'f' registers here, because combine may
;; convert SFmode hard registers to SImode hard registers when simplifying
;; subreg sets.

;; We cannot combine the similar 'r' and 'f' constraints, because it causes
;; problems with register allocation.  Reload might try to put an integer
;; in an fp register, or an fp number is an integer register.

(define_insn "*movsi_insn"
  [(set (match_operand:SI 0 "reg_or_nonsymb_mem_operand" "=r,f,r,r,f,Q,Q,d")
	(match_operand:SI 1 "move_operand" "rI,!f,K,Q,!Q,rJ,!f,J"))]
  "! TARGET_LIVE_G0
   && (register_operand (operands[0], SImode)
       || register_operand (operands[1], SImode)
       || operands[1] == const0_rtx)
   && (GET_CODE (operands[0]) != REG || ! CONSTANT_P (operands[1])
       || REGNO (operands[0]) < 32
       || REGNO (operands[0]) >= FIRST_PSEUDO_REGISTER)"
  "@
   mov %1,%0
   fmovs %1,%0
   sethi %%hi(%a1),%0
   ld %1,%0
   ld %1,%0
   st %r1,%0
   st %1,%0
   fzeros %0"
  [(set_attr "type" "move,fpmove,move,load,fpload,store,fpstore,fpmove")
   (set_attr "length" "1")])

(define_insn "*movsi_insn_liveg0"
  [(set (match_operand:SI 0 "reg_or_nonsymb_mem_operand" "=r,r,r,f,r,r,f,Q,Q")
	(match_operand:SI 1 "move_operand" "r,J,I,!f,K,Q,!Q,r,!f"))]
  "TARGET_LIVE_G0
   && (register_operand (operands[0], SImode)
       || register_operand (operands[1], SImode))"
  "@
   mov %1,%0
   and %0,0,%0
   and %0,0,%0\;or %0,%1,%0
   fmovs %1,%0
   sethi %%hi(%a1),%0
   ld %1,%0
   ld %1,%0
   st %1,%0
   st %1,%0"
  [(set_attr "type" "move,move,move,fpmove,move,load,fpload,store,fpstore")
   (set_attr "length" "1,1,2,1,1,1,1,1,1")])

(define_insn "*store_si"
  [(set (mem:SI (match_operand:SI 0 "symbolic_operand" ""))
	(match_operand:SI 1 "reg_or_0_operand" "rJ"))
   (clobber (match_scratch:SI 2 "=&r"))]
  "(reload_completed || reload_in_progress)
   && ! TARGET_PTR64"
  "sethi %%hi(%a0),%2\;st %r1,[%2+%%lo(%a0)]"
  [(set_attr "type" "store")
   (set_attr "length" "2")])

(define_expand "movdi"
  [(set (match_operand:DI 0 "reg_or_nonsymb_mem_operand" "")
	(match_operand:DI 1 "general_operand" ""))]
  ""
  "
{
  if (emit_move_sequence (operands, DImode))
    DONE;
}")

;; 32 bit V9 movdi is like regular 32 bit except: a 64 bit zero can be stored
;; to aligned memory with a single instruction, the ldd/std instructions
;; are not used, and constants can not be moved to floating point registers.

(define_insn "*movdi_sp32_v9"
  [(set (match_operand:DI 0 "reg_or_nonsymb_mem_operand" "=r,T,Q,r,r,?e,?e,?Q,?b")
	(match_operand:DI 1 "general_operand" "r,J,r,Q,i,e,Q,e,J"))]
  "TARGET_V9 && ! TARGET_ARCH64
   && (register_operand (operands[0], DImode)
       || register_operand (operands[1], DImode)
       || operands[1] == const0_rtx)
   && (GET_CODE (operands[0]) != REG || ! CONSTANT_P (operands[1])
       || REGNO (operands[0]) < 32
       || REGNO (operands[0]) >= FIRST_PSEUDO_REGISTER)"
  "*
{
  if (which_alternative == 1)
    return \"stx %%g0,%0\";
  if (which_alternative == 8)
    return \"fzero %0\";
  if (FP_REG_P (operands[0]) || FP_REG_P (operands[1]))
    return output_fp_move_double (operands);
  return output_move_double (operands);
}"
  [(set_attr "type" "move,store,store,load,multi,fp,fpload,fpstore,fpmove")
   (set_attr "length" "2,1,3,3,3,2,3,3,1")])

;; SPARC V9 deprecates std.  Split it here.
(define_split
  [(set (match_operand:DI 0 "memory_operand" "=m")
      (match_operand:DI 1 "register_operand" "r"))]
  "TARGET_V9 && ! TARGET_ARCH64 && reload_completed
   && REGNO (operands[1]) < 32 && ! MEM_VOLATILE_P (operands[0])
   && offsettable_memref_p (operands[0])"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
  "operands[3] = gen_highpart (SImode, operands[1]);
   operands[5] = gen_lowpart (SImode, operands[1]);
   operands[4] = adj_offsettable_operand (operands[0], 4);
   PUT_MODE (operands[4], SImode);
   operands[2] = copy_rtx (operands[0]);
   PUT_MODE (operands[2], SImode);")

;; Split register to register moves.
(define_split
  [(set (match_operand:DI 0 "register_operand" "=r")
	(match_operand:DI 1 "arith_double_operand" "rIN"))]
  "! TARGET_ARCH64
   && REGNO (operands[0]) < 32
   && GET_CODE (operands[1]) == REG && REGNO (operands[1]) < 32
   && ! reg_overlap_mentioned_p (operands[0], operands[1])"
  [(set (match_dup 2) (match_dup 4))
   (set (match_dup 3) (match_dup 5))]
  "operands[2] = gen_highpart (SImode, operands[0]);
   operands[3] = gen_lowpart (SImode, operands[0]);
   operands[4] = gen_highpart (SImode, operands[1]);
   operands[5] = gen_lowpart (SImode, operands[1]);")

(define_insn "*movdi_sp32"
  [(set (match_operand:DI 0 "reg_or_nonsymb_mem_operand" "=r,T,U,Q,r,r,?f,?f,?Q")
	(match_operand:DI 1 "general_operand" "r,U,T,r,Q,i,f,Q,f"))]
  "! TARGET_V9
   && (register_operand (operands[0], DImode)
       || register_operand (operands[1], DImode)
       || operands[1] == const0_rtx)"
  "*
{
  if (FP_REG_P (operands[0]) || FP_REG_P (operands[1]))
    return output_fp_move_double (operands);
  return output_move_double (operands);
}"
  [(set_attr "type" "move,store,load,store,load,multi,fp,fpload,fpstore")
   (set_attr "length" "2,1,1,3,3,3,2,3,3")])

;;; ??? The trick used below can be extended to load any negative 32 bit
;;; constant in two instructions.  Currently the compiler will use HIGH/LO_SUM
;;; for anything not matching the HIK constraints, which results in 5
;;; instructions.  Positive 32 bit constants can be loaded in the obvious way
;;; with sethi/ori.  To extend the trick, in the xor instruction, use 
;;; xor %o0, ((op1 & 0x3ff) | -0x400), %o0
;;; This needs the original value of operands[1], not the inverted value.

(define_insn "*movdi_sp64_insn"
  [(set (match_operand:DI 0 "reg_or_nonsymb_mem_operand" "=r,r,r,Q,?e,?e,?Q")
	(match_operand:DI 1 "move_operand" "rI,K,Q,rJ,e,Q,e"))]
  "TARGET_ARCH64
   && (register_operand (operands[0], DImode)
       || register_operand (operands[1], DImode)
       || operands[1] == const0_rtx)"
  "*
{
  switch (which_alternative)
    {
    case 0:
      return \"mov %1,%0\";
    case 1:
      /* Sethi does not sign extend, so we must use a little trickery
	 to use it for negative numbers.  Invert the constant before
	 loading it in, then use a xor immediate to invert the loaded bits
	 (along with the upper 32 bits) to the desired constant.  This
	 works because the sethi and immediate fields overlap.  */

      if ((INTVAL (operands[1]) & 0x80000000) == 0)
	return \"sethi %%hi(%a1),%0\";
      else
	{
	  operands[1] = GEN_INT (~INTVAL (operands[1]));
	  output_asm_insn (\"sethi %%hi(%a1),%0\", operands);
	  /* The low 10 bits are already zero, but invert the rest.
	     Assemblers don't accept 0x1c00, so use -0x400 instead.  */
	  return \"xor %0,-0x400,%0\";
	}
    case 2:
      return \"ldx %1,%0\";
    case 3:
      return \"stx %r1,%0\";
    case 4:
      return \"fmovd %1,%0\";
    case 5:
      return \"ldd %1,%0\";
    case 6:
      return \"std %1,%0\";
    default:
      abort ();
    }
}"
  [(set_attr "type" "move,move,load,store,fp,fpload,fpstore")
   (set_attr "length" "1,2,1,1,1,1,1")])

;; ??? There's no symbolic (set (mem:DI ...) ...).
;; Experimentation with v9 suggested one isn't needed.

;; Floating point move insns

;; This pattern forces (set (reg:SF ...) (const_double ...))
;; to be reloaded by putting the constant into memory.
;; It must come before the more general movsf pattern.
(define_insn "*movsf_const_insn"
  [(set (match_operand:SF 0 "general_operand" "=f,d,m,?r")
	(match_operand:SF 1 "" "m,G,G,?F"))]
  "TARGET_FPU
   && GET_CODE (operands[1]) == CONST_DOUBLE
   && (GET_CODE (operands[0]) == REG
       || fp_zero_operand (operands[1]))"
  "*
{
  switch (which_alternative)
    {
    case 0:
      return \"ld %1,%0\";
    case 1:
      return \"fzeros %0\";
    case 2:
      return \"st %%g0,%0\";
    case 3:
      return singlemove_string (operands);
    default:
      abort ();
    }
}"
  [(set_attr "type" "fpload,fpmove,store,load")
   (set_attr "length" "1,1,1,2")])

(define_expand "movsf"
  [(set (match_operand:SF 0 "general_operand" "")
	(match_operand:SF 1 "general_operand" ""))]
  ""
  "
{
  if (emit_move_sequence (operands, SFmode))
    DONE;
}")

(define_insn "*movsf_insn"
  [(set (match_operand:SF 0 "reg_or_nonsymb_mem_operand" "=f,f,Q,r,r,Q")
	(match_operand:SF 1 "reg_or_nonsymb_mem_operand"  "f,Q,f,r,Q,r"))]
  "TARGET_FPU
   && (register_operand (operands[0], SFmode)
       || register_operand (operands[1], SFmode))"
  "@
   fmovs %1,%0
   ld %1,%0
   st %1,%0
   mov %1,%0
   ld %1,%0
   st %1,%0"
  [(set_attr "type" "fpmove,fpload,fpstore,move,load,store")])

;; Exactly the same as above, except that all `f' cases are deleted.
;; This is necessary to prevent reload from ever trying to use a `f' reg
;; when -mno-fpu.

(define_insn "*movsf_no_f_insn"
  [(set (match_operand:SF 0 "reg_or_nonsymb_mem_operand" "=r,r,Q")
	(match_operand:SF 1 "reg_or_nonsymb_mem_operand" "r,Q,r"))]
  "! TARGET_FPU
   && (register_operand (operands[0], SFmode)
       || register_operand (operands[1], SFmode))"
  "@
   mov %1,%0
   ld %1,%0
   st %1,%0"
  [(set_attr "type" "move,load,store")])

(define_insn "*store_sf"
  [(set (mem:SF (match_operand:SI 0 "symbolic_operand" "i"))
	(match_operand:SF 1 "reg_or_0_operand" "rfG"))
   (clobber (match_scratch:SI 2 "=&r"))]
  "(reload_completed || reload_in_progress)
   && ! TARGET_PTR64"
  "sethi %%hi(%a0),%2\;st %r1,[%2+%%lo(%a0)]"
  [(set_attr "type" "store")
   (set_attr "length" "2")])

;; This pattern forces (set (reg:DF ...) (const_double ...))
;; to be reloaded by putting the constant into memory.
;; It must come before the more general movdf pattern.

(define_insn "*movdf_const_insn"
  [(set (match_operand:DF 0 "general_operand" "=?r,e,o,d")
	(match_operand:DF 1 "" "?F,m,G,G"))]
  "TARGET_FPU
   && GET_CODE (operands[1]) == CONST_DOUBLE
   && (GET_CODE (operands[0]) == REG
       || fp_zero_operand (operands[1]))"
  "*
{
  switch (which_alternative)
    {
    case 0:
      return output_move_double (operands);
    case 1:
      return output_fp_move_double (operands);
    case 2:
      if (TARGET_ARCH64 || (TARGET_V9 && mem_aligned_8 (operands[0])))
	{
	  return \"stx %%g0,%0\";
	}
      else
	{
	  operands[1] = adj_offsettable_operand (operands[0], 4);
	  return \"st %%g0,%0\;st %%g0,%1\";
	}
    case 3:
      return \"fzero %0\";
    default:
      abort ();
    }
}"
  [(set_attr "type" "load,fpload,store,fpmove")
   (set_attr "length" "3,3,3,1")])

(define_expand "movdf"
  [(set (match_operand:DF 0 "general_operand" "")
	(match_operand:DF 1 "general_operand" ""))]
  ""
  "
{
  if (emit_move_sequence (operands, DFmode))
    DONE;
}")

(define_insn "*movdf_insn"
  [(set (match_operand:DF 0 "reg_or_nonsymb_mem_operand" "=e,Q,e,T,U,r,Q,r")
	(match_operand:DF 1 "reg_or_nonsymb_mem_operand"  "e,e,Q,U,T,r,r,Q"))]
  "TARGET_FPU
   && (register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode))"
  "*
{
  if (FP_REG_P (operands[0]) || FP_REG_P (operands[1]))
    return output_fp_move_double (operands);
  return output_move_double (operands);
}"
  [(set_attr "type" "fp,fpstore,fpload,fpstore,fpload,move,store,load")
   (set_attr "length" "2,3,3,1,1,2,2,2")])

;; Exactly the same as above, except that all `e' cases are deleted.
;; This is necessary to prevent reload from ever trying to use a `e' reg
;; when -mno-fpu.

(define_insn "*movdf_no_e_insn"
  [(set (match_operand:DF 0 "reg_or_nonsymb_mem_operand" "=T,U,r,Q,&r")
	(match_operand:DF 1 "reg_or_nonsymb_mem_operand"  "U,T,r,r,Q"))]
  "! TARGET_FPU
   && (register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode))"
  "* return output_move_double (operands);"
  [(set_attr "type" "store,load,move,store,load")
   (set_attr "length" "1,1,2,3,3")])

;; Must handle overlapping registers here, since parameters can be unaligned
;; in registers.

(define_split
  [(set (match_operand:DF 0 "register_operand" "")
	(match_operand:DF 1 "register_operand" ""))]
  "! TARGET_ARCH64 && reload_completed
   && REGNO (operands[0]) < SPARC_FIRST_V9_FP_REG
   && REGNO (operands[1]) < SPARC_FIRST_V9_FP_REG"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
  "
{
  rtx first_set = operand_subword (operands[0], 0, 0, DFmode);
  rtx second_use = operand_subword (operands[1], 1, 0, DFmode);

  if (REGNO (first_set) == REGNO (second_use))
    {
      operands[2] = operand_subword (operands[0], 1, 0, DFmode);
      operands[3] = second_use;
      operands[4] = first_set;
      operands[5] = operand_subword (operands[1], 0, 0, DFmode);
    }
  else
    {
      operands[2] = first_set;
      operands[3] = operand_subword (operands[1], 0, 0, DFmode);
      operands[4] = operand_subword (operands[0], 1, 0, DFmode);
      operands[5] = second_use;
    }
}")

(define_insn "*store_df"
  [(set (mem:DF (match_operand:SI 0 "symbolic_operand" "i,i"))
	(match_operand:DF 1 "reg_or_0_operand" "re,G"))
   (clobber (match_scratch:SI 2 "=&r,&r"))]
  "(reload_completed || reload_in_progress)
   && ! TARGET_PTR64"
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

;; This pattern forces (set (reg:TF ...) (const_double ...))
;; to be reloaded by putting the constant into memory.
;; It must come before the more general movtf pattern.
(define_insn "*movtf_const_insn"
  [(set (match_operand:TF 0 "general_operand" "=?r,e,o")
	(match_operand:TF 1 "" "?F,m,G"))]
  "TARGET_FPU
   && GET_CODE (operands[1]) == CONST_DOUBLE
   && (GET_CODE (operands[0]) == REG
       || fp_zero_operand (operands[1]))"
  "*
{
  switch (which_alternative)
    {
    case 0:
      return output_move_quad (operands);
    case 1:
      return output_fp_move_quad (operands);
    case 2:
      if (TARGET_ARCH64 || (TARGET_V9 && mem_aligned_8 (operands[0])))
	{
	  operands[1] = adj_offsettable_operand (operands[0], 8);
	  return \"stx %%g0,%0\;stx %%g0,%1\";
	}
      else
	{
	  /* ??? Do we run off the end of the array here? */
	  operands[1] = adj_offsettable_operand (operands[0], 4);
	  operands[2] = adj_offsettable_operand (operands[0], 8);
	  operands[3] = adj_offsettable_operand (operands[0], 12);
	  return \"st %%g0,%0\;st %%g0,%1\;st %%g0,%2\;st %%g0,%3\";
	}
    default:
      abort ();
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
  if (emit_move_sequence (operands, TFmode))
    DONE;
}")

(define_insn "*movtf_insn"
  [(set (match_operand:TF 0 "reg_or_nonsymb_mem_operand" "=e,Q,e,r,Q,r")
	(match_operand:TF 1 "reg_or_nonsymb_mem_operand"  "e,e,Q,r,r,Q"))]
  "TARGET_FPU
   && (register_operand (operands[0], TFmode)
       || register_operand (operands[1], TFmode))"
  "*
{
  if (FP_REG_P (operands[0]) || FP_REG_P (operands[1]))
    return output_fp_move_quad (operands);
  return output_move_quad (operands);
}"
  [(set_attr "type" "fp,fpstore,fpload,move,store,load")
   (set_attr "length" "5,4,4,5,4,4")])

;; Exactly the same as above, except that all `e' cases are deleted.
;; This is necessary to prevent reload from ever trying to use a `e' reg
;; when -mno-fpu.

(define_insn "*movtf_no_e_insn"
  [(set (match_operand:TF 0 "reg_or_nonsymb_mem_operand" "=r,Q,&r")
	(match_operand:TF 1 "reg_or_nonsymb_mem_operand" "r,r,Q"))]
  "! TARGET_FPU
   && (register_operand (operands[0], TFmode)
       || register_operand (operands[1], TFmode))"
  "*
{
  if (FP_REG_P (operands[0]) || FP_REG_P (operands[1]))
    return output_fp_move_quad (operands);
  return output_move_quad (operands);
}"
  [(set_attr "type" "move,store,load")
   (set_attr "length" "4,5,5")])

;; This is disabled because it does not work.  Long doubles have only 8
;; byte alignment.  Adding an offset of 8 or 12 to an 8 byte aligned %lo may 
;; cause it to overflow.  See also GO_IF_LEGITIMATE_ADDRESS.
(define_insn "*store_tf"
  [(set (mem:TF (match_operand:SI 0 "symbolic_operand" "i,i"))
	(match_operand:TF 1 "reg_or_0_operand" "re,G"))
   (clobber (match_scratch:SI 2 "=&r,&r"))]
  "0 && (reload_completed || reload_in_progress)
   && ! TARGET_PTR64"
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

;; Sparc V9 conditional move instructions.

;; We can handle larger constants here for some flavors, but for now we keep
;; it simple and only allow those constants supported by all flavours.
;; Note that emit_conditional_move canonicalizes operands 2,3 so that operand
;; 3 contains the constant if one is present, but we handle either for
;; generality (sparc.c puts a constant in operand 2).

(define_expand "movqicc"
  [(set (match_operand:QI 0 "register_operand" "")
	(if_then_else:QI (match_operand 1 "comparison_operator" "")
			 (match_operand:QI 2 "arith10_operand" "")
			 (match_operand:QI 3 "arith10_operand" "")))]
  "TARGET_V9"
  "
{
  enum rtx_code code = GET_CODE (operands[1]);

  if (GET_MODE (sparc_compare_op0) == DImode
      && ! TARGET_ARCH64)
    FAIL;

  if (sparc_compare_op1 == const0_rtx
      && GET_CODE (sparc_compare_op0) == REG
      && GET_MODE (sparc_compare_op0) == DImode
      && v9_regcmp_p (code))
    {
      operands[1] = gen_rtx_fmt_ee (code, DImode,
			     sparc_compare_op0, sparc_compare_op1);
    }
  else
    {
      rtx cc_reg = gen_compare_reg (code,
				    sparc_compare_op0, sparc_compare_op1);
      operands[1] = gen_rtx_fmt_ee (code, GET_MODE (cc_reg), cc_reg, const0_rtx);
    }
}")

(define_expand "movhicc"
  [(set (match_operand:HI 0 "register_operand" "")
	(if_then_else:HI (match_operand 1 "comparison_operator" "")
			 (match_operand:HI 2 "arith10_operand" "")
			 (match_operand:HI 3 "arith10_operand" "")))]
  "TARGET_V9"
  "
{
  enum rtx_code code = GET_CODE (operands[1]);

  if (GET_MODE (sparc_compare_op0) == DImode
      && ! TARGET_ARCH64)
    FAIL;

  if (sparc_compare_op1 == const0_rtx
      && GET_CODE (sparc_compare_op0) == REG
      && GET_MODE (sparc_compare_op0) == DImode
      && v9_regcmp_p (code))
    {
      operands[1] = gen_rtx_fmt_ee (code, DImode,
			     sparc_compare_op0, sparc_compare_op1);
    }
  else
    {
      rtx cc_reg = gen_compare_reg (code,
				    sparc_compare_op0, sparc_compare_op1);
      operands[1] = gen_rtx_fmt_ee (code, GET_MODE (cc_reg), cc_reg, const0_rtx);
    }
}")

(define_expand "movsicc"
  [(set (match_operand:SI 0 "register_operand" "")
	(if_then_else:SI (match_operand 1 "comparison_operator" "")
			 (match_operand:SI 2 "arith10_operand" "")
			 (match_operand:SI 3 "arith10_operand" "")))]
  "TARGET_V9"
  "
{
  enum rtx_code code = GET_CODE (operands[1]);
  enum machine_mode op0_mode = GET_MODE (sparc_compare_op0);

  if (sparc_compare_op1 == const0_rtx
      && GET_CODE (sparc_compare_op0) == REG
      && ((TARGET_ARCH64 && op0_mode == DImode && v9_regcmp_p (code))
	  || (op0_mode == SImode && v8plus_regcmp_p (code))))
    {
      operands[1] = gen_rtx_fmt_ee (code, op0_mode,
			     sparc_compare_op0, sparc_compare_op1);
    }
  else
    {
      rtx cc_reg = gen_compare_reg (code,
				    sparc_compare_op0, sparc_compare_op1);
      operands[1] = gen_rtx_fmt_ee (code, GET_MODE (cc_reg),
				    cc_reg, const0_rtx);
    }
}")

(define_expand "movdicc"
  [(set (match_operand:DI 0 "register_operand" "")
	(if_then_else:DI (match_operand 1 "comparison_operator" "")
			 (match_operand:DI 2 "arith10_double_operand" "")
			 (match_operand:DI 3 "arith10_double_operand" "")))]
  "TARGET_ARCH64"
  "
{
  enum rtx_code code = GET_CODE (operands[1]);

  if (sparc_compare_op1 == const0_rtx
      && GET_CODE (sparc_compare_op0) == REG
      && GET_MODE (sparc_compare_op0) == DImode
      && v9_regcmp_p (code))
    {
      operands[1] = gen_rtx_fmt_ee (code, DImode,
			     sparc_compare_op0, sparc_compare_op1);
    }
  else
    {
      rtx cc_reg = gen_compare_reg (code,
				    sparc_compare_op0, sparc_compare_op1);
      operands[1] = gen_rtx_fmt_ee (code, GET_MODE (cc_reg),
				    cc_reg, const0_rtx);
    }
}")

(define_expand "movsfcc"
  [(set (match_operand:SF 0 "register_operand" "")
	(if_then_else:SF (match_operand 1 "comparison_operator" "")
			 (match_operand:SF 2 "register_operand" "")
			 (match_operand:SF 3 "register_operand" "")))]
  "TARGET_V9 && TARGET_FPU"
  "
{
  enum rtx_code code = GET_CODE (operands[1]);

  if (GET_MODE (sparc_compare_op0) == DImode
      && ! TARGET_ARCH64)
    FAIL;

  if (sparc_compare_op1 == const0_rtx
      && GET_CODE (sparc_compare_op0) == REG
      && GET_MODE (sparc_compare_op0) == DImode
      && v9_regcmp_p (code))
    {
      operands[1] = gen_rtx_fmt_ee (code, DImode,
			     sparc_compare_op0, sparc_compare_op1);
    }
  else
    {
      rtx cc_reg = gen_compare_reg (code,
				    sparc_compare_op0, sparc_compare_op1);
      operands[1] = gen_rtx_fmt_ee (code, GET_MODE (cc_reg), cc_reg, const0_rtx);
    }
}")

(define_expand "movdfcc"
  [(set (match_operand:DF 0 "register_operand" "")
	(if_then_else:DF (match_operand 1 "comparison_operator" "")
			 (match_operand:DF 2 "register_operand" "")
			 (match_operand:DF 3 "register_operand" "")))]
  "TARGET_V9 && TARGET_FPU"
  "
{
  enum rtx_code code = GET_CODE (operands[1]);

  if (GET_MODE (sparc_compare_op0) == DImode
      && ! TARGET_ARCH64)
    FAIL;

  if (sparc_compare_op1 == const0_rtx
      && GET_CODE (sparc_compare_op0) == REG
      && GET_MODE (sparc_compare_op0) == DImode
      && v9_regcmp_p (code))
    {
      operands[1] = gen_rtx_fmt_ee (code, DImode,
			     sparc_compare_op0, sparc_compare_op1);
    }
  else
    {
      rtx cc_reg = gen_compare_reg (code,
				    sparc_compare_op0, sparc_compare_op1);
      operands[1] = gen_rtx_fmt_ee (code, GET_MODE (cc_reg), cc_reg, const0_rtx);
    }
}")

(define_expand "movtfcc"
  [(set (match_operand:TF 0 "register_operand" "")
	(if_then_else:TF (match_operand 1 "comparison_operator" "")
			 (match_operand:TF 2 "register_operand" "")
			 (match_operand:TF 3 "register_operand" "")))]
  "TARGET_V9 && TARGET_FPU"
  "
{
  enum rtx_code code = GET_CODE (operands[1]);

  if (GET_MODE (sparc_compare_op0) == DImode
      && ! TARGET_ARCH64)
    FAIL;

  if (sparc_compare_op1 == const0_rtx
      && GET_CODE (sparc_compare_op0) == REG
      && GET_MODE (sparc_compare_op0) == DImode
      && v9_regcmp_p (code))
    {
      operands[1] = gen_rtx_fmt_ee (code, DImode,
			     sparc_compare_op0, sparc_compare_op1);
    }
  else
    {
      rtx cc_reg = gen_compare_reg (code,
				    sparc_compare_op0, sparc_compare_op1);
      operands[1] = gen_rtx_fmt_ee (code, GET_MODE (cc_reg), cc_reg, const0_rtx);
    }
}")

;; Conditional move define_insns.

(define_insn "*movqi_cc_sp64"
  [(set (match_operand:QI 0 "register_operand" "=r,r")
	(if_then_else:QI (match_operator 1 "comparison_operator"
				[(match_operand 2 "icc_or_fcc_reg_operand" "X,X")
				 (const_int 0)])
		      (match_operand:QI 3 "arith11_operand" "rL,0")
		      (match_operand:QI 4 "arith11_operand" "0,rL")))]
  "TARGET_V9"
  "@
   mov%C1 %x2,%3,%0
   mov%c1 %x2,%4,%0"
  [(set_attr "type" "cmove")])

(define_insn "*movhi_cc_sp64"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(if_then_else:HI (match_operator 1 "comparison_operator"
				[(match_operand 2 "icc_or_fcc_reg_operand" "X,X")
				 (const_int 0)])
		      (match_operand:HI 3 "arith11_operand" "rL,0")
		      (match_operand:HI 4 "arith11_operand" "0,rL")))]
  "TARGET_V9"
  "@
   mov%C1 %x2,%3,%0
   mov%c1 %x2,%4,%0"
  [(set_attr "type" "cmove")])

(define_insn "*movsi_cc_sp64"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(if_then_else:SI (match_operator 1 "comparison_operator"
				[(match_operand 2 "icc_or_fcc_reg_operand" "X,X")
				 (const_int 0)])
		      (match_operand:SI 3 "arith11_operand" "rL,0")
		      (match_operand:SI 4 "arith11_operand" "0,rL")))]
  "TARGET_V9"
  "@
   mov%C1 %x2,%3,%0
   mov%c1 %x2,%4,%0"
  [(set_attr "type" "cmove")])

;; ??? The constraints of operands 3,4 need work.
(define_insn "*movdi_cc_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(if_then_else:DI (match_operator 1 "comparison_operator"
				[(match_operand 2 "icc_or_fcc_reg_operand" "X,X")
				 (const_int 0)])
		      (match_operand:DI 3 "arith11_double_operand" "rLH,0")
		      (match_operand:DI 4 "arith11_double_operand" "0,rLH")))]
  "TARGET_ARCH64"
  "@
   mov%C1 %x2,%3,%0
   mov%c1 %x2,%4,%0"
  [(set_attr "type" "cmove")])

(define_insn "*movsf_cc_sp64"
  [(set (match_operand:SF 0 "register_operand" "=f,f")
	(if_then_else:SF (match_operator 1 "comparison_operator"
				[(match_operand 2 "icc_or_fcc_reg_operand" "X,X")
				 (const_int 0)])
		      (match_operand:SF 3 "register_operand" "f,0")
		      (match_operand:SF 4 "register_operand" "0,f")))]
  "TARGET_V9 && TARGET_FPU"
  "@
   fmovs%C1 %x2,%3,%0
   fmovs%c1 %x2,%4,%0"
  [(set_attr "type" "fpcmove")])

(define_insn "*movdf_cc_sp64"
  [(set (match_operand:DF 0 "register_operand" "=e,e")
	(if_then_else:DF (match_operator 1 "comparison_operator"
				[(match_operand 2 "icc_or_fcc_reg_operand" "X,X")
				 (const_int 0)])
		      (match_operand:DF 3 "register_operand" "e,0")
		      (match_operand:DF 4 "register_operand" "0,e")))]
  "TARGET_V9 && TARGET_FPU"
  "@
   fmovd%C1 %x2,%3,%0
   fmovd%c1 %x2,%4,%0"
  [(set_attr "type" "fpcmove")])

(define_insn "*movtf_cc_sp64"
  [(set (match_operand:TF 0 "register_operand" "=e,e")
	(if_then_else:TF (match_operator 1 "comparison_operator"
				[(match_operand 2 "icc_or_fcc_reg_operand" "X,X")
				 (const_int 0)])
		      (match_operand:TF 3 "register_operand" "e,0")
		      (match_operand:TF 4 "register_operand" "0,e")))]
  "TARGET_V9 && TARGET_FPU && TARGET_HARD_QUAD"
  "@
   fmovq%C1 %x2,%3,%0
   fmovq%c1 %x2,%4,%0"
  [(set_attr "type" "fpcmove")])

(define_insn "*movqi_cc_reg_sp64"
  [(set (match_operand:QI 0 "register_operand" "=r,r")
	(if_then_else:QI (match_operator 1 "v9_regcmp_op"
				[(match_operand:DI 2 "register_operand" "r,r")
				 (const_int 0)])
		      (match_operand:QI 3 "arith10_operand" "rM,0")
		      (match_operand:QI 4 "arith10_operand" "0,rM")))]
  "TARGET_ARCH64"
  "@
   movr%D1 %2,%r3,%0
   movr%d1 %2,%r4,%0"
  [(set_attr "type" "cmove")])

(define_insn "*movhi_cc_reg_sp64"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(if_then_else:HI (match_operator 1 "v9_regcmp_op"
				[(match_operand:DI 2 "register_operand" "r,r")
				 (const_int 0)])
		      (match_operand:HI 3 "arith10_operand" "rM,0")
		      (match_operand:HI 4 "arith10_operand" "0,rM")))]
  "TARGET_ARCH64"
  "@
   movr%D1 %2,%r3,%0
   movr%d1 %2,%r4,%0"
  [(set_attr "type" "cmove")])

(define_insn "*movsi_cc_reg_sp64"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(if_then_else:SI (match_operator 1 "v9_regcmp_op"
				[(match_operand:DI 2 "register_operand" "r,r")
				 (const_int 0)])
		      (match_operand:SI 3 "arith10_operand" "rM,0")
		      (match_operand:SI 4 "arith10_operand" "0,rM")))]
  "TARGET_ARCH64"
  "@
   movr%D1 %2,%r3,%0
   movr%d1 %2,%r4,%0"
  [(set_attr "type" "cmove")])

;; On UltraSPARC this is slightly worse than cmp/mov %icc if the register
;; needs to be zero extended but better on average.
(define_insn "*movsi_cc_reg_v8plus"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(if_then_else:SI (match_operator 1 "v8plus_regcmp_op"
				[(match_operand:SI 2 "register_operand" "r,r")
				 (const_int 0)])
		      (match_operand:SI 3 "arith10_operand" "rM,0")
		      (match_operand:SI 4 "arith10_operand" "0,rM")))]
  "TARGET_V9"
  "*
{
  if (! sparc_check_64 (operands[2], insn))
    output_asm_insn (\"srl %2,0,%2\", operands);
  if (which_alternative == 0)
    return \"movr%D1 %2,%r3,%0\";
  return \"movr%d1 %2,%r4,%0\";
}"
  [(set_attr "type" "cmove")
   (set_attr "length" "2")])

;; To work well this needs to know the current insn, but that is not an
;; argument to gen_split_*.

(define_split
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(if_then_else:SI (match_operator 1 "v8plus_regcmp_op"
				[(match_operand:SI 2 "register_operand" "r,r")
				 (const_int 0)])
		      (match_operand:SI 3 "arith10_operand" "rM,0")
		      (match_operand:SI 4 "arith10_operand" "0,rM")))]
  "reload_completed"
  [(set (match_dup 0)
	(unspec:SI [(match_dup 1) (match_dup 3) (match_dup 4)] 9))]
  "if (! sparc_check_64 (operands[2], NULL_RTX))
     emit_insn (gen_v8plus_clear_high (operands[2], operands[2]));")

;; A conditional move with the condition argument known to be zero extended
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(unspec:SI [(match_operator 1 "v8plus_regcmp_op"
				    [(match_operand:SI 2 "register_operand" "r,r")
				     (const_int 0)])
		    (match_operand:SI 3 "arith10_operand" "rM,0")
		    (match_operand:SI 4 "arith10_operand" "0,rM")] 9))]
  "TARGET_V9"
  "@
   movr%D1 %2,%r3,%0
   movr%d1 %2,%r4,%0"
  [(set_attr "type" "cmove")])

;; ??? The constraints of operands 3,4 need work.
(define_insn "*movdi_cc_reg_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(if_then_else:DI (match_operator 1 "v9_regcmp_op"
				[(match_operand:DI 2 "register_operand" "r,r")
				 (const_int 0)])
		      (match_operand:DI 3 "arith10_double_operand" "rMH,0")
		      (match_operand:DI 4 "arith10_double_operand" "0,rMH")))]
  "TARGET_ARCH64"
  "@
   movr%D1 %2,%r3,%0
   movr%d1 %2,%r4,%0"
  [(set_attr "type" "cmove")])

(define_insn "*movsf_cc_reg_sp64"
  [(set (match_operand:SF 0 "register_operand" "=f,f")
	(if_then_else:SF (match_operator 1 "v9_regcmp_op"
				[(match_operand:DI 2 "register_operand" "r,r")
				 (const_int 0)])
		      (match_operand:SF 3 "register_operand" "f,0")
		      (match_operand:SF 4 "register_operand" "0,f")))]
  "TARGET_ARCH64 && TARGET_FPU"
  "@
   fmovrs%D1 %2,%3,%0
   fmovrs%d1 %2,%4,%0"
  [(set_attr "type" "fpcmove")])

(define_insn "*movdf_cc_reg_sp64"
  [(set (match_operand:DF 0 "register_operand" "=e,e")
	(if_then_else:DF (match_operator 1 "v9_regcmp_op"
				[(match_operand:DI 2 "register_operand" "r,r")
				 (const_int 0)])
		      (match_operand:DF 3 "register_operand" "e,0")
		      (match_operand:DF 4 "register_operand" "0,e")))]
  "TARGET_ARCH64 && TARGET_FPU"
  "@
   fmovrd%D1 %2,%3,%0
   fmovrd%d1 %2,%4,%0"
  [(set_attr "type" "fpcmove")])

(define_insn "*movtf_cc_reg_sp64"
  [(set (match_operand:TF 0 "register_operand" "=e,e")
	(if_then_else:TF (match_operator 1 "v9_regcmp_op"
				[(match_operand:DI 2 "register_operand" "r,r")
				 (const_int 0)])
		      (match_operand:TF 3 "register_operand" "e,0")
		      (match_operand:TF 4 "register_operand" "0,e")))]
  "TARGET_ARCH64 && TARGET_FPU"
  "@
   fmovrq%D1 %2,%3,%0
   fmovrq%d1 %2,%4,%0"
  [(set_attr "type" "fpcmove")])

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
  rtx shift_16 = GEN_INT (16);
  int op1_subword = 0;

  if (GET_CODE (operand1) == SUBREG)
    {
      op1_subword = SUBREG_WORD (operand1);
      operand1 = XEXP (operand1, 0);
    }

  emit_insn (gen_ashlsi3 (temp, gen_rtx_SUBREG (SImode, operand1,
					 op1_subword),
			  shift_16));
  emit_insn (gen_lshrsi3 (operand0, temp, shift_16));
  DONE;
}")

(define_insn "*zero_extendhisi2_insn"
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

(define_insn "*zero_extendqihi2_insn"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(zero_extend:HI (match_operand:QI 1 "sparc_operand" "r,Q")))]
  "GET_CODE (operands[1]) != CONST_INT"
  "@
   and %1,0xff,%0
   ldub %1,%0"
  [(set_attr "type" "unary,load")
   (set_attr "length" "1")])

(define_expand "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(zero_extend:SI (match_operand:QI 1 "register_operand" "")))]
  ""
  "")

(define_insn "*zero_extendqisi2_insn"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extend:SI (match_operand:QI 1 "sparc_operand" "r,Q")))]
  "GET_CODE (operands[1]) != CONST_INT"
  "@
   and %1,0xff,%0
   ldub %1,%0"
  [(set_attr "type" "unary,load")
   (set_attr "length" "1")])

(define_expand "zero_extendqidi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(zero_extend:DI (match_operand:QI 1 "register_operand" "")))]
  "TARGET_ARCH64"
  "")

(define_insn "*zero_extendqidi2_insn"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(zero_extend:DI (match_operand:QI 1 "sparc_operand" "r,Q")))]
  "TARGET_ARCH64 && GET_CODE (operands[1]) != CONST_INT"
  "@
   and %1,0xff,%0
   ldub %1,%0"
  [(set_attr "type" "unary,load")
   (set_attr "length" "1")])

(define_expand "zero_extendhidi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(zero_extend:DI (match_operand:HI 1 "register_operand" "")))]
  "TARGET_ARCH64"
  "
{
  rtx temp = gen_reg_rtx (DImode);
  rtx shift_48 = GEN_INT (48);
  int op1_subword = 0;

  if (GET_CODE (operand1) == SUBREG)
    {
      op1_subword = SUBREG_WORD (operand1);
      operand1 = XEXP (operand1, 0);
    }

  emit_insn (gen_ashldi3 (temp, gen_rtx_SUBREG (DImode, operand1,
						op1_subword),
			  shift_48));
  emit_insn (gen_lshrdi3 (operand0, temp, shift_48));
  DONE;
}")

(define_insn "*zero_extendhidi2_insn"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (match_operand:HI 1 "memory_operand" "m")))]
  "TARGET_ARCH64"
  "lduh %1,%0"
  [(set_attr "type" "load")])


;; ??? Write truncdisi pattern using sra?

(define_expand "zero_extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(zero_extend:DI (match_operand:SI 1 "register_operand" "")))]
  "TARGET_ARCH64"
  "")

(define_insn "*zero_extendsidi2_insn"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(zero_extend:DI (match_operand:SI 1 "sparc_operand" "r,Q")))]
  "TARGET_ARCH64 && GET_CODE (operands[1]) != CONST_INT"
  "@
   srl %1,0,%0
   lduw %1,%0"
  [(set_attr "type" "unary,load")
   (set_attr "length" "1")])

;; Zero extend a 32 bit value in a 64 bit register.
(define_insn "v8plus_clear_high"
  [(set (match_operand:SI 0 "reg_or_nonsymb_mem_operand" "=r,Q")
	(unspec:SI [(match_operand:SI 1 "register_operand" "r,r")] 10))]
  "TARGET_V9"
  "*
if (which_alternative == 1)
  return \"st %1,%0\";
if (sparc_check_64 (operands[1], insn) > 0)
  return final_sequence ? \"nop\" : \"\";
return \"srl %1,0,%0\";
"
  [(set_attr "type" "shift,store")])

;; Simplify comparisons of extended values.

(define_insn "*cmp_zero_extendqisi2"
  [(set (reg:CC 100)
	(compare:CC (zero_extend:SI (match_operand:QI 0 "register_operand" "r"))
		    (const_int 0)))]
  ""
  "andcc %0,0xff,%%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_zero_extendqisi2_set"
  [(set (reg:CC 100)
	(compare:CC (zero_extend:SI (match_operand:QI 1 "register_operand" "r"))
		    (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (match_dup 1)))]
  ""
  "andcc %1,0xff,%0"
  [(set_attr "type" "unary")])

;; Similarly, handle SI->QI mode truncation followed by a compare.

(define_insn "*cmp_siqi_trunc"
  [(set (reg:CC 100)
	(compare:CC (subreg:QI (match_operand:SI 0 "register_operand" "r") 0)
		    (const_int 0)))]
  ""
  "andcc %0,0xff,%%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_siqi_trunc_set"
  [(set (reg:CC 100)
	(compare:CC (subreg:QI (match_operand:SI 1 "register_operand" "r") 0)
		    (const_int 0)))
   (set (match_operand:QI 0 "register_operand" "=r")
	(match_dup 1))]
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
  rtx shift_16 = GEN_INT (16);
  int op1_subword = 0;

  if (GET_CODE (operand1) == SUBREG)
    {
      op1_subword = SUBREG_WORD (operand1);
      operand1 = XEXP (operand1, 0);
    }

  emit_insn (gen_ashlsi3 (temp, gen_rtx_SUBREG (SImode, operand1,
						op1_subword),
			  shift_16));
  emit_insn (gen_ashrsi3 (operand0, temp, shift_16));
  DONE;
}")

(define_insn "*sign_extendhisi2_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:HI 1 "memory_operand" "m")))]
  ""
  "ldsh %1,%0"
  [(set_attr "type" "sload")])

(define_expand "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "")
	(sign_extend:HI (match_operand:QI 1 "register_operand" "")))]
  ""
  "
{
  rtx temp = gen_reg_rtx (SImode);
  rtx shift_24 = GEN_INT (24);
  int op1_subword = 0;
  int op0_subword = 0;

  if (GET_CODE (operand1) == SUBREG)
    {
      op1_subword = SUBREG_WORD (operand1);
      operand1 = XEXP (operand1, 0);
    }
  if (GET_CODE (operand0) == SUBREG)
    {
      op0_subword = SUBREG_WORD (operand0);
      operand0 = XEXP (operand0, 0);
    }
  emit_insn (gen_ashlsi3 (temp, gen_rtx_SUBREG (SImode, operand1,
						op1_subword),
			  shift_24));
  if (GET_MODE (operand0) != SImode)
    operand0 = gen_rtx_SUBREG (SImode, operand0, op0_subword);
  emit_insn (gen_ashrsi3 (operand0, temp, shift_24));
  DONE;
}")

(define_insn "*sign_extendqihi2_insn"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(sign_extend:HI (match_operand:QI 1 "memory_operand" "m")))]
  ""
  "ldsb %1,%0"
  [(set_attr "type" "sload")])

(define_expand "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(sign_extend:SI (match_operand:QI 1 "register_operand" "")))]
  ""
  "
{
  rtx temp = gen_reg_rtx (SImode);
  rtx shift_24 = GEN_INT (24);
  int op1_subword = 0;

  if (GET_CODE (operand1) == SUBREG)
    {
      op1_subword = SUBREG_WORD (operand1);
      operand1 = XEXP (operand1, 0);
    }

  emit_insn (gen_ashlsi3 (temp, gen_rtx_SUBREG (SImode, operand1,
						op1_subword),
			  shift_24));
  emit_insn (gen_ashrsi3 (operand0, temp, shift_24));
  DONE;
}")

(define_insn "*sign_extendqisi2_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:QI 1 "memory_operand" "m")))]
  ""
  "ldsb %1,%0"
  [(set_attr "type" "sload")])

(define_expand "extendqidi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(sign_extend:DI (match_operand:QI 1 "register_operand" "")))]
  "TARGET_ARCH64"
  "
{
  rtx temp = gen_reg_rtx (DImode);
  rtx shift_56 = GEN_INT (56);
  int op1_subword = 0;

  if (GET_CODE (operand1) == SUBREG)
    {
      op1_subword = SUBREG_WORD (operand1);
      operand1 = XEXP (operand1, 0);
    }

  emit_insn (gen_ashldi3 (temp, gen_rtx_SUBREG (DImode, operand1,
						op1_subword),
			  shift_56));
  emit_insn (gen_ashrdi3 (operand0, temp, shift_56));
  DONE;
}")

(define_insn "*sign_extendqidi2_insn"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI (match_operand:QI 1 "memory_operand" "m")))]
  "TARGET_ARCH64"
  "ldsb %1,%0"
  [(set_attr "type" "sload")])

(define_expand "extendhidi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(sign_extend:DI (match_operand:HI 1 "register_operand" "")))]
  "TARGET_ARCH64"
  "
{
  rtx temp = gen_reg_rtx (DImode);
  rtx shift_48 = GEN_INT (48);
  int op1_subword = 0;

  if (GET_CODE (operand1) == SUBREG)
    {
      op1_subword = SUBREG_WORD (operand1);
      operand1 = XEXP (operand1, 0);
    }

  emit_insn (gen_ashldi3 (temp, gen_rtx_SUBREG (DImode, operand1,
						op1_subword),
			  shift_48));
  emit_insn (gen_ashrdi3 (operand0, temp, shift_48));
  DONE;
}")

(define_insn "*sign_extendhidi2_insn"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI (match_operand:HI 1 "memory_operand" "m")))]
  "TARGET_ARCH64"
  "ldsh %1,%0"
  [(set_attr "type" "load")])

(define_expand "extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(sign_extend:DI (match_operand:SI 1 "register_operand" "")))]
  "TARGET_ARCH64"
  "")

(define_insn "*sign_extendsidi2_insn"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(sign_extend:DI (match_operand:SI 1 "sparc_operand" "r,Q")))]
  "TARGET_ARCH64"
  "@
  sra %1,0,%0
  ldsw %1,%0"
  [(set_attr "type" "unary,sload")
   (set_attr "length" "1")])

;; Special pattern for optimizing bit-field compares.  This is needed
;; because combine uses this as a canonical form.

(define_insn "*cmp_zero_extract"
  [(set (reg:CC 100)
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

  operands[1] = GEN_INT (mask);
  return \"andcc %0,%1,%%g0\";
}")

(define_insn "*cmp_zero_extract_sp64"
  [(set (reg:CCX 100)
	(compare:CCX
	 (zero_extract:DI (match_operand:DI 0 "register_operand" "r")
			  (match_operand:SI 1 "small_int" "n")
			  (match_operand:SI 2 "small_int" "n"))
	 (const_int 0)))]
  "TARGET_ARCH64 && INTVAL (operands[2]) > 51"
  "*
{
  int len = INTVAL (operands[1]);
  int pos = 64 - INTVAL (operands[2]) - len;
  unsigned HOST_WIDE_INT mask = (((unsigned HOST_WIDE_INT) 1 << len) - 1) << pos;

  operands[1] = GEN_INT (mask);
  return \"andcc %0,%1,%%g0\";
}")

;; Conversions between float, double and long double.

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(float_extend:DF
	 (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_FPU"
  "fstod %1,%0"
  [(set_attr "type" "fp")])

(define_insn "extendsftf2"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(float_extend:TF
	 (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fstoq %1,%0"
  [(set_attr "type" "fp")])

(define_insn "extenddftf2"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(float_extend:TF
	 (match_operand:DF 1 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fdtoq %1,%0"
  [(set_attr "type" "fp")])

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float_truncate:SF
	 (match_operand:DF 1 "register_operand" "e")))]
  "TARGET_FPU"
  "fdtos %1,%0"
  [(set_attr "type" "fp")])

(define_insn "trunctfsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float_truncate:SF
	 (match_operand:TF 1 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fqtos %1,%0"
  [(set_attr "type" "fp")])

(define_insn "trunctfdf2"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(float_truncate:DF
	 (match_operand:TF 1 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fqtod %1,%0"
  [(set_attr "type" "fp")])

;; Conversion between fixed point and floating point.

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float:SF (match_operand:SI 1 "register_operand" "f")))]
  "TARGET_FPU"
  "fitos %1,%0"
  [(set_attr "type" "fp")])

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(float:DF (match_operand:SI 1 "register_operand" "f")))]
  "TARGET_FPU"
  "fitod %1,%0"
  [(set_attr "type" "fp")])

(define_insn "floatsitf2"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(float:TF (match_operand:SI 1 "register_operand" "f")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fitoq %1,%0"
  [(set_attr "type" "fp")])

;; Now the same for 64 bit sources.

(define_insn "floatdisf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float:SF (match_operand:DI 1 "register_operand" "e")))]
  "TARGET_V9 && TARGET_FPU"
  "fxtos %1,%0"
  [(set_attr "type" "fp")])

(define_insn "floatdidf2"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(float:DF (match_operand:DI 1 "register_operand" "e")))]
  "TARGET_V9 && TARGET_FPU"
  "fxtod %1,%0"
  [(set_attr "type" "fp")])

(define_insn "floatditf2"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(float:TF (match_operand:DI 1 "register_operand" "e")))]
  "TARGET_V9 && TARGET_FPU && TARGET_HARD_QUAD"
  "fxtoq %1,%0"
  [(set_attr "type" "fp")])

;; Convert a float to an actual integer.
;; Truncation is performed as part of the conversion.

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (fix:SF (match_operand:SF 1 "register_operand" "f"))))]
  "TARGET_FPU"
  "fstoi %1,%0"
  [(set_attr "type" "fp")])

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (fix:DF (match_operand:DF 1 "register_operand" "e"))))]
  "TARGET_FPU"
  "fdtoi %1,%0"
  [(set_attr "type" "fp")])

(define_insn "fix_trunctfsi2"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (fix:TF (match_operand:TF 1 "register_operand" "e"))))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fqtoi %1,%0"
  [(set_attr "type" "fp")])

;; Now the same, for V9 targets

(define_insn "fix_truncsfdi2"
  [(set (match_operand:DI 0 "register_operand" "=e")
	(fix:DI (fix:SF (match_operand:SF 1 "register_operand" "f"))))]
  "TARGET_V9 && TARGET_FPU"
  "fstox %1,%0"
  [(set_attr "type" "fp")])

(define_insn "fix_truncdfdi2"
  [(set (match_operand:DI 0 "register_operand" "=e")
	(fix:DI (fix:DF (match_operand:DF 1 "register_operand" "e"))))]
  "TARGET_V9 && TARGET_FPU"
  "fdtox %1,%0"
  [(set_attr "type" "fp")])

(define_insn "fix_trunctfdi2"
  [(set (match_operand:DI 0 "register_operand" "=e")
	(fix:DI (fix:TF (match_operand:TF 1 "register_operand" "e"))))]
  "TARGET_V9 && TARGET_FPU && TARGET_HARD_QUAD"
  "fqtox %1,%0"
  [(set_attr "type" "fp")])

;;- arithmetic instructions

(define_expand "adddi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI (match_operand:DI 1 "arith_double_operand" "%r")
		 (match_operand:DI 2 "arith_double_operand" "rHI")))]
  ""
  "
{
  if (! TARGET_ARCH64)
    {
      emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
			  gen_rtx_SET (VOIDmode, operands[0],
				   gen_rtx_PLUS (DImode, operands[1],
						 operands[2])),
			  gen_rtx_CLOBBER (VOIDmode,
				   gen_rtx_REG (CCmode, SPARC_ICC_REG)))));
      DONE;
    }
}")

(define_insn "*adddi3_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI (match_operand:DI 1 "arith_double_operand" "%r")
		 (match_operand:DI 2 "arith_double_operand" "rHI")))
   (clobber (reg:CC 100))]
  "! TARGET_ARCH64"
  "*
{
  rtx op2 = operands[2];

  if (GET_CODE (op2) == CONST_INT
      || GET_CODE (op2) == CONST_DOUBLE)
    {
      rtx xoperands[4];
      xoperands[0] = operands[0];
      xoperands[1] = operands[1];
      if (WORDS_BIG_ENDIAN)
	split_double (op2, &xoperands[2], &xoperands[3]);
      else
	split_double (op2, &xoperands[3], &xoperands[2]);
      if (xoperands[3] == const0_rtx && xoperands[0] == xoperands[1])
	output_asm_insn (\"add %H1,%2,%H0\", xoperands);
      else
	output_asm_insn (\"addcc %L1,%3,%L0\;addx %H1,%2,%H0\", xoperands);
      return \"\";
    }
  return \"addcc %L1,%L2,%L0\;addx %H1,%H2,%H0\";
}"
  [(set_attr "length" "2")])


;; Split DImode arithmetic

(define_split
  [(set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI (match_operand:DI 1 "arith_double_operand" "%r")
		 (match_operand:DI 2 "arith_double_operand" "rHI")))
   (clobber (reg:CC 100))]
  "! TARGET_ARCH64 && reload_completed"
  [(parallel [(set (reg:CC_NOOV 100)
		   (compare:CC_NOOV (plus:SI (match_dup 4)
					     (match_dup 5))
				    (const_int 0)))
	      (set (match_dup 3)
		   (plus:SI (match_dup 4) (match_dup 5)))])
   (set (match_dup 6)
	(plus:SI (plus:SI (match_dup 7)
			  (match_dup 8))
		 (ltu:SI (reg:CC_NOOV 100) (const_int 0))))]
  "
{
  operands[3] = gen_lowpart (SImode, operands[0]);
  operands[4] = gen_lowpart (SImode, operands[1]);
  operands[5] = gen_lowpart (SImode, operands[2]);
  operands[6] = gen_highpart (SImode, operands[0]);
  operands[7] = gen_highpart (SImode, operands[1]);
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      if (INTVAL (operands[2]) < 0)
	operands[8] = constm1_rtx;
      else
	operands[8] = const0_rtx;
    }
  else
    operands[8] = gen_highpart (SImode, operands[2]);
}")

(define_split
  [(set (match_operand:DI 0 "register_operand" "=r")
	(minus:DI (match_operand:DI 1 "arith_double_operand" "r")
		  (match_operand:DI 2 "arith_double_operand" "rHI")))
   (clobber (reg:CC 100))]
  "! TARGET_ARCH64 && reload_completed"
  [(parallel [(set (reg:CC_NOOV 100)
		   (compare:CC_NOOV (minus:SI (match_dup 4)
					      (match_dup 5))
				    (const_int 0)))
	      (set (match_dup 3)
		   (minus:SI (match_dup 4) (match_dup 5)))])
   (set (match_dup 6)
	(minus:SI (minus:SI (match_dup 7)
			    (match_dup 8))
		  (ltu:SI (reg:CC_NOOV 100) (const_int 0))))]
  "
{
  operands[3] = gen_lowpart (SImode, operands[0]);
  operands[4] = gen_lowpart (SImode, operands[1]);
  operands[5] = gen_lowpart (SImode, operands[2]);
  operands[6] = gen_highpart (SImode, operands[0]);
  operands[7] = gen_highpart (SImode, operands[1]);
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      if (INTVAL (operands[2]) < 0)
	operands[8] = constm1_rtx;
      else
	operands[8] = const0_rtx;
    }
  else
    operands[8] = gen_highpart (SImode, operands[2]);
}")

;; LTU here means "carry set"
(define_insn "*addx"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (plus:SI (match_operand:SI 1 "arith_operand" "%r")
			  (match_operand:SI 2 "arith_operand" "rI"))
		 (ltu:SI (reg:CC_NOOV 100) (const_int 0))))]
  ""
  "addx %1,%2,%0"
  [(set_attr "type" "unary")])

(define_insn "*subx"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (minus:SI (match_operand:SI 1 "register_operand" "r")
			    (match_operand:SI 2 "arith_operand" "rI"))
		  (ltu:SI (reg:CC_NOOV 100) (const_int 0))))]
  ""
  "subx %1,%2,%0"
  [(set_attr "type" "unary")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
      (plus:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r"))
               (match_operand:DI 2 "register_operand" "r")))
   (clobber (reg:CC 100))]
  "! TARGET_ARCH64"
  "addcc %L2,%1,%L0\;addx %H2,0,%H0"
  [(set_attr "type" "multi")])

(define_insn "*adddi3_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI (match_operand:DI 1 "arith_double_operand" "%r")
		 (match_operand:DI 2 "arith_double_operand" "rHI")))]
  "TARGET_ARCH64"
  "add %1,%2,%0")

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,d")
	(plus:SI (match_operand:SI 1 "arith_operand" "%r,d")
		 (match_operand:SI 2 "arith_operand" "rI,d")))]
  ""
  "@
   add %1,%2,%0
   fpadd32s %1,%2,%0"
  [(set_attr "type" "ialu,fp")])

(define_insn "*cmp_cc_plus"
  [(set (reg:CC_NOOV 100)
	(compare:CC_NOOV (plus:SI (match_operand:SI 0 "arith_operand" "%r")
				  (match_operand:SI 1 "arith_operand" "rI"))
			 (const_int 0)))]
  ""
  "addcc %0,%1,%%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccx_plus"
  [(set (reg:CCX_NOOV 100)
	(compare:CCX_NOOV (plus:DI (match_operand:DI 0 "arith_double_operand" "%r")
				   (match_operand:DI 1 "arith_double_operand" "rHI"))
			  (const_int 0)))]
  "TARGET_ARCH64"
  "addcc %0,%1,%%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_cc_plus_set"
  [(set (reg:CC_NOOV 100)
	(compare:CC_NOOV (plus:SI (match_operand:SI 1 "arith_operand" "%r")
				  (match_operand:SI 2 "arith_operand" "rI"))
			 (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "addcc %1,%2,%0")

(define_insn "*cmp_ccx_plus_set"
  [(set (reg:CCX_NOOV 100)
	(compare:CCX_NOOV (plus:DI (match_operand:DI 1 "arith_double_operand" "%r")
				   (match_operand:DI 2 "arith_double_operand" "rHI"))
			  (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI (match_dup 1) (match_dup 2)))]
  "TARGET_ARCH64"
  "addcc %1,%2,%0")

(define_expand "subdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(minus:DI (match_operand:DI 1 "register_operand" "r")
		  (match_operand:DI 2 "arith_double_operand" "rHI")))]
  ""
  "
{
  if (! TARGET_ARCH64)
    {
      emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
			  gen_rtx_SET (VOIDmode, operands[0],
				   gen_rtx_MINUS (DImode, operands[1],
						  operands[2])),
			  gen_rtx_CLOBBER (VOIDmode,
				   gen_rtx_REG (CCmode, SPARC_ICC_REG)))));
      DONE;
    }
}")

(define_insn "*subdi3_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(minus:DI (match_operand:DI 1 "register_operand" "r")
		  (match_operand:DI 2 "arith_double_operand" "rHI")))
   (clobber (reg:CC 100))]
  "! TARGET_ARCH64"
  "*
{
  rtx op2 = operands[2];

  if (GET_CODE (op2) == CONST_INT
      || GET_CODE (op2) == CONST_DOUBLE)
    {
      rtx xoperands[4];
      xoperands[0] = operands[0];
      xoperands[1] = operands[1];
      if (WORDS_BIG_ENDIAN)
	split_double (op2, &xoperands[2], &xoperands[3]);
      else
	split_double (op2, &xoperands[3], &xoperands[2]);
      if (xoperands[3] == const0_rtx && xoperands[0] == xoperands[1])
	output_asm_insn (\"sub %H1,%2,%H0\", xoperands);
      else
	output_asm_insn (\"subcc %L1,%3,%L0\;subx %H1,%2,%H0\", xoperands);
      return \"\";
    }
  return \"subcc %L1,%L2,%L0\;subx %H1,%H2,%H0\";
}"
  [(set_attr "length" "2")])

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
      (minus:DI (match_operand:DI 1 "register_operand" "r")
                (zero_extend:DI (match_operand:SI 2 "register_operand" "r"))))
   (clobber (reg:CC 100))]
  "! TARGET_ARCH64"
  "subcc %L1,%2,%L0\;addx %H1,0,%H0"
  [(set_attr "type" "multi")])

(define_insn "*subdi3_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(minus:DI (match_operand:DI 1 "register_operand" "r")
		  (match_operand:DI 2 "arith_double_operand" "rHI")))]
  "TARGET_ARCH64"
  "sub %1,%2,%0")

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,d")
	(minus:SI (match_operand:SI 1 "register_operand" "r,d")
		  (match_operand:SI 2 "arith_operand" "rI,d")))]
  ""
  "@
   sub %1,%2,%0
   fpsub32s %1,%2,%0"
  [(set_attr "type" "ialu,fp")])

(define_insn "*cmp_minus_cc"
  [(set (reg:CC_NOOV 100)
	(compare:CC_NOOV (minus:SI (match_operand:SI 0 "register_operand" "r")
				   (match_operand:SI 1 "arith_operand" "rI"))
			 (const_int 0)))]
  ""
  "subcc %0,%1,%%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_minus_ccx"
  [(set (reg:CCX_NOOV 100)
	(compare:CCX_NOOV (minus:DI (match_operand:DI 0 "register_operand" "r")
				    (match_operand:DI 1 "arith_double_operand" "rHI"))
			  (const_int 0)))]
  "TARGET_ARCH64"
  "subcc %0,%1,%%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_minus_cc_set"
  [(set (reg:CC_NOOV 100)
	(compare:CC_NOOV (minus:SI (match_operand:SI 1 "register_operand" "r")
				   (match_operand:SI 2 "arith_operand" "rI"))
			 (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_dup 1) (match_dup 2)))]
  ""
  "subcc %1,%2,%0")

(define_insn "*cmp_minus_ccx_set"
  [(set (reg:CCX_NOOV 100)
	(compare:CCX_NOOV (minus:DI (match_operand:DI 1 "register_operand" "r")
				    (match_operand:DI 2 "arith_double_operand" "rHI"))
			  (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(minus:DI (match_dup 1) (match_dup 2)))]
  "TARGET_ARCH64"
  "subcc %1,%2,%0")

;; Integer Multiply/Divide.

;; The 32 bit multiply/divide instructions are deprecated on v9 and shouldn't
;; we used.  We still use them in 32 bit v9 compilers.
;; The 64 bit v9 compiler will (/should) widen the args and use muldi3.

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mult:SI (match_operand:SI 1 "arith_operand" "%r")
		 (match_operand:SI 2 "arith_operand" "rI")))]
  "TARGET_HARD_MUL"
  "smul %1,%2,%0"
  [(set_attr "type" "imul")])

(define_expand "muldi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (match_operand:DI 1 "arith_double_operand" "%r")
		 (match_operand:DI 2 "arith_double_operand" "rHI")))]
  "TARGET_ARCH64 || TARGET_V8PLUS"
  "
{
  if (TARGET_V8PLUS)
    {
      emit_insn (gen_muldi3_v8plus (operands[0], operands[1], operands[2]));
      DONE;
    }
}")

(define_insn "*muldi3_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (match_operand:DI 1 "arith_double_operand" "%r")
		 (match_operand:DI 2 "arith_double_operand" "rHI")))]
  "TARGET_ARCH64"
  "mulx %1,%2,%0")

;; V8plus wide multiply.
(define_insn "muldi3_v8plus"
  [(set (match_operand:DI 0 "register_operand" "=r,h")
	(mult:DI (match_operand:DI 1 "arith_double_operand" "%r,0")
		 (match_operand:DI 2 "arith_double_operand" "rHI,rHI")))
   (clobber (match_scratch:SI 3 "=&h,X"))
   (clobber (match_scratch:SI 4 "=&h,X"))]
  "TARGET_V8PLUS"
  "*
{
  if (sparc_check_64 (operands[1], insn) <= 0)
    output_asm_insn (\"srl %L1,0,%L1\", operands);
  if (which_alternative == 1)
    output_asm_insn (\"sllx %H1,32,%H1\", operands);
  if (sparc_check_64 (operands[2], insn) <= 0)
    output_asm_insn (\"srl %L2,0,%L2\", operands);
  if (which_alternative == 1)
    return \"or %L1,%H1,%H1\;sllx %H2,32,%L1\;or %L2,%L1,%L1\;mulx %H1,%L1,%L0\;srlx %L0,32,%H0\";
  else
    return \"sllx %H1,32,%3\;sllx %H2,32,%4\;or %L1,%3,%3\;or %L2,%4,%4\;mulx %3,%4,%3\;srlx %3,32,%H0\;mov %3,%L0\";
}"
  [(set_attr "length" "9,8")])

;; It is not known whether this will match.

(define_insn "*cmp_mul_set"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mult:SI (match_operand:SI 1 "arith_operand" "%r")
		 (match_operand:SI 2 "arith_operand" "rI")))
   (set (reg:CC_NOOV 100)
	(compare:CC_NOOV (mult:SI (match_dup 1) (match_dup 2))
			 (const_int 0)))]
  "TARGET_V8 || TARGET_SPARCLITE || TARGET_DEPRECATED_V8_INSNS"
  "smulcc %1,%2,%0"
  [(set_attr "type" "imul")])

(define_expand "mulsidi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" ""))
		 (sign_extend:DI (match_operand:SI 2 "arith_operand" ""))))]
  "TARGET_HARD_MUL"
  "
{
  if (CONSTANT_P (operands[2]))
    {
      if (TARGET_V8PLUS)
	{
	  emit_insn (gen_const_mulsidi3_v8plus (operands[0], operands[1],
						operands[2]));
	  DONE;
	}
      emit_insn (gen_const_mulsidi3 (operands[0], operands[1], operands[2]));
      DONE;
    }
  if (TARGET_V8PLUS)
    {
      emit_insn (gen_mulsidi3_v8plus (operands[0], operands[1], operands[2]));
      DONE;
    }
}")

;; V9 puts the 64 bit product in a 64 bit register.  Only out or global
;; registers can hold 64 bit values in the V8plus environment.
(define_insn "mulsidi3_v8plus"
  [(set (match_operand:DI 0 "register_operand" "=h,r")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r,r"))
		 (sign_extend:DI (match_operand:SI 2 "register_operand" "r,r"))))
   (clobber (match_scratch:SI 3 "=X,&h"))]
  "TARGET_V8PLUS"
  "@
   smul %1,%2,%L0\;srlx %L0,32,%H0
   smul %1,%2,%3\;srlx %3,32,%H0\;mov %3,%L0"
  [(set_attr "length" "2,3")])

(define_insn "const_mulsidi3_v8plus"
  [(set (match_operand:DI 0 "register_operand" "=h,r")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r,r"))
		 (match_operand:SI 2 "small_int" "I,I")))
   (clobber (match_scratch:SI 3 "=X,&h"))]
  "TARGET_V8PLUS"
  "@
   smul %1,%2,%L0\;srlx %L0,32,%H0
   smul %1,%2,%3\;srlx %3,32,%H0\;mov %3,%L0"
  [(set_attr "length" "2,3")])

(define_insn "*mulsidi3_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r"))
		 (sign_extend:DI (match_operand:SI 2 "register_operand" "r"))))]
  "TARGET_HARD_MUL32"
  "*
{
  return TARGET_SPARCLET ? \"smuld %1,%2,%L0\" : \"smul %1,%2,%L0\;rd %%y,%H0\";
}"
  [(set (attr "length")
	(if_then_else (eq_attr "isa" "sparclet")
		      (const_int 1) (const_int 2)))])

;; Extra pattern, because sign_extend of a constant isn't valid.

(define_insn "const_mulsidi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r"))
		 (match_operand:SI 2 "small_int" "I")))]
  "TARGET_HARD_MUL"
  "*
{
  return TARGET_SPARCLET ? \"smuld %1,%2,%L0\" : \"smul %1,%2,%L0\;rd %%y,%H0\";
}"
  [(set (attr "length")
	(if_then_else (eq_attr "isa" "sparclet")
		      (const_int 1) (const_int 2)))])

(define_expand "smulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "")
	(truncate:SI
	 (lshiftrt:DI (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" ""))
			       (sign_extend:DI (match_operand:SI 2 "arith_operand" "")))
		      (const_int 32))))]
  "TARGET_HARD_MUL"
  "
{
  if (CONSTANT_P (operands[2]))
    {
      if (TARGET_V8PLUS)
	{
	  emit_insn (gen_const_smulsi3_highpart_v8plus (operands[0],
							operands[1],
							operands[2],
							GEN_INT (32)));
	  DONE;
	}
      emit_insn (gen_const_smulsi3_highpart (operands[0], operands[1], operands[2]));
      DONE;
    }
  if (TARGET_V8PLUS)
    {
      emit_insn (gen_smulsi3_highpart_v8plus (operands[0], operands[1],
					      operands[2], GEN_INT (32)));
      DONE;
    }
}")

(define_insn "smulsi3_highpart_v8plus"
  [(set (match_operand:SI 0 "register_operand" "=h,r")
	(truncate:SI
	 (lshiftrt:DI (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r,r"))
			       (sign_extend:DI (match_operand:SI 2 "register_operand" "r,r")))
		      (match_operand:SI 3 "const_int_operand" "i,i"))))
   (clobber (match_scratch:SI 4 "=X,&h"))]
  "TARGET_V8PLUS"
  "@
   smul %1,%2,%0\;srlx %0,%3,%0
   smul %1,%2,%4\;srlx %4,%3,%0"
  [(set_attr "length" "2")])

;; The combiner changes TRUNCATE in the previous pattern to SUBREG.
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=h,r")
	(subreg:SI
	 (lshiftrt:DI
	  (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r,r"))
		   (sign_extend:DI (match_operand:SI 2 "register_operand" "r,r")))
	  (match_operand:SI 3 "const_int_operand" "i,i"))
	 1))
   (clobber (match_scratch:SI 4 "=X,&h"))]
  "TARGET_V8PLUS"
  "@
   smul %1,%2,%0\;srlx %0,%3,%0
   smul %1,%2,%4\;srlx %4,%3,%0"
  [(set_attr "length" "2")])

(define_insn "const_smulsi3_highpart_v8plus"
  [(set (match_operand:SI 0 "register_operand" "=h,r")
	(truncate:SI
	 (lshiftrt:DI (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r,r"))
			       (match_operand 2 "small_int" "i,i"))
		      (match_operand:SI 3 "const_int_operand" "i,i"))))
   (clobber (match_scratch:SI 4 "=X,&h"))]
  "TARGET_V8PLUS"
  "@
   smul %1,%2,%0\;srlx %0,%3,%0
   smul %1,%2,%4\;srlx %4,%3,%0"
  [(set_attr "length" "2")])

(define_insn "*smulsi3_highpart_sp32"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(truncate:SI
	 (lshiftrt:DI (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r"))
			       (sign_extend:DI (match_operand:SI 2 "register_operand" "r")))
		      (const_int 32))))]
  "TARGET_HARD_MUL32"
  "smul %1,%2,%%g0\;rd %%y,%0"
  [(set_attr "length" "2")])

(define_insn "const_smulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(truncate:SI
	 (lshiftrt:DI (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r"))
			       (match_operand:SI 2 "register_operand" "r"))
		      (const_int 32))))]
  "TARGET_HARD_MUL32"
  "smul %1,%2,%%g0\;rd %%y,%0"
  [(set_attr "length" "2")])

(define_expand "umulsidi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" ""))
		 (zero_extend:DI (match_operand:SI 2 "uns_arith_operand" ""))))]
  "TARGET_HARD_MUL"
  "
{
  if (CONSTANT_P (operands[2]))
    {
      if (TARGET_V8PLUS)
	{
	  emit_insn (gen_const_umulsidi3_v8plus (operands[0], operands[1],
						 operands[2]));
	  DONE;
	}
      emit_insn (gen_const_umulsidi3 (operands[0], operands[1], operands[2]));
      DONE;
    }
  if (TARGET_V8PLUS)
    {
      emit_insn (gen_umulsidi3_v8plus (operands[0], operands[1], operands[2]));
      DONE;
    }
}")

(define_insn "umulsidi3_v8plus"
  [(set (match_operand:DI 0 "register_operand" "=h,r")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r,r"))
		 (zero_extend:DI (match_operand:SI 2 "register_operand" "r,r"))))
   (clobber (match_scratch:SI 3 "=X,&h"))]
  "TARGET_V8PLUS"
  "@
   umul %1,%2,%L0\;srlx %L0,32,%H0
   umul %1,%2,%3\;srlx %3,32,%H0\;mov %3,%L0"
  [(set_attr "length" "2,3")])

(define_insn "*umulsidi3_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r"))
		 (zero_extend:DI (match_operand:SI 2 "register_operand" "r"))))]
  "TARGET_HARD_MUL32"
  "*
{
  return TARGET_SPARCLET ? \"umuld %1,%2,%L0\" : \"umul %1,%2,%L0\;rd %%y,%H0\";
}"
  [(set (attr "length")
	(if_then_else (eq_attr "isa" "sparclet")
		      (const_int 1) (const_int 2)))])

;; Extra pattern, because sign_extend of a constant isn't valid.

(define_insn "const_umulsidi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r"))
		 (match_operand:SI 2 "uns_small_int" "")))]
  "TARGET_HARD_MUL32"
  "*
{
  return TARGET_SPARCLET ? \"umuld %1,%2,%L0\" : \"umul %1,%2,%L0\;rd %%y,%H0\";
}"
  [(set (attr "length")
	(if_then_else (eq_attr "isa" "sparclet")
		      (const_int 1) (const_int 2)))])

(define_insn "const_umulsidi3_v8plus"
  [(set (match_operand:DI 0 "register_operand" "=h,r")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r,r"))
		 (match_operand:SI 2 "uns_small_int" "")))
   (clobber (match_scratch:SI 3 "=X,h"))]
  "TARGET_V8PLUS"
  "@
   umul %1,%2,%L0\;srlx %L0,32,%H0
   umul %1,%2,%3\;srlx %3,32,%H0\;mov %3,%L0"
  [(set_attr "length" "2,3")])

(define_expand "umulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "")
	(truncate:SI
	 (lshiftrt:DI (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" ""))
			       (zero_extend:DI (match_operand:SI 2 "uns_arith_operand" "")))
		      (const_int 32))))]
  "TARGET_HARD_MUL"
  "
{
  if (CONSTANT_P (operands[2]))
    {
      if (TARGET_V8PLUS)
	{
	  emit_insn (gen_const_umulsi3_highpart_v8plus (operands[0],
							operands[1],
							operands[2],
							GEN_INT (32)));
	  DONE;
	}
      emit_insn (gen_const_umulsi3_highpart (operands[0], operands[1], operands[2]));
      DONE;
    }
  if (TARGET_V8PLUS)
    {
      emit_insn (gen_umulsi3_highpart_v8plus (operands[0], operands[1],
					      operands[2], GEN_INT (32)));
      DONE;
    }
}")

(define_insn "umulsi3_highpart_v8plus"
  [(set (match_operand:SI 0 "register_operand" "=h,r")
	(truncate:SI
	 (lshiftrt:DI (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r,r"))
			       (zero_extend:DI (match_operand:SI 2 "register_operand" "r,r")))
		      (match_operand:SI 3 "const_int_operand" "i,i"))))
   (clobber (match_scratch:SI 4 "=X,h"))]
  "TARGET_V8PLUS"
  "@
   umul %1,%2,%0\;srlx %0,%3,%0
   umul %1,%2,%4\;srlx %4,%3,%0"
  [(set_attr "length" "2")])

(define_insn "const_umulsi3_highpart_v8plus"
  [(set (match_operand:SI 0 "register_operand" "=h,r")
	(truncate:SI
	 (lshiftrt:DI (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r,r"))
			       (match_operand:SI 2 "uns_small_int" ""))
		      (match_operand:SI 3 "const_int_operand" "i,i"))))
   (clobber (match_scratch:SI 4 "=X,h"))]
  "TARGET_V8PLUS"
  "@
   umul %1,%2,%0\;srlx %0,%3,%0
   umul %1,%2,%4\;srlx %4,%3,%0"
  [(set_attr "length" "2")])

(define_insn "*umulsi3_highpart_sp32"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(truncate:SI
	 (lshiftrt:DI (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r"))
			       (zero_extend:DI (match_operand:SI 2 "register_operand" "r")))
		      (const_int 32))))]
  "TARGET_HARD_MUL32"
  "umul %1,%2,%%g0\;rd %%y,%0"
  [(set_attr "length" "2")])

(define_insn "const_umulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(truncate:SI
	 (lshiftrt:DI (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r"))
			       (match_operand:SI 2 "uns_small_int" ""))
		      (const_int 32))))]
  "TARGET_HARD_MUL32"
  "umul %1,%2,%%g0\;rd %%y,%0"
  [(set_attr "length" "2")])

;; The v8 architecture specifies that there must be 3 instructions between
;; a y register write and a use of it for correct results.

(define_insn "divsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(div:SI (match_operand:SI 1 "register_operand" "r,r")
		(match_operand:SI 2 "move_operand" "rI,m")))
   (clobber (match_scratch:SI 3 "=&r,&r"))]
  "TARGET_V8 || TARGET_DEPRECATED_V8_INSNS"
  "*
{
  if (which_alternative == 0)
  if (TARGET_V9)
    return \"sra %1,31,%3\;wr %%g0,%3,%%y\;sdiv %1,%2,%0\";
  else
    return \"sra %1,31,%3\;wr %%g0,%3,%%y\;nop\;nop\;nop\;sdiv %1,%2,%0\";
  else
    if (TARGET_V9)
      return \"sra %1,31,%3\;wr %%g0,%3,%%y\;ld %2,%3\;sdiv %1,%3,%0\";
    else
      return \"sra %1,31,%3\;wr %%g0,%3,%%y\;ld %2,%3\;nop\;nop\;sdiv %1,%3,%0\";
}"
  [(set (attr "length")
	(if_then_else (eq_attr "isa" "v9")
		      (const_int 4) (const_int 7)))])

(define_insn "divdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(div:DI (match_operand:DI 1 "register_operand" "r")
		(match_operand:DI 2 "arith_double_operand" "rHI")))]
  "TARGET_ARCH64"
  "sdivx %1,%2,%0")

;; It is not known whether this will match.

(define_insn "*cmp_sdiv_cc_set"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(div:SI (match_operand:SI 1 "register_operand" "r")
		(match_operand:SI 2 "arith_operand" "rI")))
   (set (reg:CC 100)
	(compare:CC (div:SI (match_dup 1) (match_dup 2))
		    (const_int 0)))
   (clobber (match_scratch:SI 3 "=&r"))]
  "TARGET_V8 || TARGET_DEPRECATED_V8_INSNS"
  "*
{
  if (TARGET_V9)
    return \"sra %1,31,%3\;wr %%g0,%3,%%y\;sdivcc %1,%2,%0\";
  else
    return \"sra %1,31,%3\;wr %%g0,%3,%%y\;nop\;nop\;nop\;sdivcc %1,%2,%0\";
}"
  [(set (attr "length")
	(if_then_else (eq_attr "isa" "v9")
		      (const_int 3) (const_int 6)))])

(define_insn "udivsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,&r,&r")
	(udiv:SI (match_operand:SI 1 "reg_or_nonsymb_mem_operand" "r,r,m")
		 (match_operand:SI 2 "move_operand" "rI,m,r")))]
  "TARGET_V8 || TARGET_DEPRECATED_V8_INSNS"
  "*
{
  output_asm_insn (\"wr %%g0,%%g0,%%y\", operands);
  switch (which_alternative)
    {
    default:
  if (TARGET_V9)
	return \"udiv %1,%2,%0\";
      return \"nop\;nop\;nop\;udiv %1,%2,%0\";
    case 1:
      return \"ld %2,%0\;nop\;nop\;udiv %1,%0,%0\";
    case 2:
      return \"ld %1,%0\;nop\;nop\;udiv %0,%2,%0\";
    }
}"
  [(set (attr "length")
	(if_then_else (and (eq_attr "isa" "v9")
			   (eq_attr "alternative" "0"))
		      (const_int 2) (const_int 5)))])

(define_insn "udivdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(udiv:DI (match_operand:DI 1 "register_operand" "r")
		 (match_operand:DI 2 "arith_double_operand" "rHI")))]
  "TARGET_ARCH64"
  "udivx %1,%2,%0")

;; It is not known whether this will match.

(define_insn "*cmp_udiv_cc_set"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(udiv:SI (match_operand:SI 1 "register_operand" "r")
		(match_operand:SI 2 "arith_operand" "rI")))
   (set (reg:CC 100)
	(compare:CC (udiv:SI (match_dup 1) (match_dup 2))
		    (const_int 0)))]
  "TARGET_V8 || TARGET_DEPRECATED_V8_INSNS"
  "*
{
  if (TARGET_V9)
    return \"wr %%g0,%%g0,%%y\;udivcc %1,%2,%0\";
  else
    return \"wr %%g0,%%g0,%%y\;nop\;nop\;nop\;udivcc %1,%2,%0\";
}"
  [(set (attr "length")
	(if_then_else (eq_attr "isa" "v9")
		      (const_int 2) (const_int 5)))])

; sparclet multiply/accumulate insns

(define_insn "*smacsi"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "%r")
			  (match_operand:SI 2 "arith_operand" "rI"))
		 (match_operand:SI 3 "register_operand" "0")))]
  "TARGET_SPARCLET"
  "smac %1,%2,%0"
  [(set_attr "type" "imul")])

(define_insn "*smacdi"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI (mult:DI (sign_extend:DI
			   (match_operand:SI 1 "register_operand" "%r"))
			  (sign_extend:DI
			   (match_operand:SI 2 "register_operand" "r")))
		 (match_operand:DI 3 "register_operand" "0")))]
  "TARGET_SPARCLET"
  "smacd %1,%2,%L0"
  [(set_attr "type" "imul")])

(define_insn "*umacdi"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI (mult:DI (zero_extend:DI
			   (match_operand:SI 1 "register_operand" "%r"))
			  (zero_extend:DI
			   (match_operand:SI 2 "register_operand" "r")))
		 (match_operand:DI 3 "register_operand" "0")))]
  "TARGET_SPARCLET"
  "umacd %1,%2,%L0"
  [(set_attr "type" "imul")])

;;- Boolean instructions
;; We define DImode `and' so with DImode `not' we can get
;; DImode `andn'.  Other combinations are possible.

(define_expand "anddi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(and:DI (match_operand:DI 1 "arith_double_operand" "")
		(match_operand:DI 2 "arith_double_operand" "")))]
  ""
  "")

(define_insn "*anddi3_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r,b")
	(and:DI (match_operand:DI 1 "arith_double_operand" "%r,b")
		(match_operand:DI 2 "arith_double_operand" "rHI,b")))]
  "! TARGET_ARCH64"
  "*
{
  rtx op2 = operands[2];

  if (which_alternative == 1)
    return \"fand %1,%2,%0\";

  if (GET_CODE (op2) == CONST_INT
      || GET_CODE (op2) == CONST_DOUBLE)
    {
      rtx xoperands[4];
      xoperands[0] = operands[0];
      xoperands[1] = operands[1];
      if (WORDS_BIG_ENDIAN)
	split_double (op2, &xoperands[2], &xoperands[3]);
      else
	split_double (op2, &xoperands[3], &xoperands[2]);
      output_asm_insn (\"and %L1,%3,%L0\;and %H1,%2,%H0\", xoperands);
      return \"\";
    }
  return \"and %1,%2,%0\;and %R1,%R2,%R0\";
}"
  [(set_attr "length" "2,1")])

(define_insn "*anddi3_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(and:DI (match_operand:DI 1 "arith_double_operand" "%r")
		(match_operand:DI 2 "arith_double_operand" "rHI")))]
  "TARGET_ARCH64"
  "and %1,%2,%0")

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,d")
	(and:SI (match_operand:SI 1 "arith_operand" "%r,d")
		(match_operand:SI 2 "arith_operand" "rI,d")))]
  ""
  "@
   and %1,%2,%0
   fands %1,%2,%0"
  [(set_attr "type" "ialu,fp")])

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(and:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "GET_CODE (operands[2]) == CONST_INT
   && !SMALL_INT32 (operands[2])
   && (INTVAL (operands[2]) & 0x3ff) == 0x3ff"
  [(set (match_dup 3) (match_dup 4))
   (set (match_dup 0) (and:SI (not:SI (match_dup 3)) (match_dup 1)))]
  "
{
  operands[4] = GEN_INT (~INTVAL (operands[2]) & 0xffffffff);
}")

;; Split DImode logical operations requiring two instructions.
(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(match_operator:DI 1 "cc_arithop"	; AND, IOR, XOR
			   [(match_operand:DI 2 "register_operand" "")
			    (match_operand:DI 3 "arith_double_operand" "")]))]
  "! TARGET_ARCH64 && reload_completed
   && GET_CODE (operands[0]) == REG && REGNO (operands[0]) < 32"
  [(set (match_dup 4) (match_op_dup:SI 1 [(match_dup 6) (match_dup 8)]))
   (set (match_dup 5) (match_op_dup:SI 1 [(match_dup 7) (match_dup 9)]))]
  "
{
  operands[4] = gen_highpart (SImode, operands[0]);
  operands[5] = gen_lowpart (SImode, operands[0]);
  operands[6] = gen_highpart (SImode, operands[2]);
  operands[7] = gen_lowpart (SImode, operands[2]);
  if (GET_CODE (operands[3]) == CONST_INT)
    {
      if (INTVAL (operands[3]) < 0)
	operands[8] = constm1_rtx;
      else
	operands[8] = const0_rtx;
    }
  else
    operands[8] = gen_highpart (SImode, operands[3]);
  operands[9] = gen_lowpart (SImode, operands[3]);
}")

(define_insn "*and_not_di_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r,b")
	(and:DI (not:DI (match_operand:DI 1 "register_operand" "r,b"))
		(match_operand:DI 2 "register_operand" "r,b")))]
  "! TARGET_ARCH64"
  "@
   andn %2,%1,%0\;andn %R2,%R1,%R0
   fandnot1 %1,%2,%0"
  [(set_attr "length" "2,1")])

(define_insn "*and_not_di_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(and:DI (not:DI (match_operand:DI 1 "register_operand" "r"))
		(match_operand:DI 2 "register_operand" "r")))]
  "TARGET_ARCH64"
  "andn %2,%1,%0")

(define_insn "*and_not_si"
  [(set (match_operand:SI 0 "register_operand" "=r,d")
	(and:SI (not:SI (match_operand:SI 1 "register_operand" "r,d"))
		(match_operand:SI 2 "register_operand" "r,d")))]
  ""
  "@
   andn %2,%1,%0
   fandnot1s %1,%2,%0"
  [(set_attr "type" "ialu,fp")])

(define_expand "iordi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(ior:DI (match_operand:DI 1 "arith_double_operand" "")
		(match_operand:DI 2 "arith_double_operand" "")))]
  ""
  "")

(define_insn "*iordi3_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r,b")
	(ior:DI (match_operand:DI 1 "arith_double_operand" "%r,b")
		(match_operand:DI 2 "arith_double_operand" "rHI,b")))]
  "! TARGET_ARCH64"
  "*
{
  rtx op2 = operands[2];

  if (which_alternative == 1)
    return \"for %1,%2,%0\";

  if (GET_CODE (op2) == CONST_INT
      || GET_CODE (op2) == CONST_DOUBLE)
    {
      rtx xoperands[4];
      xoperands[0] = operands[0];
      xoperands[1] = operands[1];
      if (WORDS_BIG_ENDIAN)
	split_double (op2, &xoperands[2], &xoperands[3]);
      else
	split_double (op2, &xoperands[3], &xoperands[2]);
      output_asm_insn (\"or %L1,%3,%L0\;or %H1,%2,%H0\", xoperands);
      return \"\";
    }
  return \"or %1,%2,%0\;or %R1,%R2,%R0\";
}"
  [(set_attr "length" "2,1")])

(define_insn "*iordi3_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ior:DI (match_operand:DI 1 "arith_double_operand" "%r")
		(match_operand:DI 2 "arith_double_operand" "rHI")))]
  "TARGET_ARCH64"
  "or %1,%2,%0")

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,d")
	(ior:SI (match_operand:SI 1 "arith_operand" "%r,d")
		(match_operand:SI 2 "arith_operand" "rI,d")))]
  ""
  "@
   or %1,%2,%0
   fors %1,%2,%0"
  [(set_attr "type" "ialu,fp")])

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(ior:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "GET_CODE (operands[2]) == CONST_INT
   && !SMALL_INT32 (operands[2])
   && (INTVAL (operands[2]) & 0x3ff) == 0x3ff"
  [(set (match_dup 3) (match_dup 4))
   (set (match_dup 0) (ior:SI (not:SI (match_dup 3)) (match_dup 1)))]
  "
{
  operands[4] = GEN_INT (~INTVAL (operands[2]) & 0xffffffff);
}")

(define_insn "*or_not_di_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r,b")
	(ior:DI (not:DI (match_operand:DI 1 "register_operand" "r,b"))
		(match_operand:DI 2 "register_operand" "r,b")))]
  "! TARGET_ARCH64"
  "@
   orn %2,%1,%0\;orn %R2,%R1,%R0
   fornot1 %1,%2,%0"
  [(set_attr "length" "2,1")])

(define_insn "*or_not_di_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ior:DI (not:DI (match_operand:DI 1 "register_operand" "r"))
		(match_operand:DI 2 "register_operand" "r")))]
  "TARGET_ARCH64"
  "orn %2,%1,%0")

(define_insn "*or_not_si"
  [(set (match_operand:SI 0 "register_operand" "=r,d")
	(ior:SI (not:SI (match_operand:SI 1 "register_operand" "r,d"))
		(match_operand:SI 2 "register_operand" "r,d")))]
  ""
  "@
   orn %2,%1,%0
   fornot1s %1,%2,%0"
  [(set_attr "type" "ialu,fp")])

(define_expand "xordi3"
  [(set (match_operand:DI 0 "register_operand" "")
	(xor:DI (match_operand:DI 1 "arith_double_operand" "")
		(match_operand:DI 2 "arith_double_operand" "")))]
  ""
  "")

(define_insn "*xordi3_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r,b")
	(xor:DI (match_operand:DI 1 "arith_double_operand" "%r,b")
		(match_operand:DI 2 "arith_double_operand" "rHI,b")))]
  "! TARGET_ARCH64"
  "*
{
  rtx op2 = operands[2];

  if (which_alternative == 1)
    return \"fxor %1,%2,%0\";

  if (GET_CODE (op2) == CONST_INT
      || GET_CODE (op2) == CONST_DOUBLE)
    {
      rtx xoperands[4];
      xoperands[0] = operands[0];
      xoperands[1] = operands[1];
      if (WORDS_BIG_ENDIAN)
	split_double (op2, &xoperands[2], &xoperands[3]);
      else
	split_double (op2, &xoperands[3], &xoperands[2]);
      output_asm_insn (\"xor %L1,%3,%L0\;xor %H1,%2,%H0\", xoperands);
      return \"\";
    }
  return \"xor %1,%2,%0\;xor %R1,%R2,%R0\";
}"
  [(set_attr "length" "2,1")
   (set_attr "type" "ialu,fp")])

(define_insn "*xordi3_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(xor:DI (match_operand:DI 1 "arith_double_operand" "%rJ")
		(match_operand:DI 2 "arith_double_operand" "rHI")))]
  "TARGET_ARCH64"
  "xor %r1,%2,%0")

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,d")
	(xor:SI (match_operand:SI 1 "arith_operand" "%rJ,d")
		(match_operand:SI 2 "arith_operand" "rI,d")))]
  ""
  "@
   xor %r1,%2,%0
   fxors %1,%2,%0"
  [(set_attr "type" "ialu,fp")])

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(xor:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "" "")))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "GET_CODE (operands[2]) == CONST_INT
   && !SMALL_INT32 (operands[2])
   && (INTVAL (operands[2]) & 0x3ff) == 0x3ff"
  [(set (match_dup 3) (match_dup 4))
   (set (match_dup 0) (not:SI (xor:SI (match_dup 3) (match_dup 1))))]
  "
{
  operands[4] = GEN_INT (~INTVAL (operands[2]) & 0xffffffff);
}")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(not:SI (xor:SI (match_operand:SI 1 "register_operand" "")
			(match_operand:SI 2 "" ""))))
   (clobber (match_operand:SI 3 "register_operand" ""))]
  "GET_CODE (operands[2]) == CONST_INT
   && !SMALL_INT32 (operands[2])
   && (INTVAL (operands[2]) & 0x3ff) == 0x3ff"
  [(set (match_dup 3) (match_dup 4))
   (set (match_dup 0) (xor:SI (match_dup 3) (match_dup 1)))]
  "
{
  operands[4] = GEN_INT (~INTVAL (operands[2]) & 0xffffffff);
}")

;; xnor patterns.  Note that (a ^ ~b) == (~a ^ b) == ~(a ^ b).
;; Combine now canonicalizes to the rightmost expression.
(define_insn "*xor_not_di_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r,b")
	(not:DI (xor:DI (match_operand:DI 1 "register_operand" "r,b")
			(match_operand:DI 2 "register_operand" "r,b"))))]
  "! TARGET_ARCH64"
  "@
   xnor %1,%2,%0\;xnor %R1,%R2,%R0
   fxnor %1,%2,%0"
  [(set_attr "length" "2,1")
   (set_attr "type" "ialu,fp")])

(define_insn "*xor_not_di_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(not:DI (xor:DI (match_operand:DI 1 "reg_or_0_operand" "rJ")
			(match_operand:DI 2 "arith_double_operand" "rHI"))))]
  "TARGET_ARCH64"
  "xnor %r1,%2,%0"
  [(set_attr "type" "ialu")])

(define_insn "*xor_not_si"
  [(set (match_operand:SI 0 "register_operand" "=r,d")
	(not:SI (xor:SI (match_operand:SI 1 "reg_or_0_operand" "rJ,d")
			(match_operand:SI 2 "arith_operand" "rI,d"))))]
  ""
  "@
   xnor %r1,%2,%0
   fxnors %1,%2,%0"
  [(set_attr "type" "ialu,fp")])

;; These correspond to the above in the case where we also (or only)
;; want to set the condition code.  

(define_insn "*cmp_cc_arith_op"
  [(set (reg:CC 100)
	(compare:CC
	 (match_operator:SI 2 "cc_arithop"
			    [(match_operand:SI 0 "arith_operand" "%r")
			     (match_operand:SI 1 "arith_operand" "rI")])
	 (const_int 0)))]
  ""
  "%A2cc %0,%1,%%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccx_arith_op"
  [(set (reg:CCX 100)
	(compare:CCX
	 (match_operator:DI 2 "cc_arithop"
			    [(match_operand:DI 0 "arith_double_operand" "%r")
			     (match_operand:DI 1 "arith_double_operand" "rHI")])
	 (const_int 0)))]
  "TARGET_ARCH64"
  "%A2cc %0,%1,%%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_cc_arith_op_set"
  [(set (reg:CC 100)
	(compare:CC
	 (match_operator:SI 3 "cc_arithop"
			    [(match_operand:SI 1 "arith_operand" "%r")
			     (match_operand:SI 2 "arith_operand" "rI")])
	 (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(match_dup 3))]
  ""
  "%A3cc %1,%2,%0")

(define_insn "*cmp_ccx_arith_op_set"
  [(set (reg:CCX 100)
	(compare:CCX
	 (match_operator:DI 3 "cc_arithop"
			    [(match_operand:DI 1 "arith_double_operand" "%r")
			     (match_operand:DI 2 "arith_double_operand" "rHI")])
	 (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(match_dup 3))]
  "TARGET_ARCH64"
  "%A3cc %1,%2,%0")

(define_insn "*cmp_cc_xor_not"
  [(set (reg:CC 100)
	(compare:CC
	 (not:SI (xor:SI (match_operand:SI 0 "reg_or_0_operand" "%rJ")
			 (match_operand:SI 1 "arith_operand" "rI")))
	 (const_int 0)))]
  ""
  "xnorcc %r0,%1,%%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccx_xor_not"
  [(set (reg:CCX 100)
	(compare:CCX
	 (not:DI (xor:DI (match_operand:DI 0 "reg_or_0_operand" "%rJ")
			 (match_operand:DI 1 "arith_double_operand" "rHI")))
	 (const_int 0)))]
  "TARGET_ARCH64"
  "xnorcc %r0,%1,%%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_cc_xor_not_set"
  [(set (reg:CC 100)
	(compare:CC
	 (not:SI (xor:SI (match_operand:SI 1 "reg_or_0_operand" "%rJ")
			 (match_operand:SI 2 "arith_operand" "rI")))
	 (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(not:SI (xor:SI (match_dup 1) (match_dup 2))))]
  ""
  "xnorcc %r1,%2,%0")

(define_insn "*cmp_ccx_xor_not_set"
  [(set (reg:CCX 100)
	(compare:CCX
	 (not:DI (xor:DI (match_operand:DI 1 "reg_or_0_operand" "%rJ")
			 (match_operand:DI 2 "arith_double_operand" "rHI")))
	 (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(not:DI (xor:DI (match_dup 1) (match_dup 2))))]
  "TARGET_ARCH64"
  "xnorcc %r1,%2,%0")

(define_insn "*cmp_cc_arith_op_not"
  [(set (reg:CC 100)
	(compare:CC
	 (match_operator:SI 2 "cc_arithopn"
			    [(not:SI (match_operand:SI 0 "arith_operand" "rI"))
			     (match_operand:SI 1 "reg_or_0_operand" "rJ")])
	 (const_int 0)))]
  ""
  "%B2cc %r1,%0,%%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccx_arith_op_not"
  [(set (reg:CCX 100)
	(compare:CCX
	 (match_operator:DI 2 "cc_arithopn"
			    [(not:DI (match_operand:DI 0 "arith_double_operand" "rHI"))
			     (match_operand:DI 1 "reg_or_0_operand" "rJ")])
	 (const_int 0)))]
  "TARGET_ARCH64"
  "%B2cc %r1,%0,%%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_cc_arith_op_not_set"
  [(set (reg:CC 100)
	(compare:CC
	 (match_operator:SI 3 "cc_arithopn"
			    [(not:SI (match_operand:SI 1 "arith_operand" "rI"))
			     (match_operand:SI 2 "reg_or_0_operand" "rJ")])
	 (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(match_dup 3))]
  ""
  "%B3cc %r2,%1,%0")

(define_insn "*cmp_ccx_arith_op_not_set"
  [(set (reg:CCX 100)
	(compare:CCX
	 (match_operator:DI 3 "cc_arithopn"
			    [(not:DI (match_operand:DI 1 "arith_double_operand" "rHI"))
			     (match_operand:DI 2 "reg_or_0_operand" "rJ")])
	 (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(match_dup 3))]
  "TARGET_ARCH64"
  "%B3cc %r2,%1,%0")

;; We cannot use the "neg" pseudo insn because the Sun assembler
;; does not know how to make it work for constants.

(define_expand "negdi2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(neg:DI (match_operand:DI 1 "register_operand" "r")))]
  ""
  "
{
  if (! TARGET_ARCH64)
    {
      emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2,
			  gen_rtx_SET (VOIDmode, operand0,
				   gen_rtx_NEG (DImode, operand1)),
			  gen_rtx_CLOBBER (VOIDmode,
				   gen_rtx_REG (CCmode, SPARC_ICC_REG)))));
      DONE;
    }
}")

(define_insn "*negdi2_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(neg:DI (match_operand:DI 1 "register_operand" "r")))
   (clobber (reg:CC 100))]
  "! TARGET_ARCH64"
  "*
{
  if (TARGET_LIVE_G0)
    output_asm_insn (\"and %%g0,0,%%g0\", operands);
  return \"subcc %%g0,%L1,%L0\;subx %%g0,%H1,%H0\";
}"
  [(set_attr "type" "unary")
   ;; ??? This is wrong for TARGET_LIVE_G0 but it's not critical.
   (set_attr "length" "2")])

(define_insn "*negdi2_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(neg:DI (match_operand:DI 1 "register_operand" "r")))]
  "TARGET_ARCH64"
  "sub %%g0,%1,%0"
  [(set_attr "type" "unary")
   (set_attr "length" "1")])

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (match_operand:SI 1 "arith_operand" "rI")))]
  ""
  "*
{
  if (TARGET_LIVE_G0)
    return \"and %%g0,0,%%g0\;sub %%g0,%1,%0\";
  return \"sub %%g0,%1,%0\";
}"
  [(set_attr "type" "unary")
   (set (attr "length")
	(if_then_else (eq_attr "live_g0" "yes") (const_int 2) (const_int 1)))])

(define_insn "*cmp_cc_neg"
  [(set (reg:CC_NOOV 100)
	(compare:CC_NOOV (neg:SI (match_operand:SI 0 "arith_operand" "rI"))
			 (const_int 0)))]
  "! TARGET_LIVE_G0"
  "subcc %%g0,%0,%%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccx_neg"
  [(set (reg:CCX_NOOV 100)
	(compare:CCX_NOOV (neg:DI (match_operand:DI 0 "arith_double_operand" "rHI"))
			  (const_int 0)))]
  "TARGET_ARCH64"
  "subcc %%g0,%0,%%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_cc_set_neg"
  [(set (reg:CC_NOOV 100)
	(compare:CC_NOOV (neg:SI (match_operand:SI 1 "arith_operand" "rI"))
			 (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (match_dup 1)))]
  "! TARGET_LIVE_G0"
  "subcc %%g0,%1,%0"
  [(set_attr "type" "unary")])

(define_insn "*cmp_ccx_set_neg"
  [(set (reg:CCX_NOOV 100)
	(compare:CCX_NOOV (neg:DI (match_operand:DI 1 "arith_double_operand" "rHI"))
			  (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(neg:DI (match_dup 1)))]
  "TARGET_ARCH64"
  "subcc %%g0,%1,%0"
  [(set_attr "type" "unary")])

;; We cannot use the "not" pseudo insn because the Sun assembler
;; does not know how to make it work for constants.
(define_expand "one_cmpldi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(not:DI (match_operand:DI 1 "register_operand" "")))]
  ""
  "")

(define_insn "*one_cmpldi2_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r,b")
	(not:DI (match_operand:DI 1 "register_operand" "r,b")))]
  "! TARGET_ARCH64"
  "@
   xnor %1,0,%0\;xnor %R1,0,%R0
   fnot1 %1,%0"
  [(set_attr "type" "unary,fp")
   (set_attr "length" "2,1")])

(define_insn "*one_cmpldi2_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(not:DI (match_operand:DI 1 "arith_double_operand" "rHI")))]
  "TARGET_ARCH64"
  "xnor %1,0,%0"
  [(set_attr "type" "unary")])

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r,d")
	(not:SI (match_operand:SI 1 "arith_operand" "r,I,d")))]
  ""
  "*
{
  if (which_alternative == 0)
    return \"xnor %1,0,%0\";
  if (which_alternative == 2)
    return \"fnot1s %1,%0\";
  if (TARGET_LIVE_G0)
    output_asm_insn (\"and %%g0,0,%%g0\", operands);
  return \"xnor %%g0,%1,%0\";
}"
  [(set_attr "type" "unary,unary,fp")
   (set_attr_alternative "length"
     [(const_int 1)
      (if_then_else (eq_attr "live_g0" "yes") (const_int 2) (const_int 1))
      (const_int 1)])])

(define_insn "*cmp_cc_not"
  [(set (reg:CC 100)
	(compare:CC (not:SI (match_operand:SI 0 "arith_operand" "rI"))
		    (const_int 0)))]
  "! TARGET_LIVE_G0"
  "xnorcc %%g0,%0,%%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_ccx_not"
  [(set (reg:CCX 100)
	(compare:CCX (not:DI (match_operand:DI 0 "arith_double_operand" "rHI"))
		     (const_int 0)))]
  "TARGET_ARCH64"
  "xnorcc %%g0,%0,%%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_cc_set_not"
  [(set (reg:CC 100)
	(compare:CC (not:SI (match_operand:SI 1 "arith_operand" "rI"))
		    (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(not:SI (match_dup 1)))]
  "! TARGET_LIVE_G0"
  "xnorcc %%g0,%1,%0"
  [(set_attr "type" "unary")])

(define_insn "*cmp_ccx_set_not"
  [(set (reg:CCX 100)
	(compare:CCX (not:DI (match_operand:DI 1 "arith_double_operand" "rHI"))
		    (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(not:DI (match_dup 1)))]
  "TARGET_ARCH64"
  "xnorcc %%g0,%1,%0"
  [(set_attr "type" "unary")])

;; Floating point arithmetic instructions.

(define_insn "addtf3"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(plus:TF (match_operand:TF 1 "register_operand" "e")
		 (match_operand:TF 2 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "faddq %1,%2,%0"
  [(set_attr "type" "fp")])

(define_insn "adddf3"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(plus:DF (match_operand:DF 1 "register_operand" "e")
		 (match_operand:DF 2 "register_operand" "e")))]
  "TARGET_FPU"
  "faddd %1,%2,%0"
  [(set_attr "type" "fp")])

(define_insn "addsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(plus:SF (match_operand:SF 1 "register_operand" "f")
		 (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_FPU"
  "fadds %1,%2,%0"
  [(set_attr "type" "fp")])

(define_insn "subtf3"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(minus:TF (match_operand:TF 1 "register_operand" "e")
		  (match_operand:TF 2 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fsubq %1,%2,%0"
  [(set_attr "type" "fp")])

(define_insn "subdf3"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(minus:DF (match_operand:DF 1 "register_operand" "e")
		  (match_operand:DF 2 "register_operand" "e")))]
  "TARGET_FPU"
  "fsubd %1,%2,%0"
  [(set_attr "type" "fp")])

(define_insn "subsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(minus:SF (match_operand:SF 1 "register_operand" "f")
		  (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_FPU"
  "fsubs %1,%2,%0"
  [(set_attr "type" "fp")])

(define_insn "multf3"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(mult:TF (match_operand:TF 1 "register_operand" "e")
		 (match_operand:TF 2 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fmulq %1,%2,%0"
  [(set_attr "type" "fpmul")])

(define_insn "muldf3"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(mult:DF (match_operand:DF 1 "register_operand" "e")
		 (match_operand:DF 2 "register_operand" "e")))]
  "TARGET_FPU"
  "fmuld %1,%2,%0"
  [(set_attr "type" "fpmul")])

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(mult:SF (match_operand:SF 1 "register_operand" "f")
		 (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_FPU"
  "fmuls %1,%2,%0"
  [(set_attr "type" "fpmul")])

(define_insn "*muldf3_extend"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(mult:DF (float_extend:DF (match_operand:SF 1 "register_operand" "f"))
		 (float_extend:DF (match_operand:SF 2 "register_operand" "f"))))]
  "(TARGET_V8 || TARGET_V9) && TARGET_FPU"
  "fsmuld %1,%2,%0"
  [(set_attr "type" "fpmul")])

(define_insn "*multf3_extend"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(mult:TF (float_extend:TF (match_operand:DF 1 "register_operand" "e"))
		 (float_extend:TF (match_operand:DF 2 "register_operand" "e"))))]
  "(TARGET_V8 || TARGET_V9) && TARGET_FPU && TARGET_HARD_QUAD"
  "fdmulq %1,%2,%0"
  [(set_attr "type" "fpmul")])

;; don't have timing for quad-prec. divide.
(define_insn "divtf3"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(div:TF (match_operand:TF 1 "register_operand" "e")
		(match_operand:TF 2 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fdivq %1,%2,%0"
  [(set_attr "type" "fpdivd")])

(define_insn "divdf3"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(div:DF (match_operand:DF 1 "register_operand" "e")
		(match_operand:DF 2 "register_operand" "e")))]
  "TARGET_FPU"
  "fdivd %1,%2,%0"
  [(set_attr "type" "fpdivd")])

(define_insn "divsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(div:SF (match_operand:SF 1 "register_operand" "f")
		(match_operand:SF 2 "register_operand" "f")))]
  "TARGET_FPU"
  "fdivs %1,%2,%0"
  [(set_attr "type" "fpdivs")])

(define_insn "negtf2"
  [(set (match_operand:TF 0 "register_operand" "=e,e")
	(neg:TF (match_operand:TF 1 "register_operand" "0,e")))]
  ; We don't use quad float insns here so we don't need TARGET_HARD_QUAD.
  "TARGET_FPU"
  "*
{
  /* v9: can't use fnegs, won't work with upper regs.  */
  if (which_alternative == 0)
   return TARGET_V9 ? \"fnegd %0,%0\" : \"fnegs %0,%0\";
  else
   return TARGET_V9 ? \"fnegd %1,%0\;fmovd %S1,%S0\"
     : \"fnegs %1,%0\;fmovs %R1,%R0\;fmovs %S1,%S0\;fmovs %T1,%T0\";
}"
  [(set_attr "type" "fpmove")
   (set_attr_alternative "length"
     [(const_int 1)
      (if_then_else (eq_attr "isa" "v9") (const_int 2) (const_int 4))])])

(define_insn "negdf2"
  [(set (match_operand:DF 0 "register_operand" "=e,e")
	(neg:DF (match_operand:DF 1 "register_operand" "0,e")))]
  "TARGET_FPU"
  "*
{
  if (TARGET_V9)
    return \"fnegd %1,%0\";
  else if (which_alternative == 0)
   return \"fnegs %0,%0\";
  else
   return \"fnegs %1,%0\;fmovs %R1,%R0\";
}"
  [(set_attr "type" "fpmove")
   (set_attr_alternative "length"
     [(const_int 1)
      (if_then_else (eq_attr "isa" "v9") (const_int 1) (const_int 2))])])

(define_insn "negsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(neg:SF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_FPU"
  "fnegs %1,%0"
  [(set_attr "type" "fpmove")])

(define_insn "abstf2"
  [(set (match_operand:TF 0 "register_operand" "=e,e")
	(abs:TF (match_operand:TF 1 "register_operand" "0,e")))]
  ; We don't use quad float insns here so we don't need TARGET_HARD_QUAD.
  "TARGET_FPU"
  "*
{
  /* v9: can't use fabss, won't work with upper regs.  */
  if (which_alternative == 0)
    return TARGET_V9 ? \"fabsd %0,%0\" : \"fabss %0,%0\";
  else
    return TARGET_V9 ? \"fabsd %1,%0\;fmovd %S1,%S0\"
      : \"fabss %1,%0\;fmovs %R1,%R0\;fmovs %S1,%S0\;fmovs %T1,%T0\";
}"
  [(set_attr "type" "fpmove")
   (set_attr_alternative "length"
     [(const_int 1)
      (if_then_else (eq_attr "isa" "v9") (const_int 2) (const_int 4))])])

(define_insn "absdf2"
  [(set (match_operand:DF 0 "register_operand" "=e,e")
	(abs:DF (match_operand:DF 1 "register_operand" "0,e")))]
  "TARGET_FPU"
  "*
{
  if (TARGET_V9)
    return \"fabsd %1,%0\";
  else if (which_alternative == 0)
    return \"fabss %0,%0\";
  else
    return \"fabss %1,%0\;fmovs %R1,%R0\";
}"
  [(set_attr "type" "fpmove")
   (set_attr_alternative "length"
     [(const_int 1)
      (if_then_else (eq_attr "isa" "v9") (const_int 1) (const_int 2))])])

(define_insn "abssf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(abs:SF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_FPU"
  "fabss %1,%0"
  [(set_attr "type" "fpmove")])

(define_insn "sqrttf2"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(sqrt:TF (match_operand:TF 1 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fsqrtq %1,%0"
  [(set_attr "type" "fpsqrt")])

(define_insn "sqrtdf2"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(sqrt:DF (match_operand:DF 1 "register_operand" "e")))]
  "TARGET_FPU"
  "fsqrtd %1,%0"
  [(set_attr "type" "fpsqrt")])

(define_insn "sqrtsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(sqrt:SF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_FPU"
  "fsqrts %1,%0"
  [(set_attr "type" "fpsqrt")])

;;- arithmetic shift instructions

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashift:SI (match_operand:SI 1 "register_operand" "r")
		   (match_operand:SI 2 "arith_operand" "rI")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT
      && (unsigned HOST_WIDE_INT) INTVAL (operands[2]) > 31)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return \"sll %1,%2,%0\";
}"
  [(set_attr "type" "shift")])

(define_expand "ashldi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashift:DI (match_operand:DI 1 "register_operand" "r")
		   (match_operand:SI 2 "arith_operand" "rI")))]
  "TARGET_ARCH64 || TARGET_V8PLUS"
  "
{
  if (! TARGET_ARCH64)
    {
      if (GET_CODE (operands[2]) == CONST_INT)
	FAIL;
      emit_insn (gen_ashldi3_v8plus (operands[0], operands[1], operands[2]));
      DONE;
    }
}")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashift:DI (match_operand:DI 1 "register_operand" "r")
		   (match_operand:SI 2 "arith_operand" "rI")))]
  "TARGET_ARCH64"
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT
      && (unsigned HOST_WIDE_INT) INTVAL (operands[2]) > 31)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return \"sllx %1,%2,%0\";
}")

(define_insn "ashldi3_v8plus"
  [(set (match_operand:DI 0 "register_operand" "=&h,&h,r")
	(ashift:DI (match_operand:DI 1 "arith_operand" "rI,0,rI")
		   (match_operand:SI 2 "arith_operand" "rI,rI,rI")))
   (clobber (match_scratch:SI 3 "=X,X,&h"))]
  "TARGET_V8PLUS"
  "*return sparc_v8plus_shift (operands, insn, \"sllx\");"
  [(set_attr "length" "5,5,6")])

;; Optimize (1LL<<x)-1
(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=h")
	(plus:DI (ashift:DI (const_int 1)
			    (match_operand:SI 2 "arith_operand" "rI"))
		 (const_int -1)))]
  "TARGET_V8PLUS"
  "*
{
  if (GET_CODE (operands[2]) == REG && REGNO (operands[2]) == REGNO (operands[0]))
    return \"mov 1,%L0\;sllx %L0,%2,%L0\;sub %L0,1,%L0\;srlx %L0,32,%H0\";
  return \"mov 1,%H0\;sllx %H0,%2,%L0\;sub %L0,1,%L0\;srlx %L0,32,%H0\";
}"
  [(set_attr "length" "4")])

(define_insn "*cmp_cc_ashift_1"
  [(set (reg:CC_NOOV 100)
	(compare:CC_NOOV (ashift:SI (match_operand:SI 0 "register_operand" "r")
				    (const_int 1))
			 (const_int 0)))]
  ""
  "addcc %0,%0,%%g0"
  [(set_attr "type" "compare")])

(define_insn "*cmp_cc_set_ashift_1"
  [(set (reg:CC_NOOV 100)
	(compare:CC_NOOV (ashift:SI (match_operand:SI 1 "register_operand" "r")
				    (const_int 1))
			 (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(ashift:SI (match_dup 1) (const_int 1)))]
  ""
  "addcc %1,%1,%0")

(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "r")
		     (match_operand:SI 2 "arith_operand" "rI")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT
      && (unsigned HOST_WIDE_INT) INTVAL (operands[2]) > 31)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return \"sra %1,%2,%0\";
}"
  [(set_attr "type" "shift")])

(define_expand "ashrdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "r")
		     (match_operand:SI 2 "arith_operand" "rI")))]
  "TARGET_ARCH64 || TARGET_V8PLUS"
  "
if (! TARGET_ARCH64)
  {
    if (GET_CODE (operands[2]) == CONST_INT)
      FAIL;	/* prefer generic code in this case */
    emit_insn (gen_ashrdi3_v8plus (operands[0], operands[1], operands[2]));
    DONE;
  }")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "r")
		     (match_operand:SI 2 "arith_operand" "rI")))]
  "TARGET_ARCH64"
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT
      && (unsigned HOST_WIDE_INT) INTVAL (operands[2]) > 63)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return \"srax %1,%2,%0\";
}")

(define_insn "ashrdi3_v8plus"
  [(set (match_operand:DI 0 "register_operand" "=&h,&h,r")
	(ashiftrt:DI (match_operand:DI 1 "arith_operand" "rI,0,rI")
		     (match_operand:SI 2 "arith_operand" "rI,rI,rI")))
   (clobber (match_scratch:SI 3 "=X,X,&h"))]
  "TARGET_V8PLUS"
  "*return sparc_v8plus_shift (operands, insn, \"srax\");"
  [(set_attr "length" "5,5,6")])

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
		     (match_operand:SI 2 "arith_operand" "rI")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT
      && (unsigned HOST_WIDE_INT) INTVAL (operands[2]) > 31)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x1f);

  return \"srl %1,%2,%0\";
}"
  [(set_attr "type" "shift")])

(define_expand "lshrdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "r")
		     (match_operand:SI 2 "arith_operand" "rI")))]
  "TARGET_ARCH64 || TARGET_V8PLUS"
  "
if (! TARGET_ARCH64)
  {
    if (GET_CODE (operands[2]) == CONST_INT)
      FAIL;
    emit_insn (gen_lshrdi3_v8plus (operands[0], operands[1], operands[2]));
    DONE;
  }")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "r")
		     (match_operand:SI 2 "arith_operand" "rI")))]
  "TARGET_ARCH64"
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT
      && (unsigned HOST_WIDE_INT) INTVAL (operands[2]) > 63)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return \"srlx %1,%2,%0\";
}")

(define_insn "lshrdi3_v8plus"
  [(set (match_operand:DI 0 "register_operand" "=&h,&h,r")
	(lshiftrt:DI (match_operand:DI 1 "arith_operand" "rI,0,rI")
		     (match_operand:SI 2 "arith_operand" "rI,rI,rI")))
   (clobber (match_scratch:SI 3 "=X,X,&h"))]
  "TARGET_V8PLUS"
  "*return sparc_v8plus_shift (operands, insn, \"srlx\");"
  [(set_attr "length" "5,5,6")])

;; Unconditional and other jump instructions
;; On the Sparc, by setting the annul bit on an unconditional branch, the
;; following insn is never executed.  This saves us a nop.  Dbx does not
;; handle such branches though, so we only use them when optimizing.
(define_insn "jump"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  ""
  "*
{
  /* Some implementations (e.g. TurboSparc) are reported to have problems
     with
	foo: b,a foo
     i.e. an empty loop with the annul bit set.  The workaround is to use 
        foo: b foo; nop
     instead.  */

  if (flag_delayed_branch
      && (insn_addresses[INSN_UID (operands[0])]
	  == insn_addresses[INSN_UID (insn)]))
    return \"b %l0%#\";
  else
    return \"b%* %l0%(\";
}"
  [(set_attr "type" "uncond_branch")])

(define_expand "tablejump"
  [(parallel [(set (pc) (match_operand 0 "register_operand" "r"))
	      (use (label_ref (match_operand 1 "" "")))])]
  ""
  "
{
  if (GET_MODE (operands[0]) != Pmode)
    abort ();

  /* In pic mode, our address differences are against the base of the
     table.  Add that base value back in; CSE ought to be able to combine
     the two address loads.  */
  if (flag_pic)
    {
      rtx tmp, tmp2;
      tmp = gen_rtx_LABEL_REF (Pmode, operands[1]);
      tmp2 = operands[0];
      tmp = gen_rtx_PLUS (Pmode, tmp2, tmp);
      operands[0] = memory_address (Pmode, tmp);
    }
}")

(define_insn "*tablejump_sp32"
  [(set (pc) (match_operand:SI 0 "address_operand" "p"))
   (use (label_ref (match_operand 1 "" "")))]
  "! TARGET_PTR64"
  "jmp %a0%#"
  [(set_attr "type" "uncond_branch")])

(define_insn "*tablejump_sp64"
  [(set (pc) (match_operand:DI 0 "address_operand" "p"))
   (use (label_ref (match_operand 1 "" "")))]
  "TARGET_PTR64"
  "jmp %a0%#"
  [(set_attr "type" "uncond_branch")])

;; This pattern recognizes the "instruction" that appears in 
;; a function call that wants a structure value, 
;; to inform the called function if compiled with Sun CC.
;(define_insn "*unimp_insn"
;  [(match_operand:SI 0 "immediate_operand" "")]
;  "GET_CODE (operands[0]) == CONST_INT && INTVAL (operands[0]) > 0"
;  "unimp %0"
;  [(set_attr "type" "marker")])

;;- jump to subroutine
(define_expand "call"
  ;; Note that this expression is not used for generating RTL.
  ;; All the RTL is generated explicitly below.
  [(call (match_operand 0 "call_operand" "")
	 (match_operand 3 "" "i"))]
  ;; operands[2] is next_arg_register
  ;; operands[3] is struct_value_size_rtx.
  ""
  "
{
  rtx fn_rtx, nregs_rtx;

   if (GET_MODE (operands[0]) != FUNCTION_MODE)
    abort ();

  if (GET_CODE (XEXP (operands[0], 0)) == LABEL_REF)
    {
      /* This is really a PIC sequence.  We want to represent
	 it as a funny jump so its delay slots can be filled. 

	 ??? But if this really *is* a CALL, will not it clobber the
	 call-clobbered registers?  We lose this if it is a JUMP_INSN.
	 Why cannot we have delay slots filled if it were a CALL?  */

      if (! TARGET_ARCH64 && INTVAL (operands[3]) != 0)
	emit_jump_insn
	  (gen_rtx_PARALLEL (VOIDmode,
		    gen_rtvec (3,
			       gen_rtx_SET (VOIDmode, pc_rtx,
					XEXP (operands[0], 0)),
			       GEN_INT (INTVAL (operands[3]) & 0xfff),
			       gen_rtx_CLOBBER (VOIDmode,
					gen_rtx_REG (Pmode, 15)))));
      else
	emit_jump_insn
	  (gen_rtx_PARALLEL (VOIDmode,
		    gen_rtvec (2,
			       gen_rtx_SET (VOIDmode, pc_rtx,
					XEXP (operands[0], 0)),
			       gen_rtx_CLOBBER (VOIDmode,
					gen_rtx_REG (Pmode, 15)))));
      goto finish_call;
    }

  fn_rtx = operands[0];

  /* Count the number of parameter registers being used by this call.
     if that argument is NULL, it means we are using them all, which
     means 6 on the sparc.  */
#if 0
  if (operands[2])
    nregs_rtx = GEN_INT (REGNO (operands[2]) - 8);
  else
    nregs_rtx = GEN_INT (6);
#else
  nregs_rtx = const0_rtx;
#endif

  if (! TARGET_ARCH64 && INTVAL (operands[3]) != 0)
    emit_call_insn
      (gen_rtx_PARALLEL (VOIDmode,
		gen_rtvec (3, gen_rtx_CALL (VOIDmode, fn_rtx, nregs_rtx),
			   GEN_INT (INTVAL (operands[3]) & 0xfff),
			   gen_rtx_CLOBBER (VOIDmode,
				    gen_rtx_REG (Pmode, 15)))));
  else
    emit_call_insn
      (gen_rtx_PARALLEL (VOIDmode,
		gen_rtvec (2, gen_rtx_CALL (VOIDmode, fn_rtx, nregs_rtx),
			   gen_rtx_CLOBBER (VOIDmode,
				    gen_rtx_REG (Pmode, 15)))));

 finish_call:
#if 0
  /* If this call wants a structure value,
     emit an unimp insn to let the called function know about this.  */
  if (! TARGET_ARCH64 && INTVAL (operands[3]) > 0)
    {
      rtx insn = emit_insn (operands[3]);
      SCHED_GROUP_P (insn) = 1;
    }
#endif

  DONE;
}")

;; We can't use the same pattern for these two insns, because then registers
;; in the address may not be properly reloaded.

(define_insn "*call_address_sp32"
  [(call (mem:SI (match_operand:SI 0 "address_operand" "p"))
	 (match_operand 1 "" ""))
   (clobber (reg:SI 15))]
  ;;- Do not use operand 1 for most machines.
  "! TARGET_PTR64"
  "call %a0,%1%#"
  [(set_attr "type" "call")])

(define_insn "*call_symbolic_sp32"
  [(call (mem:SI (match_operand:SI 0 "symbolic_operand" "s"))
	 (match_operand 1 "" ""))
   (clobber (reg:SI 15))]
  ;;- Do not use operand 1 for most machines.
  "! TARGET_PTR64"
  "call %a0,%1%#"
  [(set_attr "type" "call")])

(define_insn "*call_address_sp64"
  [(call (mem:SI (match_operand:DI 0 "address_operand" "p"))
	 (match_operand 1 "" ""))
   (clobber (reg:DI 15))]
  ;;- Do not use operand 1 for most machines.
  "TARGET_PTR64"
  "call %a0,%1%#"
  [(set_attr "type" "call")])

(define_insn "*call_symbolic_sp64"
  [(call (mem:SI (match_operand:DI 0 "symbolic_operand" "s"))
	 (match_operand 1 "" ""))
   (clobber (reg:DI 15))]
  ;;- Do not use operand 1 for most machines.
  "TARGET_PTR64"
  "call %a0,%1%#"
  [(set_attr "type" "call")])

;; This is a call that wants a structure value.
;; There is no such critter for v9 (??? we may need one anyway).
(define_insn "*call_address_struct_value_sp32"
  [(call (mem:SI (match_operand:SI 0 "address_operand" "p"))
	 (match_operand 1 "" ""))
   (match_operand 2 "immediate_operand" "")
   (clobber (reg:SI 15))]
  ;;- Do not use operand 1 for most machines.
  "! TARGET_ARCH64 && GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) >= 0"
  "call %a0,%1\;nop\;unimp %2"
  [(set_attr "type" "call_no_delay_slot")])

;; This is a call that wants a structure value.
;; There is no such critter for v9 (??? we may need one anyway).
(define_insn "*call_symbolic_struct_value_sp32"
  [(call (mem:SI (match_operand:SI 0 "symbolic_operand" "s"))
	 (match_operand 1 "" ""))
   (match_operand 2 "immediate_operand" "")
   (clobber (reg:SI 15))]
  ;;- Do not use operand 1 for most machines.
  "! TARGET_ARCH64 && GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) >= 0"
  "call %a0,%1\;nop\;unimp %2"
  [(set_attr "type" "call_no_delay_slot")])

;; This is a call that may want a structure value.  This is used for
;; untyped_calls.
(define_insn "*call_address_untyped_struct_value_sp32"
  [(call (mem:SI (match_operand:SI 0 "address_operand" "p"))
	 (match_operand 1 "" ""))
   (match_operand 2 "immediate_operand" "")
   (clobber (reg:SI 15))]
  ;;- Do not use operand 1 for most machines.
  "! TARGET_ARCH64 && GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) < 0"
  "call %a0,%1\;nop\;nop"
  [(set_attr "type" "call_no_delay_slot")])

;; This is a call that wants a structure value.
(define_insn "*call_symbolic_untyped_struct_value_sp32"
  [(call (mem:SI (match_operand:SI 0 "symbolic_operand" "s"))
	 (match_operand 1 "" ""))
   (match_operand 2 "immediate_operand" "")
   (clobber (reg:SI 15))]
  ;;- Do not use operand 1 for most machines.
  "! TARGET_ARCH64 && GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) < 0"
  "call %a0,%1\;nop\;nop"
  [(set_attr "type" "call_no_delay_slot")])

(define_expand "call_value"
  ;; Note that this expression is not used for generating RTL.
  ;; All the RTL is generated explicitly below.
  [(set (match_operand 0 "register_operand" "=rf")
	(call (match_operand:SI 1 "" "")
	      (match_operand 4 "" "")))]
  ;; operand 2 is stack_size_rtx
  ;; operand 3 is next_arg_register
  ""
  "
{
  rtx fn_rtx, nregs_rtx;
  rtvec vec;

  if (GET_MODE (operands[1]) != FUNCTION_MODE)
    abort ();

  fn_rtx = operands[1];

#if 0
  if (operands[3])
    nregs_rtx = GEN_INT (REGNO (operands[3]) - 8);
  else
    nregs_rtx = GEN_INT (6);
#else
  nregs_rtx = const0_rtx;
#endif

  vec = gen_rtvec (2,
		   gen_rtx_SET (VOIDmode, operands[0],
			    gen_rtx_CALL (VOIDmode, fn_rtx, nregs_rtx)),
		   gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (Pmode, 15)));

  emit_call_insn (gen_rtx_PARALLEL (VOIDmode, vec));

  DONE;
}")

(define_insn "*call_value_address_sp32"
  [(set (match_operand 0 "" "=rf")
	(call (mem:SI (match_operand:SI 1 "address_operand" "p"))
	      (match_operand 2 "" "")))
   (clobber (reg:SI 15))]
  ;;- Do not use operand 2 for most machines.
  "! TARGET_PTR64"
  "call %a1,%2%#"
  [(set_attr "type" "call")])

(define_insn "*call_value_symbolic_sp32"
  [(set (match_operand 0 "" "=rf")
	(call (mem:SI (match_operand:SI 1 "symbolic_operand" "s"))
	      (match_operand 2 "" "")))
   (clobber (reg:SI 15))]
  ;;- Do not use operand 2 for most machines.
  "! TARGET_PTR64"
  "call %a1,%2%#"
  [(set_attr "type" "call")])

(define_insn "*call_value_address_sp64"
  [(set (match_operand 0 "" "")
	(call (mem:SI (match_operand:DI 1 "address_operand" "p"))
	      (match_operand 2 "" "")))
   (clobber (reg:DI 15))]
  ;;- Do not use operand 2 for most machines.
  "TARGET_PTR64"
  "call %a1,%2%#"
  [(set_attr "type" "call")])

(define_insn "*call_value_symbolic_sp64"
  [(set (match_operand 0 "" "")
	(call (mem:SI (match_operand:DI 1 "symbolic_operand" "s"))
	      (match_operand 2 "" "")))
   (clobber (reg:DI 15))]
  ;;- Do not use operand 2 for most machines.
  "TARGET_PTR64"
  "call %a1,%2%#"
  [(set_attr "type" "call")])

(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "" "")
		    (const_int 0))
	      (match_operand 1 "" "")
	      (match_operand 2 "" "")])]
  ""
  "
{
  int i;

  /* Pass constm1 to indicate that it may expect a structure value, but
     we don't know what size it is.  */
  emit_call_insn (gen_call (operands[0], const0_rtx, NULL, constm1_rtx));

  for (i = 0; i < XVECLEN (operands[2], 0); i++)
    {
      rtx set = XVECEXP (operands[2], 0, i);
      emit_move_insn (SET_DEST (set), SET_SRC (set));
    }

  /* The optimizer does not know that the call sets the function value
     registers we stored in the result block.  We avoid problems by
     claiming that all hard registers are used and clobbered at this
     point.  */
  emit_insn (gen_blockage ());

  DONE;
}")

;; UNSPEC_VOLATILE is considered to use and clobber all hard registers and
;; all of memory.  This blocks insns from being moved across this point.

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] 0)]
  ""
  "")

;; Prepare to return any type including a structure value.

(define_expand "untyped_return"
  [(match_operand:BLK 0 "memory_operand" "")
   (match_operand 1 "" "")]
  ""
  "
{
  rtx valreg1 = gen_rtx_REG (DImode, 24);
  rtx valreg2 = gen_rtx_REG (TARGET_ARCH64 ? TFmode : DFmode, 32);
  rtx result = operands[0];

  if (! TARGET_ARCH64)
    {
      rtx rtnreg = gen_rtx_REG (SImode, (leaf_function ? 15 : 31));
      rtx value = gen_reg_rtx (SImode);

      /* Fetch the instruction where we will return to and see if it's an unimp
	 instruction (the most significant 10 bits will be zero).  If so,
	 update the return address to skip the unimp instruction.  */
      emit_move_insn (value,
		      gen_rtx_MEM (SImode, plus_constant (rtnreg, 8)));
      emit_insn (gen_lshrsi3 (value, value, GEN_INT (22)));
      emit_insn (gen_update_return (rtnreg, value));
    }

  /* Reload the function value registers.  */
  emit_move_insn (valreg1, change_address (result, DImode, XEXP (result, 0)));
  emit_move_insn (valreg2,
		  change_address (result, TARGET_ARCH64 ? TFmode : DFmode,
				  plus_constant (XEXP (result, 0), 8)));

  /* Put USE insns before the return.  */
  emit_insn (gen_rtx_USE (VOIDmode, valreg1));
  emit_insn (gen_rtx_USE (VOIDmode, valreg2));

  /* Construct the return.  */
  expand_null_return ();

  DONE;
}")

;; This is a bit of a hack.  We're incrementing a fixed register (%i7),
;; and parts of the compiler don't want to believe that the add is needed.

(define_insn "update_return"
  [(unspec:SI [(match_operand:SI 0 "register_operand" "r")
	       (match_operand:SI 1 "register_operand" "r")] 1)]
  "! TARGET_ARCH64"
  "cmp %1,0\;be,a .+8\;add %0,4,%0"
  [(set_attr "type" "multi")])

(define_insn "return"
  [(return)
   (use (reg:SI 31))]
  "! TARGET_EPILOGUE"
  "* return output_return (operands);"
  [(set_attr "type" "return")])

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operand:SI 1 "arith_operand" "rI"))
   (parallel [(return)
	      (use (reg:SI 31))])]
  "sparc_return_peephole_ok (operands[0], operands[1])"
  "return %%i7+8\;mov %Y1,%Y0")

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop")

(define_expand "indirect_jump"
  [(set (pc) (match_operand 0 "address_operand" "p"))]
  ""
  "")

(define_insn "*branch_sp32"
  [(set (pc) (match_operand:SI 0 "address_operand" "p"))]
  "! TARGET_PTR64"
 "jmp %a0%#"
 [(set_attr "type" "uncond_branch")])
 
(define_insn "*branch_sp64"
  [(set (pc) (match_operand:DI 0 "address_operand" "p"))]
  "TARGET_PTR64"
  "jmp %a0%#"
  [(set_attr "type" "uncond_branch")])

;; ??? Doesn't work with -mflat.
(define_expand "nonlocal_goto"
  [(match_operand:SI 0 "general_operand" "")
   (match_operand:SI 1 "general_operand" "")
   (match_operand:SI 2 "general_operand" "")
   (match_operand:SI 3 "" "")]
  ""
  "
{
  rtx chain = operands[0];
  rtx fp = operands[1];
  rtx stack = operands[2];
  rtx lab = operands[3];
  rtx labreg;

  /* Trap instruction to flush all the register windows.  */
  emit_insn (gen_flush_register_windows ());

  /* Load the fp value for the containing fn into %fp.  This is needed
     because STACK refers to %fp.  Note that virtual register instantiation
     fails if the virtual %fp isn't set from a register.  */
  if (GET_CODE (fp) != REG)
    fp = force_reg (Pmode, fp);
  emit_move_insn (virtual_stack_vars_rtx, fp);

  /* Find the containing function's current nonlocal goto handler,
     which will do any cleanups and then jump to the label.  */
  labreg = gen_rtx_REG (Pmode, 8);
  emit_move_insn (labreg, lab);

  /* Restore %fp from stack pointer value for containing function.
     The restore insn that follows will move this to %sp,
     and reload the appropriate value into %fp.  */
  emit_move_insn (frame_pointer_rtx, stack);

  /* USE of frame_pointer_rtx added for consistency; not clear if
     really needed.  */
  /*emit_insn (gen_rtx_USE (VOIDmode, frame_pointer_rtx));*/
  emit_insn (gen_rtx_USE (VOIDmode, stack_pointer_rtx));
  /* Return, restoring reg window and jumping to goto handler.  */
  if (TARGET_V9 && GET_CODE (chain) == CONST_INT
      && ! (INTVAL (chain) & ~(HOST_WIDE_INT)0xffffffff))
    {
      emit_insn (gen_goto_handler_and_restore_v9 (labreg, static_chain_rtx,
						  chain));
      emit_barrier ();
      DONE;
    }
  /* Put in the static chain register the nonlocal label address.  */
  emit_move_insn (static_chain_rtx, chain);
  emit_insn (gen_rtx_USE (VOIDmode, static_chain_rtx));
  emit_insn (gen_goto_handler_and_restore (labreg));
  emit_barrier ();
  DONE;
}")

;; Special trap insn to flush register windows.
(define_insn "flush_register_windows"
  [(unspec_volatile [(const_int 0)] 1)]
  ""
  "* return TARGET_V9 ? \"flushw\" : \"ta 3\";"
  [(set_attr "type" "misc")])

(define_insn "goto_handler_and_restore"
  [(unspec_volatile [(match_operand:SI 0 "register_operand" "=r")] 2)]
  ""
  "jmp %0+0\;restore"
  [(set_attr "type" "misc")
   (set_attr "length" "2")])

(define_insn "goto_handler_and_restore_v9"
  [(unspec_volatile [(match_operand:SI 0 "register_operand" "=r,r")
		     (match_operand:SI 1 "register_operand" "=r,r")
		     (match_operand:SI 2 "const_int_operand" "I,n")] 3)]
  "TARGET_V9 && ! TARGET_ARCH64"
  "@
   return %0+0\;mov %2,%Y1
   sethi %%hi(%2),%1\;return %0+0\;or %Y1,%%lo(%2),%Y1"
  [(set_attr "type" "misc")
   (set_attr "length" "2,3")])

(define_insn "*goto_handler_and_restore_v9_sp64"
  [(unspec_volatile [(match_operand:DI 0 "register_operand" "=r,r")
		     (match_operand:DI 1 "register_operand" "=r,r")
		     (match_operand:SI 2 "const_int_operand" "I,n")] 3)]
  "TARGET_V9 && TARGET_ARCH64"
  "@
   return %0+0\;mov %2,%Y1
   sethi %%hi(%2),%1\;return %0+0\;or %Y1,%%lo(%2),%Y1"
  [(set_attr "type" "misc")
   (set_attr "length" "2,3")])

;; Pattern for use after a setjmp to store FP and the return register
;; into the stack area.

(define_expand "setjmp"
  [(const_int 0)]
  ""
  "
{
  if (TARGET_ARCH64)
    emit_insn (gen_setjmp_64 ());
  else
    emit_insn (gen_setjmp_32 ());
  DONE;
}")

(define_expand "setjmp_32"
  [(set (mem:SI (plus:SI (reg:SI 14) (const_int 56))) (match_dup 0))
   (set (mem:SI (plus:SI (reg:SI 14) (const_int 60))) (reg:SI 31))]
  ""
  "
{ operands[0] = frame_pointer_rtx; }")

(define_expand "setjmp_64"
  [(set (mem:DI (plus:DI (reg:DI 14) (const_int 112))) (match_dup 0))
   (set (mem:DI (plus:DI (reg:DI 14) (const_int 120))) (reg:DI 31))]
  ""
  "
{ operands[0] = frame_pointer_rtx; }")

;; Special pattern for the FLUSH instruction.

(define_insn "flush"
  [(unspec_volatile [(match_operand 0 "memory_operand" "m")] 3)]
  ""
  "* return TARGET_V9 ? \"flush %f0\" : \"iflush %f0\";"
  [(set_attr "type" "misc")])

;; find first set.

;; The scan instruction searches from the most significant bit while ffs
;; searches from the least significant bit.  The bit index and treatment of
;; zero also differ.  It takes at least 7 instructions to get the proper
;; result.  Here is an obvious 8 instruction sequence.

(define_insn "ffssi2"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(ffs:SI (match_operand:SI 1 "register_operand" "r")))
   (clobber (match_scratch:SI 2 "=&r"))]
  "TARGET_SPARCLITE || TARGET_SPARCLET"
  "*
{
  if (TARGET_LIVE_G0)
    output_asm_insn (\"and %%g0,0,%%g0\", operands);
  return \"sub %%g0,%1,%0\;and %0,%1,%0\;scan %0,0,%0\;mov 32,%2\;sub %2,%0,%0\;sra %0,31,%2\;and %2,31,%2\;add %2,%0,%0\";
}"
  [(set_attr "type" "multi")
   (set_attr "length" "8")])

;; ??? This should be a define expand, so that the extra instruction have
;; a chance of being optimized away.

;; Disabled because none of the UltraSparcs implement popc.  The HAL R1
;; does, but no one uses that and we don't have a switch for it.
;
;(define_insn "ffsdi2"
;  [(set (match_operand:DI 0 "register_operand" "=&r")
;	(ffs:DI (match_operand:DI 1 "register_operand" "r")))
;   (clobber (match_scratch:DI 2 "=&r"))]
;  "TARGET_ARCH64"
;  "neg %1,%2\;xnor %1,%2,%2\;popc %2,%0\;movzr %1,0,%0"
;  [(set_attr "type" "multi")
;   (set_attr "length" "4")])

;; Split up troublesome insns for better scheduling.  */

;; The following patterns are straightforward.  They can be applied
;; either before or after register allocation.

(define_split
  [(set (match_operand 0 "splittable_symbolic_memory_operand" "")
	(match_operand 1 "reg_or_0_operand" ""))
   (clobber (match_operand:SI 2 "register_operand" ""))]
  "! flag_pic"
  [(set (match_dup 2) (high:SI (match_dup 3)))
   (set (match_dup 4) (match_dup 1))]
  "
{
  operands[3] = XEXP (operands[0], 0);
  operands[4] = gen_rtx_MEM (GET_MODE (operands[0]),
			 gen_rtx_LO_SUM (SImode, operands[2], operands[3]));
  MEM_IN_STRUCT_P (operands[4]) = MEM_IN_STRUCT_P (operands[0]);
  MEM_VOLATILE_P (operands[4]) = MEM_VOLATILE_P (operands[0]);
  RTX_UNCHANGING_P (operands[4]) = RTX_UNCHANGING_P (operands[0]);
}")

(define_split
  [(set (match_operand 0 "splittable_immediate_memory_operand" "")
	(match_operand 1 "general_operand" ""))
   (clobber (match_operand:SI 2 "register_operand" ""))]
  "flag_pic"
  [(set (match_dup 3) (match_dup 1))]
  "
{
  rtx addr = legitimize_pic_address (XEXP (operands[0], 0),
				     GET_MODE (operands[0]),
				     operands[2]);
  operands[3] = gen_rtx_MEM (GET_MODE (operands[0]), addr);
  MEM_IN_STRUCT_P (operands[3]) = MEM_IN_STRUCT_P (operands[0]);
  MEM_VOLATILE_P (operands[3]) = MEM_VOLATILE_P (operands[0]);
  RTX_UNCHANGING_P (operands[3]) = RTX_UNCHANGING_P (operands[0]);
}")

(define_split
  [(set (match_operand 0 "register_operand" "")
	(match_operand 1 "splittable_immediate_memory_operand" ""))]
  "flag_pic"
  [(set (match_dup 0) (match_dup 2))]
  "
{
  rtx addr = legitimize_pic_address (XEXP (operands[1], 0),
				     GET_MODE (operands[1]),
				     operands[0]);
  operands[2] = gen_rtx_MEM (GET_MODE (operands[1]), addr);
  MEM_IN_STRUCT_P (operands[2]) = MEM_IN_STRUCT_P (operands[1]);
  MEM_VOLATILE_P (operands[2]) = MEM_VOLATILE_P (operands[1]);
  RTX_UNCHANGING_P (operands[2]) = RTX_UNCHANGING_P (operands[1]);
}")

;; Sign- and Zero-extend operations can have symbolic memory operands.

(define_split
  [(set (match_operand 0 "register_operand" "")
	(match_operator 1 "extend_op" [(match_operand 2 "splittable_immediate_memory_operand" "")]))]
  "flag_pic"
  [(set (match_dup 0) (match_op_dup 1 [(match_dup 3)]))]
  "
{
  rtx addr = legitimize_pic_address (XEXP (operands[2], 0),
				     GET_MODE (operands[2]),
				     operands[0]);
  operands[3] = gen_rtx_MEM (GET_MODE (operands[2]), addr);
  MEM_IN_STRUCT_P (operands[3]) = MEM_IN_STRUCT_P (operands[2]);
  MEM_VOLATILE_P (operands[3]) = MEM_VOLATILE_P (operands[2]);
  RTX_UNCHANGING_P (operands[3]) = RTX_UNCHANGING_P (operands[2]);
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

;; LABEL_REFs are not modified by `legitimize_pic_address'
;; so do not recurse infinitely in the PIC case.
(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(match_operand:SI 1 "immediate_operand" ""))]
  "flag_pic && (GET_CODE (operands[1]) == SYMBOL_REF
		|| GET_CODE (operands[1]) == CONST)"
  [(set (match_dup 0) (match_dup 1))]
  "
{
  operands[1] = legitimize_pic_address (operands[1], Pmode, operands[0]);
}")

;; These split sne/seq insns.  The forms of the resulting insns are 
;; somewhat bogus, but they avoid extra patterns and show data dependency.
;; Nothing will look at these in detail after splitting has occurred.

;; ??? v9 DImode versions are missing because addc and subc use %icc.

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(ne:SI (match_operand:SI 1 "register_operand" "")
	       (const_int 0)))
   (clobber (reg:CC 100))]
  ""
  [(set (reg:CC_NOOV 100) (compare:CC_NOOV (neg:SI (match_dup 1))
					   (const_int 0)))
   (set (match_dup 0) (ltu:SI (reg:CC 100) (const_int 0)))]
  "")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(neg:SI (ne:SI (match_operand:SI 1 "register_operand" "")
		       (const_int 0))))
   (clobber (reg:CC 100))]
  ""
  [(set (reg:CC_NOOV 100) (compare:CC_NOOV (neg:SI (match_dup 1))
					   (const_int 0)))
   (set (match_dup 0) (neg:SI (ltu:SI (reg:CC 100) (const_int 0))))]
  "")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(eq:SI (match_operand:SI 1 "register_operand" "")
	       (const_int 0)))
   (clobber (reg:CC 100))]
  ""
  [(set (reg:CC_NOOV 100) (compare:CC_NOOV (neg:SI (match_dup 1))
					   (const_int 0)))
   (set (match_dup 0) (geu:SI (reg:CC 100) (const_int 0)))]
  "")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(neg:SI (eq:SI (match_operand:SI 1 "register_operand" "")
		       (const_int 0))))
   (clobber (reg:CC 100))]
  ""
  [(set (reg:CC_NOOV 100) (compare:CC_NOOV (neg:SI (match_dup 1))
					   (const_int 0)))
   (set (match_dup 0) (neg:SI (geu:SI (reg:CC 100) (const_int 0))))]
  "")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (ne:SI (match_operand:SI 1 "register_operand" "")
			(const_int 0))
		 (match_operand:SI 2 "register_operand" "")))
   (clobber (reg:CC 100))]
  ""
  [(set (reg:CC_NOOV 100) (compare:CC_NOOV (neg:SI (match_dup 1))
					   (const_int 0)))
   (set (match_dup 0) (plus:SI (ltu:SI (reg:CC 100) (const_int 0))
			       (match_dup 2)))]
  "")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(minus:SI (match_operand:SI 2 "register_operand" "")
		  (ne:SI (match_operand:SI 1 "register_operand" "")
			 (const_int 0))))
   (clobber (reg:CC 100))]
  ""
  [(set (reg:CC_NOOV 100) (compare:CC_NOOV (neg:SI (match_dup 1))
					   (const_int 0)))
   (set (match_dup 0) (minus:SI (match_dup 2)
				(ltu:SI (reg:CC 100) (const_int 0))))]
  "")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (eq:SI (match_operand:SI 1 "register_operand" "")
			(const_int 0))
		 (match_operand:SI 2 "register_operand" "")))
   (clobber (reg:CC 100))]
  ""
  [(set (reg:CC_NOOV 100) (compare:CC_NOOV (neg:SI (match_dup 1))
					   (const_int 0)))
   (set (match_dup 0) (plus:SI (geu:SI (reg:CC 100) (const_int 0))
			       (match_dup 2)))]
  "")

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(minus:SI (match_operand:SI 2 "register_operand" "")
		  (eq:SI (match_operand:SI 1 "register_operand" "")
			 (const_int 0))))
   (clobber (reg:CC 100))]
  ""
  [(set (reg:CC_NOOV 100) (compare:CC_NOOV (neg:SI (match_dup 1))
					   (const_int 0)))
   (set (match_dup 0) (minus:SI (match_dup 2)
				(geu:SI (reg:CC 100) (const_int 0))))]
  "")

;; Peepholes go at the end.

;; Optimize consecutive loads or stores into ldd and std when possible.
;; The conditions in which we do this are very restricted and are 
;; explained in the code for {registers,memory}_ok_for_ldd functions.

(define_peephole
  [(set (match_operand:SI 0 "memory_operand" "")
      (const_int 0))
   (set (match_operand:SI 1 "memory_operand" "")
      (const_int 0))]
  "TARGET_V9
   && ! MEM_VOLATILE_P (operands[0]) && ! MEM_VOLATILE_P (operands[1])
   && addrs_ok_for_ldd_peep (XEXP (operands[0], 0), XEXP (operands[1], 0))"
  "stx %%g0,%0")

(define_peephole
  [(set (match_operand:SI 0 "memory_operand" "")
      (const_int 0))
   (set (match_operand:SI 1 "memory_operand" "")
      (const_int 0))]
  "TARGET_V9
   && ! MEM_VOLATILE_P (operands[0]) && ! MEM_VOLATILE_P (operands[1])
   && addrs_ok_for_ldd_peep (XEXP (operands[1], 0), XEXP (operands[0], 0))"
  "stx %%g0,%1")

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
   (set (reg:CC 100)
	(compare:CC (match_operand:SI 2 "register_operand" "r")
		    (const_int 0)))]
  "(rtx_equal_p (operands[2], operands[0])
    || rtx_equal_p (operands[2], operands[1]))
   && ! FP_REG_P (operands[0]) && ! FP_REG_P (operands[1])"
  "orcc %1,0,%0")

(define_peephole
  [(set (match_operand:DI 0 "register_operand" "=r")
	(match_operand:DI 1 "register_operand" "r"))
   (set (reg:CCX 100)
	(compare:CCX (match_operand:DI 2 "register_operand" "r")
		    (const_int 0)))]
  "TARGET_ARCH64
   && (rtx_equal_p (operands[2], operands[0])
       || rtx_equal_p (operands[2], operands[1]))
   && ! FP_REG_P (operands[0]) && ! FP_REG_P (operands[1])"
  "orcc %1,0,%0")

;; Floating-point move peepholes
;; ??? v9: Do we want similar ones?

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lo_sum:SI (match_dup 0)
		   (match_operand:SI 1 "immediate_operand" "i")))
   (set (match_operand:DF 2 "register_operand" "=er")
	(mem:DF (match_dup 0)))]
  "RTX_UNCHANGING_P (operands[1]) && reg_unused_after (operands[0], insn)"
  "*
{
  /* Go by way of output_move_double in case the register in operand 2
     is not properly aligned for ldd.  */
  operands[1] = gen_rtx_MEM (DFmode,
			 gen_rtx_LO_SUM (SImode, operands[0], operands[1]));
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

;; Return peepholes.  First the "normal" ones.
;; These are necessary to catch insns ending up in the epilogue delay list.

(define_insn "*return_qi"
  [(set (match_operand:QI 0 "restore_operand" "")
	(match_operand:QI 1 "arith_operand" "rI"))
   (return)]
  "! TARGET_EPILOGUE && ! TARGET_LIVE_G0"
  "*
{
  if (! TARGET_ARCH64 && current_function_returns_struct)
    return \"jmp %%i7+12\;restore %%g0,%1,%Y0\";
  else if (TARGET_V9 && (GET_CODE (operands[1]) == CONST_INT
			 || IN_OR_GLOBAL_P (operands[1])))
    return \"return %%i7+8\;mov %Y1,%Y0\";
  else
    return \"ret\;restore %%g0,%1,%Y0\";
}"
  [(set_attr "type" "multi")])

(define_insn "*return_hi"
  [(set (match_operand:HI 0 "restore_operand" "")
	(match_operand:HI 1 "arith_operand" "rI"))
   (return)]
  "! TARGET_EPILOGUE && ! TARGET_LIVE_G0"
  "*
{
  if (! TARGET_ARCH64 && current_function_returns_struct)
    return \"jmp %%i7+12\;restore %%g0,%1,%Y0\";
  else if (TARGET_V9 && (GET_CODE (operands[1]) == CONST_INT
			 || IN_OR_GLOBAL_P (operands[1])))
    return \"return %%i7+8\;mov %Y1,%Y0\";
  else
    return \"ret\;restore %%g0,%1,%Y0\";
}"
  [(set_attr "type" "multi")])

(define_insn "*return_si"
  [(set (match_operand:SI 0 "restore_operand" "")
	(match_operand:SI 1 "arith_operand" "rI"))
   (return)]
  "! TARGET_EPILOGUE && ! TARGET_LIVE_G0"
  "*
{
  if (! TARGET_ARCH64 && current_function_returns_struct)
    return \"jmp %%i7+12\;restore %%g0,%1,%Y0\";
  else if (TARGET_V9 && (GET_CODE (operands[1]) == CONST_INT
			 || IN_OR_GLOBAL_P (operands[1])))
    return \"return %%i7+8\;mov %Y1,%Y0\";
  else
    return \"ret\;restore %%g0,%1,%Y0\";
}"
  [(set_attr "type" "multi")])

;; The following pattern is only generated by delayed-branch scheduling,
;; when the insn winds up in the epilogue.  This can only happen when
;; ! TARGET_FPU because otherwise fp return values are in %f0.
(define_insn "*return_sf_no_fpu"
  [(set (match_operand:SF 0 "restore_operand" "r")
	(match_operand:SF 1 "register_operand" "r"))
   (return)]
  "! TARGET_FPU && ! TARGET_EPILOGUE && ! TARGET_LIVE_G0"
  "*
{
  if (! TARGET_ARCH64 && current_function_returns_struct)
    return \"jmp %%i7+12\;restore %%g0,%1,%Y0\";
  else if (TARGET_V9 && IN_OR_GLOBAL_P (operands[1]))
    return \"return %%i7+8\;mov %Y1,%Y0\";
  else
    return \"ret\;restore %%g0,%1,%Y0\";
}"
  [(set_attr "type" "multi")])

(define_insn "*return_addsi"
  [(set (match_operand:SI 0 "restore_operand" "")
	(plus:SI (match_operand:SI 1 "register_operand" "r")
		 (match_operand:SI 2 "arith_operand" "rI")))
   (return)]
  "! TARGET_EPILOGUE && ! TARGET_LIVE_G0"
  "*
{
  if (! TARGET_ARCH64 && current_function_returns_struct)
    return \"jmp %%i7+12\;restore %r1,%2,%Y0\";
  /* If operands are global or in registers, can use return */
  else if (TARGET_V9 && IN_OR_GLOBAL_P (operands[1])
	   && (GET_CODE (operands[2]) == CONST_INT
	       || IN_OR_GLOBAL_P (operands[2])))
    return \"return %%i7+8\;add %Y1,%Y2,%Y0\";
  else
    return \"ret\;restore %r1,%2,%Y0\";
}"
  [(set_attr "type" "multi")])

(define_insn "*return_di"
  [(set (match_operand:DI 0 "restore_operand" "")
	(match_operand:DI 1 "arith_double_operand" "rHI"))
   (return)]
  "TARGET_ARCH64 && ! TARGET_EPILOGUE"
  "ret\;restore %%g0,%1,%Y0"
  [(set_attr "type" "multi")])

(define_insn "*return_adddi"
  [(set (match_operand:DI 0 "restore_operand" "")
	(plus:DI (match_operand:DI 1 "arith_operand" "%r")
		 (match_operand:DI 2 "arith_double_operand" "rHI")))
   (return)]
  "TARGET_ARCH64 && ! TARGET_EPILOGUE"
  "ret\;restore %r1,%2,%Y0"
  [(set_attr "type" "multi")])

;; The following pattern is only generated by delayed-branch scheduling,
;; when the insn winds up in the epilogue.
(define_insn "*return_sf"
  [(set (reg:SF 32)
	(match_operand:SF 0 "register_operand" "f"))
   (return)]
  "! TARGET_EPILOGUE"
  "ret\;fmovs %0,%%f0"
  [(set_attr "type" "multi")])

;; Now peepholes to do a call followed by a jump.

(define_peephole
  [(parallel [(set (match_operand 0 "" "")
		   (call (mem:SI (match_operand:SI 1 "call_operand_address" "ps"))
			 (match_operand 2 "" "")))
	      (clobber (reg:SI 15))])
   (set (pc) (label_ref (match_operand 3 "" "")))]
  "short_branch (INSN_UID (insn), INSN_UID (operands[3])) && in_same_eh_region (insn, operands[3]) && in_same_eh_region (insn, ins1)"
  "call %a1,%2\;add %%o7,(%l3-.-4),%%o7")

(define_peephole
  [(parallel [(call (mem:SI (match_operand:SI 0 "call_operand_address" "ps"))
		    (match_operand 1 "" ""))
	      (clobber (reg:SI 15))])
   (set (pc) (label_ref (match_operand 2 "" "")))]
  "short_branch (INSN_UID (insn), INSN_UID (operands[2])) && in_same_eh_region (insn, operands[2]) && in_same_eh_region (insn, ins1)"
  "call %a0,%1\;add %%o7,(%l2-.-4),%%o7")

(define_peephole
  [(parallel [(set (match_operand 0 "" "")
		   (call (mem:SI (match_operand:DI 1 "call_operand_address" "ps"))
			 (match_operand 2 "" "")))
	      (clobber (reg:DI 15))])
   (set (pc) (label_ref (match_operand 3 "" "")))]
  "TARGET_ARCH64 && short_branch (INSN_UID (insn), INSN_UID (operands[3])) && in_same_eh_region (insn, operands[3]) && in_same_eh_region (insn, ins1)"
  "call %a1,%2\;add %%o7,(%l3-.-4),%%o7")

(define_peephole
  [(parallel [(call (mem:SI (match_operand:DI 0 "call_operand_address" "ps"))
		    (match_operand 1 "" ""))
	      (clobber (reg:DI 15))])
   (set (pc) (label_ref (match_operand 2 "" "")))]
  "TARGET_ARCH64 && short_branch (INSN_UID (insn), INSN_UID (operands[2])) && in_same_eh_region (insn, operands[2]) && in_same_eh_region (insn, ins1)"
  "call %a0,%1\;add %%o7,(%l2-.-4),%%o7")

;; After a nonlocal goto, we need to restore the PIC register, but only
;; if we need it.  So do nothing much here, but we'll check for this in
;; finalize_pic.

(define_insn "nonlocal_goto_receiver"
  [(unspec_volatile [(const_int 0)] 4)]
  "flag_pic"
  "")

(define_insn "trap"
  [(trap_if (const_int 1) (const_int 5))]
  ""
  "ta 5"
  [(set_attr "type" "misc")])

(define_expand "conditional_trap"
  [(trap_if (match_operator 0 "noov_compare_op"
			    [(match_dup 2) (match_dup 3)])
	    (match_operand:SI 1 "arith_operand" ""))]
  ""
  "operands[2] = gen_compare_reg (GET_CODE (operands[0]),
				  sparc_compare_op0, sparc_compare_op1);
   operands[3] = const0_rtx;")

(define_insn ""
  [(trap_if (match_operator 0 "noov_compare_op" [(reg:CC 100) (const_int 0)])
	    (match_operand:SI 1 "arith_operand" "rM"))]
  ""
  "t%C0 %1"
  [(set_attr "type" "misc")])

(define_insn ""
  [(trap_if (match_operator 0 "noov_compare_op" [(reg:CCX 100) (const_int 0)])
	    (match_operand:SI 1 "arith_operand" "rM"))]
  "TARGET_V9"
  "t%C0 %%xcc,%1"
  [(set_attr "type" "misc")])

