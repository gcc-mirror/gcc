;;- Machine description for SPARC chip for GNU C compiler
;;  Copyright (C) 1987, 88, 89, 92-98, 1999 Free Software Foundation, Inc.
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

;; Uses of UNSPEC and UNSPEC_VOLATILE in this file:
;;
;; UNSPEC:		0	movsi_{lo_sum,high}_pic
;;				pic_lo_sum_di
;;				pic_sethi_di
;;			1	update_return
;;			2	get_pc
;;			5	movsi_{,lo_sum_,high_}pic_label_ref
;;			6	seth44
;;			7	setm44
;;			8	setl44
;;			9	sethh
;;			10	setlm
;;			11	embmedany_sethi, embmedany_brsum
;;			13	embmedany_textuhi
;;			14	embmedany_texthi
;;			15	embmedany_textulo
;;			16	embmedany_textlo
;;			18	sethm
;;			19	setlo
;;
;; UNSPEC_VOLATILE:	0	blockage
;;			1	flush_register_windows
;;			2	goto_handler_and_restore
;;			3	goto_handler_and_restore_v9*
;;			4	flush
;;			5	nonlocal_goto_receiver
;;

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
(define_attr "cpu" "v7,cypress,v8,supersparc,sparclite,f930,f934,hypersparc,sparclite86x,sparclet,tsc701,v9,ultrasparc"
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
  "move,unary,binary,compare,load,sload,store,ialu,shift,uncond_branch,branch,call,call_no_delay_slot,return,address,imul,fpload,fpstore,fp,fpmove,fpcmove,fpcmp,fpmul,fpdivs,fpdivd,fpsqrts,fpsqrtd,cmove,multi,misc"
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
  (const (symbol_ref "current_function_uses_only_leaf_regs")))

(define_attr "eligible_for_return_delay" "false,true"
  (symbol_ref "eligible_for_return_delay(insn)"))

(define_attr "in_return_delay" "false,true"
  (if_then_else (and (and (and (eq_attr "type" "move,load,sload,store,binary,ialu")
			       (eq_attr "length" "1"))
			  (eq_attr "leaf_function" "false"))
		     (eq_attr "eligible_for_return_delay" "false"))
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
    (eq_attr "type" "fpsqrts,fpsqrtd"))
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
    (eq_attr "type" "fpsqrts,fpsqrtd"))
  12 10)

(define_function_unit "fp_mds" 1 0
  (and (eq_attr "cpu" "supersparc")
    (eq_attr "type" "imul"))
  4 4)

;; ----- hypersparc/sparclite86x scheduling
;; The Hypersparc can issue 1 - 2 insns per cycle.  The dual issue cases are:
;; L-Ld/St I-Int F-Float B-Branch LI/LF/LB/II/IF/IB/FF/FB
;; II/FF case is only when loading a 32 bit hi/lo constant
;; Single issue insns include call, jmpl, u/smul, u/sdiv, lda, sta, fcmp
;; Memory delivers its result in one cycle to IU

(define_function_unit "memory" 1 0
  (and (ior (eq_attr "cpu" "hypersparc") (eq_attr "cpu" "sparclite86x"))
    (eq_attr "type" "load,sload,fpload"))
  1 1)

(define_function_unit "memory" 1 0
  (and (ior (eq_attr "cpu" "hypersparc") (eq_attr "cpu" "sparclite86x"))
    (eq_attr "type" "store,fpstore"))
  2 1)

(define_function_unit "sparclite86x_branch" 1 0
  (and (eq_attr "cpu" "sparclite86x")
    (eq_attr "type" "branch"))
  1 1)

;; integer multiply insns 
(define_function_unit "sparclite86x_shift" 1 0
  (and (eq_attr "cpu" "sparclite86x")
    (eq_attr "type" "shift"))
  1 1)

(define_function_unit "fp_alu" 1 0
  (and (ior (eq_attr "cpu" "hypersparc") (eq_attr "cpu" "sparclite86x"))
    (eq_attr "type" "fp,fpmove,fpcmp"))
  1 1)

(define_function_unit "fp_mds" 1 0
  (and (ior (eq_attr "cpu" "hypersparc") (eq_attr "cpu" "sparclite86x"))
    (eq_attr "type" "fpmul"))
  1 1)

(define_function_unit "fp_mds" 1 0
  (and (ior (eq_attr "cpu" "hypersparc") (eq_attr "cpu" "sparclite86x"))
    (eq_attr "type" "fpdivs"))
  8 6)

(define_function_unit "fp_mds" 1 0
  (and (ior (eq_attr "cpu" "hypersparc") (eq_attr "cpu" "sparclite86x"))
    (eq_attr "type" "fpdivd"))
  12 10)

(define_function_unit "fp_mds" 1 0
  (and (ior (eq_attr "cpu" "hypersparc") (eq_attr "cpu" "sparclite86x"))
    (eq_attr "type" "fpsqrts,fpsqrtd"))
  17 15)

(define_function_unit "fp_mds" 1 0
  (and (ior (eq_attr "cpu" "hypersparc") (eq_attr "cpu" "sparclite86x"))
    (eq_attr "type" "imul"))
  17 15)

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

(define_function_unit "ieuN" 2 0
  (and (eq_attr "cpu" "ultrasparc")
    (eq_attr "type" "ialu,binary,move,unary,shift,compare,call,call_no_delay_slot,uncond_branch"))
  1 1)

(define_function_unit "ieu0" 1 0
  (and (eq_attr "cpu" "ultrasparc")
    (eq_attr "type" "shift"))
  1 1)

(define_function_unit "ieu0" 1 0
  (and (eq_attr "cpu" "ultrasparc")
    (eq_attr "type" "cmove"))
  2 1)

(define_function_unit "ieu1" 1 0
  (and (eq_attr "cpu" "ultrasparc")
    (eq_attr "type" "compare,call,call_no_delay_slot,uncond_branch"))
  1 1)

(define_function_unit "cti" 1 0
  (and (eq_attr "cpu" "ultrasparc")
    (eq_attr "type" "branch"))
  1 1)

;; Timings; throughput/latency
;; FMOV     1/1    fmov, fabs, fneg
;; FMOVcc   1/2
;; FADD     1/3    add/sub, format conv, compar
;; FMUL     1/3
;; FDIVs    12/12
;; FDIVd    22/22
;; FSQRTs   12/12
;; FSQRTd   22/22
;; FCMP takes 1 cycle to branch, 2 cycles to conditional move.
;;
;; FDIV{s,d}/FSQRT{s,d} are given their own unit since they only
;; use the FPM multiplier for final rounding 3 cycles before the
;; end of their latency and we have no real way to model that.
;;
;; ??? This is really bogus because the timings really depend upon
;; who uses the result.  We should record who the user is with
;; more descriptive 'type' attribute names and account for these
;; issues in ultrasparc_adjust_cost. 

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
  3 1)

(define_function_unit "fadd" 1 0
  (and (eq_attr "cpu" "ultrasparc")
    (eq_attr "type" "fpcmp"))
  2 1)

(define_function_unit "fmul" 1 0
  (and (eq_attr "cpu" "ultrasparc")
    (eq_attr "type" "fpmul"))
  3 1)

(define_function_unit "fadd" 1 0
  (and (eq_attr "cpu" "ultrasparc")
    (eq_attr "type" "fpcmove"))
  2 1)

(define_function_unit "fdiv" 1 0
  (and (eq_attr "cpu" "ultrasparc")
    (eq_attr "type" "fpdivs"))
  12 12)

(define_function_unit "fdiv" 1 0
  (and (eq_attr "cpu" "ultrasparc")
    (eq_attr "type" "fpdivd"))
  22 22)

(define_function_unit "fdiv" 1 0
  (and (eq_attr "cpu" "ultrasparc")
    (eq_attr "type" "fpsqrts"))
  12 12)

(define_function_unit "fdiv" 1 0
  (and (eq_attr "cpu" "ultrasparc")
    (eq_attr "type" "fpsqrtd"))
  22 22)

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
  "TARGET_ARCH64"
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
  "TARGET_FPU && (TARGET_HARD_QUAD || TARGET_ARCH64)"
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
  "cmp\\t%0, %1"
  [(set_attr "type" "compare")])

(define_insn "*cmpdi_sp64"
  [(set (reg:CCX 100)
	(compare:CCX (match_operand:DI 0 "register_operand" "r")
		     (match_operand:DI 1 "arith_double_operand" "rHI")))]
  "TARGET_ARCH64"
  "cmp\\t%0, %1"
  [(set_attr "type" "compare")])

(define_insn "*cmpsf_fpe"
  [(set (match_operand:CCFPE 0 "fcc_reg_operand" "=c")
	(compare:CCFPE (match_operand:SF 1 "register_operand" "f")
		       (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_FPU"
  "*
{
  if (TARGET_V9)
    return \"fcmpes\\t%0, %1, %2\";
  return \"fcmpes\\t%1, %2\";
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
    return \"fcmped\\t%0, %1, %2\";
  return \"fcmped\\t%1, %2\";
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
    return \"fcmpeq\\t%0, %1, %2\";
  return \"fcmpeq\\t%1, %2\";
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
    return \"fcmps\\t%0, %1, %2\";
  return \"fcmps\\t%1, %2\";
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
    return \"fcmpd\\t%0, %1, %2\";
  return \"fcmpd\\t%1, %2\";
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
    return \"fcmpq\\t%0, %1, %2\";
  return \"fcmpq\\t%1, %2\";
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
	(eq:SI (match_dup 3) (const_int 0)))]
  "TARGET_ARCH64"
  "{ operands[3] = gen_reg_rtx (DImode); }")

(define_expand "snedi_special_trunc"
  [(set (match_dup 3)
	(xor:DI (match_operand:DI 1 "register_operand" "")
		(match_operand:DI 2 "register_operand" "")))
   (set (match_operand:SI 0 "register_operand" "")
	(ne:SI (match_dup 3) (const_int 0)))]
  "TARGET_ARCH64"
  "{ operands[3] = gen_reg_rtx (DImode); }")

(define_expand "seqsi_special_extend"
  [(set (match_dup 3)
	(xor:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "register_operand" "")))
   (parallel [(set (match_operand:DI 0 "register_operand" "")
		   (eq:DI (match_dup 3) (const_int 0)))
	      (clobber (reg:CC 100))])]
  "TARGET_ARCH64"
  "{ operands[3] = gen_reg_rtx (SImode); }")

(define_expand "snesi_special_extend"
  [(set (match_dup 3)
	(xor:SI (match_operand:SI 1 "register_operand" "")
		(match_operand:SI 2 "register_operand" "")))
   (parallel [(set (match_operand:DI 0 "register_operand" "")
		   (ne:DI (match_dup 3) (const_int 0)))
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
  else if (GET_MODE (sparc_compare_op0) == TFmode && TARGET_ARCH64 && ! TARGET_HARD_QUAD)
    {
      sparc_emit_float_lib_cmp (sparc_compare_op0, sparc_compare_op1, EQ);
      emit_jump_insn (gen_sne (operands[0]));
      DONE;
    }
  else if (TARGET_V9)
    {
      if (gen_v9_scc (EQ, operands))
	DONE;
      /* fall through */
    }
  FAIL;
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
  else if (GET_MODE (sparc_compare_op0) == TFmode && TARGET_ARCH64 && ! TARGET_HARD_QUAD)
    {
      sparc_emit_float_lib_cmp (sparc_compare_op0, sparc_compare_op1, NE);
      emit_jump_insn (gen_sne (operands[0]));
      DONE;
    }
  else if (TARGET_V9)
    {
      if (gen_v9_scc (NE, operands))
	DONE;
      /* fall through */
    }
  FAIL;
}")

(define_expand "sgt"
  [(set (match_operand:SI 0 "intreg_operand" "")
	(gt:SI (match_dup 1) (const_int 0)))]
  "! TARGET_LIVE_G0"
  "
{
  if (GET_MODE (sparc_compare_op0) == TFmode && TARGET_ARCH64 && ! TARGET_HARD_QUAD)
    {
      sparc_emit_float_lib_cmp (sparc_compare_op0, sparc_compare_op1, GT);
      emit_jump_insn (gen_sne (operands[0]));
      DONE;
    }
  else if (TARGET_V9)
    {
      if (gen_v9_scc (GT, operands))
	DONE;
      /* fall through */
    }
  FAIL;
}")

(define_expand "slt"
  [(set (match_operand:SI 0 "intreg_operand" "")
	(lt:SI (match_dup 1) (const_int 0)))]
  "! TARGET_LIVE_G0"
  "
{
  if (GET_MODE (sparc_compare_op0) == TFmode && TARGET_ARCH64 && ! TARGET_HARD_QUAD)
    {
      sparc_emit_float_lib_cmp (sparc_compare_op0, sparc_compare_op1, LT);
      emit_jump_insn (gen_sne (operands[0]));
      DONE;
    }
  else if (TARGET_V9)
    {
      if (gen_v9_scc (LT, operands))
	DONE;
      /* fall through */
    }
  FAIL;
}")

(define_expand "sge"
  [(set (match_operand:SI 0 "intreg_operand" "")
	(ge:SI (match_dup 1) (const_int 0)))]
  "! TARGET_LIVE_G0"
  "
{
  if (GET_MODE (sparc_compare_op0) == TFmode && TARGET_ARCH64 && ! TARGET_HARD_QUAD)
    {
      sparc_emit_float_lib_cmp (sparc_compare_op0, sparc_compare_op1, GE);
      emit_jump_insn (gen_sne (operands[0]));
      DONE;
    }
  else if (TARGET_V9)
    {
      if (gen_v9_scc (GE, operands))
	DONE;
      /* fall through */
    }
  FAIL;
}")

(define_expand "sle"
  [(set (match_operand:SI 0 "intreg_operand" "")
	(le:SI (match_dup 1) (const_int 0)))]
  "! TARGET_LIVE_G0"
  "
{
  if (GET_MODE (sparc_compare_op0) == TFmode && TARGET_ARCH64 && ! TARGET_HARD_QUAD)
    {
      sparc_emit_float_lib_cmp (sparc_compare_op0, sparc_compare_op1, LE);
      emit_jump_insn (gen_sne (operands[0]));
      DONE;
    }
  else if (TARGET_V9)
    {
      if (gen_v9_scc (LE, operands))
	DONE;
      /* fall through */
    }
  FAIL;
}")

(define_expand "sgtu"
  [(set (match_operand:SI 0 "intreg_operand" "")
	(gtu:SI (match_dup 1) (const_int 0)))]
  "! TARGET_LIVE_G0"
  "
{
  if (! TARGET_V9)
    {
      rtx tem, pat;

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
	  pat = gen_sltu (operands[0]);
          if (pat == NULL_RTX)
            FAIL;
          emit_insn (pat);
	  DONE;
	}
    }
  else
    {
      if (gen_v9_scc (GTU, operands))
	DONE;
    }
  FAIL;
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
      rtx tem, pat;

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
	  pat = gen_sgeu (operands[0]);
          if (pat == NULL_RTX)
            FAIL;
          emit_insn (pat);
	  DONE;
	}
    }
  else
    {
      if (gen_v9_scc (LEU, operands))
	DONE;
    }
  FAIL;
}")

;; Now the DEFINE_INSNs for the scc cases.

;; The SEQ and SNE patterns are special because they can be done
;; without any branching and do not involve a COMPARE.  We want
;; them to always use the splitz below so the results can be
;; scheduled.

(define_insn "*snesi_zero"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ne:SI (match_operand:SI 1 "register_operand" "r")
	       (const_int 0)))
   (clobber (reg:CC 100))]
  "! TARGET_LIVE_G0"
  "#"
  [(set_attr "length" "2")])

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

(define_insn "*neg_snesi_zero"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (ne:SI (match_operand:SI 1 "register_operand" "r")
		       (const_int 0))))
   (clobber (reg:CC 100))]
  "! TARGET_LIVE_G0"
  "#"
  [(set_attr "length" "2")])

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

(define_insn "*snesi_zero_extend"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ne:DI (match_operand:SI 1 "register_operand" "r")
	       (const_int 0)))
   (clobber (reg:CC 100))]
  "TARGET_ARCH64"
  "#"
  [(set_attr "type" "unary")
   (set_attr "length" "2")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
        (ne:DI (match_operand:SI 1 "register_operand" "")
               (const_int 0)))
   (clobber (reg:CC 100))]
  "TARGET_ARCH64"
  [(set (reg:CC_NOOV 100) (compare:CC_NOOV (minus:SI (const_int 0) (match_dup 1))
                                           (const_int 0)))
   (set (match_dup 0) (zero_extend:DI (plus:SI (plus:SI (const_int 0)
                                                        (const_int 0))
                                               (ltu:SI (reg:CC_NOOV 100)
                                                       (const_int 0)))))]
  "")

(define_insn "*snedi_zero"
  [(set (match_operand:DI 0 "register_operand" "=&r")
	(ne:DI (match_operand:DI 1 "register_operand" "r")
	       (const_int 0)))]
  "TARGET_ARCH64"
  "#"
  [(set_attr "type" "cmove")
   (set_attr "length" "2")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
        (ne:DI (match_operand:DI 1 "register_operand" "")
               (const_int 0)))]
  "TARGET_ARCH64"
  [(set (match_dup 0) (const_int 0))
   (set (match_dup 0) (if_then_else:DI (ne:DI (match_dup 1)
                                              (const_int 0))
                                       (const_int 1)
                                       (match_dup 0)))]
  "")

(define_insn "*neg_snedi_zero"
  [(set (match_operand:DI 0 "register_operand" "=&r")
	(neg:DI (ne:DI (match_operand:DI 1 "register_operand" "r")
		       (const_int 0))))]
  "TARGET_ARCH64"
  "#"
  [(set_attr "type" "cmove")
   (set_attr "length" "2")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
        (neg:DI (ne:DI (match_operand:DI 1 "register_operand" "")
                       (const_int 0))))]
  "TARGET_ARCH64"
  [(set (match_dup 0) (const_int 0))
   (set (match_dup 0) (if_then_else:DI (ne:DI (match_dup 1)
                                              (const_int 0))
                                       (const_int -1)
                                       (match_dup 0)))]
  "")

(define_insn "*snedi_zero_trunc"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(ne:SI (match_operand:DI 1 "register_operand" "r")
	       (const_int 0)))]
  "TARGET_ARCH64"
  "#"
  [(set_attr "type" "cmove")
   (set_attr "length" "2")])

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
        (ne:SI (match_operand:DI 1 "register_operand" "")
               (const_int 0)))]
  "TARGET_ARCH64"
  [(set (match_dup 0) (const_int 0))
   (set (match_dup 0) (if_then_else:SI (ne:DI (match_dup 1)
                                              (const_int 0))
                                       (const_int 1)
                                       (match_dup 0)))]
  "")

(define_insn "*seqsi_zero"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(eq:SI (match_operand:SI 1 "register_operand" "r")
	       (const_int 0)))
   (clobber (reg:CC 100))]
  "! TARGET_LIVE_G0"
  "#"
  [(set_attr "length" "2")])

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

(define_insn "*neg_seqsi_zero"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (eq:SI (match_operand:SI 1 "register_operand" "r")
		       (const_int 0))))
   (clobber (reg:CC 100))]
  "! TARGET_LIVE_G0"
  "#"
  [(set_attr "length" "2")])

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

(define_insn "*seqsi_zero_extend"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(eq:DI (match_operand:SI 1 "register_operand" "r")
	       (const_int 0)))
   (clobber (reg:CC 100))]
  "TARGET_ARCH64"
  "#"
  [(set_attr "type" "unary")
   (set_attr "length" "2")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
        (eq:DI (match_operand:SI 1 "register_operand" "")
               (const_int 0)))
   (clobber (reg:CC 100))]
  "TARGET_ARCH64"
  [(set (reg:CC_NOOV 100) (compare:CC_NOOV (minus:SI (const_int 0) (match_dup 1))
                                           (const_int 0)))
   (set (match_dup 0) (zero_extend:DI (minus:SI (minus:SI (const_int 0)
                                                          (const_int -1))
                                                (ltu:SI (reg:CC_NOOV 100)
                                                        (const_int 0)))))]
  "")

(define_insn "*seqdi_zero"
  [(set (match_operand:DI 0 "register_operand" "=&r")
	(eq:DI (match_operand:DI 1 "register_operand" "r")
	       (const_int 0)))]
  "TARGET_ARCH64"
  "#"
  [(set_attr "type" "cmove")
   (set_attr "length" "2")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
        (eq:DI (match_operand:DI 1 "register_operand" "")
               (const_int 0)))]
  "TARGET_ARCH64"
  [(set (match_dup 0) (const_int 0))
   (set (match_dup 0) (if_then_else:DI (eq:DI (match_dup 1)
                                              (const_int 0))
                                       (const_int 1)
                                       (match_dup 0)))]
  "")

(define_insn "*neg_seqdi_zero"
  [(set (match_operand:DI 0 "register_operand" "=&r")
	(neg:DI (eq:DI (match_operand:DI 1 "register_operand" "r")
		       (const_int 0))))]
  "TARGET_ARCH64"
  "#"
  [(set_attr "type" "cmove")
   (set_attr "length" "2")]) 

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
        (neg:DI (eq:DI (match_operand:DI 1 "register_operand" "")
                       (const_int 0))))]
  "TARGET_ARCH64"
  [(set (match_dup 0) (const_int 0))
   (set (match_dup 0) (if_then_else:DI (eq:DI (match_dup 1)
                                              (const_int 0))
                                       (const_int -1)
                                       (match_dup 0)))]
  "")

(define_insn "*seqdi_zero_trunc"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(eq:SI (match_operand:DI 1 "register_operand" "r")
	       (const_int 0)))]
  "TARGET_ARCH64"
  "#"
  [(set_attr "type" "cmove")
   (set_attr "length" "2")])

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
        (eq:SI (match_operand:DI 1 "register_operand" "")
               (const_int 0)))]
  "TARGET_ARCH64"
  [(set (match_dup 0) (const_int 0))
   (set (match_dup 0) (if_then_else:SI (eq:DI (match_dup 1)
                                              (const_int 0))
                                       (const_int 1)
                                       (match_dup 0)))]
  "")

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
  "#"
  [(set_attr "length" "2")])

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (ne:SI (match_operand:SI 1 "register_operand" "")
			(const_int 0))
		 (match_operand:SI 2 "register_operand" "")))
   (clobber (reg:CC 100))]
  "! TARGET_LIVE_G0"
  [(set (reg:CC_NOOV 100) (compare:CC_NOOV (neg:SI (match_dup 1))
					   (const_int 0)))
   (set (match_dup 0) (plus:SI (ltu:SI (reg:CC 100) (const_int 0))
			       (match_dup 2)))]
  "")

(define_insn "*x_minus_i_ne_0"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 2 "register_operand" "r")
		  (ne:SI (match_operand:SI 1 "register_operand" "r")
			 (const_int 0))))
   (clobber (reg:CC 100))]
  "! TARGET_LIVE_G0"
  "#"
  [(set_attr "length" "2")])

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(minus:SI (match_operand:SI 2 "register_operand" "")
		  (ne:SI (match_operand:SI 1 "register_operand" "")
			 (const_int 0))))
   (clobber (reg:CC 100))]
  "! TARGET_LIVE_G0"
  [(set (reg:CC_NOOV 100) (compare:CC_NOOV (neg:SI (match_dup 1))
					   (const_int 0)))
   (set (match_dup 0) (minus:SI (match_dup 2)
				(ltu:SI (reg:CC 100) (const_int 0))))]
  "")

(define_insn "*x_plus_i_eq_0"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (eq:SI (match_operand:SI 1 "register_operand" "r")
			(const_int 0))
		 (match_operand:SI 2 "register_operand" "r")))
   (clobber (reg:CC 100))]
  "! TARGET_LIVE_G0"
  "#"
  [(set_attr "length" "2")])

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (eq:SI (match_operand:SI 1 "register_operand" "")
			(const_int 0))
		 (match_operand:SI 2 "register_operand" "")))
   (clobber (reg:CC 100))]
  "! TARGET_LIVE_G0"
  [(set (reg:CC_NOOV 100) (compare:CC_NOOV (neg:SI (match_dup 1))
					   (const_int 0)))
   (set (match_dup 0) (plus:SI (geu:SI (reg:CC 100) (const_int 0))
			       (match_dup 2)))]
  "")

(define_insn "*x_minus_i_eq_0"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 2 "register_operand" "r")
		  (eq:SI (match_operand:SI 1 "register_operand" "r")
			 (const_int 0))))
   (clobber (reg:CC 100))]
  "! TARGET_LIVE_G0"
  "#"
  [(set_attr "length" "2")])

(define_split
  [(set (match_operand:SI 0 "register_operand" "")
	(minus:SI (match_operand:SI 2 "register_operand" "")
		  (eq:SI (match_operand:SI 1 "register_operand" "")
			 (const_int 0))))
   (clobber (reg:CC 100))]
  "! TARGET_LIVE_G0"
  [(set (reg:CC_NOOV 100) (compare:CC_NOOV (neg:SI (match_dup 1))
					   (const_int 0)))
   (set (match_dup 0) (minus:SI (match_dup 2)
				(geu:SI (reg:CC 100) (const_int 0))))]
  "")

;; We can also do GEU and LTU directly, but these operate after a compare.
;; ??? The addx/subx insns use the 32 bit carry flag so there are no DImode
;; versions for v9.

(define_insn "*sltu_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ltu:SI (reg:CC 100) (const_int 0)))]
  "! TARGET_LIVE_G0"
  "addx\\t%%g0, 0, %0"
  [(set_attr "type" "misc")
   (set_attr "length" "1")])

(define_insn "*neg_sltu_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (ltu:SI (reg:CC 100) (const_int 0))))]
  "! TARGET_LIVE_G0"
  "subx\\t%%g0, 0, %0"
  [(set_attr "type" "misc")
   (set_attr "length" "1")])

;; ??? Combine should canonicalize these next two to the same pattern.
(define_insn "*neg_sltu_minus_x"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (neg:SI (ltu:SI (reg:CC 100) (const_int 0)))
		  (match_operand:SI 1 "arith_operand" "rI")))]
  "! TARGET_LIVE_G0"
  "subx\\t%%g0, %1, %0"
  [(set_attr "type" "misc")
   (set_attr "length" "1")])

(define_insn "*neg_sltu_plus_x"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (plus:SI (ltu:SI (reg:CC 100) (const_int 0))
			 (match_operand:SI 1 "arith_operand" "rI"))))]
  "! TARGET_LIVE_G0"
  "subx\\t%%g0, %1, %0"
  [(set_attr "type" "misc")
   (set_attr "length" "1")])

(define_insn "*sgeu_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(geu:SI (reg:CC 100) (const_int 0)))]
  "! TARGET_LIVE_G0"
  "subx\\t%%g0, -1, %0"
  [(set_attr "type" "misc")
   (set_attr "length" "1")])

(define_insn "*neg_sgeu_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (geu:SI (reg:CC 100) (const_int 0))))]
  "! TARGET_LIVE_G0"
  "addx\\t%%g0, -1, %0"
  [(set_attr "type" "misc")
   (set_attr "length" "1")])

;; We can also do (x + ((unsigned) i >= 0)) and related, so put them in.
;; ??? The addx/subx insns use the 32 bit carry flag so there are no DImode
;; versions for v9.

(define_insn "*sltu_plus_x"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (ltu:SI (reg:CC 100) (const_int 0))
		 (match_operand:SI 1 "arith_operand" "rI")))]
  "! TARGET_LIVE_G0"
  "addx\\t%%g0, %1, %0"
  [(set_attr "type" "misc")
   (set_attr "length" "1")])

(define_insn "*sltu_plus_x_plus_y"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (ltu:SI (reg:CC 100) (const_int 0))
		 (plus:SI (match_operand:SI 1 "arith_operand" "%r")
			  (match_operand:SI 2 "arith_operand" "rI"))))]
  ""
  "addx\\t%1, %2, %0"
  [(set_attr "type" "misc")
   (set_attr "length" "1")])

(define_insn "*x_minus_sltu"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 1 "register_operand" "r")
		  (ltu:SI (reg:CC 100) (const_int 0))))]
  ""
  "subx\\t%1, 0, %0"
  [(set_attr "type" "misc")
   (set_attr "length" "1")])

;; ??? Combine should canonicalize these next two to the same pattern.
(define_insn "*x_minus_y_minus_sltu"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (minus:SI (match_operand:SI 1 "reg_or_0_operand" "rJ")
			    (match_operand:SI 2 "arith_operand" "rI"))
		  (ltu:SI (reg:CC 100) (const_int 0))))]
  ""
  "subx\\t%r1, %2, %0"
  [(set_attr "type" "misc")
   (set_attr "length" "1")])

(define_insn "*x_minus_sltu_plus_y"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 1 "reg_or_0_operand" "rJ")
		  (plus:SI (ltu:SI (reg:CC 100) (const_int 0))
			   (match_operand:SI 2 "arith_operand" "rI"))))]
  ""
  "subx\\t%r1, %2, %0"
  [(set_attr "type" "misc")
   (set_attr "length" "1")])

(define_insn "*sgeu_plus_x"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (geu:SI (reg:CC 100) (const_int 0))
		 (match_operand:SI 1 "register_operand" "r")))]
  ""
  "subx\\t%1, -1, %0"
  [(set_attr "type" "misc")
   (set_attr "length" "1")])

(define_insn "*x_minus_sgeu"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 1 "register_operand" "r")
		  (geu:SI (reg:CC 100) (const_int 0))))]
  ""
  "addx\\t%1, -1, %0"
  [(set_attr "type" "misc")
   (set_attr "length" "1")])

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
  else if (GET_MODE (sparc_compare_op0) == TFmode && TARGET_ARCH64 && ! TARGET_HARD_QUAD)
    {
      sparc_emit_float_lib_cmp (sparc_compare_op0, sparc_compare_op1, EQ);
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
  else if (GET_MODE (sparc_compare_op0) == TFmode && TARGET_ARCH64 && ! TARGET_HARD_QUAD)
    {
      sparc_emit_float_lib_cmp (sparc_compare_op0, sparc_compare_op1, NE);
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
  else if (GET_MODE (sparc_compare_op0) == TFmode && TARGET_ARCH64 && ! TARGET_HARD_QUAD)
    {
      sparc_emit_float_lib_cmp (sparc_compare_op0, sparc_compare_op1, GT);
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
  else if (GET_MODE (sparc_compare_op0) == TFmode && TARGET_ARCH64 && ! TARGET_HARD_QUAD)
    {
      sparc_emit_float_lib_cmp (sparc_compare_op0, sparc_compare_op1, LT);
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
  else if (GET_MODE (sparc_compare_op0) == TFmode && TARGET_ARCH64 && ! TARGET_HARD_QUAD)
    {
      sparc_emit_float_lib_cmp (sparc_compare_op0, sparc_compare_op1, GE);
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
  else if (GET_MODE (sparc_compare_op0) == TFmode && TARGET_ARCH64 && ! TARGET_HARD_QUAD)
    {
      sparc_emit_float_lib_cmp (sparc_compare_op0, sparc_compare_op1, LE);
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

;; XXX fpcmp nop braindamage
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

;; XXX fpcmp nop braindamage
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

;; XXX fpcmp nop braindamage
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

;; XXX fpcmp nop braindamage
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

;; XXX fpcmp nop braindamage
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

;; XXX fpcmp nop braindamage
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

;; XXX
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
			  ! final_sequence, insn);
}"
  [(set_attr "type" "branch")])

;; XXX
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
			  ! final_sequence, insn);
}"
  [(set_attr "type" "branch")])

;; Load program counter insns.

(define_insn "get_pc"
  [(clobber (reg:SI 15))
   (set (match_operand 0 "register_operand" "=r")
	(unspec [(match_operand 1 "" "") (match_operand 2 "" "")] 2))]
  "flag_pic && REGNO (operands[0]) == 23"
  "sethi\\t%%hi(%a1-4), %0\\n\\tcall\\t%a2\\n\\tadd\\t%0, %%lo(%a1+4), %0"
  [(set_attr "length" "3")])

;; Currently unused...
;; (define_insn "get_pc_via_rdpc"
;;   [(set (match_operand 0 "register_operand" "=r") (pc))]
;;   "TARGET_V9"
;;   "rd\\t%%pc, %0"
;;   [(set_attr "type" "move")])


;; Move instructions

(define_expand "movqi"
  [(set (match_operand:QI 0 "general_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
  "
{
  /* Working with CONST_INTs is easier, so convert
     a double if needed.  */
  if (GET_CODE (operands[1]) == CONST_DOUBLE)
    {
      operands[1] = GEN_INT (CONST_DOUBLE_LOW (operands[1]) & 0xff);
    }
  else if (GET_CODE (operands[1]) == CONST_INT)
    {
      /* And further, we know for all QI cases that only the
	 low byte is significant, which we can always process
	 in a single insn.  So mask it now.  */
      operands[1] = GEN_INT (INTVAL (operands[1]) & 0xff);
    }

  /* Handle sets of MEM first.  */
  if (GET_CODE (operands[0]) == MEM)
    {
      /* This checks TARGET_LIVE_G0 for us.  */
      if (reg_or_0_operand (operands[1], QImode))
	goto movqi_is_ok;

      if (! reload_in_progress)
	{
	  operands[0] = validize_mem (operands[0]);
	  operands[1] = force_reg (QImode, operands[1]);
	}
    }

  /* Fixup PIC cases.  */
  if (flag_pic)
    {
      if (CONSTANT_P (operands[1])
	  && pic_address_needs_scratch (operands[1]))
	operands[1] = legitimize_pic_address (operands[1], QImode, 0);

      if (symbolic_operand (operands[1], QImode))
	{
	  operands[1] = legitimize_pic_address (operands[1],
						QImode,
						(reload_in_progress ?
						 operands[0] :
						 NULL_RTX));
	  goto movqi_is_ok;
	}
    }

  /* All QI constants require only one insn, so proceed.  */

 movqi_is_ok:
  ;
}")

(define_insn "*movqi_insn"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,r,m")
	(match_operand:QI 1 "input_operand"   "rI,m,rJ"))]
  "(register_operand (operands[0], QImode)
    || reg_or_0_operand (operands[1], QImode))"
  "@
   mov\\t%1, %0
   ldub\\t%1, %0
   stb\\t%r1, %0"
  [(set_attr "type" "move,load,store")
   (set_attr "length" "1")])

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  "
{
  /* Working with CONST_INTs is easier, so convert
     a double if needed.  */
  if (GET_CODE (operands[1]) == CONST_DOUBLE)
    operands[1] = GEN_INT (CONST_DOUBLE_LOW (operands[1]));

  /* Handle sets of MEM first.  */
  if (GET_CODE (operands[0]) == MEM)
    {
      /* This checks TARGET_LIVE_G0 for us.  */
      if (reg_or_0_operand (operands[1], HImode))
	goto movhi_is_ok;

      if (! reload_in_progress)
	{
	  operands[0] = validize_mem (operands[0]);
	  operands[1] = force_reg (HImode, operands[1]);
	}
    }

  /* Fixup PIC cases.  */
  if (flag_pic)
    {
      if (CONSTANT_P (operands[1])
	  && pic_address_needs_scratch (operands[1]))
	operands[1] = legitimize_pic_address (operands[1], HImode, 0);

      if (symbolic_operand (operands[1], HImode))
	{
	  operands[1] = legitimize_pic_address (operands[1],
						HImode,
						(reload_in_progress ?
						 operands[0] :
						 NULL_RTX));
	  goto movhi_is_ok;
	}
    }

  /* This makes sure we will not get rematched due to splittage. */
  if (! CONSTANT_P (operands[1]) || input_operand (operands[1], HImode))
    ;
  else if (CONSTANT_P (operands[1])
	   && GET_CODE (operands[1]) != HIGH
	   && GET_CODE (operands[1]) != LO_SUM)
    {
      sparc_emit_set_const32 (operands[0], operands[1]);
      DONE;
    }
 movhi_is_ok:
  ;
}")

(define_insn "*movhi_const64_special"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(match_operand:HI 1 "const64_high_operand" ""))]
  "TARGET_ARCH64"
  "sethi\\t%%hi(%a1), %0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn "*movhi_insn"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,r,m")
	(match_operand:HI 1 "input_operand"   "rI,K,m,rJ"))]
  "(register_operand (operands[0], HImode)
    || reg_or_0_operand (operands[1], HImode))"
  "@
   mov\\t%1, %0
   sethi\\t%%hi(%a1), %0
   lduh\\t%1, %0
   sth\\t%r1, %0"
  [(set_attr "type" "move,move,load,store")
   (set_attr "length" "1")])

;; We always work with constants here.
(define_insn "*movhi_lo_sum"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(ior:HI (match_operand:HI 1 "arith_operand" "%r")
                (match_operand:HI 2 "arith_operand" "I")))]
  ""
  "or\\t%1, %2, %0"
  [(set_attr "type" "ialu")
   (set_attr "length" "1")])

(define_expand "movsi"
  [(set (match_operand:SI 0 "general_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
  "
{
  /* Working with CONST_INTs is easier, so convert
     a double if needed.  */
  if (GET_CODE (operands[1]) == CONST_DOUBLE)
    operands[1] = GEN_INT (CONST_DOUBLE_LOW (operands[1]));

  /* Handle sets of MEM first.  */
  if (GET_CODE (operands[0]) == MEM)
    {
      /* This checks TARGET_LIVE_G0 for us.  */
      if (reg_or_0_operand (operands[1], SImode))
	goto movsi_is_ok;

      if (! reload_in_progress)
	{
	  operands[0] = validize_mem (operands[0]);
	  operands[1] = force_reg (SImode, operands[1]);
	}
    }

  /* Fixup PIC cases.  */
  if (flag_pic)
    {
      if (CONSTANT_P (operands[1])
	  && pic_address_needs_scratch (operands[1]))
	operands[1] = legitimize_pic_address (operands[1], SImode, 0);

      if (GET_CODE (operands[1]) == LABEL_REF)
	{
	  /* shit */
	  emit_insn (gen_movsi_pic_label_ref (operands[0], operands[1]));
	  DONE;
	}

      if (symbolic_operand (operands[1], SImode))
	{
	  operands[1] = legitimize_pic_address (operands[1],
						SImode,
						(reload_in_progress ?
						 operands[0] :
						 NULL_RTX));
	  goto movsi_is_ok;
	}
    }

  /* If we are trying to toss an integer constant into the
     FPU registers, force it into memory.  */
  if (GET_CODE (operands[0]) == REG
      && REGNO (operands[0]) >= SPARC_FIRST_FP_REG
      && REGNO (operands[0]) <= SPARC_LAST_V9_FP_REG
      && CONSTANT_P (operands[1]))
    operands[1] = validize_mem (force_const_mem (GET_MODE (operands[0]),
						 operands[1]));

  /* This makes sure we will not get rematched due to splittage. */
  if (! CONSTANT_P (operands[1]) || input_operand (operands[1], SImode))
    ;
  else if (CONSTANT_P (operands[1])
	   && GET_CODE (operands[1]) != HIGH
	   && GET_CODE (operands[1]) != LO_SUM)
    {
      sparc_emit_set_const32 (operands[0], operands[1]);
      DONE;
    }
 movsi_is_ok:
  ;
}")

;; Special LIVE_G0 pattern to obtain zero in a register.
(define_insn "*movsi_zero_liveg0"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (match_operand:SI 1 "zero_operand" "J"))]
  "TARGET_LIVE_G0"
  "and\\t%0, 0, %0"
  [(set_attr "type" "binary")
   (set_attr "length" "1")])

;; This is needed to show CSE exactly which bits are set
;; in a 64-bit register by sethi instructions.
(define_insn "*movsi_const64_special"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operand:SI 1 "const64_high_operand" ""))]
  "TARGET_ARCH64"
  "sethi\\t%%hi(%a1), %0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn "*movsi_insn"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,f,r,r,r,f,m,m,d")
	(match_operand:SI 1 "input_operand"   "rI,!f,K,J,m,!m,rJ,!f,J"))]
  "(register_operand (operands[0], SImode)
    || reg_or_0_operand (operands[1], SImode))"
  "@
   mov\\t%1, %0
   fmovs\\t%1, %0
   sethi\\t%%hi(%a1), %0
   clr\\t%0
   ld\\t%1, %0
   ld\\t%1, %0
   st\\t%r1, %0
   st\\t%1, %0
   fzeros\\t%0"
  [(set_attr "type" "move,fpmove,move,move,load,fpload,store,fpstore,fpmove")
   (set_attr "length" "1")])

(define_insn "*movsi_lo_sum"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lo_sum:SI (match_operand:SI 1 "register_operand" "r")
                   (match_operand:SI 2 "immediate_operand" "in")))]
  ""
  "or\\t%1, %%lo(%a2), %0"
  [(set_attr "type" "ialu")
   (set_attr "length" "1")])

(define_insn "*movsi_high"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(high:SI (match_operand:SI 1 "immediate_operand" "in")))]
  ""
  "sethi\\t%%hi(%a1), %0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

;; The next two patterns must wrap the SYMBOL_REF in an UNSPEC
;; so that CSE won't optimize the address computation away.
(define_insn "movsi_lo_sum_pic"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (lo_sum:SI (match_operand:SI 1 "register_operand" "r")
                   (unspec:SI [(match_operand:SI 2 "immediate_operand" "in")] 0)))]
  "flag_pic"
  "or\\t%1, %%lo(%a2), %0"
  [(set_attr "type" "ialu")
   (set_attr "length" "1")])

(define_insn "movsi_high_pic"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (high:SI (unspec:SI [(match_operand 1 "" "")] 0)))]
  "flag_pic && check_pic (1)"
  "sethi\\t%%hi(%a1), %0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_expand "movsi_pic_label_ref"
  [(set (match_dup 3) (high:SI
     (unspec:SI [(match_operand:SI 1 "label_ref_operand" "")
		 (match_dup 2)] 5)))
   (set (match_dup 4) (lo_sum:SI (match_dup 3)
     (unspec:SI [(match_dup 1) (match_dup 2)] 5)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_dup 5) (match_dup 4)))]
  "flag_pic"
  "
{
  current_function_uses_pic_offset_table = 1;
  operands[2] = gen_rtx_SYMBOL_REF (Pmode, \"_GLOBAL_OFFSET_TABLE_\");
  operands[3] = gen_reg_rtx (SImode);
  operands[4] = gen_reg_rtx (SImode);
  operands[5] = pic_offset_table_rtx;
}")

(define_insn "*movsi_high_pic_label_ref"
  [(set (match_operand:SI 0 "register_operand" "=r")
      (high:SI
        (unspec:SI [(match_operand:SI 1 "label_ref_operand" "")
		    (match_operand:SI 2 "" "")] 5)))]
  "flag_pic"
  "sethi\\t%%hi(%a2-(%a1-.)), %0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn "*movsi_lo_sum_pic_label_ref"
  [(set (match_operand:SI 0 "register_operand" "=r")
      (lo_sum:SI (match_operand:SI 1 "register_operand" "r")
        (unspec:SI [(match_operand:SI 2 "label_ref_operand" "")
		    (match_operand:SI 3 "" "")] 5)))]
  "flag_pic"
  "or\\t%1, %%lo(%a3-(%a2-.)), %0"
  [(set_attr "type" "ialu")
   (set_attr "length" "1")])

(define_expand "movdi"
  [(set (match_operand:DI 0 "reg_or_nonsymb_mem_operand" "")
	(match_operand:DI 1 "general_operand" ""))]
  ""
  "
{
  /* Where possible, convert CONST_DOUBLE into a CONST_INT.  */
  if (GET_CODE (operands[1]) == CONST_DOUBLE
#if HOST_BITS_PER_WIDE_INT == 32
      && ((CONST_DOUBLE_HIGH (operands[1]) == 0
	   && (CONST_DOUBLE_LOW (operands[1]) & 0x80000000) == 0)
	  || (CONST_DOUBLE_HIGH (operands[1]) == (HOST_WIDE_INT) 0xffffffff
	      && (CONST_DOUBLE_LOW (operands[1]) & 0x80000000) != 0))
#endif
      )
    operands[1] = GEN_INT (CONST_DOUBLE_LOW (operands[1]));

  /* Handle MEM cases first.  */
  if (GET_CODE (operands[0]) == MEM)
    {
      /* If it's a REG, we can always do it.
	 The const zero case is more complex, on v9
	 we can always perform it.  */
      if (register_operand (operands[1], DImode)
	  || (TARGET_ARCH64
              && (operands[1] == const0_rtx)))
        goto movdi_is_ok;

      if (! reload_in_progress)
	{
	  operands[0] = validize_mem (operands[0]);
	  operands[1] = force_reg (DImode, operands[1]);
	}
    }

  if (flag_pic)
    {
      if (CONSTANT_P (operands[1])
	  && pic_address_needs_scratch (operands[1]))
	operands[1] = legitimize_pic_address (operands[1], DImode, 0);

      if (GET_CODE (operands[1]) == LABEL_REF)
        {
          if (! TARGET_ARCH64)
            abort ();
          emit_insn (gen_movdi_pic_label_ref (operands[0], operands[1]));
          DONE;
        }

      if (symbolic_operand (operands[1], DImode))
	{
	  operands[1] = legitimize_pic_address (operands[1],
						DImode,
						(reload_in_progress ?
						 operands[0] :
						 NULL_RTX));
	  goto movdi_is_ok;
	}
    }

  /* If we are trying to toss an integer constant into the
     FPU registers, force it into memory.  */
  if (GET_CODE (operands[0]) == REG
      && REGNO (operands[0]) >= SPARC_FIRST_FP_REG
      && REGNO (operands[0]) <= SPARC_LAST_V9_FP_REG
      && CONSTANT_P (operands[1]))
    operands[1] = validize_mem (force_const_mem (GET_MODE (operands[0]),
						 operands[1]));

  /* This makes sure we will not get rematched due to splittage. */
  if (! CONSTANT_P (operands[1]) || input_operand (operands[1], DImode))
    ;
  else if (TARGET_ARCH64
	   && CONSTANT_P (operands[1])
           && GET_CODE (operands[1]) != HIGH
           && GET_CODE (operands[1]) != LO_SUM)
    {
      sparc_emit_set_const64 (operands[0], operands[1]);
      DONE;
    }

 movdi_is_ok:
  ;
}")

;; Be careful, fmovd does not exist when !arch64.
;; We match MEM moves directly when we have correct even
;; numbered registers, but fall into splits otherwise.
;; The constraint ordering here is really important to
;; avoid insane problems in reload, especially for patterns
;; of the form:
;;
;; (set (mem:DI (plus:SI (reg:SI 30 %fp)
;;                       (const_int -5016)))
;;      (reg:DI 2 %g2))
;;
(define_insn "*movdi_insn_sp32"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=T,U,o,r,r,r,?T,?f,?f,?o,?f")
        (match_operand:DI 1 "input_operand"    "U,T,r,o,i,r,f,T,o,f,f"))]
  "! TARGET_ARCH64 &&
   (register_operand (operands[0], DImode)
    || register_operand (operands[1], DImode))"
  "@
   std\\t%1, %0
   ldd\\t%1, %0
   #
   #
   #
   #
   std\\t%1, %0
   ldd\\t%1, %0
   #
   #
   #"
  [(set_attr "type" "store,load,*,*,*,*,fpstore,fpload,*,*,*")
   (set_attr "length" "1,1,2,2,2,2,1,1,2,2,2")])

;; The following are generated by sparc_emit_set_const64
(define_insn "*movdi_sp64_dbl"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (match_operand:DI 1 "const64_operand" ""))]
  "(TARGET_ARCH64
    && HOST_BITS_PER_WIDE_INT != 64)"
  "mov\\t%1, %0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

;; This is needed to show CSE exactly which bits are set
;; in a 64-bit register by sethi instructions.
(define_insn "*movdi_const64_special"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(match_operand:DI 1 "const64_high_operand" ""))]
  "TARGET_ARCH64"
  "sethi\\t%%hi(%a1), %0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn "*movdi_insn_sp64"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=r,r,r,r,m,?e,?e,?m,b")
        (match_operand:DI 1 "input_operand"   "rI,K,J,m,rJ,e,m,e,J"))]
  "TARGET_ARCH64 &&
   (register_operand (operands[0], DImode)
    || reg_or_0_operand (operands[1], DImode))"
  "@
   mov\\t%1, %0
   sethi\\t%%hi(%a1), %0
   clr\\t%0
   ldx\\t%1, %0
   stx\\t%r1, %0
   fmovd\\t%1, %0
   ldd\\t%1, %0
   std\\t%1, %0
   fzero\\t%0"
  [(set_attr "type" "move,move,move,load,store,fpmove,fpload,fpstore,fpmove")
   (set_attr "length" "1")])

(define_expand "movdi_pic_label_ref"
  [(set (match_dup 3) (high:DI
     (unspec:DI [(match_operand:DI 1 "label_ref_operand" "")
                 (match_dup 2)] 5)))
   (set (match_dup 4) (lo_sum:DI (match_dup 3)
     (unspec:DI [(match_dup 1) (match_dup 2)] 5)))
   (set (match_operand:DI 0 "register_operand" "=r")
        (minus:DI (match_dup 5) (match_dup 4)))]
  "TARGET_ARCH64 && flag_pic"
  "
{
  current_function_uses_pic_offset_table = 1;
  operands[2] = gen_rtx_SYMBOL_REF (Pmode, \"_GLOBAL_OFFSET_TABLE_\");
  operands[3] = gen_reg_rtx (DImode);
  operands[4] = gen_reg_rtx (DImode);
  operands[5] = pic_offset_table_rtx;
}")

(define_insn "*movdi_high_pic_label_ref"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (high:DI
          (unspec:DI [(match_operand:DI 1 "label_ref_operand" "")
                      (match_operand:DI 2 "" "")] 5)))]
  "TARGET_ARCH64 && flag_pic"
  "sethi\\t%%hi(%a2-(%a1-.)), %0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn "*movdi_lo_sum_pic_label_ref"
  [(set (match_operand:DI 0 "register_operand" "=r")
      (lo_sum:DI (match_operand:DI 1 "register_operand" "r")
        (unspec:DI [(match_operand:DI 2 "label_ref_operand" "")
                    (match_operand:DI 3 "" "")] 5)))]
  "TARGET_ARCH64 && flag_pic"
  "or\\t%1, %%lo(%a3-(%a2-.)), %0"
  [(set_attr "type" "ialu")
   (set_attr "length" "1")])

;; Sparc-v9 code model support insns.  See sparc_emit_set_symbolic_const64
;; in sparc.c to see what is going on here... PIC stuff comes first.

(define_insn "movdi_lo_sum_pic"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (lo_sum:DI (match_operand:DI 1 "register_operand" "r")
                   (unspec:DI [(match_operand:DI 2 "immediate_operand" "in")] 0)))]
  "TARGET_ARCH64 && flag_pic"
  "or\\t%1, %%lo(%a2), %0"
  [(set_attr "type" "ialu")
   (set_attr "length" "1")])

(define_insn "movdi_high_pic"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (high:DI (unspec:DI [(match_operand 1 "" "")] 0)))]
  "TARGET_ARCH64 && flag_pic && check_pic (1)"
  "sethi\\t%%hi(%a1), %0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn "*sethi_di_medlow_embmedany_pic"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (high:DI (match_operand:DI 1 "sp64_medium_pic_operand" "")))]
  "(TARGET_CM_MEDLOW || TARGET_CM_EMBMEDANY) && check_pic (1)"
  "sethi\\t%%hi(%a1), %0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn "*sethi_di_medlow"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (high:DI (match_operand:DI 1 "symbolic_operand" "")))]
  "TARGET_CM_MEDLOW && check_pic (1)"
  "sethi\\t%%hi(%a1), %0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn "*losum_di_medlow"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (lo_sum:DI (match_operand:DI 1 "register_operand" "r")
                   (match_operand:DI 2 "symbolic_operand" "")))]
  "TARGET_CM_MEDLOW"
  "or\\t%1, %%lo(%a2), %0"
  [(set_attr "type" "ialu")
   (set_attr "length" "1")])

(define_insn "seth44"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (high:DI (unspec:DI [(match_operand:DI 1 "symbolic_operand" "")] 6)))]
  "TARGET_CM_MEDMID"
  "sethi\\t%%h44(%a1), %0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn "setm44"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (lo_sum:DI (match_operand:DI 1 "register_operand" "r")
                   (unspec:DI [(match_operand:DI 2 "symbolic_operand" "")] 7)))]
  "TARGET_CM_MEDMID"
  "or\\t%1, %%m44(%a2), %0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn "setl44"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (lo_sum:DI (match_operand:DI 1 "register_operand" "r")
                   (match_operand:DI 2 "symbolic_operand" "")))]
  "TARGET_CM_MEDMID"
  "or\\t%1, %%l44(%a2), %0"
  [(set_attr "type" "ialu")
   (set_attr "length" "1")])

(define_insn "sethh"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (high:DI (unspec:DI [(match_operand:DI 1 "symbolic_operand" "")] 9)))]
  "TARGET_CM_MEDANY"
  "sethi\\t%%hh(%a1), %0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn "setlm"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (high:DI (unspec:DI [(match_operand:DI 1 "symbolic_operand" "")] 10)))]
  "TARGET_CM_MEDANY"
  "sethi\\t%%lm(%a1), %0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn "sethm"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (lo_sum:DI (match_operand:DI 1 "register_operand" "r")
                   (unspec:DI [(match_operand:DI 2 "symbolic_operand" "")] 18)))]
  "TARGET_CM_MEDANY"
  "or\\t%1, %%hm(%a2), %0"
  [(set_attr "type" "ialu")
   (set_attr "length" "1")])

(define_insn "setlo"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (lo_sum:DI (match_operand:DI 1 "register_operand" "r")
                   (match_operand:DI 2 "symbolic_operand" "")))]
  "TARGET_CM_MEDANY"
  "or\\t%1, %%lo(%a2), %0"
  [(set_attr "type" "ialu")
   (set_attr "length" "1")])

(define_insn "embmedany_sethi"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (high:DI (unspec:DI [(match_operand:DI 1 "data_segment_operand" "")] 11)))]
  "TARGET_CM_EMBMEDANY && check_pic (1)"
  "sethi\\t%%hi(%a1), %0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn "embmedany_losum"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (lo_sum:DI (match_operand:DI 1 "register_operand" "r")
                   (match_operand:DI 2 "data_segment_operand" "")))]
  "TARGET_CM_EMBMEDANY"
  "add\\t%1, %%lo(%a2), %0"
  [(set_attr "type" "ialu")
   (set_attr "length" "1")])

(define_insn "embmedany_brsum"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (unspec:DI [(match_operand:DI 1 "register_operand" "r")] 11))]
  "TARGET_CM_EMBMEDANY"
  "add\\t%1, %_, %0"
  [(set_attr "length" "1")])

(define_insn "embmedany_textuhi"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (high:DI (unspec:DI [(match_operand:DI 1 "text_segment_operand" "")] 13)))]
  "TARGET_CM_EMBMEDANY && check_pic (1)"
  "sethi\\t%%uhi(%a1), %0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn "embmedany_texthi"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (high:DI (unspec:DI [(match_operand:DI 1 "text_segment_operand" "")] 14)))]
  "TARGET_CM_EMBMEDANY && check_pic (1)"
  "sethi\\t%%hi(%a1), %0"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn "embmedany_textulo"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (lo_sum:DI (match_operand:DI 1 "register_operand" "r")
                   (unspec:DI [(match_operand:DI 2 "text_segment_operand" "")] 15)))]
  "TARGET_CM_EMBMEDANY"
  "or\\t%1, %%ulo(%a2), %0"
  [(set_attr "type" "ialu")
   (set_attr "length" "1")])

(define_insn "embmedany_textlo"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (lo_sum:DI (match_operand:DI 1 "register_operand" "r")
                   (match_operand:DI 2 "text_segment_operand" "")))]
  "TARGET_CM_EMBMEDANY"
  "or\\t%1, %%lo(%a2), %0"
  [(set_attr "type" "ialu")
   (set_attr "length" "1")])

;; Now some patterns to help reload out a bit.
(define_expand "reload_indi"
  [(parallel [(match_operand:DI 0 "register_operand" "=r")
              (match_operand:DI 1 "immediate_operand" "")
              (match_operand:TI 2 "register_operand" "=&r")])]
  "(TARGET_CM_MEDANY
    || TARGET_CM_EMBMEDANY)
   && ! flag_pic"
  "
{
  sparc_emit_set_symbolic_const64 (operands[0], operands[1],
                                   gen_rtx_REG (DImode, REGNO (operands[2])));
  DONE;
}")

(define_expand "reload_outdi"
  [(parallel [(match_operand:DI 0 "register_operand" "=r")
              (match_operand:DI 1 "immediate_operand" "")
              (match_operand:TI 2 "register_operand" "=&r")])]
  "(TARGET_CM_MEDANY
    || TARGET_CM_EMBMEDANY)
   && ! flag_pic"
  "
{
  sparc_emit_set_symbolic_const64 (operands[0], operands[1],
                                   gen_rtx_REG (DImode, REGNO (operands[2])));
  DONE;
}")

;; Split up putting CONSTs and REGs into DI regs when !arch64
(define_split
  [(set (match_operand:DI 0 "register_operand" "")
        (match_operand:DI 1 "const_int_operand" ""))]
  "! TARGET_ARCH64 && reload_completed"
  [(clobber (const_int 0))]
  "
{
  emit_insn (gen_movsi (gen_highpart (SImode, operands[0]),
			(INTVAL (operands[1]) < 0) ?
			constm1_rtx :
			const0_rtx));
  emit_insn (gen_movsi (gen_lowpart (SImode, operands[0]),
			operands[1]));
  DONE;
}")

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
        (match_operand:DI 1 "const_double_operand" ""))]
  "! TARGET_ARCH64 && reload_completed"
  [(clobber (const_int 0))]
  "
{
  emit_insn (gen_movsi (gen_highpart (SImode, operands[0]),
			GEN_INT (CONST_DOUBLE_HIGH (operands[1]))));

  /* Slick... but this trick loses if this subreg constant part
     can be done in one insn.  */
  if (CONST_DOUBLE_LOW (operands[1]) == CONST_DOUBLE_HIGH (operands[1])
      && !(SPARC_SETHI_P (CONST_DOUBLE_HIGH (operands[1]))
	   || SPARC_SIMM13_P (CONST_DOUBLE_HIGH (operands[1]))))
    {
      emit_insn (gen_movsi (gen_lowpart (SImode, operands[0]),
			    gen_highpart (SImode, operands[0])));
    }
  else
    {
      emit_insn (gen_movsi (gen_lowpart (SImode, operands[0]),
			    GEN_INT (CONST_DOUBLE_LOW (operands[1]))));
    }
  DONE;
}")

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
        (match_operand:DI 1 "register_operand" ""))]
  "! TARGET_ARCH64 && reload_completed"
  [(clobber (const_int 0))]
  "
{
  rtx set_dest = operands[0];
  rtx set_src = operands[1];
  rtx dest1, dest2;
  rtx src1, src2;

  if (GET_CODE (set_dest) == SUBREG)
    set_dest = alter_subreg (set_dest);
  if (GET_CODE (set_src) == SUBREG)
    set_src = alter_subreg (set_src);

  dest1 = gen_highpart (SImode, set_dest);
  dest2 = gen_lowpart (SImode, set_dest);
  src1 = gen_highpart (SImode, set_src);
  src2 = gen_lowpart (SImode, set_src);

  /* Now emit using the real source and destination we found, swapping
     the order if we detect overlap.  */
  if (reg_overlap_mentioned_p (dest1, src2))
    {
      emit_insn (gen_movsi (dest2, src2));
      emit_insn (gen_movsi (dest1, src1));
    }
  else
    {
      emit_insn (gen_movsi (dest1, src1));
      emit_insn (gen_movsi (dest2, src2));
    }
  DONE;
}")

;; Now handle the cases of memory moves from/to non-even
;; DI mode register pairs.
(define_split
  [(set (match_operand:DI 0 "register_operand" "")
        (match_operand:DI 1 "memory_operand" ""))]
  "(! TARGET_ARCH64
    && reload_completed
    && sparc_splitdi_legitimate (operands[0], operands[1]))"
  [(clobber (const_int 0))]
  "
{
  rtx word0 = change_address (operands[1], SImode, NULL_RTX);
  rtx word1 = change_address (operands[1], SImode,
			      plus_constant_for_output (XEXP (word0, 0), 4));
  rtx high_part = gen_highpart (SImode, operands[0]);
  rtx low_part = gen_lowpart (SImode, operands[0]);

  if (reg_overlap_mentioned_p (high_part, word1))
    {
      emit_insn (gen_movsi (low_part, word1));
      emit_insn (gen_movsi (high_part, word0));
    }
  else
    {
      emit_insn (gen_movsi (high_part, word0));
      emit_insn (gen_movsi (low_part, word1));
    }
  DONE;
}")

(define_split
  [(set (match_operand:DI 0 "memory_operand" "")
        (match_operand:DI 1 "register_operand" ""))]
  "(! TARGET_ARCH64
    && reload_completed
    && sparc_splitdi_legitimate (operands[1], operands[0]))"
  [(clobber (const_int 0))]
  "
{
  rtx word0 = change_address (operands[0], SImode, NULL_RTX);
  rtx word1 = change_address (operands[0], SImode,
			      plus_constant_for_output (XEXP (word0, 0), 4));
  rtx high_part = gen_highpart (SImode, operands[1]);
  rtx low_part = gen_lowpart (SImode, operands[1]);

  emit_insn (gen_movsi (word0, high_part));
  emit_insn (gen_movsi (word1, low_part));
  DONE;
}")


;; Floating point move insns

(define_insn "*movsf_insn_novis_liveg0"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=f,*r,*r,*r,*r,*r,f,m,m")
	(match_operand:SF 1 "input_operand"         "f,G,Q,*rR,S,m,m,f,*r"))]
  "(TARGET_FPU && ! TARGET_VIS && TARGET_LIVE_G0)
   && (register_operand (operands[0], SFmode)
       || register_operand (operands[1], SFmode))"
  "*
{
  if (GET_CODE (operands[1]) == CONST_DOUBLE
      && (which_alternative == 2
          || which_alternative == 3
          || which_alternative == 4))
    {
      REAL_VALUE_TYPE r;
      long i;

      REAL_VALUE_FROM_CONST_DOUBLE (r, operands[1]);
      REAL_VALUE_TO_TARGET_SINGLE (r, i);
      operands[1] = GEN_INT (i);
    }

  switch (which_alternative)
    {
    case 0:
      return \"fmovs\\t%1, %0\";
    case 1:
      return \"and\\t%0, 0, %0\";
    case 2:
      return \"sethi\\t%%hi(%a1), %0\";
    case 3:
      return \"mov\\t%1, %0\";
    case 4:
      return \"#\";
    case 5:
    case 6:
      return \"ld\\t%1, %0\";
    case 7:
    case 8:
      return \"st\\t%1, %0\";
    }
}"
  [(set_attr "type" "fpmove,move,move,move,*,load,fpload,fpstore,store")
   (set_attr "length" "1")])

(define_insn "*movsf_insn_novis_noliveg0"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=f,*r,*r,*r,*r,*r,f,m,m")
	(match_operand:SF 1 "input_operand"         "f,G,Q,*rR,S,m,m,f,*rG"))]
  "(TARGET_FPU && ! TARGET_VIS && ! TARGET_LIVE_G0)
   && (register_operand (operands[0], SFmode)
       || register_operand (operands[1], SFmode)
       || fp_zero_operand (operands[1]))"
  "*
{
  if (GET_CODE (operands[1]) == CONST_DOUBLE
      && (which_alternative == 2
          || which_alternative == 3
          || which_alternative == 4))
    {
      REAL_VALUE_TYPE r;
      long i;

      REAL_VALUE_FROM_CONST_DOUBLE (r, operands[1]);
      REAL_VALUE_TO_TARGET_SINGLE (r, i);
      operands[1] = GEN_INT (i);
    }

  switch (which_alternative)
    {
    case 0:
      return \"fmovs\\t%1, %0\";
    case 1:
      return \"clr\\t%0\";
    case 2:
      return \"sethi\\t%%hi(%a1), %0\";
    case 3:
      return \"mov\\t%1, %0\";
    case 4:
      return \"#\";
    case 5:
    case 6:
      return \"ld\\t%1, %0\";
    case 7:
    case 8:
      return \"st\\t%r1, %0\";
    }
}"
  [(set_attr "type" "fpmove,move,move,move,*,load,fpload,fpstore,store")
   (set_attr "length" "1")])

(define_insn "*movsf_insn_vis"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=f,f,*r,*r,*r,*r,*r,f,m,m")
	(match_operand:SF 1 "input_operand"         "f,G,G,Q,*rR,S,m,m,f,*rG"))]
  "(TARGET_FPU && TARGET_VIS)
   && (register_operand (operands[0], SFmode)
       || register_operand (operands[1], SFmode)
       || fp_zero_operand (operands[1]))"
  "*
{
  if (GET_CODE (operands[1]) == CONST_DOUBLE
      && (which_alternative == 3
          || which_alternative == 4
          || which_alternative == 5))
    {
      REAL_VALUE_TYPE r;
      long i;

      REAL_VALUE_FROM_CONST_DOUBLE (r, operands[1]);
      REAL_VALUE_TO_TARGET_SINGLE (r, i);
      operands[1] = GEN_INT (i);
    }

  switch (which_alternative)
    {
    case 0:
      return \"fmovs\\t%1, %0\";
    case 1:
      return \"fzeros\\t%0\";
    case 2:
      return \"clr\\t%0\";
    case 3:
      return \"sethi\\t%%hi(%a1), %0\";
    case 4:
      return \"mov\\t%1, %0\";
    case 5:
      return \"#\";
    case 6:
    case 7:
      return \"ld\\t%1, %0\";
    case 8:
    case 9:
      return \"st\\t%r1, %0\";
    }
}"
  [(set_attr "type" "fpmove,fpmove,move,move,move,*,load,fpload,fpstore,store")
   (set_attr "length" "1")])

(define_insn "*movsf_lo_sum"
  [(set (match_operand:SF 0 "register_operand" "")
        (lo_sum:SF (match_operand:SF 1 "register_operand" "")
                   (match_operand:SF 2 "const_double_operand" "")))]
  "TARGET_FPU && fp_high_losum_p (operands[2])"
  "*
{
  REAL_VALUE_TYPE r;
  long i;

  REAL_VALUE_FROM_CONST_DOUBLE (r, operands[2]);
  REAL_VALUE_TO_TARGET_SINGLE (r, i);
  operands[2] = GEN_INT (i);
  return \"or\\t%1, %%lo(%a2), %0\";
}"
  [(set_attr "type" "ialu")
   (set_attr "length" "1")])

(define_insn "*movsf_high"
  [(set (match_operand:SF 0 "register_operand" "")
        (high:SF (match_operand:SF 1 "const_double_operand" "")))]
  "TARGET_FPU && fp_high_losum_p (operands[1])"
  "*
{
  REAL_VALUE_TYPE r;
  long i;

  REAL_VALUE_FROM_CONST_DOUBLE (r, operands[1]);
  REAL_VALUE_TO_TARGET_SINGLE (r, i);
  operands[1] = GEN_INT (i);
  return \"sethi\\t%%hi(%1), %0\";
}"
  [(set_attr "type" "move")
   (set_attr "length" "1")])

(define_split
  [(set (match_operand:SF 0 "register_operand" "")
        (match_operand:SF 1 "const_double_operand" ""))]
  "TARGET_FPU
   && fp_high_losum_p (operands[1])
   && (GET_CODE (operands[0]) == REG
       && REGNO (operands[0]) < 32)"
  [(set (match_dup 0) (high:SF (match_dup 1)))
   (set (match_dup 0) (lo_sum:SF (match_dup 0) (match_dup 1)))])

;; Exactly the same as above, except that all `f' cases are deleted.
;; This is necessary to prevent reload from ever trying to use a `f' reg
;; when -mno-fpu.

(define_insn "*movsf_no_f_insn"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=r,r,m")
	(match_operand:SF 1 "input_operand"    "r,m,r"))]
  "! TARGET_FPU
   && (register_operand (operands[0], SFmode)
       || register_operand (operands[1], SFmode))"
  "@
   mov\\t%1, %0
   ld\\t%1, %0
   st\\t%1, %0"
  [(set_attr "type" "move,load,store")
   (set_attr "length" "1")])

(define_expand "movsf"
  [(set (match_operand:SF 0 "general_operand" "")
	(match_operand:SF 1 "general_operand" ""))]
  ""
  "
{
  /* Force SFmode constants into memory.  */
  if (GET_CODE (operands[0]) == REG
      && CONSTANT_P (operands[1]))
    {
      if (TARGET_VIS
          && GET_CODE (operands[1]) == CONST_DOUBLE
	  && fp_zero_operand (operands[1]))
	goto movsf_is_ok;

      /* emit_group_store will send such bogosity to us when it is
         not storing directly into memory.  So fix this up to avoid
         crashes in output_constant_pool.  */
      if (operands [1] == const0_rtx)
        operands[1] = CONST0_RTX (SFmode);
      operands[1] = validize_mem (force_const_mem (GET_MODE (operands[0]),
                                                   operands[1]));
    }

  /* Handle sets of MEM first.  */
  if (GET_CODE (operands[0]) == MEM)
    {
      if (register_operand (operands[1], SFmode)
	  || (! TARGET_LIVE_G0
	      && GET_CODE (operands[1]) == CONST_DOUBLE
              && fp_zero_operand (operands[1])))
	goto movsf_is_ok;

      if (! reload_in_progress)
	{
	  operands[0] = validize_mem (operands[0]);
	  operands[1] = force_reg (SFmode, operands[1]);
	}
    }

  /* Fixup PIC cases.  */
  if (flag_pic)
    {
      if (CONSTANT_P (operands[1])
	  && pic_address_needs_scratch (operands[1]))
	operands[1] = legitimize_pic_address (operands[1], SFmode, 0);

      if (symbolic_operand (operands[1], SFmode))
	{
	  operands[1] = legitimize_pic_address (operands[1],
						SFmode,
						(reload_in_progress ?
						 operands[0] :
						 NULL_RTX));
	}
    }

 movsf_is_ok:
  ;
}")

(define_insn "*clear_df"
  [(set (match_operand:DF 0 "register_operand" "=e")
        (match_operand:DF 1 "const_double_operand" ""))]
  "TARGET_VIS
   && fp_zero_operand (operands[1])"
  "fzero\\t%0"
  [(set_attr "type" "fpmove")
   (set_attr "length" "1")])

(define_insn "*clear_dfp"
  [(set (match_operand:DF 0 "memory_operand" "=m")
        (match_operand:DF 1 "const_double_operand" ""))]
  "! TARGET_LIVE_G0
   && TARGET_V9
   && fp_zero_operand (operands[1])"
  "stx\\t%%g0, %0"
  [(set_attr "type" "store")
   (set_attr "length" "1")])

(define_insn "*movdf_const_intreg_sp32"
  [(set (match_operand:DF 0 "register_operand" "=e,e,?r")
        (match_operand:DF 1 "const_double_operand" "T#F,o#F,F"))]
  "TARGET_FPU && ! TARGET_ARCH64"
  "@
   ldd\\t%1, %0
   #
   #"
  [(set_attr "type" "move")
   (set_attr "length" "1,2,2")])

;; Now that we redo life analysis with a clean slate after
;; instruction splitting for sched2 this can work.
(define_insn "*movdf_const_intreg_sp64"
  [(set (match_operand:DF 0 "register_operand" "=e,?r")
        (match_operand:DF 1 "const_double_operand" "m#F,F"))]
  "TARGET_FPU && TARGET_ARCH64"
  "@
   ldd\\t%1, %0
   #"
  [(set_attr "type" "move")
   (set_attr "length" "1,2")])

(define_split
  [(set (match_operand:DF 0 "register_operand" "")
        (match_operand:DF 1 "const_double_operand" ""))]
  "TARGET_FPU
   && (GET_CODE (operands[0]) == REG
       && REGNO (operands[0]) < 32)
   && reload_completed"
  [(clobber (const_int 0))]
  "
{
  REAL_VALUE_TYPE r;
  long l[2];

  REAL_VALUE_FROM_CONST_DOUBLE (r, operands[1]);
  REAL_VALUE_TO_TARGET_DOUBLE (r, l);
  if (GET_CODE (operands[0]) == SUBREG)
    operands[0] = alter_subreg (operands[0]);
  operands[0] = gen_rtx_raw_REG (DImode, REGNO (operands[0]));

  if (TARGET_ARCH64)
    {
#if HOST_BITS_PER_WIDE_INT == 64
      HOST_WIDE_INT val;

      val = ((HOST_WIDE_INT)(unsigned long)l[1] |
             ((HOST_WIDE_INT)(unsigned long)l[0] << 32));
      emit_insn (gen_movdi (operands[0], GEN_INT (val)));
#else
      emit_insn (gen_movdi (operands[0],
                            gen_rtx_CONST_DOUBLE (VOIDmode, const0_rtx,
                                                  l[1], l[0])));
#endif
    }
  else
    {
      emit_insn (gen_movsi (gen_highpart (SImode, operands[0]),
			    GEN_INT (l[0])));

      /* Slick... but this trick loses if this subreg constant part
         can be done in one insn.  */
      if (l[1] == l[0]
          && !(SPARC_SETHI_P (l[0])
	       || SPARC_SIMM13_P (l[0])))
        {
          emit_insn (gen_movsi (gen_lowpart (SImode, operands[0]),
			        gen_highpart (SImode, operands[0])));
        }
      else
        {
          emit_insn (gen_movsi (gen_lowpart (SImode, operands[0]),
			        GEN_INT (l[1])));
        }
    }
  DONE;
}")

(define_expand "movdf"
  [(set (match_operand:DF 0 "general_operand" "")
	(match_operand:DF 1 "general_operand" ""))]
  ""
  "
{
  /* Force DFmode constants into memory.  */
  if (GET_CODE (operands[0]) == REG
      && CONSTANT_P (operands[1]))
    {
      if (TARGET_VIS
          && GET_CODE (operands[1]) == CONST_DOUBLE
	  && fp_zero_operand (operands[1]))
	goto movdf_is_ok;

      /* emit_group_store will send such bogosity to us when it is
         not storing directly into memory.  So fix this up to avoid
         crashes in output_constant_pool.  */
      if (operands [1] == const0_rtx)
        operands[1] = CONST0_RTX (DFmode);
      operands[1] = validize_mem (force_const_mem (GET_MODE (operands[0]),
                                                   operands[1]));
    }

  /* Handle MEM cases first.  */
  if (GET_CODE (operands[0]) == MEM)
    {
      if (register_operand (operands[1], DFmode))
	goto movdf_is_ok;

      if (! reload_in_progress)
	{
	  operands[0] = validize_mem (operands[0]);
	  operands[1] = force_reg (DFmode, operands[1]);
	}
    }

  /* Fixup PIC cases.  */
  if (flag_pic)
    {
      if (CONSTANT_P (operands[1])
	  && pic_address_needs_scratch (operands[1]))
	operands[1] = legitimize_pic_address (operands[1], DFmode, 0);

      if (symbolic_operand (operands[1], DFmode))
	{
	  operands[1] = legitimize_pic_address (operands[1],
						DFmode,
						(reload_in_progress ?
						 operands[0] :
						 NULL_RTX));
	}
    }

 movdf_is_ok:
  ;
}")

;; Be careful, fmovd does not exist when !v9.
(define_insn "*movdf_insn_sp32"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=e,T,U,T,e,r,r,o,e,o")
	(match_operand:DF 1 "input_operand"    "T,e,T,U,e,r,o,r,o,e"))]
  "TARGET_FPU
   && ! TARGET_V9
   && (register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode))"
  "@
  ldd\\t%1, %0
  std\\t%1, %0
  ldd\\t%1, %0
  std\\t%1, %0
  #
  #
  #
  #
  #
  #"
 [(set_attr "type" "fpload,fpstore,load,store,*,*,*,*,*,*")
  (set_attr "length" "1,1,1,1,2,2,2,2,2,2")])

(define_insn "*movdf_no_e_insn_sp32"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=U,T,r,r,o")
	(match_operand:DF 1 "input_operand"    "T,U,r,o,r"))]
  "! TARGET_FPU
   && ! TARGET_ARCH64
   && (register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode))"
  "@
  ldd\\t%1, %0
  std\\t%1, %0
  #
  #
  #"
  [(set_attr "type" "load,store,*,*,*")
   (set_attr "length" "1,1,2,2,2")])

;; We have available v9 double floats but not 64-bit
;; integer registers.
(define_insn "*movdf_insn_v9only"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=e,e,m,U,T,r,r,o")
        (match_operand:DF 1 "input_operand"    "e,m,e,T,U,r,o,r"))]
  "TARGET_FPU
   && TARGET_V9
   && ! TARGET_ARCH64
   && (register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode))"
  "@
  fmovd\\t%1, %0
  ldd\\t%1, %0
  std\\t%1, %0
  ldd\\t%1, %0
  std\\t%1, %0
  #
  #
  #"
  [(set_attr "type" "fpmove,load,store,load,store,*,*,*")
   (set_attr "length" "1,1,1,1,1,2,2,2")])

;; We have available both v9 double floats and 64-bit
;; integer registers.
(define_insn "*movdf_insn_sp64"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=e,e,m,r,r,m")
        (match_operand:DF 1 "input_operand"    "e,m,e,r,m,r"))]
  "TARGET_FPU
   && TARGET_V9
   && TARGET_ARCH64
   && (register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode))"
  "@
  fmovd\\t%1, %0
  ldd\\t%1, %0
  std\\t%1, %0
  mov\\t%1, %0
  ldx\\t%1, %0
  stx\\t%1, %0"
  [(set_attr "type" "fpmove,load,store,move,load,store")
   (set_attr "length" "1")])

(define_insn "*movdf_no_e_insn_sp64"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=r,r,m")
        (match_operand:DF 1 "input_operand"    "r,m,r"))]
  "! TARGET_FPU
   && TARGET_ARCH64
   && (register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode))"
  "@
  mov\\t%1, %0
  ldx\\t%1, %0
  stx\\t%1, %0"
  [(set_attr "type" "move,load,store")
   (set_attr "length" "1")])

;; Ok, now the splits to handle all the multi insn and
;; mis-aligned memory address cases.
;; In these splits please take note that we must be
;; careful when V9 but not ARCH64 because the integer
;; register DFmode cases must be handled.
(define_split
  [(set (match_operand:DF 0 "register_operand" "")
        (match_operand:DF 1 "register_operand" ""))]
  "(! TARGET_V9
    || (! TARGET_ARCH64
        && ((GET_CODE (operands[0]) == REG
             && REGNO (operands[0]) < 32)
            || (GET_CODE (operands[0]) == SUBREG
                && GET_CODE (SUBREG_REG (operands[0])) == REG
                && REGNO (SUBREG_REG (operands[0])) < 32))))
   && reload_completed"
  [(clobber (const_int 0))]
  "
{
  rtx set_dest = operands[0];
  rtx set_src = operands[1];
  rtx dest1, dest2;
  rtx src1, src2;

  if (GET_CODE (set_dest) == SUBREG)
    set_dest = alter_subreg (set_dest);
  if (GET_CODE (set_src) == SUBREG)
    set_src = alter_subreg (set_src);

  dest1 = gen_highpart (SFmode, set_dest);
  dest2 = gen_lowpart (SFmode, set_dest);
  src1 = gen_highpart (SFmode, set_src);
  src2 = gen_lowpart (SFmode, set_src);

  /* Now emit using the real source and destination we found, swapping
     the order if we detect overlap.  */
  if (reg_overlap_mentioned_p (dest1, src2))
    {
      emit_insn (gen_movsf (dest2, src2));
      emit_insn (gen_movsf (dest1, src1));
    }
  else
    {
      emit_insn (gen_movsf (dest1, src1));
      emit_insn (gen_movsf (dest2, src2));
    }
  DONE;
}")

(define_split
  [(set (match_operand:DF 0 "register_operand" "")
	(match_operand:DF 1 "memory_operand" ""))]
  "((! TARGET_V9
     || (! TARGET_ARCH64
         && ((GET_CODE (operands[0]) == REG
              && REGNO (operands[0]) < 32)
             || (GET_CODE (operands[0]) == SUBREG
                 && GET_CODE (SUBREG_REG (operands[0])) == REG
                 && REGNO (SUBREG_REG (operands[0])) < 32))))
    && (reload_completed
        && (((REGNO (operands[0])) % 2) != 0
             || ! mem_min_alignment (operands[1], 8))
        && offsettable_memref_p (operands[1])))"
  [(clobber (const_int 0))]
  "
{
  rtx word0 = change_address (operands[1], SFmode, NULL_RTX);
  rtx word1 = change_address (operands[1], SFmode,
			      plus_constant_for_output (XEXP (word0, 0), 4));

  if (GET_CODE (operands[0]) == SUBREG)
    operands[0] = alter_subreg (operands[0]);

  if (reg_overlap_mentioned_p (gen_highpart (SFmode, operands[0]), word1))
    {
      emit_insn (gen_movsf (gen_lowpart (SFmode, operands[0]),
			    word1));
      emit_insn (gen_movsf (gen_highpart (SFmode, operands[0]),
			    word0));
    }
  else
    {
      emit_insn (gen_movsf (gen_highpart (SFmode, operands[0]),
			    word0));
      emit_insn (gen_movsf (gen_lowpart (SFmode, operands[0]),
			    word1));
    }
  DONE;
}")

(define_split
  [(set (match_operand:DF 0 "memory_operand" "")
	(match_operand:DF 1 "register_operand" ""))]
  "((! TARGET_V9
     || (! TARGET_ARCH64
         && ((GET_CODE (operands[1]) == REG
              && REGNO (operands[1]) < 32)
             || (GET_CODE (operands[1]) == SUBREG
                 && GET_CODE (SUBREG_REG (operands[1])) == REG
                 && REGNO (SUBREG_REG (operands[1])) < 32))))
    && (reload_completed
        && (((REGNO (operands[1])) % 2) != 0
             || ! mem_min_alignment (operands[0], 8))
        && offsettable_memref_p (operands[0])))"
  [(clobber (const_int 0))]
  "
{
  rtx word0 = change_address (operands[0], SFmode, NULL_RTX);
  rtx word1 = change_address (operands[0], SFmode,
			      plus_constant_for_output (XEXP (word0, 0), 4));

  if (GET_CODE (operands[1]) == SUBREG)
    operands[1] = alter_subreg (operands[1]);
  emit_insn (gen_movsf (word0,
			gen_highpart (SFmode, operands[1])));
  emit_insn (gen_movsf (word1,
			gen_lowpart (SFmode, operands[1])));
  DONE;
}")

(define_insn "*clear_tf"
  [(set (match_operand:TF 0 "register_operand" "=e")
        (match_operand:TF 1 "const_double_operand" ""))]
  "TARGET_VIS
   && fp_zero_operand (operands[1])"
  "#"
  [(set_attr "type" "fpmove")
   (set_attr "length" "2")])

(define_split
  [(set (match_operand:TF 0 "register_operand" "")
        (match_operand:TF 1 "const_double_operand" ""))]
  "TARGET_VIS && reload_completed
   && fp_zero_operand (operands[1])"
  [(set (subreg:DF (match_dup 0) 0) (match_dup 1))
   (set (subreg:DF (match_dup 0) 8) (match_dup 1))]
  "
{
  operands[1] = CONST0_RTX (DFmode);
}
")

(define_insn "*clear_tfp"
  [(set (match_operand:TF 0 "memory_operand" "=m")
        (match_operand:TF 1 "const_double_operand" ""))]
  "! TARGET_LIVE_G0
   && TARGET_V9
   && fp_zero_operand (operands[1])"
  "#"
  [(set_attr "type" "fpmove")
   (set_attr "length" "2")])

(define_split
  [(set (match_operand:TF 0 "memory_operand" "=m")
        (match_operand:TF 1 "const_double_operand" ""))]
  "! TARGET_LIVE_G0
   && TARGET_V9 && reload_completed
   && fp_zero_operand (operands[1])"
  [(set (subreg:DF (match_dup 0) 0) (match_dup 1))
   (set (subreg:DF (match_dup 0) 8) (match_dup 1))]
  "
{
  operands[1] = CONST0_RTX (DFmode);
}
")

(define_expand "movtf"
  [(set (match_operand:TF 0 "general_operand" "")
	(match_operand:TF 1 "general_operand" ""))]
  ""
  "
{
  /* Force TFmode constants into memory. */
  if (GET_CODE (operands[0]) == REG
      && CONSTANT_P (operands[1]))
    {
      if (TARGET_VIS
          && GET_CODE (operands[1]) == CONST_DOUBLE
	  && fp_zero_operand (operands[1]))
	goto movtf_is_ok;

      /* emit_group_store will send such bogosity to us when it is
         not storing directly into memory.  So fix this up to avoid
         crashes in output_constant_pool.  */
      if (operands [1] == const0_rtx)
        operands[1] = CONST0_RTX (TFmode);
      operands[1] = validize_mem (force_const_mem (GET_MODE (operands[0]),
                                                   operands[1]));
    }

  /* Handle MEM cases first, note that only v9 guarentees
     full 16-byte alignment for quads. */
  if (GET_CODE (operands[0]) == MEM)
    {
      if (register_operand (operands[1], TFmode))
        goto movtf_is_ok;

      if (! reload_in_progress)
	{
	  operands[0] = validize_mem (operands[0]);
	  operands[1] = force_reg (TFmode, operands[1]);
	}
    }

  /* Fixup PIC cases.  */
  if (flag_pic)
    {
      if (CONSTANT_P (operands[1])
	  && pic_address_needs_scratch (operands[1]))
	operands[1] = legitimize_pic_address (operands[1], TFmode, 0);

      if (symbolic_operand (operands[1], TFmode))
	{
	  operands[1] = legitimize_pic_address (operands[1],
						TFmode,
						(reload_in_progress ?
						 operands[0] :
						 NULL_RTX));
	}
    }

 movtf_is_ok:
  ;
}")

;; Be careful, fmovq and {st,ld}{x,q} do not exist when !arch64 so
;; we must split them all.  :-(
(define_insn "*movtf_insn_sp32"
  [(set (match_operand:TF 0 "nonimmediate_operand" "=e,o,U,o,e,r,r,o")
	(match_operand:TF 1 "input_operand"    "o,e,o,U,e,r,o,r"))]
  "TARGET_FPU
   && ! TARGET_ARCH64
   && (register_operand (operands[0], TFmode)
       || register_operand (operands[1], TFmode))"
  "#"
  [(set_attr "length" "4")])

;; Exactly the same as above, except that all `e' cases are deleted.
;; This is necessary to prevent reload from ever trying to use a `e' reg
;; when -mno-fpu.

(define_insn "*movtf_no_e_insn_sp32"
  [(set (match_operand:TF 0 "nonimmediate_operand" "=U,o,r,r,o")
	(match_operand:TF 1 "input_operand"    "o,U,r,o,r"))]
  "! TARGET_FPU
   && ! TARGET_ARCH64
   && (register_operand (operands[0], TFmode)
       || register_operand (operands[1], TFmode))"
  "#"
  [(set_attr "length" "4")])

;; Now handle the float reg cases directly when arch64,
;; hard_quad, and proper reg number alignment are all true.
(define_insn "*movtf_insn_hq_sp64"
  [(set (match_operand:TF 0 "nonimmediate_operand" "=e,e,m,r,r,o")
        (match_operand:TF 1 "input_operand"    "e,m,e,r,o,r"))]
  "TARGET_FPU
   && TARGET_ARCH64
   && TARGET_V9
   && TARGET_HARD_QUAD
   && (register_operand (operands[0], TFmode)
       || register_operand (operands[1], TFmode))"
  "@
  fmovq\\t%1, %0
  ldq\\t%1, %0
  stq\\t%1, %0
  #
  #
  #"
  [(set_attr "type" "fpmove,fpload,fpstore,*,*,*")
   (set_attr "length" "1,1,1,2,2,2")])

;; Now we allow the integer register cases even when
;; only arch64 is true.
(define_insn "*movtf_insn_sp64"
  [(set (match_operand:TF 0 "nonimmediate_operand" "=e,o,r,o,e,r")
        (match_operand:TF 1 "input_operand"    "o,e,o,r,e,r"))]
  "TARGET_FPU
   && TARGET_ARCH64
   && ! TARGET_HARD_QUAD
   && (register_operand (operands[0], TFmode)
       || register_operand (operands[1], TFmode))"
  "#"
  [(set_attr "length" "2")])

(define_insn "*movtf_no_e_insn_sp64"
  [(set (match_operand:TF 0 "nonimmediate_operand" "=r,o,r")
        (match_operand:TF 1 "input_operand"    "o,r,r"))]
  "! TARGET_FPU
   && TARGET_ARCH64
   && (register_operand (operands[0], TFmode)
       || register_operand (operands[1], TFmode))"
  "#"
  [(set_attr "length" "2")])

;; Now all the splits to handle multi-insn TF mode moves.
(define_split
  [(set (match_operand:TF 0 "register_operand" "")
        (match_operand:TF 1 "register_operand" ""))]
  "reload_completed
   && (! TARGET_ARCH64
       || (TARGET_FPU
           && ! TARGET_HARD_QUAD))"
  [(clobber (const_int 0))]
  "
{
  rtx set_dest = operands[0];
  rtx set_src = operands[1];
  rtx dest1, dest2;
  rtx src1, src2;

  if (GET_CODE (set_dest) == SUBREG)
    set_dest = alter_subreg (set_dest);
  if (GET_CODE (set_src) == SUBREG)
    set_src = alter_subreg (set_src);

  dest1 = gen_rtx_REG (DFmode,
                       REGNO (set_dest) + (WORDS_BIG_ENDIAN ? 0 : 2));
  dest2 = gen_rtx_REG (DFmode,
                       REGNO (set_dest) + (WORDS_BIG_ENDIAN ? 2 : 0));
  src1 = gen_rtx_REG (DFmode,
                      REGNO (set_src) + (WORDS_BIG_ENDIAN ? 0 : 2));
  src2 = gen_rtx_REG (DFmode,
                      REGNO (set_src) + (WORDS_BIG_ENDIAN ? 2 : 0));

  /* Now emit using the real source and destination we found, swapping
     the order if we detect overlap.  */
  if (reg_overlap_mentioned_p (dest1, src2))
    {
      emit_insn (gen_movdf (dest2, src2));
      emit_insn (gen_movdf (dest1, src1));
    }
  else
    {
      emit_insn (gen_movdf (dest1, src1));
      emit_insn (gen_movdf (dest2, src2));
    }
  DONE;
}")

(define_split
  [(set (match_operand:TF 0 "register_operand" "")
        (match_operand:TF 1 "memory_operand" ""))]
  "(reload_completed
    && offsettable_memref_p (operands[1]))"
  [(clobber (const_int 0))]
  "
{
  rtx word0 = change_address (operands[1], DFmode, NULL_RTX);
  rtx word1 = change_address (operands[1], DFmode,
			      plus_constant_for_output (XEXP (word0, 0), 8));
  rtx set_dest, dest1, dest2;

  set_dest = operands[0];
  if (GET_CODE (set_dest) == SUBREG)
    set_dest = alter_subreg (set_dest);

  dest1 = gen_rtx_REG (DFmode,
                       REGNO (set_dest) + (WORDS_BIG_ENDIAN ? 0 : 2));
  dest2 = gen_rtx_REG (DFmode,
                       REGNO (set_dest) + (WORDS_BIG_ENDIAN ? 2 : 0));

  /* Now output, ordering such that we don't clobber any registers
     mentioned in the address.  */
  if (reg_overlap_mentioned_p (dest1, word1))

    {
      emit_insn (gen_movdf (dest2, word1));
      emit_insn (gen_movdf (dest1, word0));
    }
  else
   {
      emit_insn (gen_movdf (dest1, word0));
      emit_insn (gen_movdf (dest2, word1));
   }
  DONE;
}")

(define_split
  [(set (match_operand:TF 0 "memory_operand" "")
	(match_operand:TF 1 "register_operand" ""))]
  "(reload_completed
    && offsettable_memref_p (operands[0]))"
  [(clobber (const_int 0))]
  "
{
  rtx word1 = change_address (operands[0], DFmode, NULL_RTX);
  rtx word2 = change_address (operands[0], DFmode,
			      plus_constant_for_output (XEXP (word1, 0), 8));
  rtx set_src, src1, src2;

  set_src = operands[1];
  if (GET_CODE (set_src) == SUBREG)
    set_src = alter_subreg (set_src);

  src1 = gen_rtx_REG (DFmode,
                      REGNO (set_src) + (WORDS_BIG_ENDIAN ? 0 : 2));
  src2 = gen_rtx_REG (DFmode,
                      REGNO (set_src) + (WORDS_BIG_ENDIAN ? 2 : 0));
  emit_insn (gen_movdf (word1, src1));
  emit_insn (gen_movdf (word2, src2));
  DONE;
}")

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
      && (TARGET_ARCH64 && op0_mode == DImode && v9_regcmp_p (code)))
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
   mov%C1\\t%x2, %3, %0
   mov%c1\\t%x2, %4, %0"
  [(set_attr "type" "cmove")
   (set_attr "length" "1")])

(define_insn "*movhi_cc_sp64"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(if_then_else:HI (match_operator 1 "comparison_operator"
				[(match_operand 2 "icc_or_fcc_reg_operand" "X,X")
				 (const_int 0)])
                         (match_operand:HI 3 "arith11_operand" "rL,0")
                         (match_operand:HI 4 "arith11_operand" "0,rL")))]
  "TARGET_V9"
  "@
   mov%C1\\t%x2, %3, %0
   mov%c1\\t%x2, %4, %0"
  [(set_attr "type" "cmove")
   (set_attr "length" "1")])

(define_insn "*movsi_cc_sp64"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(if_then_else:SI (match_operator 1 "comparison_operator"
				[(match_operand 2 "icc_or_fcc_reg_operand" "X,X")
				 (const_int 0)])
                         (match_operand:SI 3 "arith11_operand" "rL,0")
                         (match_operand:SI 4 "arith11_operand" "0,rL")))]
  "TARGET_V9"
  "@
   mov%C1\\t%x2, %3, %0
   mov%c1\\t%x2, %4, %0"
  [(set_attr "type" "cmove")
   (set_attr "length" "1")])

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
   mov%C1\\t%x2, %3, %0
   mov%c1\\t%x2, %4, %0"
  [(set_attr "type" "cmove")
   (set_attr "length" "1")])

(define_insn "*movdi_cc_sp64_trunc"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(if_then_else:SI (match_operator 1 "comparison_operator"
				[(match_operand 2 "icc_or_fcc_reg_operand" "X,X")
				 (const_int 0)])
                         (match_operand:SI 3 "arith11_double_operand" "rLH,0")
                         (match_operand:SI 4 "arith11_double_operand" "0,rLH")))]
  "TARGET_ARCH64"
  "@
   mov%C1\\t%x2, %3, %0
   mov%c1\\t%x2, %4, %0"
  [(set_attr "type" "cmove")
   (set_attr "length" "1")])

(define_insn "*movsf_cc_sp64"
  [(set (match_operand:SF 0 "register_operand" "=f,f")
	(if_then_else:SF (match_operator 1 "comparison_operator"
				[(match_operand 2 "icc_or_fcc_reg_operand" "X,X")
				 (const_int 0)])
                         (match_operand:SF 3 "register_operand" "f,0")
                         (match_operand:SF 4 "register_operand" "0,f")))]
  "TARGET_V9 && TARGET_FPU"
  "@
   fmovs%C1\\t%x2, %3, %0
   fmovs%c1\\t%x2, %4, %0"
  [(set_attr "type" "fpcmove")
   (set_attr "length" "1")])

(define_insn "movdf_cc_sp64"
  [(set (match_operand:DF 0 "register_operand" "=e,e")
	(if_then_else:DF (match_operator 1 "comparison_operator"
				[(match_operand 2 "icc_or_fcc_reg_operand" "X,X")
				 (const_int 0)])
                         (match_operand:DF 3 "register_operand" "e,0")
                         (match_operand:DF 4 "register_operand" "0,e")))]
  "TARGET_V9 && TARGET_FPU"
  "@
   fmovd%C1\\t%x2, %3, %0
   fmovd%c1\\t%x2, %4, %0"
  [(set_attr "type" "fpcmove")
   (set_attr "length" "1")])

(define_insn "*movtf_cc_hq_sp64"
  [(set (match_operand:TF 0 "register_operand" "=e,e")
	(if_then_else:TF (match_operator 1 "comparison_operator"
				[(match_operand 2 "icc_or_fcc_reg_operand" "X,X")
				 (const_int 0)])
                         (match_operand:TF 3 "register_operand" "e,0")
                         (match_operand:TF 4 "register_operand" "0,e")))]
  "TARGET_V9 && TARGET_FPU && TARGET_HARD_QUAD"
  "@
   fmovq%C1\\t%x2, %3, %0
   fmovq%c1\\t%x2, %4, %0"
  [(set_attr "type" "fpcmove")
   (set_attr "length" "1")])

(define_insn "*movtf_cc_sp64"
  [(set (match_operand:TF 0 "register_operand" "=e,e")
	(if_then_else:TF (match_operator 1 "comparison_operator"
				[(match_operand 2 "icc_or_fcc_reg_operand" "X,X")
				 (const_int 0)])
                         (match_operand:TF 3 "register_operand" "e,0")
                         (match_operand:TF 4 "register_operand" "0,e")))]
  "TARGET_V9 && TARGET_FPU && !TARGET_HARD_QUAD"
  "#"
  [(set_attr "type" "fpcmove")
   (set_attr "length" "2")])

(define_split
  [(set (match_operand:TF 0 "register_operand" "=e,e")
	(if_then_else:TF (match_operator 1 "comparison_operator"
				[(match_operand 2 "icc_or_fcc_reg_operand" "X,X")
				 (const_int 0)])
                         (match_operand:TF 3 "register_operand" "e,0")
                         (match_operand:TF 4 "register_operand" "0,e")))]
  "reload_completed && TARGET_V9 && TARGET_FPU && !TARGET_HARD_QUAD"
  [(clobber (const_int 0))]
  "
{
  rtx set_dest = operands[0];
  rtx set_srca = operands[3];
  rtx set_srcb = operands[4];
  int third = rtx_equal_p (set_dest, set_srca);
  rtx dest1, dest2;
  rtx srca1, srca2, srcb1, srcb2;

  if (GET_CODE (set_dest) == SUBREG)
    set_dest = alter_subreg (set_dest);
  if (GET_CODE (set_srca) == SUBREG)
    set_srca = alter_subreg (set_srca);
  if (GET_CODE (set_srcb) == SUBREG)
    set_srcb = alter_subreg (set_srcb);

  dest1 = gen_rtx_REG (DFmode,
                       REGNO (set_dest) + (WORDS_BIG_ENDIAN ? 0 : 2));
  dest2 = gen_rtx_REG (DFmode,
                       REGNO (set_dest) + (WORDS_BIG_ENDIAN ? 2 : 0));
  srca1 = gen_rtx_REG (DFmode,
                       REGNO (set_srca) + (WORDS_BIG_ENDIAN ? 0 : 2));
  srca2 = gen_rtx_REG (DFmode,
                       REGNO (set_srca) + (WORDS_BIG_ENDIAN ? 2 : 0));
  srcb1 = gen_rtx_REG (DFmode,
                       REGNO (set_srcb) + (WORDS_BIG_ENDIAN ? 0 : 2));
  srcb2 = gen_rtx_REG (DFmode,
                       REGNO (set_srcb) + (WORDS_BIG_ENDIAN ? 2 : 0));

  /* Now emit using the real source and destination we found, swapping
     the order if we detect overlap.  */
  if ((third && reg_overlap_mentioned_p (dest1, srcb2))
      || (!third && reg_overlap_mentioned_p (dest1, srca2)))
    {
      emit_insn (gen_movdf_cc_sp64 (dest2, operands[1], operands[2], srca2, srcb2));
      emit_insn (gen_movdf_cc_sp64 (dest1, operands[1], operands[2], srca1, srcb1));
    }
  else
    {
      emit_insn (gen_movdf_cc_sp64 (dest1, operands[1], operands[2], srca1, srcb1));
      emit_insn (gen_movdf_cc_sp64 (dest2, operands[1], operands[2], srca2, srcb2));
    }
  DONE;
}")

(define_insn "*movqi_cc_reg_sp64"
  [(set (match_operand:QI 0 "register_operand" "=r,r")
	(if_then_else:QI (match_operator 1 "v9_regcmp_op"
				[(match_operand:DI 2 "register_operand" "r,r")
				 (const_int 0)])
                         (match_operand:QI 3 "arith10_operand" "rM,0")
                         (match_operand:QI 4 "arith10_operand" "0,rM")))]
  "TARGET_ARCH64"
  "@
   movr%D1\\t%2, %r3, %0
   movr%d1\\t%2, %r4, %0"
  [(set_attr "type" "cmove")
   (set_attr "length" "1")])

(define_insn "*movhi_cc_reg_sp64"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(if_then_else:HI (match_operator 1 "v9_regcmp_op"
				[(match_operand:DI 2 "register_operand" "r,r")
				 (const_int 0)])
                         (match_operand:HI 3 "arith10_operand" "rM,0")
                         (match_operand:HI 4 "arith10_operand" "0,rM")))]
  "TARGET_ARCH64"
  "@
   movr%D1\\t%2, %r3, %0
   movr%d1\\t%2, %r4, %0"
  [(set_attr "type" "cmove")
   (set_attr "length" "1")])

(define_insn "*movsi_cc_reg_sp64"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(if_then_else:SI (match_operator 1 "v9_regcmp_op"
				[(match_operand:DI 2 "register_operand" "r,r")
				 (const_int 0)])
                         (match_operand:SI 3 "arith10_operand" "rM,0")
                         (match_operand:SI 4 "arith10_operand" "0,rM")))]
  "TARGET_ARCH64"
  "@
   movr%D1\\t%2, %r3, %0
   movr%d1\\t%2, %r4, %0"
  [(set_attr "type" "cmove")
   (set_attr "length" "1")])

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
   movr%D1\\t%2, %r3, %0
   movr%d1\\t%2, %r4, %0"
  [(set_attr "type" "cmove")
   (set_attr "length" "1")])

(define_insn "*movdi_cc_reg_sp64_trunc"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(if_then_else:SI (match_operator 1 "v9_regcmp_op"
				[(match_operand:DI 2 "register_operand" "r,r")
				 (const_int 0)])
                         (match_operand:SI 3 "arith10_double_operand" "rMH,0")
                         (match_operand:SI 4 "arith10_double_operand" "0,rMH")))]
  "TARGET_ARCH64"
  "@
   movr%D1\\t%2, %r3, %0
   movr%d1\\t%2, %r4, %0"
  [(set_attr "type" "cmove")
   (set_attr "length" "1")])

(define_insn "*movsf_cc_reg_sp64"
  [(set (match_operand:SF 0 "register_operand" "=f,f")
	(if_then_else:SF (match_operator 1 "v9_regcmp_op"
				[(match_operand:DI 2 "register_operand" "r,r")
				 (const_int 0)])
                         (match_operand:SF 3 "register_operand" "f,0")
                         (match_operand:SF 4 "register_operand" "0,f")))]
  "TARGET_ARCH64 && TARGET_FPU"
  "@
   fmovrs%D1\\t%2, %3, %0
   fmovrs%d1\\t%2, %4, %0"
  [(set_attr "type" "fpcmove")
   (set_attr "length" "1")])

(define_insn "movdf_cc_reg_sp64"
  [(set (match_operand:DF 0 "register_operand" "=e,e")
	(if_then_else:DF (match_operator 1 "v9_regcmp_op"
				[(match_operand:DI 2 "register_operand" "r,r")
				 (const_int 0)])
                         (match_operand:DF 3 "register_operand" "e,0")
                         (match_operand:DF 4 "register_operand" "0,e")))]
  "TARGET_ARCH64 && TARGET_FPU"
  "@
   fmovrd%D1\\t%2, %3, %0
   fmovrd%d1\\t%2, %4, %0"
  [(set_attr "type" "fpcmove")
   (set_attr "length" "1")])

(define_insn "*movtf_cc_reg_hq_sp64"
  [(set (match_operand:TF 0 "register_operand" "=e,e")
	(if_then_else:TF (match_operator 1 "v9_regcmp_op"
				[(match_operand:DI 2 "register_operand" "r,r")
				 (const_int 0)])
                         (match_operand:TF 3 "register_operand" "e,0")
                         (match_operand:TF 4 "register_operand" "0,e")))]
  "TARGET_ARCH64 && TARGET_FPU && TARGET_HARD_QUAD"
  "@
   fmovrq%D1\\t%2, %3, %0
   fmovrq%d1\\t%2, %4, %0"
  [(set_attr "type" "fpcmove")
   (set_attr "length" "1")])

(define_insn "*movtf_cc_reg_sp64"
  [(set (match_operand:TF 0 "register_operand" "=e,e")
	(if_then_else:TF (match_operator 1 "v9_regcmp_op"
				[(match_operand:DI 2 "register_operand" "r,r")
				 (const_int 0)])
                         (match_operand:TF 3 "register_operand" "e,0")
                         (match_operand:TF 4 "register_operand" "0,e")))]
  "TARGET_ARCH64 && TARGET_FPU && ! TARGET_HARD_QUAD"
  "#"
  [(set_attr "type" "fpcmove")
   (set_attr "length" "2")])

(define_split
  [(set (match_operand:TF 0 "register_operand" "=e,e")
	(if_then_else:TF (match_operator 1 "v9_regcmp_op"
				[(match_operand:DI 2 "register_operand" "r,r")
				 (const_int 0)])
                         (match_operand:TF 3 "register_operand" "e,0")
                         (match_operand:TF 4 "register_operand" "0,e")))]
  "reload_completed && TARGET_ARCH64 && TARGET_FPU && ! TARGET_HARD_QUAD"
  [(clobber (const_int 0))]
  "
{
  rtx set_dest = operands[0];
  rtx set_srca = operands[3];
  rtx set_srcb = operands[4];
  int third = rtx_equal_p (set_dest, set_srca);
  rtx dest1, dest2;
  rtx srca1, srca2, srcb1, srcb2;

  if (GET_CODE (set_dest) == SUBREG)
    set_dest = alter_subreg (set_dest);
  if (GET_CODE (set_srca) == SUBREG)
    set_srca = alter_subreg (set_srca);
  if (GET_CODE (set_srcb) == SUBREG)
    set_srcb = alter_subreg (set_srcb);

  dest1 = gen_rtx_REG (DFmode,
                       REGNO (set_dest) + (WORDS_BIG_ENDIAN ? 0 : 2));
  dest2 = gen_rtx_REG (DFmode,
                       REGNO (set_dest) + (WORDS_BIG_ENDIAN ? 2 : 0));
  srca1 = gen_rtx_REG (DFmode,
                       REGNO (set_srca) + (WORDS_BIG_ENDIAN ? 0 : 2));
  srca2 = gen_rtx_REG (DFmode,
                       REGNO (set_srca) + (WORDS_BIG_ENDIAN ? 2 : 0));
  srcb1 = gen_rtx_REG (DFmode,
                       REGNO (set_srcb) + (WORDS_BIG_ENDIAN ? 0 : 2));
  srcb2 = gen_rtx_REG (DFmode,
                       REGNO (set_srcb) + (WORDS_BIG_ENDIAN ? 2 : 0));

  /* Now emit using the real source and destination we found, swapping
     the order if we detect overlap.  */
  if ((third && reg_overlap_mentioned_p (dest1, srcb2))
      || (!third && reg_overlap_mentioned_p (dest1, srca2)))
    {
      emit_insn (gen_movdf_cc_reg_sp64 (dest2, operands[1], operands[2], srca2, srcb2));
      emit_insn (gen_movdf_cc_reg_sp64 (dest1, operands[1], operands[2], srca1, srcb1));
    }
  else
    {
      emit_insn (gen_movdf_cc_reg_sp64 (dest1, operands[1], operands[2], srca1, srcb1));
      emit_insn (gen_movdf_cc_reg_sp64 (dest2, operands[1], operands[2], srca2, srcb2));
    }
  DONE;
}")


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

  emit_insn (gen_ashlsi3 (temp, gen_rtx_SUBREG (SImode, operand1, op1_subword),
			  shift_16));
  emit_insn (gen_lshrsi3 (operand0, temp, shift_16));
  DONE;
}")

(define_insn "*zero_extendhisi2_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (match_operand:HI 1 "memory_operand" "m")))]
  ""
  "lduh\\t%1, %0"
  [(set_attr "type" "load")
   (set_attr "length" "1")])

(define_expand "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "")
	(zero_extend:HI (match_operand:QI 1 "register_operand" "")))]
  ""
  "")

(define_insn "*zero_extendqihi2_insn"
  [(set (match_operand:HI 0 "register_operand" "=r,r")
	(zero_extend:HI (match_operand:QI 1 "input_operand" "r,m")))]
  "GET_CODE (operands[1]) != CONST_INT"
  "@
   and\\t%1, 0xff, %0
   ldub\\t%1, %0"
  [(set_attr "type" "unary,load")
   (set_attr "length" "1")])

(define_expand "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(zero_extend:SI (match_operand:QI 1 "register_operand" "")))]
  ""
  "")

(define_insn "*zero_extendqisi2_insn"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extend:SI (match_operand:QI 1 "input_operand" "r,m")))]
  "GET_CODE (operands[1]) != CONST_INT"
  "@
   and\\t%1, 0xff, %0
   ldub\\t%1, %0"
  [(set_attr "type" "unary,load")
   (set_attr "length" "1")])

(define_expand "zero_extendqidi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(zero_extend:DI (match_operand:QI 1 "register_operand" "")))]
  "TARGET_ARCH64"
  "")

(define_insn "*zero_extendqidi2_insn"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(zero_extend:DI (match_operand:QI 1 "input_operand" "r,m")))]
  "TARGET_ARCH64 && GET_CODE (operands[1]) != CONST_INT"
  "@
   and\\t%1, 0xff, %0
   ldub\\t%1, %0"
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

  emit_insn (gen_ashldi3 (temp, gen_rtx_SUBREG (DImode, operand1, op1_subword),
			  shift_48));
  emit_insn (gen_lshrdi3 (operand0, temp, shift_48));
  DONE;
}")

(define_insn "*zero_extendhidi2_insn"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (match_operand:HI 1 "memory_operand" "m")))]
  "TARGET_ARCH64"
  "lduh\\t%1, %0"
  [(set_attr "type" "load")
   (set_attr "length" "1")])


;; ??? Write truncdisi pattern using sra?

(define_expand "zero_extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(zero_extend:DI (match_operand:SI 1 "register_operand" "")))]
  ""
  "")

(define_insn "*zero_extendsidi2_insn_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(zero_extend:DI (match_operand:SI 1 "input_operand" "r,m")))]
  "TARGET_ARCH64 && GET_CODE (operands[1]) != CONST_INT"
  "@
   srl\\t%1, 0, %0
   lduw\\t%1, %0"
  [(set_attr "type" "shift,load")
   (set_attr "length" "1")])

(define_insn "*zero_extendsidi2_insn_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (zero_extend:DI (match_operand:SI 1 "register_operand" "r")))]
  "! TARGET_ARCH64"
  "#"
  [(set_attr "type" "unary")
   (set_attr "length" "2")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
        (zero_extend:DI (match_operand:SI 1 "register_operand" "")))]
  "! TARGET_ARCH64 && reload_completed"
  [(set (match_dup 2) (match_dup 3))
   (set (match_dup 4) (match_dup 5))]
  "
{
  rtx dest1, dest2;

  if (GET_CODE (operands[0]) == SUBREG)
    operands[0] = alter_subreg (operands[0]);

  dest1 = gen_highpart (SImode, operands[0]);
  dest2 = gen_lowpart (SImode, operands[0]);

  /* Swap the order in case of overlap.  */
  if (REGNO (dest1) == REGNO (operands[1]))
    {
      operands[2] = dest2;
      operands[3] = operands[1];
      operands[4] = dest1;
      operands[5] = const0_rtx;
    }
  else
    {
      operands[2] = dest1;
      operands[3] = const0_rtx;
      operands[4] = dest2;
      operands[5] = operands[1];
    }
}")

;; Simplify comparisons of extended values.

(define_insn "*cmp_zero_extendqisi2"
  [(set (reg:CC 100)
	(compare:CC (zero_extend:SI (match_operand:QI 0 "register_operand" "r"))
		    (const_int 0)))]
  "! TARGET_LIVE_G0"
  "andcc\\t%0, 0xff, %%g0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_zero_qi"
  [(set (reg:CC 100)
	(compare:CC (match_operand:QI 0 "register_operand" "r")
		    (const_int 0)))]
  "! TARGET_LIVE_G0"
  "andcc\\t%0, 0xff, %%g0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_zero_extendqisi2_set"
  [(set (reg:CC 100)
	(compare:CC (zero_extend:SI (match_operand:QI 1 "register_operand" "r"))
		    (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (match_dup 1)))]
  ""
  "andcc\\t%1, 0xff, %0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_zero_extendqisi2_andcc_set"
  [(set (reg:CC 100)
	(compare:CC (and:SI (match_operand:SI 1 "register_operand" "r")
			    (const_int 255))
		    (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (subreg:QI (match_dup 1) 0)))]
  ""
  "andcc\\t%1, 0xff, %0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_zero_extendqidi2"
  [(set (reg:CCX 100)
	(compare:CCX (zero_extend:DI (match_operand:QI 0 "register_operand" "r"))
		     (const_int 0)))]
  "TARGET_ARCH64"
  "andcc\\t%0, 0xff, %%g0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_zero_qi_sp64"
  [(set (reg:CCX 100)
	(compare:CCX (match_operand:QI 0 "register_operand" "r")
		     (const_int 0)))]
  "TARGET_ARCH64"
  "andcc\\t%0, 0xff, %%g0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_zero_extendqidi2_set"
  [(set (reg:CCX 100)
	(compare:CCX (zero_extend:DI (match_operand:QI 1 "register_operand" "r"))
		     (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (match_dup 1)))]
  "TARGET_ARCH64"
  "andcc\\t%1, 0xff, %0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_zero_extendqidi2_andcc_set"
  [(set (reg:CCX 100)
	(compare:CCX (and:DI (match_operand:DI 1 "register_operand" "r")
			     (const_int 255))
		     (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (subreg:QI (match_dup 1) 0)))]
  "TARGET_ARCH64"
  "andcc\\t%1, 0xff, %0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

;; Similarly, handle {SI,DI}->QI mode truncation followed by a compare.

(define_insn "*cmp_siqi_trunc"
  [(set (reg:CC 100)
	(compare:CC (subreg:QI (match_operand:SI 0 "register_operand" "r") 0)
		    (const_int 0)))]
  "! TARGET_LIVE_G0"
  "andcc\\t%0, 0xff, %%g0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_siqi_trunc_set"
  [(set (reg:CC 100)
	(compare:CC (subreg:QI (match_operand:SI 1 "register_operand" "r") 0)
		    (const_int 0)))
   (set (match_operand:QI 0 "register_operand" "=r")
	(subreg:QI (match_dup 1) 0))]
  ""
  "andcc\\t%1, 0xff, %0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_diqi_trunc"
  [(set (reg:CC 100)
	(compare:CC (subreg:QI (match_operand:DI 0 "register_operand" "r") 0)
		    (const_int 0)))]
  "TARGET_ARCH64"
  "andcc\\t%0, 0xff, %%g0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_diqi_trunc_set"
  [(set (reg:CC 100)
	(compare:CC (subreg:QI (match_operand:DI 1 "register_operand" "r") 0)
		    (const_int 0)))
   (set (match_operand:QI 0 "register_operand" "=r")
	(subreg:QI (match_dup 1) 0))]
  "TARGET_ARCH64"
  "andcc\\t%1, 0xff, %0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

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

  emit_insn (gen_ashlsi3 (temp, gen_rtx_SUBREG (SImode, operand1, op1_subword),
			  shift_16));
  emit_insn (gen_ashrsi3 (operand0, temp, shift_16));
  DONE;
}")

(define_insn "*sign_extendhisi2_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:HI 1 "memory_operand" "m")))]
  ""
  "ldsh\\t%1, %0"
  [(set_attr "type" "sload")
   (set_attr "length" "1")])

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
  emit_insn (gen_ashlsi3 (temp, gen_rtx_SUBREG (SImode, operand1, op1_subword),
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
  "ldsb\\t%1, %0"
  [(set_attr "type" "sload")
   (set_attr "length" "1")])

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

  emit_insn (gen_ashlsi3 (temp, gen_rtx_SUBREG (SImode, operand1, op1_subword),
			  shift_24));
  emit_insn (gen_ashrsi3 (operand0, temp, shift_24));
  DONE;
}")

(define_insn "*sign_extendqisi2_insn"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI (match_operand:QI 1 "memory_operand" "m")))]
  ""
  "ldsb\\t%1, %0"
  [(set_attr "type" "sload")
   (set_attr "length" "1")])

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

  emit_insn (gen_ashldi3 (temp, gen_rtx_SUBREG (DImode, operand1, op1_subword),
			  shift_56));
  emit_insn (gen_ashrdi3 (operand0, temp, shift_56));
  DONE;
}")

(define_insn "*sign_extendqidi2_insn"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI (match_operand:QI 1 "memory_operand" "m")))]
  "TARGET_ARCH64"
  "ldsb\\t%1, %0"
  [(set_attr "type" "sload")
   (set_attr "length" "1")])

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

  emit_insn (gen_ashldi3 (temp, gen_rtx_SUBREG (DImode, operand1, op1_subword),
			  shift_48));
  emit_insn (gen_ashrdi3 (operand0, temp, shift_48));
  DONE;
}")

(define_insn "*sign_extendhidi2_insn"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI (match_operand:HI 1 "memory_operand" "m")))]
  "TARGET_ARCH64"
  "ldsh\\t%1, %0"
  [(set_attr "type" "sload")
   (set_attr "length" "1")])

(define_expand "extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "")
	(sign_extend:DI (match_operand:SI 1 "register_operand" "")))]
  "TARGET_ARCH64"
  "")

(define_insn "*sign_extendsidi2_insn"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(sign_extend:DI (match_operand:SI 1 "input_operand" "r,m")))]
  "TARGET_ARCH64"
  "@
  sra\\t%1, 0, %0
  ldsw\\t%1, %0"
  [(set_attr "type" "shift,sload")
   (set_attr "length" "1")])

;; Special pattern for optimizing bit-field compares.  This is needed
;; because combine uses this as a canonical form.

(define_insn "*cmp_zero_extract"
  [(set (reg:CC 100)
	(compare:CC
	 (zero_extract:SI (match_operand:SI 0 "register_operand" "r")
			  (match_operand:SI 1 "small_int_or_double" "n")
			  (match_operand:SI 2 "small_int_or_double" "n"))
	 (const_int 0)))]
  "! TARGET_LIVE_G0
   && ((GET_CODE (operands[2]) == CONST_INT
        && INTVAL (operands[2]) > 19)
       || (GET_CODE (operands[2]) == CONST_DOUBLE
           && CONST_DOUBLE_LOW (operands[2]) > 19))"
  "*
{
  int len = (GET_CODE (operands[1]) == CONST_INT
             ? INTVAL (operands[1])
             : CONST_DOUBLE_LOW (operands[1]));
  int pos = 32 -
            (GET_CODE (operands[2]) == CONST_INT
             ? INTVAL (operands[2])
             : CONST_DOUBLE_LOW (operands[2])) - len;
  HOST_WIDE_INT mask = ((1 << len) - 1) << pos;

  operands[1] = GEN_INT (mask);
  return \"andcc\\t%0, %1, %%g0\";
}"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_zero_extract_sp64"
  [(set (reg:CCX 100)
	(compare:CCX
	 (zero_extract:DI (match_operand:DI 0 "register_operand" "r")
			  (match_operand:SI 1 "small_int_or_double" "n")
			  (match_operand:SI 2 "small_int_or_double" "n"))
	 (const_int 0)))]
  "TARGET_ARCH64
   && ((GET_CODE (operands[2]) == CONST_INT
        && INTVAL (operands[2]) > 51)
       || (GET_CODE (operands[2]) == CONST_DOUBLE
           && CONST_DOUBLE_LOW (operands[2]) > 51))"
  "*
{
  int len = (GET_CODE (operands[1]) == CONST_INT
             ? INTVAL (operands[1])
             : CONST_DOUBLE_LOW (operands[1]));
  int pos = 64 -
            (GET_CODE (operands[2]) == CONST_INT
             ? INTVAL (operands[2])
             : CONST_DOUBLE_LOW (operands[2])) - len;
  HOST_WIDE_INT mask = (((unsigned HOST_WIDE_INT) 1 << len) - 1) << pos;

  operands[1] = GEN_INT (mask);
  return \"andcc\\t%0, %1, %%g0\";
}"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

;; Conversions between float, double and long double.

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(float_extend:DF
	 (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_FPU"
  "fstod\\t%1, %0"
  [(set_attr "type" "fp")
   (set_attr "length" "1")])

(define_expand "extendsftf2"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(float_extend:TF
	 (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_FPU && (TARGET_HARD_QUAD || TARGET_ARCH64)"
  "
{
  if (! TARGET_HARD_QUAD)
    {
      rtx slot0;

      if (GET_CODE (operands[0]) != MEM)
	slot0 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode), 0);
      else
	slot0 = operands[0];

      emit_library_call (gen_rtx (SYMBOL_REF, Pmode, \"_Qp_stoq\"), 0,
			 VOIDmode, 2,
			 XEXP (slot0, 0), Pmode,
			 operands[1], SFmode);

      if (GET_CODE (operands[0]) != MEM)
	emit_insn (gen_rtx_SET (VOIDmode, operands[0], slot0));
      DONE;
    }
}")

(define_insn "*extendsftf2_hq"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(float_extend:TF
	 (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fstoq\\t%1, %0"
  [(set_attr "type" "fp")
   (set_attr "length" "1")])

(define_expand "extenddftf2"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(float_extend:TF
	 (match_operand:DF 1 "register_operand" "e")))]
  "TARGET_FPU && (TARGET_HARD_QUAD || TARGET_ARCH64)"
  "
{
  if (! TARGET_HARD_QUAD)
    {
      rtx slot0;

      if (GET_CODE (operands[0]) != MEM)
	slot0 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode), 0);
      else
	slot0 = operands[0];

      emit_library_call (gen_rtx (SYMBOL_REF, Pmode, \"_Qp_dtoq\"), 0,
			 VOIDmode, 2,
			 XEXP (slot0, 0), Pmode,
			 operands[1], DFmode);

      if (GET_CODE (operands[0]) != MEM)
	emit_insn (gen_rtx_SET (VOIDmode, operands[0], slot0));
      DONE;
    }
}")

(define_insn "*extenddftf2_hq"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(float_extend:TF
	 (match_operand:DF 1 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fdtoq\\t%1, %0"
  [(set_attr "type" "fp")
   (set_attr "length" "1")])

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float_truncate:SF
	 (match_operand:DF 1 "register_operand" "e")))]
  "TARGET_FPU"
  "fdtos\\t%1, %0"
  [(set_attr "type" "fp")
   (set_attr "length" "1")])

(define_expand "trunctfsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float_truncate:SF
	 (match_operand:TF 1 "register_operand" "e")))]
  "TARGET_FPU && (TARGET_HARD_QUAD || TARGET_ARCH64)"
  "
{
  if (! TARGET_HARD_QUAD)
    {
      rtx slot0;

      if (GET_CODE (operands[1]) != MEM)
	{
	  slot0 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode), 0);
	  emit_insn (gen_rtx_SET (VOIDmode, slot0, operands[1]));
	}
      else
	slot0 = operands[1];

      emit_library_call_value (gen_rtx (SYMBOL_REF, Pmode, \"_Qp_qtos\"),
			       operands[0], 0, SFmode, 1,
			       XEXP (slot0, 0), Pmode);
      DONE;
    }
}")

(define_insn "*trunctfsf2_hq"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float_truncate:SF
	 (match_operand:TF 1 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fqtos\\t%1, %0"
  [(set_attr "type" "fp")
   (set_attr "length" "1")])

(define_expand "trunctfdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float_truncate:DF
	 (match_operand:TF 1 "register_operand" "e")))]
  "TARGET_FPU && (TARGET_HARD_QUAD || TARGET_ARCH64)"
  "
{
  if (! TARGET_HARD_QUAD)
    {
      rtx slot0;

      if (GET_CODE (operands[1]) != MEM)
	{
	  slot0 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode), 0);
	  emit_insn (gen_rtx_SET (VOIDmode, slot0, operands[1]));
	}
      else
	slot0 = operands[1];

      emit_library_call_value (gen_rtx (SYMBOL_REF, Pmode, \"_Qp_qtod\"),
			       operands[0], 0, DFmode, 1,
			       XEXP (slot0, 0), Pmode);
      DONE;
    }
}")

(define_insn "*trunctfdf2_hq"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(float_truncate:DF
	 (match_operand:TF 1 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fqtod\\t%1, %0"
  [(set_attr "type" "fp")
   (set_attr "length" "1")])

;; Conversion between fixed point and floating point.

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float:SF (match_operand:SI 1 "register_operand" "f")))]
  "TARGET_FPU"
  "fitos\\t%1, %0"
  [(set_attr "type" "fp")
   (set_attr "length" "1")])

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(float:DF (match_operand:SI 1 "register_operand" "f")))]
  "TARGET_FPU"
  "fitod\\t%1, %0"
  [(set_attr "type" "fp")
   (set_attr "length" "1")])

(define_expand "floatsitf2"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(float:TF (match_operand:SI 1 "register_operand" "f")))]
  "TARGET_FPU && (TARGET_HARD_QUAD || TARGET_ARCH64)"
  "
{
  if (! TARGET_HARD_QUAD)
    {
      rtx slot0;

      if (GET_CODE (operands[1]) != MEM)
	slot0 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode), 0);
      else
	slot0 = operands[1];

      emit_library_call (gen_rtx (SYMBOL_REF, Pmode, \"_Qp_itoq\"), 0,
			 VOIDmode, 2,
			 XEXP (slot0, 0), Pmode,
			 operands[1], SImode);

      if (GET_CODE (operands[0]) != MEM)
	emit_insn (gen_rtx_SET (VOIDmode, operands[0], slot0));
      DONE;
    }
}")

(define_insn "*floatsitf2_hq"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(float:TF (match_operand:SI 1 "register_operand" "f")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fitoq\\t%1, %0"
  [(set_attr "type" "fp")
   (set_attr "length" "1")])

(define_expand "floatunssitf2"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(unsigned_float:TF (match_operand:SI 1 "register_operand" "e")))]
  "TARGET_FPU && TARGET_ARCH64 && ! TARGET_HARD_QUAD"
  "
{
  rtx slot0;

  if (GET_CODE (operands[1]) != MEM)
    slot0 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode), 0);
  else
    slot0 = operands[1];

  emit_library_call (gen_rtx (SYMBOL_REF, Pmode, \"_Qp_uitoq\"), 0,
		     VOIDmode, 2,
		     XEXP (slot0, 0), Pmode,
		     operands[1], SImode);

  if (GET_CODE (operands[0]) != MEM)
    emit_insn (gen_rtx_SET (VOIDmode, operands[0], slot0));
  DONE;
}")

;; Now the same for 64 bit sources.

(define_insn "floatdisf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float:SF (match_operand:DI 1 "register_operand" "e")))]
  "TARGET_V9 && TARGET_FPU"
  "fxtos\\t%1, %0"
  [(set_attr "type" "fp")
   (set_attr "length" "1")])

(define_insn "floatdidf2"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(float:DF (match_operand:DI 1 "register_operand" "e")))]
  "TARGET_V9 && TARGET_FPU"
  "fxtod\\t%1, %0"
  [(set_attr "type" "fp")
   (set_attr "length" "1")])

(define_expand "floatditf2"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(float:TF (match_operand:DI 1 "register_operand" "e")))]
  "TARGET_FPU && TARGET_V9 && (TARGET_HARD_QUAD || TARGET_ARCH64)"
  "
{
  if (! TARGET_HARD_QUAD)
    {
      rtx slot0;

      if (GET_CODE (operands[1]) != MEM)
	slot0 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode), 0);
      else
	slot0 = operands[1];

      emit_library_call (gen_rtx (SYMBOL_REF, Pmode, \"_Qp_xtoq\"), 0,
			 VOIDmode, 2,
			 XEXP (slot0, 0), Pmode,
			 operands[1], DImode);

      if (GET_CODE (operands[0]) != MEM)
	emit_insn (gen_rtx_SET (VOIDmode, operands[0], slot0));
      DONE;
    }
}")

(define_insn "*floatditf2_hq"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(float:TF (match_operand:DI 1 "register_operand" "e")))]
  "TARGET_V9 && TARGET_FPU && TARGET_HARD_QUAD"
  "fxtoq\\t%1, %0"
  [(set_attr "type" "fp")
   (set_attr "length" "1")])

(define_expand "floatunsditf2"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(unsigned_float:TF (match_operand:DI 1 "register_operand" "e")))]
  "TARGET_FPU && TARGET_ARCH64 && ! TARGET_HARD_QUAD"
  "
{
  rtx slot0;

  if (GET_CODE (operands[1]) != MEM)
    slot0 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode), 0);
  else
    slot0 = operands[1];

  emit_library_call (gen_rtx (SYMBOL_REF, Pmode, \"_Qp_uxtoq\"), 0,
		     VOIDmode, 2,
		     XEXP (slot0, 0), Pmode,
		     operands[1], DImode);

  if (GET_CODE (operands[0]) != MEM)
    emit_insn (gen_rtx_SET (VOIDmode, operands[0], slot0));
  DONE;
}")

;; Convert a float to an actual integer.
;; Truncation is performed as part of the conversion.

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (fix:SF (match_operand:SF 1 "register_operand" "f"))))]
  "TARGET_FPU"
  "fstoi\\t%1, %0"
  [(set_attr "type" "fp")
   (set_attr "length" "1")])

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (fix:DF (match_operand:DF 1 "register_operand" "e"))))]
  "TARGET_FPU"
  "fdtoi\\t%1, %0"
  [(set_attr "type" "fp")
   (set_attr "length" "1")])

(define_expand "fix_trunctfsi2"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (fix:TF (match_operand:TF 1 "register_operand" "e"))))]
  "TARGET_FPU && (TARGET_HARD_QUAD || TARGET_ARCH64)"
  "
{
  if (! TARGET_HARD_QUAD)
    {
      rtx slot0;

      if (GET_CODE (operands[1]) != MEM)
	{
	  slot0 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode), 0);
	  emit_insn (gen_rtx_SET (VOIDmode, slot0, operands[1]));
	}
      else
	slot0 = operands[1];

      emit_library_call_value (gen_rtx (SYMBOL_REF, Pmode, \"_Qp_qtoi\"),
			       operands[0], 0, SImode, 1,
			       XEXP (slot0, 0), Pmode);
      DONE;
    }
}")

(define_insn "*fix_trunctfsi2_hq"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(fix:SI (fix:TF (match_operand:TF 1 "register_operand" "e"))))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fqtoi\\t%1, %0"
  [(set_attr "type" "fp")
   (set_attr "length" "1")])

(define_expand "fixuns_trunctfsi2"
  [(set (match_operand:SI 0 "register_operand" "=f")
	(unsigned_fix:SI (fix:TF (match_operand:TF 1 "register_operand" "e"))))]
  "TARGET_FPU && TARGET_ARCH64 && ! TARGET_HARD_QUAD"
  "
{
  rtx slot0;

  if (GET_CODE (operands[1]) != MEM)
    {
      slot0 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode), 0);
      emit_insn (gen_rtx_SET (VOIDmode, slot0, operands[1]));
    }
  else
    slot0 = operands[1];

  emit_library_call_value (gen_rtx (SYMBOL_REF, Pmode, \"_Qp_qtoui\"),
			   operands[0], 0, SImode, 1,
			   XEXP (slot0, 0), Pmode);
  DONE;
}")

;; Now the same, for V9 targets

(define_insn "fix_truncsfdi2"
  [(set (match_operand:DI 0 "register_operand" "=e")
	(fix:DI (fix:SF (match_operand:SF 1 "register_operand" "f"))))]
  "TARGET_V9 && TARGET_FPU"
  "fstox\\t%1, %0"
  [(set_attr "type" "fp")
   (set_attr "length" "1")])

(define_insn "fix_truncdfdi2"
  [(set (match_operand:DI 0 "register_operand" "=e")
	(fix:DI (fix:DF (match_operand:DF 1 "register_operand" "e"))))]
  "TARGET_V9 && TARGET_FPU"
  "fdtox\\t%1, %0"
  [(set_attr "type" "fp")
   (set_attr "length" "1")])

(define_expand "fix_trunctfdi2"
  [(set (match_operand:DI 0 "register_operand" "=e")
	(fix:SI (fix:TF (match_operand:TF 1 "register_operand" "e"))))]
  "TARGET_V9 && TARGET_FPU && (TARGET_HARD_QUAD || TARGET_ARCH64)"
  "
{
  if (! TARGET_HARD_QUAD)
    {
      rtx slot0;

      if (GET_CODE (operands[1]) != MEM)
	{
	  slot0 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode), 0);
	  emit_insn (gen_rtx_SET (VOIDmode, slot0, operands[1]));
	}
      else
	slot0 = operands[1];

      emit_library_call_value (gen_rtx (SYMBOL_REF, Pmode, \"_Qp_qtox\"),
			       operands[0], 0, DImode, 1,
			       XEXP (slot0, 0), Pmode);
      DONE;
    }
}")

(define_insn "*fix_trunctfdi2_hq"
  [(set (match_operand:DI 0 "register_operand" "=e")
	(fix:DI (fix:TF (match_operand:TF 1 "register_operand" "e"))))]
  "TARGET_V9 && TARGET_FPU && TARGET_HARD_QUAD"
  "fqtox\\t%1, %0"
  [(set_attr "type" "fp")
   (set_attr "length" "1")])

(define_expand "fixuns_trunctfdi2"
  [(set (match_operand:DI 0 "register_operand" "=f")
	(unsigned_fix:DI (fix:TF (match_operand:TF 1 "register_operand" "e"))))]
  "TARGET_FPU && TARGET_ARCH64 && ! TARGET_HARD_QUAD"
  "
{
  rtx slot0;

  if (GET_CODE (operands[1]) != MEM)
    {
      slot0 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode), 0);
      emit_insn (gen_rtx_SET (VOIDmode, slot0, operands[1]));
    }
  else
    slot0 = operands[1];

  emit_library_call_value (gen_rtx (SYMBOL_REF, Pmode, \"_Qp_qtoux\"),
			   operands[0], 0, DImode, 1,
			   XEXP (slot0, 0), Pmode);
  DONE;
}")


;;- arithmetic instructions

(define_expand "adddi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI (match_operand:DI 1 "arith_double_operand" "%r")
		 (match_operand:DI 2 "arith_double_add_operand" "rHI")))]
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
  if (arith_double_4096_operand(operands[2], DImode))
    {
      emit_insn (gen_rtx_SET (VOIDmode, operands[0],
			      gen_rtx_MINUS (DImode, operands[1],
					     GEN_INT(-4096))));
      DONE;
    }
}")

(define_insn "adddi3_insn_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI (match_operand:DI 1 "arith_double_operand" "%r")
		 (match_operand:DI 2 "arith_double_operand" "rHI")))
   (clobber (reg:CC 100))]
  "! TARGET_ARCH64"
  "#"
  [(set_attr "length" "2")])

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
(define_insn "addx"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (plus:SI (match_operand:SI 1 "arith_operand" "%r")
			  (match_operand:SI 2 "arith_operand" "rI"))
		 (ltu:SI (reg:CC_NOOV 100) (const_int 0))))]
  ""
  "addx\\t%1, %2, %0"
  [(set_attr "type" "unary")
   (set_attr "length" "1")])

(define_insn "*addx_extend_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (plus:SI (plus:SI (match_operand:SI 1 "reg_or_0_operand" "%rJ")
                                          (match_operand:SI 2 "arith_operand" "rI"))
                                 (ltu:SI (reg:CC_NOOV 100) (const_int 0)))))]
  "! TARGET_ARCH64"
  "#"
  [(set_attr "type" "unary")
   (set_attr "length" "2")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(zero_extend:DI (plus:SI (plus:SI (match_operand:SI 1 "reg_or_0_operand" "")
                                          (match_operand:SI 2 "arith_operand" ""))
                                 (ltu:SI (reg:CC_NOOV 100) (const_int 0)))))]
  "! TARGET_ARCH64 && reload_completed"
  [(set (match_dup 3) (plus:SI (plus:SI (match_dup 1) (match_dup 2))
                               (ltu:SI (reg:CC_NOOV 100) (const_int 0))))
   (set (match_dup 4) (const_int 0))]
  "operands[3] = gen_lowpart (SImode, operands[0]);
   operands[4] = gen_highpart (SImode, operands[1]);")

(define_insn "*addx_extend_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (plus:SI (plus:SI (match_operand:SI 1 "reg_or_0_operand" "%rJ")
                                          (match_operand:SI 2 "arith_operand" "rI"))
                                 (ltu:SI (reg:CC_NOOV 100) (const_int 0)))))]
  "TARGET_ARCH64"
  "addx\\t%r1, %2, %0"
  [(set_attr "type" "misc")
   (set_attr "length" "1")])

(define_insn "subx"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (minus:SI (match_operand:SI 1 "reg_or_0_operand" "rJ")
			    (match_operand:SI 2 "arith_operand" "rI"))
		  (ltu:SI (reg:CC_NOOV 100) (const_int 0))))]
  ""
  "subx\\t%r1, %2, %0"
  [(set_attr "type" "misc")
   (set_attr "length" "1")])

(define_insn "*subx_extend_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (minus:SI (minus:SI (match_operand:SI 1 "reg_or_0_operand" "rJ")
                                            (match_operand:SI 2 "arith_operand" "rI"))
                                  (ltu:SI (reg:CC_NOOV 100) (const_int 0)))))]
  "TARGET_ARCH64"
  "subx\\t%r1, %2, %0"
  [(set_attr "type" "misc")
   (set_attr "length" "1")])

(define_insn "*subx_extend"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (minus:SI (minus:SI (match_operand:SI 1 "reg_or_0_operand" "rJ")
                                            (match_operand:SI 2 "arith_operand" "rI"))
                                  (ltu:SI (reg:CC_NOOV 100) (const_int 0)))))]
  "! TARGET_ARCH64"
  "#"
  [(set_attr "type" "unary")
   (set_attr "length" "2")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extend:DI (minus:SI (minus:SI (match_operand:SI 1 "reg_or_0_operand" "rJ")
                                            (match_operand:SI 2 "arith_operand" "rI"))
                                  (ltu:SI (reg:CC_NOOV 100) (const_int 0)))))]
  "! TARGET_ARCH64 && reload_completed"
  [(set (match_dup 3) (minus:SI (minus:SI (match_dup 1) (match_dup 2))
                                (ltu:SI (reg:CC_NOOV 100) (const_int 0))))
   (set (match_dup 4) (const_int 0))]
  "operands[3] = gen_lowpart (SImode, operands[0]);
   operands[4] = gen_highpart (SImode, operands[0]);")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
        (plus:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r"))
                 (match_operand:DI 2 "register_operand" "r")))
   (clobber (reg:CC 100))]
  "! TARGET_ARCH64"
  "#"
  [(set_attr "type" "multi")
   (set_attr "length" "2")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
        (plus:DI (zero_extend:DI (match_operand:SI 1 "register_operand" ""))
                 (match_operand:DI 2 "register_operand" "")))
   (clobber (reg:CC 100))]
  "! TARGET_ARCH64 && reload_completed"
  [(parallel [(set (reg:CC_NOOV 100)
                   (compare:CC_NOOV (plus:SI (match_dup 3) (match_dup 1))
                                    (const_int 0)))
              (set (match_dup 5) (plus:SI (match_dup 3) (match_dup 1)))])
   (set (match_dup 6)
        (plus:SI (plus:SI (match_dup 4) (const_int 0))
                 (ltu:SI (reg:CC_NOOV 100) (const_int 0))))]
  "operands[3] = gen_lowpart (SImode, operands[2]);
   operands[4] = gen_highpart (SImode, operands[2]);
   operands[5] = gen_lowpart (SImode, operands[0]);
   operands[6] = gen_highpart (SImode, operands[0]);")

(define_insn "*adddi3_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI (match_operand:DI 1 "arith_double_operand" "%r")
		 (match_operand:DI 2 "arith_double_operand" "rHI")))]
  "TARGET_ARCH64"
  "add\\t%1, %2, %0"
  [(set_attr "type" "binary")
   (set_attr "length" "1")])

(define_expand "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,d")
	(plus:SI (match_operand:SI 1 "arith_operand" "%r,d")
		 (match_operand:SI 2 "arith_add_operand" "rI,d")))]
  ""
  "
{
  if (arith_4096_operand(operands[2], DImode))
    {
      emit_insn (gen_rtx_SET (VOIDmode, operands[0],
			      gen_rtx_MINUS (SImode, operands[1],
					     GEN_INT(-4096))));
      DONE;
    }
}")

(define_insn "*addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,d")
	(plus:SI (match_operand:SI 1 "arith_operand" "%r,d")
		 (match_operand:SI 2 "arith_operand" "rI,d")))]
  ""
  "@
   add\\t%1, %2, %0
   fpadd32s\\t%1, %2, %0"
  [(set_attr "type" "ialu,fp")
   (set_attr "length" "1")])

(define_insn "*cmp_cc_plus"
  [(set (reg:CC_NOOV 100)
	(compare:CC_NOOV (plus:SI (match_operand:SI 0 "arith_operand" "%r")
				  (match_operand:SI 1 "arith_operand" "rI"))
			 (const_int 0)))]
  "! TARGET_LIVE_G0"
  "addcc\\t%0, %1, %%g0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_ccx_plus"
  [(set (reg:CCX_NOOV 100)
	(compare:CCX_NOOV (plus:DI (match_operand:DI 0 "arith_double_operand" "%r")
				   (match_operand:DI 1 "arith_double_operand" "rHI"))
			  (const_int 0)))]
  "TARGET_ARCH64"
  "addcc\\t%0, %1, %%g0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_cc_plus_set"
  [(set (reg:CC_NOOV 100)
	(compare:CC_NOOV (plus:SI (match_operand:SI 1 "arith_operand" "%r")
				  (match_operand:SI 2 "arith_operand" "rI"))
			 (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  ""
  "addcc\\t%1, %2, %0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_ccx_plus_set"
  [(set (reg:CCX_NOOV 100)
	(compare:CCX_NOOV (plus:DI (match_operand:DI 1 "arith_double_operand" "%r")
				   (match_operand:DI 2 "arith_double_operand" "rHI"))
			  (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI (match_dup 1) (match_dup 2)))]
  "TARGET_ARCH64"
  "addcc\\t%1, %2, %0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_expand "subdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(minus:DI (match_operand:DI 1 "register_operand" "r")
		  (match_operand:DI 2 "arith_double_add_operand" "rHI")))]
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
  if (arith_double_4096_operand(operands[2], DImode))
    {
      emit_insn (gen_rtx_SET (VOIDmode, operands[0],
			      gen_rtx_PLUS (DImode, operands[1],
					    GEN_INT(-4096))));
      DONE;
    }
}")

(define_insn "*subdi3_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(minus:DI (match_operand:DI 1 "register_operand" "r")
		  (match_operand:DI 2 "arith_double_operand" "rHI")))
   (clobber (reg:CC 100))]
  "! TARGET_ARCH64"
  "#"
  [(set_attr "length" "2")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
        (minus:DI (match_operand:DI 1 "register_operand" "")
                  (match_operand:DI 2 "arith_double_operand" "")))
   (clobber (reg:CC 100))]
  "! TARGET_ARCH64
   && reload_completed
   && (GET_CODE (operands[2]) == CONST_INT
       || GET_CODE (operands[2]) == CONST_DOUBLE)"
  [(clobber (const_int 0))]
  "
{
  rtx highp, lowp;

  highp = gen_highpart (SImode, operands[2]);
  lowp = gen_lowpart (SImode, operands[2]);
  if ((lowp == const0_rtx)
      && (operands[0] == operands[1]))
    {
      emit_insn (gen_rtx_SET (VOIDmode,
                              gen_highpart (SImode, operands[0]),
                              gen_rtx_MINUS (SImode,
                                             gen_highpart (SImode, operands[1]),
                                             highp)));
    }
  else
    {
      emit_insn (gen_cmp_minus_cc_set (gen_lowpart (SImode, operands[0]),
                                       gen_lowpart (SImode, operands[1]),
                                       lowp));
      emit_insn (gen_subx (gen_highpart (SImode, operands[0]),
                           gen_highpart (SImode, operands[1]),
                           highp));
    }
  DONE;
}")

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
        (minus:DI (match_operand:DI 1 "register_operand" "")
                  (match_operand:DI 2 "register_operand" "")))
   (clobber (reg:CC 100))]
  "! TARGET_ARCH64
   && reload_completed"
  [(clobber (const_int 0))]
  "
{
  emit_insn (gen_cmp_minus_cc_set (gen_lowpart (SImode, operands[0]),
                                   gen_lowpart (SImode, operands[1]),
                                   gen_lowpart (SImode, operands[2])));
  emit_insn (gen_subx (gen_highpart (SImode, operands[0]),
                       gen_highpart (SImode, operands[1]),
                       gen_highpart (SImode, operands[2])));
  DONE;
}")

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
      (minus:DI (match_operand:DI 1 "register_operand" "r")
                (zero_extend:DI (match_operand:SI 2 "register_operand" "r"))))
   (clobber (reg:CC 100))]
  "! TARGET_ARCH64"
  "#"
  [(set_attr "type" "multi")
   (set_attr "length" "2")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
        (minus:DI (match_operand:DI 1 "register_operand" "")
                  (zero_extend:DI (match_operand:SI 2 "register_operand" ""))))
   (clobber (reg:CC 100))]
  "! TARGET_ARCH64 && reload_completed"
  [(parallel [(set (reg:CC_NOOV 100)
                   (compare:CC_NOOV (minus:SI (match_dup 3) (match_dup 2))
                                    (const_int 0)))
              (set (match_dup 5) (minus:SI (match_dup 3) (match_dup 2)))])
   (set (match_dup 6)
        (minus:SI (minus:SI (match_dup 4) (const_int 0))
                  (ltu:SI (reg:CC_NOOV 100) (const_int 0))))]
  "operands[3] = gen_lowpart (SImode, operands[1]);
   operands[4] = gen_highpart (SImode, operands[1]);
   operands[5] = gen_lowpart (SImode, operands[0]);
   operands[6] = gen_highpart (SImode, operands[0]);")

(define_insn "*subdi3_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(minus:DI (match_operand:DI 1 "register_operand" "r")
		  (match_operand:DI 2 "arith_double_operand" "rHI")))]
  "TARGET_ARCH64"
  "sub\\t%1, %2, %0"
  [(set_attr "type" "binary")
   (set_attr "length" "1")])

(define_expand "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,d")
	(minus:SI (match_operand:SI 1 "register_operand" "r,d")
		  (match_operand:SI 2 "arith_add_operand" "rI,d")))]
  ""
  "
{
  if (arith_4096_operand(operands[2], DImode))
    {
      emit_insn (gen_rtx_SET (VOIDmode, operands[0],
			      gen_rtx_PLUS (SImode, operands[1],
					    GEN_INT(-4096))));
      DONE;
    }
}")

(define_insn "*subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,d")
	(minus:SI (match_operand:SI 1 "register_operand" "r,d")
		  (match_operand:SI 2 "arith_operand" "rI,d")))]
  ""
  "@
   sub\\t%1, %2, %0
   fpsub32s\\t%1, %2, %0"
  [(set_attr "type" "ialu,fp")
   (set_attr "length" "1")])

(define_insn "*cmp_minus_cc"
  [(set (reg:CC_NOOV 100)
	(compare:CC_NOOV (minus:SI (match_operand:SI 0 "reg_or_0_operand" "rJ")
				   (match_operand:SI 1 "arith_operand" "rI"))
			 (const_int 0)))]
  "! TARGET_LIVE_G0"
  "subcc\\t%r0, %1, %%g0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_minus_ccx"
  [(set (reg:CCX_NOOV 100)
	(compare:CCX_NOOV (minus:DI (match_operand:DI 0 "register_operand" "r")
				    (match_operand:DI 1 "arith_double_operand" "rHI"))
			  (const_int 0)))]
  "TARGET_ARCH64"
  "subcc\\t%0, %1, %%g0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "cmp_minus_cc_set"
  [(set (reg:CC_NOOV 100)
	(compare:CC_NOOV (minus:SI (match_operand:SI 1 "reg_or_0_operand" "rJ")
				   (match_operand:SI 2 "arith_operand" "rI"))
			 (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_dup 1) (match_dup 2)))]
  ""
  "subcc\\t%r1, %2, %0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_minus_ccx_set"
  [(set (reg:CCX_NOOV 100)
	(compare:CCX_NOOV (minus:DI (match_operand:DI 1 "register_operand" "r")
				    (match_operand:DI 2 "arith_double_operand" "rHI"))
			  (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(minus:DI (match_dup 1) (match_dup 2)))]
  "TARGET_ARCH64"
  "subcc\\t%1, %2, %0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

;; Integer Multiply/Divide.

;; The 32 bit multiply/divide instructions are deprecated on v9, but at
;; least in UltraSPARC I, II and IIi it is a win tick-wise.

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mult:SI (match_operand:SI 1 "arith_operand" "%r")
		 (match_operand:SI 2 "arith_operand" "rI")))]
  "TARGET_HARD_MUL"
  "smul\\t%1, %2, %0"
  [(set_attr "type" "imul")
   (set_attr "length" "1")])

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
  "mulx\\t%1, %2, %0"
  [(set_attr "type" "imul")
   (set_attr "length" "1")])

;; V8plus wide multiply.
;; XXX
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
    output_asm_insn (\"srl\\t%L1, 0, %L1\", operands);
  if (which_alternative == 1)
    output_asm_insn (\"sllx\\t%H1, 32, %H1\", operands);
  if (sparc_check_64 (operands[2], insn) <= 0)
    output_asm_insn (\"srl\\t%L2, 0, %L2\", operands);
  if (which_alternative == 1)
    return \"or\\t%L1, %H1, %H1\\n\\tsllx\\t%H2, 32, %L1\\n\\tor\\t%L2, %L1, %L1\\n\\tmulx\\t%H1, %L1, %L0\;srlx\\t%L0, 32, %H0\";
  else
    return \"sllx\\t%H1, 32, %3\\n\\tsllx\\t%H2, 32, %4\\n\\tor\\t%L1, %3, %3\\n\\tor\\t%L2, %4, %4\\n\\tmulx\\t%3, %4, %3\\n\\tsrlx\\t%3, 32, %H0\\n\\tmov\\t%3, %L0\";
}"
  [(set_attr "length" "9,8")])

(define_insn "*cmp_mul_set"
  [(set (reg:CC 100)
	(compare:CC (mult:SI (match_operand:SI 1 "arith_operand" "%r")
		    (match_operand:SI 2 "arith_operand" "rI"))
		    (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(mult:SI (match_dup 1) (match_dup 2)))]
  "TARGET_V8 || TARGET_SPARCLITE || TARGET_DEPRECATED_V8_INSNS"
  "smulcc\\t%1, %2, %0"
  [(set_attr "type" "imul")
   (set_attr "length" "1")])

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
	emit_insn (gen_const_mulsidi3_v8plus (operands[0], operands[1],
					      operands[2]));
      else
	emit_insn (gen_const_mulsidi3_sp32 (operands[0], operands[1],
					    operands[2]));
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
;; XXX
(define_insn "mulsidi3_v8plus"
  [(set (match_operand:DI 0 "register_operand" "=h,r")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r,r"))
		 (sign_extend:DI (match_operand:SI 2 "register_operand" "r,r"))))
   (clobber (match_scratch:SI 3 "=X,&h"))]
  "TARGET_V8PLUS"
  "@
   smul\\t%1, %2, %L0\\n\\tsrlx\\t%L0, 32, %H0
   smul\\t%1, %2, %3\\n\\tsrlx\\t%3, 32, %H0\\n\\tmov\\t%3, %L0"
  [(set_attr "length" "2,3")])

;; XXX
(define_insn "const_mulsidi3_v8plus"
  [(set (match_operand:DI 0 "register_operand" "=h,r")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r,r"))
		 (match_operand:SI 2 "small_int" "I,I")))
   (clobber (match_scratch:SI 3 "=X,&h"))]
  "TARGET_V8PLUS"
  "@
   smul\\t%1, %2, %L0\\n\\tsrlx\\t%L0, 32, %H0
   smul\\t%1, %2, %3\\n\\tsrlx\\t%3, 32, %H0\\n\\tmov\\t%3, %L0"
  [(set_attr "length" "2,3")])

;; XXX
(define_insn "*mulsidi3_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r"))
		 (sign_extend:DI (match_operand:SI 2 "register_operand" "r"))))]
  "TARGET_HARD_MUL32"
  "*
{
  return TARGET_SPARCLET ? \"smuld\\t%1, %2, %L0\" : \"smul\\t%1, %2, %L0\\n\\trd\\t%%y, %H0\";
}"
  [(set (attr "length")
	(if_then_else (eq_attr "isa" "sparclet")
		      (const_int 1) (const_int 2)))])

(define_insn "*mulsidi3_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r"))
		 (sign_extend:DI (match_operand:SI 2 "register_operand" "r"))))]
  "TARGET_DEPRECATED_V8_INSNS && TARGET_ARCH64"
  "smul\\t%1, %2, %0"
  [(set_attr "length" "1")])

;; Extra pattern, because sign_extend of a constant isn't valid.

;; XXX
(define_insn "const_mulsidi3_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r"))
		 (match_operand:SI 2 "small_int" "I")))]
  "TARGET_HARD_MUL32"
  "*
{
  return TARGET_SPARCLET ? \"smuld\\t%1, %2, %L0\" : \"smul\\t%1, %2, %L0\\n\\trd\\t%%y, %H0\";
}"
  [(set (attr "length")
	(if_then_else (eq_attr "isa" "sparclet")
		      (const_int 1) (const_int 2)))])

(define_insn "const_mulsidi3_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r"))
		 (match_operand:SI 2 "small_int" "I")))]
  "TARGET_DEPRECATED_V8_INSNS && TARGET_ARCH64"
  "smul\\t%1, %2, %0"
  [(set_attr "length" "1")])

(define_expand "smulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "")
	(truncate:SI
	 (lshiftrt:DI (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" ""))
			       (sign_extend:DI (match_operand:SI 2 "arith_operand" "")))
		      (const_int 32))))]
  "TARGET_HARD_MUL && TARGET_ARCH32"
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

;; XXX
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
;; XXX
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
   smul\\t%1, %2, %0\\n\\tsrlx\\t%0, %3, %0
   smul\\t%1, %2, %4\\n\\tsrlx\\t%4, %3, %0"
  [(set_attr "length" "2")])

;; XXX
(define_insn "const_smulsi3_highpart_v8plus"
  [(set (match_operand:SI 0 "register_operand" "=h,r")
	(truncate:SI
	 (lshiftrt:DI (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r,r"))
			       (match_operand 2 "small_int" "i,i"))
		      (match_operand:SI 3 "const_int_operand" "i,i"))))
   (clobber (match_scratch:SI 4 "=X,&h"))]
  "TARGET_V8PLUS"
  "@
   smul\\t%1, %2, %0\\n\\tsrlx\\t%0, %3, %0
   smul\\t%1, %2, %4\\n\\tsrlx\\t%4, %3, %0"
  [(set_attr "length" "2")])

;; XXX
(define_insn "*smulsi3_highpart_sp32"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(truncate:SI
	 (lshiftrt:DI (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r"))
			       (sign_extend:DI (match_operand:SI 2 "register_operand" "r")))
		      (const_int 32))))]
  "TARGET_HARD_MUL32 && ! TARGET_LIVE_G0"
  "smul\\t%1, %2, %%g0\\n\\trd\\t%%y, %0"
  [(set_attr "length" "2")])

;; XXX
(define_insn "const_smulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(truncate:SI
	 (lshiftrt:DI (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r"))
			       (match_operand:SI 2 "register_operand" "r"))
		      (const_int 32))))]
  "TARGET_HARD_MUL32 && ! TARGET_LIVE_G0"
  "smul\\t%1, %2, %%g0\\n\\trd\\t%%y, %0"
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
	emit_insn (gen_const_umulsidi3_v8plus (operands[0], operands[1],
					       operands[2]));
      else
	emit_insn (gen_const_umulsidi3_sp32 (operands[0], operands[1],
					     operands[2]));
      DONE;
    }
  if (TARGET_V8PLUS)
    {
      emit_insn (gen_umulsidi3_v8plus (operands[0], operands[1], operands[2]));
      DONE;
    }
}")

;; XXX
(define_insn "umulsidi3_v8plus"
  [(set (match_operand:DI 0 "register_operand" "=h,r")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r,r"))
		 (zero_extend:DI (match_operand:SI 2 "register_operand" "r,r"))))
   (clobber (match_scratch:SI 3 "=X,&h"))]
  "TARGET_V8PLUS"
  "@
   umul\\t%1, %2, %L0\\n\\tsrlx\\t%L0, 32, %H0
   umul\\t%1, %2, %3\\n\\tsrlx\\t%3, 32, %H0\\n\\tmov\\t%3, %L0"
  [(set_attr "length" "2,3")])

;; XXX
(define_insn "*umulsidi3_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r"))
		 (zero_extend:DI (match_operand:SI 2 "register_operand" "r"))))]
  "TARGET_HARD_MUL32"
  "*
{
  return TARGET_SPARCLET ? \"umuld\\t%1, %2, %L0\" : \"umul\\t%1, %2, %L0\\n\\trd\\t%%y, %H0\";
}"
  [(set (attr "length")
	(if_then_else (eq_attr "isa" "sparclet")
		      (const_int 1) (const_int 2)))])

(define_insn "*umulsidi3_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r"))
		 (zero_extend:DI (match_operand:SI 2 "register_operand" "r"))))]
  "TARGET_DEPRECATED_V8_INSNS && TARGET_ARCH64"
  "umul\\t%1, %2, %0"
  [(set_attr "length" "1")])

;; Extra pattern, because sign_extend of a constant isn't valid.

;; XXX
(define_insn "const_umulsidi3_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r"))
		 (match_operand:SI 2 "uns_small_int" "")))]
  "TARGET_HARD_MUL32"
  "*
{
  return TARGET_SPARCLET ? \"umuld\\t%1, %2, %L0\" : \"umul\\t%1, %2, %L0\\n\\trd\\t%%y, %H0\";
}"
  [(set (attr "length")
	(if_then_else (eq_attr "isa" "sparclet")
		      (const_int 1) (const_int 2)))])

(define_insn "const_umulsidi3_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r"))
		 (match_operand:SI 2 "uns_small_int" "")))]
  "TARGET_DEPRECATED_V8_INSNS && TARGET_ARCH64"
  "umul\\t%1, %2, %0"
  [(set_attr "length" "1")])

;; XXX
(define_insn "const_umulsidi3_v8plus"
  [(set (match_operand:DI 0 "register_operand" "=h,r")
	(mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r,r"))
		 (match_operand:SI 2 "uns_small_int" "")))
   (clobber (match_scratch:SI 3 "=X,h"))]
  "TARGET_V8PLUS"
  "@
   umul\\t%1, %2, %L0\\n\\tsrlx\\t%L0, 32, %H0
   umul\\t%1, %2, %3\\n\\tsrlx\\t%3, 32, %H0\\n\\tmov\\t%3, %L0"
  [(set_attr "length" "2,3")])

(define_expand "umulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "")
	(truncate:SI
	 (lshiftrt:DI (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" ""))
			       (zero_extend:DI (match_operand:SI 2 "uns_arith_operand" "")))
		      (const_int 32))))]
  "TARGET_HARD_MUL && TARGET_ARCH32"
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

;; XXX
(define_insn "umulsi3_highpart_v8plus"
  [(set (match_operand:SI 0 "register_operand" "=h,r")
	(truncate:SI
	 (lshiftrt:DI (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r,r"))
			       (zero_extend:DI (match_operand:SI 2 "register_operand" "r,r")))
		      (match_operand:SI 3 "const_int_operand" "i,i"))))
   (clobber (match_scratch:SI 4 "=X,h"))]
  "TARGET_V8PLUS"
  "@
   umul\\t%1, %2, %0\\n\\tsrlx\\t%0, %3, %0
   umul\\t%1, %2, %4\\n\\tsrlx\\t%4, %3, %0"
  [(set_attr "length" "2")])

;; XXX
(define_insn "const_umulsi3_highpart_v8plus"
  [(set (match_operand:SI 0 "register_operand" "=h,r")
	(truncate:SI
	 (lshiftrt:DI (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r,r"))
			       (match_operand:SI 2 "uns_small_int" ""))
		      (match_operand:SI 3 "const_int_operand" "i,i"))))
   (clobber (match_scratch:SI 4 "=X,h"))]
  "TARGET_V8PLUS"
  "@
   umul\\t%1, %2, %0\\n\\tsrlx\\t%0, %3, %0
   umul\\t%1, %2, %4\\n\\tsrlx\\t%4, %3, %0"
  [(set_attr "length" "2")])

;; XXX
(define_insn "*umulsi3_highpart_sp32"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(truncate:SI
	 (lshiftrt:DI (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r"))
			       (zero_extend:DI (match_operand:SI 2 "register_operand" "r")))
		      (const_int 32))))]
  "TARGET_HARD_MUL32 && ! TARGET_LIVE_G0"
  "umul\\t%1, %2, %%g0\\n\\trd\\t%%y, %0"
  [(set_attr "length" "2")])

;; XXX
(define_insn "const_umulsi3_highpart"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(truncate:SI
	 (lshiftrt:DI (mult:DI (zero_extend:DI (match_operand:SI 1 "register_operand" "r"))
			       (match_operand:SI 2 "uns_small_int" ""))
		      (const_int 32))))]
  "TARGET_HARD_MUL32 && ! TARGET_LIVE_G0"
  "umul\\t%1, %2, %%g0\\n\\trd\\t%%y, %0"
  [(set_attr "length" "2")])

;; The v8 architecture specifies that there must be 3 instructions between
;; a y register write and a use of it for correct results.

(define_expand "divsi3"
  [(parallel [(set (match_operand:SI 0 "register_operand" "=r,r")
		   (div:SI (match_operand:SI 1 "register_operand" "r,r")
			   (match_operand:SI 2 "input_operand" "rI,m")))
	      (clobber (match_scratch:SI 3 "=&r,&r"))])]
  "TARGET_V8 || TARGET_DEPRECATED_V8_INSNS"
  "
{
  if (TARGET_ARCH64)
    {
      operands[3] = gen_reg_rtx(SImode);
      emit_insn (gen_ashrsi3 (operands[3], operands[1], GEN_INT (31)));
      emit_insn (gen_divsi3_sp64 (operands[0], operands[1], operands[2],
				  operands[3]));
      DONE;
    }
}")

(define_insn "divsi3_sp32"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(div:SI (match_operand:SI 1 "register_operand" "r,r")
		(match_operand:SI 2 "input_operand" "rI,m")))
   (clobber (match_scratch:SI 3 "=&r,&r"))]
  "(TARGET_V8 || TARGET_DEPRECATED_V8_INSNS)
   && TARGET_ARCH32"
  "*
{
  if (which_alternative == 0)
    if (TARGET_V9)
      return \"sra\\t%1, 31, %3\\n\\twr\\t%3, 0, %%y\\n\\tsdiv\\t%1, %2, %0\";
    else
      return \"sra\\t%1, 31, %3\\n\\twr\\t%3, 0, %%y\\n\\tnop\\n\\tnop\\n\\tnop\\n\\tsdiv\\t%1, %2, %0\";
  else
    if (TARGET_V9)
      return \"sra\\t%1, 31, %3\\n\\twr\\t%3, 0, %%y\\n\\tld\\t%2, %3\\n\\tsdiv\\t%1, %3, %0\";
    else
      return \"sra\\t%1, 31, %3\\n\\twr\\t%3, 0, %%y\\n\\tld\\t%2, %3\\n\\tnop\\n\\tnop\\n\\tsdiv\\t%1, %3, %0\";
}"
  [(set (attr "length")
	(if_then_else (eq_attr "isa" "v9")
		      (const_int 4) (const_int 7)))])

(define_insn "divsi3_sp64"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(div:SI (match_operand:SI 1 "register_operand" "r")
		(match_operand:SI 2 "input_operand" "rI")))
   (use (match_operand:SI 3 "register_operand" "r"))]
  "TARGET_DEPRECATED_V8_INSNS && TARGET_ARCH64"
  "wr\\t%%g0, %3, %%y\\n\\tsdiv\\t%1, %2, %0"
  [(set_attr "length" "2")])

(define_insn "divdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(div:DI (match_operand:DI 1 "register_operand" "r")
		(match_operand:DI 2 "arith_double_operand" "rHI")))]
  "TARGET_ARCH64"
  "sdivx\\t%1, %2, %0")

(define_insn "*cmp_sdiv_cc_set"
  [(set (reg:CC 100)
	(compare:CC (div:SI (match_operand:SI 1 "register_operand" "r")
			    (match_operand:SI 2 "arith_operand" "rI"))
		    (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(div:SI (match_dup 1) (match_dup 2)))
   (clobber (match_scratch:SI 3 "=&r"))]
  "TARGET_V8 || TARGET_DEPRECATED_V8_INSNS"
  "*
{
  if (TARGET_V9)
    return \"sra\\t%1, 31, %3\\n\\twr\\t%3, 0, %%y\\n\\tsdivcc\\t%1, %2, %0\";
  else
    return \"sra\\t%1, 31, %3\\n\\twr\\t%3, 0, %%y\\n\\tnop\\n\\tnop\\n\\tnop\\n\\tsdivcc\\t%1, %2, %0\";
}"
  [(set (attr "length")
	(if_then_else (eq_attr "isa" "v9")
		      (const_int 3) (const_int 6)))])

;; XXX
(define_expand "udivsi3"
  [(set (match_operand:SI 0 "register_operand" "")
	(udiv:SI (match_operand:SI 1 "reg_or_nonsymb_mem_operand" "")
		 (match_operand:SI 2 "input_operand" "")))]
  "(TARGET_V8 || TARGET_DEPRECATED_V8_INSNS) && ! TARGET_LIVE_G0"
  "")

(define_insn "udivsi3_sp32"
  [(set (match_operand:SI 0 "register_operand" "=r,&r,&r")
	(udiv:SI (match_operand:SI 1 "reg_or_nonsymb_mem_operand" "r,r,m")
		 (match_operand:SI 2 "input_operand" "rI,m,r")))]
  "(TARGET_V8
    || TARGET_DEPRECATED_V8_INSNS)
   && TARGET_ARCH32 && ! TARGET_LIVE_G0"
  "*
{
  output_asm_insn (\"wr\\t%%g0, %%g0, %%y\", operands);
  switch (which_alternative)
    {
    default:
      return \"nop\\n\\tnop\\n\\tnop\\n\\tudiv\\t%1, %2, %0\";
    case 1:
      return \"ld\\t%2, %0\\n\\tnop\\n\\tnop\\n\\tudiv\\t%1, %0, %0\";
    case 2:
      return \"ld\\t%1, %0\\n\\tnop\\n\\tnop\\n\\tudiv\\t%0, %2, %0\";
    }
}"
  [(set_attr "length" "5")])

(define_insn "udivsi3_sp64"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(udiv:SI (match_operand:SI 1 "reg_or_nonsymb_mem_operand" "r")
		 (match_operand:SI 2 "input_operand" "rI")))]
  "TARGET_DEPRECATED_V8_INSNS && TARGET_ARCH64"
  "wr\\t%%g0, 0, %%y\\n\\tudiv\\t%1, %2, %0"
  [(set_attr "length" "2")])

(define_insn "udivdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(udiv:DI (match_operand:DI 1 "register_operand" "r")
		 (match_operand:DI 2 "arith_double_operand" "rHI")))]
  "TARGET_ARCH64"
  "udivx\\t%1, %2, %0")

(define_insn "*cmp_udiv_cc_set"
  [(set (reg:CC 100)
	(compare:CC (udiv:SI (match_operand:SI 1 "register_operand" "r")
			     (match_operand:SI 2 "arith_operand" "rI"))
		    (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(udiv:SI (match_dup 1) (match_dup 2)))]
  "(TARGET_V8
    || TARGET_DEPRECATED_V8_INSNS)
   && ! TARGET_LIVE_G0"
  "*
{
  if (TARGET_V9)
    return \"wr\\t%%g0, %%g0, %%y\\n\\tudivcc\\t%1, %2, %0\";
  else
    return \"wr\\t%%g0, %%g0, %%y\\n\\tnop\\n\\tnop\\n\\tnop\\n\\tudivcc\\t%1, %2, %0\";
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
  "smac\\t%1, %2, %0"
  [(set_attr "type" "imul")
   (set_attr "length" "1")])

(define_insn "*smacdi"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI (mult:DI (sign_extend:DI
			   (match_operand:SI 1 "register_operand" "%r"))
			  (sign_extend:DI
			   (match_operand:SI 2 "register_operand" "r")))
		 (match_operand:DI 3 "register_operand" "0")))]
  "TARGET_SPARCLET"
  "smacd\\t%1, %2, %L0"
  [(set_attr "type" "imul")
   (set_attr "length" "1")])

(define_insn "*umacdi"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(plus:DI (mult:DI (zero_extend:DI
			   (match_operand:SI 1 "register_operand" "%r"))
			  (zero_extend:DI
			   (match_operand:SI 2 "register_operand" "r")))
		 (match_operand:DI 3 "register_operand" "0")))]
  "TARGET_SPARCLET"
  "umacd\\t%1, %2, %L0"
  [(set_attr "type" "imul")
   (set_attr "length" "1")])

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
  "@
  #
  fand\\t%1, %2, %0"
  [(set_attr "type" "ialu,fp")
   (set_attr "length" "2,1")])

(define_insn "*anddi3_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r,b")
	(and:DI (match_operand:DI 1 "arith_double_operand" "%r,b")
		(match_operand:DI 2 "arith_double_operand" "rHI,b")))]
  "TARGET_ARCH64"
  "@
   and\\t%1, %2, %0
   fand\\t%1, %2, %0"
  [(set_attr "type" "ialu,fp")
   (set_attr "length" "1,1")])

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,d")
	(and:SI (match_operand:SI 1 "arith_operand" "%r,d")
		(match_operand:SI 2 "arith_operand" "rI,d")))]
  ""
  "@
   and\\t%1, %2, %0
   fands\\t%1, %2, %0"
  [(set_attr "type" "ialu,fp")
   (set_attr "length" "1,1")])

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
  "! TARGET_ARCH64
   && reload_completed
   && ((GET_CODE (operands[0]) == REG
        && REGNO (operands[0]) < 32)
       || (GET_CODE (operands[0]) == SUBREG
           && GET_CODE (SUBREG_REG (operands[0])) == REG
           && REGNO (SUBREG_REG (operands[0])) < 32))"
  [(set (match_dup 4) (match_op_dup:SI 1 [(match_dup 6) (match_dup 8)]))
   (set (match_dup 5) (match_op_dup:SI 1 [(match_dup 7) (match_dup 9)]))]
  "
{
  if (GET_CODE (operands[0]) == SUBREG)
    operands[0] = alter_subreg (operands[0]);
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
   #
   fandnot1\\t%1, %2, %0"
  [(set_attr "type" "ialu,fp")
   (set_attr "length" "2,1")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
        (and:DI (not:DI (match_operand:DI 1 "register_operand" ""))
                (match_operand:DI 2 "register_operand" "")))]
  "! TARGET_ARCH64
   && reload_completed
   && ((GET_CODE (operands[0]) == REG
        && REGNO (operands[0]) < 32)
       || (GET_CODE (operands[0]) == SUBREG
           && GET_CODE (SUBREG_REG (operands[0])) == REG
           && REGNO (SUBREG_REG (operands[0])) < 32))"
  [(set (match_dup 3) (and:SI (not:SI (match_dup 4)) (match_dup 5)))
   (set (match_dup 6) (and:SI (not:SI (match_dup 7)) (match_dup 8)))]
  "if (GET_CODE (operands[0]) == SUBREG)
     operands[0] = alter_subreg (operands[0]);
   operands[3] = gen_highpart (SImode, operands[0]);
   operands[4] = gen_highpart (SImode, operands[1]);
   operands[5] = gen_highpart (SImode, operands[2]);
   operands[6] = gen_lowpart (SImode, operands[0]);
   operands[7] = gen_lowpart (SImode, operands[1]);
   operands[8] = gen_lowpart (SImode, operands[2]);")

(define_insn "*and_not_di_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r,b")
	(and:DI (not:DI (match_operand:DI 1 "register_operand" "r,b"))
		(match_operand:DI 2 "register_operand" "r,b")))]
  "TARGET_ARCH64"
  "@
   andn\\t%2, %1, %0
   fandnot1\\t%1, %2, %0"
  [(set_attr "type" "ialu,fp")
   (set_attr "length" "1,1")])

(define_insn "*and_not_si"
  [(set (match_operand:SI 0 "register_operand" "=r,d")
	(and:SI (not:SI (match_operand:SI 1 "register_operand" "r,d"))
		(match_operand:SI 2 "register_operand" "r,d")))]
  ""
  "@
   andn\\t%2, %1, %0
   fandnot1s\\t%1, %2, %0"
  [(set_attr "type" "ialu,fp")
   (set_attr "length" "1,1")])

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
  "@
  #
  for\\t%1, %2, %0"
  [(set_attr "type" "ialu,fp")
   (set_attr "length" "2,1")])

(define_insn "*iordi3_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r,b")
	(ior:DI (match_operand:DI 1 "arith_double_operand" "%r,b")
		(match_operand:DI 2 "arith_double_operand" "rHI,b")))]
  "TARGET_ARCH64"
  "@
  or\\t%1, %2, %0
  for\\t%1, %2, %0"
  [(set_attr "type" "ialu,fp")
   (set_attr "length" "1,1")])

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,d")
	(ior:SI (match_operand:SI 1 "arith_operand" "%r,d")
		(match_operand:SI 2 "arith_operand" "rI,d")))]
  ""
  "@
   or\\t%1, %2, %0
   fors\\t%1, %2, %0"
  [(set_attr "type" "ialu,fp")
   (set_attr "length" "1,1")])

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
   #
   fornot1\\t%1, %2, %0"
  [(set_attr "type" "ialu,fp")
   (set_attr "length" "2,1")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
        (ior:DI (not:DI (match_operand:DI 1 "register_operand" ""))
                (match_operand:DI 2 "register_operand" "")))]
  "! TARGET_ARCH64
   && reload_completed
   && ((GET_CODE (operands[0]) == REG
        && REGNO (operands[0]) < 32)
       || (GET_CODE (operands[0]) == SUBREG
           && GET_CODE (SUBREG_REG (operands[0])) == REG
           && REGNO (SUBREG_REG (operands[0])) < 32))"
  [(set (match_dup 3) (ior:SI (not:SI (match_dup 4)) (match_dup 5)))
   (set (match_dup 6) (ior:SI (not:SI (match_dup 7)) (match_dup 8)))]
  "if (GET_CODE (operands[0]) == SUBREG)
     operands[0] = alter_subreg (operands[0]);
   operands[3] = gen_highpart (SImode, operands[0]);
   operands[4] = gen_highpart (SImode, operands[1]);
   operands[5] = gen_highpart (SImode, operands[2]);
   operands[6] = gen_lowpart (SImode, operands[0]);
   operands[7] = gen_lowpart (SImode, operands[1]);
   operands[8] = gen_lowpart (SImode, operands[2]);")

(define_insn "*or_not_di_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r,b")
	(ior:DI (not:DI (match_operand:DI 1 "register_operand" "r,b"))
		(match_operand:DI 2 "register_operand" "r,b")))]
  "TARGET_ARCH64"
  "@
  orn\\t%2, %1, %0
  fornot1\\t%1, %2, %0"
  [(set_attr "type" "ialu,fp")
   (set_attr "length" "1,1")])

(define_insn "*or_not_si"
  [(set (match_operand:SI 0 "register_operand" "=r,d")
	(ior:SI (not:SI (match_operand:SI 1 "register_operand" "r,d"))
		(match_operand:SI 2 "register_operand" "r,d")))]
  ""
  "@
   orn\\t%2, %1, %0
   fornot1s\\t%1, %2, %0"
  [(set_attr "type" "ialu,fp")
   (set_attr "length" "1,1")])

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
  "@
  #
  fxor\\t%1, %2, %0"
  [(set_attr "length" "2,1")
   (set_attr "type" "ialu,fp")])

(define_insn "*xordi3_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r,b")
	(xor:DI (match_operand:DI 1 "arith_double_operand" "%rJ,b")
		(match_operand:DI 2 "arith_double_operand" "rHI,b")))]
  "TARGET_ARCH64"
  "@
  xor\\t%r1, %2, %0
  fxor\\t%1, %2, %0"
  [(set_attr "type" "ialu,fp")
   (set_attr "length" "1,1")])

(define_insn "*xordi3_sp64_dbl"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(xor:DI (match_operand:DI 1 "register_operand" "r")
		(match_operand:DI 2 "const64_operand" "")))]
  "(TARGET_ARCH64
    && HOST_BITS_PER_WIDE_INT != 64)"
  "xor\\t%1, %2, %0"
  [(set_attr "type" "ialu")
   (set_attr "length" "1")])

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,d")
	(xor:SI (match_operand:SI 1 "arith_operand" "%rJ,d")
		(match_operand:SI 2 "arith_operand" "rI,d")))]
  ""
  "@
   xor\\t%r1, %2, %0
   fxors\\t%1, %2, %0"
  [(set_attr "type" "ialu,fp")
   (set_attr "length" "1,1")])

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
   #
   fxnor\\t%1, %2, %0"
  [(set_attr "length" "2,1")
   (set_attr "type" "ialu,fp")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
        (not:DI (xor:DI (match_operand:DI 1 "register_operand" "")
                        (match_operand:DI 2 "register_operand" ""))))]
  "! TARGET_ARCH64
   && reload_completed
   && ((GET_CODE (operands[0]) == REG
        && REGNO (operands[0]) < 32)
       || (GET_CODE (operands[0]) == SUBREG
           && GET_CODE (SUBREG_REG (operands[0])) == REG
           && REGNO (SUBREG_REG (operands[0])) < 32))"
  [(set (match_dup 3) (not:SI (xor:SI (match_dup 4) (match_dup 5))))
   (set (match_dup 6) (not:SI (xor:SI (match_dup 7) (match_dup 8))))]
  "if (GET_CODE (operands[0]) == SUBREG)
     operands[0] = alter_subreg (operands[0]);
   operands[3] = gen_highpart (SImode, operands[0]);
   operands[4] = gen_highpart (SImode, operands[1]);
   operands[5] = gen_highpart (SImode, operands[2]);
   operands[6] = gen_lowpart (SImode, operands[0]);
   operands[7] = gen_lowpart (SImode, operands[1]);
   operands[8] = gen_lowpart (SImode, operands[2]);")

(define_insn "*xor_not_di_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r,b")
	(not:DI (xor:DI (match_operand:DI 1 "reg_or_0_operand" "rJ,b")
			(match_operand:DI 2 "arith_double_operand" "rHI,b"))))]
  "TARGET_ARCH64"
  "@
  xnor\\t%r1, %2, %0
  fxnor\\t%1, %2, %0"
  [(set_attr "type" "ialu,fp")
   (set_attr "length" "1,1")])

(define_insn "*xor_not_si"
  [(set (match_operand:SI 0 "register_operand" "=r,d")
	(not:SI (xor:SI (match_operand:SI 1 "reg_or_0_operand" "rJ,d")
			(match_operand:SI 2 "arith_operand" "rI,d"))))]
  ""
  "@
   xnor\\t%r1, %2, %0
   fxnors\\t%1, %2, %0"
  [(set_attr "type" "ialu,fp")
   (set_attr "length" "1,1")])

;; These correspond to the above in the case where we also (or only)
;; want to set the condition code.  

(define_insn "*cmp_cc_arith_op"
  [(set (reg:CC 100)
	(compare:CC
	 (match_operator:SI 2 "cc_arithop"
			    [(match_operand:SI 0 "arith_operand" "%r")
			     (match_operand:SI 1 "arith_operand" "rI")])
	 (const_int 0)))]
  "! TARGET_LIVE_G0"
  "%A2cc\\t%0, %1, %%g0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_ccx_arith_op"
  [(set (reg:CCX 100)
	(compare:CCX
	 (match_operator:DI 2 "cc_arithop"
			    [(match_operand:DI 0 "arith_double_operand" "%r")
			     (match_operand:DI 1 "arith_double_operand" "rHI")])
	 (const_int 0)))]
  "TARGET_ARCH64"
  "%A2cc\\t%0, %1, %%g0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

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
  "%A3cc\\t%1, %2, %0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

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
  "%A3cc\\t%1, %2, %0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_cc_xor_not"
  [(set (reg:CC 100)
	(compare:CC
	 (not:SI (xor:SI (match_operand:SI 0 "reg_or_0_operand" "%rJ")
			 (match_operand:SI 1 "arith_operand" "rI")))
	 (const_int 0)))]
  "! TARGET_LIVE_G0"
  "xnorcc\\t%r0, %1, %%g0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_ccx_xor_not"
  [(set (reg:CCX 100)
	(compare:CCX
	 (not:DI (xor:DI (match_operand:DI 0 "reg_or_0_operand" "%rJ")
			 (match_operand:DI 1 "arith_double_operand" "rHI")))
	 (const_int 0)))]
  "TARGET_ARCH64"
  "xnorcc\\t%r0, %1, %%g0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_cc_xor_not_set"
  [(set (reg:CC 100)
	(compare:CC
	 (not:SI (xor:SI (match_operand:SI 1 "reg_or_0_operand" "%rJ")
			 (match_operand:SI 2 "arith_operand" "rI")))
	 (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(not:SI (xor:SI (match_dup 1) (match_dup 2))))]
  ""
  "xnorcc\\t%r1, %2, %0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_ccx_xor_not_set"
  [(set (reg:CCX 100)
	(compare:CCX
	 (not:DI (xor:DI (match_operand:DI 1 "reg_or_0_operand" "%rJ")
			 (match_operand:DI 2 "arith_double_operand" "rHI")))
	 (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(not:DI (xor:DI (match_dup 1) (match_dup 2))))]
  "TARGET_ARCH64"
  "xnorcc\\t%r1, %2, %0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_cc_arith_op_not"
  [(set (reg:CC 100)
	(compare:CC
	 (match_operator:SI 2 "cc_arithopn"
			    [(not:SI (match_operand:SI 0 "arith_operand" "rI"))
			     (match_operand:SI 1 "reg_or_0_operand" "rJ")])
	 (const_int 0)))]
  "! TARGET_LIVE_G0"
  "%B2cc\\t%r1, %0, %%g0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_ccx_arith_op_not"
  [(set (reg:CCX 100)
	(compare:CCX
	 (match_operator:DI 2 "cc_arithopn"
			    [(not:DI (match_operand:DI 0 "arith_double_operand" "rHI"))
			     (match_operand:DI 1 "reg_or_0_operand" "rJ")])
	 (const_int 0)))]
  "TARGET_ARCH64"
  "%B2cc\\t%r1, %0, %%g0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

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
  "%B3cc\\t%r2, %1, %0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

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
  "%B3cc\\t%r2, %1, %0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

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
      emit_insn (gen_rtx_PARALLEL
		 (VOIDmode,
		  gen_rtvec (2,
			     gen_rtx_SET (VOIDmode, operand0,
					  gen_rtx_NEG (DImode, operand1)),
			     gen_rtx_CLOBBER (VOIDmode,
					      gen_rtx_REG (CCmode,
							   SPARC_ICC_REG)))));
      DONE;
    }
}")

(define_insn "*negdi2_sp32"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(neg:DI (match_operand:DI 1 "register_operand" "r")))
   (clobber (reg:CC 100))]
  "! TARGET_ARCH64
   && ! TARGET_LIVE_G0"
  "#"
  [(set_attr "type" "unary")
   (set_attr "length" "2")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
        (neg:DI (match_operand:DI 1 "register_operand" "")))
   (clobber (reg:CC 100))]
  "! TARGET_ARCH64
   && ! TARGET_LIVE_G0
   && reload_completed"
  [(parallel [(set (reg:CC_NOOV 100)
                   (compare:CC_NOOV (minus:SI (const_int 0) (match_dup 5))
                                    (const_int 0)))
              (set (match_dup 4) (minus:SI (const_int 0) (match_dup 5)))])
   (set (match_dup 2) (minus:SI (minus:SI (const_int 0) (match_dup 3))
                                (ltu:SI (reg:CC 100) (const_int 0))))]
  "operands[2] = gen_highpart (SImode, operands[0]);
   operands[3] = gen_highpart (SImode, operands[1]);
   operands[4] = gen_lowpart (SImode, operands[0]);
   operands[5] = gen_lowpart (SImode, operands[1]);")

(define_insn "*negdi2_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(neg:DI (match_operand:DI 1 "register_operand" "r")))]
  "TARGET_ARCH64"
  "sub\\t%%g0, %1, %0"
  [(set_attr "type" "unary")
   (set_attr "length" "1")])

(define_expand "negsi2"
  [(set (match_operand:SI 0 "register_operand" "")
        (neg:SI (match_operand:SI 1 "arith_operand" "")))]
  ""
  "
{
  if (TARGET_LIVE_G0)
    {
      rtx zero_reg = gen_reg_rtx (SImode);

      emit_insn (gen_rtx_SET (VOIDmode, zero_reg, const0_rtx));
      emit_insn (gen_rtx_SET (VOIDmode, operands[0],
                                      gen_rtx_MINUS (SImode, zero_reg,
                                                             operands[1])));
      DONE;
    }
}")

(define_insn "*negsi2_not_liveg0"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (match_operand:SI 1 "arith_operand" "rI")))]
  "! TARGET_LIVE_G0"
  "sub\\t%%g0, %1, %0"
  [(set_attr "type" "unary")
   (set_attr "length" "1")])

(define_insn "*cmp_cc_neg"
  [(set (reg:CC_NOOV 100)
	(compare:CC_NOOV (neg:SI (match_operand:SI 0 "arith_operand" "rI"))
			 (const_int 0)))]
  "! TARGET_LIVE_G0"
  "subcc\\t%%g0, %0, %%g0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_ccx_neg"
  [(set (reg:CCX_NOOV 100)
	(compare:CCX_NOOV (neg:DI (match_operand:DI 0 "arith_double_operand" "rHI"))
			  (const_int 0)))]
  "TARGET_ARCH64"
  "subcc\\t%%g0, %0, %%g0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_cc_set_neg"
  [(set (reg:CC_NOOV 100)
	(compare:CC_NOOV (neg:SI (match_operand:SI 1 "arith_operand" "rI"))
			 (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (match_dup 1)))]
  "! TARGET_LIVE_G0"
  "subcc\\t%%g0, %1, %0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_ccx_set_neg"
  [(set (reg:CCX_NOOV 100)
	(compare:CCX_NOOV (neg:DI (match_operand:DI 1 "arith_double_operand" "rHI"))
			  (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(neg:DI (match_dup 1)))]
  "TARGET_ARCH64"
  "subcc\\t%%g0, %1, %0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

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
   #
   fnot1\\t%1, %0"
  [(set_attr "type" "unary,fp")
   (set_attr "length" "2,1")])

(define_split
  [(set (match_operand:DI 0 "register_operand" "")
        (not:DI (match_operand:DI 1 "register_operand" "")))]
  "! TARGET_ARCH64
   && reload_completed
   && ((GET_CODE (operands[0]) == REG
        && REGNO (operands[0]) < 32)
       || (GET_CODE (operands[0]) == SUBREG
           && GET_CODE (SUBREG_REG (operands[0])) == REG
           && REGNO (SUBREG_REG (operands[0])) < 32))"
  [(set (match_dup 2) (not:SI (xor:SI (match_dup 3) (const_int 0))))
   (set (match_dup 4) (not:SI (xor:SI (match_dup 5) (const_int 0))))]
  "if (GET_CODE (operands[0]) == SUBREG)
     operands[0] = alter_subreg (operands[0]);
   operands[2] = gen_highpart (SImode, operands[0]);
   operands[3] = gen_highpart (SImode, operands[1]);
   operands[4] = gen_lowpart (SImode, operands[0]);
   operands[5] = gen_lowpart (SImode, operands[1]);")

(define_insn "*one_cmpldi2_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r,b")
	(not:DI (match_operand:DI 1 "arith_double_operand" "rHI,b")))]
  "TARGET_ARCH64"
  "@
   xnor\\t%%g0, %1, %0
   fnot1\\t%1, %0"
  [(set_attr "type" "unary,fp")
   (set_attr "length" "1")])

(define_expand "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "")
        (not:SI (match_operand:SI 1 "arith_operand" "")))]
  ""
  "
{
  if (TARGET_LIVE_G0
      && GET_CODE (operands[1]) == CONST_INT)
    {
      rtx zero_reg = gen_reg_rtx (SImode);

      emit_insn (gen_rtx_SET (VOIDmode, zero_reg, const0_rtx));
      emit_insn (gen_rtx_SET (VOIDmode,
                              operands[0],
                              gen_rtx_NOT (SImode,
                                           gen_rtx_XOR (SImode,
                                                        zero_reg,
                                                        operands[1]))));
      DONE;
    }
}")

(define_insn "*one_cmplsi2_not_liveg0"
  [(set (match_operand:SI 0 "register_operand" "=r,d")
	(not:SI (match_operand:SI 1 "arith_operand" "rI,d")))]
  "! TARGET_LIVE_G0"
  "@
  xnor\\t%%g0, %1, %0
  fnot1s\\t%1, %0"
  [(set_attr "type" "unary,fp")
   (set_attr "length" "1,1")])

(define_insn "*one_cmplsi2_liveg0"
  [(set (match_operand:SI 0 "register_operand" "=r,d")
	(not:SI (match_operand:SI 1 "arith_operand" "r,d")))]
  "TARGET_LIVE_G0"
  "@
  xnor\\t%1, 0, %0
  fnot1s\\t%1, %0"
  [(set_attr "type" "unary,fp")
   (set_attr "length" "1,1")])

(define_insn "*cmp_cc_not"
  [(set (reg:CC 100)
	(compare:CC (not:SI (match_operand:SI 0 "arith_operand" "rI"))
		    (const_int 0)))]
  "! TARGET_LIVE_G0"
  "xnorcc\\t%%g0, %0, %%g0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_ccx_not"
  [(set (reg:CCX 100)
	(compare:CCX (not:DI (match_operand:DI 0 "arith_double_operand" "rHI"))
		     (const_int 0)))]
  "TARGET_ARCH64"
  "xnorcc\\t%%g0, %0, %%g0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_cc_set_not"
  [(set (reg:CC 100)
	(compare:CC (not:SI (match_operand:SI 1 "arith_operand" "rI"))
		    (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(not:SI (match_dup 1)))]
  "! TARGET_LIVE_G0"
  "xnorcc\\t%%g0, %1, %0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_ccx_set_not"
  [(set (reg:CCX 100)
	(compare:CCX (not:DI (match_operand:DI 1 "arith_double_operand" "rHI"))
		    (const_int 0)))
   (set (match_operand:DI 0 "register_operand" "=r")
	(not:DI (match_dup 1)))]
  "TARGET_ARCH64"
  "xnorcc\\t%%g0, %1, %0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

;; Floating point arithmetic instructions.

(define_expand "addtf3"
  [(set (match_operand:TF 0 "nonimmediate_operand" "")
	(plus:TF (match_operand:TF 1 "general_operand" "")
		 (match_operand:TF 2 "general_operand" "")))]
  "TARGET_FPU && (TARGET_HARD_QUAD || TARGET_ARCH64)"
  "
{
  if (! TARGET_HARD_QUAD)
    {
      rtx slot0, slot1, slot2;

      if (GET_CODE (operands[0]) != MEM)
	slot0 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode), 0);
      else
	slot0 = operands[0];
      if (GET_CODE (operands[1]) != MEM)
	{
	  slot1 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode), 0);
	  emit_insn (gen_rtx_SET (VOIDmode, slot1, operands[1]));
	}
      else
	slot1 = operands[1];
      if (GET_CODE (operands[2]) != MEM)
	{
	  slot2 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode), 0);
	  emit_insn (gen_rtx_SET (VOIDmode, slot2, operands[2]));
	}
      else
	slot2 = operands[2];

      emit_library_call (gen_rtx (SYMBOL_REF, Pmode, \"_Qp_add\"), 0,
			 VOIDmode, 3,
			 XEXP (slot0, 0), Pmode,
			 XEXP (slot1, 0), Pmode,
			 XEXP (slot2, 0), Pmode);

      if (GET_CODE (operands[0]) != MEM)
	emit_insn (gen_rtx_SET (VOIDmode, operands[0], slot0));
      DONE;
    }
}")

(define_insn "*addtf3_hq"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(plus:TF (match_operand:TF 1 "register_operand" "e")
		 (match_operand:TF 2 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "faddq\\t%1, %2, %0"
  [(set_attr "type" "fp")
   (set_attr "length" "1")])

(define_insn "adddf3"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(plus:DF (match_operand:DF 1 "register_operand" "e")
		 (match_operand:DF 2 "register_operand" "e")))]
  "TARGET_FPU"
  "faddd\\t%1, %2, %0"
  [(set_attr "type" "fp")
   (set_attr "length" "1")])

(define_insn "addsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(plus:SF (match_operand:SF 1 "register_operand" "f")
		 (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_FPU"
  "fadds\\t%1, %2, %0"
  [(set_attr "type" "fp")
   (set_attr "length" "1")])

(define_expand "subtf3"
  [(set (match_operand:TF 0 "nonimmediate_operand" "")
	(minus:TF (match_operand:TF 1 "general_operand" "")
		  (match_operand:TF 2 "general_operand" "")))]
  "TARGET_FPU && (TARGET_HARD_QUAD || TARGET_ARCH64)"
  "
{
  if (! TARGET_HARD_QUAD)
    {
      rtx slot0, slot1, slot2;

      if (GET_CODE (operands[0]) != MEM)
	slot0 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode), 0);
      else
	slot0 = operands[0];
      if (GET_CODE (operands[1]) != MEM)
	{
	  slot1 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode), 0);
	  emit_insn (gen_rtx_SET (VOIDmode, slot1, operands[1]));
	}
      else
	slot1 = operands[1];
      if (GET_CODE (operands[2]) != MEM)
	{
	  slot2 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode), 0);
	  emit_insn (gen_rtx_SET (VOIDmode, slot2, operands[2]));
	}
      else
	slot2 = operands[2];

      emit_library_call (gen_rtx (SYMBOL_REF, Pmode, \"_Qp_sub\"), 0,
			 VOIDmode, 3,
			 XEXP (slot0, 0), Pmode,
			 XEXP (slot1, 0), Pmode,
			 XEXP (slot2, 0), Pmode);

      if (GET_CODE (operands[0]) != MEM)
	emit_insn (gen_rtx_SET (VOIDmode, operands[0], slot0));
      DONE;
    }
}")

(define_insn "*subtf3_hq"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(minus:TF (match_operand:TF 1 "register_operand" "e")
		  (match_operand:TF 2 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fsubq\\t%1, %2, %0"
  [(set_attr "type" "fp")
   (set_attr "length" "1")])

(define_insn "subdf3"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(minus:DF (match_operand:DF 1 "register_operand" "e")
		  (match_operand:DF 2 "register_operand" "e")))]
  "TARGET_FPU"
  "fsubd\\t%1, %2, %0"
  [(set_attr "type" "fp")
   (set_attr "length" "1")])

(define_insn "subsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(minus:SF (match_operand:SF 1 "register_operand" "f")
		  (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_FPU"
  "fsubs\\t%1, %2, %0"
  [(set_attr "type" "fp")
   (set_attr "length" "1")])

(define_expand "multf3"
  [(set (match_operand:TF 0 "nonimmediate_operand" "")
	(mult:TF (match_operand:TF 1 "general_operand" "")
		 (match_operand:TF 2 "general_operand" "")))]
  "TARGET_FPU && (TARGET_HARD_QUAD || TARGET_ARCH64)"
  "
{
  if (! TARGET_HARD_QUAD)
    {
      rtx slot0, slot1, slot2;

      if (GET_CODE (operands[0]) != MEM)
	slot0 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode), 0);
      else
	slot0 = operands[0];
      if (GET_CODE (operands[1]) != MEM)
	{
	  slot1 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode), 0);
	  emit_insn (gen_rtx_SET (VOIDmode, slot1, operands[1]));
	}
      else
	slot1 = operands[1];
      if (GET_CODE (operands[2]) != MEM)
	{
	  slot2 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode), 0);
	  emit_insn (gen_rtx_SET (VOIDmode, slot2, operands[2]));
	}
      else
	slot2 = operands[2];

      emit_library_call (gen_rtx (SYMBOL_REF, Pmode, \"_Qp_mul\"), 0,
			 VOIDmode, 3,
			 XEXP (slot0, 0), Pmode,
			 XEXP (slot1, 0), Pmode,
			 XEXP (slot2, 0), Pmode);

      if (GET_CODE (operands[0]) != MEM)
	emit_insn (gen_rtx_SET (VOIDmode, operands[0], slot0));
      DONE;
    }
}")

(define_insn "*multf3_hq"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(mult:TF (match_operand:TF 1 "register_operand" "e")
		 (match_operand:TF 2 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fmulq\\t%1, %2, %0"
  [(set_attr "type" "fpmul")
   (set_attr "length" "1")])

(define_insn "muldf3"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(mult:DF (match_operand:DF 1 "register_operand" "e")
		 (match_operand:DF 2 "register_operand" "e")))]
  "TARGET_FPU"
  "fmuld\\t%1, %2, %0"
  [(set_attr "type" "fpmul")
   (set_attr "length" "1")])

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(mult:SF (match_operand:SF 1 "register_operand" "f")
		 (match_operand:SF 2 "register_operand" "f")))]
  "TARGET_FPU"
  "fmuls\\t%1, %2, %0"
  [(set_attr "type" "fpmul")
   (set_attr "length" "1")])

(define_insn "*muldf3_extend"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(mult:DF (float_extend:DF (match_operand:SF 1 "register_operand" "f"))
		 (float_extend:DF (match_operand:SF 2 "register_operand" "f"))))]
  "(TARGET_V8 || TARGET_V9) && TARGET_FPU"
  "fsmuld\\t%1, %2, %0"
  [(set_attr "type" "fpmul")
   (set_attr "length" "1")])

(define_insn "*multf3_extend"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(mult:TF (float_extend:TF (match_operand:DF 1 "register_operand" "e"))
		 (float_extend:TF (match_operand:DF 2 "register_operand" "e"))))]
  "(TARGET_V8 || TARGET_V9) && TARGET_FPU && TARGET_HARD_QUAD"
  "fdmulq\\t%1, %2, %0"
  [(set_attr "type" "fpmul")
   (set_attr "length" "1")])

(define_expand "divtf3"
  [(set (match_operand:TF 0 "nonimmediate_operand" "")
	(div:TF (match_operand:TF 1 "general_operand" "")
		(match_operand:TF 2 "general_operand" "")))]
  "TARGET_FPU && (TARGET_HARD_QUAD || TARGET_ARCH64)"
  "
{
  if (! TARGET_HARD_QUAD)
    {
      rtx slot0, slot1, slot2;

      if (GET_CODE (operands[0]) != MEM)
	slot0 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode), 0);
      else
	slot0 = operands[0];
      if (GET_CODE (operands[1]) != MEM)
	{
	  slot1 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode), 0);
	  emit_insn (gen_rtx_SET (VOIDmode, slot1, operands[1]));
	}
      else
	slot1 = operands[1];
      if (GET_CODE (operands[2]) != MEM)
	{
	  slot2 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode), 0);
	  emit_insn (gen_rtx_SET (VOIDmode, slot2, operands[2]));
	}
      else
	slot2 = operands[2];

      emit_library_call (gen_rtx (SYMBOL_REF, Pmode, \"_Qp_div\"), 0,
			 VOIDmode, 3,
			 XEXP (slot0, 0), Pmode,
			 XEXP (slot1, 0), Pmode,
			 XEXP (slot2, 0), Pmode);

      if (GET_CODE (operands[0]) != MEM)
	emit_insn (gen_rtx_SET (VOIDmode, operands[0], slot0));
      DONE;
    }
}")

;; don't have timing for quad-prec. divide.
(define_insn "*divtf3_hq"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(div:TF (match_operand:TF 1 "register_operand" "e")
		(match_operand:TF 2 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fdivq\\t%1, %2, %0"
  [(set_attr "type" "fpdivd")
   (set_attr "length" "1")])

(define_insn "divdf3"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(div:DF (match_operand:DF 1 "register_operand" "e")
		(match_operand:DF 2 "register_operand" "e")))]
  "TARGET_FPU"
  "fdivd\\t%1, %2, %0"
  [(set_attr "type" "fpdivd")
   (set_attr "length" "1")])

(define_insn "divsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(div:SF (match_operand:SF 1 "register_operand" "f")
		(match_operand:SF 2 "register_operand" "f")))]
  "TARGET_FPU"
  "fdivs\\t%1, %2, %0"
  [(set_attr "type" "fpdivs")
   (set_attr "length" "1")])

(define_expand "negtf2"
  [(set (match_operand:TF 0 "register_operand" "=e,e")
	(neg:TF (match_operand:TF 1 "register_operand" "0,e")))]
  "TARGET_FPU"
  "")

(define_insn "*negtf2_notv9"
  [(set (match_operand:TF 0 "register_operand" "=e,e")
	(neg:TF (match_operand:TF 1 "register_operand" "0,e")))]
  ; We don't use quad float insns here so we don't need TARGET_HARD_QUAD.
  "TARGET_FPU
   && ! TARGET_V9"
  "@
  fnegs\\t%0, %0
  #"
  [(set_attr "type" "fpmove")
   (set_attr "length" "1,2")])

(define_split
  [(set (match_operand:TF 0 "register_operand" "")
	(neg:TF (match_operand:TF 1 "register_operand" "")))]
  "TARGET_FPU
   && ! TARGET_V9
   && reload_completed
   && sparc_absnegfloat_split_legitimate (operands[0], operands[1])"
  [(set (match_dup 2) (neg:SF (match_dup 3)))
   (set (match_dup 4) (match_dup 5))
   (set (match_dup 6) (match_dup 7))]
  "if (GET_CODE (operands[0]) == SUBREG)
     operands[0] = alter_subreg (operands[0]);
   if (GET_CODE (operands[1]) == SUBREG)
     operands[1] = alter_subreg (operands[1]);
   operands[2] = gen_rtx_raw_REG (SFmode, REGNO (operands[0]));
   operands[3] = gen_rtx_raw_REG (SFmode, REGNO (operands[1]));
   operands[4] = gen_rtx_raw_REG (SFmode, REGNO (operands[0]) + 1);
   operands[5] = gen_rtx_raw_REG (SFmode, REGNO (operands[1]) + 1);
   operands[6] = gen_rtx_raw_REG (DFmode, REGNO (operands[0]) + 2);
   operands[7] = gen_rtx_raw_REG (DFmode, REGNO (operands[1]) + 2);")

(define_insn "*negtf2_v9"
  [(set (match_operand:TF 0 "register_operand" "=e,e")
	(neg:TF (match_operand:TF 1 "register_operand" "0,e")))]
  ; We don't use quad float insns here so we don't need TARGET_HARD_QUAD.
  "TARGET_FPU && TARGET_V9"
  "@
  fnegd\\t%0, %0
  #"
  [(set_attr "type" "fpmove")
   (set_attr "length" "1,2")])

(define_split
  [(set (match_operand:TF 0 "register_operand" "")
	(neg:TF (match_operand:TF 1 "register_operand" "")))]
  "TARGET_FPU
   && TARGET_V9
   && reload_completed
   && sparc_absnegfloat_split_legitimate (operands[0], operands[1])"
  [(set (match_dup 2) (neg:DF (match_dup 3)))
   (set (match_dup 4) (match_dup 5))]
  "if (GET_CODE (operands[0]) == SUBREG)
     operands[0] = alter_subreg (operands[0]);
   if (GET_CODE (operands[1]) == SUBREG)
     operands[1] = alter_subreg (operands[1]);
   operands[2] = gen_rtx_raw_REG (DFmode, REGNO (operands[0]));
   operands[3] = gen_rtx_raw_REG (DFmode, REGNO (operands[1]));
   operands[4] = gen_rtx_raw_REG (DFmode, REGNO (operands[0]) + 2);
   operands[5] = gen_rtx_raw_REG (DFmode, REGNO (operands[1]) + 2);")

(define_expand "negdf2"
  [(set (match_operand:DF 0 "register_operand" "")
	(neg:DF (match_operand:DF 1 "register_operand" "")))]
  "TARGET_FPU"
  "")

(define_insn "*negdf2_notv9"
  [(set (match_operand:DF 0 "register_operand" "=e,e")
	(neg:DF (match_operand:DF 1 "register_operand" "0,e")))]
  "TARGET_FPU && ! TARGET_V9"
  "@
  fnegs\\t%0, %0
  #"
  [(set_attr "type" "fpmove")
   (set_attr "length" "1,2")])

(define_split
  [(set (match_operand:DF 0 "register_operand" "")
        (neg:DF (match_operand:DF 1 "register_operand" "")))]
  "TARGET_FPU
   && ! TARGET_V9
   && reload_completed
   && sparc_absnegfloat_split_legitimate (operands[0], operands[1])"
  [(set (match_dup 2) (neg:SF (match_dup 3)))
   (set (match_dup 4) (match_dup 5))]
  "if (GET_CODE (operands[0]) == SUBREG)
     operands[0] = alter_subreg (operands[0]);
   if (GET_CODE (operands[1]) == SUBREG)
     operands[1] = alter_subreg (operands[1]);
   operands[2] = gen_rtx_raw_REG (SFmode, REGNO (operands[0]));
   operands[3] = gen_rtx_raw_REG (SFmode, REGNO (operands[1]));
   operands[4] = gen_rtx_raw_REG (SFmode, REGNO (operands[0]) + 1);
   operands[5] = gen_rtx_raw_REG (SFmode, REGNO (operands[1]) + 1);")

(define_insn "*negdf2_v9"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(neg:DF (match_operand:DF 1 "register_operand" "e")))]
  "TARGET_FPU && TARGET_V9"
  "fnegd\\t%1, %0"
  [(set_attr "type" "fpmove")
   (set_attr "length" "1")])

(define_insn "negsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(neg:SF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_FPU"
  "fnegs\\t%1, %0"
  [(set_attr "type" "fpmove")
   (set_attr "length" "1")])

(define_expand "abstf2"
  [(set (match_operand:TF 0 "register_operand" "")
	(abs:TF (match_operand:TF 1 "register_operand" "")))]
  "TARGET_FPU"
  "")

(define_insn "*abstf2_notv9"
  [(set (match_operand:TF 0 "register_operand" "=e,e")
	(abs:TF (match_operand:TF 1 "register_operand" "0,e")))]
  ; We don't use quad float insns here so we don't need TARGET_HARD_QUAD.
  "TARGET_FPU && ! TARGET_V9"
  "@
  fabss\\t%0, %0
  #"
  [(set_attr "type" "fpmove")
   (set_attr "length" "1,2")])

(define_split
  [(set (match_operand:TF 0 "register_operand" "=e,e")
	(abs:TF (match_operand:TF 1 "register_operand" "0,e")))]
  "TARGET_FPU
   && ! TARGET_V9
   && reload_completed
   && sparc_absnegfloat_split_legitimate (operands[0], operands[1])"
  [(set (match_dup 2) (abs:SF (match_dup 3)))
   (set (match_dup 4) (match_dup 5))
   (set (match_dup 6) (match_dup 7))]
  "if (GET_CODE (operands[0]) == SUBREG)
     operands[0] = alter_subreg (operands[0]);
   if (GET_CODE (operands[1]) == SUBREG)
     operands[1] = alter_subreg (operands[1]);
   operands[2] = gen_rtx_raw_REG (SFmode, REGNO (operands[0]));
   operands[3] = gen_rtx_raw_REG (SFmode, REGNO (operands[1]));
   operands[4] = gen_rtx_raw_REG (SFmode, REGNO (operands[0]) + 1);
   operands[5] = gen_rtx_raw_REG (SFmode, REGNO (operands[1]) + 1);
   operands[6] = gen_rtx_raw_REG (DFmode, REGNO (operands[0]) + 2);
   operands[7] = gen_rtx_raw_REG (DFmode, REGNO (operands[1]) + 2);")

(define_insn "*abstf2_hq_v9"
  [(set (match_operand:TF 0 "register_operand" "=e,e")
	(abs:TF (match_operand:TF 1 "register_operand" "0,e")))]
  "TARGET_FPU && TARGET_V9 && TARGET_HARD_QUAD"
  "@
  fabsd\\t%0, %0
  fabsq\\t%1, %0"
  [(set_attr "type" "fpmove")
   (set_attr "length" "1")])

(define_insn "*abstf2_v9"
  [(set (match_operand:TF 0 "register_operand" "=e,e")
	(abs:TF (match_operand:TF 1 "register_operand" "0,e")))]
  "TARGET_FPU && TARGET_V9 && !TARGET_HARD_QUAD"
  "@
  fabsd\\t%0, %0
  #"
  [(set_attr "type" "fpmove")
   (set_attr "length" "1,2")])

(define_split
  [(set (match_operand:TF 0 "register_operand" "=e,e")
	(abs:TF (match_operand:TF 1 "register_operand" "0,e")))]
  "TARGET_FPU
   && TARGET_V9
   && reload_completed
   && sparc_absnegfloat_split_legitimate (operands[0], operands[1])"
  [(set (match_dup 2) (abs:DF (match_dup 3)))
   (set (match_dup 4) (match_dup 5))]
  "if (GET_CODE (operands[0]) == SUBREG)
     operands[0] = alter_subreg (operands[0]);
   if (GET_CODE (operands[1]) == SUBREG)
     operands[1] = alter_subreg (operands[1]);
   operands[2] = gen_rtx_raw_REG (DFmode, REGNO (operands[0]));
   operands[3] = gen_rtx_raw_REG (DFmode, REGNO (operands[1]));
   operands[4] = gen_rtx_raw_REG (DFmode, REGNO (operands[0]) + 2);
   operands[5] = gen_rtx_raw_REG (DFmode, REGNO (operands[1]) + 2);")

(define_expand "absdf2"
  [(set (match_operand:DF 0 "register_operand" "")
	(abs:DF (match_operand:DF 1 "register_operand" "")))]
  "TARGET_FPU"
  "")

(define_insn "*absdf2_notv9"
  [(set (match_operand:DF 0 "register_operand" "=e,e")
	(abs:DF (match_operand:DF 1 "register_operand" "0,e")))]
  "TARGET_FPU && ! TARGET_V9"
  "@
  fabss\\t%0, %0
  #"
  [(set_attr "type" "fpmove")
   (set_attr "length" "1,2")])

(define_split
  [(set (match_operand:DF 0 "register_operand" "=e,e")
	(abs:DF (match_operand:DF 1 "register_operand" "0,e")))]
  "TARGET_FPU
   && ! TARGET_V9
   && reload_completed
   && sparc_absnegfloat_split_legitimate (operands[0], operands[1])"
  [(set (match_dup 2) (abs:SF (match_dup 3)))
   (set (match_dup 4) (match_dup 5))]
  "if (GET_CODE (operands[0]) == SUBREG)
     operands[0] = alter_subreg (operands[0]);
   if (GET_CODE (operands[1]) == SUBREG)
     operands[1] = alter_subreg (operands[1]);
   operands[2] = gen_rtx_raw_REG (SFmode, REGNO (operands[0]));
   operands[3] = gen_rtx_raw_REG (SFmode, REGNO (operands[1]));
   operands[4] = gen_rtx_raw_REG (SFmode, REGNO (operands[0]) + 1);
   operands[5] = gen_rtx_raw_REG (SFmode, REGNO (operands[1]) + 1);")

(define_insn "*absdf2_v9"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(abs:DF (match_operand:DF 1 "register_operand" "e")))]
  "TARGET_FPU && TARGET_V9"
  "fabsd\\t%1, %0"
  [(set_attr "type" "fpmove")
   (set_attr "length" "1")])

(define_insn "abssf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(abs:SF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_FPU"
  "fabss\\t%1, %0"
  [(set_attr "type" "fpmove")
   (set_attr "length" "1")])

(define_expand "sqrttf2"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(sqrt:TF (match_operand:TF 1 "register_operand" "e")))]
  "TARGET_FPU && (TARGET_HARD_QUAD || TARGET_ARCH64)"
  "
{
  if (! TARGET_HARD_QUAD)
    {
      rtx slot0, slot1;

      if (GET_CODE (operands[0]) != MEM)
	slot0 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode), 0);
      else
	slot0 = operands[0];
      if (GET_CODE (operands[1]) != MEM)
	{
	  slot1 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode), 0);
	  emit_insn (gen_rtx_SET (VOIDmode, slot1, operands[1]));
	}
      else
	slot1 = operands[1];

      emit_library_call (gen_rtx (SYMBOL_REF, Pmode, \"_Qp_sqrt\"), 0,
			 VOIDmode, 2,
			 XEXP (slot0, 0), Pmode,
			 XEXP (slot1, 0), Pmode);

      if (GET_CODE (operands[0]) != MEM)
	emit_insn (gen_rtx_SET (VOIDmode, operands[0], slot0));
      DONE;
    }
}")

(define_insn "*sqrttf2_hq"
  [(set (match_operand:TF 0 "register_operand" "=e")
	(sqrt:TF (match_operand:TF 1 "register_operand" "e")))]
  "TARGET_FPU && TARGET_HARD_QUAD"
  "fsqrtq\\t%1, %0"
  [(set_attr "type" "fpsqrtd")
   (set_attr "length" "1")])

(define_insn "sqrtdf2"
  [(set (match_operand:DF 0 "register_operand" "=e")
	(sqrt:DF (match_operand:DF 1 "register_operand" "e")))]
  "TARGET_FPU"
  "fsqrtd\\t%1, %0"
  [(set_attr "type" "fpsqrtd")
   (set_attr "length" "1")])

(define_insn "sqrtsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(sqrt:SF (match_operand:SF 1 "register_operand" "f")))]
  "TARGET_FPU"
  "fsqrts\\t%1, %0"
  [(set_attr "type" "fpsqrts")
   (set_attr "length" "1")])

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

  return \"sll\\t%1, %2, %0\";
}"
  [(set_attr "type" "shift")
   (set_attr "length" "1")])

;; We special case multiplication by two, as add can be done
;; in both ALUs, while shift only in IEU0 on UltraSPARC.
(define_insn "*ashlsi3_const1"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ashift:SI (match_operand:SI 1 "register_operand" "r")
                   (const_int 1)))]
  ""
  "add\\t%1, %1, %0"
  [(set_attr "type" "binary")
   (set_attr "length" "1")])

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

;; We special case multiplication by two, as add can be done
;; in both ALUs, while shift only in IEU0 on UltraSPARC.
(define_insn "*ashldi3_const1"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashift:DI (match_operand:DI 1 "register_operand" "r")
		   (const_int 1)))]
  "TARGET_ARCH64"
  "add\\t%1, %1, %0"
  [(set_attr "type" "binary")
   (set_attr "length" "1")])

(define_insn "*ashldi3_sp64"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashift:DI (match_operand:DI 1 "register_operand" "r")
		   (match_operand:SI 2 "arith_operand" "rI")))]
  "TARGET_ARCH64"
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT
      && (unsigned HOST_WIDE_INT) INTVAL (operands[2]) > 63)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  return \"sllx\\t%1, %2, %0\";
}"
  [(set_attr "type" "shift")
   (set_attr "length" "1")])

;; XXX UGH!
(define_insn "ashldi3_v8plus"
  [(set (match_operand:DI 0 "register_operand" "=&h,&h,r")
	(ashift:DI (match_operand:DI 1 "arith_operand" "rI,0,rI")
		   (match_operand:SI 2 "arith_operand" "rI,rI,rI")))
   (clobber (match_scratch:SI 3 "=X,X,&h"))]
  "TARGET_V8PLUS"
  "*return sparc_v8plus_shift (operands, insn, \"sllx\");"
  [(set_attr "length" "5,5,6")])

;; Optimize (1LL<<x)-1
;; XXX this also needs to be fixed to handle equal subregs
;; XXX first before we could re-enable it.
(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=h")
	(plus:DI (ashift:DI (const_int 1)
			    (match_operand:SI 2 "arith_operand" "rI"))
		 (const_int -1)))]
  "0 && TARGET_V8PLUS"
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
  "! TARGET_LIVE_G0"
  "addcc\\t%0, %0, %%g0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

(define_insn "*cmp_cc_set_ashift_1"
  [(set (reg:CC_NOOV 100)
	(compare:CC_NOOV (ashift:SI (match_operand:SI 1 "register_operand" "r")
				    (const_int 1))
			 (const_int 0)))
   (set (match_operand:SI 0 "register_operand" "=r")
	(ashift:SI (match_dup 1) (const_int 1)))]
  ""
  "addcc\\t%1, %1, %0"
  [(set_attr "type" "compare")
   (set_attr "length" "1")])

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

  return \"sra\\t%1, %2, %0\";
}"
  [(set_attr "type" "shift")
   (set_attr "length" "1")])

(define_insn "*ashrsi3_extend"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(sign_extend:DI (ashiftrt:SI (match_operand:SI 1 "register_operand" "r")
				     (match_operand:SI 2 "arith_operand" "r"))))]
  "TARGET_ARCH64"
  "sra\\t%1, %2, %0"
  [(set_attr "type" "shift")
   (set_attr "length" "1")])

;; This handles the case as above, but with constant shift instead of
;; register. Combiner "simplifies" it for us a little bit though.
(define_insn "*ashrsi3_extend2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashiftrt:DI (ashift:DI (subreg:DI (match_operand:SI 1 "register_operand" "r") 0)
				(const_int 32))
		     (match_operand:SI 2 "small_int_or_double" "n")))]
  "TARGET_ARCH64
   && ((GET_CODE (operands[2]) == CONST_INT
        && INTVAL (operands[2]) >= 32 && INTVAL (operands[2]) < 64)
       || (GET_CODE (operands[2]) == CONST_DOUBLE
	   && !CONST_DOUBLE_HIGH (operands[2])
           && CONST_DOUBLE_LOW (operands[2]) >= 32
           && CONST_DOUBLE_LOW (operands[2]) < 64))"
  "*
{
  operands[2] = GEN_INT (INTVAL (operands[2]) - 32);

  return \"sra\\t%1, %2, %0\";
}"
  [(set_attr "type" "shift")
   (set_attr "length" "1")])

(define_expand "ashrdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(ashiftrt:DI (match_operand:DI 1 "register_operand" "r")
		     (match_operand:SI 2 "arith_operand" "rI")))]
  "TARGET_ARCH64 || TARGET_V8PLUS"
  "
{
  if (! TARGET_ARCH64)
    {
      if (GET_CODE (operands[2]) == CONST_INT)
        FAIL;	/* prefer generic code in this case */
      emit_insn (gen_ashrdi3_v8plus (operands[0], operands[1], operands[2]));
      DONE;
    }
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

  return \"srax\\t%1, %2, %0\";
}"
  [(set_attr "type" "shift")
   (set_attr "length" "1")])

;; XXX
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

  return \"srl\\t%1, %2, %0\";
}"
  [(set_attr "type" "shift")
   (set_attr "length" "1")])

;; This handles the case where
;; (zero_extend:DI (lshiftrt:SI (match_operand:SI) (match_operand:SI))),
;; but combiner "simplifies" it for us.
(define_insn "*lshrsi3_extend"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(and:DI (subreg:DI (lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
			   (match_operand:SI 2 "arith_operand" "r")) 0)
		(match_operand 3 "" "")))]
  "TARGET_ARCH64
   && ((GET_CODE (operands[3]) == CONST_DOUBLE
           && CONST_DOUBLE_HIGH (operands[3]) == 0
           && CONST_DOUBLE_LOW (operands[3]) == 0xffffffff)
#if HOST_BITS_PER_WIDE_INT >= 64
          || (GET_CODE (operands[3]) == CONST_INT
              && (unsigned HOST_WIDE_INT) INTVAL (operands[3]) == 0xffffffff)
#endif
         )"
  "srl\\t%1, %2, %0"
  [(set_attr "type" "shift")
   (set_attr "length" "1")])

;; This handles the case where
;; (lshiftrt:DI (zero_extend:DI (match_operand:SI)) (const_int >=0 < 32))
;; but combiner "simplifies" it for us.
(define_insn "*lshrsi3_extend2"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(zero_extract:DI (subreg:DI (match_operand:SI 1 "register_operand" "r") 0)
			 (match_operand 2 "small_int_or_double" "n")
			 (const_int 32)))]
  "TARGET_ARCH64
   && ((GET_CODE (operands[2]) == CONST_INT
        && (unsigned HOST_WIDE_INT) INTVAL (operands[2]) < 32)
       || (GET_CODE (operands[2]) == CONST_DOUBLE
	   && CONST_DOUBLE_HIGH (operands[2]) == 0
           && (unsigned HOST_WIDE_INT) CONST_DOUBLE_LOW (operands[2]) < 32))"
  "*
{
  operands[2] = GEN_INT (32 - INTVAL (operands[2]));

  return \"srl\\t%1, %2, %0\";
}"
  [(set_attr "type" "shift")
   (set_attr "length" "1")])

(define_expand "lshrdi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(lshiftrt:DI (match_operand:DI 1 "register_operand" "r")
		     (match_operand:SI 2 "arith_operand" "rI")))]
  "TARGET_ARCH64 || TARGET_V8PLUS"
  "
{
  if (! TARGET_ARCH64)
    {
      if (GET_CODE (operands[2]) == CONST_INT)
        FAIL;
      emit_insn (gen_lshrdi3_v8plus (operands[0], operands[1], operands[2]));
      DONE;
    }
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

  return \"srlx\\t%1, %2, %0\";
}"
  [(set_attr "type" "shift")
   (set_attr "length" "1")])

;; XXX
(define_insn "lshrdi3_v8plus"
  [(set (match_operand:DI 0 "register_operand" "=&h,&h,r")
	(lshiftrt:DI (match_operand:DI 1 "arith_operand" "rI,0,rI")
		     (match_operand:SI 2 "arith_operand" "rI,rI,rI")))
   (clobber (match_scratch:SI 3 "=X,X,&h"))]
  "TARGET_V8PLUS"
  "*return sparc_v8plus_shift (operands, insn, \"srlx\");"
  [(set_attr "length" "5,5,6")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashiftrt:SI (subreg:SI (lshiftrt:DI (match_operand:DI 1 "register_operand" "r")
					     (const_int 32)) 0)
		     (match_operand:SI 2 "small_int_or_double" "n")))]
  "TARGET_ARCH64
   && ((GET_CODE (operands[2]) == CONST_INT
        && (unsigned HOST_WIDE_INT) INTVAL (operands[2]) < 32)
       || (GET_CODE (operands[2]) == CONST_DOUBLE
	   && !CONST_DOUBLE_HIGH (operands[2])
           && (unsigned HOST_WIDE_INT) CONST_DOUBLE_LOW (operands[2]) < 32))"
  "*
{
  operands[2] = GEN_INT (INTVAL (operands[2]) + 32);

  return \"srax\\t%1, %2, %0\";
}"
  [(set_attr "type" "shift")
   (set_attr "length" "1")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lshiftrt:SI (subreg:SI (ashiftrt:DI (match_operand:DI 1 "register_operand" "r")
					     (const_int 32)) 0)
		     (match_operand:SI 2 "small_int_or_double" "n")))]
  "TARGET_ARCH64
   && ((GET_CODE (operands[2]) == CONST_INT
        && (unsigned HOST_WIDE_INT) INTVAL (operands[2]) < 32)
       || (GET_CODE (operands[2]) == CONST_DOUBLE
	   && !CONST_DOUBLE_HIGH (operands[2])
           && (unsigned HOST_WIDE_INT) CONST_DOUBLE_LOW (operands[2]) < 32))"
  "*
{
  operands[2] = GEN_INT (INTVAL (operands[2]) + 32);

  return \"srlx\\t%1, %2, %0\";
}"
  [(set_attr "type" "shift")
   (set_attr "length" "1")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashiftrt:SI (subreg:SI (ashiftrt:DI (match_operand:DI 1 "register_operand" "r")
					     (match_operand:SI 2 "small_int_or_double" "n")) 0)
		     (match_operand:SI 3 "small_int_or_double" "n")))]
  "TARGET_ARCH64
   && GET_CODE (operands[2]) == CONST_INT && GET_CODE (operands[3]) == CONST_INT
   && (unsigned HOST_WIDE_INT) INTVAL (operands[2]) >= 32
   && (unsigned HOST_WIDE_INT) INTVAL (operands[3]) < 32
   && (unsigned HOST_WIDE_INT) (INTVAL (operands[2]) + INTVAL (operands[3])) < 64"
  "*
{
  operands[2] = GEN_INT (INTVAL (operands[2]) + INTVAL (operands[3]));

  return \"srax\\t%1, %2, %0\";
}"
  [(set_attr "type" "shift")
   (set_attr "length" "1")])

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lshiftrt:SI (subreg:SI (lshiftrt:DI (match_operand:DI 1 "register_operand" "r")
					     (match_operand:SI 2 "small_int_or_double" "n")) 0)
		     (match_operand:SI 3 "small_int_or_double" "n")))]
  "TARGET_ARCH64
   && GET_CODE (operands[2]) == CONST_INT && GET_CODE (operands[3]) == CONST_INT
   && (unsigned HOST_WIDE_INT) INTVAL (operands[2]) >= 32
   && (unsigned HOST_WIDE_INT) INTVAL (operands[3]) < 32
   && (unsigned HOST_WIDE_INT) (INTVAL (operands[2]) + INTVAL (operands[3])) < 64"
  "*
{
  operands[2] = GEN_INT (INTVAL (operands[2]) + INTVAL (operands[3]));

  return \"srlx\\t%1, %2, %0\";
}"
  [(set_attr "type" "shift")
   (set_attr "length" "1")])

;; Unconditional and other jump instructions
;; On the Sparc, by setting the annul bit on an unconditional branch, the
;; following insn is never executed.  This saves us a nop.  Dbx does not
;; handle such branches though, so we only use them when optimizing.
(define_insn "jump"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  ""
  "*
{
  /* TurboSparc is reported to have problems with
     with
	foo: b,a foo
     i.e. an empty loop with the annul bit set.  The workaround is to use 
        foo: b foo; nop
     instead.  */

  if (! TARGET_V9 && flag_delayed_branch
      && (insn_addresses[INSN_UID (operands[0])]
	  == insn_addresses[INSN_UID (insn)]))
    return \"b\\t%l0%#\";
  else
    return TARGET_V9 ? \"ba,pt%*\\t%%xcc, %l0%(\" : \"b%*\\t%l0%(\";
}"
  [(set_attr "type" "uncond_branch")])

(define_expand "tablejump"
  [(parallel [(set (pc) (match_operand 0 "register_operand" "r"))
	      (use (label_ref (match_operand 1 "" "")))])]
  ""
  "
{
  if (GET_MODE (operands[0]) != CASE_VECTOR_MODE)
    abort ();

  /* In pic mode, our address differences are against the base of the
     table.  Add that base value back in; CSE ought to be able to combine
     the two address loads.  */
  if (flag_pic)
    {
      rtx tmp, tmp2;
      tmp = gen_rtx_LABEL_REF (Pmode, operands[1]);
      tmp2 = operands[0];
      if (CASE_VECTOR_MODE != Pmode)
        tmp2 = gen_rtx_SIGN_EXTEND (Pmode, tmp2);
      tmp = gen_rtx_PLUS (Pmode, tmp2, tmp);
      operands[0] = memory_address (Pmode, tmp);
    }
}")

(define_insn "*tablejump_sp32"
  [(set (pc) (match_operand:SI 0 "address_operand" "p"))
   (use (label_ref (match_operand 1 "" "")))]
  "! TARGET_PTR64"
  "jmp\\t%a0%#"
  [(set_attr "type" "uncond_branch")])

(define_insn "*tablejump_sp64"
  [(set (pc) (match_operand:DI 0 "address_operand" "p"))
   (use (label_ref (match_operand 1 "" "")))]
  "TARGET_PTR64"
  "jmp\\t%a0%#"
  [(set_attr "type" "uncond_branch")])

;; This pattern recognizes the "instruction" that appears in 
;; a function call that wants a structure value, 
;; to inform the called function if compiled with Sun CC.
;(define_insn "*unimp_insn"
;  [(match_operand:SI 0 "immediate_operand" "")]
;  "GET_CODE (operands[0]) == CONST_INT && INTVAL (operands[0]) > 0"
;  "unimp\\t%0"
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
	  (gen_rtx_PARALLEL
	   (VOIDmode,
	    gen_rtvec (3,
		       gen_rtx_SET (VOIDmode, pc_rtx, XEXP (operands[0], 0)),
		       operands[3],
		       gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (Pmode, 15)))));
      else
	emit_jump_insn
	  (gen_rtx_PARALLEL
	   (VOIDmode,
	    gen_rtvec (2,
		       gen_rtx_SET (VOIDmode, pc_rtx, XEXP (operands[0], 0)),
		       gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (Pmode, 15)))));
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
      (gen_rtx_PARALLEL
       (VOIDmode,
	gen_rtvec (3, gen_rtx_CALL (VOIDmode, fn_rtx, nregs_rtx),
		   operands[3],
		   gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (Pmode, 15)))));
  else
    emit_call_insn
      (gen_rtx_PARALLEL
       (VOIDmode,
	gen_rtvec (2, gen_rtx_CALL (VOIDmode, fn_rtx, nregs_rtx),
		   gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (Pmode, 15)))));

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
  "call\\t%a0, %1%#"
  [(set_attr "type" "call")])

(define_insn "*call_symbolic_sp32"
  [(call (mem:SI (match_operand:SI 0 "symbolic_operand" "s"))
	 (match_operand 1 "" ""))
   (clobber (reg:SI 15))]
  ;;- Do not use operand 1 for most machines.
  "! TARGET_PTR64"
  "call\\t%a0, %1%#"
  [(set_attr "type" "call")])

(define_insn "*call_address_sp64"
  [(call (mem:SI (match_operand:DI 0 "address_operand" "p"))
	 (match_operand 1 "" ""))
   (clobber (reg:DI 15))]
  ;;- Do not use operand 1 for most machines.
  "TARGET_PTR64"
  "call\\t%a0, %1%#"
  [(set_attr "type" "call")])

(define_insn "*call_symbolic_sp64"
  [(call (mem:SI (match_operand:DI 0 "symbolic_operand" "s"))
	 (match_operand 1 "" ""))
   (clobber (reg:DI 15))]
  ;;- Do not use operand 1 for most machines.
  "TARGET_PTR64"
  "call\\t%a0, %1%#"
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
  "call\\t%a0, %1\\n\\tnop\\n\\tunimp\\t%2"
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
  "call\\t%a0, %1\\n\\tnop\\n\\tunimp\\t%2"
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
  "call\\t%a0, %1\\n\\tnop\\n\\tnop"
  [(set_attr "type" "call_no_delay_slot")])

;; This is a call that wants a structure value.
(define_insn "*call_symbolic_untyped_struct_value_sp32"
  [(call (mem:SI (match_operand:SI 0 "symbolic_operand" "s"))
	 (match_operand 1 "" ""))
   (match_operand 2 "immediate_operand" "")
   (clobber (reg:SI 15))]
  ;;- Do not use operand 1 for most machines.
  "! TARGET_ARCH64 && GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) < 0"
  "call\\t%a0, %1\\n\\tnop\\n\\tnop"
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
  "call\\t%a1, %2%#"
  [(set_attr "type" "call")])

(define_insn "*call_value_symbolic_sp32"
  [(set (match_operand 0 "" "=rf")
	(call (mem:SI (match_operand:SI 1 "symbolic_operand" "s"))
	      (match_operand 2 "" "")))
   (clobber (reg:SI 15))]
  ;;- Do not use operand 2 for most machines.
  "! TARGET_PTR64"
  "call\\t%a1, %2%#"
  [(set_attr "type" "call")])

(define_insn "*call_value_address_sp64"
  [(set (match_operand 0 "" "")
	(call (mem:SI (match_operand:DI 1 "address_operand" "p"))
	      (match_operand 2 "" "")))
   (clobber (reg:DI 15))]
  ;;- Do not use operand 2 for most machines.
  "TARGET_PTR64"
  "call\\t%a1, %2%#"
  [(set_attr "type" "call")])

(define_insn "*call_value_symbolic_sp64"
  [(set (match_operand 0 "" "")
	(call (mem:SI (match_operand:DI 1 "symbolic_operand" "s"))
	      (match_operand 2 "" "")))
   (clobber (reg:DI 15))]
  ;;- Do not use operand 2 for most machines.
  "TARGET_PTR64"
  "call\\t%a1, %2%#"
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
  ""
  [(set_attr "length" "0")])

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
      rtx rtnreg = gen_rtx_REG (SImode, (current_function_uses_only_leaf_regs
					 ? 15 : 31));
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
  "return\\t%%i7+8\\n\\tmov\\t%Y1, %Y0")

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop"
  [(set_attr "type" "ialu")
   (set_attr "length" "1")])

(define_expand "indirect_jump"
  [(set (pc) (match_operand 0 "address_operand" "p"))]
  ""
  "")

(define_insn "*branch_sp32"
  [(set (pc) (match_operand:SI 0 "address_operand" "p"))]
  "! TARGET_PTR64"
 "jmp\\t%a0%#"
 [(set_attr "type" "uncond_branch")])
 
(define_insn "*branch_sp64"
  [(set (pc) (match_operand:DI 0 "address_operand" "p"))]
  "TARGET_PTR64"
  "jmp\\t%a0%#"
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
#if 0
  rtx chain = operands[0];
#endif
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

#if 0
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
#endif

  emit_insn (gen_rtx_USE (VOIDmode, static_chain_rtx));
  emit_insn (gen_goto_handler_and_restore (labreg));
  emit_barrier ();
  DONE;
}")

;; Special trap insn to flush register windows.
(define_insn "flush_register_windows"
  [(unspec_volatile [(const_int 0)] 1)]
  ""
  "* return TARGET_V9 ? \"flushw\" : \"ta\\t3\";"
  [(set_attr "type" "misc")
   (set_attr "length" "1")])

(define_insn "goto_handler_and_restore"
  [(unspec_volatile [(match_operand 0 "register_operand" "=r")] 2)]
  "GET_MODE (operands[0]) == Pmode"
  "jmp\\t%0+0\\n\\trestore"
  [(set_attr "type" "misc")
   (set_attr "length" "2")])

;;(define_insn "goto_handler_and_restore_v9"
;;  [(unspec_volatile [(match_operand:SI 0 "register_operand" "=r,r")
;;		     (match_operand:SI 1 "register_operand" "=r,r")
;;		     (match_operand:SI 2 "const_int_operand" "I,n")] 3)]
;;  "TARGET_V9 && ! TARGET_ARCH64"
;;  "@
;;   return\\t%0+0\\n\\tmov\\t%2, %Y1
;;   sethi\\t%%hi(%2), %1\\n\\treturn\\t%0+0\\n\\tor\\t%Y1, %%lo(%2), %Y1"
;;  [(set_attr "type" "misc")
;;   (set_attr "length" "2,3")])
;;
;;(define_insn "*goto_handler_and_restore_v9_sp64"
;;  [(unspec_volatile [(match_operand:DI 0 "register_operand" "=r,r")
;;		     (match_operand:DI 1 "register_operand" "=r,r")
;;		     (match_operand:SI 2 "const_int_operand" "I,n")] 3)]
;;  "TARGET_V9 && TARGET_ARCH64"
;;  "@
;;   return\\t%0+0\\n\\tmov\\t%2, %Y1
;;   sethi\\t%%hi(%2), %1\\n\\treturn\\t%0+0\\n\\tor\\t%Y1, %%lo(%2), %Y1"
;;  [(set_attr "type" "misc")
;;   (set_attr "length" "2,3")])

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

; We do SImode and DImode versions of this to quiet down genrecog's complaints
; of the define_insn otherwise missing a mode.  We make "flush", aka
; gen_flush, the default one since sparc_initialize_trampoline uses
; it on SImode mem values.

(define_insn "flush"
  [(unspec_volatile [(match_operand:SI 0 "memory_operand" "m")] 3)]
  ""
  "* return TARGET_V9 ? \"flush %f0\" : \"iflush %f0\";"
  [(set_attr "type" "misc")])

(define_insn "flushdi"
  [(unspec_volatile [(match_operand:DI 0 "memory_operand" "m")] 3)]
  ""
  "* return TARGET_V9 ? \"flush %f0\" : \"iflush %f0\";"
  [(set_attr "type" "misc")])


;; find first set.

;; The scan instruction searches from the most significant bit while ffs
;; searches from the least significant bit.  The bit index and treatment of
;; zero also differ.  It takes at least 7 instructions to get the proper
;; result.  Here is an obvious 8 instruction sequence.

;; XXX
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
   && ! MEM_VOLATILE_P (operands[0])
   && ! MEM_VOLATILE_P (operands[1])
   && addrs_ok_for_ldd_peep (XEXP (operands[0], 0), XEXP (operands[1], 0))"
  "stx\\t%%g0, %0")

(define_peephole
  [(set (match_operand:SI 0 "memory_operand" "")
      (const_int 0))
   (set (match_operand:SI 1 "memory_operand" "")
      (const_int 0))]
  "TARGET_V9
   && ! MEM_VOLATILE_P (operands[0])
   && ! MEM_VOLATILE_P (operands[1])
   && addrs_ok_for_ldd_peep (XEXP (operands[1], 0), XEXP (operands[0], 0))"
  "stx\\t%%g0, %1")

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=rf")
        (match_operand:SI 1 "memory_operand" ""))
   (set (match_operand:SI 2 "register_operand" "=rf")
        (match_operand:SI 3 "memory_operand" ""))]
  "registers_ok_for_ldd_peep (operands[0], operands[2]) 
   && ! MEM_VOLATILE_P (operands[1])
   && ! MEM_VOLATILE_P (operands[3])
   && addrs_ok_for_ldd_peep (XEXP (operands[1], 0), XEXP (operands[3], 0))" 
  "ldd\\t%1, %0")

(define_peephole
  [(set (match_operand:SI 0 "memory_operand" "")
        (match_operand:SI 1 "register_operand" "rf"))
   (set (match_operand:SI 2 "memory_operand" "")
        (match_operand:SI 3 "register_operand" "rf"))]
  "registers_ok_for_ldd_peep (operands[1], operands[3]) 
   && ! MEM_VOLATILE_P (operands[0])
   && ! MEM_VOLATILE_P (operands[2])
   && addrs_ok_for_ldd_peep (XEXP (operands[0], 0), XEXP (operands[2], 0))"
  "std\\t%1, %0")
 
(define_peephole
  [(set (match_operand:SF 0 "register_operand" "=fr")
        (match_operand:SF 1 "memory_operand" ""))
   (set (match_operand:SF 2 "register_operand" "=fr")
        (match_operand:SF 3 "memory_operand" ""))]
  "registers_ok_for_ldd_peep (operands[0], operands[2]) 
   && ! MEM_VOLATILE_P (operands[1])
   && ! MEM_VOLATILE_P (operands[3])
   && addrs_ok_for_ldd_peep (XEXP (operands[1], 0), XEXP (operands[3], 0))"
  "ldd\\t%1, %0")

(define_peephole
  [(set (match_operand:SF 0 "memory_operand" "")
        (match_operand:SF 1 "register_operand" "fr"))
   (set (match_operand:SF 2 "memory_operand" "")
        (match_operand:SF 3 "register_operand" "fr"))]
  "registers_ok_for_ldd_peep (operands[1], operands[3]) 
  && ! MEM_VOLATILE_P (operands[0])
  && ! MEM_VOLATILE_P (operands[2])
  && addrs_ok_for_ldd_peep (XEXP (operands[0], 0), XEXP (operands[2], 0))"
  "std\\t%1, %0")

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=rf")
        (match_operand:SI 1 "memory_operand" ""))
   (set (match_operand:SI 2 "register_operand" "=rf")
        (match_operand:SI 3 "memory_operand" ""))]
  "registers_ok_for_ldd_peep (operands[2], operands[0]) 
  && ! MEM_VOLATILE_P (operands[3])
  && ! MEM_VOLATILE_P (operands[1])
  && addrs_ok_for_ldd_peep (XEXP (operands[3], 0), XEXP (operands[1], 0))"
  "ldd\\t%3, %2")

(define_peephole
  [(set (match_operand:SI 0 "memory_operand" "")
        (match_operand:SI 1 "register_operand" "rf"))
   (set (match_operand:SI 2 "memory_operand" "")
        (match_operand:SI 3 "register_operand" "rf"))]
  "registers_ok_for_ldd_peep (operands[3], operands[1]) 
  && ! MEM_VOLATILE_P (operands[2])
  && ! MEM_VOLATILE_P (operands[0])
  && addrs_ok_for_ldd_peep (XEXP (operands[2], 0), XEXP (operands[0], 0))" 
  "std\\t%3, %2")
 
(define_peephole
  [(set (match_operand:SF 0 "register_operand" "=fr")
        (match_operand:SF 1 "memory_operand" ""))
   (set (match_operand:SF 2 "register_operand" "=fr")
        (match_operand:SF 3 "memory_operand" ""))]
  "registers_ok_for_ldd_peep (operands[2], operands[0]) 
  && ! MEM_VOLATILE_P (operands[3])
  && ! MEM_VOLATILE_P (operands[1])
  && addrs_ok_for_ldd_peep (XEXP (operands[3], 0), XEXP (operands[1], 0))"
  "ldd\\t%3, %2")

(define_peephole
  [(set (match_operand:SF 0 "memory_operand" "")
        (match_operand:SF 1 "register_operand" "fr"))
   (set (match_operand:SF 2 "memory_operand" "")
        (match_operand:SF 3 "register_operand" "fr"))]
  "registers_ok_for_ldd_peep (operands[3], operands[1]) 
  && ! MEM_VOLATILE_P (operands[2])
  && ! MEM_VOLATILE_P (operands[0])
  && addrs_ok_for_ldd_peep (XEXP (operands[2], 0), XEXP (operands[0], 0))"
  "std\\t%3, %2")
 
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
   && ! FP_REG_P (operands[0])
   && ! FP_REG_P (operands[1])"
  "orcc\\t%1, 0, %0")

(define_peephole
  [(set (match_operand:DI 0 "register_operand" "=r")
	(match_operand:DI 1 "register_operand" "r"))
   (set (reg:CCX 100)
	(compare:CCX (match_operand:DI 2 "register_operand" "r")
		    (const_int 0)))]
  "TARGET_ARCH64
   && (rtx_equal_p (operands[2], operands[0])
       || rtx_equal_p (operands[2], operands[1]))
   && ! FP_REG_P (operands[0])
   && ! FP_REG_P (operands[1])"
  "orcc\\t%1, 0, %0")

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
    return \"jmp\\t%%i7+12\\n\\trestore %%g0, %1, %Y0\";
  else if (TARGET_V9 && (GET_CODE (operands[1]) == CONST_INT
			 || IN_OR_GLOBAL_P (operands[1])))
    return \"return\\t%%i7+8\\n\\tmov\\t%Y1, %Y0\";
  else
    return \"ret\\n\\trestore %%g0, %1, %Y0\";
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
    return \"jmp\\t%%i7+12\\n\\trestore %%g0, %1, %Y0\";
  else if (TARGET_V9 && (GET_CODE (operands[1]) == CONST_INT
			 || IN_OR_GLOBAL_P (operands[1])))
    return \"return\\t%%i7+8\\n\\tmov\\t%Y1, %Y0\";
  else
    return \"ret\;restore %%g0, %1, %Y0\";
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
    return \"jmp\\t%%i7+12\\n\\trestore %%g0, %1, %Y0\";
  else if (TARGET_V9 && (GET_CODE (operands[1]) == CONST_INT
			 || IN_OR_GLOBAL_P (operands[1])))
    return \"return\\t%%i7+8\\n\\tmov\\t%Y1, %Y0\";
  else
    return \"ret\;restore %%g0, %1, %Y0\";
}"
  [(set_attr "type" "multi")])

;; The following pattern is only generated by delayed-branch scheduling,
;; when the insn winds up in the epilogue.  This can happen not only when
;; ! TARGET_FPU because we move complex types around by parts using
;; SF mode SUBREGs.
(define_insn "*return_sf_no_fpu"
  [(set (match_operand:SF 0 "restore_operand" "=r")
	(match_operand:SF 1 "register_operand" "r"))
   (return)]
  "! TARGET_EPILOGUE && ! TARGET_LIVE_G0"
  "*
{
  if (! TARGET_ARCH64 && current_function_returns_struct)
    return \"jmp\\t%%i7+12\\n\\trestore %%g0, %1, %Y0\";
  else if (TARGET_V9 && IN_OR_GLOBAL_P (operands[1]))
    return \"return\\t%%i7+8\\n\\tmov\\t%Y1, %Y0\";
  else
    return \"ret\;restore %%g0, %1, %Y0\";
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
    return \"jmp\\t%%i7+12\\n\\trestore %r1, %2, %Y0\";
  /* If operands are global or in registers, can use return */
  else if (TARGET_V9 && IN_OR_GLOBAL_P (operands[1])
	   && (GET_CODE (operands[2]) == CONST_INT
	       || IN_OR_GLOBAL_P (operands[2])))
    return \"return\\t%%i7+8\\n\\tadd\\t%Y1, %Y2, %Y0\";
  else
    return \"ret\;restore %r1, %2, %Y0\";
}"
  [(set_attr "type" "multi")])

(define_insn "*return_losum_si"
  [(set (match_operand:SI 0 "restore_operand" "")
	(lo_sum:SI (match_operand:SI 1 "register_operand" "r")
		   (match_operand:SI 2 "immediate_operand" "in")))
   (return)]
  "! TARGET_EPILOGUE && ! TARGET_LIVE_G0 && ! TARGET_CM_MEDMID"
  "*
{
  if (! TARGET_ARCH64 && current_function_returns_struct)
    return \"jmp\\t%%i7+12\\n\\trestore %r1, %%lo(%a2), %Y0\";
  /* If operands are global or in registers, can use return */
  else if (TARGET_V9 && IN_OR_GLOBAL_P (operands[1]))
    return \"return\\t%%i7+8\\n\\tor\\t%Y1, %%lo(%a2), %Y0\";
  else
    return \"ret\;restore %r1, %%lo(%a2), %Y0\";
}"
  [(set_attr "type" "multi")])

(define_insn "*return_di"
  [(set (match_operand:DI 0 "restore_operand" "")
	(match_operand:DI 1 "arith_double_operand" "rHI"))
   (return)]
  "TARGET_ARCH64 && ! TARGET_EPILOGUE"
  "ret\;restore %%g0, %1, %Y0"
  [(set_attr "type" "multi")])

(define_insn "*return_adddi"
  [(set (match_operand:DI 0 "restore_operand" "")
	(plus:DI (match_operand:DI 1 "arith_operand" "%r")
		 (match_operand:DI 2 "arith_double_operand" "rHI")))
   (return)]
  "TARGET_ARCH64 && ! TARGET_EPILOGUE"
  "ret\;restore %r1, %2, %Y0"
  [(set_attr "type" "multi")])

(define_insn "*return_losum_di"
  [(set (match_operand:DI 0 "restore_operand" "")
	(lo_sum:DI (match_operand:DI 1 "arith_operand" "%r")
		   (match_operand:DI 2 "immediate_operand" "in")))
   (return)]
  "TARGET_ARCH64 && ! TARGET_EPILOGUE && ! TARGET_CM_MEDMID"
  "ret\;restore %r1, %%lo(%a2), %Y0"
  [(set_attr "type" "multi")])

;; The following pattern is only generated by delayed-branch scheduling,
;; when the insn winds up in the epilogue.
(define_insn "*return_sf"
  [(set (reg:SF 32)
	(match_operand:SF 0 "register_operand" "f"))
   (return)]
  "! TARGET_EPILOGUE"
  "ret\;fmovs\\t%0, %%f0"
  [(set_attr "type" "multi")])

;; Now peepholes to do a call followed by a jump.

(define_peephole
  [(parallel [(set (match_operand 0 "" "")
		   (call (mem:SI (match_operand:SI 1 "call_operand_address" "ps"))
			 (match_operand 2 "" "")))
	      (clobber (reg:SI 15))])
   (set (pc) (label_ref (match_operand 3 "" "")))]
  "short_branch (INSN_UID (insn), INSN_UID (operands[3]))
   && in_same_eh_region (insn, operands[3])
   && in_same_eh_region (insn, ins1)"
  "call\\t%a1, %2\\n\\tadd\\t%%o7, (%l3-.-4), %%o7")

(define_peephole
  [(parallel [(call (mem:SI (match_operand:SI 0 "call_operand_address" "ps"))
		    (match_operand 1 "" ""))
	      (clobber (reg:SI 15))])
   (set (pc) (label_ref (match_operand 2 "" "")))]
  "short_branch (INSN_UID (insn), INSN_UID (operands[2]))
   && in_same_eh_region (insn, operands[2])
   && in_same_eh_region (insn, ins1)"
  "call\\t%a0, %1\\n\\tadd\\t%%o7, (%l2-.-4), %%o7")

(define_peephole
  [(parallel [(set (match_operand 0 "" "")
		   (call (mem:SI (match_operand:DI 1 "call_operand_address" "ps"))
			 (match_operand 2 "" "")))
	      (clobber (reg:DI 15))])
   (set (pc) (label_ref (match_operand 3 "" "")))]
  "TARGET_ARCH64
   && short_branch (INSN_UID (insn), INSN_UID (operands[3]))
   && in_same_eh_region (insn, operands[3])
   && in_same_eh_region (insn, ins1)"
  "call\\t%a1, %2\\n\\tadd\\t%%o7, (%l3-.-4), %%o7")

(define_peephole
  [(parallel [(call (mem:SI (match_operand:DI 0 "call_operand_address" "ps"))
		    (match_operand 1 "" ""))
	      (clobber (reg:DI 15))])
   (set (pc) (label_ref (match_operand 2 "" "")))]
  "TARGET_ARCH64
   && short_branch (INSN_UID (insn), INSN_UID (operands[2]))
   && in_same_eh_region (insn, operands[2])
   && in_same_eh_region (insn, ins1)"
  "call\\t%a0, %1\\n\\tadd\\t%%o7, (%l2-.-4), %%o7")

;; After a nonlocal goto, we need to restore the PIC register, but only
;; if we need it.  So do nothing much here, but we'll check for this in
;; finalize_pic.

;; Make sure this unspec_volatile number agrees with finalize_pic.
(define_insn "nonlocal_goto_receiver"
  [(unspec_volatile [(const_int 0)] 5)]
  "flag_pic"
  ""
  [(set_attr "length" "0")])

(define_insn "trap"
  [(trap_if (const_int 1) (const_int 5))]
  ""
  "ta\\t5"
  [(set_attr "type" "misc")
   (set_attr "length" "1")])

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
  "t%C0\\t%1"
  [(set_attr "type" "misc")
   (set_attr "length" "1")])

(define_insn ""
  [(trap_if (match_operator 0 "noov_compare_op" [(reg:CCX 100) (const_int 0)])
	    (match_operand:SI 1 "arith_operand" "rM"))]
  "TARGET_V9"
  "t%C0\\t%%xcc, %1"
  [(set_attr "type" "misc")
   (set_attr "length" "1")])
