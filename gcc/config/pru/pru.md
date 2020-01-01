;; Machine Description for TI PRU.
;; Copyright (C) 2014-2020 Free Software Foundation, Inc.
;; Contributed by Dimitar Dimitrov <dimitar@dinux.eu>
;; Based on the NIOS2 GCC port.
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; Register numbers.
(define_constants
  [
   (FIRST_ARG_REGNUM		56) ; Argument registers.
   (LAST_ARG_REGNUM		119) ;
   (FIRST_RETVAL_REGNUM		56) ; Return value registers.
   (LAST_RETVAL_REGNUM		60) ;
   (FIRST_CALLEE_SAVED_REGNUM	12) ; Callee saved registers.
   (LAST_CALEE_SAVED_REGNUM	55) ;
   (PROLOGUE_TEMP_REGNUM	4) ; Temporary register to use in prologue.

   (RA_REGNUM			14) ; Return address register r3.w2.
   (FP_REGNUM			16) ; Frame pointer register.
   (MULDST_REGNUM		104) ; Multiply destination register.
   (MULSRC0_REGNUM		112) ; Multiply source register.
   (MULSRC1_REGNUM		116) ; Multiply source register.
   (LAST_NONIO_GP_REGNUM	119) ; Last non-I/O general purpose register.
   (LOOPCNTR_REGNUM		128) ; internal LOOP counter register
   (LAST_GP_REGNUM		132) ; Last general purpose register.

   ;; Target register definitions.
   (STACK_POINTER_REGNUM	8)
   (HARD_FRAME_POINTER_REGNUM	FP_REGNUM)
   (PC_REGNUM			132)
   (FRAME_POINTER_REGNUM	136)
   (ARG_POINTER_REGNUM		140)
   (FIRST_PSEUDO_REGISTER	144)
  ]
)

;; Enumeration of UNSPECs.

(define_c_enum "unspecv" [
  UNSPECV_DELAY_CYCLES_START
  UNSPECV_DELAY_CYCLES_END
  UNSPECV_DELAY_CYCLES_2X_HI
  UNSPECV_DELAY_CYCLES_2X_SI
  UNSPECV_DELAY_CYCLES_1

  UNSPECV_LOOP_BEGIN
  UNSPECV_LOOP_END

  UNSPECV_BLOCKAGE
])

; Length of an instruction (in bytes).
(define_attr "length" "" (const_int 4))
(define_attr "type"
  "unknown,complex,control,alu,cond_alu,st,ld,shift"
  (const_string "complex"))

(define_asm_attributes
 [(set_attr "length" "4")
  (set_attr "type" "complex")])

; There is no pipeline, so our scheduling description is simple.
(define_automaton "pru")
(define_cpu_unit "cpu" "pru")

(define_insn_reservation "everything" 1 (match_test "true") "cpu")

(include "predicates.md")
(include "constraints.md")

;; All supported direct move-modes
(define_mode_iterator MOV8_16_32 [QI QQ UQQ
				  HI HQ UHQ HA UHA
				  SI SQ USQ SA USA SF SD])

(define_mode_iterator MOV8_16 [QI QQ UQQ
			       HI HQ UHQ HA UHA])
(define_mode_iterator MOV32 [SI SQ USQ SA USA SF SD])
(define_mode_iterator MOV64 [DI DF DD DQ UDQ])
(define_mode_iterator QISI [QI HI SI])
(define_mode_iterator HISI [HI SI])
(define_mode_iterator SFDF [SF DF])

;; EQS0/1 for extension source 0/1 and EQD for extension destination patterns.
(define_mode_iterator EQS0 [QI HI SI])
(define_mode_iterator EQS1 [QI HI SI])
(define_mode_iterator EQD [QI HI SI])

;; GCC sign-extends its integer constants.  Hence 0x80 will be represented
;; as -128 for QI mode and 128 for HI and SI modes.  To cope with this,
;; use different constraints to match UBYTE in different modes.
;;
;; Wherever this iterator is used, the corresponding operand has the 'u'
;; print format modifier.  That is how the QI signedness is cured, and
;; the generated assembly contains unsigned constants.
;;
;; If the pattern has no QI operands, then this iterator need not be used.
;;
;; Note that we do not require "uhword_constr" since ALU instructions
;; can use only UBYTE constants.  The MOV patterns are already separately
;; defined for each size, hence no need for an iterator.
(define_mode_attr ubyte_constr [(QI "O") (HI "I") (SI "I")])

;; Move instructions

(define_expand "mov<mode>"
  [(set (match_operand:MOV8_16_32 0 "nonimmediate_operand")
	(match_operand:MOV8_16_32 1 "general_operand"))]
  ""
{
  /* It helps to split constant loading and memory access
     early, so that the LDI/LDI32 instructions can be hoisted
     outside a loop body.  */
  if (MEM_P (operands[0]))
    operands[1] = force_reg (<MODE>mode, operands[1]);
})

;; Keep a single pattern for 32 bit MOV operations.  LRA requires that the
;; movXX patterns be unified for any given mode.
;;
;; Note: Assume that Program Mem (T constraint) can fit in 16 bits!
(define_insn "prumov<mode>"
  [(set (match_operand:MOV32 0 "nonimmediate_operand" "=m,r,r,r,r,r")
	(match_operand:MOV32 1 "general_operand"      "r,m,r,T,J,iF"))]
  ""
  "@
    sb%B0o\\t%b1, %0, %S0
    lb%B1o\\t%b0, %1, %S1
    mov\\t%0, %1
    ldi\\t%0, %%pmem(%1)
    ldi\\t%0, %1
    ldi32\\t%0, %1"
  [(set_attr "type" "st,ld,alu,alu,alu,alu")
   (set_attr "length" "4,4,4,4,4,8")])


;; Separate pattern for 8 and 16 bit moves, since LDI32 pseudo instruction
;; cannot handle byte and word-sized registers.
;;
;; Note: Constraint N is fine for both QI and HI mode, since it is used
;; in the context of 16 bit constant integer.
(define_insn "prumov<mode>"
  [(set (match_operand:MOV8_16 0 "nonimmediate_operand" "=m,r,r,r,r")
	(match_operand:MOV8_16 1 "general_operand"      "r,m,r,T,N"))]
  ""
  "@
    sb%B0o\\t%b1, %0, %S0
    lb%B1o\\t%b0, %1, %S1
    mov\\t%0, %1
    ldi\\t%0, %%pmem(%1)
    ldi\\t%0, (%1) & 0xffff"
  [(set_attr "type" "st,ld,alu,alu,alu")
   (set_attr "length" "4")])


; Pmode is 32 bits for PRU so symbolic constants cannot be 64 bits.  Hence
; this pattern handles only numeric constants.
;
; Note: Unlike the arithmetics, here we cannot use "&" output modifier.
; GCC expects to be able to move registers around "no matter what".
; Forcing DI reg alignment (akin to microblaze's HARD_REGNO_MODE_OK)
; does not seem efficient, and will violate TI ABI.
(define_insn "mov<mode>"
  [(set (match_operand:MOV64 0 "nonimmediate_operand" "=m,r,r,r,r,r")
	(match_operand:MOV64 1 "general_operand"      "r,m,r,T,J,nF"))]
  ""
{
  switch (which_alternative)
    {
    case 0:
      return "sb%B0o\\t%b1, %0, %S0";
    case 1:
      return "lb%B1o\\t%b0, %1, %S1";
    case 2:
      /* careful with overlapping source and destination regs.  */
      gcc_assert (GP_REG_P (REGNO (operands[0])));
      gcc_assert (GP_REG_P (REGNO (operands[1])));
      if (REGNO (operands[0]) == (REGNO (operands[1]) + 4))
	return "mov\\t%N0, %N1\;mov\\t%F0, %F1";
      else
	return "mov\\t%F0, %F1\;mov\\t%N0, %N1";
    case 3:
      return "ldi\\t%F0, %%pmem(%1)\;ldi\\t%N0, 0";
    case 4:
      return "ldi\\t%F0, %1\;ldi\\t%N0, 0";
    case 5:
      return "ldi32\\t%F0, %w1\;ldi32\\t%N0, %W1";
    default:
      gcc_unreachable ();
  }
}
  [(set_attr "type" "st,ld,alu,alu,alu,alu")
   (set_attr "length" "4,4,8,8,8,16")])

;
; load_multiple pattern(s).
;
; ??? Due to reload problems with replacing registers inside match_parallel
; we currently support load_multiple/store_multiple only after reload.
;
; Idea taken from the s390 port.

(define_expand "load_multiple"
  [(match_par_dup 3 [(set (match_operand 0 "")
			  (match_operand 1 ""))
		     (use (match_operand 2 ""))])]
  "reload_completed"
{
  machine_mode mode;
  int regno;
  int count;
  rtx base_reg;
  poly_int64 base_offs;
  int i;

  /* Support only loading a constant number of fixed-point registers from
     memory.  */
  if (GET_CODE (operands[2]) != CONST_INT
      || GET_CODE (operands[1]) != MEM
      || GET_CODE (operands[0]) != REG)
    FAIL;

  count = INTVAL (operands[2]);
  regno = REGNO (operands[0]);
  mode = GET_MODE (operands[0]);
  if (mode != QImode)
    FAIL;

  operands[3] = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (count));

  gcc_assert (!can_create_pseudo_p ());

  base_reg = strip_offset (XEXP (operands[1], 0), &base_offs);
  if (GET_CODE (base_reg) != REG)
    FAIL;

  for (i = 0; i < count; i++)
    XVECEXP (operands[3], 0, i)
      = gen_rtx_SET (gen_rtx_REG (mode, regno + i),
		     change_address (operands[1], mode,
		       plus_constant (Pmode, base_reg,
				      base_offs + i * GET_MODE_SIZE (mode))));
})

(define_insn "*pru_load_multiple"
  [(match_parallel 0 "load_multiple_operation"
		   [(set (match_operand:QI 1 "register_operand" "=r")
			 (match_operand:QI 2 "memory_operand"   "m"))])]
  "reload_completed"
{
  int nregs = XVECLEN (operands[0], 0);
  operands[0] = GEN_INT (nregs);
  return "lb%B2o\\t%b1, %2, %0";
}
  [(set_attr "type" "ld")])

;
; store multiple pattern(s).
;

(define_expand "store_multiple"
  [(match_par_dup 3 [(set (match_operand 0 "")
			  (match_operand 1 ""))
		     (use (match_operand 2 ""))])]
  "reload_completed"
{
  machine_mode mode;
  int regno;
  int count;
  rtx base_reg;
  poly_int64 base_offs;
  int i;

  /* Support only storing a constant number of fixed-point registers to
     memory.  */
  if (GET_CODE (operands[2]) != CONST_INT
      || GET_CODE (operands[0]) != MEM
      || GET_CODE (operands[1]) != REG)
    FAIL;

  count = INTVAL (operands[2]);
  regno = REGNO (operands[1]);
  mode = GET_MODE (operands[1]);
  if (mode != QImode)
    FAIL;

  operands[3] = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (count));

  gcc_assert (!can_create_pseudo_p ());

  base_reg = strip_offset (XEXP (operands[0], 0), &base_offs);
  if (GET_CODE (base_reg) != REG)
    FAIL;

  for (i = 0; i < count; i++)
    XVECEXP (operands[3], 0, i)
      = gen_rtx_SET (change_address (operands[0], mode,
		       plus_constant (Pmode, base_reg,
				      base_offs + i * GET_MODE_SIZE (mode))),
		     gen_rtx_REG (mode, regno + i));
})

(define_insn "*pru_store_multiple"
  [(match_parallel 0 "store_multiple_operation"
		   [(set (match_operand:QI 1 "memory_operand"   "=m")
			 (match_operand:QI 2 "register_operand" "r"))])]
  "reload_completed"
{
  int nregs = XVECLEN (operands[0], 0);
  operands[0] = GEN_INT (nregs);
  return "sb%B1o\\t%b2, %1, %0";
}
  [(set_attr "type" "st")])

;; Zero extension patterns
;;
;; Unfortunately we cannot use lbbo to load AND zero-extent a value.
;; The burst length parameter of the LBBO instruction designates not only
;; the number of memory data bytes fetched, but also the number of register
;; byte fields written.
(define_expand "zero_extend<EQS0:mode><EQD:mode>2"
  [(set (match_operand:EQD 0 "register_operand")
	(zero_extend:EQD (match_operand:EQS0 1 "register_operand")))]
  ""
  "")

(define_insn "*zero_extend<EQS0:mode><EQD:mode>2"
  [(set (match_operand:EQD 0 "register_operand" "=r")
	(zero_extend:EQD (match_operand:EQS0 1 "register_operand" "r")))]
  ""
  "mov\\t%0, %1"
  [(set_attr "type"     "alu")])

;; Sign extension patterns.  We have to emulate them due to lack of
;; signed operations in PRU's ALU.

(define_insn "extend<EQS0:mode><EQD:mode>2"
  [(set (match_operand:EQD 0 "register_operand"			  "=r")
	(sign_extend:EQD (match_operand:EQS0 1 "register_operand"  "r")))]
  ""
{
  return pru_output_sign_extend (operands);
}
  [(set_attr "type" "complex")
   (set_attr "length" "12")])

;; Bit extraction
;; We define it solely to allow combine to choose SImode
;; for word mode when trying to match our cbranch_qbbx_* insn.
;;
;; Check how combine.c:make_extraction() uses
;; get_best_reg_extraction_insn() to select the op size.
(define_insn "extzv<mode>"
  [(set (match_operand:QISI 0 "register_operand"	"=r")
	  (zero_extract:QISI
	   (match_operand:QISI 1 "register_operand"	"r")
	   (match_operand:QISI 2 "const_int_operand"	"i")
	   (match_operand:QISI 3 "const_int_operand"	"i")))]
  ""
  "lsl\\t%0, %1, (%S0 * 8 - %2 - %3)\;lsr\\t%0, %0, (%S0 * 8 - %2)"
  [(set_attr "type" "complex")
   (set_attr "length" "8")])



;; Arithmetic Operations

(define_expand "add<mode>3"
  [(set (match_operand:QISI 0 "register_operand")
	(plus:QISI (match_operand:QISI 1 "register_operand")
		 (match_operand:QISI 2 "nonmemory_operand")))]
  ""
  "")

(define_insn "adddi3"
  [(set (match_operand:DI 0 "register_operand"		    "=&r,&r,&r")
	(plus:DI (match_operand:DI 1 "register_operand"	    "%r,r,r")
		 (match_operand:DI 2 "reg_or_ubyte_operand" "r,I,M")))]
  ""
  "@
   add\\t%F0, %F1, %F2\;adc\\t%N0, %N1, %N2
   add\\t%F0, %F1, %2\;adc\\t%N0, %N1, 0
   sub\\t%F0, %F1, %n2\;suc\\t%N0, %N1, 0"
  [(set_attr "type" "alu")
   (set_attr "length" "8")])

(define_expand "sub<mode>3"
  [(set (match_operand:QISI 0 "register_operand")
	(minus:QISI (match_operand:QISI 1 "reg_or_ubyte_operand")
		    (match_operand:QISI 2 "reg_or_ubyte_operand")))]
  ""
  "")

(define_insn "subdi3"
  [(set (match_operand:DI 0 "register_operand"		      "=&r,&r")
	(minus:DI (match_operand:DI 1 "reg_or_ubyte_operand"  "r,I")
		  (match_operand:DI 2 "register_operand"      "r,r")))]
  ""
  "@
   sub\\t%F0, %F1, %F2\;suc\\t%N0, %N1, %N2
   rsb\\t%F0, %F2, %1\;rsc\\t%N0, %N2, 0"
  [(set_attr "type" "alu")
   (set_attr "length" "8")])

;;  Negate and ones complement

(define_expand "neg<mode>2"
  [(set (match_operand:QISI 0 "register_operand")
	(neg:QISI (match_operand:QISI 1 "register_operand")))]
  ""
  "")

(define_expand "one_cmpl<mode>2"
  [(set (match_operand:QISI 0 "register_operand")
	(not:QISI (match_operand:QISI 1 "register_operand")))]
  ""
  "")

;;  Integer logical Operations
;;
;; TODO - add optimized cases that exploit the fact that we can get away
;; with a single machine op for special constants, e.g. UBYTE << (0/8/16/24)

(define_code_iterator LOGICAL [and ior xor umin umax])
(define_code_attr logical_asm [(and "and") (ior "or") (xor "xor") (umin "min") (umax "max")])

(define_code_iterator LOGICAL_BITOP [and ior xor])
(define_code_attr logical_bitop_asm [(and "and") (ior "or") (xor "xor")])

(define_expand "<code><mode>3"
  [(set (match_operand:QISI 0 "register_operand")
	(LOGICAL:QISI (match_operand:QISI 1 "register_operand")
		      (match_operand:QISI 2 "reg_or_ubyte_operand")))]
  ""
  "")


;;  Shift instructions

(define_code_iterator SHIFT  [ashift lshiftrt])
(define_code_attr shift_op   [(ashift "ashl") (lshiftrt "lshr")])
(define_code_attr shift_asm  [(ashift "lsl") (lshiftrt "lsr")])

(define_expand "<shift_op><mode>3"
  [(set (match_operand:QISI 0 "register_operand")
	(SHIFT:QISI (match_operand:QISI 1 "register_operand")
		    (match_operand:QISI 2 "shift_operand")))]
  ""
  "")

; Expand to a loop of single-position arithmetic shifts, which
; we can handle.  Pseudo code:
;     tmpval = src;
;     QImode cntr = nshifts & 0xff;
;     while (cntr)
;       {
;         tmpval >>= 1;
;         cntr--;
;       }
;     dst = tmpval;
;
; Note that the number of shifts is truncated to QImode.  This is a fair
; assumption for a loop-based shifting implementation.
(define_expand "ashr<mode>3"
  [(set (match_operand:QISI 0 "register_operand")
	  (ashiftrt:QISI
	    (match_operand:QISI 1 "register_operand")
	    (match_operand:QI 2 "reg_or_const_1_operand")))]
  ""
{
  rtx dst = operands[0];
  rtx src = operands[1];
  rtx nshifts = operands[2];
  rtx_code_label *loop_label;
  rtx_code_label *ashr_end_label;
  rtx test, tmpval, cntr;

  if (const_1_operand (nshifts, VOIDmode))
    {
      emit_insn (gen_ashr<mode>3_single (dst, src, nshifts));
      DONE;
    }

  tmpval = gen_reg_rtx (<MODE>mode);
  emit_move_insn (tmpval, src);

  cntr = gen_reg_rtx (QImode);
  emit_move_insn (cntr, nshifts);

  loop_label = gen_label_rtx ();
  ashr_end_label = gen_label_rtx ();

  emit_label (loop_label);
  test = gen_rtx_EQ (VOIDmode, cntr, const0_rtx);
  emit_jump_insn (gen_cbranchqi4 (test, cntr, const0_rtx, ashr_end_label));

  emit_insn (gen_ashr<mode>3_single (tmpval, tmpval, const1_rtx));
  emit_insn (gen_addqi3 (cntr, cntr, GEN_INT (-1)));

  emit_jump_insn (gen_jump (loop_label));
  JUMP_LABEL (get_last_insn ()) = loop_label;
  LABEL_NUSES (loop_label)++;
  emit_barrier ();

  emit_label (ashr_end_label);

  emit_move_insn (dst, tmpval);

  DONE;
})

(define_insn "ashr<mode>3_single"
  [(set (match_operand:QISI 0 "register_operand"	"=r")
	  (ashiftrt:QISI
	    (match_operand:QISI 1 "register_operand"	"r")
	    (match_operand:QI 2 "const_1_operand"	"P")))]
  ""
  "lsr\\t%0, %1, 1\;qbbc LSIGN%=, %0, (%S0 * 8) - 2\;set %0, %0, (%S0 * 8) - 1\;LSIGN%=:"
  [(set_attr "type" "alu")
   (set_attr "length" "12")])


;; Include ALU patterns with zero-extension of operands.  That's where
;; the real insns are defined.

(include "alu-zext.md")

;; DI logical ops could be automatically split into WORD-mode ops in
;; expand_binop().  But then we'll miss an opportunity to use SI mode
;; operations, since WORD mode for PRU is QI.
(define_insn "<code>di3"
  [(set (match_operand:DI 0 "register_operand"		"=&r,&r")
	  (LOGICAL_BITOP:DI
	    (match_operand:DI 1 "register_operand"	"%r,r")
	    (match_operand:DI 2 "reg_or_ubyte_operand"	"r,I")))]
  ""
  "@
   <logical_bitop_asm>\\t%F0, %F1, %F2\;<logical_bitop_asm>\\t%N0, %N1, %N2
   <logical_bitop_asm>\\t%F0, %F1, %2\;<logical_bitop_asm>\\t%N0, %N1, 0"
  [(set_attr "type" "alu")
   (set_attr "length" "8")])


(define_insn "one_cmpldi2"
  [(set (match_operand:DI 0 "register_operand"		"=r")
	(not:DI (match_operand:DI 1 "register_operand"	"r")))]
  ""
{
  /* careful with overlapping source and destination regs.  */
  gcc_assert (GP_REG_P (REGNO (operands[0])));
  gcc_assert (GP_REG_P (REGNO (operands[1])));
  if (REGNO (operands[0]) == (REGNO (operands[1]) + 4))
    return "not\\t%N0, %N1\;not\\t%F0, %F1";
  else
    return "not\\t%F0, %F1\;not\\t%N0, %N1";
}
  [(set_attr "type" "alu")
   (set_attr "length" "8")])

;; Multiply instruction.  The nop is required to ensure that Rmd0 and Rms0
;; registers are sampled and multiplication is executed on those values.
;; Only after that one cycle can xin obtain the result.

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "pru_muldst_operand"	   "=Rmd0")
	(mult:SI (match_operand:SI 1 "pru_mulsrc0_operand" "%Rms0")
		 (match_operand:SI 2 "pru_mulsrc1_operand" "Rms1")))]
  ""
  "nop\;xin\\t0, %0, 4"
  [(set_attr "type" "alu")
   (set_attr "length" "8")])

;; Prologue, Epilogue and Return

(define_expand "prologue"
  [(const_int 1)]
  ""
{
  pru_expand_prologue ();
  DONE;
})

(define_expand "epilogue"
  [(return)]
  ""
{
  pru_expand_epilogue (false);
  DONE;
})

(define_expand "sibcall_epilogue"
  [(return)]
  ""
{
  pru_expand_epilogue (true);
  DONE;
})

(define_insn "return"
  [(simple_return)]
  "pru_can_use_return_insn ()"
  "ret")

(define_insn "simple_return"
  [(simple_return)]
  ""
  "ret")

;; Block any insns from being moved before this point, since the
;; profiling call to mcount can use various registers that aren't
;; saved or used to pass arguments.

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] UNSPECV_BLOCKAGE)]
  ""
  ""
  [(set_attr "type" "unknown")
   (set_attr "length" "0")])

;;  Jumps and calls

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))]
  ""
  "jmp\\t%0"
  [(set_attr "type" "control")])

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0)))]
  ""
  "jmp\\t%%label(%l0)"
  [(set_attr "type" "control")])


(define_expand "call"
  [(parallel [(call (match_operand 0 "")
		    (match_operand 1 ""))
	      (clobber (reg:HI RA_REGNUM))])]
  ""
  "")

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "")
		   (call (match_operand 1 "")
			 (match_operand 2 "")))
	      (clobber (reg:HI RA_REGNUM))])]
  ""
  "")

(define_insn "*call"
  [(call (mem:SI (match_operand:SI 0 "call_operand" "i,r"))
	 (match_operand 1))
   (clobber (reg:HI RA_REGNUM))]
  ""
  "@
    call\\t%%label(%0)
    call\\t%0"
  [(set_attr "type" "control")])

(define_insn "*call_value"
  [(set (match_operand 0)
	(call (mem:SI (match_operand:SI 1 "call_operand" "i,r"))
	      (match_operand 2)))
   (clobber (reg:HI RA_REGNUM))]
  ""
  "@
    call\\t%%label(%1)
    call\\t%1"
  [(set_attr "type" "control")])

(define_expand "sibcall"
  [(parallel [(call (match_operand 0 "")
		    (match_operand 1 ""))
	      (return)])]
  ""
  "")

(define_expand "sibcall_value"
  [(parallel [(set (match_operand 0 "")
		   (call (match_operand 1 "")
			 (match_operand 2 "")))
	      (return)])]
  ""
  "")

(define_insn "*sibcall"
 [(call (mem:SI (match_operand:SI 0 "call_operand" "i,Rsib"))
	(match_operand 1))
  (return)]
  "SIBLING_CALL_P (insn)"
  "@
    jmp\\t%%label(%0)
    jmp\\t%0"
  [(set_attr "type" "control")])

(define_insn "*sibcall_value"
 [(set (match_operand 0 "register_operand" "")
       (call (mem:SI (match_operand:SI 1 "call_operand" "i,Rsib"))
	     (match_operand 2)))
  (return)]
  "SIBLING_CALL_P (insn)"
  "@
    jmp\\t%%label(%1)
    jmp\\t%1"
  [(set_attr "type" "control")])

(define_insn "*tablejump"
  [(set (pc)
	(match_operand:SI 0 "register_operand" "r"))
   (use (label_ref (match_operand 1)))]
  ""
  "jmp\\t%0"
  [(set_attr "type" "control")])

;; Expand the cbranch pattern in order to assign different constraints for
;; signed and unsigned comparisons.
(define_expand "cbranch<mode>4"
  [(set (pc)
     (if_then_else
       (match_operator 0 "ordered_comparison_operator"
	 [(match_operand:QISI 1 "register_operand")
	  (match_operand:QISI 2 "reg_or_const_int_operand")])
       (label_ref (match_operand 3 ""))
       (pc)))]
  ""
{
  /* Ensure our patterns will be able to handle the particular const_int.  */
  if (CONST_INT_P (operands[2]))
    {
      HOST_WIDE_INT ival = INTVAL (operands[2]);

      /* For signed comparisons, we cannot play games with the const_int's
	 sign.  PRU patterns do not support negative integer constants.  */
      if (pru_signed_cmp_operator (operands[0], VOIDmode) && !UBYTE_INT (ival))
	{
	  if (can_create_pseudo_p ())
	    operands[2] = force_reg (<MODE>mode, operands[2]);
	  else
	    FAIL;
	}

      /* For unsigned comparisons, be prepared to handle the QI quirk.  */
      if (pru_cmp_operator (operands[0], VOIDmode)
	  && !const_ubyte_operand (operands[2], <MODE>mode))
	{
	  if (can_create_pseudo_p ())
	    operands[2] = force_reg (<MODE>mode, operands[2]);
	  else
	    FAIL;
	}
    }
})

(define_insn "cbranch<mode>4_unsigned"
  [(set (pc)
     (if_then_else
       (match_operator 0 "pru_cmp_operator"
	 [(match_operand:QISI 1 "register_operand" "r")
	  (match_operand:QISI 2 "reg_or_ubyte_operand" "r<QISI:ubyte_constr>")])
       (label_ref (match_operand 3))
       (pc)))]
  ""
{
  const bool is_near = (get_attr_length (insn) == 4);

  /* PRU comparisons reverse the operand order (OP2 cmp OP1),
     so swap the condition.  */
  if (is_near)
    return "qb%P0\t%l3, %1, %u2";
  else
    return "qb%Q0\t.+8, %1, %u2\;jmp\t%%label(%l3)";
}
  [(set_attr "type" "control")
   (set (attr "length")
	(if_then_else
	    (and (ge (minus (match_dup 3) (pc)) (const_int -2040))
		 (le (minus (match_dup 3) (pc)) (const_int 2036)))
	    (const_int 4)
	    (const_int 8)))])

;; Unlike ALU operations, the const_int's sign here is important.  So we
;; cannot use ubyte_constr.
;;
;; NOTE: The short branch check has no typo!  We must be conservative and
;; take into account the worst case of having a signed comparison with a
;; "far taken branch" label, which amounts to 7 instructions.
(define_insn "cbranch<mode>4_signed"
  [(set (pc)
     (if_then_else
       (match_operator 0 "pru_signed_cmp_operator"
	 [(match_operand:QISI 1 "register_operand" "r,r,r")
	  (match_operand:QISI 2 "reg_or_ubyte_operand" "r,Z,I")])
       (label_ref (match_operand 3))
       (pc)))]
  ""
{
  const int length = (get_attr_length (insn));
  const bool is_near = (length == 20);
  enum rtx_code code = GET_CODE (operands[0]);

  if (which_alternative == 0)
    return pru_output_signed_cbranch (operands, is_near);
  else if (which_alternative == 1 && (code == LT || code == GE))
    return pru_output_signed_cbranch_zeroop2 (operands, is_near);
  else
    return pru_output_signed_cbranch_ubyteop2 (operands, is_near);
}
  [(set_attr "type" "control")
   (set (attr "length")
	(if_then_else
	    (and (ge (minus (match_dup 3) (pc)) (const_int -2020))
		 (le (minus (match_dup 3) (pc)) (const_int 2016)))
	    (const_int 20)
	    (const_int 28)))])

(define_expand "cbranch<mode>4"
  [(set (pc)
	(if_then_else (match_operator 0 "pru_fp_comparison_operator"
		       [(match_operand:SFDF 1 "register_operand")
			(match_operand:SFDF 2 "register_operand")])
		      (label_ref (match_operand 3 ""))
		      (pc)))]
  ""
{
  rtx t = pru_expand_fp_compare (operands[0], VOIDmode);
  operands[0] = t;
  operands[1] = XEXP (t, 0);
  operands[2] = XEXP (t, 1);
})

;
; Bit test branch

(define_code_iterator BIT_TEST  [eq ne])
(define_code_attr qbbx_op   [(eq "qbbc") (ne "qbbs")])
(define_code_attr qbbx_negop   [(eq "qbbs") (ne "qbbc")])

(define_insn "cbranch_qbbx_<BIT_TEST:code><EQS0:mode><EQS1:mode><EQD:mode>4"
 [(set (pc)
   (if_then_else
    (BIT_TEST (zero_extract:EQD
	 (match_operand:EQS0 0 "register_operand" "r")
	 (const_int 1)
	 (match_operand:EQS1 1 "reg_or_ubyte_operand" "r<EQS1:ubyte_constr>"))
     (const_int 0))
    (label_ref (match_operand 2))
    (pc)))]
  ""
{
  const int length = (get_attr_length (insn));
  const bool is_near = (length == 4);
  if (is_near)
    return "<BIT_TEST:qbbx_op>\\t%l2, %0, %u1";
  else
    return "<BIT_TEST:qbbx_negop>\\t.+8, %0, %u1\;jmp\\t%%label(%l2)";
}
  [(set_attr "type" "control")
   (set (attr "length")
      (if_then_else
	  (and (ge (minus (match_dup 2) (pc)) (const_int -2048))
	       (le (minus (match_dup 2) (pc)) (const_int 2044)))
	  (const_int 4)
	  (const_int 8)))])

;; ::::::::::::::::::::
;; ::
;; :: Low Overhead Looping - idea "borrowed" from MEP
;; ::
;; ::::::::::::::::::::

;; This insn is volatile because we'd like it to stay in its original
;; position, just before the loop header.  If it stays there, we might
;; be able to convert it into a "loop" insn.
(define_insn "doloop_begin_internal<mode>"
  [(set (match_operand:HISI 0 "register_operand" "=r")
	(unspec_volatile:HISI
	 [(match_operand:HISI 1 "reg_or_ubyte_operand" "rI")
	  (match_operand 2 "const_int_operand" "")] UNSPECV_LOOP_BEGIN))]
  ""
{
  gcc_unreachable ();
})

(define_expand "doloop_begin"
  [(use (match_operand 0 "register_operand"))
   (use (match_operand 1 ""))]
  "TARGET_OPT_LOOP"
{
  pru_emit_doloop (operands, 0);
  DONE;
})

; Note: "JUMP_INSNs and CALL_INSNs are not allowed to have any output
; reloads;".  Hence this insn must be prepared for a counter that is
; not a register.
(define_insn "doloop_end_internal<mode>"
  [(set (pc)
	(if_then_else (ne (match_operand:HISI 0 "nonimmediate_operand" "+r,*m")
			  (const_int 1))
		      (label_ref (match_operand 1))
		      (pc)))
   (set (match_dup 0)
	(plus:HISI (match_dup 0)
		 (const_int -1)))
   (unspec [(match_operand 2 "const_int_operand" "")] UNSPECV_LOOP_END)
   (clobber (match_scratch:HISI 3 "=X,&r"))]
  ""
{
  gcc_unreachable ();
}
  ;; Worst case length:
  ;;
  ;;	  lbbo op3_reg, op3_ptr	  4'
  ;;	  sub <op3_reg>, 1	  4
  ;;	  qbeq .+8, <op3_reg>, 0  4
  ;;	  jmp <op1>		  4
  ;;	  sbbo op3_reg, op3_ptr	  4
  [(set (attr "length")
      (if_then_else
	(and (ge (minus (pc) (match_dup 1)) (const_int 0))
	     (le (minus (pc) (match_dup 1)) (const_int 1020)))
	(cond [(eq_attr "alternative" "0") (const_int 4)]
	       (const_int 12))
	(cond [(eq_attr "alternative" "0") (const_int 12)]
	       (const_int 20))))])

(define_expand "doloop_end"
  [(use (match_operand 0 "nonimmediate_operand"))
   (use (label_ref (match_operand 1 "")))]
  "TARGET_OPT_LOOP"
{
  if (GET_CODE (operands[0]) == REG && GET_MODE (operands[0]) == QImode)
    FAIL;
  pru_emit_doloop (operands, 1);
  DONE;
})

(define_insn "pruloop<mode>"
  [(set (reg:HISI LOOPCNTR_REGNUM)
	(unspec:HISI [(match_operand:HISI 0 "reg_or_ubyte_operand" "rI")
		    (label_ref (match_operand 1))]
		   UNSPECV_LOOP_BEGIN))]
  ""
  "loop\\t%l1, %0")

(define_insn "pruloop_end"
  [(unspec [(const_int 0)] UNSPECV_LOOP_END)]
  ""
  "# loop end"
  [(set_attr "length" "0")])


;;  Misc patterns

(define_insn "delay_cycles_start"
  [(unspec_volatile [(match_operand 0 "immediate_operand" "i")]
		    UNSPECV_DELAY_CYCLES_START)]
  ""
  "/* Begin %0 cycle delay.  */"
  [(set_attr "length" "0")])

(define_insn "delay_cycles_end"
  [(unspec_volatile [(match_operand 0 "immediate_operand" "i")]
		    UNSPECV_DELAY_CYCLES_END)]
  ""
  "/* End %0 cycle delay.  */"
  [(set_attr "length" "0")])


(define_insn "delay_cycles_2x_plus1_hi"
  [(unspec_volatile [(match_operand:SI 0 "const_uhword_operand" "J")]
		    UNSPECV_DELAY_CYCLES_2X_HI)
   (clobber (match_scratch:SI 1 "=&r"))]
  ""
  "ldi\\t%1, %0\;sub\\t%1, %1, 1\;qbne\\t.-4, %1, 0"
  [(set_attr "length" "12")])


; Do not use LDI32 here because we do not want
; to accidentally loose one instruction cycle.
(define_insn "delay_cycles_2x_plus2_si"
  [(unspec_volatile [(match_operand:SI 0 "const_int_operand" "n")]
		    UNSPECV_DELAY_CYCLES_2X_SI)
   (clobber (match_scratch:SI 1 "=&r"))]
  ""
  "ldi\\t%1.w0, %L0\;ldi\\t%1.w2, %H0\;sub\\t%1, %1, 1\;qbne\\t.-4, %1, 0"
  [(set_attr "length" "16")])

(define_insn "delay_cycles_1"
  [(unspec_volatile [(const_int 0) ] UNSPECV_DELAY_CYCLES_1)]
  ""
  "nop\\t# delay_cycles_1"
)


(define_insn "nop"
  [(const_int 0)]
  ""
  "nop"
  [(set_attr "type" "alu")])

(define_insn "nop_loop_guard"
  [(const_int 0)]
  ""
  "nop\\t# Loop end guard"
  [(set_attr "type" "alu")])
