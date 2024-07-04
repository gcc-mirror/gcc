;; GCC machine description for SH synchronization instructions.
;; Copyright (C) 2011-2024 Free Software Foundation, Inc.
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
;;
;;
;; Atomic integer operations for the Renesas / SuperH SH CPUs.
;;
;; On SH CPUs atomic integer operations can be done either in 'software' or
;; in 'hardware' in various styles.  True hardware support was introduced
;; with the SH4A.  Some SH2A dual-core models (e.g. SH7205) also come with
;; 'semaphore' hardware registers, but these are currently unsupported.
;; All SH CPUs support the 'tas.b' instruction, which can be optionally used
;; to implement the 'atomic_test_and_set' builtin.
;; The following atomic options and models are supported.
;;
;; tas.b atomic_test_and_set (-mtas)
;;
;; Depending on the particular hardware configuration, usage of the 'tas.b'
;; instruction might be undesired or even unsafe.  Thus, it has to be
;; enabled by the user explicitly.  If it is not enabled, the
;; 'atomic_test_and_set' builtin is implemented either with hardware or with
;; software atomics, depending on which is enabled.  It is also possible to
;; enable the 'tas.b' instruction only, without enabling support for the 
;; other atomic operations.
;;
;;
;; Hardware Atomics (-matomic-model=hard-llcs; SH4A only)
;;
;; Hardware atomics implement all atomic operations using the 'movli.l' and
;; 'movco.l' instructions that are availble on SH4A.  On multi-core hardware
;; configurations hardware atomics is the only safe mode.
;; However, it can also be safely used on single-core configurations.
;; Since these instructions operate on SImode memory only, QImode and HImode
;; have to be emulated with SImode and subreg masking, which results in
;; larger code.
;;
;;
;; gUSA Software Atomics (-matomic-model=soft-gusa; SH3*, SH4* only)
;;
;; On single-core systems there can only be one execution context running
;; at a given point in time.  This allows the usage of rewindable atomic
;; sequences, which effectively emulate locked-load / conditional-store
;; operations.  This requires complementary support in the interrupt / 
;; exception handling code (e.g. kernel) and does not work safely on multi-
;; core configurations.
;;
;; When an execution context is interrupted while it is an atomic
;; sequence, the interrupted context's PC is rewound to the beginning of
;; the atomic sequence by the interrupt / exception handling code, before
;; transferring control to another execution context.  This is done by
;; something like...
;;
;;	if (interrupted_context_in_atomic_sequence
;;	    && interrupted_pc < atomic_exitpoint)
;;	  interrupted_pc = atomic_entrypoint;
;;
;; This method is also known as gUSA ("g" User Space Atomicity) and the
;; Linux kernel for SH3/SH4 implements support for such software atomic
;; sequences.  It can also be implemented in freestanding environments.
;;
;; For this the following atomic sequence ABI is used.
;;
;; r15 >= 0:	Execution context is not in an atomic sequence.
;;
;; r15  < 0:	Execution context is in an atomic sequence and r15
;;		holds the negative byte length of the atomic sequence.
;;		In this case the following applies:
;;
;;		r0:	PC of the first instruction after the atomic
;;			write-back instruction (exit point).
;;			The entry point PC of the atomic sequence can be 
;;			determined by doing r0 + r15.
;;
;;		r1:	Saved r15 stack pointer before entering the
;;			atomic sequence.
;;
;; An example atomic add sequence would look like:
;;
;;	mova	.Lend,r0		! .Lend must be 4-byte aligned.
;;	mov	r15,r1
;;	.align 2			! Insert aligning nop if needed.
;;	mov	#(.Lstart - .Lend),r15	! Enter atomic sequence
;;.Lstart:
;;	mov.l	@r4,r2			! read value
;;	add	r2,r5			! modify value
;;	mov.l	r5,@r4			! write-back
;;.Lend:
;;	mov	r1,r15			! Exit atomic sequence
;;					! r2 holds the previous value.
;;					! r5 holds the new value.
;;
;; Notice that due to the restrictions of the mova instruction, the .Lend
;; label must always be 4-byte aligned.  Aligning the .Lend label would
;; potentially insert a nop after the write-back instruction which could
;; make the sequence to be rewound, although it has already passed the
;; write-back instruction.  This would make it execute twice.
;; For correct operation the atomic sequences must not be rewound after
;; they have passed the write-back instruction.
;;
;; This is model works only on SH3* and SH4* because the stack pointer (r15)
;; is set to an invalid pointer temporarily.  SH1* and SH2* CPUs will try
;; to push SR and PC registers on the stack when an interrupt / exception
;; occurs, and thus require the stack pointer (r15) always to be valid.
;;
;;
;; TCB Software Atomics (-matomic-model=soft-tcb)
;;
;; This model is a variation of the gUSA model.  The concept of rewindable
;; atomic sequences is the same, but it does not use the stack pointer (r15)
;; for signaling the 'is in atomic sequence' condition.  Instead, a variable
;; in the thread control block (TCB) is set to hold the exit point of the
;; atomic sequence.  This assumes that the GBR is used as a thread pointer
;; register.  The offset of the variable in the TCB to be used must be
;; specified with an additional option 'gbr-offset', such as:
;;	-matomic-model=soft-tcb,gbr-offset=4
;;
;; For this model the following atomic sequence ABI is used.
;; 
;; @(#x,gbr) == 0:  Execution context is not in an atomic sequence.
;;
;; @(#x,gbr) != 0:  Execution context is in an atomic sequence.  In this
;;		    case the following applies:
;;
;;		    @(#x,gbr):	PC of the first instruction after the atomic
;;				write-back instruction (exit point).
;;
;;		    r1:		Negative byte length of the atomic sequence.
;;				The entry point PC of the sequence can be
;;				determined by doing @(#x,gbr) + r1
;;
;; Note: #x is the user specified gbr-offset.
;;
;;
;; Interrupt-Flipping Software Atomics (-matomic-model=soft-imask)
;;
;; This model achieves atomicity by temporarily disabling interrupts for
;; the duration of the atomic sequence.  This works only when the program
;; runs in privileged mode but does not require any support from the
;; interrupt / exception handling code.  There is no particular ABI.
;; To disable interrupts the SR.IMASK bits are set to '1111'.
;; This method is not as efficient as the other software atomic models,
;; since loading and storing SR (in order to flip interrupts on / off)
;; requires using multi-cycle instructions.  Moreover, it can potentially
;; increase the interrupt latency which might be important for hard-realtime
;; applications.
;;
;;
;; Compatibility Notes
;;
;; On single-core SH4A CPUs software atomic aware interrupt / exception code
;; is actually compatible with user code that utilizes hardware atomics.
;; Since SImode hardware atomic sequences are more compact on SH4A they are
;; always used, regardless of the selected atomic model.  This atomic model
;; mixing can be disabled by setting the 'strict' flag, like:
;;	-matomic-model=soft-gusa,strict
;;
;; The software atomic models are generally compatible with each other,
;; but the interrupt / exception handling code has to support both gUSA and
;; TCB models.
;;
;; The current atomic support is limited to QImode, HImode and SImode 
;; atomic operations.  DImode operations could also be implemented but
;; would require some ABI modifications to support multiple-instruction
;; write-back.  This is because SH1/SH2/SH3/SH4 does not have a DImode
;; store instruction.  DImode stores must be split into two SImode stores.

(define_c_enum "unspec" [
  UNSPEC_ATOMIC
])
 
(define_c_enum "unspecv" [
  UNSPECV_CMPXCHG_1
  UNSPECV_CMPXCHG_2
  UNSPECV_CMPXCHG_3
])

(define_mode_attr i124extend_insn [(QI "exts.b") (HI "exts.w") (SI "mov")])

(define_code_iterator FETCHOP [plus minus ior xor and])
(define_code_attr fetchop_name
  [(plus "add") (minus "sub") (ior "or") (xor "xor") (and "and")])

;;------------------------------------------------------------------------------
;; comapre and swap

;; Only the hard_llcs SImode patterns can use an I08 for the comparison
;; or for the new swapped in value.
(define_predicate "atomic_arith_operand_0"
  (and (match_code "subreg,reg,const_int")
       (ior (match_operand 0 "arith_reg_operand")
	    (and (match_test "satisfies_constraint_I08 (op)")
		 (match_test "mode == SImode")
		 (ior (match_test "TARGET_ATOMIC_HARD_LLCS")
		      (match_test "TARGET_ATOMIC_ANY && TARGET_SH4A
				   && !TARGET_ATOMIC_STRICT"))))))

;; Displacement addressing can be used for all SImode atomic patterns, except
;; llcs.
(define_predicate "atomic_mem_operand_0"
  (and (match_code "mem")
       (ior (match_operand 0 "simple_mem_operand")
	    (and (match_test "mode == SImode")
		 (and (match_test "!TARGET_ATOMIC_HARD_LLCS")
		      (match_test "!TARGET_SH4A || TARGET_ATOMIC_STRICT"))
		 (match_operand 0 "short_displacement_mem_operand")))))

(define_expand "atomic_compare_and_swap<mode>"
  [(match_operand:SI 0 "arith_reg_dest")		;; bool success output
   (match_operand:QIHISI 1 "arith_reg_dest")		;; oldval output
   (match_operand:QIHISI 2 "atomic_mem_operand_0")	;; memory
   (match_operand:QIHISI 3 "atomic_arith_operand_0")	;; expected input
   (match_operand:QIHISI 4 "atomic_arith_operand_0")	;; newval input
   (match_operand:SI 5 "const_int_operand")		;; is_weak
   (match_operand:SI 6 "const_int_operand")		;; success model
   (match_operand:SI 7 "const_int_operand")]		;; failure model
  "TARGET_ATOMIC_ANY"
{
  rtx mem = operands[2];
  rtx old_val = gen_lowpart (SImode, operands[1]);
  rtx exp_val = operands[3];
  rtx new_val = operands[4];
  rtx atomic_insn;

  if (TARGET_ATOMIC_HARD_LLCS
      || (TARGET_SH4A && <MODE>mode == SImode && !TARGET_ATOMIC_STRICT))
    atomic_insn = gen_atomic_compare_and_swap<mode>_hard (old_val, mem,
							  exp_val, new_val);
  else if (TARGET_ATOMIC_SOFT_GUSA)
    atomic_insn = gen_atomic_compare_and_swap<mode>_soft_gusa (old_val, mem,
		      exp_val, new_val);
  else if (TARGET_ATOMIC_SOFT_TCB)
    atomic_insn = gen_atomic_compare_and_swap<mode>_soft_tcb (old_val, mem,
		      exp_val, new_val, TARGET_ATOMIC_SOFT_TCB_GBR_OFFSET_RTX);
  else if (TARGET_ATOMIC_SOFT_IMASK)
    atomic_insn = gen_atomic_compare_and_swap<mode>_soft_imask (old_val, mem,
		      exp_val, new_val);
  else
    FAIL;

  emit_insn (atomic_insn);

  if (<MODE>mode == QImode)
    emit_insn (gen_zero_extendqisi2 (gen_lowpart (SImode, operands[1]),
				     operands[1]));
  else if (<MODE>mode == HImode)
    emit_insn (gen_zero_extendhisi2 (gen_lowpart (SImode, operands[1]),
				     operands[1]));
  emit_insn (gen_movsi (operands[0], gen_rtx_REG (SImode, T_REG)));
  DONE;
})

(define_insn_and_split "atomic_compare_and_swapsi_hard"
  [(set (match_operand:SI 0 "arith_reg_dest" "=&r")
	(unspec_volatile:SI
	  [(match_operand:SI 1 "atomic_mem_operand_0" "=Sra")
	   (match_operand:SI 2 "arith_operand" "rI08")
	   (match_operand:SI 3 "arith_operand" "rI08")]
	  UNSPECV_CMPXCHG_1))
   (set (match_dup 1)
	(unspec_volatile:SI [(const_int 0)] UNSPECV_CMPXCHG_2))
   (set (reg:SI T_REG)
	(unspec_volatile:SI [(const_int 0)] UNSPECV_CMPXCHG_3))
   (clobber (reg:SI R0_REG))]
  "TARGET_ATOMIC_HARD_LLCS
   || (TARGET_SH4A && TARGET_ATOMIC_ANY && !TARGET_ATOMIC_STRICT)"
{
  return "\r0:	movli.l	%1,r0"		"\n"
	 "	cmp/eq	%2,r0"		"\n"
	 "	bf{.|/}s	0f"	"\n"
	 "	mov	r0,%0"		"\n"
	 "	mov	%3,r0"		"\n"
	 "	movco.l	r0,%1"		"\n"
	 "	bf	0b"		"\n"
	 "0:";
}
  "&& can_create_pseudo_p () && !satisfies_constraint_I08 (operands[2])"
  [(const_int 0)]
{
  /* FIXME: Sometimes the 'expected value' operand is not propagated as
     immediate value.  See PR 64974.  */
  set_of_reg op2 = sh_find_set_of_reg (operands[2], curr_insn,
				       prev_nonnote_nondebug_insn_bb);
  if (op2.set_src != NULL && satisfies_constraint_I08 (op2.set_src))
    {
      rtx* r = &XVECEXP (XEXP (XVECEXP (PATTERN (curr_insn), 0, 0), 1), 0, 1);
      validate_change (curr_insn, r, op2.set_src, false);
      DONE;
    }
  else
    FAIL;
}
  [(set_attr "length" "14")])

;; The QIHImode llcs patterns modify the address register of the memory
;; operand.  In order to express that, we have to open code the memory
;; operand.  Initially the insn is expanded like every other atomic insn
;; using the memory operand.  In split1 the insn is converted and the
;; memory operand's address register is exposed.
(define_insn_and_split "atomic_compare_and_swap<mode>_hard"
  [(set (match_operand:SI 0 "arith_reg_dest")
	(unspec_volatile:SI
	  [(match_operand:QIHI 1 "atomic_mem_operand_0")
	   (match_operand:QIHI 2 "arith_reg_operand")
	   (match_operand:QIHI 3 "arith_reg_operand")]
	  UNSPECV_CMPXCHG_1))
   (set (match_dup 1)
	(unspec_volatile:QIHI [(const_int 0)] UNSPECV_CMPXCHG_2))
   (set (reg:SI T_REG)
	(unspec_volatile:SI [(const_int 0)] UNSPECV_CMPXCHG_3))
   (clobber (reg:SI R0_REG))]
  "TARGET_ATOMIC_HARD_LLCS && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  rtx i = gen_atomic_compare_and_swap<mode>_hard_1 (
		operands[0], XEXP (operands[1], 0), operands[2], operands[3]);

  /* Replace the new mems in the new insn with the old mem to preserve
     aliasing info.  */
  XVECEXP (XEXP (XVECEXP (i, 0, 0), 1), 0, 0) = operands[1];
  XEXP (XVECEXP (i, 0, 1), 0) = operands[1];
  emit_insn (i);
})

(define_insn "atomic_compare_and_swap<mode>_hard_1"
  [(set (match_operand:SI 0 "arith_reg_dest" "=&r")
	(unspec_volatile:SI
	  [(mem:QIHI (match_operand:SI 1 "arith_reg_operand" "r"))
	   (match_operand:QIHI 2 "arith_reg_operand" "r")
	   (match_operand:QIHI 3 "arith_reg_operand" "r")]
	  UNSPECV_CMPXCHG_1))
   (set (mem:QIHI (match_dup 1))
	(unspec_volatile:QIHI [(const_int 0)] UNSPECV_CMPXCHG_2))
   (set (reg:SI T_REG)
	(unspec_volatile:SI [(const_int 0)] UNSPECV_CMPXCHG_3))
   (clobber (reg:SI R0_REG))
   (clobber (match_scratch:SI 4 "=&r"))
   (clobber (match_scratch:SI 5 "=&r"))
   (clobber (match_scratch:SI 6 "=1"))]
  "TARGET_ATOMIC_HARD_LLCS"
{
  return "\r	mov	#-4,%5"			"\n"
	 "	<i124extend_insn>	%2,%4"	"\n"
	 "	and	%1,%5"			"\n"
	 "	xor	%5,%1"			"\n"
	 "	add	r15,%1"			"\n"
	 "	add	#-4,%1"			"\n"
	 "0:	movli.l	@%5,r0"			"\n"
	 "	mov.l	r0,@-r15"		"\n"
	 "	mov.<bw>	@%1,%0"		"\n"
	 "	mov.<bw>	%3,@%1"		"\n"
	 "	cmp/eq	%4,%0"			"\n"
	 "	bf{.|/}s	0f"		"\n"
	 "	mov.l	@r15+,r0"		"\n"
	 "	movco.l	r0,@%5"			"\n"
	 "	bf	0b"			"\n"
	 "0:";
}
  [(set_attr "length" "30")])

(define_insn "atomic_compare_and_swap<mode>_soft_gusa"
  [(set (match_operand:SI 0 "arith_reg_dest" "=&u")
	(unspec_volatile:SI
	  [(match_operand:QIHISI 1 "atomic_mem_operand_0" "=AraAdd")
	   (match_operand:QIHISI 2 "arith_reg_operand" "u")
	   (match_operand:QIHISI 3 "arith_reg_operand" "u")]
	  UNSPECV_CMPXCHG_1))
   (set (match_dup 1)
	(unspec_volatile:QIHISI [(const_int 0)] UNSPECV_CMPXCHG_2))
   (set (reg:SI T_REG)
	(unspec_volatile:SI [(const_int 0)] UNSPECV_CMPXCHG_3))
   (clobber (match_scratch:SI 4 "=&u"))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))]
  "TARGET_ATOMIC_SOFT_GUSA"
{
  return "\r	mova	1f,r0"			"\n"
	 "	<i124extend_insn>	%2,%4"	"\n"
	 "	.align 2"			"\n"
	 "	mov	r15,r1"			"\n"
	 "	mov	#(0f-1f),r15"		"\n"
	 "0:	mov.<bwl>	%1,%0"		"\n"
	 "	cmp/eq	%0,%4"			"\n"
	 "	bf	1f"			"\n"
	 "	mov.<bwl>	%3,%1"		"\n"
	 "1:	mov	r1,r15";
}
  [(set_attr "length" "20")])

(define_insn "atomic_compare_and_swap<mode>_soft_tcb"
  [(set (match_operand:SI 0 "arith_reg_dest" "=&r")
	(unspec_volatile:SI
	  [(match_operand:QIHISI 1 "atomic_mem_operand_0" "=SraSdd")
	   (match_operand:QIHISI 2 "arith_reg_operand" "r")
	   (match_operand:QIHISI 3 "arith_reg_operand" "r")]
	  UNSPECV_CMPXCHG_1))
   (set (match_dup 1)
	(unspec_volatile:QIHISI [(const_int 0)] UNSPECV_CMPXCHG_2))
   (set (reg:SI T_REG)
	(unspec_volatile:SI [(const_int 0)] UNSPECV_CMPXCHG_3))
   (use (match_operand:SI 4 "gbr_displacement"))
   (clobber (match_scratch:SI 5 "=&r"))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))]
  "TARGET_ATOMIC_SOFT_TCB"
{
  return "\r	mova	1f,r0"			"\n"
	 "	.align 2"			"\n"
	 "	<i124extend_insn>	%2,%5"	"\n"
	 "	mov	#(0f-1f),r1"		"\n"
	 "	mov.l	r0,@(%O4,gbr)"		"\n"
	 "0:	mov.<bwl>	%1,%0"		"\n"
	 "	mov	#0,r0"			"\n"
	 "	cmp/eq	%0,%5"			"\n"
	 "	bf	1f"			"\n"
	 "	mov.<bwl>	%3,%1"		"\n"
	 "1:	mov.l	r0,@(%O4,gbr)";
}
  [(set_attr "length" "22")])

(define_insn "atomic_compare_and_swap<mode>_soft_imask"
  [(set (match_operand:SI 0 "arith_reg_dest" "=&z")
	(unspec_volatile:SI
	  [(match_operand:QIHISI 1 "atomic_mem_operand_0" "=SraSdd")
	   (match_operand:QIHISI 2 "arith_reg_operand" "r")
	   (match_operand:QIHISI 3 "arith_reg_operand" "r")]
	  UNSPECV_CMPXCHG_1))
   (set (match_dup 1)
	(unspec_volatile:QIHISI [(const_int 0)] UNSPECV_CMPXCHG_2))
   (set (reg:SI T_REG)
	(unspec_volatile:SI [(const_int 0)] UNSPECV_CMPXCHG_3))
   (clobber (match_scratch:SI 4 "=&r"))
   (clobber (match_scratch:SI 5 "=&r"))]
  "TARGET_ATOMIC_SOFT_IMASK"
{
  /* The comparison result is supposed to be in T_REG.
     Notice that restoring SR will overwrite the T_REG.  We handle this by
     rotating the T_REG into the saved SR before restoring SR.  On SH2A we
     can do one insn shorter by using the bst insn.  */
  if (!TARGET_SH2A)
    return "\r	stc	sr,%0"			"\n"
	   "	<i124extend_insn>	%2,%4"	"\n"
	   "	mov	%0,%5"			"\n"
	   "	or	#0xF0,%0"		"\n"
	   "	shlr	%5"			"\n"
	   "	ldc	%0,sr"			"\n"
	   "	mov.<bwl>	%1,%0"		"\n"
	   "	cmp/eq	%4,%0"			"\n"
	   "	bf	1f"			"\n"
	   "	mov.<bwl>	%3,%1"		"\n"
	   "1:	rotcl	%5"			"\n"
	   "	ldc	%5,sr";
  else
    return "\r	stc	sr,%0"			"\n"
	   "	<i124extend_insn>	%2,%4"	"\n"
	   "	mov	%0,%5"			"\n"
	   "	or	#0xF0,%0"		"\n"
	   "	ldc	%0,sr"			"\n"
	   "	mov.<bwl>	%1,%0"		"\n"
	   "	cmp/eq	%4,%0"			"\n"
	   "	bst	#0,%5"			"\n"
	   "	bf	1f"			"\n"
	   "	mov.<bwl>	%3,%1"		"\n"
	   "1:	ldc	%5,sr";
}
  [(set (attr "length") (if_then_else (match_test "!TARGET_SH2A")
				      (const_string "24")
				      (const_string "22")))])

;;------------------------------------------------------------------------------
;; read - write - return old value

(define_expand "atomic_exchange<mode>"
  [(match_operand:QIHISI 0 "arith_reg_dest")		;; oldval output
   (match_operand:QIHISI 1 "atomic_mem_operand_0")	;; memory
   (match_operand:QIHISI 2 "atomic_arith_operand_0")	;; newval input
   (match_operand:SI 3 "const_int_operand")]		;; memory model
  "TARGET_ATOMIC_ANY"
{
  rtx mem = operands[1];
  rtx val = operands[2];
  rtx atomic_insn;

  if (TARGET_ATOMIC_HARD_LLCS
      || (TARGET_SH4A && <MODE>mode == SImode && !TARGET_ATOMIC_STRICT))
    atomic_insn = gen_atomic_exchange<mode>_hard (operands[0], mem, val);
  else if (TARGET_ATOMIC_SOFT_GUSA)
    atomic_insn = gen_atomic_exchange<mode>_soft_gusa (operands[0], mem, val);
  else if (TARGET_ATOMIC_SOFT_TCB)
    atomic_insn = gen_atomic_exchange<mode>_soft_tcb (operands[0], mem, val,
		      TARGET_ATOMIC_SOFT_TCB_GBR_OFFSET_RTX);
  else if (TARGET_ATOMIC_SOFT_IMASK)
    atomic_insn = gen_atomic_exchange<mode>_soft_imask (operands[0], mem, val);
  else
    FAIL;

  emit_insn (atomic_insn);

  if (<MODE>mode == QImode)
    emit_insn (gen_zero_extendqisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  else if (<MODE>mode == HImode)
    emit_insn (gen_zero_extendhisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  DONE;
})

(define_insn "atomic_exchangesi_hard"
  [(set (match_operand:SI 0 "arith_reg_dest" "=&r")
	(match_operand:SI 1 "atomic_mem_operand_0" "=Sra"))
   (set (match_dup 1)
	(unspec:SI
	  [(match_operand:SI 2 "arith_operand" "rI08")] UNSPEC_ATOMIC))
   (set (reg:SI T_REG) (const_int 1))
   (clobber (reg:SI R0_REG))]
  "TARGET_ATOMIC_HARD_LLCS
   || (TARGET_SH4A && TARGET_ATOMIC_ANY && !TARGET_ATOMIC_STRICT)"
{
  return "\r0:	movli.l	%1,r0"		"\n"
	 "	mov	r0,%0"		"\n"
	 "	mov	%2,r0"		"\n"
	 "	movco.l r0,%1"		"\n"
	 "	bf	0b";
}
  [(set_attr "length" "10")])

;; The QIHImode llcs patterns modify the address register of the memory
;; operand.  In order to express that, we have to open code the memory
;; operand.  Initially the insn is expanded like every other atomic insn
;; using the memory operand.  In split1 the insn is converted and the
;; memory operand's address register is exposed.
(define_insn_and_split "atomic_exchange<mode>_hard"
  [(set (match_operand:QIHI 0 "arith_reg_dest")
	(match_operand:QIHI 1 "atomic_mem_operand_0"))
   (set (match_dup 1)
	(unspec:QIHI
	  [(match_operand:QIHI 2 "arith_reg_operand")] UNSPEC_ATOMIC))
   (set (reg:SI T_REG) (const_int 1))
   (clobber (reg:SI R0_REG))]
  "TARGET_ATOMIC_HARD_LLCS && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  rtx i = gen_atomic_exchange<mode>_hard_1 (operands[0], XEXP (operands[1], 0),
					    operands[2]);

  /* Replace the new mems in the new insn with the old mem to preserve
     aliasing info.  */
  XEXP (XVECEXP (i, 0, 0), 1) = operands[1];
  XEXP (XVECEXP (i, 0, 1), 0) = operands[1];
  emit_insn (i);
})

(define_insn "atomic_exchange<mode>_hard_1"
  [(set (match_operand:QIHI 0 "arith_reg_dest" "=&r")
	(mem:QIHI (match_operand:SI 1 "arith_reg_operand" "r")))
   (set (mem:QIHI (match_dup 1))
	(unspec:QIHI
	  [(match_operand:QIHI 2 "arith_reg_operand" "r")] UNSPEC_ATOMIC))
   (set (reg:SI T_REG) (const_int 1))
   (clobber (reg:SI R0_REG))
   (clobber (match_scratch:SI 3 "=&r"))
   (clobber (match_scratch:SI 4 "=1"))]
  "TARGET_ATOMIC_HARD_LLCS"
{
  return "\r	mov	#-4,%3"			"\n"
	 "	and	%1,%3"			"\n"
	 "	xor	%3,%1"			"\n"
	 "	add	r15,%1"			"\n"
	 "	add	#-4,%1"			"\n"
	 "0:	movli.l	@%3,r0"			"\n"
	 "	mov.l	r0,@-r15"		"\n"
	 "	mov.<bw>	@%1,%0"		"\n"
	 "	mov.<bw>	%2,@%1" 	"\n"
	 "	mov.l	@r15+,r0"		"\n"
	 "	movco.l	r0,@%3"			"\n"
	 "	bf	0b";
}
  [(set_attr "length" "24")])

(define_insn "atomic_exchange<mode>_soft_gusa"
  [(set (match_operand:QIHISI 0 "arith_reg_dest" "=&u")
	(match_operand:QIHISI 1 "atomic_mem_operand_0" "=AraAdd"))
   (set (match_dup 1)
	(unspec:QIHISI
	  [(match_operand:QIHISI 2 "arith_reg_operand" "u")] UNSPEC_ATOMIC))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))]
  "TARGET_ATOMIC_SOFT_GUSA"
{
  return "\r	mova	1f,r0"			"\n"
	 "	.align 2"			"\n"
	 "	mov	r15,r1"			"\n"
	 "	mov	#(0f-1f),r15"		"\n"
	 "0:	mov.<bwl>	%1,%0"		"\n"
	 "	mov.<bwl>	%2,%1"		"\n"
	 "1:	mov	r1,r15";
}
  [(set_attr "length" "14")])

(define_insn "atomic_exchange<mode>_soft_tcb"
  [(set (match_operand:QIHISI 0 "arith_reg_dest" "=&r")
	(match_operand:QIHISI 1 "atomic_mem_operand_0" "=SraSdd"))
   (set (match_dup 1)
	(unspec:QIHISI
	  [(match_operand:QIHISI 2 "arith_reg_operand" "r")] UNSPEC_ATOMIC))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))
   (use (match_operand:SI 3 "gbr_displacement"))]
  "TARGET_ATOMIC_SOFT_TCB"
{
  return "\r	mova	1f,r0"			"\n"
	 "	mov	#(0f-1f),r1"		"\n"
	 "	.align 2"			"\n"
	 "	mov.l	r0,@(%O3,gbr)"		"\n"
	 "0:	mov.<bwl>	%1,%0"		"\n"
	 "	mov	#0,r0"			"\n"
	 "	mov.<bwl>	%2,%1"		"\n"
	 "1:	mov.l	r0,@(%O3,gbr)";
}
  [(set_attr "length" "16")])

(define_insn "atomic_exchange<mode>_soft_imask"
  [(set (match_operand:QIHISI 0 "arith_reg_dest" "=&z")
	(match_operand:QIHISI 1 "atomic_mem_operand_0" "=SraSdd"))
   (set (match_dup 1)
	(unspec:QIHISI
	  [(match_operand:QIHISI 2 "arith_reg_operand" "r")] UNSPEC_ATOMIC))
   (clobber (match_scratch:SI 3 "=&r"))]
  "TARGET_ATOMIC_SOFT_IMASK"
{
  return "\r	stc	sr,%0"			"\n"
	 "	mov	%0,%3"			"\n"
	 "	or	#0xF0,%0"		"\n"
	 "	ldc	%0,sr"			"\n"
	 "	mov.<bwl>	%1,%0"		"\n"
	 "	mov.<bwl>	%2,%1"		"\n"
	 "	ldc	%3,sr";
}
  [(set_attr "length" "14")])

;;------------------------------------------------------------------------------
;; read - add|sub|or|and|xor|nand - write - return old value

;; atomic_arith_operand_1 can be used by any atomic type for a plus op,
;; since there's no r0 restriction.
(define_predicate "atomic_arith_operand_1"
  (and (match_code "subreg,reg,const_int")
       (ior (match_operand 0 "arith_reg_operand")
	    (match_test "satisfies_constraint_I08 (op)"))))

;; atomic_logic_operand_1 can be used by the hard_llcs, tcb and soft_imask
;; patterns only due to its r0 restriction. 
(define_predicate "atomic_logical_operand_1"
  (and (match_code "subreg,reg,const_int")
       (ior (match_operand 0 "arith_reg_operand")
	    (and (match_test "satisfies_constraint_K08 (op)")
		 (ior (match_test "TARGET_ATOMIC_HARD_LLCS")
		      (match_test "TARGET_ATOMIC_SOFT_IMASK")
		      (match_test "TARGET_ATOMIC_SOFT_TCB")
		      (match_test "TARGET_ATOMIC_ANY && TARGET_SH4A
				   && mode == SImode
				   && !TARGET_ATOMIC_STRICT"))))))

(define_code_attr fetchop_predicate_1
  [(plus "atomic_arith_operand_1") (minus "arith_reg_operand")
   (ior "atomic_logical_operand_1") (xor "atomic_logical_operand_1")
   (and "atomic_logical_operand_1")])

(define_code_attr fetchop_constraint_1_llcs
  [(plus "rI08") (minus "r") (ior "rK08") (xor "rK08") (and "rK08")])

(define_code_attr fetchop_constraint_1_gusa
  [(plus "uI08") (minus "u") (ior "u") (xor "u") (and "u")])

(define_code_attr fetchop_constraint_1_tcb
  [(plus "rI08") (minus "r") (ior "rK08") (xor "rK08") (and "rK08")])

(define_code_attr fetchop_constraint_1_imask
  [(plus "rI08") (minus "r") (ior "rK08") (xor "rK08") (and "rK08")])

;; Displacement addressing mode (incl. GBR relative) can be used by tcb and
;; imask atomic patterns in any mode, since all the patterns use R0 as the
;; register operand for memory loads/stores.  gusa and llcs patterns can only
;; use displacement addressing for SImode.
(define_predicate "atomic_mem_operand_1"
  (and (match_code "mem")
       (ior (match_operand 0 "simple_mem_operand")
	    (and (match_test "mode == SImode")
		 (match_test "TARGET_ATOMIC_SOFT_GUSA
			      && (!TARGET_SH4A || TARGET_ATOMIC_STRICT)")
		 (match_operand 0 "short_displacement_mem_operand"))
	    (and (ior (match_test "(TARGET_ATOMIC_SOFT_TCB
				    || TARGET_ATOMIC_SOFT_IMASK)
				   && (!TARGET_SH4A || TARGET_ATOMIC_STRICT)")
		      (match_test "(TARGET_ATOMIC_SOFT_TCB
				    || TARGET_ATOMIC_SOFT_IMASK)
				   && TARGET_SH4A && !TARGET_ATOMIC_STRICT
				   && mode != SImode"))
		 (ior (match_operand 0 "short_displacement_mem_operand")
		      (match_operand 0 "gbr_address_mem"))))))

(define_expand "atomic_fetch_<fetchop_name><mode>"
  [(set (match_operand:QIHISI 0 "arith_reg_dest")
	(match_operand:QIHISI 1 "atomic_mem_operand_1"))
   (set (match_dup 1)
	(unspec:QIHISI
	  [(FETCHOP:QIHISI (match_dup 1)
	     (match_operand:QIHISI 2 "<fetchop_predicate_1>"))]
	  UNSPEC_ATOMIC))
   (match_operand:SI 3 "const_int_operand")]
  "TARGET_ATOMIC_ANY"
{
  rtx mem = operands[1];
  rtx atomic_insn;

  if (TARGET_ATOMIC_HARD_LLCS
      || (TARGET_SH4A && <MODE>mode == SImode && !TARGET_ATOMIC_STRICT))
    atomic_insn = gen_atomic_fetch_<fetchop_name><mode>_hard (operands[0], mem,
							      operands[2]);
  else if (TARGET_ATOMIC_SOFT_GUSA)
    atomic_insn = gen_atomic_fetch_<fetchop_name><mode>_soft_gusa (operands[0],
		      mem, operands[2]);
  else if (TARGET_ATOMIC_SOFT_TCB)
    atomic_insn = gen_atomic_fetch_<fetchop_name><mode>_soft_tcb (operands[0],
		      mem, operands[2], TARGET_ATOMIC_SOFT_TCB_GBR_OFFSET_RTX);
  else if (TARGET_ATOMIC_SOFT_IMASK)
    atomic_insn = gen_atomic_fetch_<fetchop_name><mode>_soft_imask (operands[0],
		      mem, operands[2]);
  else
    FAIL;

  emit_insn (atomic_insn);

  if (<MODE>mode == QImode)
    emit_insn (gen_zero_extendqisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  else if (<MODE>mode == HImode)
    emit_insn (gen_zero_extendhisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  DONE;
})

(define_insn_and_split "atomic_fetch_<fetchop_name>si_hard"
  [(set (match_operand:SI 0 "arith_reg_dest" "=&r")
	(match_operand:SI 1 "atomic_mem_operand_1" "=Sra"))
   (set (match_dup 1)
	(unspec:SI
	  [(FETCHOP:SI (match_dup 1)
		       (match_operand:SI 2 "<fetchop_predicate_1>"
					   "<fetchop_constraint_1_llcs>"))]
	  UNSPEC_ATOMIC))
   (set (reg:SI T_REG) (const_int 1))
   (clobber (reg:SI R0_REG))]
  "TARGET_ATOMIC_HARD_LLCS
   || (TARGET_SH4A && TARGET_ATOMIC_ANY && !TARGET_ATOMIC_STRICT)"
{
  return "\r0:	movli.l	%1,r0"		"\n"
	 "	mov	r0,%0"		"\n"
	 "	<fetchop_name>	%2,r0"	"\n"
	 "	movco.l	r0,%1"		"\n"
	 "	bf	0b";
}
  "&& can_create_pseudo_p () && optimize
   && sh_reg_dead_or_unused_after_insn (insn, REGNO (operands[0]))"
  [(const_int 0)]
{
  emit_insn (gen_atomic_<fetchop_name>_fetchsi_hard (gen_reg_rtx (SImode),
						     operands[1], operands[2]));
}
  [(set_attr "length" "10")])

;; Combine pattern for xor (val, -1) / nand (val, -1).
(define_insn_and_split "atomic_fetch_notsi_hard"
  [(set (match_operand:SI 0 "arith_reg_dest" "=&r")
	(match_operand:SI 1 "atomic_mem_operand_1" "=Sra"))
   (set (match_dup 1)
	(unspec:SI [(not:SI (match_dup 1))] UNSPEC_ATOMIC))
   (set (reg:SI T_REG) (const_int 1))
   (clobber (reg:SI R0_REG))]
  "TARGET_ATOMIC_HARD_LLCS
   || (TARGET_SH4A && TARGET_ATOMIC_ANY && !TARGET_ATOMIC_STRICT)"
{
  return "\r0:	movli.l	%1,r0"		"\n"
	 "	mov	r0,%0"		"\n"
	 "	not	r0,r0"		"\n"
	 "	movco.l	r0,%1"		"\n"
	 "	bf	0b";
}
  "&& can_create_pseudo_p () && optimize
   && sh_reg_dead_or_unused_after_insn (insn, REGNO (operands[0]))"
  [(const_int 0)]
{
  emit_insn (gen_atomic_not_fetchsi_hard (gen_reg_rtx (SImode), operands[1]));
}
  [(set_attr "length" "10")])

;; The QIHImode llcs patterns modify the address register of the memory
;; operand.  In order to express that, we have to open code the memory
;; operand.  Initially the insn is expanded like every other atomic insn
;; using the memory operand.  In split1 the insn is converted and the
;; memory operand's address register is exposed.
(define_insn_and_split "atomic_fetch_<fetchop_name><mode>_hard"
  [(set (match_operand:QIHI 0 "arith_reg_dest")
	(match_operand:QIHI 1 "atomic_mem_operand_1"))
   (set (match_dup 1)
	(unspec:QIHI
	  [(FETCHOP:QIHI (match_dup 1)
			 (match_operand:QIHI 2 "<fetchop_predicate_1>"))]
	  UNSPEC_ATOMIC))
   (set (reg:SI T_REG) (const_int 1))
   (clobber (reg:SI R0_REG))]
  "TARGET_ATOMIC_HARD_LLCS && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  if (optimize
      && sh_reg_dead_or_unused_after_insn (curr_insn, REGNO (operands[0])))
    emit_insn (gen_atomic_<fetchop_name><mode>_hard (operands[1], operands[2]));
  else
    {
      rtx i = gen_atomic_fetch_<fetchop_name><mode>_hard_1 (
			operands[0], XEXP (operands[1], 0), operands[2]);

      /* Replace the new mems in the new insn with the old mem to preserve
	 aliasing info.  */
      XEXP (XVECEXP (i, 0, 0), 1) = operands[1];
      XEXP (XVECEXP (i, 0, 1), 0) = operands[1];
      XEXP (XVECEXP (XEXP (XVECEXP (i, 0, 1), 1), 0, 0), 0) = operands[1];
      emit_insn (i);
    }
})

(define_insn "atomic_fetch_<fetchop_name><mode>_hard_1"
  [(set (match_operand:QIHI 0 "arith_reg_dest" "=&r")
	(mem:QIHI (match_operand:SI 1 "arith_reg_operand" "r")))
   (set (mem:QIHI (match_dup 1))
	(unspec:QIHI
	  [(FETCHOP:QIHI (mem:QIHI (match_dup 1))
			 (match_operand:QIHI 2 "<fetchop_predicate_1>"
					       "<fetchop_constraint_1_llcs>"))]
	  UNSPEC_ATOMIC))
   (set (reg:SI T_REG) (const_int 1))
   (clobber (reg:SI R0_REG))
   (clobber (match_scratch:SI 3 "=&r"))
   (clobber (match_scratch:SI 4 "=1"))]
  "TARGET_ATOMIC_HARD_LLCS"
{
  return "\r	mov	#-4,%3"			"\n"
	 "	and	%1,%3"			"\n"
	 "	xor	%3,%1"			"\n"
	 "	add	r15,%1"			"\n"
	 "	add	#-4,%1"			"\n"
	 "0:	movli.l	@%3,r0"			"\n"
	 "	mov.l	r0,@-r15"		"\n"
	 "	mov.<bw>	@%1,r0"		"\n"
	 "	mov	r0,%0"			"\n"
	 "	<fetchop_name>	%2,r0"		"\n"
	 "	mov.<bw>	r0,@%1"		"\n"
	 "	mov.l	@r15+,r0"		"\n"
	 "	movco.l	r0,@%3"			"\n"
	 "	bf	0b";
}
  [(set_attr "length" "28")])

;; The QIHImode llcs patterns modify the address register of the memory
;; operand.  In order to express that, we have to open code the memory
;; operand.  Initially the insn is expanded like every other atomic insn
;; using the memory operand.  In split1 the insn is converted and the
;; memory operand's address register is exposed.
(define_insn_and_split "atomic_<fetchop_name><mode>_hard"
  [(set (match_operand:QIHI 0 "atomic_mem_operand_1")
	(unspec:QIHI
	  [(FETCHOP:QIHI (match_dup 0)
			 (match_operand:QIHI 1 "<fetchop_predicate_1>"))]
	  UNSPEC_ATOMIC))
   (set (reg:SI T_REG) (const_int 1))
   (clobber (reg:SI R0_REG))]
  "TARGET_ATOMIC_HARD_LLCS && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  rtx i = gen_atomic_<fetchop_name><mode>_hard_1 (XEXP (operands[0], 0),
						  operands[1]);
  /* Replace the new mems in the new insn with the old mem to preserve
     aliasing info.  */
  XEXP (XVECEXP (i, 0, 0), 0) = operands[0];
  XEXP (XVECEXP (XEXP (XVECEXP (i, 0, 0), 1), 0, 0), 0) = operands[0];
  emit_insn (i);
})

(define_insn "atomic_<fetchop_name><mode>_hard_1"
  [(set (mem:QIHI (match_operand:SI 0 "arith_reg_operand" "r"))
	(unspec:QIHI
	  [(FETCHOP:QIHI (mem:QIHI (match_dup 0))
			 (match_operand:QIHI 1 "<fetchop_predicate_1>"
					       "<fetchop_constraint_1_llcs>"))]
	  UNSPEC_ATOMIC))
   (set (reg:SI T_REG) (const_int 1))
   (clobber (reg:SI R0_REG))
   (clobber (match_scratch:SI 2 "=&r"))
   (clobber (match_scratch:SI 3 "=0"))]
  "TARGET_ATOMIC_HARD_LLCS"
{
  return "\r	mov	#-4,%2"			"\n"
	 "	and	%0,%2"			"\n"
	 "	xor	%2,%0"			"\n"
	 "	add	r15,%0"			"\n"
	 "	add	#-4,%0"			"\n"
	 "0:	movli.l	@%2,r0"			"\n"
	 "	mov.l	r0,@-r15"		"\n"
	 "	mov.<bw>	@%0,r0"		"\n"
	 "	<fetchop_name>	%1,r0"		"\n"
	 "	mov.<bw>	r0,@%0"		"\n"
	 "	mov.l	@r15+,r0"		"\n"
	 "	movco.l	r0,@%2"			"\n"
	 "	bf	0b";
}
  [(set_attr "length" "26")])

;; Combine pattern for xor (val, -1) / nand (val, -1).
(define_insn_and_split "atomic_fetch_not<mode>_hard"
  [(set (match_operand:QIHI 0 "arith_reg_dest" "=&r")
	(mem:QIHI (match_operand:SI 1 "arith_reg_operand" "r")))
   (set (mem:QIHI (match_dup 1))
	(unspec:QIHI [(not:QIHI (mem:QIHI (match_dup 1)))] UNSPEC_ATOMIC))
   (set (reg:SI T_REG) (const_int 1))
   (clobber (reg:SI R0_REG))
   (clobber (match_scratch:SI 2 "=&r"))
   (clobber (match_scratch:SI 3 "=1"))]
  "TARGET_ATOMIC_HARD_LLCS"
{
  return "\r	mov	#-4,%2"			"\n"
	 "	and	%1,%2"			"\n"
	 "	xor	%2,%1"			"\n"
	 "	add	r15,%1"			"\n"
	 "	add	#-4,%1"			"\n"
	 "0:	movli.l	@%2,r0"			"\n"
	 "	mov.l	r0,@-r15"		"\n"
	 "	mov.<bw>	@%1,%0"		"\n"
	 "	not	%0,r0"			"\n"
	 "	mov.<bw>	r0,@%1"		"\n"
	 "	mov.l	@r15+,r0"		"\n"
	 "	movco.l	r0,@%2"			"\n"
	 "	bf	0b";
}
  "&& can_create_pseudo_p () && optimize
   && sh_reg_dead_or_unused_after_insn (insn, REGNO (operands[0]))"
  [(const_int 0)]
{
  rtx i = gen_atomic_not<mode>_hard (operands[1]);

  /* Replace the new mems in the new insn with the old mem to preserve
     aliasing info.  */
  rtx m = XEXP (XVECEXP (PATTERN (curr_insn), 0, 0), 1);
  XEXP (XVECEXP (i, 0, 0), 0) = m;
  XEXP (XVECEXP (XEXP (XVECEXP (i, 0, 0), 1), 0, 0), 0) = m;
  emit_insn (i);
}
  [(set_attr "length" "26")])

(define_insn "atomic_not<mode>_hard"
  [(set (mem:QIHI (match_operand:SI 0 "arith_reg_operand" "r"))
	(unspec:QIHI [(not:QIHI (mem:QIHI (match_dup 0)))] UNSPEC_ATOMIC))
   (set (reg:SI T_REG) (const_int 1))
   (clobber (reg:SI R0_REG))
   (clobber (match_scratch:SI 1 "=&r"))
   (clobber (match_scratch:SI 2 "=0"))]
  "TARGET_ATOMIC_HARD_LLCS"
{
  return "\r	mov	#-4,%1"			"\n"
	 "	and	%0,%1"			"\n"
	 "	xor	%1,%0"			"\n"
	 "	add	r15,%0"			"\n"
	 "	add	#-4,%0"			"\n"
	 "0:	movli.l	@%1,r0"			"\n"
	 "	mov.l	r0,@-r15"		"\n"
	 "	mov.<bw>	@%0,r0"		"\n"
	 "	not	r0,r0"			"\n"
	 "	mov.<bw>	r0,@%0"		"\n"
	 "	mov.l	@r15+,r0"		"\n"
	 "	movco.l	r0,@%1"			"\n"
	 "	bf	0b";
}
  [(set_attr "length" "26")])

(define_insn_and_split "atomic_fetch_<fetchop_name><mode>_soft_gusa"
  [(set (match_operand:QIHISI 0 "arith_reg_dest" "=&u")
	(match_operand:QIHISI 1 "atomic_mem_operand_1" "=AraAdd"))
   (set (match_dup 1)
	(unspec:QIHISI
	  [(FETCHOP:QIHISI
		(match_dup 1)
		(match_operand:QIHISI 2 "<fetchop_predicate_1>"
					"<fetchop_constraint_1_gusa>"))]
	  UNSPEC_ATOMIC))
   (clobber (match_scratch:QIHISI 3 "=&u"))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))]
  "TARGET_ATOMIC_SOFT_GUSA"
{
  return "\r	mova	1f,r0"			"\n"
	 "	.align 2"			"\n"
	 "	mov	r15,r1"			"\n"
	 "	mov	#(0f-1f),r15"		"\n"
	 "0:	mov.<bwl>	%1,%0"		"\n"
	 "	mov	%0,%3"			"\n"
	 "	<fetchop_name>	%2,%3"		"\n"
	 "	mov.<bwl>	%3,%1"		"\n"
	 "1:	mov	r1,r15";
}
  "&& can_create_pseudo_p () && optimize
   && sh_reg_dead_or_unused_after_insn (insn, REGNO (operands[0]))"
  [(const_int 0)]
{
  emit_insn (gen_atomic_<fetchop_name>_fetch<mode>_soft_gusa (
		gen_reg_rtx (<MODE>mode), operands[1], operands[2]));
}
  [(set_attr "length" "18")])

;; Combine pattern for xor (val, -1) / nand (val, -1).
(define_insn_and_split "atomic_fetch_not<mode>_soft_gusa"
  [(set (match_operand:QIHISI 0 "arith_reg_dest" "=&u")
	(match_operand:QIHISI 1 "atomic_mem_operand_1" "=AraAdd"))
   (set (match_dup 1)
	(unspec:QIHISI [(not:QIHISI (match_dup 1))] UNSPEC_ATOMIC))
   (clobber (match_scratch:QIHISI 2 "=&u"))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))]
  "TARGET_ATOMIC_SOFT_GUSA"
{
  return "\r	mova	1f,r0"			"\n"
	 "	mov	r15,r1"			"\n"
	 "	.align 2"			"\n"
	 "	mov	#(0f-1f),r15"		"\n"
	 "0:	mov.<bwl>	%1,%0"		"\n"
	 "	not	%0,%2"			"\n"
	 "	mov.<bwl>	%2,%1"		"\n"
	 "1:	mov	r1,r15";
}
  "&& can_create_pseudo_p () && optimize
   && sh_reg_dead_or_unused_after_insn (insn, REGNO (operands[0]))"
  [(const_int 0)]
{
  emit_insn (gen_atomic_not_fetch<mode>_soft_gusa (gen_reg_rtx (<MODE>mode),
						   operands[1]));
}
  [(set_attr "length" "16")])

(define_insn_and_split "atomic_fetch_<fetchop_name><mode>_soft_tcb"
  [(set (match_operand:QIHISI 0 "arith_reg_dest" "=&r")
	(match_operand:QIHISI 1 "atomic_mem_operand_1" "=SraSdd"))
   (set (match_dup 1)
	(unspec:QIHISI
	  [(FETCHOP:QIHISI
		(match_dup 1)
		(match_operand:QIHISI 2 "<fetchop_predicate_1>"
					"<fetchop_constraint_1_tcb>"))]
	  UNSPEC_ATOMIC))
   (use (match_operand:SI 3 "gbr_displacement"))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))]
  "TARGET_ATOMIC_SOFT_TCB"
{
  return "\r	mova	1f,r0"			"\n"
	 "	.align 2"			"\n"
	 "	mov	#(0f-1f),r1"		"\n"
	 "	mov.l	r0,@(%O3,gbr)"		"\n"
	 "0:	mov.<bwl>	%1,r0"		"\n"
	 "	mov	r0,%0"			"\n"
	 "	<fetchop_name>	%2,r0"		"\n"
	 "	mov.<bwl>	r0,%1"		"\n"
	 "1:	mov	#0,r0"			"\n"
	 "	mov.l	r0,@(%O3,gbr)";
}
  "&& can_create_pseudo_p () && optimize
   && sh_reg_dead_or_unused_after_insn (insn, REGNO (operands[0]))"
  [(const_int 0)]
{
  emit_insn (gen_atomic_<fetchop_name><mode>_soft_tcb (
					operands[1], operands[2], operands[3]));
}
  [(set_attr "length" "20")])

(define_insn "atomic_<fetchop_name><mode>_soft_tcb"
  [(set (match_operand:QIHISI 0 "atomic_mem_operand_1" "=SraSdd")
	(unspec:QIHISI
	  [(FETCHOP:QIHISI
		(match_dup 0)
		(match_operand:QIHISI 1 "<fetchop_predicate_1>"
					"<fetchop_constraint_1_tcb>"))]
	  UNSPEC_ATOMIC))
   (use (match_operand:SI 2 "gbr_displacement"))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))]
  "TARGET_ATOMIC_SOFT_TCB"
{
  return "\r	mova	1f,r0"			"\n"
	 "	mov	#(0f-1f),r1"		"\n"
	 "	.align 2"			"\n"
	 "	mov.l	r0,@(%O2,gbr)"		"\n"
	 "0:	mov.<bwl>	%0,r0"		"\n"
	 "	<fetchop_name>	%1,r0"		"\n"
	 "	mov.<bwl>	r0,%0"		"\n"
	 "1:	mov	#0,r0"			"\n"
	 "	mov.l	r0,@(%O2,gbr)";
}
  [(set_attr "length" "18")])

;; Combine pattern for xor (val, -1) / nand (val, -1).
(define_insn_and_split "atomic_fetch_not<mode>_soft_tcb"
  [(set (match_operand:QIHISI 0 "arith_reg_dest" "=&r")
	(match_operand:QIHISI 1 "atomic_mem_operand_1" "=SraSdd"))
   (set (match_dup 1)
	(unspec:QIHISI [(not:QIHISI (match_dup 1))] UNSPEC_ATOMIC))
   (use (match_operand:SI 2 "gbr_displacement"))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))]
  "TARGET_ATOMIC_SOFT_TCB"
{
  return "\r	mova	1f,r0"			"\n"
	 "	.align 2"			"\n"
	 "	mov	#(0f-1f),r1"		"\n"
	 "	mov.l	r0,@(%O2,gbr)"		"\n"
	 "0:	mov.<bwl>	%1,r0"		"\n"
	 "	mov	r0,%0"			"\n"
	 "	not	r0,r0"			"\n"
	 "	mov.<bwl>	r0,%1"		"\n"
	 "1:	mov	#0,r0"			"\n"
	 "	mov.l	r0,@(%O2,gbr)";
}
  "&& can_create_pseudo_p () && optimize
   && sh_reg_dead_or_unused_after_insn (insn, REGNO (operands[0]))"
  [(const_int 0)]
{
  emit_insn (gen_atomic_not<mode>_soft_tcb (operands[1], operands[2]));
}
  [(set_attr "length" "20")])

(define_insn "atomic_not<mode>_soft_tcb"
  [(set (match_operand:QIHISI 0 "atomic_mem_operand_1" "=SraSdd")
	(unspec:QIHISI [(not:QIHISI (match_dup 0))] UNSPEC_ATOMIC))
   (use (match_operand:SI 1 "gbr_displacement"))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))]
  "TARGET_ATOMIC_SOFT_TCB"
{
  return "\r	mova	1f,r0"			"\n"
	 "	mov	#(0f-1f),r1"		"\n"
	 "	.align 2"			"\n"
	 "	mov.l	r0,@(%O1,gbr)"		"\n"
	 "0:	mov.<bwl>	%0,r0"		"\n"
	 "	not	r0,r0"			"\n"
	 "	mov.<bwl>	r0,%0"		"\n"
	 "1:	mov	#0,r0"			"\n"
	 "	mov.l	r0,@(%O1,gbr)";
}
  [(set_attr "length" "18")])

(define_insn_and_split "atomic_fetch_<fetchop_name><mode>_soft_imask"
  [(set (match_operand:QIHISI 0 "arith_reg_dest" "=&r")
	(match_operand:QIHISI 1 "atomic_mem_operand_1" "=SraSdd"))
   (set (match_dup 1)
	(unspec:QIHISI
	  [(FETCHOP:QIHISI
		(match_dup 1)
		(match_operand:QIHISI 2 "<fetchop_predicate_1>"
					"<fetchop_constraint_1_imask>"))]
	  UNSPEC_ATOMIC))
   (clobber (reg:SI R0_REG))
   (clobber (match_scratch:QIHISI 3 "=&r"))]
  "TARGET_ATOMIC_SOFT_IMASK"
{
  return "\r	stc	sr,r0"			"\n"
	 "	mov	r0,%3"			"\n"
	 "	or	#0xF0,r0"		"\n"
	 "	ldc	r0,sr"			"\n"
	 "	mov.<bwl>	%1,r0"		"\n"
	 "	mov	r0,%0"			"\n"
	 "	<fetchop_name>	%2,r0"		"\n"
	 "	mov.<bwl>	r0,%1"		"\n"
	 "	ldc	%3,sr";
}
  "&& can_create_pseudo_p () && optimize
   && sh_reg_dead_or_unused_after_insn (insn, REGNO (operands[0]))"
  [(const_int 0)]
{
  emit_insn (gen_atomic_<fetchop_name>_fetch<mode>_soft_imask (
			gen_reg_rtx (<MODE>mode), operands[1], operands[2]));
}
  [(set_attr "length" "18")])

;; Combine pattern for xor (val, -1) / nand (val, -1).
(define_insn_and_split "atomic_fetch_not<mode>_soft_imask"
  [(set (match_operand:QIHISI 0 "arith_reg_dest" "=&r")
	(match_operand:QIHISI 1 "atomic_mem_operand_1" "=SraSdd"))
   (set (match_dup 1)
	(unspec:QIHISI [(not:QIHISI (match_dup 1))] UNSPEC_ATOMIC))
   (clobber (reg:SI R0_REG))
   (clobber (match_scratch:QIHISI 2 "=&r"))]
  "TARGET_ATOMIC_SOFT_IMASK"
{
  return "\r	stc	sr,r0"			"\n"
	 "	mov	r0,%2"			"\n"
	 "	or	#0xF0,r0"		"\n"
	 "	ldc	r0,sr"			"\n"
	 "	mov.<bwl>	%1,r0"		"\n"
	 "	mov	r0,%0"			"\n"
	 "	not	r0,r0"			"\n"
	 "	mov.<bwl>	r0,%1"		"\n"
	 "	ldc	%2,sr";
}
  "&& can_create_pseudo_p () && optimize
   && sh_reg_dead_or_unused_after_insn (insn, REGNO (operands[0]))"
  [(const_int 0)]
{
  emit_insn (gen_atomic_not_fetch<mode>_soft_imask (gen_reg_rtx (<MODE>mode),
						    operands[1]));
}
  [(set_attr "length" "18")])

(define_expand "atomic_fetch_nand<mode>"
  [(set (match_operand:QIHISI 0 "arith_reg_dest")
	(match_operand:QIHISI 1 "atomic_mem_operand_1"))
   (set (match_dup 1)
	(unspec:QIHISI
	  [(not:QIHISI (and:QIHISI (match_dup 1)
		       (match_operand:QIHISI 2 "atomic_logical_operand_1")))]
	  UNSPEC_ATOMIC))
   (match_operand:SI 3 "const_int_operand")]
  "TARGET_ATOMIC_ANY"
{
  rtx mem = operands[1];
  rtx atomic_insn;

  if (TARGET_ATOMIC_HARD_LLCS
      || (TARGET_SH4A && <MODE>mode == SImode && !TARGET_ATOMIC_STRICT))
    atomic_insn = gen_atomic_fetch_nand<mode>_hard (operands[0], mem,
						    operands[2]);
  else if (TARGET_ATOMIC_SOFT_GUSA)
    atomic_insn = gen_atomic_fetch_nand<mode>_soft_gusa (operands[0], mem,
							 operands[2]);
  else if (TARGET_ATOMIC_SOFT_TCB)
    atomic_insn = gen_atomic_fetch_nand<mode>_soft_tcb (operands[0], mem,
		      operands[2], TARGET_ATOMIC_SOFT_TCB_GBR_OFFSET_RTX);
  else if (TARGET_ATOMIC_SOFT_IMASK)
    atomic_insn = gen_atomic_fetch_nand<mode>_soft_imask (operands[0], mem,
							  operands[2]);
  else
    FAIL;

  emit_insn (atomic_insn);

  if (<MODE>mode == QImode)
    emit_insn (gen_zero_extendqisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  else if (<MODE>mode == HImode)
    emit_insn (gen_zero_extendhisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  DONE;
})

(define_insn_and_split "atomic_fetch_nandsi_hard"
  [(set (match_operand:SI 0 "arith_reg_dest" "=&r")
	(match_operand:SI 1 "atomic_mem_operand_1" "=Sra"))
   (set (match_dup 1)
	(unspec:SI
	  [(not:SI (and:SI (match_dup 1)
		   (match_operand:SI 2 "logical_operand" "rK08")))]
	  UNSPEC_ATOMIC))
   (set (reg:SI T_REG) (const_int 1))
   (clobber (reg:SI R0_REG))]
  "TARGET_ATOMIC_HARD_LLCS
   || (TARGET_SH4A && TARGET_ATOMIC_ANY && !TARGET_ATOMIC_STRICT)"
{
  return "\r0:	movli.l	%1,r0"		"\n"
	 "	mov	r0,%0"		"\n"
	 "	and	%2,r0"		"\n"
	 "	not	r0,r0"		"\n"
	 "	movco.l	r0,%1"		"\n"
	 "	bf	0b";
}
  "&& can_create_pseudo_p () && optimize
   && sh_reg_dead_or_unused_after_insn (insn, REGNO (operands[0]))"
  [(const_int 0)]
{
  emit_insn (gen_atomic_nand_fetchsi_hard (gen_reg_rtx (SImode), operands[1],
					   operands[2]));
}
  [(set_attr "length" "12")])

;; The QIHImode llcs patterns modify the address register of the memory
;; operand.  In order to express that, we have to open code the memory
;; operand.  Initially the insn is expanded like every other atomic insn
;; using the memory operand.  In split1 the insn is converted and the
;; memory operand's address register is exposed.
(define_insn_and_split "atomic_fetch_nand<mode>_hard"
  [(set (match_operand:QIHI 0 "arith_reg_dest")
	(match_operand:QIHI 1 "atomic_mem_operand_1"))
   (set (match_dup 1)
	(unspec:QIHI
	  [(not:QIHI (and:QIHI (match_dup 1)
		     (match_operand:QIHI 2 "logical_operand" "rK08")))]
	  UNSPEC_ATOMIC))
   (set (reg:SI T_REG) (const_int 1))
   (clobber (reg:SI R0_REG))]
  "TARGET_ATOMIC_HARD_LLCS && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  if (optimize
      && sh_reg_dead_or_unused_after_insn (curr_insn, REGNO (operands[0])))
    emit_insn (gen_atomic_nand<mode>_hard (operands[1], operands[2]));
  else
    {
      rtx i = gen_atomic_fetch_nand<mode>_hard_1 (
			operands[0], XEXP (operands[1], 0), operands[2]);

      /* Replace the new mems in the new insn with the old mem to preserve
	 aliasing info.  */
      XEXP (XVECEXP (i, 0, 0), 1) = operands[1];
      XEXP (XVECEXP (i, 0, 1), 0) = operands[1];
      XEXP (XEXP (XVECEXP (XEXP (XVECEXP (i, 0, 1), 1), 0, 0), 0),
	    0) = operands[1];
      emit_insn (i);
    }
})

(define_insn "atomic_fetch_nand<mode>_hard_1"
  [(set (match_operand:QIHI 0 "arith_reg_dest" "=&r")
	(mem:QIHI (match_operand:SI 1 "arith_reg_operand" "r")))
   (set (mem:QIHI (match_dup 1))
	(unspec:QIHI
	  [(not:QIHI (and:QIHI (mem:QIHI (match_dup 1))
		     (match_operand:QIHI 2 "logical_operand" "rK08")))]
	  UNSPEC_ATOMIC))
   (set (reg:SI T_REG) (const_int 1))
   (clobber (reg:SI R0_REG))
   (clobber (match_scratch:SI 3 "=&r"))
   (clobber (match_scratch:SI 4 "=1"))]
  "TARGET_ATOMIC_HARD_LLCS"
{
  return "\r	mov	#-4,%3"			"\n"
	 "	and	%1,%3"			"\n"
	 "	xor	%3,%1"			"\n"
	 "	add	r15,%1"			"\n"
	 "	add	#-4,%1"			"\n"
	 "0:	movli.l	@%3,r0"			"\n"
	 "	mov.l	r0,@-r15"		"\n"
	 "	mov.<bw>	@%1,r0"		"\n"
	 "	mov	r0,%0"			"\n"
	 "	and	%2,r0"			"\n"
	 "	not	r0,r0"			"\n"
	 "	mov.<bw>	r0,@%1"		"\n"
	 "	mov.l	@r15+,r0"		"\n"
	 "	movco.l	r0,@%3"			"\n"
	 "	bf	0b";
}
  [(set_attr "length" "30")])

;; The QIHImode llcs patterns modify the address register of the memory
;; operand.  In order to express that, we have to open code the memory
;; operand.  Initially the insn is expanded like every other atomic insn
;; using the memory operand.  In split1 the insn is converted and the
;; memory operand's address register is exposed.
(define_insn_and_split "atomic_nand<mode>_hard"
  [(set (match_operand:QIHI 0 "atomic_mem_operand_1")
	(unspec:QIHI
	  [(not:QIHI (and:QIHI (match_dup 0)
			       (match_operand:QIHI 1 "logical_operand")))]
	  UNSPEC_ATOMIC))
   (set (reg:SI T_REG) (const_int 1))
   (clobber (reg:SI R0_REG))]
  "TARGET_ATOMIC_HARD_LLCS && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  rtx i = gen_atomic_nand<mode>_hard_1 (XEXP (operands[0], 0), operands[1]);

  /* Replace the new mems in the new insn with the old mem to preserve
     aliasing info.  */
  XEXP (XVECEXP (i, 0, 0), 0) = operands[0];
  XEXP (XEXP (XVECEXP (XEXP (XVECEXP (i, 0, 0), 1), 0, 0), 0), 0) = operands[0];
  emit_insn (i);
})

(define_insn "atomic_nand<mode>_hard_1"
  [(set (mem:QIHI (match_operand:SI 0 "arith_reg_operand" "r"))
	(unspec:QIHI
	  [(not:QIHI (and:QIHI (mem:QIHI (match_dup 0))
		     (match_operand:QIHI 1 "logical_operand" "rK08")))]
	  UNSPEC_ATOMIC))
   (set (reg:SI T_REG) (const_int 1))
   (clobber (reg:SI R0_REG))
   (clobber (match_scratch:SI 2 "=&r"))
   (clobber (match_scratch:SI 3 "=0"))]
  "TARGET_ATOMIC_HARD_LLCS"
{
  return "\r	mov	#-4,%2"			"\n"
	 "	and	%0,%2"			"\n"
	 "	xor	%2,%0"			"\n"
	 "	add	r15,%0"			"\n"
	 "	add	#-4,%0"			"\n"
	 "0:	movli.l	@%2,r0"			"\n"
	 "	mov.l	r0,@-r15"		"\n"
	 "	mov.<bw>	@%0,r0"		"\n"
	 "	and	%1,r0"			"\n"
	 "	not	r0,r0"			"\n"
	 "	mov.<bw>	r0,@%0"		"\n"
	 "	mov.l	@r15+,r0"		"\n"
	 "	movco.l	r0,@%2"			"\n"
	 "	bf	0b";
}
  [(set_attr "length" "28")])

(define_insn_and_split "atomic_fetch_nand<mode>_soft_gusa"
  [(set (match_operand:QIHISI 0 "arith_reg_dest" "=&u")
	(match_operand:QIHISI 1 "atomic_mem_operand_1" "=AraAdd"))
   (set (match_dup 1)
	(unspec:QIHISI
	  [(not:QIHISI
	     (and:QIHISI (match_dup 1)
			 (match_operand:QIHISI 2 "arith_reg_operand" "u")))]
	  UNSPEC_ATOMIC))
   (clobber (match_scratch:QIHISI 3 "=&u"))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))]
  "TARGET_ATOMIC_SOFT_GUSA"
{
  return "\r	mova	1f,r0"			"\n"
	 "	mov	r15,r1"			"\n"
	 "	.align 2"			"\n"
	 "	mov	#(0f-1f),r15"		"\n"
	 "0:	mov.<bwl>	%1,%0"		"\n"
	 "	mov	%2,%3"			"\n"
	 "	and	%0,%3"			"\n"
	 "	not	%3,%3"			"\n"
	 "	mov.<bwl>	%3,%1"		"\n"
	 "1:	mov	r1,r15";
}
  "&& can_create_pseudo_p () && optimize
   && sh_reg_dead_or_unused_after_insn (insn, REGNO (operands[0]))"
  [(const_int 0)]
{
  emit_insn (gen_atomic_nand_fetch<mode>_soft_gusa (gen_reg_rtx (<MODE>mode),
						    operands[1], operands[2]));
}
  [(set_attr "length" "20")])

(define_insn_and_split "atomic_fetch_nand<mode>_soft_tcb"
  [(set (match_operand:QIHISI 0 "arith_reg_dest" "=&r")
	(match_operand:QIHISI 1 "atomic_mem_operand_1" "=SraSdd"))
   (set (match_dup 1)
	(unspec:QIHISI
	  [(not:QIHISI
	     (and:QIHISI (match_dup 1)
			 (match_operand:QIHISI 2 "logical_operand" "rK08")))]
	  UNSPEC_ATOMIC))
   (use (match_operand:SI 3 "gbr_displacement"))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))]
  "TARGET_ATOMIC_SOFT_TCB"
{
  return "\r	mova	1f,r0"			"\n"
	 "	mov	#(0f-1f),r1"		"\n"
	 "	.align 2"			"\n"
	 "	mov.l	r0,@(%O3,gbr)"		"\n"
	 "0:	mov.<bwl>	%1,r0"		"\n"
	 "	mov	r0,%0"			"\n"
	 "	and	%2,r0"			"\n"
	 "	not	r0,r0"			"\n"
	 "	mov.<bwl>	r0,%1"		"\n"
	 "1:	mov	#0,r0"			"\n"
	 "	mov.l	r0,@(%O3,gbr)";
}
  "&& can_create_pseudo_p () && optimize
   && sh_reg_dead_or_unused_after_insn (insn, REGNO (operands[0]))"
  [(const_int 0)]
{
  emit_insn (gen_atomic_nand<mode>_soft_tcb (operands[1], operands[2],
					     operands[3]));
}
  [(set_attr "length" "22")])

(define_insn "atomic_nand<mode>_soft_tcb"
  [(set (match_operand:QIHISI 0 "atomic_mem_operand_1" "=SraSdd")
	(unspec:QIHISI
	  [(not:QIHISI
	     (and:QIHISI (match_dup 0)
			 (match_operand:QIHISI 1 "logical_operand" "rK08")))]
	  UNSPEC_ATOMIC))
   (use (match_operand:SI 2 "gbr_displacement"))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))]
  "TARGET_ATOMIC_SOFT_TCB"
{
  return "\r	mova	1f,r0"			"\n"
	 "	.align 2"			"\n"
	 "	mov	#(0f-1f),r1"		"\n"
	 "	mov.l	r0,@(%O2,gbr)"		"\n"
	 "0:	mov.<bwl>	%0,r0"		"\n"
	 "	and	%1,r0"			"\n"
	 "	not	r0,r0"			"\n"
	 "	mov.<bwl>	r0,%0"		"\n"
	 "1:	mov	#0,r0"			"\n"
	 "	mov.l	r0,@(%O2,gbr)";
}
  [(set_attr "length" "20")])

(define_insn_and_split "atomic_fetch_nand<mode>_soft_imask"
  [(set (match_operand:QIHISI 0 "arith_reg_dest" "=&r")
	(match_operand:QIHISI 1 "atomic_mem_operand_1" "=SraSdd"))
   (set (match_dup 1)
	(unspec:QIHISI
	  [(not:QIHISI
	     (and:QIHISI (match_dup 1)
			 (match_operand:QIHISI 2 "logical_operand" "rK08")))]
	  UNSPEC_ATOMIC))
   (clobber (reg:SI R0_REG))
   (clobber (match_scratch:SI 3 "=&r"))]
  "TARGET_ATOMIC_SOFT_IMASK"
{
  return "\r	stc	sr,r0"			"\n"
	 "	mov	r0,%3"			"\n"
	 "	or	#0xF0,r0"		"\n"
	 "	ldc	r0,sr"			"\n"
	 "	mov.<bwl>	%1,r0"		"\n"
	 "	mov	r0,%0"			"\n"
	 "	and	%2,r0"			"\n"
	 "	not	r0,r0"			"\n"
	 "	mov.<bwl>	r0,%1"		"\n"
	 "	ldc	%3,sr";
}
  "&& can_create_pseudo_p () && optimize
   && sh_reg_dead_or_unused_after_insn (insn, REGNO (operands[0]))"
  [(const_int 0)]
{
  emit_insn (gen_atomic_nand_fetch<mode>_soft_imask (gen_reg_rtx (<MODE>mode),
						     operands[1], operands[2]));
}
  [(set_attr "length" "20")])

;;------------------------------------------------------------------------------
;; read - add|sub|or|and|xor|nand - write - return new value

(define_expand "atomic_<fetchop_name>_fetch<mode>"
  [(set (match_operand:QIHISI 0 "arith_reg_dest")
	(FETCHOP:QIHISI
	  (match_operand:QIHISI 1 "atomic_mem_operand_1")
	  (match_operand:QIHISI 2 "<fetchop_predicate_1>")))
   (set (match_dup 1)
	(unspec:QIHISI
	  [(FETCHOP:QIHISI (match_dup 1) (match_dup 2))]
	  UNSPEC_ATOMIC))
   (match_operand:SI 3 "const_int_operand" "")]
  "TARGET_ATOMIC_ANY"
{
  rtx mem = operands[1];
  rtx atomic_insn;

  if (TARGET_ATOMIC_HARD_LLCS
      || (TARGET_SH4A && <MODE>mode == SImode && !TARGET_ATOMIC_STRICT))
    atomic_insn = gen_atomic_<fetchop_name>_fetch<mode>_hard (operands[0], mem,
							      operands[2]);
  else if (TARGET_ATOMIC_SOFT_GUSA)
    atomic_insn = gen_atomic_<fetchop_name>_fetch<mode>_soft_gusa (operands[0],
		      mem, operands[2]);
  else if (TARGET_ATOMIC_SOFT_TCB)
    atomic_insn = gen_atomic_<fetchop_name>_fetch<mode>_soft_tcb (operands[0],
		      mem, operands[2], TARGET_ATOMIC_SOFT_TCB_GBR_OFFSET_RTX);
  else if (TARGET_ATOMIC_SOFT_IMASK)
    atomic_insn = gen_atomic_<fetchop_name>_fetch<mode>_soft_imask (operands[0],
		      mem, operands[2]);
  else
    FAIL;

  emit_insn (atomic_insn);

  if (<MODE>mode == QImode)
    emit_insn (gen_zero_extendqisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  else if (<MODE>mode == HImode)
    emit_insn (gen_zero_extendhisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  DONE;
})

(define_insn "atomic_<fetchop_name>_fetchsi_hard"
  [(set (match_operand:SI 0 "arith_reg_dest" "=&z")
	(FETCHOP:SI
	  (match_operand:SI 1 "atomic_mem_operand_1" "=Sra")
	  (match_operand:SI 2 "<fetchop_predicate_1>"
			      "<fetchop_constraint_1_llcs>")))
   (set (match_dup 1)
	(unspec:SI
	  [(FETCHOP:SI (match_dup 1) (match_dup 2))]
	  UNSPEC_ATOMIC))
   (set (reg:SI T_REG) (const_int 1))]
  "TARGET_ATOMIC_HARD_LLCS
   || (TARGET_SH4A && TARGET_ATOMIC_ANY && !TARGET_ATOMIC_STRICT)"
{
  return "\r0:	movli.l	%1,%0"		"\n"
	 "	<fetchop_name>	%2,%0"	"\n"
	 "	movco.l	%0,%1"		"\n"
	 "	bf	0b";
}
  [(set_attr "length" "8")])

;; Combine pattern for xor (val, -1) / nand (val, -1).
(define_insn "atomic_not_fetchsi_hard"
  [(set (match_operand:SI 0 "arith_reg_dest" "=&z")
	(not:SI (match_operand:SI 1 "atomic_mem_operand_1" "=Sra")))
   (set (match_dup 1)
	(unspec:SI [(not:SI (match_dup 1))] UNSPEC_ATOMIC))
   (set (reg:SI T_REG) (const_int 1))]
  "TARGET_ATOMIC_HARD_LLCS
   || (TARGET_SH4A && TARGET_ATOMIC_ANY && !TARGET_ATOMIC_STRICT)"
{
  return "\r0:	movli.l	%1,%0"		"\n"
	 "	not	%0,%0"		"\n"
	 "	movco.l	%0,%1"		"\n"
	 "	bf	0b";
}
  [(set_attr "length" "8")])

;; The QIHImode llcs patterns modify the address register of the memory
;; operand.  In order to express that, we have to open code the memory
;; operand.  Initially the insn is expanded like every other atomic insn
;; using the memory operand.  In split1 the insn is converted and the
;; memory operand's address register is exposed.
(define_insn_and_split "atomic_<fetchop_name>_fetch<mode>_hard"
  [(set (match_operand:QIHI 0 "arith_reg_dest" "=&r")
	(FETCHOP:QIHI (match_operand:QIHI 1 "atomic_mem_operand_1")
		      (match_operand:QIHI 2 "<fetchop_predicate_1>")))
   (set (match_dup 1) (unspec:QIHI [(FETCHOP:QIHI (match_dup 1) (match_dup 2))]
				   UNSPEC_ATOMIC))
   (set (reg:SI T_REG) (const_int 1))
   (clobber (reg:SI R0_REG))]
  "TARGET_ATOMIC_HARD_LLCS && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  if (optimize
      && sh_reg_dead_or_unused_after_insn (curr_insn, REGNO (operands[0])))
    emit_insn (gen_atomic_<fetchop_name><mode>_hard (operands[1], operands[2]));
  else
    {
      rtx i = gen_atomic_<fetchop_name>_fetch<mode>_hard_1 (
			operands[0], XEXP (operands[1], 0), operands[2]);

      /* Replace the new mems in the new insn with the old mem to preserve
	 aliasing info.  */
      XEXP (XEXP (XVECEXP (i, 0, 0), 1), 0) = operands[1];
      XEXP (XVECEXP (i, 0, 1), 0) = operands[1];
      XEXP (XVECEXP (XEXP (XVECEXP (i, 0, 1), 1), 0, 0), 0) = operands[1];
      emit_insn (i);
    }
})

(define_insn "atomic_<fetchop_name>_fetch<mode>_hard_1"
  [(set (match_operand:QIHI 0 "arith_reg_dest" "=&r")
	(FETCHOP:QIHI
	  (mem:QIHI (match_operand:SI 1 "arith_reg_operand" "r"))
	  (match_operand:QIHI 2 "<fetchop_predicate_1>"
				"<fetchop_constraint_1_llcs>")))
   (set (mem:QIHI (match_dup 1))
	(unspec:QIHI
	  [(FETCHOP:QIHI (mem:QIHI (match_dup 1)) (match_dup 2))]
	  UNSPEC_ATOMIC))
   (set (reg:SI T_REG) (const_int 1))
   (clobber (reg:SI R0_REG))
   (clobber (match_scratch:SI 3 "=&r"))
   (clobber (match_scratch:SI 4 "=1"))]
  "TARGET_ATOMIC_HARD_LLCS"
{
  return "\r	mov	#-4,%3"			"\n"
	 "	and	%1,%3"			"\n"
	 "	xor	%3,%1"			"\n"
	 "	add	r15,%1"			"\n"
	 "	add	#-4,%1"			"\n"
	 "0:	movli.l	@%3,r0"			"\n"
	 "	mov.l	r0,@-r15"		"\n"
	 "	mov.<bw>	@%1,r0"		"\n"
	 "	<fetchop_name>	%2,r0"		"\n"
	 "	mov.<bw>	r0,@%1"		"\n"
	 "	mov	r0,%0"			"\n"
	 "	mov.l	@r15+,r0"		"\n"
	 "	movco.l	r0,@%3"			"\n"
	 "	bf	0b";
}
  [(set_attr "length" "28")])

;; Combine pattern for xor (val, -1) / nand (val, -1).
(define_insn_and_split "atomic_not_fetch<mode>_hard"
  [(set (match_operand:QIHI 0 "arith_reg_dest" "=&r")
	(not:QIHI (mem:QIHI (match_operand:SI 1 "arith_reg_operand" "r"))))
   (set (mem:QIHI (match_dup 1))
	(unspec:QIHI [(not:QIHI (mem:QIHI (match_dup 1)))] UNSPEC_ATOMIC))
   (set (reg:SI T_REG) (const_int 1))
   (clobber (reg:SI R0_REG))
   (clobber (match_scratch:SI 2 "=&r"))
   (clobber (match_scratch:SI 3 "=1"))]
  "TARGET_ATOMIC_HARD_LLCS"
{
  return "\r	mov	#-4,%2"			"\n"
	 "	and	%1,%2"			"\n"
	 "	xor	%2,%1"			"\n"
	 "	add	r15,%1"			"\n"
	 "	add	#-4,%1"			"\n"
	 "0:	movli.l	@%2,r0"			"\n"
	 "	mov.l	r0,@-r15"		"\n"
	 "	mov.<bw>	@%1,r0"		"\n"
	 "	not	r0,r0"			"\n"
	 "	mov.<bw>	r0,@%1"		"\n"
	 "	mov	r0,%0"			"\n"
	 "	mov.l	@r15+,r0"		"\n"
	 "	movco.l	r0,@%2"			"\n"
	 "	bf	0b";
}
  "&& can_create_pseudo_p () && optimize
   && sh_reg_dead_or_unused_after_insn (insn, REGNO (operands[0]))"
  [(const_int 0)]
{
  rtx i = gen_atomic_not<mode>_hard (operands[1]);

  /* Replace the new mems in the new insn with the old mem to preserve
     aliasing info.  */
  rtx m = XEXP (XEXP (XVECEXP (PATTERN (curr_insn), 0, 0), 1), 0);
  XEXP (XVECEXP (i, 0, 0), 0) = m;
  XEXP (XVECEXP (XEXP (XVECEXP (i, 0, 0), 1), 0, 0), 0) = m;
  emit_insn (i);
}
  [(set_attr "length" "28")])

(define_insn "atomic_<fetchop_name>_fetch<mode>_soft_gusa"
  [(set (match_operand:QIHISI 0 "arith_reg_dest" "=&u")
	(FETCHOP:QIHISI
	  (match_operand:QIHISI 1 "atomic_mem_operand_1" "=AraAdd")
	  (match_operand:QIHISI 2 "<fetchop_predicate_1>"
				  "<fetchop_constraint_1_gusa>")))
   (set (match_dup 1)
	(unspec:QIHISI
	  [(FETCHOP:QIHISI (match_dup 1) (match_dup 2))]
	  UNSPEC_ATOMIC))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))]
  "TARGET_ATOMIC_SOFT_GUSA"
{
  return "\r	mova	1f,r0"			"\n"
	 "	mov	r15,r1"			"\n"
	 "	.align 2"			"\n"
	 "	mov	#(0f-1f),r15"		"\n"
	 "0:	mov.<bwl>	%1,%0"		"\n"
	 "	<fetchop_name>	%2,%0"		"\n"
	 "	mov.<bwl>	%0,%1"		"\n"
	 "1:	mov	r1,r15";
}
  [(set_attr "length" "16")])

;; Combine pattern for xor (val, -1) / nand (val, -1).
(define_insn "atomic_not_fetch<mode>_soft_gusa"
  [(set (match_operand:QIHISI 0 "arith_reg_dest" "=&u")
	(not:QIHISI (match_operand:QIHISI 1 "atomic_mem_operand_1" "=AraAdd")))
   (set (match_dup 1)
	(unspec:QIHISI [(not:QIHISI (match_dup 1))] UNSPEC_ATOMIC))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))]
  "TARGET_ATOMIC_SOFT_GUSA"
{
  return "\r	mova	1f,r0"			"\n"
	 "	mov	r15,r1"			"\n"
	 "	.align 2"			"\n"
	 "	mov	#(0f-1f),r15"		"\n"
	 "0:	mov.<bwl>	%1,%0"		"\n"
	 "	not	%0,%0"			"\n"
	 "	mov.<bwl>	%0,%1"		"\n"
	 "1:	mov	r1,r15";
}
  [(set_attr "length" "16")])

(define_insn_and_split "atomic_<fetchop_name>_fetch<mode>_soft_tcb"
  [(set (match_operand:QIHISI 0 "arith_reg_dest" "=&r")
	(FETCHOP:QIHISI
	  (match_operand:QIHISI 1 "atomic_mem_operand_1" "=SraSdd")
	  (match_operand:QIHISI 2 "<fetchop_predicate_1>"
				  "<fetchop_constraint_1_tcb>")))
   (set (match_dup 1)
	(unspec:QIHISI
	  [(FETCHOP:QIHISI (match_dup 1) (match_dup 2))]
	  UNSPEC_ATOMIC))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))
   (use (match_operand:SI 3 "gbr_displacement"))]
  "TARGET_ATOMIC_SOFT_TCB"
{
  return "\r	mova	1f,r0"			"\n"
	 "	mov	#(0f-1f),r1"		"\n"
	 "	.align 2"			"\n"
	 "	mov.l	r0,@(%O3,gbr)"		"\n"
	 "0:	mov.<bwl>	%1,r0"		"\n"
	 "	<fetchop_name>	%2,r0"		"\n"
	 "	mov.<bwl>	r0,%1"		"\n"
	 "1:	mov	r0,%0"			"\n"
	 "	mov	#0,r0"			"\n"
	 "	mov.l	r0,@(%O3,gbr)";
}
  "&& can_create_pseudo_p () && optimize
   && sh_reg_dead_or_unused_after_insn (insn, REGNO (operands[0]))"
  [(const_int 0)]
{
  emit_insn (gen_atomic_<fetchop_name><mode>_soft_tcb (
				operands[1], operands[2], operands[3]));
}
  [(set_attr "length" "20")])

;; Combine pattern for xor (val, -1) / nand (val, -1).
(define_insn_and_split "atomic_not_fetch<mode>_soft_tcb"
  [(set (match_operand:QIHISI 0 "arith_reg_dest" "=&r")
	(not:QIHISI (match_operand:QIHISI 1 "atomic_mem_operand_1" "=SraSdd")))
   (set (match_dup 1)
	(unspec:QIHISI [(not:QIHISI (match_dup 1))] UNSPEC_ATOMIC))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))
   (use (match_operand:SI 2 "gbr_displacement"))]
  "TARGET_ATOMIC_SOFT_TCB"
{
  return "\r	mova	1f,r0"			"\n"
	 "	mov	#(0f-1f),r1"		"\n"
	 "	.align 2"			"\n"
	 "	mov.l	r0,@(%O2,gbr)"		"\n"
	 "0:	mov.<bwl>	%1,r0"		"\n"
	 "	not	r0,r0"			"\n"
	 "	mov.<bwl>	r0,%1"		"\n"
	 "1:	mov	r0,%0"			"\n"
	 "	mov	#0,r0"			"\n"
	 "	mov.l	r0,@(%O2,gbr)";
}
  "&& can_create_pseudo_p () && optimize
   && sh_reg_dead_or_unused_after_insn (insn, REGNO (operands[0]))"
  [(const_int 0)]
{
  emit_insn (gen_atomic_not<mode>_soft_tcb (operands[1], operands[2]));
}
  [(set_attr "length" "20")])

(define_insn "atomic_<fetchop_name>_fetch<mode>_soft_imask"
  [(set (match_operand:QIHISI 0 "arith_reg_dest" "=&z")
	(FETCHOP:QIHISI
	  (match_operand:QIHISI 1 "atomic_mem_operand_1" "=SraSdd")
	  (match_operand:QIHISI 2 "<fetchop_predicate_1>"
				  "<fetchop_constraint_1_imask>")))
   (set (match_dup 1)
	(unspec:QIHISI
	  [(FETCHOP:QIHISI (match_dup 1) (match_dup 2))]
	  UNSPEC_ATOMIC))
   (clobber (match_scratch:SI 3 "=&r"))]
  "TARGET_ATOMIC_SOFT_IMASK"
{
  return "\r	stc	sr,%0"			"\n"
	 "	mov	%0,%3"			"\n"
	 "	or	#0xF0,%0"		"\n"
	 "	ldc	%0,sr"			"\n"
	 "	mov.<bwl>	%1,%0"		"\n"
	 "	<fetchop_name>	%2,%0"		"\n"
	 "	mov.<bwl>	%0,%1"		"\n"
	 "	ldc	%3,sr";
}
  [(set_attr "length" "16")])

;; Combine pattern for xor (val, -1) / nand (val, -1).
(define_insn "atomic_not_fetch<mode>_soft_imask"
  [(set (match_operand:QIHISI 0 "arith_reg_dest" "=&z")
	(not:QIHISI (match_operand:QIHISI 1 "atomic_mem_operand_1" "=SraSdd")))
   (set (match_dup 1)
	(unspec:QIHISI [(not:QIHISI (match_dup 1))] UNSPEC_ATOMIC))
   (clobber (match_scratch:SI 2 "=&r"))]
  "TARGET_ATOMIC_SOFT_IMASK"
{
  return "\r	stc	sr,%0"			"\n"
	 "	mov	%0,%2"			"\n"
	 "	or	#0xF0,%0"		"\n"
	 "	ldc	%0,sr"			"\n"
	 "	mov.<bwl>	%1,%0"		"\n"
	 "	not	%0,%0"			"\n"
	 "	mov.<bwl>	%0,%1"		"\n"
	 "	ldc	%2,sr";
}
  [(set_attr "length" "16")])

(define_expand "atomic_nand_fetch<mode>"
  [(set (match_operand:QIHISI 0 "arith_reg_dest")
	(not:QIHISI (and:QIHISI
	  (match_operand:QIHISI 1 "atomic_mem_operand_1")
	  (match_operand:QIHISI 2 "atomic_logical_operand_1"))))
   (set (match_dup 1)
	(unspec:QIHISI
	  [(not:QIHISI (and:QIHISI (match_dup 1) (match_dup 2)))]
	  UNSPEC_ATOMIC))
   (match_operand:SI 3 "const_int_operand")]
  "TARGET_ATOMIC_ANY"
{
  rtx mem = operands[1];
  rtx atomic_insn;

  if (TARGET_ATOMIC_HARD_LLCS
      || (TARGET_SH4A && <MODE>mode == SImode && !TARGET_ATOMIC_STRICT))
    atomic_insn = gen_atomic_nand_fetch<mode>_hard (operands[0], mem,
						    operands[2]);
  else if (TARGET_ATOMIC_SOFT_GUSA)
    atomic_insn = gen_atomic_nand_fetch<mode>_soft_gusa (operands[0], mem,
							 operands[2]);
  else if (TARGET_ATOMIC_SOFT_TCB)
    atomic_insn = gen_atomic_nand_fetch<mode>_soft_tcb (operands[0], mem,
		      operands[2], TARGET_ATOMIC_SOFT_TCB_GBR_OFFSET_RTX);
  else if (TARGET_ATOMIC_SOFT_IMASK)
    atomic_insn = gen_atomic_nand_fetch<mode>_soft_imask (operands[0], mem,
							  operands[2]);
  else
    FAIL;

  emit_insn (atomic_insn);

  if (<MODE>mode == QImode)
    emit_insn (gen_zero_extendqisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  else if (<MODE>mode == HImode)
    emit_insn (gen_zero_extendhisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  DONE;
})

(define_insn "atomic_nand_fetchsi_hard"
  [(set (match_operand:SI 0 "arith_reg_dest" "=&z")
	(not:SI (and:SI (match_operand:SI 1 "atomic_mem_operand_1" "=Sra")
			(match_operand:SI 2 "logical_operand" "rK08"))))
   (set (match_dup 1)
	(unspec:SI
	  [(not:SI (and:SI (match_dup 1) (match_dup 2)))]
	  UNSPEC_ATOMIC))
   (set (reg:SI T_REG) (const_int 1))]
  "TARGET_ATOMIC_HARD_LLCS
   || (TARGET_SH4A && TARGET_ATOMIC_ANY && !TARGET_ATOMIC_STRICT)"
{
  return "\r0:	movli.l	%1,%0"		"\n"
	 "	and	%2,%0"		"\n"
	 "	not	%0,%0"		"\n"
	 "	movco.l	%0,%1"		"\n"
	 "	bf	0b";
}
  [(set_attr "length" "10")])

;; The QIHImode llcs patterns modify the address register of the memory
;; operand.  In order to express that, we have to open code the memory
;; operand.  Initially the insn is expanded like every other atomic insn
;; using the memory operand.  In split1 the insn is converted and the
;; memory operand's address register is exposed.
(define_insn_and_split "atomic_nand_fetch<mode>_hard"
  [(set (match_operand:QIHI 0 "arith_reg_dest" "=&r")
	(not:QIHI (and:QIHI (match_operand:QIHI 1 "atomic_mem_operand_1")
			    (match_operand:QIHI 2 "logical_operand"))))
   (set (match_dup 1)
	(unspec:QIHI [(not:QIHI (and:QIHI (match_dup 1) (match_dup 2)))]
		     UNSPEC_ATOMIC))
   (set (reg:SI T_REG) (const_int 1))
   (clobber (reg:SI R0_REG))]
  "TARGET_ATOMIC_HARD_LLCS && can_create_pseudo_p ()"
  "#"
  "&& 1"
  [(const_int 0)]
{
  if (optimize
      && sh_reg_dead_or_unused_after_insn (curr_insn, REGNO (operands[0])))
    emit_insn (gen_atomic_nand<mode>_hard (operands[1], operands[2]));
  else
    {
      rtx i = gen_atomic_nand_fetch<mode>_hard_1 (
			operands[0], XEXP (operands[1], 0), operands[2]);
      
      /* Replace the new mems in the new insn with the old mem to preserve
	 aliasing info.  */
      XEXP (XEXP (XEXP (XVECEXP (i, 0, 0), 1), 0), 0) = operands[1];
      XEXP (XVECEXP (i, 0, 1), 0) = operands[1];
      XEXP (XEXP (XVECEXP (XEXP (XVECEXP (i, 0, 1), 1), 0, 0), 0),
	    0) = operands[1];
      emit_insn (i);
    }
})

(define_insn "atomic_nand_fetch<mode>_hard_1"
  [(set (match_operand:QIHI 0 "arith_reg_dest" "=&r")
	(not:QIHI
	  (and:QIHI (mem:QIHI (match_operand:SI 1 "arith_reg_operand" "r"))
		    (match_operand:QIHI 2 "logical_operand" "rK08"))))
   (set (mem:QIHI (match_dup 1))
	(unspec:QIHI
	  [(not:QIHI (and:QIHI (mem:QIHI (match_dup 1)) (match_dup 2)))]
	  UNSPEC_ATOMIC))
   (set (reg:SI T_REG) (const_int 1))
   (clobber (reg:SI R0_REG))
   (clobber (match_scratch:SI 3 "=&r"))
   (clobber (match_scratch:SI 4 "=1"))]
  "TARGET_ATOMIC_HARD_LLCS"
{
  return "\r	mov	#-4,%3"			"\n"
	 "	and	%1,%3"			"\n"
	 "	xor	%3,%1"			"\n"
	 "	add	r15,%1"			"\n"
	 "	add	#-4,%1"			"\n"
	 "0:	movli.l	@%3,r0"			"\n"
	 "	mov.l	r0,@-r15"		"\n"
	 "	mov.<bw>	@%1,r0"		"\n"
	 "	and	%2,r0"			"\n"
	 "	not	r0,%0"			"\n"
	 "	mov.<bw>	%0,@%1"		"\n"
	 "	mov.l	@r15+,r0"		"\n"
	 "	movco.l	r0,@%3"			"\n"
	 "	bf	0b";
}
  [(set_attr "length" "28")])

(define_insn "atomic_nand_fetch<mode>_soft_gusa"
  [(set (match_operand:QIHISI 0 "arith_reg_dest" "=&u")
	(not:QIHISI (and:QIHISI
	  (match_operand:QIHISI 1 "atomic_mem_operand_1" "=AraAdd")
	  (match_operand:QIHISI 2 "arith_reg_operand" "u"))))
   (set (match_dup 1)
	(unspec:QIHISI
	  [(not:QIHISI (and:QIHISI (match_dup 1) (match_dup 2)))]
	  UNSPEC_ATOMIC))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))]
  "TARGET_ATOMIC_SOFT_GUSA"
{
  return "\r	mova	1f,r0"			"\n"
	 "	.align 2"			"\n"
	 "	mov	r15,r1"			"\n"
	 "	mov	#(0f-1f),r15"		"\n"
	 "0:	mov.<bwl>	%1,%0"		"\n"
	 "	and	%2,%0"			"\n"
	 "	not	%0,%0"			"\n"
	 "	mov.<bwl>	%0,%1"		"\n"
	 "1:	mov	r1,r15";
}
  [(set_attr "length" "18")])

(define_insn_and_split "atomic_nand_fetch<mode>_soft_tcb"
  [(set (match_operand:QIHISI 0 "arith_reg_dest" "=&r")
	(not:QIHISI (and:QIHISI
	  (match_operand:QIHISI 1 "atomic_mem_operand_1" "=SraSdd")
	  (match_operand:QIHISI 2 "logical_operand" "rK08"))))
   (set (match_dup 1)
	(unspec:QIHISI
	  [(not:QIHISI (and:QIHISI (match_dup 1) (match_dup 2)))]
	  UNSPEC_ATOMIC))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))
   (use (match_operand:SI 3 "gbr_displacement"))]
  "TARGET_ATOMIC_SOFT_TCB"
{
  return "\r	mova	1f,r0"			"\n"
	 "	mov	#(0f-1f),r1"		"\n"
	 "	.align 2"			"\n"
	 "	mov.l	r0,@(%O3,gbr)"		"\n"
	 "0:	mov.<bwl>	%1,r0"		"\n"
	 "	and	%2,r0"			"\n"
	 "	not	r0,r0"			"\n"
	 "	mov	r0,%0"			"\n"
	 "	mov.<bwl>	r0,%1"		"\n"
	 "1:	mov	#0,r0"			"\n"
	 "	mov.l	r0,@(%O3,gbr)";
}
  "&& can_create_pseudo_p () && optimize
   && sh_reg_dead_or_unused_after_insn (insn, REGNO (operands[0]))"
  [(const_int 0)]
{
  emit_insn (gen_atomic_nand<mode>_soft_tcb (operands[1], operands[2],
					     operands[3]));
}
  [(set_attr "length" "22")])

(define_insn "atomic_nand_fetch<mode>_soft_imask"
  [(set (match_operand:QIHISI 0 "arith_reg_dest" "=&z")
	(not:QIHISI (and:QIHISI
	  (match_operand:QIHISI 1 "atomic_mem_operand_1" "=SraSdd")
	  (match_operand:QIHISI 2 "logical_operand" "rK08"))))
   (set (match_dup 1)
	(unspec:QIHISI
	  [(not:QIHISI (and:QIHISI (match_dup 1) (match_dup 2)))]
	  UNSPEC_ATOMIC))
   (clobber (match_scratch:SI 3 "=&r"))]
  "TARGET_ATOMIC_SOFT_IMASK"
{
  return "\r	stc	sr,%0"			"\n"
	 "	mov	%0,%3"			"\n"
	 "	or	#0xF0,%0"		"\n"
	 "	ldc	%0,sr"			"\n"
	 "	mov.<bwl>	%1,%0"		"\n"
	 "	and	%2,%0"			"\n"
	 "	not	%0,%0"			"\n"
	 "	mov.<bwl>	%0,%1"		"\n"
	 "	ldc	%3,sr";
}
  [(set_attr "length" "18")])

;;------------------------------------------------------------------------------
;; read - test against zero - or with 0x80 - write - return test result

(define_expand "atomic_test_and_set"
  [(match_operand:SI 0 "register_operand" "")		;; bool result output
   (match_operand:QI 1 "memory_operand" "")		;; memory
   (match_operand:SI 2 "const_int_operand" "")]		;; model
  "TARGET_ATOMIC_ANY || TARGET_ENABLE_TAS"
{
  rtx addr = force_reg (Pmode, XEXP (operands[1], 0));

  if (TARGET_ENABLE_TAS)
    emit_insn (gen_tasb (addr));
  else
    {
      rtx val = gen_int_mode (targetm.atomic_test_and_set_trueval, QImode);
      val = force_reg (QImode, val);

      if (TARGET_ATOMIC_HARD_LLCS)
	  emit_insn (gen_atomic_test_and_set_hard (addr, val));
      else if (TARGET_ATOMIC_SOFT_GUSA)
	  emit_insn (gen_atomic_test_and_set_soft_gusa (addr, val));
      else if (TARGET_ATOMIC_SOFT_TCB)
	  emit_insn (gen_atomic_test_and_set_soft_tcb (addr, val,
			 TARGET_ATOMIC_SOFT_TCB_GBR_OFFSET_RTX));
      else if (TARGET_ATOMIC_SOFT_IMASK)
	  emit_insn (gen_atomic_test_and_set_soft_imask (addr, val));
      else
	FAIL;
    }

  /* The result of the test op is the inverse of what we are
     supposed to return.  Thus invert the T bit.  The inversion will be
     potentially optimized away and integrated into surrounding code.  */
  emit_insn (gen_movnegt (operands[0], get_t_reg_rtx ()));
  DONE;
})

(define_insn "tasb"
  [(set (reg:SI T_REG)
	(eq:SI (mem:QI (match_operand:SI 0 "register_operand" "r"))
	       (const_int 0)))
   (set (mem:QI (match_dup 0))
	(unspec:QI [(const_int 128)] UNSPEC_ATOMIC))]
  "TARGET_ENABLE_TAS"
  "tas.b	@%0"
  [(set_attr "insn_class" "co_group")])

(define_insn "atomic_test_and_set_soft_gusa"
  [(set (reg:SI T_REG)
	(eq:SI (mem:QI (match_operand:SI 0 "register_operand" "u"))
	       (const_int 0)))
   (set (mem:QI (match_dup 0))
	(unspec:QI [(match_operand:QI 1 "register_operand" "u")] UNSPEC_ATOMIC))
   (clobber (match_scratch:QI 2 "=&u"))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))]
  "TARGET_ATOMIC_SOFT_GUSA && !TARGET_ENABLE_TAS"
{
  return "\r	mova	1f,r0"		"\n"
	 "	.align 2"		"\n"
	 "	mov	r15,r1"		"\n"
	 "	mov	#(0f-1f),r15"	"\n"
	 "0:	mov.b	@%0,%2"		"\n"
	 "	mov.b	%1,@%0"		"\n"
	 "1:	mov	r1,r15"		"\n"
	 "	tst	%2,%2";
}
  [(set_attr "length" "16")])

(define_insn "atomic_test_and_set_soft_tcb"
  [(set (reg:SI T_REG)
	(eq:SI (mem:QI (match_operand:SI 0 "register_operand" "r"))
	       (const_int 0)))
   (set (mem:QI (match_dup 0))
	(unspec:QI [(match_operand:QI 1 "register_operand" "r")] UNSPEC_ATOMIC))
   (use (match_operand:SI 2 "gbr_displacement"))
   (clobber (match_scratch:QI 3 "=&r"))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))]
  "TARGET_ATOMIC_SOFT_TCB && !TARGET_ENABLE_TAS"
{
  return "\r	mova	1f,r0"		"\n"
	 "	mov	#(0f-1f),r1"	"\n"
	 "	.align 2"		"\n"
	 "	mov.l	r0,@(%O2,gbr)"	"\n"
	 "0:	mov.b	@%0,%3"		"\n"
	 "	mov	#0,r0"		"\n"
	 "	mov.b	%1,@%0"		"\n"
	 "1:	mov.l	r0,@(%O2,gbr)"	"\n"
	 "	tst	%3,%3";
}
  [(set_attr "length" "18")])

(define_insn "atomic_test_and_set_soft_imask"
  [(set (reg:SI T_REG)
	(eq:SI (mem:QI (match_operand:SI 0 "register_operand" "r"))
	       (const_int 0)))
   (set (mem:QI (match_dup 0))
	(unspec:QI [(match_operand:QI 1 "register_operand" "r")] UNSPEC_ATOMIC))
   (clobber (match_scratch:SI 2 "=&r"))
   (clobber (reg:SI R0_REG))]
  "TARGET_ATOMIC_SOFT_IMASK && !TARGET_ENABLE_TAS"
{
  return "\r	stc	sr,r0"		"\n"
	 "	mov	r0,%2"		"\n"
	 "	or	#0xF0,r0"	"\n"
	 "	ldc	r0,sr"		"\n"
	 "	mov.b	@%0,r0"		"\n"
	 "	mov.b	%1,@%0"		"\n"
	 "	ldc	%2,sr"		"\n"
	 "	tst	r0,r0";
}
  [(set_attr "length" "16")])

(define_insn "atomic_test_and_set_hard"
  [(set (reg:SI T_REG)
	(eq:SI (mem:QI (match_operand:SI 0 "register_operand" "r"))
	       (const_int 0)))
   (set (mem:QI (match_dup 0))
	(unspec:QI [(match_operand:QI 1 "register_operand" "r")] UNSPEC_ATOMIC))
   (clobber (reg:SI R0_REG))
   (clobber (match_scratch:SI 2 "=&r"))
   (clobber (match_scratch:SI 3 "=&r"))
   (clobber (match_scratch:SI 4 "=0"))]
  "TARGET_ATOMIC_HARD_LLCS && !TARGET_ENABLE_TAS"
{
  return "\r	mov	#-4,%2"		"\n"
	 "	and	%0,%2"		"\n"
	 "	xor	%2,%0"		"\n"
	 "	add	r15,%0"		"\n"
	 "	add	#-4,%0"		"\n"
	 "0:	movli.l	@%2,r0"		"\n"
	 "	mov.l	r0,@-r15"	"\n"
	 "	mov.b	@%0,%3"		"\n"
	 "	mov.b	%1,@%0"		"\n"
	 "	mov.l	@r15+,r0"	"\n"
	 "	movco.l	r0,@%2"		"\n"
	 "	bf	0b"		"\n"
	 "	tst	%3,%3";
}
  [(set_attr "length" "26")])

