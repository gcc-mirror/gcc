;; GCC machine description for SH synchronization instructions.
;; Copyright (C) 2011, 2012
;; Free Software Foundation, Inc.
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
;; On single-core systems there can only be one execution context running
;; at a given point in time.  This allows the usage of rewindable atomic
;; sequences, which effectively emulate locked-load / conditional-store
;; operations.
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
;; Linux kernel for SH3/SH4 implements support for such software
;; atomic sequences.  However, it can also be implemented in freestanding
;; environments.
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
;; The current implementation is limited to QImode, HImode and SImode 
;; atomic operations.  DImode operations could also be implemented but
;; would require some ABI modifications to support multiple-instruction
;; write-back.  This is because SH1/SH2/SH3/SH4 does not have a DImode
;; store instruction.  DImode stores must be split into two SImode stores.
;;
;; For some operations it would be possible to use insns with an immediate
;; operand such as add #imm,Rn.  However, since the original value before
;; the operation also needs to be available, this is not so handy.

(define_c_enum "unspec" [
  UNSPEC_ATOMIC
])
 
(define_c_enum "unspecv" [
  UNSPECV_CMPXCHG_1
  UNSPECV_CMPXCHG_2
  UNSPECV_CMPXCHG_3
])

(define_mode_iterator I124 [QI HI SI])

(define_mode_attr i124suffix [(QI "b") (HI "w") (SI "l")])
(define_mode_attr i124extend_insn [(QI "exts.b") (HI "exts.w") (SI "mov")])

(define_code_iterator FETCHOP [plus minus ior xor and])
(define_code_attr fetchop_name
  [(plus "add") (minus "sub") (ior "or") (xor "xor") (and "and")])

(define_expand "atomic_compare_and_swap<mode>"
  [(match_operand:SI 0 "register_operand" "")		;; bool success output
   (match_operand:I124 1 "register_operand" "")		;; oldval output
   (match_operand:I124 2 "memory_operand" "")		;; memory
   (match_operand:I124 3 "register_operand" "")		;; expected input
   (match_operand:I124 4 "register_operand" "")		;; newval input
   (match_operand:SI 5 "const_int_operand" "")		;; is_weak
   (match_operand:SI 6 "const_int_operand" "")		;; success model
   (match_operand:SI 7 "const_int_operand" "")]		;; failure model
  "TARGET_SOFT_ATOMIC && !TARGET_SHMEDIA"
{
  rtx addr;

  addr = force_reg (Pmode, XEXP (operands[2], 0));
  emit_insn (gen_atomic_compare_and_swap<mode>_soft
	     (gen_lowpart (SImode, operands[1]), addr, operands[3],
	      operands[4]));
  if (<MODE>mode == QImode)
    emit_insn (gen_zero_extendqisi2 (gen_lowpart (SImode, operands[1]),
				     operands[1]));
  else if (<MODE>mode == HImode)
    emit_insn (gen_zero_extendhisi2 (gen_lowpart (SImode, operands[1]),
				     operands[1]));
  emit_insn (gen_movsi (operands[0], gen_rtx_REG (SImode, T_REG)));
  DONE;
})

(define_insn "atomic_compare_and_swap<mode>_soft"
  [(set (match_operand:SI 0 "register_operand" "=&u")
	(unspec_volatile:SI
	  [(mem:I124 (match_operand:SI 1 "register_operand" "u"))
	   (match_operand:I124 2 "register_operand" "u")
	   (match_operand:I124 3 "register_operand" "u")]
	  UNSPECV_CMPXCHG_1))
   (set (mem:I124 (match_dup 1))
	(unspec_volatile:I124 [(const_int 0)] UNSPECV_CMPXCHG_2))
   (set (reg:SI T_REG)
	(unspec_volatile:SI [(const_int 0)] UNSPECV_CMPXCHG_3))
   (clobber (match_scratch:SI 4 "=&u"))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))]
  "TARGET_SOFT_ATOMIC && !TARGET_SHMEDIA"
{
  return "mova	1f,r0"				"\n"
	 "	<i124extend_insn>	%2,%4"	"\n"
	 "	.align 2"			"\n"
	 "	mov	r15,r1"			"\n"
	 "	mov	#(0f-1f),r15"		"\n"
	 "0:	mov.<i124suffix>	@%1,%0"	"\n"
	 "	cmp/eq	%0,%4"			"\n"
	 "	bf	1f"			"\n"
	 "	mov.<i124suffix>	%3,@%1"	"\n"
	 "1:	mov	r1,r15";
}
  [(set_attr "length" "20")])

(define_expand "atomic_fetch_<fetchop_name><mode>"
  [(set (match_operand:I124 0 "register_operand" "")
	(match_operand:I124 1 "memory_operand" ""))
   (set (match_dup 1)
	(unspec:I124
	  [(FETCHOP:I124 (match_dup 1)
	     (match_operand:I124 2 "register_operand" ""))]
	  UNSPEC_ATOMIC))
   (match_operand:SI 3 "const_int_operand" "")]
  "TARGET_SOFT_ATOMIC && !TARGET_SHMEDIA"
{
  rtx addr;

  addr = force_reg (Pmode, XEXP (operands[1], 0));
  emit_insn (gen_atomic_fetch_<fetchop_name><mode>_soft
	     (operands[0], addr, operands[2]));
  if (<MODE>mode == QImode)
    emit_insn (gen_zero_extendqisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  else if (<MODE>mode == HImode)
    emit_insn (gen_zero_extendhisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  DONE;
})

(define_insn "atomic_fetch_<fetchop_name><mode>_soft"
  [(set (match_operand:I124 0 "register_operand" "=&u")
	(mem:I124 (match_operand:SI 1 "register_operand" "u")))
   (set (mem:I124 (match_dup 1))
	(unspec:I124
	  [(FETCHOP:I124 (mem:I124 (match_dup 1))
	     (match_operand:I124 2 "register_operand" "u"))]
	  UNSPEC_ATOMIC))
   (clobber (match_scratch:I124 3 "=&u"))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))]
  "TARGET_SOFT_ATOMIC && !TARGET_SHMEDIA"
{
  return "mova	1f,r0"				"\n"
	 "	.align 2"			"\n"
	 "	mov	r15,r1"			"\n"
	 "	mov	#(0f-1f),r15"		"\n"
	 "0:	mov.<i124suffix>	@%1,%0"	"\n"
	 "	mov	%0,%3"			"\n"
	 "	<fetchop_name>	%2,%3"		"\n"
	 "	mov.<i124suffix>	%3,@%1"	"\n"
	 "1:	mov	r1,r15";
}
  [(set_attr "length" "18")])

(define_expand "atomic_fetch_nand<mode>"
  [(set (match_operand:I124 0 "register_operand" "")
	(match_operand:I124 1 "memory_operand" ""))
   (set (match_dup 1)
	(unspec:I124
	  [(not:I124 (and:I124 (match_dup 1)
	     (match_operand:I124 2 "register_operand" "")))]
	  UNSPEC_ATOMIC))
   (match_operand:SI 3 "const_int_operand" "")]
  "TARGET_SOFT_ATOMIC && !TARGET_SHMEDIA"
{
  rtx addr;

  addr = force_reg (Pmode, XEXP (operands[1], 0));
  emit_insn (gen_atomic_fetch_nand<mode>_soft
	     (operands[0], addr, operands[2]));
  if (<MODE>mode == QImode)
    emit_insn (gen_zero_extendqisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  else if (<MODE>mode == HImode)
    emit_insn (gen_zero_extendhisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  DONE;
})

(define_insn "atomic_fetch_nand<mode>_soft"
  [(set (match_operand:I124 0 "register_operand" "=&u")
	(mem:I124 (match_operand:SI 1 "register_operand" "u")))
   (set (mem:I124 (match_dup 1))
	(unspec:I124
	  [(not:I124 (and:I124 (mem:I124 (match_dup 1))
	     (match_operand:I124 2 "register_operand" "u")))]
	  UNSPEC_ATOMIC))
   (clobber (match_scratch:I124 3 "=&u"))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))]
  "TARGET_SOFT_ATOMIC && !TARGET_SHMEDIA"
{
  return "mova	1f,r0"				"\n"
	 "	mov	r15,r1"			"\n"
	 "	.align 2"			"\n"
	 "	mov	#(0f-1f),r15"		"\n"
	 "0:	mov.<i124suffix>	@%1,%0"	"\n"
	 "	mov	%2,%3"			"\n"
	 "	and	%0,%3"			"\n"
	 "	not	%3,%3"			"\n"
	 "	mov.<i124suffix>	%3,@%1"	"\n"
	 "1:	mov	r1,r15";
}
  [(set_attr "length" "20")])

(define_expand "atomic_<fetchop_name>_fetch<mode>"
  [(set (match_operand:I124 0 "register_operand" "")
	(FETCHOP:I124
	  (match_operand:I124 1 "memory_operand" "")
	  (match_operand:I124 2 "register_operand" "")))
   (set (match_dup 1)
	(unspec:I124
	  [(FETCHOP:I124 (match_dup 1) (match_dup 2))]
	  UNSPEC_ATOMIC))
   (match_operand:SI 3 "const_int_operand" "")]
  "TARGET_SOFT_ATOMIC && !TARGET_SHMEDIA"
{
  rtx addr;

  addr = force_reg (Pmode, XEXP (operands[1], 0));
  emit_insn (gen_atomic_<fetchop_name>_fetch<mode>_soft
	     (operands[0], addr, operands[2]));
  if (<MODE>mode == QImode)
    emit_insn (gen_zero_extendqisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  else if (<MODE>mode == HImode)
    emit_insn (gen_zero_extendhisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  DONE;
})

(define_insn "atomic_<fetchop_name>_fetch<mode>_soft"
  [(set (match_operand:I124 0 "register_operand" "=&u")
	(FETCHOP:I124
	  (mem:I124 (match_operand:SI 1 "register_operand" "u"))
	  (match_operand:I124 2 "register_operand" "u")))
   (set (mem:I124 (match_dup 1))
	(unspec:I124
	  [(FETCHOP:I124 (mem:I124 (match_dup 1)) (match_dup 2))]
	  UNSPEC_ATOMIC))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))]
  "TARGET_SOFT_ATOMIC && !TARGET_SHMEDIA"
{
  return "mova	1f,r0"				"\n"
	 "	mov	r15,r1"			"\n"
	 "	.align 2"			"\n"
	 "	mov	#(0f-1f),r15"		"\n"
	 "0:	mov.<i124suffix>	@%1,%0"	"\n"
	 "	<fetchop_name>	%2,%0"		"\n"
	 "	mov.<i124suffix>	%0,@%1"	"\n"
	 "1:	mov	r1,r15";
}
  [(set_attr "length" "16")])

(define_expand "atomic_nand_fetch<mode>"
  [(set (match_operand:I124 0 "register_operand" "")
	(not:I124 (and:I124
	  (match_operand:I124 1 "memory_operand" "")
	  (match_operand:I124 2 "register_operand" ""))))
   (set (match_dup 1)
	(unspec:I124
	  [(not:I124 (and:I124 (match_dup 1) (match_dup 2)))]
	  UNSPEC_ATOMIC))
   (match_operand:SI 3 "const_int_operand" "")]
  "TARGET_SOFT_ATOMIC && !TARGET_SHMEDIA"
{
  rtx addr;

  addr = force_reg (Pmode, XEXP (operands[1], 0));
  emit_insn (gen_atomic_nand_fetch<mode>_soft
	     (operands[0], addr, operands[2]));
  if (<MODE>mode == QImode)
    emit_insn (gen_zero_extendqisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  else if (<MODE>mode == HImode)
    emit_insn (gen_zero_extendhisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  DONE;
})

(define_insn "atomic_nand_fetch<mode>_soft"
  [(set (match_operand:I124 0 "register_operand" "=&u")
	(not:I124 (and:I124
	  (mem:I124 (match_operand:SI 1 "register_operand" "u"))
	  (match_operand:I124 2 "register_operand" "u"))))
   (set (mem:I124 (match_dup 1))
	(unspec:I124
	  [(not:I124 (and:I124 (mem:I124 (match_dup 1)) (match_dup 2)))]
	  UNSPEC_ATOMIC))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))]
  "TARGET_SOFT_ATOMIC && !TARGET_SHMEDIA"
{
  return "mova	1f,r0"				"\n"
	 "	.align 2"			"\n"
	 "	mov	r15,r1"			"\n"
	 "	mov	#(0f-1f),r15"		"\n"
	 "0:	mov.<i124suffix>	@%1,%0"	"\n"
	 "	and	%2,%0"			"\n"
	 "	not	%0,%0"			"\n"
	 "	mov.<i124suffix>	%0,@%1"	"\n"
	 "1:	mov	r1,r15";
}
  [(set_attr "length" "18")])
