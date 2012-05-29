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
;; On SH CPUs atomic integer operations can be done either in 'software' or
;; in 'hardware', where true hardware support was introduced with the SH4A.
;; In addition to that all SH CPUs support the 'tas.b' instruction, which
;; can be optionally used to implement the 'atomic_test_and_set' builtin.
;;
;; tas.b atomic_test_and_set (-menable-tas)
;;
;; Depending on the particular hardware configuration, usage of the 'tas.b'
;; instruction might be undesired or even unsafe.  Thus, it has to be
;; enabled by the user explicitely.  If it is not enabled, the
;; 'atomic_test_and_set' builtin is implemented either with hardware or with
;; software atomics, depending on which is enabled.  It is also possible to
;; enable the 'tas.b' instruction only, without enabling support for the 
;; other atomic operations.
;;
;;
;; Hardware Atomics (-mhard-atomic, SH4A only)
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
;; Software Atomics (-msoft-atomic)
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
;; The current atomic support is limited to QImode, HImode and SImode 
;; atomic operations.  DImode operations could also be implemented but
;; would require some ABI modifications to support multiple-instruction
;; write-back.  This is because SH1/SH2/SH3/SH4 does not have a DImode
;; store instruction.  DImode stores must be split into two SImode stores.
;;
;; On single-core SH4A CPUs software atomic aware interrupt / exception code
;; is actually compatible with user code that utilizes hardware atomics.
;; Since SImode hardware atomic sequences are more compact on SH4A they are
;; always used, regardless of the selected atomic mode.

(define_c_enum "unspec" [
  UNSPEC_ATOMIC
])
 
(define_c_enum "unspecv" [
  UNSPECV_CMPXCHG_1
  UNSPECV_CMPXCHG_2
  UNSPECV_CMPXCHG_3
])

(define_mode_iterator I124 [QI HI SI])
(define_mode_iterator I12 [QI HI])

(define_mode_attr i124suffix [(QI "b") (HI "w") (SI "l")])
(define_mode_attr i124extend_insn [(QI "exts.b") (HI "exts.w") (SI "mov")])

(define_code_iterator FETCHOP [plus minus ior xor and])
(define_code_attr fetchop_name
  [(plus "add") (minus "sub") (ior "or") (xor "xor") (and "and")])

(define_code_attr fetchop_predicate
  [(plus "atomic_arith_operand") (minus "register_operand")
   (ior "atomic_logical_operand") (xor "atomic_logical_operand")
   (and "atomic_logical_operand")])

(define_code_attr fetchop_constraint
  [(plus "rI08") (minus "r") (ior "rK08") (xor "rK08") (and "rK08")])

;;------------------------------------------------------------------------------
;; comapre and swap

(define_expand "atomic_compare_and_swap<mode>"
  [(match_operand:SI 0 "register_operand" "")		;; bool success output
   (match_operand:I124 1 "register_operand" "")		;; oldval output
   (match_operand:I124 2 "memory_operand" "")		;; memory
   (match_operand:I124 3 "atomic_arith_operand" "")	;; expected input
   (match_operand:I124 4 "atomic_arith_operand" "")	;; newval input
   (match_operand:SI 5 "const_int_operand" "")		;; is_weak
   (match_operand:SI 6 "const_int_operand" "")		;; success model
   (match_operand:SI 7 "const_int_operand" "")]		;; failure model
  "TARGET_ANY_ATOMIC && !TARGET_SHMEDIA"
{
  rtx addr = force_reg (Pmode, XEXP (operands[2], 0));
  rtx old_val = gen_lowpart (SImode, operands[1]);
  rtx exp_val = operands[3];
  rtx new_val = operands[4];
  rtx atomic_insn;

  if (TARGET_HARD_ATOMIC || (TARGET_SH4A_ARCH && <MODE>mode == SImode))
      atomic_insn = gen_atomic_compare_and_swap<mode>_hard (old_val, addr,
							    exp_val, new_val);
  else
      atomic_insn = gen_atomic_compare_and_swap<mode>_soft (old_val, addr,
							    exp_val, new_val);
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

(define_insn "atomic_compare_and_swapsi_hard"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(unspec_volatile:SI
	  [(mem:SI (match_operand:SI 1 "register_operand" "r"))
	   (match_operand:SI 2 "arith_operand" "rI08")
	   (match_operand:SI 3 "arith_operand" "rI08")]
	  UNSPECV_CMPXCHG_1))
   (set (mem:SI (match_dup 1))
	(unspec_volatile:SI [(const_int 0)] UNSPECV_CMPXCHG_2))
   (set (reg:SI T_REG)
	(unspec_volatile:SI [(const_int 0)] UNSPECV_CMPXCHG_3))
   (clobber (reg:SI R0_REG))]
  "TARGET_ANY_ATOMIC && TARGET_SH4A_ARCH"
{
  return "\r0:	movli.l	@%1,r0"		"\n"
	 "	cmp/eq	%2,r0"		"\n"
	 "	bf{.|/}s	0f"	"\n"
	 "	mov	r0,%0"		"\n"
	 "	mov	%3,r0"		"\n"
	 "	movco.l	r0,@%1"		"\n"
	 "	bf	0b"		"\n"
	 "0:";
}
  [(set_attr "length" "14")])

(define_insn "atomic_compare_and_swap<mode>_hard"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(unspec_volatile:SI
	  [(mem:I12 (match_operand:SI 1 "register_operand" "r"))
	   (match_operand:I12 2 "register_operand" "r")
	   (match_operand:I12 3 "register_operand" "r")]
	  UNSPECV_CMPXCHG_1))
   (set (mem:I12 (match_dup 1))
	(unspec_volatile:I12 [(const_int 0)] UNSPECV_CMPXCHG_2))
   (set (reg:SI T_REG)
	(unspec_volatile:SI [(const_int 0)] UNSPECV_CMPXCHG_3))
   (clobber (reg:SI R0_REG))
   (clobber (match_scratch:SI 4 "=&r"))
   (clobber (match_scratch:SI 5 "=&r"))
   (clobber (match_scratch:SI 6 "=1"))]
  "TARGET_HARD_ATOMIC && TARGET_SH4A_ARCH"
{
  return "\r	mov	#-4,%5"			"\n"
	 "	<i124extend_insn>	%2,%4"	"\n"
	 "	and	%1,%5"			"\n"
	 "	xor	%5,%1"			"\n"
	 "	add	r15,%1"			"\n"
	 "	add	#-4,%1"			"\n"
	 "0:	movli.l	@%5,r0"			"\n"
	 "	mov.l	r0,@-r15"		"\n"
	 "	mov.<i124suffix>	@%1,%0"	"\n"
	 "	mov.<i124suffix>	%3,@%1" "\n"
	 "	cmp/eq	%4,%0"			"\n"
	 "	bf{.|/}s	0f"		"\n"
	 "	mov.l	@r15+,r0"		"\n"
	 "	movco.l	r0,@%5"			"\n"
	 "	bf	0b"			"\n"
	 "0:";
}
  [(set_attr "length" "30")])

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
  return "\r	mova	1f,r0"			"\n"
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

;;------------------------------------------------------------------------------
;; read - write - return old value

(define_expand "atomic_exchange<mode>"
  [(match_operand:I124 0 "register_operand" "")		;; oldval output
   (match_operand:I124 1 "memory_operand" "")		;; memory
   (match_operand:I124 2 "atomic_arith_operand" "")	;; newval input
   (match_operand:SI 3 "const_int_operand" "")]		;; memory model
  "TARGET_ANY_ATOMIC && !TARGET_SHMEDIA"
{
  rtx addr = force_reg (Pmode, XEXP (operands[1], 0));
  rtx val = operands[2];
  rtx atomic_insn;

  if (TARGET_HARD_ATOMIC || (TARGET_SH4A_ARCH && <MODE>mode == SImode))
      atomic_insn = gen_atomic_exchange<mode>_hard (operands[0], addr, val);
  else
      atomic_insn = gen_atomic_exchange<mode>_soft (operands[0], addr, val);

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
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(mem:SI (match_operand:SI 1 "register_operand" "r")))
   (set (mem:SI (match_dup 1))
	(unspec:SI
	  [(match_operand:SI 2 "arith_operand" "rI08")] UNSPEC_ATOMIC))
   (clobber (reg:SI R0_REG))]
  "TARGET_ANY_ATOMIC && TARGET_SH4A_ARCH"
{
  return "\r0:	movli.l	@%1,r0"		"\n"
	 "	mov	r0,%0"		"\n"
	 "	mov	%2,r0"		"\n"
	 "	movco.l r0,@%1"		"\n"
	 "	bf	0b";
}
  [(set_attr "length" "10")])

(define_insn "atomic_exchange<mode>_hard"
  [(set (match_operand:I12 0 "register_operand" "=&r")
	(mem:I12 (match_operand:SI 1 "register_operand" "r")))
   (set (mem:I12 (match_dup 1))
	(unspec:I12
	  [(match_operand:I12 2 "register_operand" "r")] UNSPEC_ATOMIC))
   (clobber (reg:SI R0_REG))
   (clobber (match_scratch:SI 3 "=&r"))
   (clobber (match_scratch:SI 4 "=1"))]
  "TARGET_HARD_ATOMIC && TARGET_SH4A_ARCH"
{
  return "\r	mov	#-4,%3"			"\n"
	 "	and	%1,%3"			"\n"
	 "	xor	%3,%1"			"\n"
	 "	add	r15,%1"			"\n"
	 "	add	#-4,%1"			"\n"
	 "0:	movli.l	@%3,r0"			"\n"
	 "	mov.l	r0,@-r15"		"\n"
	 "	mov.<i124suffix>	@%1,%0"	"\n"
	 "	mov.<i124suffix>	%2,@%1" "\n"
	 "	mov.l	@r15+,r0"		"\n"
	 "	movco.l	r0,@%3"			"\n"
	 "	bf	0b";
}
  [(set_attr "length" "24")])

(define_insn "atomic_exchange<mode>_soft"
  [(set (match_operand:I124 0 "register_operand" "=&u")
	(mem:I124 (match_operand:SI 1 "register_operand" "u")))
   (set (mem:I124 (match_dup 1))
	(unspec:I124
	  [(match_operand:I124 2 "register_operand" "u")] UNSPEC_ATOMIC))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))]
  "TARGET_SOFT_ATOMIC && !TARGET_SHMEDIA"
{
  return "\r	mova	1f,r0"			"\n"
	 "	.align 2"			"\n"
	 "	mov	r15,r1"			"\n"
	 "	mov	#(0f-1f),r15"		"\n"
	 "0:	mov.<i124suffix>	@%1,%0"	"\n"
	 "	mov.<i124suffix>	%2,@%1"	"\n"
	 "1:	mov	r1,r15";
}
  [(set_attr "length" "14")])

;;------------------------------------------------------------------------------
;; read - add|sub|or|and|xor|nand - write - return old value

(define_expand "atomic_fetch_<fetchop_name><mode>"
  [(set (match_operand:I124 0 "register_operand" "")
	(match_operand:I124 1 "memory_operand" ""))
   (set (match_dup 1)
	(unspec:I124
	  [(FETCHOP:I124 (match_dup 1)
	     (match_operand:I124 2 "<fetchop_predicate>" ""))]
	  UNSPEC_ATOMIC))
   (match_operand:SI 3 "const_int_operand" "")]
  "TARGET_ANY_ATOMIC && !TARGET_SHMEDIA"
{
  rtx addr = force_reg (Pmode, XEXP (operands[1], 0));
  rtx atomic_insn;

  if (TARGET_HARD_ATOMIC || (TARGET_SH4A_ARCH && <MODE>mode == SImode))
    atomic_insn = gen_atomic_fetch_<fetchop_name><mode>_hard (operands[0], addr,
							      operands[2]);
  else
      atomic_insn = gen_atomic_fetch_<fetchop_name><mode>_soft (operands[0],
								addr,
								operands[2]);
  emit_insn (atomic_insn);

  if (<MODE>mode == QImode)
    emit_insn (gen_zero_extendqisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  else if (<MODE>mode == HImode)
    emit_insn (gen_zero_extendhisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  DONE;
})

(define_insn "atomic_fetch_<fetchop_name>si_hard"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(mem:SI (match_operand:SI 1 "register_operand" "r")))
   (set (mem:SI (match_dup 1))
	(unspec:SI
	  [(FETCHOP:SI (mem:SI (match_dup 1))
	     (match_operand:SI 2 "<fetchop_predicate>" "<fetchop_constraint>"))]
	  UNSPEC_ATOMIC))
   (clobber (reg:SI R0_REG))]
  "TARGET_ANY_ATOMIC && TARGET_SH4A_ARCH"
{
  return "\r0:	movli.l	@%1,r0"		"\n"
	 "	mov	r0,%0"		"\n"
	 "	<fetchop_name>	%2,r0"	"\n"
	 "	movco.l	r0,@%1"		"\n"
	 "	bf	0b";
}
  [(set_attr "length" "10")])

(define_insn "atomic_fetch_<fetchop_name><mode>_hard"
  [(set (match_operand:I12 0 "register_operand" "=&r")
	(mem:I12 (match_operand:SI 1 "register_operand" "r")))
   (set (mem:I12 (match_dup 1))
	(unspec:I12
	  [(FETCHOP:I12 (mem:I12 (match_dup 1))
	     (match_operand:I12 2 "<fetchop_predicate>" "<fetchop_constraint>"))]
	  UNSPEC_ATOMIC))
   (clobber (reg:SI R0_REG))
   (clobber (match_scratch:SI 3 "=&r"))
   (clobber (match_scratch:SI 4 "=1"))]
  "TARGET_HARD_ATOMIC && TARGET_SH4A_ARCH"
{
  return "\r	mov	#-4,%3"			"\n"
	 "	and	%1,%3"			"\n"
	 "	xor	%3,%1"			"\n"
	 "	add	r15,%1"			"\n"
	 "	add	#-4,%1"			"\n"
	 "0:	movli.l	@%3,r0"			"\n"
	 "	mov.l	r0,@-r15"		"\n"
	 "	mov.<i124suffix>	@%1,r0"	"\n"
	 "	mov	r0,%0"			"\n"
	 "	<fetchop_name>	%2,r0"		"\n"
	 "	mov.<i124suffix>	r0,@%1"	"\n"
	 "	mov.l	@r15+,r0"		"\n"
	 "	movco.l	r0,@%3"			"\n"
	 "	bf	0b";
}
  [(set_attr "length" "28")])

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
  return "\r	mova	1f,r0"			"\n"
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
		     (match_operand:I124 2 "atomic_logical_operand" "")))]
	  UNSPEC_ATOMIC))
   (match_operand:SI 3 "const_int_operand" "")]
  "TARGET_ANY_ATOMIC && !TARGET_SHMEDIA"
{
  rtx addr = force_reg (Pmode, XEXP (operands[1], 0));
  rtx atomic_insn;

  if (TARGET_HARD_ATOMIC || (TARGET_SH4A_ARCH && <MODE>mode == SImode))
    atomic_insn = gen_atomic_fetch_nand<mode>_hard (operands[0], addr,
						    operands[2]);
  else
    atomic_insn = gen_atomic_fetch_nand<mode>_soft (operands[0], addr,
						    operands[2]);

  emit_insn (atomic_insn);

  if (<MODE>mode == QImode)
    emit_insn (gen_zero_extendqisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  else if (<MODE>mode == HImode)
    emit_insn (gen_zero_extendhisi2 (gen_lowpart (SImode, operands[0]),
				     operands[0]));
  DONE;
})

(define_insn "atomic_fetch_nandsi_hard"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(mem:SI (match_operand:SI 1 "register_operand" "r")))
   (set (mem:SI (match_dup 1))
	(unspec:SI
	  [(not:SI (and:SI (mem:SI (match_dup 1))
		   (match_operand:SI 2 "logical_operand" "rK08")))]
	  UNSPEC_ATOMIC))
   (clobber (reg:SI R0_REG))]
  "TARGET_ANY_ATOMIC && TARGET_SH4A_ARCH"
{
  return "\r0:	movli.l	@%1,r0"		"\n"
	 "	mov	r0,%0"		"\n"
	 "	and	%2,r0"		"\n"
	 "	not	r0,r0"		"\n"
	 "	movco.l	r0,@%1"		"\n"
	 "	bf	0b";
}
  [(set_attr "length" "12")])

(define_insn "atomic_fetch_nand<mode>_hard"
  [(set (match_operand:I12 0 "register_operand" "=&r")
	(mem:I12 (match_operand:SI 1 "register_operand" "r")))
   (set (mem:I12 (match_dup 1))
	(unspec:I12
	  [(not:I12 (and:I12 (mem:I12 (match_dup 1))
		    (match_operand:I12 2 "logical_operand" "rK08")))]
	  UNSPEC_ATOMIC))
   (clobber (reg:SI R0_REG))
   (clobber (match_scratch:SI 3 "=&r"))
   (clobber (match_scratch:SI 4 "=1"))]
  "TARGET_HARD_ATOMIC && TARGET_SH4A_ARCH"
{
  return "\r	mov	#-4,%3"			"\n"
	 "	and	%1,%3"			"\n"
	 "	xor	%3,%1"			"\n"
	 "	add	r15,%1"			"\n"
	 "	add	#-4,%1"			"\n"
	 "0:	movli.l	@%3,r0"			"\n"
	 "	mov.l	r0,@-r15"		"\n"
	 "	mov.<i124suffix>	@%1,r0"	"\n"
	 "	mov	r0,%0"			"\n"
	 "	and	%2,r0"			"\n"
	 "	not	r0,r0"			"\n"
	 "	mov.<i124suffix>	r0,@%1"	"\n"
	 "	mov.l	@r15+,r0"		"\n"
	 "	movco.l	r0,@%3"			"\n"
	 "	bf	0b";
}
  [(set_attr "length" "30")])

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
  return "\r	mova	1f,r0"			"\n"
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

;;------------------------------------------------------------------------------
;; read - add|sub|or|and|xor|nand - write - return new value

(define_expand "atomic_<fetchop_name>_fetch<mode>"
  [(set (match_operand:I124 0 "register_operand" "")
	(FETCHOP:I124
	  (match_operand:I124 1 "memory_operand" "")
	  (match_operand:I124 2 "<fetchop_predicate>" "")))
   (set (match_dup 1)
	(unspec:I124
	  [(FETCHOP:I124 (match_dup 1) (match_dup 2))]
	  UNSPEC_ATOMIC))
   (match_operand:SI 3 "const_int_operand" "")]
  "TARGET_ANY_ATOMIC && !TARGET_SHMEDIA"
{
  rtx addr = force_reg (Pmode, XEXP (operands[1], 0));
  rtx atomic_insn;

  if (TARGET_HARD_ATOMIC || (TARGET_SH4A_ARCH && <MODE>mode == SImode))
    atomic_insn = gen_atomic_<fetchop_name>_fetch<mode>_hard (operands[0], addr,
							      operands[2]);
  else
    atomic_insn = gen_atomic_<fetchop_name>_fetch<mode>_soft (operands[0], addr,
							      operands[2]);
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
  [(set (match_operand:SI 0 "register_operand" "=&z")
	(FETCHOP:SI
	  (mem:SI (match_operand:SI 1 "register_operand" "r"))
	  (match_operand:SI 2 "<fetchop_predicate>" "<fetchop_constraint>")))
   (set (mem:SI (match_dup 1))
	(unspec:SI
	  [(FETCHOP:SI (mem:SI (match_dup 1)) (match_dup 2))]
	  UNSPEC_ATOMIC))]
  "TARGET_ANY_ATOMIC && TARGET_SH4A_ARCH"
{
  return "\r0:	movli.l	@%1,%0"		"\n"
	 "	<fetchop_name>	%2,%0"	"\n"
	 "	movco.l	%0,@%1"		"\n"
	 "	bf	0b";
}
  [(set_attr "length" "8")])

(define_insn "atomic_<fetchop_name>_fetch<mode>_hard"
  [(set (match_operand:I12 0 "register_operand" "=&r")
	(FETCHOP:I12
	  (mem:I12 (match_operand:SI 1 "register_operand" "r"))
	  (match_operand:I12 2 "<fetchop_predicate>" "<fetchop_constraint>")))
   (set (mem:I12 (match_dup 1))
	(unspec:I12
	  [(FETCHOP:I12 (mem:I12 (match_dup 1)) (match_dup 2))]
	  UNSPEC_ATOMIC))

   (clobber (reg:SI R0_REG))
   (clobber (match_scratch:SI 3 "=&r"))
   (clobber (match_scratch:SI 4 "=1"))]
  "TARGET_HARD_ATOMIC && TARGET_SH4A_ARCH"
{
  return "\r	mov	#-4,%3"			"\n"
	 "	and	%1,%3"			"\n"
	 "	xor	%3,%1"			"\n"
	 "	add	r15,%1"			"\n"
	 "	add	#-4,%1"			"\n"
	 "0:	movli.l	@%3,r0"			"\n"
	 "	mov.l	r0,@-r15"		"\n"
	 "	mov.<i124suffix>	@%1,r0"	"\n"
	 "	<fetchop_name>	%2,r0"		"\n"
	 "	mov.<i124suffix>	r0,@%1"	"\n"
	 "	mov	r0,%0"			"\n"
	 "	mov.l	@r15+,r0"		"\n"
	 "	movco.l	r0,@%3"			"\n"
	 "	bf	0b";
}
  [(set_attr "length" "28")])

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
  return "\r	mova	1f,r0"			"\n"
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
	  (match_operand:I124 2 "atomic_logical_operand" ""))))
   (set (match_dup 1)
	(unspec:I124
	  [(not:I124 (and:I124 (match_dup 1) (match_dup 2)))]
	  UNSPEC_ATOMIC))
   (match_operand:SI 3 "const_int_operand" "")]
  "TARGET_ANY_ATOMIC && !TARGET_SHMEDIA"
{
  rtx addr = force_reg (Pmode, XEXP (operands[1], 0));
  rtx atomic_insn;

  if (TARGET_HARD_ATOMIC || (TARGET_SH4A_ARCH && <MODE>mode == SImode))
    atomic_insn = gen_atomic_nand_fetch<mode>_hard (operands[0], addr,
						    operands[2]);
  else
    atomic_insn = gen_atomic_nand_fetch<mode>_soft (operands[0], addr,
						    operands[2]);
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
  [(set (match_operand:SI 0 "register_operand" "=&z")
	(not:SI (and:SI (mem:SI (match_operand:SI 1 "register_operand" "r"))
			(match_operand:SI 2 "logical_operand" "rK08"))))
   (set (mem:SI (match_dup 1))
	(unspec:SI
	  [(not:SI (and:SI (mem:SI (match_dup 1)) (match_dup 2)))]
	  UNSPEC_ATOMIC))]
  "TARGET_ANY_ATOMIC && TARGET_SH4A_ARCH"
{
  return "\r0:	movli.l	@%1,%0"		"\n"
	 "	and	%2,%0"		"\n"
	 "	not	%0,%0"		"\n"
	 "	movco.l	%0,@%1"		"\n"
	 "	bf	0b";
}
  [(set_attr "length" "10")])

(define_insn "atomic_nand_fetch<mode>_hard"
  [(set (match_operand:I12 0 "register_operand" "=&r")
	(not:I12 (and:I12 (mem:I12 (match_operand:SI 1 "register_operand" "r"))
			  (match_operand:I12 2 "logical_operand" "rK08"))))
   (set (mem:I12 (match_dup 1))
	(unspec:I12
	  [(not:I12 (and:I12 (mem:I12 (match_dup 1)) (match_dup 2)))]
	  UNSPEC_ATOMIC))
   (clobber (reg:SI R0_REG))
   (clobber (match_scratch:SI 3 "=&r"))
   (clobber (match_scratch:SI 4 "=1"))]
  "TARGET_HARD_ATOMIC && TARGET_SH4A_ARCH"
{
  return "\r	mov	#-4,%3"			"\n"
	 "	and	%1,%3"			"\n"
	 "	xor	%3,%1"			"\n"
	 "	add	r15,%1"			"\n"
	 "	add	#-4,%1"			"\n"
	 "0:	movli.l	@%3,r0"			"\n"
	 "	mov.l	r0,@-r15"		"\n"
	 "	mov.<i124suffix>	@%1,r0"	"\n"
	 "	and	%2,r0"			"\n"
	 "	not	r0,%0"			"\n"
	 "	mov.<i124suffix>	%0,@%1"	"\n"
	 "	mov.l	@r15+,r0"		"\n"
	 "	movco.l	r0,@%3"			"\n"
	 "	bf	0b";
}
  [(set_attr "length" "28")])

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
  return "\r	mova	1f,r0"			"\n"
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

;;------------------------------------------------------------------------------
;; read - test against zero - or with 0x80 - write - return test result

(define_expand "atomic_test_and_set"
  [(match_operand:SI 0 "register_operand" "")		;; bool result output
   (match_operand:QI 1 "memory_operand" "")		;; memory
   (match_operand:SI 2 "const_int_operand" "")]		;; model
  "(TARGET_ANY_ATOMIC || TARGET_ENABLE_TAS) && !TARGET_SHMEDIA"
{
  rtx addr = force_reg (Pmode, XEXP (operands[1], 0));

  if (TARGET_ENABLE_TAS)
    emit_insn (gen_tasb (addr));
  else
    {
      rtx val = gen_int_mode (targetm.atomic_test_and_set_trueval, QImode);
      val = force_reg (QImode, val);

      if (TARGET_HARD_ATOMIC)
	  emit_insn (gen_atomic_test_and_set_hard (addr, val));
      else
	  emit_insn (gen_atomic_test_and_set_soft (addr, val));
    }

  /* The result of the test op is the inverse of what we are
     supposed to return.  Thus invert the T bit.  The inversion will be
     potentially optimized away and integrated into surrounding code.  */
  emit_insn (gen_movnegt (operands[0]));
  DONE;
})

(define_insn "tasb"
  [(set (reg:SI T_REG)
	(eq:SI (mem:QI (match_operand:SI 0 "register_operand" "r"))
	       (const_int 0)))
   (set (mem:QI (match_dup 0))
	(unspec:QI [(const_int 128)] UNSPEC_ATOMIC))]
  "TARGET_ENABLE_TAS && !TARGET_SHMEDIA"
  "tas.b	@%0"
  [(set_attr "insn_class" "co_group")])

(define_insn "atomic_test_and_set_soft"
  [(set (reg:SI T_REG)
	(eq:SI (mem:QI (match_operand:SI 0 "register_operand" "u"))
	       (const_int 0)))
   (set (mem:QI (match_dup 0))
	(unspec:QI [(match_operand:QI 1 "register_operand" "u")] UNSPEC_ATOMIC))
   (clobber (match_scratch:QI 2 "=&u"))
   (clobber (reg:SI R0_REG))
   (clobber (reg:SI R1_REG))]
  "TARGET_SOFT_ATOMIC && !TARGET_ENABLE_TAS && !TARGET_SHMEDIA"
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
  "TARGET_HARD_ATOMIC && !TARGET_ENABLE_TAS && TARGET_SH4A_ARCH"
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

