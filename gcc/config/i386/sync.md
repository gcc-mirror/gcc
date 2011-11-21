;; GCC machine description for i386 synchronization instructions.
;; Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010, 2011
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

(define_c_enum "unspec" [
  UNSPEC_LFENCE
  UNSPEC_SFENCE
  UNSPEC_MFENCE
  UNSPEC_MOVA	; For __atomic support
])

(define_c_enum "unspecv" [
  UNSPECV_CMPXCHG_1
  UNSPECV_CMPXCHG_2
  UNSPECV_CMPXCHG_3
  UNSPECV_CMPXCHG_4
  UNSPECV_XCHG
  UNSPECV_LOCK
])

(define_expand "sse2_lfence"
  [(set (match_dup 0)
	(unspec:BLK [(match_dup 0)] UNSPEC_LFENCE))]
  "TARGET_SSE2"
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
})

(define_insn "*sse2_lfence"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0)] UNSPEC_LFENCE))]
  "TARGET_SSE2"
  "lfence"
  [(set_attr "type" "sse")
   (set_attr "length_address" "0")
   (set_attr "atom_sse_attr" "lfence")
   (set_attr "memory" "unknown")])

(define_expand "sse_sfence"
  [(set (match_dup 0)
	(unspec:BLK [(match_dup 0)] UNSPEC_SFENCE))]
  "TARGET_SSE || TARGET_3DNOW_A"
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
})

(define_insn "*sse_sfence"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0)] UNSPEC_SFENCE))]
  "TARGET_SSE || TARGET_3DNOW_A"
  "sfence"
  [(set_attr "type" "sse")
   (set_attr "length_address" "0")
   (set_attr "atom_sse_attr" "fence")
   (set_attr "memory" "unknown")])

(define_expand "sse2_mfence"
  [(set (match_dup 0)
	(unspec:BLK [(match_dup 0)] UNSPEC_MFENCE))]
  "TARGET_SSE2"
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
})

(define_insn "mfence_sse2"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0)] UNSPEC_MFENCE))]
  "TARGET_64BIT || TARGET_SSE2"
  "mfence"
  [(set_attr "type" "sse")
   (set_attr "length_address" "0")
   (set_attr "atom_sse_attr" "fence")
   (set_attr "memory" "unknown")])

(define_insn "mfence_nosse"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0)] UNSPEC_MFENCE))
   (clobber (reg:CC FLAGS_REG))]
  "!(TARGET_64BIT || TARGET_SSE2)"
  "lock{%;} or{l}\t{$0, (%%esp)|DWORD PTR [esp], 0}"
  [(set_attr "memory" "unknown")])

(define_expand "mem_thread_fence"
  [(match_operand:SI 0 "const_int_operand" "")]		;; model
  ""
{
  /* Unless this is a SEQ_CST fence, the i386 memory model is strong
     enough not to require barriers of any kind.  */
  if (INTVAL (operands[0]) == MEMMODEL_SEQ_CST)
    {
      rtx (*mfence_insn)(rtx);
      rtx mem;

      if (TARGET_64BIT || TARGET_SSE2)
	mfence_insn = gen_mfence_sse2;
      else
	mfence_insn = gen_mfence_nosse;

      mem = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
      MEM_VOLATILE_P (mem) = 1;

      emit_insn (mfence_insn (mem));
    }
  DONE;
})

;; ??? From volume 3 section 7.1.1 Guaranteed Atomic Operations,
;; Only beginning at Pentium family processors do we get any guarantee of
;; atomicity in aligned 64-bit quantities.  Beginning at P6, we get a
;; guarantee for 64-bit accesses that do not cross a cacheline boundary.
;;
;; Note that the TARGET_CMPXCHG8B test below is a stand-in for "Pentium".
;;
;; Importantly, *no* processor makes atomicity guarantees for larger
;; accesses.  In particular, there's no way to perform an atomic TImode
;; move, despite the apparent applicability of MOVDQA et al.

(define_mode_iterator ATOMIC
   [QI HI SI
    (DI "TARGET_64BIT || (TARGET_CMPXCHG8B && (TARGET_80387 || TARGET_SSE))")
   ])

(define_expand "atomic_load<mode>"
  [(set (match_operand:ATOMIC 0 "register_operand" "")
	(unspec:ATOMIC [(match_operand:ATOMIC 1 "memory_operand" "")
			(match_operand:SI 2 "const_int_operand" "")]
		       UNSPEC_MOVA))]
  ""
{
  /* For DImode on 32-bit, we can use the FPU to perform the load.  */
  if (<MODE>mode == DImode && !TARGET_64BIT)
    emit_insn (gen_atomic_loaddi_fpu
	       (operands[0], operands[1],
	        assign_386_stack_local (DImode,
					(virtuals_instantiated
					 ? SLOT_TEMP : SLOT_VIRTUAL))));
  else
    emit_move_insn (operands[0], operands[1]);
  DONE;
})

(define_insn_and_split "atomic_loaddi_fpu"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=x,m,?r")
	(unspec:DI [(match_operand:DI 1 "memory_operand" "m,m,m")]
		   UNSPEC_MOVA))
   (clobber (match_operand:DI 2 "memory_operand" "=X,X,m"))
   (clobber (match_scratch:DF 3 "=X,xf,xf"))]
  "!TARGET_64BIT && (TARGET_80387 || TARGET_SSE)"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rtx dst = operands[0], src = operands[1];
  rtx mem = operands[2], tmp = operands[3];

  if (SSE_REG_P (dst))
    emit_move_insn (dst, src);
  else
    {
      if (MEM_P (dst))
	mem = dst;

      if (FP_REG_P (tmp))
	emit_insn (gen_movdi_via_fpu (mem, src, tmp));
      else
	{
	  adjust_reg_mode (tmp, DImode);
	  emit_move_insn (tmp, src);
	  emit_move_insn (mem, tmp);
	}

      if (mem != dst)
	emit_move_insn (dst, mem);
    }
  DONE;
})

(define_expand "atomic_store<mode>"
  [(set (match_operand:ATOMIC 0 "memory_operand" "")
	(unspec:ATOMIC [(match_operand:ATOMIC 1 "register_operand" "")
			(match_operand:SI 2 "const_int_operand" "")]
		       UNSPEC_MOVA))]
  ""
{
  enum memmodel model = (enum memmodel) INTVAL (operands[2]);

  if (<MODE>mode == DImode && !TARGET_64BIT)
    {
      /* For DImode on 32-bit, we can use the FPU to perform the store.  */
      /* Note that while we could perform a cmpxchg8b loop, that turns
	 out to be significantly larger than this plus a barrier.  */
      emit_insn (gen_atomic_storedi_fpu
		 (operands[0], operands[1],
	          assign_386_stack_local (DImode,
					  (virtuals_instantiated
					   ? SLOT_TEMP : SLOT_VIRTUAL))));
    }
  else
    {
      /* For seq-cst stores, when we lack MFENCE, use XCHG.  */
      if (model == MEMMODEL_SEQ_CST && !(TARGET_64BIT || TARGET_SSE2))
	{
	  emit_insn (gen_atomic_exchange<mode> (gen_reg_rtx (<MODE>mode),
						operands[0], operands[1],
						operands[2]));
	  DONE;
	}

      /* Otherwise use a normal store.  */
      emit_move_insn (operands[0], operands[1]);
    }
  /* ... followed by an MFENCE, if required.  */
  if (model == MEMMODEL_SEQ_CST)
    emit_insn (gen_mem_thread_fence (operands[2]));
  DONE;
})

(define_insn_and_split "atomic_storedi_fpu"
  [(set (match_operand:DI 0 "memory_operand" "=m,m,m")
	(unspec:DI [(match_operand:DI 1 "register_operand" "x,m,?r")]
		   UNSPEC_MOVA))
   (clobber (match_operand:DI 2 "memory_operand" "=X,X,m"))
   (clobber (match_scratch:DF 3 "=X,xf,xf"))]
  "!TARGET_64BIT && (TARGET_80387 || TARGET_SSE)"
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  rtx dst = operands[0], src = operands[1];
  rtx mem = operands[2], tmp = operands[3];

  if (!SSE_REG_P (src))
    {
      if (REG_P (src))
	{
	  emit_move_insn (mem, src);
	  src = mem;
	}

      if (FP_REG_P (tmp))
	{
	  emit_insn (gen_movdi_via_fpu (dst, src, tmp));
	  DONE;
	}
      else
	{
	  adjust_reg_mode (tmp, DImode);
	  emit_move_insn (tmp, mem);
	  src = tmp;
	}
    }
  emit_move_insn (dst, src);
  DONE;
})

;; ??? You'd think that we'd be able to perform this via FLOAT + FIX_TRUNC
;; operations.  But the fix_trunc patterns want way more setup than we want
;; to provide.  Note that the scratch is DFmode instead of XFmode in order
;; to make it easy to allocate a scratch in either SSE or FP_REGs above.
(define_insn "movdi_via_fpu"
  [(set (match_operand:DI 0 "memory_operand" "=m")
	(unspec:DI [(match_operand:DI 1 "memory_operand" "m")] UNSPEC_MOVA))
   (clobber (match_operand:DF 2 "register_operand" "=f"))]
  "TARGET_80387"
  "fild\t%1\;fistp\t%0"
  [(set_attr "type" "multi")
   ;; Worst case based on full sib+offset32 addressing modes
   (set_attr "length" "14")])

(define_expand "atomic_compare_and_swap<mode>"
  [(match_operand:QI 0 "register_operand" "")		;; bool success output
   (match_operand:SWI124 1 "register_operand" "")	;; oldval output
   (match_operand:SWI124 2 "memory_operand" "")		;; memory
   (match_operand:SWI124 3 "register_operand" "")	;; expected input
   (match_operand:SWI124 4 "register_operand" "")	;; newval input
   (match_operand:SI 5 "const_int_operand" "")		;; is_weak
   (match_operand:SI 6 "const_int_operand" "")		;; success model
   (match_operand:SI 7 "const_int_operand" "")]		;; failure model
  "TARGET_CMPXCHG"
{
  emit_insn (gen_atomic_compare_and_swap_single<mode>
	     (operands[1], operands[2], operands[3], operands[4]));
  ix86_expand_setcc (operands[0], EQ, gen_rtx_REG (CCZmode, FLAGS_REG),
		     const0_rtx);
  DONE;
})

(define_mode_iterator CASMODE
  [(DI "TARGET_64BIT || TARGET_CMPXCHG8B")
   (TI "TARGET_64BIT && TARGET_CMPXCHG16B")])
(define_mode_iterator DCASMODE
  [(DI "!TARGET_64BIT && TARGET_CMPXCHG8B && !flag_pic")
   (TI "TARGET_64BIT && TARGET_CMPXCHG16B")])
(define_mode_attr doublemodesuffix [(DI "8") (TI "16")])
(define_mode_attr DCASHMODE [(DI "SI") (TI "DI")])

(define_expand "atomic_compare_and_swap<mode>"
  [(match_operand:QI 0 "register_operand" "")		;; bool success output
   (match_operand:CASMODE 1 "register_operand" "")	;; oldval output
   (match_operand:CASMODE 2 "memory_operand" "")	;; memory
   (match_operand:CASMODE 3 "register_operand" "")	;; expected input
   (match_operand:CASMODE 4 "register_operand" "")	;; newval input
   (match_operand:SI 5 "const_int_operand" "")		;; is_weak
   (match_operand:SI 6 "const_int_operand" "")		;; success model
   (match_operand:SI 7 "const_int_operand" "")]		;; failure model
  "TARGET_CMPXCHG"
{
  if (<MODE>mode == DImode && TARGET_64BIT)
    {
      emit_insn (gen_atomic_compare_and_swap_singledi
		 (operands[1], operands[2], operands[3], operands[4]));
    }
  else
    {
      enum machine_mode hmode = <DCASHMODE>mode;
      rtx lo_o, lo_e, lo_n, hi_o, hi_e, hi_n, mem;

      lo_o = operands[1];
      mem  = operands[2];
      lo_e = operands[3];
      lo_n = operands[4];
      hi_o = gen_highpart (hmode, lo_o);
      hi_e = gen_highpart (hmode, lo_e);
      hi_n = gen_highpart (hmode, lo_n);
      lo_o = gen_lowpart (hmode, lo_o);
      lo_e = gen_lowpart (hmode, lo_e);
      lo_n = gen_lowpart (hmode, lo_n);

      if (<MODE>mode == DImode
	  && !TARGET_64BIT
	  && flag_pic
	  && !cmpxchg8b_pic_memory_operand (mem, DImode))
	mem = replace_equiv_address (mem, force_reg (Pmode, XEXP (mem, 0)));

      emit_insn (gen_atomic_compare_and_swap_double<mode>
		 (lo_o, hi_o, mem, lo_e, hi_e, lo_n, hi_n));
    }
  ix86_expand_setcc (operands[0], EQ, gen_rtx_REG (CCZmode, FLAGS_REG),
		     const0_rtx);
  DONE;
})

(define_insn "atomic_compare_and_swap_single<mode>"
  [(set (match_operand:SWI 0 "register_operand" "=a")
	(unspec_volatile:SWI
	  [(match_operand:SWI 1 "memory_operand" "+m")
	   (match_operand:SWI 2 "register_operand" "0")
	   (match_operand:SWI 3 "register_operand" "<r>")]
	  UNSPECV_CMPXCHG_1))
   (set (match_dup 1)
	(unspec_volatile:SWI [(const_int 0)] UNSPECV_CMPXCHG_2))
   (set (reg:CCZ FLAGS_REG)
        (unspec_volatile:CCZ [(const_int 0)] UNSPECV_CMPXCHG_3))]
  "TARGET_CMPXCHG"
  "lock{%;} cmpxchg{<imodesuffix>}\t{%3, %1|%1, %3}")

;; For double-word compare and swap, we are obliged to play tricks with
;; the input newval (op5:op6) because the Intel register numbering does
;; not match the gcc register numbering, so the pair must be CX:BX.
;; That said, in order to take advantage of possible lower-subreg opts,
;; treat all of the integral operands in the same way.
(define_insn "atomic_compare_and_swap_double<mode>"
  [(set (match_operand:<DCASHMODE> 0 "register_operand" "=a")
	(unspec_volatile:<DCASHMODE>
	  [(match_operand:DCASMODE 2 "memory_operand" "+m")
	   (match_operand:<DCASHMODE> 3 "register_operand" "0")
	   (match_operand:<DCASHMODE> 4 "register_operand" "1")
	   (match_operand:<DCASHMODE> 5 "register_operand" "b")
	   (match_operand:<DCASHMODE> 6 "register_operand" "c")]
	  UNSPECV_CMPXCHG_1))
   (set (match_operand:<DCASHMODE> 1 "register_operand" "=d")
	(unspec_volatile:<DCASHMODE> [(const_int 0)] UNSPECV_CMPXCHG_2))
   (set (match_dup 2)
	(unspec_volatile:DCASMODE [(const_int 0)] UNSPECV_CMPXCHG_3))
   (set (reg:CCZ FLAGS_REG)
        (unspec_volatile:CCZ [(const_int 0)] UNSPECV_CMPXCHG_4))]
  ""
  "lock{%;} cmpxchg<doublemodesuffix>b\t%2")

;; Theoretically we'd like to use constraint "r" (any reg) for op5,
;; but that includes ecx.  If op5 and op6 are the same (like when
;; the input is -1LL) GCC might chose to allocate op5 to ecx, like
;; op6.  This breaks, as the xchg will move the PIC register contents
;; to %ecx then --> boom.  Operands 5 and 6 really need to be different
;; registers, which in this case means op5 must not be ecx.  Instead
;; of playing tricks with fake early clobbers or the like we just
;; enumerate all regs possible here, which (as this is !TARGET_64BIT)
;; are just esi and edi.
(define_insn "*atomic_compare_and_swap_doubledi_pic"
  [(set (match_operand:SI 0 "register_operand" "=a")
	(unspec_volatile:SI
	  [(match_operand:DI 2 "cmpxchg8b_pic_memory_operand" "+m")
	   (match_operand:SI 3 "register_operand" "0")
	   (match_operand:SI 4 "register_operand" "1")
	   (match_operand:SI 5 "register_operand" "SD")
	   (match_operand:SI 6 "register_operand" "c")]
	  UNSPECV_CMPXCHG_1))
   (set (match_operand:SI 1 "register_operand" "=d")
	(unspec_volatile:SI [(const_int 0)] UNSPECV_CMPXCHG_2))
   (set (match_dup 2)
	(unspec_volatile:DI [(const_int 0)] UNSPECV_CMPXCHG_3))
   (set (reg:CCZ FLAGS_REG)
        (unspec_volatile:CCZ [(const_int 0)] UNSPECV_CMPXCHG_4))]
  "!TARGET_64BIT && TARGET_CMPXCHG8B && flag_pic"
  "xchg{l}\t%%ebx, %5\;lock{%;} cmpxchg8b\t%2\;xchg{l}\t%%ebx, %5")

;; For operand 2 nonmemory_operand predicate is used instead of
;; register_operand to allow combiner to better optimize atomic
;; additions of constants.
(define_insn "atomic_fetch_add<mode>"
  [(set (match_operand:SWI 0 "register_operand" "=<r>")
	(unspec_volatile:SWI
	  [(match_operand:SWI 1 "memory_operand" "+m")
	   (match_operand:SI 3 "const_int_operand" "")]		;; model
	  UNSPECV_XCHG))
   (set (match_dup 1)
	(plus:SWI (match_dup 1)
		  (match_operand:SWI 2 "nonmemory_operand" "0")))
   (clobber (reg:CC FLAGS_REG))]
  "TARGET_XADD"
  "lock{%;} xadd{<imodesuffix>}\t{%0, %1|%1, %0}")

;; This peephole2 and following insn optimize
;; __sync_fetch_and_add (x, -N) == N into just lock {add,sub,inc,dec}
;; followed by testing of flags instead of lock xadd and comparisons.
(define_peephole2
  [(set (match_operand:SWI 0 "register_operand" "")
	(match_operand:SWI 2 "const_int_operand" ""))
   (parallel [(set (match_dup 0)
		   (unspec_volatile:SWI
		     [(match_operand:SWI 1 "memory_operand" "")
		      (match_operand:SI 4 "const_int_operand" "")]
		     UNSPECV_XCHG))
	      (set (match_dup 1)
		   (plus:SWI (match_dup 1)
			     (match_dup 0)))
	      (clobber (reg:CC FLAGS_REG))])
   (set (reg:CCZ FLAGS_REG)
	(compare:CCZ (match_dup 0)
		     (match_operand:SWI 3 "const_int_operand" "")))]
  "peep2_reg_dead_p (3, operands[0])
   && (unsigned HOST_WIDE_INT) INTVAL (operands[2])
      == -(unsigned HOST_WIDE_INT) INTVAL (operands[3])
   && !reg_overlap_mentioned_p (operands[0], operands[1])"
  [(parallel [(set (reg:CCZ FLAGS_REG)
		   (compare:CCZ
		     (unspec_volatile:SWI [(match_dup 1) (match_dup 4)]
					  UNSPECV_XCHG)
		     (match_dup 3)))
	      (set (match_dup 1)
		   (plus:SWI (match_dup 1)
			     (match_dup 2)))])])

(define_insn "*atomic_fetch_add_cmp<mode>"
  [(set (reg:CCZ FLAGS_REG)
	(compare:CCZ (unspec_volatile:SWI
		       [(match_operand:SWI 0 "memory_operand" "+m")
		        (match_operand:SI 3 "const_int_operand" "")]
		       UNSPECV_XCHG)
		     (match_operand:SWI 2 "const_int_operand" "i")))
   (set (match_dup 0)
	(plus:SWI (match_dup 0)
		  (match_operand:SWI 1 "const_int_operand" "i")))]
  "(unsigned HOST_WIDE_INT) INTVAL (operands[1])
   == -(unsigned HOST_WIDE_INT) INTVAL (operands[2])"
{
  if (TARGET_USE_INCDEC)
    {
      if (operands[1] == const1_rtx)
	return "lock{%;} inc{<imodesuffix>}\t%0";
      if (operands[1] == constm1_rtx)
	return "lock{%;} dec{<imodesuffix>}\t%0";
    }

  if (x86_maybe_negate_const_int (&operands[1], <MODE>mode))
    return "lock{%;} sub{<imodesuffix>}\t{%1, %0|%0, %1}";

  return "lock{%;} add{<imodesuffix>}\t{%1, %0|%0, %1}";
})

;; Recall that xchg implicitly sets LOCK#, so adding it again wastes space.
;; In addition, it is always a full barrier, so we can ignore the memory model.
(define_insn "atomic_exchange<mode>"
  [(set (match_operand:SWI 0 "register_operand" "=<r>")		;; output
	(unspec_volatile:SWI
	  [(match_operand:SWI 1 "memory_operand" "+m")		;; memory
	   (match_operand:SI 3 "const_int_operand" "")]		;; model
	  UNSPECV_XCHG))
   (set (match_dup 1)
	(match_operand:SWI 2 "register_operand" "0"))]		;; input
  ""
  "xchg{<imodesuffix>}\t{%1, %0|%0, %1}")

(define_insn "atomic_add<mode>"
  [(set (match_operand:SWI 0 "memory_operand" "+m")
	(unspec_volatile:SWI
	  [(plus:SWI (match_dup 0)
		     (match_operand:SWI 1 "nonmemory_operand" "<r><i>"))
	   (match_operand:SI 2 "const_int_operand" "")]		;; model
	  UNSPECV_LOCK))
   (clobber (reg:CC FLAGS_REG))]
  ""
{
  if (TARGET_USE_INCDEC)
    {
      if (operands[1] == const1_rtx)
	return "lock{%;} inc{<imodesuffix>}\t%0";
      if (operands[1] == constm1_rtx)
	return "lock{%;} dec{<imodesuffix>}\t%0";
    }

  if (x86_maybe_negate_const_int (&operands[1], <MODE>mode))
    return "lock{%;} sub{<imodesuffix>}\t{%1, %0|%0, %1}";

  return "lock{%;} add{<imodesuffix>}\t{%1, %0|%0, %1}";
})

(define_insn "atomic_sub<mode>"
  [(set (match_operand:SWI 0 "memory_operand" "+m")
	(unspec_volatile:SWI
	  [(minus:SWI (match_dup 0)
		      (match_operand:SWI 1 "nonmemory_operand" "<r><i>"))
	   (match_operand:SI 2 "const_int_operand" "")]		;; model
	  UNSPECV_LOCK))
   (clobber (reg:CC FLAGS_REG))]
  ""
{
  if (TARGET_USE_INCDEC)
    {
      if (operands[1] == const1_rtx)
	return "lock{%;} dec{<imodesuffix>}\t%0";
      if (operands[1] == constm1_rtx)
	return "lock{%;} inc{<imodesuffix>}\t%0";
    }

  if (x86_maybe_negate_const_int (&operands[1], <MODE>mode))
    return "lock{%;} add{<imodesuffix>}\t{%1, %0|%0, %1}";

  return "lock{%;} sub{<imodesuffix>}\t{%1, %0|%0, %1}";
})

(define_insn "atomic_<code><mode>"
  [(set (match_operand:SWI 0 "memory_operand" "+m")
	(unspec_volatile:SWI
	  [(any_logic:SWI (match_dup 0)
			  (match_operand:SWI 1 "nonmemory_operand" "<r><i>"))
	   (match_operand:SI 2 "const_int_operand" "")]		;; model
	  UNSPECV_LOCK))
   (clobber (reg:CC FLAGS_REG))]
  ""
  "lock{%;} <logic>{<imodesuffix>}\t{%1, %0|%0, %1}")
