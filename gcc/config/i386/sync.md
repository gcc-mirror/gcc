;; GCC machine description for i386 synchronization instructions.
;; Copyright (C) 2005-2016 Free Software Foundation, Inc.
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

  UNSPEC_FILD_ATOMIC
  UNSPEC_FIST_ATOMIC

  ;; __atomic support
  UNSPEC_LDA
  UNSPEC_STA
])

(define_c_enum "unspecv" [
  UNSPECV_CMPXCHG
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
  [(set (match_operand:BLK 0)
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
  [(set (match_operand:BLK 0)
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
  [(set (match_operand:BLK 0)
	(unspec:BLK [(match_dup 0)] UNSPEC_MFENCE))]
  "TARGET_64BIT || TARGET_SSE2"
  "mfence"
  [(set_attr "type" "sse")
   (set_attr "length_address" "0")
   (set_attr "atom_sse_attr" "fence")
   (set_attr "memory" "unknown")])

(define_insn "mfence_nosse"
  [(set (match_operand:BLK 0)
	(unspec:BLK [(match_dup 0)] UNSPEC_MFENCE))
   (clobber (reg:CC FLAGS_REG))]
  "!(TARGET_64BIT || TARGET_SSE2)"
  "lock{%;} or{l}\t{$0, (%%esp)|DWORD PTR [esp], 0}"
  [(set_attr "memory" "unknown")])

(define_expand "mem_thread_fence"
  [(match_operand:SI 0 "const_int_operand")]		;; model
  ""
{
  enum memmodel model = memmodel_from_int (INTVAL (operands[0]));

  /* Unless this is a SEQ_CST fence, the i386 memory model is strong
     enough not to require barriers of any kind.  */
  if (is_mm_seq_cst (model))
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

;; ??? From volume 3 section 8.1.1 Guaranteed Atomic Operations,
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
  [(set (match_operand:ATOMIC 0 "nonimmediate_operand")
	(unspec:ATOMIC [(match_operand:ATOMIC 1 "memory_operand")
			(match_operand:SI 2 "const_int_operand")]
		       UNSPEC_LDA))]
  ""
{
  /* For DImode on 32-bit, we can use the FPU to perform the load.  */
  if (<MODE>mode == DImode && !TARGET_64BIT)
    emit_insn (gen_atomic_loaddi_fpu
	       (operands[0], operands[1],
	        assign_386_stack_local (DImode, SLOT_TEMP)));
  else
    {
      rtx dst = operands[0];

      if (MEM_P (dst))
	dst = gen_reg_rtx (<MODE>mode);

      emit_move_insn (dst, operands[1]);

      /* Fix up the destination if needed.  */
      if (dst != operands[0])
	emit_move_insn (operands[0], dst);
    }
  DONE;
})

(define_insn_and_split "atomic_loaddi_fpu"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=x,m,?r")
	(unspec:DI [(match_operand:DI 1 "memory_operand" "m,m,m")]
		   UNSPEC_LDA))
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

      if (STACK_REG_P (tmp))
        {
	  emit_insn (gen_loaddi_via_fpu (tmp, src));
	  emit_insn (gen_storedi_via_fpu (mem, tmp));
	}
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
  [(set (match_operand:ATOMIC 0 "memory_operand")
	(unspec:ATOMIC [(match_operand:ATOMIC 1 "nonimmediate_operand")
			(match_operand:SI 2 "const_int_operand")]
		       UNSPEC_STA))]
  ""
{
  enum memmodel model = memmodel_from_int (INTVAL (operands[2]));

  if (<MODE>mode == DImode && !TARGET_64BIT)
    {
      /* For DImode on 32-bit, we can use the FPU to perform the store.  */
      /* Note that while we could perform a cmpxchg8b loop, that turns
	 out to be significantly larger than this plus a barrier.  */
      emit_insn (gen_atomic_storedi_fpu
		 (operands[0], operands[1],
	          assign_386_stack_local (DImode, SLOT_TEMP)));
    }
  else
    {
      operands[1] = force_reg (<MODE>mode, operands[1]);

      /* For seq-cst stores, when we lack MFENCE, use XCHG.  */
      if (is_mm_seq_cst (model) && !(TARGET_64BIT || TARGET_SSE2))
	{
	  emit_insn (gen_atomic_exchange<mode> (gen_reg_rtx (<MODE>mode),
						operands[0], operands[1],
						operands[2]));
	  DONE;
	}

      /* Otherwise use a store.  */
      emit_insn (gen_atomic_store<mode>_1 (operands[0], operands[1],
					   operands[2]));
    }
  /* ... followed by an MFENCE, if required.  */
  if (is_mm_seq_cst (model))
    emit_insn (gen_mem_thread_fence (operands[2]));
  DONE;
})

(define_insn "atomic_store<mode>_1"
  [(set (match_operand:SWI 0 "memory_operand" "=m")
	(unspec:SWI [(match_operand:SWI 1 "<nonmemory_operand>" "<r><i>")
		     (match_operand:SI 2 "const_int_operand")]
		    UNSPEC_STA))]
  ""
  "%K2mov{<imodesuffix>}\t{%1, %0|%0, %1}")

(define_insn_and_split "atomic_storedi_fpu"
  [(set (match_operand:DI 0 "memory_operand" "=m,m,m")
	(unspec:DI [(match_operand:DI 1 "nonimmediate_operand" "x,m,?r")]
		   UNSPEC_STA))
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

      if (STACK_REG_P (tmp))
	{
	  emit_insn (gen_loaddi_via_fpu (tmp, src));
	  emit_insn (gen_storedi_via_fpu (dst, tmp));
	  DONE;
	}
      else
	{
	  adjust_reg_mode (tmp, DImode);
	  emit_move_insn (tmp, src);
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

(define_insn "loaddi_via_fpu"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(unspec:DF [(match_operand:DI 1 "memory_operand" "m")]
		   UNSPEC_FILD_ATOMIC))]
  "TARGET_80387"
  "fild%Z1\t%1"
  [(set_attr "type" "fmov")
   (set_attr "mode" "DF")
   (set_attr "fp_int_src" "true")])

(define_insn "storedi_via_fpu"
  [(set (match_operand:DI 0 "memory_operand" "=m")
	(unspec:DI [(match_operand:DF 1 "register_operand" "f")]
		   UNSPEC_FIST_ATOMIC))]
  "TARGET_80387"
{
  gcc_assert (find_regno_note (insn, REG_DEAD, FIRST_STACK_REG) != NULL_RTX);

  return "fistp%Z0\t%0";
}
  [(set_attr "type" "fmov")
   (set_attr "mode" "DI")])

(define_expand "atomic_compare_and_swap<mode>"
  [(match_operand:QI 0 "register_operand")	;; bool success output
   (match_operand:SWI124 1 "register_operand")	;; oldval output
   (match_operand:SWI124 2 "memory_operand")	;; memory
   (match_operand:SWI124 3 "register_operand")	;; expected input
   (match_operand:SWI124 4 "register_operand")	;; newval input
   (match_operand:SI 5 "const_int_operand")	;; is_weak
   (match_operand:SI 6 "const_int_operand")	;; success model
   (match_operand:SI 7 "const_int_operand")]	;; failure model
  "TARGET_CMPXCHG"
{
  emit_insn
   (gen_atomic_compare_and_swap<mode>_1
    (operands[1], operands[2], operands[3], operands[4], operands[6]));
  ix86_expand_setcc (operands[0], EQ, gen_rtx_REG (CCZmode, FLAGS_REG),
		     const0_rtx);
  DONE;
})

(define_mode_iterator CASMODE
  [(DI "TARGET_64BIT || TARGET_CMPXCHG8B")
   (TI "TARGET_64BIT && TARGET_CMPXCHG16B")])
(define_mode_attr CASHMODE [(DI "SI") (TI "DI")])

(define_expand "atomic_compare_and_swap<mode>"
  [(match_operand:QI 0 "register_operand")	;; bool success output
   (match_operand:CASMODE 1 "register_operand")	;; oldval output
   (match_operand:CASMODE 2 "memory_operand")	;; memory
   (match_operand:CASMODE 3 "register_operand")	;; expected input
   (match_operand:CASMODE 4 "register_operand")	;; newval input
   (match_operand:SI 5 "const_int_operand")	;; is_weak
   (match_operand:SI 6 "const_int_operand")	;; success model
   (match_operand:SI 7 "const_int_operand")]	;; failure model
  "TARGET_CMPXCHG"
{
  if (<MODE>mode == DImode && TARGET_64BIT)
    {
      emit_insn
       (gen_atomic_compare_and_swapdi_1
	(operands[1], operands[2], operands[3], operands[4], operands[6]));
    }
  else
    {
      machine_mode hmode = <CASHMODE>mode;

      emit_insn
       (gen_atomic_compare_and_swap<mode>_doubleword
        (operands[1], operands[2], operands[3],
	 gen_lowpart (hmode, operands[4]), gen_highpart (hmode, operands[4]),
	 operands[6]));
    }

  ix86_expand_setcc (operands[0], EQ, gen_rtx_REG (CCZmode, FLAGS_REG),
		     const0_rtx);
  DONE;
})

;; For double-word compare and swap, we are obliged to play tricks with
;; the input newval (op3:op4) because the Intel register numbering does
;; not match the gcc register numbering, so the pair must be CX:BX.

(define_mode_attr doublemodesuffix [(SI "8") (DI "16")])

(define_insn "atomic_compare_and_swap<dwi>_doubleword"
  [(set (match_operand:<DWI> 0 "register_operand" "=A")
	(unspec_volatile:<DWI>
	  [(match_operand:<DWI> 1 "memory_operand" "+m")
	   (match_operand:<DWI> 2 "register_operand" "0")
	   (match_operand:DWIH 3 "register_operand" "b")
	   (match_operand:DWIH 4 "register_operand" "c")
	   (match_operand:SI 5 "const_int_operand")]
	  UNSPECV_CMPXCHG))
   (set (match_dup 1)
	(unspec_volatile:<DWI> [(const_int 0)] UNSPECV_CMPXCHG))
   (set (reg:CCZ FLAGS_REG)
        (unspec_volatile:CCZ [(const_int 0)] UNSPECV_CMPXCHG))]
  "TARGET_CMPXCHG<doublemodesuffix>B"
  "lock{%;} %K5cmpxchg<doublemodesuffix>b\t%1")

(define_insn "atomic_compare_and_swap<mode>_1"
  [(set (match_operand:SWI 0 "register_operand" "=a")
	(unspec_volatile:SWI
	  [(match_operand:SWI 1 "memory_operand" "+m")
	   (match_operand:SWI 2 "register_operand" "0")
	   (match_operand:SWI 3 "register_operand" "<r>")
	   (match_operand:SI 4 "const_int_operand")]
	  UNSPECV_CMPXCHG))
   (set (match_dup 1)
	(unspec_volatile:SWI [(const_int 0)] UNSPECV_CMPXCHG))
   (set (reg:CCZ FLAGS_REG)
        (unspec_volatile:CCZ [(const_int 0)] UNSPECV_CMPXCHG))]
  "TARGET_CMPXCHG"
  "lock{%;} %K4cmpxchg{<imodesuffix>}\t{%3, %1|%1, %3}")

;; For operand 2 nonmemory_operand predicate is used instead of
;; register_operand to allow combiner to better optimize atomic
;; additions of constants.
(define_insn "atomic_fetch_add<mode>"
  [(set (match_operand:SWI 0 "register_operand" "=<r>")
	(unspec_volatile:SWI
	  [(match_operand:SWI 1 "memory_operand" "+m")
	   (match_operand:SI 3 "const_int_operand")]		;; model
	  UNSPECV_XCHG))
   (set (match_dup 1)
	(plus:SWI (match_dup 1)
		  (match_operand:SWI 2 "nonmemory_operand" "0")))
   (clobber (reg:CC FLAGS_REG))]
  "TARGET_XADD"
  "lock{%;} %K3xadd{<imodesuffix>}\t{%0, %1|%1, %0}")

;; This peephole2 and following insn optimize
;; __sync_fetch_and_add (x, -N) == N into just lock {add,sub,inc,dec}
;; followed by testing of flags instead of lock xadd and comparisons.
(define_peephole2
  [(set (match_operand:SWI 0 "register_operand")
	(match_operand:SWI 2 "const_int_operand"))
   (parallel [(set (match_dup 0)
		   (unspec_volatile:SWI
		     [(match_operand:SWI 1 "memory_operand")
		      (match_operand:SI 4 "const_int_operand")]
		     UNSPECV_XCHG))
	      (set (match_dup 1)
		   (plus:SWI (match_dup 1)
			     (match_dup 0)))
	      (clobber (reg:CC FLAGS_REG))])
   (set (reg:CCZ FLAGS_REG)
	(compare:CCZ (match_dup 0)
		     (match_operand:SWI 3 "const_int_operand")))]
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

;; Likewise, but for the -Os special case of *mov<mode>_or.
(define_peephole2
  [(parallel [(set (match_operand:SWI 0 "register_operand")
		   (match_operand:SWI 2 "constm1_operand"))
	      (clobber (reg:CC FLAGS_REG))])
   (parallel [(set (match_dup 0)
		   (unspec_volatile:SWI
		     [(match_operand:SWI 1 "memory_operand")
		      (match_operand:SI 4 "const_int_operand")]
		     UNSPECV_XCHG))
	      (set (match_dup 1)
		   (plus:SWI (match_dup 1)
			     (match_dup 0)))
	      (clobber (reg:CC FLAGS_REG))])
   (set (reg:CCZ FLAGS_REG)
	(compare:CCZ (match_dup 0)
		     (match_operand:SWI 3 "const_int_operand")))]
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
	(compare:CCZ
	  (unspec_volatile:SWI
	    [(match_operand:SWI 0 "memory_operand" "+m")
	     (match_operand:SI 3 "const_int_operand")]		;; model
	    UNSPECV_XCHG)
	  (match_operand:SWI 2 "const_int_operand" "i")))
   (set (match_dup 0)
	(plus:SWI (match_dup 0)
		  (match_operand:SWI 1 "const_int_operand" "i")))]
  "(unsigned HOST_WIDE_INT) INTVAL (operands[1])
   == -(unsigned HOST_WIDE_INT) INTVAL (operands[2])"
{
  if (incdec_operand (operands[1], <MODE>mode))
    {
      if (operands[1] == const1_rtx)
	return "lock{%;} %K3inc{<imodesuffix>}\t%0";
      else
	{
	  gcc_assert (operands[1] == constm1_rtx);
	  return "lock{%;} %K3dec{<imodesuffix>}\t%0";
	}
    }

  if (x86_maybe_negate_const_int (&operands[1], <MODE>mode))
    return "lock{%;} %K3sub{<imodesuffix>}\t{%1, %0|%0, %1}";

  return "lock{%;} %K3add{<imodesuffix>}\t{%1, %0|%0, %1}";
})

;; Recall that xchg implicitly sets LOCK#, so adding it again wastes space.
;; In addition, it is always a full barrier, so we can ignore the memory model.
(define_insn "atomic_exchange<mode>"
  [(set (match_operand:SWI 0 "register_operand" "=<r>")		;; output
	(unspec_volatile:SWI
	  [(match_operand:SWI 1 "memory_operand" "+m")		;; memory
	   (match_operand:SI 3 "const_int_operand")]		;; model
	  UNSPECV_XCHG))
   (set (match_dup 1)
	(match_operand:SWI 2 "register_operand" "0"))]		;; input
  ""
  "%K3xchg{<imodesuffix>}\t{%1, %0|%0, %1}")

(define_insn "atomic_add<mode>"
  [(set (match_operand:SWI 0 "memory_operand" "+m")
	(unspec_volatile:SWI
	  [(plus:SWI (match_dup 0)
		     (match_operand:SWI 1 "nonmemory_operand" "<r><i>"))
	   (match_operand:SI 2 "const_int_operand")]		;; model
	  UNSPECV_LOCK))
   (clobber (reg:CC FLAGS_REG))]
  ""
{
  if (incdec_operand (operands[1], <MODE>mode))
    {
      if (operands[1] == const1_rtx)
	return "lock{%;} %K2inc{<imodesuffix>}\t%0";
      else
	{
	  gcc_assert (operands[1] == constm1_rtx);
	  return "lock{%;} %K2dec{<imodesuffix>}\t%0";
	}
    }

  if (x86_maybe_negate_const_int (&operands[1], <MODE>mode))
    return "lock{%;} %K2sub{<imodesuffix>}\t{%1, %0|%0, %1}";

  return "lock{%;} %K2add{<imodesuffix>}\t{%1, %0|%0, %1}";
})

(define_insn "atomic_sub<mode>"
  [(set (match_operand:SWI 0 "memory_operand" "+m")
	(unspec_volatile:SWI
	  [(minus:SWI (match_dup 0)
		      (match_operand:SWI 1 "nonmemory_operand" "<r><i>"))
	   (match_operand:SI 2 "const_int_operand")]		;; model
	  UNSPECV_LOCK))
   (clobber (reg:CC FLAGS_REG))]
  ""
{
  if (incdec_operand (operands[1], <MODE>mode))
    {
      if (operands[1] == const1_rtx)
	return "lock{%;} %K2dec{<imodesuffix>}\t%0";
      else
	{
	  gcc_assert (operands[1] == constm1_rtx);
	  return "lock{%;} %K2inc{<imodesuffix>}\t%0";
	}
    }

  if (x86_maybe_negate_const_int (&operands[1], <MODE>mode))
    return "lock{%;} %K2add{<imodesuffix>}\t{%1, %0|%0, %1}";

  return "lock{%;} %K2sub{<imodesuffix>}\t{%1, %0|%0, %1}";
})

(define_insn "atomic_<logic><mode>"
  [(set (match_operand:SWI 0 "memory_operand" "+m")
	(unspec_volatile:SWI
	  [(any_logic:SWI (match_dup 0)
			  (match_operand:SWI 1 "nonmemory_operand" "<r><i>"))
	   (match_operand:SI 2 "const_int_operand")]		;; model
	  UNSPECV_LOCK))
   (clobber (reg:CC FLAGS_REG))]
  ""
  "lock{%;} %K2<logic>{<imodesuffix>}\t{%1, %0|%0, %1}")

(define_expand "atomic_bit_test_and_set<mode>"
  [(match_operand:SWI248 0 "register_operand")
   (match_operand:SWI248 1 "memory_operand")
   (match_operand:SWI248 2 "nonmemory_operand")
   (match_operand:SI 3 "const_int_operand") ;; model
   (match_operand:SI 4 "const_int_operand")]
  ""
{
  emit_insn (gen_atomic_bit_test_and_set<mode>_1 (operands[1], operands[2],
						  operands[3]));
  rtx tem = gen_reg_rtx (QImode);
  ix86_expand_setcc (tem, EQ, gen_rtx_REG (CCCmode, FLAGS_REG), const0_rtx);
  rtx result = convert_modes (<MODE>mode, QImode, tem, 1);
  if (operands[4] == const0_rtx)
    result = expand_simple_binop (<MODE>mode, ASHIFT, result,
				  operands[2], operands[0], 0, OPTAB_DIRECT);
  if (result != operands[0])
    emit_move_insn (operands[0], result);
  DONE;
})

(define_insn "atomic_bit_test_and_set<mode>_1"
  [(set (reg:CCC FLAGS_REG)
	(compare:CCC
	  (unspec_volatile:SWI248
	    [(match_operand:SWI248 0 "memory_operand" "+m")
	     (match_operand:SI 2 "const_int_operand")]		;; model
	    UNSPECV_XCHG)
	  (const_int 0)))
   (set (zero_extract:SWI248 (match_dup 0)
			     (const_int 1)
			     (match_operand:SWI248 1 "nonmemory_operand" "rN"))
	(const_int 1))]
  ""
  "lock{%;} %K2bts{<imodesuffix>}\t{%1, %0|%0, %1}")

(define_expand "atomic_bit_test_and_complement<mode>"
  [(match_operand:SWI248 0 "register_operand")
   (match_operand:SWI248 1 "memory_operand")
   (match_operand:SWI248 2 "nonmemory_operand")
   (match_operand:SI 3 "const_int_operand") ;; model
   (match_operand:SI 4 "const_int_operand")]
  ""
{
  emit_insn (gen_atomic_bit_test_and_complement<mode>_1 (operands[1],
							 operands[2],
							 operands[3]));
  rtx tem = gen_reg_rtx (QImode);
  ix86_expand_setcc (tem, EQ, gen_rtx_REG (CCCmode, FLAGS_REG), const0_rtx);
  rtx result = convert_modes (<MODE>mode, QImode, tem, 1);
  if (operands[4] == const0_rtx)
    result = expand_simple_binop (<MODE>mode, ASHIFT, result,
				  operands[2], operands[0], 0, OPTAB_DIRECT);
  if (result != operands[0])
    emit_move_insn (operands[0], result);
  DONE;
})

(define_insn "atomic_bit_test_and_complement<mode>_1"
  [(set (reg:CCC FLAGS_REG)
	(compare:CCC
	  (unspec_volatile:SWI248
	    [(match_operand:SWI248 0 "memory_operand" "+m")
	     (match_operand:SI 2 "const_int_operand")]		;; model
	    UNSPECV_XCHG)
	  (const_int 0)))
   (set (zero_extract:SWI248 (match_dup 0)
			     (const_int 1)
			     (match_operand:SWI248 1 "nonmemory_operand" "rN"))
	(not:SWI248 (zero_extract:SWI248 (match_dup 0)
					 (const_int 1)
					 (match_dup 1))))]
  ""
  "lock{%;} %K2btc{<imodesuffix>}\t{%1, %0|%0, %1}")

(define_expand "atomic_bit_test_and_reset<mode>"
  [(match_operand:SWI248 0 "register_operand")
   (match_operand:SWI248 1 "memory_operand")
   (match_operand:SWI248 2 "nonmemory_operand")
   (match_operand:SI 3 "const_int_operand") ;; model
   (match_operand:SI 4 "const_int_operand")]
  ""
{
  emit_insn (gen_atomic_bit_test_and_reset<mode>_1 (operands[1], operands[2],
						    operands[3]));
  rtx tem = gen_reg_rtx (QImode);
  ix86_expand_setcc (tem, EQ, gen_rtx_REG (CCCmode, FLAGS_REG), const0_rtx);
  rtx result = convert_modes (<MODE>mode, QImode, tem, 1);
  if (operands[4] == const0_rtx)
    result = expand_simple_binop (<MODE>mode, ASHIFT, result,
				  operands[2], operands[0], 0, OPTAB_DIRECT);
  if (result != operands[0])
    emit_move_insn (operands[0], result);
  DONE;
})

(define_insn "atomic_bit_test_and_reset<mode>_1"
  [(set (reg:CCC FLAGS_REG)
	(compare:CCC
	  (unspec_volatile:SWI248
	    [(match_operand:SWI248 0 "memory_operand" "+m")
	     (match_operand:SI 2 "const_int_operand")]		;; model
	    UNSPECV_XCHG)
	  (const_int 0)))
   (set (zero_extract:SWI248 (match_dup 0)
			     (const_int 1)
			     (match_operand:SWI248 1 "nonmemory_operand" "rN"))
	(const_int 0))]
  ""
  "lock{%;} %K2btr{<imodesuffix>}\t{%1, %0|%0, %1}")
