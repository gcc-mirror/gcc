;; Machine description for PowerPC synchronization instructions.
;; Copyright (C) 2005-2018 Free Software Foundation, Inc.
;; Contributed by Geoffrey Keating.

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_mode_attr larx [(QI "lbarx")
			(HI "lharx")
			(SI "lwarx")
			(DI "ldarx")
			(TI "lqarx")])

(define_mode_attr stcx [(QI "stbcx.")
			(HI "sthcx.")
			(SI "stwcx.")
			(DI "stdcx.")
			(TI "stqcx.")])

(define_code_iterator FETCHOP [plus minus ior xor and])
(define_code_attr fetchop_name
  [(plus "add") (minus "sub") (ior "or") (xor "xor") (and "and")])
(define_code_attr fetchop_pred
  [(plus "add_operand") (minus "int_reg_operand")
   (ior "logical_operand") (xor "logical_operand") (and "and_operand")])

(define_expand "mem_thread_fence"
  [(match_operand:SI 0 "const_int_operand")]		;; model
  ""
{
  enum memmodel model = memmodel_base (INTVAL (operands[0]));
  switch (model)
    {
    case MEMMODEL_RELAXED:
      break;
    case MEMMODEL_CONSUME:
    case MEMMODEL_ACQUIRE:
    case MEMMODEL_RELEASE:
    case MEMMODEL_ACQ_REL:
      emit_insn (gen_lwsync ());
      break;
    case MEMMODEL_SEQ_CST:
      emit_insn (gen_hwsync ());
      break;
    default:
      gcc_unreachable ();
    }
  DONE;
})

(define_expand "hwsync"
  [(set (match_dup 0)
	(unspec:BLK [(match_dup 0)] UNSPEC_SYNC))]
  ""
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
})

(define_insn "*hwsync"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0)] UNSPEC_SYNC))]
  ""
  "sync"
  [(set_attr "type" "sync")])

(define_expand "lwsync"
  [(set (match_dup 0)
	(unspec:BLK [(match_dup 0)] UNSPEC_LWSYNC))]
  ""
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
})

(define_insn "*lwsync"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0)] UNSPEC_LWSYNC))]
  ""
{
  /* Some AIX assemblers don't accept lwsync, so we use a .long.  */
  if (TARGET_NO_LWSYNC)
    return "sync";
  else if (TARGET_LWSYNC_INSTRUCTION)
    return "lwsync";
  else
    return ".long 0x7c2004ac";
}
  [(set_attr "type" "sync")])

(define_insn "isync"
  [(unspec_volatile:BLK [(const_int 0)] UNSPECV_ISYNC)]
  ""
  "isync"
  [(set_attr "type" "isync")])

;; Types that we should provide atomic instructions for.
(define_mode_iterator AINT [QI
			    HI
			    SI
			    (DI "TARGET_POWERPC64")
			    (TI "TARGET_SYNC_TI")])

;; The control dependency used for load dependency described
;; in B.2.3 of the Power ISA 2.06B.
(define_insn "loadsync_<mode>"
  [(unspec_volatile:BLK [(match_operand:AINT 0 "register_operand" "r")]
			UNSPECV_ISYNC)
   (clobber (match_scratch:CC 1 "=y"))]
  ""
  "cmpw %1,%0,%0\;bne- %1,$+4\;isync"
  [(set_attr "type" "isync")
   (set_attr "length" "12")])

(define_insn "load_quadpti"
  [(set (match_operand:PTI 0 "quad_int_reg_operand" "=&r")
	(unspec:PTI
	 [(match_operand:TI 1 "quad_memory_operand" "wQ")] UNSPEC_LSQ))]
  "TARGET_SYNC_TI
   && !reg_mentioned_p (operands[0], operands[1])"
  "lq %0,%1"
  [(set_attr "type" "load")])

(define_expand "atomic_load<mode>"
  [(set (match_operand:AINT 0 "register_operand")		;; output
	(match_operand:AINT 1 "memory_operand"))		;; memory
   (use (match_operand:SI 2 "const_int_operand"))]		;; model
  ""
{
  if (<MODE>mode == TImode && !TARGET_SYNC_TI)
    FAIL;

  enum memmodel model = memmodel_base (INTVAL (operands[2]));

  if (is_mm_seq_cst (model))
    emit_insn (gen_hwsync ());

  if (<MODE>mode != TImode)
    emit_move_insn (operands[0], operands[1]);
  else
    {
      rtx op0 = operands[0];
      rtx op1 = operands[1];
      rtx pti_reg = gen_reg_rtx (PTImode);

      if (!quad_address_p (XEXP (op1, 0), TImode, false))
	{
	  rtx old_addr = XEXP (op1, 0);
	  rtx new_addr = force_reg (Pmode, old_addr);
	  operands[1] = op1 = replace_equiv_address (op1, new_addr);
	}

      emit_insn (gen_load_quadpti (pti_reg, op1));

      if (WORDS_BIG_ENDIAN)
	emit_move_insn (op0, gen_lowpart (TImode, pti_reg));
      else
	{
	  emit_move_insn (gen_lowpart (DImode, op0), gen_highpart (DImode, pti_reg));
	  emit_move_insn (gen_highpart (DImode, op0), gen_lowpart (DImode, pti_reg));
	}
    }

  switch (model)
    {
    case MEMMODEL_RELAXED:
      break;
    case MEMMODEL_CONSUME:
    case MEMMODEL_ACQUIRE:
    case MEMMODEL_SEQ_CST:
      emit_insn (gen_loadsync_<mode> (operands[0]));
      break;
    default:
      gcc_unreachable ();
    }
  DONE;
})

(define_insn "store_quadpti"
  [(set (match_operand:PTI 0 "quad_memory_operand" "=wQ")
	(unspec:PTI
	 [(match_operand:PTI 1 "quad_int_reg_operand" "r")] UNSPEC_LSQ))]
  "TARGET_SYNC_TI"
  "stq %1,%0"
  [(set_attr "type" "store")])

(define_expand "atomic_store<mode>"
  [(set (match_operand:AINT 0 "memory_operand")		;; memory
	(match_operand:AINT 1 "register_operand"))	;; input
   (use (match_operand:SI 2 "const_int_operand"))]	;; model
  ""
{
  if (<MODE>mode == TImode && !TARGET_SYNC_TI)
    FAIL;

  enum memmodel model = memmodel_base (INTVAL (operands[2]));
  switch (model)
    {
    case MEMMODEL_RELAXED:
      break;
    case MEMMODEL_RELEASE:
      emit_insn (gen_lwsync ());
      break;
    case MEMMODEL_SEQ_CST:
      emit_insn (gen_hwsync ());
      break;
    default:
      gcc_unreachable ();
    }
  if (<MODE>mode != TImode)
    emit_move_insn (operands[0], operands[1]);
  else
    {
      rtx op0 = operands[0];
      rtx op1 = operands[1];
      rtx pti_reg = gen_reg_rtx (PTImode);

      if (!quad_address_p (XEXP (op0, 0), TImode, false))
	{
	  rtx old_addr = XEXP (op0, 0);
	  rtx new_addr = force_reg (Pmode, old_addr);
	  operands[0] = op0 = replace_equiv_address (op0, new_addr);
	}

      if (WORDS_BIG_ENDIAN)
	emit_move_insn (pti_reg, gen_lowpart (PTImode, op1));
      else
	{
	  emit_move_insn (gen_lowpart (DImode, pti_reg), gen_highpart (DImode, op1));
	  emit_move_insn (gen_highpart (DImode, pti_reg), gen_lowpart (DImode, op1));
	}

      emit_insn (gen_store_quadpti (gen_lowpart (PTImode, op0), pti_reg));
    }

  DONE;
})

;; Any supported integer mode that has atomic l<x>arx/st<x>cx. instrucitons
;; other than the quad memory operations, which have special restrictions.
;; Byte/halfword atomic instructions were added in ISA 2.06B, but were phased
;; in and did not show up until power8.  TImode atomic lqarx/stqcx. require
;; special handling due to even/odd register requirements.
(define_mode_iterator ATOMIC [(QI "TARGET_SYNC_HI_QI")
			      (HI "TARGET_SYNC_HI_QI")
			      SI
			      (DI "TARGET_POWERPC64")])

(define_insn "load_locked<mode>"
  [(set (match_operand:ATOMIC 0 "int_reg_operand" "=r")
	(unspec_volatile:ATOMIC
         [(match_operand:ATOMIC 1 "memory_operand" "Z")] UNSPECV_LL))]
  ""
  "<larx> %0,%y1"
  [(set_attr "type" "load_l")])

(define_insn "load_locked<QHI:mode>_si"
  [(set (match_operand:SI 0 "int_reg_operand" "=r")
	(unspec_volatile:SI
	  [(match_operand:QHI 1 "memory_operand" "Z")] UNSPECV_LL))]
  "TARGET_SYNC_HI_QI"
  "<QHI:larx> %0,%y1"
  [(set_attr "type" "load_l")])

;; Use PTImode to get even/odd register pairs.
;; Use a temporary register to force getting an even register for the
;; lqarx/stqcrx. instructions.  Normal optimizations will eliminate this extra
;; copy on big endian systems.

;; On little endian systems where non-atomic quad word load/store instructions
;; are not used, the address can be register+offset, so make sure the address
;; is indexed or indirect before register allocation.

(define_expand "load_lockedti"
  [(use (match_operand:TI 0 "quad_int_reg_operand"))
   (use (match_operand:TI 1 "memory_operand"))]
  "TARGET_SYNC_TI"
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  rtx pti = gen_reg_rtx (PTImode);

  if (!indexed_or_indirect_operand (op1, TImode))
    {
      rtx old_addr = XEXP (op1, 0);
      rtx new_addr = force_reg (Pmode, old_addr);
      operands[1] = op1 = change_address (op1, TImode, new_addr);
    }

  emit_insn (gen_load_lockedpti (pti, op1));
  if (WORDS_BIG_ENDIAN)
    emit_move_insn (op0, gen_lowpart (TImode, pti));
  else
    {
      emit_move_insn (gen_lowpart (DImode, op0), gen_highpart (DImode, pti));
      emit_move_insn (gen_highpart (DImode, op0), gen_lowpart (DImode, pti));
    }
  DONE;
})

(define_insn "load_lockedpti"
  [(set (match_operand:PTI 0 "quad_int_reg_operand" "=&r")
	(unspec_volatile:PTI
         [(match_operand:TI 1 "indexed_or_indirect_operand" "Z")] UNSPECV_LL))]
  "TARGET_SYNC_TI
   && !reg_mentioned_p (operands[0], operands[1])
   && quad_int_reg_operand (operands[0], PTImode)"
  "lqarx %0,%y1"
  [(set_attr "type" "load_l")])

(define_insn "store_conditional<mode>"
  [(set (match_operand:CC 0 "cc_reg_operand" "=x")
	(unspec_volatile:CC [(const_int 0)] UNSPECV_SC))
   (set (match_operand:ATOMIC 1 "memory_operand" "=Z")
	(match_operand:ATOMIC 2 "int_reg_operand" "r"))]
  ""
  "<stcx> %2,%y1"
  [(set_attr "type" "store_c")])

;; Use a temporary register to force getting an even register for the
;; lqarx/stqcrx. instructions.  Normal optimizations will eliminate this extra
;; copy on big endian systems.

;; On little endian systems where non-atomic quad word load/store instructions
;; are not used, the address can be register+offset, so make sure the address
;; is indexed or indirect before register allocation.

(define_expand "store_conditionalti"
  [(use (match_operand:CC 0 "cc_reg_operand"))
   (use (match_operand:TI 1 "memory_operand"))
   (use (match_operand:TI 2 "quad_int_reg_operand"))]
  "TARGET_SYNC_TI"
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  rtx op2 = operands[2];
  rtx addr = XEXP (op1, 0);
  rtx pti_mem;
  rtx pti_reg;

  if (!indexed_or_indirect_operand (op1, TImode))
    {
      rtx new_addr = force_reg (Pmode, addr);
      operands[1] = op1 = change_address (op1, TImode, new_addr);
      addr = new_addr;
    }

  pti_mem = change_address (op1, PTImode, addr);
  pti_reg = gen_reg_rtx (PTImode);

  if (WORDS_BIG_ENDIAN)
    emit_move_insn (pti_reg, gen_lowpart (PTImode, op2));
  else
    {
      emit_move_insn (gen_lowpart (DImode, pti_reg), gen_highpart (DImode, op2));
      emit_move_insn (gen_highpart (DImode, pti_reg), gen_lowpart (DImode, op2));
    }

  emit_insn (gen_store_conditionalpti (op0, pti_mem, pti_reg));
  DONE;
})

(define_insn "store_conditionalpti"
  [(set (match_operand:CC 0 "cc_reg_operand" "=x")
	(unspec_volatile:CC [(const_int 0)] UNSPECV_SC))
   (set (match_operand:PTI 1 "indexed_or_indirect_operand" "=Z")
	(match_operand:PTI 2 "quad_int_reg_operand" "r"))]
  "TARGET_SYNC_TI && quad_int_reg_operand (operands[2], PTImode)"
  "stqcx. %2,%y1"
  [(set_attr "type" "store_c")])

(define_expand "atomic_compare_and_swap<mode>"
  [(match_operand:SI 0 "int_reg_operand")		;; bool out
   (match_operand:AINT 1 "int_reg_operand")		;; val out
   (match_operand:AINT 2 "memory_operand")		;; memory
   (match_operand:AINT 3 "reg_or_short_operand")	;; expected
   (match_operand:AINT 4 "int_reg_operand")		;; desired
   (match_operand:SI 5 "const_int_operand")		;; is_weak
   (match_operand:SI 6 "const_int_operand")		;; model succ
   (match_operand:SI 7 "const_int_operand")]		;; model fail
  ""
{
  rs6000_expand_atomic_compare_and_swap (operands);
  DONE;
})

(define_expand "atomic_exchange<mode>"
  [(match_operand:AINT 0 "int_reg_operand")		;; output
   (match_operand:AINT 1 "memory_operand")		;; memory
   (match_operand:AINT 2 "int_reg_operand")		;; input
   (match_operand:SI 3 "const_int_operand")]		;; model
  ""
{
  rs6000_expand_atomic_exchange (operands);
  DONE;
})

(define_expand "atomic_<fetchop_name><mode>"
  [(match_operand:AINT 0 "memory_operand")		;; memory
   (FETCHOP:AINT (match_dup 0)
     (match_operand:AINT 1 "<fetchop_pred>"))		;; operand
   (match_operand:SI 2 "const_int_operand")]		;; model
  ""
{
  rs6000_expand_atomic_op (<CODE>, operands[0], operands[1],
			   NULL_RTX, NULL_RTX, operands[2]);
  DONE;
})

(define_expand "atomic_nand<mode>"
  [(match_operand:AINT 0 "memory_operand")		;; memory
   (match_operand:AINT 1 "int_reg_operand")		;; operand
   (match_operand:SI 2 "const_int_operand")]		;; model
  ""
{
  rs6000_expand_atomic_op (NOT, operands[0], operands[1],
			   NULL_RTX, NULL_RTX, operands[2]);
  DONE;
})

(define_expand "atomic_fetch_<fetchop_name><mode>"
  [(match_operand:AINT 0 "int_reg_operand")		;; output
   (match_operand:AINT 1 "memory_operand")		;; memory
   (FETCHOP:AINT (match_dup 1)
     (match_operand:AINT 2 "<fetchop_pred>"))		;; operand
   (match_operand:SI 3 "const_int_operand")]		;; model
  ""
{ 
  rs6000_expand_atomic_op (<CODE>, operands[1], operands[2],
			   operands[0], NULL_RTX, operands[3]);
  DONE;
})

(define_expand "atomic_fetch_nand<mode>"
  [(match_operand:AINT 0 "int_reg_operand")		;; output
   (match_operand:AINT 1 "memory_operand")		;; memory
   (match_operand:AINT 2 "int_reg_operand")		;; operand
   (match_operand:SI 3 "const_int_operand")]		;; model
  ""
{
  rs6000_expand_atomic_op (NOT, operands[1], operands[2],
			   operands[0], NULL_RTX, operands[3]);
  DONE;
})

(define_expand "atomic_<fetchop_name>_fetch<mode>"
  [(match_operand:AINT 0 "int_reg_operand")		;; output
   (match_operand:AINT 1 "memory_operand")		;; memory
   (FETCHOP:AINT (match_dup 1)
     (match_operand:AINT 2 "<fetchop_pred>"))		;; operand
   (match_operand:SI 3 "const_int_operand")]		;; model
  ""
{
  rs6000_expand_atomic_op (<CODE>, operands[1], operands[2],
			   NULL_RTX, operands[0], operands[3]);
  DONE;
})

(define_expand "atomic_nand_fetch<mode>"
  [(match_operand:AINT 0 "int_reg_operand")		;; output
   (match_operand:AINT 1 "memory_operand")		;; memory
   (match_operand:AINT 2 "int_reg_operand")		;; operand
   (match_operand:SI 3 "const_int_operand")]		;; model
  ""
{
  rs6000_expand_atomic_op (NOT, operands[1], operands[2],
			   NULL_RTX, operands[0], operands[3]);
  DONE;
})
