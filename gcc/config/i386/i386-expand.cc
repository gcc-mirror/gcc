/* Copyright (C) 1988-2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "gimple.h"
#include "cfghooks.h"
#include "cfgloop.h"
#include "df.h"
#include "tm_p.h"
#include "stringpool.h"
#include "expmed.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "cfgbuild.h"
#include "alias.h"
#include "fold-const.h"
#include "attribs.h"
#include "calls.h"
#include "stor-layout.h"
#include "varasm.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "except.h"
#include "explow.h"
#include "expr.h"
#include "cfgrtl.h"
#include "common/common-target.h"
#include "langhooks.h"
#include "reload.h"
#include "gimplify.h"
#include "dwarf2.h"
#include "tm-constrs.h"
#include "cselib.h"
#include "sched-int.h"
#include "opts.h"
#include "tree-pass.h"
#include "context.h"
#include "pass_manager.h"
#include "target-globals.h"
#include "gimple-iterator.h"
#include "shrink-wrap.h"
#include "builtins.h"
#include "rtl-iter.h"
#include "tree-iterator.h"
#include "dbgcnt.h"
#include "case-cfn-macros.h"
#include "dojump.h"
#include "fold-const-call.h"
#include "tree-vrp.h"
#include "tree-ssanames.h"
#include "selftest.h"
#include "selftest-rtl.h"
#include "print-rtl.h"
#include "intl.h"
#include "ifcvt.h"
#include "symbol-summary.h"
#include "sreal.h"
#include "ipa-cp.h"
#include "ipa-prop.h"
#include "ipa-fnsummary.h"
#include "wide-int-bitmask.h"
#include "tree-vector-builder.h"
#include "debug.h"
#include "dwarf2out.h"
#include "i386-options.h"
#include "i386-builtins.h"
#include "i386-expand.h"
#include "asan.h"

/* Split one or more double-mode RTL references into pairs of half-mode
   references.  The RTL can be REG, offsettable MEM, integer constant, or
   CONST_DOUBLE.  "operands" is a pointer to an array of double-mode RTLs to
   split and "num" is its length.  lo_half and hi_half are output arrays
   that parallel "operands".  */

void
split_double_mode (machine_mode mode, rtx operands[],
		   int num, rtx lo_half[], rtx hi_half[])
{
  machine_mode half_mode;
  unsigned int byte;
  rtx mem_op = NULL_RTX;
  int mem_num = 0;

  switch (mode)
    {
    case E_TImode:
      half_mode = DImode;
      break;
    case E_DImode:
      half_mode = SImode;
      break;
    case E_P2HImode:
      half_mode = HImode;
      break;
    case E_P2QImode:
      half_mode = QImode;
      break;
    default:
      gcc_unreachable ();
    }

  byte = GET_MODE_SIZE (half_mode);

  while (num--)
    {
      rtx op = operands[num];

      /* simplify_subreg refuse to split volatile memory addresses,
         but we still have to handle it.  */
      if (MEM_P (op))
	{
	  if (mem_op && rtx_equal_p (op, mem_op))
	    {
	      lo_half[num] = lo_half[mem_num];
	      hi_half[num] = hi_half[mem_num];
	    }
	  else
	    {
	      mem_op = op;
	      mem_num = num;
	      lo_half[num] = adjust_address (op, half_mode, 0);
	      hi_half[num] = adjust_address (op, half_mode, byte);
	    }
	}
      else
	{
	  lo_half[num] = simplify_gen_subreg (half_mode, op,
					      GET_MODE (op) == VOIDmode
					      ? mode : GET_MODE (op), 0);

	  rtx tmp = simplify_gen_subreg (half_mode, op,
					 GET_MODE (op) == VOIDmode
					 ? mode : GET_MODE (op), byte);
	  /* simplify_gen_subreg will return NULL RTX for the
	     high half of the paradoxical subreg. */
	  hi_half[num] = tmp ? tmp : gen_reg_rtx (half_mode);
	}
    }
}

/* Emit the double word assignment DST = { LO, HI }.  */

void
split_double_concat (machine_mode mode, rtx dst, rtx lo, rtx hi)
{
  rtx dlo, dhi;
  int deleted_move_count = 0;
  split_double_mode (mode, &dst, 1, &dlo, &dhi);
  /* Constraints ensure that if both lo and hi are MEMs, then
     dst has early-clobber and thus addresses of MEMs don't use
     dlo/dhi registers.  Otherwise if at least one of li and hi are MEMs,
     dlo/dhi are registers.  */
  if (MEM_P (lo)
      && rtx_equal_p (dlo, hi)
      && reg_overlap_mentioned_p (dhi, lo))
    {
      /* If dlo is same as hi and lo's address uses dhi register,
	 code below would first emit_move_insn (dhi, hi)
	 and then emit_move_insn (dlo, lo).  But the former
	 would invalidate lo's address.  Load into dhi first,
	 then swap.  */
      emit_move_insn (dhi, lo);
      lo = dhi;
    }
  else if (MEM_P (hi)
	   && !MEM_P (lo)
	   && !rtx_equal_p (dlo, lo)
	   && reg_overlap_mentioned_p (dlo, hi))
    {
      /* In this case, code below would first emit_move_insn (dlo, lo)
	 and then emit_move_insn (dhi, hi).  But the former would
	 invalidate hi's address.  */
      if (rtx_equal_p (dhi, lo))
	{
	  /* We can't load into dhi first, so load into dlo
	     first and we'll swap.  */
	  emit_move_insn (dlo, hi);
	  hi = dlo;
	}
      else
	{
	  /* Load into dhi first.  */
	  emit_move_insn (dhi, hi);
	  hi = dhi;
	}
    }
  if (!rtx_equal_p (dlo, hi))
    {
      if (!rtx_equal_p (dlo, lo))
	emit_move_insn (dlo, lo);
      else
	deleted_move_count++;
      if (!rtx_equal_p (dhi, hi))
	emit_move_insn (dhi, hi);
      else
	deleted_move_count++;
    }
  else if (!rtx_equal_p (lo, dhi))
    {
      if (!rtx_equal_p (dhi, hi))
	emit_move_insn (dhi, hi);
      else
	deleted_move_count++;
      if (!rtx_equal_p (dlo, lo))
	emit_move_insn (dlo, lo);
      else
	deleted_move_count++;
    }
  else if (mode == TImode)
    emit_insn (gen_swapdi (dlo, dhi));
  else
    emit_insn (gen_swapsi (dlo, dhi));

  if (deleted_move_count == 2)
    emit_note (NOTE_INSN_DELETED);
}


/* Generate either "mov $0, reg" or "xor reg, reg", as appropriate
   for the target.  */

void
ix86_expand_clear (rtx dest)
{
  rtx tmp;

  /* We play register width games, which are only valid after reload.  */
  gcc_assert (reload_completed);

  /* Avoid HImode and its attendant prefix byte.  */
  if (GET_MODE_SIZE (GET_MODE (dest)) < 4)
    dest = gen_rtx_REG (SImode, REGNO (dest));
  tmp = gen_rtx_SET (dest, const0_rtx);

  if (!TARGET_USE_MOV0 || optimize_insn_for_size_p ())
    {
      rtx clob = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (CCmode, FLAGS_REG));
      tmp = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, tmp, clob));
    }

  emit_insn (tmp);
}

/* Return true if V can be broadcasted from an integer of WIDTH bits
   which is returned in VAL_BROADCAST.  Otherwise, return false.  */

static bool
ix86_broadcast (HOST_WIDE_INT v, unsigned int width,
		HOST_WIDE_INT &val_broadcast)
{
  wide_int val = wi::uhwi (v, HOST_BITS_PER_WIDE_INT);
  val_broadcast = wi::extract_uhwi (val, 0, width);
  for (unsigned int i = width; i < HOST_BITS_PER_WIDE_INT; i += width)
    {
      HOST_WIDE_INT each = wi::extract_uhwi (val, i, width);
      if (val_broadcast != each)
	return false;
    }
  val_broadcast = sext_hwi (val_broadcast, width);
  return true;
}

/* Convert the CONST_WIDE_INT operand OP to broadcast in MODE.  */

rtx
ix86_convert_const_wide_int_to_broadcast (machine_mode mode, rtx op)
{
  /* Don't use integer vector broadcast if we can't move from GPR to SSE
     register directly.  */
  if (!TARGET_INTER_UNIT_MOVES_TO_VEC)
    return nullptr;

  unsigned int msize = GET_MODE_SIZE (mode);

  /* Only optimized for vpbroadcast[bwsd]/vbroadcastss with xmm/ymm/zmm.  */
  if (msize != 16 && msize != 32 && msize != 64)
    return nullptr;

  /* Convert CONST_WIDE_INT to a non-standard SSE constant integer
     broadcast only if vector broadcast is available.  */
  if (!TARGET_AVX
      || !CONST_WIDE_INT_P (op)
      || standard_sse_constant_p (op, mode)
      || (CONST_WIDE_INT_NUNITS (op) * HOST_BITS_PER_WIDE_INT
	  != GET_MODE_BITSIZE (mode)))
    return nullptr;

  HOST_WIDE_INT val = CONST_WIDE_INT_ELT (op, 0);
  HOST_WIDE_INT val_broadcast;
  scalar_int_mode broadcast_mode;
  /* vpbroadcastb zmm requires TARGET_AVX512BW.  */
  if ((msize == 64 ? TARGET_AVX512BW : TARGET_AVX2)
      && ix86_broadcast (val, GET_MODE_BITSIZE (QImode),
			 val_broadcast))
    broadcast_mode = QImode;
  else if ((msize == 64 ? TARGET_AVX512BW : TARGET_AVX2)
	   && ix86_broadcast (val, GET_MODE_BITSIZE (HImode),
			      val_broadcast))
    broadcast_mode = HImode;
  /* vbroadcasts[sd] only support memory operand w/o AVX2.
     When msize == 16, pshufs is used for vec_duplicate.
     when msize == 64, vpbroadcastd is used, and TARGET_AVX512F must be existed.  */
  else if ((msize != 32 || TARGET_AVX2)
	   && ix86_broadcast (val, GET_MODE_BITSIZE (SImode),
			   val_broadcast))
    broadcast_mode = SImode;
  else if (TARGET_64BIT && (msize != 32 || TARGET_AVX2)
	   && ix86_broadcast (val, GET_MODE_BITSIZE (DImode),
			      val_broadcast))
    broadcast_mode = DImode;
  else
    return nullptr;

  /* Check if OP can be broadcasted from VAL.  */
  for (int i = 1; i < CONST_WIDE_INT_NUNITS (op); i++)
    if (val != CONST_WIDE_INT_ELT (op, i))
      return nullptr;

  unsigned int nunits = (GET_MODE_SIZE (mode)
			 / GET_MODE_SIZE (broadcast_mode));
  machine_mode vector_mode;
  if (!mode_for_vector (broadcast_mode, nunits).exists (&vector_mode))
    gcc_unreachable ();
  rtx target = gen_reg_rtx (vector_mode);
  bool ok = ix86_expand_vector_init_duplicate (false, vector_mode,
					       target,
					       GEN_INT (val_broadcast));
  if (!ok)
    return nullptr;
  target = lowpart_subreg (mode, target, vector_mode);
  return target;
}

void
ix86_expand_move (machine_mode mode, rtx operands[])
{
  rtx op0, op1;
  rtx tmp, addend = NULL_RTX;
  enum tls_model model;

  op0 = operands[0];
  op1 = operands[1];

  /* Avoid complex sets of likely spilled hard registers before reload.  */
  if (!ix86_hardreg_mov_ok (op0, op1))
    {
      tmp = gen_reg_rtx (mode);
      operands[0] = tmp;
      ix86_expand_move (mode, operands);
      operands[0] = op0;
      operands[1] = tmp;
      op1 = tmp;
    }

  switch (GET_CODE (op1))
    {
    case CONST:
      tmp = XEXP (op1, 0);

      if (GET_CODE (tmp) != PLUS
	  || GET_CODE (XEXP (tmp, 0)) != SYMBOL_REF)
	break;

      op1 = XEXP (tmp, 0);
      addend = XEXP (tmp, 1);
      /* FALLTHRU */

    case SYMBOL_REF:
      model = SYMBOL_REF_TLS_MODEL (op1);

      if (model)
	op1 = legitimize_tls_address (op1, model, true);
      else if (ix86_force_load_from_GOT_p (op1))
	{
	  /* Load the external function address via GOT slot to avoid PLT.  */
	  op1 = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, op1),
				(TARGET_64BIT
				 ? UNSPEC_GOTPCREL
				 : UNSPEC_GOT));
	  op1 = gen_rtx_CONST (Pmode, op1);
	  op1 = gen_const_mem (Pmode, op1);
	  set_mem_alias_set (op1, ix86_GOT_alias_set ());
	}
      else
	{
	  tmp = legitimize_pe_coff_symbol (op1, addend != NULL_RTX);
	  if (tmp)
	    {
	      op1 = tmp;
	      if (!addend)
		break;
	    }
	  else
	    {
	      op1 = operands[1];
	      break;
	    }
	}

      if (addend)
	{
	  op1 = force_operand (op1, NULL_RTX);
	  op1 = expand_simple_binop (Pmode, PLUS, op1, addend,
				     op0, 1, OPTAB_DIRECT);
	}
      else
	op1 = force_operand (op1, op0);

      if (op1 == op0)
	return;

      op1 = convert_to_mode (mode, op1, 1);

    default:
      break;

    case SUBREG:
      /* Transform TImode paradoxical SUBREG into zero_extendditi2.  */
      if (TARGET_64BIT
	  && mode == TImode
	  && SUBREG_P (op1)
	  && GET_MODE (SUBREG_REG (op1)) == DImode
	  && SUBREG_BYTE (op1) == 0)
	op1 = gen_rtx_ZERO_EXTEND (TImode, SUBREG_REG (op1));
      /* As not all values in XFmode are representable in real_value,
	 we might be called with unfoldable SUBREGs of constants.  */
      if (mode == XFmode
	  && CONSTANT_P (SUBREG_REG (op1))
	  && can_create_pseudo_p ())
	{
	  machine_mode imode = GET_MODE (SUBREG_REG (op1));
	  rtx r = force_const_mem (imode, SUBREG_REG (op1));
	  if (r)
	    r = validize_mem (r);
	  else
	    r = force_reg (imode, SUBREG_REG (op1));
	  op1 = simplify_gen_subreg (mode, r, imode, SUBREG_BYTE (op1));
	}
      break;
    }

  if ((flag_pic || MACHOPIC_INDIRECT)
      && symbolic_operand (op1, mode))
    {
#if TARGET_MACHO
      if (TARGET_MACHO && !TARGET_64BIT)
	{
	  /* dynamic-no-pic */
	  if (MACHOPIC_INDIRECT)
	    {
	      rtx temp = (op0 && REG_P (op0) && mode == Pmode)
			 ? op0 : gen_reg_rtx (Pmode);
	      op1 = machopic_indirect_data_reference (op1, temp);
	      if (MACHOPIC_PURE)
		op1 = machopic_legitimize_pic_address (op1, mode,
						       temp == op1 ? 0 : temp);
	    }
	  if (op0 != op1 && GET_CODE (op0) != MEM)
	    {
	      rtx insn = gen_rtx_SET (op0, op1);
	      emit_insn (insn);
	      return;
	    }
	}
#endif

      if (MEM_P (op0))
	op1 = force_reg (mode, op1);
      else if (!(TARGET_64BIT && x86_64_movabs_operand (op1, DImode)))
	{
	  rtx reg = can_create_pseudo_p () ? NULL_RTX : op0;
	  op1 = legitimize_pic_address (op1, reg);
	  if (op0 == op1)
	    return;
	  op1 = convert_to_mode (mode, op1, 1);
	}
    }
  else
    {
      if (MEM_P (op0)
	  && (PUSH_ROUNDING (GET_MODE_SIZE (mode)) != GET_MODE_SIZE (mode)
	      || !push_operand (op0, mode))
	  && MEM_P (op1))
	op1 = force_reg (mode, op1);

      if (push_operand (op0, mode)
	  && ! general_no_elim_operand (op1, mode))
	op1 = copy_to_mode_reg (mode, op1);

      /* Force large constants in 64bit compilation into register
	 to get them CSEed.  */
      if (can_create_pseudo_p ()
	  && (mode == DImode) && TARGET_64BIT
	  && immediate_operand (op1, mode)
	  && !x86_64_zext_immediate_operand (op1, VOIDmode)
	  && !register_operand (op0, mode)
	  && optimize)
	op1 = copy_to_mode_reg (mode, op1);

      if (can_create_pseudo_p ())
	{
	  if (CONST_DOUBLE_P (op1))
	    {
	      /* If we are loading a floating point constant to a
		 register, force the value to memory now, since we'll
		 get better code out the back end.  */

	      op1 = validize_mem (force_const_mem (mode, op1));
	      if (!register_operand (op0, mode))
		{
		  rtx temp = gen_reg_rtx (mode);
		  emit_insn (gen_rtx_SET (temp, op1));
		  emit_move_insn (op0, temp);
		  return;
		}
	    }
	}
    }

  /* Special case inserting 64-bit values into a TImode register.  */
  if (TARGET_64BIT
      /* Disable for -O0 (see PR110587) unless naked (PR110533).  */
      && (optimize || ix86_function_naked (current_function_decl))
      && (mode == DImode || mode == DFmode)
      && SUBREG_P (op0)
      && GET_MODE (SUBREG_REG (op0)) == TImode
      && REG_P (SUBREG_REG (op0))
      && REG_P (op1))
    {
      /* Use *insvti_lowpart_1 to set lowpart.  */
      if (SUBREG_BYTE (op0) == 0)
	{
	  wide_int mask = wi::mask (64, true, 128);
	  rtx tmp = immed_wide_int_const (mask, TImode);
	  op0 = SUBREG_REG (op0);
	  tmp = gen_rtx_AND (TImode, copy_rtx (op0), tmp);
	  if (mode == DFmode)
	    op1 = gen_lowpart (DImode, op1);
	  op1 = gen_rtx_ZERO_EXTEND (TImode, op1);
	  op1 = gen_rtx_IOR (TImode, tmp, op1);
	}
      /* Use *insvti_highpart_1 to set highpart.  */
      else if (SUBREG_BYTE (op0) == 8)
	{
	  wide_int mask = wi::mask (64, false, 128);
	  rtx tmp = immed_wide_int_const (mask, TImode);
	  op0 = SUBREG_REG (op0);
	  tmp = gen_rtx_AND (TImode, copy_rtx (op0), tmp);
	  if (mode == DFmode)
	    op1 = gen_lowpart (DImode, op1);
	  op1 = gen_rtx_ZERO_EXTEND (TImode, op1);
	  op1 = gen_rtx_ASHIFT (TImode, op1, GEN_INT (64));
	  op1 = gen_rtx_IOR (TImode, tmp, op1);
	}
    }

  emit_insn (gen_rtx_SET (op0, op1));
}

/* OP is a memref of CONST_VECTOR, return scalar constant mem
   if CONST_VECTOR is a vec_duplicate, else return NULL.  */
static rtx
ix86_broadcast_from_constant (machine_mode mode, rtx op)
{
  int nunits = GET_MODE_NUNITS (mode);
  if (nunits < 2)
    return nullptr;

  /* Don't use integer vector broadcast if we can't move from GPR to SSE
     register directly.  */
  if (!TARGET_INTER_UNIT_MOVES_TO_VEC
      && INTEGRAL_MODE_P (mode))
    return nullptr;

  /* Convert CONST_VECTOR to a non-standard SSE constant integer
     broadcast only if vector broadcast is available.  */
  if (standard_sse_constant_p (op, mode))
    return nullptr;

  if (GET_MODE_INNER (mode) == TImode)
    return nullptr;

  rtx constant = get_pool_constant (XEXP (op, 0));
  if (GET_CODE (constant) != CONST_VECTOR)
    return nullptr;

  /* There could be some rtx like
     (mem/u/c:V16QI (symbol_ref/u:DI ("*.LC1")))
     but with "*.LC1" refer to V2DI constant vector.  */
  if (GET_MODE (constant) != mode)
    {
      constant = simplify_subreg (mode, constant, GET_MODE (constant),
				  0);
      if (constant == nullptr || GET_CODE (constant) != CONST_VECTOR)
	return nullptr;
    }

  rtx first = XVECEXP (constant, 0, 0);

  for (int i = 1; i < nunits; ++i)
    {
      rtx tmp = XVECEXP (constant, 0, i);
      /* Vector duplicate value.  */
      if (!rtx_equal_p (tmp, first))
	return nullptr;
    }

  return first;
}

void
ix86_expand_vector_move (machine_mode mode, rtx operands[])
{
  rtx op0 = operands[0], op1 = operands[1];
  /* Use GET_MODE_BITSIZE instead of GET_MODE_ALIGNMENT for IA MCU
     psABI since the biggest alignment is 4 byte for IA MCU psABI.  */
  unsigned int align = (TARGET_IAMCU
			? GET_MODE_BITSIZE (mode)
			: GET_MODE_ALIGNMENT (mode));

  if (push_operand (op0, VOIDmode))
    op0 = emit_move_resolve_push (mode, op0);

  /* Force constants other than zero into memory.  We do not know how
     the instructions used to build constants modify the upper 64 bits
     of the register, once we have that information we may be able
     to handle some of them more efficiently.  */
  if (can_create_pseudo_p ()
      && (CONSTANT_P (op1)
	  || (SUBREG_P (op1)
	      && CONSTANT_P (SUBREG_REG (op1))))
      && ((register_operand (op0, mode)
	   && !standard_sse_constant_p (op1, mode))
	  /* ix86_expand_vector_move_misalign() does not like constants.  */
	  || (SSE_REG_MODE_P (mode)
	      && MEM_P (op0)
	      && MEM_ALIGN (op0) < align)))
    {
      if (SUBREG_P (op1))
	{
	  machine_mode imode = GET_MODE (SUBREG_REG (op1));
	  rtx r = force_const_mem (imode, SUBREG_REG (op1));
	  if (r)
	    r = validize_mem (r);
	  else
	    r = force_reg (imode, SUBREG_REG (op1));
	  op1 = simplify_gen_subreg (mode, r, imode, SUBREG_BYTE (op1));
	}
      else
	{
	  machine_mode mode = GET_MODE (op0);
	  rtx tmp = ix86_convert_const_wide_int_to_broadcast
	    (mode, op1);
	  if (tmp == nullptr)
	    op1 = validize_mem (force_const_mem (mode, op1));
	  else
	    op1 = tmp;
	}
    }

  if (can_create_pseudo_p ()
      && GET_MODE_SIZE (mode) >= 16
      && VECTOR_MODE_P (mode)
      && (MEM_P (op1)
	  && SYMBOL_REF_P (XEXP (op1, 0))
	  && CONSTANT_POOL_ADDRESS_P (XEXP (op1, 0))))
    {
      rtx first = ix86_broadcast_from_constant (mode, op1);
      if (first != nullptr)
	{
	  /* Broadcast to XMM/YMM/ZMM register from an integer
	     constant or scalar mem.  */
	  rtx tmp = gen_reg_rtx (mode);
	  if (FLOAT_MODE_P (mode))
	    first = force_const_mem (GET_MODE_INNER (mode), first);
	  bool ok = ix86_expand_vector_init_duplicate (false, mode,
						       tmp, first);
	  if (!ok && !TARGET_64BIT && GET_MODE_INNER (mode) == DImode)
	    {
	      first = force_const_mem (GET_MODE_INNER (mode), first);
	      ok = ix86_expand_vector_init_duplicate (false, mode,
						      tmp, first);
	    }
	  if (ok)
	    {
	      emit_move_insn (op0, tmp);
	      return;
	    }
	}
    }

  /* We need to check memory alignment for SSE mode since attribute
     can make operands unaligned.  */
  if (can_create_pseudo_p ()
      && SSE_REG_MODE_P (mode)
      && ((MEM_P (op0) && (MEM_ALIGN (op0) < align))
	  || (MEM_P (op1) && (MEM_ALIGN (op1) < align))))
    {
      rtx tmp[2];

      /* ix86_expand_vector_move_misalign() does not like both
	 arguments in memory.  */
      if (!register_operand (op0, mode)
	  && !register_operand (op1, mode))
	{
	  rtx scratch = gen_reg_rtx (mode);
	  emit_move_insn (scratch, op1);
	  op1 = scratch;
	}

      tmp[0] = op0; tmp[1] = op1;
      ix86_expand_vector_move_misalign (mode, tmp);
      return;
    }

  /* Special case TImode to 128-bit vector conversions via V2DI.  */
  if (VECTOR_MODE_P (mode)
      && GET_MODE_SIZE (mode) == 16
      && SUBREG_P (op1)
      && GET_MODE (SUBREG_REG (op1)) == TImode
      && TARGET_64BIT && TARGET_SSE
      && can_create_pseudo_p ())
    {
      rtx tmp = gen_reg_rtx (V2DImode);
      rtx lo = gen_reg_rtx (DImode);
      rtx hi = gen_reg_rtx (DImode);
      emit_move_insn (lo, gen_lowpart (DImode, SUBREG_REG (op1)));
      emit_move_insn (hi, gen_highpart (DImode, SUBREG_REG (op1)));
      emit_insn (gen_vec_concatv2di (tmp, lo, hi));
      emit_move_insn (op0, gen_lowpart (mode, tmp));
      return;
    }

  /* If operand0 is a hard register, make operand1 a pseudo.  */
  if (can_create_pseudo_p ()
      && !ix86_hardreg_mov_ok (op0, op1))
    {
      rtx tmp = gen_reg_rtx (GET_MODE (op0));
      emit_move_insn (tmp, op1);
      emit_move_insn (op0, tmp);
      return;
    }

  /* Make operand1 a register if it isn't already.  */
  if (can_create_pseudo_p ()
      && !register_operand (op0, mode)
      && !register_operand (op1, mode))
    {
      rtx tmp = gen_reg_rtx (GET_MODE (op0));
      emit_move_insn (tmp, op1);
      emit_move_insn (op0, tmp);
      return;
    }

  emit_insn (gen_rtx_SET (op0, op1));
}

/* Split 32-byte AVX unaligned load and store if needed.  */

static void
ix86_avx256_split_vector_move_misalign (rtx op0, rtx op1)
{
  rtx m;
  rtx (*extract) (rtx, rtx, rtx);
  machine_mode mode;

  if ((MEM_P (op1) && !TARGET_AVX256_SPLIT_UNALIGNED_LOAD)
      || (MEM_P (op0) && !TARGET_AVX256_SPLIT_UNALIGNED_STORE))
    {
      emit_insn (gen_rtx_SET (op0, op1));
      return;
    }

  rtx orig_op0 = NULL_RTX;
  mode = GET_MODE (op0);
  switch (GET_MODE_CLASS (mode))
    {
    case MODE_VECTOR_INT:
    case MODE_INT:
      if (mode != V32QImode)
	{
	  if (!MEM_P (op0))
	    {
	      orig_op0 = op0;
	      op0 = gen_reg_rtx (V32QImode);
	    }
	  else
	    op0 = gen_lowpart (V32QImode, op0);
	  op1 = gen_lowpart (V32QImode, op1);
	  mode = V32QImode;
	}
      break;
    case MODE_VECTOR_FLOAT:
      break;
    default:
      gcc_unreachable ();
    }

  switch (mode)
    {
    default:
      gcc_unreachable ();
    case E_V32QImode:
      extract = gen_avx_vextractf128v32qi;
      mode = V16QImode;
      break;
    case E_V16BFmode:
      extract = gen_avx_vextractf128v16bf;
      mode = V8BFmode;
      break;
    case E_V16HFmode:
      extract = gen_avx_vextractf128v16hf;
      mode = V8HFmode;
      break;
    case E_V8SFmode:
      extract = gen_avx_vextractf128v8sf;
      mode = V4SFmode;
      break;
    case E_V4DFmode:
      extract = gen_avx_vextractf128v4df;
      mode = V2DFmode;
      break;
    }

  if (MEM_P (op1))
    {
      rtx r = gen_reg_rtx (mode);
      m = adjust_address (op1, mode, 0);
      emit_move_insn (r, m);
      m = adjust_address (op1, mode, 16);
      r = gen_rtx_VEC_CONCAT (GET_MODE (op0), r, m);
      emit_move_insn (op0, r);
    }
  else if (MEM_P (op0))
    {
      m = adjust_address (op0, mode, 0);
      emit_insn (extract (m, op1, const0_rtx));
      m = adjust_address (op0, mode, 16);
      emit_insn (extract (m, copy_rtx (op1), const1_rtx));
    }
  else
    gcc_unreachable ();

  if (orig_op0)
    emit_move_insn (orig_op0, gen_lowpart (GET_MODE (orig_op0), op0));
}

/* Implement the movmisalign patterns for SSE.  Non-SSE modes go
   straight to ix86_expand_vector_move.  */
/* Code generation for scalar reg-reg moves of single and double precision data:
     if (x86_sse_partial_reg_dependency == true | x86_sse_split_regs == true)
       movaps reg, reg
     else
       movss reg, reg
     if (x86_sse_partial_reg_dependency == true)
       movapd reg, reg
     else
       movsd reg, reg

   Code generation for scalar loads of double precision data:
     if (x86_sse_split_regs == true)
       movlpd mem, reg      (gas syntax)
     else
       movsd mem, reg

   Code generation for unaligned packed loads of single precision data
   (x86_sse_unaligned_move_optimal overrides x86_sse_partial_reg_dependency):
     if (x86_sse_unaligned_move_optimal)
       movups mem, reg

     if (x86_sse_partial_reg_dependency == true)
       {
         xorps  reg, reg
         movlps mem, reg
         movhps mem+8, reg
       }
     else
       {
         movlps mem, reg
         movhps mem+8, reg
       }

   Code generation for unaligned packed loads of double precision data
   (x86_sse_unaligned_move_optimal overrides x86_sse_split_regs):
     if (x86_sse_unaligned_move_optimal)
       movupd mem, reg

     if (x86_sse_split_regs == true)
       {
         movlpd mem, reg
         movhpd mem+8, reg
       }
     else
       {
         movsd  mem, reg
         movhpd mem+8, reg
       }
 */

void
ix86_expand_vector_move_misalign (machine_mode mode, rtx operands[])
{
  rtx op0, op1, m;

  op0 = operands[0];
  op1 = operands[1];

  /* Use unaligned load/store for AVX512 or when optimizing for size.  */
  if (GET_MODE_SIZE (mode) == 64 || optimize_insn_for_size_p ())
    {
      emit_insn (gen_rtx_SET (op0, op1));
      return;
    }

  if (TARGET_AVX)
    {
      if (GET_MODE_SIZE (mode) == 32)
	ix86_avx256_split_vector_move_misalign (op0, op1);
      else
	/* Always use 128-bit mov<mode>_internal pattern for AVX.  */
	emit_insn (gen_rtx_SET (op0, op1));
      return;
    }

  if (TARGET_SSE_UNALIGNED_LOAD_OPTIMAL
      || TARGET_SSE_PACKED_SINGLE_INSN_OPTIMAL)
    {
      emit_insn (gen_rtx_SET (op0, op1));
      return;
    }

  /* ??? If we have typed data, then it would appear that using
     movdqu is the only way to get unaligned data loaded with
     integer type.  */
  if (TARGET_SSE2 && GET_MODE_CLASS (mode) == MODE_VECTOR_INT)
    {
      emit_insn (gen_rtx_SET (op0, op1));
      return;
    }

  if (MEM_P (op1))
    {
      if (TARGET_SSE2 && mode == V2DFmode)
        {
          rtx zero;

	  /* When SSE registers are split into halves, we can avoid
	     writing to the top half twice.  */
	  if (TARGET_SSE_SPLIT_REGS)
	    {
	      emit_clobber (op0);
	      zero = op0;
	    }
	  else
	    {
	      /* ??? Not sure about the best option for the Intel chips.
		 The following would seem to satisfy; the register is
		 entirely cleared, breaking the dependency chain.  We
		 then store to the upper half, with a dependency depth
		 of one.  A rumor has it that Intel recommends two movsd
		 followed by an unpacklpd, but this is unconfirmed.  And
		 given that the dependency depth of the unpacklpd would
		 still be one, I'm not sure why this would be better.  */
	      zero = CONST0_RTX (V2DFmode);
	    }

	  m = adjust_address (op1, DFmode, 0);
	  emit_insn (gen_sse2_loadlpd (op0, zero, m));
	  m = adjust_address (op1, DFmode, 8);
	  emit_insn (gen_sse2_loadhpd (op0, op0, m));
	}
      else
        {
	  rtx t;

	  if (mode != V4SFmode)
	    t = gen_reg_rtx (V4SFmode);
	  else
	    t = op0;
	    
	  if (TARGET_SSE_PARTIAL_REG_DEPENDENCY)
	    emit_move_insn (t, CONST0_RTX (V4SFmode));
	  else
	    emit_clobber (t);

	  m = adjust_address (op1, V2SFmode, 0);
	  emit_insn (gen_sse_loadlps (t, t, m));
	  m = adjust_address (op1, V2SFmode, 8);
	  emit_insn (gen_sse_loadhps (t, t, m));
	  if (mode != V4SFmode)
	    emit_move_insn (op0, gen_lowpart (mode, t));
	}
    }
  else if (MEM_P (op0))
    {
      if (TARGET_SSE2 && mode == V2DFmode)
	{
	  m = adjust_address (op0, DFmode, 0);
	  emit_insn (gen_sse2_storelpd (m, op1));
	  m = adjust_address (op0, DFmode, 8);
	  emit_insn (gen_sse2_storehpd (m, op1));
	}
      else
	{
	  if (mode != V4SFmode)
	    op1 = gen_lowpart (V4SFmode, op1);

	  m = adjust_address (op0, V2SFmode, 0);
	  emit_insn (gen_sse_storelps (m, op1));
	  m = adjust_address (op0, V2SFmode, 8);
	  emit_insn (gen_sse_storehps (m, copy_rtx (op1)));
	}
    }
  else
    gcc_unreachable ();
}

/* Move bits 64:95 to bits 32:63.  */

void
ix86_move_vector_high_sse_to_mmx (rtx op)
{
  rtx mask = gen_rtx_PARALLEL (VOIDmode,
			       gen_rtvec (4, GEN_INT (0), GEN_INT (2),
					  GEN_INT (0), GEN_INT (0)));
  rtx dest = lowpart_subreg (V4SImode, op, GET_MODE (op));
  op = gen_rtx_VEC_SELECT (V4SImode, dest, mask);
  rtx insn = gen_rtx_SET (dest, op);
  emit_insn (insn);
}

/* Split MMX pack with signed/unsigned saturation with SSE/SSE2.  */

void
ix86_split_mmx_pack (rtx operands[], enum rtx_code code)
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  rtx op2 = operands[2];
  rtx src;

  machine_mode dmode = GET_MODE (op0);
  machine_mode smode = GET_MODE (op1);
  machine_mode inner_dmode = GET_MODE_INNER (dmode);
  machine_mode inner_smode = GET_MODE_INNER (smode);

  /* Get the corresponding SSE mode for destination.  */
  int nunits = 16 / GET_MODE_SIZE (inner_dmode);
  machine_mode sse_dmode = mode_for_vector (GET_MODE_INNER (dmode),
					    nunits).require ();
  machine_mode sse_half_dmode = mode_for_vector (GET_MODE_INNER (dmode),
						 nunits / 2).require ();

  /* Get the corresponding SSE mode for source.  */
  nunits = 16 / GET_MODE_SIZE (inner_smode);
  machine_mode sse_smode = mode_for_vector (GET_MODE_INNER (smode),
					    nunits).require ();

  /* Generate SSE pack with signed/unsigned saturation.  */
  rtx dest = lowpart_subreg (sse_dmode, op0, GET_MODE (op0));
  op1 = lowpart_subreg (sse_smode, op1, GET_MODE (op1));
  op2 = lowpart_subreg (sse_smode, op2, GET_MODE (op2));

  /* paskusdw/packuswb does unsigned saturation of a signed source
     which is different from generic us_truncate RTX.  */
  if (code == US_TRUNCATE)
    src = gen_rtx_UNSPEC (sse_dmode,
			  gen_rtvec (2, op1, op2),
			  UNSPEC_US_TRUNCATE);
  else
    {
      op1 = gen_rtx_fmt_e (code, sse_half_dmode, op1);
      op2 = gen_rtx_fmt_e (code, sse_half_dmode, op2);
      src = gen_rtx_VEC_CONCAT (sse_dmode, op1, op2);
    }

  emit_move_insn (dest, src);

  ix86_move_vector_high_sse_to_mmx (op0);
}

/* Split MMX punpcklXX/punpckhXX with SSE punpcklXX.  This is also used
   for a full unpack of OPERANDS[1] and OPERANDS[2] into a wider
   OPERANDS[0].  */

void
ix86_split_mmx_punpck (rtx operands[], bool high_p)
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  rtx op2 = operands[2];
  machine_mode mode = GET_MODE (op1);
  rtx mask;
  /* The corresponding SSE mode.  */
  machine_mode sse_mode, double_sse_mode;

  switch (mode)
    {
    case E_V8QImode:
    case E_V4QImode:
    case E_V2QImode:
      sse_mode = V16QImode;
      double_sse_mode = V32QImode;
      mask = gen_rtx_PARALLEL (VOIDmode,
			       gen_rtvec (16,
					  GEN_INT (0), GEN_INT (16),
					  GEN_INT (1), GEN_INT (17),
					  GEN_INT (2), GEN_INT (18),
					  GEN_INT (3), GEN_INT (19),
					  GEN_INT (4), GEN_INT (20),
					  GEN_INT (5), GEN_INT (21),
					  GEN_INT (6), GEN_INT (22),
					  GEN_INT (7), GEN_INT (23)));
      break;

    case E_V4HImode:
    case E_V2HImode:
      sse_mode = V8HImode;
      double_sse_mode = V16HImode;
      mask = gen_rtx_PARALLEL (VOIDmode,
			       gen_rtvec (8,
					  GEN_INT (0), GEN_INT (8),
					  GEN_INT (1), GEN_INT (9),
					  GEN_INT (2), GEN_INT (10),
					  GEN_INT (3), GEN_INT (11)));
      break;

    case E_V2SImode:
      sse_mode = V4SImode;
      double_sse_mode = V8SImode;
      mask = gen_rtx_PARALLEL (VOIDmode,
			       gen_rtvec (4,
					  GEN_INT (0), GEN_INT (4),
					  GEN_INT (1), GEN_INT (5)));
      break;

    case E_V2SFmode:
      sse_mode = V4SFmode;
      double_sse_mode = V8SFmode;
      mask = gen_rtx_PARALLEL (VOIDmode,
			       gen_rtvec (4,
					  GEN_INT (0), GEN_INT (4),
					  GEN_INT (1), GEN_INT (5)));
      break;

    default:
      gcc_unreachable ();
    }

  /* Generate SSE punpcklXX.  */
  rtx dest = lowpart_subreg (sse_mode, op0, GET_MODE (op0));
  op1 = lowpart_subreg (sse_mode, op1, GET_MODE (op1));
  op2 = lowpart_subreg (sse_mode, op2, GET_MODE (op2));

  op1 = gen_rtx_VEC_CONCAT (double_sse_mode, op1, op2);
  op2 = gen_rtx_VEC_SELECT (sse_mode, op1, mask);
  rtx insn = gen_rtx_SET (dest, op2);
  emit_insn (insn);

  /* Move high bits to low bits.  */
  if (high_p)
    {
      if (sse_mode == V4SFmode)
	{
	  mask = gen_rtx_PARALLEL (VOIDmode,
				   gen_rtvec (4, GEN_INT (2), GEN_INT (3),
					      GEN_INT (4), GEN_INT (5)));
	  op2 = gen_rtx_VEC_CONCAT (V8SFmode, dest, dest);
	  op1 = gen_rtx_VEC_SELECT (V4SFmode, op2, mask);
	}
      else
	{
	  int sz = GET_MODE_SIZE (mode);

	  if (sz == 4)
	    mask = gen_rtx_PARALLEL (VOIDmode,
				     gen_rtvec (4, GEN_INT (1), GEN_INT (0),
						GEN_INT (0), GEN_INT (1)));
	  else if (sz == 8)
	    mask = gen_rtx_PARALLEL (VOIDmode,
				     gen_rtvec (4, GEN_INT (2), GEN_INT (3),
						GEN_INT (0), GEN_INT (1)));
	  else
	    gcc_unreachable ();

	  dest = lowpart_subreg (V4SImode, dest, GET_MODE (dest));
	  op1 = gen_rtx_VEC_SELECT (V4SImode, dest, mask);
	}

      insn = gen_rtx_SET (dest, op1);
      emit_insn (insn);
    }
}

/* Helper function of ix86_fixup_binary_operands to canonicalize
   operand order.  Returns true if the operands should be swapped.  */

static bool
ix86_swap_binary_operands_p (enum rtx_code code, machine_mode mode,
			     rtx operands[])
{
  rtx dst = operands[0];
  rtx src1 = operands[1];
  rtx src2 = operands[2];

  /* If the operation is not commutative, we can't do anything.  */
  if (GET_RTX_CLASS (code) != RTX_COMM_ARITH
      && GET_RTX_CLASS (code) != RTX_COMM_COMPARE)
    return false;

  /* Highest priority is that src1 should match dst.  */
  if (rtx_equal_p (dst, src1))
    return false;
  if (rtx_equal_p (dst, src2))
    return true;

  /* Next highest priority is that immediate constants come second.  */
  if (immediate_operand (src2, mode))
    return false;
  if (immediate_operand (src1, mode))
    return true;

  /* Lowest priority is that memory references should come second.  */
  if (MEM_P (src2))
    return false;
  if (MEM_P (src1))
    return true;

  return false;
}

/* Fix up OPERANDS to satisfy ix86_binary_operator_ok.  Return the
   destination to use for the operation.  If different from the true
   destination in operands[0], a copy operation will be required except
   under TARGET_APX_NDD.  */

rtx
ix86_fixup_binary_operands (enum rtx_code code, machine_mode mode,
			    rtx operands[], bool use_ndd)
{
  rtx dst = operands[0];
  rtx src1 = operands[1];
  rtx src2 = operands[2];

  /* Canonicalize operand order.  */
  if (ix86_swap_binary_operands_p (code, mode, operands))
    {
      /* It is invalid to swap operands of different modes.  */
      gcc_assert (GET_MODE (src1) == GET_MODE (src2));

      std::swap (src1, src2);
    }

  /* Both source operands cannot be in memory.  */
  if (MEM_P (src1) && MEM_P (src2))
    {
      /* Optimization: Only read from memory once.  */
      if (rtx_equal_p (src1, src2))
	{
	  src2 = force_reg (mode, src2);
	  src1 = src2;
	}
      else if (rtx_equal_p (dst, src1))
	src2 = force_reg (mode, src2);
      else
	src1 = force_reg (mode, src1);
    }

  /* If the destination is memory, and we do not have matching source
     operands, do things in registers.  */
  if (MEM_P (dst) && !rtx_equal_p (dst, src1))
    dst = gen_reg_rtx (mode);

  /* Source 1 cannot be a constant.  */
  if (CONSTANT_P (src1))
    src1 = force_reg (mode, src1);

  /* Source 1 cannot be a non-matching memory.  */
  if (!use_ndd && MEM_P (src1) && !rtx_equal_p (dst, src1))
    src1 = force_reg (mode, src1);

  /* Improve address combine.  */
  if (code == PLUS
      && GET_MODE_CLASS (mode) == MODE_INT
      && MEM_P (src2))
    src2 = force_reg (mode, src2);

  operands[1] = src1;
  operands[2] = src2;
  return dst;
}

/* Similarly, but assume that the destination has already been
   set up properly.  */

void
ix86_fixup_binary_operands_no_copy (enum rtx_code code,
				    machine_mode mode, rtx operands[],
				    bool use_ndd)
{
  rtx dst = ix86_fixup_binary_operands (code, mode, operands, use_ndd);
  gcc_assert (dst == operands[0]);
}

/* Attempt to expand a binary operator.  Make the expansion closer to the
   actual machine, then just general_operand, which will allow 3 separate
   memory references (one output, two input) in a single insn.  */

void
ix86_expand_binary_operator (enum rtx_code code, machine_mode mode,
			     rtx operands[], bool use_ndd)
{
  rtx src1, src2, dst, op, clob;

  dst = ix86_fixup_binary_operands (code, mode, operands, use_ndd);
  src1 = operands[1];
  src2 = operands[2];

 /* Emit the instruction.  */

  op = gen_rtx_SET (dst, gen_rtx_fmt_ee (code, mode, src1, src2));

  if (reload_completed
      && code == PLUS
      && !rtx_equal_p (dst, src1)
      && !use_ndd)
    {
      /* This is going to be an LEA; avoid splitting it later.  */
      emit_insn (op);
    }
  else
    {
      clob = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (CCmode, FLAGS_REG));
      emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, op, clob)));
    }

  /* Fix up the destination if needed.  */
  if (dst != operands[0])
    emit_move_insn (operands[0], dst);
}

/* Expand vector logical operation CODE (AND, IOR, XOR) in MODE with
   the given OPERANDS.  */

void
ix86_expand_vector_logical_operator (enum rtx_code code, machine_mode mode,
				     rtx operands[])
{
  rtx op1 = NULL_RTX, op2 = NULL_RTX;
  if (SUBREG_P (operands[1]))
    {
      op1 = operands[1];
      op2 = operands[2];
    }
  else if (SUBREG_P (operands[2]))
    {
      op1 = operands[2];
      op2 = operands[1];
    }
  /* Optimize (__m128i) d | (__m128i) e and similar code
     when d and e are float vectors into float vector logical
     insn.  In C/C++ without using intrinsics there is no other way
     to express vector logical operation on float vectors than
     to cast them temporarily to integer vectors.  */
  if (op1
      && !TARGET_SSE_PACKED_SINGLE_INSN_OPTIMAL
      && (SUBREG_P (op2) || GET_CODE (op2) == CONST_VECTOR)
      && GET_MODE_CLASS (GET_MODE (SUBREG_REG (op1))) == MODE_VECTOR_FLOAT
      && GET_MODE_SIZE (GET_MODE (SUBREG_REG (op1))) == GET_MODE_SIZE (mode)
      && SUBREG_BYTE (op1) == 0
      && (GET_CODE (op2) == CONST_VECTOR
	  || (GET_MODE (SUBREG_REG (op1)) == GET_MODE (SUBREG_REG (op2))
	      && SUBREG_BYTE (op2) == 0))
      && can_create_pseudo_p ())
    {
      rtx dst;
      switch (GET_MODE (SUBREG_REG (op1)))
	{
	case E_V4SFmode:
	case E_V8SFmode:
	case E_V16SFmode:
	case E_V2DFmode:
	case E_V4DFmode:
	case E_V8DFmode:
	  dst = gen_reg_rtx (GET_MODE (SUBREG_REG (op1)));
	  if (GET_CODE (op2) == CONST_VECTOR)
	    {
	      op2 = gen_lowpart (GET_MODE (dst), op2);
	      op2 = force_reg (GET_MODE (dst), op2);
	    }
	  else
	    {
	      op1 = operands[1];
	      op2 = SUBREG_REG (operands[2]);
	      if (!vector_operand (op2, GET_MODE (dst)))
		op2 = force_reg (GET_MODE (dst), op2);
	    }
	  op1 = SUBREG_REG (op1);
	  if (!vector_operand (op1, GET_MODE (dst)))
	    op1 = force_reg (GET_MODE (dst), op1);
	  emit_insn (gen_rtx_SET (dst,
				  gen_rtx_fmt_ee (code, GET_MODE (dst),
						  op1, op2)));
	  emit_move_insn (operands[0], gen_lowpart (mode, dst));
	  return;
	default:
	  break;
	}
    }
  if (!vector_operand (operands[1], mode))
    operands[1] = force_reg (mode, operands[1]);
  if (!vector_operand (operands[2], mode))
    operands[2] = force_reg (mode, operands[2]);
  ix86_fixup_binary_operands_no_copy (code, mode, operands);
  emit_insn (gen_rtx_SET (operands[0],
			  gen_rtx_fmt_ee (code, mode, operands[1],
					  operands[2])));
}

/* Return TRUE or FALSE depending on whether the binary operator meets the
   appropriate constraints.  */

bool
ix86_binary_operator_ok (enum rtx_code code, machine_mode mode,
			 rtx operands[3], bool use_ndd)
{
  rtx dst = operands[0];
  rtx src1 = operands[1];
  rtx src2 = operands[2];

  /* Both source operands cannot be in memory.  */
  if ((MEM_P (src1) || bcst_mem_operand (src1, mode))
      && (MEM_P (src2) || bcst_mem_operand (src2, mode)))
    return false;

  /* Canonicalize operand order for commutative operators.  */
  if (ix86_swap_binary_operands_p (code, mode, operands))
    std::swap (src1, src2);

  /* If the destination is memory, we must have a matching source operand.  */
  if (MEM_P (dst) && !rtx_equal_p (dst, src1))
    return false;

  /* Source 1 cannot be a constant.  */
  if (CONSTANT_P (src1))
    return false;

  /* Source 1 cannot be a non-matching memory.  */
  if (!use_ndd && MEM_P (src1) && !rtx_equal_p (dst, src1))
    /* Support "andhi/andsi/anddi" as a zero-extending move.  */
    return (code == AND
	    && (mode == HImode
		|| mode == SImode
		|| (TARGET_64BIT && mode == DImode))
	    && satisfies_constraint_L (src2));

  return true;
}

/* Attempt to expand a unary operator.  Make the expansion closer to the
   actual machine, then just general_operand, which will allow 2 separate
   memory references (one output, one input) in a single insn.  */

void
ix86_expand_unary_operator (enum rtx_code code, machine_mode mode,
			    rtx operands[], bool use_ndd)
{
  bool matching_memory = false;
  rtx src, dst, op, clob;

  dst = operands[0];
  src = operands[1];

  /* If the destination is memory, and we do not have matching source
     operands, do things in registers.  */
  if (MEM_P (dst))
    {
      if (rtx_equal_p (dst, src))
	matching_memory = true;
      else
	dst = gen_reg_rtx (mode);
    }

  /* When source operand is memory, destination must match.  */
  if (!use_ndd && MEM_P (src) && !matching_memory)
    src = force_reg (mode, src);

  /* Emit the instruction.  */

  op = gen_rtx_SET (dst, gen_rtx_fmt_e (code, mode, src));

  if (code == NOT)
    emit_insn (op);
  else
    {
      clob = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (CCmode, FLAGS_REG));
      emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, op, clob)));
    }

  /* Fix up the destination if needed.  */
  if (dst != operands[0])
    emit_move_insn (operands[0], dst);
}

/* Return TRUE or FALSE depending on whether the unary operator meets the
   appropriate constraints.  */

bool
ix86_unary_operator_ok (enum rtx_code,
			machine_mode,
			rtx operands[2],
			bool use_ndd)
{
  /* If one of operands is memory, source and destination must match.  */
  if ((MEM_P (operands[0])
       || (!use_ndd && MEM_P (operands[1])))
      && ! rtx_equal_p (operands[0], operands[1]))
    return false;
  return true;
}

/* Predict just emitted jump instruction to be taken with probability PROB.  */

static void
predict_jump (int prob)
{
  rtx_insn *insn = get_last_insn ();
  gcc_assert (JUMP_P (insn));
  add_reg_br_prob_note (insn, profile_probability::from_reg_br_prob_base (prob));
}

/* Split 32bit/64bit divmod with 8bit unsigned divmod if dividend and
   divisor are within the range [0-255].  */

void
ix86_split_idivmod (machine_mode mode, rtx operands[],
		    bool unsigned_p)
{
  rtx_code_label *end_label, *qimode_label;
  rtx div, mod;
  rtx_insn *insn;
  rtx scratch, tmp0, tmp1, tmp2;
  rtx (*gen_divmod4_1) (rtx, rtx, rtx, rtx);

  operands[2] = force_reg (mode, operands[2]);
  operands[3] = force_reg (mode, operands[3]);

  switch (mode)
    {
    case E_SImode:
      if (GET_MODE (operands[0]) == SImode)
	{
	  if (GET_MODE (operands[1]) == SImode)
	    gen_divmod4_1 = unsigned_p ? gen_udivmodsi4_1 : gen_divmodsi4_1;
	  else
	    gen_divmod4_1
	      = unsigned_p ? gen_udivmodsi4_zext_2 : gen_divmodsi4_zext_2;
	}
      else
	gen_divmod4_1
	  = unsigned_p ? gen_udivmodsi4_zext_1 : gen_divmodsi4_zext_1;
      break;

    case E_DImode:
      gen_divmod4_1 = unsigned_p ? gen_udivmoddi4_1 : gen_divmoddi4_1;
      break;

    default:
      gcc_unreachable ();
    }

  end_label = gen_label_rtx ();
  qimode_label = gen_label_rtx ();

  scratch = gen_reg_rtx (mode);

  /* Use 8bit unsigned divimod if dividend and divisor are within
     the range [0-255].  */
  emit_move_insn (scratch, operands[2]);
  scratch = expand_simple_binop (mode, IOR, scratch, operands[3],
				 scratch, 1, OPTAB_DIRECT);
  emit_insn (gen_test_ccno_1 (mode, scratch, GEN_INT (-0x100)));
  tmp0 = gen_rtx_REG (CCNOmode, FLAGS_REG);
  tmp0 = gen_rtx_EQ (VOIDmode, tmp0, const0_rtx);
  tmp0 = gen_rtx_IF_THEN_ELSE (VOIDmode, tmp0,
			       gen_rtx_LABEL_REF (VOIDmode, qimode_label),
			       pc_rtx);
  insn = emit_jump_insn (gen_rtx_SET (pc_rtx, tmp0));
  predict_jump (REG_BR_PROB_BASE * 50 / 100);
  JUMP_LABEL (insn) = qimode_label;

  /* Generate original signed/unsigned divimod.  */
  emit_insn (gen_divmod4_1 (operands[0], operands[1],
			    operands[2], operands[3]));

  /* Branch to the end.  */
  emit_jump_insn (gen_jump (end_label));
  emit_barrier ();

  /* Generate 8bit unsigned divide.  */
  emit_label (qimode_label);
  /* Don't use operands[0] for result of 8bit divide since not all
     registers support QImode ZERO_EXTRACT.  */
  tmp0 = lowpart_subreg (HImode, scratch, mode);
  tmp1 = lowpart_subreg (HImode, operands[2], mode);
  tmp2 = lowpart_subreg (QImode, operands[3], mode);
  emit_insn (gen_udivmodhiqi3 (tmp0, tmp1, tmp2));

  if (unsigned_p)
    {
      div = gen_rtx_UDIV (mode, operands[2], operands[3]);
      mod = gen_rtx_UMOD (mode, operands[2], operands[3]);
    }
  else
    {
      div = gen_rtx_DIV (mode, operands[2], operands[3]);
      mod = gen_rtx_MOD (mode, operands[2], operands[3]);
    }
  if (mode == SImode)
    {
      if (GET_MODE (operands[0]) != SImode)
	div = gen_rtx_ZERO_EXTEND (DImode, div);
      if (GET_MODE (operands[1]) != SImode)
	mod = gen_rtx_ZERO_EXTEND (DImode, mod);
    }

  /* Extract remainder from AH.  */
  scratch = gen_lowpart (GET_MODE (operands[1]), scratch);
  tmp1 = gen_rtx_ZERO_EXTRACT (GET_MODE (operands[1]), scratch,
			       GEN_INT (8), GEN_INT (8));
  insn = emit_move_insn (operands[1], tmp1);
  set_unique_reg_note (insn, REG_EQUAL, mod);

  /* Zero extend quotient from AL.  */
  tmp1 = gen_lowpart (QImode, tmp0);
  insn = emit_insn (gen_extend_insn
		    (operands[0], tmp1,
		     GET_MODE (operands[0]), QImode, 1));
  set_unique_reg_note (insn, REG_EQUAL, div);

  emit_label (end_label);
}

/* Emit x86 binary operand CODE in mode MODE, where the first operand
   matches destination.  RTX includes clobber of FLAGS_REG.  */

void
ix86_emit_binop (enum rtx_code code, machine_mode mode,
		 rtx dst, rtx src)
{
  rtx op, clob;

  op = gen_rtx_SET (dst, gen_rtx_fmt_ee (code, mode, dst, src));
  clob = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (CCmode, FLAGS_REG));
  
  emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, op, clob)));
}

/* Return true if regno1 def is nearest to the insn.  */

static bool
find_nearest_reg_def (rtx_insn *insn, int regno1, int regno2)
{
  rtx_insn *prev = insn;
  rtx_insn *start = BB_HEAD (BLOCK_FOR_INSN (insn));

  if (insn == start)
    return false;
  while (prev && prev != start)
    {
      if (!INSN_P (prev) || !NONDEBUG_INSN_P (prev))
	{
	  prev = PREV_INSN (prev);
	  continue;
	}
      if (insn_defines_reg (regno1, INVALID_REGNUM, prev))
	return true;
      else if (insn_defines_reg (regno2, INVALID_REGNUM, prev))
	return false;
      prev = PREV_INSN (prev);
    }

  /* None of the regs is defined in the bb.  */
  return false;
}

/* INSN_UID of the last insn emitted by zero store peephole2s.  */
int ix86_last_zero_store_uid;

/* Split lea instructions into a sequence of instructions
   which are executed on ALU to avoid AGU stalls.
   It is assumed that it is allowed to clobber flags register
   at lea position.  */

void
ix86_split_lea_for_addr (rtx_insn *insn, rtx operands[], machine_mode mode)
{
  unsigned int regno0, regno1, regno2;
  struct ix86_address parts;
  rtx target, tmp;
  int ok, adds;

  ok = ix86_decompose_address (operands[1], &parts);
  gcc_assert (ok);

  target = gen_lowpart (mode, operands[0]);

  regno0 = true_regnum (target);
  regno1 = INVALID_REGNUM;
  regno2 = INVALID_REGNUM;

  if (parts.base)
    {
      parts.base = gen_lowpart (mode, parts.base);
      regno1 = true_regnum (parts.base);
    }

  if (parts.index)
    {
      parts.index = gen_lowpart (mode, parts.index);
      regno2 = true_regnum (parts.index);
    }

  if (parts.disp)
    parts.disp = gen_lowpart (mode, parts.disp);

  if (parts.scale > 1)
    {
      /* Case r1 = r1 + ...  */
      if (regno1 == regno0)
	{
	  /* If we have a case r1 = r1 + C * r2 then we
	     should use multiplication which is very
	     expensive.  Assume cost model is wrong if we
	     have such case here.  */
	  gcc_assert (regno2 != regno0);

	  for (adds = parts.scale; adds > 0; adds--)
	    ix86_emit_binop (PLUS, mode, target, parts.index);
	}
      else
	{
	  /* r1 = r2 + r3 * C case.  Need to move r3 into r1.  */
	  if (regno0 != regno2)
	    emit_insn (gen_rtx_SET (target, parts.index));

	  /* Use shift for scaling, but emit it as MULT instead
	     to avoid it being immediately peephole2 optimized back
	     into lea.  */
	  ix86_emit_binop (MULT, mode, target, GEN_INT (parts.scale));

	  if (parts.base)
	    ix86_emit_binop (PLUS, mode, target, parts.base);

	  if (parts.disp && parts.disp != const0_rtx)
	    ix86_emit_binop (PLUS, mode, target, parts.disp);
	}
    }
  else if (!parts.base && !parts.index)
    {
      gcc_assert(parts.disp);
      emit_insn (gen_rtx_SET (target, parts.disp));
    }
  else
    {
      if (!parts.base)
	{
	  if (regno0 != regno2)
	    emit_insn (gen_rtx_SET (target, parts.index));
	}
      else if (!parts.index)
	{
	  if (regno0 != regno1)
	    emit_insn (gen_rtx_SET (target, parts.base));
	}
      else
	{
	  if (regno0 == regno1)
	    tmp = parts.index;
	  else if (regno0 == regno2)
	    tmp = parts.base;
	  else
	    {
	      rtx tmp1;

	      /* Find better operand for SET instruction, depending
		 on which definition is farther from the insn.  */
	      if (find_nearest_reg_def (insn, regno1, regno2))
		tmp = parts.index, tmp1 = parts.base;
	      else
		tmp = parts.base, tmp1 = parts.index;

	      emit_insn (gen_rtx_SET (target, tmp));

	      if (parts.disp && parts.disp != const0_rtx)
		ix86_emit_binop (PLUS, mode, target, parts.disp);

	      ix86_emit_binop (PLUS, mode, target, tmp1);
	      return;
	    }

	  ix86_emit_binop (PLUS, mode, target, tmp);
	}

      if (parts.disp && parts.disp != const0_rtx)
	ix86_emit_binop (PLUS, mode, target, parts.disp);
    }
}

/* Post-reload splitter for converting an SF or DFmode value in an
   SSE register into an unsigned SImode.  */

void
ix86_split_convert_uns_si_sse (rtx operands[])
{
  machine_mode vecmode;
  rtx value, large, zero_or_two31, input, two31, x;

  large = operands[1];
  zero_or_two31 = operands[2];
  input = operands[3];
  two31 = operands[4];
  vecmode = GET_MODE (large);
  value = gen_rtx_REG (vecmode, REGNO (operands[0]));

  /* Load up the value into the low element.  We must ensure that the other
     elements are valid floats -- zero is the easiest such value.  */
  if (MEM_P (input))
    {
      if (vecmode == V4SFmode)
	emit_insn (gen_vec_setv4sf_0 (value, CONST0_RTX (V4SFmode), input));
      else
	emit_insn (gen_sse2_loadlpd (value, CONST0_RTX (V2DFmode), input));
    }
  else
    {
      input = gen_rtx_REG (vecmode, REGNO (input));
      emit_move_insn (value, CONST0_RTX (vecmode));
      if (vecmode == V4SFmode)
	emit_insn (gen_sse_movss_v4sf (value, value, input));
      else
	emit_insn (gen_sse2_movsd_v2df (value, value, input));
    }

  emit_move_insn (large, two31);
  emit_move_insn (zero_or_two31, MEM_P (two31) ? large : two31);

  x = gen_rtx_fmt_ee (LE, vecmode, large, value);
  emit_insn (gen_rtx_SET (large, x));

  x = gen_rtx_AND (vecmode, zero_or_two31, large);
  emit_insn (gen_rtx_SET (zero_or_two31, x));

  x = gen_rtx_MINUS (vecmode, value, zero_or_two31);
  emit_insn (gen_rtx_SET (value, x));

  large = gen_rtx_REG (V4SImode, REGNO (large));
  emit_insn (gen_ashlv4si3 (large, large, GEN_INT (31)));

  x = gen_rtx_REG (V4SImode, REGNO (value));
  if (vecmode == V4SFmode)
    emit_insn (gen_fix_truncv4sfv4si2 (x, value));
  else
    emit_insn (gen_sse2_cvttpd2dq (x, value));
  value = x;

  emit_insn (gen_xorv4si3 (value, value, large));
}

static bool ix86_expand_vector_init_one_nonzero (bool mmx_ok,
						 machine_mode mode, rtx target,
						 rtx var, int one_var);

/* Convert an unsigned DImode value into a DFmode, using only SSE.
   Expects the 64-bit DImode to be supplied in a pair of integral
   registers.  Requires SSE2; will use SSE3 if available.  For x86_32,
   -mfpmath=sse, !optimize_size only.  */

void
ix86_expand_convert_uns_didf_sse (rtx target, rtx input)
{
  REAL_VALUE_TYPE bias_lo_rvt, bias_hi_rvt;
  rtx int_xmm, fp_xmm;
  rtx biases, exponents;
  rtx x;

  int_xmm = gen_reg_rtx (V4SImode);
  if (TARGET_INTER_UNIT_MOVES_TO_VEC)
    emit_insn (gen_movdi_to_sse (int_xmm, input));
  else if (TARGET_SSE_SPLIT_REGS)
    {
      emit_clobber (int_xmm);
      emit_move_insn (gen_lowpart (DImode, int_xmm), input);
    }
  else
    {
      x = gen_reg_rtx (V2DImode);
      ix86_expand_vector_init_one_nonzero (false, V2DImode, x, input, 0);
      emit_move_insn (int_xmm, gen_lowpart (V4SImode, x));
    }

  x = gen_rtx_CONST_VECTOR (V4SImode,
			    gen_rtvec (4, GEN_INT (0x43300000UL),
				       GEN_INT (0x45300000UL),
				       const0_rtx, const0_rtx));
  exponents = validize_mem (force_const_mem (V4SImode, x));

  /* int_xmm = {0x45300000UL, fp_xmm/hi, 0x43300000, fp_xmm/lo } */
  emit_insn (gen_vec_interleave_lowv4si (int_xmm, int_xmm, exponents));

  /* Concatenating (juxtaposing) (0x43300000UL ## fp_value_low_xmm)
     yields a valid DF value equal to (0x1.0p52 + double(fp_value_lo_xmm)).
     Similarly (0x45300000UL ## fp_value_hi_xmm) yields
     (0x1.0p84 + double(fp_value_hi_xmm)).
     Note these exponents differ by 32.  */

  fp_xmm = copy_to_mode_reg (V2DFmode, gen_lowpart (V2DFmode, int_xmm));

  /* Subtract off those 0x1.0p52 and 0x1.0p84 biases, to produce values
     in [0,2**32-1] and [0]+[2**32,2**64-1] respectively.  */
  real_ldexp (&bias_lo_rvt, &dconst1, 52);
  real_ldexp (&bias_hi_rvt, &dconst1, 84);
  biases = const_double_from_real_value (bias_lo_rvt, DFmode);
  x = const_double_from_real_value (bias_hi_rvt, DFmode);
  biases = gen_rtx_CONST_VECTOR (V2DFmode, gen_rtvec (2, biases, x));
  biases = validize_mem (force_const_mem (V2DFmode, biases));
  emit_insn (gen_subv2df3 (fp_xmm, fp_xmm, biases));

  /* Add the upper and lower DFmode values together.  */
  if (TARGET_SSE3)
    emit_insn (gen_sse3_haddv2df3 (fp_xmm, fp_xmm, fp_xmm));
  else
    {
      x = copy_to_mode_reg (V2DFmode, fp_xmm);
      emit_insn (gen_vec_interleave_highv2df (fp_xmm, fp_xmm, fp_xmm));
      emit_insn (gen_addv2df3 (fp_xmm, fp_xmm, x));
    }

  ix86_expand_vector_extract (false, target, fp_xmm, 0);
}

/* Not used, but eases macroization of patterns.  */
void
ix86_expand_convert_uns_sixf_sse (rtx, rtx)
{
  gcc_unreachable ();
}

static rtx ix86_expand_sse_fabs (rtx op0, rtx *smask);

/* Convert an unsigned SImode value into a DFmode.  Only currently used
   for SSE, but applicable anywhere.  */

void
ix86_expand_convert_uns_sidf_sse (rtx target, rtx input)
{
  REAL_VALUE_TYPE TWO31r;
  rtx x, fp;

  x = expand_simple_binop (SImode, PLUS, input, GEN_INT (-2147483647 - 1),
			   NULL, 1, OPTAB_DIRECT);

  fp = gen_reg_rtx (DFmode);
  emit_insn (gen_floatsidf2 (fp, x));

  real_ldexp (&TWO31r, &dconst1, 31);
  x = const_double_from_real_value (TWO31r, DFmode);

  x = expand_simple_binop (DFmode, PLUS, fp, x, target, 0, OPTAB_DIRECT);

  /* Remove the sign with FE_DOWNWARD, where x - x = -0.0.  */
  if (HONOR_SIGNED_ZEROS (DFmode) && flag_rounding_math)
    x = ix86_expand_sse_fabs (x, NULL);

  if (x != target)
    emit_move_insn (target, x);
}

/* Convert a signed DImode value into a DFmode.  Only used for SSE in
   32-bit mode; otherwise we have a direct convert instruction.  */

void
ix86_expand_convert_sign_didf_sse (rtx target, rtx input)
{
  REAL_VALUE_TYPE TWO32r;
  rtx fp_lo, fp_hi, x;

  fp_lo = gen_reg_rtx (DFmode);
  fp_hi = gen_reg_rtx (DFmode);

  emit_insn (gen_floatsidf2 (fp_hi, gen_highpart (SImode, input)));

  real_ldexp (&TWO32r, &dconst1, 32);
  x = const_double_from_real_value (TWO32r, DFmode);
  fp_hi = expand_simple_binop (DFmode, MULT, fp_hi, x, fp_hi, 0, OPTAB_DIRECT);

  ix86_expand_convert_uns_sidf_sse (fp_lo, gen_lowpart (SImode, input));

  x = expand_simple_binop (DFmode, PLUS, fp_hi, fp_lo, target,
			   0, OPTAB_DIRECT);
  if (x != target)
    emit_move_insn (target, x);
}

/* Convert an unsigned SImode value into a SFmode, using only SSE.
   For x86_32, -mfpmath=sse, !optimize_size only.  */
void
ix86_expand_convert_uns_sisf_sse (rtx target, rtx input)
{
  REAL_VALUE_TYPE ONE16r;
  rtx fp_hi, fp_lo, int_hi, int_lo, x;

  real_ldexp (&ONE16r, &dconst1, 16);
  x = const_double_from_real_value (ONE16r, SFmode);
  int_lo = expand_simple_binop (SImode, AND, input, GEN_INT(0xffff),
				      NULL, 0, OPTAB_DIRECT);
  int_hi = expand_simple_binop (SImode, LSHIFTRT, input, GEN_INT(16),
				      NULL, 0, OPTAB_DIRECT);
  fp_hi = gen_reg_rtx (SFmode);
  fp_lo = gen_reg_rtx (SFmode);
  emit_insn (gen_floatsisf2 (fp_hi, int_hi));
  emit_insn (gen_floatsisf2 (fp_lo, int_lo));
  if (TARGET_FMA)
    {
      x = validize_mem (force_const_mem (SFmode, x));
      fp_hi = gen_rtx_FMA (SFmode, fp_hi, x, fp_lo);
      emit_move_insn (target, fp_hi);
    }
  else
    {
      fp_hi = expand_simple_binop (SFmode, MULT, fp_hi, x, fp_hi,
				   0, OPTAB_DIRECT);
      fp_hi = expand_simple_binop (SFmode, PLUS, fp_hi, fp_lo, target,
				   0, OPTAB_DIRECT);
      if (!rtx_equal_p (target, fp_hi))
	emit_move_insn (target, fp_hi);
    }
}

/* floatunsv{4,8}siv{4,8}sf2 expander.  Expand code to convert
   a vector of unsigned ints VAL to vector of floats TARGET.  */

void
ix86_expand_vector_convert_uns_vsivsf (rtx target, rtx val)
{
  rtx tmp[8];
  REAL_VALUE_TYPE TWO16r;
  machine_mode intmode = GET_MODE (val);
  machine_mode fltmode = GET_MODE (target);
  rtx (*cvt) (rtx, rtx);

  if (intmode == V4SImode)
    cvt = gen_floatv4siv4sf2;
  else
    cvt = gen_floatv8siv8sf2;
  tmp[0] = ix86_build_const_vector (intmode, 1, GEN_INT (0xffff));
  tmp[0] = force_reg (intmode, tmp[0]);
  tmp[1] = expand_simple_binop (intmode, AND, val, tmp[0], NULL_RTX, 1,
				OPTAB_DIRECT);
  tmp[2] = expand_simple_binop (intmode, LSHIFTRT, val, GEN_INT (16),
				NULL_RTX, 1, OPTAB_DIRECT);
  tmp[3] = gen_reg_rtx (fltmode);
  emit_insn (cvt (tmp[3], tmp[1]));
  tmp[4] = gen_reg_rtx (fltmode);
  emit_insn (cvt (tmp[4], tmp[2]));
  real_ldexp (&TWO16r, &dconst1, 16);
  tmp[5] = const_double_from_real_value (TWO16r, SFmode);
  tmp[5] = force_reg (fltmode, ix86_build_const_vector (fltmode, 1, tmp[5]));
  if (TARGET_FMA)
    {
      tmp[6] = gen_rtx_FMA (fltmode, tmp[4], tmp[5], tmp[3]);
      emit_move_insn (target, tmp[6]);
    }
  else
    {
      tmp[6] = expand_simple_binop (fltmode, MULT, tmp[4], tmp[5],
				    NULL_RTX, 1, OPTAB_DIRECT);
      tmp[7] = expand_simple_binop (fltmode, PLUS, tmp[3], tmp[6],
				    target, 1, OPTAB_DIRECT);
      if (tmp[7] != target)
	emit_move_insn (target, tmp[7]);
    }
}

/* Adjust a V*SFmode/V*DFmode value VAL so that *sfix_trunc* resp. fix_trunc*
   pattern can be used on it instead of fixuns_trunc*.
   This is done by doing just signed conversion if < 0x1p31, and otherwise by
   subtracting 0x1p31 first and xoring in 0x80000000 from *XORP afterwards.  */

rtx
ix86_expand_adjust_ufix_to_sfix_si (rtx val, rtx *xorp)
{
  REAL_VALUE_TYPE TWO31r;
  rtx two31r, tmp[4];
  machine_mode mode = GET_MODE (val);
  machine_mode scalarmode = GET_MODE_INNER (mode);
  machine_mode intmode = GET_MODE_SIZE (mode) == 32 ? V8SImode : V4SImode;
  rtx (*cmp) (rtx, rtx, rtx, rtx);
  int i;

  for (i = 0; i < 3; i++)
    tmp[i] = gen_reg_rtx (mode);
  real_ldexp (&TWO31r, &dconst1, 31);
  two31r = const_double_from_real_value (TWO31r, scalarmode);
  two31r = ix86_build_const_vector (mode, 1, two31r);
  two31r = force_reg (mode, two31r);
  switch (mode)
    {
    case E_V8SFmode: cmp = gen_avx_maskcmpv8sf3; break;
    case E_V4SFmode: cmp = gen_sse_maskcmpv4sf3; break;
    case E_V4DFmode: cmp = gen_avx_maskcmpv4df3; break;
    case E_V2DFmode: cmp = gen_sse2_maskcmpv2df3; break;
    default: gcc_unreachable ();
    }
  tmp[3] = gen_rtx_LE (mode, two31r, val);
  emit_insn (cmp (tmp[0], two31r, val, tmp[3]));
  tmp[1] = expand_simple_binop (mode, AND, tmp[0], two31r, tmp[1],
				0, OPTAB_DIRECT);
  if (intmode == V4SImode || TARGET_AVX2)
    *xorp = expand_simple_binop (intmode, ASHIFT,
				 gen_lowpart (intmode, tmp[0]),
				 GEN_INT (31), NULL_RTX, 0,
				 OPTAB_DIRECT);
  else
    {
      rtx two31 = gen_int_mode (HOST_WIDE_INT_1U << 31, SImode);
      two31 = ix86_build_const_vector (intmode, 1, two31);
      *xorp = expand_simple_binop (intmode, AND,
				   gen_lowpart (intmode, tmp[0]),
				   two31, NULL_RTX, 0,
				   OPTAB_DIRECT);
    }
  return expand_simple_binop (mode, MINUS, val, tmp[1], tmp[2],
			      0, OPTAB_DIRECT);
}

/* Generate code for floating point ABS or NEG.  */

void
ix86_expand_fp_absneg_operator (enum rtx_code code, machine_mode mode,
				rtx operands[])
{
  rtx set, dst, src;
  bool use_sse = false;
  bool vector_mode = VECTOR_MODE_P (mode);
  machine_mode vmode = mode;
  rtvec par;

  if (vector_mode || mode == TFmode || mode == HFmode)
    {
      use_sse = true;
      if (mode == HFmode)
	vmode = V8HFmode;
    }
  else if (TARGET_SSE_MATH)
    {
      use_sse = SSE_FLOAT_MODE_P (mode);
      if (mode == SFmode)
	vmode = V4SFmode;
      else if (mode == DFmode)
	vmode = V2DFmode;
    }

  dst = operands[0];
  src = operands[1];

  set = gen_rtx_fmt_e (code, mode, src);
  set = gen_rtx_SET (dst, set);

  if (use_sse)
    {
      rtx mask, use, clob;

      /* NEG and ABS performed with SSE use bitwise mask operations.
	 Create the appropriate mask now.  */
      mask = ix86_build_signbit_mask (vmode, vector_mode, code == ABS);
      use = gen_rtx_USE (VOIDmode, mask);
      if (vector_mode || mode == TFmode)
	par = gen_rtvec (2, set, use);
      else
	{
          clob = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (CCmode, FLAGS_REG));
	  par = gen_rtvec (3, set, use, clob);
        }
    }
  else
    {
      rtx clob;

      /* Changing of sign for FP values is doable using integer unit too.  */
      clob = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (CCmode, FLAGS_REG));
      par = gen_rtvec (2, set, clob);
    }

  emit_insn (gen_rtx_PARALLEL (VOIDmode, par));
}

/* Deconstruct a floating point ABS or NEG operation
   with integer registers into integer operations.  */

void
ix86_split_fp_absneg_operator (enum rtx_code code, machine_mode mode,
			       rtx operands[])
{
  enum rtx_code absneg_op;
  rtx dst, set;

  gcc_assert (operands_match_p (operands[0], operands[1]));

  switch (mode)
    {
    case E_SFmode:
      dst = gen_lowpart (SImode, operands[0]);

      if (code == ABS)
	{
	  set = gen_int_mode (0x7fffffff, SImode);
	  absneg_op = AND;
	}
      else
	{
	  set = gen_int_mode (0x80000000, SImode);
	  absneg_op = XOR;
	}
      set = gen_rtx_fmt_ee (absneg_op, SImode, dst, set);
      break;

    case E_DFmode:
      if (TARGET_64BIT)
	{
	  dst = gen_lowpart (DImode, operands[0]);
	  dst = gen_rtx_ZERO_EXTRACT (DImode, dst, const1_rtx, GEN_INT (63));

	  if (code == ABS)
	    set = const0_rtx;
	  else
	    set = gen_rtx_NOT (DImode, dst);
	}
      else
	{
	  dst = gen_highpart (SImode, operands[0]);

	  if (code == ABS)
	    {
	      set = gen_int_mode (0x7fffffff, SImode);
	      absneg_op = AND;
	    }
	  else
	    {
	      set = gen_int_mode (0x80000000, SImode);
	      absneg_op = XOR;
	    }
	  set = gen_rtx_fmt_ee (absneg_op, SImode, dst, set);
	}
      break;

    case E_XFmode:
      dst = gen_rtx_REG (SImode,
			 REGNO (operands[0]) + (TARGET_64BIT ? 1 : 2));
      if (code == ABS)
	{
	  set = GEN_INT (0x7fff);
	  absneg_op = AND;
	}
      else
	{
	  set = GEN_INT (0x8000);
	  absneg_op = XOR;
	}
      set = gen_rtx_fmt_ee (absneg_op, SImode, dst, set);
      break;

    default:
      gcc_unreachable ();
    }

  set = gen_rtx_SET (dst, set);

  rtx clob = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (CCmode, FLAGS_REG));
  rtvec par = gen_rtvec (2, set, clob);

  emit_insn (gen_rtx_PARALLEL (VOIDmode, par));
}

/* Expand a copysign operation.  Special case operand 0 being a constant.  */

void
ix86_expand_copysign (rtx operands[])
{
  machine_mode mode, vmode;
  rtx dest, vdest, op0, op1, mask, op2, op3;

  mode = GET_MODE (operands[0]);

  if (mode == HFmode)
    vmode = V8HFmode;
  else if (mode == SFmode)
    vmode = V4SFmode;
  else if (mode == DFmode)
    vmode = V2DFmode;
  else if (mode == TFmode)
    vmode = mode;
  else
    gcc_unreachable ();

  if (rtx_equal_p (operands[1], operands[2]))
    {
      emit_move_insn (operands[0], operands[1]);
      return;
    }

  dest = operands[0];
  vdest = lowpart_subreg (vmode, dest, mode);
  if (vdest == NULL_RTX)
    vdest = gen_reg_rtx (vmode);
  else
    dest = NULL_RTX;
  op1 = lowpart_subreg (vmode, force_reg (mode, operands[2]), mode);
  mask = ix86_build_signbit_mask (vmode, TARGET_AVX512F && mode != HFmode, 0);

  if (CONST_DOUBLE_P (operands[1]))
    {
      op0 = simplify_unary_operation (ABS, mode, operands[1], mode);
      /* Optimize for 0, simplify b = copy_signf (0.0f, a) to b = mask & a.  */
      if (op0 == CONST0_RTX (mode))
	{
	  emit_move_insn (vdest, gen_rtx_AND (vmode, mask, op1));
	  if (dest)
	    emit_move_insn (dest, lowpart_subreg (mode, vdest, vmode));
	  return;
	}

      if (GET_MODE_SIZE (mode) < 16)
	op0 = ix86_build_const_vector (vmode, false, op0);
      op0 = force_reg (vmode, op0);
    }
  else
    op0 = lowpart_subreg (vmode, force_reg (mode, operands[1]), mode);

  op2 = gen_reg_rtx (vmode);
  op3 = gen_reg_rtx (vmode);
  emit_move_insn (op2, gen_rtx_AND (vmode,
				    gen_rtx_NOT (vmode, mask),
				    op0));
  emit_move_insn (op3, gen_rtx_AND (vmode, mask, op1));
  emit_move_insn (vdest, gen_rtx_IOR (vmode, op2, op3));
  if (dest)
    emit_move_insn (dest, lowpart_subreg (mode, vdest, vmode));
}

/* Expand an xorsign operation.  */

void
ix86_expand_xorsign (rtx operands[])
{
  machine_mode mode, vmode;
  rtx dest, vdest, op0, op1, mask, x, temp;

  dest = operands[0];
  op0 = operands[1];
  op1 = operands[2];

  mode = GET_MODE (dest);

  if (mode == HFmode)
    vmode = V8HFmode;
  else if (mode == SFmode)
    vmode = V4SFmode;
  else if (mode == DFmode)
    vmode = V2DFmode;
  else
    gcc_unreachable ();

  temp = gen_reg_rtx (vmode);
  mask = ix86_build_signbit_mask (vmode, 0, 0);

  op1 = lowpart_subreg (vmode, force_reg (mode, op1), mode);
  x = gen_rtx_AND (vmode, op1, mask);
  emit_insn (gen_rtx_SET (temp, x));

  op0 = lowpart_subreg (vmode, force_reg (mode, op0), mode);
  x = gen_rtx_XOR (vmode, temp, op0);

  vdest = lowpart_subreg (vmode, dest, mode);
  if (vdest == NULL_RTX)
    vdest = gen_reg_rtx (vmode);
  else
    dest = NULL_RTX;
  emit_insn (gen_rtx_SET (vdest, x));

  if (dest)
    emit_move_insn (dest, lowpart_subreg (mode, vdest, vmode));
}

static rtx ix86_expand_compare (enum rtx_code code, rtx op0, rtx op1);

void
ix86_expand_branch (enum rtx_code code, rtx op0, rtx op1, rtx label)
{
  machine_mode mode = GET_MODE (op0);
  rtx tmp;

  /* Handle special case - vector comparsion with boolean result, transform
     it using ptest instruction or vpcmpeq + kortest.  */
  if (GET_MODE_CLASS (mode) == MODE_VECTOR_INT
      || (mode == TImode && !TARGET_64BIT)
      || mode == OImode
      || GET_MODE_SIZE (mode) == 64)
    {
      unsigned msize = GET_MODE_SIZE (mode);
      machine_mode p_mode
	= msize == 64 ? V16SImode : msize == 32 ? V4DImode : V2DImode;
      /* kortest set CF when result is 0xFFFF (op0 == op1).  */
      rtx flag = gen_rtx_REG (msize == 64 ? CCCmode : CCZmode, FLAGS_REG);

      gcc_assert (code == EQ || code == NE);

      /* Using vpcmpeq zmm zmm k + kortest for 512-bit vectors.  */
      if (msize == 64)
	{
	  if (mode != V16SImode)
	    {
	      op0 = lowpart_subreg (p_mode, force_reg (mode, op0), mode);
	      op1 = lowpart_subreg (p_mode, force_reg (mode, op1), mode);
	    }

	  tmp = gen_reg_rtx (HImode);
	  emit_insn (gen_avx512f_cmpv16si3 (tmp, op0, op1, GEN_INT (0)));
	  emit_insn (gen_kortesthi_ccc (tmp, tmp));
	}
      /* Using ptest for 128/256-bit vectors.  */
      else
	{
	  if (GET_MODE_CLASS (mode) != MODE_VECTOR_INT)
	    {
	      op0 = lowpart_subreg (p_mode, force_reg (mode, op0), mode);
	      op1 = lowpart_subreg (p_mode, force_reg (mode, op1), mode);
	      mode = p_mode;
	    }

	  /* Generate XOR since we can't check that one operand is zero
	     vector.  */
	  tmp = gen_reg_rtx (mode);
	  rtx ops[3] = { tmp, op0, op1 };
	  ix86_expand_vector_logical_operator (XOR, mode, ops);
	  tmp = gen_lowpart (p_mode, tmp);
	  emit_insn (gen_rtx_SET (gen_rtx_REG (CCZmode, FLAGS_REG),
				  gen_rtx_UNSPEC (CCZmode,
						  gen_rtvec (2, tmp, tmp),
						  UNSPEC_PTEST)));
	}
      tmp = gen_rtx_fmt_ee (code, VOIDmode, flag, const0_rtx);
      tmp = gen_rtx_IF_THEN_ELSE (VOIDmode, tmp,
				  gen_rtx_LABEL_REF (VOIDmode, label),
				  pc_rtx);
      emit_jump_insn (gen_rtx_SET (pc_rtx, tmp));
      return;
    }

  switch (mode)
    {
    case E_HFmode:
    case E_SFmode:
    case E_DFmode:
    case E_XFmode:
    case E_QImode:
    case E_HImode:
    case E_SImode:
      simple:
      tmp = ix86_expand_compare (code, op0, op1);
      tmp = gen_rtx_IF_THEN_ELSE (VOIDmode, tmp,
				  gen_rtx_LABEL_REF (VOIDmode, label),
				  pc_rtx);
      emit_jump_insn (gen_rtx_SET (pc_rtx, tmp));
      return;

    case E_DImode:
      if (TARGET_64BIT)
	goto simple;
      /* FALLTHRU */
    case E_TImode:
      /* DI and TI mode equality/inequality comparisons may be performed
         on SSE registers.  Avoid splitting them, except when optimizing
	 for size.  */
      if ((code == EQ || code == NE)
	  && !optimize_insn_for_size_p ())
	goto simple;

      /* Expand DImode branch into multiple compare+branch.  */
      {
	rtx lo[2], hi[2];
	rtx_code_label *label2;
	enum rtx_code code1, code2, code3;
	machine_mode submode;

	if (CONSTANT_P (op0) && !CONSTANT_P (op1))
	  {
	    std::swap (op0, op1);
	    code = swap_condition (code);
	  }

	split_double_mode (mode, &op0, 1, lo+0, hi+0);
	split_double_mode (mode, &op1, 1, lo+1, hi+1);

	submode = mode == DImode ? SImode : DImode;

	/* If we are doing less-than or greater-or-equal-than,
	   op1 is a constant and the low word is zero, then we can just
	   examine the high word.  Similarly for low word -1 and
	   less-or-equal-than or greater-than.  */

	if (CONST_INT_P (hi[1]))
	  switch (code)
	    {
	    case LT: case LTU: case GE: case GEU:
	      if (lo[1] == const0_rtx)
		{
		  ix86_expand_branch (code, hi[0], hi[1], label);
		  return;
		}
	      break;
	    case LE: case LEU: case GT: case GTU:
	      if (lo[1] == constm1_rtx)
		{
		  ix86_expand_branch (code, hi[0], hi[1], label);
		  return;
		}
	      break;
	    default:
	      break;
	    }

	/* Emulate comparisons that do not depend on Zero flag with
	   double-word subtraction.  Note that only Overflow, Sign
	   and Carry flags are valid, so swap arguments and condition
	   of comparisons that would otherwise test Zero flag.  */

	switch (code)
	  {
	  case LE: case LEU: case GT: case GTU:
	    std::swap (lo[0], lo[1]);
	    std::swap (hi[0], hi[1]);
	    code = swap_condition (code);
	    /* FALLTHRU */

	  case LT: case LTU: case GE: case GEU:
	    {
	      bool uns = (code == LTU || code == GEU);
	      rtx (*sbb_insn) (machine_mode, rtx, rtx, rtx)
		= uns ? gen_sub3_carry_ccc : gen_sub3_carry_ccgz;

	      if (!nonimmediate_operand (lo[0], submode))
		lo[0] = force_reg (submode, lo[0]);
	      if (!x86_64_general_operand (lo[1], submode))
		lo[1] = force_reg (submode, lo[1]);

	      if (!register_operand (hi[0], submode))
		hi[0] = force_reg (submode, hi[0]);
	      if ((uns && !nonimmediate_operand (hi[1], submode))
		  || (!uns && !x86_64_general_operand (hi[1], submode)))
		hi[1] = force_reg (submode, hi[1]);

	      emit_insn (gen_cmp_1 (submode, lo[0], lo[1]));

	      tmp = gen_rtx_SCRATCH (submode);
	      emit_insn (sbb_insn (submode, tmp, hi[0], hi[1]));

	      tmp = gen_rtx_REG (uns ? CCCmode : CCGZmode, FLAGS_REG);
	      ix86_expand_branch (code, tmp, const0_rtx, label);
	      return;
	    }

	  default:
	    break;
	  }

	/* Otherwise, we need two or three jumps.  */

	label2 = gen_label_rtx ();

	code1 = code;
	code2 = swap_condition (code);
	code3 = unsigned_condition (code);

	switch (code)
	  {
	  case LT: case GT: case LTU: case GTU:
	    break;

	  case LE:   code1 = LT;  code2 = GT;  break;
	  case GE:   code1 = GT;  code2 = LT;  break;
	  case LEU:  code1 = LTU; code2 = GTU; break;
	  case GEU:  code1 = GTU; code2 = LTU; break;

	  case EQ:   code1 = UNKNOWN; code2 = NE;  break;
	  case NE:   code2 = UNKNOWN; break;

	  default:
	    gcc_unreachable ();
	  }

	/*
	 * a < b =>
	 *    if (hi(a) < hi(b)) goto true;
	 *    if (hi(a) > hi(b)) goto false;
	 *    if (lo(a) < lo(b)) goto true;
	 *  false:
	 */

	if (code1 != UNKNOWN)
	  ix86_expand_branch (code1, hi[0], hi[1], label);
	if (code2 != UNKNOWN)
	  ix86_expand_branch (code2, hi[0], hi[1], label2);

	ix86_expand_branch (code3, lo[0], lo[1], label);

	if (code2 != UNKNOWN)
	  emit_label (label2);
	return;
      }

    default:
      gcc_assert (GET_MODE_CLASS (GET_MODE (op0)) == MODE_CC);
      goto simple;
    }
}

/* Figure out whether to use unordered fp comparisons.  */

static bool
ix86_unordered_fp_compare (enum rtx_code code)
{
  if (!TARGET_IEEE_FP)
    return false;

  switch (code)
    {
    case LT:
    case LE:
    case GT:
    case GE:
    case LTGT:
      return false;

    case EQ:
    case NE:

    case UNORDERED:
    case ORDERED:
    case UNLT:
    case UNLE:
    case UNGT:
    case UNGE:
    case UNEQ:
      return true;

    default:
      gcc_unreachable ();
    }
}

/* Return a comparison we can do and that it is equivalent to
   swap_condition (code) apart possibly from orderedness.
   But, never change orderedness if TARGET_IEEE_FP, returning
   UNKNOWN in that case if necessary.  */

static enum rtx_code
ix86_fp_swap_condition (enum rtx_code code)
{
  switch (code)
    {
    case GT:                   /* GTU - CF=0 & ZF=0 */
      return TARGET_IEEE_FP ? UNKNOWN : UNLT;
    case GE:                   /* GEU - CF=0 */
      return TARGET_IEEE_FP ? UNKNOWN : UNLE;
    case UNLT:                 /* LTU - CF=1 */
      return TARGET_IEEE_FP ? UNKNOWN : GT;
    case UNLE:                 /* LEU - CF=1 | ZF=1 */
      return TARGET_IEEE_FP ? UNKNOWN : GE;
    default:
      return swap_condition (code);
    }
}

/* Return cost of comparison CODE using the best strategy for performance.
   All following functions do use number of instructions as a cost metrics.
   In future this should be tweaked to compute bytes for optimize_size and
   take into account performance of various instructions on various CPUs.  */

static int
ix86_fp_comparison_cost (enum rtx_code code)
{
  int arith_cost;

  /* The cost of code using bit-twiddling on %ah.  */
  switch (code)
    {
    case UNLE:
    case UNLT:
    case LTGT:
    case GT:
    case GE:
    case UNORDERED:
    case ORDERED:
    case UNEQ:
      arith_cost = 4;
      break;
    case LT:
    case NE:
    case EQ:
    case UNGE:
      arith_cost = TARGET_IEEE_FP ? 5 : 4;
      break;
    case LE:
    case UNGT:
      arith_cost = TARGET_IEEE_FP ? 6 : 4;
      break;
    default:
      gcc_unreachable ();
    }

  switch (ix86_fp_comparison_strategy (code))
    {
    case IX86_FPCMP_COMI:
      return arith_cost > 4 ? 3 : 2;
    case IX86_FPCMP_SAHF:
      return arith_cost > 4 ? 4 : 3;
    default:
      return arith_cost;
    }
}

/* Swap, force into registers, or otherwise massage the two operands
   to a fp comparison.  The operands are updated in place; the new
   comparison code is returned.  */

static enum rtx_code
ix86_prepare_fp_compare_args (enum rtx_code code, rtx *pop0, rtx *pop1)
{
  bool unordered_compare = ix86_unordered_fp_compare (code);
  rtx op0 = *pop0, op1 = *pop1;
  machine_mode op_mode = GET_MODE (op0);
  bool is_sse = SSE_FLOAT_MODE_SSEMATH_OR_HF_P (op_mode);

  if (op_mode == BFmode)
    {
      rtx op = gen_lowpart (HImode, op0);
      if (CONST_INT_P (op))
	op = simplify_const_unary_operation (FLOAT_EXTEND, SFmode,
					     op0, BFmode);
      else
	{
	  rtx t1 = gen_reg_rtx (SImode);
	  emit_insn (gen_zero_extendhisi2 (t1, op));
	  emit_insn (gen_ashlsi3 (t1, t1, GEN_INT (16)));
	  op = gen_lowpart (SFmode, t1);
	}
      *pop0 = op;
      op = gen_lowpart (HImode, op1);
      if (CONST_INT_P (op))
	op = simplify_const_unary_operation (FLOAT_EXTEND, SFmode,
					     op1, BFmode);
      else
	{
	  rtx t1 = gen_reg_rtx (SImode);
	  emit_insn (gen_zero_extendhisi2 (t1, op));
	  emit_insn (gen_ashlsi3 (t1, t1, GEN_INT (16)));
	  op = gen_lowpart (SFmode, t1);
	}
      *pop1 = op;
      return ix86_prepare_fp_compare_args (code, pop0, pop1);
    }

  /* All of the unordered compare instructions only work on registers.
     The same is true of the fcomi compare instructions.  The XFmode
     compare instructions require registers except when comparing
     against zero or when converting operand 1 from fixed point to
     floating point.  */

  if (!is_sse
      && (unordered_compare
	  || (op_mode == XFmode
	      && ! (standard_80387_constant_p (op0) == 1
		    || standard_80387_constant_p (op1) == 1)
	      && GET_CODE (op1) != FLOAT)
	  || ix86_fp_comparison_strategy (code) == IX86_FPCMP_COMI))
    {
      op0 = force_reg (op_mode, op0);
      op1 = force_reg (op_mode, op1);
    }
  else
    {
      /* %%% We only allow op1 in memory; op0 must be st(0).  So swap
	 things around if they appear profitable, otherwise force op0
	 into a register.  */

      if (standard_80387_constant_p (op0) == 0
	  || (MEM_P (op0)
	      && ! (standard_80387_constant_p (op1) == 0
		    || MEM_P (op1))))
	{
	  enum rtx_code new_code = ix86_fp_swap_condition (code);
	  if (new_code != UNKNOWN)
	    {
	      std::swap (op0, op1);
	      code = new_code;
	    }
	}

      if (!REG_P (op0))
	op0 = force_reg (op_mode, op0);

      if (CONSTANT_P (op1))
	{
	  int tmp = standard_80387_constant_p (op1);
	  if (tmp == 0)
	    op1 = validize_mem (force_const_mem (op_mode, op1));
	  else if (tmp == 1)
	    {
	      if (TARGET_CMOVE)
		op1 = force_reg (op_mode, op1);
	    }
	  else
	    op1 = force_reg (op_mode, op1);
	}
    }

  /* Try to rearrange the comparison to make it cheaper.  */
  if (ix86_fp_comparison_cost (code)
      > ix86_fp_comparison_cost (swap_condition (code))
      && (REG_P (op1) || can_create_pseudo_p ()))
    {
      std::swap (op0, op1);
      code = swap_condition (code);
      if (!REG_P (op0))
	op0 = force_reg (op_mode, op0);
    }

  *pop0 = op0;
  *pop1 = op1;
  return code;
}

/* Generate insn patterns to do a floating point compare of OPERANDS.  */

static rtx
ix86_expand_fp_compare (enum rtx_code code, rtx op0, rtx op1)
{
  bool unordered_compare = ix86_unordered_fp_compare (code);
  machine_mode cmp_mode;
  rtx tmp, scratch;

  code = ix86_prepare_fp_compare_args (code, &op0, &op1);

  tmp = gen_rtx_COMPARE (CCFPmode, op0, op1);
  if (unordered_compare)
    tmp = gen_rtx_UNSPEC (CCFPmode, gen_rtvec (1, tmp), UNSPEC_NOTRAP);

  /* Do fcomi/sahf based test when profitable.  */
  switch (ix86_fp_comparison_strategy (code))
    {
    case IX86_FPCMP_COMI:
      cmp_mode = CCFPmode;
      emit_insn (gen_rtx_SET (gen_rtx_REG (CCFPmode, FLAGS_REG), tmp));
      break;

    case IX86_FPCMP_SAHF:
      cmp_mode = CCFPmode;
      tmp = gen_rtx_UNSPEC (HImode, gen_rtvec (1, tmp), UNSPEC_FNSTSW);
      scratch = gen_reg_rtx (HImode);
      emit_insn (gen_rtx_SET (scratch, tmp));
      emit_insn (gen_x86_sahf_1 (scratch));
      break;

    case IX86_FPCMP_ARITH:
      cmp_mode = CCNOmode;
      tmp = gen_rtx_UNSPEC (HImode, gen_rtvec (1, tmp), UNSPEC_FNSTSW);
      scratch = gen_reg_rtx (HImode);
      emit_insn (gen_rtx_SET (scratch, tmp));

      /* In the unordered case, we have to check C2 for NaN's, which
	 doesn't happen to work out to anything nice combination-wise.
	 So do some bit twiddling on the value we've got in AH to come
	 up with an appropriate set of condition codes.  */

      switch (code)
	{
	case GT:
	case UNGT:
	  if (code == GT || !TARGET_IEEE_FP)
	    {
	      emit_insn (gen_testqi_ext_1_ccno (scratch, GEN_INT (0x45)));
	      code = EQ;
	    }
	  else
	    {
	      emit_insn (gen_andqi_ext_1 (scratch, scratch, GEN_INT (0x45)));
	      emit_insn (gen_addqi_ext_1 (scratch, scratch, constm1_rtx));
	      emit_insn (gen_cmpqi_ext_3 (scratch, GEN_INT (0x44)));
	      cmp_mode = CCmode;
	      code = GEU;
	    }
	  break;
	case LT:
	case UNLT:
	  if (code == LT && TARGET_IEEE_FP)
	    {
	      emit_insn (gen_andqi_ext_1 (scratch, scratch, GEN_INT (0x45)));
	      emit_insn (gen_cmpqi_ext_3 (scratch, const1_rtx));
	      cmp_mode = CCmode;
	      code = EQ;
	    }
	  else
	    {
	      emit_insn (gen_testqi_ext_1_ccno (scratch, const1_rtx));
	      code = NE;
	    }
	  break;
	case GE:
	case UNGE:
	  if (code == GE || !TARGET_IEEE_FP)
	    {
	      emit_insn (gen_testqi_ext_1_ccno (scratch, GEN_INT (0x05)));
	      code = EQ;
	    }
	  else
	    {
	      emit_insn (gen_andqi_ext_1 (scratch, scratch, GEN_INT (0x45)));
	      emit_insn (gen_xorqi_ext_1_cc (scratch, scratch, const1_rtx));
	      code = NE;
	    }
	  break;
	case LE:
	case UNLE:
	  if (code == LE && TARGET_IEEE_FP)
	    {
	      emit_insn (gen_andqi_ext_1 (scratch, scratch, GEN_INT (0x45)));
	      emit_insn (gen_addqi_ext_1 (scratch, scratch, constm1_rtx));
	      emit_insn (gen_cmpqi_ext_3 (scratch, GEN_INT (0x40)));
	      cmp_mode = CCmode;
	      code = LTU;
	    }
	  else
	    {
	      emit_insn (gen_testqi_ext_1_ccno (scratch, GEN_INT (0x45)));
	      code = NE;
	    }
	  break;
	case EQ:
	case UNEQ:
	  if (code == EQ && TARGET_IEEE_FP)
	    {
	      emit_insn (gen_andqi_ext_1 (scratch, scratch, GEN_INT (0x45)));
	      emit_insn (gen_cmpqi_ext_3 (scratch, GEN_INT (0x40)));
	      cmp_mode = CCmode;
	      code = EQ;
	    }
	  else
	    {
	      emit_insn (gen_testqi_ext_1_ccno (scratch, GEN_INT (0x40)));
	      code = NE;
	    }
	  break;
	case NE:
	case LTGT:
	  if (code == NE && TARGET_IEEE_FP)
	    {
	      emit_insn (gen_andqi_ext_1 (scratch, scratch, GEN_INT (0x45)));
	      emit_insn (gen_xorqi_ext_1_cc (scratch, scratch,
					     GEN_INT (0x40)));
	      code = NE;
	    }
	  else
	    {
	      emit_insn (gen_testqi_ext_1_ccno (scratch, GEN_INT (0x40)));
	      code = EQ;
	    }
	  break;

	case UNORDERED:
	  emit_insn (gen_testqi_ext_1_ccno (scratch, GEN_INT (0x04)));
	  code = NE;
	  break;
	case ORDERED:
	  emit_insn (gen_testqi_ext_1_ccno (scratch, GEN_INT (0x04)));
	  code = EQ;
	  break;

	default:
	  gcc_unreachable ();
	}
	break;

    default:
      gcc_unreachable();
    }

  /* Return the test that should be put into the flags user, i.e.
     the bcc, scc, or cmov instruction.  */
  return gen_rtx_fmt_ee (code, VOIDmode,
			 gen_rtx_REG (cmp_mode, FLAGS_REG),
			 const0_rtx);
}

/* Generate insn patterns to do an integer compare of OPERANDS.  */

static rtx
ix86_expand_int_compare (enum rtx_code code, rtx op0, rtx op1)
{
  machine_mode cmpmode;
  rtx tmp, flags;

  /* Swap operands to emit carry flag comparison.  */
  if ((code == GTU || code == LEU)
      && nonimmediate_operand (op1, VOIDmode))
    {
      std::swap (op0, op1);
      code = swap_condition (code);
    }

  cmpmode = SELECT_CC_MODE (code, op0, op1);
  flags = gen_rtx_REG (cmpmode, FLAGS_REG);

  /* Attempt to use PTEST, if available, when testing vector modes for
     equality/inequality against zero.  */
  if (op1 == const0_rtx
      && SUBREG_P (op0)
      && cmpmode == CCZmode
      && SUBREG_BYTE (op0) == 0
      && REG_P (SUBREG_REG (op0))
      && VECTOR_MODE_P (GET_MODE (SUBREG_REG (op0)))
      && TARGET_SSE4_1
      && GET_MODE (op0) == TImode
      && GET_MODE_SIZE (GET_MODE (SUBREG_REG (op0))) == 16)
    {
      tmp = SUBREG_REG (op0);
      tmp = gen_rtx_UNSPEC (CCZmode, gen_rtvec (2, tmp, tmp), UNSPEC_PTEST);
    }
  else
    tmp = gen_rtx_COMPARE (cmpmode, op0, op1);

  /* This is very simple, but making the interface the same as in the
     FP case makes the rest of the code easier.  */
  emit_insn (gen_rtx_SET (flags, tmp));

  /* Return the test that should be put into the flags user, i.e.
     the bcc, scc, or cmov instruction.  */
  return gen_rtx_fmt_ee (code, VOIDmode, flags, const0_rtx);
}

static rtx
ix86_expand_compare (enum rtx_code code, rtx op0, rtx op1)
{
  rtx ret;

  if (GET_MODE_CLASS (GET_MODE (op0)) == MODE_CC)
    ret = gen_rtx_fmt_ee (code, VOIDmode, op0, op1);

  else if (SCALAR_FLOAT_MODE_P (GET_MODE (op0)))
    {
      gcc_assert (!DECIMAL_FLOAT_MODE_P (GET_MODE (op0)));
      ret = ix86_expand_fp_compare (code, op0, op1);
    }
  else
    ret = ix86_expand_int_compare (code, op0, op1);

  return ret;
}

void
ix86_expand_setcc (rtx dest, enum rtx_code code, rtx op0, rtx op1)
{
  rtx ret;

  gcc_assert (GET_MODE (dest) == QImode);

  ret = ix86_expand_compare (code, op0, op1);
  PUT_MODE (ret, QImode);
  emit_insn (gen_rtx_SET (dest, ret));
}

/* Expand floating point op0 <=> op1, i.e.
   dest = op0 == op1 ? 0 : op0 < op1 ? -1 : op0 > op1 ? 1 : 2.  */

void
ix86_expand_fp_spaceship (rtx dest, rtx op0, rtx op1)
{
  gcc_checking_assert (ix86_fp_comparison_strategy (GT) != IX86_FPCMP_ARITH);
  rtx gt = ix86_expand_fp_compare (GT, op0, op1);
  rtx l0 = gen_label_rtx ();
  rtx l1 = gen_label_rtx ();
  rtx l2 = TARGET_IEEE_FP ? gen_label_rtx () : NULL_RTX;
  rtx lend = gen_label_rtx ();
  rtx tmp;
  rtx_insn *jmp;
  if (l2)
    {
      rtx un = gen_rtx_fmt_ee (UNORDERED, VOIDmode,
			       gen_rtx_REG (CCFPmode, FLAGS_REG), const0_rtx);
      tmp = gen_rtx_IF_THEN_ELSE (VOIDmode, un,
				  gen_rtx_LABEL_REF (VOIDmode, l2), pc_rtx);
      jmp = emit_jump_insn (gen_rtx_SET (pc_rtx, tmp));
      add_reg_br_prob_note (jmp, profile_probability:: very_unlikely ());
    }
  rtx eq = gen_rtx_fmt_ee (UNEQ, VOIDmode,
			   gen_rtx_REG (CCFPmode, FLAGS_REG), const0_rtx);
  tmp = gen_rtx_IF_THEN_ELSE (VOIDmode, eq,
			      gen_rtx_LABEL_REF (VOIDmode, l0), pc_rtx);
  jmp = emit_jump_insn (gen_rtx_SET (pc_rtx, tmp));
  add_reg_br_prob_note (jmp, profile_probability::unlikely ());
  tmp = gen_rtx_IF_THEN_ELSE (VOIDmode, gt,
			      gen_rtx_LABEL_REF (VOIDmode, l1), pc_rtx);
  jmp = emit_jump_insn (gen_rtx_SET (pc_rtx, tmp));
  add_reg_br_prob_note (jmp, profile_probability::even ());
  emit_move_insn (dest, constm1_rtx);
  emit_jump (lend);
  emit_label (l0);
  emit_move_insn (dest, const0_rtx);
  emit_jump (lend);
  emit_label (l1);
  emit_move_insn (dest, const1_rtx);
  emit_jump (lend);
  if (l2)
    {
      emit_label (l2);
      emit_move_insn (dest, const2_rtx);
    }
  emit_label (lend);
}

/* Expand comparison setting or clearing carry flag.  Return true when
   successful and set pop for the operation.  */
static bool
ix86_expand_carry_flag_compare (enum rtx_code code, rtx op0, rtx op1, rtx *pop)
{
  machine_mode mode
    = GET_MODE (op0) != VOIDmode ? GET_MODE (op0) : GET_MODE (op1);

  /* Do not handle double-mode compares that go through special path.  */
  if (mode == (TARGET_64BIT ? TImode : DImode))
    return false;

  if (SCALAR_FLOAT_MODE_P (mode))
    {
      rtx compare_op;
      rtx_insn *compare_seq;

      gcc_assert (!DECIMAL_FLOAT_MODE_P (mode));

      /* Shortcut:  following common codes never translate
	 into carry flag compares.  */
      if (code == EQ || code == NE || code == UNEQ || code == LTGT
	  || code == ORDERED || code == UNORDERED)
	return false;

      /* These comparisons require zero flag; swap operands so they won't.  */
      if ((code == GT || code == UNLE || code == LE || code == UNGT)
	  && !TARGET_IEEE_FP)
	{
	  std::swap (op0, op1);
	  code = swap_condition (code);
	}

      /* Try to expand the comparison and verify that we end up with
	 carry flag based comparison.  This fails to be true only when
	 we decide to expand comparison using arithmetic that is not
	 too common scenario.  */
      start_sequence ();
      compare_op = ix86_expand_fp_compare (code, op0, op1);
      compare_seq = get_insns ();
      end_sequence ();

      if (GET_MODE (XEXP (compare_op, 0)) == CCFPmode)
        code = ix86_fp_compare_code_to_integer (GET_CODE (compare_op));
      else
	code = GET_CODE (compare_op);

      if (code != LTU && code != GEU)
	return false;

      emit_insn (compare_seq);
      *pop = compare_op;
      return true;
    }

  if (!INTEGRAL_MODE_P (mode))
    return false;

  switch (code)
    {
    case LTU:
    case GEU:
      break;

    /* Convert a==0 into (unsigned)a<1.  */
    case EQ:
    case NE:
      if (op1 != const0_rtx)
	return false;
      op1 = const1_rtx;
      code = (code == EQ ? LTU : GEU);
      break;

    /* Convert a>b into b<a or a>=b-1.  */
    case GTU:
    case LEU:
      if (CONST_INT_P (op1))
	{
	  op1 = gen_int_mode (INTVAL (op1) + 1, GET_MODE (op0));
	  /* Bail out on overflow.  We still can swap operands but that
	     would force loading of the constant into register.  */
	  if (op1 == const0_rtx
	      || !x86_64_immediate_operand (op1, GET_MODE (op1)))
	    return false;
	  code = (code == GTU ? GEU : LTU);
	}
      else
	{
	  std::swap (op0, op1);
	  code = (code == GTU ? LTU : GEU);
	}
      break;

    /* Convert a>=0 into (unsigned)a<0x80000000.  */
    case LT:
    case GE:
      if (mode == DImode || op1 != const0_rtx)
	return false;
      op1 = gen_int_mode (1 << (GET_MODE_BITSIZE (mode) - 1), mode);
      code = (code == LT ? GEU : LTU);
      break;
    case LE:
    case GT:
      if (mode == DImode || op1 != constm1_rtx)
	return false;
      op1 = gen_int_mode (1 << (GET_MODE_BITSIZE (mode) - 1), mode);
      code = (code == LE ? GEU : LTU);
      break;

    default:
      return false;
    }
  /* Swapping operands may cause constant to appear as first operand.  */
  if (!nonimmediate_operand (op0, VOIDmode))
    {
      if (!can_create_pseudo_p ())
	return false;
      op0 = force_reg (mode, op0);
    }
  *pop = ix86_expand_compare (code, op0, op1);
  gcc_assert (GET_CODE (*pop) == LTU || GET_CODE (*pop) == GEU);
  return true;
}

/* Expand conditional increment or decrement using adb/sbb instructions.
   The default case using setcc followed by the conditional move can be
   done by generic code.  */
bool
ix86_expand_int_addcc (rtx operands[])
{
  enum rtx_code code = GET_CODE (operands[1]);
  rtx flags;
  rtx (*insn) (machine_mode, rtx, rtx, rtx, rtx, rtx);
  rtx compare_op;
  rtx val = const0_rtx;
  bool fpcmp = false;
  machine_mode mode;
  rtx op0 = XEXP (operands[1], 0);
  rtx op1 = XEXP (operands[1], 1);

  if (operands[3] != const1_rtx
      && operands[3] != constm1_rtx)
    return false;
  if (!ix86_expand_carry_flag_compare (code, op0, op1, &compare_op))
     return false;
  code = GET_CODE (compare_op);

  flags = XEXP (compare_op, 0);

  if (GET_MODE (flags) == CCFPmode)
    {
      fpcmp = true;
      code = ix86_fp_compare_code_to_integer (code);
    }

  if (code != LTU)
    {
      val = constm1_rtx;
      if (fpcmp)
	PUT_CODE (compare_op,
		  reverse_condition_maybe_unordered
		    (GET_CODE (compare_op)));
      else
	PUT_CODE (compare_op, reverse_condition (GET_CODE (compare_op)));
    }

  mode = GET_MODE (operands[0]);

  /* Construct either adc or sbb insn.  */
  if ((code == LTU) == (operands[3] == constm1_rtx))
    insn = gen_sub3_carry;
  else
    insn = gen_add3_carry;

  emit_insn (insn (mode, operands[0], operands[2], val, flags, compare_op));

  return true;
}

bool
ix86_expand_int_movcc (rtx operands[])
{
  enum rtx_code code = GET_CODE (operands[1]), compare_code;
  rtx_insn *compare_seq;
  rtx compare_op;
  machine_mode mode = GET_MODE (operands[0]);
  bool sign_bit_compare_p = false;
  bool negate_cc_compare_p = false;
  rtx op0 = XEXP (operands[1], 0);
  rtx op1 = XEXP (operands[1], 1);
  rtx op2 = operands[2];
  rtx op3 = operands[3];

  if (GET_MODE (op0) == TImode
      || (GET_MODE (op0) == DImode
	  && !TARGET_64BIT))
    return false;

  if (GET_MODE (op0) == BFmode
      && !ix86_fp_comparison_operator (operands[1], VOIDmode))
    return false;

  start_sequence ();
  compare_op = ix86_expand_compare (code, op0, op1);
  compare_seq = get_insns ();
  end_sequence ();

  compare_code = GET_CODE (compare_op);

  if ((op1 == const0_rtx && (code == GE || code == LT))
      || (op1 == constm1_rtx && (code == GT || code == LE)))
    sign_bit_compare_p = true;

  /* op0 == op1 ? op0 : op3 is equivalent to op0 == op1 ? op1 : op3,
     but if op1 is a constant, the latter form allows more optimizations,
     either through the last 2 ops being constant handling, or the one
     constant and one variable cases.  On the other side, for cmov the
     former might be better as we don't need to load the constant into
     another register.  */
  if (code == EQ && CONST_INT_P (op1) && rtx_equal_p (op0, op2))
    op2 = op1;
  /* Similarly for op0 != op1 ? op2 : op0 and op0 != op1 ? op2 : op1.  */
  else if (code == NE && CONST_INT_P (op1) && rtx_equal_p (op0, op3))
    op3 = op1;

  /* Don't attempt mode expansion here -- if we had to expand 5 or 6
     HImode insns, we'd be swallowed in word prefix ops.  */

  if ((mode != HImode || TARGET_FAST_PREFIX)
      && (mode != (TARGET_64BIT ? TImode : DImode))
      && CONST_INT_P (op2)
      && CONST_INT_P (op3))
    {
      rtx out = operands[0];
      HOST_WIDE_INT ct = INTVAL (op2);
      HOST_WIDE_INT cf = INTVAL (op3);
      HOST_WIDE_INT diff;

      if ((mode == SImode
	   || (TARGET_64BIT && mode == DImode))
	  && (GET_MODE (op0) == SImode
	      || (TARGET_64BIT && GET_MODE (op0) == DImode)))
	{
	  /* Special case x != 0 ? -1 : y.  */
	  if (code == NE && op1 == const0_rtx && ct == -1)
	    {
	      negate_cc_compare_p = true;
	      std::swap (ct, cf);
	      code = EQ;
	    }
	  else if (code == EQ && op1 == const0_rtx && cf == -1)
	    negate_cc_compare_p = true;
	}

      diff = ct - cf;
      /*  Sign bit compares are better done using shifts than we do by using
	  sbb.  */
      if (sign_bit_compare_p
	  || negate_cc_compare_p
	  || ix86_expand_carry_flag_compare (code, op0, op1, &compare_op))
	{
	  /* Detect overlap between destination and compare sources.  */
	  rtx tmp = out;

	  if (negate_cc_compare_p)
	    {
	      if (GET_MODE (op0) == DImode)
		emit_insn (gen_x86_negdi_ccc (gen_reg_rtx (DImode), op0));
	      else
		emit_insn (gen_x86_negsi_ccc (gen_reg_rtx (SImode),
					      gen_lowpart (SImode, op0)));

	      tmp = gen_reg_rtx (mode);
	      if (mode == DImode)
		emit_insn (gen_x86_movdicc_0_m1_neg (tmp));
	      else
		emit_insn (gen_x86_movsicc_0_m1_neg (gen_lowpart (SImode,
								  tmp)));
	    }
	  else if (!sign_bit_compare_p)
	    {
	      rtx flags;
	      bool fpcmp = false;

	      compare_code = GET_CODE (compare_op);

	      flags = XEXP (compare_op, 0);

	      if (GET_MODE (flags) == CCFPmode)
		{
		  fpcmp = true;
		  compare_code
		    = ix86_fp_compare_code_to_integer (compare_code);
		}

	      /* To simplify rest of code, restrict to the GEU case.  */
	      if (compare_code == LTU)
		{
		  std::swap (ct, cf);
		  compare_code = reverse_condition (compare_code);
		  code = reverse_condition (code);
		}
	      else
		{
		  if (fpcmp)
		    PUT_CODE (compare_op,
			      reverse_condition_maybe_unordered
			        (GET_CODE (compare_op)));
		  else
		    PUT_CODE (compare_op,
			      reverse_condition (GET_CODE (compare_op)));
		}
	      diff = ct - cf;

	      if (reg_overlap_mentioned_p (out, compare_op))
		tmp = gen_reg_rtx (mode);

	      if (mode == DImode)
		emit_insn (gen_x86_movdicc_0_m1 (tmp, flags, compare_op));
	      else
		emit_insn (gen_x86_movsicc_0_m1	(gen_lowpart (SImode, tmp),
						 flags, compare_op));
	    }
	  else
	    {
	      if (code == GT || code == GE)
		code = reverse_condition (code);
	      else
		{
		  std::swap (ct, cf);
		  diff = ct - cf;
		}
	      tmp = emit_store_flag (tmp, code, op0, op1, VOIDmode, 0, -1);
	    }

	  if (diff == 1)
	    {
	      /*
	       * cmpl op0,op1
	       * sbbl dest,dest
	       * [addl dest, ct]
	       *
	       * Size 5 - 8.
	       */
	      if (ct)
		tmp = expand_simple_binop (mode, PLUS,
					   tmp, GEN_INT (ct),
					   copy_rtx (tmp), 1, OPTAB_DIRECT);
	    }
	  else if (cf == -1)
	    {
	      /*
	       * cmpl op0,op1
	       * sbbl dest,dest
	       * orl $ct, dest
	       *
	       * Size 8.
	       */
	      tmp = expand_simple_binop (mode, IOR,
					 tmp, GEN_INT (ct),
					 copy_rtx (tmp), 1, OPTAB_DIRECT);
	    }
	  else if (diff == -1 && ct)
	    {
	      /*
	       * cmpl op0,op1
	       * sbbl dest,dest
	       * notl dest
	       * [addl dest, cf]
	       *
	       * Size 8 - 11.
	       */
	      tmp = expand_simple_unop (mode, NOT, tmp, copy_rtx (tmp), 1);
	      if (cf)
		tmp = expand_simple_binop (mode, PLUS,
					   copy_rtx (tmp), GEN_INT (cf),
					   copy_rtx (tmp), 1, OPTAB_DIRECT);
	    }
	  else
	    {
	      /*
	       * cmpl op0,op1
	       * sbbl dest,dest
	       * [notl dest]
	       * andl cf - ct, dest
	       * [addl dest, ct]
	       *
	       * Size 8 - 11.
	       */

	      if (cf == 0)
		{
		  cf = ct;
		  ct = 0;
		  tmp = expand_simple_unop (mode, NOT, tmp, copy_rtx (tmp), 1);
		}

	      tmp = expand_simple_binop (mode, AND,
					 copy_rtx (tmp),
					 gen_int_mode (cf - ct, mode),
					 copy_rtx (tmp), 1, OPTAB_DIRECT);
	      if (ct)
		tmp = expand_simple_binop (mode, PLUS,
					   copy_rtx (tmp), GEN_INT (ct),
					   copy_rtx (tmp), 1, OPTAB_DIRECT);
	    }

	  if (!rtx_equal_p (tmp, out))
	    emit_move_insn (copy_rtx (out), copy_rtx (tmp));

	  return true;
	}

      if (diff < 0)
	{
	  machine_mode cmp_mode = GET_MODE (op0);
	  enum rtx_code new_code;

	  if (SCALAR_FLOAT_MODE_P (cmp_mode))
	    {
	      gcc_assert (!DECIMAL_FLOAT_MODE_P (cmp_mode));

	      /* We may be reversing a non-trapping
		 comparison to a trapping comparison.  */
		  if (HONOR_NANS (cmp_mode) && flag_trapping_math
		      && code != EQ && code != NE
		      && code != ORDERED && code != UNORDERED)
		    new_code = UNKNOWN;
		  else
		    new_code = reverse_condition_maybe_unordered (code);
	    }
	  else
	    new_code = ix86_reverse_condition (code, cmp_mode);
	  if (new_code != UNKNOWN)
	    {
	      std::swap (ct, cf);
	      diff = -diff;
	      code = new_code;
	    }
	}

      compare_code = UNKNOWN;
      if (GET_MODE_CLASS (GET_MODE (op0)) == MODE_INT
	  && CONST_INT_P (op1))
	{
	  if (op1 == const0_rtx
	      && (code == LT || code == GE))
	    compare_code = code;
	  else if (op1 == constm1_rtx)
	    {
	      if (code == LE)
		compare_code = LT;
	      else if (code == GT)
		compare_code = GE;
	    }
	}

      /* Optimize dest = (op0 < 0) ? -1 : cf.  */
      if (compare_code != UNKNOWN
	  && GET_MODE (op0) == GET_MODE (out)
	  && (cf == -1 || ct == -1))
	{
	  /* If lea code below could be used, only optimize
	     if it results in a 2 insn sequence.  */

	  if (! (diff == 1 || diff == 2 || diff == 4 || diff == 8
		 || diff == 3 || diff == 5 || diff == 9)
	      || (compare_code == LT && ct == -1)
	      || (compare_code == GE && cf == -1))
	    {
	      /*
	       * notl op1	(if necessary)
	       * sarl $31, op1
	       * orl cf, op1
	       */
	      if (ct != -1)
		{
		  cf = ct;
		  ct = -1;
		  code = reverse_condition (code);
		}

	      out = emit_store_flag (out, code, op0, op1, VOIDmode, 0, -1);

	      out = expand_simple_binop (mode, IOR,
					 out, GEN_INT (cf),
					 out, 1, OPTAB_DIRECT);
	      if (out != operands[0])
		emit_move_insn (operands[0], out);

	      return true;
	    }
	}


      if ((diff == 1 || diff == 2 || diff == 4 || diff == 8
	   || diff == 3 || diff == 5 || diff == 9)
	  && ((mode != QImode && mode != HImode) || !TARGET_PARTIAL_REG_STALL)
	  && (mode != DImode
	      || x86_64_immediate_operand (GEN_INT (cf), VOIDmode)))
	{
	  /*
	   * xorl dest,dest
	   * cmpl op1,op2
	   * setcc dest
	   * lea cf(dest*(ct-cf)),dest
	   *
	   * Size 14.
	   *
	   * This also catches the degenerate setcc-only case.
	   */

	  rtx tmp;
	  int nops;

	  out = emit_store_flag (out, code, op0, op1, VOIDmode, 0, 1);

	  nops = 0;
	  /* On x86_64 the lea instruction operates on Pmode, so we need
	     to get arithmetics done in proper mode to match.  */
	  if (diff == 1)
	    tmp = copy_rtx (out);
	  else
	    {
	      rtx out1;
	      out1 = copy_rtx (out);
	      tmp = gen_rtx_MULT (mode, out1, GEN_INT (diff & ~1));
	      nops++;
	      if (diff & 1)
		{
		  tmp = gen_rtx_PLUS (mode, tmp, out1);
		  nops++;
		}
	    }
	  if (cf != 0)
	    {
	      tmp = plus_constant (mode, tmp, cf);
	      nops++;
	    }
	  if (!rtx_equal_p (tmp, out))
	    {
	      if (nops == 1)
		out = force_operand (tmp, copy_rtx (out));
	      else
		emit_insn (gen_rtx_SET (copy_rtx (out), copy_rtx (tmp)));
	    }
	  if (!rtx_equal_p (out, operands[0]))
	    emit_move_insn (operands[0], copy_rtx (out));

	  return true;
	}

      /*
       * General case:			Jumpful:
       *   xorl dest,dest		cmpl op1, op2
       *   cmpl op1, op2		movl ct, dest
       *   setcc dest			jcc 1f
       *   decl dest			movl cf, dest
       *   andl (cf-ct),dest		1:
       *   addl ct,dest
       *
       * Size 20.			Size 14.
       *
       * This is reasonably steep, but branch mispredict costs are
       * high on modern cpus, so consider failing only if optimizing
       * for space.
       */

      if ((!TARGET_CMOVE || (mode == QImode && TARGET_PARTIAL_REG_STALL))
	  && BRANCH_COST (optimize_insn_for_speed_p (),
		  	  false) >= 2)
	{
	  if (cf == 0)
	    {
	      machine_mode cmp_mode = GET_MODE (op0);
	      enum rtx_code new_code;

	      if (SCALAR_FLOAT_MODE_P (cmp_mode))
		{
		  gcc_assert (!DECIMAL_FLOAT_MODE_P (cmp_mode));

		  /* We may be reversing a non-trapping
		     comparison to a trapping comparison.  */
		  if (HONOR_NANS (cmp_mode) && flag_trapping_math
		      && code != EQ && code != NE
		      && code != ORDERED && code != UNORDERED)
		    new_code = UNKNOWN;
		  else
		    new_code = reverse_condition_maybe_unordered (code);

		}
	      else
		{
		  new_code = ix86_reverse_condition (code, cmp_mode);
		  if (compare_code != UNKNOWN && new_code != UNKNOWN)
		    compare_code = reverse_condition (compare_code);
		}

	      if (new_code != UNKNOWN)
		{
		  cf = ct;
		  ct = 0;
		  code = new_code;
		}
	    }

	  if (compare_code != UNKNOWN)
	    {
	      /* notl op1	(if needed)
		 sarl $31, op1
		 andl (cf-ct), op1
		 addl ct, op1

		 For x < 0 (resp. x <= -1) there will be no notl,
		 so if possible swap the constants to get rid of the
		 complement.
		 True/false will be -1/0 while code below (store flag
		 followed by decrement) is 0/-1, so the constants need
		 to be exchanged once more.  */

	      if (compare_code == GE || !cf)
		{
		  code = reverse_condition (code);
		  compare_code = LT;
		}
	      else
		std::swap (ct, cf);

	      out = emit_store_flag (out, code, op0, op1, VOIDmode, 0, -1);
	    }
	  else
	    {
	      out = emit_store_flag (out, code, op0, op1, VOIDmode, 0, 1);

	      out = expand_simple_binop (mode, PLUS, copy_rtx (out),
					 constm1_rtx,
					 copy_rtx (out), 1, OPTAB_DIRECT);
	    }

	  out = expand_simple_binop (mode, AND, copy_rtx (out),
				     gen_int_mode (cf - ct, mode),
				     copy_rtx (out), 1, OPTAB_DIRECT);
	  if (ct)
	    out = expand_simple_binop (mode, PLUS, copy_rtx (out), GEN_INT (ct),
				       copy_rtx (out), 1, OPTAB_DIRECT);
	  if (!rtx_equal_p (out, operands[0]))
	    emit_move_insn (operands[0], copy_rtx (out));

	  return true;
	}
    }

  if (!TARGET_CMOVE || (mode == QImode && TARGET_PARTIAL_REG_STALL))
    {
      /* Try a few things more with specific constants and a variable.  */

      optab op;
      rtx var, orig_out, out, tmp;

      if (BRANCH_COST (optimize_insn_for_speed_p (), false) <= 2)
	return false;

      operands[2] = op2;
      operands[3] = op3;

      /* If one of the two operands is an interesting constant, load a
	 constant with the above and mask it in with a logical operation.  */

      if (CONST_INT_P (operands[2]))
	{
	  var = operands[3];
	  if (INTVAL (operands[2]) == 0 && operands[3] != constm1_rtx)
	    operands[3] = constm1_rtx, op = and_optab;
	  else if (INTVAL (operands[2]) == -1 && operands[3] != const0_rtx)
	    operands[3] = const0_rtx, op = ior_optab;
	  else
	    return false;
	}
      else if (CONST_INT_P (operands[3]))
	{
	  var = operands[2];
	  if (INTVAL (operands[3]) == 0 && operands[2] != constm1_rtx)
	    {
	      /* For smin (x, 0), expand as "x < 0 ? x : 0" instead of
		 "x <= 0 ? x : 0" to enable sign_bit_compare_p.  */
	      if (code == LE && op1 == const0_rtx && rtx_equal_p (op0, var))
		operands[1] = simplify_gen_relational (LT, VOIDmode,
						       GET_MODE (op0),
						       op0, const0_rtx);

	      operands[2] = constm1_rtx;
	      op = and_optab;
	    }
	  else if (INTVAL (operands[3]) == -1 && operands[3] != const0_rtx)
	    operands[2] = const0_rtx, op = ior_optab;
	  else
	    return false;
	}
      else
        return false;

      orig_out = operands[0];
      tmp = gen_reg_rtx (mode);
      operands[0] = tmp;

      /* Recurse to get the constant loaded.  */
      if (!ix86_expand_int_movcc (operands))
        return false;

      /* Mask in the interesting variable.  */
      out = expand_binop (mode, op, var, tmp, orig_out, 0,
			  OPTAB_WIDEN);
      if (!rtx_equal_p (out, orig_out))
	emit_move_insn (copy_rtx (orig_out), copy_rtx (out));

      return true;
    }

  /*
   * For comparison with above,
   *
   * movl cf,dest
   * movl ct,tmp
   * cmpl op1,op2
   * cmovcc tmp,dest
   *
   * Size 15.
   */

  if (! nonimmediate_operand (operands[2], mode))
    operands[2] = force_reg (mode, operands[2]);
  if (! nonimmediate_operand (operands[3], mode))
    operands[3] = force_reg (mode, operands[3]);

  if (! register_operand (operands[2], VOIDmode)
      && (mode == QImode
          || ! register_operand (operands[3], VOIDmode)))
    operands[2] = force_reg (mode, operands[2]);

  if (mode == QImode
      && ! register_operand (operands[3], VOIDmode))
    operands[3] = force_reg (mode, operands[3]);

  emit_insn (compare_seq);
  emit_insn (gen_rtx_SET (operands[0],
			  gen_rtx_IF_THEN_ELSE (mode,
						compare_op, operands[2],
						operands[3])));
  return true;
}

/* Detect conditional moves that exactly match min/max operational
   semantics.  Note that this is IEEE safe, as long as we don't
   interchange the operands.

   Returns FALSE if this conditional move doesn't match a MIN/MAX,
   and TRUE if the operation is successful and instructions are emitted.  */

static bool
ix86_expand_sse_fp_minmax (rtx dest, enum rtx_code code, rtx cmp_op0,
			   rtx cmp_op1, rtx if_true, rtx if_false)
{
  machine_mode mode;
  bool is_min;
  rtx tmp;

  if (code == LT)
    ;
  else if (code == UNGE)
    std::swap (if_true, if_false);
  else
    return false;

  if (rtx_equal_p (cmp_op0, if_true) && rtx_equal_p (cmp_op1, if_false))
    is_min = true;
  else if (rtx_equal_p (cmp_op1, if_true) && rtx_equal_p (cmp_op0, if_false))
    is_min = false;
  else
    return false;

  mode = GET_MODE (dest);

  /* We want to check HONOR_NANS and HONOR_SIGNED_ZEROS here,
     but MODE may be a vector mode and thus not appropriate.  */
  if (!flag_finite_math_only || flag_signed_zeros)
    {
      int u = is_min ? UNSPEC_IEEE_MIN : UNSPEC_IEEE_MAX;
      rtvec v;

      if_true = force_reg (mode, if_true);
      v = gen_rtvec (2, if_true, if_false);
      tmp = gen_rtx_UNSPEC (mode, v, u);
    }
  else
    {
      code = is_min ? SMIN : SMAX;
      if (MEM_P (if_true) && MEM_P (if_false))
	if_true = force_reg (mode, if_true);
      tmp = gen_rtx_fmt_ee (code, mode, if_true, if_false);
    }

  emit_insn (gen_rtx_SET (dest, tmp));
  return true;
}

/* Return true if MODE is valid for vector compare to mask register,
   Same result for conditionl vector move with mask register.  */
static bool
ix86_valid_mask_cmp_mode (machine_mode mode)
{
  /* XOP has its own vector conditional movement.  */
  if (TARGET_XOP && !TARGET_AVX512F)
    return false;

  /* HFmode only supports vcmpsh whose dest is mask register.  */
  if (TARGET_AVX512FP16 && mode == HFmode)
    return true;

  /* AVX512F is needed for mask operation.  */
  if (!(TARGET_AVX512F && VECTOR_MODE_P (mode)))
    return false;

  /* AVX512BW is needed for vector QI/HImode,
     AVX512VL is needed for 128/256-bit vector.  */
  machine_mode inner_mode = GET_MODE_INNER (mode);
  int vector_size = GET_MODE_SIZE (mode);
  if ((inner_mode == QImode || inner_mode == HImode) && !TARGET_AVX512BW)
    return false;

  return (vector_size == 64 && TARGET_EVEX512) || TARGET_AVX512VL;
}

/* Return true if integer mask comparison should be used.  */
static bool
ix86_use_mask_cmp_p (machine_mode mode, machine_mode cmp_mode,
		     rtx op_true, rtx op_false)
{
  int vector_size = GET_MODE_SIZE (mode);

  if (cmp_mode == HFmode)
    return true;
  else if (vector_size < 16)
    return false;
  else if (vector_size == 64)
    return true;
  else if (GET_MODE_INNER (cmp_mode) == HFmode)
    return true;

  /* When op_true is NULL, op_false must be NULL, or vice versa.  */
  gcc_assert (!op_true == !op_false);

  /* When op_true/op_false is NULL or cmp_mode is not valid mask cmp mode,
     vector dest is required.  */
  if (!op_true || !ix86_valid_mask_cmp_mode (cmp_mode))
    return false;

  /* Exclude those that could be optimized in ix86_expand_sse_movcc.  */
  if (op_false == CONST0_RTX (mode)
      || op_true == CONST0_RTX (mode)
      || (INTEGRAL_MODE_P (mode)
	  && (op_true == CONSTM1_RTX (mode)
	      || op_false == CONSTM1_RTX (mode))))
    return false;

  return true;
}

/* Expand an SSE comparison.  Return the register with the result.  */

static rtx
ix86_expand_sse_cmp (rtx dest, enum rtx_code code, rtx cmp_op0, rtx cmp_op1,
		     rtx op_true, rtx op_false)
{
  machine_mode mode = GET_MODE (dest);
  machine_mode cmp_ops_mode = GET_MODE (cmp_op0);

  /* In general case result of comparison can differ from operands' type.  */
  machine_mode cmp_mode;

  /* In AVX512F the result of comparison is an integer mask.  */
  bool maskcmp = false;
  rtx x;

  if (ix86_use_mask_cmp_p (mode, cmp_ops_mode, op_true, op_false))
    {
      unsigned int nbits = GET_MODE_NUNITS (cmp_ops_mode);
      maskcmp = true;
      cmp_mode = nbits > 8 ? int_mode_for_size (nbits, 0).require () : E_QImode;
    }
  else
    cmp_mode = cmp_ops_mode;

  cmp_op0 = force_reg (cmp_ops_mode, cmp_op0);

  bool (*op1_predicate)(rtx, machine_mode)
    = VECTOR_MODE_P (cmp_ops_mode) ? vector_operand : nonimmediate_operand;

  if (!op1_predicate (cmp_op1, cmp_ops_mode))
    cmp_op1 = force_reg (cmp_ops_mode, cmp_op1);

  if (optimize
      || (maskcmp && cmp_mode != mode)
      || (op_true && reg_overlap_mentioned_p (dest, op_true))
      || (op_false && reg_overlap_mentioned_p (dest, op_false)))
    dest = gen_reg_rtx (maskcmp ? cmp_mode : mode);

  if (maskcmp)
    {
      bool ok = ix86_expand_mask_vec_cmp (dest, code, cmp_op0, cmp_op1);
      gcc_assert (ok);
      return dest;
    }

  x = gen_rtx_fmt_ee (code, cmp_mode, cmp_op0, cmp_op1);

  if (cmp_mode != mode)
    {
      x = force_reg (cmp_ops_mode, x);
      convert_move (dest, x, false);
    }
  else
    emit_insn (gen_rtx_SET (dest, x));

  return dest;
}

/* Emit x86 binary operand CODE in mode MODE for SSE vector
   instructions that can be performed using GP registers.  */

static void
ix86_emit_vec_binop (enum rtx_code code, machine_mode mode,
		     rtx dst, rtx src1, rtx src2)
{
  rtx tmp;

  tmp = gen_rtx_SET (dst, gen_rtx_fmt_ee (code, mode, src1, src2));

  if (GET_MODE_SIZE (mode) <= GET_MODE_SIZE (SImode)
      && GET_MODE_CLASS (mode) == MODE_VECTOR_INT)
    {
      rtx clob = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (CCmode, FLAGS_REG));
      tmp = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, tmp, clob));
    }

  emit_insn (tmp);
}

/* Expand DEST = CMP ? OP_TRUE : OP_FALSE into a sequence of logical
   operations.  This is used for both scalar and vector conditional moves.  */

void
ix86_expand_sse_movcc (rtx dest, rtx cmp, rtx op_true, rtx op_false)
{
  machine_mode mode = GET_MODE (dest);
  machine_mode cmpmode = GET_MODE (cmp);
  rtx x;

  /* Simplify trivial VEC_COND_EXPR to avoid ICE in pr97506.  */
  if (rtx_equal_p (op_true, op_false))
    {
      emit_move_insn (dest, op_true);
      return;
    }

  /* If we have an integer mask and FP value then we need
     to cast mask to FP mode.  */
  if (mode != cmpmode && VECTOR_MODE_P (cmpmode))
    {
      cmp = force_reg (cmpmode, cmp);
      cmp = gen_rtx_SUBREG (mode, cmp, 0);
    }

  /* In AVX512F the result of comparison is an integer mask.  */
  if (mode != cmpmode
      && GET_MODE_CLASS (cmpmode) == MODE_INT)
    {
      gcc_assert (ix86_valid_mask_cmp_mode (mode));
      /* Using scalar/vector move with mask register.  */
      cmp = force_reg (cmpmode, cmp);
      /* Optimize for mask zero.  */
      op_true = (op_true != CONST0_RTX (mode)
		 ? force_reg (mode, op_true) : op_true);
      op_false = (op_false != CONST0_RTX (mode)
		  ? force_reg (mode, op_false) : op_false);
      if (op_true == CONST0_RTX (mode))
	{
	  if (cmpmode == E_DImode && !TARGET_64BIT)
	    {
	      x = gen_reg_rtx (cmpmode);
	      emit_insn (gen_knotdi (x, cmp));
	    }
	  else
	    x = expand_simple_unop (cmpmode, NOT, cmp, NULL, 1);
	  cmp = x;
	  /* Reverse op_true op_false.  */
	  std::swap (op_true, op_false);
	}

      if (mode == HFmode)
	emit_insn (gen_movhf_mask (dest, op_true, op_false, cmp));
      else
	emit_insn (gen_rtx_SET (dest,
				gen_rtx_VEC_MERGE (mode,
						   op_true, op_false, cmp)));
      return;
    }

  if (vector_all_ones_operand (op_true, mode)
      && op_false == CONST0_RTX (mode))
    {
      emit_move_insn (dest, cmp);
      return;
    }
  else if (op_false == CONST0_RTX (mode))
    {
      x = expand_simple_binop (mode, AND, cmp, op_true,
			       dest, 1, OPTAB_DIRECT);
      if (x != dest)
	emit_move_insn (dest, x);
      return;
    }
  else if (op_true == CONST0_RTX (mode))
    {
      op_false = force_reg (mode, op_false);
      x = gen_rtx_NOT (mode, cmp);
      ix86_emit_vec_binop (AND, mode, dest, x, op_false);
      return;
    }
  else if (vector_all_ones_operand (op_true, mode))
    {
      x = expand_simple_binop (mode, IOR, cmp, op_false,
			       dest, 1, OPTAB_DIRECT);
      if (x != dest)
	emit_move_insn (dest, x);
      return;
    }

  if (TARGET_XOP)
    {
      op_true = force_reg (mode, op_true);

      if (GET_MODE_SIZE (mode) < 16
	  || !nonimmediate_operand (op_false, mode))
	op_false = force_reg (mode, op_false);

      emit_insn (gen_rtx_SET (dest,
			      gen_rtx_IF_THEN_ELSE (mode, cmp,
						    op_true, op_false)));
      return;
    }

  rtx (*gen) (rtx, rtx, rtx, rtx) = NULL;
  machine_mode blend_mode = mode;

  if (GET_MODE_SIZE (mode) < 16
      || !vector_operand (op_true, mode))
    op_true = force_reg (mode, op_true);

  op_false = force_reg (mode, op_false);

  switch (mode)
    {
    case E_V2SFmode:
      if (TARGET_SSE4_1)
	gen = gen_mmx_blendvps;
      break;
    case E_V4SFmode:
      if (TARGET_SSE4_1)
	gen = gen_sse4_1_blendvps;
      break;
    case E_V2DFmode:
      if (TARGET_SSE4_1)
	gen = gen_sse4_1_blendvpd;
      break;
    case E_SFmode:
      if (TARGET_SSE4_1)
	gen = gen_sse4_1_blendvss;
      break;
    case E_DFmode:
      if (TARGET_SSE4_1)
	gen = gen_sse4_1_blendvsd;
      break;
    case E_V8QImode:
    case E_V4HImode:
    case E_V4HFmode:
    case E_V4BFmode:
    case E_V2SImode:
      if (TARGET_SSE4_1)
	{
	  gen = gen_mmx_pblendvb_v8qi;
	  blend_mode = V8QImode;
	}
      break;
    case E_V4QImode:
    case E_V2HImode:
    case E_V2HFmode:
    case E_V2BFmode:
      if (TARGET_SSE4_1)
	{
	  gen = gen_mmx_pblendvb_v4qi;
	  blend_mode = V4QImode;
	}
      break;
    case E_V2QImode:
      if (TARGET_SSE4_1)
	gen = gen_mmx_pblendvb_v2qi;
      break;
    case E_V16QImode:
    case E_V8HImode:
    case E_V8HFmode:
    case E_V8BFmode:
    case E_V4SImode:
    case E_V2DImode:
    case E_V1TImode:
      if (TARGET_SSE4_1)
	{
	  gen = gen_sse4_1_pblendvb;
	  blend_mode = V16QImode;
	}
      break;
    case E_V8SFmode:
      if (TARGET_AVX)
	gen = gen_avx_blendvps256;
      break;
    case E_V4DFmode:
      if (TARGET_AVX)
	gen = gen_avx_blendvpd256;
      break;
    case E_V32QImode:
    case E_V16HImode:
    case E_V16HFmode:
    case E_V16BFmode:
    case E_V8SImode:
    case E_V4DImode:
      if (TARGET_AVX2)
	{
	  gen = gen_avx2_pblendvb;
	  blend_mode = V32QImode;
	}
      break;

    case E_V64QImode:
      gen = gen_avx512bw_blendmv64qi;
      break;
    case E_V32HImode:
      gen = gen_avx512bw_blendmv32hi;
      break;
    case E_V32HFmode:
      gen = gen_avx512bw_blendmv32hf;
      break;
    case E_V32BFmode:
      gen = gen_avx512bw_blendmv32bf;
      break;
    case E_V16SImode:
      gen = gen_avx512f_blendmv16si;
      break;
    case E_V8DImode:
      gen = gen_avx512f_blendmv8di;
      break;
    case E_V8DFmode:
      gen = gen_avx512f_blendmv8df;
      break;
    case E_V16SFmode:
      gen = gen_avx512f_blendmv16sf;
      break;

    default:
      break;
    }

  if (gen != NULL)
    {
      if (blend_mode == mode)
	x = dest;
      else
	{
	  x = gen_reg_rtx (blend_mode);
	  op_false = gen_lowpart (blend_mode, op_false);
	  op_true = gen_lowpart (blend_mode, op_true);
	  cmp = gen_lowpart (blend_mode, cmp);
	}

      emit_insn (gen (x, op_false, op_true, cmp));

      if (x != dest)
	emit_move_insn (dest, gen_lowpart (mode, x));
    }
  else
    {
      rtx t2, t3;

      t2 = expand_simple_binop (mode, AND, op_true, cmp,
				NULL, 1, OPTAB_DIRECT);

      t3 = gen_reg_rtx (mode);
      x = gen_rtx_NOT (mode, cmp);
      ix86_emit_vec_binop (AND, mode, t3, x, op_false);

      x = expand_simple_binop (mode, IOR, t3, t2,
			       dest, 1, OPTAB_DIRECT);
      if (x != dest)
	emit_move_insn (dest, x);
    }
}

/* Swap, force into registers, or otherwise massage the two operands
   to an sse comparison with a mask result.  Thus we differ a bit from
   ix86_prepare_fp_compare_args which expects to produce a flags result.

   The DEST operand exists to help determine whether to commute commutative
   operators.  The POP0/POP1 operands are updated in place.  The new
   comparison code is returned, or UNKNOWN if not implementable.  */

static enum rtx_code
ix86_prepare_sse_fp_compare_args (rtx dest, enum rtx_code code,
				  rtx *pop0, rtx *pop1)
{
  switch (code)
    {
    case LTGT:
    case UNEQ:
      /* AVX supports all the needed comparisons.  */
      if (TARGET_AVX)
	break;
      /* We have no LTGT as an operator.  We could implement it with
	 NE & ORDERED, but this requires an extra temporary.  It's
	 not clear that it's worth it.  */
      return UNKNOWN;

    case LT:
    case LE:
    case UNGT:
    case UNGE:
      /* These are supported directly.  */
      break;

    case EQ:
    case NE:
    case UNORDERED:
    case ORDERED:
      /* AVX has 3 operand comparisons, no need to swap anything.  */
      if (TARGET_AVX)
	break;
      /* For commutative operators, try to canonicalize the destination
	 operand to be first in the comparison - this helps reload to
	 avoid extra moves.  */
      if (!dest || !rtx_equal_p (dest, *pop1))
	break;
      /* FALLTHRU */

    case GE:
    case GT:
    case UNLE:
    case UNLT:
      /* These are not supported directly before AVX, and furthermore
	 ix86_expand_sse_fp_minmax only optimizes LT/UNGE.  Swap the
	 comparison operands to transform into something that is
	 supported.  */
      std::swap (*pop0, *pop1);
      code = swap_condition (code);
      break;

    default:
      gcc_unreachable ();
    }

  return code;
}

/* Expand a floating-point conditional move.  Return true if successful.  */

bool
ix86_expand_fp_movcc (rtx operands[])
{
  machine_mode mode = GET_MODE (operands[0]);
  enum rtx_code code = GET_CODE (operands[1]);
  rtx tmp, compare_op;
  rtx op0 = XEXP (operands[1], 0);
  rtx op1 = XEXP (operands[1], 1);

  if (GET_MODE (op0) == BFmode
      && !ix86_fp_comparison_operator (operands[1], VOIDmode))
    return false;

  if (SSE_FLOAT_MODE_SSEMATH_OR_HF_P (mode))
    {
      machine_mode cmode;

      /* Since we've no cmove for sse registers, don't force bad register
	 allocation just to gain access to it.  Deny movcc when the
	 comparison mode doesn't match the move mode.  */
      cmode = GET_MODE (op0);
      if (cmode == VOIDmode)
	cmode = GET_MODE (op1);
      if (cmode != mode)
	return false;

      code = ix86_prepare_sse_fp_compare_args (operands[0], code, &op0, &op1);
      if (code == UNKNOWN)
	return false;

      if (ix86_expand_sse_fp_minmax (operands[0], code, op0, op1,
				     operands[2], operands[3]))
	return true;

      tmp = ix86_expand_sse_cmp (operands[0], code, op0, op1,
				 operands[2], operands[3]);
      ix86_expand_sse_movcc (operands[0], tmp, operands[2], operands[3]);
      return true;
    }

  if (GET_MODE (op0) == TImode
      || (GET_MODE (op0) == DImode
	  && !TARGET_64BIT))
    return false;

  /* The floating point conditional move instructions don't directly
     support conditions resulting from a signed integer comparison.  */

  compare_op = ix86_expand_compare (code, op0, op1);
  if (!fcmov_comparison_operator (compare_op, VOIDmode))
    {
      tmp = gen_reg_rtx (QImode);
      ix86_expand_setcc (tmp, code, op0, op1);

      compare_op = ix86_expand_compare (NE, tmp, const0_rtx);
    }

  emit_insn (gen_rtx_SET (operands[0],
			  gen_rtx_IF_THEN_ELSE (mode, compare_op,
						operands[2], operands[3])));

  return true;
}

/* Helper for ix86_cmp_code_to_pcmp_immediate for int modes.  */

static int
ix86_int_cmp_code_to_pcmp_immediate (enum rtx_code code)
{
  switch (code)
    {
    case EQ:
      return 0;
    case LT:
    case LTU:
      return 1;
    case LE:
    case LEU:
      return 2;
    case NE:
      return 4;
    case GE:
    case GEU:
      return 5;
    case GT:
    case GTU:
      return 6;
    default:
      gcc_unreachable ();
    }
}

/* Helper for ix86_cmp_code_to_pcmp_immediate for fp modes.  */

static int
ix86_fp_cmp_code_to_pcmp_immediate (enum rtx_code code)
{
  switch (code)
    {
    case EQ:
      return 0x00;
    case NE:
      return 0x04;
    case GT:
      return 0x0e;
    case LE:
      return 0x02;
    case GE:
      return 0x0d;
    case LT:
      return 0x01;
    case UNLE:
      return 0x0a;
    case UNLT:
      return 0x09;
    case UNGE:
      return 0x05;
    case UNGT:
      return 0x06;
    case UNEQ:
      return 0x18;
    case LTGT:
      return 0x0c;
    case ORDERED:
      return 0x07;
    case UNORDERED:
      return 0x03;
    default:
      gcc_unreachable ();
    }
}

/* Return immediate value to be used in UNSPEC_PCMP
   for comparison CODE in MODE.  */

static int
ix86_cmp_code_to_pcmp_immediate (enum rtx_code code, machine_mode mode)
{
  if (FLOAT_MODE_P (mode))
    return ix86_fp_cmp_code_to_pcmp_immediate (code);
  return ix86_int_cmp_code_to_pcmp_immediate (code);
}

/* Expand AVX-512 vector comparison.  */

bool
ix86_expand_mask_vec_cmp (rtx dest, enum rtx_code code, rtx cmp_op0, rtx cmp_op1)
{
  machine_mode mask_mode = GET_MODE (dest);
  machine_mode cmp_mode = GET_MODE (cmp_op0);
  rtx imm = GEN_INT (ix86_cmp_code_to_pcmp_immediate (code, cmp_mode));
  int unspec_code;
  rtx unspec;

  switch (code)
    {
    case LEU:
    case GTU:
    case GEU:
    case LTU:
      unspec_code = UNSPEC_UNSIGNED_PCMP;
      break;

    default:
      unspec_code = UNSPEC_PCMP;
    }

  unspec = gen_rtx_UNSPEC (mask_mode, gen_rtvec (3, cmp_op0, cmp_op1, imm),
			   unspec_code);
  emit_insn (gen_rtx_SET (dest, unspec));

  return true;
}

/* Expand fp vector comparison.  */

bool
ix86_expand_fp_vec_cmp (rtx operands[])
{
  enum rtx_code code = GET_CODE (operands[1]);
  rtx cmp;

  code = ix86_prepare_sse_fp_compare_args (operands[0], code,
					   &operands[2], &operands[3]);
  if (code == UNKNOWN)
    {
      rtx temp;
      switch (GET_CODE (operands[1]))
	{
	case LTGT:
	  temp = ix86_expand_sse_cmp (operands[0], ORDERED, operands[2],
				      operands[3], NULL, NULL);
	  cmp = ix86_expand_sse_cmp (operands[0], NE, operands[2],
				     operands[3], NULL, NULL);
	  code = AND;
	  break;
	case UNEQ:
	  temp = ix86_expand_sse_cmp (operands[0], UNORDERED, operands[2],
				      operands[3], NULL, NULL);
	  cmp = ix86_expand_sse_cmp (operands[0], EQ, operands[2],
				     operands[3], NULL, NULL);
	  code = IOR;
	  break;
	default:
	  gcc_unreachable ();
	}
      cmp = expand_simple_binop (GET_MODE (cmp), code, temp, cmp, cmp, 1,
				 OPTAB_DIRECT);
    }
  else
    cmp = ix86_expand_sse_cmp (operands[0], code, operands[2], operands[3],
			       NULL, NULL);

  if (operands[0] != cmp)
    emit_move_insn (operands[0], cmp);

  return true;
}

static rtx
ix86_expand_int_sse_cmp (rtx dest, enum rtx_code code, rtx cop0, rtx cop1,
			 rtx op_true, rtx op_false, bool *negate)
{
  machine_mode data_mode = GET_MODE (dest);
  machine_mode mode = GET_MODE (cop0);
  rtx x;

  *negate = false;

  /* XOP supports all of the comparisons on all 128-bit vector int types.  */
  if (TARGET_XOP
      && GET_MODE_CLASS (mode) == MODE_VECTOR_INT
      && GET_MODE_SIZE (mode) <= 16)
    ;
  /* AVX512F supports all of the comparsions
     on all 128/256/512-bit vector int types.  */
  else if (ix86_use_mask_cmp_p (data_mode, mode, op_true, op_false))
    ;
  else
    {
      /* Canonicalize the comparison to EQ, GT, GTU.  */
      switch (code)
	{
	case EQ:
	case GT:
	case GTU:
	  break;

	case LE:
	case LEU:
	  /* x <= cst can be handled as x < cst + 1 unless there is
	     wrap around in cst + 1.  */
	  if (GET_CODE (cop1) == CONST_VECTOR
	      && GET_MODE_INNER (mode) != TImode)
	    {
	      unsigned int n_elts = GET_MODE_NUNITS (mode), i;
	      machine_mode eltmode = GET_MODE_INNER (mode);
	      for (i = 0; i < n_elts; ++i)
		{
		  rtx elt = CONST_VECTOR_ELT (cop1, i);
		  if (!CONST_INT_P (elt))
		    break;
		  if (code == LE)
		    {
		      /* For LE punt if some element is signed maximum.  */
		      if ((INTVAL (elt) & (GET_MODE_MASK (eltmode) >> 1))
			  == (GET_MODE_MASK (eltmode) >> 1))
			break;
		    }
		  /* For LEU punt if some element is unsigned maximum.  */
		  else if (elt == constm1_rtx)
		    break;
		}
	      if (i == n_elts)
		{
		  rtvec v = rtvec_alloc (n_elts);
		  for (i = 0; i < n_elts; ++i)
		    RTVEC_ELT (v, i)
		      = gen_int_mode (INTVAL (CONST_VECTOR_ELT (cop1, i)) + 1,
				      eltmode);
		  cop1 = gen_rtx_CONST_VECTOR (mode, v);
		  std::swap (cop0, cop1);
		  code = code == LE ? GT : GTU;
		  break;
		}
	    }
	  /* FALLTHRU */
	case NE:
	  code = reverse_condition (code);
	  *negate = true;
	  break;

	case GE:
	case GEU:
	  /* x >= cst can be handled as x > cst - 1 unless there is
	     wrap around in cst - 1.  */
	  if (GET_CODE (cop1) == CONST_VECTOR
	      && GET_MODE_INNER (mode) != TImode)
	    {
	      unsigned int n_elts = GET_MODE_NUNITS (mode), i;
	      machine_mode eltmode = GET_MODE_INNER (mode);
	      for (i = 0; i < n_elts; ++i)
		{
		  rtx elt = CONST_VECTOR_ELT (cop1, i);
		  if (!CONST_INT_P (elt))
		    break;
		  if (code == GE)
		    {
		      /* For GE punt if some element is signed minimum.  */
		      if (INTVAL (elt) < 0
			  && ((INTVAL (elt) & (GET_MODE_MASK (eltmode) >> 1))
			      == 0))
			break;
		    }
		  /* For GEU punt if some element is zero.  */
		  else if (elt == const0_rtx)
		    break;
		}
	      if (i == n_elts)
		{
		  rtvec v = rtvec_alloc (n_elts);
		  for (i = 0; i < n_elts; ++i)
		    RTVEC_ELT (v, i)
		      = gen_int_mode (INTVAL (CONST_VECTOR_ELT (cop1, i)) - 1,
				      eltmode);
		  cop1 = gen_rtx_CONST_VECTOR (mode, v);
		  code = code == GE ? GT : GTU;
		  break;
		}
	    }
	  code = reverse_condition (code);
	  *negate = true;
	  /* FALLTHRU */

	case LT:
	case LTU:
	  std::swap (cop0, cop1);
	  code = swap_condition (code);
	  break;

	default:
	  gcc_unreachable ();
	}

      /* Only SSE4.1/SSE4.2 supports V2DImode.  */
      if (mode == V2DImode)
	{
	  switch (code)
	    {
	    case EQ:
	      /* SSE4.1 supports EQ.  */
	      if (!TARGET_SSE4_1)
		return NULL;
	      break;

	    case GT:
	    case GTU:
	      /* SSE4.2 supports GT/GTU.  */
	      if (!TARGET_SSE4_2)
		return NULL;
	      break;

	    default:
	      gcc_unreachable ();
	    }
	}

      if (GET_CODE (cop0) == CONST_VECTOR)
	cop0 = force_reg (mode, cop0);
      else if (GET_CODE (cop1) == CONST_VECTOR)
	cop1 = force_reg (mode, cop1);

      rtx optrue = op_true ? op_true : CONSTM1_RTX (data_mode);
      rtx opfalse = op_false ? op_false : CONST0_RTX (data_mode);
      if (*negate)
	std::swap (optrue, opfalse);

      /* Transform x > y ? 0 : -1 (i.e. x <= y ? -1 : 0 or x <= y) when
	 not using integer masks into min (x, y) == x ? -1 : 0 (i.e.
	 min (x, y) == x).  While we add one instruction (the minimum),
	 we remove the need for two instructions in the negation, as the
	 result is done this way.
	 When using masks, do it for SI/DImode element types, as it is shorter
	 than the two subtractions.  */
      if ((code != EQ
	   && GET_MODE_SIZE (mode) != 64
	   && vector_all_ones_operand (opfalse, data_mode)
	   && optrue == CONST0_RTX (data_mode))
	  || (code == GTU
	      && GET_MODE_SIZE (GET_MODE_INNER (mode)) >= 4
	      /* Don't do it if not using integer masks and we'd end up with
		 the right values in the registers though.  */
	      && ((GET_MODE_SIZE (mode) == 64 && TARGET_EVEX512)
		  || !vector_all_ones_operand (optrue, data_mode)
		  || opfalse != CONST0_RTX (data_mode))))
	{
	  rtx (*gen) (rtx, rtx, rtx) = NULL;

	  switch (mode)
	    {
	    case E_V16SImode:
	      gen = (code == GTU) ? gen_uminv16si3 : gen_sminv16si3;
	      break;
	    case E_V8DImode:
	      gen = (code == GTU) ? gen_uminv8di3 : gen_sminv8di3;
	      cop0 = force_reg (mode, cop0);
	      cop1 = force_reg (mode, cop1);
	      break;
	    case E_V32QImode:
	      if (TARGET_AVX2)
		gen = (code == GTU) ? gen_uminv32qi3 : gen_sminv32qi3;
	      break;
	    case E_V16HImode:
	      if (TARGET_AVX2)
		gen = (code == GTU) ? gen_uminv16hi3 : gen_sminv16hi3;
	      break;
	    case E_V8SImode:
	      if (TARGET_AVX2)
		gen = (code == GTU) ? gen_uminv8si3 : gen_sminv8si3;
	      break;
	    case E_V4DImode:
	      if (TARGET_AVX512VL)
		{
		  gen = (code == GTU) ? gen_uminv4di3 : gen_sminv4di3;
		  cop0 = force_reg (mode, cop0);
		  cop1 = force_reg (mode, cop1);
		}
	      break;
	    case E_V16QImode:
	      if (code == GTU && TARGET_SSE2)
		gen = gen_uminv16qi3;
	      else if (code == GT && TARGET_SSE4_1)
		gen = gen_sminv16qi3;
	      break;
	    case E_V8QImode:
	      if (code == GTU && TARGET_SSE2)
		gen = gen_uminv8qi3;
	      else if (code == GT && TARGET_SSE4_1)
		gen = gen_sminv8qi3;
	      break;
	    case E_V4QImode:
	      if (code == GTU && TARGET_SSE2)
		gen = gen_uminv4qi3;
	      else if (code == GT && TARGET_SSE4_1)
		gen = gen_sminv4qi3;
	      break;
	    case E_V2QImode:
	      if (code == GTU && TARGET_SSE2)
		gen = gen_uminv2qi3;
	      else if (code == GT && TARGET_SSE4_1)
		gen = gen_sminv2qi3;
	      break;
	    case E_V8HImode:
	      if (code == GTU && TARGET_SSE4_1)
		gen = gen_uminv8hi3;
	      else if (code == GT && TARGET_SSE2)
		gen = gen_sminv8hi3;
	      break;
	    case E_V4HImode:
	      if (code == GTU && TARGET_SSE4_1)
		gen = gen_uminv4hi3;
	      else if (code == GT && TARGET_SSE2)
		gen = gen_sminv4hi3;
	      break;
	    case E_V2HImode:
	      if (code == GTU && TARGET_SSE4_1)
		gen = gen_uminv2hi3;
	      else if (code == GT && TARGET_SSE2)
		gen = gen_sminv2hi3;
	      break;
	    case E_V4SImode:
	      if (TARGET_SSE4_1)
		gen = (code == GTU) ? gen_uminv4si3 : gen_sminv4si3;
	      break;
	    case E_V2SImode:
	      if (TARGET_SSE4_1)
		gen = (code == GTU) ? gen_uminv2si3 : gen_sminv2si3;
	      break;
	    case E_V2DImode:
	      if (TARGET_AVX512VL)
		{
		  gen = (code == GTU) ? gen_uminv2di3 : gen_sminv2di3;
		  cop0 = force_reg (mode, cop0);
		  cop1 = force_reg (mode, cop1);
		}
	      break;
	    default:
	      break;
	    }

	  if (gen)
	    {
	      rtx tem = gen_reg_rtx (mode);
	      if (!vector_operand (cop0, mode))
		cop0 = force_reg (mode, cop0);
	      if (!vector_operand (cop1, mode))
		cop1 = force_reg (mode, cop1);
	      *negate = !*negate;
	      emit_insn (gen (tem, cop0, cop1));
	      cop1 = tem;
	      code = EQ;
	    }
	}

      /* Unsigned parallel compare is not supported by the hardware.
	 Play some tricks to turn this into a signed comparison
	 against 0.  */
      if (code == GTU)
	{
	  cop0 = force_reg (mode, cop0);

	  switch (mode)
	    {
	    case E_V16SImode:
	    case E_V8DImode:
	    case E_V8SImode:
	    case E_V4DImode:
	    case E_V4SImode:
	    case E_V2SImode:
	    case E_V2DImode:
		{
		  rtx t1, t2, mask;

		  /* Subtract (-(INT MAX) - 1) from both operands to make
		     them signed.  */
		  mask = ix86_build_signbit_mask (mode, true, false);
		  t1 = gen_reg_rtx (mode);
		  emit_insn (gen_sub3_insn (t1, cop0, mask));

		  t2 = gen_reg_rtx (mode);
		  emit_insn (gen_sub3_insn (t2, cop1, mask));

		  cop0 = t1;
		  cop1 = t2;
		  code = GT;
		}
	      break;

	    case E_V64QImode:
	    case E_V32HImode:
	    case E_V32QImode:
	    case E_V16HImode:
	    case E_V16QImode:
	    case E_V8QImode:
	    case E_V4QImode:
	    case E_V2QImode:
	    case E_V8HImode:
	    case E_V4HImode:
	    case E_V2HImode:
	      /* Perform a parallel unsigned saturating subtraction.  */
	      x = gen_reg_rtx (mode);
	      emit_insn (gen_rtx_SET
			 (x, gen_rtx_US_MINUS (mode, cop0, cop1)));
	      cop0 = x;
	      cop1 = CONST0_RTX (mode);
	      code = EQ;
	      *negate = !*negate;
	      break;

	    default:
	      gcc_unreachable ();
	    }
	}
    }

  if (*negate)
    std::swap (op_true, op_false);

  if (GET_CODE (cop1) == CONST_VECTOR)
    cop1 = force_reg (mode, cop1);

  /* Allow the comparison to be done in one mode, but the movcc to
     happen in another mode.  */
  if (data_mode == mode)
    x = ix86_expand_sse_cmp (dest, code, cop0, cop1, op_true, op_false);
  else
    {
      gcc_assert (GET_MODE_SIZE (data_mode) == GET_MODE_SIZE (mode));
      x = ix86_expand_sse_cmp (gen_reg_rtx (mode), code, cop0, cop1,
			       op_true, op_false);
      if (GET_MODE (x) == mode)
	x = gen_lowpart (data_mode, x);
    }

  return x;
}

/* Expand integer vector comparison.  */

bool
ix86_expand_int_vec_cmp (rtx operands[])
{
  rtx_code code = GET_CODE (operands[1]);
  bool negate = false;
  rtx cmp = ix86_expand_int_sse_cmp (operands[0], code, operands[2],
				     operands[3], NULL, NULL, &negate);

  if (!cmp)
    return false;

  if (negate)
    cmp = ix86_expand_int_sse_cmp (operands[0], EQ, cmp,
				   CONST0_RTX (GET_MODE (cmp)),
				   NULL, NULL, &negate);

  gcc_assert (!negate);

  if (operands[0] != cmp)
    emit_move_insn (operands[0], cmp);

  return true;
}

/* Expand a floating-point vector conditional move; a vcond operation
   rather than a movcc operation.  */

bool
ix86_expand_fp_vcond (rtx operands[])
{
  enum rtx_code code = GET_CODE (operands[3]);
  rtx cmp;

  code = ix86_prepare_sse_fp_compare_args (operands[0], code,
					   &operands[4], &operands[5]);
  if (code == UNKNOWN)
    {
      rtx temp;
      switch (GET_CODE (operands[3]))
	{
	case LTGT:
	  temp = ix86_expand_sse_cmp (operands[0], ORDERED, operands[4],
				      operands[5], operands[0], operands[0]);
	  cmp = ix86_expand_sse_cmp (operands[0], NE, operands[4],
				     operands[5], operands[1], operands[2]);
	  code = AND;
	  break;
	case UNEQ:
	  temp = ix86_expand_sse_cmp (operands[0], UNORDERED, operands[4],
				      operands[5], operands[0], operands[0]);
	  cmp = ix86_expand_sse_cmp (operands[0], EQ, operands[4],
				     operands[5], operands[1], operands[2]);
	  code = IOR;
	  break;
	default:
	  gcc_unreachable ();
	}
      cmp = expand_simple_binop (GET_MODE (cmp), code, temp, cmp, cmp, 1,
				 OPTAB_DIRECT);
      ix86_expand_sse_movcc (operands[0], cmp, operands[1], operands[2]);
      return true;
    }

  if (ix86_expand_sse_fp_minmax (operands[0], code, operands[4],
				 operands[5], operands[1], operands[2]))
    return true;

  cmp = ix86_expand_sse_cmp (operands[0], code, operands[4], operands[5],
			     operands[1], operands[2]);
  ix86_expand_sse_movcc (operands[0], cmp, operands[1], operands[2]);
  return true;
}

/* Expand a signed/unsigned integral vector conditional move.  */

bool
ix86_expand_int_vcond (rtx operands[])
{
  machine_mode data_mode = GET_MODE (operands[0]);
  machine_mode mode = GET_MODE (operands[4]);
  enum rtx_code code = GET_CODE (operands[3]);
  bool negate = false;
  rtx x, cop0, cop1;

  cop0 = operands[4];
  cop1 = operands[5];

  /* Try to optimize x < 0 ? -1 : 0 into (signed) x >> 31
     and x < 0 ? 1 : 0 into (unsigned) x >> 31.  */
  if ((code == LT || code == GE)
      && data_mode == mode
      && cop1 == CONST0_RTX (mode)
      && operands[1 + (code == LT)] == CONST0_RTX (data_mode)
      && GET_MODE_UNIT_SIZE (data_mode) > 1
      && GET_MODE_UNIT_SIZE (data_mode) <= 8
      && (GET_MODE_SIZE (data_mode) == 16
	  || (TARGET_AVX2 && GET_MODE_SIZE (data_mode) == 32)))
    {
      rtx negop = operands[2 - (code == LT)];
      int shift = GET_MODE_UNIT_BITSIZE (data_mode) - 1;
      if (negop == CONST1_RTX (data_mode))
	{
	  rtx res = expand_simple_binop (mode, LSHIFTRT, cop0, GEN_INT (shift),
					 operands[0], 1, OPTAB_DIRECT);
	  if (res != operands[0])
	    emit_move_insn (operands[0], res);
	  return true;
	}
      else if (GET_MODE_INNER (data_mode) != DImode
	       && vector_all_ones_operand (negop, data_mode))
	{
	  rtx res = expand_simple_binop (mode, ASHIFTRT, cop0, GEN_INT (shift),
					 operands[0], 0, OPTAB_DIRECT);
	  if (res != operands[0])
	    emit_move_insn (operands[0], res);
	  return true;
	}
    }

  if (!nonimmediate_operand (cop1, mode))
    cop1 = force_reg (mode, cop1);
  if (!general_operand (operands[1], data_mode))
    operands[1] = force_reg (data_mode, operands[1]);
  if (!general_operand (operands[2], data_mode))
    operands[2] = force_reg (data_mode, operands[2]);

  x = ix86_expand_int_sse_cmp (operands[0], code, cop0, cop1,
			       operands[1], operands[2], &negate);

  if (!x)
    return false;

  ix86_expand_sse_movcc (operands[0], x, operands[1+negate],
			 operands[2-negate]);
  return true;
}

static bool
ix86_expand_vec_perm_vpermt2 (rtx target, rtx mask, rtx op0, rtx op1,
			      struct expand_vec_perm_d *d)
{
  /* ix86_expand_vec_perm_vpermt2 is called from both const and non-const
     expander, so args are either in d, or in op0, op1 etc.  */
  machine_mode mode = GET_MODE (d ? d->op0 : op0);
  machine_mode maskmode = mode;
  rtx (*gen) (rtx, rtx, rtx, rtx) = NULL;

  switch (mode)
    {
    case E_V16QImode:
      if (TARGET_AVX512VL && TARGET_AVX512VBMI)
	gen = gen_avx512vl_vpermt2varv16qi3;
      break;
    case E_V32QImode:
      if (TARGET_AVX512VL && TARGET_AVX512VBMI)
	gen = gen_avx512vl_vpermt2varv32qi3;
      break;
    case E_V64QImode:
      if (TARGET_AVX512VBMI)
	gen = gen_avx512bw_vpermt2varv64qi3;
      break;
    case E_V8HImode:
      if (TARGET_AVX512VL && TARGET_AVX512BW)
	gen = gen_avx512vl_vpermt2varv8hi3;
      break;
    case E_V16HImode:
      if (TARGET_AVX512VL && TARGET_AVX512BW)
	gen = gen_avx512vl_vpermt2varv16hi3;
      break;
    case E_V32HImode:
      if (TARGET_AVX512BW)
	gen = gen_avx512bw_vpermt2varv32hi3;
      break;
    case E_V4SImode:
      if (TARGET_AVX512VL)
	gen = gen_avx512vl_vpermt2varv4si3;
      break;
    case E_V8SImode:
      if (TARGET_AVX512VL)
	gen = gen_avx512vl_vpermt2varv8si3;
      break;
    case E_V16SImode:
      if (TARGET_AVX512F)
	gen = gen_avx512f_vpermt2varv16si3;
      break;
    case E_V4SFmode:
      if (TARGET_AVX512VL)
	{
	  gen = gen_avx512vl_vpermt2varv4sf3;
	  maskmode = V4SImode;
	}
      break;
    case E_V8SFmode:
      if (TARGET_AVX512VL)
	{
	  gen = gen_avx512vl_vpermt2varv8sf3;
	  maskmode = V8SImode;
	}
      break;
    case E_V16SFmode:
      if (TARGET_AVX512F)
	{
	  gen = gen_avx512f_vpermt2varv16sf3;
	  maskmode = V16SImode;
	}
      break;
    case E_V2DImode:
      if (TARGET_AVX512VL)
	gen = gen_avx512vl_vpermt2varv2di3;
      break;
    case E_V4DImode:
      if (TARGET_AVX512VL)
	gen = gen_avx512vl_vpermt2varv4di3;
      break;
    case E_V8DImode:
      if (TARGET_AVX512F)
	gen = gen_avx512f_vpermt2varv8di3;
      break;
    case E_V2DFmode:
      if (TARGET_AVX512VL)
	{
	  gen = gen_avx512vl_vpermt2varv2df3;
	  maskmode = V2DImode;
	}
      break;
    case E_V4DFmode:
      if (TARGET_AVX512VL)
	{
	  gen = gen_avx512vl_vpermt2varv4df3;
	  maskmode = V4DImode;
	}
      break;
    case E_V8DFmode:
      if (TARGET_AVX512F)
	{
	  gen = gen_avx512f_vpermt2varv8df3;
	  maskmode = V8DImode;
	}
      break;
    default:
      break;
    }

  if (gen == NULL)
    return false;

  if (d && d->testing_p)
    return true;

  /* ix86_expand_vec_perm_vpermt2 is called from both const and non-const
     expander, so args are either in d, or in op0, op1 etc.  */
  if (d)
    {
      rtx vec[64];
      target = d->target;
      op0 = d->op0;
      op1 = d->op1;
      for (int i = 0; i < d->nelt; ++i)
	vec[i] = GEN_INT (d->perm[i]);
      mask = gen_rtx_CONST_VECTOR (maskmode, gen_rtvec_v (d->nelt, vec));
    }

  emit_insn (gen (target, force_reg (maskmode, mask), op0, op1));
  return true;
}

/* Expand a variable vector permutation.  */

void
ix86_expand_vec_perm (rtx operands[])
{
  rtx target = operands[0];
  rtx op0 = operands[1];
  rtx op1 = operands[2];
  rtx mask = operands[3];
  rtx t1, t2, t3, t4, t5, t6, t7, t8, vt, vt2, vec[32];
  machine_mode mode = GET_MODE (op0);
  machine_mode maskmode = GET_MODE (mask);
  int w, e, i;
  bool one_operand_shuffle = rtx_equal_p (op0, op1);

  /* Number of elements in the vector.  */
  w = GET_MODE_NUNITS (mode);
  e = GET_MODE_UNIT_SIZE (mode);
  gcc_assert (w <= 64);

  /* For HF mode vector, convert it to HI using subreg.  */
  if (GET_MODE_INNER (mode) == HFmode)
    {
      machine_mode orig_mode = mode;
      mode = mode_for_vector (HImode, w).require ();
      target = lowpart_subreg (mode, target, orig_mode);
      op0 = lowpart_subreg (mode, op0, orig_mode);
      op1 = lowpart_subreg (mode, op1, orig_mode);
    }

  if (TARGET_AVX512F && one_operand_shuffle)
    {
      rtx (*gen) (rtx, rtx, rtx) = NULL;
      switch (mode)
	{
	case E_V16SImode:
	  gen =gen_avx512f_permvarv16si;
	  break;
	case E_V16SFmode:
	  gen = gen_avx512f_permvarv16sf;
	  break;
	case E_V8DImode:
	  gen = gen_avx512f_permvarv8di;
	  break;
	case E_V8DFmode:
	  gen = gen_avx512f_permvarv8df;
	  break;
	default:
	  break;
	}
      if (gen != NULL)
	{
	  emit_insn (gen (target, op0, mask));
	  return;
	}
    }

  if (ix86_expand_vec_perm_vpermt2 (target, mask, op0, op1, NULL))
    return;

  if (TARGET_AVX2)
    {
      if (mode == V4DImode || mode == V4DFmode || mode == V16HImode)
	{
	  /* Unfortunately, the VPERMQ and VPERMPD instructions only support
	     an constant shuffle operand.  With a tiny bit of effort we can
	     use VPERMD instead.  A re-interpretation stall for V4DFmode is
	     unfortunate but there's no avoiding it.
	     Similarly for V16HImode we don't have instructions for variable
	     shuffling, while for V32QImode we can use after preparing suitable
	     masks vpshufb; vpshufb; vpermq; vpor.  */

	  if (mode == V16HImode)
	    {
	      maskmode = mode = V32QImode;
	      w = 32;
	      e = 1;
	    }
	  else
	    {
	      maskmode = mode = V8SImode;
	      w = 8;
	      e = 4;
	    }
	  t1 = gen_reg_rtx (maskmode);

	  /* Replicate the low bits of the V4DImode mask into V8SImode:
	       mask = { A B C D }
	       t1 = { A A B B C C D D }.  */
	  for (i = 0; i < w / 2; ++i)
	    vec[i*2 + 1] = vec[i*2] = GEN_INT (i * 2);
	  vt = gen_rtx_CONST_VECTOR (maskmode, gen_rtvec_v (w, vec));
	  vt = force_reg (maskmode, vt);
	  mask = gen_lowpart (maskmode, mask);
	  if (maskmode == V8SImode)
	    emit_insn (gen_avx2_permvarv8si (t1, mask, vt));
	  else
	    emit_insn (gen_avx2_pshufbv32qi3 (t1, mask, vt));

	  /* Multiply the shuffle indicies by two.  */
	  t1 = expand_simple_binop (maskmode, PLUS, t1, t1, t1, 1,
				    OPTAB_DIRECT);

	  /* Add one to the odd shuffle indicies:
		t1 = { A*2, A*2+1, B*2, B*2+1, ... }.  */
	  for (i = 0; i < w / 2; ++i)
	    {
	      vec[i * 2] = const0_rtx;
	      vec[i * 2 + 1] = const1_rtx;
	    }
	  vt = gen_rtx_CONST_VECTOR (maskmode, gen_rtvec_v (w, vec));
	  vt = validize_mem (force_const_mem (maskmode, vt));
	  t1 = expand_simple_binop (maskmode, PLUS, t1, vt, t1, 1,
				    OPTAB_DIRECT);

	  /* Continue as if V8SImode (resp. V32QImode) was used initially.  */
	  operands[3] = mask = t1;
	  target = gen_reg_rtx (mode);
	  op0 = gen_lowpart (mode, op0);
	  op1 = gen_lowpart (mode, op1);
	}

      switch (mode)
	{
	case E_V8SImode:
	  /* The VPERMD and VPERMPS instructions already properly ignore
	     the high bits of the shuffle elements.  No need for us to
	     perform an AND ourselves.  */
	  if (one_operand_shuffle)
	    {
	      emit_insn (gen_avx2_permvarv8si (target, op0, mask));
	      if (target != operands[0])
		emit_move_insn (operands[0],
				gen_lowpart (GET_MODE (operands[0]), target));
	    }
	  else
	    {
	      t1 = gen_reg_rtx (V8SImode);
	      t2 = gen_reg_rtx (V8SImode);
	      emit_insn (gen_avx2_permvarv8si (t1, op0, mask));
	      emit_insn (gen_avx2_permvarv8si (t2, op1, mask));
	      goto merge_two;
	    }
	  return;

	case E_V8SFmode:
	  mask = gen_lowpart (V8SImode, mask);
	  if (one_operand_shuffle)
	    emit_insn (gen_avx2_permvarv8sf (target, op0, mask));
	  else
	    {
	      t1 = gen_reg_rtx (V8SFmode);
	      t2 = gen_reg_rtx (V8SFmode);
	      emit_insn (gen_avx2_permvarv8sf (t1, op0, mask));
	      emit_insn (gen_avx2_permvarv8sf (t2, op1, mask));
	      goto merge_two;
	    }
	  return;

        case E_V4SImode:
	  /* By combining the two 128-bit input vectors into one 256-bit
	     input vector, we can use VPERMD and VPERMPS for the full
	     two-operand shuffle.  */
	  t1 = gen_reg_rtx (V8SImode);
	  t2 = gen_reg_rtx (V8SImode);
	  emit_insn (gen_avx_vec_concatv8si (t1, op0, op1));
	  emit_insn (gen_avx_vec_concatv8si (t2, mask, mask));
	  emit_insn (gen_avx2_permvarv8si (t1, t1, t2));
	  emit_insn (gen_avx_vextractf128v8si (target, t1, const0_rtx));
	  return;

        case E_V4SFmode:
	  t1 = gen_reg_rtx (V8SFmode);
	  t2 = gen_reg_rtx (V8SImode);
	  mask = gen_lowpart (V4SImode, mask);
	  emit_insn (gen_avx_vec_concatv8sf (t1, op0, op1));
	  emit_insn (gen_avx_vec_concatv8si (t2, mask, mask));
	  emit_insn (gen_avx2_permvarv8sf (t1, t1, t2));
	  emit_insn (gen_avx_vextractf128v8sf (target, t1, const0_rtx));
	  return;

	case E_V32QImode:
	  t1 = gen_reg_rtx (V32QImode);
	  t2 = gen_reg_rtx (V32QImode);
	  t3 = gen_reg_rtx (V32QImode);
	  vt2 = GEN_INT (-128);
	  vt = gen_const_vec_duplicate (V32QImode, vt2);
	  vt = force_reg (V32QImode, vt);
	  for (i = 0; i < 32; i++)
	    vec[i] = i < 16 ? vt2 : const0_rtx;
	  vt2 = gen_rtx_CONST_VECTOR (V32QImode, gen_rtvec_v (32, vec));
	  vt2 = force_reg (V32QImode, vt2);
	  /* From mask create two adjusted masks, which contain the same
	     bits as mask in the low 7 bits of each vector element.
	     The first mask will have the most significant bit clear
	     if it requests element from the same 128-bit lane
	     and MSB set if it requests element from the other 128-bit lane.
	     The second mask will have the opposite values of the MSB,
	     and additionally will have its 128-bit lanes swapped.
	     E.g. { 07 12 1e 09 ... | 17 19 05 1f ... } mask vector will have
	     t1   { 07 92 9e 09 ... | 17 19 85 1f ... } and
	     t3   { 97 99 05 9f ... | 87 12 1e 89 ... } where each ...
	     stands for other 12 bytes.  */
	  /* The bit whether element is from the same lane or the other
	     lane is bit 4, so shift it up by 3 to the MSB position.  */
	  t5 = gen_reg_rtx (V4DImode);
	  emit_insn (gen_ashlv4di3 (t5, gen_lowpart (V4DImode, mask),
				    GEN_INT (3)));
	  /* Clear MSB bits from the mask just in case it had them set.  */
	  emit_insn (gen_avx2_andnotv32qi3 (t2, vt, mask));
	  /* After this t1 will have MSB set for elements from other lane.  */
	  emit_insn (gen_xorv32qi3 (t1, gen_lowpart (V32QImode, t5), vt2));
	  /* Clear bits other than MSB.  */
	  emit_insn (gen_andv32qi3 (t1, t1, vt));
	  /* Or in the lower bits from mask into t3.  */
	  emit_insn (gen_iorv32qi3 (t3, t1, t2));
	  /* And invert MSB bits in t1, so MSB is set for elements from the same
	     lane.  */
	  emit_insn (gen_xorv32qi3 (t1, t1, vt));
	  /* Swap 128-bit lanes in t3.  */
	  t6 = gen_reg_rtx (V4DImode);
	  emit_insn (gen_avx2_permv4di_1 (t6, gen_lowpart (V4DImode, t3),
					  const2_rtx, GEN_INT (3),
					  const0_rtx, const1_rtx));
	  /* And or in the lower bits from mask into t1.  */
	  emit_insn (gen_iorv32qi3 (t1, t1, t2));
	  if (one_operand_shuffle)
	    {
	      /* Each of these shuffles will put 0s in places where
		 element from the other 128-bit lane is needed, otherwise
		 will shuffle in the requested value.  */
	      emit_insn (gen_avx2_pshufbv32qi3 (t3, op0,
						gen_lowpart (V32QImode, t6)));
	      emit_insn (gen_avx2_pshufbv32qi3 (t1, op0, t1));
	      /* For t3 the 128-bit lanes are swapped again.  */
	      t7 = gen_reg_rtx (V4DImode);
	      emit_insn (gen_avx2_permv4di_1 (t7, gen_lowpart (V4DImode, t3),
					      const2_rtx, GEN_INT (3),
					      const0_rtx, const1_rtx));
	      /* And oring both together leads to the result.  */
	      emit_insn (gen_iorv32qi3 (target, t1,
					gen_lowpart (V32QImode, t7)));
	      if (target != operands[0])
		emit_move_insn (operands[0],
				gen_lowpart (GET_MODE (operands[0]), target));
	      return;
	    }

	  t4 = gen_reg_rtx (V32QImode);
	  /* Similarly to the above one_operand_shuffle code,
	     just for repeated twice for each operand.  merge_two:
	     code will merge the two results together.  */
	  emit_insn (gen_avx2_pshufbv32qi3 (t4, op0,
					    gen_lowpart (V32QImode, t6)));
	  emit_insn (gen_avx2_pshufbv32qi3 (t3, op1,
					    gen_lowpart (V32QImode, t6)));
	  emit_insn (gen_avx2_pshufbv32qi3 (t2, op0, t1));
	  emit_insn (gen_avx2_pshufbv32qi3 (t1, op1, t1));
	  t7 = gen_reg_rtx (V4DImode);
	  emit_insn (gen_avx2_permv4di_1 (t7, gen_lowpart (V4DImode, t4),
					  const2_rtx, GEN_INT (3),
					  const0_rtx, const1_rtx));
	  t8 = gen_reg_rtx (V4DImode);
	  emit_insn (gen_avx2_permv4di_1 (t8, gen_lowpart (V4DImode, t3),
					  const2_rtx, GEN_INT (3),
					  const0_rtx, const1_rtx));
	  emit_insn (gen_iorv32qi3 (t4, t2, gen_lowpart (V32QImode, t7)));
	  emit_insn (gen_iorv32qi3 (t3, t1, gen_lowpart (V32QImode, t8)));
	  t1 = t4;
	  t2 = t3;
	  goto merge_two;

	default:
	  gcc_assert (GET_MODE_SIZE (mode) <= 16);
	  break;
	}
    }

  if (TARGET_XOP)
    {
      /* The XOP VPPERM insn supports three inputs.  By ignoring the 
	 one_operand_shuffle special case, we avoid creating another
	 set of constant vectors in memory.  */
      one_operand_shuffle = false;

      /* mask = mask & {2*w-1, ...} */
      vt = GEN_INT (2*w - 1);
    }
  else
    {
      /* mask = mask & {w-1, ...} */
      vt = GEN_INT (w - 1);
    }

  vt = gen_const_vec_duplicate (maskmode, vt);
  mask = expand_simple_binop (maskmode, AND, mask, vt,
			      NULL_RTX, 0, OPTAB_DIRECT);

  /* For non-QImode operations, convert the word permutation control
     into a byte permutation control.  */
  if (mode != V16QImode)
    {
      mask = expand_simple_binop (maskmode, ASHIFT, mask,
				  GEN_INT (exact_log2 (e)),
				  NULL_RTX, 0, OPTAB_DIRECT);

      /* Convert mask to vector of chars.  */
      mask = force_reg (V16QImode, gen_lowpart (V16QImode, mask));

      /* Replicate each of the input bytes into byte positions:
	 (v2di) --> {0,0,0,0,0,0,0,0, 8,8,8,8,8,8,8,8}
	 (v4si) --> {0,0,0,0, 4,4,4,4, 8,8,8,8, 12,12,12,12}
	 (v8hi) --> {0,0, 2,2, 4,4, 6,6, ...}.  */
      for (i = 0; i < 16; ++i)
	vec[i] = GEN_INT (i/e * e);
      vt = gen_rtx_CONST_VECTOR (V16QImode, gen_rtvec_v (16, vec));
      vt = validize_mem (force_const_mem (V16QImode, vt));
      if (TARGET_XOP)
	emit_insn (gen_xop_pperm (mask, mask, mask, vt));
      else
	emit_insn (gen_ssse3_pshufbv16qi3 (mask, mask, vt));

      /* Convert it into the byte positions by doing
	 mask = mask + {0,1,..,16/w, 0,1,..,16/w, ...}  */
      for (i = 0; i < 16; ++i)
	vec[i] = GEN_INT (i % e);
      vt = gen_rtx_CONST_VECTOR (V16QImode, gen_rtvec_v (16, vec));
      vt = validize_mem (force_const_mem (V16QImode, vt));
      emit_insn (gen_addv16qi3 (mask, mask, vt));
    }

  /* The actual shuffle operations all operate on V16QImode.  */
  op0 = gen_lowpart (V16QImode, op0);
  op1 = gen_lowpart (V16QImode, op1);

  if (TARGET_XOP)
    {
      if (GET_MODE (target) != V16QImode)
	target = gen_reg_rtx (V16QImode);
      emit_insn (gen_xop_pperm (target, op0, op1, mask));
      if (target != operands[0])
	emit_move_insn (operands[0],
			gen_lowpart (GET_MODE (operands[0]), target));
    }
  else if (one_operand_shuffle)
    {
      if (GET_MODE (target) != V16QImode)
	target = gen_reg_rtx (V16QImode);
      emit_insn (gen_ssse3_pshufbv16qi3 (target, op0, mask));
      if (target != operands[0])
	emit_move_insn (operands[0],
			gen_lowpart (GET_MODE (operands[0]), target));
    }
  else
    {
      rtx xops[6];
      bool ok;

      /* Shuffle the two input vectors independently.  */
      t1 = gen_reg_rtx (V16QImode);
      t2 = gen_reg_rtx (V16QImode);
      emit_insn (gen_ssse3_pshufbv16qi3 (t1, op0, mask));
      emit_insn (gen_ssse3_pshufbv16qi3 (t2, op1, mask));

 merge_two:
      /* Then merge them together.  The key is whether any given control
         element contained a bit set that indicates the second word.  */
      mask = operands[3];
      vt = GEN_INT (w);
      if (maskmode == V2DImode && !TARGET_SSE4_1)
	{
	  /* Without SSE4.1, we don't have V2DImode EQ.  Perform one
	     more shuffle to convert the V2DI input mask into a V4SI
	     input mask.  At which point the masking that expand_int_vcond
	     will work as desired.  */
	  rtx t3 = gen_reg_rtx (V4SImode);
	  emit_insn (gen_sse2_pshufd_1 (t3, gen_lowpart (V4SImode, mask),
				        const0_rtx, const0_rtx,
				        const2_rtx, const2_rtx));
	  mask = t3;
	  maskmode = V4SImode;
	  e = w = 4;
	}

      vt = gen_const_vec_duplicate (maskmode, vt);
      vt = force_reg (maskmode, vt);
      mask = expand_simple_binop (maskmode, AND, mask, vt,
				  NULL_RTX, 0, OPTAB_DIRECT);

      if (GET_MODE (target) != mode)
	target = gen_reg_rtx (mode);
      xops[0] = target;
      xops[1] = gen_lowpart (mode, t2);
      xops[2] = gen_lowpart (mode, t1);
      xops[3] = gen_rtx_EQ (maskmode, mask, vt);
      xops[4] = mask;
      xops[5] = vt;
      ok = ix86_expand_int_vcond (xops);
      gcc_assert (ok);
      if (target != operands[0])
	emit_move_insn (operands[0],
			gen_lowpart (GET_MODE (operands[0]), target));
    }
}

/* Extend SRC into next wider integer vector type.  UNSIGNED_P is
   true if we should do zero extension, else sign extension.  */

void
ix86_expand_sse_extend (rtx dest, rtx src, bool unsigned_p)
{
  machine_mode imode = GET_MODE (src);
  rtx ops[3];

  switch (imode)
    {
    case E_V8QImode:
    case E_V4QImode:
    case E_V2QImode:
    case E_V4HImode:
    case E_V2HImode:
    case E_V2SImode:
      break;
    default:
      gcc_unreachable ();
    }

  ops[0] = dest;

  ops[1] = force_reg (imode, src);

  if (unsigned_p)
    ops[2] = force_reg (imode, CONST0_RTX (imode));
  else
    ops[2] = ix86_expand_sse_cmp (gen_reg_rtx (imode), GT, CONST0_RTX (imode),
				  ops[1], pc_rtx, pc_rtx);

  ix86_split_mmx_punpck (ops, false);
}

/* Unpack SRC into the next wider integer vector type.  UNSIGNED_P is
   true if we should do zero extension, else sign extension.  HIGH_P is
   true if we want the N/2 high elements, else the low elements.  */

void
ix86_expand_sse_unpack (rtx dest, rtx src, bool unsigned_p, bool high_p)
{
  machine_mode imode = GET_MODE (src);
  rtx tmp;

  if (TARGET_SSE4_1)
    {
      rtx (*unpack)(rtx, rtx);
      rtx (*extract)(rtx, rtx) = NULL;
      machine_mode halfmode = BLKmode;

      switch (imode)
	{
	case E_V64QImode:
	  if (unsigned_p)
	    unpack = gen_avx512bw_zero_extendv32qiv32hi2;
	  else
	    unpack = gen_avx512bw_sign_extendv32qiv32hi2;
	  halfmode = V32QImode;
	  extract
	    = high_p ? gen_vec_extract_hi_v64qi : gen_vec_extract_lo_v64qi;
	  break;
	case E_V32QImode:
	  if (unsigned_p)
	    unpack = gen_avx2_zero_extendv16qiv16hi2;
	  else
	    unpack = gen_avx2_sign_extendv16qiv16hi2;
	  halfmode = V16QImode;
	  extract
	    = high_p ? gen_vec_extract_hi_v32qi : gen_vec_extract_lo_v32qi;
	  break;
	case E_V32HImode:
	  if (unsigned_p)
	    unpack = gen_avx512f_zero_extendv16hiv16si2;
	  else
	    unpack = gen_avx512f_sign_extendv16hiv16si2;
	  halfmode = V16HImode;
	  extract
	    = high_p ? gen_vec_extract_hi_v32hi : gen_vec_extract_lo_v32hi;
	  break;
	case E_V16HImode:
	  if (unsigned_p)
	    unpack = gen_avx2_zero_extendv8hiv8si2;
	  else
	    unpack = gen_avx2_sign_extendv8hiv8si2;
	  halfmode = V8HImode;
	  extract
	    = high_p ? gen_vec_extract_hi_v16hi : gen_vec_extract_lo_v16hi;
	  break;
	case E_V16SImode:
	  if (unsigned_p)
	    unpack = gen_avx512f_zero_extendv8siv8di2;
	  else
	    unpack = gen_avx512f_sign_extendv8siv8di2;
	  halfmode = V8SImode;
	  extract
	    = high_p ? gen_vec_extract_hi_v16si : gen_vec_extract_lo_v16si;
	  break;
	case E_V8SImode:
	  if (unsigned_p)
	    unpack = gen_avx2_zero_extendv4siv4di2;
	  else
	    unpack = gen_avx2_sign_extendv4siv4di2;
	  halfmode = V4SImode;
	  extract
	    = high_p ? gen_vec_extract_hi_v8si : gen_vec_extract_lo_v8si;
	  break;
	case E_V16QImode:
	  if (unsigned_p)
	    unpack = gen_sse4_1_zero_extendv8qiv8hi2;
	  else
	    unpack = gen_sse4_1_sign_extendv8qiv8hi2;
	  break;
	case E_V8HImode:
	  if (unsigned_p)
	    unpack = gen_sse4_1_zero_extendv4hiv4si2;
	  else
	    unpack = gen_sse4_1_sign_extendv4hiv4si2;
	  break;
	case E_V4SImode:
	  if (unsigned_p)
	    unpack = gen_sse4_1_zero_extendv2siv2di2;
	  else
	    unpack = gen_sse4_1_sign_extendv2siv2di2;
	  break;
	case E_V8QImode:
	  if (unsigned_p)
	    unpack = gen_sse4_1_zero_extendv4qiv4hi2;
	  else
	    unpack = gen_sse4_1_sign_extendv4qiv4hi2;
	  break;
	case E_V4HImode:
	  if (unsigned_p)
	    unpack = gen_sse4_1_zero_extendv2hiv2si2;
	  else
	    unpack = gen_sse4_1_sign_extendv2hiv2si2;
	  break;
	case E_V4QImode:
	  if (unsigned_p)
	    unpack = gen_sse4_1_zero_extendv2qiv2hi2;
	  else
	    unpack = gen_sse4_1_sign_extendv2qiv2hi2;
	  break;
	default:
	  gcc_unreachable ();
	}

      if (GET_MODE_SIZE (imode) >= 32)
	{
	  tmp = gen_reg_rtx (halfmode);
	  emit_insn (extract (tmp, src));
	}
      else if (high_p)
	{
	  switch (GET_MODE_SIZE (imode))
	    {
	    case 16:
	      /* Shift higher 8 bytes to lower 8 bytes.  */
	      tmp = gen_reg_rtx (V1TImode);
	      emit_insn (gen_sse2_lshrv1ti3 (tmp, gen_lowpart (V1TImode, src),
					     GEN_INT (64)));
	      break;
	    case 8:
	      /* Shift higher 4 bytes to lower 4 bytes.  */
	      tmp = gen_reg_rtx (V1DImode);
	      emit_insn (gen_mmx_lshrv1di3 (tmp, gen_lowpart (V1DImode, src),
					    GEN_INT (32)));
	      break;
	    case 4:
	      /* Shift higher 2 bytes to lower 2 bytes.  */
	      tmp = gen_reg_rtx (V1SImode);
	      emit_insn (gen_mmx_lshrv1si3 (tmp, gen_lowpart (V1SImode, src),
					    GEN_INT (16)));
	      break;
	    default:
	      gcc_unreachable ();
	    }

	  tmp = gen_lowpart (imode, tmp);
	}
      else
	tmp = src;

      emit_insn (unpack (dest, tmp));
    }
  else
    {
      rtx (*unpack)(rtx, rtx, rtx);

      switch (imode)
	{
	case E_V16QImode:
	  if (high_p)
	    unpack = gen_vec_interleave_highv16qi;
	  else
	    unpack = gen_vec_interleave_lowv16qi;
	  break;
	case E_V8HImode:
	  if (high_p)
	    unpack = gen_vec_interleave_highv8hi;
	  else
	    unpack = gen_vec_interleave_lowv8hi;
	  break;
	case E_V4SImode:
	  if (high_p)
	    unpack = gen_vec_interleave_highv4si;
	  else
	    unpack = gen_vec_interleave_lowv4si;
	  break;
	case E_V8QImode:
	  if (high_p)
	    unpack = gen_mmx_punpckhbw;
	  else
	    unpack = gen_mmx_punpcklbw;
	  break;
	case E_V4HImode:
	  if (high_p)
	    unpack = gen_mmx_punpckhwd;
	  else
	    unpack = gen_mmx_punpcklwd;
	  break;
	case E_V4QImode:
	  if (high_p)
	    unpack = gen_mmx_punpckhbw_low;
	  else
	    unpack = gen_mmx_punpcklbw_low;
	  break;
	default:
	  gcc_unreachable ();
	}

      if (unsigned_p)
	tmp = force_reg (imode, CONST0_RTX (imode));
      else
	tmp = ix86_expand_sse_cmp (gen_reg_rtx (imode), GT, CONST0_RTX (imode),
				   src, pc_rtx, pc_rtx);

      rtx tmp2 = gen_reg_rtx (imode);
      emit_insn (unpack (tmp2, src, tmp));
      emit_move_insn (dest, gen_lowpart (GET_MODE (dest), tmp2));
    }
}

/* Return true if mem is pool constant which contains a const_vector
   perm index, assign the index to PERM.  */
bool
ix86_extract_perm_from_pool_constant (int* perm, rtx mem)
{
  machine_mode mode = GET_MODE (mem);
  int nelt = GET_MODE_NUNITS (mode);

  if (!INTEGRAL_MODE_P (mode))
    return false;

    /* Needs to be constant pool.  */
  if (!(MEM_P (mem))
      || !SYMBOL_REF_P (XEXP (mem, 0))
      || !CONSTANT_POOL_ADDRESS_P (XEXP (mem, 0)))
   return false;

  rtx constant = get_pool_constant (XEXP (mem, 0));

  if (GET_CODE (constant) != CONST_VECTOR)
    return false;

  /* There could be some rtx like
     (mem/u/c:V16QI (symbol_ref/u:DI ("*.LC1")))
     but with "*.LC1" refer to V2DI constant vector.  */
  if (GET_MODE (constant) != mode)
    {
      constant = simplify_subreg (mode, constant, GET_MODE (constant), 0);

      if (constant == nullptr || GET_CODE (constant) != CONST_VECTOR)
	return false;
    }

  for (int i = 0; i != nelt; i++)
    perm[i] = UINTVAL (XVECEXP (constant, 0, i));

  return true;
}

/* Split operands 0 and 1 into half-mode parts.  Similar to split_double_mode,
   but works for floating pointer parameters and nonoffsetable memories.
   For pushes, it returns just stack offsets; the values will be saved
   in the right order.  Maximally three parts are generated.  */

static int
ix86_split_to_parts (rtx operand, rtx *parts, machine_mode mode)
{
  int size;

  if (!TARGET_64BIT)
    size = mode==XFmode ? 3 : GET_MODE_SIZE (mode) / 4;
  else
    size = (GET_MODE_SIZE (mode) + 4) / 8;

  gcc_assert (!REG_P (operand) || !MMX_REGNO_P (REGNO (operand)));
  gcc_assert (size >= 2 && size <= 4);

  /* Optimize constant pool reference to immediates.  This is used by fp
     moves, that force all constants to memory to allow combining.  */
  if (MEM_P (operand) && MEM_READONLY_P (operand))
    operand = avoid_constant_pool_reference (operand);

  if (MEM_P (operand) && !offsettable_memref_p (operand))
    {
      /* The only non-offsetable memories we handle are pushes.  */
      int ok = push_operand (operand, VOIDmode);

      gcc_assert (ok);

      operand = copy_rtx (operand);
      PUT_MODE (operand, word_mode);
      parts[0] = parts[1] = parts[2] = parts[3] = operand;
      return size;
    }

  if (GET_CODE (operand) == CONST_VECTOR)
    {
      scalar_int_mode imode = int_mode_for_mode (mode).require ();
      /* Caution: if we looked through a constant pool memory above,
	 the operand may actually have a different mode now.  That's
	 ok, since we want to pun this all the way back to an integer.  */
      operand = simplify_subreg (imode, operand, GET_MODE (operand), 0);
      gcc_assert (operand != NULL);
      mode = imode;
    }

  if (!TARGET_64BIT)
    {
      if (mode == DImode)
	split_double_mode (mode, &operand, 1, &parts[0], &parts[1]);
      else
	{
	  int i;

	  if (REG_P (operand))
	    {
	      gcc_assert (reload_completed);
	      for (i = 0; i < size; i++)
		parts[i] = gen_rtx_REG (SImode, REGNO (operand) + i);
	    }
	  else if (offsettable_memref_p (operand))
	    {
	      operand = adjust_address (operand, SImode, 0);
	      parts[0] = operand;
	      for (i = 1; i < size; i++)
		parts[i] = adjust_address (operand, SImode, 4 * i);
	    }
	  else if (CONST_DOUBLE_P (operand))
	    {
	      const REAL_VALUE_TYPE *r;
	      long l[4];

	      r = CONST_DOUBLE_REAL_VALUE (operand);
	      switch (mode)
		{
		case E_TFmode:
		  real_to_target (l, r, mode);
		  parts[3] = gen_int_mode (l[3], SImode);
		  parts[2] = gen_int_mode (l[2], SImode);
		  break;
		case E_XFmode:
		  /* We can't use REAL_VALUE_TO_TARGET_LONG_DOUBLE since
		     long double may not be 80-bit.  */
		  real_to_target (l, r, mode);
		  parts[2] = gen_int_mode (l[2], SImode);
		  break;
		case E_DFmode:
		  REAL_VALUE_TO_TARGET_DOUBLE (*r, l);
		  break;
		default:
		  gcc_unreachable ();
		}
	      parts[1] = gen_int_mode (l[1], SImode);
	      parts[0] = gen_int_mode (l[0], SImode);
	    }
	  else
	    gcc_unreachable ();
	}
    }
  else
    {
      if (mode == TImode)
	split_double_mode (mode, &operand, 1, &parts[0], &parts[1]);
      if (mode == XFmode || mode == TFmode)
	{
	  machine_mode upper_mode = mode==XFmode ? SImode : DImode;
	  if (REG_P (operand))
	    {
	      gcc_assert (reload_completed);
	      parts[0] = gen_rtx_REG (DImode, REGNO (operand) + 0);
	      parts[1] = gen_rtx_REG (upper_mode, REGNO (operand) + 1);
	    }
	  else if (offsettable_memref_p (operand))
	    {
	      operand = adjust_address (operand, DImode, 0);
	      parts[0] = operand;
	      parts[1] = adjust_address (operand, upper_mode, 8);
	    }
	  else if (CONST_DOUBLE_P (operand))
	    {
	      long l[4];

	      real_to_target (l, CONST_DOUBLE_REAL_VALUE (operand), mode);

	      /* real_to_target puts 32-bit pieces in each long.  */
	      parts[0] = gen_int_mode ((l[0] & HOST_WIDE_INT_C (0xffffffff))
				       | ((l[1] & HOST_WIDE_INT_C (0xffffffff))
					  << 32), DImode);

	      if (upper_mode == SImode)
	        parts[1] = gen_int_mode (l[2], SImode);
	      else
	        parts[1]
		  = gen_int_mode ((l[2] & HOST_WIDE_INT_C (0xffffffff))
				  | ((l[3] & HOST_WIDE_INT_C (0xffffffff))
				     << 32), DImode);
	    }
	  else
	    gcc_unreachable ();
	}
    }

  return size;
}

/* Emit insns to perform a move or push of DI, DF, XF, and TF values.
   Return false when normal moves are needed; true when all required
   insns have been emitted.  Operands 2-4 contain the input values
   int the correct order; operands 5-7 contain the output values.  */

void
ix86_split_long_move (rtx operands[])
{
  rtx part[2][4];
  int nparts, i, j;
  int push = 0;
  int collisions = 0;
  machine_mode mode = GET_MODE (operands[0]);
  bool collisionparts[4];

  /* The DFmode expanders may ask us to move double.
     For 64bit target this is single move.  By hiding the fact
     here we simplify i386.md splitters.  */
  if (TARGET_64BIT && GET_MODE_SIZE (GET_MODE (operands[0])) == 8)
    {
      /* Optimize constant pool reference to immediates.  This is used by
	 fp moves, that force all constants to memory to allow combining.  */

      if (MEM_P (operands[1])
	  && GET_CODE (XEXP (operands[1], 0)) == SYMBOL_REF
	  && CONSTANT_POOL_ADDRESS_P (XEXP (operands[1], 0)))
	operands[1] = get_pool_constant (XEXP (operands[1], 0));
      if (push_operand (operands[0], VOIDmode))
	{
	  operands[0] = copy_rtx (operands[0]);
	  PUT_MODE (operands[0], word_mode);
	}
      else
        operands[0] = gen_lowpart (DImode, operands[0]);
      operands[1] = gen_lowpart (DImode, operands[1]);
      emit_move_insn (operands[0], operands[1]);
      return;
    }

  /* The only non-offsettable memory we handle is push.  */
  if (push_operand (operands[0], VOIDmode))
    push = 1;
  else
    gcc_assert (!MEM_P (operands[0])
		|| offsettable_memref_p (operands[0]));

  nparts = ix86_split_to_parts (operands[1], part[1], GET_MODE (operands[0]));
  ix86_split_to_parts (operands[0], part[0], GET_MODE (operands[0]));

  /* When emitting push, take care for source operands on the stack.  */
  if (push && MEM_P (operands[1])
      && reg_overlap_mentioned_p (stack_pointer_rtx, operands[1]))
    {
      rtx src_base = XEXP (part[1][nparts - 1], 0);

      /* Compensate for the stack decrement by 4.  */
      if (!TARGET_64BIT && nparts == 3
	  && mode == XFmode && TARGET_128BIT_LONG_DOUBLE)
	src_base = plus_constant (Pmode, src_base, 4);

      /* src_base refers to the stack pointer and is
	 automatically decreased by emitted push.  */
      for (i = 0; i < nparts; i++)
	part[1][i] = change_address (part[1][i],
				     GET_MODE (part[1][i]), src_base);
    }

  /* We need to do copy in the right order in case an address register
     of the source overlaps the destination.  */
  if (REG_P (part[0][0]) && MEM_P (part[1][0]))
    {
      rtx tmp;

      for (i = 0; i < nparts; i++)
	{
	  collisionparts[i]
	    = reg_overlap_mentioned_p (part[0][i], XEXP (part[1][0], 0));
	  if (collisionparts[i])
	    collisions++;
	}

      /* Collision in the middle part can be handled by reordering.  */
      if (collisions == 1 && nparts == 3 && collisionparts [1])
	{
	  std::swap (part[0][1], part[0][2]);
	  std::swap (part[1][1], part[1][2]);
	}
      else if (collisions == 1
	       && nparts == 4
	       && (collisionparts [1] || collisionparts [2]))
	{
	  if (collisionparts [1])
	    {
	      std::swap (part[0][1], part[0][2]);
	      std::swap (part[1][1], part[1][2]);
	    }
	  else
	    {
	      std::swap (part[0][2], part[0][3]);
	      std::swap (part[1][2], part[1][3]);
	    }
	}

      /* If there are more collisions, we can't handle it by reordering.
	 Do an lea to the last part and use only one colliding move.  */
      else if (collisions > 1)
	{
	  rtx base, addr;

	  collisions = 1;

	  base = part[0][nparts - 1];

	  /* Handle the case when the last part isn't valid for lea.
	     Happens in 64-bit mode storing the 12-byte XFmode.  */
	  if (GET_MODE (base) != Pmode)
	    base = gen_rtx_REG (Pmode, REGNO (base));

	  addr = XEXP (part[1][0], 0);
	  if (TARGET_TLS_DIRECT_SEG_REFS)
	    {
	      struct ix86_address parts;
	      int ok = ix86_decompose_address (addr, &parts);
	      gcc_assert (ok);
	      /* It is not valid to use %gs: or %fs: in lea.  */
	      gcc_assert (parts.seg == ADDR_SPACE_GENERIC);
	    }
	  emit_insn (gen_rtx_SET (base, addr));
	  part[1][0] = replace_equiv_address (part[1][0], base);
	  for (i = 1; i < nparts; i++)
	    {
	      tmp = plus_constant (Pmode, base, UNITS_PER_WORD * i);
	      part[1][i] = replace_equiv_address (part[1][i], tmp);
	    }
	}
    }

  if (push)
    {
      if (!TARGET_64BIT)
	{
	  if (nparts == 3)
	    {
	      if (TARGET_128BIT_LONG_DOUBLE && mode == XFmode)
                emit_insn (gen_add2_insn (stack_pointer_rtx, GEN_INT (-4)));
	      emit_move_insn (part[0][2], part[1][2]);
	    }
	  else if (nparts == 4)
	    {
	      emit_move_insn (part[0][3], part[1][3]);
	      emit_move_insn (part[0][2], part[1][2]);
	    }
	}
      else
	{
	  /* In 64bit mode we don't have 32bit push available.  In case this is
	     register, it is OK - we will just use larger counterpart.  We also
	     retype memory - these comes from attempt to avoid REX prefix on
	     moving of second half of TFmode value.  */
	  if (GET_MODE (part[1][1]) == SImode)
	    {
	      switch (GET_CODE (part[1][1]))
		{
		case MEM:
		  part[1][1] = adjust_address (part[1][1], DImode, 0);
		  break;

		case REG:
		  part[1][1] = gen_rtx_REG (DImode, REGNO (part[1][1]));
		  break;

		default:
		  gcc_unreachable ();
		}

	      if (GET_MODE (part[1][0]) == SImode)
		part[1][0] = part[1][1];
	    }
	}
      emit_move_insn (part[0][1], part[1][1]);
      emit_move_insn (part[0][0], part[1][0]);
      return;
    }

  /* Choose correct order to not overwrite the source before it is copied.  */
  if ((REG_P (part[0][0])
       && REG_P (part[1][1])
       && (REGNO (part[0][0]) == REGNO (part[1][1])
	   || (nparts == 3
	       && REGNO (part[0][0]) == REGNO (part[1][2]))
	   || (nparts == 4
	       && REGNO (part[0][0]) == REGNO (part[1][3]))))
      || (collisions > 0
	  && reg_overlap_mentioned_p (part[0][0], XEXP (part[1][0], 0))))
    {
      for (i = 0, j = nparts - 1; i < nparts; i++, j--)
	{
	  operands[2 + i] = part[0][j];
	  operands[6 + i] = part[1][j];
	}
    }
  else
    {
      for (i = 0; i < nparts; i++)
	{
	  operands[2 + i] = part[0][i];
	  operands[6 + i] = part[1][i];
	}
    }

  /* Attempt to locally unCSE nonzero constants.  */
  for (j = 0; j < nparts - 1; j++)
    if (CONST_INT_P (operands[6 + j])
	&& operands[6 + j] != const0_rtx
	&& REG_P (operands[2 + j]))
      for (i = j; i < nparts - 1; i++)
	if (CONST_INT_P (operands[7 + i])
	    && INTVAL (operands[7 + i]) == INTVAL (operands[6 + j]))
	  operands[7 + i] = operands[2 + j];

  for (i = 0; i < nparts; i++)
    emit_move_insn (operands[2 + i], operands[6 + i]);

  return;
}

/* Helper function of ix86_split_ashl used to generate an SImode/DImode
   left shift by a constant, either using a single shift or
   a sequence of add instructions.  */

static void
ix86_expand_ashl_const (rtx operand, int count, machine_mode mode)
{
  if (count == 1
      || (count * ix86_cost->add <= ix86_cost->shift_const
	  && !optimize_insn_for_size_p ()))
    {
      while (count-- > 0)
	emit_insn (gen_add2_insn (operand, operand));
    }
  else
    {
      rtx (*insn)(rtx, rtx, rtx);

      insn = mode == DImode ? gen_ashlsi3 : gen_ashldi3;
      emit_insn (insn (operand, operand, GEN_INT (count)));
    }
}

void
ix86_split_ashl (rtx *operands, rtx scratch, machine_mode mode)
{
  rtx (*gen_ashl3)(rtx, rtx, rtx);
  rtx (*gen_shld)(rtx, rtx, rtx);
  int half_width = GET_MODE_BITSIZE (mode) >> 1;
  machine_mode half_mode;

  rtx low[2], high[2];
  int count;

  if (CONST_INT_P (operands[2]))
    {
      split_double_mode (mode, operands, 2, low, high);
      count = INTVAL (operands[2]) & (GET_MODE_BITSIZE (mode) - 1);

      if (count >= half_width)
	{
	  emit_move_insn (high[0], low[1]);
	  ix86_expand_clear (low[0]);

	  if (count > half_width)
	    ix86_expand_ashl_const (high[0], count - half_width, mode);
	}
      else if (count == 1)
	{
	  if (!rtx_equal_p (operands[0], operands[1]))
	    emit_move_insn (operands[0], operands[1]);
	  rtx x3 = gen_rtx_REG (CCCmode, FLAGS_REG);
	  rtx x4 = gen_rtx_LTU (mode, x3, const0_rtx);
	  half_mode = mode == DImode ? SImode : DImode;
	  emit_insn (gen_add3_cc_overflow_1 (half_mode, low[0],
					     low[0], low[0]));
	  emit_insn (gen_add3_carry (half_mode, high[0], high[0], high[0],
				     x3, x4));
	}
      else
	{
	  gen_shld = mode == DImode ? gen_x86_shld : gen_x86_64_shld;

	  if (!rtx_equal_p (operands[0], operands[1]))
	    emit_move_insn (operands[0], operands[1]);

	  emit_insn (gen_shld (high[0], low[0], GEN_INT (count)));
	  ix86_expand_ashl_const (low[0], count, mode);
	}
      return;
    }

  split_double_mode (mode, operands, 1, low, high);
  half_mode = mode == DImode ? SImode : DImode;

  gen_ashl3 = mode == DImode ? gen_ashlsi3 : gen_ashldi3;

  if (operands[1] == const1_rtx)
    {
      /* Assuming we've chosen a QImode capable registers, then 1 << N
	 can be done with two 32/64-bit shifts, no branches, no cmoves.  */
      if (ANY_QI_REG_P (low[0]) && ANY_QI_REG_P (high[0]))
	{
	  rtx s, d, flags = gen_rtx_REG (CCZmode, FLAGS_REG);

	  ix86_expand_clear (low[0]);
	  ix86_expand_clear (high[0]);
	  emit_insn (gen_testqi_ccz_1 (operands[2], GEN_INT (half_width)));

	  d = gen_lowpart (QImode, low[0]);
	  d = gen_rtx_STRICT_LOW_PART (VOIDmode, d);
	  s = gen_rtx_EQ (QImode, flags, const0_rtx);
	  emit_insn (gen_rtx_SET (d, s));

	  d = gen_lowpart (QImode, high[0]);
	  d = gen_rtx_STRICT_LOW_PART (VOIDmode, d);
	  s = gen_rtx_NE (QImode, flags, const0_rtx);
	  emit_insn (gen_rtx_SET (d, s));
	}

      /* Otherwise, we can get the same results by manually performing
	 a bit extract operation on bit 5/6, and then performing the two
	 shifts.  The two methods of getting 0/1 into low/high are exactly
	 the same size.  Avoiding the shift in the bit extract case helps
	 pentium4 a bit; no one else seems to care much either way.  */
      else
	{
	  rtx (*gen_lshr3)(rtx, rtx, rtx);
	  rtx (*gen_and3)(rtx, rtx, rtx);
	  rtx (*gen_xor3)(rtx, rtx, rtx);
	  HOST_WIDE_INT bits;
	  rtx x;

	  if (mode == DImode)
	    {
	      gen_lshr3 = gen_lshrsi3;
	      gen_and3 = gen_andsi3;
	      gen_xor3 = gen_xorsi3;
	      bits = 5;
	    }
	  else
	    {
	      gen_lshr3 = gen_lshrdi3;
	      gen_and3 = gen_anddi3;
	      gen_xor3 = gen_xordi3;
	      bits = 6;
	    }

	  if (TARGET_PARTIAL_REG_STALL && !optimize_insn_for_size_p ())
	    x = gen_rtx_ZERO_EXTEND (half_mode, operands[2]);
	  else
	    x = gen_lowpart (half_mode, operands[2]);
	  emit_insn (gen_rtx_SET (high[0], x));

	  emit_insn (gen_lshr3 (high[0], high[0], GEN_INT (bits)));
	  emit_insn (gen_and3 (high[0], high[0], const1_rtx));
	  emit_move_insn (low[0], high[0]);
	  emit_insn (gen_xor3 (low[0], low[0], const1_rtx));
	}

      emit_insn (gen_ashl3 (low[0], low[0], operands[2]));
      emit_insn (gen_ashl3 (high[0], high[0], operands[2]));
      return;
    }

  if (operands[1] == constm1_rtx)
    {
      /* For -1 << N, we can avoid the shld instruction, because we
	 know that we're shifting 0...31/63 ones into a -1.  */
      emit_move_insn (low[0], constm1_rtx);
      if (optimize_insn_for_size_p ())
	emit_move_insn (high[0], low[0]);
      else
	emit_move_insn (high[0], constm1_rtx);
    }
  else
    {
      gen_shld = mode == DImode ? gen_x86_shld : gen_x86_64_shld;

      if (!rtx_equal_p (operands[0], operands[1]))
	emit_move_insn (operands[0], operands[1]);

      split_double_mode (mode, operands, 1, low, high);
      emit_insn (gen_shld (high[0], low[0], operands[2]));
    }

  emit_insn (gen_ashl3 (low[0], low[0], operands[2]));

  if (TARGET_CMOVE && scratch)
    {
      ix86_expand_clear (scratch);
      emit_insn (gen_x86_shift_adj_1
		 (half_mode, high[0], low[0], operands[2], scratch));
    }
  else
    emit_insn (gen_x86_shift_adj_2 (half_mode, high[0], low[0], operands[2]));
}

void
ix86_split_ashr (rtx *operands, rtx scratch, machine_mode mode)
{
  rtx (*gen_ashr3)(rtx, rtx, rtx)
    = mode == DImode ? gen_ashrsi3 : gen_ashrdi3;
  rtx (*gen_shrd)(rtx, rtx, rtx);
  int half_width = GET_MODE_BITSIZE (mode) >> 1;

  rtx low[2], high[2];
  int count;

  if (CONST_INT_P (operands[2]))
    {
      split_double_mode (mode, operands, 2, low, high);
      count = INTVAL (operands[2]) & (GET_MODE_BITSIZE (mode) - 1);

      if (count == GET_MODE_BITSIZE (mode) - 1)
	{
	  emit_move_insn (high[0], high[1]);
	  emit_insn (gen_ashr3 (high[0], high[0],
				GEN_INT (half_width - 1)));
	  emit_move_insn (low[0], high[0]);

	}
      else if (count >= half_width)
	{
	  emit_move_insn (low[0], high[1]);
	  emit_move_insn (high[0], low[0]);
	  emit_insn (gen_ashr3 (high[0], high[0],
				GEN_INT (half_width - 1)));

	  if (count > half_width)
	    emit_insn (gen_ashr3 (low[0], low[0],
				  GEN_INT (count - half_width)));
	}
      else if (count == 1
	       && (TARGET_USE_RCR || optimize_size > 1))
	{
	  if (!rtx_equal_p (operands[0], operands[1]))
	    emit_move_insn (operands[0], operands[1]);
	  if (mode == DImode)
	    {
	      emit_insn (gen_ashrsi3_carry (high[0], high[0]));
	      emit_insn (gen_rcrsi2 (low[0], low[0]));
	    }
	  else
	    {
	      emit_insn (gen_ashrdi3_carry (high[0], high[0]));
	      emit_insn (gen_rcrdi2 (low[0], low[0]));
	    }
	}
      else
	{
	  gen_shrd = mode == DImode ? gen_x86_shrd : gen_x86_64_shrd;

	  if (!rtx_equal_p (operands[0], operands[1]))
	    emit_move_insn (operands[0], operands[1]);

	  emit_insn (gen_shrd (low[0], high[0], GEN_INT (count)));
	  emit_insn (gen_ashr3 (high[0], high[0], GEN_INT (count)));
	}
    }
  else
    {
      machine_mode half_mode;

      gen_shrd = mode == DImode ? gen_x86_shrd : gen_x86_64_shrd;

     if (!rtx_equal_p (operands[0], operands[1]))
	emit_move_insn (operands[0], operands[1]);

      split_double_mode (mode, operands, 1, low, high);
      half_mode = mode == DImode ? SImode : DImode;

      emit_insn (gen_shrd (low[0], high[0], operands[2]));
      emit_insn (gen_ashr3 (high[0], high[0], operands[2]));

      if (TARGET_CMOVE && scratch)
	{
	  emit_move_insn (scratch, high[0]);
	  emit_insn (gen_ashr3 (scratch, scratch,
				GEN_INT (half_width - 1)));
	  emit_insn (gen_x86_shift_adj_1
		     (half_mode, low[0], high[0], operands[2], scratch));
	}
      else
	emit_insn (gen_x86_shift_adj_3
		   (half_mode, low[0], high[0], operands[2]));
    }
}

void
ix86_split_lshr (rtx *operands, rtx scratch, machine_mode mode)
{
  rtx (*gen_lshr3)(rtx, rtx, rtx)
    = mode == DImode ? gen_lshrsi3 : gen_lshrdi3;
  rtx (*gen_shrd)(rtx, rtx, rtx);
  int half_width = GET_MODE_BITSIZE (mode) >> 1;

  rtx low[2], high[2];
  int count;

  if (CONST_INT_P (operands[2]))
    {
      split_double_mode (mode, operands, 2, low, high);
      count = INTVAL (operands[2]) & (GET_MODE_BITSIZE (mode) - 1);

      if (count >= half_width)
	{
	  emit_move_insn (low[0], high[1]);
	  ix86_expand_clear (high[0]);

	  if (count > half_width)
	    emit_insn (gen_lshr3 (low[0], low[0],
				  GEN_INT (count - half_width)));
	}
      else if (count == 1
	       && (TARGET_USE_RCR || optimize_size > 1))
	{
	  if (!rtx_equal_p (operands[0], operands[1]))
	    emit_move_insn (operands[0], operands[1]);
	  if (mode == DImode)
	    {
	      emit_insn (gen_lshrsi3_carry (high[0], high[0]));
	      emit_insn (gen_rcrsi2 (low[0], low[0]));
	    }
	  else
	    {
	      emit_insn (gen_lshrdi3_carry (high[0], high[0]));
	      emit_insn (gen_rcrdi2 (low[0], low[0]));
	    }
	}
      else
	{
	  gen_shrd = mode == DImode ? gen_x86_shrd : gen_x86_64_shrd;

	  if (!rtx_equal_p (operands[0], operands[1]))
	    emit_move_insn (operands[0], operands[1]);

	  emit_insn (gen_shrd (low[0], high[0], GEN_INT (count)));
	  emit_insn (gen_lshr3 (high[0], high[0], GEN_INT (count)));
	}
    }
  else
    {
      machine_mode half_mode;

      gen_shrd = mode == DImode ? gen_x86_shrd : gen_x86_64_shrd;

      if (!rtx_equal_p (operands[0], operands[1]))
	emit_move_insn (operands[0], operands[1]);

      split_double_mode (mode, operands, 1, low, high);
      half_mode = mode == DImode ? SImode : DImode;

      emit_insn (gen_shrd (low[0], high[0], operands[2]));
      emit_insn (gen_lshr3 (high[0], high[0], operands[2]));

      if (TARGET_CMOVE && scratch)
	{
	  ix86_expand_clear (scratch);
	  emit_insn (gen_x86_shift_adj_1
		     (half_mode, low[0], high[0], operands[2], scratch));
	}
      else
	emit_insn (gen_x86_shift_adj_2
		   (half_mode, low[0], high[0], operands[2]));
    }
}

/* Helper function to split TImode ashl under NDD.  */
void
ix86_split_ashl_ndd (rtx *operands, rtx scratch)
{
  gcc_assert (TARGET_APX_NDD);
  int half_width = GET_MODE_BITSIZE (TImode) >> 1;

  rtx low[2], high[2];
  int count;

  split_double_mode (TImode, operands, 2, low, high);
  if (CONST_INT_P (operands[2]))
    {
      count = INTVAL (operands[2]) & (GET_MODE_BITSIZE (TImode) - 1);

      if (count >= half_width)
	{
	  count = count - half_width;
	  if (count == 0)
	    {
	      if (!rtx_equal_p (high[0], low[1]))
		emit_move_insn (high[0], low[1]);
	    }
	  else if (count == 1)
	    emit_insn (gen_adddi3 (high[0], low[1], low[1]));
	  else
	    emit_insn (gen_ashldi3 (high[0], low[1], GEN_INT (count)));

	  ix86_expand_clear (low[0]);
	}
      else if (count == 1)
	{
	  rtx x3 = gen_rtx_REG (CCCmode, FLAGS_REG);
	  rtx x4 = gen_rtx_LTU (TImode, x3, const0_rtx);
	  emit_insn (gen_add3_cc_overflow_1 (DImode, low[0],
					     low[1], low[1]));
	  emit_insn (gen_add3_carry (DImode, high[0], high[1], high[1],
				     x3, x4));
	}
      else
	{
	  emit_insn (gen_x86_64_shld_ndd (high[0], high[1], low[1],
					  GEN_INT (count)));
	  emit_insn (gen_ashldi3 (low[0], low[1], GEN_INT (count)));
	}
    }
  else
    {
      emit_insn (gen_x86_64_shld_ndd (high[0], high[1], low[1],
				      operands[2]));
      emit_insn (gen_ashldi3 (low[0], low[1], operands[2]));
      if (TARGET_CMOVE && scratch)
	{
	  ix86_expand_clear (scratch);
	  emit_insn (gen_x86_shift_adj_1
		     (DImode, high[0], low[0], operands[2], scratch));
	}
      else
	emit_insn (gen_x86_shift_adj_2 (DImode, high[0], low[0], operands[2]));
    }
}

/* Helper function to split TImode l/ashr under NDD.  */
void
ix86_split_rshift_ndd (enum rtx_code code, rtx *operands, rtx scratch)
{
  gcc_assert (TARGET_APX_NDD);
  int half_width = GET_MODE_BITSIZE (TImode) >> 1;
  bool ashr_p = code == ASHIFTRT;
  rtx (*gen_shr)(rtx, rtx, rtx) = ashr_p ? gen_ashrdi3
					 : gen_lshrdi3;

  rtx low[2], high[2];
  int count;

  split_double_mode (TImode, operands, 2, low, high);
  if (CONST_INT_P (operands[2]))
    {
      count = INTVAL (operands[2]) & (GET_MODE_BITSIZE (TImode) - 1);

      if (ashr_p && (count == GET_MODE_BITSIZE (TImode) - 1))
	{
	  emit_insn (gen_shr (high[0], high[1],
			      GEN_INT (half_width - 1)));
	  emit_move_insn (low[0], high[0]);
	}
      else if (count >= half_width)
	{
	  if (ashr_p)
	    emit_insn (gen_shr (high[0], high[1],
				GEN_INT (half_width - 1)));
	  else
	    ix86_expand_clear (high[0]);

	  if (count > half_width)
	    emit_insn (gen_shr (low[0], high[1],
				GEN_INT (count - half_width)));
	  else
	    emit_move_insn (low[0], high[1]);
	}
      else
	{
	  emit_insn (gen_x86_64_shrd_ndd (low[0], low[1], high[1],
					  GEN_INT (count)));
	  emit_insn (gen_shr (high[0], high[1], GEN_INT (count)));
	}
    }
  else
    {
      emit_insn (gen_x86_64_shrd_ndd (low[0], low[1], high[1],
				      operands[2]));
      emit_insn (gen_shr (high[0], high[1], operands[2]));

      if (TARGET_CMOVE && scratch)
	{
	  if (ashr_p)
	    {
	      emit_move_insn (scratch, high[0]);
	      emit_insn (gen_shr (scratch, scratch,
				  GEN_INT (half_width - 1)));
	    }
	  else
	    ix86_expand_clear (scratch);

	  emit_insn (gen_x86_shift_adj_1
		     (DImode, low[0], high[0], operands[2], scratch));
	}
      else if (ashr_p)
	emit_insn (gen_x86_shift_adj_3
		   (DImode, low[0], high[0], operands[2]));
      else
	emit_insn (gen_x86_shift_adj_2
		   (DImode, low[0], high[0], operands[2]));
    }
}

/* Expand move of V1TI mode register X to a new TI mode register.  */
static rtx
ix86_expand_v1ti_to_ti (rtx x)
{
  rtx result = gen_reg_rtx (TImode);
  if (TARGET_SSE2)
    {
      rtx temp = force_reg (V2DImode, gen_lowpart (V2DImode, x));
      rtx lo = gen_lowpart (DImode, result);
      emit_insn (gen_vec_extractv2didi (lo, temp, const0_rtx));
      rtx hi = gen_highpart (DImode, result);
      emit_insn (gen_vec_extractv2didi (hi, temp, const1_rtx));
    }
  else
    emit_move_insn (result, gen_lowpart (TImode, x));
  return result;
}

/* Expand move of TI mode register X to a new V1TI mode register.  */
static rtx
ix86_expand_ti_to_v1ti (rtx x)
{
  if (TARGET_SSE2)
    {
      rtx lo = gen_lowpart (DImode, x);
      rtx hi = gen_highpart (DImode, x);
      rtx tmp = gen_reg_rtx (V2DImode);
      emit_insn (gen_vec_concatv2di (tmp, lo, hi));
      return force_reg (V1TImode, gen_lowpart (V1TImode, tmp));
    }

  return force_reg (V1TImode, gen_lowpart (V1TImode, x));
}

/* Expand V1TI mode shift (of rtx_code CODE) by constant.  */
void
ix86_expand_v1ti_shift (enum rtx_code code, rtx operands[])
{
  rtx op1 = force_reg (V1TImode, operands[1]);

  if (!CONST_INT_P (operands[2]))
    {
      rtx tmp1 = ix86_expand_v1ti_to_ti (op1);
      rtx tmp2 = gen_reg_rtx (TImode);
      rtx (*shift) (rtx, rtx, rtx)
	    = (code == ASHIFT) ? gen_ashlti3 : gen_lshrti3;
      emit_insn (shift (tmp2, tmp1, operands[2]));
      rtx tmp3 = ix86_expand_ti_to_v1ti (tmp2);
      emit_move_insn (operands[0], tmp3);
      return;
    }

  HOST_WIDE_INT bits = INTVAL (operands[2]) & 127;

  if (bits == 0)
    {
      emit_move_insn (operands[0], op1);
      return;
    }

  if ((bits & 7) == 0)
    {
      rtx tmp = gen_reg_rtx (V1TImode);
      if (code == ASHIFT)
	emit_insn (gen_sse2_ashlv1ti3 (tmp, op1, GEN_INT (bits)));
      else
	emit_insn (gen_sse2_lshrv1ti3 (tmp, op1, GEN_INT (bits)));
      emit_move_insn (operands[0], tmp);
      return;
    }

  rtx tmp1 = gen_reg_rtx (V1TImode);
  if (code == ASHIFT)
    emit_insn (gen_sse2_ashlv1ti3 (tmp1, op1, GEN_INT (64)));
  else
    emit_insn (gen_sse2_lshrv1ti3 (tmp1, op1, GEN_INT (64)));

  /* tmp2 is operands[1] shifted by 64, in V2DImode.  */
  rtx tmp2 = force_reg (V2DImode, gen_lowpart (V2DImode, tmp1));

  /* tmp3 will be the V2DImode result.  */
  rtx tmp3 = gen_reg_rtx (V2DImode);

  if (bits > 64)
    {
      if (code == ASHIFT)
	emit_insn (gen_ashlv2di3 (tmp3, tmp2, GEN_INT (bits - 64)));
      else
	emit_insn (gen_lshrv2di3 (tmp3, tmp2, GEN_INT (bits - 64)));
    }
  else
    {
      /* tmp4 is operands[1], in V2DImode.  */
      rtx tmp4 = force_reg (V2DImode, gen_lowpart (V2DImode, op1));

      rtx tmp5 = gen_reg_rtx (V2DImode);
      if (code == ASHIFT)
	emit_insn (gen_ashlv2di3 (tmp5, tmp4, GEN_INT (bits)));
      else
	emit_insn (gen_lshrv2di3 (tmp5, tmp4, GEN_INT (bits)));

      rtx tmp6 = gen_reg_rtx (V2DImode);
      if (code == ASHIFT)
	emit_insn (gen_lshrv2di3 (tmp6, tmp2, GEN_INT (64 - bits)));
      else
	emit_insn (gen_ashlv2di3 (tmp6, tmp2, GEN_INT (64 - bits)));

      emit_insn (gen_iorv2di3 (tmp3, tmp5, tmp6));
    }

  /* Convert the result back to V1TImode and store in operands[0].  */
  rtx tmp7 = force_reg (V1TImode, gen_lowpart (V1TImode, tmp3));
  emit_move_insn (operands[0], tmp7);
}

/* Expand V1TI mode rotate (of rtx_code CODE) by constant.  */
void
ix86_expand_v1ti_rotate (enum rtx_code code, rtx operands[])
{
  rtx op1 = force_reg (V1TImode, operands[1]);

  if (!CONST_INT_P (operands[2]))
    {
      rtx tmp1 = ix86_expand_v1ti_to_ti (op1);
      rtx tmp2 = gen_reg_rtx (TImode);
      rtx (*rotate) (rtx, rtx, rtx)
	    = (code == ROTATE) ? gen_rotlti3 : gen_rotrti3;
      emit_insn (rotate (tmp2, tmp1, operands[2]));
      rtx tmp3 = ix86_expand_ti_to_v1ti (tmp2);
      emit_move_insn (operands[0], tmp3);
      return;
    }

  HOST_WIDE_INT bits = INTVAL (operands[2]) & 127;

  if (bits == 0)
    {
      emit_move_insn (operands[0], op1);
      return;
    }

  if (code == ROTATERT)
    bits = 128 - bits;

  if ((bits & 31) == 0)
    {
      rtx tmp2 = gen_reg_rtx (V4SImode);
      rtx tmp1 = force_reg (V4SImode, gen_lowpart (V4SImode, op1));
      if (bits == 32)
	emit_insn (gen_sse2_pshufd (tmp2, tmp1, GEN_INT (0x93)));
      else if (bits == 64)
	emit_insn (gen_sse2_pshufd (tmp2, tmp1, GEN_INT (0x4e)));
      else
	emit_insn (gen_sse2_pshufd (tmp2, tmp1, GEN_INT (0x39)));
      emit_move_insn (operands[0], gen_lowpart (V1TImode, tmp2));
      return;
    }

  if ((bits & 7) == 0)
    {
      rtx tmp1 = gen_reg_rtx (V1TImode);
      rtx tmp2 = gen_reg_rtx (V1TImode);
      rtx tmp3 = gen_reg_rtx (V1TImode);

      emit_insn (gen_sse2_ashlv1ti3 (tmp1, op1, GEN_INT (bits)));
      emit_insn (gen_sse2_lshrv1ti3 (tmp2, op1, GEN_INT (128 - bits)));
      emit_insn (gen_iorv1ti3 (tmp3, tmp1, tmp2));
      emit_move_insn (operands[0], tmp3);
      return;
    }

  rtx op1_v4si = force_reg (V4SImode, gen_lowpart (V4SImode, op1));

  rtx lobits;
  rtx hibits;

  switch (bits >> 5)
    {
    case 0:
      lobits = op1_v4si;
      hibits = gen_reg_rtx (V4SImode);
      emit_insn (gen_sse2_pshufd (hibits, op1_v4si, GEN_INT (0x93)));
      break;

    case 1:
      lobits = gen_reg_rtx (V4SImode);
      hibits = gen_reg_rtx (V4SImode);
      emit_insn (gen_sse2_pshufd (lobits, op1_v4si, GEN_INT (0x93)));
      emit_insn (gen_sse2_pshufd (hibits, op1_v4si, GEN_INT (0x4e)));
      break;

    case 2:
      lobits = gen_reg_rtx (V4SImode);
      hibits = gen_reg_rtx (V4SImode);
      emit_insn (gen_sse2_pshufd (lobits, op1_v4si, GEN_INT (0x4e)));
      emit_insn (gen_sse2_pshufd (hibits, op1_v4si, GEN_INT (0x39)));
      break;

    default:
      lobits = gen_reg_rtx (V4SImode);
      emit_insn (gen_sse2_pshufd (lobits, op1_v4si, GEN_INT (0x39)));
      hibits = op1_v4si;
      break;
    }

  rtx tmp1 = gen_reg_rtx (V4SImode);
  rtx tmp2 = gen_reg_rtx (V4SImode);
  rtx tmp3 = gen_reg_rtx (V4SImode);

  emit_insn (gen_ashlv4si3 (tmp1, lobits, GEN_INT (bits & 31)));
  emit_insn (gen_lshrv4si3 (tmp2, hibits, GEN_INT (32 - (bits & 31))));
  emit_insn (gen_iorv4si3 (tmp3, tmp1, tmp2));

  emit_move_insn (operands[0], gen_lowpart (V1TImode, tmp3));
}

/* Expand V1TI mode ashiftrt by constant.  */
void
ix86_expand_v1ti_ashiftrt (rtx operands[])
{
  rtx op1 = force_reg (V1TImode, operands[1]);

  if (!CONST_INT_P (operands[2]))
    {
      rtx tmp1 = ix86_expand_v1ti_to_ti (op1);
      rtx tmp2 = gen_reg_rtx (TImode);
      emit_insn (gen_ashrti3 (tmp2, tmp1, operands[2]));
      rtx tmp3 = ix86_expand_ti_to_v1ti (tmp2);
      emit_move_insn (operands[0], tmp3);
      return;
    }

  HOST_WIDE_INT bits = INTVAL (operands[2]) & 127;

  if (bits == 0)
    {
      emit_move_insn (operands[0], op1);
      return;
    }

  if (bits == 127)
    {
      /* Two operations.  */
      rtx tmp1 = force_reg(V4SImode, gen_lowpart (V4SImode, op1));
      rtx tmp2 = gen_reg_rtx (V4SImode);
      emit_insn (gen_sse2_pshufd (tmp2, tmp1, GEN_INT (0xff)));

      rtx tmp3 = gen_reg_rtx (V4SImode);
      emit_insn (gen_ashrv4si3 (tmp3, tmp2, GEN_INT (31)));

      emit_move_insn (operands[0], gen_lowpart (V1TImode, tmp3));
      return;
    }

  if (bits == 64)
    {
      /* Three operations.  */
      rtx tmp1 = force_reg(V4SImode, gen_lowpart (V4SImode, op1));
      rtx tmp2 = gen_reg_rtx (V4SImode);
      emit_insn (gen_sse2_pshufd (tmp2, tmp1, GEN_INT (0xff)));

      rtx tmp3 = gen_reg_rtx (V4SImode);
      emit_insn (gen_ashrv4si3 (tmp3, tmp2, GEN_INT (31)));

      rtx tmp4 = force_reg (V2DImode, gen_lowpart (V2DImode, tmp1));
      rtx tmp5 = force_reg (V2DImode, gen_lowpart (V2DImode, tmp3));
      rtx tmp6 = gen_reg_rtx (V2DImode);
      emit_insn (gen_vec_interleave_highv2di (tmp6, tmp4, tmp5));

      emit_move_insn (operands[0], gen_lowpart (V1TImode, tmp6));
      return;
    }

  if (bits == 96)
    {
      /* Three operations.  */
      rtx tmp1 = force_reg(V4SImode, gen_lowpart (V4SImode, op1));
      rtx tmp2 = gen_reg_rtx (V4SImode);
      emit_insn (gen_ashrv4si3 (tmp2, tmp1, GEN_INT (31)));

      rtx tmp3 = force_reg (V2DImode, gen_lowpart (V2DImode, tmp1));
      rtx tmp4 = force_reg (V2DImode, gen_lowpart (V2DImode, tmp2));
      rtx tmp5 = gen_reg_rtx (V2DImode);
      emit_insn (gen_vec_interleave_highv2di (tmp5, tmp3, tmp4));

      rtx tmp6 = force_reg(V4SImode, gen_lowpart (V4SImode, tmp5));
      rtx tmp7 = gen_reg_rtx (V4SImode);
      emit_insn (gen_sse2_pshufd (tmp7, tmp6, GEN_INT (0xfd)));

      emit_move_insn (operands[0], gen_lowpart (V1TImode, tmp7));
      return;
    }

  if (bits >= 111)
    {
      /* Three operations.  */
      rtx tmp1 = force_reg (V4SImode, gen_lowpart (V4SImode, op1));
      rtx tmp2 = gen_reg_rtx (V4SImode);
      emit_insn (gen_ashrv4si3 (tmp2, tmp1, GEN_INT (bits - 96)));

      rtx tmp3 = force_reg (V8HImode, gen_lowpart (V8HImode, tmp2));
      rtx tmp4 = gen_reg_rtx (V8HImode);
      emit_insn (gen_sse2_pshufhw (tmp4, tmp3, GEN_INT (0xfe)));

      rtx tmp5 = force_reg (V4SImode, gen_lowpart (V4SImode, tmp4));
      rtx tmp6 = gen_reg_rtx (V4SImode);
      emit_insn (gen_sse2_pshufd (tmp6, tmp5, GEN_INT (0xfe)));

      emit_move_insn (operands[0], gen_lowpart (V1TImode, tmp6));
      return;
    }

  if (TARGET_AVX2 || TARGET_SSE4_1)
    {
      /* Three operations.  */
      if (bits == 32)
	{
	  rtx tmp1 = force_reg (V4SImode, gen_lowpart (V4SImode, op1));
	  rtx tmp2 = gen_reg_rtx (V4SImode);
	  emit_insn (gen_ashrv4si3 (tmp2, tmp1, GEN_INT (31)));

	  rtx tmp3 = gen_reg_rtx (V1TImode);
	  emit_insn (gen_sse2_lshrv1ti3 (tmp3, op1, GEN_INT (32)));

	  if (TARGET_AVX2)
	    {
	      rtx tmp4 = force_reg (V4SImode, gen_lowpart (V4SImode, tmp3));
	      rtx tmp5 = gen_reg_rtx (V4SImode);
	      emit_insn (gen_avx2_pblenddv4si (tmp5, tmp2, tmp4,
					       GEN_INT (7)));

	      emit_move_insn (operands[0], gen_lowpart (V1TImode, tmp5));
	    }
	  else
	    {
	      rtx tmp4 = force_reg (V8HImode, gen_lowpart (V8HImode, tmp2));
	      rtx tmp5 = force_reg (V8HImode, gen_lowpart (V8HImode, tmp3));
	      rtx tmp6 = gen_reg_rtx (V8HImode);
	      emit_insn (gen_sse4_1_pblendw (tmp6, tmp4, tmp5,
					     GEN_INT (0x3f)));

	      emit_move_insn (operands[0], gen_lowpart (V1TImode, tmp6));
	    }
	  return;
	}

      /* Three operations.  */
      if (bits == 8 || bits == 16 || bits == 24)
	{
	  rtx tmp1 = force_reg (V4SImode, gen_lowpart (V4SImode, op1));
	  rtx tmp2 = gen_reg_rtx (V4SImode);
	  emit_insn (gen_ashrv4si3 (tmp2, tmp1, GEN_INT (bits)));

	  rtx tmp3 = gen_reg_rtx (V1TImode);
	  emit_insn (gen_sse2_lshrv1ti3 (tmp3, op1, GEN_INT (bits)));

	  if (TARGET_AVX2)
	    {
	      rtx tmp4 = force_reg (V4SImode, gen_lowpart (V4SImode, tmp3));
	      rtx tmp5 = gen_reg_rtx (V4SImode);
	      emit_insn (gen_avx2_pblenddv4si (tmp5, tmp2, tmp4,
					       GEN_INT (7)));

	      emit_move_insn (operands[0], gen_lowpart (V1TImode, tmp5));
	    }
	  else
	    {
	      rtx tmp4 = force_reg (V8HImode, gen_lowpart (V8HImode, tmp2));
	      rtx tmp5 = force_reg (V8HImode, gen_lowpart (V8HImode, tmp3));
	      rtx tmp6 = gen_reg_rtx (V8HImode);
	      emit_insn (gen_sse4_1_pblendw (tmp6, tmp4, tmp5,
					     GEN_INT (0x3f)));

	      emit_move_insn (operands[0], gen_lowpart (V1TImode, tmp6));
	    }
	  return;
	}
    }

  if (bits > 96)
    {
      /* Four operations.  */
      rtx tmp1 = force_reg (V4SImode, gen_lowpart (V4SImode, op1));
      rtx tmp2 = gen_reg_rtx (V4SImode);
      emit_insn (gen_ashrv4si3 (tmp2, tmp1, GEN_INT (bits - 96)));

      rtx tmp3 = gen_reg_rtx (V4SImode);
      emit_insn (gen_ashrv4si3 (tmp3, tmp1, GEN_INT (31)));

      rtx tmp4 = force_reg (V2DImode, gen_lowpart (V2DImode, tmp2));
      rtx tmp5 = force_reg (V2DImode, gen_lowpart (V2DImode, tmp3));
      rtx tmp6 = gen_reg_rtx (V2DImode);
      emit_insn (gen_vec_interleave_highv2di (tmp6, tmp4, tmp5));

      rtx tmp7 = force_reg (V4SImode, gen_lowpart (V4SImode, tmp6));
      rtx tmp8 = gen_reg_rtx (V4SImode);
      emit_insn (gen_sse2_pshufd (tmp8, tmp7, GEN_INT (0xfd)));

      emit_move_insn (operands[0], gen_lowpart (V1TImode, tmp8));
      return;
    }

  if (TARGET_SSE4_1 && (bits == 48 || bits == 80))
    {
      /* Four operations.  */
      rtx tmp1 = force_reg (V4SImode, gen_lowpart (V4SImode, op1));
      rtx tmp2 = gen_reg_rtx (V4SImode);
      emit_insn (gen_sse2_pshufd (tmp2, tmp1, GEN_INT (0xff)));

      rtx tmp3 = gen_reg_rtx (V4SImode);
      emit_insn (gen_ashrv4si3 (tmp3, tmp2, GEN_INT (31)));

      rtx tmp4 = gen_reg_rtx (V1TImode);
      emit_insn (gen_sse2_lshrv1ti3 (tmp4, op1, GEN_INT (bits)));

      rtx tmp5 = force_reg (V8HImode, gen_lowpart (V8HImode, tmp3));
      rtx tmp6 = force_reg (V8HImode, gen_lowpart (V8HImode, tmp4));
      rtx tmp7 = gen_reg_rtx (V8HImode);
      emit_insn (gen_sse4_1_pblendw (tmp7, tmp5, tmp6,
				     GEN_INT (bits == 48 ? 0x1f : 0x07)));

      emit_move_insn (operands[0], gen_lowpart (V1TImode, tmp7));
      return;
    }

  if ((bits & 7) == 0)
    {
      /* Five operations.  */
      rtx tmp1 = force_reg (V4SImode, gen_lowpart (V4SImode, op1));
      rtx tmp2 = gen_reg_rtx (V4SImode);
      emit_insn (gen_sse2_pshufd (tmp2, tmp1, GEN_INT (0xff)));

      rtx tmp3 = gen_reg_rtx (V4SImode);
      emit_insn (gen_ashrv4si3 (tmp3, tmp2, GEN_INT (31)));

      rtx tmp4 = gen_reg_rtx (V1TImode);
      emit_insn (gen_sse2_lshrv1ti3 (tmp4, op1, GEN_INT (bits)));

      rtx tmp5 = force_reg (V1TImode, gen_lowpart (V1TImode, tmp3));
      rtx tmp6 = gen_reg_rtx (V1TImode);
      emit_insn (gen_sse2_ashlv1ti3 (tmp6, tmp5, GEN_INT (128 - bits)));

      rtx tmp7 = force_reg (V2DImode, gen_lowpart (V2DImode, tmp4));
      rtx tmp8 = force_reg (V2DImode, gen_lowpart (V2DImode, tmp6));
      rtx tmp9 = gen_reg_rtx (V2DImode);
      emit_insn (gen_iorv2di3 (tmp9, tmp7, tmp8));

      emit_move_insn (operands[0], gen_lowpart (V1TImode, tmp9));
      return;
    }

  if (TARGET_AVX2 && bits < 32)
    {
      /* Six operations.  */
      rtx tmp1 = force_reg (V4SImode, gen_lowpart (V4SImode, op1));
      rtx tmp2 = gen_reg_rtx (V4SImode);
      emit_insn (gen_ashrv4si3 (tmp2, tmp1, GEN_INT (bits)));

      rtx tmp3 = gen_reg_rtx (V1TImode);
      emit_insn (gen_sse2_lshrv1ti3 (tmp3, op1, GEN_INT (64)));

      rtx tmp4 = force_reg (V2DImode, gen_lowpart (V2DImode, op1));
      rtx tmp5 = gen_reg_rtx (V2DImode);
      emit_insn (gen_lshrv2di3 (tmp5, tmp4, GEN_INT (bits)));

      rtx tmp6 = force_reg (V2DImode, gen_lowpart (V2DImode, tmp3));
      rtx tmp7 = gen_reg_rtx (V2DImode);
      emit_insn (gen_ashlv2di3 (tmp7, tmp6, GEN_INT (64 - bits)));

      rtx tmp8 = gen_reg_rtx (V2DImode);
      emit_insn (gen_iorv2di3 (tmp8, tmp5, tmp7));

      rtx tmp9 = force_reg (V4SImode, gen_lowpart (V4SImode, tmp8));
      rtx tmp10 = gen_reg_rtx (V4SImode);
      emit_insn (gen_avx2_pblenddv4si (tmp10, tmp2, tmp9, GEN_INT (7)));

      emit_move_insn (operands[0], gen_lowpart (V1TImode, tmp10));
      return;
    }

  if (TARGET_SSE4_1 && bits < 15)
    {
      /* Six operations.  */
      rtx tmp1 = force_reg (V4SImode, gen_lowpart (V4SImode, op1));
      rtx tmp2 = gen_reg_rtx (V4SImode);
      emit_insn (gen_ashrv4si3 (tmp2, tmp1, GEN_INT (bits)));

      rtx tmp3 = gen_reg_rtx (V1TImode);
      emit_insn (gen_sse2_lshrv1ti3 (tmp3, op1, GEN_INT (64)));

      rtx tmp4 = force_reg (V2DImode, gen_lowpart (V2DImode, op1));
      rtx tmp5 = gen_reg_rtx (V2DImode);
      emit_insn (gen_lshrv2di3 (tmp5, tmp4, GEN_INT (bits)));

      rtx tmp6 = force_reg (V2DImode, gen_lowpart (V2DImode, tmp3));
      rtx tmp7 = gen_reg_rtx (V2DImode);
      emit_insn (gen_ashlv2di3 (tmp7, tmp6, GEN_INT (64 - bits)));

      rtx tmp8 = gen_reg_rtx (V2DImode);
      emit_insn (gen_iorv2di3 (tmp8, tmp5, tmp7));

      rtx tmp9 = force_reg (V8HImode, gen_lowpart (V8HImode, tmp2));
      rtx tmp10 = force_reg (V8HImode, gen_lowpart (V8HImode, tmp8));
      rtx tmp11 = gen_reg_rtx (V8HImode);
      emit_insn (gen_sse4_1_pblendw (tmp11, tmp9, tmp10, GEN_INT (0x3f)));

      emit_move_insn (operands[0], gen_lowpart (V1TImode, tmp11));
      return;
    }

  if (bits == 1)
    {
      /* Eight operations.  */
      rtx tmp1 = gen_reg_rtx (V1TImode);
      emit_insn (gen_sse2_lshrv1ti3 (tmp1, op1, GEN_INT (64)));

      rtx tmp2 = force_reg (V2DImode, gen_lowpart (V2DImode, op1));
      rtx tmp3 = gen_reg_rtx (V2DImode);
      emit_insn (gen_lshrv2di3 (tmp3, tmp2, GEN_INT (1)));

      rtx tmp4 = force_reg (V2DImode, gen_lowpart (V2DImode, tmp1));
      rtx tmp5 = gen_reg_rtx (V2DImode);
      emit_insn (gen_ashlv2di3 (tmp5, tmp4, GEN_INT (63)));

      rtx tmp6 = gen_reg_rtx (V2DImode);
      emit_insn (gen_iorv2di3 (tmp6, tmp3, tmp5));

      rtx tmp7 = gen_reg_rtx (V2DImode);
      emit_insn (gen_lshrv2di3 (tmp7, tmp2, GEN_INT (63)));

      rtx tmp8 = force_reg (V4SImode, gen_lowpart (V4SImode, tmp7));
      rtx tmp9 = gen_reg_rtx (V4SImode);
      emit_insn (gen_sse2_pshufd (tmp9, tmp8, GEN_INT (0xbf)));

      rtx tmp10 = force_reg (V2DImode, gen_lowpart (V2DImode, tmp9));
      rtx tmp11 = gen_reg_rtx (V2DImode);
      emit_insn (gen_ashlv2di3 (tmp11, tmp10, GEN_INT (31)));

      rtx tmp12 = gen_reg_rtx (V2DImode);
      emit_insn (gen_iorv2di3 (tmp12, tmp6, tmp11));

      emit_move_insn (operands[0], gen_lowpart (V1TImode, tmp12));
      return;
    }

  if (bits > 64)
    {
      /* Eight operations.  */
      rtx tmp1 = force_reg (V4SImode, gen_lowpart (V4SImode, op1));
      rtx tmp2 = gen_reg_rtx (V4SImode);
      emit_insn (gen_sse2_pshufd (tmp2, tmp1, GEN_INT (0xff)));

      rtx tmp3 = gen_reg_rtx (V4SImode);
      emit_insn (gen_ashrv4si3 (tmp3, tmp2, GEN_INT (31)));

      rtx tmp4 = gen_reg_rtx (V1TImode);
      emit_insn (gen_sse2_lshrv1ti3 (tmp4, op1, GEN_INT (64)));

      rtx tmp5 = force_reg (V2DImode, gen_lowpart (V2DImode, tmp4));
      rtx tmp6 = gen_reg_rtx (V2DImode);
      emit_insn (gen_lshrv2di3 (tmp6, tmp5, GEN_INT (bits - 64)));

      rtx tmp7 = force_reg (V1TImode, gen_lowpart (V1TImode, tmp3));
      rtx tmp8 = gen_reg_rtx (V1TImode);
      emit_insn (gen_sse2_ashlv1ti3 (tmp8, tmp7, GEN_INT (64)));
 
      rtx tmp9 = force_reg (V2DImode, gen_lowpart (V2DImode, tmp3));
      rtx tmp10 = gen_reg_rtx (V2DImode);
      emit_insn (gen_ashlv2di3 (tmp10, tmp9, GEN_INT (128 - bits)));

      rtx tmp11 = force_reg (V2DImode, gen_lowpart (V2DImode, tmp8));
      rtx tmp12 = gen_reg_rtx (V2DImode);
      emit_insn (gen_iorv2di3 (tmp12, tmp10, tmp11));

      rtx tmp13 = gen_reg_rtx (V2DImode);
      emit_insn (gen_iorv2di3 (tmp13, tmp6, tmp12));

      emit_move_insn (operands[0], gen_lowpart (V1TImode, tmp13));
    }
  else
    {
      /* Nine operations.  */
      rtx tmp1 = force_reg (V4SImode, gen_lowpart (V4SImode, op1));
      rtx tmp2 = gen_reg_rtx (V4SImode);
      emit_insn (gen_sse2_pshufd (tmp2, tmp1, GEN_INT (0xff)));

      rtx tmp3 = gen_reg_rtx (V4SImode);
      emit_insn (gen_ashrv4si3 (tmp3, tmp2, GEN_INT (31)));

      rtx tmp4 = gen_reg_rtx (V1TImode);
      emit_insn (gen_sse2_lshrv1ti3 (tmp4, op1, GEN_INT (64)));

      rtx tmp5 = force_reg (V2DImode, gen_lowpart (V2DImode, op1));
      rtx tmp6 = gen_reg_rtx (V2DImode);
      emit_insn (gen_lshrv2di3 (tmp6, tmp5, GEN_INT (bits)));

      rtx tmp7 = force_reg (V2DImode, gen_lowpart (V2DImode, tmp4));
      rtx tmp8 = gen_reg_rtx (V2DImode);
      emit_insn (gen_ashlv2di3 (tmp8, tmp7, GEN_INT (64 - bits)));

      rtx tmp9 = gen_reg_rtx (V2DImode);
      emit_insn (gen_iorv2di3 (tmp9, tmp6, tmp8));

      rtx tmp10 = force_reg (V1TImode, gen_lowpart (V1TImode, tmp3));
      rtx tmp11 = gen_reg_rtx (V1TImode);
      emit_insn (gen_sse2_ashlv1ti3 (tmp11, tmp10, GEN_INT (64)));

      rtx tmp12 = force_reg (V2DImode, gen_lowpart (V2DImode, tmp11));
      rtx tmp13 = gen_reg_rtx (V2DImode);
      emit_insn (gen_ashlv2di3 (tmp13, tmp12, GEN_INT (64 - bits)));

      rtx tmp14 = gen_reg_rtx (V2DImode);
      emit_insn (gen_iorv2di3 (tmp14, tmp9, tmp13));

      emit_move_insn (operands[0], gen_lowpart (V1TImode, tmp14));
    }
}

/* Replace all occurrences of REG FROM with REG TO in X, including
   occurrences with different modes.  */

rtx
ix86_replace_reg_with_reg (rtx x, rtx from, rtx to)
{
  gcc_checking_assert (REG_P (from)
		       && REG_P (to)
		       && GET_MODE (from) == GET_MODE (to));
  if (!reg_overlap_mentioned_p (from, x))
    return x;
  rtx ret = copy_rtx (x);
  subrtx_ptr_iterator::array_type array;
  FOR_EACH_SUBRTX_PTR (iter, array, &ret, NONCONST)
    {
      rtx *loc = *iter;
      x = *loc;
      if (REG_P (x) && REGNO (x) == REGNO (from))
	{
	  if (x == from)
	    *loc = to;
	  else
	    {
	      gcc_checking_assert (REG_NREGS (x) == 1);
	      *loc = gen_rtx_REG (GET_MODE (x), REGNO (to));
	    }
	}
    }
  return ret;
}

/* Return mode for the memcpy/memset loop counter.  Prefer SImode over
   DImode for constant loop counts.  */

static machine_mode
counter_mode (rtx count_exp)
{
  if (GET_MODE (count_exp) != VOIDmode)
    return GET_MODE (count_exp);
  if (!CONST_INT_P (count_exp))
    return Pmode;
  if (TARGET_64BIT && (INTVAL (count_exp) & ~0xffffffff))
    return DImode;
  return SImode;
}

/* When ISSETMEM is FALSE, output simple loop to move memory pointer to SRCPTR
   to DESTPTR via chunks of MODE unrolled UNROLL times, overall size is COUNT
   specified in bytes.  When ISSETMEM is TRUE, output the equivalent loop to set
   memory by VALUE (supposed to be in MODE).

   The size is rounded down to whole number of chunk size moved at once.
   SRCMEM and DESTMEM provide MEMrtx to feed proper aliasing info.  */


static void
expand_set_or_cpymem_via_loop (rtx destmem, rtx srcmem,
			       rtx destptr, rtx srcptr, rtx value,
			       rtx count, machine_mode mode, int unroll,
			       int expected_size, bool issetmem)
{
  rtx_code_label *out_label, *top_label;
  rtx iter, tmp;
  machine_mode iter_mode = counter_mode (count);
  int piece_size_n = GET_MODE_SIZE (mode) * unroll;
  rtx piece_size = GEN_INT (piece_size_n);
  rtx piece_size_mask = GEN_INT (~((GET_MODE_SIZE (mode) * unroll) - 1));
  rtx size;
  int i;

  top_label = gen_label_rtx ();
  out_label = gen_label_rtx ();
  iter = gen_reg_rtx (iter_mode);

  size = expand_simple_binop (iter_mode, AND, count, piece_size_mask,
			      NULL, 1, OPTAB_DIRECT);
  /* Those two should combine.  */
  if (piece_size == const1_rtx)
    {
      emit_cmp_and_jump_insns (size, const0_rtx, EQ, NULL_RTX, iter_mode,
			       true, out_label);
      predict_jump (REG_BR_PROB_BASE * 10 / 100);
    }
  emit_move_insn (iter, const0_rtx);

  emit_label (top_label);

  tmp = convert_modes (Pmode, iter_mode, iter, true);

  /* This assert could be relaxed - in this case we'll need to compute
     smallest power of two, containing in PIECE_SIZE_N and pass it to
     offset_address.  */
  gcc_assert ((piece_size_n & (piece_size_n - 1)) == 0);
  destmem = offset_address (destmem, tmp, piece_size_n);
  destmem = adjust_address (destmem, mode, 0);

  if (!issetmem)
    {
      srcmem = offset_address (srcmem, copy_rtx (tmp), piece_size_n);
      srcmem = adjust_address (srcmem, mode, 0);

      /* When unrolling for chips that reorder memory reads and writes,
	 we can save registers by using single temporary.
	 Also using 4 temporaries is overkill in 32bit mode.  */
      if (!TARGET_64BIT && 0)
	{
	  for (i = 0; i < unroll; i++)
	    {
	      if (i)
		{
		  destmem = adjust_address (copy_rtx (destmem), mode,
					    GET_MODE_SIZE (mode));
		  srcmem = adjust_address (copy_rtx (srcmem), mode,
					   GET_MODE_SIZE (mode));
		}
	      emit_move_insn (destmem, srcmem);
	    }
	}
      else
	{
	  rtx tmpreg[4];
	  gcc_assert (unroll <= 4);
	  for (i = 0; i < unroll; i++)
	    {
	      tmpreg[i] = gen_reg_rtx (mode);
	      if (i)
		srcmem = adjust_address (copy_rtx (srcmem), mode,
					 GET_MODE_SIZE (mode));
	      emit_move_insn (tmpreg[i], srcmem);
	    }
	  for (i = 0; i < unroll; i++)
	    {
	      if (i)
		destmem = adjust_address (copy_rtx (destmem), mode,
					  GET_MODE_SIZE (mode));
	      emit_move_insn (destmem, tmpreg[i]);
	    }
	}
    }
  else
    for (i = 0; i < unroll; i++)
      {
	if (i)
	  destmem = adjust_address (copy_rtx (destmem), mode,
				    GET_MODE_SIZE (mode));
	emit_move_insn (destmem, value);
      }

  tmp = expand_simple_binop (iter_mode, PLUS, iter, piece_size, iter,
			     true, OPTAB_LIB_WIDEN);
  if (tmp != iter)
    emit_move_insn (iter, tmp);

  emit_cmp_and_jump_insns (iter, size, LT, NULL_RTX, iter_mode,
			   true, top_label);
  if (expected_size != -1)
    {
      expected_size /= GET_MODE_SIZE (mode) * unroll;
      if (expected_size == 0)
	predict_jump (0);
      else if (expected_size > REG_BR_PROB_BASE)
	predict_jump (REG_BR_PROB_BASE - 1);
      else
        predict_jump (REG_BR_PROB_BASE - (REG_BR_PROB_BASE + expected_size / 2)
		      / expected_size);
    }
  else
    predict_jump (REG_BR_PROB_BASE * 80 / 100);
  iter = ix86_zero_extend_to_Pmode (iter);
  tmp = expand_simple_binop (Pmode, PLUS, destptr, iter, destptr,
			     true, OPTAB_LIB_WIDEN);
  if (tmp != destptr)
    emit_move_insn (destptr, tmp);
  if (!issetmem)
    {
      tmp = expand_simple_binop (Pmode, PLUS, srcptr, iter, srcptr,
				 true, OPTAB_LIB_WIDEN);
      if (tmp != srcptr)
	emit_move_insn (srcptr, tmp);
    }
  emit_label (out_label);
}

/* Divide COUNTREG by SCALE.  */
static rtx
scale_counter (rtx countreg, int scale)
{
  rtx sc;

  if (scale == 1)
    return countreg;
  if (CONST_INT_P (countreg))
    return GEN_INT (INTVAL (countreg) / scale);
  gcc_assert (REG_P (countreg));

  sc = expand_simple_binop (GET_MODE (countreg), LSHIFTRT, countreg,
			    GEN_INT (exact_log2 (scale)),
			    NULL, 1, OPTAB_DIRECT);
  return sc;
}

/* Output "rep; mov" or "rep; stos" instruction depending on ISSETMEM argument.
   When ISSETMEM is true, arguments SRCMEM and SRCPTR are ignored.
   When ISSETMEM is false, arguments VALUE and ORIG_VALUE are ignored.
   For setmem case, VALUE is a promoted to a wider size ORIG_VALUE.
   ORIG_VALUE is the original value passed to memset to fill the memory with.
   Other arguments have same meaning as for previous function.  */

static void
expand_set_or_cpymem_via_rep (rtx destmem, rtx srcmem,
			   rtx destptr, rtx srcptr, rtx value, rtx orig_value,
			   rtx count,
			   machine_mode mode, bool issetmem)
{
  rtx destexp;
  rtx srcexp;
  rtx countreg;
  HOST_WIDE_INT rounded_count;

  /* If possible, it is shorter to use rep movs.
     TODO: Maybe it is better to move this logic to decide_alg.  */
  if (mode == QImode && CONST_INT_P (count) && !(INTVAL (count) & 3)
      && !TARGET_PREFER_KNOWN_REP_MOVSB_STOSB
      && (!issetmem || orig_value == const0_rtx))
    mode = SImode;

  if (destptr != XEXP (destmem, 0) || GET_MODE (destmem) != BLKmode)
    destmem = adjust_automodify_address_nv (destmem, BLKmode, destptr, 0);

  countreg = ix86_zero_extend_to_Pmode (scale_counter (count,
						       GET_MODE_SIZE (mode)));
  if (mode != QImode)
    {
      destexp = gen_rtx_ASHIFT (Pmode, countreg,
				GEN_INT (exact_log2 (GET_MODE_SIZE (mode))));
      destexp = gen_rtx_PLUS (Pmode, destexp, destptr);
    }
  else
    destexp = gen_rtx_PLUS (Pmode, destptr, countreg);
  if ((!issetmem || orig_value == const0_rtx) && CONST_INT_P (count))
    {
      rounded_count
	= ROUND_DOWN (INTVAL (count), (HOST_WIDE_INT) GET_MODE_SIZE (mode));
      destmem = shallow_copy_rtx (destmem);
      set_mem_size (destmem, rounded_count);
    }
  else if (MEM_SIZE_KNOWN_P (destmem))
    clear_mem_size (destmem);

  if (issetmem)
    {
      value = force_reg (mode, gen_lowpart (mode, value));
      emit_insn (gen_rep_stos (destptr, countreg, destmem, value, destexp));
    }
  else
    {
      if (srcptr != XEXP (srcmem, 0) || GET_MODE (srcmem) != BLKmode)
	srcmem = adjust_automodify_address_nv (srcmem, BLKmode, srcptr, 0);
      if (mode != QImode)
	{
	  srcexp = gen_rtx_ASHIFT (Pmode, countreg,
				   GEN_INT (exact_log2 (GET_MODE_SIZE (mode))));
	  srcexp = gen_rtx_PLUS (Pmode, srcexp, srcptr);
	}
      else
	srcexp = gen_rtx_PLUS (Pmode, srcptr, countreg);
      if (CONST_INT_P (count))
	{
	  rounded_count
	    = ROUND_DOWN (INTVAL (count), (HOST_WIDE_INT) GET_MODE_SIZE (mode));
	  srcmem = shallow_copy_rtx (srcmem);
	  set_mem_size (srcmem, rounded_count);
	}
      else
	{
	  if (MEM_SIZE_KNOWN_P (srcmem))
	    clear_mem_size (srcmem);
	}
      emit_insn (gen_rep_mov (destptr, destmem, srcptr, srcmem, countreg,
			      destexp, srcexp));
    }
}

/* This function emits moves to copy SIZE_TO_MOVE bytes from SRCMEM to
   DESTMEM.
   SRC is passed by pointer to be updated on return.
   Return value is updated DST.  */
static rtx
emit_memmov (rtx destmem, rtx *srcmem, rtx destptr, rtx srcptr,
	     HOST_WIDE_INT size_to_move)
{
  rtx dst = destmem, src = *srcmem, tempreg;
  enum insn_code code;
  machine_mode move_mode;
  int piece_size, i;

  /* Find the widest mode in which we could perform moves.
     Start with the biggest power of 2 less than SIZE_TO_MOVE and half
     it until move of such size is supported.  */
  piece_size = 1 << floor_log2 (size_to_move);
  while (!int_mode_for_size (piece_size * BITS_PER_UNIT, 0).exists (&move_mode)
	 || (code = optab_handler (mov_optab, move_mode)) == CODE_FOR_nothing)
    {
      gcc_assert (piece_size > 1);
      piece_size >>= 1;
    }

  /* Find the corresponding vector mode with the same size as MOVE_MODE.
     MOVE_MODE is an integer mode at the moment (SI, DI, TI, etc.).  */
  if (GET_MODE_SIZE (move_mode) > GET_MODE_SIZE (word_mode))
    {
      int nunits = GET_MODE_SIZE (move_mode) / GET_MODE_SIZE (word_mode);
      if (!mode_for_vector (word_mode, nunits).exists (&move_mode)
	  || (code = optab_handler (mov_optab, move_mode)) == CODE_FOR_nothing)
	{
	  move_mode = word_mode;
	  piece_size = GET_MODE_SIZE (move_mode);
	  code = optab_handler (mov_optab, move_mode);
	}
    }
  gcc_assert (code != CODE_FOR_nothing);

  dst = adjust_automodify_address_nv (dst, move_mode, destptr, 0);
  src = adjust_automodify_address_nv (src, move_mode, srcptr, 0);

  /* Emit moves.  We'll need SIZE_TO_MOVE/PIECE_SIZES moves.  */
  gcc_assert (size_to_move % piece_size == 0);

  for (i = 0; i < size_to_move; i += piece_size)
    {
      /* We move from memory to memory, so we'll need to do it via
	 a temporary register.  */
      tempreg = gen_reg_rtx (move_mode);
      emit_insn (GEN_FCN (code) (tempreg, src));
      emit_insn (GEN_FCN (code) (dst, tempreg));

      emit_move_insn (destptr,
		      plus_constant (Pmode, copy_rtx (destptr), piece_size));
      emit_move_insn (srcptr,
		      plus_constant (Pmode, copy_rtx (srcptr), piece_size));

      dst = adjust_automodify_address_nv (dst, move_mode, destptr,
					  piece_size);
      src = adjust_automodify_address_nv (src, move_mode, srcptr,
					  piece_size);
    }

  /* Update DST and SRC rtx.  */
  *srcmem = src;
  return dst;
}

/* Helper function for the string operations below.  Dest VARIABLE whether
   it is aligned to VALUE bytes.  If true, jump to the label.  */

static rtx_code_label *
ix86_expand_aligntest (rtx variable, int value, bool epilogue)
{
  rtx_code_label *label = gen_label_rtx ();
  rtx tmpcount = gen_reg_rtx (GET_MODE (variable));
  if (GET_MODE (variable) == DImode)
    emit_insn (gen_anddi3 (tmpcount, variable, GEN_INT (value)));
  else
    emit_insn (gen_andsi3 (tmpcount, variable, GEN_INT (value)));
  emit_cmp_and_jump_insns (tmpcount, const0_rtx, EQ, 0, GET_MODE (variable),
			   1, label);
  if (epilogue)
    predict_jump (REG_BR_PROB_BASE * 50 / 100);
  else
    predict_jump (REG_BR_PROB_BASE * 90 / 100);
  return label;
}


/* Output code to copy at most count & (max_size - 1) bytes from SRC to DEST.  */

static void
expand_cpymem_epilogue (rtx destmem, rtx srcmem,
			rtx destptr, rtx srcptr, rtx count, int max_size)
{
  rtx src, dest;
  if (CONST_INT_P (count))
    {
      HOST_WIDE_INT countval = INTVAL (count);
      HOST_WIDE_INT epilogue_size = countval % max_size;
      int i;

      /* For now MAX_SIZE should be a power of 2.  This assert could be
	 relaxed, but it'll require a bit more complicated epilogue
	 expanding.  */
      gcc_assert ((max_size & (max_size - 1)) == 0);
      for (i = max_size; i >= 1; i >>= 1)
	{
	  if (epilogue_size & i)
	    destmem = emit_memmov (destmem, &srcmem, destptr, srcptr, i);
	}
      return;
    }
  if (max_size > 8)
    {
      count = expand_simple_binop (GET_MODE (count), AND, count, GEN_INT (max_size - 1),
				    count, 1, OPTAB_DIRECT);
      expand_set_or_cpymem_via_loop (destmem, srcmem, destptr, srcptr, NULL,
				     count, QImode, 1, 4, false);
      return;
    }

  /* When there are stringops, we can cheaply increase dest and src pointers.
     Otherwise we save code size by maintaining offset (zero is readily
     available from preceding rep operation) and using x86 addressing modes.
   */
  if (TARGET_SINGLE_STRINGOP)
    {
      if (max_size > 4)
	{
	  rtx_code_label *label = ix86_expand_aligntest (count, 4, true);
	  src = change_address (srcmem, SImode, srcptr);
	  dest = change_address (destmem, SImode, destptr);
	  emit_insn (gen_strmov (destptr, dest, srcptr, src));
	  emit_label (label);
	  LABEL_NUSES (label) = 1;
	}
      if (max_size > 2)
	{
	  rtx_code_label *label = ix86_expand_aligntest (count, 2, true);
	  src = change_address (srcmem, HImode, srcptr);
	  dest = change_address (destmem, HImode, destptr);
	  emit_insn (gen_strmov (destptr, dest, srcptr, src));
	  emit_label (label);
	  LABEL_NUSES (label) = 1;
	}
      if (max_size > 1)
	{
	  rtx_code_label *label = ix86_expand_aligntest (count, 1, true);
	  src = change_address (srcmem, QImode, srcptr);
	  dest = change_address (destmem, QImode, destptr);
	  emit_insn (gen_strmov (destptr, dest, srcptr, src));
	  emit_label (label);
	  LABEL_NUSES (label) = 1;
	}
    }
  else
    {
      rtx offset = force_reg (Pmode, const0_rtx);
      rtx tmp;

      if (max_size > 4)
	{
	  rtx_code_label *label = ix86_expand_aligntest (count, 4, true);
	  src = change_address (srcmem, SImode, srcptr);
	  dest = change_address (destmem, SImode, destptr);
	  emit_move_insn (dest, src);
	  tmp = expand_simple_binop (Pmode, PLUS, offset, GEN_INT (4), NULL,
				     true, OPTAB_LIB_WIDEN);
	  if (tmp != offset)
	    emit_move_insn (offset, tmp);
	  emit_label (label);
	  LABEL_NUSES (label) = 1;
	}
      if (max_size > 2)
	{
	  rtx_code_label *label = ix86_expand_aligntest (count, 2, true);
	  tmp = gen_rtx_PLUS (Pmode, srcptr, offset);
	  src = change_address (srcmem, HImode, tmp);
	  tmp = gen_rtx_PLUS (Pmode, destptr, offset);
	  dest = change_address (destmem, HImode, tmp);
	  emit_move_insn (dest, src);
	  tmp = expand_simple_binop (Pmode, PLUS, offset, GEN_INT (2), tmp,
				     true, OPTAB_LIB_WIDEN);
	  if (tmp != offset)
	    emit_move_insn (offset, tmp);
	  emit_label (label);
	  LABEL_NUSES (label) = 1;
	}
      if (max_size > 1)
	{
	  rtx_code_label *label = ix86_expand_aligntest (count, 1, true);
	  tmp = gen_rtx_PLUS (Pmode, srcptr, offset);
	  src = change_address (srcmem, QImode, tmp);
	  tmp = gen_rtx_PLUS (Pmode, destptr, offset);
	  dest = change_address (destmem, QImode, tmp);
	  emit_move_insn (dest, src);
	  emit_label (label);
	  LABEL_NUSES (label) = 1;
	}
    }
}

/* This function emits moves to fill SIZE_TO_MOVE bytes starting from DESTMEM
   with value PROMOTED_VAL.
   SRC is passed by pointer to be updated on return.
   Return value is updated DST.  */
static rtx
emit_memset (rtx destmem, rtx destptr, rtx promoted_val,
	     HOST_WIDE_INT size_to_move)
{
  rtx dst = destmem;
  enum insn_code code;
  machine_mode move_mode;
  int piece_size, i;

  /* Find the widest mode in which we could perform moves.
     Start with the biggest power of 2 less than SIZE_TO_MOVE and half
     it until move of such size is supported.  */
  move_mode = GET_MODE (promoted_val);
  if (move_mode == VOIDmode)
    move_mode = QImode;
  if (size_to_move < GET_MODE_SIZE (move_mode))
    {
      unsigned int move_bits = size_to_move * BITS_PER_UNIT;
      move_mode = int_mode_for_size (move_bits, 0).require ();
      promoted_val = gen_lowpart (move_mode, promoted_val);
    }
  piece_size = GET_MODE_SIZE (move_mode);
  code = optab_handler (mov_optab, move_mode);
  gcc_assert (code != CODE_FOR_nothing && promoted_val != NULL_RTX);

  dst = adjust_automodify_address_nv (dst, move_mode, destptr, 0);

  /* Emit moves.  We'll need SIZE_TO_MOVE/PIECE_SIZES moves.  */
  gcc_assert (size_to_move % piece_size == 0);

  for (i = 0; i < size_to_move; i += piece_size)
    {
      if (piece_size <= GET_MODE_SIZE (word_mode))
	{
	  emit_insn (gen_strset (destptr, dst, promoted_val));
	  dst = adjust_automodify_address_nv (dst, move_mode, destptr,
					      piece_size);
	  continue;
	}

      emit_insn (GEN_FCN (code) (dst, promoted_val));

      emit_move_insn (destptr,
		      plus_constant (Pmode, copy_rtx (destptr), piece_size));

      dst = adjust_automodify_address_nv (dst, move_mode, destptr,
					  piece_size);
    }

  /* Update DST rtx.  */
  return dst;
}
/* Output code to set at most count & (max_size - 1) bytes starting by DEST.  */
static void
expand_setmem_epilogue_via_loop (rtx destmem, rtx destptr, rtx value,
				 rtx count, int max_size)
{
  count = expand_simple_binop (counter_mode (count), AND, count,
			       GEN_INT (max_size - 1), count, 1, OPTAB_DIRECT);
  expand_set_or_cpymem_via_loop (destmem, NULL, destptr, NULL,
				 gen_lowpart (QImode, value), count, QImode,
				 1, max_size / 2, true);
}

/* Output code to set at most count & (max_size - 1) bytes starting by DEST.  */
static void
expand_setmem_epilogue (rtx destmem, rtx destptr, rtx value, rtx vec_value,
			rtx count, int max_size)
{
  rtx dest;

  if (CONST_INT_P (count))
    {
      HOST_WIDE_INT countval = INTVAL (count);
      HOST_WIDE_INT epilogue_size = countval % max_size;
      int i;

      /* For now MAX_SIZE should be a power of 2.  This assert could be
	 relaxed, but it'll require a bit more complicated epilogue
	 expanding.  */
      gcc_assert ((max_size & (max_size - 1)) == 0);
      for (i = max_size; i >= 1; i >>= 1)
	{
	  if (epilogue_size & i)
	    {
	      if (vec_value && i > GET_MODE_SIZE (GET_MODE (value)))
		destmem = emit_memset (destmem, destptr, vec_value, i);
	      else
		destmem = emit_memset (destmem, destptr, value, i);
	    }
	}
      return;
    }
  if (max_size > 32)
    {
      expand_setmem_epilogue_via_loop (destmem, destptr, value, count, max_size);
      return;
    }
  if (max_size > 16)
    {
      rtx_code_label *label = ix86_expand_aligntest (count, 16, true);
      if (TARGET_64BIT)
	{
	  dest = change_address (destmem, DImode, destptr);
	  emit_insn (gen_strset (destptr, dest, value));
	  dest = adjust_automodify_address_nv (dest, DImode, destptr, 8);
	  emit_insn (gen_strset (destptr, dest, value));
	}
      else
	{
	  dest = change_address (destmem, SImode, destptr);
	  emit_insn (gen_strset (destptr, dest, value));
	  dest = adjust_automodify_address_nv (dest, SImode, destptr, 4);
	  emit_insn (gen_strset (destptr, dest, value));
	  dest = adjust_automodify_address_nv (dest, SImode, destptr, 8);
	  emit_insn (gen_strset (destptr, dest, value));
	  dest = adjust_automodify_address_nv (dest, SImode, destptr, 12);
	  emit_insn (gen_strset (destptr, dest, value));
	}
      emit_label (label);
      LABEL_NUSES (label) = 1;
    }
  if (max_size > 8)
    {
      rtx_code_label *label = ix86_expand_aligntest (count, 8, true);
      if (TARGET_64BIT)
	{
	  dest = change_address (destmem, DImode, destptr);
	  emit_insn (gen_strset (destptr, dest, value));
	}
      else
	{
	  dest = change_address (destmem, SImode, destptr);
	  emit_insn (gen_strset (destptr, dest, value));
	  dest = adjust_automodify_address_nv (dest, SImode, destptr, 4);
	  emit_insn (gen_strset (destptr, dest, value));
	}
      emit_label (label);
      LABEL_NUSES (label) = 1;
    }
  if (max_size > 4)
    {
      rtx_code_label *label = ix86_expand_aligntest (count, 4, true);
      dest = change_address (destmem, SImode, destptr);
      emit_insn (gen_strset (destptr, dest, gen_lowpart (SImode, value)));
      emit_label (label);
      LABEL_NUSES (label) = 1;
    }
  if (max_size > 2)
    {
      rtx_code_label *label = ix86_expand_aligntest (count, 2, true);
      dest = change_address (destmem, HImode, destptr);
      emit_insn (gen_strset (destptr, dest, gen_lowpart (HImode, value)));
      emit_label (label);
      LABEL_NUSES (label) = 1;
    }
  if (max_size > 1)
    {
      rtx_code_label *label = ix86_expand_aligntest (count, 1, true);
      dest = change_address (destmem, QImode, destptr);
      emit_insn (gen_strset (destptr, dest, gen_lowpart (QImode, value)));
      emit_label (label);
      LABEL_NUSES (label) = 1;
    }
}

/* Adjust COUNTER by the VALUE.  */
static void
ix86_adjust_counter (rtx countreg, HOST_WIDE_INT value)
{
  emit_insn (gen_add2_insn (countreg, GEN_INT (-value)));
}

/* Depending on ISSETMEM, copy enough from SRCMEM to DESTMEM or set enough to
   DESTMEM to align it to DESIRED_ALIGNMENT.  Original alignment is ALIGN.
   Depending on ISSETMEM, either arguments SRCMEM/SRCPTR or VALUE/VEC_VALUE are
   ignored.
   Return value is updated DESTMEM.  */

static rtx
expand_set_or_cpymem_prologue (rtx destmem, rtx srcmem,
				  rtx destptr, rtx srcptr, rtx value,
				  rtx vec_value, rtx count, int align,
				  int desired_alignment, bool issetmem)
{
  int i;
  for (i = 1; i < desired_alignment; i <<= 1)
    {
      if (align <= i)
	{
	  rtx_code_label *label = ix86_expand_aligntest (destptr, i, false);
	  if (issetmem)
	    {
	      if (vec_value && i > GET_MODE_SIZE (GET_MODE (value)))
		destmem = emit_memset (destmem, destptr, vec_value, i);
	      else
		destmem = emit_memset (destmem, destptr, value, i);
	    }
	  else
	    destmem = emit_memmov (destmem, &srcmem, destptr, srcptr, i);
	  ix86_adjust_counter (count, i);
	  emit_label (label);
	  LABEL_NUSES (label) = 1;
	  set_mem_align (destmem, i * 2 * BITS_PER_UNIT);
	}
    }
  return destmem;
}

/* Test if COUNT&SIZE is nonzero and if so, expand movme
   or setmem sequence that is valid for SIZE..2*SIZE-1 bytes
   and jump to DONE_LABEL.  */
static void
expand_small_cpymem_or_setmem (rtx destmem, rtx srcmem,
			       rtx destptr, rtx srcptr,
			       rtx value, rtx vec_value,
			       rtx count, int size,
			       rtx done_label, bool issetmem)
{
  rtx_code_label *label = ix86_expand_aligntest (count, size, false);
  machine_mode mode = int_mode_for_size (size * BITS_PER_UNIT, 1).else_blk ();
  rtx modesize;
  int n;

  /* If we do not have vector value to copy, we must reduce size.  */
  if (issetmem)
    {
      if (!vec_value)
	{
	  if (GET_MODE (value) == VOIDmode && size > 8)
	    mode = Pmode;
	  else if (GET_MODE_SIZE (mode) > GET_MODE_SIZE (GET_MODE (value)))
	    mode = GET_MODE (value);
	}
      else
	mode = GET_MODE (vec_value), value = vec_value;
    }
  else
    {
      /* Choose appropriate vector mode.  */
      if (size >= 32)
	mode = TARGET_AVX ? V32QImode : TARGET_SSE ? V16QImode : DImode;
      else if (size >= 16)
	mode = TARGET_SSE ? V16QImode : DImode;
      srcmem = change_address (srcmem, mode, srcptr);
    }
  destmem = change_address (destmem, mode, destptr);
  modesize = GEN_INT (GET_MODE_SIZE (mode));
  gcc_assert (GET_MODE_SIZE (mode) <= size);
  for (n = 0; n * GET_MODE_SIZE (mode) < size; n++)
    {
      if (issetmem)
	emit_move_insn (destmem, gen_lowpart (mode, value));
      else
	{
          emit_move_insn (destmem, srcmem);
          srcmem = offset_address (srcmem, modesize, GET_MODE_SIZE (mode));
	}
      destmem = offset_address (destmem, modesize, GET_MODE_SIZE (mode));
    }

  destmem = offset_address (destmem, count, 1);
  destmem = offset_address (destmem, GEN_INT (-2 * size),
			    GET_MODE_SIZE (mode));
  if (!issetmem)
    {
      srcmem = offset_address (srcmem, count, 1);
      srcmem = offset_address (srcmem, GEN_INT (-2 * size),
			       GET_MODE_SIZE (mode));
    }
  for (n = 0; n * GET_MODE_SIZE (mode) < size; n++)
    {
      if (issetmem)
	emit_move_insn (destmem, gen_lowpart (mode, value));
      else
	{
	  emit_move_insn (destmem, srcmem);
	  srcmem = offset_address (srcmem, modesize, GET_MODE_SIZE (mode));
	}
      destmem = offset_address (destmem, modesize, GET_MODE_SIZE (mode));
    }
  emit_jump_insn (gen_jump (done_label));
  emit_barrier ();

  emit_label (label);
  LABEL_NUSES (label) = 1;
}

/* Handle small memcpy (up to SIZE that is supposed to be small power of 2.
   and get ready for the main memcpy loop by copying iniital DESIRED_ALIGN-ALIGN
   bytes and last SIZE bytes adjusitng DESTPTR/SRCPTR/COUNT in a way we can
   proceed with an loop copying SIZE bytes at once. Do moves in MODE.
   DONE_LABEL is a label after the whole copying sequence. The label is created
   on demand if *DONE_LABEL is NULL.
   MIN_SIZE is minimal size of block copied.  This value gets adjusted for new
   bounds after the initial copies. 

   DESTMEM/SRCMEM are memory expressions pointing to the copies block,
   DESTPTR/SRCPTR are pointers to the block. DYNAMIC_CHECK indicate whether
   we will dispatch to a library call for large blocks.

   In pseudocode we do:

   if (COUNT < SIZE)
     {
       Assume that SIZE is 4. Bigger sizes are handled analogously
       if (COUNT & 4)
	 {
	    copy 4 bytes from SRCPTR to DESTPTR
	    copy 4 bytes from SRCPTR + COUNT - 4 to DESTPTR + COUNT - 4
	    goto done_label
	 }
       if (!COUNT)
	 goto done_label;
       copy 1 byte from SRCPTR to DESTPTR
       if (COUNT & 2)
	 {
	    copy 2 bytes from SRCPTR to DESTPTR
	    copy 2 bytes from SRCPTR + COUNT - 2 to DESTPTR + COUNT - 2
	 }
     }
   else
     {
       copy at least DESIRED_ALIGN-ALIGN bytes from SRCPTR to DESTPTR
       copy SIZE bytes from SRCPTR + COUNT - SIZE to DESTPTR + COUNT -SIZE

       OLD_DESPTR = DESTPTR;
       Align DESTPTR up to DESIRED_ALIGN
       SRCPTR += DESTPTR - OLD_DESTPTR
       COUNT -= DEST_PTR - OLD_DESTPTR
       if (DYNAMIC_CHECK)
	 Round COUNT down to multiple of SIZE
       << optional caller supplied zero size guard is here >>
       << optional caller supplied dynamic check is here >>
       << caller supplied main copy loop is here >>
     }
   done_label:
  */
static void
expand_set_or_cpymem_prologue_epilogue_by_misaligned_moves (rtx destmem, rtx srcmem,
							    rtx *destptr, rtx *srcptr,
							    machine_mode mode,
							    rtx value, rtx vec_value,
							    rtx *count,
							    rtx_code_label **done_label,
							    int size,
							    int desired_align,
							    int align,
							    unsigned HOST_WIDE_INT *min_size,
							    bool dynamic_check,
							    bool issetmem)
{
  rtx_code_label *loop_label = NULL, *label;
  int n;
  rtx modesize;
  int prolog_size = 0;
  rtx mode_value;

  /* Chose proper value to copy.  */
  if (issetmem && VECTOR_MODE_P (mode))
    mode_value = vec_value;
  else
    mode_value = value;
  gcc_assert (GET_MODE_SIZE (mode) <= size);

  /* See if block is big or small, handle small blocks.  */
  if (!CONST_INT_P (*count) && *min_size < (unsigned HOST_WIDE_INT)size)
    {
      int size2 = size;
      loop_label = gen_label_rtx ();

      if (!*done_label)
	*done_label = gen_label_rtx ();

      emit_cmp_and_jump_insns (*count, GEN_INT (size2), GE, 0, GET_MODE (*count),
			       1, loop_label);
      size2 >>= 1;

      /* Handle sizes > 3.  */
      for (;size2 > 2; size2 >>= 1)
	expand_small_cpymem_or_setmem (destmem, srcmem,
				       *destptr, *srcptr,
				       value, vec_value,
				       *count,
				       size2, *done_label, issetmem);
      /* Nothing to copy?  Jump to DONE_LABEL if so */
      emit_cmp_and_jump_insns (*count, const0_rtx, EQ, 0, GET_MODE (*count),
			       1, *done_label);

      /* Do a byte copy.  */
      destmem = change_address (destmem, QImode, *destptr);
      if (issetmem)
	emit_move_insn (destmem, gen_lowpart (QImode, value));
      else
	{
          srcmem = change_address (srcmem, QImode, *srcptr);
          emit_move_insn (destmem, srcmem);
	}

      /* Handle sizes 2 and 3.  */
      label = ix86_expand_aligntest (*count, 2, false);
      destmem = change_address (destmem, HImode, *destptr);
      destmem = offset_address (destmem, *count, 1);
      destmem = offset_address (destmem, GEN_INT (-2), 2);
      if (issetmem)
        emit_move_insn (destmem, gen_lowpart (HImode, value));
      else
	{
	  srcmem = change_address (srcmem, HImode, *srcptr);
	  srcmem = offset_address (srcmem, *count, 1);
	  srcmem = offset_address (srcmem, GEN_INT (-2), 2);
	  emit_move_insn (destmem, srcmem);
	}

      emit_label (label);
      LABEL_NUSES (label) = 1;
      emit_jump_insn (gen_jump (*done_label));
      emit_barrier ();
    }
  else
    gcc_assert (*min_size >= (unsigned HOST_WIDE_INT)size
		|| UINTVAL (*count) >= (unsigned HOST_WIDE_INT)size);

  /* Start memcpy for COUNT >= SIZE.  */
  if (loop_label)
    {
       emit_label (loop_label);
       LABEL_NUSES (loop_label) = 1;
    }

  /* Copy first desired_align bytes.  */
  if (!issetmem)
    srcmem = change_address (srcmem, mode, *srcptr);
  destmem = change_address (destmem, mode, *destptr);
  modesize = GEN_INT (GET_MODE_SIZE (mode));
  for (n = 0; prolog_size < desired_align - align; n++)
    {
      if (issetmem)
        emit_move_insn (destmem, mode_value);
      else
	{
          emit_move_insn (destmem, srcmem);
          srcmem = offset_address (srcmem, modesize, GET_MODE_SIZE (mode));
	}
      destmem = offset_address (destmem, modesize, GET_MODE_SIZE (mode));
      prolog_size += GET_MODE_SIZE (mode);
    }


  /* Copy last SIZE bytes.  */
  destmem = offset_address (destmem, *count, 1);
  destmem = offset_address (destmem,
			    GEN_INT (-size - prolog_size),
			    1);
  if (issetmem)
    emit_move_insn (destmem, mode_value);
  else
    {
      srcmem = offset_address (srcmem, *count, 1);
      srcmem = offset_address (srcmem,
			       GEN_INT (-size - prolog_size),
			       1);
      emit_move_insn (destmem, srcmem);
    }
  for (n = 1; n * GET_MODE_SIZE (mode) < size; n++)
    {
      destmem = offset_address (destmem, modesize, 1);
      if (issetmem)
	emit_move_insn (destmem, mode_value);
      else
	{
          srcmem = offset_address (srcmem, modesize, 1);
          emit_move_insn (destmem, srcmem);
	}
    }

  /* Align destination.  */
  if (desired_align > 1 && desired_align > align)
    {
      rtx saveddest = *destptr;

      gcc_assert (desired_align <= size);
      /* Align destptr up, place it to new register.  */
      *destptr = expand_simple_binop (GET_MODE (*destptr), PLUS, *destptr,
				      GEN_INT (prolog_size),
				      NULL_RTX, 1, OPTAB_DIRECT);
      if (REG_P (*destptr) && REG_P (saveddest) && REG_POINTER (saveddest))
	REG_POINTER (*destptr) = 1;
      *destptr = expand_simple_binop (GET_MODE (*destptr), AND, *destptr,
				      GEN_INT (-desired_align),
				      *destptr, 1, OPTAB_DIRECT);
      /* See how many bytes we skipped.  */
      saveddest = expand_simple_binop (GET_MODE (*destptr), MINUS, saveddest,
				       *destptr,
				       NULL_RTX, 1, OPTAB_DIRECT);
      /* Adjust srcptr and count.  */
      if (!issetmem)
	*srcptr = expand_simple_binop (GET_MODE (*srcptr), MINUS, *srcptr,
				       saveddest, *srcptr, 1, OPTAB_DIRECT);
      *count = expand_simple_binop (GET_MODE (*count), PLUS, *count,
				    saveddest, *count, 1, OPTAB_DIRECT);
      /* We copied at most size + prolog_size.  */
      if (*min_size > (unsigned HOST_WIDE_INT)(size + prolog_size))
	*min_size
	  = ROUND_DOWN (*min_size - size, (unsigned HOST_WIDE_INT)size);
      else
	*min_size = 0;

      /* Our loops always round down the block size, but for dispatch to
         library we need precise value.  */
      if (dynamic_check)
	*count = expand_simple_binop (GET_MODE (*count), AND, *count,
				      GEN_INT (-size), *count, 1, OPTAB_DIRECT);
    }
  else
    {
      gcc_assert (prolog_size == 0);
      /* Decrease count, so we won't end up copying last word twice.  */
      if (!CONST_INT_P (*count))
	*count = expand_simple_binop (GET_MODE (*count), PLUS, *count,
				      constm1_rtx, *count, 1, OPTAB_DIRECT);
      else
	*count = GEN_INT (ROUND_DOWN (UINTVAL (*count) - 1,
				      (unsigned HOST_WIDE_INT)size));
      if (*min_size)
	*min_size = ROUND_DOWN (*min_size - 1, (unsigned HOST_WIDE_INT)size);
    }
}


/* This function is like the previous one, except here we know how many bytes
   need to be copied.  That allows us to update alignment not only of DST, which
   is returned, but also of SRC, which is passed as a pointer for that
   reason.  */
static rtx
expand_set_or_cpymem_constant_prologue (rtx dst, rtx *srcp, rtx destreg,
					   rtx srcreg, rtx value, rtx vec_value,
					   int desired_align, int align_bytes,
					   bool issetmem)
{
  rtx src = NULL;
  rtx orig_dst = dst;
  rtx orig_src = NULL;
  int piece_size = 1;
  int copied_bytes = 0;

  if (!issetmem)
    {
      gcc_assert (srcp != NULL);
      src = *srcp;
      orig_src = src;
    }

  for (piece_size = 1;
       piece_size <= desired_align && copied_bytes < align_bytes;
       piece_size <<= 1)
    {
      if (align_bytes & piece_size)
	{
	  if (issetmem)
	    {
	      if (vec_value && piece_size > GET_MODE_SIZE (GET_MODE (value)))
		dst = emit_memset (dst, destreg, vec_value, piece_size);
	      else
		dst = emit_memset (dst, destreg, value, piece_size);
	    }
	  else
	    dst = emit_memmov (dst, &src, destreg, srcreg, piece_size);
	  copied_bytes += piece_size;
	}
    }
  if (MEM_ALIGN (dst) < (unsigned int) desired_align * BITS_PER_UNIT)
    set_mem_align (dst, desired_align * BITS_PER_UNIT);
  if (MEM_SIZE_KNOWN_P (orig_dst))
    set_mem_size (dst, MEM_SIZE (orig_dst) - align_bytes);

  if (!issetmem)
    {
      int src_align_bytes = get_mem_align_offset (src, desired_align
						       * BITS_PER_UNIT);
      if (src_align_bytes >= 0)
	src_align_bytes = desired_align - src_align_bytes;
      if (src_align_bytes >= 0)
	{
	  unsigned int src_align;
	  for (src_align = desired_align; src_align >= 2; src_align >>= 1)
	    {
	      if ((src_align_bytes & (src_align - 1))
		   == (align_bytes & (src_align - 1)))
		break;
	    }
	  if (src_align > (unsigned int) desired_align)
	    src_align = desired_align;
	  if (MEM_ALIGN (src) < src_align * BITS_PER_UNIT)
	    set_mem_align (src, src_align * BITS_PER_UNIT);
	}
      if (MEM_SIZE_KNOWN_P (orig_src))
	set_mem_size (src, MEM_SIZE (orig_src) - align_bytes);
      *srcp = src;
    }

  return dst;
}

/* Return true if ALG can be used in current context.  
   Assume we expand memset if MEMSET is true.  */
static bool
alg_usable_p (enum stringop_alg alg, bool memset, bool have_as)
{
  if (alg == no_stringop)
    return false;
  /* It is not possible to use a library call if we have non-default
     address space.  We can do better than the generic byte-at-a-time
     loop, used as a fallback.  */
  if (alg == libcall && have_as)
    return false;
  if (alg == vector_loop)
    return TARGET_SSE || TARGET_AVX;
  /* Algorithms using the rep prefix want at least edi and ecx;
     additionally, memset wants eax and memcpy wants esi.  Don't
     consider such algorithms if the user has appropriated those
     registers for their own purposes, or if we have a non-default
     address space, since some string insns cannot override the segment.  */
  if (alg == rep_prefix_1_byte
      || alg == rep_prefix_4_byte
      || alg == rep_prefix_8_byte)
    {
      if (have_as)
	return false;
      if (fixed_regs[CX_REG]
	  || fixed_regs[DI_REG]
	  || (memset ? fixed_regs[AX_REG] : fixed_regs[SI_REG]))
	return false;
    }
  return true;
}

/* Given COUNT and EXPECTED_SIZE, decide on codegen of string operation.  */
static enum stringop_alg
decide_alg (HOST_WIDE_INT count, HOST_WIDE_INT expected_size,
	    unsigned HOST_WIDE_INT min_size, unsigned HOST_WIDE_INT max_size,
	    bool memset, bool zero_memset, bool have_as,
	    int *dynamic_check, bool *noalign, bool recur)
{
  const struct stringop_algs *algs;
  bool optimize_for_speed;
  int max = 0;
  const struct processor_costs *cost;
  int i;
  bool any_alg_usable_p = false;

  *noalign = false;
  *dynamic_check = -1;

  /* Even if the string operation call is cold, we still might spend a lot
     of time processing large blocks.  */
  if (optimize_function_for_size_p (cfun)
      || (optimize_insn_for_size_p ()
 	  && (max_size < 256
              || (expected_size != -1 && expected_size < 256))))
    optimize_for_speed = false;
  else
    optimize_for_speed = true;

  cost = optimize_for_speed ? ix86_cost : &ix86_size_cost;
  if (memset)
    algs = &cost->memset[TARGET_64BIT != 0];
  else
    algs = &cost->memcpy[TARGET_64BIT != 0];

  /* See maximal size for user defined algorithm.  */
  for (i = 0; i < MAX_STRINGOP_ALGS; i++)
    {
      enum stringop_alg candidate = algs->size[i].alg;
      bool usable = alg_usable_p (candidate, memset, have_as);
      any_alg_usable_p |= usable;

      if (candidate != libcall && candidate && usable)
	max = algs->size[i].max;
    }

  /* If expected size is not known but max size is small enough
     so inline version is a win, set expected size into
     the range.  */
  if (((max > 1 && (unsigned HOST_WIDE_INT) max >= max_size) || max == -1)
      && expected_size == -1)
    expected_size = min_size / 2 + max_size / 2;

  /* If user specified the algorithm, honor it if possible.  */
  if (ix86_stringop_alg != no_stringop
      && alg_usable_p (ix86_stringop_alg, memset, have_as))
    return ix86_stringop_alg;
  /* rep; movq or rep; movl is the smallest variant.  */
  else if (!optimize_for_speed)
    {
      *noalign = true;
      if (!count || (count & 3) || (memset && !zero_memset))
	return alg_usable_p (rep_prefix_1_byte, memset, have_as)
	       ? rep_prefix_1_byte : loop_1_byte;
      else
	return alg_usable_p (rep_prefix_4_byte, memset, have_as)
	       ? rep_prefix_4_byte : loop;
    }
  /* Very tiny blocks are best handled via the loop, REP is expensive to
     setup.  */
  else if (expected_size != -1 && expected_size < 4)
    return loop_1_byte;
  else if (expected_size != -1)
    {
      enum stringop_alg alg = libcall;
      bool alg_noalign = false;
      for (i = 0; i < MAX_STRINGOP_ALGS; i++)
	{
	  /* We get here if the algorithms that were not libcall-based
	     were rep-prefix based and we are unable to use rep prefixes
	     based on global register usage.  Break out of the loop and
	     use the heuristic below.  */
	  if (algs->size[i].max == 0)
	    break;
	  if (algs->size[i].max >= expected_size || algs->size[i].max == -1)
	    {
	      enum stringop_alg candidate = algs->size[i].alg;

	      if (candidate != libcall
		  && alg_usable_p (candidate, memset, have_as))
		{
		  alg = candidate;
		  alg_noalign = algs->size[i].noalign;
		}
	      /* Honor TARGET_INLINE_ALL_STRINGOPS by picking
		 last non-libcall inline algorithm.  */
	      if (TARGET_INLINE_ALL_STRINGOPS)
		{
		  /* When the current size is best to be copied by a libcall,
		     but we are still forced to inline, run the heuristic below
		     that will pick code for medium sized blocks.  */
		  if (alg != libcall)
		    {
		      *noalign = alg_noalign;
		      return alg;
		    }
		  else if (!any_alg_usable_p)
		    break;
		}
	      else if (alg_usable_p (candidate, memset, have_as)
		       && !(TARGET_PREFER_KNOWN_REP_MOVSB_STOSB
			    && candidate == rep_prefix_1_byte
			    /* NB: If min_size != max_size, size is
			       unknown.  */
			    && min_size != max_size))
		{
		  *noalign = algs->size[i].noalign;
		  return candidate;
		}
	    }
	}
    }
  /* When asked to inline the call anyway, try to pick meaningful choice.
     We look for maximal size of block that is faster to copy by hand and
     take blocks of at most of that size guessing that average size will
     be roughly half of the block.

     If this turns out to be bad, we might simply specify the preferred
     choice in ix86_costs.  */
  if ((TARGET_INLINE_ALL_STRINGOPS || TARGET_INLINE_STRINGOPS_DYNAMICALLY)
      && (algs->unknown_size == libcall
	  || !alg_usable_p (algs->unknown_size, memset, have_as)))
    {
      enum stringop_alg alg;
      HOST_WIDE_INT new_expected_size = (max > 0 ? max : 4096) / 2;

      /* If there aren't any usable algorithms or if recursing already,
	 then recursing on smaller sizes or same size isn't going to
	 find anything.  Just return the simple byte-at-a-time copy loop.  */
      if (!any_alg_usable_p || recur)
	{
	  /* Pick something reasonable.  */
	  if (TARGET_INLINE_STRINGOPS_DYNAMICALLY && !recur)
	    *dynamic_check = 128;
	  return loop_1_byte;
	}
      alg = decide_alg (count, new_expected_size, min_size, max_size, memset,
			zero_memset, have_as, dynamic_check, noalign, true);
      gcc_assert (*dynamic_check == -1);
      if (TARGET_INLINE_STRINGOPS_DYNAMICALLY)
	*dynamic_check = max;
      else
	gcc_assert (alg != libcall);
      return alg;
    }

  /* Try to use some reasonable fallback algorithm.  Note that for
     non-default address spaces we default to a loop instead of
     a libcall.  */
  return (alg_usable_p (algs->unknown_size, memset, have_as)
	  ? algs->unknown_size : have_as ? loop : libcall);
}

/* Decide on alignment.  We know that the operand is already aligned to ALIGN
   (ALIGN can be based on profile feedback and thus it is not 100% guaranteed).  */
static int
decide_alignment (int align,
		  enum stringop_alg alg,
		  int expected_size,
		  machine_mode move_mode)
{
  int desired_align = 0;

  gcc_assert (alg != no_stringop);

  if (alg == libcall)
    return 0;
  if (move_mode == VOIDmode)
    return 0;

  desired_align = GET_MODE_SIZE (move_mode);
  /* PentiumPro has special logic triggering for 8 byte aligned blocks.
     copying whole cacheline at once.  */
  if (TARGET_CPU_P (PENTIUMPRO)
      && (alg == rep_prefix_4_byte || alg == rep_prefix_1_byte))
    desired_align = 8;

  if (optimize_size)
    desired_align = 1;
  if (desired_align < align)
    desired_align = align;
  if (expected_size != -1 && expected_size < 4)
    desired_align = align;

  return desired_align;
}


/* Helper function for memcpy.  For QImode value 0xXY produce
   0xXYXYXYXY of wide specified by MODE.  This is essentially
   a * 0x10101010, but we can do slightly better than
   synth_mult by unwinding the sequence by hand on CPUs with
   slow multiply.  */
static rtx
promote_duplicated_reg (machine_mode mode, rtx val)
{
  machine_mode valmode = GET_MODE (val);
  rtx tmp;
  int nops = mode == DImode ? 3 : 2;

  gcc_assert (mode == SImode || mode == DImode || val == const0_rtx);
  if (val == const0_rtx)
    return copy_to_mode_reg (mode, CONST0_RTX (mode));
  if (CONST_INT_P (val))
    {
      HOST_WIDE_INT v = INTVAL (val) & 255;

      v |= v << 8;
      v |= v << 16;
      if (mode == DImode)
        v |= (v << 16) << 16;
      return copy_to_mode_reg (mode, gen_int_mode (v, mode));
    }

  if (valmode == VOIDmode)
    valmode = QImode;
  if (valmode != QImode)
    val = gen_lowpart (QImode, val);
  if (mode == QImode)
    return val;
  if (!TARGET_PARTIAL_REG_STALL)
    nops--;
  if (ix86_cost->mult_init[mode == DImode ? 3 : 2]
      + ix86_cost->mult_bit * (mode == DImode ? 8 : 4)
      <= (ix86_cost->shift_const + ix86_cost->add) * nops
          + (COSTS_N_INSNS (TARGET_PARTIAL_REG_STALL == 0)))
    {
      rtx reg = convert_modes (mode, QImode, val, true);
      tmp = promote_duplicated_reg (mode, const1_rtx);
      return expand_simple_binop (mode, MULT, reg, tmp, NULL, 1,
				  OPTAB_DIRECT);
    }
  else
    {
      rtx reg = convert_modes (mode, QImode, val, true);

      if (!TARGET_PARTIAL_REG_STALL)
	emit_insn (gen_insv_1 (mode, reg, reg));
      else
	{
	  tmp = expand_simple_binop (mode, ASHIFT, reg, GEN_INT (8),
				     NULL, 1, OPTAB_DIRECT);
	  reg = expand_simple_binop (mode, IOR, reg, tmp, reg, 1,
				     OPTAB_DIRECT);
	}
      tmp = expand_simple_binop (mode, ASHIFT, reg, GEN_INT (16),
			         NULL, 1, OPTAB_DIRECT);
      reg = expand_simple_binop (mode, IOR, reg, tmp, reg, 1, OPTAB_DIRECT);
      if (mode == SImode)
	return reg;
      tmp = expand_simple_binop (mode, ASHIFT, reg, GEN_INT (32),
				 NULL, 1, OPTAB_DIRECT);
      reg = expand_simple_binop (mode, IOR, reg, tmp, reg, 1, OPTAB_DIRECT);
      return reg;
    }
}

/* Duplicate value VAL using promote_duplicated_reg into maximal size that will
   be needed by main loop copying SIZE_NEEDED chunks and prologue getting
   alignment from ALIGN to DESIRED_ALIGN.  */
static rtx
promote_duplicated_reg_to_size (rtx val, int size_needed, int desired_align,
				int align)
{
  rtx promoted_val;

  if (TARGET_64BIT
      && (size_needed > 4 || (desired_align > align && desired_align > 4)))
    promoted_val = promote_duplicated_reg (DImode, val);
  else if (size_needed > 2 || (desired_align > align && desired_align > 2))
    promoted_val = promote_duplicated_reg (SImode, val);
  else if (size_needed > 1 || (desired_align > align && desired_align > 1))
    promoted_val = promote_duplicated_reg (HImode, val);
  else
    promoted_val = val;

  return promoted_val;
}

/* Copy the address to a Pmode register.  This is used for x32 to
   truncate DImode TLS address to a SImode register. */

static rtx
ix86_copy_addr_to_reg (rtx addr)
{
  rtx reg;
  if (GET_MODE (addr) == Pmode || GET_MODE (addr) == VOIDmode)
    {
      reg = copy_addr_to_reg (addr);
      REG_POINTER (reg) = 1;
      return reg;
    }
  else
    {
      gcc_assert (GET_MODE (addr) == DImode && Pmode == SImode);
      reg = copy_to_mode_reg (DImode, addr);
      REG_POINTER (reg) = 1;
      return gen_rtx_SUBREG (SImode, reg, 0);
    }
}

/* Expand string move (memcpy) ot store (memset) operation.  Use i386 string
   operations when profitable.  The code depends upon architecture, block size
   and alignment, but always has one of the following overall structures:

   Aligned move sequence:

     1) Prologue guard: Conditional that jumps up to epilogues for small
	blocks that can be handled by epilogue alone.  This is faster
	but also needed for correctness, since prologue assume the block
	is larger than the desired alignment.

	Optional dynamic check for size and libcall for large
	blocks is emitted here too, with -minline-stringops-dynamically.

     2) Prologue: copy first few bytes in order to get destination
	aligned to DESIRED_ALIGN.  It is emitted only when ALIGN is less
	than DESIRED_ALIGN and up to DESIRED_ALIGN - ALIGN bytes can be
	copied.  We emit either a jump tree on power of two sized
	blocks, or a byte loop.

     3) Main body: the copying loop itself, copying in SIZE_NEEDED chunks
	with specified algorithm.

     4) Epilogue: code copying tail of the block that is too small to be
	handled by main body (or up to size guarded by prologue guard). 

  Misaligned move sequence

     1) missaligned move prologue/epilogue containing:
        a) Prologue handling small memory blocks and jumping to done_label
	   (skipped if blocks are known to be large enough)
	b) Signle move copying first DESIRED_ALIGN-ALIGN bytes if alignment is
           needed by single possibly misaligned move
	   (skipped if alignment is not needed)
        c) Copy of last SIZE_NEEDED bytes by possibly misaligned moves

     2) Zero size guard dispatching to done_label, if needed

     3) dispatch to library call, if needed,

     3) Main body: the copying loop itself, copying in SIZE_NEEDED chunks
	with specified algorithm.  */
bool
ix86_expand_set_or_cpymem (rtx dst, rtx src, rtx count_exp, rtx val_exp,
			   rtx align_exp, rtx expected_align_exp,
			   rtx expected_size_exp, rtx min_size_exp,
			   rtx max_size_exp, rtx probable_max_size_exp,
			   bool issetmem)
{
  rtx destreg;
  rtx srcreg = NULL;
  rtx_code_label *label = NULL;
  rtx tmp;
  rtx_code_label *jump_around_label = NULL;
  HOST_WIDE_INT align = 1;
  unsigned HOST_WIDE_INT count = 0;
  HOST_WIDE_INT expected_size = -1;
  int size_needed = 0, epilogue_size_needed;
  int desired_align = 0, align_bytes = 0;
  enum stringop_alg alg;
  rtx promoted_val = NULL;
  rtx vec_promoted_val = NULL;
  bool force_loopy_epilogue = false;
  int dynamic_check;
  bool need_zero_guard = false;
  bool noalign;
  machine_mode move_mode = VOIDmode;
  machine_mode wider_mode;
  int unroll_factor = 1;
  /* TODO: Once value ranges are available, fill in proper data.  */
  unsigned HOST_WIDE_INT min_size = 0;
  unsigned HOST_WIDE_INT max_size = -1;
  unsigned HOST_WIDE_INT probable_max_size = -1;
  bool misaligned_prologue_used = false;
  bool have_as;

  if (CONST_INT_P (align_exp))
    align = INTVAL (align_exp);
  /* i386 can do misaligned access on reasonably increased cost.  */
  if (CONST_INT_P (expected_align_exp)
      && INTVAL (expected_align_exp) > align)
    align = INTVAL (expected_align_exp);
  /* ALIGN is the minimum of destination and source alignment, but we care here
     just about destination alignment.  */
  else if (!issetmem
	   && MEM_ALIGN (dst) > (unsigned HOST_WIDE_INT) align * BITS_PER_UNIT)
    align = MEM_ALIGN (dst) / BITS_PER_UNIT;

  if (CONST_INT_P (count_exp))
    {
      min_size = max_size = probable_max_size = count = expected_size
	= INTVAL (count_exp);
      /* When COUNT is 0, there is nothing to do.  */
      if (!count)
	return true;
    }
  else
    {
      if (min_size_exp)
	min_size = INTVAL (min_size_exp);
      if (max_size_exp)
	max_size = INTVAL (max_size_exp);
      if (probable_max_size_exp)
	probable_max_size = INTVAL (probable_max_size_exp);
      if (CONST_INT_P (expected_size_exp))
	expected_size = INTVAL (expected_size_exp);
     }

  /* Make sure we don't need to care about overflow later on.  */
  if (count > (HOST_WIDE_INT_1U << 30))
    return false;

  have_as = !ADDR_SPACE_GENERIC_P (MEM_ADDR_SPACE (dst));
  if (!issetmem)
    have_as |= !ADDR_SPACE_GENERIC_P (MEM_ADDR_SPACE (src));

  /* Step 0: Decide on preferred algorithm, desired alignment and
     size of chunks to be copied by main loop.  */
  alg = decide_alg (count, expected_size, min_size, probable_max_size,
		    issetmem,
		    issetmem && val_exp == const0_rtx, have_as,
		    &dynamic_check, &noalign, false);

  if (dump_file)
    fprintf (dump_file, "Selected stringop expansion strategy: %s\n",
	     stringop_alg_names[alg]);

  if (alg == libcall)
    return false;
  gcc_assert (alg != no_stringop);

  /* For now vector-version of memset is generated only for memory zeroing, as
     creating of promoted vector value is very cheap in this case.  */
  if (issetmem && alg == vector_loop && val_exp != const0_rtx)
    alg = unrolled_loop;

  if (!count)
    count_exp = copy_to_mode_reg (GET_MODE (count_exp), count_exp);
  destreg = ix86_copy_addr_to_reg (XEXP (dst, 0));
  if (!issetmem)
    srcreg = ix86_copy_addr_to_reg (XEXP (src, 0));

  unroll_factor = 1;
  move_mode = word_mode;
  switch (alg)
    {
    case libcall:
    case no_stringop:
    case last_alg:
      gcc_unreachable ();
    case loop_1_byte:
      need_zero_guard = true;
      move_mode = QImode;
      break;
    case loop:
      need_zero_guard = true;
      break;
    case unrolled_loop:
      need_zero_guard = true;
      unroll_factor = (TARGET_64BIT ? 4 : 2);
      break;
    case vector_loop:
      need_zero_guard = true;
      unroll_factor = 4;
      /* Find the widest supported mode.  */
      move_mode = word_mode;
      while (GET_MODE_WIDER_MODE (move_mode).exists (&wider_mode)
	     && optab_handler (mov_optab, wider_mode) != CODE_FOR_nothing)
	move_mode = wider_mode;

      if (TARGET_AVX256_SPLIT_REGS && GET_MODE_BITSIZE (move_mode) > 128)
	move_mode = TImode;
      if (TARGET_AVX512_SPLIT_REGS && GET_MODE_BITSIZE (move_mode) > 256)
	move_mode = OImode;

      /* Find the corresponding vector mode with the same size as MOVE_MODE.
	 MOVE_MODE is an integer mode at the moment (SI, DI, TI, etc.).  */
      if (GET_MODE_SIZE (move_mode) > GET_MODE_SIZE (word_mode))
	{
	  int nunits = GET_MODE_SIZE (move_mode) / GET_MODE_SIZE (word_mode);
	  if (!mode_for_vector (word_mode, nunits).exists (&move_mode)
	      || optab_handler (mov_optab, move_mode) == CODE_FOR_nothing)
	    move_mode = word_mode;
	}
      gcc_assert (optab_handler (mov_optab, move_mode) != CODE_FOR_nothing);
      break;
    case rep_prefix_8_byte:
      move_mode = DImode;
      break;
    case rep_prefix_4_byte:
      move_mode = SImode;
      break;
    case rep_prefix_1_byte:
      move_mode = QImode;
      break;
    }
  size_needed = GET_MODE_SIZE (move_mode) * unroll_factor;
  epilogue_size_needed = size_needed;

  /* If we are going to call any library calls conditionally, make sure any
     pending stack adjustment happen before the first conditional branch,
     otherwise they will be emitted before the library call only and won't
     happen from the other branches.  */
  if (dynamic_check != -1)
    do_pending_stack_adjust ();

  desired_align = decide_alignment (align, alg, expected_size, move_mode);
  if (!TARGET_ALIGN_STRINGOPS || noalign)
    align = desired_align;

  /* Step 1: Prologue guard.  */

  /* Alignment code needs count to be in register.  */
  if (CONST_INT_P (count_exp) && desired_align > align)
    {
      if (INTVAL (count_exp) > desired_align
	  && INTVAL (count_exp) > size_needed)
	{
	  align_bytes
	    = get_mem_align_offset (dst, desired_align * BITS_PER_UNIT);
	  if (align_bytes <= 0)
	    align_bytes = 0;
	  else
	    align_bytes = desired_align - align_bytes;
	}
      if (align_bytes == 0)
	count_exp = force_reg (counter_mode (count_exp), count_exp);
    }
  gcc_assert (desired_align >= 1 && align >= 1);

  /* Misaligned move sequences handle both prologue and epilogue at once.
     Default code generation results in a smaller code for large alignments
     and also avoids redundant job when sizes are known precisely.  */
  misaligned_prologue_used
    = (TARGET_MISALIGNED_MOVE_STRING_PRO_EPILOGUES
       && MAX (desired_align, epilogue_size_needed) <= 32
       && desired_align <= epilogue_size_needed
       && ((desired_align > align && !align_bytes)
	   || (!count && epilogue_size_needed > 1)));

  /* Do the cheap promotion to allow better CSE across the
     main loop and epilogue (ie one load of the big constant in the
     front of all code.  
     For now the misaligned move sequences do not have fast path
     without broadcasting.  */
  if (issetmem && ((CONST_INT_P (val_exp) || misaligned_prologue_used)))
    {
      if (alg == vector_loop)
	{
	  gcc_assert (val_exp == const0_rtx);
	  vec_promoted_val = promote_duplicated_reg (move_mode, val_exp);
	  promoted_val = promote_duplicated_reg_to_size (val_exp,
							 GET_MODE_SIZE (word_mode),
							 desired_align, align);
	}
      else
	{
	  promoted_val = promote_duplicated_reg_to_size (val_exp, size_needed,
							 desired_align, align);
	}
    }
  /* Misaligned move sequences handles both prologues and epilogues at once.
     Default code generation results in smaller code for large alignments and
     also avoids redundant job when sizes are known precisely.  */
  if (misaligned_prologue_used)
    {
      /* Misaligned move prologue handled small blocks by itself.  */
      expand_set_or_cpymem_prologue_epilogue_by_misaligned_moves
	   (dst, src, &destreg, &srcreg,
	    move_mode, promoted_val, vec_promoted_val,
	    &count_exp,
	    &jump_around_label,
            desired_align < align
	    ? MAX (desired_align, epilogue_size_needed) : epilogue_size_needed,
	    desired_align, align, &min_size, dynamic_check, issetmem);
      if (!issetmem)
        src = change_address (src, BLKmode, srcreg);
      dst = change_address (dst, BLKmode, destreg);
      set_mem_align (dst, desired_align * BITS_PER_UNIT);
      epilogue_size_needed = 0;
      if (need_zero_guard
	  && min_size < (unsigned HOST_WIDE_INT) size_needed)
	{
	  /* It is possible that we copied enough so the main loop will not
	     execute.  */
	  gcc_assert (size_needed > 1);
	  if (jump_around_label == NULL_RTX)
	    jump_around_label = gen_label_rtx ();
	  emit_cmp_and_jump_insns (count_exp,
				   GEN_INT (size_needed),
				   LTU, 0, counter_mode (count_exp), 1, jump_around_label);
	  if (expected_size == -1
	      || expected_size < (desired_align - align) / 2 + size_needed)
	    predict_jump (REG_BR_PROB_BASE * 20 / 100);
	  else
	    predict_jump (REG_BR_PROB_BASE * 60 / 100);
	}
    }
  /* Ensure that alignment prologue won't copy past end of block.  */
  else if (size_needed > 1 || (desired_align > 1 && desired_align > align))
    {
      epilogue_size_needed = MAX (size_needed - 1, desired_align - align);
      /* Epilogue always copies COUNT_EXP & EPILOGUE_SIZE_NEEDED bytes.
	 Make sure it is power of 2.  */
      epilogue_size_needed = 1 << (floor_log2 (epilogue_size_needed) + 1);

      /* To improve performance of small blocks, we jump around the VAL
	 promoting mode.  This mean that if the promoted VAL is not constant,
	 we might not use it in the epilogue and have to use byte
	 loop variant.  */
      if (issetmem && epilogue_size_needed > 2 && !promoted_val)
	force_loopy_epilogue = true;
      if ((count && count < (unsigned HOST_WIDE_INT) epilogue_size_needed)
	  || max_size < (unsigned HOST_WIDE_INT) epilogue_size_needed)
	{
	  /* If main algorithm works on QImode, no epilogue is needed.
	     For small sizes just don't align anything.  */
	  if (size_needed == 1)
	    desired_align = align;
	  else
	    goto epilogue;
	}
      else if (!count
	       && min_size < (unsigned HOST_WIDE_INT) epilogue_size_needed)
	{
	  label = gen_label_rtx ();
	  emit_cmp_and_jump_insns (count_exp,
				   GEN_INT (epilogue_size_needed),
				   LTU, 0, counter_mode (count_exp), 1, label);
	  if (expected_size == -1 || expected_size < epilogue_size_needed)
	    predict_jump (REG_BR_PROB_BASE * 60 / 100);
	  else
	    predict_jump (REG_BR_PROB_BASE * 20 / 100);
	}
    }

  /* Emit code to decide on runtime whether library call or inline should be
     used.  */
  if (dynamic_check != -1)
    {
      if (!issetmem && CONST_INT_P (count_exp))
	{
	  if (UINTVAL (count_exp) >= (unsigned HOST_WIDE_INT)dynamic_check)
	    {
	      emit_block_copy_via_libcall (dst, src, count_exp);
	      count_exp = const0_rtx;
	      goto epilogue;
	    }
	}
      else
	{
	  rtx_code_label *hot_label = gen_label_rtx ();
	  if (jump_around_label == NULL_RTX)
	    jump_around_label = gen_label_rtx ();
	  emit_cmp_and_jump_insns (count_exp, GEN_INT (dynamic_check - 1),
				   LEU, 0, counter_mode (count_exp),
				   1, hot_label);
	  predict_jump (REG_BR_PROB_BASE * 90 / 100);
	  if (issetmem)
	    set_storage_via_libcall (dst, count_exp, val_exp);
	  else
	    emit_block_copy_via_libcall (dst, src, count_exp);
	  emit_jump (jump_around_label);
	  emit_label (hot_label);
	}
    }

  /* Step 2: Alignment prologue.  */
  /* Do the expensive promotion once we branched off the small blocks.  */
  if (issetmem && !promoted_val)
    promoted_val = promote_duplicated_reg_to_size (val_exp, size_needed,
						   desired_align, align);

  if (desired_align > align && !misaligned_prologue_used)
    {
      if (align_bytes == 0)
	{
	  /* Except for the first move in prologue, we no longer know
	     constant offset in aliasing info.  It don't seems to worth
	     the pain to maintain it for the first move, so throw away
	     the info early.  */
	  dst = change_address (dst, BLKmode, destreg);
	  if (!issetmem)
	    src = change_address (src, BLKmode, srcreg);
	  dst = expand_set_or_cpymem_prologue (dst, src, destreg, srcreg,
					    promoted_val, vec_promoted_val,
					    count_exp, align, desired_align,
					    issetmem);
	  /* At most desired_align - align bytes are copied.  */
	  if (min_size < (unsigned)(desired_align - align))
	    min_size = 0;
	  else
	    min_size -= desired_align - align;
	}
      else
	{
	  /* If we know how many bytes need to be stored before dst is
	     sufficiently aligned, maintain aliasing info accurately.  */
	  dst = expand_set_or_cpymem_constant_prologue (dst, &src, destreg,
							   srcreg,
							   promoted_val,
							   vec_promoted_val,
							   desired_align,
							   align_bytes,
							   issetmem);

	  count_exp = plus_constant (counter_mode (count_exp),
				     count_exp, -align_bytes);
	  count -= align_bytes;
	  min_size -= align_bytes;
	  max_size -= align_bytes;
	}
      if (need_zero_guard
	  && min_size < (unsigned HOST_WIDE_INT) size_needed
	  && (count < (unsigned HOST_WIDE_INT) size_needed
	      || (align_bytes == 0
		  && count < ((unsigned HOST_WIDE_INT) size_needed
			      + desired_align - align))))
	{
	  /* It is possible that we copied enough so the main loop will not
	     execute.  */
	  gcc_assert (size_needed > 1);
	  if (label == NULL_RTX)
	    label = gen_label_rtx ();
	  emit_cmp_and_jump_insns (count_exp,
				   GEN_INT (size_needed),
				   LTU, 0, counter_mode (count_exp), 1, label);
	  if (expected_size == -1
	      || expected_size < (desired_align - align) / 2 + size_needed)
	    predict_jump (REG_BR_PROB_BASE * 20 / 100);
	  else
	    predict_jump (REG_BR_PROB_BASE * 60 / 100);
	}
    }
  if (label && size_needed == 1)
    {
      emit_label (label);
      LABEL_NUSES (label) = 1;
      label = NULL;
      epilogue_size_needed = 1;
      if (issetmem)
	promoted_val = val_exp;
    }
  else if (label == NULL_RTX && !misaligned_prologue_used)
    epilogue_size_needed = size_needed;

  /* Step 3: Main loop.  */

  switch (alg)
    {
    case libcall:
    case no_stringop:
    case last_alg:
      gcc_unreachable ();
    case loop_1_byte:
    case loop:
    case unrolled_loop:
      expand_set_or_cpymem_via_loop (dst, src, destreg, srcreg, promoted_val,
				     count_exp, move_mode, unroll_factor,
				     expected_size, issetmem);
      break;
    case vector_loop:
      expand_set_or_cpymem_via_loop (dst, src, destreg, srcreg,
				     vec_promoted_val, count_exp, move_mode,
				     unroll_factor, expected_size, issetmem);
      break;
    case rep_prefix_8_byte:
    case rep_prefix_4_byte:
    case rep_prefix_1_byte:
      expand_set_or_cpymem_via_rep (dst, src, destreg, srcreg, promoted_val,
				       val_exp, count_exp, move_mode, issetmem);
      break;
    }
  /* Adjust properly the offset of src and dest memory for aliasing.  */
  if (CONST_INT_P (count_exp))
    {
      if (!issetmem)
	src = adjust_automodify_address_nv (src, BLKmode, srcreg,
					    (count / size_needed) * size_needed);
      dst = adjust_automodify_address_nv (dst, BLKmode, destreg,
					  (count / size_needed) * size_needed);
    }
  else
    {
      if (!issetmem)
	src = change_address (src, BLKmode, srcreg);
      dst = change_address (dst, BLKmode, destreg);
    }

  /* Step 4: Epilogue to copy the remaining bytes.  */
 epilogue:
  if (label)
    {
      /* When the main loop is done, COUNT_EXP might hold original count,
	 while we want to copy only COUNT_EXP & SIZE_NEEDED bytes.
	 Epilogue code will actually copy COUNT_EXP & EPILOGUE_SIZE_NEEDED
	 bytes. Compensate if needed.  */

      if (size_needed < epilogue_size_needed)
	{
	  tmp = expand_simple_binop (counter_mode (count_exp), AND, count_exp,
				     GEN_INT (size_needed - 1), count_exp, 1,
				     OPTAB_DIRECT);
	  if (tmp != count_exp)
	    emit_move_insn (count_exp, tmp);
	}
      emit_label (label);
      LABEL_NUSES (label) = 1;
    }

  if (count_exp != const0_rtx && epilogue_size_needed > 1)
    {
      if (force_loopy_epilogue)
	expand_setmem_epilogue_via_loop (dst, destreg, val_exp, count_exp,
					 epilogue_size_needed);
      else
	{
	  if (issetmem)
	    expand_setmem_epilogue (dst, destreg, promoted_val,
				    vec_promoted_val, count_exp,
				    epilogue_size_needed);
	  else
	    expand_cpymem_epilogue (dst, src, destreg, srcreg, count_exp,
				    epilogue_size_needed);
	}
    }
  if (jump_around_label)
    emit_label (jump_around_label);
  return true;
}

/* Expand cmpstrn or memcmp.  */

bool
ix86_expand_cmpstrn_or_cmpmem (rtx result, rtx src1, rtx src2,
			       rtx length, rtx align, bool is_cmpstrn)
{
  /* Expand strncmp and memcmp only with -minline-all-stringops since
     "repz cmpsb" can be much slower than strncmp and memcmp functions
     implemented with vector instructions, see

     https://gcc.gnu.org/bugzilla/show_bug.cgi?id=43052
   */
  if (!TARGET_INLINE_ALL_STRINGOPS)
    return false;

  /* Can't use this if the user has appropriated ecx, esi or edi.  */
  if (fixed_regs[CX_REG] || fixed_regs[SI_REG] || fixed_regs[DI_REG])
    return false;

  if (is_cmpstrn)
    {
      /* For strncmp, length is the maximum length, which can be larger
	 than actual string lengths.  We can expand the cmpstrn pattern
	 to "repz cmpsb" only if one of the strings is a constant so
	 that expand_builtin_strncmp() can write the length argument to
	 be the minimum of the const string length and the actual length
	 argument.  Otherwise, "repz cmpsb" may pass the 0 byte.  */
      tree t1 = MEM_EXPR (src1);
      tree t2 = MEM_EXPR (src2);
      if (!((t1 && TREE_CODE (t1) == MEM_REF
	     && TREE_CODE (TREE_OPERAND (t1, 0)) == ADDR_EXPR
	     && (TREE_CODE (TREE_OPERAND (TREE_OPERAND (t1, 0), 0))
		 == STRING_CST))
	    || (t2 && TREE_CODE (t2) == MEM_REF
		&& TREE_CODE (TREE_OPERAND (t2, 0)) == ADDR_EXPR
		&& (TREE_CODE (TREE_OPERAND (TREE_OPERAND (t2, 0), 0))
		    == STRING_CST))))
	return false;
    }

  rtx addr1 = copy_addr_to_reg (XEXP (src1, 0));
  rtx addr2 = copy_addr_to_reg (XEXP (src2, 0));
  if (addr1 != XEXP (src1, 0))
    src1 = replace_equiv_address_nv (src1, addr1);
  if (addr2 != XEXP (src2, 0))
    src2 = replace_equiv_address_nv (src2, addr2);

  /* NB: Make a copy of the data length to avoid changing the original
     data length by cmpstrnqi patterns.  */
  length = ix86_zero_extend_to_Pmode (length);
  rtx lengthreg = gen_reg_rtx (Pmode);
  emit_move_insn (lengthreg, length);

  /* If we are testing strict equality, we can use known alignment to
     good advantage.  This may be possible with combine, particularly
     once cc0 is dead.  */
  if (CONST_INT_P (length))
    {
      if (length == const0_rtx)
	{
	  emit_move_insn (result, const0_rtx);
	  return true;
	}
      emit_insn (gen_cmpstrnqi_nz_1 (addr1, addr2, lengthreg, align,
				     src1, src2));
    }
  else
    {
      emit_insn (gen_cmp_1 (Pmode, lengthreg, lengthreg));
      emit_insn (gen_cmpstrnqi_1 (addr1, addr2, lengthreg, align,
				  src1, src2));
    }

  rtx out = gen_lowpart (QImode, result);
  emit_insn (gen_cmpintqi (out));
  emit_move_insn (result, gen_rtx_SIGN_EXTEND (SImode, out));

  return true;
}

/* Expand the appropriate insns for doing strlen if not just doing
   repnz; scasb

   out = result, initialized with the start address
   align_rtx = alignment of the address.
   scratch = scratch register, initialized with the startaddress when
	not aligned, otherwise undefined

   This is just the body. It needs the initializations mentioned above and
   some address computing at the end.  These things are done in i386.md.  */

static void
ix86_expand_strlensi_unroll_1 (rtx out, rtx src, rtx align_rtx)
{
  int align;
  rtx tmp;
  rtx_code_label *align_2_label = NULL;
  rtx_code_label *align_3_label = NULL;
  rtx_code_label *align_4_label = gen_label_rtx ();
  rtx_code_label *end_0_label = gen_label_rtx ();
  rtx mem;
  rtx tmpreg = gen_reg_rtx (SImode);
  rtx scratch = gen_reg_rtx (SImode);
  rtx cmp;

  align = 0;
  if (CONST_INT_P (align_rtx))
    align = INTVAL (align_rtx);

  /* Loop to check 1..3 bytes for null to get an aligned pointer.  */

  /* Is there a known alignment and is it less than 4?  */
  if (align < 4)
    {
      rtx scratch1 = gen_reg_rtx (Pmode);
      emit_move_insn (scratch1, out);
      /* Is there a known alignment and is it not 2? */
      if (align != 2)
	{
	  align_3_label = gen_label_rtx (); /* Label when aligned to 3-byte */
	  align_2_label = gen_label_rtx (); /* Label when aligned to 2-byte */

	  /* Leave just the 3 lower bits.  */
	  align_rtx = expand_binop (Pmode, and_optab, scratch1, GEN_INT (3),
				    NULL_RTX, 0, OPTAB_WIDEN);

	  emit_cmp_and_jump_insns (align_rtx, const0_rtx, EQ, NULL,
				   Pmode, 1, align_4_label);
	  emit_cmp_and_jump_insns (align_rtx, const2_rtx, EQ, NULL,
				   Pmode, 1, align_2_label);
	  emit_cmp_and_jump_insns (align_rtx, const2_rtx, GTU, NULL,
				   Pmode, 1, align_3_label);
	}
      else
        {
	  /* Since the alignment is 2, we have to check 2 or 0 bytes;
	     check if is aligned to 4 - byte.  */

	  align_rtx = expand_binop (Pmode, and_optab, scratch1, const2_rtx,
				    NULL_RTX, 0, OPTAB_WIDEN);

	  emit_cmp_and_jump_insns (align_rtx, const0_rtx, EQ, NULL,
				   Pmode, 1, align_4_label);
        }

      mem = change_address (src, QImode, out);

      /* Now compare the bytes.  */

      /* Compare the first n unaligned byte on a byte per byte basis.  */
      emit_cmp_and_jump_insns (mem, const0_rtx, EQ, NULL,
			       QImode, 1, end_0_label);

      /* Increment the address.  */
      emit_insn (gen_add2_insn (out, const1_rtx));

      /* Not needed with an alignment of 2 */
      if (align != 2)
	{
	  emit_label (align_2_label);

	  emit_cmp_and_jump_insns (mem, const0_rtx, EQ, NULL, QImode, 1,
				   end_0_label);

	  emit_insn (gen_add2_insn (out, const1_rtx));

	  emit_label (align_3_label);
	}

      emit_cmp_and_jump_insns (mem, const0_rtx, EQ, NULL, QImode, 1,
			       end_0_label);

      emit_insn (gen_add2_insn (out, const1_rtx));
    }

  /* Generate loop to check 4 bytes at a time.  It is not a good idea to
     align this loop.  It gives only huge programs, but does not help to
     speed up.  */
  emit_label (align_4_label);

  mem = change_address (src, SImode, out);
  emit_move_insn (scratch, mem);
  emit_insn (gen_add2_insn (out, GEN_INT (4)));

  /* This formula yields a nonzero result iff one of the bytes is zero.
     This saves three branches inside loop and many cycles.  */

  emit_insn (gen_addsi3 (tmpreg, scratch, GEN_INT (-0x01010101)));
  emit_insn (gen_one_cmplsi2 (scratch, scratch));
  emit_insn (gen_andsi3 (tmpreg, tmpreg, scratch));
  emit_insn (gen_andsi3 (tmpreg, tmpreg,
			 gen_int_mode (0x80808080, SImode)));
  emit_cmp_and_jump_insns (tmpreg, const0_rtx, EQ, 0, SImode, 1,
			   align_4_label);

  if (TARGET_CMOVE)
    {
       rtx reg = gen_reg_rtx (SImode);
       rtx reg2 = gen_reg_rtx (Pmode);
       emit_move_insn (reg, tmpreg);
       emit_insn (gen_lshrsi3 (reg, reg, GEN_INT (16)));

       /* If zero is not in the first two bytes, move two bytes forward.  */
       emit_insn (gen_testsi_ccno_1 (tmpreg, GEN_INT (0x8080)));
       tmp = gen_rtx_REG (CCNOmode, FLAGS_REG);
       tmp = gen_rtx_EQ (VOIDmode, tmp, const0_rtx);
       emit_insn (gen_rtx_SET (tmpreg,
			       gen_rtx_IF_THEN_ELSE (SImode, tmp,
						     reg,
						     tmpreg)));
       /* Emit lea manually to avoid clobbering of flags.  */
       emit_insn (gen_rtx_SET (reg2, plus_constant (Pmode, out, 2)));

       tmp = gen_rtx_REG (CCNOmode, FLAGS_REG);
       tmp = gen_rtx_EQ (VOIDmode, tmp, const0_rtx);
       emit_insn (gen_rtx_SET (out,
			       gen_rtx_IF_THEN_ELSE (Pmode, tmp,
						     reg2,
						     out)));
    }
  else
    {
       rtx_code_label *end_2_label = gen_label_rtx ();
       /* Is zero in the first two bytes? */

       emit_insn (gen_testsi_ccno_1 (tmpreg, GEN_INT (0x8080)));
       tmp = gen_rtx_REG (CCNOmode, FLAGS_REG);
       tmp = gen_rtx_NE (VOIDmode, tmp, const0_rtx);
       tmp = gen_rtx_IF_THEN_ELSE (VOIDmode, tmp,
                            gen_rtx_LABEL_REF (VOIDmode, end_2_label),
                            pc_rtx);
       tmp = emit_jump_insn (gen_rtx_SET (pc_rtx, tmp));
       JUMP_LABEL (tmp) = end_2_label;

       /* Not in the first two.  Move two bytes forward.  */
       emit_insn (gen_lshrsi3 (tmpreg, tmpreg, GEN_INT (16)));
       emit_insn (gen_add2_insn (out, const2_rtx));

       emit_label (end_2_label);

    }

  /* Avoid branch in fixing the byte.  */
  tmpreg = gen_lowpart (QImode, tmpreg);
  emit_insn (gen_addqi3_cconly_overflow (tmpreg, tmpreg));
  tmp = gen_rtx_REG (CCmode, FLAGS_REG);
  cmp = gen_rtx_LTU (VOIDmode, tmp, const0_rtx);
  emit_insn (gen_sub3_carry (Pmode, out, out, GEN_INT (3), tmp, cmp));

  emit_label (end_0_label);
}

/* Expand strlen.  */

bool
ix86_expand_strlen (rtx out, rtx src, rtx eoschar, rtx align)
{
if (TARGET_UNROLL_STRLEN
	   && TARGET_INLINE_ALL_STRINGOPS
	   && eoschar == const0_rtx
	   && optimize > 1)
    {
      /* The generic case of strlen expander is long.  Avoid it's
	 expanding unless TARGET_INLINE_ALL_STRINGOPS.  */
      rtx addr = force_reg (Pmode, XEXP (src, 0));
      /* Well it seems that some optimizer does not combine a call like
	 foo(strlen(bar), strlen(bar));
	 when the move and the subtraction is done here.  It does calculate
	 the length just once when these instructions are done inside of
	 output_strlen_unroll().  But I think since &bar[strlen(bar)] is
	 often used and I use one fewer register for the lifetime of
	 output_strlen_unroll() this is better.  */

      emit_move_insn (out, addr);

      ix86_expand_strlensi_unroll_1 (out, src, align);

      /* strlensi_unroll_1 returns the address of the zero at the end of
	 the string, like memchr(), so compute the length by subtracting
	 the start address.  */
      emit_insn (gen_sub2_insn (out, addr));
      return true;
    }
  else
    return false;
}

/* For given symbol (function) construct code to compute address of it's PLT
   entry in large x86-64 PIC model.  */

static rtx
construct_plt_address (rtx symbol)
{
  rtx tmp, unspec;

  gcc_assert (GET_CODE (symbol) == SYMBOL_REF);
  gcc_assert (ix86_cmodel == CM_LARGE_PIC && !TARGET_PECOFF);
  gcc_assert (Pmode == DImode);

  tmp = gen_reg_rtx (Pmode);
  unspec = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, symbol), UNSPEC_PLTOFF);

  emit_move_insn (tmp, gen_rtx_CONST (Pmode, unspec));
  emit_insn (gen_add2_insn (tmp, pic_offset_table_rtx));
  return tmp;
}

/* Additional registers that are clobbered by SYSV calls.  */

static int const x86_64_ms_sysv_extra_clobbered_registers
		 [NUM_X86_64_MS_CLOBBERED_REGS] =
{
  SI_REG, DI_REG,
  XMM6_REG, XMM7_REG,
  XMM8_REG, XMM9_REG, XMM10_REG, XMM11_REG,
  XMM12_REG, XMM13_REG, XMM14_REG, XMM15_REG
};

rtx_insn *
ix86_expand_call (rtx retval, rtx fnaddr, rtx callarg1,
		  rtx callarg2,
		  rtx pop, bool sibcall)
{
  rtx vec[3];
  rtx use = NULL, call;
  unsigned int vec_len = 0;
  tree fndecl;
  bool call_no_callee_saved_registers = false;

  if (GET_CODE (XEXP (fnaddr, 0)) == SYMBOL_REF)
    {
      fndecl = SYMBOL_REF_DECL (XEXP (fnaddr, 0));
      if (fndecl)
	{
	  if (lookup_attribute ("interrupt",
				TYPE_ATTRIBUTES (TREE_TYPE (fndecl))))
	    error ("interrupt service routine cannot be called directly");
	  else if (lookup_attribute ("no_callee_saved_registers",
				     TYPE_ATTRIBUTES (TREE_TYPE (fndecl))))
	    call_no_callee_saved_registers = true;
	}
    }
  else
    {
      if (MEM_P (fnaddr))
	{
	  tree mem_expr = MEM_EXPR (fnaddr);
	  if (mem_expr != nullptr
	      && TREE_CODE (mem_expr) == MEM_REF
	      && lookup_attribute ("no_callee_saved_registers",
				   TYPE_ATTRIBUTES (TREE_TYPE (mem_expr))))
	    call_no_callee_saved_registers = true;
	}

      fndecl = NULL_TREE;
    }

  if (pop == const0_rtx)
    pop = NULL;
  gcc_assert (!TARGET_64BIT || !pop);

  rtx addr = XEXP (fnaddr, 0);
  if (TARGET_MACHO && !TARGET_64BIT)
    {
#if TARGET_MACHO
      if (flag_pic && GET_CODE (XEXP (fnaddr, 0)) == SYMBOL_REF)
	fnaddr = machopic_indirect_call_target (fnaddr);
#endif
    }
  else
    {
      /* Static functions and indirect calls don't need the pic register.  Also,
	 check if PLT was explicitly avoided via no-plt or "noplt" attribute, making
	 it an indirect call.  */
      if (flag_pic
	  && GET_CODE (addr) == SYMBOL_REF
	  && ix86_call_use_plt_p (addr))
	{
	  if (flag_plt
	      && (SYMBOL_REF_DECL (addr) == NULL_TREE
		  || !lookup_attribute ("noplt",
					DECL_ATTRIBUTES (SYMBOL_REF_DECL (addr)))))
	    {
	      if (!TARGET_64BIT
		  || (ix86_cmodel == CM_LARGE_PIC
		      && DEFAULT_ABI != MS_ABI))
		{
		  use_reg (&use, gen_rtx_REG (Pmode,
					      REAL_PIC_OFFSET_TABLE_REGNUM));
		  if (ix86_use_pseudo_pic_reg ())
		    emit_move_insn (gen_rtx_REG (Pmode,
						 REAL_PIC_OFFSET_TABLE_REGNUM),
				    pic_offset_table_rtx);
		}
	    }
	  else if (!TARGET_PECOFF && !TARGET_MACHO)
	    {
	      if (TARGET_64BIT
		  && ix86_cmodel == CM_LARGE_PIC
		  && DEFAULT_ABI != MS_ABI)
		{
		  fnaddr = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr),
					   UNSPEC_GOT);
		  fnaddr = gen_rtx_CONST (Pmode, fnaddr);
		  fnaddr = force_reg (Pmode, fnaddr);
		  fnaddr = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, fnaddr);
		}
	      else if (TARGET_64BIT)
		{
		  fnaddr = gen_rtx_UNSPEC (Pmode,
					   gen_rtvec (1, addr),
					   UNSPEC_GOTPCREL);
		  fnaddr = gen_rtx_CONST (Pmode, fnaddr);
		}
	      else
		{
		  fnaddr = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr),
					   UNSPEC_GOT);
		  fnaddr = gen_rtx_CONST (Pmode, fnaddr);
		  fnaddr = gen_rtx_PLUS (Pmode, pic_offset_table_rtx,
					 fnaddr);
		}
	      fnaddr = gen_const_mem (Pmode, fnaddr);
	      /* Pmode may not be the same as word_mode for x32, which
		 doesn't support indirect branch via 32-bit memory slot.
		 Since x32 GOT slot is 64 bit with zero upper 32 bits,
		 indirect branch via x32 GOT slot is OK.  */
	      if (GET_MODE (fnaddr) != word_mode)
		fnaddr = gen_rtx_ZERO_EXTEND (word_mode, fnaddr);
	      fnaddr = gen_rtx_MEM (QImode, fnaddr);
	    }
	}
    }

  /* Skip setting up RAX register for -mskip-rax-setup when there are no
     parameters passed in vector registers.  */
  if (TARGET_64BIT
      && (INTVAL (callarg2) > 0
	  || (INTVAL (callarg2) == 0
	      && (TARGET_SSE || !flag_skip_rax_setup))))
    {
      rtx al = gen_rtx_REG (QImode, AX_REG);
      emit_move_insn (al, callarg2);
      use_reg (&use, al);
    }

  if (ix86_cmodel == CM_LARGE_PIC
      && !TARGET_PECOFF
      && MEM_P (fnaddr)
      && GET_CODE (XEXP (fnaddr, 0)) == SYMBOL_REF
      && !local_symbolic_operand (XEXP (fnaddr, 0), VOIDmode))
    fnaddr = gen_rtx_MEM (QImode, construct_plt_address (XEXP (fnaddr, 0)));
  /* Since x32 GOT slot is 64 bit with zero upper 32 bits, indirect
     branch via x32 GOT slot is OK.  */
  else if (!(TARGET_X32
	     && MEM_P (fnaddr)
	     && GET_CODE (XEXP (fnaddr, 0)) == ZERO_EXTEND
	     && GOT_memory_operand (XEXP (XEXP (fnaddr, 0), 0), Pmode))
	   && (sibcall
	       ? !sibcall_insn_operand (XEXP (fnaddr, 0), word_mode)
	       : !call_insn_operand (XEXP (fnaddr, 0), word_mode)))
    {
      fnaddr = convert_to_mode (word_mode, XEXP (fnaddr, 0), 1);
      fnaddr = gen_rtx_MEM (QImode, copy_to_mode_reg (word_mode, fnaddr));
    }

  /* PR100665: Hwasan may tag code pointer which is not supported by LAM,
     mask off code pointers here.
     TODO: also need to handle indirect jump.  */
  if (ix86_memtag_can_tag_addresses () && !fndecl
      && sanitize_flags_p (SANITIZE_HWADDRESS))
    {
      rtx untagged_addr = ix86_memtag_untagged_pointer (XEXP (fnaddr, 0),
							NULL_RTX);
      fnaddr = gen_rtx_MEM (QImode, untagged_addr);
    }

  call = gen_rtx_CALL (VOIDmode, fnaddr, callarg1);

  if (retval)
    call = gen_rtx_SET (retval, call);
  vec[vec_len++] = call;

  if (pop)
    {
      pop = gen_rtx_PLUS (Pmode, stack_pointer_rtx, pop);
      pop = gen_rtx_SET (stack_pointer_rtx, pop);
      vec[vec_len++] = pop;
    }

  static const char ix86_call_used_regs[] = CALL_USED_REGISTERS;

  if ((cfun->machine->call_saved_registers
       == TYPE_NO_CALLER_SAVED_REGISTERS)
      && (!fndecl
	  || (!TREE_THIS_VOLATILE (fndecl)
	      && !lookup_attribute ("no_caller_saved_registers",
				    TYPE_ATTRIBUTES (TREE_TYPE (fndecl))))))
    {
      bool is_64bit_ms_abi = (TARGET_64BIT
			      && ix86_function_abi (fndecl) == MS_ABI);
      char c_mask = CALL_USED_REGISTERS_MASK (is_64bit_ms_abi);

      /* If there are no caller-saved registers, add all registers
	 that are clobbered by the call which returns.  */
      for (int i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (!fixed_regs[i]
	    && (ix86_call_used_regs[i] == 1
		|| (ix86_call_used_regs[i] & c_mask))
	    && !STACK_REGNO_P (i)
	    && !MMX_REGNO_P (i))
	  clobber_reg (&use,
		       gen_rtx_REG (GET_MODE (regno_reg_rtx[i]), i));
    }
  else if (TARGET_64BIT_MS_ABI
	   && (!callarg2 || INTVAL (callarg2) != -2))
    {
      unsigned i;

      for (i = 0; i < NUM_X86_64_MS_CLOBBERED_REGS; i++)
	{
	  int regno = x86_64_ms_sysv_extra_clobbered_registers[i];
	  machine_mode mode = SSE_REGNO_P (regno) ? TImode : DImode;

	  clobber_reg (&use, gen_rtx_REG (mode, regno));
	}

      /* Set here, but it may get cleared later.  */
      if (TARGET_CALL_MS2SYSV_XLOGUES)
	{
	  if (!TARGET_SSE)
	    ;

	  /* Don't break hot-patched functions.  */
	  else if (ix86_function_ms_hook_prologue (current_function_decl))
	    ;

	  /* TODO: Cases not yet examined.  */
	  else if (flag_split_stack)
	    warn_once_call_ms2sysv_xlogues ("-fsplit-stack");

	  else
	    {
	      gcc_assert (!reload_completed);
	      cfun->machine->call_ms2sysv = true;
	    }
	}
    }

  if (TARGET_MACHO && TARGET_64BIT && !sibcall
      && ((GET_CODE (addr) == SYMBOL_REF && !SYMBOL_REF_LOCAL_P (addr))
	  || !fndecl || TREE_PUBLIC (fndecl)))
    {
      /* We allow public functions defined in a TU to bind locally for PIC
	 code (the default) on 64bit Mach-O.
	 If such functions are not inlined, we cannot tell at compile-time if
	 they will be called via the lazy symbol resolver (this can depend on
	 options given at link-time).  Therefore, we must assume that the lazy
	 resolver could be used which clobbers R11 and R10.  */
      clobber_reg (&use, gen_rtx_REG (DImode, R11_REG));
      clobber_reg (&use, gen_rtx_REG (DImode, R10_REG));
    }

  if (call_no_callee_saved_registers)
    {
      /* After calling a no_callee_saved_registers function, all
	 registers may be clobbered.  Clobber all registers that are
	 not used by the callee.  */
      bool is_64bit_ms_abi = (TARGET_64BIT
			      && ix86_function_abi (fndecl) == MS_ABI);
      char c_mask = CALL_USED_REGISTERS_MASK (is_64bit_ms_abi);
      for (int i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (!fixed_regs[i]
	    && !(ix86_call_used_regs[i] == 1
		 || (ix86_call_used_regs[i] & c_mask))
	    && !STACK_REGNO_P (i)
	    && !MMX_REGNO_P (i))
	  clobber_reg (&use,
		       gen_rtx_REG (GET_MODE (regno_reg_rtx[i]), i));
    }

  if (vec_len > 1)
    call = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (vec_len, vec));
  rtx_insn *call_insn = emit_call_insn (call);
  if (use)
    CALL_INSN_FUNCTION_USAGE (call_insn) = use;

  return call_insn;
}

/* Split simple return with popping POPC bytes from stack to indirect
   branch with stack adjustment .  */

void
ix86_split_simple_return_pop_internal (rtx popc)
{
  struct machine_function *m = cfun->machine;
  rtx ecx = gen_rtx_REG (SImode, CX_REG);
  rtx_insn *insn;

  /* There is no "pascal" calling convention in any 64bit ABI.  */
  gcc_assert (!TARGET_64BIT);

  insn = emit_insn (gen_pop (ecx));
  m->fs.cfa_offset -= UNITS_PER_WORD;
  m->fs.sp_offset -= UNITS_PER_WORD;

  rtx x = plus_constant (Pmode, stack_pointer_rtx, UNITS_PER_WORD);
  x = gen_rtx_SET (stack_pointer_rtx, x);
  add_reg_note (insn, REG_CFA_ADJUST_CFA, x);
  add_reg_note (insn, REG_CFA_REGISTER, gen_rtx_SET (ecx, pc_rtx));
  RTX_FRAME_RELATED_P (insn) = 1;

  x = gen_rtx_PLUS (Pmode, stack_pointer_rtx, popc);
  x = gen_rtx_SET (stack_pointer_rtx, x);
  insn = emit_insn (x);
  add_reg_note (insn, REG_CFA_ADJUST_CFA, x);
  RTX_FRAME_RELATED_P (insn) = 1;

  /* Now return address is in ECX.  */
  emit_jump_insn (gen_simple_return_indirect_internal (ecx));
}

/* Errors in the source file can cause expand_expr to return const0_rtx
   where we expect a vector.  To avoid crashing, use one of the vector
   clear instructions.  */

static rtx
safe_vector_operand (rtx x, machine_mode mode)
{
  if (x == const0_rtx)
    x = CONST0_RTX (mode);
  return x;
}

/* Subroutine of ix86_expand_builtin to take care of binop insns.  */

static rtx
ix86_expand_binop_builtin (enum insn_code icode, tree exp, rtx target)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  machine_mode tmode = insn_data[icode].operand[0].mode;
  machine_mode mode0 = insn_data[icode].operand[1].mode;
  machine_mode mode1 = insn_data[icode].operand[2].mode;

  if (VECTOR_MODE_P (mode0))
    op0 = safe_vector_operand (op0, mode0);
  if (VECTOR_MODE_P (mode1))
    op1 = safe_vector_operand (op1, mode1);

  if (optimize || !target
      || GET_MODE (target) != tmode
      || !insn_data[icode].operand[0].predicate (target, tmode))
    target = gen_reg_rtx (tmode);

  if (GET_MODE (op1) == SImode && mode1 == TImode)
    {
      rtx x = gen_reg_rtx (V4SImode);
      emit_insn (gen_sse2_loadd (x, op1));
      op1 = gen_lowpart (TImode, x);
    }

  if (!insn_data[icode].operand[1].predicate (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if (!insn_data[icode].operand[2].predicate (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);

  pat = GEN_FCN (icode) (target, op0, op1);
  if (! pat)
    return 0;

  emit_insn (pat);

  return target;
}

/* Subroutine of ix86_expand_builtin to take care of 2-4 argument insns.  */

static rtx
ix86_expand_multi_arg_builtin (enum insn_code icode, tree exp, rtx target,
			       enum ix86_builtin_func_type m_type,
			       enum rtx_code sub_code)
{
  rtx pat;
  unsigned int i, nargs;
  bool comparison_p = false;
  bool tf_p = false;
  bool last_arg_constant = false;
  int num_memory = 0;
  rtx xops[4];

  machine_mode tmode = insn_data[icode].operand[0].mode;

  switch (m_type)
    {
    case MULTI_ARG_4_DF2_DI_I:
    case MULTI_ARG_4_DF2_DI_I1:
    case MULTI_ARG_4_SF2_SI_I:
    case MULTI_ARG_4_SF2_SI_I1:
      nargs = 4;
      last_arg_constant = true;
      break;

    case MULTI_ARG_3_SF:
    case MULTI_ARG_3_DF:
    case MULTI_ARG_3_SF2:
    case MULTI_ARG_3_DF2:
    case MULTI_ARG_3_DI:
    case MULTI_ARG_3_SI:
    case MULTI_ARG_3_SI_DI:
    case MULTI_ARG_3_HI:
    case MULTI_ARG_3_HI_SI:
    case MULTI_ARG_3_QI:
    case MULTI_ARG_3_DI2:
    case MULTI_ARG_3_SI2:
    case MULTI_ARG_3_HI2:
    case MULTI_ARG_3_QI2:
      nargs = 3;
      break;

    case MULTI_ARG_2_SF:
    case MULTI_ARG_2_DF:
    case MULTI_ARG_2_DI:
    case MULTI_ARG_2_SI:
    case MULTI_ARG_2_HI:
    case MULTI_ARG_2_QI:
      nargs = 2;
      break;

    case MULTI_ARG_2_DI_IMM:
    case MULTI_ARG_2_SI_IMM:
    case MULTI_ARG_2_HI_IMM:
    case MULTI_ARG_2_QI_IMM:
      nargs = 2;
      last_arg_constant = true;
      break;

    case MULTI_ARG_1_SF:
    case MULTI_ARG_1_DF:
    case MULTI_ARG_1_SF2:
    case MULTI_ARG_1_DF2:
    case MULTI_ARG_1_DI:
    case MULTI_ARG_1_SI:
    case MULTI_ARG_1_HI:
    case MULTI_ARG_1_QI:
    case MULTI_ARG_1_SI_DI:
    case MULTI_ARG_1_HI_DI:
    case MULTI_ARG_1_HI_SI:
    case MULTI_ARG_1_QI_DI:
    case MULTI_ARG_1_QI_SI:
    case MULTI_ARG_1_QI_HI:
      nargs = 1;
      break;

    case MULTI_ARG_2_DI_CMP:
    case MULTI_ARG_2_SI_CMP:
    case MULTI_ARG_2_HI_CMP:
    case MULTI_ARG_2_QI_CMP:
      nargs = 2;
      comparison_p = true;
      break;

    case MULTI_ARG_2_SF_TF:
    case MULTI_ARG_2_DF_TF:
    case MULTI_ARG_2_DI_TF:
    case MULTI_ARG_2_SI_TF:
    case MULTI_ARG_2_HI_TF:
    case MULTI_ARG_2_QI_TF:
      nargs = 2;
      tf_p = true;
      break;

    default:
      gcc_unreachable ();
    }

  if (optimize || !target
      || GET_MODE (target) != tmode
      || !insn_data[icode].operand[0].predicate (target, tmode))
    target = gen_reg_rtx (tmode);
  else if (memory_operand (target, tmode))
    num_memory++;

  gcc_assert (nargs <= ARRAY_SIZE (xops));

  for (i = 0; i < nargs; i++)
    {
      tree arg = CALL_EXPR_ARG (exp, i);
      rtx op = expand_normal (arg);
      int adjust = (comparison_p) ? 1 : 0;
      machine_mode mode = insn_data[icode].operand[i+adjust+1].mode;

      if (last_arg_constant && i == nargs - 1)
	{
	  if (!insn_data[icode].operand[i + 1].predicate (op, mode))
	    {
	      enum insn_code new_icode = icode;
	      switch (icode)
		{
		case CODE_FOR_xop_vpermil2v2df3:
		case CODE_FOR_xop_vpermil2v4sf3:
		case CODE_FOR_xop_vpermil2v4df3:
		case CODE_FOR_xop_vpermil2v8sf3:
		  error ("the last argument must be a 2-bit immediate");
		  return gen_reg_rtx (tmode);
		case CODE_FOR_xop_rotlv2di3:
		  new_icode = CODE_FOR_rotlv2di3;
		  goto xop_rotl;
		case CODE_FOR_xop_rotlv4si3:
		  new_icode = CODE_FOR_rotlv4si3;
		  goto xop_rotl;
		case CODE_FOR_xop_rotlv8hi3:
		  new_icode = CODE_FOR_rotlv8hi3;
		  goto xop_rotl;
		case CODE_FOR_xop_rotlv16qi3:
		  new_icode = CODE_FOR_rotlv16qi3;
		xop_rotl:
		  if (CONST_INT_P (op))
		    {
		      int mask = GET_MODE_UNIT_BITSIZE (tmode) - 1;
		      op = GEN_INT (INTVAL (op) & mask);
		      gcc_checking_assert
			(insn_data[icode].operand[i + 1].predicate (op, mode));
		    }
		  else
		    {
		      gcc_checking_assert
			(nargs == 2
			 && insn_data[new_icode].operand[0].mode == tmode
			 && insn_data[new_icode].operand[1].mode == tmode
			 && insn_data[new_icode].operand[2].mode == mode
			 && insn_data[new_icode].operand[0].predicate
			    == insn_data[icode].operand[0].predicate
			 && insn_data[new_icode].operand[1].predicate
			    == insn_data[icode].operand[1].predicate);
		      icode = new_icode;
		      goto non_constant;
		    }
		  break;
		default:
		  gcc_unreachable ();
		}
	    }
	}
      else
	{
	non_constant:
	  if (VECTOR_MODE_P (mode))
	    op = safe_vector_operand (op, mode);

	  /* If we aren't optimizing, only allow one memory operand to be
	     generated.  */
	  if (memory_operand (op, mode))
	    num_memory++;

	  gcc_assert (GET_MODE (op) == mode || GET_MODE (op) == VOIDmode);

	  if (optimize
	      || !insn_data[icode].operand[i+adjust+1].predicate (op, mode)
	      || num_memory > 1)
	    op = force_reg (mode, op);
	}

      xops[i] = op;
    }

  switch (nargs)
    {
    case 1:
      pat = GEN_FCN (icode) (target, xops[0]);
      break;

    case 2:
      if (tf_p)
	pat = GEN_FCN (icode) (target, xops[0], xops[1],
			       GEN_INT ((int)sub_code));
      else if (! comparison_p)
	pat = GEN_FCN (icode) (target, xops[0], xops[1]);
      else
	{
	  rtx cmp_op = gen_rtx_fmt_ee (sub_code, GET_MODE (target),
				       xops[0], xops[1]);

	  pat = GEN_FCN (icode) (target, cmp_op, xops[0], xops[1]);
	}
      break;

    case 3:
      pat = GEN_FCN (icode) (target, xops[0], xops[1], xops[2]);
      break;

    case 4:
      pat = GEN_FCN (icode) (target, xops[0], xops[1], xops[2], xops[3]);
      break;

    default:
      gcc_unreachable ();
    }

  if (! pat)
    return 0;

  emit_insn (pat);
  return target;
}

/* Subroutine of ix86_expand_args_builtin to take care of scalar unop
   insns with vec_merge.  */

static rtx
ix86_expand_unop_vec_merge_builtin (enum insn_code icode, tree exp,
				    rtx target)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  rtx op1, op0 = expand_normal (arg0);
  machine_mode tmode = insn_data[icode].operand[0].mode;
  machine_mode mode0 = insn_data[icode].operand[1].mode;

  if (optimize || !target
      || GET_MODE (target) != tmode
      || !insn_data[icode].operand[0].predicate (target, tmode))
    target = gen_reg_rtx (tmode);

  if (VECTOR_MODE_P (mode0))
    op0 = safe_vector_operand (op0, mode0);

  if ((optimize && !register_operand (op0, mode0))
      || !insn_data[icode].operand[1].predicate (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);

  op1 = op0;
  if (!insn_data[icode].operand[2].predicate (op1, mode0))
    op1 = copy_to_mode_reg (mode0, op1);

  pat = GEN_FCN (icode) (target, op0, op1);
  if (! pat)
    return 0;
  emit_insn (pat);
  return target;
}

/* Subroutine of ix86_expand_builtin to take care of comparison insns.  */

static rtx
ix86_expand_sse_compare (const struct builtin_description *d,
			 tree exp, rtx target, bool swap)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  rtx op2;
  machine_mode tmode = insn_data[d->icode].operand[0].mode;
  machine_mode mode0 = insn_data[d->icode].operand[1].mode;
  machine_mode mode1 = insn_data[d->icode].operand[2].mode;
  enum rtx_code comparison = d->comparison;

  if (VECTOR_MODE_P (mode0))
    op0 = safe_vector_operand (op0, mode0);
  if (VECTOR_MODE_P (mode1))
    op1 = safe_vector_operand (op1, mode1);

  /* Swap operands if we have a comparison that isn't available in
     hardware.  */
  if (swap)
    std::swap (op0, op1);

  if (optimize || !target
      || GET_MODE (target) != tmode
      || !insn_data[d->icode].operand[0].predicate (target, tmode))
    target = gen_reg_rtx (tmode);

  if ((optimize && !register_operand (op0, mode0))
      || !insn_data[d->icode].operand[1].predicate (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if ((optimize && !register_operand (op1, mode1))
      || !insn_data[d->icode].operand[2].predicate (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);

  op2 = gen_rtx_fmt_ee (comparison, mode0, op0, op1);
  pat = GEN_FCN (d->icode) (target, op0, op1, op2);
  if (! pat)
    return 0;
  emit_insn (pat);
  return target;
}

/* Subroutine of ix86_sse_comi and ix86_sse_comi_round to take care of
 * ordered EQ or unordered NE, generate PF jump.  */

static rtx
ix86_ssecom_setcc (const enum rtx_code comparison,
		   bool check_unordered, machine_mode mode,
		   rtx set_dst, rtx target)
{

  rtx_code_label *label = NULL;

  /* NB: For ordered EQ or unordered NE, check ZF alone isn't sufficient
     with NAN operands.  */
  if (check_unordered)
    {
      gcc_assert (comparison == EQ || comparison == NE);

      rtx flag = gen_rtx_REG (CCFPmode, FLAGS_REG);
      label = gen_label_rtx ();
      rtx tmp = gen_rtx_fmt_ee (UNORDERED, VOIDmode, flag, const0_rtx);
      tmp = gen_rtx_IF_THEN_ELSE (VOIDmode, tmp,
				  gen_rtx_LABEL_REF (VOIDmode, label),
				  pc_rtx);
      emit_jump_insn (gen_rtx_SET (pc_rtx, tmp));
    }

  /* NB: Set CCFPmode and check a different CCmode which is in subset
     of CCFPmode.  */
  if (GET_MODE (set_dst) != mode)
    {
      gcc_assert (mode == CCAmode || mode == CCCmode
		  || mode == CCOmode || mode == CCPmode
		  || mode == CCSmode || mode == CCZmode);
      set_dst = gen_rtx_REG (mode, FLAGS_REG);
    }

  emit_insn (gen_rtx_SET (gen_rtx_STRICT_LOW_PART (VOIDmode, target),
			  gen_rtx_fmt_ee (comparison, QImode,
					  set_dst,
					  const0_rtx)));

  if (label)
    emit_label (label);

  return SUBREG_REG (target);
}

/* Subroutine of ix86_expand_builtin to take care of comi insns.  */

static rtx
ix86_expand_sse_comi (const struct builtin_description *d, tree exp,
		      rtx target)
{
  rtx pat, set_dst;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  enum insn_code icode = d->icode;
  const struct insn_data_d *insn_p = &insn_data[icode];
  machine_mode mode0 = insn_p->operand[0].mode;
  machine_mode mode1 = insn_p->operand[1].mode;

  if (VECTOR_MODE_P (mode0))
    op0 = safe_vector_operand (op0, mode0);
  if (VECTOR_MODE_P (mode1))
    op1 = safe_vector_operand (op1, mode1);

  enum rtx_code comparison = d->comparison;
  rtx const_val = const0_rtx;

  bool check_unordered = false;
  machine_mode mode = CCFPmode;
  switch (comparison)
    {
    case LE:	/* -> GE  */
    case LT:	/* -> GT  */
      std::swap (op0, op1);
      comparison = swap_condition (comparison);
      /* FALLTHRU */
    case GT:
    case GE:
      break;
    case EQ:
      check_unordered = true;
      mode = CCZmode;
      break;
    case NE:
      check_unordered = true;
      mode = CCZmode;
      const_val = const1_rtx;
      break;
    default:
      gcc_unreachable ();
    }

  target = gen_reg_rtx (SImode);
  emit_move_insn (target, const_val);
  target = gen_rtx_SUBREG (QImode, target, 0);

  if ((optimize && !register_operand (op0, mode0))
      || !insn_p->operand[0].predicate (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if ((optimize && !register_operand (op1, mode1))
      || !insn_p->operand[1].predicate (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);

  pat = GEN_FCN (icode) (op0, op1);
  if (! pat)
    return 0;

  set_dst = SET_DEST (pat);
  emit_insn (pat);
  return ix86_ssecom_setcc (comparison, check_unordered, mode,
			    set_dst, target);
}

/* Subroutines of ix86_expand_args_builtin to take care of round insns.  */

static rtx
ix86_expand_sse_round (const struct builtin_description *d, tree exp,
		       rtx target)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  rtx op1, op0 = expand_normal (arg0);
  machine_mode tmode = insn_data[d->icode].operand[0].mode;
  machine_mode mode0 = insn_data[d->icode].operand[1].mode;

  if (optimize || target == 0
      || GET_MODE (target) != tmode
      || !insn_data[d->icode].operand[0].predicate (target, tmode))
    target = gen_reg_rtx (tmode);

  if (VECTOR_MODE_P (mode0))
    op0 = safe_vector_operand (op0, mode0);

  if ((optimize && !register_operand (op0, mode0))
      || !insn_data[d->icode].operand[0].predicate (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);

  op1 = GEN_INT (d->comparison);

  pat = GEN_FCN (d->icode) (target, op0, op1);
  if (! pat)
    return 0;
  emit_insn (pat);
  return target;
}

static rtx
ix86_expand_sse_round_vec_pack_sfix (const struct builtin_description *d,
				     tree exp, rtx target)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  rtx op2;
  machine_mode tmode = insn_data[d->icode].operand[0].mode;
  machine_mode mode0 = insn_data[d->icode].operand[1].mode;
  machine_mode mode1 = insn_data[d->icode].operand[2].mode;

  if (optimize || target == 0
      || GET_MODE (target) != tmode
      || !insn_data[d->icode].operand[0].predicate (target, tmode))
    target = gen_reg_rtx (tmode);

  op0 = safe_vector_operand (op0, mode0);
  op1 = safe_vector_operand (op1, mode1);

  if ((optimize && !register_operand (op0, mode0))
      || !insn_data[d->icode].operand[0].predicate (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if ((optimize && !register_operand (op1, mode1))
      || !insn_data[d->icode].operand[1].predicate (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);

  op2 = GEN_INT (d->comparison);

  pat = GEN_FCN (d->icode) (target, op0, op1, op2);
  if (! pat)
    return 0;
  emit_insn (pat);
  return target;
}

/* Subroutine of ix86_expand_builtin to take care of ptest insns.  */

static rtx
ix86_expand_sse_ptest (const struct builtin_description *d, tree exp,
		       rtx target)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  machine_mode mode0 = insn_data[d->icode].operand[0].mode;
  machine_mode mode1 = insn_data[d->icode].operand[1].mode;
  enum rtx_code comparison = d->comparison;

  /* ptest reg, reg sets the carry flag.  */
  if (comparison == LTU
      && (d->code == IX86_BUILTIN_PTESTC
	  || d->code == IX86_BUILTIN_PTESTC256)
      && rtx_equal_p (op0, op1))
    {
      if (!target)
	target = gen_reg_rtx (SImode);
      emit_move_insn (target, const1_rtx);
      return target;
    }

  if (VECTOR_MODE_P (mode0))
    op0 = safe_vector_operand (op0, mode0);
  if (VECTOR_MODE_P (mode1))
    op1 = safe_vector_operand (op1, mode1);

  target = gen_reg_rtx (SImode);
  emit_move_insn (target, const0_rtx);
  target = gen_rtx_SUBREG (QImode, target, 0);

  if ((optimize && !register_operand (op0, mode0))
      || !insn_data[d->icode].operand[0].predicate (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if ((optimize && !register_operand (op1, mode1))
      || !insn_data[d->icode].operand[1].predicate (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);

  pat = GEN_FCN (d->icode) (op0, op1);
  if (! pat)
    return 0;
  emit_insn (pat);
  emit_insn (gen_rtx_SET (gen_rtx_STRICT_LOW_PART (VOIDmode, target),
			  gen_rtx_fmt_ee (comparison, QImode,
					  SET_DEST (pat),
					  const0_rtx)));

  return SUBREG_REG (target);
}

/* Subroutine of ix86_expand_builtin to take care of pcmpestr[im] insns.  */

static rtx
ix86_expand_sse_pcmpestr (const struct builtin_description *d,
			  tree exp, rtx target)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  tree arg2 = CALL_EXPR_ARG (exp, 2);
  tree arg3 = CALL_EXPR_ARG (exp, 3);
  tree arg4 = CALL_EXPR_ARG (exp, 4);
  rtx scratch0, scratch1;
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  rtx op2 = expand_normal (arg2);
  rtx op3 = expand_normal (arg3);
  rtx op4 = expand_normal (arg4);
  machine_mode tmode0, tmode1, modev2, modei3, modev4, modei5, modeimm;

  tmode0 = insn_data[d->icode].operand[0].mode;
  tmode1 = insn_data[d->icode].operand[1].mode;
  modev2 = insn_data[d->icode].operand[2].mode;
  modei3 = insn_data[d->icode].operand[3].mode;
  modev4 = insn_data[d->icode].operand[4].mode;
  modei5 = insn_data[d->icode].operand[5].mode;
  modeimm = insn_data[d->icode].operand[6].mode;

  if (VECTOR_MODE_P (modev2))
    op0 = safe_vector_operand (op0, modev2);
  if (VECTOR_MODE_P (modev4))
    op2 = safe_vector_operand (op2, modev4);

  if (!insn_data[d->icode].operand[2].predicate (op0, modev2))
    op0 = copy_to_mode_reg (modev2, op0);
  if (!insn_data[d->icode].operand[3].predicate (op1, modei3))
    op1 = copy_to_mode_reg (modei3, op1);
  if ((optimize && !register_operand (op2, modev4))
      || !insn_data[d->icode].operand[4].predicate (op2, modev4))
    op2 = copy_to_mode_reg (modev4, op2);
  if (!insn_data[d->icode].operand[5].predicate (op3, modei5))
    op3 = copy_to_mode_reg (modei5, op3);

  if (!insn_data[d->icode].operand[6].predicate (op4, modeimm))
    {
      error ("the fifth argument must be an 8-bit immediate");
      return const0_rtx;
    }

  if (d->code == IX86_BUILTIN_PCMPESTRI128)
    {
      if (optimize || !target
	  || GET_MODE (target) != tmode0
	  || !insn_data[d->icode].operand[0].predicate (target, tmode0))
	target = gen_reg_rtx (tmode0);

      scratch1 = gen_reg_rtx (tmode1);

      pat = GEN_FCN (d->icode) (target, scratch1, op0, op1, op2, op3, op4);
    }
  else if (d->code == IX86_BUILTIN_PCMPESTRM128)
    {
      if (optimize || !target
	  || GET_MODE (target) != tmode1
	  || !insn_data[d->icode].operand[1].predicate (target, tmode1))
	target = gen_reg_rtx (tmode1);

      scratch0 = gen_reg_rtx (tmode0);

      pat = GEN_FCN (d->icode) (scratch0, target, op0, op1, op2, op3, op4);
    }
  else
    {
      gcc_assert (d->flag);

      scratch0 = gen_reg_rtx (tmode0);
      scratch1 = gen_reg_rtx (tmode1);

      pat = GEN_FCN (d->icode) (scratch0, scratch1, op0, op1, op2, op3, op4);
    }

  if (! pat)
    return 0;

  emit_insn (pat);

  if (d->flag)
    {
      target = gen_reg_rtx (SImode);
      emit_move_insn (target, const0_rtx);
      target = gen_rtx_SUBREG (QImode, target, 0);

      emit_insn
	(gen_rtx_SET (gen_rtx_STRICT_LOW_PART (VOIDmode, target),
		      gen_rtx_fmt_ee (EQ, QImode,
				      gen_rtx_REG ((machine_mode) d->flag,
						   FLAGS_REG),
				      const0_rtx)));
      return SUBREG_REG (target);
    }
  else
    return target;
}


/* Subroutine of ix86_expand_builtin to take care of pcmpistr[im] insns.  */

static rtx
ix86_expand_sse_pcmpistr (const struct builtin_description *d,
			  tree exp, rtx target)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  tree arg2 = CALL_EXPR_ARG (exp, 2);
  rtx scratch0, scratch1;
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  rtx op2 = expand_normal (arg2);
  machine_mode tmode0, tmode1, modev2, modev3, modeimm;

  tmode0 = insn_data[d->icode].operand[0].mode;
  tmode1 = insn_data[d->icode].operand[1].mode;
  modev2 = insn_data[d->icode].operand[2].mode;
  modev3 = insn_data[d->icode].operand[3].mode;
  modeimm = insn_data[d->icode].operand[4].mode;

  if (VECTOR_MODE_P (modev2))
    op0 = safe_vector_operand (op0, modev2);
  if (VECTOR_MODE_P (modev3))
    op1 = safe_vector_operand (op1, modev3);

  if (!insn_data[d->icode].operand[2].predicate (op0, modev2))
    op0 = copy_to_mode_reg (modev2, op0);
  if ((optimize && !register_operand (op1, modev3))
      || !insn_data[d->icode].operand[3].predicate (op1, modev3))
    op1 = copy_to_mode_reg (modev3, op1);

  if (!insn_data[d->icode].operand[4].predicate (op2, modeimm))
    {
      error ("the third argument must be an 8-bit immediate");
      return const0_rtx;
    }

  if (d->code == IX86_BUILTIN_PCMPISTRI128)
    {
      if (optimize || !target
	  || GET_MODE (target) != tmode0
	  || !insn_data[d->icode].operand[0].predicate (target, tmode0))
	target = gen_reg_rtx (tmode0);

      scratch1 = gen_reg_rtx (tmode1);

      pat = GEN_FCN (d->icode) (target, scratch1, op0, op1, op2);
    }
  else if (d->code == IX86_BUILTIN_PCMPISTRM128)
    {
      if (optimize || !target
	  || GET_MODE (target) != tmode1
	  || !insn_data[d->icode].operand[1].predicate (target, tmode1))
	target = gen_reg_rtx (tmode1);

      scratch0 = gen_reg_rtx (tmode0);

      pat = GEN_FCN (d->icode) (scratch0, target, op0, op1, op2);
    }
  else
    {
      gcc_assert (d->flag);

      scratch0 = gen_reg_rtx (tmode0);
      scratch1 = gen_reg_rtx (tmode1);

      pat = GEN_FCN (d->icode) (scratch0, scratch1, op0, op1, op2);
    }

  if (! pat)
    return 0;

  emit_insn (pat);

  if (d->flag)
    {
      target = gen_reg_rtx (SImode);
      emit_move_insn (target, const0_rtx);
      target = gen_rtx_SUBREG (QImode, target, 0);

      emit_insn
	(gen_rtx_SET (gen_rtx_STRICT_LOW_PART (VOIDmode, target),
		      gen_rtx_fmt_ee (EQ, QImode,
				      gen_rtx_REG ((machine_mode) d->flag,
						   FLAGS_REG),
				      const0_rtx)));
      return SUBREG_REG (target);
    }
  else
    return target;
}

/* Fixup modeless constants to fit required mode.  */

static rtx
fixup_modeless_constant (rtx x, machine_mode mode)
{
  if (GET_MODE (x) == VOIDmode)
    x = convert_to_mode (mode, x, 1);
  return x;
}

/* Subroutine of ix86_expand_builtin to take care of insns with
   variable number of operands.  */

static rtx
ix86_expand_args_builtin (const struct builtin_description *d,
			  tree exp, rtx target)
{
  rtx pat, real_target;
  unsigned int i, nargs;
  unsigned int nargs_constant = 0;
  unsigned int mask_pos = 0;
  int num_memory = 0;
  rtx xops[6];
  bool second_arg_count = false;
  enum insn_code icode = d->icode;
  const struct insn_data_d *insn_p = &insn_data[icode];
  machine_mode tmode = insn_p->operand[0].mode;
  machine_mode rmode = VOIDmode;
  bool swap = false;
  enum rtx_code comparison = d->comparison;

  switch ((enum ix86_builtin_func_type) d->flag)
    {
    case V2DF_FTYPE_V2DF_ROUND:
    case V4DF_FTYPE_V4DF_ROUND:
    case V8DF_FTYPE_V8DF_ROUND:
    case V4SF_FTYPE_V4SF_ROUND:
    case V8SF_FTYPE_V8SF_ROUND:
    case V16SF_FTYPE_V16SF_ROUND:
    case V8HF_FTYPE_V8HF_ROUND:
    case V16HF_FTYPE_V16HF_ROUND:
    case V32HF_FTYPE_V32HF_ROUND:
    case V4SI_FTYPE_V4SF_ROUND:
    case V8SI_FTYPE_V8SF_ROUND:
    case V16SI_FTYPE_V16SF_ROUND:
      return ix86_expand_sse_round (d, exp, target);
    case V4SI_FTYPE_V2DF_V2DF_ROUND:
    case V8SI_FTYPE_V4DF_V4DF_ROUND:
    case V16SI_FTYPE_V8DF_V8DF_ROUND:
      return ix86_expand_sse_round_vec_pack_sfix (d, exp, target);
    case INT_FTYPE_V8SF_V8SF_PTEST:
    case INT_FTYPE_V4DI_V4DI_PTEST:
    case INT_FTYPE_V4DF_V4DF_PTEST:
    case INT_FTYPE_V4SF_V4SF_PTEST:
    case INT_FTYPE_V2DI_V2DI_PTEST:
    case INT_FTYPE_V2DF_V2DF_PTEST:
      return ix86_expand_sse_ptest (d, exp, target);
    case FLOAT128_FTYPE_FLOAT128:
    case FLOAT_FTYPE_FLOAT:
    case FLOAT_FTYPE_BFLOAT16:
    case INT_FTYPE_INT:
    case UINT_FTYPE_UINT:
    case UINT16_FTYPE_UINT16:
    case UINT64_FTYPE_INT:
    case UINT64_FTYPE_UINT64:
    case INT64_FTYPE_INT64:
    case INT64_FTYPE_V4SF:
    case INT64_FTYPE_V2DF:
    case INT_FTYPE_V16QI:
    case INT_FTYPE_V8QI:
    case INT_FTYPE_V8SF:
    case INT_FTYPE_V4DF:
    case INT_FTYPE_V4SF:
    case INT_FTYPE_V2DF:
    case INT_FTYPE_V32QI:
    case V16QI_FTYPE_V16QI:
    case V8SI_FTYPE_V8SF:
    case V8SI_FTYPE_V4SI:
    case V8HI_FTYPE_V8HI:
    case V8HI_FTYPE_V16QI:
    case V8QI_FTYPE_V8QI:
    case V8SF_FTYPE_V8SF:
    case V8SF_FTYPE_V8SI:
    case V8SF_FTYPE_V4SF:
    case V8SF_FTYPE_V8HI:
    case V4SI_FTYPE_V4SI:
    case V4SI_FTYPE_V16QI:
    case V4SI_FTYPE_V4SF:
    case V4SI_FTYPE_V8SI:
    case V4SI_FTYPE_V8HI:
    case V4SI_FTYPE_V4DF:
    case V4SI_FTYPE_V2DF:
    case V4HI_FTYPE_V4HI:
    case V4DF_FTYPE_V4DF:
    case V4DF_FTYPE_V4SI:
    case V4DF_FTYPE_V4SF:
    case V4DF_FTYPE_V2DF:
    case V4SF_FTYPE_V4SF:
    case V4SF_FTYPE_V4SI:
    case V4SF_FTYPE_V8SF:
    case V4SF_FTYPE_V4DF:
    case V4SF_FTYPE_V8HI:
    case V4SF_FTYPE_V2DF:
    case V2DI_FTYPE_V2DI:
    case V2DI_FTYPE_V16QI:
    case V2DI_FTYPE_V8HI:
    case V2DI_FTYPE_V4SI:
    case V2DF_FTYPE_V2DF:
    case V2DF_FTYPE_V4SI:
    case V2DF_FTYPE_V4DF:
    case V2DF_FTYPE_V4SF:
    case V2DF_FTYPE_V2SI:
    case V2SI_FTYPE_V2SI:
    case V2SI_FTYPE_V4SF:
    case V2SI_FTYPE_V2SF:
    case V2SI_FTYPE_V2DF:
    case V2SF_FTYPE_V2SF:
    case V2SF_FTYPE_V2SI:
    case V32QI_FTYPE_V32QI:
    case V32QI_FTYPE_V16QI:
    case V16HI_FTYPE_V16HI:
    case V16HI_FTYPE_V8HI:
    case V8SI_FTYPE_V8SI:
    case V16HI_FTYPE_V16QI:
    case V8SI_FTYPE_V16QI:
    case V4DI_FTYPE_V16QI:
    case V8SI_FTYPE_V8HI:
    case V4DI_FTYPE_V8HI:
    case V4DI_FTYPE_V4SI:
    case V4DI_FTYPE_V2DI:
    case UQI_FTYPE_UQI:
    case UHI_FTYPE_UHI:
    case USI_FTYPE_USI:
    case USI_FTYPE_UQI:
    case USI_FTYPE_UHI:
    case UDI_FTYPE_UDI:
    case UHI_FTYPE_V16QI:
    case USI_FTYPE_V32QI:
    case UDI_FTYPE_V64QI:
    case V16QI_FTYPE_UHI:
    case V32QI_FTYPE_USI:
    case V64QI_FTYPE_UDI:
    case V8HI_FTYPE_UQI:
    case V16HI_FTYPE_UHI:
    case V32HI_FTYPE_USI:
    case V4SI_FTYPE_UQI:
    case V8SI_FTYPE_UQI:
    case V4SI_FTYPE_UHI:
    case V8SI_FTYPE_UHI:
    case UQI_FTYPE_V8HI:
    case UHI_FTYPE_V16HI:
    case USI_FTYPE_V32HI:
    case UQI_FTYPE_V4SI:
    case UQI_FTYPE_V8SI:
    case UHI_FTYPE_V16SI:
    case UQI_FTYPE_V2DI:
    case UQI_FTYPE_V4DI:
    case UQI_FTYPE_V8DI:
    case V16SI_FTYPE_UHI:
    case V2DI_FTYPE_UQI:
    case V4DI_FTYPE_UQI:
    case V16SI_FTYPE_INT:
    case V16SF_FTYPE_V8SF:
    case V16SI_FTYPE_V8SI:
    case V16SF_FTYPE_V4SF:
    case V16SI_FTYPE_V4SI:
    case V16SI_FTYPE_V16SF:
    case V16SI_FTYPE_V16SI:
    case V64QI_FTYPE_V64QI:
    case V32HI_FTYPE_V32HI:
    case V16SF_FTYPE_V16SF:
    case V8DI_FTYPE_UQI:
    case V8DI_FTYPE_V8DI:
    case V8DF_FTYPE_V4DF:
    case V8DF_FTYPE_V2DF:
    case V8DF_FTYPE_V8DF:
    case V4DI_FTYPE_V4DI:
    case V16BF_FTYPE_V16SF:
    case V8BF_FTYPE_V8SF:
    case V8BF_FTYPE_V4SF:
      nargs = 1;
      break;
    case V4SF_FTYPE_V4SF_VEC_MERGE:
    case V2DF_FTYPE_V2DF_VEC_MERGE:
      return ix86_expand_unop_vec_merge_builtin (icode, exp, target);
    case FLOAT128_FTYPE_FLOAT128_FLOAT128:
    case V16QI_FTYPE_V16QI_V16QI:
    case V16QI_FTYPE_V8HI_V8HI:
    case V16HF_FTYPE_V16HF_V16HF:
    case V16SF_FTYPE_V16SF_V16SF:
    case V8QI_FTYPE_V8QI_V8QI:
    case V8QI_FTYPE_V4HI_V4HI:
    case V8HI_FTYPE_V8HI_V8HI:
    case V8HI_FTYPE_V16QI_V16QI:
    case V8HI_FTYPE_V4SI_V4SI:
    case V8HF_FTYPE_V8HF_V8HF:
    case V8SF_FTYPE_V8SF_V8SF:
    case V8SF_FTYPE_V8SF_V8SI:
    case V8DF_FTYPE_V8DF_V8DF:
    case V4SI_FTYPE_V4SI_V4SI:
    case V4SI_FTYPE_V8HI_V8HI:
    case V4SI_FTYPE_V2DF_V2DF:
    case V4HI_FTYPE_V4HI_V4HI:
    case V4HI_FTYPE_V8QI_V8QI:
    case V4HI_FTYPE_V2SI_V2SI:
    case V4DF_FTYPE_V4DF_V4DF:
    case V4DF_FTYPE_V4DF_V4DI:
    case V4SF_FTYPE_V4SF_V4SF:
    case V4SF_FTYPE_V4SF_V4SI:
    case V4SF_FTYPE_V4SF_V2SI:
    case V4SF_FTYPE_V4SF_V2DF:
    case V4SF_FTYPE_V4SF_UINT:
    case V4SF_FTYPE_V4SF_DI:
    case V4SF_FTYPE_V4SF_SI:
    case V4DI_FTYPE_V4DI_V2DI:
    case V2DI_FTYPE_V2DI_V2DI:
    case V2DI_FTYPE_V16QI_V16QI:
    case V2DI_FTYPE_V4SI_V4SI:
    case V2DI_FTYPE_V2DI_V16QI:
    case V2SI_FTYPE_V2SI_V2SI:
    case V2SI_FTYPE_V4HI_V4HI:
    case V2SI_FTYPE_V2SF_V2SF:
    case V2DF_FTYPE_V2DF_V2DF:
    case V2DF_FTYPE_V2DF_V4SF:
    case V2DF_FTYPE_V2DF_V2DI:
    case V2DF_FTYPE_V2DF_DI:
    case V2DF_FTYPE_V2DF_SI:
    case V2DF_FTYPE_V2DF_UINT:
    case V2SF_FTYPE_V2SF_V2SF:
    case V1DI_FTYPE_V1DI_V1DI:
    case V1DI_FTYPE_V8QI_V8QI:
    case V1DI_FTYPE_V2SI_V2SI:
    case V32QI_FTYPE_V16HI_V16HI:
    case V16HI_FTYPE_V8SI_V8SI:
    case V64QI_FTYPE_V64QI_V64QI:
    case V32QI_FTYPE_V32QI_V32QI:
    case V16HI_FTYPE_V32QI_V32QI:
    case V16HI_FTYPE_V16HI_V16HI:
    case V8SI_FTYPE_V4DF_V4DF:
    case V8SI_FTYPE_V8SI_V8SI:
    case V8SI_FTYPE_V16HI_V16HI:
    case V4DI_FTYPE_V4DI_V4DI:
    case V4DI_FTYPE_V8SI_V8SI:
    case V4DI_FTYPE_V32QI_V32QI:
    case V8DI_FTYPE_V64QI_V64QI:
      if (comparison == UNKNOWN)
	return ix86_expand_binop_builtin (icode, exp, target);
      nargs = 2;
      break;
    case V4SF_FTYPE_V4SF_V4SF_SWAP:
    case V2DF_FTYPE_V2DF_V2DF_SWAP:
      gcc_assert (comparison != UNKNOWN);
      nargs = 2;
      swap = true;
      break;
    case V16HI_FTYPE_V16HI_V8HI_COUNT:
    case V16HI_FTYPE_V16HI_SI_COUNT:
    case V8SI_FTYPE_V8SI_V4SI_COUNT:
    case V8SI_FTYPE_V8SI_SI_COUNT:
    case V4DI_FTYPE_V4DI_V2DI_COUNT:
    case V4DI_FTYPE_V4DI_INT_COUNT:
    case V8HI_FTYPE_V8HI_V8HI_COUNT:
    case V8HI_FTYPE_V8HI_SI_COUNT:
    case V4SI_FTYPE_V4SI_V4SI_COUNT:
    case V4SI_FTYPE_V4SI_SI_COUNT:
    case V4HI_FTYPE_V4HI_V4HI_COUNT:
    case V4HI_FTYPE_V4HI_SI_COUNT:
    case V2DI_FTYPE_V2DI_V2DI_COUNT:
    case V2DI_FTYPE_V2DI_SI_COUNT:
    case V2SI_FTYPE_V2SI_V2SI_COUNT:
    case V2SI_FTYPE_V2SI_SI_COUNT:
    case V1DI_FTYPE_V1DI_V1DI_COUNT:
    case V1DI_FTYPE_V1DI_SI_COUNT:
      nargs = 2;
      second_arg_count = true;
      break;
    case V16HI_FTYPE_V16HI_INT_V16HI_UHI_COUNT:
    case V16HI_FTYPE_V16HI_V8HI_V16HI_UHI_COUNT:
    case V16SI_FTYPE_V16SI_INT_V16SI_UHI_COUNT:
    case V16SI_FTYPE_V16SI_V4SI_V16SI_UHI_COUNT:
    case V2DI_FTYPE_V2DI_INT_V2DI_UQI_COUNT:
    case V2DI_FTYPE_V2DI_V2DI_V2DI_UQI_COUNT:
    case V32HI_FTYPE_V32HI_INT_V32HI_USI_COUNT:
    case V32HI_FTYPE_V32HI_V8HI_V32HI_USI_COUNT:
    case V4DI_FTYPE_V4DI_INT_V4DI_UQI_COUNT:
    case V4DI_FTYPE_V4DI_V2DI_V4DI_UQI_COUNT:
    case V4SI_FTYPE_V4SI_INT_V4SI_UQI_COUNT:
    case V4SI_FTYPE_V4SI_V4SI_V4SI_UQI_COUNT:
    case V8DI_FTYPE_V8DI_INT_V8DI_UQI_COUNT:
    case V8DI_FTYPE_V8DI_V2DI_V8DI_UQI_COUNT:
    case V8HI_FTYPE_V8HI_INT_V8HI_UQI_COUNT:
    case V8HI_FTYPE_V8HI_V8HI_V8HI_UQI_COUNT:
    case V8SI_FTYPE_V8SI_INT_V8SI_UQI_COUNT:
    case V8SI_FTYPE_V8SI_V4SI_V8SI_UQI_COUNT:
      nargs = 4;
      second_arg_count = true;
      break;
    case UINT64_FTYPE_UINT64_UINT64:
    case UINT_FTYPE_UINT_UINT:
    case UINT_FTYPE_UINT_USHORT:
    case UINT_FTYPE_UINT_UCHAR:
    case UINT16_FTYPE_UINT16_INT:
    case UINT8_FTYPE_UINT8_INT:
    case UQI_FTYPE_UQI_UQI:
    case UHI_FTYPE_UHI_UHI:
    case USI_FTYPE_USI_USI:
    case UDI_FTYPE_UDI_UDI:
    case V16SI_FTYPE_V8DF_V8DF:
    case V32BF_FTYPE_V16SF_V16SF:
    case V16BF_FTYPE_V8SF_V8SF:
    case V8BF_FTYPE_V4SF_V4SF:
    case V16BF_FTYPE_V16SF_UHI:
    case V8BF_FTYPE_V8SF_UQI:
    case V8BF_FTYPE_V4SF_UQI:
      nargs = 2;
      break;
    case V2DI_FTYPE_V2DI_INT_CONVERT:
      nargs = 2;
      rmode = V1TImode;
      nargs_constant = 1;
      break;
    case V4DI_FTYPE_V4DI_INT_CONVERT:
      nargs = 2;
      rmode = V2TImode;
      nargs_constant = 1;
      break;
    case V8DI_FTYPE_V8DI_INT_CONVERT:
      nargs = 2;
      rmode = V4TImode;
      nargs_constant = 1;
      break;
    case V8HI_FTYPE_V8HI_INT:
    case V8HI_FTYPE_V8SF_INT:
    case V16HI_FTYPE_V16SF_INT:
    case V8HI_FTYPE_V4SF_INT:
    case V8SF_FTYPE_V8SF_INT:
    case V4SF_FTYPE_V16SF_INT:
    case V16SF_FTYPE_V16SF_INT:
    case V4SI_FTYPE_V4SI_INT:
    case V4SI_FTYPE_V8SI_INT:
    case V4HI_FTYPE_V4HI_INT:
    case V4DF_FTYPE_V4DF_INT:
    case V4DF_FTYPE_V8DF_INT:
    case V4SF_FTYPE_V4SF_INT:
    case V4SF_FTYPE_V8SF_INT:
    case V2DI_FTYPE_V2DI_INT:
    case V2DF_FTYPE_V2DF_INT:
    case V2DF_FTYPE_V4DF_INT:
    case V16HI_FTYPE_V16HI_INT:
    case V8SI_FTYPE_V8SI_INT:
    case V16SI_FTYPE_V16SI_INT:
    case V4SI_FTYPE_V16SI_INT:
    case V4DI_FTYPE_V4DI_INT:
    case V2DI_FTYPE_V4DI_INT:
    case V4DI_FTYPE_V8DI_INT:
    case UQI_FTYPE_UQI_UQI_CONST:
    case UHI_FTYPE_UHI_UQI:
    case USI_FTYPE_USI_UQI:
    case UDI_FTYPE_UDI_UQI:
      nargs = 2;
      nargs_constant = 1;
      break;
    case V16QI_FTYPE_V16QI_V16QI_V16QI:
    case V8SF_FTYPE_V8SF_V8SF_V8SF:
    case V4DF_FTYPE_V4DF_V4DF_V4DF:
    case V4SF_FTYPE_V4SF_V4SF_V4SF:
    case V2DF_FTYPE_V2DF_V2DF_V2DF:
    case V32QI_FTYPE_V32QI_V32QI_V32QI:
    case UHI_FTYPE_V16SI_V16SI_UHI:
    case UQI_FTYPE_V8DI_V8DI_UQI:
    case V16HI_FTYPE_V16SI_V16HI_UHI:
    case V16QI_FTYPE_V16SI_V16QI_UHI:
    case V16QI_FTYPE_V8DI_V16QI_UQI:
    case V32HF_FTYPE_V32HF_V32HF_USI:
    case V16SF_FTYPE_V16SF_V16SF_UHI:
    case V16SF_FTYPE_V4SF_V16SF_UHI:
    case V16SI_FTYPE_SI_V16SI_UHI:
    case V16SI_FTYPE_V16HI_V16SI_UHI:
    case V16SI_FTYPE_V16QI_V16SI_UHI:
    case V8SF_FTYPE_V4SF_V8SF_UQI:
    case V4DF_FTYPE_V2DF_V4DF_UQI:
    case V8SI_FTYPE_V4SI_V8SI_UQI:
    case V8SI_FTYPE_SI_V8SI_UQI:
    case V4SI_FTYPE_V4SI_V4SI_UQI:
    case V4SI_FTYPE_SI_V4SI_UQI:
    case V4DI_FTYPE_V2DI_V4DI_UQI:
    case V4DI_FTYPE_DI_V4DI_UQI:
    case V2DI_FTYPE_V2DI_V2DI_UQI:
    case V2DI_FTYPE_DI_V2DI_UQI:
    case V64QI_FTYPE_V64QI_V64QI_UDI:
    case V64QI_FTYPE_V16QI_V64QI_UDI:
    case V64QI_FTYPE_QI_V64QI_UDI:
    case V32QI_FTYPE_V32QI_V32QI_USI:
    case V32QI_FTYPE_V16QI_V32QI_USI:
    case V32QI_FTYPE_QI_V32QI_USI:
    case V16QI_FTYPE_V16QI_V16QI_UHI:
    case V16QI_FTYPE_QI_V16QI_UHI:
    case V32HI_FTYPE_V8HI_V32HI_USI:
    case V32HI_FTYPE_HI_V32HI_USI:
    case V16HI_FTYPE_V8HI_V16HI_UHI:
    case V16HI_FTYPE_HI_V16HI_UHI:
    case V8HI_FTYPE_V8HI_V8HI_UQI:
    case V8HI_FTYPE_HI_V8HI_UQI:
    case V16HF_FTYPE_V16HF_V16HF_UHI:
    case V8SF_FTYPE_V8HI_V8SF_UQI:
    case V4SF_FTYPE_V8HI_V4SF_UQI:
    case V8SI_FTYPE_V8HF_V8SI_UQI:
    case V8SF_FTYPE_V8HF_V8SF_UQI:
    case V8SI_FTYPE_V8SF_V8SI_UQI:
    case V4SI_FTYPE_V4SF_V4SI_UQI:
    case V4SI_FTYPE_V8HF_V4SI_UQI:
    case V4SF_FTYPE_V8HF_V4SF_UQI:
    case V4DI_FTYPE_V8HF_V4DI_UQI:
    case V4DI_FTYPE_V4SF_V4DI_UQI:
    case V2DI_FTYPE_V8HF_V2DI_UQI:
    case V2DI_FTYPE_V4SF_V2DI_UQI:
    case V8HF_FTYPE_V8HF_V8HF_UQI:
    case V8HF_FTYPE_V8HF_V8HF_V8HF:
    case V8HF_FTYPE_V8HI_V8HF_UQI:
    case V8HF_FTYPE_V8SI_V8HF_UQI:
    case V8HF_FTYPE_V8SF_V8HF_UQI:
    case V8HF_FTYPE_V4SI_V8HF_UQI:
    case V8HF_FTYPE_V4SF_V8HF_UQI:
    case V8HF_FTYPE_V4DI_V8HF_UQI:
    case V8HF_FTYPE_V4DF_V8HF_UQI:
    case V8HF_FTYPE_V2DI_V8HF_UQI:
    case V8HF_FTYPE_V2DF_V8HF_UQI:
    case V4SF_FTYPE_V4DI_V4SF_UQI:
    case V4SF_FTYPE_V2DI_V4SF_UQI:
    case V4DF_FTYPE_V4DI_V4DF_UQI:
    case V4DF_FTYPE_V8HF_V4DF_UQI:
    case V2DF_FTYPE_V8HF_V2DF_UQI:
    case V2DF_FTYPE_V2DI_V2DF_UQI:
    case V16QI_FTYPE_V8HI_V16QI_UQI:
    case V16QI_FTYPE_V16HI_V16QI_UHI:
    case V16QI_FTYPE_V4SI_V16QI_UQI:
    case V16QI_FTYPE_V8SI_V16QI_UQI:
    case V8HI_FTYPE_V8HF_V8HI_UQI:
    case V8HI_FTYPE_V4SI_V8HI_UQI:
    case V8HI_FTYPE_V8SI_V8HI_UQI:
    case V16QI_FTYPE_V2DI_V16QI_UQI:
    case V16QI_FTYPE_V4DI_V16QI_UQI:
    case V8HI_FTYPE_V2DI_V8HI_UQI:
    case V8HI_FTYPE_V4DI_V8HI_UQI:
    case V4SI_FTYPE_V2DI_V4SI_UQI:
    case V4SI_FTYPE_V4DI_V4SI_UQI:
    case V32QI_FTYPE_V32HI_V32QI_USI:
    case UHI_FTYPE_V16QI_V16QI_UHI:
    case USI_FTYPE_V32QI_V32QI_USI:
    case UDI_FTYPE_V64QI_V64QI_UDI:
    case UQI_FTYPE_V8HI_V8HI_UQI:
    case UHI_FTYPE_V16HI_V16HI_UHI:
    case USI_FTYPE_V32HI_V32HI_USI:
    case UQI_FTYPE_V4SI_V4SI_UQI:
    case UQI_FTYPE_V8SI_V8SI_UQI:
    case UQI_FTYPE_V2DI_V2DI_UQI:
    case UQI_FTYPE_V4DI_V4DI_UQI:
    case V4SF_FTYPE_V2DF_V4SF_UQI:
    case V4SF_FTYPE_V4DF_V4SF_UQI:
    case V16SI_FTYPE_V16SI_V16SI_UHI:
    case V16SI_FTYPE_V4SI_V16SI_UHI:
    case V2DI_FTYPE_V4SI_V2DI_UQI:
    case V2DI_FTYPE_V8HI_V2DI_UQI:
    case V2DI_FTYPE_V16QI_V2DI_UQI:
    case V4DI_FTYPE_V4DI_V4DI_UQI:
    case V4DI_FTYPE_V4SI_V4DI_UQI:
    case V4DI_FTYPE_V8HI_V4DI_UQI:
    case V4DI_FTYPE_V16QI_V4DI_UQI:
    case V4DI_FTYPE_V4DF_V4DI_UQI:
    case V2DI_FTYPE_V2DF_V2DI_UQI:
    case V4SI_FTYPE_V4DF_V4SI_UQI:
    case V4SI_FTYPE_V2DF_V4SI_UQI:
    case V4SI_FTYPE_V8HI_V4SI_UQI:
    case V4SI_FTYPE_V16QI_V4SI_UQI:
    case V4DI_FTYPE_V4DI_V4DI_V4DI:
    case V8DF_FTYPE_V2DF_V8DF_UQI:
    case V8DF_FTYPE_V4DF_V8DF_UQI:
    case V8DF_FTYPE_V8DF_V8DF_UQI:
    case V8SF_FTYPE_V8SF_V8SF_UQI:
    case V8SF_FTYPE_V8SI_V8SF_UQI:
    case V4DF_FTYPE_V4DF_V4DF_UQI:
    case V4SF_FTYPE_V4SF_V4SF_UQI:
    case V2DF_FTYPE_V2DF_V2DF_UQI:
    case V2DF_FTYPE_V4SF_V2DF_UQI:
    case V2DF_FTYPE_V4SI_V2DF_UQI:
    case V4SF_FTYPE_V4SI_V4SF_UQI:
    case V4DF_FTYPE_V4SF_V4DF_UQI:
    case V4DF_FTYPE_V4SI_V4DF_UQI:
    case V8SI_FTYPE_V8SI_V8SI_UQI:
    case V8SI_FTYPE_V8HI_V8SI_UQI:
    case V8SI_FTYPE_V16QI_V8SI_UQI:
    case V8DF_FTYPE_V8SI_V8DF_UQI:
    case V8DI_FTYPE_DI_V8DI_UQI:
    case V16SF_FTYPE_V8SF_V16SF_UHI:
    case V16SI_FTYPE_V8SI_V16SI_UHI:
    case V16HF_FTYPE_V16HI_V16HF_UHI:
    case V16HF_FTYPE_V16HF_V16HF_V16HF:
    case V16HI_FTYPE_V16HF_V16HI_UHI:
    case V16HI_FTYPE_V16HI_V16HI_UHI:
    case V8HI_FTYPE_V16QI_V8HI_UQI:
    case V16HI_FTYPE_V16QI_V16HI_UHI:
    case V32HI_FTYPE_V32HI_V32HI_USI:
    case V32HI_FTYPE_V32QI_V32HI_USI:
    case V8DI_FTYPE_V16QI_V8DI_UQI:
    case V8DI_FTYPE_V2DI_V8DI_UQI:
    case V8DI_FTYPE_V4DI_V8DI_UQI:
    case V8DI_FTYPE_V8DI_V8DI_UQI:
    case V8DI_FTYPE_V8HI_V8DI_UQI:
    case V8DI_FTYPE_V8SI_V8DI_UQI:
    case V8HI_FTYPE_V8DI_V8HI_UQI:
    case V8SI_FTYPE_V8DI_V8SI_UQI:
    case V4SI_FTYPE_V4SI_V4SI_V4SI:
    case V4DI_FTYPE_V4DI_V4DI_V2DI:
    case V16SI_FTYPE_V16SI_V16SI_V16SI:
    case V8DI_FTYPE_V8DI_V8DI_V8DI:
    case V32HI_FTYPE_V32HI_V32HI_V32HI:
    case V2DI_FTYPE_V2DI_V2DI_V2DI:
    case V16HI_FTYPE_V16HI_V16HI_V16HI:
    case V8SI_FTYPE_V8SI_V8SI_V8SI:
    case V8HI_FTYPE_V8HI_V8HI_V8HI:
    case V32BF_FTYPE_V16SF_V16SF_USI:
    case V16BF_FTYPE_V8SF_V8SF_UHI:
    case V8BF_FTYPE_V4SF_V4SF_UQI:
    case V16BF_FTYPE_V16SF_V16BF_UHI:
    case V8BF_FTYPE_V8SF_V8BF_UQI:
    case V8BF_FTYPE_V4SF_V8BF_UQI:
    case V16SF_FTYPE_V16SF_V32BF_V32BF:
    case V8SF_FTYPE_V8SF_V16BF_V16BF:
    case V4SF_FTYPE_V4SF_V8BF_V8BF:
      nargs = 3;
      break;
    case V32QI_FTYPE_V32QI_V32QI_INT:
    case V16HI_FTYPE_V16HI_V16HI_INT:
    case V16QI_FTYPE_V16QI_V16QI_INT:
    case V4DI_FTYPE_V4DI_V4DI_INT:
    case V8HI_FTYPE_V8HI_V8HI_INT:
    case V8SI_FTYPE_V8SI_V8SI_INT:
    case V8SI_FTYPE_V8SI_V4SI_INT:
    case V8SF_FTYPE_V8SF_V8SF_INT:
    case V8SF_FTYPE_V8SF_V4SF_INT:
    case V4SI_FTYPE_V4SI_V4SI_INT:
    case V4DF_FTYPE_V4DF_V4DF_INT:
    case V16SF_FTYPE_V16SF_V16SF_INT:
    case V16SF_FTYPE_V16SF_V4SF_INT:
    case V16SI_FTYPE_V16SI_V4SI_INT:
    case V4DF_FTYPE_V4DF_V2DF_INT:
    case V4SF_FTYPE_V4SF_V4SF_INT:
    case V2DI_FTYPE_V2DI_V2DI_INT:
    case V4DI_FTYPE_V4DI_V2DI_INT:
    case V2DF_FTYPE_V2DF_V2DF_INT:
    case UQI_FTYPE_V8DI_V8UDI_INT:
    case UQI_FTYPE_V8DF_V8DF_INT:
    case UQI_FTYPE_V2DF_V2DF_INT:
    case UQI_FTYPE_V4SF_V4SF_INT:
    case UHI_FTYPE_V16SI_V16SI_INT:
    case UHI_FTYPE_V16SF_V16SF_INT:
    case V64QI_FTYPE_V64QI_V64QI_INT:
    case V32HI_FTYPE_V32HI_V32HI_INT:
    case V16SI_FTYPE_V16SI_V16SI_INT:
    case V8DI_FTYPE_V8DI_V8DI_INT:
      nargs = 3;
      nargs_constant = 1;
      break;
    case V4DI_FTYPE_V4DI_V4DI_INT_CONVERT:
      nargs = 3;
      rmode = V4DImode;
      nargs_constant = 1;
      break;
    case V2DI_FTYPE_V2DI_V2DI_INT_CONVERT:
      nargs = 3;
      rmode = V2DImode;
      nargs_constant = 1;
      break;
    case V1DI_FTYPE_V1DI_V1DI_INT_CONVERT:
      nargs = 3;
      rmode = DImode;
      nargs_constant = 1;
      break;
    case V2DI_FTYPE_V2DI_UINT_UINT:
      nargs = 3;
      nargs_constant = 2;
      break;
    case V8DI_FTYPE_V8DI_V8DI_INT_CONVERT:
      nargs = 3;
      rmode = V8DImode;
      nargs_constant = 1;
      break;
    case V8DI_FTYPE_V8DI_V8DI_INT_V8DI_UDI_CONVERT:
      nargs = 5;
      rmode = V8DImode;
      mask_pos = 2;
      nargs_constant = 1;
      break;
    case QI_FTYPE_V8DF_INT_UQI:
    case QI_FTYPE_V4DF_INT_UQI:
    case QI_FTYPE_V2DF_INT_UQI:
    case HI_FTYPE_V16SF_INT_UHI:
    case QI_FTYPE_V8SF_INT_UQI:
    case QI_FTYPE_V4SF_INT_UQI:
    case QI_FTYPE_V8HF_INT_UQI:
    case HI_FTYPE_V16HF_INT_UHI:
    case SI_FTYPE_V32HF_INT_USI:
    case V4SI_FTYPE_V4SI_V4SI_UHI:
    case V8SI_FTYPE_V8SI_V8SI_UHI:
      nargs = 3;
      mask_pos = 1;
      nargs_constant = 1;
      break;
    case V4DI_FTYPE_V4DI_V4DI_INT_V4DI_USI_CONVERT:
      nargs = 5;
      rmode = V4DImode;
      mask_pos = 2;
      nargs_constant = 1;
      break;
    case V2DI_FTYPE_V2DI_V2DI_INT_V2DI_UHI_CONVERT:
      nargs = 5;
      rmode = V2DImode;
      mask_pos = 2;
      nargs_constant = 1;
      break;
    case V32QI_FTYPE_V32QI_V32QI_V32QI_USI:
    case V32HI_FTYPE_V32HI_V32HI_V32HI_USI:
    case V32HI_FTYPE_V64QI_V64QI_V32HI_USI:
    case V16SI_FTYPE_V32HI_V32HI_V16SI_UHI:
    case V64QI_FTYPE_V64QI_V64QI_V64QI_UDI:
    case V32HI_FTYPE_V32HI_V8HI_V32HI_USI:
    case V16HI_FTYPE_V16HI_V8HI_V16HI_UHI:
    case V8SI_FTYPE_V8SI_V4SI_V8SI_UQI:
    case V4DI_FTYPE_V4DI_V2DI_V4DI_UQI:
    case V64QI_FTYPE_V32HI_V32HI_V64QI_UDI:
    case V32QI_FTYPE_V16HI_V16HI_V32QI_USI:
    case V16QI_FTYPE_V8HI_V8HI_V16QI_UHI:
    case V32HI_FTYPE_V16SI_V16SI_V32HI_USI:
    case V16HI_FTYPE_V8SI_V8SI_V16HI_UHI:
    case V8HI_FTYPE_V4SI_V4SI_V8HI_UQI:
    case V4DF_FTYPE_V4DF_V4DI_V4DF_UQI:
    case V32HF_FTYPE_V32HF_V32HF_V32HF_USI:
    case V8SF_FTYPE_V8SF_V8SI_V8SF_UQI:
    case V4SF_FTYPE_V4SF_V4SI_V4SF_UQI:
    case V2DF_FTYPE_V2DF_V2DI_V2DF_UQI:
    case V2DI_FTYPE_V4SI_V4SI_V2DI_UQI:
    case V4DI_FTYPE_V8SI_V8SI_V4DI_UQI:
    case V4DF_FTYPE_V4DI_V4DF_V4DF_UQI:
    case V8SF_FTYPE_V8SI_V8SF_V8SF_UQI:
    case V2DF_FTYPE_V2DI_V2DF_V2DF_UQI:
    case V4SF_FTYPE_V4SI_V4SF_V4SF_UQI:
    case V16SF_FTYPE_V16SF_V16SF_V16SF_UHI:
    case V16SF_FTYPE_V16SF_V16SI_V16SF_UHI:
    case V16SF_FTYPE_V16SI_V16SF_V16SF_UHI:
    case V16SI_FTYPE_V16SI_V16SI_V16SI_UHI:
    case V16SI_FTYPE_V16SI_V4SI_V16SI_UHI:
    case V8HI_FTYPE_V8HI_V8HI_V8HI_UQI:
    case V8SI_FTYPE_V8SI_V8SI_V8SI_UQI:
    case V4SI_FTYPE_V4SI_V4SI_V4SI_UQI:
    case V16HF_FTYPE_V16HF_V16HF_V16HF_UQI:
    case V16HF_FTYPE_V16HF_V16HF_V16HF_UHI:
    case V8SF_FTYPE_V8SF_V8SF_V8SF_UQI:
    case V16QI_FTYPE_V16QI_V16QI_V16QI_UHI:
    case V16HI_FTYPE_V16HI_V16HI_V16HI_UHI:
    case V2DI_FTYPE_V2DI_V2DI_V2DI_UQI:
    case V2DF_FTYPE_V2DF_V2DF_V2DF_UQI:
    case V4DI_FTYPE_V4DI_V4DI_V4DI_UQI:
    case V4DF_FTYPE_V4DF_V4DF_V4DF_UQI:
    case V8HF_FTYPE_V8HF_V8HF_V8HF_UQI:
    case V4SF_FTYPE_V4SF_V4SF_V4SF_UQI:
    case V8DF_FTYPE_V8DF_V8DF_V8DF_UQI:
    case V8DF_FTYPE_V8DF_V8DI_V8DF_UQI:
    case V8DF_FTYPE_V8DI_V8DF_V8DF_UQI:
    case V8DI_FTYPE_V16SI_V16SI_V8DI_UQI:
    case V8DI_FTYPE_V8DI_V2DI_V8DI_UQI:
    case V8DI_FTYPE_V8DI_V8DI_V8DI_UQI:
    case V8HI_FTYPE_V16QI_V16QI_V8HI_UQI:
    case V16HI_FTYPE_V32QI_V32QI_V16HI_UHI:
    case V8SI_FTYPE_V16HI_V16HI_V8SI_UQI:
    case V4SI_FTYPE_V8HI_V8HI_V4SI_UQI:
    case V32BF_FTYPE_V16SF_V16SF_V32BF_USI:
    case V16BF_FTYPE_V8SF_V8SF_V16BF_UHI:
    case V8BF_FTYPE_V4SF_V4SF_V8BF_UQI:
      nargs = 4;
      break;
    case V2DF_FTYPE_V2DF_V2DF_V2DI_INT:
    case V4DF_FTYPE_V4DF_V4DF_V4DI_INT:
    case V4SF_FTYPE_V4SF_V4SF_V4SI_INT:
    case V8SF_FTYPE_V8SF_V8SF_V8SI_INT:
    case V16SF_FTYPE_V16SF_V16SF_V16SI_INT:
    case V4SI_FTYPE_V4SI_V4SI_V4SI_INT:
      nargs = 4;
      nargs_constant = 1;
      break;
    case UQI_FTYPE_V4DI_V4DI_INT_UQI:
    case UQI_FTYPE_V8SI_V8SI_INT_UQI:
    case QI_FTYPE_V4DF_V4DF_INT_UQI:
    case QI_FTYPE_V8SF_V8SF_INT_UQI:
    case UHI_FTYPE_V16HF_V16HF_INT_UHI:
    case UQI_FTYPE_V2DI_V2DI_INT_UQI:
    case UQI_FTYPE_V4SI_V4SI_INT_UQI:
    case UQI_FTYPE_V2DF_V2DF_INT_UQI:
    case UQI_FTYPE_V4SF_V4SF_INT_UQI:
    case UQI_FTYPE_V8HF_V8HF_INT_UQI:
    case UDI_FTYPE_V64QI_V64QI_INT_UDI:
    case USI_FTYPE_V32QI_V32QI_INT_USI:
    case UHI_FTYPE_V16QI_V16QI_INT_UHI:
    case USI_FTYPE_V32HI_V32HI_INT_USI:
    case USI_FTYPE_V32HF_V32HF_INT_USI:
    case UHI_FTYPE_V16HI_V16HI_INT_UHI:
    case UQI_FTYPE_V8HI_V8HI_INT_UQI:
      nargs = 4;
      mask_pos = 1;
      nargs_constant = 1;
      break;
    case V2DI_FTYPE_V2DI_V2DI_UINT_UINT:
      nargs = 4;
      nargs_constant = 2;
      break;
    case UCHAR_FTYPE_UCHAR_UINT_UINT_PUNSIGNED:
    case UCHAR_FTYPE_UCHAR_ULONGLONG_ULONGLONG_PULONGLONG:
    case V16SF_FTYPE_V16SF_V32BF_V32BF_UHI:
    case V8SF_FTYPE_V8SF_V16BF_V16BF_UQI:
    case V4SF_FTYPE_V4SF_V8BF_V8BF_UQI:
      nargs = 4;
      break;
    case UQI_FTYPE_V8DI_V8DI_INT_UQI:
    case UHI_FTYPE_V16SI_V16SI_INT_UHI:
      mask_pos = 1;
      nargs = 4;
      nargs_constant = 1;
      break;
    case V8SF_FTYPE_V8SF_INT_V8SF_UQI:
    case V4SF_FTYPE_V4SF_INT_V4SF_UQI:
    case V2DF_FTYPE_V4DF_INT_V2DF_UQI:
    case V2DI_FTYPE_V4DI_INT_V2DI_UQI:
    case V8SF_FTYPE_V16SF_INT_V8SF_UQI:
    case V8SI_FTYPE_V16SI_INT_V8SI_UQI:
    case V2DF_FTYPE_V8DF_INT_V2DF_UQI:
    case V2DI_FTYPE_V8DI_INT_V2DI_UQI:
    case V4SF_FTYPE_V8SF_INT_V4SF_UQI:
    case V4SI_FTYPE_V8SI_INT_V4SI_UQI:
    case V8HI_FTYPE_V8SF_INT_V8HI_UQI:
    case V8HI_FTYPE_V4SF_INT_V8HI_UQI:
    case V32HI_FTYPE_V32HI_INT_V32HI_USI:
    case V16HI_FTYPE_V16HI_INT_V16HI_UHI:
    case V8HI_FTYPE_V8HI_INT_V8HI_UQI:
    case V4DI_FTYPE_V4DI_INT_V4DI_UQI:
    case V2DI_FTYPE_V2DI_INT_V2DI_UQI:
    case V8SI_FTYPE_V8SI_INT_V8SI_UQI:
    case V4SI_FTYPE_V4SI_INT_V4SI_UQI:
    case V4DF_FTYPE_V4DF_INT_V4DF_UQI:
    case V2DF_FTYPE_V2DF_INT_V2DF_UQI:
    case V8DF_FTYPE_V8DF_INT_V8DF_UQI:
    case V16SF_FTYPE_V16SF_INT_V16SF_UHI:
    case V16HI_FTYPE_V16SF_INT_V16HI_UHI:
    case V16SI_FTYPE_V16SI_INT_V16SI_UHI:
    case V16HF_FTYPE_V16HF_INT_V16HF_UHI:
    case V8HF_FTYPE_V8HF_INT_V8HF_UQI:
    case V4SI_FTYPE_V16SI_INT_V4SI_UQI:
    case V4DI_FTYPE_V8DI_INT_V4DI_UQI:
    case V4DF_FTYPE_V8DF_INT_V4DF_UQI:
    case V4SF_FTYPE_V16SF_INT_V4SF_UQI:
    case V8DI_FTYPE_V8DI_INT_V8DI_UQI:
      nargs = 4;
      mask_pos = 2;
      nargs_constant = 1;
      break;
    case V16SF_FTYPE_V16SF_V4SF_INT_V16SF_UHI:
    case V16SI_FTYPE_V16SI_V4SI_INT_V16SI_UHI:
    case V8DF_FTYPE_V8DF_V8DF_INT_V8DF_UQI:
    case V8DI_FTYPE_V8DI_V8DI_INT_V8DI_UQI:
    case V16SF_FTYPE_V16SF_V16SF_INT_V16SF_UHI:
    case V16SI_FTYPE_V16SI_V16SI_INT_V16SI_UHI:
    case V4SF_FTYPE_V4SF_V4SF_INT_V4SF_UQI:
    case V2DF_FTYPE_V2DF_V2DF_INT_V2DF_UQI:
    case V8DF_FTYPE_V8DF_V4DF_INT_V8DF_UQI:
    case V8DI_FTYPE_V8DI_V4DI_INT_V8DI_UQI:
    case V4DF_FTYPE_V4DF_V4DF_INT_V4DF_UQI:
    case V8SF_FTYPE_V8SF_V8SF_INT_V8SF_UQI:
    case V8DF_FTYPE_V8DF_V2DF_INT_V8DF_UQI:
    case V8DI_FTYPE_V8DI_V2DI_INT_V8DI_UQI:
    case V8SI_FTYPE_V8SI_V8SI_INT_V8SI_UQI:
    case V4DI_FTYPE_V4DI_V4DI_INT_V4DI_UQI:
    case V4SI_FTYPE_V4SI_V4SI_INT_V4SI_UQI:
    case V2DI_FTYPE_V2DI_V2DI_INT_V2DI_UQI:
    case V32HI_FTYPE_V64QI_V64QI_INT_V32HI_USI:
    case V16HI_FTYPE_V32QI_V32QI_INT_V16HI_UHI:
    case V8HI_FTYPE_V16QI_V16QI_INT_V8HI_UQI:
    case V16SF_FTYPE_V16SF_V8SF_INT_V16SF_UHI:
    case V16SI_FTYPE_V16SI_V8SI_INT_V16SI_UHI:
    case V8SF_FTYPE_V8SF_V4SF_INT_V8SF_UQI:
    case V8SI_FTYPE_V8SI_V4SI_INT_V8SI_UQI:
    case V4DI_FTYPE_V4DI_V2DI_INT_V4DI_UQI:
    case V4DF_FTYPE_V4DF_V2DF_INT_V4DF_UQI:
      nargs = 5;
      mask_pos = 2;
      nargs_constant = 1;
      break;
    case V8DI_FTYPE_V8DI_V8DI_V8DI_INT_UQI:
    case V16SI_FTYPE_V16SI_V16SI_V16SI_INT_UHI:
    case V2DF_FTYPE_V2DF_V2DF_V2DI_INT_UQI:
    case V4SF_FTYPE_V4SF_V4SF_V4SI_INT_UQI:
    case V8SF_FTYPE_V8SF_V8SF_V8SI_INT_UQI:
    case V8SI_FTYPE_V8SI_V8SI_V8SI_INT_UQI:
    case V4DF_FTYPE_V4DF_V4DF_V4DI_INT_UQI:
    case V4DI_FTYPE_V4DI_V4DI_V4DI_INT_UQI:
    case V4SI_FTYPE_V4SI_V4SI_V4SI_INT_UQI:
    case V2DI_FTYPE_V2DI_V2DI_V2DI_INT_UQI:
      nargs = 5;
      mask_pos = 1;
      nargs_constant = 1;
      break;
    case V64QI_FTYPE_V64QI_V64QI_INT_V64QI_UDI:
    case V32QI_FTYPE_V32QI_V32QI_INT_V32QI_USI:
    case V16QI_FTYPE_V16QI_V16QI_INT_V16QI_UHI:
    case V32HI_FTYPE_V32HI_V32HI_INT_V32HI_INT:
    case V16SI_FTYPE_V16SI_V16SI_INT_V16SI_INT:
    case V8DI_FTYPE_V8DI_V8DI_INT_V8DI_INT:
    case V16HI_FTYPE_V16HI_V16HI_INT_V16HI_INT:
    case V8SI_FTYPE_V8SI_V8SI_INT_V8SI_INT:
    case V4DI_FTYPE_V4DI_V4DI_INT_V4DI_INT:
    case V8HI_FTYPE_V8HI_V8HI_INT_V8HI_INT:
    case V4SI_FTYPE_V4SI_V4SI_INT_V4SI_INT:
    case V2DI_FTYPE_V2DI_V2DI_INT_V2DI_INT:
      nargs = 5;
      mask_pos = 1;
      nargs_constant = 2;
      break;

    default:
      gcc_unreachable ();
    }

  gcc_assert (nargs <= ARRAY_SIZE (xops));

  if (comparison != UNKNOWN)
    {
      gcc_assert (nargs == 2);
      return ix86_expand_sse_compare (d, exp, target, swap);
    }

  if (rmode == VOIDmode || rmode == tmode)
    {
      if (optimize
	  || target == 0
	  || GET_MODE (target) != tmode
	  || !insn_p->operand[0].predicate (target, tmode))
	target = gen_reg_rtx (tmode);
      else if (memory_operand (target, tmode))
	num_memory++;
      real_target = target;
    }
  else
    {
      real_target = gen_reg_rtx (tmode);
      target = lowpart_subreg (rmode, real_target, tmode);
    }

  for (i = 0; i < nargs; i++)
    {
      tree arg = CALL_EXPR_ARG (exp, i);
      rtx op = expand_normal (arg);
      machine_mode mode = insn_p->operand[i + 1].mode;
      bool match = insn_p->operand[i + 1].predicate (op, mode);

      if (second_arg_count && i == 1)
	{
	  /* SIMD shift insns take either an 8-bit immediate or
	     register as count.  But builtin functions take int as
	     count.  If count doesn't match, we put it in register.
	     The instructions are using 64-bit count, if op is just
	     32-bit, zero-extend it, as negative shift counts
	     are undefined behavior and zero-extension is more
	     efficient.  */
	  if (!match)
	    {
	      if (SCALAR_INT_MODE_P (GET_MODE (op)))
		op = convert_modes (mode, GET_MODE (op), op, 1);
	      else
		op = lowpart_subreg (mode, op, GET_MODE (op));
	      if (!insn_p->operand[i + 1].predicate (op, mode))
		op = copy_to_reg (op);
	    }
	}
      else if ((mask_pos && (nargs - i - mask_pos) == nargs_constant) ||
	       (!mask_pos && (nargs - i) <= nargs_constant))
	{
	  if (!match)
	    switch (icode)
	      {
	      case CODE_FOR_avx_vinsertf128v4di:
	      case CODE_FOR_avx_vextractf128v4di:
		error ("the last argument must be an 1-bit immediate");
		return const0_rtx;

	      case CODE_FOR_avx512f_cmpv8di3_mask:
	      case CODE_FOR_avx512f_cmpv16si3_mask:
	      case CODE_FOR_avx512f_ucmpv8di3_mask:
	      case CODE_FOR_avx512f_ucmpv16si3_mask:
	      case CODE_FOR_avx512vl_cmpv4di3_mask:
	      case CODE_FOR_avx512vl_cmpv8si3_mask:
	      case CODE_FOR_avx512vl_ucmpv4di3_mask:
	      case CODE_FOR_avx512vl_ucmpv8si3_mask:
	      case CODE_FOR_avx512vl_cmpv2di3_mask:
	      case CODE_FOR_avx512vl_cmpv4si3_mask:
	      case CODE_FOR_avx512vl_ucmpv2di3_mask:
	      case CODE_FOR_avx512vl_ucmpv4si3_mask:
		error ("the last argument must be a 3-bit immediate");
		return const0_rtx;

	      case CODE_FOR_sse4_1_roundsd:
	      case CODE_FOR_sse4_1_roundss:

	      case CODE_FOR_sse4_1_roundpd:
	      case CODE_FOR_sse4_1_roundps:
	      case CODE_FOR_avx_roundpd256:
	      case CODE_FOR_avx_roundps256:

	      case CODE_FOR_sse4_1_roundpd_vec_pack_sfix:
	      case CODE_FOR_sse4_1_roundps_sfix:
	      case CODE_FOR_avx_roundpd_vec_pack_sfix256:
	      case CODE_FOR_avx_roundps_sfix256:

	      case CODE_FOR_sse4_1_blendps:
	      case CODE_FOR_avx_blendpd256:
	      case CODE_FOR_avx_vpermilv4df:
	      case CODE_FOR_avx_vpermilv4df_mask:
	      case CODE_FOR_avx512f_getmantv8df_mask:
	      case CODE_FOR_avx512f_getmantv16sf_mask:
	      case CODE_FOR_avx512vl_getmantv16hf_mask:
	      case CODE_FOR_avx512vl_getmantv8sf_mask:
	      case CODE_FOR_avx512vl_getmantv4df_mask:
	      case CODE_FOR_avx512fp16_getmantv8hf_mask:
	      case CODE_FOR_avx512vl_getmantv4sf_mask:
	      case CODE_FOR_avx512vl_getmantv2df_mask:
	      case CODE_FOR_avx512dq_rangepv8df_mask_round:
	      case CODE_FOR_avx512dq_rangepv16sf_mask_round:
	      case CODE_FOR_avx512dq_rangepv4df_mask:
	      case CODE_FOR_avx512dq_rangepv8sf_mask:
	      case CODE_FOR_avx512dq_rangepv2df_mask:
	      case CODE_FOR_avx512dq_rangepv4sf_mask:
	      case CODE_FOR_avx_shufpd256_mask:
		error ("the last argument must be a 4-bit immediate");
		return const0_rtx;

	      case CODE_FOR_sha1rnds4:
	      case CODE_FOR_sse4_1_blendpd:
	      case CODE_FOR_avx_vpermilv2df:
	      case CODE_FOR_avx_vpermilv2df_mask:
	      case CODE_FOR_xop_vpermil2v2df3:
	      case CODE_FOR_xop_vpermil2v4sf3:
	      case CODE_FOR_xop_vpermil2v4df3:
	      case CODE_FOR_xop_vpermil2v8sf3:
	      case CODE_FOR_avx512f_vinsertf32x4_mask:
	      case CODE_FOR_avx512f_vinserti32x4_mask:
	      case CODE_FOR_avx512f_vextractf32x4_mask:
	      case CODE_FOR_avx512f_vextracti32x4_mask:
	      case CODE_FOR_sse2_shufpd:
	      case CODE_FOR_sse2_shufpd_mask:
	      case CODE_FOR_avx512dq_shuf_f64x2_mask:
	      case CODE_FOR_avx512dq_shuf_i64x2_mask:
	      case CODE_FOR_avx512vl_shuf_i32x4_mask:
	      case CODE_FOR_avx512vl_shuf_f32x4_mask:
		error ("the last argument must be a 2-bit immediate");
		return const0_rtx;

	      case CODE_FOR_avx_vextractf128v4df:
	      case CODE_FOR_avx_vextractf128v8sf:
	      case CODE_FOR_avx_vextractf128v8si:
	      case CODE_FOR_avx_vinsertf128v4df:
	      case CODE_FOR_avx_vinsertf128v8sf:
	      case CODE_FOR_avx_vinsertf128v8si:
	      case CODE_FOR_avx512f_vinsertf64x4_mask:
	      case CODE_FOR_avx512f_vinserti64x4_mask:
	      case CODE_FOR_avx512f_vextractf64x4_mask:
	      case CODE_FOR_avx512f_vextracti64x4_mask:
	      case CODE_FOR_avx512dq_vinsertf32x8_mask:
	      case CODE_FOR_avx512dq_vinserti32x8_mask:
	      case CODE_FOR_avx512vl_vinsertv4df:
	      case CODE_FOR_avx512vl_vinsertv4di:
	      case CODE_FOR_avx512vl_vinsertv8sf:
	      case CODE_FOR_avx512vl_vinsertv8si:
		error ("the last argument must be a 1-bit immediate");
		return const0_rtx;

	      case CODE_FOR_avx_vmcmpv2df3:
	      case CODE_FOR_avx_vmcmpv4sf3:
	      case CODE_FOR_avx_cmpv2df3:
	      case CODE_FOR_avx_cmpv4sf3:
	      case CODE_FOR_avx_cmpv4df3:
	      case CODE_FOR_avx_cmpv8sf3:
	      case CODE_FOR_avx512f_cmpv8df3_mask:
	      case CODE_FOR_avx512f_cmpv16sf3_mask:
	      case CODE_FOR_avx512f_vmcmpv2df3_mask:
	      case CODE_FOR_avx512f_vmcmpv4sf3_mask:
	      case CODE_FOR_avx512bw_cmpv32hf3_mask:
	      case CODE_FOR_avx512vl_cmpv16hf3_mask:
	      case CODE_FOR_avx512fp16_cmpv8hf3_mask:
		error ("the last argument must be a 5-bit immediate");
		return const0_rtx;

	      default:
		switch (nargs_constant)
		  {
		  case 2:
		    if ((mask_pos && (nargs - i - mask_pos) == nargs_constant) ||
			(!mask_pos && (nargs - i) == nargs_constant))
		      {
			error ("the next to last argument must be an 8-bit immediate");
			break;
		      }
		    /* FALLTHRU */
		  case 1:
		    error ("the last argument must be an 8-bit immediate");
		    break;
		  default:
		    gcc_unreachable ();
		  }
		return const0_rtx;
	      }
	}
      else
	{
	  if (VECTOR_MODE_P (mode))
	    op = safe_vector_operand (op, mode);

	  /* If we aren't optimizing, only allow one memory operand to
	     be generated.  */
	  if (memory_operand (op, mode))
	    num_memory++;

	  op = fixup_modeless_constant (op, mode);

	  if (GET_MODE (op) == mode || GET_MODE (op) == VOIDmode)
	    {
	      if (optimize || !match || num_memory > 1)
		op = copy_to_mode_reg (mode, op);
	    }
	  else
	    {
	      op = copy_to_reg (op);
	      op = lowpart_subreg (mode, op, GET_MODE (op));
	    }
	}

      xops[i] = op;
    }

  switch (nargs)
    {
    case 1:
      pat = GEN_FCN (icode) (real_target, xops[0]);
      break;
    case 2:
      pat = GEN_FCN (icode) (real_target, xops[0], xops[1]);
      break;
    case 3:
      pat = GEN_FCN (icode) (real_target, xops[0], xops[1], xops[2]);
      break;
    case 4:
      pat = GEN_FCN (icode) (real_target, xops[0], xops[1],
			     xops[2], xops[3]);
      break;
    case 5:
      pat = GEN_FCN (icode) (real_target, xops[0], xops[1],
			     xops[2], xops[3], xops[4]);
      break;
    case 6:
      pat = GEN_FCN (icode) (real_target, xops[0], xops[1],
			     xops[2], xops[3], xops[4], xops[5]);
      break;
    default:
      gcc_unreachable ();
    }

  if (! pat)
    return 0;

  emit_insn (pat);
  return target;
}

/* Transform pattern of following layout:
     (set A
       (unspec [B C] UNSPEC_EMBEDDED_ROUNDING))
     )
   into:
     (set (A B)) */

static rtx
ix86_erase_embedded_rounding (rtx pat)
{
  if (GET_CODE (pat) == INSN)
    pat = PATTERN (pat);

  gcc_assert (GET_CODE (pat) == SET);
  rtx src = SET_SRC (pat);
  gcc_assert (XVECLEN (src, 0) == 2);
  rtx p0 = XVECEXP (src, 0, 0);
  gcc_assert (GET_CODE (src) == UNSPEC
	      && XINT (src, 1) == UNSPEC_EMBEDDED_ROUNDING);
  rtx res = gen_rtx_SET (SET_DEST (pat), p0);
  return res;
}

/* Subroutine of ix86_expand_round_builtin to take care of comi insns
   with rounding.  */
static rtx
ix86_expand_sse_comi_round (const struct builtin_description *d,
			    tree exp, rtx target)
{
  rtx pat, set_dst;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  tree arg2 = CALL_EXPR_ARG (exp, 2);
  tree arg3 = CALL_EXPR_ARG (exp, 3);
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  rtx op2 = expand_normal (arg2);
  rtx op3 = expand_normal (arg3);
  enum insn_code icode = d->icode;
  const struct insn_data_d *insn_p = &insn_data[icode];
  machine_mode mode0 = insn_p->operand[0].mode;
  machine_mode mode1 = insn_p->operand[1].mode;

  /* See avxintrin.h for values.  */
  static const enum rtx_code comparisons[32] =
    {
      EQ, LT, LE, UNORDERED, NE, UNGE, UNGT, ORDERED,
      UNEQ, UNLT, UNLE, UNORDERED, LTGT, GE, GT, ORDERED,
      EQ, LT, LE, UNORDERED, NE, UNGE, UNGT, ORDERED,
      UNEQ, UNLT, UNLE, UNORDERED, LTGT, GE, GT, ORDERED
    };
  static const bool ordereds[32] =
    {
      true,  true,  true,  false, false, false, false, true,
      false, false, false, true,  true,  true,  true,  false,
      true,  true,  true,  false, false, false, false, true,
      false, false, false, true,  true,  true,  true,  false
    };
  static const bool non_signalings[32] =
    {
      true,  false, false, true,  true,  false, false, true,
      true,  false, false, true,  true,  false, false, true,
      false, true,  true,  false, false, true,  true,  false,
      false, true,  true,  false, false, true,  true,  false
    };

  if (!CONST_INT_P (op2))
    {
      error ("the third argument must be comparison constant");
      return const0_rtx;
    }
  if (INTVAL (op2) < 0 || INTVAL (op2) >= 32)
    {
      error ("incorrect comparison mode");
      return const0_rtx;
    }

  if (!insn_p->operand[2].predicate (op3, SImode))
    {
      error ("incorrect rounding operand");
      return const0_rtx;
    }

  if (VECTOR_MODE_P (mode0))
    op0 = safe_vector_operand (op0, mode0);
  if (VECTOR_MODE_P (mode1))
    op1 = safe_vector_operand (op1, mode1);

  enum rtx_code comparison = comparisons[INTVAL (op2)];
  bool ordered = ordereds[INTVAL (op2)];
  bool non_signaling = non_signalings[INTVAL (op2)];
  rtx const_val = const0_rtx;

  bool check_unordered = false;
  machine_mode mode = CCFPmode;
  switch (comparison)
    {
    case ORDERED:
      if (!ordered)
	{
	  /* NB: Use CCSmode/NE for _CMP_TRUE_UQ/_CMP_TRUE_US.  */
	  if (!non_signaling)
	    ordered = true;
	  mode = CCSmode;
	}
      else
	{
	  /* NB: Use CCPmode/NE for _CMP_ORD_Q/_CMP_ORD_S.  */
	  if (non_signaling)
	    ordered = false;
	  mode = CCPmode;
	}
      comparison = NE;
      break;
    case UNORDERED:
      if (ordered)
	{
	  /* NB: Use CCSmode/EQ for _CMP_FALSE_OQ/_CMP_FALSE_OS.  */
	  if (non_signaling)
	    ordered = false;
	  mode = CCSmode;
	}
      else
	{
	  /* NB: Use CCPmode/NE for _CMP_UNORD_Q/_CMP_UNORD_S.  */
	  if (!non_signaling)
	    ordered = true;
	  mode = CCPmode;
	}
      comparison = EQ;
      break;

    case LE:	/* -> GE  */
    case LT:	/* -> GT  */
    case UNGE:	/* -> UNLE  */
    case UNGT:	/* -> UNLT  */
      std::swap (op0, op1);
      comparison = swap_condition (comparison);
      /* FALLTHRU */
    case GT:
    case GE:
    case UNEQ:
    case UNLT:
    case UNLE:
    case LTGT:
      /* These are supported by CCFPmode.  NB: Use ordered/signaling
	 COMI or unordered/non-signaling UCOMI.  Both set ZF, PF, CF
	 with NAN operands.  */
      if (ordered == non_signaling)
	ordered = !ordered;
      break;
    case EQ:
      /* NB: COMI/UCOMI will set ZF with NAN operands.  Use CCZmode for
	 _CMP_EQ_OQ/_CMP_EQ_OS.  */
      check_unordered = true;
      mode = CCZmode;
      break;
    case NE:
      /* NB: COMI/UCOMI will set ZF with NAN operands.  Use CCZmode for
	 _CMP_NEQ_UQ/_CMP_NEQ_US.  */
      gcc_assert (!ordered);
      check_unordered = true;
      mode = CCZmode;
      const_val = const1_rtx;
      break;
    default:
      gcc_unreachable ();
    }

  target = gen_reg_rtx (SImode);
  emit_move_insn (target, const_val);
  target = gen_rtx_SUBREG (QImode, target, 0);

  if ((optimize && !register_operand (op0, mode0))
      || !insn_p->operand[0].predicate (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if ((optimize && !register_operand (op1, mode1))
      || !insn_p->operand[1].predicate (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);

  /*
     1. COMI: ordered and signaling.
     2. UCOMI: unordered and non-signaling.
   */
  if (non_signaling)
    icode = (icode == CODE_FOR_sse_comi_round
	     ? CODE_FOR_sse_ucomi_round
	     : CODE_FOR_sse2_ucomi_round);

  pat = GEN_FCN (icode) (op0, op1, op3);
  if (! pat)
    return 0;

  /* Rounding operand can be either NO_ROUND or ROUND_SAE at this point.  */
  if (INTVAL (op3) == NO_ROUND)
    {
      pat = ix86_erase_embedded_rounding (pat);
      if (! pat)
	return 0;

      set_dst = SET_DEST (pat);
    }
  else
    {
      gcc_assert (GET_CODE (pat) == SET);
      set_dst = SET_DEST (pat);
    }

  emit_insn (pat);

  return ix86_ssecom_setcc (comparison, check_unordered, mode,
			    set_dst, target);
}

static rtx
ix86_expand_round_builtin (const struct builtin_description *d,
			   tree exp, rtx target)
{
  rtx pat;
  unsigned int i, nargs;
  rtx xops[6];
  enum insn_code icode = d->icode;
  const struct insn_data_d *insn_p = &insn_data[icode];
  machine_mode tmode = insn_p->operand[0].mode;
  unsigned int nargs_constant = 0;
  unsigned int redundant_embed_rnd = 0;

  switch ((enum ix86_builtin_func_type) d->flag)
    {
    case UINT64_FTYPE_V2DF_INT:
    case UINT64_FTYPE_V4SF_INT:
    case UINT64_FTYPE_V8HF_INT:
    case UINT_FTYPE_V2DF_INT:
    case UINT_FTYPE_V4SF_INT:
    case UINT_FTYPE_V8HF_INT:
    case INT64_FTYPE_V2DF_INT:
    case INT64_FTYPE_V4SF_INT:
    case INT64_FTYPE_V8HF_INT:
    case INT_FTYPE_V2DF_INT:
    case INT_FTYPE_V4SF_INT:
    case INT_FTYPE_V8HF_INT:
      nargs = 2;
      break;
    case V32HF_FTYPE_V32HF_V32HF_INT:
    case V8HF_FTYPE_V8HF_V8HF_INT:
    case V8HF_FTYPE_V8HF_INT_INT:
    case V8HF_FTYPE_V8HF_UINT_INT:
    case V8HF_FTYPE_V8HF_INT64_INT:
    case V8HF_FTYPE_V8HF_UINT64_INT:
    case V4SF_FTYPE_V4SF_UINT_INT:
    case V4SF_FTYPE_V4SF_UINT64_INT:
    case V2DF_FTYPE_V2DF_UINT64_INT:
    case V4SF_FTYPE_V4SF_INT_INT:
    case V4SF_FTYPE_V4SF_INT64_INT:
    case V2DF_FTYPE_V2DF_INT64_INT:
    case V4SF_FTYPE_V4SF_V4SF_INT:
    case V2DF_FTYPE_V2DF_V2DF_INT:
    case V4SF_FTYPE_V4SF_V2DF_INT:
    case V2DF_FTYPE_V2DF_V4SF_INT:
      nargs = 3;
      break;
    case V8SF_FTYPE_V8DF_V8SF_QI_INT:
    case V8DF_FTYPE_V8DF_V8DF_QI_INT:
    case V32HI_FTYPE_V32HF_V32HI_USI_INT:
    case V8SI_FTYPE_V8DF_V8SI_QI_INT:
    case V8DI_FTYPE_V8HF_V8DI_UQI_INT:
    case V8DI_FTYPE_V8DF_V8DI_QI_INT:
    case V8SF_FTYPE_V8DI_V8SF_QI_INT:
    case V8DF_FTYPE_V8DI_V8DF_QI_INT:
    case V8DF_FTYPE_V8HF_V8DF_UQI_INT:
    case V16SF_FTYPE_V16HF_V16SF_UHI_INT:
    case V32HF_FTYPE_V32HI_V32HF_USI_INT:
    case V32HF_FTYPE_V32HF_V32HF_USI_INT:
    case V32HF_FTYPE_V32HF_V32HF_V32HF_INT:
    case V16SF_FTYPE_V16SF_V16SF_HI_INT:
    case V8DI_FTYPE_V8SF_V8DI_QI_INT:
    case V16SF_FTYPE_V16SI_V16SF_HI_INT:
    case V16SI_FTYPE_V16SF_V16SI_HI_INT:
    case V16SI_FTYPE_V16HF_V16SI_UHI_INT:
    case V16HF_FTYPE_V16SI_V16HF_UHI_INT:
    case V8DF_FTYPE_V8SF_V8DF_QI_INT:
    case V16SF_FTYPE_V16HI_V16SF_HI_INT:
    case V2DF_FTYPE_V2DF_V2DF_V2DF_INT:
    case V4SF_FTYPE_V4SF_V4SF_V4SF_INT:
    case V8HF_FTYPE_V8DI_V8HF_UQI_INT:
    case V8HF_FTYPE_V8DF_V8HF_UQI_INT:
    case V16HF_FTYPE_V16SF_V16HF_UHI_INT:
    case V8HF_FTYPE_V8HF_V8HF_V8HF_INT:
      nargs = 4;
      break;
    case V4SF_FTYPE_V4SF_V4SF_INT_INT:
    case V2DF_FTYPE_V2DF_V2DF_INT_INT:
      nargs_constant = 2;
      nargs = 4;
      break;
    case INT_FTYPE_V4SF_V4SF_INT_INT:
    case INT_FTYPE_V2DF_V2DF_INT_INT:
      return ix86_expand_sse_comi_round (d, exp, target);
    case V8DF_FTYPE_V8DF_V8DF_V8DF_UQI_INT:
    case V2DF_FTYPE_V2DF_V2DF_V2DF_UQI_INT:
    case V4SF_FTYPE_V4SF_V4SF_V4SF_UQI_INT:
    case V4SF_FTYPE_V8HF_V4SF_V4SF_UQI_INT:
    case V16SF_FTYPE_V16SF_V16SF_V16SF_HI_INT:
    case V32HF_FTYPE_V32HF_V32HF_V32HF_UHI_INT:
    case V32HF_FTYPE_V32HF_V32HF_V32HF_USI_INT:
    case V2DF_FTYPE_V8HF_V2DF_V2DF_UQI_INT:
    case V2DF_FTYPE_V2DF_V2DF_V2DF_QI_INT:
    case V2DF_FTYPE_V2DF_V4SF_V2DF_QI_INT:
    case V2DF_FTYPE_V2DF_V4SF_V2DF_UQI_INT:
    case V4SF_FTYPE_V4SF_V4SF_V4SF_QI_INT:
    case V4SF_FTYPE_V4SF_V2DF_V4SF_QI_INT:
    case V4SF_FTYPE_V4SF_V2DF_V4SF_UQI_INT:
    case V8HF_FTYPE_V8HF_V8HF_V8HF_UQI_INT:
    case V8HF_FTYPE_V2DF_V8HF_V8HF_UQI_INT:
    case V8HF_FTYPE_V4SF_V8HF_V8HF_UQI_INT:
      nargs = 5;
      break;
    case V32HF_FTYPE_V32HF_INT_V32HF_USI_INT:
    case V16SF_FTYPE_V16SF_INT_V16SF_HI_INT:
    case V8DF_FTYPE_V8DF_INT_V8DF_QI_INT:
    case V8DF_FTYPE_V8DF_INT_V8DF_UQI_INT:
    case V16SF_FTYPE_V16SF_INT_V16SF_UHI_INT:
      nargs_constant = 4;
      nargs = 5;
      break;
    case UQI_FTYPE_V8DF_V8DF_INT_UQI_INT:
    case UQI_FTYPE_V2DF_V2DF_INT_UQI_INT:
    case UHI_FTYPE_V16SF_V16SF_INT_UHI_INT:
    case UQI_FTYPE_V4SF_V4SF_INT_UQI_INT:
    case USI_FTYPE_V32HF_V32HF_INT_USI_INT:
    case UQI_FTYPE_V8HF_V8HF_INT_UQI_INT:
      nargs_constant = 3;
      nargs = 5;
      break;
    case V16SF_FTYPE_V16SF_V16SF_INT_V16SF_HI_INT:
    case V8DF_FTYPE_V8DF_V8DF_INT_V8DF_QI_INT:
    case V4SF_FTYPE_V4SF_V4SF_INT_V4SF_QI_INT:
    case V2DF_FTYPE_V2DF_V2DF_INT_V2DF_QI_INT:
    case V2DF_FTYPE_V2DF_V2DF_INT_V2DF_UQI_INT:
    case V4SF_FTYPE_V4SF_V4SF_INT_V4SF_UQI_INT:
    case V8HF_FTYPE_V8HF_V8HF_INT_V8HF_UQI_INT:
      nargs = 6;
      nargs_constant = 4;
      break;
    case V8DF_FTYPE_V8DF_V8DF_V8DI_INT_QI_INT:
    case V16SF_FTYPE_V16SF_V16SF_V16SI_INT_HI_INT:
    case V2DF_FTYPE_V2DF_V2DF_V2DI_INT_QI_INT:
    case V4SF_FTYPE_V4SF_V4SF_V4SI_INT_QI_INT:
      nargs = 6;
      nargs_constant = 3;
      break;
    default:
      gcc_unreachable ();
    }
  gcc_assert (nargs <= ARRAY_SIZE (xops));

  if (optimize
      || target == 0
      || GET_MODE (target) != tmode
      || !insn_p->operand[0].predicate (target, tmode))
    target = gen_reg_rtx (tmode);

  for (i = 0; i < nargs; i++)
    {
      tree arg = CALL_EXPR_ARG (exp, i);
      rtx op = expand_normal (arg);
      machine_mode mode = insn_p->operand[i + 1].mode;
      bool match = insn_p->operand[i + 1].predicate (op, mode);

      if (i == nargs - nargs_constant)
	{
	  if (!match)
	    {
	      switch (icode)
		{
		case CODE_FOR_avx512f_getmantv8df_mask_round:
		case CODE_FOR_avx512f_getmantv16sf_mask_round:
		case CODE_FOR_avx512bw_getmantv32hf_mask_round:
		case CODE_FOR_avx512f_vgetmantv2df_round:
		case CODE_FOR_avx512f_vgetmantv2df_mask_round:
		case CODE_FOR_avx512f_vgetmantv4sf_round:
		case CODE_FOR_avx512f_vgetmantv4sf_mask_round:
		case CODE_FOR_avx512f_vgetmantv8hf_mask_round:
		  error ("the immediate argument must be a 4-bit immediate");
		  return const0_rtx;
		case CODE_FOR_avx512f_cmpv8df3_mask_round:
		case CODE_FOR_avx512f_cmpv16sf3_mask_round:
		case CODE_FOR_avx512f_vmcmpv2df3_mask_round:
		case CODE_FOR_avx512f_vmcmpv4sf3_mask_round:
		case CODE_FOR_avx512f_vmcmpv8hf3_mask_round:
		case CODE_FOR_avx512bw_cmpv32hf3_mask_round:
		  error ("the immediate argument must be a 5-bit immediate");
		  return const0_rtx;
		default:
		  error ("the immediate argument must be an 8-bit immediate");
		  return const0_rtx;
		}
	    }
	}
      else if (i == nargs-1)
	{
	  if (!insn_p->operand[nargs].predicate (op, SImode))
	    {
	      error ("incorrect rounding operand");
	      return const0_rtx;
	    }

	  /* If there is no rounding use normal version of the pattern.  */
	  if (INTVAL (op) == NO_ROUND)
	    {
	      /* Skip erasing embedded rounding for below expanders who
		 generates multiple insns.  In ix86_erase_embedded_rounding
		 the pattern will be transformed to a single set, and emit_insn
		 appends the set insead of insert it to chain.  So the insns
		 emitted inside define_expander would be ignored.  */
	      switch (icode)
		{
		case CODE_FOR_avx512bw_fmaddc_v32hf_mask1_round:
		case CODE_FOR_avx512bw_fcmaddc_v32hf_mask1_round:
		case CODE_FOR_avx512fp16_fmaddcsh_v8hf_mask1_round:
		case CODE_FOR_avx512fp16_fcmaddcsh_v8hf_mask1_round:
		case CODE_FOR_avx512fp16_fmaddcsh_v8hf_mask3_round:
		case CODE_FOR_avx512fp16_fcmaddcsh_v8hf_mask3_round:
		  redundant_embed_rnd = 0;
		  break;
		default:
		  redundant_embed_rnd = 1;
		  break;
		}
	    }
	}
      else
	{
	  if (VECTOR_MODE_P (mode))
	    op = safe_vector_operand (op, mode);

	  op = fixup_modeless_constant (op, mode);

	  if (GET_MODE (op) == mode || GET_MODE (op) == VOIDmode)
	    {
	      if (optimize || !match)
		op = copy_to_mode_reg (mode, op);
	    }
	  else
	    {
	      op = copy_to_reg (op);
	      op = lowpart_subreg (mode, op, GET_MODE (op));
	    }
	}

      xops[i] = op;
    }

  switch (nargs)
    {
    case 1:
      pat = GEN_FCN (icode) (target, xops[0]);
      break;
    case 2:
      pat = GEN_FCN (icode) (target, xops[0], xops[1]);
      break;
    case 3:
      pat = GEN_FCN (icode) (target, xops[0], xops[1], xops[2]);
      break;
    case 4:
      pat = GEN_FCN (icode) (target, xops[0], xops[1],
			     xops[2], xops[3]);
      break;
    case 5:
      pat = GEN_FCN (icode) (target, xops[0], xops[1],
			     xops[2], xops[3], xops[4]);
      break;
    case 6:
      pat = GEN_FCN (icode) (target, xops[0], xops[1],
			     xops[2], xops[3], xops[4], xops[5]);
      break;
    default:
      gcc_unreachable ();
    }

  if (!pat)
    return 0;

  if (redundant_embed_rnd)
    pat = ix86_erase_embedded_rounding (pat);

  emit_insn (pat);
  return target;
}

/* Subroutine of ix86_expand_builtin to take care of special insns
   with variable number of operands.  */

static rtx
ix86_expand_special_args_builtin (const struct builtin_description *d,
				  tree exp, rtx target)
{
  tree arg;
  rtx pat, op;
  unsigned int i, nargs, arg_adjust, memory;
  unsigned int constant = 100;
  bool aligned_mem = false;
  rtx xops[4];
  enum insn_code icode = d->icode;
  const struct insn_data_d *insn_p = &insn_data[icode];
  machine_mode tmode = insn_p->operand[0].mode;
  enum { load, store } klass;

  switch ((enum ix86_builtin_func_type) d->flag)
    {
    case VOID_FTYPE_VOID:
      emit_insn (GEN_FCN (icode) (target));
      return 0;
    case VOID_FTYPE_UINT64:
    case VOID_FTYPE_UNSIGNED:
      nargs = 0;
      klass = store;
      memory = 0;
      break;

    case INT_FTYPE_VOID:
    case USHORT_FTYPE_VOID:
    case UINT64_FTYPE_VOID:
    case UINT_FTYPE_VOID:
    case UINT8_FTYPE_VOID:
    case UNSIGNED_FTYPE_VOID:
      nargs = 0;
      klass = load;
      memory = 0;
      break;
    case UINT64_FTYPE_PUNSIGNED:
    case V2DI_FTYPE_PV2DI:
    case V4DI_FTYPE_PV4DI:
    case V32QI_FTYPE_PCCHAR:
    case V16QI_FTYPE_PCCHAR:
    case V8SF_FTYPE_PCV4SF:
    case V8SF_FTYPE_PCFLOAT:
    case V4SF_FTYPE_PCFLOAT:
    case V4SF_FTYPE_PCFLOAT16:
    case V4SF_FTYPE_PCBFLOAT16:
    case V4SF_FTYPE_PCV8BF:
    case V4SF_FTYPE_PCV8HF:
    case V8SF_FTYPE_PCFLOAT16:
    case V8SF_FTYPE_PCBFLOAT16:
    case V8SF_FTYPE_PCV16HF:
    case V8SF_FTYPE_PCV16BF:
    case V4DF_FTYPE_PCV2DF:
    case V4DF_FTYPE_PCDOUBLE:
    case V2DF_FTYPE_PCDOUBLE:
    case VOID_FTYPE_PVOID:
    case V8DI_FTYPE_PV8DI:
      nargs = 1;
      klass = load;
      memory = 0;
      switch (icode)
	{
	case CODE_FOR_sse4_1_movntdqa:
	case CODE_FOR_avx2_movntdqa:
	case CODE_FOR_avx512f_movntdqa:
	  aligned_mem = true;
	  break;
	default:
	  break;
	}
      break;
    case VOID_FTYPE_PV2SF_V4SF:
    case VOID_FTYPE_PV8DI_V8DI:
    case VOID_FTYPE_PV4DI_V4DI:
    case VOID_FTYPE_PV2DI_V2DI:
    case VOID_FTYPE_PCHAR_V32QI:
    case VOID_FTYPE_PCHAR_V16QI:
    case VOID_FTYPE_PFLOAT_V16SF:
    case VOID_FTYPE_PFLOAT_V8SF:
    case VOID_FTYPE_PFLOAT_V4SF:
    case VOID_FTYPE_PDOUBLE_V8DF:
    case VOID_FTYPE_PDOUBLE_V4DF:
    case VOID_FTYPE_PDOUBLE_V2DF:
    case VOID_FTYPE_PLONGLONG_LONGLONG:
    case VOID_FTYPE_PULONGLONG_ULONGLONG:
    case VOID_FTYPE_PUNSIGNED_UNSIGNED:
    case VOID_FTYPE_PINT_INT:
      nargs = 1;
      klass = store;
      /* Reserve memory operand for target.  */
      memory = ARRAY_SIZE (xops);
      switch (icode)
	{
	/* These builtins and instructions require the memory
	   to be properly aligned.  */
	case CODE_FOR_avx_movntv4di:
	case CODE_FOR_sse2_movntv2di:
	case CODE_FOR_avx_movntv8sf:
	case CODE_FOR_sse_movntv4sf:
	case CODE_FOR_sse4a_vmmovntv4sf:
	case CODE_FOR_avx_movntv4df:
	case CODE_FOR_sse2_movntv2df:
	case CODE_FOR_sse4a_vmmovntv2df:
	case CODE_FOR_sse2_movntidi:
	case CODE_FOR_sse_movntq:
	case CODE_FOR_sse2_movntisi:
	case CODE_FOR_avx512f_movntv16sf:
	case CODE_FOR_avx512f_movntv8df:
	case CODE_FOR_avx512f_movntv8di:
	  aligned_mem = true;
	  break;
	default:
	  break;
	}
      break;
    case VOID_FTYPE_PVOID_PCVOID:
	nargs = 1;
	klass = store;
	memory = 0;

	break;
    case V4SF_FTYPE_V4SF_PCV2SF:
    case V2DF_FTYPE_V2DF_PCDOUBLE:
      nargs = 2;
      klass = load;
      memory = 1;
      break;
    case V8SF_FTYPE_PCV8SF_V8SI:
    case V4DF_FTYPE_PCV4DF_V4DI:
    case V4SF_FTYPE_PCV4SF_V4SI:
    case V2DF_FTYPE_PCV2DF_V2DI:
    case V8SI_FTYPE_PCV8SI_V8SI:
    case V4DI_FTYPE_PCV4DI_V4DI:
    case V4SI_FTYPE_PCV4SI_V4SI:
    case V2DI_FTYPE_PCV2DI_V2DI:
    case VOID_FTYPE_INT_INT64:
      nargs = 2;
      klass = load;
      memory = 0;
      break;
    case VOID_FTYPE_PV8DF_V8DF_UQI:
    case VOID_FTYPE_PV4DF_V4DF_UQI:
    case VOID_FTYPE_PV2DF_V2DF_UQI:
    case VOID_FTYPE_PV16SF_V16SF_UHI:
    case VOID_FTYPE_PV8SF_V8SF_UQI:
    case VOID_FTYPE_PV4SF_V4SF_UQI:
    case VOID_FTYPE_PV8DI_V8DI_UQI:
    case VOID_FTYPE_PV4DI_V4DI_UQI:
    case VOID_FTYPE_PV2DI_V2DI_UQI:
    case VOID_FTYPE_PV16SI_V16SI_UHI:
    case VOID_FTYPE_PV8SI_V8SI_UQI:
    case VOID_FTYPE_PV4SI_V4SI_UQI:
    case VOID_FTYPE_PV64QI_V64QI_UDI:
    case VOID_FTYPE_PV32HI_V32HI_USI:
    case VOID_FTYPE_PV32QI_V32QI_USI:
    case VOID_FTYPE_PV16QI_V16QI_UHI:
    case VOID_FTYPE_PV16HI_V16HI_UHI:
    case VOID_FTYPE_PV8HI_V8HI_UQI:
      switch (icode)
	{
	/* These builtins and instructions require the memory
	   to be properly aligned.  */
	case CODE_FOR_avx512f_storev16sf_mask:
	case CODE_FOR_avx512f_storev16si_mask:
	case CODE_FOR_avx512f_storev8df_mask:
	case CODE_FOR_avx512f_storev8di_mask:
	case CODE_FOR_avx512vl_storev8sf_mask:
	case CODE_FOR_avx512vl_storev8si_mask:
	case CODE_FOR_avx512vl_storev4df_mask:
	case CODE_FOR_avx512vl_storev4di_mask:
	case CODE_FOR_avx512vl_storev4sf_mask:
	case CODE_FOR_avx512vl_storev4si_mask:
	case CODE_FOR_avx512vl_storev2df_mask:
	case CODE_FOR_avx512vl_storev2di_mask:
	  aligned_mem = true;
	  break;
	default:
	  break;
	}
      /* FALLTHRU */
    case VOID_FTYPE_PV8SF_V8SI_V8SF:
    case VOID_FTYPE_PV4DF_V4DI_V4DF:
    case VOID_FTYPE_PV4SF_V4SI_V4SF:
    case VOID_FTYPE_PV2DF_V2DI_V2DF:
    case VOID_FTYPE_PV8SI_V8SI_V8SI:
    case VOID_FTYPE_PV4DI_V4DI_V4DI:
    case VOID_FTYPE_PV4SI_V4SI_V4SI:
    case VOID_FTYPE_PV2DI_V2DI_V2DI:
    case VOID_FTYPE_PV8SI_V8DI_UQI:
    case VOID_FTYPE_PV8HI_V8DI_UQI:
    case VOID_FTYPE_PV16HI_V16SI_UHI:
    case VOID_FTYPE_PUDI_V8DI_UQI:
    case VOID_FTYPE_PV16QI_V16SI_UHI:
    case VOID_FTYPE_PV4SI_V4DI_UQI:
    case VOID_FTYPE_PUDI_V2DI_UQI:
    case VOID_FTYPE_PUDI_V4DI_UQI:
    case VOID_FTYPE_PUSI_V2DI_UQI:
    case VOID_FTYPE_PV8HI_V8SI_UQI:
    case VOID_FTYPE_PUDI_V4SI_UQI:
    case VOID_FTYPE_PUSI_V4DI_UQI:
    case VOID_FTYPE_PUHI_V2DI_UQI:
    case VOID_FTYPE_PUDI_V8SI_UQI:
    case VOID_FTYPE_PUSI_V4SI_UQI:
    case VOID_FTYPE_PCHAR_V64QI_UDI:
    case VOID_FTYPE_PCHAR_V32QI_USI:
    case VOID_FTYPE_PCHAR_V16QI_UHI:
    case VOID_FTYPE_PSHORT_V32HI_USI:
    case VOID_FTYPE_PSHORT_V16HI_UHI:
    case VOID_FTYPE_PSHORT_V8HI_UQI:
    case VOID_FTYPE_PINT_V16SI_UHI:
    case VOID_FTYPE_PINT_V8SI_UQI:
    case VOID_FTYPE_PINT_V4SI_UQI:
    case VOID_FTYPE_PINT64_V8DI_UQI:
    case VOID_FTYPE_PINT64_V4DI_UQI:
    case VOID_FTYPE_PINT64_V2DI_UQI:
    case VOID_FTYPE_PDOUBLE_V8DF_UQI:
    case VOID_FTYPE_PDOUBLE_V4DF_UQI:
    case VOID_FTYPE_PDOUBLE_V2DF_UQI:
    case VOID_FTYPE_PFLOAT_V16SF_UHI:
    case VOID_FTYPE_PFLOAT_V8SF_UQI:
    case VOID_FTYPE_PFLOAT_V4SF_UQI:
    case VOID_FTYPE_PCFLOAT16_V8HF_UQI:
    case VOID_FTYPE_PV32QI_V32HI_USI:
    case VOID_FTYPE_PV16QI_V16HI_UHI:
    case VOID_FTYPE_PUDI_V8HI_UQI:
      nargs = 2;
      klass = store;
      /* Reserve memory operand for target.  */
      memory = ARRAY_SIZE (xops);
      break;
    case V4SF_FTYPE_PCV4SF_V4SF_UQI:
    case V8SF_FTYPE_PCV8SF_V8SF_UQI:
    case V16SF_FTYPE_PCV16SF_V16SF_UHI:
    case V4SI_FTYPE_PCV4SI_V4SI_UQI:
    case V8SI_FTYPE_PCV8SI_V8SI_UQI:
    case V16SI_FTYPE_PCV16SI_V16SI_UHI:
    case V2DF_FTYPE_PCV2DF_V2DF_UQI:
    case V4DF_FTYPE_PCV4DF_V4DF_UQI:
    case V8DF_FTYPE_PCV8DF_V8DF_UQI:
    case V2DI_FTYPE_PCV2DI_V2DI_UQI:
    case V4DI_FTYPE_PCV4DI_V4DI_UQI:
    case V8DI_FTYPE_PCV8DI_V8DI_UQI:
    case V64QI_FTYPE_PCV64QI_V64QI_UDI:
    case V32HI_FTYPE_PCV32HI_V32HI_USI:
    case V32QI_FTYPE_PCV32QI_V32QI_USI:
    case V16QI_FTYPE_PCV16QI_V16QI_UHI:
    case V16HI_FTYPE_PCV16HI_V16HI_UHI:
    case V8HI_FTYPE_PCV8HI_V8HI_UQI:
      switch (icode)
	{
	/* These builtins and instructions require the memory
	   to be properly aligned.  */
	case CODE_FOR_avx512f_loadv16sf_mask:
	case CODE_FOR_avx512f_loadv16si_mask:
	case CODE_FOR_avx512f_loadv8df_mask:
	case CODE_FOR_avx512f_loadv8di_mask:
	case CODE_FOR_avx512vl_loadv8sf_mask:
	case CODE_FOR_avx512vl_loadv8si_mask:
	case CODE_FOR_avx512vl_loadv4df_mask:
	case CODE_FOR_avx512vl_loadv4di_mask:
	case CODE_FOR_avx512vl_loadv4sf_mask:
	case CODE_FOR_avx512vl_loadv4si_mask:
	case CODE_FOR_avx512vl_loadv2df_mask:
	case CODE_FOR_avx512vl_loadv2di_mask:
	case CODE_FOR_avx512bw_loadv64qi_mask:
	case CODE_FOR_avx512vl_loadv32qi_mask:
	case CODE_FOR_avx512vl_loadv16qi_mask:
	case CODE_FOR_avx512bw_loadv32hi_mask:
	case CODE_FOR_avx512vl_loadv16hi_mask:
	case CODE_FOR_avx512vl_loadv8hi_mask:
	  aligned_mem = true;
	  break;
	default:
	  break;
	}
      /* FALLTHRU */
    case V64QI_FTYPE_PCCHAR_V64QI_UDI:
    case V32QI_FTYPE_PCCHAR_V32QI_USI:
    case V16QI_FTYPE_PCCHAR_V16QI_UHI:
    case V32HI_FTYPE_PCSHORT_V32HI_USI:
    case V16HI_FTYPE_PCSHORT_V16HI_UHI:
    case V8HI_FTYPE_PCSHORT_V8HI_UQI:
    case V16SI_FTYPE_PCINT_V16SI_UHI:
    case V8SI_FTYPE_PCINT_V8SI_UQI:
    case V4SI_FTYPE_PCINT_V4SI_UQI:
    case V8DI_FTYPE_PCINT64_V8DI_UQI:
    case V4DI_FTYPE_PCINT64_V4DI_UQI:
    case V2DI_FTYPE_PCINT64_V2DI_UQI:
    case V8DF_FTYPE_PCDOUBLE_V8DF_UQI:
    case V4DF_FTYPE_PCDOUBLE_V4DF_UQI:
    case V2DF_FTYPE_PCDOUBLE_V2DF_UQI:
    case V16SF_FTYPE_PCFLOAT_V16SF_UHI:
    case V8SF_FTYPE_PCFLOAT_V8SF_UQI:
    case V4SF_FTYPE_PCFLOAT_V4SF_UQI:
    case V8HF_FTYPE_PCFLOAT16_V8HF_UQI:
      nargs = 3;
      klass = load;
      memory = 0;
      break;
    case INT_FTYPE_PINT_INT_INT_INT:
    case LONGLONG_FTYPE_PLONGLONG_LONGLONG_LONGLONG_INT:
      nargs = 4;
      klass = load;
      memory = 0;
      constant = 3;
      break;
    default:
      gcc_unreachable ();
    }

  gcc_assert (nargs <= ARRAY_SIZE (xops));

  if (klass == store)
    {
      arg = CALL_EXPR_ARG (exp, 0);
      op = expand_normal (arg);
      gcc_assert (target == 0);
      if (memory)
	{
	  op = ix86_zero_extend_to_Pmode (op);
	  target = gen_rtx_MEM (tmode, op);
	  /* target at this point has just BITS_PER_UNIT MEM_ALIGN
	     on it.  Try to improve it using get_pointer_alignment,
	     and if the special builtin is one that requires strict
	     mode alignment, also from it's GET_MODE_ALIGNMENT.
	     Failure to do so could lead to ix86_legitimate_combined_insn
	     rejecting all changes to such insns.  */
	  unsigned int align = get_pointer_alignment (arg);
	  if (aligned_mem && align < GET_MODE_ALIGNMENT (tmode))
	    align = GET_MODE_ALIGNMENT (tmode);
	  if (MEM_ALIGN (target) < align)
	    set_mem_align (target, align);
	}
      else
	target = force_reg (tmode, op);
      arg_adjust = 1;
    }
  else
    {
      arg_adjust = 0;
      if (optimize
	  || target == 0
	  || !register_operand (target, tmode)
	  || GET_MODE (target) != tmode)
	target = gen_reg_rtx (tmode);
    }

  for (i = 0; i < nargs; i++)
    {
      machine_mode mode = insn_p->operand[i + 1].mode;

      arg = CALL_EXPR_ARG (exp, i + arg_adjust);
      op = expand_normal (arg);

      if (i == memory)
	{
	  /* This must be the memory operand.  */
	  op = ix86_zero_extend_to_Pmode (op);
	  op = gen_rtx_MEM (mode, op);
	  /* op at this point has just BITS_PER_UNIT MEM_ALIGN
	     on it.  Try to improve it using get_pointer_alignment,
	     and if the special builtin is one that requires strict
	     mode alignment, also from it's GET_MODE_ALIGNMENT.
	     Failure to do so could lead to ix86_legitimate_combined_insn
	     rejecting all changes to such insns.  */
	  unsigned int align = get_pointer_alignment (arg);
	  if (aligned_mem && align < GET_MODE_ALIGNMENT (mode))
	    align = GET_MODE_ALIGNMENT (mode);
	  if (MEM_ALIGN (op) < align)
	    set_mem_align (op, align);
	}
      else if (i == constant)
	{
	  /* This must be the constant.  */
	  if (!insn_p->operand[nargs].predicate(op, SImode))
	    {
	      error ("the fourth argument must be one of enum %qs", "_CMPCCX_ENUM");
	      return const0_rtx;
	    }
	}
      else
	{
	  /* This must be register.  */
	  if (VECTOR_MODE_P (mode))
	    op = safe_vector_operand (op, mode);

	  op = fixup_modeless_constant (op, mode);

	  /* NB: 3-operands load implied it's a mask load or v{p}expand*,
	     and that mask operand shoud be at the end.
	     Keep all-ones mask which would be simplified by the expander.  */
	  if (nargs == 3 && i == 2 && klass == load
	      && constm1_operand (op, mode)
	      && insn_p->operand[i].predicate (op, mode))
	    ;
	  else if (GET_MODE (op) == mode || GET_MODE (op) == VOIDmode)
	    op = copy_to_mode_reg (mode, op);
	  else
	    {
	      op = copy_to_reg (op);
	      op = lowpart_subreg (mode, op, GET_MODE (op));
	    }
	}

      xops[i]= op;
    }

  switch (nargs)
    {
    case 0:
      pat = GEN_FCN (icode) (target);
      break;
    case 1:
      pat = GEN_FCN (icode) (target, xops[0]);
      break;
    case 2:
      pat = GEN_FCN (icode) (target, xops[0], xops[1]);
      break;
    case 3:
      pat = GEN_FCN (icode) (target, xops[0], xops[1], xops[2]);
      break;
    case 4:
      pat = GEN_FCN (icode) (target, xops[0], xops[1], xops[2], xops[3]);
      break;
    default:
      gcc_unreachable ();
    }

  if (! pat)
    return 0;

  emit_insn (pat);
  return klass == store ? 0 : target;
}

/* Return the integer constant in ARG.  Constrain it to be in the range
   of the subparts of VEC_TYPE; issue an error if not.  */

static int
get_element_number (tree vec_type, tree arg)
{
  unsigned HOST_WIDE_INT elt, max = TYPE_VECTOR_SUBPARTS (vec_type) - 1;

  if (!tree_fits_uhwi_p (arg)
      || (elt = tree_to_uhwi (arg), elt > max))
    {
      error ("selector must be an integer constant in the range "
	     "[0, %wi]", max);
      return 0;
    }

  return elt;
}

/* A subroutine of ix86_expand_builtin.  These builtins are a wrapper around
   ix86_expand_vector_init.  We DO have language-level syntax for this, in
   the form of  (type){ init-list }.  Except that since we can't place emms
   instructions from inside the compiler, we can't allow the use of MMX
   registers unless the user explicitly asks for it.  So we do *not* define
   vec_set/vec_extract/vec_init patterns for MMX modes in mmx.md.  Instead
   we have builtins invoked by mmintrin.h that gives us license to emit
   these sorts of instructions.  */

static rtx
ix86_expand_vec_init_builtin (tree type, tree exp, rtx target)
{
  machine_mode tmode = TYPE_MODE (type);
  machine_mode inner_mode = GET_MODE_INNER (tmode);
  int i, n_elt = GET_MODE_NUNITS (tmode);
  rtvec v = rtvec_alloc (n_elt);

  gcc_assert (VECTOR_MODE_P (tmode));
  gcc_assert (call_expr_nargs (exp) == n_elt);

  for (i = 0; i < n_elt; ++i)
    {
      rtx x = expand_normal (CALL_EXPR_ARG (exp, i));
      RTVEC_ELT (v, i) = gen_lowpart (inner_mode, x);
    }

  if (!target || !register_operand (target, tmode))
    target = gen_reg_rtx (tmode);

  ix86_expand_vector_init (true, target, gen_rtx_PARALLEL (tmode, v));
  return target;
}

/* A subroutine of ix86_expand_builtin.  These builtins are a wrapper around
   ix86_expand_vector_extract.  They would be redundant (for non-MMX) if we
   had a language-level syntax for referencing vector elements.  */

static rtx
ix86_expand_vec_ext_builtin (tree exp, rtx target)
{
  machine_mode tmode, mode0;
  tree arg0, arg1;
  int elt;
  rtx op0;

  arg0 = CALL_EXPR_ARG (exp, 0);
  arg1 = CALL_EXPR_ARG (exp, 1);

  op0 = expand_normal (arg0);
  elt = get_element_number (TREE_TYPE (arg0), arg1);

  tmode = TYPE_MODE (TREE_TYPE (TREE_TYPE (arg0)));
  mode0 = TYPE_MODE (TREE_TYPE (arg0));
  gcc_assert (VECTOR_MODE_P (mode0));

  op0 = force_reg (mode0, op0);

  if (optimize || !target || !register_operand (target, tmode))
    target = gen_reg_rtx (tmode);

  ix86_expand_vector_extract (true, target, op0, elt);

  return target;
}

/* A subroutine of ix86_expand_builtin.  These builtins are a wrapper around
   ix86_expand_vector_set.  They would be redundant (for non-MMX) if we had
   a language-level syntax for referencing vector elements.  */

static rtx
ix86_expand_vec_set_builtin (tree exp)
{
  machine_mode tmode, mode1;
  tree arg0, arg1, arg2;
  int elt;
  rtx op0, op1, target;

  arg0 = CALL_EXPR_ARG (exp, 0);
  arg1 = CALL_EXPR_ARG (exp, 1);
  arg2 = CALL_EXPR_ARG (exp, 2);

  tmode = TYPE_MODE (TREE_TYPE (arg0));
  mode1 = TYPE_MODE (TREE_TYPE (TREE_TYPE (arg0)));
  gcc_assert (VECTOR_MODE_P (tmode));

  op0 = expand_expr (arg0, NULL_RTX, tmode, EXPAND_NORMAL);
  op1 = expand_expr (arg1, NULL_RTX, mode1, EXPAND_NORMAL);
  elt = get_element_number (TREE_TYPE (arg0), arg2);

  if (GET_MODE (op1) != mode1)
    op1 = convert_modes (mode1, GET_MODE (op1), op1, true);

  op0 = force_reg (tmode, op0);
  op1 = force_reg (mode1, op1);

  /* OP0 is the source of these builtin functions and shouldn't be
     modified.  Create a copy, use it and return it as target.  */
  target = gen_reg_rtx (tmode);
  emit_move_insn (target, op0);
  ix86_expand_vector_set (true, target, op1, elt);

  return target;
}

/* Return true if the necessary isa options for this builtin exist,
   else false.
   fcode = DECL_MD_FUNCTION_CODE (fndecl);  */
bool
ix86_check_builtin_isa_match (unsigned int fcode,
			      HOST_WIDE_INT* pbisa,
			      HOST_WIDE_INT* pbisa2)
{
  HOST_WIDE_INT isa = ix86_isa_flags;
  HOST_WIDE_INT isa2 = ix86_isa_flags2;
  HOST_WIDE_INT bisa = ix86_builtins_isa[fcode].isa;
  HOST_WIDE_INT bisa2 = ix86_builtins_isa[fcode].isa2;
  HOST_WIDE_INT tmp_isa = isa, tmp_isa2 = isa2;
  /* The general case is we require all the ISAs specified in bisa{,2}
     to be enabled.
     The exceptions are:
     OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_3DNOW_A
     OPTION_MASK_ISA_SSE4_2 | OPTION_MASK_ISA_CRC32
     OPTION_MASK_ISA_FMA | OPTION_MASK_ISA_FMA4
     (OPTION_MASK_ISA_AVX512VNNI | OPTION_MASK_ISA_AVX512VL) or
       OPTION_MASK_ISA2_AVXVNNI
     (OPTION_MASK_ISA_AVX512IFMA | OPTION_MASK_ISA_AVX512VL) or
       OPTION_MASK_ISA2_AVXIFMA
     (OPTION_MASK_ISA_AVX512VL | OPTION_MASK_ISA2_AVX512BF16) or
       OPTION_MASK_ISA2_AVXNECONVERT
     OPTION_MASK_ISA_AES or (OPTION_MASK_ISA_AVX512VL | OPTION_MASK_ISA2_VAES)
     where for each such pair it is sufficient if either of the ISAs is
     enabled, plus if it is ored with other options also those others.
     OPTION_MASK_ISA_MMX in bisa is satisfied also if TARGET_MMX_WITH_SSE.  */

#define SHARE_BUILTIN(A1, A2, B1, B2) \
  if ((((bisa & (A1)) == (A1) && (bisa2 & (A2)) == (A2)) \
       && ((bisa & (B1)) == (B1) && (bisa2 & (B2)) == (B2))) \
      && (((isa & (A1)) == (A1) && (isa2 & (A2)) == (A2)) \
	  || ((isa & (B1)) == (B1) && (isa2 & (B2)) == (B2)))) \
    { \
      tmp_isa |= (A1) | (B1); \
      tmp_isa2 |= (A2) | (B2); \
    }

  SHARE_BUILTIN (OPTION_MASK_ISA_SSE, 0, OPTION_MASK_ISA_3DNOW_A, 0);
  SHARE_BUILTIN (OPTION_MASK_ISA_SSE4_2, 0, OPTION_MASK_ISA_CRC32, 0);
  SHARE_BUILTIN (OPTION_MASK_ISA_FMA, 0, OPTION_MASK_ISA_FMA4, 0);
  SHARE_BUILTIN (OPTION_MASK_ISA_AVX512VNNI | OPTION_MASK_ISA_AVX512VL, 0, 0,
		 OPTION_MASK_ISA2_AVXVNNI);
  SHARE_BUILTIN (OPTION_MASK_ISA_AVX512IFMA | OPTION_MASK_ISA_AVX512VL, 0, 0,
		 OPTION_MASK_ISA2_AVXIFMA);
  SHARE_BUILTIN (OPTION_MASK_ISA_AVX512VL, OPTION_MASK_ISA2_AVX512BF16, 0,
		 OPTION_MASK_ISA2_AVXNECONVERT);
  SHARE_BUILTIN (OPTION_MASK_ISA_AES, 0, OPTION_MASK_ISA_AVX512VL,
		 OPTION_MASK_ISA2_VAES);
  isa = tmp_isa;
  isa2 = tmp_isa2;

  if ((bisa & OPTION_MASK_ISA_MMX) && !TARGET_MMX && TARGET_MMX_WITH_SSE
      /* __builtin_ia32_maskmovq requires MMX registers.  */
      && fcode != IX86_BUILTIN_MASKMOVQ)
    {
      bisa &= ~OPTION_MASK_ISA_MMX;
      bisa |= OPTION_MASK_ISA_SSE2;
    }

  if (pbisa)
    *pbisa = bisa;
  if (pbisa2)
    *pbisa2 = bisa2;

  return (bisa & isa) == bisa && (bisa2 & isa2) == bisa2;
}

/* Emit instructions to set the carry flag from ARG.  */

void
ix86_expand_carry (rtx arg)
{
  if (!CONST_INT_P (arg) || arg == const0_rtx)
    {
      arg = convert_to_mode (QImode, arg, 1);
      arg = copy_to_mode_reg (QImode, arg);
      emit_insn (gen_addqi3_cconly_overflow (arg, constm1_rtx));
    }
  else
    emit_insn (gen_x86_stc ());
}

/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

rtx
ix86_expand_builtin (tree exp, rtx target, rtx subtarget,
		     machine_mode mode, int ignore)
{
  size_t i;
  enum insn_code icode, icode2;
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  tree arg0, arg1, arg2, arg3, arg4;
  rtx op0, op1, op2, op3, op4, pat, pat2, insn;
  machine_mode mode0, mode1, mode2, mode3, mode4;
  unsigned int fcode = DECL_MD_FUNCTION_CODE (fndecl);
  HOST_WIDE_INT bisa, bisa2;

  /* For CPU builtins that can be folded, fold first and expand the fold.  */
  switch (fcode)
    {
    case IX86_BUILTIN_CPU_INIT:
      {
	/* Make it call __cpu_indicator_init in libgcc.  */
	tree call_expr, fndecl, type;
	type = build_function_type_list (integer_type_node, NULL_TREE);
	fndecl = build_fn_decl ("__cpu_indicator_init", type);
	call_expr = build_call_expr (fndecl, 0);
	return expand_expr (call_expr, target, mode, EXPAND_NORMAL);
      }
    case IX86_BUILTIN_CPU_IS:
    case IX86_BUILTIN_CPU_SUPPORTS:
      {
	tree arg0 = CALL_EXPR_ARG (exp, 0);
	tree fold_expr = fold_builtin_cpu (fndecl, &arg0);
	gcc_assert (fold_expr != NULL_TREE);
	return expand_expr (fold_expr, target, mode, EXPAND_NORMAL);
      }
    }

  if (!ix86_check_builtin_isa_match (fcode, &bisa, &bisa2))
    {
      bool add_abi_p = bisa & OPTION_MASK_ISA_64BIT;
      if (TARGET_ABI_X32)
	bisa |= OPTION_MASK_ABI_X32;
      else
	bisa |= OPTION_MASK_ABI_64;
      char *opts = ix86_target_string (bisa, bisa2, 0, 0, NULL, NULL,
				       (enum fpmath_unit) 0,
				       (enum prefer_vector_width) 0,
				       PVW_NONE, PVW_NONE,
				       false, add_abi_p);
      if (!opts)
	error ("%qE needs unknown isa option", fndecl);
      else
	{
	  gcc_assert (opts != NULL);
	  error ("%qE needs isa option %s", fndecl, opts);
	  free (opts);
	}
      return expand_call (exp, target, ignore);
    }

  switch (fcode)
    {
    case IX86_BUILTIN_MASKMOVQ:
    case IX86_BUILTIN_MASKMOVDQU:
      icode = (fcode == IX86_BUILTIN_MASKMOVQ
	       ? CODE_FOR_mmx_maskmovq
	       : CODE_FOR_sse2_maskmovdqu);
      /* Note the arg order is different from the operand order.  */
      arg1 = CALL_EXPR_ARG (exp, 0);
      arg2 = CALL_EXPR_ARG (exp, 1);
      arg0 = CALL_EXPR_ARG (exp, 2);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
      op2 = expand_normal (arg2);
      mode0 = insn_data[icode].operand[0].mode;
      mode1 = insn_data[icode].operand[1].mode;
      mode2 = insn_data[icode].operand[2].mode;

      op0 = ix86_zero_extend_to_Pmode (op0);
      op0 = gen_rtx_MEM (mode1, op0);

      if (!insn_data[icode].operand[0].predicate (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);
      if (!insn_data[icode].operand[1].predicate (op1, mode1))
	op1 = copy_to_mode_reg (mode1, op1);
      if (!insn_data[icode].operand[2].predicate (op2, mode2))
	op2 = copy_to_mode_reg (mode2, op2);
      pat = GEN_FCN (icode) (op0, op1, op2);
      if (! pat)
	return 0;
      emit_insn (pat);
      return 0;

    case IX86_BUILTIN_LDMXCSR:
      op0 = expand_normal (CALL_EXPR_ARG (exp, 0));
      target = assign_386_stack_local (SImode, SLOT_TEMP);
      emit_move_insn (target, op0);
      emit_insn (gen_sse_ldmxcsr (target));
      return 0;

    case IX86_BUILTIN_STMXCSR:
      target = assign_386_stack_local (SImode, SLOT_TEMP);
      emit_insn (gen_sse_stmxcsr (target));
      return copy_to_mode_reg (SImode, target);

    case IX86_BUILTIN_CLFLUSH:
	arg0 = CALL_EXPR_ARG (exp, 0);
	op0 = expand_normal (arg0);
	icode = CODE_FOR_sse2_clflush;
	if (!insn_data[icode].operand[0].predicate (op0, Pmode))
	  op0 = ix86_zero_extend_to_Pmode (op0);

	emit_insn (gen_sse2_clflush (op0));
	return 0;

    case IX86_BUILTIN_CLWB:
	arg0 = CALL_EXPR_ARG (exp, 0);
	op0 = expand_normal (arg0);
	icode = CODE_FOR_clwb;
	if (!insn_data[icode].operand[0].predicate (op0, Pmode))
	  op0 = ix86_zero_extend_to_Pmode (op0);

	emit_insn (gen_clwb (op0));
	return 0;

    case IX86_BUILTIN_CLFLUSHOPT:
	arg0 = CALL_EXPR_ARG (exp, 0);
	op0 = expand_normal (arg0);
	icode = CODE_FOR_clflushopt;
	if (!insn_data[icode].operand[0].predicate (op0, Pmode))
	  op0 = ix86_zero_extend_to_Pmode (op0);

	emit_insn (gen_clflushopt (op0));
	return 0;

    case IX86_BUILTIN_MONITOR:
    case IX86_BUILTIN_MONITORX:
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      arg2 = CALL_EXPR_ARG (exp, 2);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
      op2 = expand_normal (arg2);
      if (!REG_P (op0))
	op0 = ix86_zero_extend_to_Pmode (op0);
      if (!REG_P (op1))
	op1 = copy_to_mode_reg (SImode, op1);
      if (!REG_P (op2))
	op2 = copy_to_mode_reg (SImode, op2);

      emit_insn (fcode == IX86_BUILTIN_MONITOR 
		 ? gen_sse3_monitor (Pmode, op0, op1, op2)
		 : gen_monitorx (Pmode, op0, op1, op2));
      return 0;

    case IX86_BUILTIN_MWAIT:
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
      if (!REG_P (op0))
	op0 = copy_to_mode_reg (SImode, op0);
      if (!REG_P (op1))
	op1 = copy_to_mode_reg (SImode, op1);
      emit_insn (gen_sse3_mwait (op0, op1));
      return 0;

    case IX86_BUILTIN_MWAITX:
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      arg2 = CALL_EXPR_ARG (exp, 2);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
      op2 = expand_normal (arg2);
      if (!REG_P (op0))
	op0 = copy_to_mode_reg (SImode, op0);
      if (!REG_P (op1))
	op1 = copy_to_mode_reg (SImode, op1);
      if (!REG_P (op2))
	op2 = copy_to_mode_reg (SImode, op2);
      emit_insn (gen_mwaitx (op0, op1, op2));
      return 0;

    case IX86_BUILTIN_UMONITOR:
      arg0 = CALL_EXPR_ARG (exp, 0);
      op0 = expand_normal (arg0);

      op0 = ix86_zero_extend_to_Pmode (op0);
      emit_insn (gen_umonitor (Pmode, op0));
      return 0;

    case IX86_BUILTIN_UMWAIT:
    case IX86_BUILTIN_TPAUSE:
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);

      if (!REG_P (op0))
	op0 = copy_to_mode_reg (SImode, op0);

      op1 = force_reg (DImode, op1);

      if (TARGET_64BIT)
	{
	  op2 = expand_simple_binop (DImode, LSHIFTRT, op1, GEN_INT (32),
				     NULL, 1, OPTAB_DIRECT);
	  switch (fcode)
	    {
	    case IX86_BUILTIN_UMWAIT:
	      icode = CODE_FOR_umwait_rex64;
	      break;
	    case IX86_BUILTIN_TPAUSE:
	      icode = CODE_FOR_tpause_rex64;
	      break;
	    default:
	      gcc_unreachable ();
	    }

	  op2 = gen_lowpart (SImode, op2);
	  op1 = gen_lowpart (SImode, op1);
	  pat = GEN_FCN (icode) (op0, op1, op2);
	}
      else
	{
	  switch (fcode)
	    {
	    case IX86_BUILTIN_UMWAIT:
	      icode = CODE_FOR_umwait;
	      break;
	    case IX86_BUILTIN_TPAUSE:
	      icode = CODE_FOR_tpause;
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  pat = GEN_FCN (icode) (op0, op1);
	}

      if (!pat)
	return 0;

      emit_insn (pat);

      if (target == 0
	  || !register_operand (target, QImode))
	target = gen_reg_rtx (QImode);

      pat = gen_rtx_EQ (QImode, gen_rtx_REG (CCCmode, FLAGS_REG),
			const0_rtx);
      emit_insn (gen_rtx_SET (target, pat));

      return target;

    case IX86_BUILTIN_TESTUI:
      emit_insn (gen_testui ());

      if (target == 0
	  || !register_operand (target, QImode))
	target = gen_reg_rtx (QImode);

      pat = gen_rtx_LTU (QImode, gen_rtx_REG (CCCmode, FLAGS_REG),
			 const0_rtx);
      emit_insn (gen_rtx_SET (target, pat));

      return target;

    case IX86_BUILTIN_CLZERO:
      arg0 = CALL_EXPR_ARG (exp, 0);
      op0 = expand_normal (arg0);
      if (!REG_P (op0))
	op0 = ix86_zero_extend_to_Pmode (op0);
      emit_insn (gen_clzero (Pmode, op0));
      return 0;

    case IX86_BUILTIN_CLDEMOTE:
      arg0 = CALL_EXPR_ARG (exp, 0);
      op0 = expand_normal (arg0);
      icode = CODE_FOR_cldemote;
      if (!insn_data[icode].operand[0].predicate (op0, Pmode))
	op0 = ix86_zero_extend_to_Pmode (op0);

      emit_insn (gen_cldemote (op0));
      return 0;

    case IX86_BUILTIN_LOADIWKEY:
      {
	arg0 = CALL_EXPR_ARG (exp, 0);
	arg1 = CALL_EXPR_ARG (exp, 1);
	arg2 = CALL_EXPR_ARG (exp, 2);
	arg3 = CALL_EXPR_ARG (exp, 3);

	op0 = expand_normal (arg0);
	op1 = expand_normal (arg1);
	op2 = expand_normal (arg2);
	op3 = expand_normal (arg3);

	if (!REG_P (op0))
	  op0 = copy_to_mode_reg (V2DImode, op0);
	if (!REG_P (op1))
	  op1 = copy_to_mode_reg (V2DImode, op1);
	if (!REG_P (op2))
	  op2 = copy_to_mode_reg (V2DImode, op2);
	if (!REG_P (op3))
	  op3 = copy_to_mode_reg (SImode, op3);

	emit_insn (gen_loadiwkey (op0, op1, op2, op3));

	return 0;
      }

    case IX86_BUILTIN_AESDEC128KLU8:
      icode = CODE_FOR_aesdec128klu8;
      goto aesdecenc_expand;

    case IX86_BUILTIN_AESDEC256KLU8:
      icode = CODE_FOR_aesdec256klu8;
      goto aesdecenc_expand;

    case IX86_BUILTIN_AESENC128KLU8:
      icode = CODE_FOR_aesenc128klu8;
      goto aesdecenc_expand;

    case IX86_BUILTIN_AESENC256KLU8:
      icode = CODE_FOR_aesenc256klu8;

    aesdecenc_expand:

      arg0 = CALL_EXPR_ARG (exp, 0); // __m128i *odata
      arg1 = CALL_EXPR_ARG (exp, 1); // __m128i idata
      arg2 = CALL_EXPR_ARG (exp, 2); // const void *p

      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
      op2 = expand_normal (arg2);

      if (!address_operand (op0, V2DImode))
	{
	  op0 = convert_memory_address (Pmode, op0);
	  op0 = copy_addr_to_reg (op0);
	}
      op0 = gen_rtx_MEM (V2DImode, op0);

      if (!REG_P (op1))
	op1 = copy_to_mode_reg (V2DImode, op1);

      if (!address_operand (op2, VOIDmode))
	{
	  op2 = convert_memory_address (Pmode, op2);
	  op2 = copy_addr_to_reg (op2);
	}
      op2 = gen_rtx_MEM (BLKmode, op2);

      emit_insn (GEN_FCN (icode) (op1, op1, op2));

      if (target == 0)
	target = gen_reg_rtx (QImode);

      /* NB: For aesenc/aesdec keylocker insn, ZF will be set when runtime
	 error occurs. Then the output should be cleared for safety. */
      rtx_code_label *ok_label;
      rtx tmp;

      tmp = gen_rtx_REG (CCZmode, FLAGS_REG);
      pat = gen_rtx_EQ (QImode, tmp, const0_rtx);
      ok_label = gen_label_rtx ();
      emit_cmp_and_jump_insns (tmp, const0_rtx, NE, 0, GET_MODE (tmp),
			       true, ok_label);
      /* Usually the runtime error seldom occur, so predict OK path as
	 hotspot to optimize it as fallthrough block. */
      predict_jump (REG_BR_PROB_BASE * 90 / 100);

      emit_insn (gen_rtx_SET (op1, const0_rtx));

      emit_label (ok_label);
      emit_insn (gen_rtx_SET (target, pat));
      emit_insn (gen_rtx_SET (op0, op1));

      return target;

    case IX86_BUILTIN_AESDECWIDE128KLU8:
      icode = CODE_FOR_aesdecwide128klu8;
      goto wideaesdecenc_expand;

    case IX86_BUILTIN_AESDECWIDE256KLU8:
      icode = CODE_FOR_aesdecwide256klu8;
      goto wideaesdecenc_expand;

    case IX86_BUILTIN_AESENCWIDE128KLU8:
      icode = CODE_FOR_aesencwide128klu8;
      goto wideaesdecenc_expand;

    case IX86_BUILTIN_AESENCWIDE256KLU8:
      icode = CODE_FOR_aesencwide256klu8;

    wideaesdecenc_expand:

      rtx xmm_regs[8];
      rtx op;

      arg0 = CALL_EXPR_ARG (exp, 0); // __m128i * odata
      arg1 = CALL_EXPR_ARG (exp, 1); // const __m128i * idata
      arg2 = CALL_EXPR_ARG (exp, 2); // const void *p

      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
      op2 = expand_normal (arg2);

      if (!address_operand (op2, VOIDmode))
	{
	  op2 = convert_memory_address (Pmode, op2);
	  op2 = copy_addr_to_reg (op2);
	}
      op2 = gen_rtx_MEM (BLKmode, op2);

      for (i = 0; i < 8; i++)
	{
	  xmm_regs[i] = gen_rtx_REG (V2DImode, GET_SSE_REGNO (i));

	  op = gen_rtx_MEM (V2DImode,
			    plus_constant (Pmode, op1, (i * 16)));

	  emit_move_insn (xmm_regs[i], op);
	}

      emit_insn (GEN_FCN (icode) (op2));

      if (target == 0)
	target = gen_reg_rtx (QImode);

      tmp = gen_rtx_REG (CCZmode, FLAGS_REG);
      pat = gen_rtx_EQ (QImode, tmp, const0_rtx);
      ok_label = gen_label_rtx ();
      emit_cmp_and_jump_insns (tmp, const0_rtx, NE, 0, GET_MODE (tmp),
			       true, ok_label);
      predict_jump (REG_BR_PROB_BASE * 90 / 100);

      for (i = 0; i < 8; i++)
	emit_insn (gen_rtx_SET (xmm_regs[i], const0_rtx));

      emit_label (ok_label);
      emit_insn (gen_rtx_SET (target, pat));

      for (i = 0; i < 8; i++)
	{
	  op = gen_rtx_MEM (V2DImode,
			    plus_constant (Pmode, op0, (i * 16)));
	  emit_move_insn (op, xmm_regs[i]);
	}

      return target;

    case IX86_BUILTIN_ENCODEKEY128U32:
      {
	rtx op, xmm_regs[7];

	arg0 = CALL_EXPR_ARG (exp, 0); // unsigned int htype
	arg1 = CALL_EXPR_ARG (exp, 1); // __m128i key
	arg2 = CALL_EXPR_ARG (exp, 2); // void *h

	op0 = expand_normal (arg0);
	op1 = expand_normal (arg1);
	op2 = expand_normal (arg2);

	if (!REG_P (op0))
	  op0 = copy_to_mode_reg (SImode, op0);

	op = gen_rtx_REG (V2DImode, GET_SSE_REGNO (0));
	emit_move_insn (op, op1);

	for (i = 0; i < 3; i++)
	  xmm_regs[i] = gen_rtx_REG (V2DImode, GET_SSE_REGNO (i));

	if (target == 0)
	  target = gen_reg_rtx (SImode);

	emit_insn (gen_encodekey128u32 (target, op0));

	for (i = 0; i < 3; i++)
	  {
	    op = gen_rtx_MEM (V2DImode,
			      plus_constant (Pmode, op2, (i * 16)));
	    emit_move_insn (op, xmm_regs[i]);
	  }

	return target;
      }
    case IX86_BUILTIN_ENCODEKEY256U32:
      {
	rtx op, xmm_regs[7];

	arg0 = CALL_EXPR_ARG (exp, 0); // unsigned int htype
	arg1 = CALL_EXPR_ARG (exp, 1); // __m128i keylow
	arg2 = CALL_EXPR_ARG (exp, 2); // __m128i keyhi
	arg3 = CALL_EXPR_ARG (exp, 3); // void *h

	op0 = expand_normal (arg0);
	op1 = expand_normal (arg1);
	op2 = expand_normal (arg2);
	op3 = expand_normal (arg3);

	if (!REG_P (op0))
	  op0 = copy_to_mode_reg (SImode, op0);

	/* Force to use xmm0, xmm1 for keylow, keyhi*/
	op = gen_rtx_REG (V2DImode, GET_SSE_REGNO (0));
	emit_move_insn (op, op1);
	op = gen_rtx_REG (V2DImode, GET_SSE_REGNO (1));
	emit_move_insn (op, op2);

	for (i = 0; i < 4; i++)
	  xmm_regs[i] = gen_rtx_REG (V2DImode, GET_SSE_REGNO (i));

	if (target == 0)
	  target = gen_reg_rtx (SImode);

	emit_insn (gen_encodekey256u32 (target, op0));

	for (i = 0; i < 4; i++)
	  {
	    op = gen_rtx_MEM (V2DImode,
			      plus_constant (Pmode, op3, (i * 16)));
	    emit_move_insn (op, xmm_regs[i]);
	  }

	return target;
      }

    case IX86_BUILTIN_PREFETCH:
      {
	arg0 = CALL_EXPR_ARG (exp, 0); // const void *
	arg1 = CALL_EXPR_ARG (exp, 1); // const int
	arg2 = CALL_EXPR_ARG (exp, 2); // const int
	arg3 = CALL_EXPR_ARG (exp, 3); // const int

	op0 = expand_normal (arg0);
	op1 = expand_normal (arg1);
	op2 = expand_normal (arg2);
	op3 = expand_normal (arg3);

	if (!CONST_INT_P (op1) || !CONST_INT_P (op2) || !CONST_INT_P (op3))
	  {
	    error ("second, third and fourth argument must be a const");
	    return const0_rtx;
	  }

	if (INTVAL (op3) == 1)
	  {
	    if (INTVAL (op2) < 2 || INTVAL (op2) > 3)
	      {
		error ("invalid third argument");
		return const0_rtx;
	      }

	    if (TARGET_64BIT && TARGET_PREFETCHI
		&& local_func_symbolic_operand (op0, GET_MODE (op0)))
	      emit_insn (gen_prefetchi (op0, op2));
	    else
	      {
		warning (0, "instruction prefetch applies when in 64-bit mode"
			    " with RIP-relative addressing and"
			    " option %<-mprefetchi%>;"
			    " they stay NOPs otherwise");
		emit_insn (gen_nop ());
	      }
	  }
	else
	  {
	    if (!address_operand (op0, VOIDmode))
	      {
		op0 = convert_memory_address (Pmode, op0);
		op0 = copy_addr_to_reg (op0);
	      }

	    if (INTVAL (op2) < 0 || INTVAL (op2) > 3)
	      {
		warning (0, "invalid third argument to %<__builtin_ia32_prefetch%>; using zero");
		op2 = const0_rtx;
	      }

	    if (TARGET_3DNOW || TARGET_PREFETCH_SSE
		|| TARGET_PRFCHW || TARGET_PREFETCHWT1)
	      emit_insn (gen_prefetch (op0, op1, op2));
	    else if (!MEM_P (op0) && side_effects_p (op0))
	      /* Don't do anything with direct references to volatile memory,
		 but generate code to handle other side effects.  */
	      emit_insn (op0);
	  }

	return 0;
      }

    case IX86_BUILTIN_PREFETCHI:
      {
	arg0 = CALL_EXPR_ARG (exp, 0); // const void *
	arg1 = CALL_EXPR_ARG (exp, 1); // const int

	op0 = expand_normal (arg0);
	op1 = expand_normal (arg1);

	if (!CONST_INT_P (op1))
	  {
	    error ("second argument must be a const");
	    return const0_rtx;
	  }

	/* GOT/PLT_PIC should not be available for instruction prefetch.
	   It must be real instruction address.  */
	if (TARGET_64BIT
	    && local_func_symbolic_operand (op0, GET_MODE (op0)))
	  emit_insn (gen_prefetchi (op0, op1));
	else
	  {
	    /* Ignore the hint.  */
	    warning (0, "instruction prefetch applies when in 64-bit mode"
			" with RIP-relative addressing and"
			" option %<-mprefetchi%>;"
			" they stay NOPs otherwise");
	    emit_insn (gen_nop ());
	  }

	return 0;
      }

    case IX86_BUILTIN_URDMSR:
    case IX86_BUILTIN_UWRMSR:
      {
	arg0 = CALL_EXPR_ARG (exp, 0);
	op0 = expand_normal (arg0);

	if (CONST_INT_P (op0))
	  {
	    unsigned HOST_WIDE_INT val = UINTVAL (op0);
	    if (val > 0xffffffff)
	      op0 = force_reg (DImode, op0);
	  }
	else
	  op0 = force_reg (DImode, op0);

	if (fcode == IX86_BUILTIN_UWRMSR)
	  {
	    arg1 = CALL_EXPR_ARG (exp, 1);
	    op1 = expand_normal (arg1);
	    op1 = force_reg (DImode, op1);
	    icode = CODE_FOR_uwrmsr;
	    target = 0;
	  }
	else
	  {
	    if (target == 0)
	      target = gen_reg_rtx (DImode);
	    icode = CODE_FOR_urdmsr;
	    op1 = op0;
	    op0 = target;
	  }
	emit_insn (GEN_FCN (icode) (op0, op1));
	return target;
      }

    case IX86_BUILTIN_VEC_INIT_V2SI:
    case IX86_BUILTIN_VEC_INIT_V4HI:
    case IX86_BUILTIN_VEC_INIT_V8QI:
      return ix86_expand_vec_init_builtin (TREE_TYPE (exp), exp, target);

    case IX86_BUILTIN_VEC_EXT_V2DF:
    case IX86_BUILTIN_VEC_EXT_V2DI:
    case IX86_BUILTIN_VEC_EXT_V4SF:
    case IX86_BUILTIN_VEC_EXT_V4SI:
    case IX86_BUILTIN_VEC_EXT_V8HI:
    case IX86_BUILTIN_VEC_EXT_V2SI:
    case IX86_BUILTIN_VEC_EXT_V4HI:
    case IX86_BUILTIN_VEC_EXT_V16QI:
      return ix86_expand_vec_ext_builtin (exp, target);

    case IX86_BUILTIN_VEC_SET_V2DI:
    case IX86_BUILTIN_VEC_SET_V4SF:
    case IX86_BUILTIN_VEC_SET_V4SI:
    case IX86_BUILTIN_VEC_SET_V8HI:
    case IX86_BUILTIN_VEC_SET_V4HI:
    case IX86_BUILTIN_VEC_SET_V16QI:
      return ix86_expand_vec_set_builtin (exp);

    case IX86_BUILTIN_NANQ:
    case IX86_BUILTIN_NANSQ:
      return expand_call (exp, target, ignore);

    case IX86_BUILTIN_RDPID:

      op0 = gen_reg_rtx (word_mode);

      if (TARGET_64BIT)
	{
	  insn = gen_rdpid_rex64 (op0);
	  op0 = convert_to_mode (SImode, op0, 1);
	}
      else
	insn = gen_rdpid (op0);

      emit_insn (insn);

      if (target == 0
	  || !register_operand (target, SImode))
	target = gen_reg_rtx (SImode);

      emit_move_insn (target, op0);
      return target;

    case IX86_BUILTIN_2INTERSECTD512:
    case IX86_BUILTIN_2INTERSECTQ512:
    case IX86_BUILTIN_2INTERSECTD256:
    case IX86_BUILTIN_2INTERSECTQ256:
    case IX86_BUILTIN_2INTERSECTD128:
    case IX86_BUILTIN_2INTERSECTQ128:
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      arg2 = CALL_EXPR_ARG (exp, 2);
      arg3 = CALL_EXPR_ARG (exp, 3);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
      op2 = expand_normal (arg2);
      op3 = expand_normal (arg3);

      if (!address_operand (op0, VOIDmode))
	{
	  op0 = convert_memory_address (Pmode, op0);
	  op0 = copy_addr_to_reg (op0);
	}
      if (!address_operand (op1, VOIDmode))
	{
	  op1 = convert_memory_address (Pmode, op1);
	  op1 = copy_addr_to_reg (op1);
	}

      switch (fcode)
	{
	case IX86_BUILTIN_2INTERSECTD512:
	  mode4 = P2HImode;
	  icode = CODE_FOR_avx512vp2intersect_2intersectv16si;
	  break;
	case IX86_BUILTIN_2INTERSECTQ512:
	  mode4 = P2QImode;
	  icode = CODE_FOR_avx512vp2intersect_2intersectv8di;
	  break;
	case IX86_BUILTIN_2INTERSECTD256:
	  mode4 = P2QImode;
	  icode = CODE_FOR_avx512vp2intersect_2intersectv8si;
	  break;
	case IX86_BUILTIN_2INTERSECTQ256:
	  mode4 = P2QImode;
	  icode = CODE_FOR_avx512vp2intersect_2intersectv4di;
	  break;
	case IX86_BUILTIN_2INTERSECTD128:
	  mode4 = P2QImode;
	  icode = CODE_FOR_avx512vp2intersect_2intersectv4si;
	  break;
	case IX86_BUILTIN_2INTERSECTQ128:
	  mode4 = P2QImode;
	  icode = CODE_FOR_avx512vp2intersect_2intersectv2di;
	  break;
	default:
	  gcc_unreachable ();
	}

      mode2 = insn_data[icode].operand[1].mode;
      mode3 = insn_data[icode].operand[2].mode;
      if (!insn_data[icode].operand[1].predicate (op2, mode2))
	op2 = copy_to_mode_reg (mode2, op2);
      if (!insn_data[icode].operand[2].predicate (op3, mode3))
	op3 = copy_to_mode_reg (mode3, op3);

      op4 = gen_reg_rtx (mode4);
      emit_insn (GEN_FCN (icode) (op4, op2, op3));
      mode0 = mode4 == P2HImode ? HImode : QImode;
      emit_move_insn (gen_rtx_MEM (mode0, op0),
		      gen_lowpart (mode0, op4));
      emit_move_insn (gen_rtx_MEM (mode0, op1),
		      gen_highpart (mode0, op4));

      return 0;

    case IX86_BUILTIN_RDPMC:
    case IX86_BUILTIN_RDTSC:
    case IX86_BUILTIN_RDTSCP:
    case IX86_BUILTIN_XGETBV:

      op0 = gen_reg_rtx (DImode);
      op1 = gen_reg_rtx (DImode);

      if (fcode == IX86_BUILTIN_RDPMC)
	{
	  arg0 = CALL_EXPR_ARG (exp, 0);
	  op2 = expand_normal (arg0);
	  if (!register_operand (op2, SImode))
	    op2 = copy_to_mode_reg (SImode, op2);

	  insn = (TARGET_64BIT
		  ? gen_rdpmc_rex64 (op0, op1, op2)
		  : gen_rdpmc (op0, op2));
	  emit_insn (insn);
	}
      else if (fcode == IX86_BUILTIN_XGETBV)
	{
	  arg0 = CALL_EXPR_ARG (exp, 0);
	  op2 = expand_normal (arg0);
	  if (!register_operand (op2, SImode))
	    op2 = copy_to_mode_reg (SImode, op2);

	  insn = (TARGET_64BIT
		  ? gen_xgetbv_rex64 (op0, op1, op2)
		  : gen_xgetbv (op0, op2));
	  emit_insn (insn);
	}
      else if (fcode == IX86_BUILTIN_RDTSC)
	{
	  insn = (TARGET_64BIT
		  ? gen_rdtsc_rex64 (op0, op1)
		  : gen_rdtsc (op0));
	  emit_insn (insn);
	}
      else
	{
	  op2 = gen_reg_rtx (SImode);

	  insn = (TARGET_64BIT
		  ? gen_rdtscp_rex64 (op0, op1, op2)
		  : gen_rdtscp (op0, op2));
	  emit_insn (insn);

	  arg0 = CALL_EXPR_ARG (exp, 0);
	  op4 = expand_normal (arg0);
	  if (!address_operand (op4, VOIDmode))
	    {
	      op4 = convert_memory_address (Pmode, op4);
	      op4 = copy_addr_to_reg (op4);
	    }
	  emit_move_insn (gen_rtx_MEM (SImode, op4), op2);
	}

      if (target == 0
	  || !register_operand (target, DImode))
        target = gen_reg_rtx (DImode);

      if (TARGET_64BIT)
	{
	  op1 = expand_simple_binop (DImode, ASHIFT, op1, GEN_INT (32),
				     op1, 1, OPTAB_DIRECT);
	  op0 = expand_simple_binop (DImode, IOR, op0, op1,
				     op0, 1, OPTAB_DIRECT);
	}

      emit_move_insn (target, op0);
      return target;

    case IX86_BUILTIN_ENQCMD:
    case IX86_BUILTIN_ENQCMDS:
    case IX86_BUILTIN_MOVDIR64B:

      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);

      op0 = ix86_zero_extend_to_Pmode (op0);
      if (!address_operand (op1, VOIDmode))
      {
	op1 = convert_memory_address (Pmode, op1);
	op1 = copy_addr_to_reg (op1);
      }
      op1 = gen_rtx_MEM (XImode, op1);

      if (fcode == IX86_BUILTIN_MOVDIR64B)
	{
	  emit_insn (gen_movdir64b (Pmode, op0, op1));
	  return 0;
	}
      else
	{
	  if (target == 0
	      || !register_operand (target, SImode))
	    target = gen_reg_rtx (SImode);

	  emit_move_insn (target, const0_rtx);
	  target = gen_rtx_SUBREG (QImode, target, 0);

	  int unspecv = (fcode == IX86_BUILTIN_ENQCMD
			 ? UNSPECV_ENQCMD
			 : UNSPECV_ENQCMDS);
	  icode = code_for_enqcmd (unspecv, Pmode);
	  emit_insn (GEN_FCN (icode) (op0, op1));

	  emit_insn
	    (gen_rtx_SET (gen_rtx_STRICT_LOW_PART (VOIDmode, target),
			  gen_rtx_fmt_ee (EQ, QImode,
					  gen_rtx_REG (CCZmode, FLAGS_REG),
					  const0_rtx)));
	  return SUBREG_REG (target);
	}

    case IX86_BUILTIN_FXSAVE:
    case IX86_BUILTIN_FXRSTOR:
    case IX86_BUILTIN_FXSAVE64:
    case IX86_BUILTIN_FXRSTOR64:
    case IX86_BUILTIN_FNSTENV:
    case IX86_BUILTIN_FLDENV:
      mode0 = BLKmode;
      switch (fcode)
	{
	case IX86_BUILTIN_FXSAVE:
	  icode = CODE_FOR_fxsave;
	  break;
	case IX86_BUILTIN_FXRSTOR:
	  icode = CODE_FOR_fxrstor;
	  break;
	case IX86_BUILTIN_FXSAVE64:
	  icode = CODE_FOR_fxsave64;
	  break;
	case IX86_BUILTIN_FXRSTOR64:
	  icode = CODE_FOR_fxrstor64;
	  break;
	case IX86_BUILTIN_FNSTENV:
	  icode = CODE_FOR_fnstenv;
	  break;
	case IX86_BUILTIN_FLDENV:
	  icode = CODE_FOR_fldenv;
	  break;
	default:
	  gcc_unreachable ();
	}

      arg0 = CALL_EXPR_ARG (exp, 0);
      op0 = expand_normal (arg0);

      if (!address_operand (op0, VOIDmode))
	{
	  op0 = convert_memory_address (Pmode, op0);
	  op0 = copy_addr_to_reg (op0);
	}
      op0 = gen_rtx_MEM (mode0, op0);

      pat = GEN_FCN (icode) (op0);
      if (pat)
	emit_insn (pat);
      return 0;

    case IX86_BUILTIN_XSETBV:
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);

      if (!REG_P (op0))
	op0 = copy_to_mode_reg (SImode, op0);

      op1 = force_reg (DImode, op1);

      if (TARGET_64BIT)
	{
	  op2 = expand_simple_binop (DImode, LSHIFTRT, op1, GEN_INT (32),
				     NULL, 1, OPTAB_DIRECT);

	  icode = CODE_FOR_xsetbv_rex64;

	  op2 = gen_lowpart (SImode, op2);
	  op1 = gen_lowpart (SImode, op1);
	  pat = GEN_FCN (icode) (op0, op1, op2);
	}
      else
	{
	  icode = CODE_FOR_xsetbv;

	  pat = GEN_FCN (icode) (op0, op1);
	}
      if (pat)
	emit_insn (pat);
      return 0;

    case IX86_BUILTIN_XSAVE:
    case IX86_BUILTIN_XRSTOR:
    case IX86_BUILTIN_XSAVE64:
    case IX86_BUILTIN_XRSTOR64:
    case IX86_BUILTIN_XSAVEOPT:
    case IX86_BUILTIN_XSAVEOPT64:
    case IX86_BUILTIN_XSAVES:
    case IX86_BUILTIN_XRSTORS:
    case IX86_BUILTIN_XSAVES64:
    case IX86_BUILTIN_XRSTORS64:
    case IX86_BUILTIN_XSAVEC:
    case IX86_BUILTIN_XSAVEC64:
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);

      if (!address_operand (op0, VOIDmode))
	{
	  op0 = convert_memory_address (Pmode, op0);
	  op0 = copy_addr_to_reg (op0);
	}
      op0 = gen_rtx_MEM (BLKmode, op0);

      op1 = force_reg (DImode, op1);

      if (TARGET_64BIT)
	{
	  op2 = expand_simple_binop (DImode, LSHIFTRT, op1, GEN_INT (32),
				     NULL, 1, OPTAB_DIRECT);
	  switch (fcode)
	    {
	    case IX86_BUILTIN_XSAVE:
	      icode = CODE_FOR_xsave_rex64;
	      break;
	    case IX86_BUILTIN_XRSTOR:
	      icode = CODE_FOR_xrstor_rex64;
	      break;
	    case IX86_BUILTIN_XSAVE64:
	      icode = CODE_FOR_xsave64;
	      break;
	    case IX86_BUILTIN_XRSTOR64:
	      icode = CODE_FOR_xrstor64;
	      break;
	    case IX86_BUILTIN_XSAVEOPT:
	      icode = CODE_FOR_xsaveopt_rex64;
	      break;
	    case IX86_BUILTIN_XSAVEOPT64:
	      icode = CODE_FOR_xsaveopt64;
	      break;
	    case IX86_BUILTIN_XSAVES:
	      icode = CODE_FOR_xsaves_rex64;
	      break;
	    case IX86_BUILTIN_XRSTORS:
	      icode = CODE_FOR_xrstors_rex64;
	      break;
	    case IX86_BUILTIN_XSAVES64:
	      icode = CODE_FOR_xsaves64;
	      break;
	    case IX86_BUILTIN_XRSTORS64:
	      icode = CODE_FOR_xrstors64;
	      break;
	    case IX86_BUILTIN_XSAVEC:
	      icode = CODE_FOR_xsavec_rex64;
	      break;
	    case IX86_BUILTIN_XSAVEC64:
	      icode = CODE_FOR_xsavec64;
	      break;
	    default:
	      gcc_unreachable ();
	    }

	  op2 = gen_lowpart (SImode, op2);
	  op1 = gen_lowpart (SImode, op1);
	  pat = GEN_FCN (icode) (op0, op1, op2);
	}
      else
	{
	  switch (fcode)
	    {
	    case IX86_BUILTIN_XSAVE:
	      icode = CODE_FOR_xsave;
	      break;
	    case IX86_BUILTIN_XRSTOR:
	      icode = CODE_FOR_xrstor;
	      break;
	    case IX86_BUILTIN_XSAVEOPT:
	      icode = CODE_FOR_xsaveopt;
	      break;
	    case IX86_BUILTIN_XSAVES:
	      icode = CODE_FOR_xsaves;
	      break;
	    case IX86_BUILTIN_XRSTORS:
	      icode = CODE_FOR_xrstors;
	      break;
	    case IX86_BUILTIN_XSAVEC:
	      icode = CODE_FOR_xsavec;
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  pat = GEN_FCN (icode) (op0, op1);
	}

      if (pat)
	emit_insn (pat);
      return 0;

    case IX86_BUILTIN_LDTILECFG:
    case IX86_BUILTIN_STTILECFG:
      arg0 = CALL_EXPR_ARG (exp, 0);
      op0 = expand_normal (arg0);

      if (!address_operand (op0, VOIDmode))
	{
	  op0 = convert_memory_address (Pmode, op0);
	  op0 = copy_addr_to_reg (op0);
	}
      op0 = gen_rtx_MEM (XImode, op0);
      if (fcode == IX86_BUILTIN_LDTILECFG)
	icode = CODE_FOR_ldtilecfg;
      else
	icode = CODE_FOR_sttilecfg;
      pat = GEN_FCN (icode) (op0);
      emit_insn (pat);
      return 0;

    case IX86_BUILTIN_LLWPCB:
      arg0 = CALL_EXPR_ARG (exp, 0);
      op0 = expand_normal (arg0);

      if (!register_operand (op0, Pmode))
	op0 = ix86_zero_extend_to_Pmode (op0);
      emit_insn (gen_lwp_llwpcb (Pmode, op0));
      return 0;

    case IX86_BUILTIN_SLWPCB:
      if (!target
	  || !register_operand (target, Pmode))
	target = gen_reg_rtx (Pmode);
      emit_insn (gen_lwp_slwpcb (Pmode, target));
      return target;

    case IX86_BUILTIN_LWPVAL32:
    case IX86_BUILTIN_LWPVAL64:
    case IX86_BUILTIN_LWPINS32:
    case IX86_BUILTIN_LWPINS64:
      mode = ((fcode == IX86_BUILTIN_LWPVAL32
	       || fcode == IX86_BUILTIN_LWPINS32)
	      ? SImode : DImode);

      if (fcode == IX86_BUILTIN_LWPVAL32
	  || fcode == IX86_BUILTIN_LWPVAL64)
	icode = code_for_lwp_lwpval (mode);
      else
	icode = code_for_lwp_lwpins (mode);

      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      arg2 = CALL_EXPR_ARG (exp, 2);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
      op2 = expand_normal (arg2);
      mode0 = insn_data[icode].operand[0].mode;

      if (!insn_data[icode].operand[0].predicate (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);
      if (!insn_data[icode].operand[1].predicate (op1, SImode))
	op1 = copy_to_mode_reg (SImode, op1);

      if (!CONST_INT_P (op2))
	{
	  error ("the last argument must be a 32-bit immediate");
	  return const0_rtx;
	}

      emit_insn (GEN_FCN (icode) (op0, op1, op2));

      if (fcode == IX86_BUILTIN_LWPINS32
	  || fcode == IX86_BUILTIN_LWPINS64)
	{
	  if (target == 0
	      || !nonimmediate_operand (target, QImode))
	    target = gen_reg_rtx (QImode);

	  pat = gen_rtx_EQ (QImode, gen_rtx_REG (CCCmode, FLAGS_REG),
			    const0_rtx);
	  emit_insn (gen_rtx_SET (target, pat));

	  return target;
	}
      else
	return 0;

    case IX86_BUILTIN_BEXTRI32:
    case IX86_BUILTIN_BEXTRI64:
      mode = (fcode == IX86_BUILTIN_BEXTRI32 ? SImode : DImode);

      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);

      if (!CONST_INT_P (op1))
	{
	  error ("last argument must be an immediate");
	  return const0_rtx;
	}
      else
	{
	  unsigned char lsb_index = UINTVAL (op1);
	  unsigned char length = UINTVAL (op1) >> 8;

	  unsigned char bitsize = GET_MODE_BITSIZE (mode);

	  icode = code_for_tbm_bextri (mode);

	  mode1 = insn_data[icode].operand[1].mode;
	  if (!insn_data[icode].operand[1].predicate (op0, mode1))
	    op0 = copy_to_mode_reg (mode1, op0);

	  mode0 = insn_data[icode].operand[0].mode;
	  if (target == 0
	      || !register_operand (target, mode0))
	    target = gen_reg_rtx (mode0);

	  if (length == 0 || lsb_index >= bitsize)
	    {
	      emit_move_insn (target, const0_rtx);
	      return target;
	    }

	  if (length + lsb_index > bitsize)
	    length = bitsize - lsb_index;

	  op1 = GEN_INT (length);
	  op2 = GEN_INT (lsb_index);

	  emit_insn (GEN_FCN (icode) (target, op0, op1, op2));
	  return target;
	}

    case IX86_BUILTIN_RDRAND16_STEP:
      mode = HImode;
      goto rdrand_step;

    case IX86_BUILTIN_RDRAND32_STEP:
      mode = SImode;
      goto rdrand_step;

    case IX86_BUILTIN_RDRAND64_STEP:
      mode = DImode;

rdrand_step:
      arg0 = CALL_EXPR_ARG (exp, 0);
      op1 = expand_normal (arg0);
      if (!address_operand (op1, VOIDmode))
	{
	  op1 = convert_memory_address (Pmode, op1);
	  op1 = copy_addr_to_reg (op1);
	}

      op0 = gen_reg_rtx (mode);
      emit_insn (gen_rdrand (mode, op0));

      emit_move_insn (gen_rtx_MEM (mode, op1), op0);

      op1 = force_reg (SImode, const1_rtx);

      /* Emit SImode conditional move.  */
      if (mode == HImode)
	{
	  if (TARGET_ZERO_EXTEND_WITH_AND
	      && optimize_function_for_speed_p (cfun))
	    {
	      op2 = force_reg (SImode, const0_rtx);

	      emit_insn (gen_movstricthi
			 (gen_lowpart (HImode, op2), op0));
	    }
	  else
	    {
	      op2 = gen_reg_rtx (SImode);

	      emit_insn (gen_zero_extendhisi2 (op2, op0));
	    }
	}
      else if (mode == SImode)
	op2 = op0;
      else
	op2 = gen_rtx_SUBREG (SImode, op0, 0);

      if (target == 0
	  || !register_operand (target, SImode))
	target = gen_reg_rtx (SImode);

      pat = gen_rtx_GEU (VOIDmode, gen_rtx_REG (CCCmode, FLAGS_REG),
			 const0_rtx);
      emit_insn (gen_rtx_SET (target,
			      gen_rtx_IF_THEN_ELSE (SImode, pat, op2, op1)));
      return target;

    case IX86_BUILTIN_RDSEED16_STEP:
      mode = HImode;
      goto rdseed_step;

    case IX86_BUILTIN_RDSEED32_STEP:
      mode = SImode;
      goto rdseed_step;

    case IX86_BUILTIN_RDSEED64_STEP:
      mode = DImode;

rdseed_step:
      arg0 = CALL_EXPR_ARG (exp, 0);
      op1 = expand_normal (arg0);
      if (!address_operand (op1, VOIDmode))
	{
	  op1 = convert_memory_address (Pmode, op1);
	  op1 = copy_addr_to_reg (op1);
	}

      op0 = gen_reg_rtx (mode);
      emit_insn (gen_rdseed (mode, op0));

      emit_move_insn (gen_rtx_MEM (mode, op1), op0);

      op2 = gen_reg_rtx (QImode);

      pat = gen_rtx_LTU (QImode, gen_rtx_REG (CCCmode, FLAGS_REG),
                         const0_rtx);
      emit_insn (gen_rtx_SET (op2, pat));

      if (target == 0
	  || !register_operand (target, SImode))
        target = gen_reg_rtx (SImode);

      emit_insn (gen_zero_extendqisi2 (target, op2));
      return target;

    case IX86_BUILTIN_SBB32:
      icode = CODE_FOR_subborrowsi;
      icode2 = CODE_FOR_subborrowsi_0;
      mode0 = SImode;
      mode1 = DImode;
      mode2 = CCmode;
      goto handlecarry;

    case IX86_BUILTIN_SBB64:
      icode = CODE_FOR_subborrowdi;
      icode2 = CODE_FOR_subborrowdi_0;
      mode0 = DImode;
      mode1 = TImode;
      mode2 = CCmode;
      goto handlecarry;

    case IX86_BUILTIN_ADDCARRYX32:
      icode = CODE_FOR_addcarrysi;
      icode2 = CODE_FOR_addcarrysi_0;
      mode0 = SImode;
      mode1 = DImode;
      mode2 = CCCmode;
      goto handlecarry;

    case IX86_BUILTIN_ADDCARRYX64:
      icode = CODE_FOR_addcarrydi;
      icode2 = CODE_FOR_addcarrydi_0;
      mode0 = DImode;
      mode1 = TImode;
      mode2 = CCCmode;

    handlecarry:
      arg0 = CALL_EXPR_ARG (exp, 0); /* unsigned char c_in.  */
      arg1 = CALL_EXPR_ARG (exp, 1); /* unsigned int src1.  */
      arg2 = CALL_EXPR_ARG (exp, 2); /* unsigned int src2.  */
      arg3 = CALL_EXPR_ARG (exp, 3); /* unsigned int *sum_out.  */

      op1 = expand_normal (arg0);

      op2 = expand_normal (arg1);
      if (!register_operand (op2, mode0))
	op2 = copy_to_mode_reg (mode0, op2);

      op3 = expand_normal (arg2);
      if (!register_operand (op3, mode0))
	op3 = copy_to_mode_reg (mode0, op3);

      op4 = expand_normal (arg3);
      if (!address_operand (op4, VOIDmode))
	{
	  op4 = convert_memory_address (Pmode, op4);
	  op4 = copy_addr_to_reg (op4);
	}

      op0 = gen_reg_rtx (mode0);
      if (op1 == const0_rtx)
	{
	  /* If arg0 is 0, optimize right away into add or sub
	     instruction that sets CCCmode flags.  */
	  op1 = gen_rtx_REG (mode2, FLAGS_REG);
	  emit_insn (GEN_FCN (icode2) (op0, op2, op3));
	}
      else
	{
	  /* Generate CF from input operand.  */
	  ix86_expand_carry (op1);

	  /* Generate instruction that consumes CF.  */
	  op1 = gen_rtx_REG (CCCmode, FLAGS_REG);
	  pat = gen_rtx_LTU (mode1, op1, const0_rtx);
	  pat2 = gen_rtx_LTU (mode0, op1, const0_rtx);
	  emit_insn (GEN_FCN (icode) (op0, op2, op3, op1, pat, pat2));
	}

      /* Return current CF value.  */
      if (target == 0)
        target = gen_reg_rtx (QImode);

      pat = gen_rtx_LTU (QImode, op1, const0_rtx);
      emit_insn (gen_rtx_SET (target, pat));

      /* Store the result.  */
      emit_move_insn (gen_rtx_MEM (mode0, op4), op0);

      return target;

    case IX86_BUILTIN_READ_FLAGS:
      if (ignore)
	return const0_rtx;

      emit_insn (gen_pushfl ());

      if (optimize
	  || target == NULL_RTX
	  || !nonimmediate_operand (target, word_mode)
	  || GET_MODE (target) != word_mode)
	target = gen_reg_rtx (word_mode);

      emit_insn (gen_pop (target));
      return target;

    case IX86_BUILTIN_WRITE_FLAGS:

      arg0 = CALL_EXPR_ARG (exp, 0);
      op0 = expand_normal (arg0);
      if (!general_no_elim_operand (op0, word_mode))
	op0 = copy_to_mode_reg (word_mode, op0);

      emit_insn (gen_push (op0));
      emit_insn (gen_popfl ());
      return 0;

    case IX86_BUILTIN_KTESTC8:
      icode = CODE_FOR_ktestqi;
      mode3 = CCCmode;
      goto kortest;

    case IX86_BUILTIN_KTESTZ8:
      icode = CODE_FOR_ktestqi;
      mode3 = CCZmode;
      goto kortest;

    case IX86_BUILTIN_KTESTC16:
      icode = CODE_FOR_ktesthi;
      mode3 = CCCmode;
      goto kortest;

    case IX86_BUILTIN_KTESTZ16:
      icode = CODE_FOR_ktesthi;
      mode3 = CCZmode;
      goto kortest;

    case IX86_BUILTIN_KTESTC32:
      icode = CODE_FOR_ktestsi;
      mode3 = CCCmode;
      goto kortest;

    case IX86_BUILTIN_KTESTZ32:
      icode = CODE_FOR_ktestsi;
      mode3 = CCZmode;
      goto kortest;

    case IX86_BUILTIN_KTESTC64:
      icode = CODE_FOR_ktestdi;
      mode3 = CCCmode;
      goto kortest;

    case IX86_BUILTIN_KTESTZ64:
      icode = CODE_FOR_ktestdi;
      mode3 = CCZmode;
      goto kortest;

    case IX86_BUILTIN_KORTESTC8:
      icode = CODE_FOR_kortestqi;
      mode3 = CCCmode;
      goto kortest;

    case IX86_BUILTIN_KORTESTZ8:
      icode = CODE_FOR_kortestqi;
      mode3 = CCZmode;
      goto kortest;

    case IX86_BUILTIN_KORTESTC16:
      icode = CODE_FOR_kortesthi;
      mode3 = CCCmode;
      goto kortest;

    case IX86_BUILTIN_KORTESTZ16:
      icode = CODE_FOR_kortesthi;
      mode3 = CCZmode;
      goto kortest;

    case IX86_BUILTIN_KORTESTC32:
      icode = CODE_FOR_kortestsi;
      mode3 = CCCmode;
      goto kortest;

    case IX86_BUILTIN_KORTESTZ32:
      icode = CODE_FOR_kortestsi;
      mode3 = CCZmode;
      goto kortest;

    case IX86_BUILTIN_KORTESTC64:
      icode = CODE_FOR_kortestdi;
      mode3 = CCCmode;
      goto kortest;

    case IX86_BUILTIN_KORTESTZ64:
      icode = CODE_FOR_kortestdi;
      mode3 = CCZmode;

    kortest:
      arg0 = CALL_EXPR_ARG (exp, 0); /* Mask reg src1.  */
      arg1 = CALL_EXPR_ARG (exp, 1); /* Mask reg src2.  */
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);

      mode0 = insn_data[icode].operand[0].mode;
      mode1 = insn_data[icode].operand[1].mode;

      if (GET_MODE (op0) != VOIDmode)
	op0 = force_reg (GET_MODE (op0), op0);

      op0 = gen_lowpart (mode0, op0);

      if (!insn_data[icode].operand[0].predicate (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);

      if (GET_MODE (op1) != VOIDmode)
	op1 = force_reg (GET_MODE (op1), op1);

      op1 = gen_lowpart (mode1, op1);

      if (!insn_data[icode].operand[1].predicate (op1, mode1))
	op1 = copy_to_mode_reg (mode1, op1);

      target = gen_reg_rtx (QImode);

      /* Emit kortest.  */
      emit_insn (GEN_FCN (icode) (op0, op1));
      /* And use setcc to return result from flags.  */
      ix86_expand_setcc (target, EQ,
			 gen_rtx_REG (mode3, FLAGS_REG), const0_rtx);
      return target;

    case IX86_BUILTIN_GATHERSIV2DF:
      icode = CODE_FOR_avx2_gathersiv2df;
      goto gather_gen;
    case IX86_BUILTIN_GATHERSIV4DF:
      icode = CODE_FOR_avx2_gathersiv4df;
      goto gather_gen;
    case IX86_BUILTIN_GATHERDIV2DF:
      icode = CODE_FOR_avx2_gatherdiv2df;
      goto gather_gen;
    case IX86_BUILTIN_GATHERDIV4DF:
      icode = CODE_FOR_avx2_gatherdiv4df;
      goto gather_gen;
    case IX86_BUILTIN_GATHERSIV4SF:
      icode = CODE_FOR_avx2_gathersiv4sf;
      goto gather_gen;
    case IX86_BUILTIN_GATHERSIV8SF:
      icode = CODE_FOR_avx2_gathersiv8sf;
      goto gather_gen;
    case IX86_BUILTIN_GATHERDIV4SF:
      icode = CODE_FOR_avx2_gatherdiv4sf;
      goto gather_gen;
    case IX86_BUILTIN_GATHERDIV8SF:
      icode = CODE_FOR_avx2_gatherdiv8sf;
      goto gather_gen;
    case IX86_BUILTIN_GATHERSIV2DI:
      icode = CODE_FOR_avx2_gathersiv2di;
      goto gather_gen;
    case IX86_BUILTIN_GATHERSIV4DI:
      icode = CODE_FOR_avx2_gathersiv4di;
      goto gather_gen;
    case IX86_BUILTIN_GATHERDIV2DI:
      icode = CODE_FOR_avx2_gatherdiv2di;
      goto gather_gen;
    case IX86_BUILTIN_GATHERDIV4DI:
      icode = CODE_FOR_avx2_gatherdiv4di;
      goto gather_gen;
    case IX86_BUILTIN_GATHERSIV4SI:
      icode = CODE_FOR_avx2_gathersiv4si;
      goto gather_gen;
    case IX86_BUILTIN_GATHERSIV8SI:
      icode = CODE_FOR_avx2_gathersiv8si;
      goto gather_gen;
    case IX86_BUILTIN_GATHERDIV4SI:
      icode = CODE_FOR_avx2_gatherdiv4si;
      goto gather_gen;
    case IX86_BUILTIN_GATHERDIV8SI:
      icode = CODE_FOR_avx2_gatherdiv8si;
      goto gather_gen;
    case IX86_BUILTIN_GATHERALTSIV4DF:
      icode = CODE_FOR_avx2_gathersiv4df;
      goto gather_gen;
    case IX86_BUILTIN_GATHERALTDIV8SF:
      icode = CODE_FOR_avx2_gatherdiv8sf;
      goto gather_gen;
    case IX86_BUILTIN_GATHERALTSIV4DI:
      icode = CODE_FOR_avx2_gathersiv4di;
      goto gather_gen;
    case IX86_BUILTIN_GATHERALTDIV8SI:
      icode = CODE_FOR_avx2_gatherdiv8si;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3SIV16SF:
      icode = CODE_FOR_avx512f_gathersiv16sf;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3SIV8DF:
      icode = CODE_FOR_avx512f_gathersiv8df;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3DIV16SF:
      icode = CODE_FOR_avx512f_gatherdiv16sf;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3DIV8DF:
      icode = CODE_FOR_avx512f_gatherdiv8df;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3SIV16SI:
      icode = CODE_FOR_avx512f_gathersiv16si;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3SIV8DI:
      icode = CODE_FOR_avx512f_gathersiv8di;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3DIV16SI:
      icode = CODE_FOR_avx512f_gatherdiv16si;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3DIV8DI:
      icode = CODE_FOR_avx512f_gatherdiv8di;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3ALTSIV8DF:
      icode = CODE_FOR_avx512f_gathersiv8df;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3ALTDIV16SF:
      icode = CODE_FOR_avx512f_gatherdiv16sf;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3ALTSIV8DI:
      icode = CODE_FOR_avx512f_gathersiv8di;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3ALTDIV16SI:
      icode = CODE_FOR_avx512f_gatherdiv16si;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3SIV2DF:
      icode = CODE_FOR_avx512vl_gathersiv2df;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3SIV4DF:
      icode = CODE_FOR_avx512vl_gathersiv4df;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3DIV2DF:
      icode = CODE_FOR_avx512vl_gatherdiv2df;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3DIV4DF:
      icode = CODE_FOR_avx512vl_gatherdiv4df;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3SIV4SF:
      icode = CODE_FOR_avx512vl_gathersiv4sf;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3SIV8SF:
      icode = CODE_FOR_avx512vl_gathersiv8sf;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3DIV4SF:
      icode = CODE_FOR_avx512vl_gatherdiv4sf;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3DIV8SF:
      icode = CODE_FOR_avx512vl_gatherdiv8sf;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3SIV2DI:
      icode = CODE_FOR_avx512vl_gathersiv2di;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3SIV4DI:
      icode = CODE_FOR_avx512vl_gathersiv4di;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3DIV2DI:
      icode = CODE_FOR_avx512vl_gatherdiv2di;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3DIV4DI:
      icode = CODE_FOR_avx512vl_gatherdiv4di;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3SIV4SI:
      icode = CODE_FOR_avx512vl_gathersiv4si;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3SIV8SI:
      icode = CODE_FOR_avx512vl_gathersiv8si;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3DIV4SI:
      icode = CODE_FOR_avx512vl_gatherdiv4si;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3DIV8SI:
      icode = CODE_FOR_avx512vl_gatherdiv8si;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3ALTSIV4DF:
      icode = CODE_FOR_avx512vl_gathersiv4df;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3ALTDIV8SF:
      icode = CODE_FOR_avx512vl_gatherdiv8sf;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3ALTSIV4DI:
      icode = CODE_FOR_avx512vl_gathersiv4di;
      goto gather_gen;
    case IX86_BUILTIN_GATHER3ALTDIV8SI:
      icode = CODE_FOR_avx512vl_gatherdiv8si;
      goto gather_gen;
    case IX86_BUILTIN_SCATTERSIV16SF:
      icode = CODE_FOR_avx512f_scattersiv16sf;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERSIV8DF:
      icode = CODE_FOR_avx512f_scattersiv8df;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERDIV16SF:
      icode = CODE_FOR_avx512f_scatterdiv16sf;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERDIV8DF:
      icode = CODE_FOR_avx512f_scatterdiv8df;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERSIV16SI:
      icode = CODE_FOR_avx512f_scattersiv16si;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERSIV8DI:
      icode = CODE_FOR_avx512f_scattersiv8di;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERDIV16SI:
      icode = CODE_FOR_avx512f_scatterdiv16si;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERDIV8DI:
      icode = CODE_FOR_avx512f_scatterdiv8di;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERSIV8SF:
      icode = CODE_FOR_avx512vl_scattersiv8sf;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERSIV4SF:
      icode = CODE_FOR_avx512vl_scattersiv4sf;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERSIV4DF:
      icode = CODE_FOR_avx512vl_scattersiv4df;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERSIV2DF:
      icode = CODE_FOR_avx512vl_scattersiv2df;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERDIV8SF:
      icode = CODE_FOR_avx512vl_scatterdiv8sf;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERDIV4SF:
      icode = CODE_FOR_avx512vl_scatterdiv4sf;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERDIV4DF:
      icode = CODE_FOR_avx512vl_scatterdiv4df;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERDIV2DF:
      icode = CODE_FOR_avx512vl_scatterdiv2df;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERSIV8SI:
      icode = CODE_FOR_avx512vl_scattersiv8si;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERSIV4SI:
      icode = CODE_FOR_avx512vl_scattersiv4si;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERSIV4DI:
      icode = CODE_FOR_avx512vl_scattersiv4di;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERSIV2DI:
      icode = CODE_FOR_avx512vl_scattersiv2di;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERDIV8SI:
      icode = CODE_FOR_avx512vl_scatterdiv8si;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERDIV4SI:
      icode = CODE_FOR_avx512vl_scatterdiv4si;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERDIV4DI:
      icode = CODE_FOR_avx512vl_scatterdiv4di;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERDIV2DI:
      icode = CODE_FOR_avx512vl_scatterdiv2di;
      goto scatter_gen;
    case IX86_BUILTIN_GATHERPFDPD:
      icode = CODE_FOR_avx512pf_gatherpfv8sidf;
      goto vec_prefetch_gen;
    case IX86_BUILTIN_SCATTERALTSIV8DF:
      icode = CODE_FOR_avx512f_scattersiv8df;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERALTDIV16SF:
      icode = CODE_FOR_avx512f_scatterdiv16sf;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERALTSIV8DI:
      icode = CODE_FOR_avx512f_scattersiv8di;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERALTDIV16SI:
      icode = CODE_FOR_avx512f_scatterdiv16si;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERALTSIV4DF:
      icode = CODE_FOR_avx512vl_scattersiv4df;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERALTDIV8SF:
      icode = CODE_FOR_avx512vl_scatterdiv8sf;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERALTSIV4DI:
      icode = CODE_FOR_avx512vl_scattersiv4di;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERALTDIV8SI:
      icode = CODE_FOR_avx512vl_scatterdiv8si;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERALTSIV2DF:
      icode = CODE_FOR_avx512vl_scattersiv2df;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERALTDIV4SF:
      icode = CODE_FOR_avx512vl_scatterdiv4sf;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERALTSIV2DI:
      icode = CODE_FOR_avx512vl_scattersiv2di;
      goto scatter_gen;
    case IX86_BUILTIN_SCATTERALTDIV4SI:
      icode = CODE_FOR_avx512vl_scatterdiv4si;
      goto scatter_gen;
    case IX86_BUILTIN_GATHERPFDPS:
      icode = CODE_FOR_avx512pf_gatherpfv16sisf;
      goto vec_prefetch_gen;
    case IX86_BUILTIN_GATHERPFQPD:
      icode = CODE_FOR_avx512pf_gatherpfv8didf;
      goto vec_prefetch_gen;
    case IX86_BUILTIN_GATHERPFQPS:
      icode = CODE_FOR_avx512pf_gatherpfv8disf;
      goto vec_prefetch_gen;
    case IX86_BUILTIN_SCATTERPFDPD:
      icode = CODE_FOR_avx512pf_scatterpfv8sidf;
      goto vec_prefetch_gen;
    case IX86_BUILTIN_SCATTERPFDPS:
      icode = CODE_FOR_avx512pf_scatterpfv16sisf;
      goto vec_prefetch_gen;
    case IX86_BUILTIN_SCATTERPFQPD:
      icode = CODE_FOR_avx512pf_scatterpfv8didf;
      goto vec_prefetch_gen;
    case IX86_BUILTIN_SCATTERPFQPS:
      icode = CODE_FOR_avx512pf_scatterpfv8disf;
      goto vec_prefetch_gen;

    gather_gen:
      rtx half;
      rtx (*gen) (rtx, rtx);

      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      arg2 = CALL_EXPR_ARG (exp, 2);
      arg3 = CALL_EXPR_ARG (exp, 3);
      arg4 = CALL_EXPR_ARG (exp, 4);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
      op2 = expand_normal (arg2);
      op3 = expand_normal (arg3);
      op4 = expand_normal (arg4);
      /* Note the arg order is different from the operand order.  */
      mode0 = insn_data[icode].operand[1].mode;
      mode2 = insn_data[icode].operand[3].mode;
      mode3 = insn_data[icode].operand[4].mode;
      mode4 = insn_data[icode].operand[5].mode;

      if (target == NULL_RTX
	  || GET_MODE (target) != insn_data[icode].operand[0].mode
	  || !insn_data[icode].operand[0].predicate (target,
						     GET_MODE (target)))
	subtarget = gen_reg_rtx (insn_data[icode].operand[0].mode);
      else
	subtarget = target;

      switch (fcode)
	{
	case IX86_BUILTIN_GATHER3ALTSIV8DF:
	case IX86_BUILTIN_GATHER3ALTSIV8DI:
	  half = gen_reg_rtx (V8SImode);
	  if (!nonimmediate_operand (op2, V16SImode))
	    op2 = copy_to_mode_reg (V16SImode, op2);
	  emit_insn (gen_vec_extract_lo_v16si (half, op2));
	  op2 = half;
	  break;
	case IX86_BUILTIN_GATHER3ALTSIV4DF:
	case IX86_BUILTIN_GATHER3ALTSIV4DI:
	case IX86_BUILTIN_GATHERALTSIV4DF:
	case IX86_BUILTIN_GATHERALTSIV4DI:
	  half = gen_reg_rtx (V4SImode);
	  if (!nonimmediate_operand (op2, V8SImode))
	    op2 = copy_to_mode_reg (V8SImode, op2);
	  emit_insn (gen_vec_extract_lo_v8si (half, op2));
	  op2 = half;
	  break;
	case IX86_BUILTIN_GATHER3ALTDIV16SF:
	case IX86_BUILTIN_GATHER3ALTDIV16SI:
	  half = gen_reg_rtx (mode0);
	  if (mode0 == V8SFmode)
	    gen = gen_vec_extract_lo_v16sf;
	  else
	    gen = gen_vec_extract_lo_v16si;
	  if (!nonimmediate_operand (op0, GET_MODE (op0)))
	    op0 = copy_to_mode_reg (GET_MODE (op0), op0);
	  emit_insn (gen (half, op0));
	  op0 = half;
	  op3 = lowpart_subreg (QImode, op3, HImode);
	  break;
	case IX86_BUILTIN_GATHER3ALTDIV8SF:
	case IX86_BUILTIN_GATHER3ALTDIV8SI:
	case IX86_BUILTIN_GATHERALTDIV8SF:
	case IX86_BUILTIN_GATHERALTDIV8SI:
	  half = gen_reg_rtx (mode0);
	  if (mode0 == V4SFmode)
	    gen = gen_vec_extract_lo_v8sf;
	  else
	    gen = gen_vec_extract_lo_v8si;
	  if (!nonimmediate_operand (op0, GET_MODE (op0)))
	    op0 = copy_to_mode_reg (GET_MODE (op0), op0);
	  emit_insn (gen (half, op0));
	  op0 = half;
	  if (VECTOR_MODE_P (GET_MODE (op3)))
	    {
	      half = gen_reg_rtx (mode0);
	      if (!nonimmediate_operand (op3, GET_MODE (op3)))
		op3 = copy_to_mode_reg (GET_MODE (op3), op3);
	      emit_insn (gen (half, op3));
	      op3 = half;
	    }
	  break;
	default:
	  break;
	}

      /* Force memory operand only with base register here.  But we
	 don't want to do it on memory operand for other builtin
	 functions.  */
      op1 = ix86_zero_extend_to_Pmode (op1);

      if (!insn_data[icode].operand[1].predicate (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);
      if (!insn_data[icode].operand[2].predicate (op1, Pmode))
	op1 = copy_to_mode_reg (Pmode, op1);
      if (!insn_data[icode].operand[3].predicate (op2, mode2))
	op2 = copy_to_mode_reg (mode2, op2);

      op3 = fixup_modeless_constant (op3, mode3);

      if (GET_MODE (op3) == mode3 || GET_MODE (op3) == VOIDmode)
	{
	  if (!insn_data[icode].operand[4].predicate (op3, mode3))
	    op3 = copy_to_mode_reg (mode3, op3);
	}
      else
	{
	  op3 = copy_to_reg (op3);
	  op3 = lowpart_subreg (mode3, op3, GET_MODE (op3));
	}
      if (!insn_data[icode].operand[5].predicate (op4, mode4))
	{
          error ("the last argument must be scale 1, 2, 4, 8");
          return const0_rtx;
	}

      /* Optimize.  If mask is known to have all high bits set,
	 replace op0 with pc_rtx to signal that the instruction
	 overwrites the whole destination and doesn't use its
	 previous contents.  */
      if (optimize)
	{
	  if (TREE_CODE (arg3) == INTEGER_CST)
	    {
	      if (integer_all_onesp (arg3))
		op0 = pc_rtx;
	    }
	  else if (TREE_CODE (arg3) == VECTOR_CST)
	    {
	      unsigned int negative = 0;
	      for (i = 0; i < VECTOR_CST_NELTS (arg3); ++i)
		{
		  tree cst = VECTOR_CST_ELT (arg3, i);
		  if (TREE_CODE (cst) == INTEGER_CST
		      && tree_int_cst_sign_bit (cst))
		    negative++;
		  else if (TREE_CODE (cst) == REAL_CST
			   && REAL_VALUE_NEGATIVE (TREE_REAL_CST (cst)))
		    negative++;
		}
	      if (negative == TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg3)))
		op0 = pc_rtx;
	    }
	  else if (TREE_CODE (arg3) == SSA_NAME
		   && VECTOR_TYPE_P (TREE_TYPE (arg3)))
	    {
	      /* Recognize also when mask is like:
		 __v2df src = _mm_setzero_pd ();
		 __v2df mask = _mm_cmpeq_pd (src, src);
		 or
		 __v8sf src = _mm256_setzero_ps ();
		 __v8sf mask = _mm256_cmp_ps (src, src, _CMP_EQ_OQ);
		 as that is a cheaper way to load all ones into
		 a register than having to load a constant from
		 memory.  */
	      gimple *def_stmt = SSA_NAME_DEF_STMT (arg3);
	      if (is_gimple_call (def_stmt))
		{
		  tree fndecl = gimple_call_fndecl (def_stmt);
		  if (fndecl
		      && fndecl_built_in_p (fndecl, BUILT_IN_MD))
		    switch (DECL_MD_FUNCTION_CODE (fndecl))
		      {
		      case IX86_BUILTIN_CMPPD:
		      case IX86_BUILTIN_CMPPS:
		      case IX86_BUILTIN_CMPPD256:
		      case IX86_BUILTIN_CMPPS256:
			if (!integer_zerop (gimple_call_arg (def_stmt, 2)))
			  break;
			/* FALLTHRU */
		      case IX86_BUILTIN_CMPEQPD:
		      case IX86_BUILTIN_CMPEQPS:
			if (initializer_zerop (gimple_call_arg (def_stmt, 0))
			    && initializer_zerop (gimple_call_arg (def_stmt,
								   1)))
			  op0 = pc_rtx;
			break;
		      default:
			break;
		      }
		}
	    }
	}

      pat = GEN_FCN (icode) (subtarget, op0, op1, op2, op3, op4);
      if (! pat)
	return const0_rtx;
      emit_insn (pat);

      switch (fcode)
	{
	case IX86_BUILTIN_GATHER3DIV16SF:
	  if (target == NULL_RTX)
	    target = gen_reg_rtx (V8SFmode);
	  emit_insn (gen_vec_extract_lo_v16sf (target, subtarget));
	  break;
	case IX86_BUILTIN_GATHER3DIV16SI:
	  if (target == NULL_RTX)
	    target = gen_reg_rtx (V8SImode);
	  emit_insn (gen_vec_extract_lo_v16si (target, subtarget));
	  break;
	case IX86_BUILTIN_GATHER3DIV8SF:
	case IX86_BUILTIN_GATHERDIV8SF:
	  if (target == NULL_RTX)
	    target = gen_reg_rtx (V4SFmode);
	  emit_insn (gen_vec_extract_lo_v8sf (target, subtarget));
	  break;
	case IX86_BUILTIN_GATHER3DIV8SI:
	case IX86_BUILTIN_GATHERDIV8SI:
	  if (target == NULL_RTX)
	    target = gen_reg_rtx (V4SImode);
	  emit_insn (gen_vec_extract_lo_v8si (target, subtarget));
	  break;
	default:
	  target = subtarget;
	  break;
	}
      return target;

    scatter_gen:
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      arg2 = CALL_EXPR_ARG (exp, 2);
      arg3 = CALL_EXPR_ARG (exp, 3);
      arg4 = CALL_EXPR_ARG (exp, 4);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
      op2 = expand_normal (arg2);
      op3 = expand_normal (arg3);
      op4 = expand_normal (arg4);
      mode1 = insn_data[icode].operand[1].mode;
      mode2 = insn_data[icode].operand[2].mode;
      mode3 = insn_data[icode].operand[3].mode;
      mode4 = insn_data[icode].operand[4].mode;

      /* Scatter instruction stores operand op3 to memory with
	 indices from op2 and scale from op4 under writemask op1.
	 If index operand op2 has more elements then source operand
	 op3 one need to use only its low half. And vice versa.  */
      switch (fcode)
	{
	case IX86_BUILTIN_SCATTERALTSIV8DF:
	case IX86_BUILTIN_SCATTERALTSIV8DI:
	  half = gen_reg_rtx (V8SImode);
	  if (!nonimmediate_operand (op2, V16SImode))
	    op2 = copy_to_mode_reg (V16SImode, op2);
	  emit_insn (gen_vec_extract_lo_v16si (half, op2));
	  op2 = half;
	  break;
	case IX86_BUILTIN_SCATTERALTDIV16SF:
	case IX86_BUILTIN_SCATTERALTDIV16SI:
	  half = gen_reg_rtx (mode3);
	  if (mode3 == V8SFmode)
	    gen = gen_vec_extract_lo_v16sf;
	  else
	    gen = gen_vec_extract_lo_v16si;
	  if (!nonimmediate_operand (op3, GET_MODE (op3)))
	    op3 = copy_to_mode_reg (GET_MODE (op3), op3);
	  emit_insn (gen (half, op3));
	  op3 = half;
	  break;
	case IX86_BUILTIN_SCATTERALTSIV4DF:
	case IX86_BUILTIN_SCATTERALTSIV4DI:
	  half = gen_reg_rtx (V4SImode);
	  if (!nonimmediate_operand (op2, V8SImode))
	    op2 = copy_to_mode_reg (V8SImode, op2);
	  emit_insn (gen_vec_extract_lo_v8si (half, op2));
	  op2 = half;
	  break;
	case IX86_BUILTIN_SCATTERALTDIV8SF:
	case IX86_BUILTIN_SCATTERALTDIV8SI:
	  half = gen_reg_rtx (mode3);
	  if (mode3 == V4SFmode)
	    gen = gen_vec_extract_lo_v8sf;
	  else
	    gen = gen_vec_extract_lo_v8si;
	  if (!nonimmediate_operand (op3, GET_MODE (op3)))
	    op3 = copy_to_mode_reg (GET_MODE (op3), op3);
	  emit_insn (gen (half, op3));
	  op3 = half;
	  break;
	case IX86_BUILTIN_SCATTERALTSIV2DF:
	case IX86_BUILTIN_SCATTERALTSIV2DI:
	  if (!nonimmediate_operand (op2, V4SImode))
	    op2 = copy_to_mode_reg (V4SImode, op2);
	  break;
	case IX86_BUILTIN_SCATTERALTDIV4SF:
	case IX86_BUILTIN_SCATTERALTDIV4SI:
	  if (!nonimmediate_operand (op3, GET_MODE (op3)))
	    op3 = copy_to_mode_reg (GET_MODE (op3), op3);
	  break;
	default:
	  break;
	}

      /* Force memory operand only with base register here.  But we
	 don't want to do it on memory operand for other builtin
	 functions.  */
      op0 = force_reg (Pmode, convert_to_mode (Pmode, op0, 1));

      if (!insn_data[icode].operand[0].predicate (op0, Pmode))
	op0 = copy_to_mode_reg (Pmode, op0);

      op1 = fixup_modeless_constant (op1, mode1);

      if (GET_MODE (op1) == mode1 || GET_MODE (op1) == VOIDmode)
	{
	  if (!insn_data[icode].operand[1].predicate (op1, mode1))
	    op1 = copy_to_mode_reg (mode1, op1);
	}
      else
	{
	  op1 = copy_to_reg (op1);
	  op1 = lowpart_subreg (mode1, op1, GET_MODE (op1));
	}

      if (!insn_data[icode].operand[2].predicate (op2, mode2))
	op2 = copy_to_mode_reg (mode2, op2);

      if (!insn_data[icode].operand[3].predicate (op3, mode3))
	op3 = copy_to_mode_reg (mode3, op3);

      if (!insn_data[icode].operand[4].predicate (op4, mode4))
	{
	  error ("the last argument must be scale 1, 2, 4, 8");
	  return const0_rtx;
	}

      pat = GEN_FCN (icode) (op0, op1, op2, op3, op4);
      if (! pat)
	return const0_rtx;

      emit_insn (pat);
      return 0;

    vec_prefetch_gen:
      arg0 = CALL_EXPR_ARG (exp, 0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      arg2 = CALL_EXPR_ARG (exp, 2);
      arg3 = CALL_EXPR_ARG (exp, 3);
      arg4 = CALL_EXPR_ARG (exp, 4);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
      op2 = expand_normal (arg2);
      op3 = expand_normal (arg3);
      op4 = expand_normal (arg4);
      mode0 = insn_data[icode].operand[0].mode;
      mode1 = insn_data[icode].operand[1].mode;
      mode3 = insn_data[icode].operand[3].mode;
      mode4 = insn_data[icode].operand[4].mode;

      op0 = fixup_modeless_constant (op0, mode0);

      if (GET_MODE (op0) == mode0 || GET_MODE (op0) == VOIDmode)
	{
	  if (!insn_data[icode].operand[0].predicate (op0, mode0))
	    op0 = copy_to_mode_reg (mode0, op0);
	}
      else
	{
	  op0 = copy_to_reg (op0);
	  op0 = lowpart_subreg (mode0, op0, GET_MODE (op0));
	}

      if (!insn_data[icode].operand[1].predicate (op1, mode1))
	op1 = copy_to_mode_reg (mode1, op1);

      /* Force memory operand only with base register here.  But we
	 don't want to do it on memory operand for other builtin
	 functions.  */
      op2 = force_reg (Pmode, convert_to_mode (Pmode, op2, 1));

      if (!insn_data[icode].operand[2].predicate (op2, Pmode))
	op2 = copy_to_mode_reg (Pmode, op2);

      if (!insn_data[icode].operand[3].predicate (op3, mode3))
	{
	  error ("the forth argument must be scale 1, 2, 4, 8");
	  return const0_rtx;
	}

      if (!insn_data[icode].operand[4].predicate (op4, mode4))
	{
	  error ("incorrect hint operand");
	  return const0_rtx;
	}

      pat = GEN_FCN (icode) (op0, op1, op2, op3, op4);
      if (! pat)
	return const0_rtx;

      emit_insn (pat);

      return 0;

    case IX86_BUILTIN_XABORT:
      icode = CODE_FOR_xabort;
      arg0 = CALL_EXPR_ARG (exp, 0);
      op0 = expand_normal (arg0);
      mode0 = insn_data[icode].operand[0].mode;
      if (!insn_data[icode].operand[0].predicate (op0, mode0))
	{
	  error ("the argument to %<xabort%> intrinsic must "
		 "be an 8-bit immediate");
	  return const0_rtx;
	}
      emit_insn (gen_xabort (op0));
      return 0;

    case IX86_BUILTIN_RDSSPD:
    case IX86_BUILTIN_RDSSPQ:
      mode = (fcode == IX86_BUILTIN_RDSSPD ? SImode : DImode);

      if (target == 0
	  || !register_operand (target, mode))
	target = gen_reg_rtx (mode);

      op0 = force_reg (mode, const0_rtx);

      emit_insn (gen_rdssp (mode, target, op0));
      return target;

    case IX86_BUILTIN_INCSSPD:
    case IX86_BUILTIN_INCSSPQ:
      mode = (fcode == IX86_BUILTIN_INCSSPD ? SImode : DImode);

      arg0 = CALL_EXPR_ARG (exp, 0);
      op0 = expand_normal (arg0);

      op0 = force_reg (mode, op0);

      emit_insn (gen_incssp (mode, op0));
      return 0;

    case IX86_BUILTIN_HRESET:
      icode = CODE_FOR_hreset;
      arg0 = CALL_EXPR_ARG (exp, 0);
      op0 = expand_normal (arg0);
      op0 = force_reg (SImode, op0);
      emit_insn (gen_hreset (op0));
      return 0;

    case IX86_BUILTIN_RSTORSSP:
    case IX86_BUILTIN_CLRSSBSY:
      arg0 = CALL_EXPR_ARG (exp, 0);
      op0 = expand_normal (arg0);
      icode = (fcode == IX86_BUILTIN_RSTORSSP
	       ? CODE_FOR_rstorssp
	       : CODE_FOR_clrssbsy);

      if (!address_operand (op0, VOIDmode))
	{
	  op0 = convert_memory_address (Pmode, op0);
	  op0 = copy_addr_to_reg (op0);
	}
      emit_insn (GEN_FCN (icode) (gen_rtx_MEM (DImode, op0)));
      return 0;

    case IX86_BUILTIN_WRSSD:
    case IX86_BUILTIN_WRSSQ:
    case IX86_BUILTIN_WRUSSD:
    case IX86_BUILTIN_WRUSSQ:
      mode = ((fcode == IX86_BUILTIN_WRSSD
	       || fcode == IX86_BUILTIN_WRUSSD)
	      ? SImode : DImode);

      arg0 = CALL_EXPR_ARG (exp, 0);
      op0 = expand_normal (arg0);
      arg1 = CALL_EXPR_ARG (exp, 1);
      op1 = expand_normal (arg1);

      op0 = force_reg (mode, op0);

      if (!address_operand (op1, VOIDmode))
	{
	  op1 = convert_memory_address (Pmode, op1);
	  op1 = copy_addr_to_reg (op1);
	}
      op1 = gen_rtx_MEM (mode, op1);

      icode = ((fcode == IX86_BUILTIN_WRSSD
		|| fcode == IX86_BUILTIN_WRSSQ)
	       ? code_for_wrss (mode)
	       : code_for_wruss (mode));
      emit_insn (GEN_FCN (icode) (op0, op1));

      return 0;

    default:
      break;
    }

  if (fcode >= IX86_BUILTIN__BDESC_SPECIAL_ARGS_FIRST
      && fcode <= IX86_BUILTIN__BDESC_SPECIAL_ARGS_LAST)
    {
      i = fcode - IX86_BUILTIN__BDESC_SPECIAL_ARGS_FIRST;
      return ix86_expand_special_args_builtin (bdesc_special_args + i, exp,
					       target);
    }

  if (fcode >= IX86_BUILTIN__BDESC_PURE_ARGS_FIRST
      && fcode <= IX86_BUILTIN__BDESC_PURE_ARGS_LAST)
    {
      i = fcode - IX86_BUILTIN__BDESC_PURE_ARGS_FIRST;
      return ix86_expand_special_args_builtin (bdesc_pure_args + i, exp,
					       target);
    }

  if (fcode >= IX86_BUILTIN__BDESC_ARGS_FIRST
      && fcode <= IX86_BUILTIN__BDESC_ARGS_LAST)
    {
      i = fcode - IX86_BUILTIN__BDESC_ARGS_FIRST;
      rtx (*fcn) (rtx, rtx, rtx, rtx) = NULL;
      rtx (*fcn_mask) (rtx, rtx, rtx, rtx, rtx);
      rtx (*fcn_maskz) (rtx, rtx, rtx, rtx, rtx, rtx);
      int masked = 1;
      machine_mode mode, wide_mode, nar_mode;

      nar_mode  = V4SFmode;
      mode      = V16SFmode;
      wide_mode = V64SFmode;
      fcn_mask  = gen_avx5124fmaddps_4fmaddps_mask;
      fcn_maskz = gen_avx5124fmaddps_4fmaddps_maskz;

      switch (fcode)
	{
	case IX86_BUILTIN_4FMAPS:
	  fcn = gen_avx5124fmaddps_4fmaddps;
	  masked = 0;
	  goto v4fma_expand;

	case IX86_BUILTIN_4DPWSSD:
	  nar_mode  = V4SImode;
	  mode      = V16SImode;
	  wide_mode = V64SImode;
	  fcn = gen_avx5124vnniw_vp4dpwssd;
	  masked = 0;
	  goto v4fma_expand;

	case IX86_BUILTIN_4DPWSSDS:
	  nar_mode  = V4SImode;
	  mode      = V16SImode;
	  wide_mode = V64SImode;
	  fcn = gen_avx5124vnniw_vp4dpwssds;
	  masked = 0;
	  goto v4fma_expand;

	case IX86_BUILTIN_4FNMAPS:
	  fcn = gen_avx5124fmaddps_4fnmaddps;
	  masked = 0;
	  goto v4fma_expand;

	case IX86_BUILTIN_4FNMAPS_MASK:
	  fcn_mask  = gen_avx5124fmaddps_4fnmaddps_mask;
	  fcn_maskz = gen_avx5124fmaddps_4fnmaddps_maskz;
	  goto v4fma_expand;

	case IX86_BUILTIN_4DPWSSD_MASK:
	  nar_mode  = V4SImode;
	  mode      = V16SImode;
	  wide_mode = V64SImode;
	  fcn_mask  = gen_avx5124vnniw_vp4dpwssd_mask;
	  fcn_maskz = gen_avx5124vnniw_vp4dpwssd_maskz;
	  goto v4fma_expand;

	case IX86_BUILTIN_4DPWSSDS_MASK:
	  nar_mode  = V4SImode;
	  mode      = V16SImode;
	  wide_mode = V64SImode;
	  fcn_mask  = gen_avx5124vnniw_vp4dpwssds_mask;
	  fcn_maskz = gen_avx5124vnniw_vp4dpwssds_maskz;
	  goto v4fma_expand;

	case IX86_BUILTIN_4FMAPS_MASK:
	  {
	    tree args[4];
	    rtx ops[4];
	    rtx wide_reg;
	    rtx accum;
	    rtx addr;
	    rtx mem;

v4fma_expand:
	    wide_reg = gen_reg_rtx (wide_mode);
	    for (i = 0; i < 4; i++)
	      {
		args[i] = CALL_EXPR_ARG (exp, i);
		ops[i] = expand_normal (args[i]);

		emit_move_insn (gen_rtx_SUBREG (mode, wide_reg, i * 64),
				ops[i]);
	      }

	    accum = expand_normal (CALL_EXPR_ARG (exp, 4));
	    accum = force_reg (mode, accum);

	    addr = expand_normal (CALL_EXPR_ARG (exp, 5));
	    addr = force_reg (Pmode, addr);

	    mem = gen_rtx_MEM (nar_mode, addr);

	    target = gen_reg_rtx (mode);

	    emit_move_insn (target, accum);

	    if (! masked)
	      emit_insn (fcn (target, accum, wide_reg, mem));
	    else
	      {
		rtx merge, mask;
		merge = expand_normal (CALL_EXPR_ARG (exp, 6));

		mask = expand_normal (CALL_EXPR_ARG (exp, 7));

		if (CONST_INT_P (mask))
		  mask = fixup_modeless_constant (mask, HImode);

		mask = force_reg (HImode, mask);

		if (GET_MODE (mask) != HImode)
		  mask = gen_rtx_SUBREG (HImode, mask, 0);

		/* If merge is 0 then we're about to emit z-masked variant.  */
		if (const0_operand (merge, mode))
		  emit_insn (fcn_maskz (target, accum, wide_reg, mem, merge, mask));
		/* If merge is the same as accum then emit merge-masked variant.  */
		else if (CALL_EXPR_ARG (exp, 6) == CALL_EXPR_ARG (exp, 4))
		  {
		    merge = force_reg (mode, merge);
		    emit_insn (fcn_mask (target, wide_reg, mem, merge, mask));
		  }
		/* Merge with something unknown might happen if we z-mask w/ -O0.  */
		else
		  {
		    target = gen_reg_rtx (mode);
		    emit_move_insn (target, merge);
		    emit_insn (fcn_mask (target, wide_reg, mem, target, mask));
		  }
	      }
	    return target;
	  }

	case IX86_BUILTIN_4FNMASS:
	  fcn = gen_avx5124fmaddps_4fnmaddss;
	  masked = 0;
	  goto s4fma_expand;

	case IX86_BUILTIN_4FMASS:
	  fcn = gen_avx5124fmaddps_4fmaddss;
	  masked = 0;
	  goto s4fma_expand;

	case IX86_BUILTIN_4FNMASS_MASK:
	  fcn_mask = gen_avx5124fmaddps_4fnmaddss_mask;
	  fcn_maskz = gen_avx5124fmaddps_4fnmaddss_maskz;
	  goto s4fma_expand;

	case IX86_BUILTIN_4FMASS_MASK:
	  {
	    tree args[4];
	    rtx ops[4];
	    rtx wide_reg;
	    rtx accum;
	    rtx addr;
	    rtx mem;

	    fcn_mask = gen_avx5124fmaddps_4fmaddss_mask;
	    fcn_maskz = gen_avx5124fmaddps_4fmaddss_maskz;

s4fma_expand:
	    mode = V4SFmode;
	    wide_reg = gen_reg_rtx (V64SFmode);
	    for (i = 0; i < 4; i++)
	      {
		rtx tmp;
		args[i] = CALL_EXPR_ARG (exp, i);
		ops[i] = expand_normal (args[i]);

		tmp = gen_reg_rtx (SFmode);
		emit_move_insn (tmp, gen_rtx_SUBREG (SFmode, ops[i], 0));

		emit_move_insn (gen_rtx_SUBREG (V16SFmode, wide_reg, i * 64),
				gen_rtx_SUBREG (V16SFmode, tmp, 0));
	      }

	    accum = expand_normal (CALL_EXPR_ARG (exp, 4));
	    accum = force_reg (V4SFmode, accum);

	    addr = expand_normal (CALL_EXPR_ARG (exp, 5));
	    addr = force_reg (Pmode, addr);

	    mem = gen_rtx_MEM (V4SFmode, addr);

	    target = gen_reg_rtx (V4SFmode);

	    emit_move_insn (target, accum);

	    if (! masked)
	      emit_insn (fcn (target, accum, wide_reg, mem));
	    else
	      {
		rtx merge, mask;
		merge = expand_normal (CALL_EXPR_ARG (exp, 6));

		mask = expand_normal (CALL_EXPR_ARG (exp, 7));

		if (CONST_INT_P (mask))
		  mask = fixup_modeless_constant (mask, QImode);

		mask = force_reg (QImode, mask);

		if (GET_MODE (mask) != QImode)
		  mask = gen_rtx_SUBREG (QImode, mask, 0);

		/* If merge is 0 then we're about to emit z-masked variant.  */
		if (const0_operand (merge, mode))
		  emit_insn (fcn_maskz (target, accum, wide_reg, mem, merge, mask));
		/* If merge is the same as accum then emit merge-masked
		   variant.  */
		else if (CALL_EXPR_ARG (exp, 6) == CALL_EXPR_ARG (exp, 4))
		  {
		    merge = force_reg (mode, merge);
		    emit_insn (fcn_mask (target, wide_reg, mem, merge, mask));
		  }
		/* Merge with something unknown might happen if we z-mask
		   w/ -O0.  */
		else
		  {
		    target = gen_reg_rtx (mode);
		    emit_move_insn (target, merge);
		    emit_insn (fcn_mask (target, wide_reg, mem, target, mask));
		  }
		}
	      return target;
	    }
	  case IX86_BUILTIN_RDPID:
	    return ix86_expand_special_args_builtin (bdesc_args + i, exp,
						     target);
	  case IX86_BUILTIN_FABSQ:
	  case IX86_BUILTIN_COPYSIGNQ:
	    if (!TARGET_SSE)
	      /* Emit a normal call if SSE isn't available.  */
	      return expand_call (exp, target, ignore);
	    /* FALLTHRU */
	  default:
	    return ix86_expand_args_builtin (bdesc_args + i, exp, target);
	  }
    }

  if (fcode >= IX86_BUILTIN__BDESC_COMI_FIRST
      && fcode <= IX86_BUILTIN__BDESC_COMI_LAST)
    {
      i = fcode - IX86_BUILTIN__BDESC_COMI_FIRST;
      return ix86_expand_sse_comi (bdesc_comi + i, exp, target);
    }

  if (fcode >= IX86_BUILTIN__BDESC_ROUND_ARGS_FIRST
      && fcode <= IX86_BUILTIN__BDESC_ROUND_ARGS_LAST)
    {
      i = fcode - IX86_BUILTIN__BDESC_ROUND_ARGS_FIRST;
      return ix86_expand_round_builtin (bdesc_round_args + i, exp, target);
    }

  if (fcode >= IX86_BUILTIN__BDESC_PCMPESTR_FIRST
      && fcode <= IX86_BUILTIN__BDESC_PCMPESTR_LAST)
    {
      i = fcode - IX86_BUILTIN__BDESC_PCMPESTR_FIRST;
      return ix86_expand_sse_pcmpestr (bdesc_pcmpestr + i, exp, target);
    }

  if (fcode >= IX86_BUILTIN__BDESC_PCMPISTR_FIRST
      && fcode <= IX86_BUILTIN__BDESC_PCMPISTR_LAST)
    {
      i = fcode - IX86_BUILTIN__BDESC_PCMPISTR_FIRST;
      return ix86_expand_sse_pcmpistr (bdesc_pcmpistr + i, exp, target);
    }

  if (fcode >= IX86_BUILTIN__BDESC_MULTI_ARG_FIRST
      && fcode <= IX86_BUILTIN__BDESC_MULTI_ARG_LAST)
    {
      i = fcode - IX86_BUILTIN__BDESC_MULTI_ARG_FIRST;
      const struct builtin_description *d = bdesc_multi_arg + i;
      return ix86_expand_multi_arg_builtin (d->icode, exp, target,
					    (enum ix86_builtin_func_type)
					    d->flag, d->comparison);
    }

  if (fcode >= IX86_BUILTIN__BDESC_CET_FIRST
      && fcode <= IX86_BUILTIN__BDESC_CET_LAST)
    {
      i = fcode - IX86_BUILTIN__BDESC_CET_FIRST;
      return ix86_expand_special_args_builtin (bdesc_cet + i, exp,
					       target);
    }

  gcc_unreachable ();
}

/* A subroutine of ix86_expand_vector_init_duplicate.  Tries to
   fill target with val via vec_duplicate.  */

static bool
ix86_vector_duplicate_value (machine_mode mode, rtx target, rtx val)
{
  bool ok;
  rtx_insn *insn;
  rtx dup;
  /* Save/restore recog_data in case this is called from splitters
     or other routines where recog_data needs to stay valid across
     force_reg.  See PR106577.  */
  recog_data_d recog_data_save = recog_data;

  /* First attempt to recognize VAL as-is.  */
  dup = gen_vec_duplicate (mode, val);
  insn = emit_insn (gen_rtx_SET (target, dup));
  if (recog_memoized (insn) < 0)
    {
      rtx_insn *seq;
      machine_mode innermode = GET_MODE_INNER (mode);
      rtx reg;

      /* If that fails, force VAL into a register.  */

      start_sequence ();
      reg = force_reg (innermode, val);
      if (GET_MODE (reg) != innermode)
	reg = gen_lowpart (innermode, reg);
      SET_SRC (PATTERN (insn)) = gen_vec_duplicate (mode, reg);
      seq = get_insns ();
      end_sequence ();
      if (seq)
	emit_insn_before (seq, insn);

      ok = recog_memoized (insn) >= 0;
      gcc_assert (ok);
    }
  recog_data = recog_data_save;
  return true;
}

/* Get a vector mode of the same size as the original but with elements
   twice as wide.  This is only guaranteed to apply to integral vectors.  */

static machine_mode
get_mode_wider_vector (machine_mode o)
{
  /* ??? Rely on the ordering that genmodes.cc gives to vectors.  */
  machine_mode n = GET_MODE_NEXT_MODE (o).require ();
  gcc_assert (GET_MODE_NUNITS (o) == GET_MODE_NUNITS (n) * 2);
  gcc_assert (GET_MODE_SIZE (o) == GET_MODE_SIZE (n));
  return n;
}

static bool expand_vec_perm_broadcast_1 (struct expand_vec_perm_d *d);
static bool expand_vec_perm_1 (struct expand_vec_perm_d *d);

/* A subroutine of ix86_expand_vector_init.  Store into TARGET a vector
   with all elements equal to VAR.  Return true if successful.  */

bool
ix86_expand_vector_init_duplicate (bool mmx_ok, machine_mode mode,
				   rtx target, rtx val)
{
  bool ok;

  switch (mode)
    {
    case E_V2DImode:
      if (CONST_INT_P (val))
	{
	  int tmp = (int)INTVAL (val);
	  if (tmp == (int)(INTVAL (val) >> 32))
	    {
	      rtx reg = gen_reg_rtx (V4SImode);
	      ok = ix86_vector_duplicate_value (V4SImode, reg,
						GEN_INT (tmp));
	      if (ok)
		{
		  emit_move_insn (target, gen_lowpart (V2DImode, reg));
		  return true;
		}
	    }
	}
      return ix86_vector_duplicate_value (mode, target, val);

    case E_V4DImode:
      if (CONST_INT_P (val))
	{
	  int tmp = (int)INTVAL (val);
	  if (tmp == (int)(INTVAL (val) >> 32))
	    {
	      rtx reg = gen_reg_rtx (V8SImode);
	      ok = ix86_vector_duplicate_value (V8SImode, reg,
						GEN_INT (tmp));
	      if (ok)
		{
		  emit_move_insn (target, gen_lowpart (V4DImode, reg));
		  return true;
		}
	    }
	}
      return ix86_vector_duplicate_value (mode, target, val);

    case E_V2SImode:
    case E_V2SFmode:
      if (!mmx_ok)
	return false;
      /* FALLTHRU */

    case E_V4DFmode:
    case E_V8SFmode:
    case E_V8SImode:
    case E_V2DFmode:
    case E_V4SFmode:
    case E_V4SImode:
    case E_V16SImode:
    case E_V8DImode:
    case E_V16SFmode:
    case E_V8DFmode:
      return ix86_vector_duplicate_value (mode, target, val);

    case E_V4HImode:
      if (!mmx_ok)
	return false;
      if (TARGET_SSE || TARGET_3DNOW_A)
	{
	  rtx x;

	  val = gen_lowpart (SImode, val);
	  if (CONST_INT_P (val))
	    return false;
	  x = gen_rtx_TRUNCATE (HImode, val);
	  x = gen_rtx_VEC_DUPLICATE (mode, x);
	  emit_insn (gen_rtx_SET (target, x));
	  return true;
	}
      goto widen;

    case E_V4HFmode:
    case E_V4BFmode:
      if (TARGET_MMX_WITH_SSE)
	{
	  val = force_reg (GET_MODE_INNER (mode), val);
	  rtx x = gen_rtx_VEC_DUPLICATE (mode, val);
	  emit_insn (gen_rtx_SET (target, x));
	  return true;
	}
      return false;

    case E_V2HImode:
      if (TARGET_SSE2)
	{
	  rtx x;

	  val = gen_lowpart (SImode, val);
	  if (CONST_INT_P (val))
	    return false;
	  x = gen_rtx_TRUNCATE (HImode, val);
	  x = gen_rtx_VEC_DUPLICATE (mode, x);
	  emit_insn (gen_rtx_SET (target, x));
	  return true;
	}
      return false;

    case E_V2HFmode:
    case E_V2BFmode:
      if (TARGET_SSE2)
	{
	  val = force_reg (GET_MODE_INNER (mode), val);
	  rtx x = gen_rtx_VEC_DUPLICATE (mode, val);
	  emit_insn (gen_rtx_SET (target, x));
	  return true;
	}
      return false;

    case E_V8QImode:
    case E_V4QImode:
      if (!mmx_ok)
	return false;
      goto widen;

    case E_V8HImode:
      if (CONST_INT_P (val))
	goto widen;
      /* FALLTHRU */

    case E_V8HFmode:
    case E_V8BFmode:
      if (TARGET_AVX2)
	return ix86_vector_duplicate_value (mode, target, val);

      if (TARGET_SSE2)
	{
	  struct expand_vec_perm_d dperm;
	  rtx tmp1, tmp2;

	permute:
	  memset (&dperm, 0, sizeof (dperm));
	  dperm.target = target;
	  dperm.vmode = mode;
	  dperm.nelt = GET_MODE_NUNITS (mode);
	  dperm.op0 = dperm.op1 = gen_reg_rtx (mode);
	  dperm.one_operand_p = true;

	  if (mode == V8HFmode || mode == V8BFmode)
	    {
	      tmp1 = force_reg (GET_MODE_INNER (mode), val);
	      tmp2 = gen_reg_rtx (mode);
	      emit_insn (gen_vec_set_0 (mode, tmp2, CONST0_RTX (mode), tmp1));
	      tmp1 = gen_lowpart (mode, tmp2);
	    }
	  else
	    {
	      /* Extend to SImode using a paradoxical SUBREG.  */
	      tmp1 = gen_reg_rtx (SImode);
	      emit_move_insn (tmp1, gen_lowpart (SImode, val));

	      /* Insert the SImode value as
		 low element of a V4SImode vector.  */
	      tmp2 = gen_reg_rtx (V4SImode);
	      emit_insn (gen_vec_setv4si_0 (tmp2, CONST0_RTX (V4SImode), tmp1));
	      tmp1 = gen_lowpart (mode, tmp2);
	    }

	  emit_move_insn (dperm.op0, tmp1);
	  ok = (expand_vec_perm_1 (&dperm)
		|| expand_vec_perm_broadcast_1 (&dperm));
	  gcc_assert (ok);
	  return ok;
	}
      goto widen;

    case E_V16QImode:
      if (CONST_INT_P (val))
	goto widen;
      if (TARGET_AVX2)
	return ix86_vector_duplicate_value (mode, target, val);

      if (TARGET_SSE2)
	goto permute;
      goto widen;

    widen:
      /* Replicate the value once into the next wider mode and recurse.  */
      {
	machine_mode smode, wsmode, wvmode;
	rtx x;

	smode = GET_MODE_INNER (mode);
	wvmode = get_mode_wider_vector (mode);
	wsmode = GET_MODE_INNER (wvmode);

	val = convert_modes (wsmode, smode, val, true);

	if (CONST_INT_P (val))
	  {
	    x = simplify_binary_operation (ASHIFT, wsmode, val,
					   GEN_INT (GET_MODE_BITSIZE (smode)));
	    val = simplify_binary_operation (IOR, wsmode, val, x);
	  }
	else if (smode == QImode && !TARGET_PARTIAL_REG_STALL)
	  emit_insn (gen_insv_1 (wsmode, val, val));
	else
	  {
	    x = expand_simple_binop (wsmode, ASHIFT, val,
				     GEN_INT (GET_MODE_BITSIZE (smode)),
				     NULL_RTX, 1, OPTAB_LIB_WIDEN);
	    val = expand_simple_binop (wsmode, IOR, val, x, x, 1,
				       OPTAB_LIB_WIDEN);
	  }

	x = gen_reg_rtx (wvmode);
	ok = ix86_expand_vector_init_duplicate (mmx_ok, wvmode, x, val);
	if (!ok)
	  return false;
	emit_move_insn (target, gen_lowpart (GET_MODE (target), x));
	return true;
      }

    case E_V16HImode:
    case E_V32QImode:
      if (CONST_INT_P (val))
	goto widen;
      /* FALLTHRU */

    case E_V16HFmode:
    case E_V16BFmode:
      if (TARGET_AVX2)
	return ix86_vector_duplicate_value (mode, target, val);
      else
	{
	  machine_mode hvmode;
	  switch (mode)
	    {
	    case V16HImode:
	      hvmode = V8HImode;
	      break;
	    case V16HFmode:
	      hvmode = V8HFmode;
	      break;
	    case V16BFmode:
	      hvmode = V8BFmode;
	      break;
	    case V32QImode:
	      hvmode = V16QImode;
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  rtx x = gen_reg_rtx (hvmode);

	  ok = ix86_expand_vector_init_duplicate (false, hvmode, x, val);
	  if (!ok)
	    return false;

	  x = gen_rtx_VEC_CONCAT (mode, x, x);
	  emit_insn (gen_rtx_SET (target, x));
	}
      return true;

    case E_V32HImode:
    case E_V32HFmode:
    case E_V32BFmode:
    case E_V64QImode:
      gcc_assert (TARGET_EVEX512);
      if (TARGET_AVX512BW)
	return ix86_vector_duplicate_value (mode, target, val);
      else
	{
	  machine_mode hvmode;
	  switch (mode)
	    {
	    case V32HImode:
	      hvmode = V16HImode;
	      break;
	    case V32HFmode:
	      hvmode = V16HFmode;
	      break;
	    case V32BFmode:
	      hvmode = V16BFmode;
	      break;
	    case V64QImode:
	      hvmode = V32QImode;
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  rtx x = gen_reg_rtx (hvmode);

	  ok = ix86_expand_vector_init_duplicate (false, hvmode, x, val);
	  if (!ok)
	    return false;

	  x = gen_rtx_VEC_CONCAT (mode, x, x);
	  emit_insn (gen_rtx_SET (target, x));
	}
      return true;

    default:
      return false;
    }
}

/* A subroutine of ix86_expand_vector_init.  Store into TARGET a vector
   whose ONE_VAR element is VAR, and other elements are zero.  Return true
   if successful.  */

static bool
ix86_expand_vector_init_one_nonzero (bool mmx_ok, machine_mode mode,
				     rtx target, rtx var, int one_var)
{
  machine_mode vsimode;
  rtx new_target;
  rtx x, tmp;
  bool use_vector_set = false;
  rtx (*gen_vec_set_0) (rtx, rtx, rtx) = NULL;

  if (GET_MODE_SIZE (mode) == 64 && !TARGET_EVEX512)
    return false;

  switch (mode)
    {
    case E_V2DImode:
      /* For SSE4.1, we normally use vector set.  But if the second
	 element is zero and inter-unit moves are OK, we use movq
	 instead.  */
      use_vector_set = (TARGET_64BIT && TARGET_SSE4_1
			&& !(TARGET_INTER_UNIT_MOVES_TO_VEC
			     && one_var == 0));
      break;
    case E_V16QImode:
    case E_V4SImode:
    case E_V4SFmode:
      use_vector_set = TARGET_SSE4_1;
      break;
    case E_V8HImode:
      use_vector_set = TARGET_SSE2;
      gen_vec_set_0 = TARGET_AVX512FP16 && one_var == 0
	? gen_vec_setv8hi_0 : NULL;
      break;
    case E_V8QImode:
      use_vector_set = TARGET_MMX_WITH_SSE && TARGET_SSE4_1;
      break;
    case E_V4HImode:
    case E_V4HFmode:
    case E_V4BFmode:
      use_vector_set = TARGET_SSE || TARGET_3DNOW_A;
      break;
    case E_V4QImode:
      use_vector_set = TARGET_SSE4_1;
      break;
    case E_V32QImode:
      use_vector_set = TARGET_AVX;
      break;
    case E_V16HImode:
      use_vector_set = TARGET_AVX;
      gen_vec_set_0 = TARGET_AVX512FP16 && one_var == 0
	? gen_vec_setv16hi_0 : NULL;
      break;
    case E_V8SImode:
      use_vector_set = TARGET_AVX;
      gen_vec_set_0 = gen_vec_setv8si_0;
      break;
    case E_V8SFmode:
      use_vector_set = TARGET_AVX;
      gen_vec_set_0 = gen_vec_setv8sf_0;
      break;
    case E_V4DFmode:
      use_vector_set = TARGET_AVX;
      gen_vec_set_0 = gen_vec_setv4df_0;
      break;
    case E_V4DImode:
      /* Use ix86_expand_vector_set in 64bit mode only.  */
      use_vector_set = TARGET_AVX && TARGET_64BIT;
      gen_vec_set_0 = gen_vec_setv4di_0;
      break;
    case E_V16SImode:
      use_vector_set = TARGET_AVX512F && one_var == 0;
      gen_vec_set_0 = gen_vec_setv16si_0;
      break;
    case E_V16SFmode:
      use_vector_set = TARGET_AVX512F && one_var == 0;
      gen_vec_set_0 = gen_vec_setv16sf_0;
      break;
    case E_V8DFmode:
      use_vector_set = TARGET_AVX512F && one_var == 0;
      gen_vec_set_0 = gen_vec_setv8df_0;
      break;
    case E_V8DImode:
      /* Use ix86_expand_vector_set in 64bit mode only.  */
      use_vector_set = TARGET_AVX512F && TARGET_64BIT && one_var == 0;
      gen_vec_set_0 = gen_vec_setv8di_0;
      break;
    case E_V8HFmode:
      use_vector_set = TARGET_AVX512FP16 && one_var == 0;
      gen_vec_set_0 = gen_vec_setv8hf_0;
      break;
    case E_V16HFmode:
      use_vector_set = TARGET_AVX512FP16 && one_var == 0;
      gen_vec_set_0 = gen_vec_setv16hf_0;
      break;
    case E_V32HFmode:
      use_vector_set = TARGET_AVX512FP16 && one_var == 0;
      gen_vec_set_0 = gen_vec_setv32hf_0;
      break;
    case E_V8BFmode:
      use_vector_set = TARGET_AVX512FP16 && one_var == 0;
      gen_vec_set_0 = gen_vec_setv8bf_0;
      break;
    case E_V16BFmode:
      use_vector_set = TARGET_AVX512FP16 && one_var == 0;
      gen_vec_set_0 = gen_vec_setv16bf_0;
      break;
    case E_V32BFmode:
      use_vector_set = TARGET_AVX512FP16 && one_var == 0;
      gen_vec_set_0 = gen_vec_setv32bf_0;
      break;
    case E_V32HImode:
      use_vector_set = TARGET_AVX512FP16 && one_var == 0;
      gen_vec_set_0 = gen_vec_setv32hi_0;
    default:
      break;
    }

  if (use_vector_set)
    {
      if (gen_vec_set_0 && one_var == 0)
	{
	  var = force_reg (GET_MODE_INNER (mode), var);
	  emit_insn (gen_vec_set_0 (target, CONST0_RTX (mode), var));
	  return true;
	}
      emit_insn (gen_rtx_SET (target, CONST0_RTX (mode)));
      var = force_reg (GET_MODE_INNER (mode), var);
      ix86_expand_vector_set (mmx_ok, target, var, one_var);
      return true;
    }

  switch (mode)
    {
    case E_V2SFmode:
    case E_V2SImode:
      if (!mmx_ok)
	return false;
      /* FALLTHRU */

    case E_V2DFmode:
    case E_V2DImode:
      if (one_var != 0)
	return false;
      var = force_reg (GET_MODE_INNER (mode), var);
      x = gen_rtx_VEC_CONCAT (mode, var, CONST0_RTX (GET_MODE_INNER (mode)));
      emit_insn (gen_rtx_SET (target, x));
      return true;

    case E_V4SFmode:
    case E_V4SImode:
      if (!REG_P (target) || REGNO (target) < FIRST_PSEUDO_REGISTER)
	new_target = gen_reg_rtx (mode);
      else
	new_target = target;
      var = force_reg (GET_MODE_INNER (mode), var);
      x = gen_rtx_VEC_DUPLICATE (mode, var);
      x = gen_rtx_VEC_MERGE (mode, x, CONST0_RTX (mode), const1_rtx);
      emit_insn (gen_rtx_SET (new_target, x));
      if (one_var != 0)
	{
	  /* We need to shuffle the value to the correct position, so
	     create a new pseudo to store the intermediate result.  */

	  /* With SSE2, we can use the integer shuffle insns.  */
	  if (mode != V4SFmode && TARGET_SSE2)
	    {
	      emit_insn (gen_sse2_pshufd_1 (new_target, new_target,
					    const1_rtx,
					    GEN_INT (one_var == 1 ? 0 : 1),
					    GEN_INT (one_var == 2 ? 0 : 1),
					    GEN_INT (one_var == 3 ? 0 : 1)));
	      if (target != new_target)
		emit_move_insn (target, new_target);
	      return true;
	    }

	  /* Otherwise convert the intermediate result to V4SFmode and
	     use the SSE1 shuffle instructions.  */
	  if (mode != V4SFmode)
	    {
	      tmp = gen_reg_rtx (V4SFmode);
	      emit_move_insn (tmp, gen_lowpart (V4SFmode, new_target));
	    }
	  else
	    tmp = new_target;

	  emit_insn (gen_sse_shufps_v4sf (tmp, tmp, tmp,
				       const1_rtx,
				       GEN_INT (one_var == 1 ? 0 : 1),
				       GEN_INT (one_var == 2 ? 0+4 : 1+4),
				       GEN_INT (one_var == 3 ? 0+4 : 1+4)));

	  if (mode != V4SFmode)
	    emit_move_insn (target, gen_lowpart (V4SImode, tmp));
	  else if (tmp != target)
	    emit_move_insn (target, tmp);
	}
      else if (target != new_target)
	emit_move_insn (target, new_target);
      return true;

    case E_V8HImode:
    case E_V16QImode:
      vsimode = V4SImode;
      goto widen;
    case E_V4HImode:
    case E_V8QImode:
      if (!mmx_ok)
	return false;
      vsimode = V2SImode;
      goto widen;
    widen:
      if (one_var != 0)
	return false;

      /* Zero extend the variable element to SImode and recurse.  */
      var = convert_modes (SImode, GET_MODE_INNER (mode), var, true);

      x = gen_reg_rtx (vsimode);
      if (!ix86_expand_vector_init_one_nonzero (mmx_ok, vsimode, x,
						var, one_var))
	gcc_unreachable ();

      emit_move_insn (target, gen_lowpart (mode, x));
      return true;

    default:
      return false;
    }
}

/* A subroutine of ix86_expand_vector_init.  Store into TARGET a vector
   consisting of the values in VALS.  It is known that all elements
   except ONE_VAR are constants.  Return true if successful.  */

static bool
ix86_expand_vector_init_one_var (bool mmx_ok, machine_mode mode,
				 rtx target, rtx vals, int one_var)
{
  rtx var = XVECEXP (vals, 0, one_var);
  machine_mode wmode;
  rtx const_vec, x;

  const_vec = copy_rtx (vals);
  XVECEXP (const_vec, 0, one_var) = CONST0_RTX (GET_MODE_INNER (mode));
  const_vec = gen_rtx_CONST_VECTOR (mode, XVEC (const_vec, 0));

  switch (mode)
    {
    case E_V2DFmode:
    case E_V2DImode:
    case E_V2SFmode:
    case E_V2SImode:
      /* For the two element vectors, it's just as easy to use
	 the general case.  */
      return false;

    case E_V4DImode:
      /* Use ix86_expand_vector_set in 64bit mode only.  */
      if (!TARGET_64BIT)
	return false;
      /* FALLTHRU */
    case E_V8HFmode:
    case E_V16HFmode:
    case E_V8BFmode:
    case E_V16BFmode:
    case E_V4DFmode:
    case E_V8SFmode:
    case E_V8SImode:
    case E_V16HImode:
    case E_V32QImode:
    case E_V4SFmode:
    case E_V4SImode:
    case E_V8HImode:
    case E_V4HImode:
    case E_V4HFmode:
    case E_V4BFmode:
      break;

    case E_V16QImode:
      if (TARGET_SSE4_1)
	break;
      wmode = V8HImode;
      goto widen;
    case E_V8QImode:
      if (TARGET_MMX_WITH_SSE && TARGET_SSE4_1)
	break;
      wmode = V4HImode;
      goto widen;
    case E_V4QImode:
      if (TARGET_SSE4_1)
	break;
      wmode = V2HImode;
    widen:
      /* There's no way to set one QImode entry easily.  Combine
	 the variable value with its adjacent constant value, and
	 promote to an HImode set.  */
      x = XVECEXP (vals, 0, one_var ^ 1);
      if (one_var & 1)
	{
	  var = convert_modes (HImode, QImode, var, true);
	  var = expand_simple_binop (HImode, ASHIFT, var, GEN_INT (8),
				     NULL_RTX, 1, OPTAB_LIB_WIDEN);
	  x = GEN_INT (INTVAL (x) & 0xff);
	}
      else
	{
	  var = convert_modes (HImode, QImode, var, true);
	  x = gen_int_mode (UINTVAL (x) << 8, HImode);
	}
      if (x != const0_rtx)
	var = expand_simple_binop (HImode, IOR, var, x, var,
				   1, OPTAB_LIB_WIDEN);

      x = gen_reg_rtx (wmode);
      emit_move_insn (x, gen_lowpart (wmode, const_vec));
      ix86_expand_vector_set (mmx_ok, x, var, one_var >> 1);

      emit_move_insn (target, gen_lowpart (mode, x));
      return true;

    default:
      return false;
    }

  emit_move_insn (target, const_vec);
  ix86_expand_vector_set (mmx_ok, target, var, one_var);
  return true;
}

/* A subroutine of ix86_expand_vector_init_general.  Use vector
   concatenate to handle the most general case: all values variable,
   and none identical.  */

static void
ix86_expand_vector_init_concat (machine_mode mode,
				rtx target, rtx *ops, int n)
{
  machine_mode half_mode = VOIDmode;
  rtx half[2];
  rtvec v;
  int i, j;

  switch (n)
    {
    case 2:
      switch (mode)
	{
	case E_V32HFmode:
	  half_mode = V16HFmode;
	  break;
	case E_V32BFmode:
	  half_mode = V16BFmode;
	  break;
	case E_V16SImode:
	  half_mode = V8SImode;
	  break;
	case E_V16SFmode:
	  half_mode = V8SFmode;
	  break;
	case E_V8DImode:
	  half_mode = V4DImode;
	  break;
	case E_V8DFmode:
	  half_mode = V4DFmode;
	  break;
	case E_V16HFmode:
	  half_mode = V8HFmode;
	  break;
	case E_V16BFmode:
	  half_mode = V8BFmode;
	  break;
	case E_V8SImode:
	  half_mode = V4SImode;
	  break;
	case E_V8SFmode:
	  half_mode = V4SFmode;
	  break;
	case E_V4DImode:
	  half_mode = V2DImode;
	  break;
	case E_V4DFmode:
	  half_mode = V2DFmode;
	  break;
	case E_V4SImode:
	  half_mode = V2SImode;
	  break;
	case E_V4SFmode:
	  half_mode = V2SFmode;
	  break;
	case E_V2DImode:
	  half_mode = DImode;
	  break;
	case E_V2SImode:
	  half_mode = SImode;
	  break;
	case E_V2DFmode:
	  half_mode = DFmode;
	  break;
	case E_V2SFmode:
	  half_mode = SFmode;
	  break;
	default:
	  gcc_unreachable ();
	}

      if (!register_operand (ops[1], half_mode))
	ops[1] = force_reg (half_mode, ops[1]);
      if (!register_operand (ops[0], half_mode))
	ops[0] = force_reg (half_mode, ops[0]);
      emit_insn (gen_rtx_SET (target, gen_rtx_VEC_CONCAT (mode, ops[0],
							  ops[1])));
      break;

    case 4:
      switch (mode)
	{
	case E_V4DImode:
	  half_mode = V2DImode;
	  break;
	case E_V4DFmode:
	  half_mode = V2DFmode;
	  break;
	case E_V4SImode:
	  half_mode = V2SImode;
	  break;
	case E_V4SFmode:
	  half_mode = V2SFmode;
	  break;
	default:
	  gcc_unreachable ();
	}
      goto half;

    case 8:
      switch (mode)
	{
	case E_V8DImode:
	  half_mode = V4DImode;
	  break;
	case E_V8DFmode:
	  half_mode = V4DFmode;
	  break;
	case E_V8SImode:
	  half_mode = V4SImode;
	  break;
	case E_V8SFmode:
	  half_mode = V4SFmode;
	  break;
	default:
	  gcc_unreachable ();
	}
      goto half;

    case 16:
      switch (mode)
	{
	case E_V16SImode:
	  half_mode = V8SImode;
	  break;
	case E_V16SFmode:
	  half_mode = V8SFmode;
	  break;
	default:
	  gcc_unreachable ();
	}
      goto half;

half:
      /* FIXME: We process inputs backward to help RA.  PR 36222.  */
      i = n - 1;
      for (j = 1; j != -1; j--)
	{
	  half[j] = gen_reg_rtx (half_mode);
	  switch (n >> 1)
	    {
	    case 2:
	      v = gen_rtvec (2, ops[i-1], ops[i]);
	      i -= 2;
	      break;
	    case 4:
	      v = gen_rtvec (4, ops[i-3], ops[i-2], ops[i-1], ops[i]);
	      i -= 4;
	      break;
	    case 8:
	      v = gen_rtvec (8, ops[i-7], ops[i-6], ops[i-5], ops[i-4],
			     ops[i-3], ops[i-2], ops[i-1], ops[i]);
	      i -= 8;
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  ix86_expand_vector_init (false, half[j],
				   gen_rtx_PARALLEL (half_mode, v));
	}

      ix86_expand_vector_init_concat (mode, target, half, 2);
      break;

    default:
      gcc_unreachable ();
    }
}

/* A subroutine of ix86_expand_vector_init_general.  Use vector
   interleave to handle the most general case: all values variable,
   and none identical.  */

static void
ix86_expand_vector_init_interleave (machine_mode mode,
				    rtx target, rtx *ops, int n)
{
  machine_mode first_imode, second_imode, third_imode, inner_mode;
  int i, j;
  rtx op, op0, op1;
  rtx (*gen_load_even) (rtx, rtx, rtx);
  rtx (*gen_interleave_first_low) (rtx, rtx, rtx);
  rtx (*gen_interleave_second_low) (rtx, rtx, rtx);

  switch (mode)
    {
    case E_V8HFmode:
      gen_load_even = gen_vec_interleave_lowv8hf;
      gen_interleave_first_low = gen_vec_interleave_lowv4si;
      gen_interleave_second_low = gen_vec_interleave_lowv2di;
      inner_mode = HFmode;
      first_imode = V4SImode;
      second_imode = V2DImode;
      third_imode = VOIDmode;
      break;
    case E_V8BFmode:
      gen_load_even = gen_vec_interleave_lowv8bf;
      gen_interleave_first_low = gen_vec_interleave_lowv4si;
      gen_interleave_second_low = gen_vec_interleave_lowv2di;
      inner_mode = BFmode;
      first_imode = V4SImode;
      second_imode = V2DImode;
      third_imode = VOIDmode;
      break;
    case E_V8HImode:
      gen_load_even = gen_vec_setv8hi;
      gen_interleave_first_low = gen_vec_interleave_lowv4si;
      gen_interleave_second_low = gen_vec_interleave_lowv2di;
      inner_mode = HImode;
      first_imode = V4SImode;
      second_imode = V2DImode;
      third_imode = VOIDmode;
      break;
    case E_V16QImode:
      gen_load_even = gen_vec_setv16qi;
      gen_interleave_first_low = gen_vec_interleave_lowv8hi;
      gen_interleave_second_low = gen_vec_interleave_lowv4si;
      inner_mode = QImode;
      first_imode = V8HImode;
      second_imode = V4SImode;
      third_imode = V2DImode;
      break;
    default:
      gcc_unreachable ();
    }

  for (i = 0; i < n; i++)
    {
      op = ops [i + i];
      if (inner_mode == HFmode || inner_mode == BFmode)
	{
	  rtx even, odd;
	  /* Use vpuncklwd to pack 2 HFmode or BFmode.  */
	  machine_mode vec_mode =
	    (inner_mode == HFmode) ? V8HFmode : V8BFmode;
	  op0 = gen_reg_rtx (vec_mode);
	  even = lowpart_subreg (vec_mode,
				 force_reg (inner_mode, op), inner_mode);
	  odd = lowpart_subreg (vec_mode,
				force_reg (inner_mode, ops[i + i + 1]),
				inner_mode);
	  emit_insn (gen_load_even (op0, even, odd));
	}
      else
	{
	  /* Extend the odd elment to SImode using a paradoxical SUBREG.  */
	  op0 = gen_reg_rtx (SImode);
	  emit_move_insn (op0, gen_lowpart (SImode, op));

	  /* Insert the SImode value as low element of V4SImode vector.  */
	  op1 = gen_reg_rtx (V4SImode);
	  op0 = gen_rtx_VEC_MERGE (V4SImode,
				   gen_rtx_VEC_DUPLICATE (V4SImode,
							  op0),
				   CONST0_RTX (V4SImode),
				   const1_rtx);
	  emit_insn (gen_rtx_SET (op1, op0));

	  /* Cast the V4SImode vector back to a vector in orignal mode.  */
	  op0 = gen_reg_rtx (mode);
	  emit_move_insn (op0, gen_lowpart (mode, op1));

	  /* Load even elements into the second position.  */
	  emit_insn (gen_load_even (op0,
				    force_reg (inner_mode,
					       ops[i + i + 1]),
				    const1_rtx));
	}

      /* Cast vector to FIRST_IMODE vector.  */
      ops[i] = gen_reg_rtx (first_imode);
      emit_move_insn (ops[i], gen_lowpart (first_imode, op0));
    }

  /* Interleave low FIRST_IMODE vectors.  */
  for (i = j = 0; i < n; i += 2, j++)
    {
      op0 = gen_reg_rtx (first_imode);
      emit_insn (gen_interleave_first_low (op0, ops[i], ops[i + 1]));

      /* Cast FIRST_IMODE vector to SECOND_IMODE vector.  */
      ops[j] = gen_reg_rtx (second_imode);
      emit_move_insn (ops[j], gen_lowpart (second_imode, op0));
    }

  /* Interleave low SECOND_IMODE vectors.  */
  switch (second_imode)
    {
    case E_V4SImode:
      for (i = j = 0; i < n / 2; i += 2, j++)
	{
	  op0 = gen_reg_rtx (second_imode);
	  emit_insn (gen_interleave_second_low (op0, ops[i],
						ops[i + 1]));

	  /* Cast the SECOND_IMODE vector to the THIRD_IMODE
	     vector.  */
	  ops[j] = gen_reg_rtx (third_imode);
	  emit_move_insn (ops[j], gen_lowpart (third_imode, op0));
	}
      second_imode = V2DImode;
      gen_interleave_second_low = gen_vec_interleave_lowv2di;
      /* FALLTHRU */

    case E_V2DImode:
      op0 = gen_reg_rtx (second_imode);
      emit_insn (gen_interleave_second_low (op0, ops[0],
					    ops[1]));

      /* Cast the SECOND_IMODE vector back to a vector on original
	 mode.  */
      emit_insn (gen_rtx_SET (target, gen_lowpart (mode, op0)));
      break;

    default:
      gcc_unreachable ();
    }
}

/* A subroutine of ix86_expand_vector_init.  Handle the most general case:
   all values variable, and none identical.  */

static void
ix86_expand_vector_init_general (bool mmx_ok, machine_mode mode,
				 rtx target, rtx vals)
{
  rtx ops[64], op0, op1, op2, op3, op4, op5;
  machine_mode half_mode = VOIDmode;
  machine_mode quarter_mode = VOIDmode;
  machine_mode int_inner_mode = VOIDmode;
  int n, i;

  switch (mode)
    {
    case E_V2SFmode:
    case E_V2SImode:
      if (!mmx_ok && !TARGET_SSE)
	break;
      /* FALLTHRU */

    case E_V16SImode:
    case E_V16SFmode:
    case E_V8DFmode:
    case E_V8DImode:
    case E_V8SFmode:
    case E_V8SImode:
    case E_V4DFmode:
    case E_V4DImode:
    case E_V4SFmode:
    case E_V4SImode:
    case E_V2DFmode:
    case E_V2DImode:
      n = GET_MODE_NUNITS (mode);
      for (i = 0; i < n; i++)
	ops[i] = XVECEXP (vals, 0, i);
      ix86_expand_vector_init_concat (mode, target, ops, n);
      return;

    case E_V2TImode:
      for (i = 0; i < 2; i++)
	ops[i] = gen_lowpart (V2DImode, XVECEXP (vals, 0, i));
      op0 = gen_reg_rtx (V4DImode);
      ix86_expand_vector_init_concat (V4DImode, op0, ops, 2);
      emit_move_insn (target, gen_lowpart (GET_MODE (target), op0));
      return;

    case E_V4TImode:
      for (i = 0; i < 4; i++)
	ops[i] = gen_lowpart (V2DImode, XVECEXP (vals, 0, i));
      ops[4] = gen_reg_rtx (V4DImode);
      ix86_expand_vector_init_concat (V4DImode, ops[4], ops, 2);
      ops[5] = gen_reg_rtx (V4DImode);
      ix86_expand_vector_init_concat (V4DImode, ops[5], ops + 2, 2);
      op0 = gen_reg_rtx (V8DImode);
      ix86_expand_vector_init_concat (V8DImode, op0, ops + 4, 2);
      emit_move_insn (target, gen_lowpart (GET_MODE (target), op0));
      return;

    case E_V32QImode:
      half_mode = V16QImode;
      goto half;

    case E_V16HImode:
      half_mode = V8HImode;
      goto half;

    case E_V16HFmode:
      half_mode = V8HFmode;
      goto half;

    case E_V16BFmode:
      half_mode = V8BFmode;
      goto half;

half:
      n = GET_MODE_NUNITS (mode);
      for (i = 0; i < n; i++)
	ops[i] = XVECEXP (vals, 0, i);
      op0 = gen_reg_rtx (half_mode);
      op1 = gen_reg_rtx (half_mode);
      ix86_expand_vector_init_interleave (half_mode, op0, ops,
					  n >> 2);
      ix86_expand_vector_init_interleave (half_mode, op1,
					  &ops [n >> 1], n >> 2);
      emit_insn (gen_rtx_SET (target, gen_rtx_VEC_CONCAT (mode, op0, op1)));
      return;

    case E_V64QImode:
      quarter_mode = V16QImode;
      half_mode = V32QImode;
      goto quarter;

    case E_V32HImode:
      quarter_mode = V8HImode;
      half_mode = V16HImode;
      goto quarter;

    case E_V32HFmode:
      quarter_mode = V8HFmode;
      half_mode = V16HFmode;
      goto quarter;

    case E_V32BFmode:
      quarter_mode = V8BFmode;
      half_mode = V16BFmode;
      goto quarter;

quarter:
      n = GET_MODE_NUNITS (mode);
      for (i = 0; i < n; i++)
	ops[i] = XVECEXP (vals, 0, i);
      op0 = gen_reg_rtx (quarter_mode);
      op1 = gen_reg_rtx (quarter_mode);
      op2 = gen_reg_rtx (quarter_mode);
      op3 = gen_reg_rtx (quarter_mode);
      op4 = gen_reg_rtx (half_mode);
      op5 = gen_reg_rtx (half_mode);
      ix86_expand_vector_init_interleave (quarter_mode, op0, ops,
					  n >> 3);
      ix86_expand_vector_init_interleave (quarter_mode, op1,
					  &ops [n >> 2], n >> 3);
      ix86_expand_vector_init_interleave (quarter_mode, op2,
					  &ops [n >> 1], n >> 3);
      ix86_expand_vector_init_interleave (quarter_mode, op3,
					  &ops [(n >> 1) | (n >> 2)], n >> 3);
      emit_insn (gen_rtx_SET (op4, gen_rtx_VEC_CONCAT (half_mode, op0, op1)));
      emit_insn (gen_rtx_SET (op5, gen_rtx_VEC_CONCAT (half_mode, op2, op3)));
      emit_insn (gen_rtx_SET (target, gen_rtx_VEC_CONCAT (mode, op4, op5)));
      return;

    case E_V16QImode:
      if (!TARGET_SSE4_1)
	break;
      /* FALLTHRU */

    case E_V8HImode:
      if (!TARGET_SSE2)
	break;

      /* Don't use ix86_expand_vector_init_interleave if we can't
	 move from GPR to SSE register directly.  */
      if (!TARGET_INTER_UNIT_MOVES_TO_VEC)
	break;
      /* FALLTHRU */

    case E_V8HFmode:
    case E_V8BFmode:

      n = GET_MODE_NUNITS (mode);
      for (i = 0; i < n; i++)
	ops[i] = XVECEXP (vals, 0, i);
      ix86_expand_vector_init_interleave (mode, target, ops, n >> 1);
      return;

    case E_V4HFmode:
    case E_V4BFmode:
    case E_V2HFmode:
    case E_V2BFmode:
      int_inner_mode = HImode;
      break;

    case E_V4HImode:
    case E_V8QImode:

    case E_V2HImode:
    case E_V4QImode:
      break;

    default:
      gcc_unreachable ();
    }

    {
      int i, j, n_elts, n_words, n_elt_per_word;
      machine_mode tmp_mode, inner_mode;
      rtx words[4], shift;

      tmp_mode = (GET_MODE_SIZE (mode) < UNITS_PER_WORD) ? SImode : word_mode;

      inner_mode = GET_MODE_INNER (mode);
      n_elts = GET_MODE_NUNITS (mode);
      n_words = GET_MODE_SIZE (mode) / GET_MODE_SIZE (tmp_mode);
      n_elt_per_word = n_elts / n_words;
      shift = GEN_INT (GET_MODE_BITSIZE (inner_mode));

      for (i = 0; i < n_words; ++i)
	{
	  rtx word = NULL_RTX;

	  for (j = 0; j < n_elt_per_word; ++j)
	    {
	      rtx elt = XVECEXP (vals, 0, (i+1)*n_elt_per_word - j - 1);
	      if (int_inner_mode != E_VOIDmode)
		{
		  gcc_assert (TARGET_SSE2 && int_inner_mode == HImode);
		  rtx tmp = gen_reg_rtx (int_inner_mode);
		  elt = lowpart_subreg (int_inner_mode,
					force_reg (inner_mode, elt),
					inner_mode);
		  emit_move_insn (tmp, elt);
		  elt = tmp;
		}
	      elt = convert_modes (tmp_mode, inner_mode, elt, true);

	      if (j == 0)
		word = elt;
	      else
		{
		  word = expand_simple_binop (tmp_mode, ASHIFT, word, shift,
					      NULL_RTX, 1, OPTAB_LIB_WIDEN);
		  word = expand_simple_binop (tmp_mode, IOR, word, elt,
					      NULL_RTX, 1, OPTAB_LIB_WIDEN);
		}
	    }

	  words[i] = word;
	}

      if (n_words == 1)
	emit_move_insn (target, gen_lowpart (mode, words[0]));
      else if (n_words == 2)
	{
	  gcc_assert (tmp_mode == DImode || tmp_mode == SImode);
	  machine_mode concat_mode = tmp_mode == DImode ? V2DImode : V2SImode;
	  rtx tmp = gen_reg_rtx (concat_mode);
	  vals = gen_rtx_PARALLEL (concat_mode, gen_rtvec_v (2, words));
	  ix86_expand_vector_init_general (mmx_ok, concat_mode, tmp, vals);
	  emit_move_insn (target, gen_lowpart (mode, tmp));
	}
      else if (n_words == 4)
	{
	  rtx tmp = gen_reg_rtx (V4SImode);
	  gcc_assert (tmp_mode == SImode);
	  vals = gen_rtx_PARALLEL (V4SImode, gen_rtvec_v (4, words));
	  ix86_expand_vector_init_general (false, V4SImode, tmp, vals);
	  emit_move_insn (target, gen_lowpart (mode, tmp));
	}
      else
	gcc_unreachable ();
    }
}

/* Initialize vector TARGET via VALS.  Suppress the use of MMX
   instructions unless MMX_OK is true.  */

void
ix86_expand_vector_init (bool mmx_ok, rtx target, rtx vals)
{
  machine_mode mode = GET_MODE (target);
  machine_mode inner_mode = GET_MODE_INNER (mode);
  int n_elts = GET_MODE_NUNITS (mode);
  int n_var = 0, one_var = -1;
  bool all_same = true, all_const_zero = true;
  int i;
  rtx x;

  /* Handle first initialization from vector elts.  */
  if (n_elts != XVECLEN (vals, 0))
    {
      rtx subtarget = target;
      x = XVECEXP (vals, 0, 0);
      gcc_assert (GET_MODE_INNER (GET_MODE (x)) == inner_mode);
      if (GET_MODE_NUNITS (GET_MODE (x)) * 2 == n_elts)
	{
	  rtx ops[2] = { XVECEXP (vals, 0, 0), XVECEXP (vals, 0, 1) };
	  if (inner_mode == QImode
	      || inner_mode == HImode
	      || inner_mode == TImode
	      || inner_mode == HFmode
	      || inner_mode == BFmode)
	    {
	      unsigned int n_bits = n_elts * GET_MODE_SIZE (inner_mode);
	      scalar_mode elt_mode = inner_mode == TImode ? DImode : SImode;
	      n_bits /= GET_MODE_SIZE (elt_mode);
	      mode = mode_for_vector (elt_mode, n_bits).require ();
	      inner_mode = mode_for_vector (elt_mode, n_bits / 2).require ();
	      ops[0] = gen_lowpart (inner_mode, ops[0]);
	      ops[1] = gen_lowpart (inner_mode, ops[1]);
	      subtarget = gen_reg_rtx (mode);
	    }
	  ix86_expand_vector_init_concat (mode, subtarget, ops, 2);
	  if (subtarget != target)
	    emit_move_insn (target, gen_lowpart (GET_MODE (target), subtarget));
	  return;
	}
      gcc_unreachable ();
    }

  for (i = 0; i < n_elts; ++i)
    {
      x = XVECEXP (vals, 0, i);
      if (!(CONST_SCALAR_INT_P (x)
	    || CONST_DOUBLE_P (x)
	    || CONST_FIXED_P (x)))
	n_var++, one_var = i;
      else if (x != CONST0_RTX (inner_mode))
	all_const_zero = false;
      if (i > 0 && !rtx_equal_p (x, XVECEXP (vals, 0, 0)))
	all_same = false;
    }

  /* If all values are identical, broadcast the value.  */
  if (all_same
      && ix86_expand_vector_init_duplicate (mmx_ok, mode, target,
					    XVECEXP (vals, 0, 0)))
    return;

  /* Constants are best loaded from the constant pool.  */
  if (n_var == 0)
    {
      emit_move_insn (target, gen_rtx_CONST_VECTOR (mode, XVEC (vals, 0)));
      return;
    }

  /* Values where only one field is non-constant are best loaded from
     the pool and overwritten via move later.  */
  if (n_var == 1)
    {
      if (all_const_zero
	  && ix86_expand_vector_init_one_nonzero (mmx_ok, mode, target,
						  XVECEXP (vals, 0, one_var),
						  one_var))
	return;

      if (ix86_expand_vector_init_one_var (mmx_ok, mode, target, vals, one_var))
	return;
    }

  ix86_expand_vector_init_general (mmx_ok, mode, target, vals);
}

/* Implemented as
   V setg (V v, int idx, T val)
   {
     V idxv = (V){idx, idx, idx, idx, idx, idx, idx, idx};
     V valv = (V){val, val, val, val, val, val, val, val};
     V mask = ((V){0, 1, 2, 3, 4, 5, 6, 7} == idxv);
     v = (v & ~mask) | (valv & mask);
     return v;
   }.  */
void
ix86_expand_vector_set_var (rtx target, rtx val, rtx idx)
{
  rtx vec[64];
  machine_mode mode = GET_MODE (target);
  machine_mode cmp_mode = mode;
  int n_elts = GET_MODE_NUNITS (mode);
  rtx valv,idxv,constv,idx_tmp;
  bool ok = false;

  /* 512-bits vector byte/word broadcast and comparison only available
     under TARGET_AVX512BW, break 512-bits vector into two 256-bits vector
     when without TARGET_AVX512BW.  */
  if ((mode == V32HImode || mode == V32HFmode || mode == V32BFmode
       || mode == V64QImode)
      && !TARGET_AVX512BW)
    {
      gcc_assert (TARGET_AVX512F);
      rtx vhi, vlo, idx_hi;
      machine_mode half_mode;
      rtx (*extract_hi)(rtx, rtx);
      rtx (*extract_lo)(rtx, rtx);

      if (mode == V32HImode)
	{
	  half_mode = V16HImode;
	  extract_hi = gen_vec_extract_hi_v32hi;
	  extract_lo = gen_vec_extract_lo_v32hi;
	}
      else if (mode == V32HFmode)
	{
	  half_mode = V16HFmode;
	  extract_hi = gen_vec_extract_hi_v32hf;
	  extract_lo = gen_vec_extract_lo_v32hf;
	}
      else if (mode == V32BFmode)
	{
	  half_mode = V16BFmode;
	  extract_hi = gen_vec_extract_hi_v32bf;
	  extract_lo = gen_vec_extract_lo_v32bf;
	}
      else
	{
	  half_mode = V32QImode;
	  extract_hi = gen_vec_extract_hi_v64qi;
	  extract_lo = gen_vec_extract_lo_v64qi;
	}

      vhi = gen_reg_rtx (half_mode);
      vlo = gen_reg_rtx (half_mode);
      idx_hi = gen_reg_rtx (GET_MODE (idx));
      emit_insn (extract_hi (vhi, target));
      emit_insn (extract_lo (vlo, target));
      vec[0] = idx_hi;
      vec[1] = idx;
      vec[2] = GEN_INT (n_elts/2);
      ix86_expand_binary_operator (MINUS, GET_MODE (idx), vec);
      ix86_expand_vector_set_var (vhi, val, idx_hi);
      ix86_expand_vector_set_var (vlo, val, idx);
      emit_insn (gen_rtx_SET (target, gen_rtx_VEC_CONCAT (mode, vlo, vhi)));
      return;
    }

  if (FLOAT_MODE_P (GET_MODE_INNER (mode)))
    {
      switch (mode)
	{
	case E_V2DFmode:
	  cmp_mode = V2DImode;
	  break;
	case E_V4DFmode:
	  cmp_mode = V4DImode;
	  break;
	case E_V8DFmode:
	  cmp_mode = V8DImode;
	  break;
	case E_V2SFmode:
	  cmp_mode = V2SImode;
	  break;
	case E_V4SFmode:
	  cmp_mode = V4SImode;
	  break;
	case E_V8SFmode:
	  cmp_mode = V8SImode;
	  break;
	case E_V16SFmode:
	  cmp_mode = V16SImode;
	  break;
	case E_V2HFmode:
	case E_V2BFmode:
	  cmp_mode = V2HImode;
	  break;
	case E_V4HFmode:
	case E_V4BFmode:
	  cmp_mode = V4HImode;
	  break;
	case E_V8HFmode:
	  cmp_mode = V8HImode;
	  break;
	case E_V16HFmode:
	  cmp_mode = V16HImode;
	  break;
	case E_V32HFmode:
	  cmp_mode = V32HImode;
	  break;
	case E_V8BFmode:
	  cmp_mode = V8HImode;
	  break;
	case E_V16BFmode:
	  cmp_mode = V16HImode;
	  break;
	case E_V32BFmode:
	  cmp_mode = V32HImode;
	  break;
	default:
	  gcc_unreachable ();
	}
    }

  for (int i = 0; i != n_elts; i++)
    vec[i] = GEN_INT (i);
  constv = gen_rtx_CONST_VECTOR (cmp_mode, gen_rtvec_v (n_elts, vec));
  valv = gen_reg_rtx (mode);
  idxv = gen_reg_rtx (cmp_mode);
  idx_tmp = convert_to_mode (GET_MODE_INNER (cmp_mode), idx, 1);

  ok = ix86_expand_vector_init_duplicate (TARGET_MMX_WITH_SSE,
					  mode, valv, val);
  gcc_assert (ok);
  ok = ix86_expand_vector_init_duplicate (TARGET_MMX_WITH_SSE,
					  cmp_mode, idxv, idx_tmp);
  gcc_assert (ok);
  vec[0] = target;
  vec[1] = valv;
  vec[2] = target;
  vec[3] = gen_rtx_EQ (mode, idxv, constv);
  vec[4] = idxv;
  vec[5] = constv;
  ok = ix86_expand_int_vcond (vec);
  gcc_assert (ok);
}

void
ix86_expand_vector_set (bool mmx_ok, rtx target, rtx val, int elt)
{
  machine_mode mode = GET_MODE (target);
  machine_mode inner_mode = GET_MODE_INNER (mode);
  machine_mode half_mode;
  bool use_vec_merge = false;
  bool blendm_const = false;
  rtx tmp;
  static rtx (*gen_extract[8][2]) (rtx, rtx)
    = {
	{ gen_vec_extract_lo_v32qi, gen_vec_extract_hi_v32qi },
	{ gen_vec_extract_lo_v16hi, gen_vec_extract_hi_v16hi },
	{ gen_vec_extract_lo_v8si, gen_vec_extract_hi_v8si },
	{ gen_vec_extract_lo_v4di, gen_vec_extract_hi_v4di },
	{ gen_vec_extract_lo_v8sf, gen_vec_extract_hi_v8sf },
	{ gen_vec_extract_lo_v4df, gen_vec_extract_hi_v4df },
	{ gen_vec_extract_lo_v16hf, gen_vec_extract_hi_v16hf },
	{ gen_vec_extract_lo_v16bf, gen_vec_extract_hi_v16bf }
      };
  static rtx (*gen_insert[8][2]) (rtx, rtx, rtx)
    = {
	{ gen_vec_set_lo_v32qi, gen_vec_set_hi_v32qi },
	{ gen_vec_set_lo_v16hi, gen_vec_set_hi_v16hi },
	{ gen_vec_set_lo_v8si, gen_vec_set_hi_v8si },
	{ gen_vec_set_lo_v4di, gen_vec_set_hi_v4di },
	{ gen_vec_set_lo_v8sf, gen_vec_set_hi_v8sf },
	{ gen_vec_set_lo_v4df, gen_vec_set_hi_v4df },
	{ gen_vec_set_lo_v16hf, gen_vec_set_hi_v16hf },
	{ gen_vec_set_lo_v16bf, gen_vec_set_hi_v16bf },
      };
  int i, j, n;
  machine_mode mmode = VOIDmode;
  rtx (*gen_blendm) (rtx, rtx, rtx, rtx);

  switch (mode)
    {
    case E_V2SImode:
      use_vec_merge = TARGET_MMX_WITH_SSE && TARGET_SSE4_1;
      if (use_vec_merge)
	break;
      /* FALLTHRU */

    case E_V2SFmode:
      if (mmx_ok)
	{
	  tmp = gen_reg_rtx (GET_MODE_INNER (mode));
	  ix86_expand_vector_extract (true, tmp, target, 1 - elt);
	  if (elt == 0)
	    tmp = gen_rtx_VEC_CONCAT (mode, val, tmp);
	  else
	    tmp = gen_rtx_VEC_CONCAT (mode, tmp, val);
	  emit_insn (gen_rtx_SET (target, tmp));
	  return;
	}
      break;

    case E_V2DImode:
      use_vec_merge = TARGET_SSE4_1 && TARGET_64BIT;
      if (use_vec_merge)
	break;

      tmp = gen_reg_rtx (GET_MODE_INNER (mode));
      ix86_expand_vector_extract (false, tmp, target, 1 - elt);
      if (elt == 0)
	tmp = gen_rtx_VEC_CONCAT (mode, val, tmp);
      else
	tmp = gen_rtx_VEC_CONCAT (mode, tmp, val);
      emit_insn (gen_rtx_SET (target, tmp));
      return;

    case E_V2DFmode:
      /* NB: For ELT == 0, use standard scalar operation patterns which
	 preserve the rest of the vector for combiner:

	 (vec_merge:V2DF
	   (vec_duplicate:V2DF (reg:DF))
	   (reg:V2DF)
	   (const_int 1))
       */
      if (elt == 0)
	goto do_vec_merge;

      {
	rtx op0, op1;

	/* For the two element vectors, we implement a VEC_CONCAT with
	   the extraction of the other element.  */

	tmp = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (1, GEN_INT (1 - elt)));
	tmp = gen_rtx_VEC_SELECT (inner_mode, target, tmp);

	if (elt == 0)
	  op0 = val, op1 = tmp;
	else
	  op0 = tmp, op1 = val;

	tmp = gen_rtx_VEC_CONCAT (mode, op0, op1);
	emit_insn (gen_rtx_SET (target, tmp));
      }
      return;

    case E_V4SFmode:
      use_vec_merge = TARGET_SSE4_1;
      if (use_vec_merge)
	break;

      switch (elt)
	{
	case 0:
	  use_vec_merge = true;
	  break;

	case 1:
	  /* tmp = target = A B C D */
	  tmp = copy_to_reg (target);
	  /* target = A A B B */
	  emit_insn (gen_vec_interleave_lowv4sf (target, target, target));
	  /* target = X A B B */
	  ix86_expand_vector_set (false, target, val, 0);
	  /* target = A X C D  */
	  emit_insn (gen_sse_shufps_v4sf (target, target, tmp,
					  const1_rtx, const0_rtx,
					  GEN_INT (2+4), GEN_INT (3+4)));
	  return;

	case 2:
	  /* tmp = target = A B C D */
	  tmp = copy_to_reg (target);
	  /* tmp = X B C D */
	  ix86_expand_vector_set (false, tmp, val, 0);
	  /* target = A B X D */
	  emit_insn (gen_sse_shufps_v4sf (target, target, tmp,
					  const0_rtx, const1_rtx,
					  GEN_INT (0+4), GEN_INT (3+4)));
	  return;

	case 3:
	  /* tmp = target = A B C D */
	  tmp = copy_to_reg (target);
	  /* tmp = X B C D */
	  ix86_expand_vector_set (false, tmp, val, 0);
	  /* target = A B X D */
	  emit_insn (gen_sse_shufps_v4sf (target, target, tmp,
					  const0_rtx, const1_rtx,
					  GEN_INT (2+4), GEN_INT (0+4)));
	  return;

	default:
	  gcc_unreachable ();
	}
      break;

    case E_V4SImode:
      use_vec_merge = TARGET_SSE4_1;
      if (use_vec_merge)
	break;

      /* Element 0 handled by vec_merge below.  */
      if (elt == 0)
	{
	  use_vec_merge = true;
	  break;
	}

      if (TARGET_SSE2)
	{
	  /* With SSE2, use integer shuffles to swap element 0 and ELT,
	     store into element 0, then shuffle them back.  */

	  rtx order[4];

	  order[0] = GEN_INT (elt);
	  order[1] = const1_rtx;
	  order[2] = const2_rtx;
	  order[3] = GEN_INT (3);
	  order[elt] = const0_rtx;

	  emit_insn (gen_sse2_pshufd_1 (target, target, order[0],
					order[1], order[2], order[3]));

	  ix86_expand_vector_set (false, target, val, 0);

	  emit_insn (gen_sse2_pshufd_1 (target, target, order[0],
					order[1], order[2], order[3]));
	}
      else
	{
	  /* For SSE1, we have to reuse the V4SF code.  */
	  rtx t = gen_reg_rtx (V4SFmode);
	  emit_move_insn (t, gen_lowpart (V4SFmode, target));
	  ix86_expand_vector_set (false, t, gen_lowpart (SFmode, val), elt);
	  emit_move_insn (target, gen_lowpart (mode, t));
	}
      return;

    case E_V8HImode:
    case E_V8HFmode:
    case E_V8BFmode:
    case E_V2HImode:
    case E_V2HFmode:
    case E_V2BFmode:
      use_vec_merge = TARGET_SSE2;
      break;
    case E_V4HImode:
    case E_V4HFmode:
    case E_V4BFmode:
      use_vec_merge = mmx_ok && (TARGET_SSE || TARGET_3DNOW_A);
      break;

    case E_V16QImode:
    case E_V4QImode:
      use_vec_merge = TARGET_SSE4_1;
      break;

    case E_V8QImode:
      use_vec_merge = TARGET_MMX_WITH_SSE && TARGET_SSE4_1;
      break;

    case E_V32QImode:
      half_mode = V16QImode;
      j = 0;
      n = 16;
      goto half;

    case E_V16HFmode:
    case E_V16BFmode:
      /* For ELT == 0, vec_setv8hf_0 can save 1 vpbroadcastw.  */
      if (TARGET_AVX2 && elt != 0)
	{
	  mmode = SImode;
	  gen_blendm = ((mode == E_V16HFmode) ? gen_avx2_pblendph_1
						: gen_avx2_pblendbf_1);
	  blendm_const = true;
	  break;
	}
      else
	{
	  half_mode = ((mode == E_V16HFmode) ? V8HFmode : V8BFmode);
	  j = ((mode == E_V16HFmode) ? 6 : 7);
	  n = 8;
	  goto half;
	}

    case E_V16HImode:
      half_mode = V8HImode;
      j = 1;
      n = 8;
      goto half;

    case E_V8SImode:
      half_mode = V4SImode;
      j = 2;
      n = 4;
      goto half;

    case E_V4DImode:
      half_mode = V2DImode;
      j = 3;
      n = 2;
      goto half;

    case E_V8SFmode:
      half_mode = V4SFmode;
      j = 4;
      n = 4;
      goto half;

    case E_V4DFmode:
      half_mode = V2DFmode;
      j = 5;
      n = 2;
      goto half;

half:
      /* Compute offset.  */
      i = elt / n;
      elt %= n;

      gcc_assert (i <= 1);

      /* Extract the half.  */
      tmp = gen_reg_rtx (half_mode);
      emit_insn (gen_extract[j][i] (tmp, target));

      /* Put val in tmp at elt.  */
      ix86_expand_vector_set (false, tmp, val, elt);

      /* Put it back.  */
      emit_insn (gen_insert[j][i] (target, target, tmp));
      return;

    case E_V8DFmode:
      if (TARGET_AVX512F)
	{
	  mmode = QImode;
	  gen_blendm = gen_avx512f_blendmv8df;
	}
      break;

    case E_V8DImode:
      if (TARGET_AVX512F)
	{
	  mmode = QImode;
	  gen_blendm = gen_avx512f_blendmv8di;
	}
      break;

    case E_V16SFmode:
      if (TARGET_AVX512F)
	{
	  mmode = HImode;
	  gen_blendm = gen_avx512f_blendmv16sf;
	}
      break;

    case E_V16SImode:
      if (TARGET_AVX512F)
	{
	  mmode = HImode;
	  gen_blendm = gen_avx512f_blendmv16si;
	}
      break;

    case E_V32HFmode:
      if (TARGET_AVX512BW)
	{
	  mmode = SImode;
	  gen_blendm = gen_avx512bw_blendmv32hf;
	}
      break;
    case E_V32BFmode:
      if (TARGET_AVX512BW)
	{
	  mmode = SImode;
	  gen_blendm = gen_avx512bw_blendmv32bf;
	}
      break;
    case E_V32HImode:
      if (TARGET_AVX512BW)
	{
	  mmode = SImode;
	  gen_blendm = gen_avx512bw_blendmv32hi;
	}
      else if (TARGET_AVX512F)
	{
	  half_mode = E_V8HImode;
	  n = 8;
	  goto quarter;
	}
      break;

    case E_V64QImode:
      if (TARGET_AVX512BW)
	{
	  mmode = DImode;
	  gen_blendm = gen_avx512bw_blendmv64qi;
	}
      else if (TARGET_AVX512F)
	{
	  half_mode = E_V16QImode;
	  n = 16;
	  goto quarter;
	}
      break;

quarter:
      /* Compute offset.  */
      i = elt / n;
      elt %= n;

      gcc_assert (i <= 3);

      {
	/* Extract the quarter.  */
	tmp = gen_reg_rtx (V4SImode);
	rtx tmp2 = gen_lowpart (V16SImode, target);
	rtx mask = gen_reg_rtx (QImode);

	emit_move_insn (mask, constm1_rtx);
	emit_insn (gen_avx512f_vextracti32x4_mask (tmp, tmp2, GEN_INT (i),
						   tmp, mask));

	tmp2 = gen_reg_rtx (half_mode);
	emit_move_insn (tmp2, gen_lowpart (half_mode, tmp));
	tmp = tmp2;

	/* Put val in tmp at elt.  */
	ix86_expand_vector_set (false, tmp, val, elt);

	/* Put it back.  */
	tmp2 = gen_reg_rtx (V16SImode);
	rtx tmp3 = gen_lowpart (V16SImode, target);
	mask = gen_reg_rtx (HImode);
	emit_move_insn (mask, constm1_rtx);
	tmp = gen_lowpart (V4SImode, tmp);
	emit_insn (gen_avx512f_vinserti32x4_mask (tmp2, tmp3, tmp, GEN_INT (i),
						  tmp3, mask));
	emit_move_insn (target, gen_lowpart (mode, tmp2));
      }
      return;

    default:
      break;
    }

  if (mmode != VOIDmode)
    {
      tmp = gen_reg_rtx (mode);
      emit_insn (gen_rtx_SET (tmp, gen_rtx_VEC_DUPLICATE (mode, val)));
      rtx merge_mask = gen_int_mode (HOST_WIDE_INT_1U << elt, mmode);
      /* The avx512*_blendm<mode> expanders have different operand order
	 from VEC_MERGE.  In VEC_MERGE, the first input operand is used for
	 elements where the mask is set and second input operand otherwise,
	 in {sse,avx}*_*blend* the first input operand is used for elements
	 where the mask is clear and second input operand otherwise.  */
      if (!blendm_const)
	merge_mask = force_reg (mmode, merge_mask);
      emit_insn (gen_blendm (target, target, tmp, merge_mask));
    }
  else if (use_vec_merge)
    {
do_vec_merge:
      tmp = gen_rtx_VEC_DUPLICATE (mode, val);
      tmp = gen_rtx_VEC_MERGE (mode, tmp, target,
			       GEN_INT (HOST_WIDE_INT_1U << elt));
      emit_insn (gen_rtx_SET (target, tmp));
    }
  else
    {
      rtx mem = assign_stack_temp (mode, GET_MODE_SIZE (mode));

      emit_move_insn (mem, target);

      tmp = adjust_address (mem, inner_mode, elt * GET_MODE_SIZE (inner_mode));
      emit_move_insn (tmp, val);

      emit_move_insn (target, mem);
    }
}

void
ix86_expand_vector_extract (bool mmx_ok, rtx target, rtx vec, int elt)
{
  machine_mode mode = GET_MODE (vec);
  machine_mode inner_mode = GET_MODE_INNER (mode);
  bool use_vec_extr = false;
  rtx tmp;

  switch (mode)
    {
    case E_V2SImode:
      use_vec_extr = TARGET_MMX_WITH_SSE && TARGET_SSE4_1;
      if (use_vec_extr)
	break;
      /* FALLTHRU */

    case E_V2SFmode:
      if (!mmx_ok)
	break;
      /* FALLTHRU */

    case E_V2DFmode:
    case E_V2DImode:
    case E_V2TImode:
    case E_V4TImode:
      use_vec_extr = true;
      break;

    case E_V4SFmode:
      use_vec_extr = TARGET_SSE4_1;
      if (use_vec_extr)
	break;

      switch (elt)
	{
	case 0:
	  tmp = vec;
	  break;

	case 1:
	case 3:
	  tmp = gen_reg_rtx (mode);
	  emit_insn (gen_sse_shufps_v4sf (tmp, vec, vec,
				       GEN_INT (elt), GEN_INT (elt),
				       GEN_INT (elt+4), GEN_INT (elt+4)));
	  break;

	case 2:
	  tmp = gen_reg_rtx (mode);
	  emit_insn (gen_vec_interleave_highv4sf (tmp, vec, vec));
	  break;

	default:
	  gcc_unreachable ();
	}
      vec = tmp;
      use_vec_extr = true;
      elt = 0;
      break;

    case E_V4SImode:
      use_vec_extr = TARGET_SSE4_1;
      if (use_vec_extr)
	break;

      if (TARGET_SSE2)
	{
	  switch (elt)
	    {
	    case 0:
	      tmp = vec;
	      break;

	    case 1:
	    case 3:
	      tmp = gen_reg_rtx (mode);
	      emit_insn (gen_sse2_pshufd_1 (tmp, vec,
					    GEN_INT (elt), GEN_INT (elt),
					    GEN_INT (elt), GEN_INT (elt)));
	      break;

	    case 2:
	      tmp = gen_reg_rtx (mode);
	      emit_insn (gen_vec_interleave_highv4si (tmp, vec, vec));
	      break;

	    default:
	      gcc_unreachable ();
	    }
	  vec = tmp;
	  use_vec_extr = true;
	  elt = 0;
	}
      else
	{
	  /* For SSE1, we have to reuse the V4SF code.  */
	  ix86_expand_vector_extract (false, gen_lowpart (SFmode, target),
				      gen_lowpart (V4SFmode, vec), elt);
	  return;
	}
      break;

    case E_V8HImode:
    case E_V8HFmode:
    case E_V8BFmode:
    case E_V2HImode:
    case E_V2HFmode:
    case E_V2BFmode:
      use_vec_extr = TARGET_SSE2;
      break;
    case E_V4HImode:
    case E_V4HFmode:
    case E_V4BFmode:
      use_vec_extr = mmx_ok && (TARGET_SSE || TARGET_3DNOW_A);
      break;

    case E_V16QImode:
      use_vec_extr = TARGET_SSE4_1;
      if (!use_vec_extr
	  && TARGET_SSE2
	  && elt == 0
	  && (optimize_insn_for_size_p () || TARGET_INTER_UNIT_MOVES_FROM_VEC))
	{
	  tmp = gen_reg_rtx (SImode);
	  ix86_expand_vector_extract (false, tmp, gen_lowpart (V4SImode, vec),
				      0);
	  emit_insn (gen_rtx_SET (target, gen_lowpart (QImode, tmp)));
	  return;
	}
      break;
    case E_V4QImode:
      use_vec_extr = TARGET_SSE4_1;
      break;

    case E_V8SFmode:
      if (TARGET_AVX)
	{
	  tmp = gen_reg_rtx (V4SFmode);
	  if (elt < 4)
	    emit_insn (gen_vec_extract_lo_v8sf (tmp, vec));
	  else
	    emit_insn (gen_vec_extract_hi_v8sf (tmp, vec));
	  ix86_expand_vector_extract (false, target, tmp, elt & 3);
	  return;
	}
      break;

    case E_V4DFmode:
      if (TARGET_AVX)
	{
	  tmp = gen_reg_rtx (V2DFmode);
	  if (elt < 2)
	    emit_insn (gen_vec_extract_lo_v4df (tmp, vec));
	  else
	    emit_insn (gen_vec_extract_hi_v4df (tmp, vec));
	  ix86_expand_vector_extract (false, target, tmp, elt & 1);
	  return;
	}
      break;

    case E_V32QImode:
      if (TARGET_AVX)
	{
	  tmp = gen_reg_rtx (V16QImode);
	  if (elt < 16)
	    emit_insn (gen_vec_extract_lo_v32qi (tmp, vec));
	  else
	    emit_insn (gen_vec_extract_hi_v32qi (tmp, vec));
	  ix86_expand_vector_extract (false, target, tmp, elt & 15);
	  return;
	}
      break;

    case E_V16HImode:
      if (TARGET_AVX)
	{
	  tmp = gen_reg_rtx (V8HImode);
	  if (elt < 8)
	    emit_insn (gen_vec_extract_lo_v16hi (tmp, vec));
	  else
	    emit_insn (gen_vec_extract_hi_v16hi (tmp, vec));
	  ix86_expand_vector_extract (false, target, tmp, elt & 7);
	  return;
	}
      break;

    case E_V8SImode:
      if (TARGET_AVX)
	{
	  tmp = gen_reg_rtx (V4SImode);
	  if (elt < 4)
	    emit_insn (gen_vec_extract_lo_v8si (tmp, vec));
	  else
	    emit_insn (gen_vec_extract_hi_v8si (tmp, vec));
	  ix86_expand_vector_extract (false, target, tmp, elt & 3);
	  return;
	}
      break;

    case E_V4DImode:
      if (TARGET_AVX)
	{
	  tmp = gen_reg_rtx (V2DImode);
	  if (elt < 2)
	    emit_insn (gen_vec_extract_lo_v4di (tmp, vec));
	  else
	    emit_insn (gen_vec_extract_hi_v4di (tmp, vec));
	  ix86_expand_vector_extract (false, target, tmp, elt & 1);
	  return;
	}
      break;

    case E_V32HImode:
      if (TARGET_AVX512BW)
	{
	  tmp = gen_reg_rtx (V16HImode);
	  if (elt < 16)
	    emit_insn (gen_vec_extract_lo_v32hi (tmp, vec));
	  else
	    emit_insn (gen_vec_extract_hi_v32hi (tmp, vec));
	  ix86_expand_vector_extract (false, target, tmp, elt & 15);
	  return;
	}
      break;

    case E_V64QImode:
      if (TARGET_AVX512BW)
	{
	  tmp = gen_reg_rtx (V32QImode);
	  if (elt < 32)
	    emit_insn (gen_vec_extract_lo_v64qi (tmp, vec));
	  else
	    emit_insn (gen_vec_extract_hi_v64qi (tmp, vec));
	  ix86_expand_vector_extract (false, target, tmp, elt & 31);
	  return;
	}
      break;

    case E_V16SFmode:
      tmp = gen_reg_rtx (V8SFmode);
      if (elt < 8)
	emit_insn (gen_vec_extract_lo_v16sf (tmp, vec));
      else
	emit_insn (gen_vec_extract_hi_v16sf (tmp, vec));
      ix86_expand_vector_extract (false, target, tmp, elt & 7);
      return;

    case E_V8DFmode:
      tmp = gen_reg_rtx (V4DFmode);
      if (elt < 4)
	emit_insn (gen_vec_extract_lo_v8df (tmp, vec));
      else
	emit_insn (gen_vec_extract_hi_v8df (tmp, vec));
      ix86_expand_vector_extract (false, target, tmp, elt & 3);
      return;

    case E_V16SImode:
      tmp = gen_reg_rtx (V8SImode);
      if (elt < 8)
	emit_insn (gen_vec_extract_lo_v16si (tmp, vec));
      else
	emit_insn (gen_vec_extract_hi_v16si (tmp, vec));
      ix86_expand_vector_extract (false, target, tmp, elt & 7);
      return;

    case E_V8DImode:
      tmp = gen_reg_rtx (V4DImode);
      if (elt < 4)
	emit_insn (gen_vec_extract_lo_v8di (tmp, vec));
      else
	emit_insn (gen_vec_extract_hi_v8di (tmp, vec));
      ix86_expand_vector_extract (false, target, tmp, elt & 3);
      return;

    case E_V32HFmode:
    case E_V32BFmode:
      if (TARGET_AVX512BW)
	{
	  tmp = (mode == E_V32HFmode
		 ? gen_reg_rtx (V16HFmode)
		 : gen_reg_rtx (V16BFmode));
	  if (elt < 16)
	    emit_insn (gen_vec_extract_lo (mode, tmp, vec));
	  else
	    emit_insn (gen_vec_extract_hi (mode, tmp, vec));
	  ix86_expand_vector_extract (false, target, tmp, elt & 15);
	  return;
	}
      break;

    case E_V16HFmode:
    case E_V16BFmode:
      if (TARGET_AVX)
	{
	  tmp = (mode == E_V16HFmode
		 ? gen_reg_rtx (V8HFmode)
		 : gen_reg_rtx (V8BFmode));
	  if (elt < 8)
	    emit_insn (gen_vec_extract_lo (mode, tmp, vec));
	  else
	    emit_insn (gen_vec_extract_hi (mode, tmp, vec));
	  ix86_expand_vector_extract (false, target, tmp, elt & 7);
	  return;
	}
      break;

    case E_V8QImode:
      use_vec_extr = TARGET_MMX_WITH_SSE && TARGET_SSE4_1;
      /* ??? Could extract the appropriate HImode element and shift.  */
      break;

    default:
      break;
    }

  if (use_vec_extr)
    {
      tmp = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (1, GEN_INT (elt)));
      tmp = gen_rtx_VEC_SELECT (inner_mode, vec, tmp);

      /* Let the rtl optimizers know about the zero extension performed.  */
      if (inner_mode == QImode || inner_mode == HImode)
	{
	  rtx reg = gen_reg_rtx (SImode);
	  tmp = gen_rtx_ZERO_EXTEND (SImode, tmp);
	  emit_move_insn (reg, tmp);
	  tmp = gen_lowpart (inner_mode, reg);
	  SUBREG_PROMOTED_VAR_P (tmp) = 1;
	  SUBREG_PROMOTED_SET (tmp, 1);
	}

      emit_move_insn (target, tmp);
    }
  else
    {
      rtx mem = assign_stack_temp (mode, GET_MODE_SIZE (mode));

      emit_move_insn (mem, vec);

      tmp = adjust_address (mem, inner_mode, elt*GET_MODE_SIZE (inner_mode));
      emit_move_insn (target, tmp);
    }
}

/* Generate code to copy vector bits i / 2 ... i - 1 from vector SRC
   to bits 0 ... i / 2 - 1 of vector DEST, which has the same mode.
   The upper bits of DEST are undefined, though they shouldn't cause
   exceptions (some bits from src or all zeros are ok).  */

static void
emit_reduc_half (rtx dest, rtx src, int i)
{
  rtx tem, d = dest;
  switch (GET_MODE (src))
    {
    case E_V4SFmode:
      if (i == 128)
	tem = gen_sse_movhlps (dest, src, src);
      else
	tem = gen_sse_shufps_v4sf (dest, src, src, const1_rtx, const1_rtx,
				   GEN_INT (1 + 4), GEN_INT (1 + 4));
      break;
    case E_V2DFmode:
      tem = gen_vec_interleave_highv2df (dest, src, src);
      break;
    case E_V4QImode:
      d = gen_reg_rtx (V1SImode);
      tem = gen_mmx_lshrv1si3 (d, gen_lowpart (V1SImode, src),
			       GEN_INT (i / 2));
      break;
    case E_V8QImode:
    case E_V4HImode:
      d = gen_reg_rtx (V1DImode);
      tem = gen_mmx_lshrv1di3 (d, gen_lowpart (V1DImode, src),
			       GEN_INT (i / 2));
      break;
    case E_V16QImode:
    case E_V8HImode:
    case E_V8HFmode:
    case E_V4SImode:
    case E_V2DImode:
      d = gen_reg_rtx (V1TImode);
      tem = gen_sse2_lshrv1ti3 (d, gen_lowpart (V1TImode, src),
				GEN_INT (i / 2));
      break;
    case E_V8SFmode:
      if (i == 256)
	tem = gen_avx_vperm2f128v8sf3 (dest, src, src, const1_rtx);
      else
	tem = gen_avx_shufps256 (dest, src, src,
				 GEN_INT (i == 128 ? 2 + (3 << 2) : 1));
      break;
    case E_V4DFmode:
      if (i == 256)
	tem = gen_avx_vperm2f128v4df3 (dest, src, src, const1_rtx);
      else
	tem = gen_avx_shufpd256 (dest, src, src, const1_rtx);
      break;
    case E_V32QImode:
    case E_V16HImode:
    case E_V16HFmode:
    case E_V8SImode:
    case E_V4DImode:
      if (i == 256)
	{
	  if (GET_MODE (dest) != V4DImode)
	    d = gen_reg_rtx (V4DImode);
	  tem = gen_avx2_permv2ti (d, gen_lowpart (V4DImode, src),
				   gen_lowpart (V4DImode, src),
				   const1_rtx);
	}
      else
	{
	  d = gen_reg_rtx (V2TImode);
	  tem = gen_avx2_lshrv2ti3 (d, gen_lowpart (V2TImode, src),
				    GEN_INT (i / 2));
	}
      break;
    case E_V64QImode:
    case E_V32HImode:
    case E_V32HFmode:
      if (i < 64)
	{
	  d = gen_reg_rtx (V4TImode);
	  tem = gen_avx512bw_lshrv4ti3 (d, gen_lowpart (V4TImode, src),
					GEN_INT (i / 2));
	  break;
	}
      /* FALLTHRU */
    case E_V16SImode:
    case E_V16SFmode:
    case E_V8DImode:
    case E_V8DFmode:
      if (i > 128)
	tem = gen_avx512f_shuf_i32x4_1 (gen_lowpart (V16SImode, dest),
					gen_lowpart (V16SImode, src),
					gen_lowpart (V16SImode, src),
					GEN_INT (0x4 + (i == 512 ? 4 : 0)),
					GEN_INT (0x5 + (i == 512 ? 4 : 0)),
					GEN_INT (0x6 + (i == 512 ? 4 : 0)),
					GEN_INT (0x7 + (i == 512 ? 4 : 0)),
					GEN_INT (0xC), GEN_INT (0xD),
					GEN_INT (0xE), GEN_INT (0xF),
					GEN_INT (0x10), GEN_INT (0x11),
					GEN_INT (0x12), GEN_INT (0x13),
					GEN_INT (0x14), GEN_INT (0x15),
					GEN_INT (0x16), GEN_INT (0x17));
      else
	tem = gen_avx512f_pshufd_1 (gen_lowpart (V16SImode, dest),
				    gen_lowpart (V16SImode, src),
				    GEN_INT (i == 128 ? 0x2 : 0x1),
				    GEN_INT (0x3),
				    GEN_INT (0x3),
				    GEN_INT (0x3),
				    GEN_INT (i == 128 ? 0x6 : 0x5),
				    GEN_INT (0x7),
				    GEN_INT (0x7),
				    GEN_INT (0x7),
				    GEN_INT (i == 128 ? 0xA : 0x9),
				    GEN_INT (0xB),
				    GEN_INT (0xB),
				    GEN_INT (0xB),
				    GEN_INT (i == 128 ? 0xE : 0xD),
				    GEN_INT (0xF),
				    GEN_INT (0xF),
				    GEN_INT (0xF));
      break;
    default:
      gcc_unreachable ();
    }
  emit_insn (tem);
  if (d != dest)
    emit_move_insn (dest, gen_lowpart (GET_MODE (dest), d));
}

/* Expand a vector reduction.  FN is the binary pattern to reduce;
   DEST is the destination; IN is the input vector.  */

void
ix86_expand_reduc (rtx (*fn) (rtx, rtx, rtx), rtx dest, rtx in)
{
  rtx half, dst, vec = in;
  machine_mode mode = GET_MODE (in);
  int i;

  /* SSE4 has a special instruction for V8HImode UMIN reduction.  */
  if (TARGET_SSE4_1
      && mode == V8HImode
      && fn == gen_uminv8hi3)
    {
      emit_insn (gen_sse4_1_phminposuw (dest, in));
      return;
    }

  for (i = GET_MODE_BITSIZE (mode);
       i > GET_MODE_UNIT_BITSIZE (mode);
       i >>= 1)
    {
      half = gen_reg_rtx (mode);
      emit_reduc_half (half, vec, i);
      if (i == GET_MODE_UNIT_BITSIZE (mode) * 2)
	dst = dest;
      else
	dst = gen_reg_rtx (mode);
      emit_insn (fn (dst, half, vec));
      vec = dst;
    }
}

/* Output code to perform a conditional jump to LABEL, if C2 flag in
   FP status register is set.  */

void
ix86_emit_fp_unordered_jump (rtx label)
{
  rtx reg = gen_reg_rtx (HImode);
  rtx_insn *insn;
  rtx temp;

  emit_insn (gen_x86_fnstsw_1 (reg));

  if (TARGET_SAHF && (TARGET_USE_SAHF || optimize_insn_for_size_p ()))
    {
      emit_insn (gen_x86_sahf_1 (reg));

      temp = gen_rtx_REG (CCmode, FLAGS_REG);
      temp = gen_rtx_UNORDERED (VOIDmode, temp, const0_rtx);
    }
  else
    {
      emit_insn (gen_testqi_ext_1_ccno (reg, GEN_INT (0x04)));

      temp = gen_rtx_REG (CCNOmode, FLAGS_REG);
      temp = gen_rtx_NE (VOIDmode, temp, const0_rtx);
    }

  temp = gen_rtx_IF_THEN_ELSE (VOIDmode, temp,
			      gen_rtx_LABEL_REF (VOIDmode, label),
			      pc_rtx);
  insn = emit_jump_insn (gen_rtx_SET (pc_rtx, temp));
  predict_jump (REG_BR_PROB_BASE * 10 / 100);
  JUMP_LABEL (insn) = label;
}

/* Output code to perform an sinh XFmode calculation.  */

void
ix86_emit_i387_sinh (rtx op0, rtx op1)
{
  rtx e1 = gen_reg_rtx (XFmode);
  rtx e2 = gen_reg_rtx (XFmode);
  rtx scratch = gen_reg_rtx (HImode);
  rtx flags = gen_rtx_REG (CCNOmode, FLAGS_REG);
  rtx half = const_double_from_real_value (dconsthalf, XFmode);
  rtx cst1, tmp;
  rtx_code_label *jump_label = gen_label_rtx ();
  rtx_insn *insn;

  /* scratch = fxam (op1) */
  emit_insn (gen_fxamxf2_i387 (scratch, op1));

  /* e1 = expm1 (|op1|) */
  emit_insn (gen_absxf2 (e2, op1));
  emit_insn (gen_expm1xf2 (e1, e2));

  /* e2 = e1 / (e1 + 1.0) + e1 */
  cst1 = force_reg (XFmode, CONST1_RTX (XFmode));
  emit_insn (gen_addxf3 (e2, e1, cst1));
  emit_insn (gen_divxf3 (e2, e1, e2));
  emit_insn (gen_addxf3 (e2, e2, e1));

  /* flags = signbit (op1) */
  emit_insn (gen_testqi_ext_1_ccno (scratch, GEN_INT (0x02)));

  /* if (flags) then e2 = -e2 */
  tmp = gen_rtx_IF_THEN_ELSE (VOIDmode,
			      gen_rtx_EQ (VOIDmode, flags, const0_rtx),
			      gen_rtx_LABEL_REF (VOIDmode, jump_label),
			      pc_rtx);
  insn = emit_jump_insn (gen_rtx_SET (pc_rtx, tmp));
  predict_jump (REG_BR_PROB_BASE * 50 / 100);
  JUMP_LABEL (insn) = jump_label;

  emit_insn (gen_negxf2 (e2, e2));

  emit_label (jump_label);
  LABEL_NUSES (jump_label) = 1;

  /* op0 = 0.5 * e2 */
  half = force_reg (XFmode, half);
  emit_insn (gen_mulxf3 (op0, e2, half));
}

/* Output code to perform an cosh XFmode calculation.  */

void
ix86_emit_i387_cosh (rtx op0, rtx op1)
{
  rtx e1 = gen_reg_rtx (XFmode);
  rtx e2 = gen_reg_rtx (XFmode);
  rtx half = const_double_from_real_value (dconsthalf, XFmode);
  rtx cst1;

  /* e1 = exp (op1) */
  emit_insn (gen_expxf2 (e1, op1));

  /* e2 = e1 + 1.0 / e1 */
  cst1 = force_reg (XFmode, CONST1_RTX (XFmode));
  emit_insn (gen_divxf3 (e2, cst1, e1));
  emit_insn (gen_addxf3 (e2, e1, e2));

  /* op0 = 0.5 * e2 */
  half = force_reg (XFmode, half);
  emit_insn (gen_mulxf3 (op0, e2, half));
}

/* Output code to perform an tanh XFmode calculation.  */

void
ix86_emit_i387_tanh (rtx op0, rtx op1)
{
  rtx e1 = gen_reg_rtx (XFmode);
  rtx e2 = gen_reg_rtx (XFmode);
  rtx scratch = gen_reg_rtx (HImode);
  rtx flags = gen_rtx_REG (CCNOmode, FLAGS_REG);
  rtx cst2, tmp;
  rtx_code_label *jump_label = gen_label_rtx ();
  rtx_insn *insn;

  /* scratch = fxam (op1) */
  emit_insn (gen_fxamxf2_i387 (scratch, op1));

  /* e1 = expm1 (-|2 * op1|) */
  emit_insn (gen_addxf3 (e2, op1, op1));
  emit_insn (gen_absxf2 (e2, e2));
  emit_insn (gen_negxf2 (e2, e2));
  emit_insn (gen_expm1xf2 (e1, e2));

  /* e2 = e1 / (e1 + 2.0) */
  cst2 = force_reg (XFmode, CONST2_RTX (XFmode));
  emit_insn (gen_addxf3 (e2, e1, cst2));
  emit_insn (gen_divxf3 (e2, e1, e2));

  /* flags = signbit (op1) */
  emit_insn (gen_testqi_ext_1_ccno (scratch, GEN_INT (0x02)));

  /* if (!flags) then e2 = -e2 */
  tmp = gen_rtx_IF_THEN_ELSE (VOIDmode,
			      gen_rtx_NE (VOIDmode, flags, const0_rtx),
			      gen_rtx_LABEL_REF (VOIDmode, jump_label),
			      pc_rtx);
  insn = emit_jump_insn (gen_rtx_SET (pc_rtx, tmp));
  predict_jump (REG_BR_PROB_BASE * 50 / 100);
  JUMP_LABEL (insn) = jump_label;

  emit_insn (gen_negxf2 (e2, e2));

  emit_label (jump_label);
  LABEL_NUSES (jump_label) = 1;

  emit_move_insn (op0, e2);
}

/* Output code to perform an asinh XFmode calculation.  */

void
ix86_emit_i387_asinh (rtx op0, rtx op1)
{
  rtx e1 = gen_reg_rtx (XFmode);
  rtx e2 = gen_reg_rtx (XFmode);
  rtx scratch = gen_reg_rtx (HImode);
  rtx flags = gen_rtx_REG (CCNOmode, FLAGS_REG);
  rtx cst1, tmp;
  rtx_code_label *jump_label = gen_label_rtx ();
  rtx_insn *insn;

  /* e2 = sqrt (op1^2 + 1.0) + 1.0 */
  emit_insn (gen_mulxf3 (e1, op1, op1));
  cst1 = force_reg (XFmode, CONST1_RTX (XFmode));
  emit_insn (gen_addxf3 (e2, e1, cst1));
  emit_insn (gen_sqrtxf2 (e2, e2));
  emit_insn (gen_addxf3 (e2, e2, cst1));

  /* e1 = e1 / e2 */
  emit_insn (gen_divxf3 (e1, e1, e2));

  /* scratch = fxam (op1) */
  emit_insn (gen_fxamxf2_i387 (scratch, op1));

  /* e1 = e1 + |op1| */
  emit_insn (gen_absxf2 (e2, op1));
  emit_insn (gen_addxf3 (e1, e1, e2));

  /* e2 = log1p (e1) */
  ix86_emit_i387_log1p (e2, e1);

  /* flags = signbit (op1) */
  emit_insn (gen_testqi_ext_1_ccno (scratch, GEN_INT (0x02)));

  /* if (flags) then e2 = -e2 */
  tmp = gen_rtx_IF_THEN_ELSE (VOIDmode,
			      gen_rtx_EQ (VOIDmode, flags, const0_rtx),
			      gen_rtx_LABEL_REF (VOIDmode, jump_label),
			      pc_rtx);
  insn = emit_jump_insn (gen_rtx_SET (pc_rtx, tmp));
  predict_jump (REG_BR_PROB_BASE * 50 / 100);
  JUMP_LABEL (insn) = jump_label;

  emit_insn (gen_negxf2 (e2, e2));

  emit_label (jump_label);
  LABEL_NUSES (jump_label) = 1;

  emit_move_insn (op0, e2);
}

/* Output code to perform an acosh XFmode calculation.  */

void
ix86_emit_i387_acosh (rtx op0, rtx op1)
{
  rtx e1 = gen_reg_rtx (XFmode);
  rtx e2 = gen_reg_rtx (XFmode);
  rtx cst1 = force_reg (XFmode, CONST1_RTX (XFmode));

  /* e2 = sqrt (op1 + 1.0) */
  emit_insn (gen_addxf3 (e2, op1, cst1));
  emit_insn (gen_sqrtxf2 (e2, e2));

  /* e1 = sqrt (op1 - 1.0) */
  emit_insn (gen_subxf3 (e1, op1, cst1));
  emit_insn (gen_sqrtxf2 (e1, e1));

  /* e1 = e1 * e2 */
  emit_insn (gen_mulxf3 (e1, e1, e2));

  /* e1 = e1 + op1 */
  emit_insn (gen_addxf3 (e1, e1, op1));

  /* op0 = log (e1) */
  emit_insn (gen_logxf2 (op0, e1));
}

/* Output code to perform an atanh XFmode calculation.  */

void
ix86_emit_i387_atanh (rtx op0, rtx op1)
{
  rtx e1 = gen_reg_rtx (XFmode);
  rtx e2 = gen_reg_rtx (XFmode);
  rtx scratch = gen_reg_rtx (HImode);
  rtx flags = gen_rtx_REG (CCNOmode, FLAGS_REG);
  rtx half = const_double_from_real_value (dconsthalf, XFmode);
  rtx cst1, tmp;
  rtx_code_label *jump_label = gen_label_rtx ();
  rtx_insn *insn;

  /* scratch = fxam (op1) */
  emit_insn (gen_fxamxf2_i387 (scratch, op1));

  /* e2 = |op1| */
  emit_insn (gen_absxf2 (e2, op1));

  /* e1 = -(e2 + e2) / (e2 + 1.0) */
  cst1 = force_reg (XFmode, CONST1_RTX (XFmode));
  emit_insn (gen_addxf3 (e1, e2, cst1));
  emit_insn (gen_addxf3 (e2, e2, e2));
  emit_insn (gen_negxf2 (e2, e2));
  emit_insn (gen_divxf3 (e1, e2, e1));

  /* e2 = log1p (e1) */
  ix86_emit_i387_log1p (e2, e1);

  /* flags = signbit (op1) */
  emit_insn (gen_testqi_ext_1_ccno (scratch, GEN_INT (0x02)));

  /* if (!flags) then e2 = -e2 */
  tmp = gen_rtx_IF_THEN_ELSE (VOIDmode,
			      gen_rtx_NE (VOIDmode, flags, const0_rtx),
			      gen_rtx_LABEL_REF (VOIDmode, jump_label),
			      pc_rtx);
  insn = emit_jump_insn (gen_rtx_SET (pc_rtx, tmp));
  predict_jump (REG_BR_PROB_BASE * 50 / 100);
  JUMP_LABEL (insn) = jump_label;

  emit_insn (gen_negxf2 (e2, e2));

  emit_label (jump_label);
  LABEL_NUSES (jump_label) = 1;

  /* op0 = 0.5 * e2 */
  half = force_reg (XFmode, half);
  emit_insn (gen_mulxf3 (op0, e2, half));
}

/* Output code to perform a log1p XFmode calculation.  */

void
ix86_emit_i387_log1p (rtx op0, rtx op1)
{
  rtx_code_label *label1 = gen_label_rtx ();
  rtx_code_label *label2 = gen_label_rtx ();

  rtx tmp = gen_reg_rtx (XFmode);
  rtx res = gen_reg_rtx (XFmode);
  rtx cst, cstln2, cst1;
  rtx_insn *insn;

  /* The emit_jump call emits pending stack adjust, make sure it is emitted
     before the conditional jump, otherwise the stack adjustment will be
     only conditional.  */
  do_pending_stack_adjust ();

  cst = const_double_from_real_value
    (REAL_VALUE_ATOF ("0.29289321881345247561810596348408353", XFmode), XFmode);
  cstln2 = force_reg (XFmode, standard_80387_constant_rtx (4)); /* fldln2 */

  emit_insn (gen_absxf2 (tmp, op1));

  cst = force_reg (XFmode, cst);
  ix86_expand_branch (GE, tmp, cst, label1);
  predict_jump (REG_BR_PROB_BASE * 10 / 100);
  insn = get_last_insn ();
  JUMP_LABEL (insn) = label1;

  emit_insn (gen_fyl2xp1xf3_i387 (res, op1, cstln2));
  emit_jump (label2);

  emit_label (label1);
  LABEL_NUSES (label1) = 1;

  cst1 = force_reg (XFmode, CONST1_RTX (XFmode));
  emit_insn (gen_rtx_SET (tmp, gen_rtx_PLUS (XFmode, op1, cst1)));
  emit_insn (gen_fyl2xxf3_i387 (res, tmp, cstln2));

  emit_label (label2);
  LABEL_NUSES (label2) = 1;

  emit_move_insn (op0, res);
}

/* Emit code for round calculation.  */
void
ix86_emit_i387_round (rtx op0, rtx op1)
{
  machine_mode inmode = GET_MODE (op1);
  machine_mode outmode = GET_MODE (op0);
  rtx e1 = gen_reg_rtx (XFmode);
  rtx e2 = gen_reg_rtx (XFmode);
  rtx scratch = gen_reg_rtx (HImode);
  rtx flags = gen_rtx_REG (CCNOmode, FLAGS_REG);
  rtx half = const_double_from_real_value (dconsthalf, XFmode);
  rtx res = gen_reg_rtx (outmode);
  rtx_code_label *jump_label = gen_label_rtx ();
  rtx (*floor_insn) (rtx, rtx);
  rtx (*neg_insn) (rtx, rtx);
  rtx_insn *insn;
  rtx tmp;

  switch (inmode)
    {
    case E_SFmode:
    case E_DFmode:
      tmp = gen_reg_rtx (XFmode);

      emit_insn (gen_rtx_SET (tmp, gen_rtx_FLOAT_EXTEND (XFmode, op1)));
      op1 = tmp;
      break;
    case E_XFmode:
      break;
    default:
      gcc_unreachable ();
    }

  switch (outmode)
    {
    case E_SFmode:
      floor_insn = gen_frndintxf2_floor;
      neg_insn = gen_negsf2;
      break;
    case E_DFmode:
      floor_insn = gen_frndintxf2_floor;
      neg_insn = gen_negdf2;
      break;
    case E_XFmode:
      floor_insn = gen_frndintxf2_floor;
      neg_insn = gen_negxf2;
      break;
    case E_HImode:
      floor_insn = gen_lfloorxfhi2;
      neg_insn = gen_neghi2;
      break;
    case E_SImode:
      floor_insn = gen_lfloorxfsi2;
      neg_insn = gen_negsi2;
      break;
    case E_DImode:
      floor_insn = gen_lfloorxfdi2;
      neg_insn = gen_negdi2;
      break;
    default:
      gcc_unreachable ();
    }

  /* round(a) = sgn(a) * floor(fabs(a) + 0.5) */

  /* scratch = fxam(op1) */
  emit_insn (gen_fxamxf2_i387 (scratch, op1));

  /* e1 = fabs(op1) */
  emit_insn (gen_absxf2 (e1, op1));

  /* e2 = e1 + 0.5 */
  half = force_reg (XFmode, half);
  emit_insn (gen_rtx_SET (e2, gen_rtx_PLUS (XFmode, e1, half)));

  /* res = floor(e2) */
  switch (outmode)
    {
    case E_SFmode:
    case E_DFmode:
      {
	tmp = gen_reg_rtx (XFmode);

	emit_insn (floor_insn (tmp, e2));
	emit_insn (gen_rtx_SET (res,
				gen_rtx_UNSPEC (outmode, gen_rtvec (1, tmp),
						UNSPEC_TRUNC_NOOP)));
      }
      break;
    default:
      emit_insn (floor_insn (res, e2));
    }

  /* flags = signbit(a) */
  emit_insn (gen_testqi_ext_1_ccno (scratch, GEN_INT (0x02)));

  /* if (flags) then res = -res */
  tmp = gen_rtx_IF_THEN_ELSE (VOIDmode,
			      gen_rtx_EQ (VOIDmode, flags, const0_rtx),
			      gen_rtx_LABEL_REF (VOIDmode, jump_label),
			      pc_rtx);
  insn = emit_jump_insn (gen_rtx_SET (pc_rtx, tmp));
  predict_jump (REG_BR_PROB_BASE * 50 / 100);
  JUMP_LABEL (insn) = jump_label;

  emit_insn (neg_insn (res, res));

  emit_label (jump_label);
  LABEL_NUSES (jump_label) = 1;

  emit_move_insn (op0, res);
}

/* Output code to perform a Newton-Rhapson approximation of a single precision
   floating point divide [http://en.wikipedia.org/wiki/N-th_root_algorithm].  */

void
ix86_emit_swdivsf (rtx res, rtx a, rtx b, machine_mode mode)
{
  rtx x0, x1, e0, e1;

  x0 = gen_reg_rtx (mode);
  e0 = gen_reg_rtx (mode);
  e1 = gen_reg_rtx (mode);
  x1 = gen_reg_rtx (mode);

  /* a / b = a * ((rcp(b) + rcp(b)) - (b * rcp(b) * rcp (b))) */

  b = force_reg (mode, b);

  /* x0 = rcp(b) estimate */
  if (mode == V16SFmode || mode == V8DFmode)
    {
      if (TARGET_AVX512ER)
	{
	  emit_insn (gen_rtx_SET (x0, gen_rtx_UNSPEC (mode, gen_rtvec (1, b),
						      UNSPEC_RCP28)));
	  /* res = a * x0 */
	  emit_insn (gen_rtx_SET (res, gen_rtx_MULT (mode, a, x0)));
	  return;
	}
      else
	emit_insn (gen_rtx_SET (x0, gen_rtx_UNSPEC (mode, gen_rtvec (1, b),
						    UNSPEC_RCP14)));
    }
  else
    emit_insn (gen_rtx_SET (x0, gen_rtx_UNSPEC (mode, gen_rtvec (1, b),
						UNSPEC_RCP)));

  /* e0 = x0 * b */
  emit_insn (gen_rtx_SET (e0, gen_rtx_MULT (mode, x0, b)));

  /* e0 = x0 * e0 */
  emit_insn (gen_rtx_SET (e0, gen_rtx_MULT (mode, x0, e0)));

  /* e1 = x0 + x0 */
  emit_insn (gen_rtx_SET (e1, gen_rtx_PLUS (mode, x0, x0)));

  /* x1 = e1 - e0 */
  emit_insn (gen_rtx_SET (x1, gen_rtx_MINUS (mode, e1, e0)));

  /* res = a * x1 */
  emit_insn (gen_rtx_SET (res, gen_rtx_MULT (mode, a, x1)));
}

/* Output code to perform a Newton-Rhapson approximation of a
   single precision floating point [reciprocal] square root.  */

void
ix86_emit_swsqrtsf (rtx res, rtx a, machine_mode mode, bool recip)
{
  rtx x0, e0, e1, e2, e3, mthree, mhalf;
  REAL_VALUE_TYPE r;
  int unspec;

  x0 = gen_reg_rtx (mode);
  e0 = gen_reg_rtx (mode);
  e1 = gen_reg_rtx (mode);
  e2 = gen_reg_rtx (mode);
  e3 = gen_reg_rtx (mode);

  if (TARGET_AVX512ER && mode == V16SFmode)
    {
      if (recip)
	/* res = rsqrt28(a) estimate */
	emit_insn (gen_rtx_SET (res, gen_rtx_UNSPEC (mode, gen_rtvec (1, a),
						     UNSPEC_RSQRT28)));
      else
	{
	  /* x0 = rsqrt28(a) estimate */
	  emit_insn (gen_rtx_SET (x0, gen_rtx_UNSPEC (mode, gen_rtvec (1, a),
						      UNSPEC_RSQRT28)));
	  /* res = rcp28(x0) estimate */
	  emit_insn (gen_rtx_SET (res, gen_rtx_UNSPEC (mode, gen_rtvec (1, x0),
						       UNSPEC_RCP28)));
	}
      return;
    }

  real_from_integer (&r, VOIDmode, -3, SIGNED);
  mthree = const_double_from_real_value (r, SFmode);

  real_arithmetic (&r, NEGATE_EXPR, &dconsthalf, NULL);
  mhalf = const_double_from_real_value (r, SFmode);
  unspec = UNSPEC_RSQRT;

  if (VECTOR_MODE_P (mode))
    {
      mthree = ix86_build_const_vector (mode, true, mthree);
      mhalf = ix86_build_const_vector (mode, true, mhalf);
      /* There is no 512-bit rsqrt.  There is however rsqrt14.  */
      if (GET_MODE_SIZE (mode) == 64)
	unspec = UNSPEC_RSQRT14;
    }

  /* sqrt(a)  = -0.5 * a * rsqrtss(a) * (a * rsqrtss(a) * rsqrtss(a) - 3.0)
     rsqrt(a) = -0.5     * rsqrtss(a) * (a * rsqrtss(a) * rsqrtss(a) - 3.0) */

  a = force_reg (mode, a);

  /* x0 = rsqrt(a) estimate */
  emit_insn (gen_rtx_SET (x0, gen_rtx_UNSPEC (mode, gen_rtvec (1, a),
					      unspec)));

  /* If (a == 0.0) Filter out infinity to prevent NaN for sqrt(0.0).  */
  if (!recip)
    {
      rtx zero = force_reg (mode, CONST0_RTX(mode));
      rtx mask;

      /* Handle masked compare.  */
      if (VECTOR_MODE_P (mode) && GET_MODE_SIZE (mode) == 64)
	{
	  mask = gen_reg_rtx (HImode);
	  /* Imm value 0x4 corresponds to not-equal comparison.  */
	  emit_insn (gen_avx512f_cmpv16sf3 (mask, zero, a, GEN_INT (0x4)));
	  emit_insn (gen_avx512f_blendmv16sf (x0, zero, x0, mask));
	}
      else
	{
	  mask = gen_reg_rtx (mode);
	  emit_insn (gen_rtx_SET (mask, gen_rtx_NE (mode, zero, a)));
	  emit_insn (gen_rtx_SET (x0, gen_rtx_AND (mode, x0, mask)));
	}
    }

  mthree = force_reg (mode, mthree);

  /* e0 = x0 * a */
  emit_insn (gen_rtx_SET (e0, gen_rtx_MULT (mode, x0, a)));

  unsigned vector_size = GET_MODE_SIZE (mode);
  if (TARGET_FMA
      || (TARGET_AVX512F && TARGET_EVEX512 && vector_size == 64)
      || (TARGET_AVX512VL && (vector_size == 32 || vector_size == 16)))
    emit_insn (gen_rtx_SET (e2,
			    gen_rtx_FMA (mode, e0, x0, mthree)));
  else
    {
      /* e1 = e0 * x0 */
      emit_insn (gen_rtx_SET (e1, gen_rtx_MULT (mode, e0, x0)));

      /* e2 = e1 - 3. */
      emit_insn (gen_rtx_SET (e2, gen_rtx_PLUS (mode, e1, mthree)));
    }

  mhalf = force_reg (mode, mhalf);
  if (recip)
    /* e3 = -.5 * x0 */
    emit_insn (gen_rtx_SET (e3, gen_rtx_MULT (mode, x0, mhalf)));
  else
    /* e3 = -.5 * e0 */
    emit_insn (gen_rtx_SET (e3, gen_rtx_MULT (mode, e0, mhalf)));
  /* ret = e2 * e3 */
  emit_insn (gen_rtx_SET (res, gen_rtx_MULT (mode, e2, e3)));
}

/* Expand fabs (OP0) and return a new rtx that holds the result.  The
   mask for masking out the sign-bit is stored in *SMASK, if that is
   non-null.  */

static rtx
ix86_expand_sse_fabs (rtx op0, rtx *smask)
{
  machine_mode vmode, mode = GET_MODE (op0);
  rtx xa, mask;

  xa = gen_reg_rtx (mode);
  if (mode == SFmode)
    vmode = V4SFmode;
  else if (mode == DFmode)
    vmode = V2DFmode;
  else
    vmode = mode;
  mask = ix86_build_signbit_mask (vmode, VECTOR_MODE_P (mode), true);
  if (!VECTOR_MODE_P (mode))
    {
      /* We need to generate a scalar mode mask in this case.  */
      rtx tmp = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (1, const0_rtx));
      tmp = gen_rtx_VEC_SELECT (mode, mask, tmp);
      mask = gen_reg_rtx (mode);
      emit_insn (gen_rtx_SET (mask, tmp));
    }
  emit_insn (gen_rtx_SET (xa, gen_rtx_AND (mode, op0, mask)));

  if (smask)
    *smask = mask;

  return xa;
}

/* Expands a comparison of OP0 with OP1 using comparison code CODE,
   swapping the operands if SWAP_OPERANDS is true.  The expanded
   code is a forward jump to a newly created label in case the
   comparison is true.  The generated label rtx is returned.  */
static rtx_code_label *
ix86_expand_sse_compare_and_jump (enum rtx_code code, rtx op0, rtx op1,
                                  bool swap_operands)
{
  bool unordered_compare = ix86_unordered_fp_compare (code);
  rtx_code_label *label;
  rtx tmp, reg;

  if (swap_operands)
    std::swap (op0, op1);

  label = gen_label_rtx ();
  tmp = gen_rtx_COMPARE (CCFPmode, op0, op1);
  if (unordered_compare)
    tmp = gen_rtx_UNSPEC (CCFPmode, gen_rtvec (1, tmp), UNSPEC_NOTRAP);
  reg = gen_rtx_REG (CCFPmode, FLAGS_REG);
  emit_insn (gen_rtx_SET (reg, tmp));
  tmp = gen_rtx_fmt_ee (code, VOIDmode, reg, const0_rtx);
  tmp = gen_rtx_IF_THEN_ELSE (VOIDmode, tmp,
			      gen_rtx_LABEL_REF (VOIDmode, label), pc_rtx);
  tmp = emit_jump_insn (gen_rtx_SET (pc_rtx, tmp));
  JUMP_LABEL (tmp) = label;

  return label;
}

/* Expand a mask generating SSE comparison instruction comparing OP0 with OP1
   using comparison code CODE.  Operands are swapped for the comparison if
   SWAP_OPERANDS is true.  Returns a rtx for the generated mask.  */
static rtx
ix86_expand_sse_compare_mask (enum rtx_code code, rtx op0, rtx op1,
			      bool swap_operands)
{
  rtx (*insn)(rtx, rtx, rtx, rtx);
  machine_mode mode = GET_MODE (op0);
  rtx mask = gen_reg_rtx (mode);

  if (swap_operands)
    std::swap (op0, op1);

  insn = mode == DFmode ? gen_setcc_df_sse : gen_setcc_sf_sse;

  emit_insn (insn (mask, op0, op1,
		   gen_rtx_fmt_ee (code, mode, op0, op1)));
  return mask;
}

/* Expand copysign from SIGN to the positive value ABS_VALUE
   storing in RESULT.  If MASK is non-null, it shall be a mask to mask out
   the sign-bit.  */

static void
ix86_sse_copysign_to_positive (rtx result, rtx abs_value, rtx sign, rtx mask)
{
  machine_mode mode = GET_MODE (sign);
  rtx sgn = gen_reg_rtx (mode);
  if (mask == NULL_RTX)
    {
      machine_mode vmode;

      if (mode == SFmode)
	vmode = V4SFmode;
      else if (mode == DFmode)
	vmode = V2DFmode;
      else if (mode == HFmode)
	vmode = V8HFmode;
      else
	vmode = mode;

      mask = ix86_build_signbit_mask (vmode, VECTOR_MODE_P (mode), false);
      if (!VECTOR_MODE_P (mode))
	{
	  /* We need to generate a scalar mode mask in this case.  */
	  rtx tmp = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (1, const0_rtx));
	  tmp = gen_rtx_VEC_SELECT (mode, mask, tmp);
	  mask = gen_reg_rtx (mode);
	  emit_insn (gen_rtx_SET (mask, tmp));
	}
    }
  else
    mask = gen_rtx_NOT (mode, mask);
  emit_insn (gen_rtx_SET (sgn, gen_rtx_AND (mode, mask, sign)));
  emit_insn (gen_rtx_SET (result, gen_rtx_IOR (mode, abs_value, sgn)));
}

/* Expand SSE sequence for computing lround from OP1 storing
   into OP0.  */

void
ix86_expand_lround (rtx op0, rtx op1)
{
  /* C code for the stuff we're doing below:
	tmp = op1 + copysign (nextafter (0.5, 0.0), op1)
	return (long)tmp;
   */
  machine_mode mode = GET_MODE (op1);
  const struct real_format *fmt;
  REAL_VALUE_TYPE pred_half, half_minus_pred_half;
  rtx adj;

  /* load nextafter (0.5, 0.0) */
  fmt = REAL_MODE_FORMAT (mode);
  real_2expN (&half_minus_pred_half, -(fmt->p) - 1, mode);
  real_arithmetic (&pred_half, MINUS_EXPR, &dconsthalf, &half_minus_pred_half);

  /* adj = copysign (0.5, op1) */
  adj = force_reg (mode, const_double_from_real_value (pred_half, mode));
  ix86_sse_copysign_to_positive (adj, adj, force_reg (mode, op1), NULL_RTX);

  /* adj = op1 + adj */
  adj = expand_simple_binop (mode, PLUS, adj, op1, NULL_RTX, 0, OPTAB_DIRECT);

  /* op0 = (imode)adj */
  expand_fix (op0, adj, 0);
}

/* Expand SSE2 sequence for computing lround from OPERAND1 storing
   into OPERAND0.  */

void
ix86_expand_lfloorceil (rtx op0, rtx op1, bool do_floor)
{
  /* C code for the stuff we're doing below (for do_floor):
	xi = (long)op1;
	xi -= (double)xi > op1 ? 1 : 0;
	return xi;
   */
  machine_mode fmode = GET_MODE (op1);
  machine_mode imode = GET_MODE (op0);
  rtx ireg, freg, tmp;
  rtx_code_label *label;

  /* reg = (long)op1 */
  ireg = gen_reg_rtx (imode);
  expand_fix (ireg, op1, 0);

  /* freg = (double)reg */
  freg = gen_reg_rtx (fmode);
  expand_float (freg, ireg, 0);

  /* ireg = (freg > op1) ? ireg - 1 : ireg */
  label = ix86_expand_sse_compare_and_jump (UNLE,
					    freg, op1, !do_floor);
  tmp = expand_simple_binop (imode, do_floor ? MINUS : PLUS,
			     ireg, const1_rtx, NULL_RTX, 0, OPTAB_DIRECT);
  emit_move_insn (ireg, tmp);

  emit_label (label);
  LABEL_NUSES (label) = 1;

  emit_move_insn (op0, ireg);
}

/* Generate and return a rtx of mode MODE for 2**n where n is the number
   of bits of the mantissa of MODE, which must be one of DFmode or SFmode.  */

static rtx
ix86_gen_TWO52 (machine_mode mode)
{
  const struct real_format *fmt;
  REAL_VALUE_TYPE TWO52r;
  rtx TWO52;

  fmt = REAL_MODE_FORMAT (mode);
  real_2expN (&TWO52r, fmt->p - 1, mode);
  TWO52 = const_double_from_real_value (TWO52r, mode);
  TWO52 = force_reg (mode, TWO52);

  return TWO52;
}

/* Expand rint rounding OPERAND1 and storing the result in OPERAND0.  */

void
ix86_expand_rint (rtx operand0, rtx operand1)
{
  /* C code for the stuff we're doing below:
	xa = fabs (operand1);
	if (!isless (xa, 2**52))
	  return operand1;
	two52 = 2**52;
	if (flag_rounding_math)
	  {
	    two52 = copysign (two52, operand1);
	    xa = operand1;
	  }
	xa = xa + two52 - two52;
	return copysign (xa, operand1);
   */
  machine_mode mode = GET_MODE (operand0);
  rtx res, xa, TWO52, mask;
  rtx_code_label *label;

  TWO52 = ix86_gen_TWO52 (mode);

  /* Temporary for holding the result, initialized to the input
     operand to ease control flow.  */
  res = copy_to_reg (operand1);

  /* xa = abs (operand1) */
  xa = ix86_expand_sse_fabs (res, &mask);

  /* if (!isless (xa, TWO52)) goto label; */
  label = ix86_expand_sse_compare_and_jump (UNLE, TWO52, xa, false);

  if (flag_rounding_math)
    {
      ix86_sse_copysign_to_positive (TWO52, TWO52, res, mask);
      xa = res;
    }

  xa = expand_simple_binop (mode, PLUS, xa, TWO52, NULL_RTX, 0, OPTAB_DIRECT);
  xa = expand_simple_binop (mode, MINUS, xa, TWO52, xa, 0, OPTAB_DIRECT);

  /* Remove the sign with FE_DOWNWARD, where x - x = -0.0.  */
  if (HONOR_SIGNED_ZEROS (mode) && flag_rounding_math)
    xa = ix86_expand_sse_fabs (xa, NULL);

  ix86_sse_copysign_to_positive (res, xa, res, mask);

  emit_label (label);
  LABEL_NUSES (label) = 1;

  emit_move_insn (operand0, res);
}

/* Expand SSE2 sequence for computing floor or ceil
   from OPERAND1 storing into OPERAND0.  */
void
ix86_expand_floorceil (rtx operand0, rtx operand1, bool do_floor)
{
  /* C code for the stuff we expand below.
	double xa = fabs (x), x2;
	if (!isless (xa, TWO52))
	  return x;
	x2 = (double)(long)x;

     Compensate.  Floor:
	if (x2 > x)
	  x2 -= 1;
     Compensate.  Ceil:
	if (x2 < x)
	  x2 += 1;

	if (HONOR_SIGNED_ZEROS (mode))
	  return copysign (x2, x);
	return x2;
   */
  machine_mode mode = GET_MODE (operand0);
  rtx xa, xi, TWO52, tmp, one, res, mask;
  rtx_code_label *label;

  TWO52 = ix86_gen_TWO52 (mode);

  /* Temporary for holding the result, initialized to the input
     operand to ease control flow.  */
  res = copy_to_reg (operand1);

  /* xa = abs (operand1) */
  xa = ix86_expand_sse_fabs (res, &mask);

  /* if (!isless (xa, TWO52)) goto label; */
  label = ix86_expand_sse_compare_and_jump (UNLE, TWO52, xa, false);

  /* xa = (double)(long)x */
  xi = gen_reg_rtx (int_mode_for_mode (mode).require ());
  expand_fix (xi, res, 0);
  expand_float (xa, xi, 0);

  /* generate 1.0 */
  one = force_reg (mode, const_double_from_real_value (dconst1, mode));

  /* Compensate: xa = xa - (xa > operand1 ? 1 : 0) */
  tmp = ix86_expand_sse_compare_mask (UNGT, xa, res, !do_floor);
  emit_insn (gen_rtx_SET (tmp, gen_rtx_AND (mode, one, tmp)));
  tmp = expand_simple_binop (mode, do_floor ? MINUS : PLUS,
			     xa, tmp, NULL_RTX, 0, OPTAB_DIRECT);
  if (HONOR_SIGNED_ZEROS (mode))
    {
      /* Remove the sign with FE_DOWNWARD, where x - x = -0.0.  */
      if (do_floor && flag_rounding_math)
	tmp = ix86_expand_sse_fabs (tmp, NULL);

      ix86_sse_copysign_to_positive (tmp, tmp, res, mask);
    }
  emit_move_insn (res, tmp);

  emit_label (label);
  LABEL_NUSES (label) = 1;

  emit_move_insn (operand0, res);
}

/* Expand SSE2 sequence for computing floor or ceil from OPERAND1 storing
   into OPERAND0 without relying on DImode truncation via cvttsd2siq
   that is only available on 64bit targets.  */
void
ix86_expand_floorceildf_32 (rtx operand0, rtx operand1, bool do_floor)
{
  /* C code for the stuff we expand below.
	double xa = fabs (x), x2;
	if (!isless (xa, TWO52))
	  return x;
	xa = xa + TWO52 - TWO52;
	x2 = copysign (xa, x);

     Compensate.  Floor:
	if (x2 > x)
	  x2 -= 1;
     Compensate.  Ceil:
	if (x2 < x)
	  x2 += 1;

	if (HONOR_SIGNED_ZEROS (mode))
	  x2 = copysign (x2, x);
	return x2;
   */
  machine_mode mode = GET_MODE (operand0);
  rtx xa, TWO52, tmp, one, res, mask;
  rtx_code_label *label;

  TWO52 = ix86_gen_TWO52 (mode);

  /* Temporary for holding the result, initialized to the input
     operand to ease control flow.  */
  res = copy_to_reg (operand1);

  /* xa = abs (operand1) */
  xa = ix86_expand_sse_fabs (res, &mask);

  /* if (!isless (xa, TWO52)) goto label; */
  label = ix86_expand_sse_compare_and_jump (UNLE, TWO52, xa, false);

  /* xa = xa + TWO52 - TWO52; */
  xa = expand_simple_binop (mode, PLUS, xa, TWO52, NULL_RTX, 0, OPTAB_DIRECT);
  xa = expand_simple_binop (mode, MINUS, xa, TWO52, xa, 0, OPTAB_DIRECT);

  /* xa = copysign (xa, operand1) */
  ix86_sse_copysign_to_positive (xa, xa, res, mask);

  /* generate 1.0 */
  one = force_reg (mode, const_double_from_real_value (dconst1, mode));

  /* Compensate: xa = xa - (xa > operand1 ? 1 : 0) */
  tmp = ix86_expand_sse_compare_mask (UNGT, xa, res, !do_floor);
  emit_insn (gen_rtx_SET (tmp, gen_rtx_AND (mode, one, tmp)));
  tmp = expand_simple_binop (mode, do_floor ? MINUS : PLUS,
			     xa, tmp, NULL_RTX, 0, OPTAB_DIRECT);
  if (HONOR_SIGNED_ZEROS (mode))
    {
      /* Remove the sign with FE_DOWNWARD, where x - x = -0.0.  */
      if (do_floor && flag_rounding_math)
	tmp = ix86_expand_sse_fabs (tmp, NULL);

      ix86_sse_copysign_to_positive (tmp, tmp, res, mask);
    }
  emit_move_insn (res, tmp);

  emit_label (label);
  LABEL_NUSES (label) = 1;

  emit_move_insn (operand0, res);
}

/* Expand SSE sequence for computing trunc
   from OPERAND1 storing into OPERAND0.  */
void
ix86_expand_trunc (rtx operand0, rtx operand1)
{
  /* C code for SSE variant we expand below.
	double xa = fabs (x), x2;
	if (!isless (xa, TWO52))
	  return x;
	x2 = (double)(long)x;
	if (HONOR_SIGNED_ZEROS (mode))
	  return copysign (x2, x);
	return x2;
   */
  machine_mode mode = GET_MODE (operand0);
  rtx xa, xi, TWO52, res, mask;
  rtx_code_label *label;

  TWO52 = ix86_gen_TWO52 (mode);

  /* Temporary for holding the result, initialized to the input
     operand to ease control flow.  */
  res = copy_to_reg (operand1);

  /* xa = abs (operand1) */
  xa = ix86_expand_sse_fabs (res, &mask);

  /* if (!isless (xa, TWO52)) goto label; */
  label = ix86_expand_sse_compare_and_jump (UNLE, TWO52, xa, false);

  /* xa = (double)(long)x */
  xi = gen_reg_rtx (int_mode_for_mode (mode).require ());
  expand_fix (xi, res, 0);
  expand_float (xa, xi, 0);

  if (HONOR_SIGNED_ZEROS (mode))
    ix86_sse_copysign_to_positive (xa, xa, res, mask);

  emit_move_insn (res, xa);

  emit_label (label);
  LABEL_NUSES (label) = 1;

  emit_move_insn (operand0, res);
}

/* Expand SSE sequence for computing trunc from OPERAND1 storing
   into OPERAND0 without relying on DImode truncation via cvttsd2siq
   that is only available on 64bit targets.  */
void
ix86_expand_truncdf_32 (rtx operand0, rtx operand1)
{
  machine_mode mode = GET_MODE (operand0);
  rtx xa, xa2, TWO52, tmp, one, res, mask;
  rtx_code_label *label;

  /* C code for SSE variant we expand below.
	double xa = fabs (x), x2;
	if (!isless (xa, TWO52))
	  return x;
	xa2 = xa + TWO52 - TWO52;
     Compensate:
	if (xa2 > xa)
	  xa2 -= 1.0;
	x2 = copysign (xa2, x);
	return x2;
   */

  TWO52 = ix86_gen_TWO52 (mode);

  /* Temporary for holding the result, initialized to the input
     operand to ease control flow.  */
  res =copy_to_reg (operand1);

  /* xa = abs (operand1) */
  xa = ix86_expand_sse_fabs (res, &mask);

  /* if (!isless (xa, TWO52)) goto label; */
  label = ix86_expand_sse_compare_and_jump (UNLE, TWO52, xa, false);

  /* xa2 = xa + TWO52 - TWO52; */
  xa2 = expand_simple_binop (mode, PLUS, xa, TWO52, NULL_RTX, 0, OPTAB_DIRECT);
  xa2 = expand_simple_binop (mode, MINUS, xa2, TWO52, xa2, 0, OPTAB_DIRECT);

  /* generate 1.0 */
  one = force_reg (mode, const_double_from_real_value (dconst1, mode));

  /* Compensate: xa2 = xa2 - (xa2 > xa ? 1 : 0)  */
  tmp = ix86_expand_sse_compare_mask (UNGT, xa2, xa, false);
  emit_insn (gen_rtx_SET (tmp, gen_rtx_AND (mode, one, tmp)));
  tmp = expand_simple_binop (mode, MINUS,
			     xa2, tmp, NULL_RTX, 0, OPTAB_DIRECT);
  /* Remove the sign with FE_DOWNWARD, where x - x = -0.0.  */
  if (HONOR_SIGNED_ZEROS (mode) && flag_rounding_math)
    tmp = ix86_expand_sse_fabs (tmp, NULL);

  /* res = copysign (xa2, operand1) */
  ix86_sse_copysign_to_positive (res, tmp, res, mask);

  emit_label (label);
  LABEL_NUSES (label) = 1;

  emit_move_insn (operand0, res);
}

/* Expand SSE sequence for computing round
   from OPERAND1 storing into OPERAND0.  */
void
ix86_expand_round (rtx operand0, rtx operand1)
{
  /* C code for the stuff we're doing below:
	double xa = fabs (x);
	if (!isless (xa, TWO52))
	  return x;
	xa = (double)(long)(xa + nextafter (0.5, 0.0));
	return copysign (xa, x);
   */
  machine_mode mode = GET_MODE (operand0);
  rtx res, TWO52, xa, xi, half, mask;
  rtx_code_label *label;
  const struct real_format *fmt;
  REAL_VALUE_TYPE pred_half, half_minus_pred_half;

  /* Temporary for holding the result, initialized to the input
     operand to ease control flow.  */
  res = copy_to_reg (operand1);

  TWO52 = ix86_gen_TWO52 (mode);
  xa = ix86_expand_sse_fabs (res, &mask);
  label = ix86_expand_sse_compare_and_jump (UNLE, TWO52, xa, false);

  /* load nextafter (0.5, 0.0) */
  fmt = REAL_MODE_FORMAT (mode);
  real_2expN (&half_minus_pred_half, -(fmt->p) - 1, mode);
  real_arithmetic (&pred_half, MINUS_EXPR, &dconsthalf, &half_minus_pred_half);

  /* xa = xa + 0.5 */
  half = force_reg (mode, const_double_from_real_value (pred_half, mode));
  xa = expand_simple_binop (mode, PLUS, xa, half, NULL_RTX, 0, OPTAB_DIRECT);

  /* xa = (double)(int64_t)xa */
  xi = gen_reg_rtx (int_mode_for_mode (mode).require ());
  expand_fix (xi, xa, 0);
  expand_float (xa, xi, 0);

  /* res = copysign (xa, operand1) */
  ix86_sse_copysign_to_positive (res, xa, res, mask);

  emit_label (label);
  LABEL_NUSES (label) = 1;

  emit_move_insn (operand0, res);
}

/* Expand SSE sequence for computing round from OPERAND1 storing
   into OPERAND0 without relying on DImode truncation via cvttsd2siq
   that is only available on 64bit targets.  */
void
ix86_expand_rounddf_32 (rtx operand0, rtx operand1)
{
  /* C code for the stuff we expand below.
	double xa = fabs (x), xa2, x2;
	if (!isless (xa, TWO52))
	  return x;
     Using the absolute value and copying back sign makes
     -0.0 -> -0.0 correct.
	xa2 = xa + TWO52 - TWO52;
     Compensate.
	dxa = xa2 - xa;
	if (dxa <= -0.5)
	  xa2 += 1;
	else if (dxa > 0.5)
	  xa2 -= 1;
	x2 = copysign (xa2, x);
	return x2;
   */
  machine_mode mode = GET_MODE (operand0);
  rtx xa, xa2, dxa, TWO52, tmp, half, mhalf, one, res, mask;
  rtx_code_label *label;

  TWO52 = ix86_gen_TWO52 (mode);

  /* Temporary for holding the result, initialized to the input
     operand to ease control flow.  */
  res = copy_to_reg (operand1);

  /* xa = abs (operand1) */
  xa = ix86_expand_sse_fabs (res, &mask);

  /* if (!isless (xa, TWO52)) goto label; */
  label = ix86_expand_sse_compare_and_jump (UNLE, TWO52, xa, false);

  /* xa2 = xa + TWO52 - TWO52; */
  xa2 = expand_simple_binop (mode, PLUS, xa, TWO52, NULL_RTX, 0, OPTAB_DIRECT);
  xa2 = expand_simple_binop (mode, MINUS, xa2, TWO52, xa2, 0, OPTAB_DIRECT);

  /* dxa = xa2 - xa; */
  dxa = expand_simple_binop (mode, MINUS, xa2, xa, NULL_RTX, 0, OPTAB_DIRECT);

  /* generate 0.5, 1.0 and -0.5 */
  half = force_reg (mode, const_double_from_real_value (dconsthalf, mode));
  one = expand_simple_binop (mode, PLUS, half, half, NULL_RTX, 0, OPTAB_DIRECT);
  mhalf = expand_simple_binop (mode, MINUS, half, one, NULL_RTX,
			       0, OPTAB_DIRECT);

  /* Compensate.  */
  /* xa2 = xa2 - (dxa > 0.5 ? 1 : 0) */
  tmp = ix86_expand_sse_compare_mask (UNGT, dxa, half, false);
  emit_insn (gen_rtx_SET (tmp, gen_rtx_AND (mode, tmp, one)));
  xa2 = expand_simple_binop (mode, MINUS, xa2, tmp, NULL_RTX, 0, OPTAB_DIRECT);
  /* xa2 = xa2 + (dxa <= -0.5 ? 1 : 0) */
  tmp = ix86_expand_sse_compare_mask (UNGE, mhalf, dxa, false);
  emit_insn (gen_rtx_SET (tmp, gen_rtx_AND (mode, tmp, one)));
  xa2 = expand_simple_binop (mode, PLUS, xa2, tmp, NULL_RTX, 0, OPTAB_DIRECT);

  /* res = copysign (xa2, operand1) */
  ix86_sse_copysign_to_positive (res, xa2, res, mask);

  emit_label (label);
  LABEL_NUSES (label) = 1;

  emit_move_insn (operand0, res);
}

/* Expand SSE sequence for computing round
   from OP1 storing into OP0 using sse4 round insn.  */
void
ix86_expand_round_sse4 (rtx op0, rtx op1)
{
  machine_mode mode = GET_MODE (op0);
  rtx e1, e2, res, half;
  const struct real_format *fmt;
  REAL_VALUE_TYPE pred_half, half_minus_pred_half;
  rtx (*gen_copysign) (rtx, rtx, rtx);
  rtx (*gen_round) (rtx, rtx, rtx);

  switch (mode)
    {
    case E_HFmode:
      gen_copysign = gen_copysignhf3;
      gen_round = gen_sse4_1_roundhf2;
      break;
    case E_SFmode:
      gen_copysign = gen_copysignsf3;
      gen_round = gen_sse4_1_roundsf2;
      break;
    case E_DFmode:
      gen_copysign = gen_copysigndf3;
      gen_round = gen_sse4_1_rounddf2;
      break;
    default:
      gcc_unreachable ();
    }

  /* round (a) = trunc (a + copysign (0.5, a)) */

  /* load nextafter (0.5, 0.0) */
  fmt = REAL_MODE_FORMAT (mode);
  real_2expN (&half_minus_pred_half, -(fmt->p) - 1, mode);
  real_arithmetic (&pred_half, MINUS_EXPR, &dconsthalf, &half_minus_pred_half);
  half = const_double_from_real_value (pred_half, mode);

  /* e1 = copysign (0.5, op1) */
  e1 = gen_reg_rtx (mode);
  emit_insn (gen_copysign (e1, half, op1));

  /* e2 = op1 + e1 */
  e2 = expand_simple_binop (mode, PLUS, op1, e1, NULL_RTX, 0, OPTAB_DIRECT);

  /* res = trunc (e2) */
  res = gen_reg_rtx (mode);
  emit_insn (gen_round (res, e2, GEN_INT (ROUND_TRUNC)));

  emit_move_insn (op0, res);
}

/* A cached (set (nil) (vselect (vconcat (nil) (nil)) (parallel [])))
   insn, so that expand_vselect{,_vconcat} doesn't have to create a fresh
   insn every time.  */

static GTY(()) rtx_insn *vselect_insn;

/* Initialize vselect_insn.  */

static void
init_vselect_insn (void)
{
  unsigned i;
  rtx x;

  x = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (MAX_VECT_LEN));
  for (i = 0; i < MAX_VECT_LEN; ++i)
    XVECEXP (x, 0, i) = const0_rtx;
  x = gen_rtx_VEC_SELECT (V2DFmode, gen_rtx_VEC_CONCAT (V4DFmode, const0_rtx,
							const0_rtx), x);
  x = gen_rtx_SET (const0_rtx, x);
  start_sequence ();
  vselect_insn = emit_insn (x);
  end_sequence ();
}

/* Construct (set target (vec_select op0 (parallel perm))) and
   return true if that's a valid instruction in the active ISA.  */

static bool
expand_vselect (rtx target, rtx op0, const unsigned char *perm,
		unsigned nelt, bool testing_p)
{
  unsigned int i;
  rtx x, save_vconcat;
  int icode;

  if (vselect_insn == NULL_RTX)
    init_vselect_insn ();

  x = XEXP (SET_SRC (PATTERN (vselect_insn)), 1);
  PUT_NUM_ELEM (XVEC (x, 0), nelt);
  for (i = 0; i < nelt; ++i)
    XVECEXP (x, 0, i) = GEN_INT (perm[i]);
  save_vconcat = XEXP (SET_SRC (PATTERN (vselect_insn)), 0);
  XEXP (SET_SRC (PATTERN (vselect_insn)), 0) = op0;
  PUT_MODE (SET_SRC (PATTERN (vselect_insn)), GET_MODE (target));
  SET_DEST (PATTERN (vselect_insn)) = target;
  icode = recog_memoized (vselect_insn);

  if (icode >= 0 && !testing_p)
    emit_insn (copy_rtx (PATTERN (vselect_insn)));

  SET_DEST (PATTERN (vselect_insn)) = const0_rtx;
  XEXP (SET_SRC (PATTERN (vselect_insn)), 0) = save_vconcat;
  INSN_CODE (vselect_insn) = -1;

  return icode >= 0;
}

/* Similar, but generate a vec_concat from op0 and op1 as well.  */

static bool
expand_vselect_vconcat (rtx target, rtx op0, rtx op1,
			const unsigned char *perm, unsigned nelt,
			bool testing_p)
{
  machine_mode v2mode;
  rtx x;
  bool ok;

  if (vselect_insn == NULL_RTX)
    init_vselect_insn ();

  if (!GET_MODE_2XWIDER_MODE (GET_MODE (op0)).exists (&v2mode))
    return false;
  x = XEXP (SET_SRC (PATTERN (vselect_insn)), 0);
  PUT_MODE (x, v2mode);
  XEXP (x, 0) = op0;
  XEXP (x, 1) = op1;
  ok = expand_vselect (target, x, perm, nelt, testing_p);
  XEXP (x, 0) = const0_rtx;
  XEXP (x, 1) = const0_rtx;
  return ok;
}

/* A subroutine of ix86_expand_vec_perm_const_1.  Try to implement D
   using movss or movsd.  */
static bool
expand_vec_perm_movs (struct expand_vec_perm_d *d)
{
  machine_mode vmode = d->vmode;
  unsigned i, nelt = d->nelt;
  rtx x;

  if (d->one_operand_p)
    return false;

  if (!(TARGET_SSE && (vmode == V4SFmode || vmode == V4SImode))
      && !(TARGET_MMX_WITH_SSE && (vmode == V2SFmode || vmode == V2SImode))
      && !(TARGET_SSE2 && (vmode == V2DFmode || vmode == V2DImode)))
    return false;

  /* Only the first element is changed.  */
  if (d->perm[0] != nelt && d->perm[0] != 0)
    return false;
  for (i = 1; i < nelt; ++i)
    if (d->perm[i] != i + nelt - d->perm[0])
      return false;

  if (d->testing_p)
    return true;

  if (d->perm[0] == nelt)
    x = gen_rtx_VEC_MERGE (vmode, d->op1, d->op0, GEN_INT (1));
  else
    x = gen_rtx_VEC_MERGE (vmode, d->op0, d->op1, GEN_INT (1));

  emit_insn (gen_rtx_SET (d->target, x));

  return true;
}

/* A subroutine of ix86_expand_vec_perm_const_1.  Try to implement D
   using insertps.  */
static bool
expand_vec_perm_insertps (struct expand_vec_perm_d *d)
{
  machine_mode vmode = d->vmode;
  unsigned i, cnt_s, nelt = d->nelt;
  int cnt_d = -1;
  rtx src, dst;

  if (d->one_operand_p)
    return false;

  if (!(TARGET_SSE4_1
	&& (vmode == V4SFmode || vmode == V4SImode
	    || (TARGET_MMX_WITH_SSE
		&& (vmode == V2SFmode || vmode == V2SImode)))))
    return false;

  for (i = 0; i < nelt; ++i)
    {
      if (d->perm[i] == i)
	continue;
      if (cnt_d != -1)
	{
	  cnt_d = -1;
	  break;
	}
      cnt_d = i;
    }

  if (cnt_d == -1)
    {
      for (i = 0; i < nelt; ++i)
	{
	  if (d->perm[i] == i + nelt)
	    continue;
	  if (cnt_d != -1)
	    return false;
	  cnt_d = i;
	}

      if (cnt_d == -1)
	return false;
    }

  if (d->testing_p)
    return true;

  gcc_assert (cnt_d != -1);

  cnt_s = d->perm[cnt_d];
  if (cnt_s < nelt)
    {
      src = d->op0;
      dst = d->op1;
    }
  else
    {
      cnt_s -= nelt;
      src = d->op1;
      dst = d->op0;
     }
  gcc_assert (cnt_s < nelt);

  rtx x = gen_sse4_1_insertps (vmode, d->target, dst, src,
			       GEN_INT (cnt_s << 6 | cnt_d << 4));
  emit_insn (x);

  return true;
}

/* A subroutine of ix86_expand_vec_perm_const_1.  Try to implement D
   in terms of blendp[sd] / pblendw / pblendvb / vpblendd.  */

static bool
expand_vec_perm_blend (struct expand_vec_perm_d *d)
{
  machine_mode mmode, vmode = d->vmode;
  unsigned i, nelt = d->nelt;
  unsigned HOST_WIDE_INT mask;
  rtx target, op0, op1, maskop, x;
  rtx rperm[32], vperm;

  if (d->one_operand_p)
    return false;
  if (TARGET_AVX512F && GET_MODE_SIZE (vmode) == 64
      && (TARGET_AVX512BW
	  || GET_MODE_UNIT_SIZE (vmode) >= 4))
    ;
  else if (TARGET_AVX2 && GET_MODE_SIZE (vmode) == 32)
    ;
  else if (TARGET_AVX && (vmode == V4DFmode || vmode == V8SFmode))
    ;
  else if (TARGET_SSE4_1
	   && (GET_MODE_SIZE (vmode) == 16
	       || (TARGET_MMX_WITH_SSE && GET_MODE_SIZE (vmode) == 8)
	       || GET_MODE_SIZE (vmode) == 4))
    ;
  else
    return false;

  /* This is a blend, not a permute.  Elements must stay in their
     respective lanes.  */
  for (i = 0; i < nelt; ++i)
    {
      unsigned e = d->perm[i];
      if (!(e == i || e == i + nelt))
	return false;
    }

  if (d->testing_p)
    return true;

  /* ??? Without SSE4.1, we could implement this with and/andn/or.  This
     decision should be extracted elsewhere, so that we only try that
     sequence once all budget==3 options have been tried.  */
  target = d->target;
  op0 = d->op0;
  op1 = d->op1;
  mask = 0;

  switch (vmode)
    {
    case E_V8DFmode:
    case E_V16SFmode:
    case E_V4DFmode:
    case E_V8SFmode:
    case E_V2DFmode:
    case E_V4SFmode:
    case E_V2SFmode:
    case E_V2HImode:
    case E_V4HImode:
    case E_V8HImode:
    case E_V8SImode:
    case E_V32HImode:
    case E_V64QImode:
    case E_V16SImode:
    case E_V8DImode:
      for (i = 0; i < nelt; ++i)
	mask |= ((unsigned HOST_WIDE_INT) (d->perm[i] >= nelt)) << i;
      break;

    case E_V2DImode:
      for (i = 0; i < 2; ++i)
	mask |= (d->perm[i] >= 2 ? 15 : 0) << (i * 4);
      vmode = V8HImode;
      goto do_subreg;

    case E_V2SImode:
      for (i = 0; i < 2; ++i)
	mask |= (d->perm[i] >= 2 ? 3 : 0) << (i * 2);
      vmode = V4HImode;
      goto do_subreg;

    case E_V4SImode:
      if (TARGET_AVX2)
	{
	  /* Use vpblendd instead of vpblendw.  */
	  for (i = 0; i < nelt; ++i)
	    mask |= ((unsigned HOST_WIDE_INT) (d->perm[i] >= nelt)) << i;
	  break;
	}
      else
	{
	  for (i = 0; i < 4; ++i)
	    mask |= (d->perm[i] >= 4 ? 3 : 0) << (i * 2);
	  vmode = V8HImode;
	  goto do_subreg;
	}

    case E_V16QImode:
      /* See if bytes move in pairs so we can use pblendw with
	 an immediate argument, rather than pblendvb with a vector
	 argument.  */
      for (i = 0; i < 16; i += 2)
	if (d->perm[i] + 1 != d->perm[i + 1])
	  {
	  use_pblendvb:
	    for (i = 0; i < nelt; ++i)
	      rperm[i] = (d->perm[i] < nelt ? const0_rtx : constm1_rtx);

	  finish_pblendvb:
	    vperm = gen_rtx_CONST_VECTOR (vmode, gen_rtvec_v (nelt, rperm));
	    vperm = force_reg (vmode, vperm);

	    if (GET_MODE_SIZE (vmode) == 4)
	      emit_insn (gen_mmx_pblendvb_v4qi (target, op0, op1, vperm));
	    else if (GET_MODE_SIZE (vmode) == 8)
	      emit_insn (gen_mmx_pblendvb_v8qi (target, op0, op1, vperm));
	    else if (GET_MODE_SIZE (vmode) == 16)
	      emit_insn (gen_sse4_1_pblendvb (target, op0, op1, vperm));
	    else
	      emit_insn (gen_avx2_pblendvb (target, op0, op1, vperm));
	    if (target != d->target)
	      emit_move_insn (d->target, gen_lowpart (d->vmode, target));
	    return true;
	  }

      for (i = 0; i < 8; ++i)
	mask |= (d->perm[i * 2] >= 16) << i;
      vmode = V8HImode;
      /* FALLTHRU */

    do_subreg:
      target = gen_reg_rtx (vmode);
      op0 = gen_lowpart (vmode, op0);
      op1 = gen_lowpart (vmode, op1);
      break;

    case E_V8QImode:
      for (i = 0; i < 8; i += 2)
	if (d->perm[i] + 1 != d->perm[i + 1])
	  goto use_pblendvb;

      for (i = 0; i < 4; ++i)
	mask |= (d->perm[i * 2] >= 8) << i;
      vmode = V4HImode;
      goto do_subreg;

    case E_V4QImode:
      for (i = 0; i < 4; i += 2)
	if (d->perm[i] + 1 != d->perm[i + 1])
	  goto use_pblendvb;

      for (i = 0; i < 2; ++i)
	mask |= (d->perm[i * 2] >= 4) << i;
      vmode = V2HImode;
      goto do_subreg;

    case E_V32QImode:
      /* See if bytes move in pairs.  If not, vpblendvb must be used.  */
      for (i = 0; i < 32; i += 2)
	if (d->perm[i] + 1 != d->perm[i + 1])
	  goto use_pblendvb;
      /* See if bytes move in quadruplets.  If yes, vpblendd
	 with immediate can be used.  */
      for (i = 0; i < 32; i += 4)
	if (d->perm[i] + 2 != d->perm[i + 2])
	  break;
      if (i < 32)
	{
	  /* See if bytes move the same in both lanes.  If yes,
	     vpblendw with immediate can be used.  */
	  for (i = 0; i < 16; i += 2)
	    if (d->perm[i] + 16 != d->perm[i + 16])
	      goto use_pblendvb;

	  /* Use vpblendw.  */
	  for (i = 0; i < 16; ++i)
	    mask |= (d->perm[i * 2] >= 32) << i;
	  vmode = V16HImode;
	  goto do_subreg;
	}

      /* Use vpblendd.  */
      for (i = 0; i < 8; ++i)
	mask |= (d->perm[i * 4] >= 32) << i;
      vmode = V8SImode;
      goto do_subreg;

    case E_V16HImode:
      /* See if words move in pairs.  If yes, vpblendd can be used.  */
      for (i = 0; i < 16; i += 2)
	if (d->perm[i] + 1 != d->perm[i + 1])
	  break;
      if (i < 16)
	{
	  /* See if words move the same in both lanes.  If not,
	     vpblendvb must be used.  */
	  for (i = 0; i < 8; i++)
	    if (d->perm[i] + 8 != d->perm[i + 8])
	      {
		/* Use vpblendvb.  */
		for (i = 0; i < 32; ++i)
		  rperm[i] = (d->perm[i / 2] < 16 ? const0_rtx : constm1_rtx);

		vmode = V32QImode;
		nelt = 32;
		target = gen_reg_rtx (vmode);
		op0 = gen_lowpart (vmode, op0);
		op1 = gen_lowpart (vmode, op1);
		goto finish_pblendvb;
	      }

	  /* Use vpblendw.  */
	  for (i = 0; i < 16; ++i)
	    mask |= (d->perm[i] >= 16) << i;
	  break;
	}

      /* Use vpblendd.  */
      for (i = 0; i < 8; ++i)
	mask |= (d->perm[i * 2] >= 16) << i;
      vmode = V8SImode;
      goto do_subreg;

    case E_V4DImode:
      /* Use vpblendd.  */
      for (i = 0; i < 4; ++i)
	mask |= (d->perm[i] >= 4 ? 3 : 0) << (i * 2);
      vmode = V8SImode;
      goto do_subreg;

    default:
      gcc_unreachable ();
    }

  switch (vmode)
    {
    case E_V8DFmode:
    case E_V8DImode:
      mmode = QImode;
      break;
    case E_V16SFmode:
    case E_V16SImode:
      mmode = HImode;
      break;
    case E_V32HImode:
      mmode = SImode;
      break;
    case E_V64QImode:
      mmode = DImode;
      break;
    default:
      mmode = VOIDmode;
    }

  /* Canonicalize vec_merge.  */
  if (swap_commutative_operands_p (op1, op0)
      /* Two operands have same precedence, then
	 first bit of mask select first operand.  */
      || (!swap_commutative_operands_p (op0, op1)
	  && !(mask & 1)))
    {
      unsigned n_elts = GET_MODE_NUNITS (vmode);
      std::swap (op0, op1);
      unsigned HOST_WIDE_INT mask_all = HOST_WIDE_INT_1U;
      if (n_elts == HOST_BITS_PER_WIDE_INT)
	mask_all  = -1;
      else
	mask_all = (HOST_WIDE_INT_1U << n_elts) - 1;
      mask = ~mask & mask_all;
    }

  if (mmode != VOIDmode)
    maskop = force_reg (mmode, gen_int_mode (mask, mmode));
  else
    maskop = GEN_INT (mask);

  /* This matches five different patterns with the different modes.  */
  x = gen_rtx_VEC_MERGE (vmode, op1, op0, maskop);
  x = gen_rtx_SET (target, x);
  emit_insn (x);
  if (target != d->target)
    emit_move_insn (d->target, gen_lowpart (d->vmode, target));

  return true;
}

/* A subroutine of ix86_expand_vec_perm_const_1.  Try to implement D
   in terms of the variable form of vpermilps.

   Note that we will have already failed the immediate input vpermilps,
   which requires that the high and low part shuffle be identical; the
   variable form doesn't require that.  */

static bool
expand_vec_perm_vpermil (struct expand_vec_perm_d *d)
{
  rtx rperm[8], vperm;
  unsigned i;

  if (!TARGET_AVX || d->vmode != V8SFmode || !d->one_operand_p)
    return false;

  /* We can only permute within the 128-bit lane.  */
  for (i = 0; i < 8; ++i)
    {
      unsigned e = d->perm[i];
      if (i < 4 ? e >= 4 : e < 4)
	return false;
    }

  if (d->testing_p)
    return true;

  for (i = 0; i < 8; ++i)
    {
      unsigned e = d->perm[i];

      /* Within each 128-bit lane, the elements of op0 are numbered
	 from 0 and the elements of op1 are numbered from 4.  */
      if (e >= 8 + 4)
	e -= 8;
      else if (e >= 4)
	e -= 4;

      rperm[i] = GEN_INT (e);
    }

  vperm = gen_rtx_CONST_VECTOR (V8SImode, gen_rtvec_v (8, rperm));
  vperm = force_reg (V8SImode, vperm);
  emit_insn (gen_avx_vpermilvarv8sf3 (d->target, d->op0, vperm));

  return true;
}

/* For V*[QHS]Imode permutations, check if the same permutation
   can't be performed in a 2x, 4x or 8x wider inner mode.  */

static bool
canonicalize_vector_int_perm (const struct expand_vec_perm_d *d,
			      struct expand_vec_perm_d *nd)
{
  int i;
  machine_mode mode = VOIDmode;

  switch (d->vmode)
    {
    case E_V8QImode: mode = V4HImode; break;
    case E_V16QImode: mode = V8HImode; break;
    case E_V32QImode: mode = V16HImode; break;
    case E_V64QImode: mode = V32HImode; break;
    case E_V4HImode: mode = V2SImode; break;
    case E_V8HImode: mode = V4SImode; break;
    case E_V16HImode: mode = V8SImode; break;
    case E_V32HImode: mode = V16SImode; break;
    case E_V4SImode: mode = V2DImode; break;
    case E_V8SImode: mode = V4DImode; break;
    case E_V16SImode: mode = V8DImode; break;
    default: return false;
    }
  for (i = 0; i < d->nelt; i += 2)
    if ((d->perm[i] & 1) || d->perm[i + 1] != d->perm[i] + 1)
      return false;
  nd->vmode = mode;
  nd->nelt = d->nelt / 2;
  for (i = 0; i < nd->nelt; i++)
    nd->perm[i] = d->perm[2 * i] / 2;
  if (GET_MODE_INNER (mode) != DImode)
    canonicalize_vector_int_perm (nd, nd);
  if (nd != d)
    {
      nd->one_operand_p = d->one_operand_p;
      nd->testing_p = d->testing_p;
      if (d->op0 == d->op1)
	nd->op0 = nd->op1 = gen_lowpart (nd->vmode, d->op0);
      else
	{
	  nd->op0 = gen_lowpart (nd->vmode, d->op0);
	  nd->op1 = gen_lowpart (nd->vmode, d->op1);
	}
      if (d->testing_p)
	nd->target = gen_raw_REG (nd->vmode, LAST_VIRTUAL_REGISTER + 1);
      else
	nd->target = gen_reg_rtx (nd->vmode);
    }
  return true;
}

/* Return true if permutation D can be performed as VMODE permutation
   instead.  */

static bool
valid_perm_using_mode_p (machine_mode vmode, struct expand_vec_perm_d *d)
{
  unsigned int i, j, chunk;

  if (GET_MODE_CLASS (vmode) != MODE_VECTOR_INT
      || GET_MODE_CLASS (d->vmode) != MODE_VECTOR_INT
      || GET_MODE_SIZE (vmode) != GET_MODE_SIZE (d->vmode))
    return false;

  if (GET_MODE_NUNITS (vmode) >= d->nelt)
    return true;

  chunk = d->nelt / GET_MODE_NUNITS (vmode);
  for (i = 0; i < d->nelt; i += chunk)
    if (d->perm[i] & (chunk - 1))
      return false;
    else
      for (j = 1; j < chunk; ++j)
	if (d->perm[i] + j != d->perm[i + j])
	  return false;

  return true;
}

/* A subroutine of ix86_expand_vec_perm_const_1.  Try to implement D
   in terms of pshufb, vpperm, vpermq, vpermd, vpermps or vperm2i128.  */

static bool
expand_vec_perm_pshufb (struct expand_vec_perm_d *d)
{
  unsigned i, nelt, eltsz, mask;
  unsigned char perm[64];
  machine_mode vmode;
  struct expand_vec_perm_d nd;
  rtx rperm[64], vperm, target, op0, op1;

  nelt = d->nelt;

  if (!d->one_operand_p)
    switch (GET_MODE_SIZE (d->vmode))
      {
      case 4:
	if (!TARGET_XOP)
	  return false;
	vmode = V4QImode;
	break;

      case 8:
	if (!TARGET_XOP)
	  return false;
	vmode = V8QImode;
	break;

      case 16:
	if (!TARGET_XOP)
	  return false;
	vmode = V16QImode;
	break;

      case 32:
	if (!TARGET_AVX2)
	  return false;

	if (valid_perm_using_mode_p (V2TImode, d))
	  {
	    if (d->testing_p)
	      return true;

	    /* Use vperm2i128 insn.  The pattern uses
	       V4DImode instead of V2TImode.  */
	    target = d->target;
	    if (d->vmode != V4DImode)
	      target = gen_reg_rtx (V4DImode);
	    op0 = gen_lowpart (V4DImode, d->op0);
	    op1 = gen_lowpart (V4DImode, d->op1);
	    rperm[0]
	      = GEN_INT ((d->perm[0] / (nelt / 2))
			 | ((d->perm[nelt / 2] / (nelt / 2)) * 16));
	    emit_insn (gen_avx2_permv2ti (target, op0, op1, rperm[0]));
	    if (target != d->target)
	      emit_move_insn (d->target, gen_lowpart (d->vmode, target));
	    return true;
	  }
	/* FALLTHRU */

      default:
	return false;
      }
  else
    switch (GET_MODE_SIZE (d->vmode))
      {
      case 4:
	if (!TARGET_SSSE3)
	  return false;
	vmode = V4QImode;
	break;

      case 8:
	if (!TARGET_SSSE3)
	  return false;
	vmode = V8QImode;
	break;

      case 16:
	if (!TARGET_SSSE3)
	  return false;
	vmode = V16QImode;
	break;

      case 32:
	if (!TARGET_AVX2)
	  return false;

	/* V4DImode should be already handled through
	   expand_vselect by vpermq instruction.  */
	gcc_assert (d->vmode != V4DImode);

	vmode = V32QImode;
	if (d->vmode == V8SImode
	    || d->vmode == V16HImode
	    || d->vmode == V32QImode)
	  {
	    /* First see if vpermq can be used for
	       V8SImode/V16HImode/V32QImode.  */
	    if (valid_perm_using_mode_p (V4DImode, d))
	      {
		for (i = 0; i < 4; i++)
		  perm[i] = (d->perm[i * nelt / 4] * 4 / nelt) & 3;
		if (d->testing_p)
		  return true;
		target = gen_reg_rtx (V4DImode);
		if (expand_vselect (target, gen_lowpart (V4DImode, d->op0),
				    perm, 4, false))
		  {
		    emit_move_insn (d->target,
				    gen_lowpart (d->vmode, target));
		    return true;
		  }
		return false;
	      }

	    /* Next see if vpermd can be used.  */
	    if (valid_perm_using_mode_p (V8SImode, d))
	      vmode = V8SImode;
	  }
	/* Or if vpermps can be used.  */
	else if (d->vmode == V8SFmode)
	  vmode = V8SImode;

	if (vmode == V32QImode)
	  {
	    /* vpshufb only works intra lanes, it is not
	       possible to shuffle bytes in between the lanes.  */
	    for (i = 0; i < nelt; ++i)
	      if ((d->perm[i] ^ i) & (nelt / 2))
		return false;
	  }
	break;

      case 64:
	if (!TARGET_AVX512BW)
	  return false;

	/* If vpermq didn't work, vpshufb won't work either.  */
	if (d->vmode == V8DFmode || d->vmode == V8DImode)
	  return false;

	vmode = V64QImode;
	if (d->vmode == V16SImode
	    || d->vmode == V32HImode
	    || d->vmode == V64QImode)
	  {
	    /* First see if vpermq can be used for
	       V16SImode/V32HImode/V64QImode.  */
	    if (valid_perm_using_mode_p (V8DImode, d))
	      {
		for (i = 0; i < 8; i++)
		  perm[i] = (d->perm[i * nelt / 8] * 8 / nelt) & 7;
		if (d->testing_p)
		  return true;
		target = gen_reg_rtx (V8DImode);
		if (expand_vselect (target, gen_lowpart (V8DImode, d->op0),
				    perm, 8, false))
		  {
		    emit_move_insn (d->target,
				    gen_lowpart (d->vmode, target));
		    return true;
		  }
		return false;
	      }

	    /* Next see if vpermd can be used.  */
	    if (valid_perm_using_mode_p (V16SImode, d))
	      vmode = V16SImode;
	  }
	/* Or if vpermps can be used.  */
	else if (d->vmode == V16SFmode)
	  vmode = V16SImode;

	if (vmode == V64QImode)
	  {
	    /* vpshufb only works intra lanes, it is not
	       possible to shuffle bytes in between the lanes.  */
	    for (i = 0; i < nelt; ++i)
	      if ((d->perm[i] ^ i) & (3 * nelt / 4))
		return false;
	  }
	break;

      default:
	return false;
      }

  if (d->testing_p)
    return true;

  /* Try to avoid variable permutation instruction.  */
  if (canonicalize_vector_int_perm (d, &nd) && expand_vec_perm_1 (&nd))
    {
      emit_move_insn (d->target, gen_lowpart (d->vmode, nd.target));
      return true;
    }

  if (vmode == V8SImode)
    for (i = 0; i < 8; ++i)
      rperm[i] = GEN_INT ((d->perm[i * nelt / 8] * 8 / nelt) & 7);
  else if (vmode == V16SImode)
    for (i = 0; i < 16; ++i)
      rperm[i] = GEN_INT ((d->perm[i * nelt / 16] * 16 / nelt) & 15);
  else
    {
      eltsz = GET_MODE_UNIT_SIZE (d->vmode);
      if (!d->one_operand_p)
	mask = 2 * nelt - 1;
      else if (vmode == V64QImode)
	mask = nelt / 4 - 1;
      else if (vmode == V32QImode)
	mask = nelt / 2 - 1;
      else
	mask = nelt - 1;

      for (i = 0; i < nelt; ++i)
	{
	  unsigned j, e = d->perm[i] & mask;
	  for (j = 0; j < eltsz; ++j)
	    rperm[i * eltsz + j] = GEN_INT (e * eltsz + j);
	}
    }

  machine_mode vpmode = vmode;

  nelt = GET_MODE_SIZE (vmode);

  /* Emulate narrow modes with V16QI instructions.  */
  if (nelt < 16)
    {
      rtx m128 = GEN_INT (-128);

      /* Remap elements from the second operand, as we have to
	 account for inactive top elements from the first operand.  */
      if (!d->one_operand_p)
	{
	  for (i = 0; i < nelt; ++i)
	    {
	      unsigned ival = UINTVAL (rperm[i]);
	      if (ival >= nelt)
		rperm[i] = GEN_INT (ival + 16 - nelt);
	    }
	}

      /* Fill inactive elements in the top positions with zeros.  */
      for (i = nelt; i < 16; ++i)
	rperm[i] = m128;

      vpmode = V16QImode;
    }

  vperm = gen_rtx_CONST_VECTOR (vpmode,
				gen_rtvec_v (GET_MODE_NUNITS (vpmode), rperm));
  vperm = force_reg (vpmode, vperm);

  if (vmode == d->vmode)
    target = d->target;
  else
    target = gen_reg_rtx (vmode);

  op0 = gen_lowpart (vmode, d->op0);

  if (d->one_operand_p)
    {
      rtx (*gen) (rtx, rtx, rtx);

      if (vmode == V4QImode)
	gen = gen_mmx_pshufbv4qi3;
      else if (vmode == V8QImode)
	gen = gen_mmx_pshufbv8qi3;
      else if (vmode == V16QImode)
	gen = gen_ssse3_pshufbv16qi3;
      else if (vmode == V32QImode)
	gen = gen_avx2_pshufbv32qi3;
      else if (vmode == V64QImode)
	gen = gen_avx512bw_pshufbv64qi3;
      else if (vmode == V8SFmode)
	gen = gen_avx2_permvarv8sf;
      else if (vmode == V8SImode)
	gen = gen_avx2_permvarv8si;
      else if (vmode == V16SFmode)
	gen = gen_avx512f_permvarv16sf;
      else if (vmode == V16SImode)
	gen = gen_avx512f_permvarv16si;
      else
	gcc_unreachable ();

      emit_insn (gen (target, op0, vperm));
    }
  else
    {
      rtx (*gen) (rtx, rtx, rtx, rtx);

      op1 = gen_lowpart (vmode, d->op1);

      if (vmode == V4QImode)
	gen = gen_mmx_ppermv32;
      else if (vmode == V8QImode)
	gen = gen_mmx_ppermv64;
      else if (vmode == V16QImode)
	gen = gen_xop_pperm;
      else
	gcc_unreachable ();

      emit_insn (gen (target, op0, op1, vperm));
    }

  if (target != d->target)
    emit_move_insn (d->target, gen_lowpart (d->vmode, target));

  return true;
}

/* Try to expand one-operand permutation with constant mask.  */

static bool
ix86_expand_vec_one_operand_perm_avx512 (struct expand_vec_perm_d *d)
{
  machine_mode mode = GET_MODE (d->op0);
  machine_mode maskmode = mode;
  unsigned inner_size = GET_MODE_SIZE (GET_MODE_INNER (mode));
  rtx (*gen) (rtx, rtx, rtx) = NULL;
  rtx target, op0, mask;
  rtx vec[64];

  if (!rtx_equal_p (d->op0, d->op1))
    return false;

  if (!TARGET_AVX512F)
    return false;

  /* Accept VNxHImode and VNxQImode now.  */
  if (!TARGET_AVX512VL && GET_MODE_SIZE (mode) < 64)
    return false;

  /* vpermw.  */
  if (!TARGET_AVX512BW && inner_size == 2)
    return false;

  /* vpermb.  */
  if (!TARGET_AVX512VBMI && inner_size == 1)
    return false;

  switch (mode)
    {
    case E_V16SImode:
      gen = gen_avx512f_permvarv16si;
      break;
    case E_V16SFmode:
      gen = gen_avx512f_permvarv16sf;
      maskmode = V16SImode;
      break;
    case E_V8DImode:
      gen = gen_avx512f_permvarv8di;
      break;
    case E_V8DFmode:
      gen = gen_avx512f_permvarv8df;
      maskmode = V8DImode;
      break;
    case E_V32HImode:
      gen = gen_avx512bw_permvarv32hi;
      break;
    case E_V16HImode:
      gen = gen_avx512vl_permvarv16hi;
      break;
    case E_V8HImode:
      gen = gen_avx512vl_permvarv8hi;
      break;
    case E_V64QImode:
      gen = gen_avx512bw_permvarv64qi;
      break;
    case E_V32QImode:
      gen = gen_avx512vl_permvarv32qi;
      break;
    case E_V16QImode:
      gen = gen_avx512vl_permvarv16qi;
      break;

    default:
      return false;
    }

  if (d->testing_p)
    return true;

  target = d->target;
  op0 = d->op0;
  for (int i = 0; i < d->nelt; ++i)
    vec[i] = GEN_INT (d->perm[i]);
  mask = gen_rtx_CONST_VECTOR (maskmode, gen_rtvec_v (d->nelt, vec));
  emit_insn (gen (target, op0, force_reg (maskmode, mask)));
  return true;
}

static bool expand_vec_perm_palignr (struct expand_vec_perm_d *d, bool);

/* A subroutine of ix86_expand_vec_perm_const_1.  Try to instantiate D
   in a single instruction.  */

static bool
expand_vec_perm_1 (struct expand_vec_perm_d *d)
{
  unsigned i, nelt = d->nelt;
  struct expand_vec_perm_d nd;

  /* Check plain VEC_SELECT first, because AVX has instructions that could
     match both SEL and SEL+CONCAT, but the plain SEL will allow a memory
     input where SEL+CONCAT may not.  */
  if (d->one_operand_p)
    {
      int mask = nelt - 1;
      bool identity_perm = true;
      bool broadcast_perm = true;

      for (i = 0; i < nelt; i++)
	{
	  nd.perm[i] = d->perm[i] & mask;
	  if (nd.perm[i] != i)
	    identity_perm = false;
	  if (nd.perm[i])
	    broadcast_perm = false;
	}

      if (identity_perm)
	{
	  if (!d->testing_p)
	    emit_move_insn (d->target, d->op0);
	  return true;
	}
      else if (broadcast_perm && TARGET_AVX2)
	{
	  /* Use vpbroadcast{b,w,d}.  */
	  rtx (*gen) (rtx, rtx) = NULL;
	  switch (d->vmode)
	    {
	    case E_V64QImode:
	      if (TARGET_AVX512BW)
		gen = gen_avx512bw_vec_dupv64qi_1;
	      break;
	    case E_V32QImode:
	      gen = gen_avx2_pbroadcastv32qi_1;
	      break;
	    case E_V32HImode:
	      if (TARGET_AVX512BW)
		gen = gen_avx512bw_vec_dupv32hi_1;
	      break;
	    case E_V16HImode:
	      gen = gen_avx2_pbroadcastv16hi_1;
	      break;
	    case E_V16SImode:
	      if (TARGET_AVX512F)
		gen = gen_avx512f_vec_dupv16si_1;
	      break;
	    case E_V8SImode:
	      gen = gen_avx2_pbroadcastv8si_1;
	      break;
	    case E_V16QImode:
	      gen = gen_avx2_pbroadcastv16qi;
	      break;
	    case E_V8HImode:
	      gen = gen_avx2_pbroadcastv8hi;
	      break;
	    case E_V16SFmode:
	      if (TARGET_AVX512F)
		gen = gen_avx512f_vec_dupv16sf_1;
	      break;
	    case E_V8SFmode:
	      gen = gen_avx2_vec_dupv8sf_1;
	      break;
	    case E_V8DFmode:
	      if (TARGET_AVX512F)
		gen = gen_avx512f_vec_dupv8df_1;
	      break;
	    case E_V8DImode:
	      if (TARGET_AVX512F)
		gen = gen_avx512f_vec_dupv8di_1;
	      break;
	    /* For other modes prefer other shuffles this function creates.  */
	    default: break;
	    }
	  if (gen != NULL)
	    {
	      if (!d->testing_p)
		emit_insn (gen (d->target, d->op0));
	      return true;
	    }
	}

      if (expand_vselect (d->target, d->op0, nd.perm, nelt, d->testing_p))
	return true;

      /* There are plenty of patterns in sse.md that are written for
	 SEL+CONCAT and are not replicated for a single op.  Perhaps
	 that should be changed, to avoid the nastiness here.  */

      /* Recognize interleave style patterns, which means incrementing
	 every other permutation operand.  */
      for (i = 0; i < nelt; i += 2)
	{
	  nd.perm[i] = d->perm[i] & mask;
	  nd.perm[i + 1] = (d->perm[i + 1] & mask) + nelt;
	}
      if (expand_vselect_vconcat (d->target, d->op0, d->op0, nd.perm, nelt,
				  d->testing_p))
	return true;

      /* Recognize shufps, which means adding {0, 0, nelt, nelt}.  */
      if (nelt >= 4)
	{
	  for (i = 0; i < nelt; i += 4)
	    {
	      nd.perm[i + 0] = d->perm[i + 0] & mask;
	      nd.perm[i + 1] = d->perm[i + 1] & mask;
	      nd.perm[i + 2] = (d->perm[i + 2] & mask) + nelt;
	      nd.perm[i + 3] = (d->perm[i + 3] & mask) + nelt;
	    }

	  if (expand_vselect_vconcat (d->target, d->op0, d->op0, nd.perm, nelt,
				      d->testing_p))
	    return true;
	}
    }

  /* Try the SSE4.1 blend variable merge instructions.  */
  if (expand_vec_perm_blend (d))
    return true;

  /* Try movss/movsd instructions.  */
  if (expand_vec_perm_movs (d))
    return true;

  /* Try the SSE4.1 insertps instruction.  */
  if (expand_vec_perm_insertps (d))
    return true;

  /* Try the fully general two operand permute.  */
  if (expand_vselect_vconcat (d->target, d->op0, d->op1, d->perm, nelt,
			      d->testing_p))
    return true;

  /* Recognize interleave style patterns with reversed operands.  */
  if (!d->one_operand_p)
    {
      for (i = 0; i < nelt; ++i)
	{
	  unsigned e = d->perm[i];
	  if (e >= nelt)
	    e -= nelt;
	  else
	    e += nelt;
	  nd.perm[i] = e;
	}

      if (expand_vselect_vconcat (d->target, d->op1, d->op0, nd.perm, nelt,
				  d->testing_p))
	return true;
    }

  /* Try one of the AVX vpermil variable permutations.  */
  if (expand_vec_perm_vpermil (d))
    return true;

  /* Try the SSSE3 pshufb or XOP vpperm or AVX2 vperm2i128,
     vpshufb, vpermd, vpermps or vpermq variable permutation.  */
  if (expand_vec_perm_pshufb (d))
    return true;

  /* Try the AVX2 vpalignr instruction.  */
  if (expand_vec_perm_palignr (d, true))
    return true;

  /* Try the AVX512F vperm{w,b,s,d} instructions  */
  if (ix86_expand_vec_one_operand_perm_avx512 (d))
    return true;

  /* Try the AVX512F vpermt2/vpermi2 instructions.  */
  if (ix86_expand_vec_perm_vpermt2 (NULL_RTX, NULL_RTX, NULL_RTX, NULL_RTX, d))
    return true;

  /* See if we can get the same permutation in different vector integer
     mode.  */
  if (canonicalize_vector_int_perm (d, &nd) && expand_vec_perm_1 (&nd))
    {
      if (!d->testing_p)
	emit_move_insn (d->target, gen_lowpart (d->vmode, nd.target));
      return true;
    }
  return false;
}

/* Canonicalize vec_perm index to make the first index
   always comes from the first vector.  */
static void
ix86_vec_perm_index_canon (struct expand_vec_perm_d *d)
{
  unsigned nelt = d->nelt;
  if (d->perm[0] < nelt)
    return;

  for (unsigned i = 0; i != nelt; i++)
    d->perm[i] = (d->perm[i] + nelt) % (2 * nelt);

  std::swap (d->op0, d->op1);
  return;
}

/* A subroutine of ix86_expand_vec_perm_const_1. Try to implement D
   in terms of a pair of shufps+ shufps/pshufd instructions.  */
static bool
expand_vec_perm_shufps_shufps (struct expand_vec_perm_d *d)
{
  unsigned char perm1[4];
  machine_mode vmode = d->vmode;
  bool ok;
  unsigned i, j, k, count = 0;

  if (d->one_operand_p
      || (vmode != V4SImode && vmode != V4SFmode))
    return false;

  if (d->testing_p)
    return true;

  ix86_vec_perm_index_canon (d);
  for (i = 0; i < 4; ++i)
    count += d->perm[i] > 3 ? 1 : 0;

  gcc_assert (count & 3);

  rtx tmp = gen_reg_rtx (vmode);
  /* 2 from op0 and 2 from op1.  */
  if (count == 2)
    {
      unsigned char perm2[4];
      for (i = 0, j = 0, k = 2; i < 4; ++i)
	if (d->perm[i] & 4)
	  {
	    perm1[k++] = d->perm[i];
	    perm2[i] = k - 1;
	  }
	else
	  {
	    perm1[j++] = d->perm[i];
	    perm2[i] = j - 1;
	  }

      /* shufps.  */
      ok = expand_vselect_vconcat (tmp, d->op0, d->op1,
				  perm1, d->nelt, false);
      gcc_assert (ok);
      if (vmode == V4SImode && TARGET_SSE2)
      /* pshufd.  */
	ok = expand_vselect (d->target, tmp,
			     perm2, d->nelt, false);
      else
	{
	  /* shufps.  */
	  perm2[2] += 4;
	  perm2[3] += 4;
	  ok = expand_vselect_vconcat (d->target, tmp, tmp,
				       perm2, d->nelt, false);
	}
      gcc_assert (ok);
    }
  /* 3 from one op and 1 from another.  */
  else
    {
      unsigned pair_idx = 8, lone_idx = 8, shift;

      /* Find the lone index.  */
      for (i = 0; i < 4; ++i)
	if ((d->perm[i] > 3 && count == 1)
	    || (d->perm[i] < 4 && count == 3))
	  lone_idx = i;

      /* When lone_idx is not 0, it must from second op(count == 1).  */
      gcc_assert (count == (lone_idx ? 1 : 3));

      /* Find the pair index that sits in the same half as the lone index.  */
      shift = lone_idx & 2;
      pair_idx = 1 - lone_idx + 2 * shift;

      /* First permutate lone index and pair index into the same vector as
	 [ lone, lone, pair, pair ].  */
      perm1[1] = perm1[0]
	= (count == 3) ? d->perm[lone_idx] : d->perm[lone_idx] - 4;
      perm1[3] = perm1[2]
	= (count == 3) ? d->perm[pair_idx] : d->perm[pair_idx] + 4;

      /* Alway put the vector contains lone indx at the first.  */
      if (count == 1)
	std::swap (d->op0, d->op1);

      /* shufps.  */
      ok = expand_vselect_vconcat (tmp, d->op0, d->op1,
				   perm1, d->nelt, false);
      gcc_assert (ok);

      /* Refine lone and pair index to original order.  */
      perm1[shift] = lone_idx << 1;
      perm1[shift + 1] = pair_idx << 1;

      /* Select the remaining 2 elements in another vector.  */
      for (i = 2 - shift; i < 4 - shift; ++i)
	perm1[i] = lone_idx == 1 ? d->perm[i] + 4 : d->perm[i];

      /* Adjust to original selector.  */
      if (lone_idx > 1)
	std::swap (tmp, d->op1);

      /* shufps.  */
      ok = expand_vselect_vconcat (d->target, tmp, d->op1,
				   perm1, d->nelt, false);

      gcc_assert (ok);
    }

  return true;
}

/* A subroutine of ix86_expand_vec_perm_const_1.  Try to implement D
   in terms of a pair of pshuflw + pshufhw instructions.  */

static bool
expand_vec_perm_pshuflw_pshufhw (struct expand_vec_perm_d *d)
{
  unsigned char perm2[MAX_VECT_LEN];
  unsigned i;
  bool ok;

  if (d->vmode != V8HImode || !d->one_operand_p)
    return false;

  /* The two permutations only operate in 64-bit lanes.  */
  for (i = 0; i < 4; ++i)
    if (d->perm[i] >= 4)
      return false;
  for (i = 4; i < 8; ++i)
    if (d->perm[i] < 4)
      return false;

  if (d->testing_p)
    return true;

  /* Emit the pshuflw.  */
  memcpy (perm2, d->perm, 4);
  for (i = 4; i < 8; ++i)
    perm2[i] = i;
  ok = expand_vselect (d->target, d->op0, perm2, 8, d->testing_p);
  gcc_assert (ok);

  /* Emit the pshufhw.  */
  memcpy (perm2 + 4, d->perm + 4, 4);
  for (i = 0; i < 4; ++i)
    perm2[i] = i;
  ok = expand_vselect (d->target, d->target, perm2, 8, d->testing_p);
  gcc_assert (ok);

  return true;
}

/* A subroutine of ix86_expand_vec_perm_const_1.  Try to simplify
   the permutation using the SSSE3 palignr instruction.  This succeeds
   when all of the elements in PERM fit within one vector and we merely
   need to shift them down so that a single vector permutation has a
   chance to succeed.  If SINGLE_INSN_ONLY_P, succeed if only
   the vpalignr instruction itself can perform the requested permutation.  */

static bool
expand_vec_perm_palignr (struct expand_vec_perm_d *d, bool single_insn_only_p)
{
  unsigned i, nelt = d->nelt;
  unsigned min, max, minswap, maxswap;
  bool in_order, ok, swap = false;
  rtx shift, target;
  struct expand_vec_perm_d dcopy;

  /* Even with AVX, palignr only operates on 128-bit vectors,
     in AVX2 palignr operates on both 128-bit lanes.  */
  if ((!TARGET_SSSE3 || GET_MODE_SIZE (d->vmode) != 16)
      && (!TARGET_AVX2 || GET_MODE_SIZE (d->vmode) != 32))
    return false;

  min = 2 * nelt;
  max = 0;
  minswap = 2 * nelt;
  maxswap = 0;
  for (i = 0; i < nelt; ++i)
    {
      unsigned e = d->perm[i];
      unsigned eswap = d->perm[i] ^ nelt;
      if (GET_MODE_SIZE (d->vmode) == 32)
	{
	  e = (e & ((nelt / 2) - 1)) | ((e & nelt) >> 1);
	  eswap = e ^ (nelt / 2);
	}
      if (e < min)
	min = e;
      if (e > max)
	max = e;
      if (eswap < minswap)
	minswap = eswap;
      if (eswap > maxswap)
	maxswap = eswap;
    }
  if (min == 0
      || max - min >= (GET_MODE_SIZE (d->vmode) == 32 ? nelt / 2 : nelt))
    {
      if (d->one_operand_p
	  || minswap == 0
	  || maxswap - minswap >= (GET_MODE_SIZE (d->vmode) == 32
				   ? nelt / 2 : nelt))
	return false;
      swap = true;
      min = minswap;
      max = maxswap;
    }

  /* Given that we have SSSE3, we know we'll be able to implement the
     single operand permutation after the palignr with pshufb for
     128-bit vectors.  If SINGLE_INSN_ONLY_P, in_order has to be computed
     first.  */
  if (d->testing_p && GET_MODE_SIZE (d->vmode) == 16 && !single_insn_only_p)
    return true;

  dcopy = *d;
  if (swap)
    {
      dcopy.op0 = d->op1;
      dcopy.op1 = d->op0;
      for (i = 0; i < nelt; ++i)
	dcopy.perm[i] ^= nelt;
    }

  in_order = true;
  for (i = 0; i < nelt; ++i)
    {
      unsigned e = dcopy.perm[i];
      if (GET_MODE_SIZE (d->vmode) == 32
	  && e >= nelt
	  && (e & (nelt / 2 - 1)) < min)
	e = e - min - (nelt / 2);
      else
	e = e - min;
      if (e != i)
	in_order = false;
      dcopy.perm[i] = e;
    }
  dcopy.one_operand_p = true;

  if (single_insn_only_p && !in_order)
    return false;

  /* For AVX2, test whether we can permute the result in one instruction.  */
  if (d->testing_p)
    {
      if (in_order)
	return true;
      dcopy.op1 = dcopy.op0;
      return expand_vec_perm_1 (&dcopy);
    }

  shift = GEN_INT (min * GET_MODE_UNIT_BITSIZE (d->vmode));
  if (GET_MODE_SIZE (d->vmode) == 16)
    {
      target = gen_reg_rtx (V1TImode);
      emit_insn (gen_ssse3_palignrv1ti (target,
					gen_lowpart (V1TImode, dcopy.op1),
					gen_lowpart (V1TImode, dcopy.op0),
					shift));
    }
  else
    {
      target = gen_reg_rtx (V2TImode);
      emit_insn (gen_avx2_palignrv2ti (target,
				       gen_lowpart (V2TImode, dcopy.op1),
				       gen_lowpart (V2TImode, dcopy.op0),
				       shift));
    }

  dcopy.op0 = dcopy.op1 = gen_lowpart (d->vmode, target);

  /* Test for the degenerate case where the alignment by itself
     produces the desired permutation.  */
  if (in_order)
    {
      emit_move_insn (d->target, dcopy.op0);
      return true;
    }

  ok = expand_vec_perm_1 (&dcopy);
  gcc_assert (ok || GET_MODE_SIZE (d->vmode) == 32);

  return ok;
}

/* A subroutine of ix86_expand_vec_perm_const_1.  Try to simplify
   the permutation using the SSE4_1 pblendv instruction.  Potentially
   reduces permutation from 2 pshufb and or to 1 pshufb and pblendv.  */

static bool
expand_vec_perm_pblendv (struct expand_vec_perm_d *d)
{
  unsigned i, which, nelt = d->nelt;
  struct expand_vec_perm_d dcopy, dcopy1;
  machine_mode vmode = d->vmode;
  bool ok;

  /* Use the same checks as in expand_vec_perm_blend.  */
  if (d->one_operand_p)
    return false;
  if (TARGET_AVX2 && GET_MODE_SIZE (vmode) == 32)
    ;
  else if (TARGET_AVX && (vmode == V4DFmode || vmode == V8SFmode))
    ;
  else if (TARGET_SSE4_1
	   && (GET_MODE_SIZE (vmode) == 16
	       || (TARGET_MMX_WITH_SSE && GET_MODE_SIZE (vmode) == 8)
	       || GET_MODE_SIZE (vmode) == 4))
    ;
  else
    return false;

  /* Figure out where permutation elements stay not in their
     respective lanes.  */
  for (i = 0, which = 0; i < nelt; ++i)
    {
      unsigned e = d->perm[i];
      if (e != i)
	which |= (e < nelt ? 1 : 2);
    }
  /* We can pblend the part where elements stay not in their
     respective lanes only when these elements are all in one
     half of a permutation.
     {0 1 8 3 4 5 9 7} is ok as 8, 9 are at not at their respective
     lanes, but both 8 and 9 >= 8
     {0 1 8 3 4 5 2 7} is not ok as 2 and 8 are not at their
     respective lanes and 8 >= 8, but 2 not.  */
  if (which != 1 && which != 2)
    return false;
  if (d->testing_p && GET_MODE_SIZE (vmode) == 16)
    return true;

  /* First we apply one operand permutation to the part where
     elements stay not in their respective lanes.  */
  dcopy = *d;
  if (which == 2)
    dcopy.op0 = dcopy.op1 = d->op1;
  else
    dcopy.op0 = dcopy.op1 = d->op0;
  if (!d->testing_p)
    dcopy.target = gen_reg_rtx (vmode);
  dcopy.one_operand_p = true;

  for (i = 0; i < nelt; ++i)
    dcopy.perm[i] = d->perm[i] & (nelt - 1);

  ok = expand_vec_perm_1 (&dcopy);
  if (GET_MODE_SIZE (vmode) != 16 && !ok)
    return false;
  else
    gcc_assert (ok);
  if (d->testing_p)
    return true;

  /* Next we put permuted elements into their positions.  */
  dcopy1 = *d;
  if (which == 2)
    dcopy1.op1 = dcopy.target;
  else
    dcopy1.op0 = dcopy.target;

  for (i = 0; i < nelt; ++i)
    dcopy1.perm[i] = ((d->perm[i] >= nelt) ? (nelt + i) : i);

  ok = expand_vec_perm_blend (&dcopy1);
  gcc_assert (ok);

  return true;
}

static bool expand_vec_perm_interleave3 (struct expand_vec_perm_d *d);

/* A subroutine of ix86_expand_vec_perm_const_1.  Try to simplify
   a two vector permutation into a single vector permutation by using
   an interleave operation to merge the vectors.  */

static bool
expand_vec_perm_interleave2 (struct expand_vec_perm_d *d)
{
  struct expand_vec_perm_d dremap, dfinal;
  unsigned i, nelt = d->nelt, nelt2 = nelt / 2;
  unsigned HOST_WIDE_INT contents;
  unsigned char remap[2 * MAX_VECT_LEN];
  rtx_insn *seq;
  bool ok, same_halves = false;

  if (GET_MODE_SIZE (d->vmode) == 4
      || GET_MODE_SIZE (d->vmode) == 8
      || GET_MODE_SIZE (d->vmode) == 16)
    {
      if (d->one_operand_p)
	return false;
    }
  else if (GET_MODE_SIZE (d->vmode) == 32)
    {
      if (!TARGET_AVX)
	return false;
      /* For 32-byte modes allow even d->one_operand_p.
	 The lack of cross-lane shuffling in some instructions
	 might prevent a single insn shuffle.  */
      dfinal = *d;
      dfinal.testing_p = true;
      /* If expand_vec_perm_interleave3 can expand this into
	 a 3 insn sequence, give up and let it be expanded as
	 3 insn sequence.  While that is one insn longer,
	 it doesn't need a memory operand and in the common
	 case that both interleave low and high permutations
	 with the same operands are adjacent needs 4 insns
	 for both after CSE.  */
      if (expand_vec_perm_interleave3 (&dfinal))
	return false;
    }
  else
    return false;

  /* Examine from whence the elements come.  */
  contents = 0;
  for (i = 0; i < nelt; ++i)
    contents |= HOST_WIDE_INT_1U << d->perm[i];

  memset (remap, 0xff, sizeof (remap));
  dremap = *d;

  if (GET_MODE_SIZE (d->vmode) == 4
      || GET_MODE_SIZE (d->vmode) == 8)
    {
      unsigned HOST_WIDE_INT h1, h2, h3, h4;

      /* Split the two input vectors into 4 halves.  */
      h1 = (HOST_WIDE_INT_1U << nelt2) - 1;
      h2 = h1 << nelt2;
      h3 = h2 << nelt2;
      h4 = h3 << nelt2;

      /* If the elements from the low halves use interleave low,
	 and similarly for interleave high.  */
      if ((contents & (h1 | h3)) == contents)
	{
	  /* punpckl* */
	  for (i = 0; i < nelt2; ++i)
	    {
	      remap[i] = i * 2;
	      remap[i + nelt] = i * 2 + 1;
	      dremap.perm[i * 2] = i;
	      dremap.perm[i * 2 + 1] = i + nelt;
	    }
	}
      else if ((contents & (h2 | h4)) == contents)
	{
	  /* punpckh* */
	  for (i = 0; i < nelt2; ++i)
	    {
	      remap[i + nelt2] = i * 2;
	      remap[i + nelt + nelt2] = i * 2 + 1;
	      dremap.perm[i * 2] = i + nelt2;
	      dremap.perm[i * 2 + 1] = i + nelt + nelt2;
	    }
	}
      else
	return false;
    }
  else if (GET_MODE_SIZE (d->vmode) == 16)
    {
      unsigned HOST_WIDE_INT h1, h2, h3, h4;

      /* Split the two input vectors into 4 halves.  */
      h1 = (HOST_WIDE_INT_1U << nelt2) - 1;
      h2 = h1 << nelt2;
      h3 = h2 << nelt2;
      h4 = h3 << nelt2;

      /* If the elements from the low halves use interleave low, and similarly
	 for interleave high.  If the elements are from mis-matched halves, we
	 can use shufps for V4SF/V4SI or do a DImode shuffle.  */
      if ((contents & (h1 | h3)) == contents)
	{
	  /* punpckl* */
	  for (i = 0; i < nelt2; ++i)
	    {
	      remap[i] = i * 2;
	      remap[i + nelt] = i * 2 + 1;
	      dremap.perm[i * 2] = i;
	      dremap.perm[i * 2 + 1] = i + nelt;
	    }
	  if (!TARGET_SSE2 && d->vmode == V4SImode)
	    dremap.vmode = V4SFmode;
	}
      else if ((contents & (h2 | h4)) == contents)
	{
	  /* punpckh* */
	  for (i = 0; i < nelt2; ++i)
	    {
	      remap[i + nelt2] = i * 2;
	      remap[i + nelt + nelt2] = i * 2 + 1;
	      dremap.perm[i * 2] = i + nelt2;
	      dremap.perm[i * 2 + 1] = i + nelt + nelt2;
	    }
	  if (!TARGET_SSE2 && d->vmode == V4SImode)
	    dremap.vmode = V4SFmode;
	}
      else if ((contents & (h1 | h4)) == contents)
	{
	  /* shufps */
	  for (i = 0; i < nelt2; ++i)
	    {
	      remap[i] = i;
	      remap[i + nelt + nelt2] = i + nelt2;
	      dremap.perm[i] = i;
	      dremap.perm[i + nelt2] = i + nelt + nelt2;
	    }
	  if (nelt != 4)
	    {
	      /* shufpd */
	      dremap.vmode = V2DImode;
	      dremap.nelt = 2;
	      dremap.perm[0] = 0;
	      dremap.perm[1] = 3;
	    }
	}
      else if ((contents & (h2 | h3)) == contents)
	{
	  /* shufps */
	  for (i = 0; i < nelt2; ++i)
	    {
	      remap[i + nelt2] = i;
	      remap[i + nelt] = i + nelt2;
	      dremap.perm[i] = i + nelt2;
	      dremap.perm[i + nelt2] = i + nelt;
	    }
	  if (nelt != 4)
	    {
	      /* shufpd */
	      dremap.vmode = V2DImode;
	      dremap.nelt = 2;
	      dremap.perm[0] = 1;
	      dremap.perm[1] = 2;
	    }
	}
      else
	return false;
    }
  else
    {
      unsigned int nelt4 = nelt / 4, nzcnt = 0;
      unsigned HOST_WIDE_INT q[8];
      unsigned int nonzero_halves[4];

      /* Split the two input vectors into 8 quarters.  */
      q[0] = (HOST_WIDE_INT_1U << nelt4) - 1;
      for (i = 1; i < 8; ++i)
	q[i] = q[0] << (nelt4 * i);
      for (i = 0; i < 4; ++i)
	if (((q[2 * i] | q[2 * i + 1]) & contents) != 0)
	  {
	    nonzero_halves[nzcnt] = i;
	    ++nzcnt;
	  }

      if (nzcnt == 1)
	{
	  gcc_assert (d->one_operand_p);
	  nonzero_halves[1] = nonzero_halves[0];
	  same_halves = true;
	}
      else if (d->one_operand_p)
	{
	  gcc_assert (nonzero_halves[0] == 0);
	  gcc_assert (nonzero_halves[1] == 1);
	}

      if (nzcnt <= 2)
	{
	  if (d->perm[0] / nelt2 == nonzero_halves[1])
	    {
	      /* Attempt to increase the likelihood that dfinal
		 shuffle will be intra-lane.  */
	      std::swap (nonzero_halves[0], nonzero_halves[1]);
	    }

	  /* vperm2f128 or vperm2i128.  */
	  for (i = 0; i < nelt2; ++i)
	    {
	      remap[i + nonzero_halves[1] * nelt2] = i + nelt2;
	      remap[i + nonzero_halves[0] * nelt2] = i;
	      dremap.perm[i + nelt2] = i + nonzero_halves[1] * nelt2;
	      dremap.perm[i] = i + nonzero_halves[0] * nelt2;
	    }

	  if (d->vmode != V8SFmode
	      && d->vmode != V4DFmode
	      && d->vmode != V8SImode)
	    {
	      dremap.vmode = V8SImode;
	      dremap.nelt = 8;
	      for (i = 0; i < 4; ++i)
		{
		  dremap.perm[i] = i + nonzero_halves[0] * 4;
		  dremap.perm[i + 4] = i + nonzero_halves[1] * 4;
		}
	    }
	}
      else if (d->one_operand_p)
	return false;
      else if (TARGET_AVX2
	       && (contents & (q[0] | q[2] | q[4] | q[6])) == contents)
	{
	  /* vpunpckl* */
	  for (i = 0; i < nelt4; ++i)
	    {
	      remap[i] = i * 2;
	      remap[i + nelt] = i * 2 + 1;
	      remap[i + nelt2] = i * 2 + nelt2;
	      remap[i + nelt + nelt2] = i * 2 + nelt2 + 1;
	      dremap.perm[i * 2] = i;
	      dremap.perm[i * 2 + 1] = i + nelt;
	      dremap.perm[i * 2 + nelt2] = i + nelt2;
	      dremap.perm[i * 2 + nelt2 + 1] = i + nelt + nelt2;
	    }
	}
      else if (TARGET_AVX2
	       && (contents & (q[1] | q[3] | q[5] | q[7])) == contents)
	{
	  /* vpunpckh* */
	  for (i = 0; i < nelt4; ++i)
	    {
	      remap[i + nelt4] = i * 2;
	      remap[i + nelt + nelt4] = i * 2 + 1;
	      remap[i + nelt2 + nelt4] = i * 2 + nelt2;
	      remap[i + nelt + nelt2 + nelt4] = i * 2 + nelt2 + 1;
	      dremap.perm[i * 2] = i + nelt4;
	      dremap.perm[i * 2 + 1] = i + nelt + nelt4;
	      dremap.perm[i * 2 + nelt2] = i + nelt2 + nelt4;
	      dremap.perm[i * 2 + nelt2 + 1] = i + nelt + nelt2 + nelt4;
	    }
	}
      else
	return false;
    }

  /* Use the remapping array set up above to move the elements from their
     swizzled locations into their final destinations.  */
  dfinal = *d;
  for (i = 0; i < nelt; ++i)
    {
      unsigned e = remap[d->perm[i]];
      gcc_assert (e < nelt);
      /* If same_halves is true, both halves of the remapped vector are the
	 same.  Avoid cross-lane accesses if possible.  */
      if (same_halves && i >= nelt2)
	{
	  gcc_assert (e < nelt2);
	  dfinal.perm[i] = e + nelt2;
	}
      else
	dfinal.perm[i] = e;
    }
  if (!d->testing_p)
    {
      dremap.target = gen_reg_rtx (dremap.vmode);
      dfinal.op0 = gen_lowpart (dfinal.vmode, dremap.target);
    }
  dfinal.op1 = dfinal.op0;
  dfinal.one_operand_p = true;

  /* Test if the final remap can be done with a single insn.  For V4SFmode or
     V4SImode this *will* succeed.  For V8HImode or V16QImode it may not.  */
  start_sequence ();
  ok = expand_vec_perm_1 (&dfinal);
  seq = get_insns ();
  end_sequence ();

  if (!ok)
    return false;

  if (d->testing_p)
    return true;

  if (dremap.vmode != dfinal.vmode)
    {
      dremap.op0 = gen_lowpart (dremap.vmode, dremap.op0);
      dremap.op1 = gen_lowpart (dremap.vmode, dremap.op1);
    }

  ok = expand_vec_perm_1 (&dremap);
  gcc_assert (ok);

  emit_insn (seq);
  return true;
}

/* A subroutine of ix86_expand_vec_perm_const_1.  Try to simplify
   a single vector cross-lane permutation into vpermq followed
   by any of the single insn permutations.  */

static bool
expand_vec_perm_vpermq_perm_1 (struct expand_vec_perm_d *d)
{
  struct expand_vec_perm_d dremap, dfinal;
  unsigned i, j, nelt = d->nelt, nelt2 = nelt / 2, nelt4 = nelt / 4;
  unsigned contents[2];
  bool ok;

  if (!(TARGET_AVX2
	&& (d->vmode == V32QImode || d->vmode == V16HImode)
	&& d->one_operand_p))
    return false;

  contents[0] = 0;
  contents[1] = 0;
  for (i = 0; i < nelt2; ++i)
    {
      contents[0] |= 1u << (d->perm[i] / nelt4);
      contents[1] |= 1u << (d->perm[i + nelt2] / nelt4);
    }

  for (i = 0; i < 2; ++i)
    {
      unsigned int cnt = 0;
      for (j = 0; j < 4; ++j)
	if ((contents[i] & (1u << j)) != 0 && ++cnt > 2)
	  return false;
    }

  if (d->testing_p)
    return true;

  dremap = *d;
  dremap.vmode = V4DImode;
  dremap.nelt = 4;
  dremap.target = gen_reg_rtx (V4DImode);
  dremap.op0 = gen_lowpart (V4DImode, d->op0);
  dremap.op1 = dremap.op0;
  dremap.one_operand_p = true;
  for (i = 0; i < 2; ++i)
    {
      unsigned int cnt = 0;
      for (j = 0; j < 4; ++j)
	if ((contents[i] & (1u << j)) != 0)
	  dremap.perm[2 * i + cnt++] = j;
      for (; cnt < 2; ++cnt)
	dremap.perm[2 * i + cnt] = 0;
    }

  dfinal = *d;
  dfinal.op0 = gen_lowpart (dfinal.vmode, dremap.target);
  dfinal.op1 = dfinal.op0;
  dfinal.one_operand_p = true;
  for (i = 0, j = 0; i < nelt; ++i)
    {
      if (i == nelt2)
	j = 2;
      dfinal.perm[i] = (d->perm[i] & (nelt4 - 1)) | (j ? nelt2 : 0);
      if ((d->perm[i] / nelt4) == dremap.perm[j])
	;
      else if ((d->perm[i] / nelt4) == dremap.perm[j + 1])
	dfinal.perm[i] |= nelt4;
      else
	gcc_unreachable ();
    }

  ok = expand_vec_perm_1 (&dremap);
  gcc_assert (ok);

  ok = expand_vec_perm_1 (&dfinal);
  gcc_assert (ok);

  return true;
}

static bool canonicalize_perm (struct expand_vec_perm_d *d);

/* A subroutine of ix86_expand_vec_perm_const_1.  Try to expand
   a vector permutation using two instructions, vperm2f128 resp.
   vperm2i128 followed by any single in-lane permutation.  */

static bool
expand_vec_perm_vperm2f128 (struct expand_vec_perm_d *d)
{
  struct expand_vec_perm_d dfirst, dsecond;
  unsigned i, j, nelt = d->nelt, nelt2 = nelt / 2, perm;
  bool ok;

  if (!TARGET_AVX
      || GET_MODE_SIZE (d->vmode) != 32
      || (d->vmode != V8SFmode && d->vmode != V4DFmode && !TARGET_AVX2))
    return false;

  dsecond = *d;
  dsecond.one_operand_p = false;
  dsecond.testing_p = true;

  /* ((perm << 2)|perm) & 0x33 is the vperm2[fi]128
     immediate.  For perm < 16 the second permutation uses
     d->op0 as first operand, for perm >= 16 it uses d->op1
     as first operand.  The second operand is the result of
     vperm2[fi]128.  */
  for (perm = 0; perm < 32; perm++)
    {
      /* Ignore permutations which do not move anything cross-lane.  */
      if (perm < 16)
	{
	  /* The second shuffle for e.g. V4DFmode has
	     0123 and ABCD operands.
	     Ignore AB23, as 23 is already in the second lane
	     of the first operand.  */
	  if ((perm & 0xc) == (1 << 2)) continue;
	  /* And 01CD, as 01 is in the first lane of the first
	     operand.  */
	  if ((perm & 3) == 0) continue;
	  /* And 4567, as then the vperm2[fi]128 doesn't change
	     anything on the original 4567 second operand.  */
	  if ((perm & 0xf) == ((3 << 2) | 2)) continue;
	}
      else
	{
	  /* The second shuffle for e.g. V4DFmode has
	     4567 and ABCD operands.
	     Ignore AB67, as 67 is already in the second lane
	     of the first operand.  */
	  if ((perm & 0xc) == (3 << 2)) continue;
	  /* And 45CD, as 45 is in the first lane of the first
	     operand.  */
	  if ((perm & 3) == 2) continue;
	  /* And 0123, as then the vperm2[fi]128 doesn't change
	     anything on the original 0123 first operand.  */
	  if ((perm & 0xf) == (1 << 2)) continue;
	}

      for (i = 0; i < nelt; i++)
	{
	  j = d->perm[i] / nelt2;
	  if (j == ((perm >> (2 * (i >= nelt2))) & 3))
	    dsecond.perm[i] = nelt + (i & nelt2) + (d->perm[i] & (nelt2 - 1));
	  else if (j == (unsigned) (i >= nelt2) + 2 * (perm >= 16))
	    dsecond.perm[i] = d->perm[i] & (nelt - 1);
	  else
	    break;
	}

      if (i == nelt)
	{
	  start_sequence ();
	  ok = expand_vec_perm_1 (&dsecond);
	  end_sequence ();
	}
      else
	ok = false;

      if (ok)
	{
	  if (d->testing_p)
	    return true;

	  /* Found a usable second shuffle.  dfirst will be
	     vperm2f128 on d->op0 and d->op1.  */
	  dsecond.testing_p = false;
	  dfirst = *d;
	  dfirst.target = gen_reg_rtx (d->vmode);
	  for (i = 0; i < nelt; i++)
	    dfirst.perm[i] = (i & (nelt2 - 1))
			     + ((perm >> (2 * (i >= nelt2))) & 3) * nelt2;

	  canonicalize_perm (&dfirst);
	  ok = expand_vec_perm_1 (&dfirst);
	  gcc_assert (ok);

	  /* And dsecond is some single insn shuffle, taking
	     d->op0 and result of vperm2f128 (if perm < 16) or
	     d->op1 and result of vperm2f128 (otherwise).  */
	  if (perm >= 16)
	    dsecond.op0 = dsecond.op1;
	  dsecond.op1 = dfirst.target;

	  ok = expand_vec_perm_1 (&dsecond);
	  gcc_assert (ok);

	  return true;
	}

      /* For one operand, the only useful vperm2f128 permutation is 0x01
	 aka lanes swap.  */
      if (d->one_operand_p)
	return false;
    }

  return false;
}

/* A subroutine of ix86_expand_vec_perm_const_1.  Try to simplify
   a two vector permutation using 2 intra-lane interleave insns
   and cross-lane shuffle for 32-byte vectors.  */

static bool
expand_vec_perm_interleave3 (struct expand_vec_perm_d *d)
{
  unsigned i, nelt;
  rtx (*gen) (rtx, rtx, rtx);

  if (d->one_operand_p)
    return false;
  if (TARGET_AVX2 && GET_MODE_SIZE (d->vmode) == 32)
    ;
  else if (TARGET_AVX && (d->vmode == V8SFmode || d->vmode == V4DFmode))
    ;
  else
    return false;

  nelt = d->nelt;
  if (d->perm[0] != 0 && d->perm[0] != nelt / 2)
    return false;
  for (i = 0; i < nelt; i += 2)
    if (d->perm[i] != d->perm[0] + i / 2
	|| d->perm[i + 1] != d->perm[0] + i / 2 + nelt)
      return false;

  if (d->testing_p)
    return true;

  switch (d->vmode)
    {
    case E_V32QImode:
      if (d->perm[0])
	gen = gen_vec_interleave_highv32qi;
      else
	gen = gen_vec_interleave_lowv32qi;
      break;
    case E_V16HImode:
      if (d->perm[0])
	gen = gen_vec_interleave_highv16hi;
      else
	gen = gen_vec_interleave_lowv16hi;
      break;
    case E_V8SImode:
      if (d->perm[0])
	gen = gen_vec_interleave_highv8si;
      else
	gen = gen_vec_interleave_lowv8si;
      break;
    case E_V4DImode:
      if (d->perm[0])
	gen = gen_vec_interleave_highv4di;
      else
	gen = gen_vec_interleave_lowv4di;
      break;
    case E_V8SFmode:
      if (d->perm[0])
	gen = gen_vec_interleave_highv8sf;
      else
	gen = gen_vec_interleave_lowv8sf;
      break;
    case E_V4DFmode:
      if (d->perm[0])
	gen = gen_vec_interleave_highv4df;
      else
	gen = gen_vec_interleave_lowv4df;
      break;
    default:
      gcc_unreachable ();
    }

  emit_insn (gen (d->target, d->op0, d->op1));
  return true;
}

/* A subroutine of ix86_expand_vec_perm_const_1.  Try to implement
   a single vector permutation using a single intra-lane vector
   permutation, vperm2f128 swapping the lanes and vblend* insn blending
   the non-swapped and swapped vectors together.  */

static bool
expand_vec_perm_vperm2f128_vblend (struct expand_vec_perm_d *d)
{
  struct expand_vec_perm_d dfirst, dsecond;
  unsigned i, j, msk, nelt = d->nelt, nelt2 = nelt / 2;
  rtx_insn *seq;
  bool ok;
  rtx (*blend) (rtx, rtx, rtx, rtx) = NULL;

  if (!TARGET_AVX
      || TARGET_AVX2
      || (d->vmode != V8SFmode && d->vmode != V4DFmode)
      || !d->one_operand_p)
    return false;

  dfirst = *d;
  for (i = 0; i < nelt; i++)
    dfirst.perm[i] = 0xff;
  for (i = 0, msk = 0; i < nelt; i++)
    {
      j = (d->perm[i] & nelt2) ? i | nelt2 : i & ~nelt2;
      if (dfirst.perm[j] != 0xff && dfirst.perm[j] != d->perm[i])
	return false;
      dfirst.perm[j] = d->perm[i];
      if (j != i)
	msk |= (1 << i);
    }
  for (i = 0; i < nelt; i++)
    if (dfirst.perm[i] == 0xff)
      dfirst.perm[i] = i;

  if (!d->testing_p)
    dfirst.target = gen_reg_rtx (dfirst.vmode);

  start_sequence ();
  ok = expand_vec_perm_1 (&dfirst);
  seq = get_insns ();
  end_sequence ();

  if (!ok)
    return false;

  if (d->testing_p)
    return true;

  emit_insn (seq);

  dsecond = *d;
  dsecond.op0 = dfirst.target;
  dsecond.op1 = dfirst.target;
  dsecond.one_operand_p = true;
  dsecond.target = gen_reg_rtx (dsecond.vmode);
  for (i = 0; i < nelt; i++)
    dsecond.perm[i] = i ^ nelt2;

  ok = expand_vec_perm_1 (&dsecond);
  gcc_assert (ok);

  blend = d->vmode == V8SFmode ? gen_avx_blendps256 : gen_avx_blendpd256;
  emit_insn (blend (d->target, dfirst.target, dsecond.target, GEN_INT (msk)));
  return true;
}

/* A subroutine of ix86_expand_vec_perm_const_1.  Try to implement
   a two vector permutation using two single vector permutations and
   {,v}{,p}unpckl{ps,pd,bw,wd,dq}.  If two_insn, succeed only if one
   of dfirst or dsecond is identity permutation.  */

static bool
expand_vec_perm_2perm_interleave (struct expand_vec_perm_d *d, bool two_insn)
{
  unsigned i, nelt = d->nelt, nelt2 = nelt / 2, lane = nelt;
  struct expand_vec_perm_d dfirst, dsecond, dfinal;
  bool ident1 = true, ident2 = true;

  if (d->one_operand_p)
    return false;

  if (GET_MODE_SIZE (d->vmode) == 16)
    {
      if (!TARGET_SSE)
	return false;
      if (d->vmode != V4SFmode && d->vmode != V2DFmode && !TARGET_SSE2)
	return false;
    }
  else if (GET_MODE_SIZE (d->vmode) == 32)
    {
      if (!TARGET_AVX)
	return false;
      if (d->vmode != V8SFmode && d->vmode != V4DFmode && !TARGET_AVX2)
	return false;
      lane = nelt2;
    }
  else
    return false;

  for (i = 1; i < nelt; i++)
    if ((d->perm[i] >= nelt) != ((d->perm[0] >= nelt) ^ (i & 1)))
      return false;

  dfirst = *d;
  dsecond = *d;
  dfinal = *d;
  dfirst.op1 = dfirst.op0;
  dfirst.one_operand_p = true;
  dsecond.op0 = dsecond.op1;
  dsecond.one_operand_p = true;

  for (i = 0; i < nelt; i++)
    if (d->perm[i] >= nelt)
      {
	dsecond.perm[i / 2 + (i >= lane ? lane / 2 : 0)] = d->perm[i] - nelt;
	if (d->perm[i] - nelt != i / 2 + (i >= lane ? lane / 2 : 0))
	  ident2 = false;
	dsecond.perm[i / 2 + (i >= lane ? lane : lane / 2)]
	  = d->perm[i] - nelt;
      }
    else
      {
	dfirst.perm[i / 2 + (i >= lane ? lane / 2 : 0)] = d->perm[i];
	if (d->perm[i] != i / 2 + (i >= lane ? lane / 2 : 0))
	  ident1 = false;
	dfirst.perm[i / 2 + (i >= lane ? lane : lane / 2)] = d->perm[i];
      }

  if (two_insn && !ident1 && !ident2)
    return false;

  if (!d->testing_p)
    {
      if (!ident1)
	dfinal.op0 = dfirst.target = gen_reg_rtx (d->vmode);
      if (!ident2)
	dfinal.op1 = dsecond.target = gen_reg_rtx (d->vmode);
      if (d->perm[0] >= nelt)
	std::swap (dfinal.op0, dfinal.op1);
    }

  bool ok;
  rtx_insn *seq1 = NULL, *seq2 = NULL;

  if (!ident1)
    {
      start_sequence ();
      ok = expand_vec_perm_1 (&dfirst);
      seq1 = get_insns ();
      end_sequence ();

      if (!ok)
	return false;
    }

  if (!ident2)
    {
      start_sequence ();
      ok = expand_vec_perm_1 (&dsecond);
      seq2 = get_insns ();
      end_sequence ();

      if (!ok)
	return false;
    }

  if (d->testing_p)
    return true;

  for (i = 0; i < nelt; i++)
    {
      dfinal.perm[i] = i / 2;
      if (i >= lane)
	dfinal.perm[i] += lane / 2;
      if ((i & 1) != 0)
	dfinal.perm[i] += nelt;
    }
  emit_insn (seq1);
  emit_insn (seq2);
  ok = expand_vselect_vconcat (dfinal.target, dfinal.op0, dfinal.op1,
			       dfinal.perm, dfinal.nelt, false);
  gcc_assert (ok);
  return true;
}

/* A subroutine of ix86_expand_vec_perm_const_1.  Try to simplify
   the permutation using two single vector permutations and the SSE4_1 pblendv
   instruction.  If two_insn, succeed only if one of dfirst or dsecond is
   identity permutation.  */

static bool
expand_vec_perm_2perm_pblendv (struct expand_vec_perm_d *d, bool two_insn)
{
  unsigned i, nelt = d->nelt;
  struct expand_vec_perm_d dfirst, dsecond, dfinal;
  machine_mode vmode = d->vmode;
  bool ident1 = true, ident2 = true;

  /* Use the same checks as in expand_vec_perm_blend.  */
  if (d->one_operand_p)
    return false;
  if (TARGET_AVX2 && GET_MODE_SIZE (vmode) == 32)
    ;
  else if (TARGET_AVX && (vmode == V4DFmode || vmode == V8SFmode))
    ;
  else if (TARGET_SSE4_1
	   && (GET_MODE_SIZE (vmode) == 16
	       || (TARGET_MMX_WITH_SSE && GET_MODE_SIZE (vmode) == 8)
	       || GET_MODE_SIZE (vmode) == 4))
    ;
  else
    return false;

  dfirst = *d;
  dsecond = *d;
  dfinal = *d;
  dfirst.op1 = dfirst.op0;
  dfirst.one_operand_p = true;
  dsecond.op0 = dsecond.op1;
  dsecond.one_operand_p = true;

  for (i = 0; i < nelt; ++i)
    if (d->perm[i] >= nelt)
      {
	dfirst.perm[i] = 0xff;
	dsecond.perm[i] = d->perm[i] - nelt;
	if (d->perm[i] != i + nelt)
	  ident2 = false;
      }
    else
      {
	dsecond.perm[i] = 0xff;
	dfirst.perm[i] = d->perm[i];
	if (d->perm[i] != i)
	  ident1 = false;
      }

  if (two_insn && !ident1 && !ident2)
    return false;

  /* For now.  Ideally treat 0xff as a wildcard.  */
  for (i = 0; i < nelt; ++i)
    if (dfirst.perm[i] == 0xff)
      {
	if (GET_MODE_SIZE (vmode) == 32
	    && dfirst.perm[i ^ (nelt / 2)] != 0xff)
	  dfirst.perm[i] = dfirst.perm[i ^ (nelt / 2)] ^ (nelt / 2);
	else
	  dfirst.perm[i] = i;
      }
    else
      {
	if (GET_MODE_SIZE (vmode) == 32
	    && dsecond.perm[i ^ (nelt / 2)] != 0xff)
	  dsecond.perm[i] = dsecond.perm[i ^ (nelt / 2)] ^ (nelt / 2);
	else
	  dsecond.perm[i] = i;
      }

  if (!d->testing_p)
    {
      if (!ident1)
	dfinal.op0 = dfirst.target = gen_reg_rtx (d->vmode);
      if (!ident2)
	dfinal.op1 = dsecond.target = gen_reg_rtx (d->vmode);
    }

  bool ok;
  rtx_insn *seq1 = NULL, *seq2 = NULL;

  if (!ident1)
    {
      start_sequence ();
      ok = expand_vec_perm_1 (&dfirst);
      seq1 = get_insns ();
      end_sequence ();

      if (!ok)
	return false;
    }

  if (!ident2)
    {
      start_sequence ();
      ok = expand_vec_perm_1 (&dsecond);
      seq2 = get_insns ();
      end_sequence ();

      if (!ok)
	return false;
    }

  if (d->testing_p)
    return true;

  for (i = 0; i < nelt; ++i)
    dfinal.perm[i] = (d->perm[i] >= nelt ? i + nelt : i);

  emit_insn (seq1);
  emit_insn (seq2);
  ok = expand_vec_perm_blend (&dfinal);
  gcc_assert (ok);
  return true;
}

/* A subroutine of ix86_expand_vec_perm_const_1.  Implement a V4DF
   permutation using two vperm2f128, followed by a vshufpd insn blending
   the two vectors together.  */

static bool
expand_vec_perm_2vperm2f128_vshuf (struct expand_vec_perm_d *d)
{
  struct expand_vec_perm_d dfirst, dsecond, dthird;
  bool ok;

  if (!TARGET_AVX || (d->vmode != V4DFmode))
    return false;

  if (d->testing_p)
    return true;

  dfirst = *d;
  dsecond = *d;
  dthird = *d;

  dfirst.perm[0] = (d->perm[0] & ~1);
  dfirst.perm[1] = (d->perm[0] & ~1) + 1;
  dfirst.perm[2] = (d->perm[2] & ~1);
  dfirst.perm[3] = (d->perm[2] & ~1) + 1;
  dsecond.perm[0] = (d->perm[1] & ~1);
  dsecond.perm[1] = (d->perm[1] & ~1) + 1;
  dsecond.perm[2] = (d->perm[3] & ~1);
  dsecond.perm[3] = (d->perm[3] & ~1) + 1;
  dthird.perm[0] = (d->perm[0] % 2);
  dthird.perm[1] = (d->perm[1] % 2) + 4;
  dthird.perm[2] = (d->perm[2] % 2) + 2;
  dthird.perm[3] = (d->perm[3] % 2) + 6;

  dfirst.target = gen_reg_rtx (dfirst.vmode);
  dsecond.target = gen_reg_rtx (dsecond.vmode);
  dthird.op0 = dfirst.target;
  dthird.op1 = dsecond.target;
  dthird.one_operand_p = false;

  canonicalize_perm (&dfirst);
  canonicalize_perm (&dsecond);

  ok = expand_vec_perm_1 (&dfirst)
       && expand_vec_perm_1 (&dsecond)
       && expand_vec_perm_1 (&dthird);

  gcc_assert (ok);

  return true;
}

static bool ix86_expand_vec_perm_const_1 (struct expand_vec_perm_d *);

/* A subroutine of ix86_expand_vec_perm_const_1.  Try to implement
   a two vector permutation using two intra-lane vector
   permutations, vperm2f128 swapping the lanes and vblend* insn blending
   the non-swapped and swapped vectors together.  */

static bool
expand_vec_perm2_vperm2f128_vblend (struct expand_vec_perm_d *d)
{
  struct expand_vec_perm_d dfirst, dsecond, dthird;
  unsigned i, j, msk, nelt = d->nelt, nelt2 = nelt / 2, which1 = 0, which2 = 0;
  rtx_insn *seq1, *seq2;
  bool ok;
  rtx (*blend) (rtx, rtx, rtx, rtx) = NULL;

  if (!TARGET_AVX
      || TARGET_AVX2
      || (d->vmode != V8SFmode && d->vmode != V4DFmode)
      || d->one_operand_p)
    return false;

  dfirst = *d;
  dsecond = *d;
  for (i = 0; i < nelt; i++)
    {
      dfirst.perm[i] = 0xff;
      dsecond.perm[i] = 0xff;
    }
  for (i = 0, msk = 0; i < nelt; i++)
    {
      j = (d->perm[i] & nelt2) ? i | nelt2 : i & ~nelt2;
      if (j == i)
	{
	  dfirst.perm[j] = d->perm[i];
	  which1 |= (d->perm[i] < nelt ? 1 : 2);
	}
      else
	{
	  dsecond.perm[j] = d->perm[i];
	  which2 |= (d->perm[i] < nelt ? 1 : 2);
	  msk |= (1U << i);
	}
    }
  if (msk == 0 || msk == (1U << nelt) - 1)
    return false;

  if (!d->testing_p)
    {
      dfirst.target = gen_reg_rtx (dfirst.vmode);
      dsecond.target = gen_reg_rtx (dsecond.vmode);
    }

  for (i = 0; i < nelt; i++)
    {
      if (dfirst.perm[i] == 0xff)
	dfirst.perm[i] = (which1 == 2 ? i + nelt : i);
      if (dsecond.perm[i] == 0xff)
	dsecond.perm[i] = (which2 == 2 ? i + nelt : i);
    }
  canonicalize_perm (&dfirst);
  start_sequence ();
  ok = ix86_expand_vec_perm_const_1 (&dfirst);
  seq1 = get_insns ();
  end_sequence ();

  if (!ok)
    return false;

  canonicalize_perm (&dsecond);
  start_sequence ();
  ok = ix86_expand_vec_perm_const_1 (&dsecond);
  seq2 = get_insns ();
  end_sequence ();

  if (!ok)
    return false;

  if (d->testing_p)
    return true;

  emit_insn (seq1);
  emit_insn (seq2);

  dthird = *d;
  dthird.op0 = dsecond.target;
  dthird.op1 = dsecond.target;
  dthird.one_operand_p = true;
  dthird.target = gen_reg_rtx (dthird.vmode);
  for (i = 0; i < nelt; i++)
    dthird.perm[i] = i ^ nelt2;

  ok = expand_vec_perm_1 (&dthird);
  gcc_assert (ok);

  blend = d->vmode == V8SFmode ? gen_avx_blendps256 : gen_avx_blendpd256;
  emit_insn (blend (d->target, dfirst.target, dthird.target, GEN_INT (msk)));
  return true;
}

/* A subroutine of expand_vec_perm_even_odd_1.  Implement the double-word
   permutation with two pshufb insns and an ior.  We should have already
   failed all two instruction sequences.  */

static bool
expand_vec_perm_pshufb2 (struct expand_vec_perm_d *d)
{
  rtx rperm[2][16], vperm, l, h, op, m128;
  unsigned int i, nelt, eltsz;
  machine_mode mode;
  rtx (*gen) (rtx, rtx, rtx);

  if (!TARGET_SSSE3 || (GET_MODE_SIZE (d->vmode) != 16
			&& GET_MODE_SIZE (d->vmode) != 8
			&& GET_MODE_SIZE (d->vmode) != 4))
    return false;
  gcc_assert (!d->one_operand_p);

  if (d->testing_p)
    return true;

  switch (GET_MODE_SIZE (d->vmode))
    {
    case 4:
      mode = V4QImode;
      gen = gen_mmx_pshufbv4qi3;
      break;
    case 8:
      mode = V8QImode;
      gen = gen_mmx_pshufbv8qi3;
      break;
    case 16:
      mode = V16QImode;
      gen = gen_ssse3_pshufbv16qi3;
      break;
    default:
      gcc_unreachable ();
    }

  nelt = d->nelt;
  eltsz = GET_MODE_UNIT_SIZE (d->vmode);

  /* Generate two permutation masks.  If the required element is within
     the given vector it is shuffled into the proper lane.  If the required
     element is in the other vector, force a zero into the lane by setting
     bit 7 in the permutation mask.  */
  m128 = GEN_INT (-128);
  for (i = 0; i < nelt; ++i)
    {
      unsigned j, k, e = d->perm[i];
      unsigned which = (e >= nelt);
      if (e >= nelt)
	e -= nelt;

      for (j = 0; j < eltsz; ++j)
	{
	  rperm[which][i*eltsz + j] = GEN_INT (e*eltsz + j);
	  rperm[1-which][i*eltsz + j] = m128;
	}

      for (k = i*eltsz + j; k < 16; ++k)
	rperm[0][k] = rperm[1][k] = m128;
    }

  vperm = gen_rtx_CONST_VECTOR (V16QImode, gen_rtvec_v (16, rperm[0]));
  vperm = force_reg (V16QImode, vperm);

  l = gen_reg_rtx (mode);
  op = gen_lowpart (mode, d->op0);
  emit_insn (gen (l, op, vperm));

  vperm = gen_rtx_CONST_VECTOR (V16QImode, gen_rtvec_v (16, rperm[1]));
  vperm = force_reg (V16QImode, vperm);

  h = gen_reg_rtx (mode);
  op = gen_lowpart (mode, d->op1);
  emit_insn (gen (h, op, vperm));

  op = d->target;
  if (d->vmode != mode)
    op = gen_reg_rtx (mode);
  ix86_emit_vec_binop (IOR, mode, op, l, h);
  if (op != d->target)
    emit_move_insn (d->target, gen_lowpart (d->vmode, op));

  return true;
}

/* Implement arbitrary permutation of one V32QImode and V16QImode operand
   with two vpshufb insns, vpermq and vpor.  We should have already failed
   all two or three instruction sequences.  */

static bool
expand_vec_perm_vpshufb2_vpermq (struct expand_vec_perm_d *d)
{
  rtx rperm[2][32], vperm, l, h, hp, op, m128;
  unsigned int i, nelt, eltsz;

  if (!TARGET_AVX2
      || !d->one_operand_p
      || (d->vmode != V32QImode && d->vmode != V16HImode))
    return false;

  if (d->testing_p)
    return true;

  nelt = d->nelt;
  eltsz = GET_MODE_UNIT_SIZE (d->vmode);

  /* Generate two permutation masks.  If the required element is within
     the same lane, it is shuffled in.  If the required element from the
     other lane, force a zero by setting bit 7 in the permutation mask.
     In the other mask the mask has non-negative elements if element
     is requested from the other lane, but also moved to the other lane,
     so that the result of vpshufb can have the two V2TImode halves
     swapped.  */
  m128 = GEN_INT (-128);
  for (i = 0; i < nelt; ++i)
    {
      unsigned j, e = d->perm[i] & (nelt / 2 - 1);
      unsigned which = ((d->perm[i] ^ i) & (nelt / 2)) * eltsz;

      for (j = 0; j < eltsz; ++j)
	{
	  rperm[!!which][(i * eltsz + j) ^ which] = GEN_INT (e * eltsz + j);
	  rperm[!which][(i * eltsz + j) ^ (which ^ 16)] = m128;
	}
    }

  vperm = gen_rtx_CONST_VECTOR (V32QImode, gen_rtvec_v (32, rperm[1]));
  vperm = force_reg (V32QImode, vperm);

  h = gen_reg_rtx (V32QImode);
  op = gen_lowpart (V32QImode, d->op0);
  emit_insn (gen_avx2_pshufbv32qi3 (h, op, vperm));

  /* Swap the 128-byte lanes of h into hp.  */
  hp = gen_reg_rtx (V4DImode);
  op = gen_lowpart (V4DImode, h);
  emit_insn (gen_avx2_permv4di_1 (hp, op, const2_rtx, GEN_INT (3), const0_rtx,
				  const1_rtx));

  vperm = gen_rtx_CONST_VECTOR (V32QImode, gen_rtvec_v (32, rperm[0]));
  vperm = force_reg (V32QImode, vperm);

  l = gen_reg_rtx (V32QImode);
  op = gen_lowpart (V32QImode, d->op0);
  emit_insn (gen_avx2_pshufbv32qi3 (l, op, vperm));

  op = d->target;
  if (d->vmode != V32QImode)
    op = gen_reg_rtx (V32QImode);
  emit_insn (gen_iorv32qi3 (op, l, gen_lowpart (V32QImode, hp)));
  if (op != d->target)
    emit_move_insn (d->target, gen_lowpart (d->vmode, op));

  return true;
}

/* A subroutine of expand_vec_perm_even_odd_1.  Implement extract-even
   and extract-odd permutations of two V32QImode and V16QImode operand
   with two vpshufb insns, vpor and vpermq.  We should have already
   failed all two or three instruction sequences.  */

static bool
expand_vec_perm_vpshufb2_vpermq_even_odd (struct expand_vec_perm_d *d)
{
  rtx rperm[2][32], vperm, l, h, ior, op, m128;
  unsigned int i, nelt, eltsz;

  if (!TARGET_AVX2
      || d->one_operand_p
      || (d->vmode != V32QImode && d->vmode != V16HImode))
    return false;

  for (i = 0; i < d->nelt; ++i)
    if ((d->perm[i] ^ (i * 2)) & (3 * d->nelt / 2))
      return false;

  if (d->testing_p)
    return true;

  nelt = d->nelt;
  eltsz = GET_MODE_UNIT_SIZE (d->vmode);

  /* Generate two permutation masks.  In the first permutation mask
     the first quarter will contain indexes for the first half
     of the op0, the second quarter will contain bit 7 set, third quarter
     will contain indexes for the second half of the op0 and the
     last quarter bit 7 set.  In the second permutation mask
     the first quarter will contain bit 7 set, the second quarter
     indexes for the first half of the op1, the third quarter bit 7 set
     and last quarter indexes for the second half of the op1.
     I.e. the first mask e.g. for V32QImode extract even will be:
     0, 2, ..., 0xe, -128, ..., -128, 0, 2, ..., 0xe, -128, ..., -128
     (all values masked with 0xf except for -128) and second mask
     for extract even will be
     -128, ..., -128, 0, 2, ..., 0xe, -128, ..., -128, 0, 2, ..., 0xe.  */
  m128 = GEN_INT (-128);
  for (i = 0; i < nelt; ++i)
    {
      unsigned j, e = d->perm[i] & (nelt / 2 - 1);
      unsigned which = d->perm[i] >= nelt;
      unsigned xorv = (i >= nelt / 4 && i < 3 * nelt / 4) ? 24 : 0;

      for (j = 0; j < eltsz; ++j)
	{
	  rperm[which][(i * eltsz + j) ^ xorv] = GEN_INT (e * eltsz + j);
	  rperm[1 - which][(i * eltsz + j) ^ xorv] = m128;
	}
    }

  vperm = gen_rtx_CONST_VECTOR (V32QImode, gen_rtvec_v (32, rperm[0]));
  vperm = force_reg (V32QImode, vperm);

  l = gen_reg_rtx (V32QImode);
  op = gen_lowpart (V32QImode, d->op0);
  emit_insn (gen_avx2_pshufbv32qi3 (l, op, vperm));

  vperm = gen_rtx_CONST_VECTOR (V32QImode, gen_rtvec_v (32, rperm[1]));
  vperm = force_reg (V32QImode, vperm);

  h = gen_reg_rtx (V32QImode);
  op = gen_lowpart (V32QImode, d->op1);
  emit_insn (gen_avx2_pshufbv32qi3 (h, op, vperm));

  ior = gen_reg_rtx (V32QImode);
  emit_insn (gen_iorv32qi3 (ior, l, h));

  /* Permute the V4DImode quarters using { 0, 2, 1, 3 } permutation.  */
  op = gen_reg_rtx (V4DImode);
  ior = gen_lowpart (V4DImode, ior);
  emit_insn (gen_avx2_permv4di_1 (op, ior, const0_rtx, const2_rtx,
				  const1_rtx, GEN_INT (3)));
  emit_move_insn (d->target, gen_lowpart (d->vmode, op));

  return true;
}

/* Implement permutation with pslldq + psrldq + por when pshufb is not
   available.  */
static bool
expand_vec_perm_pslldq_psrldq_por (struct expand_vec_perm_d *d, bool pandn)
{
  unsigned i, nelt = d->nelt;
  unsigned start1, end1 = -1;
  machine_mode vmode = d->vmode, imode;
  int start2 = -1;
  bool clear_op0, clear_op1;
  unsigned inner_size;
  rtx op0, op1, dop1;
  rtx (*gen_vec_shr) (rtx, rtx, rtx);
  rtx (*gen_vec_shl) (rtx, rtx, rtx);

  /* pshufd can be used for V4SI/V2DI under TARGET_SSE2.  */
  if (!TARGET_SSE2 || (vmode != E_V16QImode && vmode != E_V8HImode))
    return false;

  start1 = d->perm[0];
  for (i = 1; i < nelt; i++)
    {
      if (d->perm[i] != d->perm[i-1] + 1
	  || d->perm[i] == nelt)
	{
	  if (start2 == -1)
	    {
	      start2 = d->perm[i];
	      end1 = d->perm[i-1];
	    }
	  else
	    return false;
	}
    }

  clear_op0 = end1 != nelt - 1;
  clear_op1 = start2 % nelt != 0;
  /* pandn/pand is needed to clear upper/lower bits of op0/op1.  */
  if (!pandn && (clear_op0 || clear_op1))
    return false;

  if (d->testing_p)
    return true;

  gen_vec_shr = vmode == E_V16QImode ? gen_vec_shr_v16qi : gen_vec_shr_v8hi;
  gen_vec_shl = vmode == E_V16QImode ? gen_vec_shl_v16qi : gen_vec_shl_v8hi;
  imode = GET_MODE_INNER (vmode);
  inner_size = GET_MODE_BITSIZE (imode);
  op0 = gen_reg_rtx (vmode);
  op1 = gen_reg_rtx (vmode);

  if (start1)
    emit_insn (gen_vec_shr (op0, d->op0, GEN_INT (start1 * inner_size)));
  else
    emit_move_insn (op0, d->op0);

  dop1 = d->op1;
  if (d->one_operand_p)
    dop1 = d->op0;

  int shl_offset = end1 - start1 + 1 - start2 % nelt;
  if (shl_offset)
    emit_insn (gen_vec_shl (op1, dop1, GEN_INT (shl_offset * inner_size)));
  else
    emit_move_insn (op1, dop1);

  /* Clear lower/upper bits for op0/op1.  */
  if (clear_op0 || clear_op1)
    {
      rtx vec[16];
      rtx const_vec;
      rtx clear;
      for (i = 0; i != nelt; i++)
	{
	  if (i < (end1 - start1 + 1))
	    vec[i] = gen_int_mode ((HOST_WIDE_INT_1U << inner_size) - 1, imode);
	  else
	    vec[i] = CONST0_RTX (imode);
	}
      const_vec = gen_rtx_CONST_VECTOR (vmode, gen_rtvec_v (nelt, vec));
      const_vec = validize_mem (force_const_mem (vmode, const_vec));
      clear = force_reg (vmode, const_vec);

      if (clear_op0)
	emit_move_insn (op0, gen_rtx_AND (vmode, op0, clear));
      if (clear_op1)
	emit_move_insn (op1, gen_rtx_AND (vmode,
					  gen_rtx_NOT (vmode, clear),
					  op1));
    }

  emit_move_insn (d->target, gen_rtx_IOR (vmode, op0, op1));
  return true;
}

/* A subroutine of expand_vec_perm_even_odd_1.  Implement extract-even
   and extract-odd permutations of two V8QI, V8HI, V16QI, V16HI or V32QI
   operands with two "and" and "pack" or two "shift" and "pack" insns.
   We should have already failed all two instruction sequences.  */

static bool
expand_vec_perm_even_odd_pack (struct expand_vec_perm_d *d)
{
  rtx op, dop0, dop1, t;
  unsigned i, odd, c, s, nelt = d->nelt;
  bool end_perm = false;
  machine_mode half_mode;
  rtx (*gen_and) (rtx, rtx, rtx);
  rtx (*gen_pack) (rtx, rtx, rtx);
  rtx (*gen_shift) (rtx, rtx, rtx);

  if (d->one_operand_p)
    return false;

  switch (d->vmode)
    {
    case E_V4HImode:
      /* Required for "pack".  */
      if (!TARGET_SSE4_1)
	return false;
      c = 0xffff;
      s = 16;
      half_mode = V2SImode;
      gen_and = gen_andv2si3;
      gen_pack = gen_mmx_packusdw;
      gen_shift = gen_lshrv2si3;
      break;
    case E_V8HImode:
      /* Required for "pack".  */
      if (!TARGET_SSE4_1)
        return false;
      c = 0xffff;
      s = 16;
      half_mode = V4SImode;
      gen_and = gen_andv4si3;
      gen_pack = gen_sse4_1_packusdw;
      gen_shift = gen_lshrv4si3;
      break;
    case E_V8QImode:
      /* No check as all instructions are SSE2.  */
      c = 0xff;
      s = 8;
      half_mode = V4HImode;
      gen_and = gen_andv4hi3;
      gen_pack = gen_mmx_packuswb;
      gen_shift = gen_lshrv4hi3;
      break;
    case E_V16QImode:
      /* No check as all instructions are SSE2.  */
      c = 0xff;
      s = 8;
      half_mode = V8HImode;
      gen_and = gen_andv8hi3;
      gen_pack = gen_sse2_packuswb;
      gen_shift = gen_lshrv8hi3;
      break;
    case E_V16HImode:
      if (!TARGET_AVX2)
        return false;
      c = 0xffff;
      s = 16;
      half_mode = V8SImode;
      gen_and = gen_andv8si3;
      gen_pack = gen_avx2_packusdw;
      gen_shift = gen_lshrv8si3;
      end_perm = true;
      break;
    case E_V32QImode:
      if (!TARGET_AVX2)
        return false;
      c = 0xff;
      s = 8;
      half_mode = V16HImode;
      gen_and = gen_andv16hi3;
      gen_pack = gen_avx2_packuswb;
      gen_shift = gen_lshrv16hi3;
      end_perm = true;
      break;
    default:
      /* Only V4HI, V8QI, V8HI, V16QI, V16HI and V32QI modes
	 are more profitable than general shuffles.  */
      return false;
    }

  /* Check that permutation is even or odd.  */
  odd = d->perm[0];
  if (odd > 1)
    return false;

  for (i = 1; i < nelt; ++i)
    if (d->perm[i] != 2 * i + odd)
      return false;

  if (d->testing_p)
    return true;

  dop0 = gen_reg_rtx (half_mode);
  dop1 = gen_reg_rtx (half_mode);
  if (odd == 0)
    {
      t = gen_const_vec_duplicate (half_mode, GEN_INT (c));
      t = force_reg (half_mode, t);
      emit_insn (gen_and (dop0, t, gen_lowpart (half_mode, d->op0)));
      emit_insn (gen_and (dop1, t, gen_lowpart (half_mode, d->op1)));
    }
  else
    {
      emit_insn (gen_shift (dop0,
			    gen_lowpart (half_mode, d->op0),
			    GEN_INT (s)));
      emit_insn (gen_shift (dop1,
			    gen_lowpart (half_mode, d->op1),
			    GEN_INT (s)));
    }
  /* In AVX2 for 256 bit case we need to permute pack result.  */
  if (TARGET_AVX2 && end_perm)
    {
      op = gen_reg_rtx (d->vmode);
      t = gen_reg_rtx (V4DImode);
      emit_insn (gen_pack (op, dop0, dop1));
      emit_insn (gen_avx2_permv4di_1 (t,
				      gen_lowpart (V4DImode, op),
				      const0_rtx,
				      const2_rtx,
				      const1_rtx,
				      GEN_INT (3)));
      emit_move_insn (d->target, gen_lowpart (d->vmode, t));
    }
  else
    emit_insn (gen_pack (d->target, dop0, dop1));

  return true;
}

/* A subroutine of expand_vec_perm_even_odd_1.  Implement extract-even
   and extract-odd permutations of two V64QI operands
   with two "shifts", two "truncs" and one "concat" insns for "odd"
   and two "truncs" and one concat insn for "even."
   Have already failed all two instruction sequences.  */

static bool
expand_vec_perm_even_odd_trunc (struct expand_vec_perm_d *d)
{
  rtx t1, t2, t3, t4;
  unsigned i, odd, nelt = d->nelt;

  if (!TARGET_AVX512BW
      || d->one_operand_p
      || d->vmode != V64QImode)
    return false;

  /* Check that permutation is even or odd.  */
  odd = d->perm[0];
  if (odd > 1)
    return false;

  for (i = 1; i < nelt; ++i)
    if (d->perm[i] != 2 * i + odd)
      return false;

  if (d->testing_p)
    return true;


  if (odd)
    {
      t1 = gen_reg_rtx (V32HImode);
      t2 = gen_reg_rtx (V32HImode);
      emit_insn (gen_lshrv32hi3 (t1,
				 gen_lowpart (V32HImode, d->op0),
				 GEN_INT (8)));
      emit_insn (gen_lshrv32hi3 (t2,
				 gen_lowpart (V32HImode, d->op1),
				 GEN_INT (8)));
    }
  else
    {
      t1 = gen_lowpart (V32HImode, d->op0);
      t2 = gen_lowpart (V32HImode, d->op1);
    }

  t3 = gen_reg_rtx (V32QImode);
  t4 = gen_reg_rtx (V32QImode);
  emit_insn (gen_avx512bw_truncatev32hiv32qi2 (t3, t1));
  emit_insn (gen_avx512bw_truncatev32hiv32qi2 (t4, t2));
  emit_insn (gen_avx_vec_concatv64qi (d->target, t3, t4));

  return true;
}

/* A subroutine of ix86_expand_vec_perm_const_1.  Implement extract-even
   and extract-odd permutations.  */

static bool
expand_vec_perm_even_odd_1 (struct expand_vec_perm_d *d, unsigned odd)
{
  rtx t1, t2, t3, t4, t5;

  switch (d->vmode)
    {
    case E_V4DFmode:
      if (d->testing_p)
	break;
      t1 = gen_reg_rtx (V4DFmode);
      t2 = gen_reg_rtx (V4DFmode);

      /* Shuffle the lanes around into { 0 1 4 5 } and { 2 3 6 7 }.  */
      emit_insn (gen_avx_vperm2f128v4df3 (t1, d->op0, d->op1, GEN_INT (0x20)));
      emit_insn (gen_avx_vperm2f128v4df3 (t2, d->op0, d->op1, GEN_INT (0x31)));

      /* Now an unpck[lh]pd will produce the result required.  */
      if (odd)
	t3 = gen_avx_unpckhpd256 (d->target, t1, t2);
      else
	t3 = gen_avx_unpcklpd256 (d->target, t1, t2);
      emit_insn (t3);
      break;

    case E_V8SFmode:
      {
	int mask = odd ? 0xdd : 0x88;

	if (d->testing_p)
	  break;
	t1 = gen_reg_rtx (V8SFmode);
	t2 = gen_reg_rtx (V8SFmode);
	t3 = gen_reg_rtx (V8SFmode);

	/* Shuffle within the 128-bit lanes to produce:
	   { 0 2 8 a 4 6 c e } | { 1 3 9 b 5 7 d f }.  */
	emit_insn (gen_avx_shufps256 (t1, d->op0, d->op1,
				      GEN_INT (mask)));

	/* Shuffle the lanes around to produce:
	   { 4 6 c e 0 2 8 a } and { 5 7 d f 1 3 9 b }.  */
	emit_insn (gen_avx_vperm2f128v8sf3 (t2, t1, t1,
					    GEN_INT (0x3)));

	/* Shuffle within the 128-bit lanes to produce:
	   { 0 2 4 6 4 6 0 2 } | { 1 3 5 7 5 7 1 3 }.  */
	emit_insn (gen_avx_shufps256 (t3, t1, t2, GEN_INT (0x44)));

	/* Shuffle within the 128-bit lanes to produce:
	   { 8 a c e c e 8 a } | { 9 b d f d f 9 b }.  */
	emit_insn (gen_avx_shufps256 (t2, t1, t2, GEN_INT (0xee)));

	/* Shuffle the lanes around to produce:
	   { 0 2 4 6 8 a c e } | { 1 3 5 7 9 b d f }.  */
	emit_insn (gen_avx_vperm2f128v8sf3 (d->target, t3, t2,
					    GEN_INT (0x20)));
      }
      break;

    case E_V2DFmode:
    case E_V4SFmode:
    case E_V2DImode:
    case E_V2SImode:
    case E_V4SImode:
    case E_V2HImode:
      /* These are always directly implementable by expand_vec_perm_1.  */
      gcc_unreachable ();

    case E_V2SFmode:
      gcc_assert (TARGET_MMX_WITH_SSE);
      /* We have no suitable instructions.  */
      if (d->testing_p)
	return false;
      break;

    case E_V4QImode:
      if (TARGET_SSSE3 && !TARGET_SLOW_PSHUFB)
	return expand_vec_perm_pshufb2 (d);
      else
	{
	  if (d->testing_p)
	    break;
	  /* We need 2*log2(N)-1 operations to achieve odd/even
	     with interleave. */
	  t1 = gen_reg_rtx (V4QImode);
	  emit_insn (gen_mmx_punpckhbw_low (t1, d->op0, d->op1));
	  emit_insn (gen_mmx_punpcklbw_low (d->target, d->op0, d->op1));
	  if (odd)
	    t2 = gen_mmx_punpckhbw_low (d->target, d->target, t1);
	  else
	    t2 = gen_mmx_punpcklbw_low (d->target, d->target, t1);
	  emit_insn (t2);
	}
      break;

    case E_V4HImode:
      if (TARGET_SSE4_1)
	return expand_vec_perm_even_odd_pack (d);
      else if (TARGET_SSSE3 && !TARGET_SLOW_PSHUFB)
	return expand_vec_perm_pshufb2 (d);
      else
	{
	  if (d->testing_p)
	    break;
	  /* We need 2*log2(N)-1 operations to achieve odd/even
	     with interleave. */
	  t1 = gen_reg_rtx (V4HImode);
	  emit_insn (gen_mmx_punpckhwd (t1, d->op0, d->op1));
	  emit_insn (gen_mmx_punpcklwd (d->target, d->op0, d->op1));
	  if (odd)
	    t2 = gen_mmx_punpckhwd (d->target, d->target, t1);
	  else
	    t2 = gen_mmx_punpcklwd (d->target, d->target, t1);
	  emit_insn (t2);
	}
      break;

    case E_V8HImode:
      if (TARGET_SSE4_1)
	return expand_vec_perm_even_odd_pack (d);
      else if (TARGET_SSSE3 && !TARGET_SLOW_PSHUFB)
	return expand_vec_perm_pshufb2 (d);
      else
	{
	  if (d->testing_p)
	    break;
	  /* We need 2*log2(N)-1 operations to achieve odd/even
	     with interleave. */
	  t1 = gen_reg_rtx (V8HImode);
	  t2 = gen_reg_rtx (V8HImode);
	  emit_insn (gen_vec_interleave_highv8hi (t1, d->op0, d->op1));
	  emit_insn (gen_vec_interleave_lowv8hi (d->target, d->op0, d->op1));
	  emit_insn (gen_vec_interleave_highv8hi (t2, d->target, t1));
	  emit_insn (gen_vec_interleave_lowv8hi (d->target, d->target, t1));
	  if (odd)
	    t3 = gen_vec_interleave_highv8hi (d->target, d->target, t2);
	  else
	    t3 = gen_vec_interleave_lowv8hi (d->target, d->target, t2);
	  emit_insn (t3);
	}
      break;

    case E_V8QImode:
    case E_V16QImode:
      return expand_vec_perm_even_odd_pack (d);

    case E_V16HImode:
    case E_V32QImode:
      return expand_vec_perm_even_odd_pack (d);

    case E_V64QImode:
      return expand_vec_perm_even_odd_trunc (d);

    case E_V4DImode:
      if (!TARGET_AVX2)
	{
	  struct expand_vec_perm_d d_copy = *d;
	  d_copy.vmode = V4DFmode;
	  if (d->testing_p)
	    d_copy.target = gen_raw_REG (V4DFmode, LAST_VIRTUAL_REGISTER + 1);
	  else
	    d_copy.target = gen_reg_rtx (V4DFmode);
	  d_copy.op0 = gen_lowpart (V4DFmode, d->op0);
	  d_copy.op1 = gen_lowpart (V4DFmode, d->op1);
	  if (expand_vec_perm_even_odd_1 (&d_copy, odd))
	    {
	      if (!d->testing_p)
		emit_move_insn (d->target,
				gen_lowpart (V4DImode, d_copy.target));
	      return true;
	    }
	  return false;
	}

      if (d->testing_p)
	break;

      t1 = gen_reg_rtx (V4DImode);
      t2 = gen_reg_rtx (V4DImode);

      /* Shuffle the lanes around into { 0 1 4 5 } and { 2 3 6 7 }.  */
      emit_insn (gen_avx2_permv2ti (t1, d->op0, d->op1, GEN_INT (0x20)));
      emit_insn (gen_avx2_permv2ti (t2, d->op0, d->op1, GEN_INT (0x31)));

      /* Now an vpunpck[lh]qdq will produce the result required.  */
      if (odd)
	t3 = gen_avx2_interleave_highv4di (d->target, t1, t2);
      else
	t3 = gen_avx2_interleave_lowv4di (d->target, t1, t2);
      emit_insn (t3);
      break;

    case E_V8SImode:
      if (!TARGET_AVX2)
	{
	  struct expand_vec_perm_d d_copy = *d;
	  d_copy.vmode = V8SFmode;
	  if (d->testing_p)
	    d_copy.target = gen_raw_REG (V8SFmode, LAST_VIRTUAL_REGISTER + 1);
	  else
	    d_copy.target = gen_reg_rtx (V8SFmode);
	  d_copy.op0 = gen_lowpart (V8SFmode, d->op0);
	  d_copy.op1 = gen_lowpart (V8SFmode, d->op1);
	  if (expand_vec_perm_even_odd_1 (&d_copy, odd))
	    {
	      if (!d->testing_p)
		emit_move_insn (d->target,
				gen_lowpart (V8SImode, d_copy.target));
	      return true;
	    }
	  return false;
	}

      if (d->testing_p)
	break;

      t1 = gen_reg_rtx (V8SImode);
      t2 = gen_reg_rtx (V8SImode);
      t3 = gen_reg_rtx (V4DImode);
      t4 = gen_reg_rtx (V4DImode);
      t5 = gen_reg_rtx (V4DImode);

      /* Shuffle the lanes around into
	 { 0 1 2 3 8 9 a b } and { 4 5 6 7 c d e f }.  */
      emit_insn (gen_avx2_permv2ti (t3, gen_lowpart (V4DImode, d->op0),
				    gen_lowpart (V4DImode, d->op1),
				    GEN_INT (0x20)));
      emit_insn (gen_avx2_permv2ti (t4, gen_lowpart (V4DImode, d->op0),
				    gen_lowpart (V4DImode, d->op1),
				    GEN_INT (0x31)));

      /* Swap the 2nd and 3rd position in each lane into
	 { 0 2 1 3 8 a 9 b } and { 4 6 5 7 c e d f }.  */
      emit_insn (gen_avx2_pshufdv3 (t1, gen_lowpart (V8SImode, t3),
				    GEN_INT (2 * 4 + 1 * 16 + 3 * 64)));
      emit_insn (gen_avx2_pshufdv3 (t2, gen_lowpart (V8SImode, t4),
				    GEN_INT (2 * 4 + 1 * 16 + 3 * 64)));

      /* Now an vpunpck[lh]qdq will produce
	 { 0 2 4 6 8 a c e } resp. { 1 3 5 7 9 b d f }.  */
      if (odd)
	t3 = gen_avx2_interleave_highv4di (t5, gen_lowpart (V4DImode, t1),
					   gen_lowpart (V4DImode, t2));
      else
	t3 = gen_avx2_interleave_lowv4di (t5, gen_lowpart (V4DImode, t1),
					  gen_lowpart (V4DImode, t2));
      emit_insn (t3);
      emit_move_insn (d->target, gen_lowpart (V8SImode, t5));
      break;

    default:
      gcc_unreachable ();
    }

  return true;
}

/* A subroutine of ix86_expand_vec_perm_const_1.  Pattern match
   extract-even and extract-odd permutations.  */

static bool
expand_vec_perm_even_odd (struct expand_vec_perm_d *d)
{
  unsigned i, odd, nelt = d->nelt;

  odd = d->perm[0];
  if (odd != 0 && odd != 1)
    return false;

  for (i = 1; i < nelt; ++i)
    if (d->perm[i] != 2 * i + odd)
      return false;

  if (d->vmode == E_V32HImode
      && d->testing_p
      && !TARGET_AVX512BW)
    return false;

  return expand_vec_perm_even_odd_1 (d, odd);
}

/* A subroutine of ix86_expand_vec_perm_const_1.  Implement broadcast
   permutations.  We assume that expand_vec_perm_1 has already failed.  */

static bool
expand_vec_perm_broadcast_1 (struct expand_vec_perm_d *d)
{
  unsigned elt = d->perm[0], nelt2 = d->nelt / 2;
  machine_mode vmode = d->vmode;
  rtx (*gen) (rtx, rtx, rtx);
  unsigned char perm2[4];
  rtx op0 = d->op0, dest;
  bool ok;

  switch (vmode)
    {
    case E_V4DFmode:
    case E_V8SFmode:
      /* These are special-cased in sse.md so that we can optionally
	 use the vbroadcast instruction.  They expand to two insns
	 if the input happens to be in a register.  */
      gcc_unreachable ();

    case E_V2DFmode:
    case E_V2SFmode:
    case E_V4SFmode:
    case E_V2DImode:
    case E_V2SImode:
    case E_V4SImode:
    case E_V2HImode:
    case E_V4HImode:
      /* These are always implementable using standard shuffle patterns.  */
      gcc_unreachable ();

    case E_V4QImode:
      /* This can be implemented via interleave and pshuflw.  */
      if (d->testing_p)
	return true;

      if (elt >= nelt2)
	{
	  gen = gen_mmx_punpckhbw_low;
	  elt -= nelt2;
	}
      else
	gen = gen_mmx_punpcklbw_low;

      dest = gen_reg_rtx (vmode);
      emit_insn (gen (dest, op0, op0));
      vmode = get_mode_wider_vector (vmode);
      op0 = gen_lowpart (vmode, dest);

      memset (perm2, elt, 2);
      dest = gen_reg_rtx (vmode);
      ok = expand_vselect (dest, op0, perm2, 2, d->testing_p);
      gcc_assert (ok);

      emit_move_insn (d->target, gen_lowpart (d->vmode, dest));
      return true;

    case E_V8QImode:
      /* This can be implemented via interleave.  We save one insn by
	 stopping once we have promoted to V2SImode and then use pshufd.  */
      if (d->testing_p)
	return true;
      do
	{
	  if (elt >= nelt2)
	    {
	      gen = vmode == V8QImode ? gen_mmx_punpckhbw
				      : gen_mmx_punpckhwd;
	      elt -= nelt2;
	    }
	  else
	    gen = vmode == V8QImode ? gen_mmx_punpcklbw
				    : gen_mmx_punpcklwd;
	  nelt2 /= 2;

	  dest = gen_reg_rtx (vmode);
	  emit_insn (gen (dest, op0, op0));
	  vmode = get_mode_wider_vector (vmode);
	  op0 = gen_lowpart (vmode, dest);
	}
      while (vmode != V2SImode);

      memset (perm2, elt, 2);
      dest = gen_reg_rtx (vmode);
      ok = expand_vselect (dest, op0, perm2, 2, d->testing_p);
      gcc_assert (ok);

      emit_move_insn (d->target, gen_lowpart (d->vmode, dest));
      return true;

    case E_V8HImode:
    case E_V16QImode:
      /* These can be implemented via interleave.  We save one insn by
	 stopping once we have promoted to V4SImode and then use pshufd.  */
      if (d->testing_p)
	return true;
      do
	{
	  if (elt >= nelt2)
	    {
	      gen = vmode == V16QImode ? gen_vec_interleave_highv16qi
				       : gen_vec_interleave_highv8hi;
	      elt -= nelt2;
	    }
	  else
	    gen = vmode == V16QImode ? gen_vec_interleave_lowv16qi
				     : gen_vec_interleave_lowv8hi;
	  nelt2 /= 2;

	  dest = gen_reg_rtx (vmode);
	  emit_insn (gen (dest, op0, op0));
	  vmode = get_mode_wider_vector (vmode);
	  op0 = gen_lowpart (vmode, dest);
	}
      while (vmode != V4SImode);

      memset (perm2, elt, 4);
      dest = gen_reg_rtx (vmode);
      ok = expand_vselect (dest, op0, perm2, 4, d->testing_p);
      gcc_assert (ok);

      emit_move_insn (d->target, gen_lowpart (d->vmode, dest));
      return true;

    case E_V8HFmode:
    case E_V8BFmode:
      /* This can be implemented via interleave and pshufd.  */
      if (d->testing_p)
	return true;

      rtx (*gen_interleave) (machine_mode, int, rtx, rtx, rtx);
      if (elt >= nelt2)
	{
	  gen_interleave = gen_vec_interleave_high;
	  elt -= nelt2;
	}
      else
	gen_interleave = gen_vec_interleave_low;
      nelt2 /= 2;

      dest = gen_reg_rtx (vmode);
      emit_insn (gen_interleave (vmode, 1, dest, op0, op0));

      vmode = V4SImode;
      op0 = gen_lowpart (vmode, dest);

      memset (perm2, elt, 4);
      dest = gen_reg_rtx (vmode);
      ok = expand_vselect (dest, op0, perm2, 4, d->testing_p);
      gcc_assert (ok);

      emit_move_insn (d->target, gen_lowpart (d->vmode, dest));
      return true;

    case E_V32QImode:
    case E_V16HImode:
    case E_V8SImode:
    case E_V4DImode:
      /* For AVX2 broadcasts of the first element vpbroadcast* or
	 vpermq should be used by expand_vec_perm_1.  */
      gcc_assert (!TARGET_AVX2 || d->perm[0]);
      return false;

    case E_V64QImode:
      gcc_assert (!TARGET_AVX512BW || d->perm[0]);
      return false;

    case E_V32HImode:
      gcc_assert (!TARGET_AVX512BW);
      return false;

    default:
      gcc_unreachable ();
    }
}

/* A subroutine of ix86_expand_vec_perm_const_1.  Pattern match
   broadcast permutations.  */

static bool
expand_vec_perm_broadcast (struct expand_vec_perm_d *d)
{
  unsigned i, elt, nelt = d->nelt;

  if (!d->one_operand_p)
    return false;

  elt = d->perm[0];
  for (i = 1; i < nelt; ++i)
    if (d->perm[i] != elt)
      return false;

  return expand_vec_perm_broadcast_1 (d);
}

/* Implement arbitrary permutations of two V64QImode operands
   with 2 vperm[it]2w, 2 vpshufb and one vpor instruction.  */
static bool
expand_vec_perm_vpermt2_vpshub2 (struct expand_vec_perm_d *d)
{
  if (!TARGET_AVX512BW || !(d->vmode == V64QImode))
    return false;

  if (d->testing_p)
    return true;

  struct expand_vec_perm_d ds[2];
  rtx rperm[128], vperm, target0, target1;
  unsigned int i, nelt;
  machine_mode vmode;

  nelt = d->nelt;
  vmode = V64QImode;

  for (i = 0; i < 2; i++)
    {
      ds[i] = *d;
      ds[i].vmode = V32HImode;
      ds[i].nelt = 32;
      ds[i].target = gen_reg_rtx (V32HImode);
      ds[i].op0 = gen_lowpart (V32HImode, d->op0);
      ds[i].op1 = gen_lowpart (V32HImode, d->op1);
    }

  /* Prepare permutations such that the first one takes care of
     putting the even bytes into the right positions or one higher
     positions (ds[0]) and the second one takes care of
     putting the odd bytes into the right positions or one below
     (ds[1]).  */

  for (i = 0; i < nelt; i++)
    {
      ds[i & 1].perm[i / 2] = d->perm[i] / 2;
      if (i & 1)
	{
	  rperm[i] = constm1_rtx;
	  rperm[i + 64] = GEN_INT ((i & 14) + (d->perm[i] & 1));
	}
      else
	{
	  rperm[i] = GEN_INT ((i & 14) + (d->perm[i] & 1));
	  rperm[i + 64] = constm1_rtx;
	}
    }

  bool ok = expand_vec_perm_1 (&ds[0]);
  gcc_assert (ok);
  ds[0].target = gen_lowpart (V64QImode, ds[0].target);

  ok = expand_vec_perm_1 (&ds[1]);
  gcc_assert (ok);
  ds[1].target = gen_lowpart (V64QImode, ds[1].target);

  vperm = gen_rtx_CONST_VECTOR (V64QImode, gen_rtvec_v (64, rperm));
  vperm = force_reg (vmode, vperm);
  target0 = gen_reg_rtx (V64QImode);
  emit_insn (gen_avx512bw_pshufbv64qi3 (target0, ds[0].target, vperm));

  vperm = gen_rtx_CONST_VECTOR (V64QImode, gen_rtvec_v (64, rperm + 64));
  vperm = force_reg (vmode, vperm);
  target1 = gen_reg_rtx (V64QImode);
  emit_insn (gen_avx512bw_pshufbv64qi3 (target1, ds[1].target, vperm));

  emit_insn (gen_iorv64qi3 (d->target, target0, target1));
  return true;
}

/* Implement arbitrary permutation of two V32QImode and V16QImode operands
   with 4 vpshufb insns, 2 vpermq and 3 vpor.  We should have already failed
   all the shorter instruction sequences.  */

static bool
expand_vec_perm_vpshufb4_vpermq2 (struct expand_vec_perm_d *d)
{
  rtx rperm[4][32], vperm, l[2], h[2], op, m128;
  unsigned int i, nelt, eltsz;
  bool used[4];

  if (!TARGET_AVX2
      || d->one_operand_p
      || (d->vmode != V32QImode && d->vmode != V16HImode))
    return false;

  if (d->testing_p)
    return true;

  nelt = d->nelt;
  eltsz = GET_MODE_UNIT_SIZE (d->vmode);

  /* Generate 4 permutation masks.  If the required element is within
     the same lane, it is shuffled in.  If the required element from the
     other lane, force a zero by setting bit 7 in the permutation mask.
     In the other mask the mask has non-negative elements if element
     is requested from the other lane, but also moved to the other lane,
     so that the result of vpshufb can have the two V2TImode halves
     swapped.  */
  m128 = GEN_INT (-128);
  for (i = 0; i < 32; ++i)
    {
      rperm[0][i] = m128;
      rperm[1][i] = m128;
      rperm[2][i] = m128;
      rperm[3][i] = m128;
    }
  used[0] = false;
  used[1] = false;
  used[2] = false;
  used[3] = false;
  for (i = 0; i < nelt; ++i)
    {
      unsigned j, e = d->perm[i] & (nelt / 2 - 1);
      unsigned xlane = ((d->perm[i] ^ i) & (nelt / 2)) * eltsz;
      unsigned int which = ((d->perm[i] & nelt) ? 2 : 0) + (xlane ? 1 : 0);

      for (j = 0; j < eltsz; ++j)
	rperm[which][(i * eltsz + j) ^ xlane] = GEN_INT (e * eltsz + j);
      used[which] = true;
    }

  for (i = 0; i < 2; ++i)
    {
      if (!used[2 * i + 1])
	{
	  h[i] = NULL_RTX;
	  continue;
	}
      vperm = gen_rtx_CONST_VECTOR (V32QImode,
				    gen_rtvec_v (32, rperm[2 * i + 1]));
      vperm = force_reg (V32QImode, vperm);
      h[i] = gen_reg_rtx (V32QImode);
      op = gen_lowpart (V32QImode, i ? d->op1 : d->op0);
      emit_insn (gen_avx2_pshufbv32qi3 (h[i], op, vperm));
    }

  /* Swap the 128-byte lanes of h[X].  */
  for (i = 0; i < 2; ++i)
   {
     if (h[i] == NULL_RTX)
       continue;
     op = gen_reg_rtx (V4DImode);
     emit_insn (gen_avx2_permv4di_1 (op, gen_lowpart (V4DImode, h[i]),
				     const2_rtx, GEN_INT (3), const0_rtx,
				     const1_rtx));
     h[i] = gen_lowpart (V32QImode, op);
   }

  for (i = 0; i < 2; ++i)
    {
      if (!used[2 * i])
	{
	  l[i] = NULL_RTX;
	  continue;
	}
      vperm = gen_rtx_CONST_VECTOR (V32QImode, gen_rtvec_v (32, rperm[2 * i]));
      vperm = force_reg (V32QImode, vperm);
      l[i] = gen_reg_rtx (V32QImode);
      op = gen_lowpart (V32QImode, i ? d->op1 : d->op0);
      emit_insn (gen_avx2_pshufbv32qi3 (l[i], op, vperm));
    }

  for (i = 0; i < 2; ++i)
    {
      if (h[i] && l[i])
	{
	  op = gen_reg_rtx (V32QImode);
	  emit_insn (gen_iorv32qi3 (op, l[i], h[i]));
	  l[i] = op;
	}
      else if (h[i])
	l[i] = h[i];
    }

  gcc_assert (l[0] && l[1]);
  op = d->target;
  if (d->vmode != V32QImode)
    op = gen_reg_rtx (V32QImode);
  emit_insn (gen_iorv32qi3 (op, l[0], l[1]));
  if (op != d->target)
    emit_move_insn (d->target, gen_lowpart (d->vmode, op));
  return true;
}

/* The guts of ix86_vectorize_vec_perm_const.  With all of the interface bits
   taken care of, perform the expansion in D and return true on success.  */

static bool
ix86_expand_vec_perm_const_1 (struct expand_vec_perm_d *d)
{
  /* Try a single instruction expansion.  */
  if (expand_vec_perm_1 (d))
    return true;

  /* Try sequences of two instructions.  */

  if (expand_vec_perm_pshuflw_pshufhw (d))
    return true;

  if (expand_vec_perm_palignr (d, false))
    return true;

  if (expand_vec_perm_interleave2 (d))
    return true;

  if (expand_vec_perm_broadcast (d))
    return true;

  if (expand_vec_perm_vpermq_perm_1 (d))
    return true;

  if (expand_vec_perm_vperm2f128 (d))
    return true;

  if (expand_vec_perm_pblendv (d))
    return true;

  if (expand_vec_perm_2perm_interleave (d, true))
    return true;

  if (expand_vec_perm_2perm_pblendv (d, true))
    return true;

  if (expand_vec_perm_shufps_shufps (d))
    return true;

  /* Try sequences of three instructions.  */

  if (expand_vec_perm_even_odd_pack (d))
    return true;

  if (expand_vec_perm_2vperm2f128_vshuf (d))
    return true;

  if (expand_vec_perm_pshufb2 (d))
    return true;

  if (expand_vec_perm_pslldq_psrldq_por (d, false))
    return true;

  if (expand_vec_perm_interleave3 (d))
    return true;

  if (expand_vec_perm_vperm2f128_vblend (d))
    return true;

  if (expand_vec_perm_2perm_interleave (d, false))
    return true;

  if (expand_vec_perm_2perm_pblendv (d, false))
    return true;

  /* Try sequences of four instructions.  */

  if (expand_vec_perm_even_odd_trunc (d))
    return true;
  if (expand_vec_perm_vpshufb2_vpermq (d))
    return true;

  if (expand_vec_perm_vpshufb2_vpermq_even_odd (d))
    return true;

  if (expand_vec_perm_vpermt2_vpshub2 (d))
    return true;

  /* ??? Look for narrow permutations whose element orderings would
     allow the promotion to a wider mode.  */

  /* ??? Look for sequences of interleave or a wider permute that place
     the data into the correct lanes for a half-vector shuffle like
     pshuf[lh]w or vpermilps.  */

  /* ??? Look for sequences of interleave that produce the desired results.
     The combinatorics of punpck[lh] get pretty ugly... */

  if (expand_vec_perm_even_odd (d))
    return true;

  /* Generate four or five instructions.  */
  if (expand_vec_perm_pslldq_psrldq_por (d, true))
    return true;

  /* Even longer sequences.  */
  if (expand_vec_perm_vpshufb4_vpermq2 (d))
    return true;

  /* See if we can get the same permutation in different vector integer
     mode.  */
  struct expand_vec_perm_d nd;
  if (canonicalize_vector_int_perm (d, &nd) && expand_vec_perm_1 (&nd))
    {
      if (!d->testing_p)
	emit_move_insn (d->target, gen_lowpart (d->vmode, nd.target));
      return true;
    }

  /* Even longer, including recursion to ix86_expand_vec_perm_const_1.  */
  if (expand_vec_perm2_vperm2f128_vblend (d))
    return true;

  return false;
}

/* If a permutation only uses one operand, make it clear. Returns true
   if the permutation references both operands.  */

static bool
canonicalize_perm (struct expand_vec_perm_d *d)
{
  int i, which, nelt = d->nelt;

  for (i = which = 0; i < nelt; ++i)
    which |= (d->perm[i] < nelt ? 1 : 2);

  d->one_operand_p = true;
  switch (which)
    {
    default:
      gcc_unreachable();

    case 3:
      if (!rtx_equal_p (d->op0, d->op1))
        {
	  d->one_operand_p = false;
	  break;
        }
      /* The elements of PERM do not suggest that only the first operand
	 is used, but both operands are identical.  Allow easier matching
	 of the permutation by folding the permutation into the single
	 input vector.  */
      /* FALLTHRU */

    case 2:
      for (i = 0; i < nelt; ++i)
        d->perm[i] &= nelt - 1;
      d->op0 = d->op1;
      break;

    case 1:
      d->op1 = d->op0;
      break;
    }

  return (which == 3);
}

/* Implement TARGET_VECTORIZE_VEC_PERM_CONST.  */

bool
ix86_vectorize_vec_perm_const (machine_mode vmode, machine_mode op_mode,
			       rtx target, rtx op0, rtx op1,
			       const vec_perm_indices &sel)
{
  if (vmode != op_mode)
    return false;

  struct expand_vec_perm_d d;
  unsigned char perm[MAX_VECT_LEN];
  unsigned int i, nelt, which;
  bool two_args;

  if (GET_MODE_SIZE (vmode) == 64 && !TARGET_EVEX512)
    return false;

  /* For HF mode vector, convert it to HI using subreg.  */
  if (GET_MODE_INNER (vmode) == HFmode)
    {
      machine_mode orig_mode = vmode;
      vmode = mode_for_vector (HImode,
			       GET_MODE_NUNITS (vmode)).require ();
      if (target)
	target = lowpart_subreg (vmode, target, orig_mode);
      if (op0)
	op0 = lowpart_subreg (vmode, op0, orig_mode);
      if (op1)
	op1 = lowpart_subreg (vmode, op1, orig_mode);
    }

  d.target = target;
  d.op0 = op0;
  d.op1 = op1;

  d.vmode = vmode;
  gcc_assert (VECTOR_MODE_P (d.vmode));
  d.nelt = nelt = GET_MODE_NUNITS (d.vmode);
  d.testing_p = !target;

  gcc_assert (sel.length () == nelt);
  gcc_checking_assert (sizeof (d.perm) == sizeof (perm));

  /* Given sufficient ISA support we can just return true here
     for selected vector modes.  */
  switch (d.vmode)
    {
    case E_V16SFmode:
    case E_V16SImode:
    case E_V8DImode:
    case E_V8DFmode:
      if (!TARGET_AVX512F)
	return false;
      /* All implementable with a single vperm[it]2 insn.  */
      if (d.testing_p)
	return true;
      break;
    case E_V32HImode:
      if (!TARGET_AVX512F)
	return false;
      if (d.testing_p && TARGET_AVX512BW)
	/* All implementable with a single vperm[it]2 insn.  */
	return true;
      break;
    case E_V64QImode:
      if (!TARGET_AVX512F)
	return false;
      if (d.testing_p && TARGET_AVX512BW)
	/* Implementable with 2 vperm[it]2, 2 vpshufb and 1 or insn.  */
	return true;
      break;
    case E_V8SImode:
    case E_V8SFmode:
    case E_V4DFmode:
    case E_V4DImode:
      if (!TARGET_AVX)
	return false;
      if (d.testing_p && TARGET_AVX512VL)
	/* All implementable with a single vperm[it]2 insn.  */
	return true;
      break;
    case E_V16HImode:
      if (!TARGET_SSE2)
	return false;
      if (d.testing_p && TARGET_AVX2)
	/* Implementable with 4 vpshufb insns, 2 vpermq and 3 vpor insns.  */
	return true;
      break;
    case E_V32QImode:
      if (!TARGET_SSE2)
	return false;
      if (d.testing_p && TARGET_AVX2)
	/* Implementable with 4 vpshufb insns, 2 vpermq and 3 vpor insns.  */
	return true;
      break;
    case E_V8HImode:
    case E_V16QImode:
      if (!TARGET_SSE2)
	return false;
      /* Fall through.  */
    case E_V4SImode:
    case E_V4SFmode:
      if (!TARGET_SSE)
	return false;
      /* All implementable with a single vpperm insn.  */
      if (d.testing_p && TARGET_XOP)
	return true;
      /* All implementable with 2 pshufb + 1 ior.  */
      if (d.testing_p && TARGET_SSSE3)
	return true;
      break;
    case E_V2SFmode:
    case E_V2SImode:
    case E_V4HImode:
    case E_V8QImode:
      if (!TARGET_MMX_WITH_SSE)
	return false;
      break;
    case E_V2HImode:
      if (!TARGET_SSE2)
	return false;
      /* All implementable with *punpckwd.  */
      if (d.testing_p)
	return true;
      break;
    case E_V4QImode:
      if (!TARGET_SSE2)
	return false;
      break;
    case E_V2DImode:
    case E_V2DFmode:
      if (!TARGET_SSE)
	return false;
      /* All implementable with shufpd or unpck[lh]pd.  */
      if (d.testing_p)
	return true;
      break;
    default:
      return false;
    }

  for (i = which = 0; i < nelt; ++i)
    {
      unsigned char e = sel[i];
      gcc_assert (e < 2 * nelt);
      d.perm[i] = e;
      perm[i] = e;
      which |= (e < nelt ? 1 : 2);
    }

  if (d.testing_p)
    {
      /* For all elements from second vector, fold the elements to first.  */
      if (which == 2)
	for (i = 0; i < nelt; ++i)
	  d.perm[i] -= nelt;

      /* Check whether the mask can be applied to the vector type.  */
      d.one_operand_p = (which != 3);

      /* Implementable with shufps, pshufd or pshuflw.  */
      if (d.one_operand_p
	  && (d.vmode == V4SFmode || d.vmode == V2SFmode
	      || d.vmode == V4SImode || d.vmode == V2SImode
	      || d.vmode == V4HImode || d.vmode == V2HImode))
	return true;

      /* Otherwise we have to go through the motions and see if we can
	 figure out how to generate the requested permutation.  */
      d.target = gen_raw_REG (d.vmode, LAST_VIRTUAL_REGISTER + 1);
      d.op1 = d.op0 = gen_raw_REG (d.vmode, LAST_VIRTUAL_REGISTER + 2);
      if (!d.one_operand_p)
	d.op1 = gen_raw_REG (d.vmode, LAST_VIRTUAL_REGISTER + 3);

      start_sequence ();
      bool ret = ix86_expand_vec_perm_const_1 (&d);
      end_sequence ();

      return ret;
    }

  two_args = canonicalize_perm (&d);

  /* If one of the operands is a zero vector, try to match pmovzx.  */
  if (two_args && (d.op0 == CONST0_RTX (vmode) || d.op1 == CONST0_RTX (vmode)))
    {
      struct expand_vec_perm_d dzero = d;
      if (d.op0 == CONST0_RTX (vmode))
	{
	  d.op1 = dzero.op1 = force_reg (vmode, d.op1);
	  std::swap (dzero.op0, dzero.op1);
	  for (i = 0; i < nelt; ++i)
	    dzero.perm[i] ^= nelt;
	}
      else
	d.op0 = dzero.op0 = force_reg (vmode, d.op0);

      if (expand_vselect_vconcat (dzero.target, dzero.op0, dzero.op1,
				  dzero.perm, nelt, dzero.testing_p))
	return true;
    }

  /* Force operands into registers.  */
  rtx nop0 = force_reg (vmode, d.op0);
  if (d.op0 == d.op1)
    d.op1 = nop0;
  d.op0 = nop0;
  d.op1 = force_reg (vmode, d.op1);

  if (ix86_expand_vec_perm_const_1 (&d))
    return true;

  /* If the selector says both arguments are needed, but the operands are the
     same, the above tried to expand with one_operand_p and flattened selector.
     If that didn't work, retry without one_operand_p; we succeeded with that
     during testing.  */
  if (two_args && d.one_operand_p)
    {
      d.one_operand_p = false;
      memcpy (d.perm, perm, sizeof (perm));
      return ix86_expand_vec_perm_const_1 (&d);
    }

  return false;
}

void
ix86_expand_vec_extract_even_odd (rtx targ, rtx op0, rtx op1, unsigned odd)
{
  struct expand_vec_perm_d d;
  unsigned i, nelt;

  d.target = targ;
  d.op0 = op0;
  d.op1 = op1;
  d.vmode = GET_MODE (targ);
  d.nelt = nelt = GET_MODE_NUNITS (d.vmode);
  d.one_operand_p = false;
  d.testing_p = false;

  for (i = 0; i < nelt; ++i)
    d.perm[i] = i * 2 + odd;

  /* We'll either be able to implement the permutation directly...  */
  if (expand_vec_perm_1 (&d))
    return;

  /* ... or we use the special-case patterns.  */
  expand_vec_perm_even_odd_1 (&d, odd);
}

static void
ix86_expand_vec_interleave (rtx targ, rtx op0, rtx op1, bool high_p)
{
  struct expand_vec_perm_d d;
  unsigned i, nelt, base;
  bool ok;

  d.target = targ;
  d.op0 = op0;
  d.op1 = op1;
  d.vmode = GET_MODE (targ);
  d.nelt = nelt = GET_MODE_NUNITS (d.vmode);
  d.one_operand_p = false;
  d.testing_p = false;

  base = high_p ? nelt / 2 : 0;
  for (i = 0; i < nelt / 2; ++i)
    {
      d.perm[i * 2] = i + base;
      d.perm[i * 2 + 1] = i + base + nelt;
    }

  /* Note that for AVX this isn't one instruction.  */
  ok = ix86_expand_vec_perm_const_1 (&d);
  gcc_assert (ok);
}

/* Expand a vector operation shift by constant for a V*QImode in terms of the
   same operation on V*HImode. Return true if success. */
static bool
ix86_expand_vec_shift_qihi_constant (enum rtx_code code,
				     rtx dest, rtx op1, rtx op2)
{
  machine_mode qimode, himode;
  HOST_WIDE_INT and_constant, xor_constant;
  HOST_WIDE_INT shift_amount;
  rtx vec_const_and, vec_const_xor;
  rtx tmp, op1_subreg;
  rtx (*gen_shift) (rtx, rtx, rtx);
  rtx (*gen_and) (rtx, rtx, rtx);
  rtx (*gen_xor) (rtx, rtx, rtx);
  rtx (*gen_sub) (rtx, rtx, rtx);

  /* Only optimize shift by constant.  */
  if (!CONST_INT_P (op2))
    return false;

  qimode = GET_MODE (dest);
  shift_amount = INTVAL (op2);
  /* Do nothing when shift amount greater equal 8.  */
  if (shift_amount > 7)
    return false;

  gcc_assert (code == ASHIFT || code == ASHIFTRT || code == LSHIFTRT);
  /* Record sign bit.  */
  xor_constant = 1 << (8 - shift_amount - 1);

  /* Zero upper/lower bits shift from left/right element.  */
  and_constant
    = (code == ASHIFT ? 256 - (1 << shift_amount)
       : (1 << (8 - shift_amount)) - 1);

  switch (qimode)
    {
    case V16QImode:
      himode = V8HImode;
      gen_shift =
	((code == ASHIFT)
	 ? gen_ashlv8hi3
	 : (code == ASHIFTRT) ? gen_ashrv8hi3 : gen_lshrv8hi3);
      gen_and = gen_andv16qi3;
      gen_xor = gen_xorv16qi3;
      gen_sub = gen_subv16qi3;
      break;
    case V32QImode:
      himode = V16HImode;
      gen_shift =
	((code == ASHIFT)
	 ? gen_ashlv16hi3
	 : (code == ASHIFTRT) ? gen_ashrv16hi3 : gen_lshrv16hi3);
      gen_and = gen_andv32qi3;
      gen_xor = gen_xorv32qi3;
      gen_sub = gen_subv32qi3;
      break;
    case V64QImode:
      himode = V32HImode;
      gen_shift =
	((code == ASHIFT)
	 ? gen_ashlv32hi3
	 : (code == ASHIFTRT) ? gen_ashrv32hi3 : gen_lshrv32hi3);
      gen_and = gen_andv64qi3;
      gen_xor = gen_xorv64qi3;
      gen_sub = gen_subv64qi3;
      break;
    default:
      gcc_unreachable ();
    }

  tmp = gen_reg_rtx (himode);
  vec_const_and = gen_reg_rtx (qimode);
  op1_subreg = lowpart_subreg (himode, op1, qimode);

  /* For ASHIFT and LSHIFTRT, perform operation like
     vpsllw/vpsrlw $shift_amount, %op1, %dest.
     vpand %vec_const_and, %dest.  */
  emit_insn (gen_shift (tmp, op1_subreg, op2));
  emit_move_insn (dest, simplify_gen_subreg (qimode, tmp, himode, 0));
  emit_move_insn (vec_const_and,
		  ix86_build_const_vector (qimode, true,
					   gen_int_mode (and_constant, QImode)));
  emit_insn (gen_and (dest, dest, vec_const_and));

  /* For ASHIFTRT, perform extra operation like
     vpxor %vec_const_xor, %dest, %dest
     vpsubb %vec_const_xor, %dest, %dest  */
  if (code == ASHIFTRT)
    {
      vec_const_xor = gen_reg_rtx (qimode);
      emit_move_insn (vec_const_xor,
		      ix86_build_const_vector (qimode, true,
					       gen_int_mode (xor_constant, QImode)));
      emit_insn (gen_xor (dest, dest, vec_const_xor));
      emit_insn (gen_sub (dest, dest, vec_const_xor));
    }
  return true;
}

void
ix86_expand_vecop_qihi_partial (enum rtx_code code, rtx dest, rtx op1, rtx op2)
{
  machine_mode qimode = GET_MODE (dest);
  rtx qop1, qop2, hop1, hop2, qdest, hdest;
  bool op2vec = GET_MODE_CLASS (GET_MODE (op2)) == MODE_VECTOR_INT;
  bool uns_p = code != ASHIFTRT;

  switch (qimode)
    {
    case E_V4QImode:
    case E_V8QImode:
      break;
    default:
      gcc_unreachable ();
    }

  qop1 = lowpart_subreg (V16QImode, force_reg (qimode, op1), qimode);

  if (op2vec)
    qop2 = lowpart_subreg (V16QImode, force_reg (qimode, op2), qimode);
  else
    qop2 = op2;

  qdest = gen_reg_rtx (V16QImode);

  if (CONST_INT_P (op2)
      && (code == ASHIFT || code == LSHIFTRT || code == ASHIFTRT)
      && ix86_expand_vec_shift_qihi_constant (code, qdest, qop1, qop2))
    {
      emit_move_insn (dest, gen_lowpart (qimode, qdest));
      return;
    }

  switch (code)
    {
    case MULT:
      gcc_assert (op2vec);
      if (!TARGET_SSE4_1)
	{
	  /* Unpack data such that we've got a source byte in each low byte
	     of each word.  We don't care what goes into the high byte of
	     each word.  Rather than trying to get zero in there, most
	     convenient is to let it be a copy of the low byte.  */
	  hop1 = copy_to_reg (qop1);
	  hop2 = copy_to_reg (qop2);
	  emit_insn (gen_vec_interleave_lowv16qi (hop1, hop1, hop1));
	  emit_insn (gen_vec_interleave_lowv16qi (hop2, hop2, hop2));
	  break;
	}
      /* FALLTHRU */
    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      hop1 = gen_reg_rtx (V8HImode);
      ix86_expand_sse_unpack (hop1, qop1, uns_p, false);
      /* mult/vashr/vlshr/vashl  */
      if (op2vec)
	{
	  hop2 = gen_reg_rtx (V8HImode);
	  ix86_expand_sse_unpack (hop2, qop2, uns_p, false);
	}
      else
	hop2 = qop2;

      break;
    default:
      gcc_unreachable ();
    }

  if (code != MULT && op2vec)
    {
      /* Expand vashr/vlshr/vashl.  */
      hdest = gen_reg_rtx (V8HImode);
      emit_insn (gen_rtx_SET (hdest,
			      simplify_gen_binary (code, V8HImode,
						   hop1, hop2)));
    }
  else
    /* Expand mult/ashr/lshr/ashl.  */
    hdest = expand_simple_binop (V8HImode, code, hop1, hop2,
				NULL_RTX, 1, OPTAB_DIRECT);

  if (TARGET_AVX512BW && TARGET_AVX512VL)
    {
      if (qimode == V8QImode)
	qdest = dest;
      else
	qdest = gen_reg_rtx (V8QImode);

      emit_insn (gen_truncv8hiv8qi2 (qdest, hdest));
    }
  else
    {
      struct expand_vec_perm_d d;
      rtx qres = gen_lowpart (V16QImode, hdest);
      bool ok;
      int i;

      /* Merge the data back into the right place.  */
      d.target = qdest;
      d.op0 = d.op1 = qres;
      d.vmode = V16QImode;
      d.nelt = 16;
      d.one_operand_p = false;
      d.testing_p = false;

      for (i = 0; i < d.nelt; ++i)
	d.perm[i] = i * 2;

      ok = ix86_expand_vec_perm_const_1 (&d);
      gcc_assert (ok);
    }

  if (qdest != dest)
    emit_move_insn (dest, gen_lowpart (qimode, qdest));
}

/* Emit instruction in 2x wider mode.  For example, optimize
   vector MUL generation like

   vpmovzxbw ymm2, xmm0
   vpmovzxbw ymm3, xmm1
   vpmullw   ymm4, ymm2, ymm3
   vpmovwb   xmm0, ymm4

   it would take less instructions than ix86_expand_vecop_qihi.
   Return true if success.  */

static bool
ix86_expand_vecop_qihi2 (enum rtx_code code, rtx dest, rtx op1, rtx op2)
{
  machine_mode himode, qimode = GET_MODE (dest);
  machine_mode wqimode;
  rtx qop1, qop2, hop1, hop2, hdest;
  rtx (*gen_truncate)(rtx, rtx) = NULL;
  bool op2vec = GET_MODE_CLASS (GET_MODE (op2)) == MODE_VECTOR_INT;
  bool uns_p = code != ASHIFTRT;

  /* Without VPMOVWB (provided by AVX512BW ISA), the expansion uses the
     generic permutation to merge the data back into the right place.  This
     permutation results in VPERMQ, which is slow, so better fall back to
     ix86_expand_vecop_qihi.  */
  if (!TARGET_AVX512BW)
    return false;

  if ((qimode == V16QImode && !TARGET_AVX2)
      || (qimode == V32QImode && (!TARGET_AVX512BW || !TARGET_EVEX512))
      /* There are no V64HImode instructions.  */
      || qimode == V64QImode)
     return false;

  /* Do not generate ymm/zmm instructions when
     target prefers 128/256 bit vector width.  */
  if ((qimode == V16QImode && TARGET_PREFER_AVX128)
      || (qimode == V32QImode && TARGET_PREFER_AVX256))
    return false;

  switch (qimode)
    {
    case E_V16QImode:
      himode = V16HImode;
      if (TARGET_AVX512VL && TARGET_AVX512BW)
	gen_truncate = gen_truncv16hiv16qi2;
      break;
    case E_V32QImode:
      himode = V32HImode;
      gen_truncate = gen_truncv32hiv32qi2;
      break;
    default:
      gcc_unreachable ();
    }

  wqimode = GET_MODE_2XWIDER_MODE (qimode).require ();
  qop1 = lowpart_subreg (wqimode, force_reg (qimode, op1), qimode);

  if (op2vec)
    qop2 = lowpart_subreg (wqimode, force_reg (qimode, op2), qimode);
  else
    qop2 = op2;

  hop1 = gen_reg_rtx (himode);
  ix86_expand_sse_unpack (hop1, qop1, uns_p, false);

  if (op2vec)
    {
      hop2 = gen_reg_rtx (himode);
      ix86_expand_sse_unpack (hop2, qop2, uns_p, false);
    }
  else
    hop2 = qop2;

  if (code != MULT && op2vec)
    {
      /* Expand vashr/vlshr/vashl.  */
      hdest = gen_reg_rtx (himode);
      emit_insn (gen_rtx_SET (hdest,
			      simplify_gen_binary (code, himode,
						   hop1, hop2)));
    }
  else
    /* Expand mult/ashr/lshr/ashl.  */
    hdest = expand_simple_binop (himode, code, hop1, hop2,
				 NULL_RTX, 1, OPTAB_DIRECT);

  if (gen_truncate)
    emit_insn (gen_truncate (dest, hdest));
  else
    {
      struct expand_vec_perm_d d;
      rtx wqdest = gen_reg_rtx (wqimode);
      rtx wqres = gen_lowpart (wqimode, hdest);
      bool ok;
      int i;

      /* Merge the data back into the right place.  */
      d.target = wqdest;
      d.op0 = d.op1 = wqres;
      d.vmode = wqimode;
      d.nelt = GET_MODE_NUNITS (wqimode);
      d.one_operand_p = false;
      d.testing_p = false;

      for (i = 0; i < d.nelt; ++i)
	d.perm[i] = i * 2;

      ok = ix86_expand_vec_perm_const_1 (&d);
      gcc_assert (ok);

      emit_move_insn (dest, gen_lowpart (qimode, wqdest));
    }

  return true;
}

/* Expand a vector operation CODE for a V*QImode in terms of the
   same operation on V*HImode.  */

void
ix86_expand_vecop_qihi (enum rtx_code code, rtx dest, rtx op1, rtx op2)
{
  machine_mode qimode = GET_MODE (dest);
  machine_mode himode;
  rtx (*gen_il) (rtx, rtx, rtx);
  rtx (*gen_ih) (rtx, rtx, rtx);
  rtx op1_l, op1_h, op2_l, op2_h, res_l, res_h;
  bool op2vec = GET_MODE_CLASS (GET_MODE (op2)) == MODE_VECTOR_INT;
  struct expand_vec_perm_d d;
  bool full_interleave = true;
  bool uns_p = code != ASHIFTRT;
  bool ok;
  int i;

  if (CONST_INT_P (op2)
      && (code == ASHIFT || code == LSHIFTRT || code == ASHIFTRT)
      && ix86_expand_vec_shift_qihi_constant (code, dest, op1, op2))
    return;

  if (ix86_expand_vecop_qihi2 (code, dest, op1, op2))
    return;

  switch (qimode)
    {
    case E_V16QImode:
      himode = V8HImode;
      break;
    case E_V32QImode:
      himode = V16HImode;
      break;
    case E_V64QImode:
      himode = V32HImode;
      break;
    default:
      gcc_unreachable ();
    }

  switch (code)
    {
    case MULT:
      gcc_assert (op2vec);
      /* Unpack data such that we've got a source byte in each low byte of
	 each word.  We don't care what goes into the high byte of each word.
	 Rather than trying to get zero in there, most convenient is to let
	 it be a copy of the low byte.  */
      switch (qimode)
	{
	case E_V16QImode:
	  gen_il = gen_vec_interleave_lowv16qi;
	  gen_ih = gen_vec_interleave_highv16qi;
	  break;
	case E_V32QImode:
	  gen_il = gen_avx2_interleave_lowv32qi;
	  gen_ih = gen_avx2_interleave_highv32qi;
	  full_interleave = false;
	  break;
	case E_V64QImode:
	  gen_il = gen_avx512bw_interleave_lowv64qi;
	  gen_ih = gen_avx512bw_interleave_highv64qi;
	  full_interleave = false;
	  break;
	default:
	  gcc_unreachable ();
	}

      op2_l = gen_reg_rtx (qimode);
      op2_h = gen_reg_rtx (qimode);
      emit_insn (gen_il (op2_l, op2, op2));
      emit_insn (gen_ih (op2_h, op2, op2));

      op1_l = gen_reg_rtx (qimode);
      op1_h = gen_reg_rtx (qimode);
      emit_insn (gen_il (op1_l, op1, op1));
      emit_insn (gen_ih (op1_h, op1, op1));
      break;

    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      op1_l = gen_reg_rtx (himode);
      op1_h = gen_reg_rtx (himode);
      ix86_expand_sse_unpack (op1_l, op1, uns_p, false);
      ix86_expand_sse_unpack (op1_h, op1, uns_p, true);
      /* vashr/vlshr/vashl  */
      if (op2vec)
	{
	  rtx tmp = force_reg (qimode, op2);
	  op2_l = gen_reg_rtx (himode);
	  op2_h = gen_reg_rtx (himode);
	  ix86_expand_sse_unpack (op2_l, tmp, uns_p, false);
	  ix86_expand_sse_unpack (op2_h, tmp, uns_p, true);
	}
      else
	op2_l = op2_h = op2;

      break;
    default:
      gcc_unreachable ();
    }

  if (code != MULT && op2vec)
    {
      /* Expand vashr/vlshr/vashl.  */
      res_l = gen_reg_rtx (himode);
      res_h = gen_reg_rtx (himode);
      emit_insn (gen_rtx_SET (res_l,
			      simplify_gen_binary (code, himode,
						   op1_l, op2_l)));
      emit_insn (gen_rtx_SET (res_h,
			      simplify_gen_binary (code, himode,
						   op1_h, op2_h)));
    }
  else
    {
      /* Expand mult/ashr/lshr/ashl.  */
      res_l = expand_simple_binop (himode, code, op1_l, op2_l, NULL_RTX,
				   1, OPTAB_DIRECT);
      res_h = expand_simple_binop (himode, code, op1_h, op2_h, NULL_RTX,
				   1, OPTAB_DIRECT);
    }

  gcc_assert (res_l && res_h);

  /* Merge the data back into the right place.  */
  d.target = dest;
  d.op0 = gen_lowpart (qimode, res_l);
  d.op1 = gen_lowpart (qimode, res_h);
  d.vmode = qimode;
  d.nelt = GET_MODE_NUNITS (qimode);
  d.one_operand_p = false;
  d.testing_p = false;

  if (full_interleave)
    {
      /* We used the full interleave, the desired
	 results are in the even elements.  */
      for (i = 0; i < d.nelt; ++i)
	d.perm[i] = i * 2;
    }
  else
    {
      /* For AVX, the interleave used above was not cross-lane.  So the
	 extraction is evens but with the second and third quarter swapped.
	 Happily, that is even one insn shorter than even extraction.
	 For AVX512BW we have 4 lanes.  We extract evens from within a lane,
	 always first from the first and then from the second source operand,
	 the index bits above the low 4 bits remains the same.
	 Thus, for d.nelt == 32 we want permutation
	 0,2,4,..14, 32,34,36,..46, 16,18,20,..30, 48,50,52,..62
	 and for d.nelt == 64 we want permutation
	 0,2,4,..14, 64,66,68,..78, 16,18,20,..30, 80,82,84,..94,
	 32,34,36,..46, 96,98,100,..110, 48,50,52,..62, 112,114,116,..126.  */
      for (i = 0; i < d.nelt; ++i)
	d.perm[i] = ((i * 2) & 14) + ((i & 8) ? d.nelt : 0) + (i & ~15);
    }

  ok = ix86_expand_vec_perm_const_1 (&d);
  gcc_assert (ok);
}

/* Helper function of ix86_expand_mul_widen_evenodd.  Return true
   if op is CONST_VECTOR with all odd elements equal to their
   preceding element.  */

static bool
const_vector_equal_evenodd_p (rtx op)
{
  machine_mode mode = GET_MODE (op);
  int i, nunits = GET_MODE_NUNITS (mode);
  if (GET_CODE (op) != CONST_VECTOR
      || nunits != CONST_VECTOR_NUNITS (op))
    return false;
  for (i = 0; i < nunits; i += 2)
    if (CONST_VECTOR_ELT (op, i) != CONST_VECTOR_ELT (op, i + 1))
      return false;
  return true;
}

void
ix86_expand_mul_widen_evenodd (rtx dest, rtx op1, rtx op2,
			       bool uns_p, bool odd_p)
{
  machine_mode mode = GET_MODE (op1);
  machine_mode wmode = GET_MODE (dest);
  rtx x;
  rtx orig_op1 = op1, orig_op2 = op2;

  if (!nonimmediate_operand (op1, mode))
    op1 = force_reg (mode, op1);
  if (!nonimmediate_operand (op2, mode))
    op2 = force_reg (mode, op2);

  /* We only play even/odd games with vectors of SImode.  */
  gcc_assert (mode == V4SImode || mode == V8SImode || mode == V16SImode);

  /* If we're looking for the odd results, shift those members down to
     the even slots.  For some cpus this is faster than a PSHUFD.  */
  if (odd_p)
    {
      /* For XOP use vpmacsdqh, but only for smult, as it is only
	 signed.  */
      if (TARGET_XOP && mode == V4SImode && !uns_p)
	{
	  x = force_reg (wmode, CONST0_RTX (wmode));
	  emit_insn (gen_xop_pmacsdqh (dest, op1, op2, x));
	  return;
	}

      x = GEN_INT (GET_MODE_UNIT_BITSIZE (mode));
      if (!const_vector_equal_evenodd_p (orig_op1))
	op1 = expand_binop (wmode, lshr_optab, gen_lowpart (wmode, op1),
			    x, NULL, 1, OPTAB_DIRECT);
      if (!const_vector_equal_evenodd_p (orig_op2))
	op2 = expand_binop (wmode, lshr_optab, gen_lowpart (wmode, op2),
			    x, NULL, 1, OPTAB_DIRECT);
      op1 = gen_lowpart (mode, op1);
      op2 = gen_lowpart (mode, op2);
    }

  if (mode == V16SImode)
    {
      if (uns_p)
	x = gen_vec_widen_umult_even_v16si (dest, op1, op2);
      else
	x = gen_vec_widen_smult_even_v16si (dest, op1, op2);
    }
  else if (mode == V8SImode)
    {
      if (uns_p)
	x = gen_vec_widen_umult_even_v8si (dest, op1, op2);
      else
	x = gen_vec_widen_smult_even_v8si (dest, op1, op2);
    }
  else if (uns_p)
    x = gen_vec_widen_umult_even_v4si (dest, op1, op2);
  else if (TARGET_SSE4_1)
    x = gen_sse4_1_mulv2siv2di3 (dest, op1, op2);
  else
    {
      rtx s1, s2, t0, t1, t2;

      /* The easiest way to implement this without PMULDQ is to go through
	 the motions as if we are performing a full 64-bit multiply.  With
	 the exception that we need to do less shuffling of the elements.  */

      /* Compute the sign-extension, aka highparts, of the two operands.  */
      s1 = ix86_expand_sse_cmp (gen_reg_rtx (mode), GT, CONST0_RTX (mode),
				op1, pc_rtx, pc_rtx);
      s2 = ix86_expand_sse_cmp (gen_reg_rtx (mode), GT, CONST0_RTX (mode),
				op2, pc_rtx, pc_rtx);

      /* Multiply LO(A) * HI(B), and vice-versa.  */
      t1 = gen_reg_rtx (wmode);
      t2 = gen_reg_rtx (wmode);
      emit_insn (gen_vec_widen_umult_even_v4si (t1, s1, op2));
      emit_insn (gen_vec_widen_umult_even_v4si (t2, s2, op1));

      /* Multiply LO(A) * LO(B).  */
      t0 = gen_reg_rtx (wmode);
      emit_insn (gen_vec_widen_umult_even_v4si (t0, op1, op2));

      /* Combine and shift the highparts into place.  */
      t1 = expand_binop (wmode, add_optab, t1, t2, t1, 1, OPTAB_DIRECT);
      t1 = expand_binop (wmode, ashl_optab, t1, GEN_INT (32), t1,
			 1, OPTAB_DIRECT);

      /* Combine high and low parts.  */
      force_expand_binop (wmode, add_optab, t0, t1, dest, 1, OPTAB_DIRECT);
      return;
    }
  emit_insn (x);
}

void
ix86_expand_mul_widen_hilo (rtx dest, rtx op1, rtx op2,
			    bool uns_p, bool high_p)
{
  machine_mode wmode = GET_MODE (dest);
  machine_mode mode = GET_MODE (op1);
  rtx t1, t2, t3, t4, mask;

  switch (mode)
    {
    case E_V4SImode:
      t1 = gen_reg_rtx (mode);
      t2 = gen_reg_rtx (mode);
      if (TARGET_XOP && !uns_p)
	{
	  /* With XOP, we have pmacsdqh, aka mul_widen_odd.  In this case,
	     shuffle the elements once so that all elements are in the right
	     place for immediate use: { A C B D }.  */
	  emit_insn (gen_sse2_pshufd_1 (t1, op1, const0_rtx, const2_rtx,
					const1_rtx, GEN_INT (3)));
	  emit_insn (gen_sse2_pshufd_1 (t2, op2, const0_rtx, const2_rtx,
					const1_rtx, GEN_INT (3)));
	}
      else
	{
	  /* Put the elements into place for the multiply.  */
	  ix86_expand_vec_interleave (t1, op1, op1, high_p);
	  ix86_expand_vec_interleave (t2, op2, op2, high_p);
	  high_p = false;
	}
      ix86_expand_mul_widen_evenodd (dest, t1, t2, uns_p, high_p);
      break;

    case E_V8SImode:
      /* Shuffle the elements between the lanes.  After this we
	 have { A B E F | C D G H } for each operand.  */
      t1 = gen_reg_rtx (V4DImode);
      t2 = gen_reg_rtx (V4DImode);
      emit_insn (gen_avx2_permv4di_1 (t1, gen_lowpart (V4DImode, op1),
				      const0_rtx, const2_rtx,
				      const1_rtx, GEN_INT (3)));
      emit_insn (gen_avx2_permv4di_1 (t2, gen_lowpart (V4DImode, op2),
				      const0_rtx, const2_rtx,
				      const1_rtx, GEN_INT (3)));

      /* Shuffle the elements within the lanes.  After this we
	 have { A A B B | C C D D } or { E E F F | G G H H }.  */
      t3 = gen_reg_rtx (V8SImode);
      t4 = gen_reg_rtx (V8SImode);
      mask = GEN_INT (high_p
		      ? 2 + (2 << 2) + (3 << 4) + (3 << 6)
		      : 0 + (0 << 2) + (1 << 4) + (1 << 6));
      emit_insn (gen_avx2_pshufdv3 (t3, gen_lowpart (V8SImode, t1), mask));
      emit_insn (gen_avx2_pshufdv3 (t4, gen_lowpart (V8SImode, t2), mask));

      ix86_expand_mul_widen_evenodd (dest, t3, t4, uns_p, false);
      break;

    case E_V8HImode:
    case E_V16HImode:
      t1 = expand_binop (mode, smul_optab, op1, op2, NULL_RTX,
			 uns_p, OPTAB_DIRECT);
      t2 = expand_binop (mode,
			 uns_p ? umul_highpart_optab : smul_highpart_optab,
			 op1, op2, NULL_RTX, uns_p, OPTAB_DIRECT);
      gcc_assert (t1 && t2);

      t3 = gen_reg_rtx (mode);
      ix86_expand_vec_interleave (t3, t1, t2, high_p);
      emit_move_insn (dest, gen_lowpart (wmode, t3));
      break;

    case E_V16QImode:
    case E_V32QImode:
    case E_V32HImode:
    case E_V16SImode:
    case E_V64QImode:
      t1 = gen_reg_rtx (wmode);
      t2 = gen_reg_rtx (wmode);
      ix86_expand_sse_unpack (t1, op1, uns_p, high_p);
      ix86_expand_sse_unpack (t2, op2, uns_p, high_p);

      emit_insn (gen_rtx_SET (dest, gen_rtx_MULT (wmode, t1, t2)));
      break;

    default:
      gcc_unreachable ();
    }
}

void
ix86_expand_sse2_mulv4si3 (rtx op0, rtx op1, rtx op2)
{
  rtx res_1, res_2, res_3, res_4;

  res_1 = gen_reg_rtx (V4SImode);
  res_2 = gen_reg_rtx (V4SImode);
  res_3 = gen_reg_rtx (V2DImode);
  res_4 = gen_reg_rtx (V2DImode);
  ix86_expand_mul_widen_evenodd (res_3, op1, op2, true, false);
  ix86_expand_mul_widen_evenodd (res_4, op1, op2, true, true);

  /* Move the results in element 2 down to element 1; we don't care
     what goes in elements 2 and 3.  Then we can merge the parts
     back together with an interleave.

     Note that two other sequences were tried:
     (1) Use interleaves at the start instead of psrldq, which allows
     us to use a single shufps to merge things back at the end.
     (2) Use shufps here to combine the two vectors, then pshufd to
     put the elements in the correct order.
     In both cases the cost of the reformatting stall was too high
     and the overall sequence slower.  */

  emit_insn (gen_sse2_pshufd_1 (res_1, gen_lowpart (V4SImode, res_3),
				const0_rtx, const2_rtx,
				const0_rtx, const0_rtx));
  emit_insn (gen_sse2_pshufd_1 (res_2, gen_lowpart (V4SImode, res_4),
				const0_rtx, const2_rtx,
				const0_rtx, const0_rtx));
  res_1 = emit_insn (gen_vec_interleave_lowv4si (op0, res_1, res_2));

  set_unique_reg_note (res_1, REG_EQUAL, gen_rtx_MULT (V4SImode, op1, op2));
}

void
ix86_expand_sse2_mulvxdi3 (rtx op0, rtx op1, rtx op2)
{
  machine_mode mode = GET_MODE (op0);
  rtx t1, t2, t3, t4, t5, t6;

  if (TARGET_AVX512DQ && TARGET_EVEX512 && mode == V8DImode)
    emit_insn (gen_avx512dq_mulv8di3 (op0, op1, op2));
  else if (TARGET_AVX512DQ && TARGET_AVX512VL && mode == V4DImode)
    emit_insn (gen_avx512dq_mulv4di3 (op0, op1, op2));
  else if (TARGET_AVX512DQ && TARGET_AVX512VL && mode == V2DImode)
    emit_insn (gen_avx512dq_mulv2di3 (op0, op1, op2));
  else if (TARGET_XOP && mode == V2DImode)
    {
      /* op1: A,B,C,D, op2: E,F,G,H */
      op1 = gen_lowpart (V4SImode, op1);
      op2 = gen_lowpart (V4SImode, op2);

      t1 = gen_reg_rtx (V4SImode);
      t2 = gen_reg_rtx (V4SImode);
      t3 = gen_reg_rtx (V2DImode);
      t4 = gen_reg_rtx (V2DImode);

      /* t1: B,A,D,C */
      emit_insn (gen_sse2_pshufd_1 (t1, op1,
				    GEN_INT (1),
				    GEN_INT (0),
				    GEN_INT (3),
				    GEN_INT (2)));

      /* t2: (B*E),(A*F),(D*G),(C*H) */
      emit_insn (gen_mulv4si3 (t2, t1, op2));

      /* t3: (B*E)+(A*F), (D*G)+(C*H) */
      emit_insn (gen_xop_phadddq (t3, t2));

      /* t4: ((B*E)+(A*F))<<32, ((D*G)+(C*H))<<32 */
      emit_insn (gen_ashlv2di3 (t4, t3, GEN_INT (32)));

      /* Multiply lower parts and add all */
      t5 = gen_reg_rtx (V2DImode);
      emit_insn (gen_vec_widen_umult_even_v4si (t5, 
					gen_lowpart (V4SImode, op1),
					gen_lowpart (V4SImode, op2)));
      force_expand_binop (mode, add_optab, t5, t4, op0, 1, OPTAB_DIRECT);
    }
  else
    {
      machine_mode nmode;
      rtx (*umul) (rtx, rtx, rtx);

      if (mode == V2DImode)
	{
	  umul = gen_vec_widen_umult_even_v4si;
	  nmode = V4SImode;
	}
      else if (mode == V4DImode)
	{
	  umul = gen_vec_widen_umult_even_v8si;
	  nmode = V8SImode;
	}
      else if (mode == V8DImode)
	{
	  umul = gen_vec_widen_umult_even_v16si;
	  nmode = V16SImode;
	}
      else
	gcc_unreachable ();


      /* Multiply low parts.  */
      t1 = gen_reg_rtx (mode);
      emit_insn (umul (t1, gen_lowpart (nmode, op1), gen_lowpart (nmode, op2)));

      /* Shift input vectors right 32 bits so we can multiply high parts.  */
      t6 = GEN_INT (32);
      t2 = expand_binop (mode, lshr_optab, op1, t6, NULL, 1, OPTAB_DIRECT);
      t3 = expand_binop (mode, lshr_optab, op2, t6, NULL, 1, OPTAB_DIRECT);

      /* Multiply high parts by low parts.  */
      t4 = gen_reg_rtx (mode);
      t5 = gen_reg_rtx (mode);
      emit_insn (umul (t4, gen_lowpart (nmode, t2), gen_lowpart (nmode, op2)));
      emit_insn (umul (t5, gen_lowpart (nmode, t3), gen_lowpart (nmode, op1)));

      /* Combine and shift the highparts back.  */
      t4 = expand_binop (mode, add_optab, t4, t5, t4, 1, OPTAB_DIRECT);
      t4 = expand_binop (mode, ashl_optab, t4, t6, t4, 1, OPTAB_DIRECT);

      /* Combine high and low parts.  */
      force_expand_binop (mode, add_optab, t1, t4, op0, 1, OPTAB_DIRECT);
    }

  set_unique_reg_note (get_last_insn (), REG_EQUAL,
		       gen_rtx_MULT (mode, op1, op2));
}

/* Return 1 if control tansfer instruction INSN
   should be encoded with notrack prefix.  */

bool
ix86_notrack_prefixed_insn_p (rtx_insn *insn)
{
  if (!insn || !((flag_cf_protection & CF_BRANCH)))
    return false;

  if (CALL_P (insn))
    {
      rtx call = get_call_rtx_from (insn);
      gcc_assert (call != NULL_RTX);
      rtx addr = XEXP (call, 0);

      /* Do not emit 'notrack' if it's not an indirect call.  */
      if (MEM_P (addr)
	  && GET_CODE (XEXP (addr, 0)) == SYMBOL_REF)
	return false;
      else
	return find_reg_note (insn, REG_CALL_NOCF_CHECK, 0);
    }

  if (JUMP_P (insn) && !flag_cet_switch)
    {
      rtx target = JUMP_LABEL (insn);
      if (target == NULL_RTX || ANY_RETURN_P (target))
	return false;

      /* Check the jump is a switch table.  */
      rtx_insn *label = as_a<rtx_insn *> (target);
      rtx_insn *table = next_insn (label);
      if (table == NULL_RTX || !JUMP_TABLE_DATA_P (table))
	return false;
      else
	return true;
    }
  return false;
}

/* Calculate integer abs() using only SSE2 instructions.  */

void
ix86_expand_sse2_abs (rtx target, rtx input)
{
  machine_mode mode = GET_MODE (target);
  rtx tmp0, tmp1, x;

  switch (mode)
    {
    case E_V2DImode:
    case E_V4DImode:
      /* For 64-bit signed integer X, with SSE4.2 use
	 pxor t0, t0; pcmpgtq X, t0; pxor t0, X; psubq t0, X.
	 Otherwise handle it similarly to V4SImode, except use 64 as W instead of
	 32 and use logical instead of arithmetic right shift (which is
	 unimplemented) and subtract.  */
      if (TARGET_SSE4_2)
	{
	  tmp0 = gen_reg_rtx (mode);
	  tmp1 = gen_reg_rtx (mode);
	  emit_move_insn (tmp1, CONST0_RTX (mode));
	  if (mode == E_V2DImode)
	    emit_insn (gen_sse4_2_gtv2di3 (tmp0, tmp1, input));
	  else
	    emit_insn (gen_avx2_gtv4di3 (tmp0, tmp1, input));
	}
      else
	{
	  tmp0 = expand_simple_binop (mode, LSHIFTRT, input,
				      GEN_INT (GET_MODE_UNIT_BITSIZE (mode)
					       - 1), NULL, 0, OPTAB_DIRECT);
	  tmp0 = expand_simple_unop (mode, NEG, tmp0, NULL, false);
	}

      tmp1 = expand_simple_binop (mode, XOR, tmp0, input,
				  NULL, 0, OPTAB_DIRECT);
      x = expand_simple_binop (mode, MINUS, tmp1, tmp0,
			       target, 0, OPTAB_DIRECT);
      break;

    case E_V4SImode:
      /* For 32-bit signed integer X, the best way to calculate the absolute
	 value of X is (((signed) X >> (W-1)) ^ X) - ((signed) X >> (W-1)).  */
      tmp0 = expand_simple_binop (mode, ASHIFTRT, input,
				  GEN_INT (GET_MODE_UNIT_BITSIZE (mode) - 1),
				  NULL, 0, OPTAB_DIRECT);
      tmp1 = expand_simple_binop (mode, XOR, tmp0, input,
				  NULL, 0, OPTAB_DIRECT);
      x = expand_simple_binop (mode, MINUS, tmp1, tmp0,
			       target, 0, OPTAB_DIRECT);
      break;

    case E_V8HImode:
      /* For 16-bit signed integer X, the best way to calculate the absolute
	 value of X is max (X, -X), as SSE2 provides the PMAXSW insn.  */
      tmp0 = expand_unop (mode, neg_optab, input, NULL_RTX, 0);

      x = expand_simple_binop (mode, SMAX, tmp0, input,
			       target, 0, OPTAB_DIRECT);
      break;

    case E_V16QImode:
      /* For 8-bit signed integer X, the best way to calculate the absolute
	 value of X is min ((unsigned char) X, (unsigned char) (-X)),
	 as SSE2 provides the PMINUB insn.  */
      tmp0 = expand_unop (mode, neg_optab, input, NULL_RTX, 0);

      x = expand_simple_binop (V16QImode, UMIN, tmp0, input,
			       target, 0, OPTAB_DIRECT);
      break;

    default:
      gcc_unreachable ();
    }

  if (x != target)
    emit_move_insn (target, x);
}

/* Expand an extract from a vector register through pextr insn.
   Return true if successful.  */

bool
ix86_expand_pextr (rtx *operands)
{
  rtx dst = operands[0];
  rtx src = operands[1];

  unsigned int size = INTVAL (operands[2]);
  unsigned int pos = INTVAL (operands[3]);

  if (SUBREG_P (dst))
    {
      /* Reject non-lowpart subregs.  */
      if (SUBREG_BYTE (dst) > 0)
	return false;
      dst = SUBREG_REG (dst);
    }
	
  if (SUBREG_P (src))
    {
      pos += SUBREG_BYTE (src) * BITS_PER_UNIT;
      src = SUBREG_REG (src);
    }

  switch (GET_MODE (src))
    {
    case E_V16QImode:
    case E_V8HImode:
    case E_V4SImode:
    case E_V2DImode:
    case E_V1TImode:
      {
	machine_mode srcmode, dstmode;
	rtx d, pat;

	if (!int_mode_for_size (size, 0).exists (&dstmode))
	  return false;

	switch (dstmode)
	  {
	  case E_QImode:
	    if (!TARGET_SSE4_1)
	      return false;
	    srcmode = V16QImode;
	    break;

	  case E_HImode:
	    if (!TARGET_SSE2)
	      return false;
	    srcmode = V8HImode;
	    break;

	  case E_SImode:
	    if (!TARGET_SSE4_1)
	      return false;
	    srcmode = V4SImode;
	    break;

	  case E_DImode:
	    gcc_assert (TARGET_64BIT);
	    if (!TARGET_SSE4_1)
	      return false;
	    srcmode = V2DImode;
	    break;

	  default:
	    return false;
	  }

	/* Reject extractions from misaligned positions.  */
	if (pos & (size-1))
	  return false;

	if (GET_MODE (dst) == dstmode)
	  d = dst;
	else
	  d = gen_reg_rtx (dstmode);

	/* Construct insn pattern.  */
	pat = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (1, GEN_INT (pos / size)));
	pat = gen_rtx_VEC_SELECT (dstmode, gen_lowpart (srcmode, src), pat);

	/* Let the rtl optimizers know about the zero extension performed.  */
	if (dstmode == QImode || dstmode == HImode)
	  {
	    pat = gen_rtx_ZERO_EXTEND (SImode, pat);
	    d = gen_lowpart (SImode, d);
	  }

	emit_insn (gen_rtx_SET (d, pat));

	if (d != dst)
	  emit_move_insn (dst, gen_lowpart (GET_MODE (dst), d));
	return true;
      }

    default:
      return false;
    }
}

/* Expand an insert into a vector register through pinsr insn.
   Return true if successful.  */

bool
ix86_expand_pinsr (rtx *operands)
{
  rtx dst = operands[0];
  rtx src = operands[3];

  unsigned int size = INTVAL (operands[1]);
  unsigned int pos = INTVAL (operands[2]);

  if (SUBREG_P (dst))
    {
      pos += SUBREG_BYTE (dst) * BITS_PER_UNIT;
      dst = SUBREG_REG (dst);
    }

  switch (GET_MODE (dst))
    {
    case E_V16QImode:
    case E_V8HImode:
    case E_V4SImode:
    case E_V2DImode:
    case E_V1TImode:
      {
	machine_mode srcmode, dstmode;
	rtx (*pinsr)(rtx, rtx, rtx, rtx);
	rtx d;

	if (!int_mode_for_size (size, 0).exists (&srcmode))
	  return false;

	switch (srcmode)
	  {
	  case E_QImode:
	    if (!TARGET_SSE4_1)
	      return false;
	    dstmode = V16QImode;
	    pinsr = gen_sse4_1_pinsrb;
	    break;

	  case E_HImode:
	    if (!TARGET_SSE2)
	      return false;
	    dstmode = V8HImode;
	    pinsr = gen_sse2_pinsrw;
	    break;

	  case E_SImode:
	    if (!TARGET_SSE4_1)
	      return false;
	    dstmode = V4SImode;
	    pinsr = gen_sse4_1_pinsrd;
	    break;

	  case E_DImode:
	    gcc_assert (TARGET_64BIT);
	    if (!TARGET_SSE4_1)
	      return false;
	    dstmode = V2DImode;
	    pinsr = gen_sse4_1_pinsrq;
	    break;

	  default:
	    return false;
	  }

	/* Reject insertions to misaligned positions.  */
	if (pos & (size-1))
	  return false;

	if (SUBREG_P (src))
	  {
	    unsigned int srcpos = SUBREG_BYTE (src);

	    if (srcpos > 0)
	      {
		rtx extr_ops[4];

		extr_ops[0] = gen_reg_rtx (srcmode);
		extr_ops[1] = gen_lowpart (srcmode, SUBREG_REG (src));
		extr_ops[2] = GEN_INT (size);
		extr_ops[3] = GEN_INT (srcpos * BITS_PER_UNIT);

		if (!ix86_expand_pextr (extr_ops))
		  return false;

		src = extr_ops[0];
	      }
	    else
	      src = gen_lowpart (srcmode, SUBREG_REG (src));
	  }

	if (GET_MODE (dst) == dstmode)
	  d = dst;
	else
	  d = gen_reg_rtx (dstmode);

	emit_insn (pinsr (d, gen_lowpart (dstmode, dst),
			  gen_lowpart (srcmode, src),
			  GEN_INT (1 << (pos / size))));
	if (d != dst)
	  emit_move_insn (dst, gen_lowpart (GET_MODE (dst), d));
	return true;
      }

    default:
      return false;
    }
}

/* All CPUs prefer to avoid cross-lane operations so perform reductions
   upper against lower halves up to SSE reg size.  */

machine_mode
ix86_split_reduction (machine_mode mode)
{
  /* Reduce lowpart against highpart until we reach SSE reg width to
     avoid cross-lane operations.  */
  switch (mode)
    {
    case E_V8DImode:
    case E_V4DImode:
      return V2DImode;
    case E_V16SImode:
    case E_V8SImode:
      return V4SImode;
    case E_V32HImode:
    case E_V16HImode:
      return V8HImode;
    case E_V64QImode:
    case E_V32QImode:
      return V16QImode;
    case E_V16SFmode:
    case E_V8SFmode:
      return V4SFmode;
    case E_V8DFmode:
    case E_V4DFmode:
      return V2DFmode;
    default:
      return mode;
    }
}

/* Generate call to __divmoddi4.  */

void
ix86_expand_divmod_libfunc (rtx libfunc, machine_mode mode,
			    rtx op0, rtx op1,
			    rtx *quot_p, rtx *rem_p)
{
  rtx rem = assign_386_stack_local (mode, SLOT_TEMP);

  rtx quot = emit_library_call_value (libfunc, NULL_RTX, LCT_NORMAL,
				      mode, op0, mode, op1, mode,
				      XEXP (rem, 0), Pmode);
  *quot_p = quot;
  *rem_p = rem;
}

void
ix86_expand_atomic_fetch_op_loop (rtx target, rtx mem, rtx val,
				  enum rtx_code code, bool after,
				  bool doubleword)
{
  rtx old_reg, new_reg, old_mem, success;
  machine_mode mode = GET_MODE (target);
  rtx_code_label *loop_label = NULL;

  old_reg = gen_reg_rtx (mode);
  new_reg = old_reg;
  old_mem = copy_to_reg (mem);
  loop_label = gen_label_rtx ();
  emit_label (loop_label);
  emit_move_insn (old_reg, old_mem);

  /* return value for atomic_fetch_op.  */
  if (!after)
    emit_move_insn (target, old_reg);

  if (code == NOT)
    {
      new_reg = expand_simple_binop (mode, AND, new_reg, val, NULL_RTX,
				     true, OPTAB_LIB_WIDEN);
      new_reg = expand_simple_unop (mode, code, new_reg, NULL_RTX, true);
    }
  else
    new_reg = expand_simple_binop (mode, code, new_reg, val, NULL_RTX,
				   true, OPTAB_LIB_WIDEN);

  /* return value for atomic_op_fetch.  */
  if (after)
    emit_move_insn (target, new_reg);

  success = NULL_RTX;

  ix86_expand_cmpxchg_loop (&success, old_mem, mem, old_reg, new_reg,
			    gen_int_mode (MEMMODEL_SYNC_SEQ_CST,
					  SImode),
			    doubleword, loop_label);
}

/* Relax cmpxchg instruction, param loop_label indicates whether
   the instruction should be relaxed with a pause loop.  If not,
   it will be relaxed to an atomic load + compare, and skip
   cmpxchg instruction if mem != exp_input.  */

void
ix86_expand_cmpxchg_loop (rtx *ptarget_bool, rtx target_val,
			  rtx mem, rtx exp_input, rtx new_input,
			  rtx mem_model, bool doubleword,
			  rtx_code_label *loop_label)
{
  rtx_code_label *cmp_label = NULL;
  rtx_code_label *done_label = NULL;
  rtx target_bool = NULL_RTX, new_mem = NULL_RTX;
  rtx (*gen) (rtx, rtx, rtx, rtx, rtx) = NULL;
  rtx (*gendw) (rtx, rtx, rtx, rtx, rtx, rtx) = NULL;
  machine_mode mode = GET_MODE (target_val), hmode = mode;

  if (*ptarget_bool == NULL)
    target_bool = gen_reg_rtx (QImode);
  else
    target_bool = *ptarget_bool;

  cmp_label = gen_label_rtx ();
  done_label = gen_label_rtx ();

  new_mem = gen_reg_rtx (mode);
  /* Load memory first.  */
  expand_atomic_load (new_mem, mem, MEMMODEL_SEQ_CST);

  switch (mode)
    {
    case E_TImode:
      gendw = gen_atomic_compare_and_swapti_doubleword;
      hmode = DImode;
      break;
    case E_DImode:
      if (doubleword)
	{
	  gendw = gen_atomic_compare_and_swapdi_doubleword;
	  hmode = SImode;
	}
      else
	gen = gen_atomic_compare_and_swapdi_1;
      break;
    case E_SImode:
      gen = gen_atomic_compare_and_swapsi_1;
      break;
    case E_HImode:
      gen = gen_atomic_compare_and_swaphi_1;
      break;
    case E_QImode:
      gen = gen_atomic_compare_and_swapqi_1;
      break;
    default:
      gcc_unreachable ();
    }

  /* Compare mem value with expected value.  */
  if (doubleword)
    {
      rtx low_new_mem = gen_lowpart (hmode, new_mem);
      rtx low_exp_input = gen_lowpart (hmode, exp_input);
      rtx high_new_mem = gen_highpart (hmode, new_mem);
      rtx high_exp_input = gen_highpart (hmode, exp_input);
      emit_cmp_and_jump_insns (low_new_mem, low_exp_input, NE, NULL_RTX,
			       hmode, 1, cmp_label,
			       profile_probability::guessed_never ());
      emit_cmp_and_jump_insns (high_new_mem, high_exp_input, NE, NULL_RTX,
			       hmode, 1, cmp_label,
			       profile_probability::guessed_never ());
    }
  else
    emit_cmp_and_jump_insns (new_mem, exp_input, NE, NULL_RTX,
			     GET_MODE (exp_input), 1, cmp_label,
			     profile_probability::guessed_never ());

  /* Directly emits cmpxchg here.  */
  if (doubleword)
    emit_insn (gendw (target_val, mem, exp_input,
		      gen_lowpart (hmode, new_input),
		      gen_highpart (hmode, new_input),
		      mem_model));
  else
    emit_insn (gen (target_val, mem, exp_input, new_input, mem_model));

  if (!loop_label)
  {
    emit_jump_insn (gen_jump (done_label));
    emit_barrier ();
    emit_label (cmp_label);
    emit_move_insn (target_val, new_mem);
    emit_label (done_label);
    ix86_expand_setcc (target_bool, EQ, gen_rtx_REG (CCZmode, FLAGS_REG),
		       const0_rtx);
  }
  else
  {
    ix86_expand_setcc (target_bool, EQ, gen_rtx_REG (CCZmode, FLAGS_REG),
		       const0_rtx);
    emit_cmp_and_jump_insns (target_bool, const0_rtx, EQ, const0_rtx,
			     GET_MODE (target_bool), 1, loop_label,
			     profile_probability::guessed_never ());
    emit_jump_insn (gen_jump (done_label));
    emit_barrier ();

    /* If mem is not expected, pause and loop back.  */
    emit_label (cmp_label);
    emit_move_insn (target_val, new_mem);
    emit_insn (gen_pause ());
    emit_jump_insn (gen_jump (loop_label));
    emit_barrier ();
    emit_label (done_label);
  }

  *ptarget_bool = target_bool;
}

/* Convert a BFmode VAL to SFmode without signaling sNaNs.
   This is done by returning SF SUBREG of ((HI SUBREG) (VAL)) << 16.  */

rtx
ix86_expand_fast_convert_bf_to_sf (rtx val)
{
  rtx op = gen_lowpart (HImode, val), ret;
  if (CONST_INT_P (op))
    {
      ret = simplify_const_unary_operation (FLOAT_EXTEND, SFmode,
					    val, BFmode);
      if (ret)
	return ret;
      /* FLOAT_EXTEND simplification will fail if VAL is a sNaN.  */
      ret = gen_reg_rtx (SImode);
      emit_move_insn (ret, GEN_INT (INTVAL (op) & 0xffff));
      emit_insn (gen_ashlsi3 (ret, ret, GEN_INT (16)));
      return gen_lowpart (SFmode, ret);
    }

  ret = gen_reg_rtx (SFmode);
  emit_insn (gen_extendbfsf2_1 (ret, force_reg (BFmode, val)));
  return ret;
}

#include "gt-i386-expand.h"
