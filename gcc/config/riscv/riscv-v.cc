/* Subroutines used for code generation for RISC-V 'V' Extension for
   GNU compiler.
   Copyright (C) 2022-2023 Free Software Foundation, Inc.
   Contributed by Juzhe Zhong (juzhe.zhong@rivai.ai), RiVAI Technologies Ltd.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "backend.h"
#include "rtl.h"
#include "insn-config.h"
#include "insn-attr.h"
#include "recog.h"
#include "alias.h"
#include "tree.h"
#include "stringpool.h"
#include "attribs.h"
#include "explow.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "tm_p.h"
#include "target.h"
#include "expr.h"
#include "optabs.h"
#include "tm-constrs.h"
#include "rtx-vector-builder.h"

using namespace riscv_vector;

namespace riscv_vector {

template <int MAX_OPERANDS> class insn_expander
{
public:
  insn_expander () : m_opno (0) {}
  void add_output_operand (rtx x, machine_mode mode)
  {
    create_output_operand (&m_ops[m_opno++], x, mode);
    gcc_assert (m_opno <= MAX_OPERANDS);
  }
  void add_input_operand (rtx x, machine_mode mode)
  {
    create_input_operand (&m_ops[m_opno++], x, mode);
    gcc_assert (m_opno <= MAX_OPERANDS);
  }
  void add_all_one_mask_operand (machine_mode mode)
  {
    add_input_operand (CONSTM1_RTX (mode), mode);
  }
  void add_vundef_operand (machine_mode mode)
  {
    add_input_operand (RVV_VUNDEF (mode), mode);
  }
  void add_policy_operand (enum tail_policy vta, enum mask_policy vma)
  {
    rtx tail_policy_rtx = gen_int_mode (vta, Pmode);
    rtx mask_policy_rtx = gen_int_mode (vma, Pmode);
    add_input_operand (tail_policy_rtx, Pmode);
    add_input_operand (mask_policy_rtx, Pmode);
  }
  void add_avl_type_operand (avl_type type)
  {
    add_input_operand (gen_int_mode (type, Pmode), Pmode);
  }

  void expand (enum insn_code icode, bool temporary_volatile_p = false)
  {
    if (temporary_volatile_p)
      {
	temporary_volatile_ok v (true);
	expand_insn (icode, m_opno, m_ops);
      }
    else
      expand_insn (icode, m_opno, m_ops);
  }

private:
  int m_opno;
  expand_operand m_ops[MAX_OPERANDS];
};

static unsigned
get_sew (machine_mode mode)
{
  unsigned int sew = GET_MODE_CLASS (mode) == MODE_VECTOR_BOOL
		       ? 8
		       : GET_MODE_BITSIZE (GET_MODE_INNER (mode));
  return sew;
}

/* Return true if X is a const_vector with all duplicate elements, which is in
   the range between MINVAL and MAXVAL.  */
bool
const_vec_all_same_in_range_p (rtx x, HOST_WIDE_INT minval,
			       HOST_WIDE_INT maxval)
{
  rtx elt;
  return (const_vec_duplicate_p (x, &elt) && CONST_INT_P (elt)
	  && IN_RANGE (INTVAL (elt), minval, maxval));
}

/* Emit a vlmax vsetvl instruction.  This should only be used when
   optimization is disabled or after vsetvl insertion pass.  */
void
emit_hard_vlmax_vsetvl (machine_mode vmode, rtx vl)
{
  unsigned int sew = get_sew (vmode);
  emit_insn (gen_vsetvl (Pmode, vl, RVV_VLMAX, gen_int_mode (sew, Pmode),
			 gen_int_mode (get_vlmul (vmode), Pmode), const0_rtx,
			 const0_rtx));
}

void
emit_vlmax_vsetvl (machine_mode vmode, rtx vl)
{
  unsigned int sew = get_sew (vmode);
  enum vlmul_type vlmul = get_vlmul (vmode);
  unsigned int ratio = calculate_ratio (sew, vlmul);

  if (!optimize)
    emit_hard_vlmax_vsetvl (vmode, vl);
  else
    emit_insn (gen_vlmax_avl (Pmode, vl, gen_int_mode (ratio, Pmode)));
}

/* Calculate SEW/LMUL ratio.  */
unsigned int
calculate_ratio (unsigned int sew, enum vlmul_type vlmul)
{
  unsigned int ratio;
  switch (vlmul)
    {
    case LMUL_1:
      ratio = sew;
      break;
    case LMUL_2:
      ratio = sew / 2;
      break;
    case LMUL_4:
      ratio = sew / 4;
      break;
    case LMUL_8:
      ratio = sew / 8;
      break;
    case LMUL_F8:
      ratio = sew * 8;
      break;
    case LMUL_F4:
      ratio = sew * 4;
      break;
    case LMUL_F2:
      ratio = sew * 2;
      break;
    default:
      gcc_unreachable ();
    }
  return ratio;
}

/* Emit an RVV unmask && vl mov from SRC to DEST.  */
static void
emit_pred_op (unsigned icode, rtx mask, rtx dest, rtx src, rtx len,
	      machine_mode mask_mode, bool vlmax_p)
{
  insn_expander<8> e;
  machine_mode mode = GET_MODE (dest);

  e.add_output_operand (dest, mode);

  if (mask)
    e.add_input_operand (mask, GET_MODE (mask));
  else
    e.add_all_one_mask_operand (mask_mode);

  e.add_vundef_operand (mode);

  e.add_input_operand (src, GET_MODE (src));

  if (len)
    e.add_input_operand (len, Pmode);
  else
    {
      rtx vlmax = gen_reg_rtx (Pmode);
      emit_vlmax_vsetvl (mode, vlmax);
      e.add_input_operand (vlmax, Pmode);
    }

  if (GET_MODE_CLASS (mode) != MODE_VECTOR_BOOL)
    e.add_policy_operand (get_prefer_tail_policy (), get_prefer_mask_policy ());

  if (vlmax_p)
    e.add_avl_type_operand (avl_type::VLMAX);
  else
    e.add_avl_type_operand (avl_type::NONVLMAX);

  e.expand ((enum insn_code) icode, MEM_P (dest) || MEM_P (src));
}

void
emit_vlmax_op (unsigned icode, rtx dest, rtx src, machine_mode mask_mode)
{
  emit_pred_op (icode, NULL_RTX, dest, src, NULL_RTX, mask_mode, true);
}

void
emit_vlmax_op (unsigned icode, rtx dest, rtx src, rtx len,
	       machine_mode mask_mode)
{
  emit_pred_op (icode, NULL_RTX, dest, src, len, mask_mode, true);
}

void
emit_nonvlmax_op (unsigned icode, rtx dest, rtx src, rtx len,
		  machine_mode mask_mode)
{
  emit_pred_op (icode, NULL_RTX, dest, src, len, mask_mode, false);
}

static void
expand_const_vector (rtx target, rtx src, machine_mode mask_mode)
{
  machine_mode mode = GET_MODE (target);
  scalar_mode elt_mode = GET_MODE_INNER (mode);
  if (GET_MODE_CLASS (mode) == MODE_VECTOR_BOOL)
    {
      rtx elt;
      gcc_assert (
	const_vec_duplicate_p (src, &elt)
	&& (rtx_equal_p (elt, const0_rtx) || rtx_equal_p (elt, const1_rtx)));
      emit_vlmax_op (code_for_pred_mov (mode), target, src, mask_mode);
      return;
    }

  rtx elt;
  if (const_vec_duplicate_p (src, &elt))
    {
      rtx tmp = register_operand (target, mode) ? target : gen_reg_rtx (mode);
      /* Element in range -16 ~ 15 integer or 0.0 floating-point,
	 we use vmv.v.i instruction.  */
      if (satisfies_constraint_vi (src) || satisfies_constraint_Wc0 (src))
	emit_vlmax_op (code_for_pred_mov (mode), tmp, src, mask_mode);
      else
	emit_vlmax_op (code_for_pred_broadcast (mode), tmp,
		       force_reg (elt_mode, elt), mask_mode);

      if (tmp != target)
	emit_move_insn (target, tmp);
      return;
    }

  /* TODO: We only support const duplicate vector for now. More cases
     will be supported when we support auto-vectorization:

       1. series vector.
       2. multiple elts duplicate vector.
       3. multiple patterns with multiple elts.  */
}

/* Expand a pre-RA RVV data move from SRC to DEST.
   It expands move for RVV fractional vector modes.  */
bool
legitimize_move (rtx dest, rtx src, machine_mode mask_mode)
{
  machine_mode mode = GET_MODE (dest);
  if (CONST_VECTOR_P (src))
    {
      expand_const_vector (dest, src, mask_mode);
      return true;
    }

  /* In order to decrease the memory traffic, we don't use whole register
   * load/store for the LMUL less than 1 and mask mode, so those case will
   * require one extra general purpose register, but it's not allowed during LRA
   * process, so we have a special move pattern used for LRA, which will defer
   * the expansion after LRA.  */
  if ((known_lt (GET_MODE_SIZE (mode), BYTES_PER_RISCV_VECTOR)
       || GET_MODE_CLASS (mode) == MODE_VECTOR_BOOL)
      && lra_in_progress)
    {
      emit_insn (gen_mov_lra (mode, Pmode, dest, src));
      return true;
    }

  if (known_ge (GET_MODE_SIZE (mode), BYTES_PER_RISCV_VECTOR)
      && GET_MODE_CLASS (mode) != MODE_VECTOR_BOOL)
    {
      /* Need to force register if mem <- !reg.  */
      if (MEM_P (dest) && !REG_P (src))
	src = force_reg (mode, src);

      return false;
    }

  if (register_operand (src, mode) && register_operand (dest, mode))
    {
      emit_insn (gen_rtx_SET (dest, src));
      return true;
    }

  if (!register_operand (src, mode) && !register_operand (dest, mode))
    {
      rtx tmp = gen_reg_rtx (mode);
      if (MEM_P (src))
	emit_vlmax_op (code_for_pred_mov (mode), tmp, src, mask_mode);
      else
	emit_move_insn (tmp, src);
      src = tmp;
    }

  if (satisfies_constraint_vu (src))
    return false;

  emit_vlmax_op (code_for_pred_mov (mode), dest, src, mask_mode);
  return true;
}

/* VTYPE information for machine_mode.  */
struct mode_vtype_group
{
  enum vlmul_type vlmul_for_min_vlen32[NUM_MACHINE_MODES];
  uint8_t ratio_for_min_vlen32[NUM_MACHINE_MODES];
  enum vlmul_type vlmul_for_min_vlen64[NUM_MACHINE_MODES];
  uint8_t ratio_for_min_vlen64[NUM_MACHINE_MODES];
  mode_vtype_group ()
  {
#define ENTRY(MODE, REQUIREMENT, VLMUL_FOR_MIN_VLEN32, RATIO_FOR_MIN_VLEN32,   \
	      VLMUL_FOR_MIN_VLEN64, RATIO_FOR_MIN_VLEN64)                      \
  vlmul_for_min_vlen32[MODE##mode] = VLMUL_FOR_MIN_VLEN32;                     \
  ratio_for_min_vlen32[MODE##mode] = RATIO_FOR_MIN_VLEN32;                     \
  vlmul_for_min_vlen64[MODE##mode] = VLMUL_FOR_MIN_VLEN64;                     \
  ratio_for_min_vlen64[MODE##mode] = RATIO_FOR_MIN_VLEN64;
#include "riscv-vector-switch.def"
  }
};

static mode_vtype_group mode_vtype_infos;

/* Get vlmul field value by comparing LMUL with BYTES_PER_RISCV_VECTOR.  */
enum vlmul_type
get_vlmul (machine_mode mode)
{
  if (TARGET_MIN_VLEN == 32)
    return mode_vtype_infos.vlmul_for_min_vlen32[mode];
  else
    return mode_vtype_infos.vlmul_for_min_vlen64[mode];
}

/* Get ratio according to machine mode.  */
unsigned int
get_ratio (machine_mode mode)
{
  if (TARGET_MIN_VLEN == 32)
    return mode_vtype_infos.ratio_for_min_vlen32[mode];
  else
    return mode_vtype_infos.ratio_for_min_vlen64[mode];
}

/* Get ta according to operand[tail_op_idx].  */
int
get_ta (rtx ta)
{
  if (INTVAL (ta) == TAIL_ANY)
    return INVALID_ATTRIBUTE;
  return INTVAL (ta);
}

/* Get ma according to operand[mask_op_idx].  */
int
get_ma (rtx ma)
{
  if (INTVAL (ma) == MASK_ANY)
    return INVALID_ATTRIBUTE;
  return INTVAL (ma);
}

/* Get prefer tail policy.  */
enum tail_policy
get_prefer_tail_policy ()
{
  /* TODO: By default, we choose to use TAIL_ANY which allows
     compiler pick up either agnostic or undisturbed. Maybe we
     will have a compile option like -mprefer=agnostic to set
     this value???.  */
  return TAIL_ANY;
}

/* Get prefer mask policy.  */
enum mask_policy
get_prefer_mask_policy ()
{
  /* TODO: By default, we choose to use MASK_ANY which allows
     compiler pick up either agnostic or undisturbed. Maybe we
     will have a compile option like -mprefer=agnostic to set
     this value???.  */
  return MASK_ANY;
}

/* Get avl_type rtx.  */
rtx
get_avl_type_rtx (enum avl_type type)
{
  return gen_int_mode (type, Pmode);
}

/* Return the RVV vector mode that has NUNITS elements of mode INNER_MODE.
   This function is not only used by builtins, but also will be used by
   auto-vectorization in the future.  */
opt_machine_mode
get_vector_mode (scalar_mode inner_mode, poly_uint64 nunits)
{
  enum mode_class mclass;
  if (inner_mode == E_BImode)
    mclass = MODE_VECTOR_BOOL;
  else if (FLOAT_MODE_P (inner_mode))
    mclass = MODE_VECTOR_FLOAT;
  else
    mclass = MODE_VECTOR_INT;
  machine_mode mode;
  FOR_EACH_MODE_IN_CLASS (mode, mclass)
    if (inner_mode == GET_MODE_INNER (mode)
	&& known_eq (nunits, GET_MODE_NUNITS (mode))
	&& riscv_v_ext_vector_mode_p (mode))
      return mode;
  return opt_machine_mode ();
}

bool
simm5_p (rtx x)
{
  if (!CONST_INT_P (x))
    return false;
  return IN_RANGE (INTVAL (x), -16, 15);
}

bool
neg_simm5_p (rtx x)
{
  if (!CONST_INT_P (x))
    return false;
  return IN_RANGE (INTVAL (x), -15, 16);
}

bool
has_vi_variant_p (rtx_code code, rtx x)
{
  switch (code)
    {
    case PLUS:
    case AND:
    case IOR:
    case XOR:
    case SS_PLUS:
    case US_PLUS:
    case EQ:
    case NE:
    case LE:
    case LEU:
    case GT:
    case GTU:
      return simm5_p (x);

    case LT:
    case LTU:
    case GE:
    case GEU:
    case MINUS:
    case SS_MINUS:
      return neg_simm5_p (x);

    default:
      return false;
    }
}

bool
sew64_scalar_helper (rtx *operands, rtx *scalar_op, rtx vl,
		     machine_mode vector_mode, machine_mode mask_mode,
		     bool has_vi_variant_p,
		     void (*emit_vector_func) (rtx *, rtx))
{
  machine_mode scalar_mode = GET_MODE_INNER (vector_mode);
  if (has_vi_variant_p)
    {
      *scalar_op = force_reg (scalar_mode, *scalar_op);
      return false;
    }

  if (TARGET_64BIT)
    {
      if (!rtx_equal_p (*scalar_op, const0_rtx))
	*scalar_op = force_reg (scalar_mode, *scalar_op);
      return false;
    }

  if (immediate_operand (*scalar_op, Pmode))
    {
      if (!rtx_equal_p (*scalar_op, const0_rtx))
	*scalar_op = force_reg (Pmode, *scalar_op);

      *scalar_op = gen_rtx_SIGN_EXTEND (scalar_mode, *scalar_op);
      return false;
    }

  if (CONST_INT_P (*scalar_op))
    *scalar_op = force_reg (scalar_mode, *scalar_op);

  rtx tmp = gen_reg_rtx (vector_mode);
  riscv_vector::emit_nonvlmax_op (code_for_pred_broadcast (vector_mode), tmp,
				  *scalar_op, vl, mask_mode);
  emit_vector_func (operands, tmp);

  return true;
}

/* Get { ... ,0, 0, 0, ..., 0, 0, 0, 1 } mask.  */
rtx
gen_scalar_move_mask (machine_mode mode)
{
  rtx_vector_builder builder (mode, 1, 2);
  builder.quick_push (const1_rtx);
  builder.quick_push (const0_rtx);
  return builder.build ();
}

static unsigned
compute_vlmax (unsigned vector_bits, unsigned elt_size, unsigned min_size)
{
  // Original equation:
  //   VLMAX = (VectorBits / EltSize) * LMUL
  //   where LMUL = MinSize / TARGET_MIN_VLEN
  // The following equations have been reordered to prevent loss of precision
  // when calculating fractional LMUL.
  return ((vector_bits / elt_size) * min_size) / TARGET_MIN_VLEN;
}

static unsigned
get_unknown_min_value (machine_mode mode)
{
  enum vlmul_type vlmul = get_vlmul (mode);
  switch (vlmul)
    {
    case LMUL_1:
      return TARGET_MIN_VLEN;
    case LMUL_2:
      return TARGET_MIN_VLEN * 2;
    case LMUL_4:
      return TARGET_MIN_VLEN * 4;
    case LMUL_8:
      return TARGET_MIN_VLEN * 8;
    default:
      gcc_unreachable ();
    }
}

static rtx
force_vector_length_operand (rtx vl)
{
  if (CONST_INT_P (vl) && !satisfies_constraint_K (vl))
    return force_reg (Pmode, vl);
  return vl;
}

static rtx
gen_no_side_effects_vsetvl_rtx (machine_mode vmode, rtx vl, rtx avl)
{
  unsigned int sew = get_sew (vmode);
  return gen_vsetvl_no_side_effects (Pmode, vl, avl, gen_int_mode (sew, Pmode),
				     gen_int_mode (get_vlmul (vmode), Pmode),
				     const0_rtx, const0_rtx);
}

/* GET VL * 2 rtx.  */
static rtx
get_vl_x2_rtx (rtx avl, machine_mode mode, machine_mode demote_mode)
{
  rtx i32vl = NULL_RTX;
  if (CONST_INT_P (avl))
    {
      unsigned elt_size = GET_MODE_BITSIZE (GET_MODE_INNER (mode));
      unsigned min_size = get_unknown_min_value (mode);
      unsigned vlen_max = RVV_65536;
      unsigned vlmax_max = compute_vlmax (vlen_max, elt_size, min_size);
      unsigned vlen_min = TARGET_MIN_VLEN;
      unsigned vlmax_min = compute_vlmax (vlen_min, elt_size, min_size);

      unsigned HOST_WIDE_INT avl_int = INTVAL (avl);
      if (avl_int <= vlmax_min)
	i32vl = gen_int_mode (2 * avl_int, Pmode);
      else if (avl_int >= 2 * vlmax_max)
	{
	  // Just set i32vl to VLMAX in this situation
	  i32vl = gen_reg_rtx (Pmode);
	  emit_insn (
	    gen_no_side_effects_vsetvl_rtx (demote_mode, i32vl, RVV_VLMAX));
	}
      else
	{
	  // For AVL between (MinVLMAX, 2 * MaxVLMAX), the actual working vl
	  // is related to the hardware implementation.
	  // So let the following code handle
	}
    }
  if (!i32vl)
    {
      // Using vsetvli instruction to get actually used length which related to
      // the hardware implementation
      rtx i64vl = gen_reg_rtx (Pmode);
      emit_insn (
	gen_no_side_effects_vsetvl_rtx (mode, i64vl, force_reg (Pmode, avl)));
      // scale 2 for 32-bit length
      i32vl = gen_reg_rtx (Pmode);
      emit_insn (
	gen_rtx_SET (i32vl, gen_rtx_ASHIFT (Pmode, i64vl, const1_rtx)));
    }

  return force_vector_length_operand (i32vl);
}

bool
slide1_sew64_helper (int unspec, machine_mode mode, machine_mode demote_mode,
		     machine_mode demote_mask_mode, rtx *ops)
{
  rtx scalar_op = ops[4];
  rtx avl = ops[5];
  machine_mode scalar_mode = GET_MODE_INNER (mode);
  if (rtx_equal_p (scalar_op, const0_rtx))
    {
      ops[5] = force_vector_length_operand (ops[5]);
      return false;
    }

  if (TARGET_64BIT)
    {
      ops[4] = force_reg (scalar_mode, scalar_op);
      ops[5] = force_vector_length_operand (ops[5]);
      return false;
    }

  if (immediate_operand (scalar_op, Pmode))
    {
      ops[4] = gen_rtx_SIGN_EXTEND (scalar_mode, force_reg (Pmode, scalar_op));
      ops[5] = force_vector_length_operand (ops[5]);
      return false;
    }

  if (CONST_INT_P (scalar_op))
    scalar_op = force_reg (scalar_mode, scalar_op);

  rtx vl_x2 = get_vl_x2_rtx (avl, mode, demote_mode);

  rtx demote_scalar_op1, demote_scalar_op2;
  if (unspec == UNSPEC_VSLIDE1UP)
    {
      demote_scalar_op1 = gen_highpart (Pmode, scalar_op);
      demote_scalar_op2 = gen_lowpart (Pmode, scalar_op);
    }
  else
    {
      demote_scalar_op1 = gen_lowpart (Pmode, scalar_op);
      demote_scalar_op2 = gen_highpart (Pmode, scalar_op);
    }

  rtx temp = gen_reg_rtx (demote_mode);
  rtx ta = gen_int_mode (get_prefer_tail_policy (), Pmode);
  rtx ma = gen_int_mode (get_prefer_mask_policy (), Pmode);
  rtx merge = RVV_VUNDEF (demote_mode);
  /* Handle vslide1<ud>_tu.  */
  if (register_operand (ops[2], mode)
      && rtx_equal_p (ops[1], CONSTM1_RTX (GET_MODE (ops[1]))))
    {
      merge = gen_lowpart (demote_mode, ops[2]);
      ta = ops[6];
      ma = ops[7];
    }

  emit_insn (gen_pred_slide (unspec, demote_mode, temp,
			     CONSTM1_RTX (demote_mask_mode), merge,
			     gen_lowpart (demote_mode, ops[3]),
			     demote_scalar_op1, vl_x2, ta, ma, ops[8]));
  emit_insn (gen_pred_slide (unspec, demote_mode,
			     gen_lowpart (demote_mode, ops[0]),
			     CONSTM1_RTX (demote_mask_mode), merge, temp,
			     demote_scalar_op2, vl_x2, ta, ma, ops[8]));

  if (rtx_equal_p (ops[1], CONSTM1_RTX (GET_MODE (ops[1]))))
    return true;
  else
    emit_insn (gen_pred_merge (mode, ops[0], ops[2], ops[2], ops[0], ops[1],
			       force_vector_length_operand (ops[5]), ops[6],
			       ops[8]));
  return true;
}

rtx
gen_avl_for_scalar_move (rtx avl)
{
  /* AVL for scalar move has different behavior between 0 and large than 0.  */
  if (CONST_INT_P (avl))
    {
      /* So we could just set AVL to 1 for any constant other than 0.  */
      if (rtx_equal_p (avl, const0_rtx))
	return const0_rtx;
      else
	return const1_rtx;
    }
  else
    {
      /* For non-constant value, we set any non zero value to 1 by
	 `sgtu new_avl,input_avl,zero` + `vsetvli`.  */
      rtx tmp = gen_reg_rtx (Pmode);
      emit_insn (
	gen_rtx_SET (tmp, gen_rtx_fmt_ee (GTU, Pmode, avl, const0_rtx)));
      return tmp;
    }
}

} // namespace riscv_vector
