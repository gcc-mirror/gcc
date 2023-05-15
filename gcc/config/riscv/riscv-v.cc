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
#include "targhooks.h"
#include "expr.h"
#include "optabs.h"
#include "tm-constrs.h"
#include "rtx-vector-builder.h"
#include "targhooks.h"

using namespace riscv_vector;

namespace riscv_vector {

/* Return true if vlmax is constant value and can be used in vsetivl.  */
static bool
const_vlmax_p (machine_mode mode)
{
  poly_uint64 nuints = GET_MODE_NUNITS (mode);

  return nuints.is_constant ()
    /* The vsetivli can only hold register 0~31.  */
    ? (IN_RANGE (nuints.to_constant (), 0, 31))
    /* Only allowed in VLS-VLMAX mode.  */
    : false;
}

template <int MAX_OPERANDS> class insn_expander
{
public:
  insn_expander () : m_opno (0), has_dest(false) {}
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

  void set_dest_and_mask (rtx mask, rtx dest, machine_mode mask_mode)
  {
    dest_mode = GET_MODE (dest);
    has_dest = true;

    add_output_operand (dest, dest_mode);

    if (mask)
      add_input_operand (mask, GET_MODE (mask));
    else
      add_all_one_mask_operand (mask_mode);

    add_vundef_operand (dest_mode);
  }

  void set_len_and_policy (rtx len, bool force_vlmax = false)
    {
      bool vlmax_p = force_vlmax || !len;
      gcc_assert (has_dest);

      if (vlmax_p && const_vlmax_p (dest_mode))
	{
	  /* Optimize VLS-VLMAX code gen, we can use vsetivli instead of the
	     vsetvli to obtain the value of vlmax.  */
	  poly_uint64 nunits = GET_MODE_NUNITS (dest_mode);
	  len = gen_int_mode (nunits, Pmode);
	  vlmax_p = false; /* It has became NONVLMAX now.  */
	}
      else if (!len)
	{
	  len = gen_reg_rtx (Pmode);
	  emit_vlmax_vsetvl (dest_mode, len);
	}

      add_input_operand (len, Pmode);

      if (GET_MODE_CLASS (dest_mode) != MODE_VECTOR_BOOL)
	add_policy_operand (get_prefer_tail_policy (), get_prefer_mask_policy ());

      add_avl_type_operand (vlmax_p ? avl_type::VLMAX : avl_type::NONVLMAX);
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
  bool has_dest;
  machine_mode dest_mode;
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

/* SCALABLE means that the vector-length is agnostic (run-time invariant and
   compile-time unknown). FIXED meands that the vector-length is specific
   (compile-time known). Both RVV_SCALABLE and RVV_FIXED_VLMAX are doing
   auto-vectorization using VLMAX vsetvl configuration.  */
static bool
autovec_use_vlmax_p (void)
{
  return (riscv_autovec_preference == RVV_SCALABLE
	  || riscv_autovec_preference == RVV_FIXED_VLMAX);
}

/* Emit an RVV unmask && vl mov from SRC to DEST.  */
static void
emit_pred_op (unsigned icode, rtx mask, rtx dest, rtx src, rtx len,
	      machine_mode mask_mode, bool force_vlmax = false)
{
  insn_expander<8> e;
  e.set_dest_and_mask (mask, dest, mask_mode);

  e.add_input_operand (src, GET_MODE (src));

  e.set_len_and_policy (len, force_vlmax);

  e.expand ((enum insn_code) icode, MEM_P (dest) || MEM_P (src));
}

/* Emit an RVV binop.  If one of SRC1 and SRC2 is a scalar operand, its mode is
   specified using SCALAR_MODE.  */
static void
emit_pred_binop (unsigned icode, rtx mask, rtx dest, rtx src1, rtx src2,
		 rtx len, machine_mode mask_mode,
		 machine_mode scalar_mode = VOIDmode)
{
  insn_expander<9> e;
  e.set_dest_and_mask (mask, dest, mask_mode);

  gcc_assert (VECTOR_MODE_P (GET_MODE (src1))
	      || VECTOR_MODE_P (GET_MODE (src2)));

  if (VECTOR_MODE_P (GET_MODE (src1)))
    e.add_input_operand (src1, GET_MODE (src1));
  else
    e.add_input_operand (src1, scalar_mode);

  if (VECTOR_MODE_P (GET_MODE (src2)))
    e.add_input_operand (src2, GET_MODE (src2));
  else
    e.add_input_operand (src2, scalar_mode);

  e.set_len_and_policy (len);

  e.expand ((enum insn_code) icode, MEM_P (dest) || MEM_P (src1) || MEM_P (src2));
}

/* The RISC-V vsetvli pass uses "known vlmax" operations for optimization.
   Whether or not an instruction actually is a vlmax operation is not
   recognizable from the length operand alone but the avl_type operand
   is used instead.  In general, there are two cases:

    - Emit a vlmax operation by passing a NULL length.  Here we emit
      a vsetvli with vlmax configuration and set the avl_type to VLMAX.
    - Emit an operation that uses the existing (last-set) length and
      set the avl_type to NONVLMAX.

    Sometimes we also need to set the VLMAX avl_type to an operation that
    already uses a given length register.  This can happen during or after
    register allocation when we are not allowed to create a new register.
    For that case we also allow to set the avl_type to VLMAX.
*/

/* This function emits a VLMAX vsetvli followed by the actual operation.  */
void
emit_vlmax_op (unsigned icode, rtx dest, rtx src, machine_mode mask_mode)
{
  emit_pred_op (icode, NULL_RTX, dest, src, NULL_RTX, mask_mode);
}

/* This function emits an operation with a given LEN that is determined
   by a previously emitted VLMAX vsetvli.  */
void
emit_len_op (unsigned icode, rtx dest, rtx src, rtx len,
	     machine_mode mask_mode)
{
  emit_pred_op (icode, NULL_RTX, dest, src, len, mask_mode);
}

/* This function emits an operation with a given LEN that is known to be
   a preceding VLMAX.  It also sets the VLMAX flag which allows further
   optimization in the vsetvli pass.  */
void
emit_vlmax_reg_op (unsigned icode, rtx dest, rtx src, rtx len,
		   machine_mode mask_mode)
{
  emit_pred_op (icode, NULL_RTX, dest, src, len, mask_mode,
		/* Force VLMAX */ true);
}

void
emit_len_binop (unsigned icode, rtx dest, rtx src1, rtx src2, rtx len,
		machine_mode mask_mode, machine_mode scalar_mode)
{
  emit_pred_binop (icode, NULL_RTX, dest, src1, src2, len,
		   mask_mode, scalar_mode);
}

/* Emit vid.v instruction.  */

static void
emit_index_op (rtx dest, machine_mode mask_mode)
{
  insn_expander<7> e;
  e.set_dest_and_mask (NULL, dest, mask_mode);

  e.set_len_and_policy (NULL, true);

  e.expand (code_for_pred_series (GET_MODE (dest)), false);
}

/* Expand series const vector.  */

void
expand_vec_series (rtx dest, rtx base, rtx step)
{
  machine_mode mode = GET_MODE (dest);
  machine_mode inner_mode = GET_MODE_INNER (mode);
  machine_mode mask_mode;
  gcc_assert (get_mask_mode (mode).exists (&mask_mode));

  /* VECT_IV = BASE + I * STEP.  */

  /* Step 1: Generate I = { 0, 1, 2, ... } by vid.v.  */
  rtx vid = gen_reg_rtx (mode);
  emit_index_op (vid, mask_mode);

  /* Step 2: Generate I * STEP.
     - STEP is 1, we don't emit any instructions.
     - STEP is power of 2, we use vsll.vi/vsll.vx.
     - STEP is non-power of 2, we use vmul.vx.  */
  rtx step_adj;
  if (rtx_equal_p (step, const1_rtx))
    step_adj = vid;
  else
    {
      step_adj = gen_reg_rtx (mode);
      if (CONST_INT_P (step) && pow2p_hwi (INTVAL (step)))
	{
	  /* Emit logical left shift operation.  */
	  int shift = exact_log2 (INTVAL (step));
	  rtx shift_amount = gen_int_mode (shift, Pmode);
	  insn_code icode = code_for_pred_scalar (ASHIFT, mode);
	  emit_len_binop (icode, step_adj, vid, shift_amount,
			  NULL, mask_mode, Pmode);
	}
      else
	{
	  insn_code icode = code_for_pred_scalar (MULT, mode);
	  emit_len_binop (icode, step_adj, vid, step,
			  NULL, mask_mode, inner_mode);
	}
    }

  /* Step 3: Generate BASE + I * STEP.
     - BASE is 0, use result of vid.
     - BASE is not 0, we use vadd.vx/vadd.vi.  */
  if (rtx_equal_p (base, const0_rtx))
    {
      emit_move_insn (dest, step_adj);
    }
  else
    {
      rtx result = gen_reg_rtx (mode);
      insn_code icode = code_for_pred_scalar (PLUS, mode);
      emit_len_binop (icode, result, step_adj, base,
			   NULL, mask_mode, inner_mode);
      emit_move_insn (dest, result);
    }
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

  /* Support scalable const series vector.  */
  rtx base, step;
  if (const_vec_series_p (src, &base, &step))
    {
      emit_insn (gen_vec_series (mode, target, base, step));
      return;
    }

  /* TODO: We only support const duplicate vector for now. More cases
     will be supported when we support auto-vectorization:

       1. multiple elts duplicate vector.
       2. multiple patterns with multiple elts.  */
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
  enum vlmul_type vlmul_for_for_vlen128[NUM_MACHINE_MODES];
  uint8_t ratio_for_for_vlen128[NUM_MACHINE_MODES];
  machine_mode subpart_mode[NUM_MACHINE_MODES];
  uint8_t nf[NUM_MACHINE_MODES];
  mode_vtype_group ()
  {
#define ENTRY(MODE, REQUIREMENT, VLMUL_FOR_MIN_VLEN32, RATIO_FOR_MIN_VLEN32,   \
	      VLMUL_FOR_MIN_VLEN64, RATIO_FOR_MIN_VLEN64,                      \
	      VLMUL_FOR_MIN_VLEN128, RATIO_FOR_MIN_VLEN128)                    \
  vlmul_for_min_vlen32[MODE##mode] = VLMUL_FOR_MIN_VLEN32;                     \
  ratio_for_min_vlen32[MODE##mode] = RATIO_FOR_MIN_VLEN32;                     \
  vlmul_for_min_vlen64[MODE##mode] = VLMUL_FOR_MIN_VLEN64;                     \
  ratio_for_min_vlen64[MODE##mode] = RATIO_FOR_MIN_VLEN64;                     \
  vlmul_for_for_vlen128[MODE##mode] = VLMUL_FOR_MIN_VLEN128;                   \
  ratio_for_for_vlen128[MODE##mode] = RATIO_FOR_MIN_VLEN128;
#include "riscv-vector-switch.def"
#define TUPLE_ENTRY(MODE, REQUIREMENT, SUBPART_MODE, NF, VLMUL_FOR_MIN_VLEN32, \
		    RATIO_FOR_MIN_VLEN32, VLMUL_FOR_MIN_VLEN64,                \
		    RATIO_FOR_MIN_VLEN64, VLMUL_FOR_MIN_VLEN128,               \
		    RATIO_FOR_MIN_VLEN128)                                     \
  subpart_mode[MODE##mode] = SUBPART_MODE##mode;                               \
  nf[MODE##mode] = NF;                                                         \
  vlmul_for_min_vlen32[MODE##mode] = VLMUL_FOR_MIN_VLEN32;                     \
  ratio_for_min_vlen32[MODE##mode] = RATIO_FOR_MIN_VLEN32;                     \
  vlmul_for_min_vlen64[MODE##mode] = VLMUL_FOR_MIN_VLEN64;                     \
  ratio_for_min_vlen64[MODE##mode] = RATIO_FOR_MIN_VLEN64;                     \
  vlmul_for_for_vlen128[MODE##mode] = VLMUL_FOR_MIN_VLEN128;                   \
  ratio_for_for_vlen128[MODE##mode] = RATIO_FOR_MIN_VLEN128;
#include "riscv-vector-switch.def"
  }
};

static mode_vtype_group mode_vtype_infos;

/* Get vlmul field value by comparing LMUL with BYTES_PER_RISCV_VECTOR.  */
enum vlmul_type
get_vlmul (machine_mode mode)
{
  if (TARGET_MIN_VLEN >= 128)
    return mode_vtype_infos.vlmul_for_for_vlen128[mode];
  else if (TARGET_MIN_VLEN == 32)
    return mode_vtype_infos.vlmul_for_min_vlen32[mode];
  else
    return mode_vtype_infos.vlmul_for_min_vlen64[mode];
}

/* Return the NF value of the corresponding mode.  */
unsigned int
get_nf (machine_mode mode)
{
  /* We don't allow non-tuple modes go through this function.  */
  gcc_assert (riscv_v_ext_tuple_mode_p (mode));
  return mode_vtype_infos.nf[mode];
}

/* Return the subpart mode of the tuple mode. For VNx2x1SImode,
   the subpart mode is VNx1SImode. This will help to build
   array/struct type in builtins.  */
machine_mode
get_subpart_mode (machine_mode mode)
{
  /* We don't allow non-tuple modes go through this function.  */
  gcc_assert (riscv_v_ext_tuple_mode_p (mode));
  return mode_vtype_infos.subpart_mode[mode];
}

/* Get ratio according to machine mode.  */
unsigned int
get_ratio (machine_mode mode)
{
  if (TARGET_MIN_VLEN >= 128)
    return mode_vtype_infos.ratio_for_for_vlen128[mode];
  else if (TARGET_MIN_VLEN == 32)
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

/* Return the appropriate mask mode for MODE.  */

opt_machine_mode
get_mask_mode (machine_mode mode)
{
  return get_vector_mode (BImode, GET_MODE_NUNITS (mode));
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

/* Return the RVV tuple mode if we can find the legal tuple mode for the
   corresponding subpart mode and NF.  */
opt_machine_mode
get_tuple_mode (machine_mode subpart_mode, unsigned int nf)
{
  poly_uint64 nunits = GET_MODE_NUNITS (subpart_mode) * nf;
  scalar_mode inner_mode = GET_MODE_INNER (subpart_mode);
  enum mode_class mclass = GET_MODE_CLASS (subpart_mode);
  machine_mode mode;
  FOR_EACH_MODE_IN_CLASS (mode, mclass)
    if (inner_mode == GET_MODE_INNER (mode)
	&& known_eq (nunits, GET_MODE_NUNITS (mode))
	&& riscv_v_ext_tuple_mode_p (mode)
	&& get_subpart_mode (mode) == subpart_mode)
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
  riscv_vector::emit_len_op (code_for_pred_broadcast (vector_mode), tmp,
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

/* Expand tuple modes data movement for.  */
void
expand_tuple_move (machine_mode mask_mode, rtx *ops)
{
  unsigned int i;
  machine_mode tuple_mode = GET_MODE (ops[0]);
  machine_mode subpart_mode = get_subpart_mode (tuple_mode);
  poly_int64 subpart_size = GET_MODE_SIZE (subpart_mode);
  unsigned int nf = get_nf (tuple_mode);
  bool fractional_p = known_lt (subpart_size, BYTES_PER_RISCV_VECTOR);

  if (REG_P (ops[0]) && CONST_VECTOR_P (ops[1]))
    {
      rtx val;
      gcc_assert (can_create_pseudo_p ()
		  && const_vec_duplicate_p (ops[1], &val));
      for (i = 0; i < nf; ++i)
	{
	  poly_int64 offset = i * subpart_size;
	  rtx subreg
	    = simplify_gen_subreg (subpart_mode, ops[0], tuple_mode, offset);
	  rtx dup = gen_const_vec_duplicate (subpart_mode, val);
	  emit_move_insn (subreg, dup);
	}
    }
  else if (REG_P (ops[0]) && REG_P (ops[1]))
    {
      for (i = 0; i < nf; ++i)
	{
	  int index = i;

	  /* Take NF = 2 and LMUL = 1 for example:

	      - move v8 to v9:
		 vmv1r v10,v9
		 vmv1r v9,v8

	      - move v8 to v7:
		 vmv1r v7,v8
		 vmv1r v8,v9  */
	  if (REGNO (ops[0]) > REGNO (ops[1]))
	    index = nf - 1 - i;
	  poly_int64 offset = index * subpart_size;
	  rtx dst_subreg
	    = simplify_gen_subreg (subpart_mode, ops[0], tuple_mode, offset);
	  rtx src_subreg
	    = simplify_gen_subreg (subpart_mode, ops[1], tuple_mode, offset);
	  emit_insn (gen_rtx_SET (dst_subreg, src_subreg));
	}
    }
  else
    {
      /* Expand tuple memory data movement.  */
      gcc_assert (MEM_P (ops[0]) || MEM_P (ops[1]));
      rtx offset = gen_int_mode (subpart_size, Pmode);
      if (!subpart_size.is_constant ())
	{
	  emit_move_insn (ops[2], gen_int_mode (BYTES_PER_RISCV_VECTOR, Pmode));
	  if (fractional_p)
	    {
	      unsigned int factor
		= exact_div (BYTES_PER_RISCV_VECTOR, subpart_size)
		    .to_constant ();
	      rtx pat
		= gen_rtx_ASHIFTRT (Pmode, ops[2],
				    gen_int_mode (exact_log2 (factor), Pmode));
	      emit_insn (gen_rtx_SET (ops[2], pat));
	    }

	  if (known_gt (subpart_size, BYTES_PER_RISCV_VECTOR))
	    {
	      unsigned int factor
		= exact_div (subpart_size, BYTES_PER_RISCV_VECTOR)
		    .to_constant ();
	      rtx pat
		= gen_rtx_ASHIFT (Pmode, ops[2],
				  gen_int_mode (exact_log2 (factor), Pmode));
	      emit_insn (gen_rtx_SET (ops[2], pat));
	    }
	  offset = ops[2];
	}

      if (MEM_P (ops[1]))
	{
	  /* Load operations.  */
	  emit_move_insn (ops[3], XEXP (ops[1], 0));
	  for (i = 0; i < nf; i++)
	    {
	      rtx subreg = simplify_gen_subreg (subpart_mode, ops[0],
						tuple_mode, i * subpart_size);
	      if (i != 0)
		{
		  rtx new_addr = gen_rtx_PLUS (Pmode, ops[3], offset);
		  emit_insn (gen_rtx_SET (ops[3], new_addr));
		}
	      rtx mem = gen_rtx_MEM (subpart_mode, ops[3]);

	      if (fractional_p)
		emit_vlmax_reg_op (code_for_pred_mov (subpart_mode), subreg, mem,
			       ops[4], mask_mode);
	      else
		emit_move_insn (subreg, mem);
	    }
	}
      else
	{
	  /* Store operations.  */
	  emit_move_insn (ops[3], XEXP (ops[0], 0));
	  for (i = 0; i < nf; i++)
	    {
	      rtx subreg = simplify_gen_subreg (subpart_mode, ops[1],
						tuple_mode, i * subpart_size);
	      if (i != 0)
		{
		  rtx new_addr = gen_rtx_PLUS (Pmode, ops[3], offset);
		  emit_insn (gen_rtx_SET (ops[3], new_addr));
		}
	      rtx mem = gen_rtx_MEM (subpart_mode, ops[3]);

	      if (fractional_p)
		emit_vlmax_reg_op (code_for_pred_mov (subpart_mode), mem, subreg,
			       ops[4], mask_mode);
	      else
		emit_move_insn (mem, subreg);
	    }
	}
    }
}

/* Return the vectorization machine mode for RVV according to LMUL.  */
machine_mode
preferred_simd_mode (scalar_mode mode)
{
  /* We will disable auto-vectorization when TARGET_MIN_VLEN < 128 &&
     riscv_autovec_lmul < RVV_M2. Since GCC loop vectorizer report ICE when we
     enable -march=rv64gc_zve32* and -march=rv32gc_zve64*. in the
     'can_duplicate_and_interleave_p' of tree-vect-slp.cc. Since we have
     VNx1SImode in -march=*zve32* and VNx1DImode in -march=*zve64*, they are
     enabled in targetm. vector_mode_supported_p and SLP vectorizer will try to
     use them. Currently, we can support auto-vectorization in
     -march=rv32_zve32x_zvl128b. Wheras, -march=rv32_zve32x_zvl32b or
     -march=rv32_zve32x_zvl64b are disabled.  */
  if (autovec_use_vlmax_p ())
    {
      if (TARGET_MIN_VLEN < 128 && riscv_autovec_lmul < RVV_M2)
	return word_mode;
      /* We use LMUL = 1 as base bytesize which is BYTES_PER_RISCV_VECTOR and
	 riscv_autovec_lmul as multiply factor to calculate the the NUNITS to
	 get the auto-vectorization mode.  */
      poly_uint64 nunits;
      poly_uint64 vector_size
	= BYTES_PER_RISCV_VECTOR * ((int) riscv_autovec_lmul);
      poly_uint64 scalar_size = GET_MODE_SIZE (mode);
      gcc_assert (multiple_p (vector_size, scalar_size, &nunits));
      machine_mode rvv_mode;
      if (get_vector_mode (mode, nunits).exists (&rvv_mode))
	return rvv_mode;
    }
  /* TODO: We will support minimum length VLS auto-vectorization in
     the future.  */
  return word_mode;
}

class rvv_builder : public rtx_vector_builder
{
public:
  rvv_builder () : rtx_vector_builder () {}
  rvv_builder (machine_mode mode, unsigned int npatterns,
	       unsigned int nelts_per_pattern)
    : rtx_vector_builder (mode, npatterns, nelts_per_pattern)
  {
    m_inner_mode = GET_MODE_INNER (mode);
    m_inner_size = GET_MODE_BITSIZE (m_inner_mode).to_constant ();
  }

  bool can_duplicate_repeating_sequence_p ();
  rtx get_merged_repeating_sequence ();

  machine_mode new_mode () const { return m_new_mode; }

private:
  machine_mode m_inner_mode;
  machine_mode m_new_mode;
  scalar_int_mode m_new_inner_mode;
  unsigned int m_inner_size;
};

/* Return true if the vector duplicated by a super element which is the fusion
   of consecutive elements.

     v = { a, b, a, b } super element = ab, v = { ab, ab }  */
bool
rvv_builder::can_duplicate_repeating_sequence_p ()
{
  poly_uint64 new_size = exact_div (full_nelts (), npatterns ());
  unsigned int new_inner_size = m_inner_size * npatterns ();
  if (!int_mode_for_size (new_inner_size, 0).exists (&m_new_inner_mode)
      || GET_MODE_SIZE (m_new_inner_mode) > UNITS_PER_WORD
      || !get_vector_mode (m_new_inner_mode, new_size).exists (&m_new_mode))
    return false;
  return repeating_sequence_p (0, full_nelts ().to_constant (), npatterns ());
}

/* Merge the repeating sequence into a single element and return the RTX.  */
rtx
rvv_builder::get_merged_repeating_sequence ()
{
  scalar_int_mode mode = Pmode;
  rtx target = gen_reg_rtx (mode);
  emit_move_insn (target, const0_rtx);
  rtx imm = gen_int_mode ((1ULL << m_inner_size) - 1, mode);
  /* { a, b, a, b }: Generate duplicate element = b << bits | a.  */
  for (unsigned int i = 0; i < npatterns (); i++)
    {
      unsigned int loc = m_inner_size * i;
      rtx shift = gen_int_mode (loc, mode);
      rtx ele = gen_lowpart (mode, elt (i));
      rtx tmp = expand_simple_binop (mode, AND, ele, imm, NULL_RTX, false,
				     OPTAB_DIRECT);
      rtx tmp2 = expand_simple_binop (mode, ASHIFT, tmp, shift, NULL_RTX, false,
				      OPTAB_DIRECT);
      rtx tmp3 = expand_simple_binop (mode, IOR, tmp2, target, NULL_RTX, false,
				      OPTAB_DIRECT);
      emit_move_insn (target, tmp3);
    }
  if (GET_MODE_SIZE (m_new_inner_mode) < UNITS_PER_WORD)
    return gen_lowpart (m_new_inner_mode, target);
  return target;
}

/* Subroutine of riscv_vector_expand_vector_init.
   Works as follows:
   (a) Initialize TARGET by broadcasting element NELTS_REQD - 1 of BUILDER.
   (b) Skip leading elements from BUILDER, which are the same as
       element NELTS_REQD - 1.
   (c) Insert earlier elements in reverse order in TARGET using vslide1down.  */

static void
expand_vector_init_insert_elems (rtx target, const rvv_builder &builder,
				 int nelts_reqd)
{
  machine_mode mode = GET_MODE (target);
  scalar_mode elem_mode = GET_MODE_INNER (mode);
  machine_mode mask_mode;
  gcc_assert (get_mask_mode (mode).exists (&mask_mode));
  rtx dup = expand_vector_broadcast (mode, builder.elt (0));
  emit_move_insn (target, dup);
  int ndups = builder.count_dups (0, nelts_reqd - 1, 1);
  for (int i = ndups; i < nelts_reqd; i++)
    {
      unsigned int unspec
	= FLOAT_MODE_P (mode) ? UNSPEC_VFSLIDE1DOWN : UNSPEC_VSLIDE1DOWN;
      insn_code icode = code_for_pred_slide (unspec, mode);
      emit_len_binop (icode, target, target, builder.elt (i), NULL, mask_mode,
		      elem_mode);
    }
}

/* Initialize register TARGET from the elements in PARALLEL rtx VALS.  */

void
expand_vec_init (rtx target, rtx vals)
{
  machine_mode mode = GET_MODE (target);
  int nelts = XVECLEN (vals, 0);

  rvv_builder v (mode, nelts, 1);
  for (int i = 0; i < nelts; i++)
    v.quick_push (XVECEXP (vals, 0, i));
  v.finalize ();

  if (nelts > 3)
    {
      /* Case 1: Convert v = { a, b, a, b } into v = { ab, ab }.  */
      if (v.can_duplicate_repeating_sequence_p ())
	{
	  rtx ele = v.get_merged_repeating_sequence ();
	  rtx dup = expand_vector_broadcast (v.new_mode (), ele);
	  emit_move_insn (target, gen_lowpart (mode, dup));
	  return;
	}
      /* TODO: We will support more Initialization of vector in the future.  */
    }

  /* Handle common situation by vslide1down. This function can handle any
     situation of vec_init<mode>. Only the cases that are not optimized above
     will fall through here.  */
  expand_vector_init_insert_elems (target, v, nelts);
}

} // namespace riscv_vector
