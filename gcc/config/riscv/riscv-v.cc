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
  insn_expander ()
    : m_opno (0), m_op_num (0), m_has_dest_p (false),
      m_fully_unmasked_p (false), m_use_real_merge_p (false),
      m_needs_avl_p (false), m_vlmax_p (false), m_has_tail_policy_p (false),
      m_has_mask_policy_p (false), m_tail_policy (TAIL_ANY),
      m_mask_policy (MASK_ANY), m_dest_mode (VOIDmode), m_mask_mode (VOIDmode),
      m_vl_op (NULL_RTX)
  {}

  /* Initializer for various configurations.  */
  insn_expander (int op_num, bool has_dest_p, bool use_all_trues_mask_p,
		 bool use_real_merge_p, bool needs_avl_p, bool vlmax_p,
		 machine_mode dest_mode, machine_mode mask_mode)
    : m_opno (0), m_op_num (op_num), m_has_dest_p (has_dest_p),
      m_fully_unmasked_p (use_all_trues_mask_p),
      m_use_real_merge_p (use_real_merge_p), m_needs_avl_p (needs_avl_p),
      m_vlmax_p (vlmax_p), m_has_tail_policy_p (false),
      m_has_mask_policy_p (false), m_tail_policy (TAIL_ANY),
      m_mask_policy (MASK_ANY), m_dest_mode (dest_mode),
      m_mask_mode (mask_mode), m_vl_op (NULL_RTX)
  {}

  void set_policy (enum tail_policy ta)
  {
    m_has_tail_policy_p = true;
    m_tail_policy = ta;
  }
  void set_policy (enum mask_policy ma)
  {
    m_has_mask_policy_p = true;
    m_mask_policy = ma;
  }
  void set_vl (rtx vl) { m_vl_op = vl; }

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
  void add_all_one_mask_operand ()
  {
    add_input_operand (CONSTM1_RTX (m_mask_mode), m_mask_mode);
  }
  void add_vundef_operand ()
  {
    add_input_operand (RVV_VUNDEF (m_dest_mode), m_dest_mode);
  }
  void add_policy_operand ()
  {
    if (m_has_tail_policy_p)
      {
	rtx tail_policy_rtx = gen_int_mode (m_tail_policy, Pmode);
	add_input_operand (tail_policy_rtx, Pmode);
      }
    if (m_has_mask_policy_p)
      {
	rtx mask_policy_rtx = gen_int_mode (m_mask_policy, Pmode);
	add_input_operand (mask_policy_rtx, Pmode);
      }
  }
  void add_avl_type_operand (avl_type type)
  {
    add_input_operand (gen_int_mode (type, Pmode), Pmode);
  }

  void emit_insn (enum insn_code icode, rtx *ops)
  {
    int opno = 0;
    /* It's true if any operand is memory operand.  */
    bool any_mem_p = false;
    /* It's true if all operands are mask operand.  */
    bool all_mask_p = true;
    if (m_has_dest_p)
      {
	any_mem_p |= MEM_P (ops[opno]);
	all_mask_p &= GET_MODE_CLASS (GET_MODE (ops[opno])) == MODE_VECTOR_BOOL;
	add_output_operand (ops[opno++], m_dest_mode);
      }

    if (m_fully_unmasked_p)
      add_all_one_mask_operand ();

    if (!m_use_real_merge_p)
      add_vundef_operand ();

    for (; opno < m_op_num; opno++)
      {
	any_mem_p |= MEM_P (ops[opno]);
	all_mask_p &= GET_MODE_CLASS (GET_MODE (ops[opno])) == MODE_VECTOR_BOOL;
	machine_mode mode = insn_data[(int) icode].operand[m_opno].mode;
	/* 'create_input_operand doesn't allow VOIDmode.
	   According to vector.md, we may have some patterns that do not have
	   explicit machine mode specifying the operand. Such operands are
	   always Pmode.  */
	if (mode == VOIDmode)
	  mode = Pmode;
	add_input_operand (ops[opno], mode);
      }

    if (m_needs_avl_p)
      {
	rtx len = m_vl_op;
	if (m_vlmax_p)
	  {
	    if (const_vlmax_p (m_dest_mode))
	      {
		/* Optimize VLS-VLMAX code gen, we can use vsetivli instead of
		   the vsetvli to obtain the value of vlmax.  */
		poly_uint64 nunits = GET_MODE_NUNITS (m_dest_mode);
		len = gen_int_mode (nunits, Pmode);
		m_vlmax_p = false; /* It has became NONVLMAX now.  */
	      }
	    else if (can_create_pseudo_p ())
	      {
		len = gen_reg_rtx (Pmode);
		emit_vlmax_vsetvl (m_dest_mode, len);
	      }
	  }
	add_input_operand (len, Pmode);
      }

    if (!all_mask_p)
      add_policy_operand ();
    if (m_needs_avl_p)
      add_avl_type_operand (m_vlmax_p ? avl_type::VLMAX : avl_type::NONVLMAX);
    expand (icode, any_mem_p);
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
  int m_op_num;
  /* It't true when the pattern has a dest operand. Most of the patterns have
     dest operand wheras some patterns like STOREs does not have dest operand.
     For example, according to vector.md. We can see indexed loads/stores do
     not have dest operand.
  */
  bool m_has_dest_p;
  /* It't true if the pattern uses all trues mask operand.  */
  bool m_fully_unmasked_p;
  /* It's true if the pattern uses real merge operand.  */
  bool m_use_real_merge_p;
  bool m_needs_avl_p;
  bool m_vlmax_p;
  bool m_has_tail_policy_p;
  bool m_has_mask_policy_p;
  enum tail_policy m_tail_policy;
  enum mask_policy m_mask_policy;
  machine_mode m_dest_mode;
  machine_mode m_mask_mode;
  rtx m_vl_op;
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

/* This function emits a {VLMAX, TAIL_ANY, MASK_ANY} vsetvli followed by the
 * actual operation.  */
void
emit_vlmax_insn (unsigned icode, int op_num, rtx *ops, rtx vl)
{
  machine_mode data_mode = GET_MODE (ops[0]);
  machine_mode mask_mode = get_mask_mode (data_mode).require ();
  /* We have a maximum of 11 operands for RVV instruction patterns according to
   * vector.md.  */
  insn_expander<11> e (/*OP_NUM*/ op_num, /*HAS_DEST_P*/ true,
		       /*FULLY_UNMASKED_P*/ true,
		       /*USE_REAL_MERGE_P*/ false, /*HAS_AVL_P*/ true,
		       /*VLMAX_P*/ true,
		       /*DEST_MODE*/ data_mode, /*MASK_MODE*/ mask_mode);
  e.set_policy (TAIL_ANY);
  e.set_policy (MASK_ANY);
  /* According to LRA mov pattern in vector.md, we have a clobber operand
     to be used ad VL operand.  */
  e.set_vl (vl);
  e.emit_insn ((enum insn_code) icode, ops);
}

/* This function emits a {NONVLMAX, TAIL_ANY, MASK_ANY} vsetvli followed by the
 * actual operation.  */
void
emit_nonvlmax_insn (unsigned icode, int op_num, rtx *ops, rtx avl)
{
  machine_mode data_mode = GET_MODE (ops[0]);
  machine_mode mask_mode = get_mask_mode (data_mode).require ();
  /* We have a maximum of 11 operands for RVV instruction patterns according to
   * vector.md.  */
  insn_expander<11> e (/*OP_NUM*/ op_num, /*HAS_DEST_P*/ true,
		       /*FULLY_UNMASKED_P*/ true,
		       /*USE_REAL_MERGE_P*/ false, /*HAS_AVL_P*/ true,
		       /*VLMAX_P*/ false,
		       /*DEST_MODE*/ data_mode, /*MASK_MODE*/ mask_mode);
  e.set_policy (TAIL_ANY);
  e.set_policy (MASK_ANY);
  e.set_vl (avl);
  e.emit_insn ((enum insn_code) icode, ops);
}

/* This function emits merge instruction.  */
void
emit_vlmax_merge_insn (unsigned icode, int op_num, rtx *ops)
{
  machine_mode dest_mode = GET_MODE (ops[0]);
  machine_mode mask_mode = get_mask_mode (dest_mode).require ();
  insn_expander<11> e (/*OP_NUM*/ op_num, /*HAS_DEST_P*/ true,
		       /*FULLY_UNMASKED_P*/ false,
		       /*USE_REAL_MERGE_P*/ false, /*HAS_AVL_P*/ true,
		       /*VLMAX_P*/ true, dest_mode, mask_mode);
  e.set_policy (TAIL_ANY);
  e.emit_insn ((enum insn_code) icode, ops);
}

/* This function emits cmp instruction.  */
void
emit_vlmax_cmp_insn (unsigned icode, rtx *ops)
{
  machine_mode mode = GET_MODE (ops[0]);
  insn_expander<11> e (/*OP_NUM*/ RVV_CMP_OP, /*HAS_DEST_P*/ true,
		       /*FULLY_UNMASKED_P*/ true,
		       /*USE_REAL_MERGE_P*/ false,
		       /*HAS_AVL_P*/ true,
		       /*VLMAX_P*/ true,
		       /*DEST_MODE*/ mode, /*MASK_MODE*/ mode);
  e.set_policy (MASK_ANY);
  e.emit_insn ((enum insn_code) icode, ops);
}

/* This function emits cmp with MU instruction.  */
void
emit_vlmax_cmp_mu_insn (unsigned icode, rtx *ops)
{
  machine_mode mode = GET_MODE (ops[0]);
  insn_expander<11> e (/*OP_NUM*/ RVV_CMP_MU_OP, /*HAS_DEST_P*/ true,
		       /*FULLY_UNMASKED_P*/ false,
		       /*USE_REAL_MERGE_P*/ true,
		       /*HAS_AVL_P*/ true,
		       /*VLMAX_P*/ true,
		       /*DEST_MODE*/ mode, /*MASK_MODE*/ mode);
  e.set_policy (MASK_UNDISTURBED);
  e.emit_insn ((enum insn_code) icode, ops);
}

/* This function emits a masked instruction.  */
void
emit_vlmax_masked_mu_insn (unsigned icode, int op_num, rtx *ops)
{
  machine_mode dest_mode = GET_MODE (ops[0]);
  machine_mode mask_mode = get_mask_mode (dest_mode).require ();
  insn_expander<11> e (/*OP_NUM*/ op_num, /*HAS_DEST_P*/ true,
		       /*FULLY_UNMASKED_P*/ false,
		       /*USE_REAL_MERGE_P*/ true,
		       /*HAS_AVL_P*/ true,
		       /*VLMAX_P*/ true, dest_mode, mask_mode);
  e.set_policy (TAIL_ANY);
  e.set_policy (MASK_UNDISTURBED);
  e.emit_insn ((enum insn_code) icode, ops);
}

/* Expand series const vector.  */

void
expand_vec_series (rtx dest, rtx base, rtx step)
{
  machine_mode mode = GET_MODE (dest);
  machine_mode mask_mode;
  gcc_assert (get_mask_mode (mode).exists (&mask_mode));

  /* VECT_IV = BASE + I * STEP.  */

  /* Step 1: Generate I = { 0, 1, 2, ... } by vid.v.  */
  rtx vid = gen_reg_rtx (mode);
  rtx op[1] = {vid};
  emit_vlmax_insn (code_for_pred_series (mode), RVV_MISC_OP, op);

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
	  rtx ops[] = {step_adj, vid, shift_amount};
	  emit_vlmax_insn (icode, RVV_BINOP, ops);
	}
      else
	{
	  insn_code icode = code_for_pred_scalar (MULT, mode);
	  rtx ops[] = {step_adj, vid, step};
	  emit_vlmax_insn (icode, RVV_BINOP, ops);
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
      rtx ops[] = {result, step_adj, base};
      emit_vlmax_insn (icode, RVV_BINOP, ops);
      emit_move_insn (dest, result);
    }
}

static void
expand_const_vector (rtx target, rtx src)
{
  machine_mode mode = GET_MODE (target);
  scalar_mode elt_mode = GET_MODE_INNER (mode);
  if (GET_MODE_CLASS (mode) == MODE_VECTOR_BOOL)
    {
      rtx elt;
      gcc_assert (
	const_vec_duplicate_p (src, &elt)
	&& (rtx_equal_p (elt, const0_rtx) || rtx_equal_p (elt, const1_rtx)));
      rtx ops[] = {target, src};
      emit_vlmax_insn (code_for_pred_mov (mode), RVV_UNOP, ops);
      return;
    }

  rtx elt;
  if (const_vec_duplicate_p (src, &elt))
    {
      rtx tmp = register_operand (target, mode) ? target : gen_reg_rtx (mode);
      /* Element in range -16 ~ 15 integer or 0.0 floating-point,
	 we use vmv.v.i instruction.  */
      if (satisfies_constraint_vi (src) || satisfies_constraint_Wc0 (src))
	{
	  rtx ops[] = {tmp, src};
	  emit_vlmax_insn (code_for_pred_mov (mode), RVV_UNOP, ops);
	}
      else
	{
	  elt = force_reg (elt_mode, elt);
	  rtx ops[] = {tmp, elt};
	  emit_vlmax_insn (code_for_pred_broadcast (mode), RVV_UNOP, ops);
	}

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
legitimize_move (rtx dest, rtx src)
{
  machine_mode mode = GET_MODE (dest);
  if (CONST_VECTOR_P (src))
    {
      expand_const_vector (dest, src);
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
	{
	  rtx ops[] = {tmp, src};
	  emit_vlmax_insn (code_for_pred_mov (mode), RVV_UNOP, ops);
	}
      else
	emit_move_insn (tmp, src);
      src = tmp;
    }

  if (satisfies_constraint_vu (src))
    return false;

  rtx ops[] = {dest, src};
  emit_vlmax_insn (code_for_pred_mov (mode), RVV_UNOP, ops);
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
		     machine_mode vector_mode, bool has_vi_variant_p,
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
  rtx ops[] = {tmp, *scalar_op};
  riscv_vector::emit_nonvlmax_insn (code_for_pred_broadcast (vector_mode),
				    riscv_vector::RVV_UNOP, ops, vl);
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
expand_tuple_move (rtx *ops)
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
		{
		  rtx operands[] = {subreg, mem};
		  emit_vlmax_insn (code_for_pred_mov (subpart_mode), RVV_UNOP,
				   operands, ops[4]);
		}
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
		{
		  rtx operands[] = {mem, subreg};
		  emit_vlmax_insn (code_for_pred_mov (subpart_mode), RVV_UNOP,
				   operands, ops[4]);
		}
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
      rtx ops[] = {target, target, builder.elt (i)};
      emit_vlmax_insn (icode, RVV_BINOP, ops);
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

/* Get insn code for corresponding comparison.  */

static insn_code
get_cmp_insn_code (rtx_code code, machine_mode mode)
{
  insn_code icode;
  switch (code)
    {
    case EQ:
    case NE:
    case LE:
    case LEU:
    case GT:
    case GTU:
    case LTGT:
      icode = code_for_pred_cmp (mode);
      break;
    case LT:
    case LTU:
    case GE:
    case GEU:
      if (FLOAT_MODE_P (mode))
	icode = code_for_pred_cmp (mode);
      else
	icode = code_for_pred_ltge (mode);
      break;
    default:
      gcc_unreachable ();
    }
  return icode;
}

/* This hook gives the vectorizer more vector mode options.  We want it to not
   only try modes with the maximum number of units a full vector can hold but
   for example also half the number of units for a smaller elements size.
   Such vectors can be promoted to a full vector of widened elements
   (still with the same number of elements, essentially vectorizing at a
   fixed number of units rather than a fixed number of bytes).  */
unsigned int
autovectorize_vector_modes (vector_modes *modes, bool)
{
  if (autovec_use_vlmax_p ())
    {
      /* TODO: We will support RVV VLS auto-vectorization mode in the future. */
      poly_uint64 full_size
	= BYTES_PER_RISCV_VECTOR * ((int) riscv_autovec_lmul);

      /* Start with a VNxYYQImode where YY is the number of units that
	 fit a whole vector.
	 Then try YY = nunits / 2, nunits / 4 and nunits / 8 which
	 is guided by the extensions we have available (vf2, vf4 and vf8).

	 - full_size: Try using full vectors for all element types.
	 - full_size / 2:
	   Try using 16-bit containers for 8-bit elements and full vectors
	   for wider elements.
	 - full_size / 4:
	   Try using 32-bit containers for 8-bit and 16-bit elements and
	   full vectors for wider elements.
	 - full_size / 8:
	   Try using 64-bit containers for all element types.  */
      static const int rvv_factors[] = {1, 2, 4, 8};
      for (unsigned int i = 0; i < sizeof (rvv_factors) / sizeof (int); i++)
	{
	  poly_uint64 units;
	  machine_mode mode;
	  if (can_div_trunc_p (full_size, rvv_factors[i], &units)
	      && get_vector_mode (QImode, units).exists (&mode))
	    modes->safe_push (mode);
	}
    }
  return 0;
}

/* If the given VECTOR_MODE is an RVV mode,  first get the largest number
   of units that fit into a full vector at the given ELEMENT_MODE.
   We will have the vectorizer call us with a successively decreasing
   number of units (as specified in autovectorize_vector_modes).
   The starting mode is always the one specified by preferred_simd_mode. */
opt_machine_mode
vectorize_related_mode (machine_mode vector_mode, scalar_mode element_mode,
			poly_uint64 nunits)
{
  /* TODO: We will support RVV VLS auto-vectorization mode in the future. */
  poly_uint64 min_units;
  if (autovec_use_vlmax_p () && riscv_v_ext_vector_mode_p (vector_mode)
      && multiple_p (BYTES_PER_RISCV_VECTOR * ((int) riscv_autovec_lmul),
		     GET_MODE_SIZE (element_mode), &min_units))
    {
      machine_mode rvv_mode;
      if (maybe_ne (nunits, 0U))
	{
	  /* If we were given a number of units NUNITS, try to find an
	     RVV vector mode of inner mode ELEMENT_MODE with the same
	     number of units.  */
	  if (multiple_p (min_units, nunits)
	      && get_vector_mode (element_mode, nunits).exists (&rvv_mode))
	    return rvv_mode;
	}
      else
	{
	  /* Look for a vector mode with the same number of units as the
	     VECTOR_MODE we were given.  We keep track of the minimum
	     number of units so far which determines the smallest necessary
	     but largest possible, suitable mode for vectorization.  */
	  min_units = ordered_min (min_units, GET_MODE_SIZE (vector_mode));
	  if (get_vector_mode (element_mode, min_units).exists (&rvv_mode))
	    return rvv_mode;
	}
    }

  return default_vectorize_related_mode (vector_mode, element_mode, nunits);
}

/* Expand an RVV comparison.  */

void
expand_vec_cmp (rtx target, rtx_code code, rtx op0, rtx op1)
{
  machine_mode mask_mode = GET_MODE (target);
  machine_mode data_mode = GET_MODE (op0);
  insn_code icode = get_cmp_insn_code (code, data_mode);

  if (code == LTGT)
    {
      rtx lt = gen_reg_rtx (mask_mode);
      rtx gt = gen_reg_rtx (mask_mode);
      expand_vec_cmp (lt, LT, op0, op1);
      expand_vec_cmp (gt, GT, op0, op1);
      icode = code_for_pred (IOR, mask_mode);
      rtx ops[] = {target, lt, gt};
      emit_vlmax_insn (icode, riscv_vector::RVV_BINOP, ops);
      return;
    }

  rtx cmp = gen_rtx_fmt_ee (code, mask_mode, op0, op1);
  rtx ops[] = {target, cmp, op0, op1};
  emit_vlmax_cmp_insn (icode, ops);
}

void
expand_vec_cmp (rtx target, rtx_code code, rtx mask, rtx maskoff, rtx op0,
		rtx op1)
{
  machine_mode mask_mode = GET_MODE (target);
  machine_mode data_mode = GET_MODE (op0);
  insn_code icode = get_cmp_insn_code (code, data_mode);

  if (code == LTGT)
    {
      rtx lt = gen_reg_rtx (mask_mode);
      rtx gt = gen_reg_rtx (mask_mode);
      expand_vec_cmp (lt, LT, mask, maskoff, op0, op1);
      expand_vec_cmp (gt, GT, mask, maskoff, op0, op1);
      icode = code_for_pred (IOR, mask_mode);
      rtx ops[] = {target, lt, gt};
      emit_vlmax_insn (icode, RVV_BINOP, ops);
      return;
    }

  rtx cmp = gen_rtx_fmt_ee (code, mask_mode, op0, op1);
  rtx ops[] = {target, mask, maskoff, cmp, op0, op1};
  emit_vlmax_cmp_mu_insn (icode, ops);
}

/* Expand an RVV floating-point comparison:

   If CAN_INVERT_P is true, the caller can also handle inverted results;
   return true if the result is in fact inverted.  */

bool
expand_vec_cmp_float (rtx target, rtx_code code, rtx op0, rtx op1,
		      bool can_invert_p)
{
  machine_mode mask_mode = GET_MODE (target);
  machine_mode data_mode = GET_MODE (op0);

  /* If can_invert_p = true:
     It suffices to implement a u>= b as !(a < b) but with the NaNs masked off:

       vmfeq.vv    v0, va, va
       vmfeq.vv    v1, vb, vb
       vmand.mm    v0, v0, v1
       vmflt.vv    v0, va, vb, v0.t
       vmnot.m     v0, v0

     And, if !HONOR_SNANS, then you can remove the vmand.mm by masking the
     second vmfeq.vv:

       vmfeq.vv    v0, va, va
       vmfeq.vv    v0, vb, vb, v0.t
       vmflt.vv    v0, va, vb, v0.t
       vmnot.m     v0, v0

     If can_invert_p = false:

       # Example of implementing isgreater()
       vmfeq.vv v0, va, va        # Only set where A is not NaN.
       vmfeq.vv v1, vb, vb        # Only set where B is not NaN.
       vmand.mm v0, v0, v1        # Only set where A and B are ordered,
       vmfgt.vv v0, va, vb, v0.t  #  so only set flags on ordered values.
  */

  rtx eq0 = gen_reg_rtx (mask_mode);
  rtx eq1 = gen_reg_rtx (mask_mode);
  switch (code)
    {
    case EQ:
    case NE:
    case LT:
    case LE:
    case GT:
    case GE:
    case LTGT:
      /* There is native support for the comparison.  */
      expand_vec_cmp (target, code, op0, op1);
      return false;
    case UNEQ:
    case ORDERED:
    case UNORDERED:
    case UNLT:
    case UNLE:
    case UNGT:
    case UNGE:
      /* vmfeq.vv v0, va, va  */
      expand_vec_cmp (eq0, EQ, op0, op0);
      if (HONOR_SNANS (data_mode))
	{
	  /*
	     vmfeq.vv    v1, vb, vb
	     vmand.mm    v0, v0, v1
	  */
	  expand_vec_cmp (eq1, EQ, op1, op1);
	  insn_code icode = code_for_pred (AND, mask_mode);
	  rtx ops[] = {eq0, eq0, eq1};
	  emit_vlmax_insn (icode, riscv_vector::RVV_BINOP, ops);
	}
      else
	{
	  /* vmfeq.vv    v0, vb, vb, v0.t  */
	  expand_vec_cmp (eq0, EQ, eq0, eq0, op1, op1);
	}
      break;
    default:
      gcc_unreachable ();
    }

  if (code == ORDERED)
    {
      emit_move_insn (target, eq0);
      return false;
    }

  /* There is native support for the inverse comparison.  */
  code = reverse_condition_maybe_unordered (code);
  if (code == ORDERED)
    emit_move_insn (target, eq0);
  else
    expand_vec_cmp (eq0, code, eq0, eq0, op0, op1);

  if (can_invert_p)
    {
      emit_move_insn (target, eq0);
      return true;
    }

  /* We use one_cmpl<mode>2 to make Combine PASS to combine mask instructions
     into: vmand.mm/vmnor.mm/vmnand.mm/vmnor.mm/vmxnor.mm.  */
  emit_insn (gen_rtx_SET (target, gen_rtx_NOT (mask_mode, eq0)));
  return false;
}

/* Expand an RVV vcond pattern with operands OPS.  DATA_MODE is the mode
   of the data being merged and CMP_MODE is the mode of the values being
   compared.  */

void
expand_vcond (rtx *ops)
{
  machine_mode cmp_mode = GET_MODE (ops[4]);
  machine_mode data_mode = GET_MODE (ops[1]);
  machine_mode mask_mode = get_mask_mode (cmp_mode).require ();
  rtx mask = gen_reg_rtx (mask_mode);
  if (FLOAT_MODE_P (cmp_mode))
    {
      if (expand_vec_cmp_float (mask, GET_CODE (ops[3]), ops[4], ops[5], true))
	std::swap (ops[1], ops[2]);
    }
  else
    expand_vec_cmp (mask, GET_CODE (ops[3]), ops[4], ops[5]);
  emit_insn (
    gen_vcond_mask (data_mode, data_mode, ops[0], ops[1], ops[2], mask));
}

} // namespace riscv_vector
