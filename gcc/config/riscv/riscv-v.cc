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

/* We have a maximum of 11 operands for RVV instruction patterns according to
   the vector.md.  */
#define RVV_INSN_OPERANDS_MAX 11

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
      m_has_mask_policy_p (false), m_has_fp_rounding_mode_p (false),
      m_tail_policy (TAIL_ANY), m_mask_policy (MASK_ANY),
      m_fp_rounding_mode (FRM_DYN),
      m_dest_mode (VOIDmode), m_mask_mode (VOIDmode),
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
      m_has_mask_policy_p (false), m_has_fp_rounding_mode_p (false),
      m_tail_policy (TAIL_ANY), m_mask_policy (MASK_ANY),
      m_fp_rounding_mode (FRM_DYN),
      m_dest_mode (dest_mode),
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

  void set_rounding_mode (enum floating_point_rounding_mode mode)
  {
    m_has_fp_rounding_mode_p = true;
    m_fp_rounding_mode = mode;
  }

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

  void add_rounding_mode_operand ()
  {
    if (m_has_fp_rounding_mode_p)
      {
	rtx frm_rtx = gen_int_mode (m_fp_rounding_mode, Pmode);
	add_input_operand (frm_rtx, Pmode);
      }
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
	    if (riscv_v_ext_vls_mode_p (m_dest_mode))
	      {
		/* VLS modes always set VSETVL by
		   "vsetvl zero, rs1/imm".  */
		poly_uint64 nunits = GET_MODE_NUNITS (m_dest_mode);
		len = gen_int_mode (nunits, Pmode);
		if (!satisfies_constraint_K (len))
		  len = force_reg (Pmode, len);
		m_vlmax_p = false; /* It has became NONVLMAX now.  */
	      }
	    else if (const_vlmax_p (m_dest_mode))
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

    add_rounding_mode_operand ();

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
  bool m_has_fp_rounding_mode_p;
  enum tail_policy m_tail_policy;
  enum mask_policy m_mask_policy;
  enum floating_point_rounding_mode m_fp_rounding_mode;
  machine_mode m_dest_mode;
  machine_mode m_mask_mode;
  rtx m_vl_op;
  expand_operand m_ops[MAX_OPERANDS];
};


class rvv_builder : public rtx_vector_builder
{
public:
  rvv_builder () : rtx_vector_builder () {}
  rvv_builder (machine_mode mode, unsigned int npatterns,
	       unsigned int nelts_per_pattern)
    : rtx_vector_builder (mode, npatterns, nelts_per_pattern)
  {
    m_inner_mode = GET_MODE_INNER (mode);
    m_inner_bits_size = GET_MODE_BITSIZE (m_inner_mode);
    m_inner_bytes_size = GET_MODE_SIZE (m_inner_mode);
    m_mask_mode = get_mask_mode (mode);

    gcc_assert (
      int_mode_for_size (inner_bits_size (), 0).exists (&m_inner_int_mode));
    m_int_mode
      = get_vector_mode (m_inner_int_mode, GET_MODE_NUNITS (mode)).require ();
  }

  bool can_duplicate_repeating_sequence_p ();
  rtx get_merged_repeating_sequence ();

  bool repeating_sequence_use_merge_profitable_p ();
  rtx get_merge_scalar_mask (unsigned int) const;

  bool single_step_npatterns_p () const;
  bool npatterns_all_equal_p () const;

  machine_mode new_mode () const { return m_new_mode; }
  scalar_mode inner_mode () const { return m_inner_mode; }
  scalar_int_mode inner_int_mode () const { return m_inner_int_mode; }
  machine_mode mask_mode () const { return m_mask_mode; }
  machine_mode int_mode () const { return m_int_mode; }
  unsigned int inner_bits_size () const { return m_inner_bits_size; }
  unsigned int inner_bytes_size () const { return m_inner_bytes_size; }

private:
  scalar_mode m_inner_mode;
  scalar_int_mode m_inner_int_mode;
  machine_mode m_new_mode;
  scalar_int_mode m_new_inner_mode;
  machine_mode m_mask_mode;
  machine_mode m_int_mode;
  unsigned int m_inner_bits_size;
  unsigned int m_inner_bytes_size;
};

/* Return true if the vector duplicated by a super element which is the fusion
   of consecutive elements.

     v = { a, b, a, b } super element = ab, v = { ab, ab }  */
bool
rvv_builder::can_duplicate_repeating_sequence_p ()
{
  poly_uint64 new_size = exact_div (full_nelts (), npatterns ());
  unsigned int new_inner_size = m_inner_bits_size * npatterns ();
  if (!int_mode_for_size (new_inner_size, 0).exists (&m_new_inner_mode)
      || GET_MODE_SIZE (m_new_inner_mode) > UNITS_PER_WORD
      || !get_vector_mode (m_new_inner_mode, new_size).exists (&m_new_mode))
    return false;
  if (full_nelts ().is_constant ())
    return repeating_sequence_p (0, full_nelts ().to_constant (), npatterns ());
  return nelts_per_pattern () == 1;
}

/* Return true if it is a repeating sequence that using
   merge approach has better codegen than using default
   approach (slide1down).

   Sequence A:
     {a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b}

   nelts = 16
   npatterns = 2

   for merging a we need mask 101010....
   for merging b we need mask 010101....

   Foreach element in the npattern, we need to build a mask in scalar register.
   Mostely we need 3 instructions (aka COST = 3), which is consist of 2 scalar
   instruction and 1 scalar move to v0 register.  Finally we need vector merge
   to merge them.

   lui		a5, #imm
   add		a5, #imm
   vmov.s.x	v0, a5
   vmerge.vxm	v9, v9, a1, v0

   So the overall (roughly) COST of Sequence A = (3 + 1) * npatterns = 8.
   If we use slide1down, the COST = nelts = 16 > 8 (COST of merge).
   So return true in this case as it is profitable.

   Sequence B:
     {a, b, c, d, e, f, g, h, a, b, c, d, e, f, g, h}

   nelts = 16
   npatterns = 8

   COST of merge approach = (3 + 1) * npatterns = 24
   COST of slide1down approach = nelts = 16
   Return false in this case as it is NOT profitable in merge approach.
*/
bool
rvv_builder::repeating_sequence_use_merge_profitable_p ()
{
  if (inner_bytes_size () > UNITS_PER_WORD)
    return false;

  unsigned int nelts = full_nelts ().to_constant ();

  if (!repeating_sequence_p (0, nelts, npatterns ()))
    return false;

  unsigned int merge_cost = 1;
  unsigned int build_merge_mask_cost = 3;
  unsigned int slide1down_cost = nelts;

  return (build_merge_mask_cost + merge_cost) * npatterns () < slide1down_cost;
}

/* Merge the repeating sequence into a single element and return the RTX.  */
rtx
rvv_builder::get_merged_repeating_sequence ()
{
  scalar_int_mode mode = Pmode;
  rtx target = gen_reg_rtx (mode);
  emit_move_insn (target, const0_rtx);
  rtx imm = gen_int_mode ((1ULL << m_inner_bits_size) - 1, mode);
  /* { a, b, a, b }: Generate duplicate element = b << bits | a.  */
  for (unsigned int i = 0; i < npatterns (); i++)
    {
      unsigned int loc = m_inner_bits_size * i;
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

/* Get the mask for merge approach.

   Consider such following case:
     {a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b}
   To merge "a", the mask should be 1010....
   To merge "b", the mask should be 0101....
*/
rtx
rvv_builder::get_merge_scalar_mask (unsigned int index_in_pattern) const
{
  unsigned HOST_WIDE_INT mask = 0;
  unsigned HOST_WIDE_INT base_mask = (1ULL << index_in_pattern);
  /* Here we construct a mask pattern that will later be broadcast
     to a vector register.  The maximum broadcast size for vmv.v.x/vmv.s.x
     is determined by the length of a vector element (ELEN) and not by
     XLEN so make sure we do not exceed it.  One example is -march=zve32*
     which mandates ELEN == 32 but can be combined with -march=rv64
     with XLEN == 64.  */
  unsigned int elen = TARGET_VECTOR_ELEN_64 ? 64 : 32;

  gcc_assert (elen % npatterns () == 0);

  int limit = elen / npatterns ();

  for (int i = 0; i < limit; i++)
    mask |= base_mask << (i * npatterns ());

  return gen_int_mode (mask, inner_int_mode ());
}

/* Return true if the variable-length vector is single step.
   Single step means step all patterns in NPATTERNS are equal.
   Consider this following case:

     CASE 1: NPATTERNS = 2, NELTS_PER_PATTERN = 3.
       { 0, 2, 2, 4, 4, 6, ... }
     First pattern: step1 = 2 - 0 = 2
		    step2 = 4 - 2 = 2
     Second pattern: step1 = 4 - 2 = 2
		     step2 = 6 - 4 = 2
     Since all steps of NPATTERNS are equal step = 2.
     Return true in this case.

     CASE 2: NPATTERNS = 2, NELTS_PER_PATTERN = 3.
       { 0, 1, 2, 4, 4, 7, ... }
     First pattern: step1 = 2 - 0 = 2
		    step2 = 4 - 2 = 2
     Second pattern: step1 = 4 - 1 = 3
		     step2 = 7 - 4 = 3
     Since not all steps are equal, return false.  */
bool
rvv_builder::single_step_npatterns_p () const
{
  if (nelts_per_pattern () != 3)
    return false;

  poly_int64 step
    = rtx_to_poly_int64 (elt (npatterns ())) - rtx_to_poly_int64 (elt (0));
  for (unsigned int i = 0; i < npatterns (); i++)
    {
      poly_int64 ele0 = rtx_to_poly_int64 (elt (i));
      poly_int64 ele1 = rtx_to_poly_int64 (elt (npatterns () + i));
      poly_int64 ele2 = rtx_to_poly_int64 (elt (npatterns () * 2 + i));
      poly_int64 diff1 = ele1 - ele0;
      poly_int64 diff2 = ele2 - ele1;
      if (maybe_ne (step, diff1) || maybe_ne (step, diff2))
	return false;
    }
  return true;
}

/* Return true if all elements of NPATTERNS are equal.

   E.g. NPATTERNS = 4:
     { 2, 2, 2, 2, 4, 4, 4, 4, 8, 8, 8, 8, 16, 16, 16, 16, ... }
   E.g. NPATTERNS = 8:
     { 2, 2, 2, 2, 2, 2, 2, 2, 8, 8, 8, 8, 8, 8, 8, 8, ... }
   We only check ele[0] ~ ele[NPATTERNS - 1] whether they are the same.
   We don't need to check the elements[n] with n >= NPATTERNS since
   they don't belong to the same pattern.
*/
bool
rvv_builder::npatterns_all_equal_p () const
{
  poly_int64 ele0 = rtx_to_poly_int64 (elt (0));
  for (unsigned int i = 1; i < npatterns (); i++)
    {
      poly_int64 ele = rtx_to_poly_int64 (elt (i));
      if (!known_eq (ele, ele0))
	return false;
    }
  return true;
}

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

/* Return true if VEC is a constant in which every element is in the range
   [MINVAL, MAXVAL].  The elements do not need to have the same value.

   This function also exists in aarch64, we may unify it in middle-end in the
   future.  */

static bool
const_vec_all_in_range_p (rtx vec, poly_int64 minval, poly_int64 maxval)
{
  if (!CONST_VECTOR_P (vec)
      || GET_MODE_CLASS (GET_MODE (vec)) != MODE_VECTOR_INT)
    return false;

  int nunits;
  if (!CONST_VECTOR_STEPPED_P (vec))
    nunits = const_vector_encoded_nelts (vec);
  else if (!CONST_VECTOR_NUNITS (vec).is_constant (&nunits))
    return false;

  for (int i = 0; i < nunits; i++)
    {
      rtx vec_elem = CONST_VECTOR_ELT (vec, i);
      poly_int64 value;
      if (!poly_int_rtx_p (vec_elem, &value)
	  || maybe_lt (value, minval)
	  || maybe_gt (value, maxval))
	return false;
    }
  return true;
}

/* Return a const vector of VAL. The VAL can be either const_int or
   const_poly_int.  */

static rtx
gen_const_vector_dup (machine_mode mode, poly_int64 val)
{
  scalar_mode smode = GET_MODE_INNER (mode);
  rtx c = gen_int_mode (val, smode);
  if (!val.is_constant () && GET_MODE_SIZE (smode) > GET_MODE_SIZE (Pmode))
    {
      /* When VAL is const_poly_int value, we need to explicitly broadcast
	 it into a vector using RVV broadcast instruction.  */
      rtx dup = gen_reg_rtx (mode);
      emit_insn (gen_vec_duplicate (mode, dup, c));
      return dup;
    }
   return gen_const_vec_duplicate (mode, c);
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
  machine_mode dest_mode = GET_MODE (ops[0]);
  machine_mode mask_mode = get_mask_mode (dest_mode);
  insn_expander<RVV_INSN_OPERANDS_MAX> e (op_num,
					  /* HAS_DEST_P */ true,
					  /* FULLY_UNMASKED_P */ true,
					  /* USE_REAL_MERGE_P */ false,
					  /* HAS_AVL_P */ true,
					  /* VLMAX_P */ true,
					  dest_mode,
					  mask_mode);

  e.set_policy (TAIL_ANY);
  e.set_policy (MASK_ANY);
  /* According to LRA mov pattern in vector.md, we have a clobber operand
     to be used ad VL operand.  */
  e.set_vl (vl);
  e.emit_insn ((enum insn_code) icode, ops);
}

void
emit_vlmax_fp_insn (unsigned icode, int op_num, rtx *ops, rtx vl)
{
  machine_mode dest_mode = GET_MODE (ops[0]);
  machine_mode mask_mode = get_mask_mode (dest_mode);
  insn_expander<RVV_INSN_OPERANDS_MAX> e (op_num,
					  /* HAS_DEST_P */ true,
					  /* FULLY_UNMASKED_P */ true,
					  /* USE_REAL_MERGE_P */ false,
					  /* HAS_AVL_P */ true,
					  /* VLMAX_P */ true,
					  dest_mode,
					  mask_mode);

  e.set_policy (TAIL_ANY);
  e.set_policy (MASK_ANY);
  e.set_rounding_mode (FRM_DYN);
  e.set_vl (vl);
  e.emit_insn ((enum insn_code) icode, ops);
}

/* This function emits a {VLMAX, TAIL_ANY, MASK_ANY} vsetvli followed by the
 * ternary operation which always has a real merge operand.  */
void
emit_vlmax_ternary_insn (unsigned icode, int op_num, rtx *ops, rtx vl)
{
  machine_mode dest_mode = GET_MODE (ops[0]);
  machine_mode mask_mode = get_mask_mode (dest_mode);
  insn_expander<RVV_INSN_OPERANDS_MAX> e (/*OP_NUM*/ op_num,
					  /*HAS_DEST_P*/ true,
					  /*FULLY_UNMASKED_P*/ true,
					  /*USE_REAL_MERGE_P*/ true,
					  /*HAS_AVL_P*/ true,
					  /*VLMAX_P*/ true,
					  /*DEST_MODE*/ dest_mode,
					  /*MASK_MODE*/ mask_mode);
  e.set_policy (TAIL_ANY);
  e.set_policy (MASK_ANY);
  e.set_vl (vl);
  e.emit_insn ((enum insn_code) icode, ops);
}

/* This function emits a {VLMAX, TAIL_ANY, MASK_ANY} vsetvli followed by the
 * ternary operation which always has a real merge operand.  */
void
emit_vlmax_fp_ternary_insn (unsigned icode, int op_num, rtx *ops, rtx vl)
{
  machine_mode dest_mode = GET_MODE (ops[0]);
  machine_mode mask_mode = get_mask_mode (dest_mode);
  insn_expander<RVV_INSN_OPERANDS_MAX> e (/*OP_NUM*/ op_num,
					  /*HAS_DEST_P*/ true,
					  /*FULLY_UNMASKED_P*/ true,
					  /*USE_REAL_MERGE_P*/ true,
					  /*HAS_AVL_P*/ true,
					  /*VLMAX_P*/ true,
					  /*DEST_MODE*/ dest_mode,
					  /*MASK_MODE*/ mask_mode);
  e.set_policy (TAIL_ANY);
  e.set_policy (MASK_ANY);
  e.set_rounding_mode (FRM_DYN);
  e.set_vl (vl);
  e.emit_insn ((enum insn_code) icode, ops);
}

/* This function emits a {NONVLMAX, TAIL_UNDISTURBED, MASK_ANY} vsetvli followed
 * by the ternary operation which always has a real merge operand.  */
static void
emit_nonvlmax_fp_ternary_tu_insn (unsigned icode, int op_num, rtx *ops, rtx vl)
{
  machine_mode dest_mode = GET_MODE (ops[0]);
  machine_mode mask_mode = get_mask_mode (dest_mode);
  insn_expander<RVV_INSN_OPERANDS_MAX> e (/*OP_NUM*/ op_num,
					  /*HAS_DEST_P*/ true,
					  /*FULLY_UNMASKED_P*/ false,
					  /*USE_REAL_MERGE_P*/ true,
					  /*HAS_AVL_P*/ true,
					  /*VLMAX_P*/ false,
					  /*DEST_MODE*/ dest_mode,
					  /*MASK_MODE*/ mask_mode);
  e.set_policy (TAIL_UNDISTURBED);
  e.set_policy (MASK_ANY);
  e.set_rounding_mode (FRM_DYN);
  e.set_vl (vl);
  e.emit_insn ((enum insn_code) icode, ops);
}

/* This function emits a {NONVLMAX, TAIL_ANY, MASK_ANY} vsetvli followed by the
 * actual operation.  */
void
emit_nonvlmax_insn (unsigned icode, int op_num, rtx *ops, rtx avl)
{
  machine_mode dest_mode = GET_MODE (ops[0]);
  machine_mode mask_mode = get_mask_mode (dest_mode);
  insn_expander<RVV_INSN_OPERANDS_MAX> e (op_num,
					  /* HAS_DEST_P */ true,
					  /* FULLY_UNMASKED_P */ true,
					  /* USE_REAL_MERGE_P */ false,
					  /* HAS_AVL_P */ true,
					  /* VLMAX_P */ false,
					  dest_mode,
					  mask_mode);

  e.set_policy (TAIL_ANY);
  e.set_policy (MASK_ANY);
  e.set_vl (avl);
  e.emit_insn ((enum insn_code) icode, ops);
}

/* This function emits a {NONVLMAX, TAIL_UNDISTURBED, MASK_ANY} vsetvli
   followed by a vslide insn (with real merge operand).  */
void
emit_vlmax_slide_insn (unsigned icode, rtx *ops)
{
  machine_mode dest_mode = GET_MODE (ops[0]);
  machine_mode mask_mode = get_mask_mode (dest_mode);
  insn_expander<RVV_INSN_OPERANDS_MAX> e (RVV_SLIDE_OP,
					  /* HAS_DEST_P */ true,
					  /* FULLY_UNMASKED_P */ true,
					  /* USE_REAL_MERGE_P */ true,
					  /* HAS_AVL_P */ true,
					  /* VLMAX_P */ true,
					  dest_mode,
					  mask_mode);

  e.set_policy (TAIL_ANY);
  e.set_policy (MASK_ANY);

  e.emit_insn ((enum insn_code) icode, ops);
}

/* This function emits a {NONVLMAX, TAIL_UNDISTURBED, MASK_ANY} vsetvli
   followed by a vslide insn (with real merge operand).  */
void
emit_nonvlmax_slide_tu_insn (unsigned icode, rtx *ops, rtx avl)
{
  machine_mode dest_mode = GET_MODE (ops[0]);
  machine_mode mask_mode = get_mask_mode (dest_mode);
  insn_expander<RVV_INSN_OPERANDS_MAX> e (RVV_SLIDE_OP,
					  /* HAS_DEST_P */ true,
					  /* FULLY_UNMASKED_P */ true,
					  /* USE_REAL_MERGE_P */ true,
					  /* HAS_AVL_P */ true,
					  /* VLMAX_P */ false,
					  dest_mode,
					  mask_mode);

  e.set_policy (TAIL_UNDISTURBED);
  e.set_policy (MASK_ANY);
  e.set_vl (avl);

  e.emit_insn ((enum insn_code) icode, ops);
}


/* This function emits merge instruction.  */
void
emit_vlmax_merge_insn (unsigned icode, int op_num, rtx *ops)
{
  machine_mode dest_mode = GET_MODE (ops[0]);
  machine_mode mask_mode = get_mask_mode (dest_mode);
  insn_expander<RVV_INSN_OPERANDS_MAX> e (op_num,
					  /* HAS_DEST_P */ true,
					  /* FULLY_UNMASKED_P */ false,
					  /* USE_REAL_MERGE_P */ false,
					  /* HAS_AVL_P */ true,
					  /* VLMAX_P */ true,
					  dest_mode,
					  mask_mode);

  e.set_policy (TAIL_ANY);
  e.emit_insn ((enum insn_code) icode, ops);
}

/* This function emits cmp instruction.  */
void
emit_vlmax_cmp_insn (unsigned icode, rtx *ops)
{
  machine_mode mode = GET_MODE (ops[0]);
  insn_expander<RVV_INSN_OPERANDS_MAX> e (RVV_CMP_OP,
					  /* HAS_DEST_P */ true,
					  /* FULLY_UNMASKED_P */ true,
					  /* USE_REAL_MERGE_P */ false,
					  /* HAS_AVL_P */ true,
					  /* VLMAX_P */ true,
					  mode,
					  mode);

  e.set_policy (MASK_ANY);
  e.emit_insn ((enum insn_code) icode, ops);
}

/* This function emits cmp with MU instruction.  */
void
emit_vlmax_cmp_mu_insn (unsigned icode, rtx *ops)
{
  machine_mode mode = GET_MODE (ops[0]);
  insn_expander<RVV_INSN_OPERANDS_MAX> e (RVV_CMP_MU_OP,
					  /* HAS_DEST_P */ true,
					  /* FULLY_UNMASKED_P */ false,
					  /* USE_REAL_MERGE_P */ true,
					  /* HAS_AVL_P */ true,
					  /* VLMAX_P */ true,
					  mode,
					  mode);

  e.set_policy (MASK_UNDISTURBED);
  e.emit_insn ((enum insn_code) icode, ops);
}

/* This function emits a masked instruction.  */
static void
emit_vlmax_masked_insn (unsigned icode, int op_num, rtx *ops)
{
  machine_mode dest_mode = GET_MODE (ops[0]);
  machine_mode mask_mode = get_mask_mode (dest_mode);
  insn_expander<RVV_INSN_OPERANDS_MAX> e (/*OP_NUM*/ op_num,
					  /*HAS_DEST_P*/ true,
					  /*FULLY_UNMASKED_P*/ false,
					  /*USE_REAL_MERGE_P*/ true,
					  /*HAS_AVL_P*/ true,
					  /*VLMAX_P*/ true, dest_mode,
					  mask_mode);
  e.set_policy (TAIL_ANY);
  e.set_policy (MASK_ANY);
  e.emit_insn ((enum insn_code) icode, ops);
}

/* This function emits a masked instruction.  */
static void
emit_nonvlmax_masked_insn (unsigned icode, int op_num, rtx *ops, rtx avl)
{
  machine_mode dest_mode = GET_MODE (ops[0]);
  machine_mode mask_mode = get_mask_mode (dest_mode);
  insn_expander<RVV_INSN_OPERANDS_MAX> e (/*OP_NUM*/ op_num,
					  /*HAS_DEST_P*/ true,
					  /*FULLY_UNMASKED_P*/ false,
					  /*USE_REAL_MERGE_P*/ true,
					  /*HAS_AVL_P*/ true,
					  /*VLMAX_P*/ false, dest_mode,
					  mask_mode);
  e.set_policy (TAIL_ANY);
  e.set_policy (MASK_ANY);
  e.set_vl (avl);
  e.emit_insn ((enum insn_code) icode, ops);
}

/* This function emits a VLMAX masked store instruction.  */
static void
emit_vlmax_masked_store_insn (unsigned icode, int op_num, rtx *ops)
{
  machine_mode dest_mode = GET_MODE (ops[0]);
  machine_mode mask_mode = get_mask_mode (dest_mode);
  insn_expander<RVV_INSN_OPERANDS_MAX> e (/*OP_NUM*/ op_num,
					  /*HAS_DEST_P*/ false,
					  /*FULLY_UNMASKED_P*/ false,
					  /*USE_REAL_MERGE_P*/ true,
					  /*HAS_AVL_P*/ true,
					  /*VLMAX_P*/ true, dest_mode,
					  mask_mode);
  e.emit_insn ((enum insn_code) icode, ops);
}

/* This function emits a non-VLMAX masked store instruction.  */
static void
emit_nonvlmax_masked_store_insn (unsigned icode, int op_num, rtx *ops, rtx avl)
{
  machine_mode dest_mode = GET_MODE (ops[0]);
  machine_mode mask_mode = get_mask_mode (dest_mode);
  insn_expander<RVV_INSN_OPERANDS_MAX> e (/*OP_NUM*/ op_num,
					  /*HAS_DEST_P*/ false,
					  /*FULLY_UNMASKED_P*/ false,
					  /*USE_REAL_MERGE_P*/ true,
					  /*HAS_AVL_P*/ true,
					  /*VLMAX_P*/ false, dest_mode,
					  mask_mode);
  e.set_vl (avl);
  e.emit_insn ((enum insn_code) icode, ops);
}

/* This function emits a masked instruction.  */
void
emit_vlmax_masked_mu_insn (unsigned icode, int op_num, rtx *ops)
{
  machine_mode dest_mode = GET_MODE (ops[0]);
  machine_mode mask_mode = get_mask_mode (dest_mode);
  insn_expander<RVV_INSN_OPERANDS_MAX> e (/*OP_NUM*/ op_num,
					  /*HAS_DEST_P*/ true,
					  /*FULLY_UNMASKED_P*/ false,
					  /*USE_REAL_MERGE_P*/ true,
					  /*HAS_AVL_P*/ true,
					  /*VLMAX_P*/ true, dest_mode,
					  mask_mode);
  e.set_policy (TAIL_ANY);
  e.set_policy (MASK_UNDISTURBED);
  e.emit_insn ((enum insn_code) icode, ops);
}

/* This function emits a TU instruction.  */
static void
emit_nonvlmax_tu_insn (unsigned icode, int op_num, rtx *ops, rtx avl)
{
  machine_mode dest_mode = GET_MODE (ops[0]);
  machine_mode mask_mode = get_mask_mode (dest_mode);
  insn_expander<RVV_INSN_OPERANDS_MAX> e (/*OP_NUM*/ op_num,
					  /*HAS_DEST_P*/ true,
					  /*FULLY_UNMASKED_P*/ false,
					  /*USE_REAL_MERGE_P*/ true,
					  /*HAS_AVL_P*/ true,
					  /*VLMAX_P*/ false, dest_mode,
					  mask_mode);
  e.set_policy (TAIL_UNDISTURBED);
  e.set_policy (MASK_ANY);
  e.set_vl (avl);
  e.emit_insn ((enum insn_code) icode, ops);
}

/* This function emits a TU instruction.  */
static void
emit_nonvlmax_fp_tu_insn (unsigned icode, int op_num, rtx *ops, rtx avl)
{
  machine_mode dest_mode = GET_MODE (ops[0]);
  machine_mode mask_mode = get_mask_mode (dest_mode);
  insn_expander<RVV_INSN_OPERANDS_MAX> e (/*OP_NUM*/ op_num,
					  /*HAS_DEST_P*/ true,
					  /*FULLY_UNMASKED_P*/ false,
					  /*USE_REAL_MERGE_P*/ true,
					  /*HAS_AVL_P*/ true,
					  /*VLMAX_P*/ false, dest_mode,
					  mask_mode);
  e.set_policy (TAIL_UNDISTURBED);
  e.set_policy (MASK_ANY);
  e.set_rounding_mode (FRM_DYN);
  e.set_vl (avl);
  e.emit_insn ((enum insn_code) icode, ops);
}

/* Emit vmv.s.x instruction.  */

void
emit_scalar_move_insn (unsigned icode, rtx *ops, rtx len)
{
  machine_mode dest_mode = GET_MODE (ops[0]);
  machine_mode mask_mode = get_mask_mode (dest_mode);
  insn_expander<RVV_INSN_OPERANDS_MAX> e (RVV_SCALAR_MOV_OP,
					  /* HAS_DEST_P */ true,
					  /* FULLY_UNMASKED_P */ false,
					  /* USE_REAL_MERGE_P */ true,
					  /* HAS_AVL_P */ true,
					  /* VLMAX_P */ false,
					  dest_mode,
					  mask_mode);

  e.set_policy (TAIL_ANY);
  e.set_policy (MASK_ANY);
  e.set_vl (len ? len : CONST1_RTX (Pmode));
  e.emit_insn ((enum insn_code) icode, ops);
}

/* Emit vmv.v.x instruction with vlmax.  */

static void
emit_vlmax_integer_move_insn (unsigned icode, rtx *ops, rtx vl)
{
  emit_vlmax_insn (icode, riscv_vector::RVV_UNOP, ops, vl);
}

/* Emit vmv.v.x instruction with nonvlmax.  */

void
emit_nonvlmax_integer_move_insn (unsigned icode, rtx *ops, rtx avl)
{
  emit_nonvlmax_insn (icode, riscv_vector::RVV_UNOP, ops, avl);
}

/* This function emits VLMAX vrgather instruction. Emit vrgather.vx/vi when sel
   is a const duplicate vector. Otherwise, emit vrgather.vv.  */
static void
emit_vlmax_gather_insn (rtx target, rtx op, rtx sel)
{
  rtx elt;
  insn_code icode;
  machine_mode data_mode = GET_MODE (target);
  machine_mode sel_mode = GET_MODE (sel);
  if (maybe_ne (GET_MODE_SIZE (data_mode), GET_MODE_SIZE (sel_mode)))
    icode = code_for_pred_gatherei16 (data_mode);
  else if (const_vec_duplicate_p (sel, &elt))
    {
      icode = code_for_pred_gather_scalar (data_mode);
      sel = elt;
    }
  else
    icode = code_for_pred_gather (data_mode);
  rtx ops[] = {target, op, sel};
  emit_vlmax_insn (icode, RVV_BINOP, ops);
}

static void
emit_vlmax_masked_gather_mu_insn (rtx target, rtx op, rtx sel, rtx mask)
{
  rtx elt;
  insn_code icode;
  machine_mode data_mode = GET_MODE (target);
  machine_mode sel_mode = GET_MODE (sel);
  if (maybe_ne (GET_MODE_SIZE (data_mode), GET_MODE_SIZE (sel_mode)))
    icode = code_for_pred_gatherei16 (data_mode);
  else if (const_vec_duplicate_p (sel, &elt))
    {
      icode = code_for_pred_gather_scalar (data_mode);
      sel = elt;
    }
  else
    icode = code_for_pred_gather (data_mode);
  rtx ops[] = {target, mask, target, op, sel};
  emit_vlmax_masked_mu_insn (icode, RVV_BINOP_MU, ops);
}

/* According to RVV ISA spec (16.5.1. Synthesizing vdecompress):
   https://github.com/riscv/riscv-v-spec/blob/master/v-spec.adoc

  There is no inverse vdecompress provided, as this operation can be readily
  synthesized using iota and a masked vrgather:

      Desired functionality of 'vdecompress'
	7 6 5 4 3 2 1 0     # vid

	      e d c b a     # packed vector of 5 elements
	1 0 0 1 1 1 0 1     # mask vector of 8 elements
	p q r s t u v w     # destination register before vdecompress

	e q r d c b v a     # result of vdecompress
       # v0 holds mask
       # v1 holds packed data
       # v11 holds input expanded vector and result
       viota.m v10, v0                 # Calc iota from mask in v0
       vrgather.vv v11, v1, v10, v0.t  # Expand into destination
     p q r s t u v w  # v11 destination register
	   e d c b a  # v1 source vector
     1 0 0 1 1 1 0 1  # v0 mask vector

     4 4 4 3 2 1 1 0  # v10 result of viota.m
     e q r d c b v a  # v11 destination after vrgather using viota.m under mask
*/
static void
emit_vlmax_decompress_insn (rtx target, rtx op0, rtx op1, rtx mask)
{
  machine_mode data_mode = GET_MODE (target);
  machine_mode sel_mode = related_int_vector_mode (data_mode).require ();
  if (GET_MODE_INNER (data_mode) == QImode)
    sel_mode = get_vector_mode (HImode, GET_MODE_NUNITS (data_mode)).require ();

  rtx sel = gen_reg_rtx (sel_mode);
  rtx iota_ops[] = {sel, mask};
  emit_vlmax_insn (code_for_pred_iota (sel_mode), RVV_UNOP, iota_ops);
  emit_vlmax_gather_insn (target, op0, sel);
  emit_vlmax_masked_gather_mu_insn (target, op1, sel, mask);
}

/* Emit compress instruction.  */
static void
emit_vlmax_compress_insn (unsigned icode, rtx *ops)
{
  machine_mode dest_mode = GET_MODE (ops[0]);
  machine_mode mask_mode = get_mask_mode (dest_mode);
  insn_expander<RVV_INSN_OPERANDS_MAX> e (RVV_COMPRESS_OP,
					  /* HAS_DEST_P */ true,
					  /* FULLY_UNMASKED_P */ false,
					  /* USE_REAL_MERGE_P */ true,
					  /* HAS_AVL_P */ true,
					  /* VLMAX_P */ true, dest_mode,
					  mask_mode);

  e.set_policy (TAIL_ANY);
  e.emit_insn ((enum insn_code) icode, ops);
}

/* Emit reduction instruction.  */
static void
emit_vlmax_reduction_insn (unsigned icode, int op_num, rtx *ops)
{
  machine_mode dest_mode = GET_MODE (ops[0]);
  machine_mode mask_mode = get_mask_mode (GET_MODE (ops[1]));
  insn_expander<RVV_INSN_OPERANDS_MAX> e (op_num,
					  /* HAS_DEST_P */ true,
					  /* FULLY_UNMASKED_P */ true,
					  /* USE_REAL_MERGE_P */ false,
					  /* HAS_AVL_P */ true,
					  /* VLMAX_P */ true, dest_mode,
					  mask_mode);

  e.set_policy (TAIL_ANY);
  e.emit_insn ((enum insn_code) icode, ops);
}

/* Emit reduction instruction.  */
static void
emit_vlmax_fp_reduction_insn (unsigned icode, int op_num, rtx *ops)
{
  machine_mode dest_mode = GET_MODE (ops[0]);
  machine_mode mask_mode = get_mask_mode (GET_MODE (ops[1]));
  insn_expander<RVV_INSN_OPERANDS_MAX> e (op_num,
					  /* HAS_DEST_P */ true,
					  /* FULLY_UNMASKED_P */ true,
					  /* USE_REAL_MERGE_P */ false,
					  /* HAS_AVL_P */ true,
					  /* VLMAX_P */ true, dest_mode,
					  mask_mode);

  e.set_policy (TAIL_ANY);
  e.set_rounding_mode (FRM_DYN);
  e.emit_insn ((enum insn_code) icode, ops);
}

/* Emit reduction instruction.  */
static void
emit_nonvlmax_fp_reduction_insn (unsigned icode, int op_num, rtx *ops, rtx vl)
{
  machine_mode dest_mode = GET_MODE (ops[0]);
  machine_mode mask_mode = get_mask_mode (GET_MODE (ops[1]));
  insn_expander<RVV_INSN_OPERANDS_MAX> e (op_num,
					  /* HAS_DEST_P */ true,
					  /* FULLY_UNMASKED_P */ false,
					  /* USE_REAL_MERGE_P */ true,
					  /* HAS_AVL_P */ true,
					  /* VLMAX_P */ false, dest_mode,
					  mask_mode);

  e.set_policy (TAIL_ANY);
  e.set_rounding_mode (FRM_DYN);
  e.set_vl (vl);
  e.emit_insn ((enum insn_code) icode, ops);
}

/* Emit merge instruction.  */

static machine_mode
get_repeating_sequence_dup_machine_mode (const rvv_builder &builder)
{
  poly_uint64 dup_nunits = GET_MODE_NUNITS (builder.mode ());

  if (known_ge (GET_MODE_SIZE (builder.mode ()), BYTES_PER_RISCV_VECTOR))
    {
      dup_nunits = exact_div (BYTES_PER_RISCV_VECTOR,
	builder.inner_bytes_size ());
    }

  return get_vector_mode (builder.inner_int_mode (), dup_nunits).require ();
}

/* Expand series const vector.  */

void
expand_vec_series (rtx dest, rtx base, rtx step)
{
  machine_mode mode = GET_MODE (dest);
  poly_int64 nunits_m1 = GET_MODE_NUNITS (mode) - 1;
  poly_int64 value;

  /* VECT_IV = BASE + I * STEP.  */

  /* Step 1: Generate I = { 0, 1, 2, ... } by vid.v.  */
  rtx vid = gen_reg_rtx (mode);
  rtx op[] = {vid};
  emit_vlmax_insn (code_for_pred_series (mode), RVV_MISC_OP, op);

  /* Step 2: Generate I * STEP.
     - STEP is 1, we don't emit any instructions.
     - STEP is power of 2, we use vsll.vi/vsll.vx.
     - STEP is non-power of 2, we use vmul.vx.  */
  rtx step_adj;
  if (rtx_equal_p (step, const1_rtx))
    step_adj = vid;
  else if (rtx_equal_p (step, constm1_rtx) && poly_int_rtx_p (base, &value)
	   && known_eq (nunits_m1, value))
    {
      /* Special case:
	   {nunits - 1, nunits - 2, ... , 0}.
	   nunits can be either const_int or const_poly_int.

	 Code sequence:
	   vid.v v
	   vrsub nunits - 1, v.  */
      rtx ops[] = {dest, vid, gen_int_mode (nunits_m1, GET_MODE_INNER (mode))};
      insn_code icode = code_for_pred_sub_reverse_scalar (mode);
      emit_vlmax_insn (icode, RVV_BINOP, ops);
      return;
    }
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

  /* Handle variable-length vector.  */
  unsigned int nelts_per_pattern = CONST_VECTOR_NELTS_PER_PATTERN (src);
  unsigned int npatterns = CONST_VECTOR_NPATTERNS (src);
  rvv_builder builder (mode, npatterns, nelts_per_pattern);
  for (unsigned int i = 0; i < nelts_per_pattern; i++)
    {
      for (unsigned int j = 0; j < npatterns; j++)
	builder.quick_push (CONST_VECTOR_ELT (src, i * npatterns + j));
    }
  builder.finalize ();

  if (CONST_VECTOR_DUPLICATE_P (src))
    {
      /* Handle the case with repeating sequence that NELTS_PER_PATTERN = 1
	 E.g. NPATTERNS = 4, v = { 0, 2, 6, 7, ... }
	      NPATTERNS = 8, v = { 0, 2, 6, 7, 19, 20, 8, 7 ... }
	The elements within NPATTERNS are not necessary regular.  */
      if (builder.can_duplicate_repeating_sequence_p ())
	{
	  /* We handle the case that we can find a vector containter to hold
	     element bitsize = NPATTERNS * ele_bitsize.

	       NPATTERNS = 8, element width = 8
		 v = { 0, 1, 2, 3, 4, 5, 6, 7, ... }
	       In this case, we can combine NPATTERNS element into a larger
	       element. Use element width = 64 and broadcast a vector with
	       all element equal to 0x0706050403020100.  */
	  rtx ele = builder.get_merged_repeating_sequence ();
	  rtx dup = expand_vector_broadcast (builder.new_mode (), ele);
	  emit_move_insn (target, gen_lowpart (mode, dup));
	}
      else
	{
	  /* We handle the case that we can't find a vector containter to hold
	     element bitsize = NPATTERNS * ele_bitsize.

	       NPATTERNS = 8, element width = 16
		 v = { 0, 1, 2, 3, 4, 5, 6, 7, ... }
	       Since NPATTERNS * element width = 128, we can't find a container
	       to hold it.

	       In this case, we use NPATTERNS merge operations to generate such
	       vector.  */
	  unsigned int nbits = npatterns - 1;

	  /* Generate vid = { 0, 1, 2, 3, 4, 5, 6, 7, ... }.  */
	  rtx vid = gen_reg_rtx (builder.int_mode ());
	  rtx op[] = {vid};
	  emit_vlmax_insn (code_for_pred_series (builder.int_mode ()),
			   RVV_MISC_OP, op);

	  /* Generate vid_repeat = { 0, 1, ... nbits, ... }  */
	  rtx vid_repeat = gen_reg_rtx (builder.int_mode ());
	  rtx and_ops[] = {vid_repeat, vid,
			   gen_int_mode (nbits, builder.inner_int_mode ())};
	  emit_vlmax_insn (code_for_pred_scalar (AND, builder.int_mode ()),
			   RVV_BINOP, and_ops);

	  rtx tmp = gen_reg_rtx (builder.mode ());
	  rtx dup_ops[] = {tmp, builder.elt (0)};
	  emit_vlmax_insn (code_for_pred_broadcast (builder.mode ()), RVV_UNOP,
			   dup_ops);
	  for (unsigned int i = 1; i < builder.npatterns (); i++)
	    {
	      /* Generate mask according to i.  */
	      rtx mask = gen_reg_rtx (builder.mask_mode ());
	      rtx const_vec = gen_const_vector_dup (builder.int_mode (), i);
	      expand_vec_cmp (mask, EQ, vid_repeat, const_vec);

	      /* Merge scalar to each i.  */
	      rtx tmp2 = gen_reg_rtx (builder.mode ());
	      rtx merge_ops[] = {tmp2, tmp, builder.elt (i), mask};
	      insn_code icode = code_for_pred_merge_scalar (builder.mode ());
	      emit_vlmax_merge_insn (icode, RVV_MERGE_OP, merge_ops);
	      tmp = tmp2;
	    }
	  emit_move_insn (target, tmp);
	}
    }
  else if (CONST_VECTOR_STEPPED_P (src))
    {
      gcc_assert (GET_MODE_CLASS (mode) == MODE_VECTOR_INT);
      if (builder.single_step_npatterns_p ())
	{
	  /* Describe the case by choosing NPATTERNS = 4 as an example.  */
	  insn_code icode;

	  /* Step 1: Generate vid = { 0, 1, 2, 3, 4, 5, 6, 7, ... }.  */
	  rtx vid = gen_reg_rtx (builder.mode ());
	  rtx vid_ops[] = {vid};
	  icode = code_for_pred_series (builder.mode ());
	  emit_vlmax_insn (icode, RVV_MISC_OP, vid_ops);

	  if (builder.npatterns_all_equal_p ())
	    {
	      /* Generate the variable-length vector following this rule:
		 { a, a, a + step, a + step, a + step * 2, a + step * 2, ...}
		   E.g. { 0, 0, 8, 8, 16, 16, ... } */
	      /* We want to create a pattern where value[ix] = floor (ix /
		 NPATTERNS). As NPATTERNS is always a power of two we can
		 rewrite this as = ix & -NPATTERNS.  */
	      /* Step 2: VID AND -NPATTERNS:
		 { 0&-4, 1&-4, 2&-4, 3 &-4, 4 &-4, 5 &-4, 6 &-4, 7 &-4, ... }
	      */
	      rtx imm
		= gen_int_mode (-builder.npatterns (), builder.inner_mode ());
	      rtx tmp = gen_reg_rtx (builder.mode ());
	      rtx and_ops[] = {tmp, vid, imm};
	      icode = code_for_pred_scalar (AND, builder.mode ());
	      emit_vlmax_insn (icode, RVV_BINOP, and_ops);
	      HOST_WIDE_INT init_val = INTVAL (builder.elt (0));
	      if (init_val == 0)
		emit_move_insn (target, tmp);
	      else
		{
		  rtx dup = gen_const_vector_dup (builder.mode (), init_val);
		  rtx add_ops[] = {target, tmp, dup};
		  icode = code_for_pred (PLUS, builder.mode ());
		  emit_vlmax_insn (icode, RVV_BINOP, add_ops);
		}
	    }
	  else
	    {
	      /* Generate the variable-length vector following this rule:
		 { a, b, a, b, a + step, b + step, a + step*2, b + step*2, ...}
		   E.g. { 3, 2, 1, 0, 7, 6, 5, 4, ... } */
	      /* Step 2: Generate diff = TARGET - VID:
		 { 3-0, 2-1, 1-2, 0-3, 7-4, 6-5, 5-6, 4-7, ... }*/
	      rvv_builder v (builder.mode (), builder.npatterns (), 1);
	      for (unsigned int i = 0; i < v.npatterns (); ++i)
		{
		  /* Calculate the diff between the target sequence and
		     vid sequence.  */
		  HOST_WIDE_INT diff = INTVAL (builder.elt (i)) - i;
		  v.quick_push (gen_int_mode (diff, v.inner_mode ()));
		}
	      /* Step 2: Generate result = VID + diff.  */
	      rtx vec = v.build ();
	      rtx add_ops[] = {target, vid, vec};
	      emit_vlmax_insn (code_for_pred (PLUS, builder.mode ()), RVV_BINOP,
			       add_ops);
	    }
	}
      else
	/* TODO: We will enable more variable-length vector in the future.  */
	gcc_unreachable ();
    }
  else
    gcc_unreachable ();
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

  if (riscv_v_ext_vls_mode_p (mode))
    {
      if (GET_MODE_NUNITS (mode).to_constant () <= 31)
	{
	  /* For NUNITS <= 31 VLS modes, we don't need extrac
	     scalar regisers so we apply the naive (set (op0) (op1)) pattern. */
	  if (can_create_pseudo_p ())
	    {
	      /* Need to force register if mem <- !reg.  */
	      if (MEM_P (dest) && !REG_P (src))
		src = force_reg (mode, src);

	      return false;
	    }
	}
      else if (GET_MODE_NUNITS (mode).to_constant () > 31 && lra_in_progress)
	{
	  emit_insn (gen_mov_lra (mode, Pmode, dest, src));
	  return true;
	}
    }
  else
    {
      /* In order to decrease the memory traffic, we don't use whole register
       * load/store for the LMUL less than 1 and mask mode, so those case will
       * require one extra general purpose register, but it's not allowed during
       * LRA process, so we have a special move pattern used for LRA, which will
       * defer the expansion after LRA.  */
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
  enum vlmul_type vlmul[NUM_MACHINE_MODES];
  uint8_t ratio[NUM_MACHINE_MODES];
  machine_mode subpart_mode[NUM_MACHINE_MODES];
  uint8_t nf[NUM_MACHINE_MODES];
  mode_vtype_group ()
  {
#define ENTRY(MODE, REQUIREMENT, VLMUL, RATIO)                                 \
  vlmul[MODE##mode] = VLMUL;                                                   \
  ratio[MODE##mode] = RATIO;
#define TUPLE_ENTRY(MODE, REQUIREMENT, SUBPART_MODE, NF, VLMUL, RATIO)         \
  subpart_mode[MODE##mode] = SUBPART_MODE##mode;                               \
  nf[MODE##mode] = NF;                                                         \
  vlmul[MODE##mode] = VLMUL;                                                   \
  ratio[MODE##mode] = RATIO;
#include "riscv-vector-switch.def"
#undef ENTRY
#undef TUPLE_ENTRY
  }
};

static mode_vtype_group mode_vtype_infos;

/* Get vlmul field value by comparing LMUL with BYTES_PER_RISCV_VECTOR.  */
enum vlmul_type
get_vlmul (machine_mode mode)
{
  /* For VLS modes, the vlmul should be dynamically
     calculated since we need to adjust VLMUL according
     to TARGET_MIN_VLEN.  */
  if (riscv_v_ext_vls_mode_p (mode))
    {
      int size = GET_MODE_BITSIZE (mode).to_constant ();
      int inner_size = GET_MODE_BITSIZE (GET_MODE_INNER (mode));
      if (size < TARGET_MIN_VLEN)
	{
	  int factor = TARGET_MIN_VLEN / size;
	  if (inner_size == 8)
	    factor = MIN (factor, 8);
	  else if (inner_size == 16)
	    factor = MIN (factor, 4);
	  else if (inner_size == 32)
	    factor = MIN (factor, 2);
	  else if (inner_size == 64)
	    factor = MIN (factor, 1);
	  else
	    gcc_unreachable ();

	  switch (factor)
	    {
	    case 1:
	      return LMUL_1;
	    case 2:
	      return LMUL_F2;
	    case 4:
	      return LMUL_F4;
	    case 8:
	      return LMUL_F8;

	    default:
	      gcc_unreachable ();
	    }
	}
      else
	{
	  int factor = size / TARGET_MIN_VLEN;
	  switch (factor)
	    {
	    case 1:
	      return LMUL_1;
	    case 2:
	      return LMUL_2;
	    case 4:
	      return LMUL_4;
	    case 8:
	      return LMUL_8;

	    default:
	      gcc_unreachable ();
	    }
	}
    }
  return mode_vtype_infos.vlmul[mode];
}

/* Return the NF value of the corresponding mode.  */
unsigned int
get_nf (machine_mode mode)
{
  /* We don't allow non-tuple modes go through this function.  */
  gcc_assert (riscv_v_ext_tuple_mode_p (mode));
  return mode_vtype_infos.nf[mode];
}

/* Return the subpart mode of the tuple mode. For RVVM2x2SImode,
   the subpart mode is RVVM2SImode. This will help to build
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
  if (riscv_v_ext_vls_mode_p (mode))
    {
      unsigned int sew = get_sew (mode);
      vlmul_type vlmul = get_vlmul (mode);
      switch (vlmul)
	{
	case LMUL_1:
	  return sew;
	case LMUL_2:
	  return sew / 2;
	case LMUL_4:
	  return sew / 4;
	case LMUL_8:
	  return sew / 8;
	case LMUL_F8:
	  return sew * 8;
	case LMUL_F4:
	  return sew * 4;
	case LMUL_F2:
	  return sew * 2;

	default:
	  gcc_unreachable ();
	}
    }
  return mode_vtype_infos.ratio[mode];
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

machine_mode
get_mask_mode (machine_mode mode)
{
  return get_vector_mode (BImode, GET_MODE_NUNITS (mode)).require();
}

/* Return the appropriate M1 mode for MODE.  */

static opt_machine_mode
get_m1_mode (machine_mode mode)
{
  scalar_mode smode = GET_MODE_INNER (mode);
  unsigned int bytes = GET_MODE_SIZE (smode);
  poly_uint64 m1_nunits = exact_div (BYTES_PER_RISCV_VECTOR, bytes);
  return get_vector_mode (smode, m1_nunits);
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
	&& (riscv_v_ext_vector_mode_p (mode)
	    || riscv_v_ext_vls_mode_p (mode)))
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
     'can_duplicate_and_interleave_p' of tree-vect-slp.cc. Since both
     RVVM1SImode in -march=*zve32*_zvl32b and RVVM1DImode in
     -march=*zve64*_zvl64b are NUNITS = poly (1, 1), they will cause ICE in loop
     vectorizer when we enable them in this target hook. Currently, we can
     support auto-vectorization in -march=rv32_zve32x_zvl128b. Wheras,
     -march=rv32_zve32x_zvl32b or -march=rv32_zve32x_zvl64b are disabled.  */
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

/* Use merge approach to initialize the vector with repeating sequence.
   v = {a, b, a, b, a, b, a, b}.

   v = broadcast (a).
   mask = 0b01010101....
   v = merge (v, b, mask)
*/
static void
expand_vector_init_merge_repeating_sequence (rtx target,
					     const rvv_builder &builder)
{
  machine_mode dup_mode = get_repeating_sequence_dup_machine_mode (builder);
  machine_mode dup_mask_mode = get_mask_mode (dup_mode);
  machine_mode mask_mode = get_mask_mode (builder.mode ());
  uint64_t full_nelts = builder.full_nelts ().to_constant ();

  /* Step 1: Broadcast the first pattern.  */
  rtx ops[] = {target, force_reg (GET_MODE_INNER (dup_mode), builder.elt (0))};
  emit_vlmax_integer_move_insn (code_for_pred_broadcast (builder.mode ()),
				ops, NULL_RTX);

  /* Step 2: Merge the rest iteration of pattern.  */
  for (unsigned int i = 1; i < builder.npatterns (); i++)
    {
      /* Step 2-1: Generate mask register v0 for each merge.  */
      rtx merge_mask = builder.get_merge_scalar_mask (i);
      rtx mask = gen_reg_rtx (mask_mode);
      rtx dup = gen_reg_rtx (dup_mode);

      if (full_nelts <= builder.inner_bits_size ()) /* vmv.s.x.  */
	{
	  rtx ops[] = {dup, gen_scalar_move_mask (dup_mask_mode),
	    RVV_VUNDEF (dup_mode), merge_mask};
	  emit_scalar_move_insn (code_for_pred_broadcast (GET_MODE (dup)),
				 ops);
	}
      else /* vmv.v.x.  */
	{
	  rtx ops[] = {dup, force_reg (GET_MODE_INNER (dup_mode), merge_mask)};
	  rtx vl = gen_int_mode (CEIL (full_nelts, builder.inner_bits_size ()),
				 Pmode);
	  emit_nonvlmax_integer_move_insn (code_for_pred_broadcast (dup_mode),
					   ops, vl);
	}

      emit_move_insn (mask, gen_lowpart (mask_mode, dup));

      /* Step 2-2: Merge pattern according to the mask.  */
      rtx ops[] = {target, target, builder.elt (i), mask};
      emit_vlmax_merge_insn (code_for_pred_merge_scalar (GET_MODE (target)),
			     riscv_vector::RVV_MERGE_OP, ops);
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

      /* Case 2: Optimize repeating sequence cases that Case 1 can
	 not handle and it is profitable.  For example:
	 ELEMENT BITSIZE = 64.
	 v = {a, b, a, b, a, b, a, b, a, b, a, b, a, b, a, b}.
	 We can't find a vector mode for "ab" which will be combined into
	 128-bit element to duplicate.  */
      if (v.repeating_sequence_use_merge_profitable_p ())
	{
	  expand_vector_init_merge_repeating_sequence (target, v);
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
      poly_uint64 full_size
	= BYTES_PER_RISCV_VECTOR * ((int) riscv_autovec_lmul);

      /* Start with a RVV<LMUL>QImode where LMUL is the number of units that
	 fit a whole vector.
	 Then try LMUL = nunits / 2, nunits / 4 and nunits / 8 which
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
  unsigned int flag = 0;
  if (TARGET_VECTOR_VLS)
    {
      /* Enable VECT_COMPARE_COSTS between VLA modes VLS modes for scalable
	 auto-vectorization.  */
      flag |= VECT_COMPARE_COSTS;
      /* Push all VLSmodes according to TARGET_MIN_VLEN.  */
      unsigned int i = 0;
      unsigned int base_size = TARGET_MIN_VLEN * riscv_autovec_lmul / 8;
      unsigned int size = base_size;
      machine_mode mode;
      while (size > 0 && get_vector_mode (QImode, size).exists (&mode))
	{
	  modes->safe_push (mode);
	  i++;
	  size = base_size / (1U << i);
	}
    }
  return flag;
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

/* Modulo all SEL indices to ensure they are all in range if [0, MAX_SEL].  */
static rtx
modulo_sel_indices (rtx sel, poly_uint64 max_sel)
{
  rtx sel_mod;
  machine_mode sel_mode = GET_MODE (sel);
  poly_uint64 nunits = GET_MODE_NUNITS (sel_mode);
  /* If SEL is variable-length CONST_VECTOR, we don't need to modulo it.  */
  if (!nunits.is_constant () && CONST_VECTOR_P (sel))
    sel_mod = sel;
  else
    {
      rtx mod = gen_const_vector_dup (sel_mode, max_sel);
      sel_mod
	= expand_simple_binop (sel_mode, AND, sel, mod, NULL, 0, OPTAB_DIRECT);
    }
  return sel_mod;
}

/* Implement vec_perm<mode>.  */

void
expand_vec_perm (rtx target, rtx op0, rtx op1, rtx sel)
{
  machine_mode data_mode = GET_MODE (target);
  machine_mode sel_mode = GET_MODE (sel);
  poly_uint64 nunits = GET_MODE_NUNITS (sel_mode);

  /* Check if the sel only references the first values vector. If each select
     index is in range of [0, nunits - 1]. A single vrgather instructions is
     enough. Since we will use vrgatherei16.vv for variable-length vector,
     it is never out of range and we don't need to modulo the index.  */
  if (nunits.is_constant () && const_vec_all_in_range_p (sel, 0, nunits - 1))
    {
      emit_vlmax_gather_insn (target, op0, sel);
      return;
    }

  /* Check if all the indices are same.  */
  rtx elt;
  if (const_vec_duplicate_p (sel, &elt))
    {
      poly_uint64 value = rtx_to_poly_int64 (elt);
      rtx op = op0;
      if (maybe_gt (value, nunits - 1))
	{
	  sel = gen_const_vector_dup (sel_mode, value - nunits);
	  op = op1;
	}
      emit_vlmax_gather_insn (target, op, sel);
    }

  /* Note: vec_perm indices are supposed to wrap when they go beyond the
     size of the two value vectors, i.e. the upper bits of the indices
     are effectively ignored.  RVV vrgather instead produces 0 for any
     out-of-range indices, so we need to modulo all the vec_perm indices
     to ensure they are all in range of [0, nunits - 1] when op0 == op1
     or all in range of [0, 2 * nunits - 1] when op0 != op1.  */
  rtx sel_mod
    = modulo_sel_indices (sel,
			  rtx_equal_p (op0, op1) ? nunits - 1 : 2 * nunits - 1);

  /* Check if the two values vectors are the same.  */
  if (rtx_equal_p (op0, op1))
    {
      emit_vlmax_gather_insn (target, op0, sel_mod);
      return;
    }

  rtx max_sel = gen_const_vector_dup (sel_mode, 2 * nunits - 1);

  /* This following sequence is handling the case that:
     __builtin_shufflevector (vec1, vec2, index...), the index can be any
     value in range of [0, 2 * nunits - 1].  */
  machine_mode mask_mode;
  mask_mode = get_mask_mode (data_mode);
  rtx mask = gen_reg_rtx (mask_mode);
  max_sel = gen_const_vector_dup (sel_mode, nunits);

  /* Step 1: generate a mask that should select everything >= nunits into the
   * mask.  */
  expand_vec_cmp (mask, GEU, sel_mod, max_sel);

  /* Step2: gather every op0 values indexed by sel into target,
	    we don't need to care about the result of the element
	    whose index >= nunits.  */
  emit_vlmax_gather_insn (target, op0, sel_mod);

  /* Step3: shift the range from (nunits, max_of_mode] to
	    [0, max_of_mode - nunits].  */
  rtx tmp = gen_reg_rtx (sel_mode);
  rtx ops[] = {tmp, sel_mod, max_sel};
  emit_vlmax_insn (code_for_pred (MINUS, sel_mode), RVV_BINOP, ops);

  /* Step4: gather those into the previously masked-out elements
	    of target.  */
  emit_vlmax_masked_gather_mu_insn (target, op1, tmp, mask);
}

/* Implement TARGET_VECTORIZE_VEC_PERM_CONST for RVV.  */

/* vec_perm support.  */

struct expand_vec_perm_d
{
  rtx target, op0, op1;
  vec_perm_indices perm;
  machine_mode vmode;
  machine_mode op_mode;
  bool one_vector_p;
  bool testing_p;
};

/* Recognize the patterns that we can use merge operation to shuffle the
   vectors. The value of Each element (index i) in selector can only be
   either i or nunits + i.  We will check the pattern is actually monotonic.

   E.g.
   v = VEC_PERM_EXPR (v0, v1, selector),
   selector = { 0, nunits + 1, 2, nunits + 3, 4, nunits + 5, ...  }

   We can transform such pattern into:

   v = vcond_mask (v0, v1, mask),
   mask = { 0, 1, 0, 1, 0, 1, ... }.  */

static bool
shuffle_merge_patterns (struct expand_vec_perm_d *d)
{
  machine_mode vmode = d->vmode;
  machine_mode sel_mode = related_int_vector_mode (vmode).require ();
  int n_patterns = d->perm.encoding ().npatterns ();
  poly_int64 vec_len = d->perm.length ();

  for (int i = 0; i < n_patterns; ++i)
    if (!known_eq (d->perm[i], i) && !known_eq (d->perm[i], vec_len + i))
      return false;

  /* Check the pattern is monotonic here, otherwise, return false.  */
  for (int i = n_patterns; i < n_patterns * 2; i++)
    if (!d->perm.series_p (i, n_patterns, i, n_patterns)
	&& !d->perm.series_p (i, n_patterns, vec_len + i, n_patterns))
      return false;

  if (d->testing_p)
    return true;

  machine_mode mask_mode = get_mask_mode (vmode);
  rtx mask = gen_reg_rtx (mask_mode);

  rtx sel = vec_perm_indices_to_rtx (sel_mode, d->perm);

  /* MASK = SELECTOR < NUNTIS ? 1 : 0.  */
  rtx x = gen_int_mode (vec_len, GET_MODE_INNER (sel_mode));
  insn_code icode = code_for_pred_cmp_scalar (sel_mode);
  rtx cmp = gen_rtx_fmt_ee (LTU, mask_mode, sel, x);
  rtx ops[] = {mask, cmp, sel, x};
  emit_vlmax_cmp_insn (icode, ops);

  /* TARGET = MASK ? OP0 : OP1.  */
  emit_insn (gen_vcond_mask (vmode, vmode, d->target, d->op0, d->op1, mask));
  return true;
}

/* Recognize the patterns that we can use compress operation to shuffle the
   vectors. The perm selector of compress pattern is divided into 2 part:
   The first part is the random index number < NUNITS.
   The second part is consecutive last N index number >= NUNITS.

   E.g.
   v = VEC_PERM_EXPR (v0, v1, selector),
   selector = { 0, 2, 6, 7 }

   We can transform such pattern into:

   op1 = vcompress (op0, mask)
   mask = { 1, 0, 1, 0 }
   v = op1.  */

static bool
shuffle_compress_patterns (struct expand_vec_perm_d *d)
{
  machine_mode vmode = d->vmode;
  poly_int64 vec_len = d->perm.length ();

  if (!vec_len.is_constant ())
    return false;

  int vlen = vec_len.to_constant ();

  /* It's not worthwhile the compress pattern has elemenets < 4
     and we can't modulo indices for compress pattern.  */
  if (known_ge (d->perm[vlen - 1], vlen * 2) || vlen < 4)
    return false;

  /* Compress pattern doesn't work for one vector.  */
  if (d->one_vector_p)
    return false;

  /* Compress point is the point that all elements value with index i >=
     compress point of the selector are all consecutive series increasing and
     each selector value >= NUNTIS. In this case, we could compress all elements
     of i < compress point into the op1.  */
  int compress_point = -1;
  for (int i = 0; i < vlen; i++)
    {
      if (compress_point < 0 && known_ge (d->perm[i], vec_len))
	{
	  compress_point = i;
	  break;
	}
    }

  /* We don't apply compress approach if we can't find the compress point.  */
  if (compress_point < 0)
    return false;

  /* It must be series increasing from compress point.  */
  if (!d->perm.series_p (compress_point, 1, d->perm[compress_point], 1))
    return false;

  /* We can only apply compress approach when all index values from 0 to
     compress point are increasing.  */
  for (int i = 1; i < compress_point; i++)
    if (known_le (d->perm[i], d->perm[i - 1]))
      return false;

  /* Success!  */
  if (d->testing_p)
    return true;

  /* Check whether we need to slideup op1 to apply compress approach.

       E.g. For index = { 0, 2, 6, 7}, since d->perm[i - 1] = 7 which
	    is 2 * NUNITS - 1, so we don't need to slide up.

	    For index = { 0, 2, 5, 6}, we need to slide op1 up before
	    we apply compress approach.  */
  bool need_slideup_p = maybe_ne (d->perm[vlen - 1], 2 * vec_len - 1);

  /* If we leave it directly be handled by general gather,
     the code sequence will be:
	VECTOR LOAD  selector
	GEU          mask, selector, NUNITS
	GATHER       dest, op0, selector
	SUB          selector, selector, NUNITS
	GATHER       dest, op1, selector, mask
     Each ALU operation is considered as COST = 1 and VECTOR LOAD is considered
     as COST = 4. So, we consider the general gather handling COST = 9.
     TODO: This cost is not accurate, we can adjust it by tune info.  */
  int general_cost = 9;

  /* If we can use compress approach, the code squence will be:
	MASK LOAD    mask
	COMPRESS     op1, op0, mask
     If it needs slide up, it will be:
	MASK LOAD    mask
	SLIDEUP      op1
	COMPRESS     op1, op0, mask
     By default, mask load COST = 2.
     TODO: This cost is not accurate, we can adjust it by tune info.  */
  int compress_cost = 4;

  if (general_cost <= compress_cost)
    return false;

  /* Build a mask that is true when selector element is true.  */
  machine_mode mask_mode = get_mask_mode (vmode);
  rvv_builder builder (mask_mode, vlen, 1);
  for (int i = 0; i < vlen; i++)
    {
      bool is_compress_index = false;
      for (int j = 0; j < compress_point; j++)
	{
	  if (known_eq (d->perm[j], i))
	    {
	      is_compress_index = true;
	      break;
	    }
	}
      if (is_compress_index)
	builder.quick_push (CONST1_RTX (BImode));
      else
	builder.quick_push (CONST0_RTX (BImode));
    }
  rtx mask = force_reg (mask_mode, builder.build ());

  rtx merge = d->op1;
  if (need_slideup_p)
    {
      int slideup_cnt = vlen - (d->perm[vlen - 1].to_constant () % vlen) - 1;
      rtx ops[] = {d->target, RVV_VUNDEF (vmode), d->op1,
		   gen_int_mode (slideup_cnt, Pmode)};
      insn_code icode = code_for_pred_slide (UNSPEC_VSLIDEUP, vmode);
      emit_vlmax_slide_insn (icode, ops);
      merge = d->target;
    }

  insn_code icode = code_for_pred_compress (vmode);
  rtx ops[] = {d->target, merge, d->op0, mask};
  emit_vlmax_compress_insn (icode, ops);
  return true;
}

/* Recognize decompress patterns:

   1. VEC_PERM_EXPR op0 and op1
      with isel = { 0, nunits, 1, nunits + 1, ... }.
      Decompress op0 and op1 vector with the mask = { 0, 1, 0, 1, ... }.

   2. VEC_PERM_EXPR op0 and op1
      with isel = { 1/2 nunits, 3/2 nunits, 1/2 nunits+1, 3/2 nunits+1,... }.
      Slide down op0 and op1 with OFFSET = 1/2 nunits.
      Decompress op0 and op1 vector with the mask = { 0, 1, 0, 1, ... }.
*/
static bool
shuffle_decompress_patterns (struct expand_vec_perm_d *d)
{
  poly_uint64 nelt = d->perm.length ();
  machine_mode mask_mode = get_mask_mode (d->vmode);

  /* For constant size indices, we dont't need to handle it here.
     Just leave it to vec_perm<mode>.  */
  if (d->perm.length ().is_constant ())
    return false;

  poly_uint64 first = d->perm[0];
  if ((maybe_ne (first, 0U) && maybe_ne (first * 2, nelt))
      || !d->perm.series_p (0, 2, first, 1)
      || !d->perm.series_p (1, 2, first + nelt, 1))
    return false;

  /* Permuting two SEW8 variable-length vectors need vrgatherei16.vv.
     Otherwise, it could overflow the index range.  */
  machine_mode sel_mode = related_int_vector_mode (d->vmode).require ();
  if (GET_MODE_INNER (d->vmode) == QImode
      && !get_vector_mode (HImode, nelt).exists (&sel_mode))
    return false;

  /* Success!  */
  if (d->testing_p)
    return true;

  rtx op0, op1;
  if (known_eq (first, 0U))
    {
      op0 = d->op0;
      op1 = d->op1;
    }
  else
    {
      op0 = gen_reg_rtx (d->vmode);
      op1 = gen_reg_rtx (d->vmode);
      insn_code icode = code_for_pred_slide (UNSPEC_VSLIDEDOWN, d->vmode);
      rtx ops0[] = {op0, d->op0, gen_int_mode (first, Pmode)};
      rtx ops1[] = {op1, d->op1, gen_int_mode (first, Pmode)};
      emit_vlmax_insn (icode, RVV_BINOP, ops0);
      emit_vlmax_insn (icode, RVV_BINOP, ops1);
    }
  /* Generate { 0, 1, .... } mask.  */
  rtx vid = gen_reg_rtx (sel_mode);
  rtx vid_repeat = gen_reg_rtx (sel_mode);
  emit_insn (gen_vec_series (sel_mode, vid, const0_rtx, const1_rtx));
  rtx and_ops[] = {vid_repeat, vid, const1_rtx};
  emit_vlmax_insn (code_for_pred_scalar (AND, sel_mode), RVV_BINOP, and_ops);
  rtx const_vec = gen_const_vector_dup (sel_mode, 1);
  rtx mask = gen_reg_rtx (mask_mode);
  expand_vec_cmp (mask, EQ, vid_repeat, const_vec);
  emit_vlmax_decompress_insn (d->target, op0, op1, mask);
  return true;
}

/* Recognize the pattern that can be shuffled by generic approach.  */

static bool
shuffle_generic_patterns (struct expand_vec_perm_d *d)
{
  machine_mode sel_mode = related_int_vector_mode (d->vmode).require ();
  poly_uint64 nunits = GET_MODE_NUNITS (d->vmode);

  /* We don't enable SLP for non-power of 2 NPATTERNS.  */
  if (!pow2p_hwi (d->perm.encoding().npatterns ()))
    return false;

  /* For constant size indices, we dont't need to handle it here.
     Just leave it to vec_perm<mode>.  */
  if (d->perm.length ().is_constant ())
    return false;

  /* Permuting two SEW8 variable-length vectors need vrgatherei16.vv.
     Otherwise, it could overflow the index range.  */
  if (GET_MODE_INNER (d->vmode) == QImode
      && !get_vector_mode (HImode, nunits).exists (&sel_mode))
    return false;

  /* Success! */
  if (d->testing_p)
    return true;

  rtx sel = vec_perm_indices_to_rtx (sel_mode, d->perm);
  expand_vec_perm (d->target, d->op0, d->op1, force_reg (sel_mode, sel));
  return true;
}

/* This function recognizes and supports different permutation patterns
   and enable VLA SLP auto-vectorization.  */
static bool
expand_vec_perm_const_1 (struct expand_vec_perm_d *d)
{
  gcc_assert (d->op_mode != E_VOIDmode);

  /* The pattern matching functions above are written to look for a small
     number to begin the sequence (0, 1, N/2).  If we begin with an index
     from the second operand, we can swap the operands.  */
  poly_int64 nelt = d->perm.length ();
  if (known_ge (d->perm[0], nelt))
    {
      d->perm.rotate_inputs (1);
      std::swap (d->op0, d->op1);
    }

  if (known_gt (nelt, 1))
    {
      if (d->vmode == d->op_mode)
	{
	  if (shuffle_merge_patterns (d))
	    return true;
	  if (shuffle_compress_patterns (d))
	    return true;
	  if (shuffle_decompress_patterns (d))
	    return true;
	  if (shuffle_generic_patterns (d))
	    return true;
	  return false;
	}
      else
	return false;
    }
  return false;
}

/* This function implements TARGET_VECTORIZE_VEC_PERM_CONST by using RVV
 * instructions.  */
bool
expand_vec_perm_const (machine_mode vmode, machine_mode op_mode, rtx target,
		       rtx op0, rtx op1, const vec_perm_indices &sel)
{
  /* RVV doesn't have Mask type pack/unpack instructions and we don't use
     mask to do the iteration loop control. Just disable it directly.  */
  if (GET_MODE_CLASS (vmode) == MODE_VECTOR_BOOL)
    return false;

  struct expand_vec_perm_d d;

  /* Check whether the mask can be applied to a single vector.  */
  if (sel.ninputs () == 1 || (op0 && rtx_equal_p (op0, op1)))
    d.one_vector_p = true;
  else if (sel.all_from_input_p (0))
    {
      d.one_vector_p = true;
      op1 = op0;
    }
  else if (sel.all_from_input_p (1))
    {
      d.one_vector_p = true;
      op0 = op1;
    }
  else
    d.one_vector_p = false;

  d.perm.new_vector (sel.encoding (), d.one_vector_p ? 1 : 2,
		     sel.nelts_per_input ());
  d.vmode = vmode;
  d.op_mode = op_mode;
  d.target = target;
  d.op0 = op0;
  if (op0 == op1)
    d.op1 = d.op0;
  else
    d.op1 = op1;
  d.testing_p = !target;

  if (!d.testing_p)
    return expand_vec_perm_const_1 (&d);

  rtx_insn *last = get_last_insn ();
  bool ret = expand_vec_perm_const_1 (&d);
  gcc_assert (last == get_last_insn ());

  return ret;
}

/* Generate no side effects vsetvl to get the vector length.  */
void
expand_select_vl (rtx *ops)
{
  poly_int64 nunits = rtx_to_poly_int64 (ops[2]);
  /* We arbitrary picked QImode as inner scalar mode to get vector mode.
     since vsetvl only demand ratio. We let VSETVL PASS to optimize it.  */
  scalar_int_mode mode = QImode;
  machine_mode rvv_mode = get_vector_mode (mode, nunits).require ();
  emit_insn (gen_no_side_effects_vsetvl_rtx (rvv_mode, ops[0], ops[1]));
}

/* Expand MASK_LEN_{LOAD,STORE}.  */
void
expand_load_store (rtx *ops, bool is_load)
{
  poly_int64 value;
  rtx mask = ops[2];
  rtx len = ops[3];
  machine_mode mode = GET_MODE (ops[0]);

  if (poly_int_rtx_p (len, &value) && known_eq (value, GET_MODE_NUNITS (mode)))
    {
      /* If the length operand is equal to VF, it is VLMAX load/store.  */
      if (is_load)
	{
	  rtx m_ops[] = {ops[0], mask, RVV_VUNDEF (mode), ops[1]};
	  emit_vlmax_masked_insn (code_for_pred_mov (mode), RVV_UNOP_M, m_ops);
	}
      else
	{
	  len = gen_reg_rtx (Pmode);
	  emit_vlmax_vsetvl (mode, len);
	  emit_insn (gen_pred_store (mode, ops[0], mask, ops[1], len,
				     get_avl_type_rtx (VLMAX)));
	}
    }
  else
    {
      if (!satisfies_constraint_K (len))
	len = force_reg (Pmode, len);
      if (is_load)
	{
	  rtx m_ops[] = {ops[0], mask, RVV_VUNDEF (mode), ops[1]};
	  emit_nonvlmax_masked_insn (code_for_pred_mov (mode), RVV_UNOP_M,
				     m_ops, len);
	}
      else
	emit_insn (gen_pred_store (mode, ops[0], mask, ops[1], len,
				   get_avl_type_rtx (NONVLMAX)));
    }
}


/* Return true if the operation is the floating-point operation need FRM.  */
static bool
needs_fp_rounding (rtx_code code, machine_mode mode)
{
  if (!FLOAT_MODE_P (mode))
    return false;
  return code != SMIN && code != SMAX;
}

/* Expand COND_LEN_*.  */
void
expand_cond_len_binop (rtx_code code, rtx *ops)
{
  rtx dest = ops[0];
  rtx mask = ops[1];
  rtx src1 = ops[2];
  rtx src2 = ops[3];
  rtx merge = ops[4];
  rtx len = ops[5];
  machine_mode mode = GET_MODE (dest);
  machine_mode mask_mode = GET_MODE (mask);

  poly_uint64 value;
  bool is_dummy_mask = rtx_equal_p (mask, CONSTM1_RTX (mask_mode));

  if (is_dummy_mask)
    {
      /* Use TU, MASK ANY policy.  */
      rtx ops[] = {dest, mask, merge, src1, src2};
      insn_code icode = code_for_pred (code, mode);
      if (needs_fp_rounding (code, mode))
	emit_nonvlmax_fp_tu_insn (icode, RVV_BINOP_TU, ops, len);
      else
	emit_nonvlmax_tu_insn (icode, RVV_BINOP_TU, ops, len);
    }
  else
    /* FIXME: Enable this case when we support it in the middle-end.  */
    gcc_unreachable ();
}

/* Prepare insn_code for gather_load/scatter_store according to
   the vector mode and index mode.  */
static insn_code
prepare_gather_scatter (machine_mode vec_mode, machine_mode idx_mode,
			bool is_load)
{
  if (!is_load)
    return code_for_pred_indexed_store (UNSPEC_UNORDERED, vec_mode, idx_mode);
  else
    {
      unsigned src_eew_bitsize = GET_MODE_BITSIZE (GET_MODE_INNER (idx_mode));
      unsigned dst_eew_bitsize = GET_MODE_BITSIZE (GET_MODE_INNER (vec_mode));
      if (dst_eew_bitsize == src_eew_bitsize)
	return code_for_pred_indexed_load_same_eew (UNSPEC_UNORDERED, vec_mode);
      else if (dst_eew_bitsize > src_eew_bitsize)
	{
	  unsigned factor = dst_eew_bitsize / src_eew_bitsize;
	  switch (factor)
	    {
	    case 2:
	      return code_for_pred_indexed_load_x2_greater_eew (
		UNSPEC_UNORDERED, vec_mode);
	    case 4:
	      return code_for_pred_indexed_load_x4_greater_eew (
		UNSPEC_UNORDERED, vec_mode);
	    case 8:
	      return code_for_pred_indexed_load_x8_greater_eew (
		UNSPEC_UNORDERED, vec_mode);
	    default:
	      gcc_unreachable ();
	    }
	}
      else
	{
	  unsigned factor = src_eew_bitsize / dst_eew_bitsize;
	  switch (factor)
	    {
	    case 2:
	      return code_for_pred_indexed_load_x2_smaller_eew (
		UNSPEC_UNORDERED, vec_mode);
	    case 4:
	      return code_for_pred_indexed_load_x4_smaller_eew (
		UNSPEC_UNORDERED, vec_mode);
	    case 8:
	      return code_for_pred_indexed_load_x8_smaller_eew (
		UNSPEC_UNORDERED, vec_mode);
	    default:
	      gcc_unreachable ();
	    }
	}
    }
}

/* Expand LEN_MASK_{GATHER_LOAD,SCATTER_STORE}.  */
void
expand_gather_scatter (rtx *ops, bool is_load)
{
  rtx ptr, vec_offset, vec_reg;
  bool zero_extend_p;
  int scale_log2;
  rtx mask = ops[5];
  rtx len = ops[6];
  if (is_load)
    {
      vec_reg = ops[0];
      ptr = ops[1];
      vec_offset = ops[2];
      zero_extend_p = INTVAL (ops[3]);
      scale_log2 = exact_log2 (INTVAL (ops[4]));
    }
  else
    {
      vec_reg = ops[4];
      ptr = ops[0];
      vec_offset = ops[1];
      zero_extend_p = INTVAL (ops[2]);
      scale_log2 = exact_log2 (INTVAL (ops[3]));
    }

  machine_mode vec_mode = GET_MODE (vec_reg);
  machine_mode idx_mode = GET_MODE (vec_offset);
  scalar_mode inner_vec_mode = GET_MODE_INNER (vec_mode);
  scalar_mode inner_idx_mode = GET_MODE_INNER (idx_mode);
  unsigned inner_vsize = GET_MODE_BITSIZE (inner_vec_mode);
  unsigned inner_offsize = GET_MODE_BITSIZE (inner_idx_mode);
  poly_int64 nunits = GET_MODE_NUNITS (vec_mode);
  poly_int64 value;
  bool is_vlmax = poly_int_rtx_p (len, &value) && known_eq (value, nunits);

  if (inner_offsize < inner_vsize)
    {
      /* 7.2. Vector Load/Store Addressing Modes.
	 If the vector offset elements are narrower than XLEN, they are
	 zero-extended to XLEN before adding to the ptr effective address. If
	 the vector offset elements are wider than XLEN, the least-significant
	 XLEN bits are used in the address calculation. An implementation must
	 raise an illegal instruction exception if the EEW is not supported for
	 offset elements.

	 RVV spec only refers to the scale_log == 0 case.  */
      if (!zero_extend_p || (zero_extend_p && scale_log2 != 0))
	{
	  if (zero_extend_p)
	    inner_idx_mode
	      = int_mode_for_size (inner_offsize * 2, 0).require ();
	  else
	    inner_idx_mode = int_mode_for_size (BITS_PER_WORD, 0).require ();
	  machine_mode new_idx_mode
	    = get_vector_mode (inner_idx_mode, nunits).require ();
	  rtx tmp = gen_reg_rtx (new_idx_mode);
	  emit_insn (gen_extend_insn (tmp, vec_offset, new_idx_mode, idx_mode,
				      zero_extend_p ? true : false));
	  vec_offset = tmp;
	  idx_mode = new_idx_mode;
	}
    }

  if (scale_log2 != 0)
    {
      rtx tmp = expand_binop (idx_mode, ashl_optab, vec_offset,
			      gen_int_mode (scale_log2, Pmode), NULL_RTX, 0,
			      OPTAB_DIRECT);
      vec_offset = tmp;
    }

  insn_code icode = prepare_gather_scatter (vec_mode, idx_mode, is_load);
  if (is_vlmax)
    {
      if (is_load)
	{
	  rtx load_ops[]
	    = {vec_reg, mask, RVV_VUNDEF (vec_mode), ptr, vec_offset};
	  emit_vlmax_masked_insn (icode, RVV_GATHER_M_OP, load_ops);
	}
      else
	{
	  rtx store_ops[] = {mask, ptr, vec_offset, vec_reg};
	  emit_vlmax_masked_store_insn (icode, RVV_SCATTER_M_OP, store_ops);
	}
    }
  else
    {
      if (is_load)
	{
	  rtx load_ops[]
	    = {vec_reg, mask, RVV_VUNDEF (vec_mode), ptr, vec_offset};
	  emit_nonvlmax_masked_insn (icode, RVV_GATHER_M_OP, load_ops, len);
	}
      else
	{
	  rtx store_ops[] = {mask, ptr, vec_offset, vec_reg};
	  emit_nonvlmax_masked_store_insn (icode, RVV_SCATTER_M_OP, store_ops,
					   len);
	}
    }
}

/* Expand COND_LEN_*.  */
void
expand_cond_len_ternop (unsigned icode, rtx *ops)
{
  rtx dest = ops[0];
  rtx mask = ops[1];
  rtx len = ops[6];
  machine_mode mode = GET_MODE (dest);
  machine_mode mask_mode = GET_MODE (mask);

  poly_uint64 value;
  bool is_dummy_mask = rtx_equal_p (mask, CONSTM1_RTX (mask_mode));

  if (is_dummy_mask)
    {
      /* Use TU, MASK ANY policy.  */
      if (FLOAT_MODE_P (mode))
	emit_nonvlmax_fp_ternary_tu_insn (icode, RVV_TERNOP_TU, ops, len);
      else
	/* FIXME: Enable this case when we support it in the middle-end.  */
	gcc_unreachable ();
    }
  else
    /* FIXME: Enable this case when we support it in the middle-end.  */
    gcc_unreachable ();
}

/* Expand reduction operations.  */
void
expand_reduction (rtx_code code, rtx *ops, rtx init, reduction_type type)
{
  rtx vector = type == reduction_type::UNORDERED ? ops[1] : ops[2];
  machine_mode vmode = GET_MODE (vector);
  machine_mode m1_mode = get_m1_mode (vmode).require ();
  machine_mode m1_mmode = get_mask_mode (m1_mode);

  rtx m1_tmp = gen_reg_rtx (m1_mode);
  rtx m1_mask = gen_scalar_move_mask (m1_mmode);
  rtx m1_undef = RVV_VUNDEF (m1_mode);
  rtx scalar_move_ops[] = {m1_tmp, m1_mask, m1_undef, init};
  rtx len = type == reduction_type::MASK_LEN_FOLD_LEFT ? ops[4] : NULL_RTX;
  emit_scalar_move_insn (code_for_pred_broadcast (m1_mode), scalar_move_ops,
			 len);

  rtx m1_tmp2 = gen_reg_rtx (m1_mode);
  rtx reduc_ops[] = {m1_tmp2, vector, m1_tmp};

  if (FLOAT_MODE_P (vmode) && code == PLUS)
    {
      insn_code icode
	= code_for_pred_reduc_plus (type == reduction_type::UNORDERED
				      ? UNSPEC_UNORDERED
				      : UNSPEC_ORDERED,
				    vmode, m1_mode);
      if (type == reduction_type::MASK_LEN_FOLD_LEFT)
	{
	  rtx mask = ops[3];
	  rtx mask_len_reduc_ops[]
	    = {m1_tmp2, mask, RVV_VUNDEF (m1_mode), vector, m1_tmp};
	  emit_nonvlmax_fp_reduction_insn (icode, RVV_REDUCTION_TU_OP,
					   mask_len_reduc_ops, len);
	}
      else
	emit_vlmax_fp_reduction_insn (icode, RVV_REDUCTION_OP, reduc_ops);
    }
  else
    {
      insn_code icode = code_for_pred_reduc (code, vmode, m1_mode);
      emit_vlmax_reduction_insn (icode, RVV_REDUCTION_OP, reduc_ops);
    }

  emit_insn (gen_pred_extract_first (m1_mode, ops[0], m1_tmp2));
}

} // namespace riscv_vector
