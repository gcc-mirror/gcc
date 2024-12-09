/* Subroutines used for code generation for RISC-V 'V' Extension for
   GNU compiler.
   Copyright (C) 2022-2024 Free Software Foundation, Inc.
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
#include "predict.h"
#include "errors.h"
#include "riscv-v.h"

using namespace riscv_vector;

namespace riscv_vector {

/* Return true if NUNITS <=31 so that we can use immediate AVL in vsetivli.  */
bool
imm_avl_p (machine_mode mode)
{
  poly_uint64 nunits = GET_MODE_NUNITS (mode);

  return nunits.is_constant ()
	   /* The vsetivli can only hold register 0~31.  */
	   ? (IN_RANGE (nunits.to_constant (), 0, 31))
	   /* Only allowed in VLS-VLMAX mode.  */
	   : false;
}

/* Return true if LEN is equal to NUNITS that out of the range [0, 31].  */
static bool
is_vlmax_len_p (machine_mode mode, rtx len)
{
  poly_int64 value;
  return poly_int_rtx_p (len, &value)
	 && known_eq (value, GET_MODE_NUNITS (mode));
}

/* Helper functions for insn_flags && insn_types */

/* Return true if caller need pass mask operand for insn pattern with
   INSN_FLAGS. */

static bool
need_mask_operand_p (unsigned insn_flags)
{
  return (insn_flags & HAS_MASK_P)
	 && !(insn_flags & (USE_ONE_TRUE_MASK_P | USE_ALL_TRUES_MASK_P));
}

template <int MAX_OPERANDS> class insn_expander
{
public:
  insn_expander () = delete;

  insn_expander (unsigned insn_flags, bool vlmax_p)
    : m_insn_flags (insn_flags), m_opno (0), m_vlmax_p (vlmax_p),
      m_vl_op (NULL_RTX)
  {
    check_insn_flags ();
  }

  void check_insn_flags () const
  {
    if (m_insn_flags & USE_ONE_TRUE_MASK_P)
      /* USE_ONE_TRUE_MASK_P is dependent on HAS_MASK_P.  */
      gcc_assert ((m_insn_flags & HAS_MASK_P));

    if (m_insn_flags & USE_ALL_TRUES_MASK_P)
      /* USE_ALL_TRUES_MASK_P is dependent on HAS_MASK_P.  */
      gcc_assert ((m_insn_flags & HAS_MASK_P));

    /* USE_ONE_TRUE_MASK_P and USE_ALL_TRUES_MASK_P are mutually exclusive.  */
    gcc_assert (!((m_insn_flags & USE_ONE_TRUE_MASK_P)
		  && (m_insn_flags & USE_ALL_TRUES_MASK_P)));

    if (m_insn_flags & USE_VUNDEF_MERGE_P)
      /* USE_VUNDEF_MERGE_P is dependent on HAS_MERGE_P.  */
      gcc_assert ((m_insn_flags & HAS_MERGE_P));

    /* TU_POLICY_P and TDEFAULT_POLICY_P are mutually exclusive.  */
    gcc_assert (
      !((m_insn_flags & TU_POLICY_P) && (m_insn_flags & TDEFAULT_POLICY_P)));

    /* MU_POLICY_P and MDEFAULT_POLICY_P are mutually exclusive.  */
    gcc_assert (
      !((m_insn_flags & MU_POLICY_P) && (m_insn_flags & MDEFAULT_POLICY_P)));

    /* NULLARY_OP_P, UNARY_OP_P, BINARY_OP_P, TERNARY_OP_P are mutually
       exclusive.  */
    gcc_assert (
      !((m_insn_flags & NULLARY_OP_P)
	&& ((m_insn_flags & UNARY_OP_P) || (m_insn_flags & BINARY_OP_P)
	    || (m_insn_flags & TERNARY_OP_P))));
    gcc_assert (
      !((m_insn_flags & UNARY_OP_P)
	&& ((m_insn_flags & NULLARY_OP_P) || (m_insn_flags & BINARY_OP_P)
	    || (m_insn_flags & TERNARY_OP_P))));
    gcc_assert (
      !((m_insn_flags & BINARY_OP_P)
	&& ((m_insn_flags & NULLARY_OP_P) || (m_insn_flags & UNARY_OP_P)
	    || (m_insn_flags & TERNARY_OP_P))));
    gcc_assert (
      !((m_insn_flags & TERNARY_OP_P)
	&& ((m_insn_flags & NULLARY_OP_P) || (m_insn_flags & UNARY_OP_P)
	    || (m_insn_flags & BINARY_OP_P))));
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
  void add_all_one_mask_operand (machine_mode mask_mode)
  {
    add_input_operand (CONSTM1_RTX (mask_mode), mask_mode);
  }
  void add_first_one_true_mask_operand (machine_mode mask_mode)
  {
    add_input_operand (gen_scalar_move_mask (mask_mode), mask_mode);
  }
  void add_vundef_operand (machine_mode dest_mode)
  {
    add_input_operand (RVV_VUNDEF (dest_mode), dest_mode);
  }
  void add_policy_operand ()
  {
    if (m_insn_flags & TU_POLICY_P)
      {
	rtx tail_policy_rtx = gen_int_mode (TAIL_UNDISTURBED, Pmode);
	add_input_operand (tail_policy_rtx, Pmode);
      }
    else if (m_insn_flags & TDEFAULT_POLICY_P)
      {
	rtx tail_policy_rtx = gen_int_mode (get_prefer_tail_policy (), Pmode);
	add_input_operand (tail_policy_rtx, Pmode);
      }

    if (m_insn_flags & MU_POLICY_P)
      {
	rtx mask_policy_rtx = gen_int_mode (MASK_UNDISTURBED, Pmode);
	add_input_operand (mask_policy_rtx, Pmode);
      }
    else if (m_insn_flags & MDEFAULT_POLICY_P)
      {
	rtx mask_policy_rtx = gen_int_mode (get_prefer_mask_policy (), Pmode);
	add_input_operand (mask_policy_rtx, Pmode);
      }
  }
  void add_avl_type_operand (avl_type type)
  {
    add_input_operand (gen_int_mode (type, Pmode), Pmode);
  }

  void
  add_rounding_mode_operand (enum floating_point_rounding_mode rounding_mode)
  {
    rtx frm_rtx = gen_int_mode (rounding_mode, Pmode);
    add_input_operand (frm_rtx, Pmode);
  }

  void
  add_rounding_mode_operand (enum fixed_point_rounding_mode rounding_mode)
  {
    rtx frm_rtx = gen_int_mode (rounding_mode, Pmode);
    add_input_operand (frm_rtx, Pmode);
  }

  /* Return the vtype mode based on insn_flags.
     vtype mode mean the mode vsetvl insn set. */
  machine_mode
  get_vtype_mode (rtx *ops)
  {
    machine_mode vtype_mode;
    if (m_insn_flags & VTYPE_MODE_FROM_OP1_P)
      vtype_mode = GET_MODE (ops[1]);
    else
      vtype_mode = GET_MODE (ops[0]);
    return vtype_mode;
  }

  void emit_insn (enum insn_code icode, rtx *ops)
  {
    int opno = 0;
    int num_ops;
    /* It's true if any operand is memory operand.  */
    bool any_mem_p = false;

    machine_mode vtype_mode = get_vtype_mode (ops);
    machine_mode mask_mode = get_mask_mode (vtype_mode);

    /* Add dest operand.  */
    if (m_insn_flags & HAS_DEST_P)
      {
	rtx op = ops[opno++];
	any_mem_p |= MEM_P (op);
	add_output_operand (op, GET_MODE (op));
      }

    /* Add mask operand.  */
    if (m_insn_flags & USE_ONE_TRUE_MASK_P)
      add_first_one_true_mask_operand (mask_mode);
    else if (m_insn_flags & USE_ALL_TRUES_MASK_P)
      add_all_one_mask_operand (mask_mode);
    else if (m_insn_flags & HAS_MASK_P)
      {
	machine_mode mode = insn_data[(int) icode].operand[m_opno].mode;
	gcc_assert (mode != VOIDmode);
	add_input_operand (ops[opno++], mode);
      }

    /* Add merge operand.  */
    if (m_insn_flags & USE_VUNDEF_MERGE_P)
      /* Same as dest operand.  */
      add_vundef_operand (GET_MODE (ops[0]));
    else if (m_insn_flags & HAS_MERGE_P)
      {
	machine_mode mode = insn_data[(int) icode].operand[m_opno].mode;
	gcc_assert (mode != VOIDmode);
	add_input_operand (ops[opno++], mode);
      }

    if (m_insn_flags & NULLARY_OP_P)
      num_ops = 0;
    else if (m_insn_flags & UNARY_OP_P)
      num_ops = 1;
    else if (m_insn_flags & BINARY_OP_P)
      num_ops = 2;
    else if (m_insn_flags & TERNARY_OP_P)
      num_ops = 3;
    else
      gcc_unreachable ();

    /* Add the remain operands.  */
    for (; num_ops; num_ops--, opno++)
      {
	any_mem_p |= MEM_P (ops[opno]);
	machine_mode mode = insn_data[(int) icode].operand[m_opno].mode;
	/* 'create_input_operand doesn't allow VOIDmode.
	   According to vector.md, we may have some patterns that do not have
	   explicit machine mode specifying the operand. Such operands are
	   always Pmode.  */
	if (mode == VOIDmode)
	  mode = Pmode;

	/* Early assertion ensures same mode since maybe_legitimize_operand
	   will check this.  */
	machine_mode required_mode = GET_MODE (ops[opno]);
	if (required_mode != VOIDmode && required_mode != mode)
	  internal_error ("expected mode %s for operand %d of "
			  "insn %s but got mode %s.\n",
			  GET_MODE_NAME (mode),
			  opno,
			  insn_data[(int) icode].name,
			  GET_MODE_NAME (required_mode));

	add_input_operand (ops[opno], mode);
      }

    /* Add vl operand.  */
    rtx len = m_vl_op;
    bool vls_p = false;
    if (m_vlmax_p)
      {
	if (riscv_v_ext_vls_mode_p (vtype_mode))
	  {
	    /* VLS modes always set VSETVL by
	       "vsetvl zero, rs1/imm".  */
	    poly_uint64 nunits = GET_MODE_NUNITS (vtype_mode);
	    len = gen_int_mode (nunits, Pmode);
	    vls_p = true;
	  }
	else if (can_create_pseudo_p ())
	  {
	    len = gen_reg_rtx (Pmode);
	    emit_vlmax_vsetvl (vtype_mode, len);
	  }
      }

    gcc_assert (len != NULL_RTX);
    add_input_operand (len, Pmode);

    /* Add tail and mask policy operands.  */
    add_policy_operand ();

    /* Add avl_type operand.  */
    add_avl_type_operand (
      vls_p ? avl_type::VLS
	    : (m_vlmax_p ? avl_type::VLMAX : avl_type::NONVLMAX));

    /* Add rounding mode operand.  */
    if (m_insn_flags & FRM_DYN_P)
      add_rounding_mode_operand (FRM_DYN);
    else if (m_insn_flags & FRM_RUP_P)
      add_rounding_mode_operand (FRM_RUP);
    else if (m_insn_flags & FRM_RDN_P)
      add_rounding_mode_operand (FRM_RDN);
    else if (m_insn_flags & FRM_RMM_P)
      add_rounding_mode_operand (FRM_RMM);
    else if (m_insn_flags & FRM_RNE_P)
      add_rounding_mode_operand (FRM_RNE);
    else if (m_insn_flags & VXRM_RNU_P)
      add_rounding_mode_operand (VXRM_RNU);
    else if (m_insn_flags & VXRM_RDN_P)
      add_rounding_mode_operand (VXRM_RDN);


    if (insn_data[(int) icode].n_operands != m_opno)
      internal_error ("invalid number of operands for insn %s, "
		      "expected %d but got %d.\n",
		      insn_data[(int) icode].name,
		      insn_data[(int) icode].n_operands, m_opno);

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
  unsigned m_insn_flags;
  int m_opno;
  bool m_vlmax_p;
  rtx m_vl_op;
  expand_operand m_ops[MAX_OPERANDS];
};

/* Emit an RVV insn with a vector length that equals the number of units of the
   vector mode.  For VLA modes this corresponds to VLMAX.

   Unless the vector length can be encoded in the vsetivl[i] instruction this
   function must only be used as long as we can create pseudo registers. This is
   because it will set a pseudo register to VLMAX using vsetvl and use this as
   definition for the vector length.  */
void
emit_vlmax_insn (unsigned icode, unsigned insn_flags, rtx *ops)
{
  insn_expander<RVV_INSN_OPERANDS_MAX> e (insn_flags, true);
  gcc_assert (can_create_pseudo_p () || imm_avl_p (e.get_vtype_mode (ops)));

  e.emit_insn ((enum insn_code) icode, ops);
}

/* Like emit_vlmax_insn but must only be used when we cannot create pseudo
   registers anymore.  This function, however, takes a predefined vector length
   from the value in VL. */
void
emit_vlmax_insn_lra (unsigned icode, unsigned insn_flags, rtx *ops, rtx vl)
{
  gcc_assert (!can_create_pseudo_p ());
  machine_mode mode = GET_MODE (ops[0]);

  if (imm_avl_p (mode))
    {
      /* Even though VL is a real hardreg already allocated since
	 it is post-RA now, we still gain benefits that we emit
	 vsetivli zero, imm instead of vsetvli VL, zero which is
	 we can be more flexible in post-RA instruction scheduling.  */
      insn_expander<RVV_INSN_OPERANDS_MAX> e (insn_flags, false);
      e.set_vl (gen_int_mode (GET_MODE_NUNITS (mode), Pmode));
      e.emit_insn ((enum insn_code) icode, ops);
    }
  else
    {
      insn_expander<RVV_INSN_OPERANDS_MAX> e (insn_flags, true);
      e.set_vl (vl);
      e.emit_insn ((enum insn_code) icode, ops);
    }
}

/* Emit an RVV insn with a predefined vector length.  Contrary to
   emit_vlmax_insn the instruction's vector length is not deduced from its mode
   but taken from  the value in VL.  */
void
emit_nonvlmax_insn (unsigned icode, unsigned insn_flags, rtx *ops, rtx vl)
{
  insn_expander<RVV_INSN_OPERANDS_MAX> e (insn_flags, false);
  e.set_vl (vl);
  e.emit_insn ((enum insn_code) icode, ops);
}

/* Return true if the vector duplicated by a super element which is the fusion
   of consecutive elements.

     v = { a, b, a, b } super element = ab, v = { ab, ab }  */
bool
rvv_builder::can_duplicate_repeating_sequence_p ()
{
  poly_uint64 new_size = exact_div (full_nelts (), npatterns ());
  unsigned int new_inner_size = m_inner_bits_size * npatterns ();
  if (m_inner_mode == Pmode
      || !int_mode_for_size (new_inner_size, 0).exists (&m_new_inner_mode)
      || GET_MODE_SIZE (m_new_inner_mode) > UNITS_PER_WORD
      || !get_vector_mode (m_new_inner_mode, new_size).exists (&m_new_mode))
    return false;
  return repeating_sequence_p (0, encoded_nelts (), npatterns ());
}

/* Return true if the vector is a simple sequence with one pattern and all
   elements the same.  */
bool
rvv_builder::is_repeating_sequence ()
{
  if (npatterns () > 1)
    return false;
  return repeating_sequence_p (0, encoded_nelts (), 1);
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
   Mostly we need 3 instructions (aka COST = 3), which consists of 2 scalar
   instructions and 1 scalar move to v0 register.  Finally we need vector merge
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

  if (!repeating_sequence_p (0, encoded_nelts (), npatterns ()))
    return false;

  unsigned int merge_cost = 1;
  unsigned int build_merge_mask_cost = 3;
  unsigned int slide1down_cost = nelts;

  return (build_merge_mask_cost + merge_cost) * npatterns () < slide1down_cost;
}

/* Return true if it's worthwhile to use slideup combine 2 vectors.  */
bool
rvv_builder::combine_sequence_use_slideup_profitable_p ()
{
  int nelts = full_nelts ().to_constant ();
  int leading_ndups = this->count_dups (0, nelts - 1, 1);
  int trailing_ndups = this->count_dups (nelts - 1, -1, -1);

  /* ??? Current heuristic we do is we do combine 2 vectors
     by slideup when:
       1. # of leading same elements is equal to # of trailing same elements.
       2. Both of above are equal to nelts / 2.
     Otherwise, it is not profitable.  */
  return leading_ndups == trailing_ndups && trailing_ndups == nelts / 2;
}

/* Return true if it's worthwhile to use merge combine vector with a scalar.  */
bool
rvv_builder::combine_sequence_use_merge_profitable_p ()
{
  int nelts = full_nelts ().to_constant ();
  int leading_ndups = this->count_dups (0, nelts - 1, 1);
  int trailing_ndups = this->count_dups (nelts - 1, -1, -1);
  int nregs = riscv_get_v_regno_alignment (int_mode ());

  if (leading_ndups + trailing_ndups != nelts)
    return false;

  /* Leading elements num > 255 which exceeds the maximum value
     of QImode, we will need to use HImode.  */
  machine_mode mode;
  if (leading_ndups > 255 || nregs > 2)
    {
      if (!get_vector_mode (HImode, nelts).exists (&mode))
	return false;
      /* We will need one more AVL/VL toggling vsetvl instruction.  */
      return leading_ndups > 4 && trailing_ndups > 4;
    }

  /* { a, a, a, b, b, ... , b } and { b, b, b, a, a, ... , a }
     consume 3 slide instructions.  */
  return leading_ndups > 3 && trailing_ndups > 3;
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
rvv_builder::get_merge_scalar_mask (unsigned int index_in_pattern,
				    machine_mode inner_mode) const
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

  return gen_int_mode (mask, inner_mode);
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

/* Return true if the diff between const vector and vid sequence
   is repeated. For example as below cases:
   The diff means the const vector - vid.
     CASE 1:
     CONST VECTOR: {3, 2, 1, 0, 7, 6, 5, 4, ... }
     VID         : {0, 1, 2, 3, 4, 5, 6, 7, ... }
     DIFF(MINUS) : {3, 1,-1,-3, 3, 1,-1,-3, ... }
     The diff sequence {3, 1,-1,-3} is repeated in the npattern and
     return TRUE for case 1.

     CASE 2:
     CONST VECTOR: {-4, 4,-3, 5,-2, 6,-1, 7, ...}
     VID         : { 0, 1, 2, 3, 4, 5, 6, 7, ... }
     DIFF(MINUS) : {-4, 3,-5,-2,-6, 1,-7, 0, ... }
     The diff sequence {-4, 3} is not repeated in the npattern and
     return FALSE for case 2.  */
bool
rvv_builder::npatterns_vid_diff_repeated_p () const
{
  if (nelts_per_pattern () != 3)
    return false;
  else if (npatterns () == 0)
    return false;

  for (unsigned i = 0; i < npatterns (); i++)
    {
      poly_int64 diff_0 = rtx_to_poly_int64 (elt (i)) - i;
      poly_int64 diff_1
	= rtx_to_poly_int64 (elt (npatterns () + i)) - npatterns () - i;

      if (maybe_ne (diff_0, diff_1))
	return false;
    }

  return true;
}

/* Return true if the permutation consists of two
   interleaved patterns with a constant step each.
   TODO: We currently only support NPATTERNS = 2.  */
bool
rvv_builder::interleaved_stepped_npatterns_p () const
{
  if (npatterns () != 2 || nelts_per_pattern () != 3)
    return false;
  for (unsigned int i = 0; i < npatterns (); i++)
    {
      poly_int64 ele0 = rtx_to_poly_int64 (elt (i));
      poly_int64 ele1 = rtx_to_poly_int64 (elt (npatterns () + i));
      poly_int64 ele2 = rtx_to_poly_int64 (elt (npatterns () * 2 + i));
      poly_int64 diff1 = ele1 - ele0;
      poly_int64 diff2 = ele2 - ele1;
      if (maybe_ne (diff1, diff2))
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

/* Returns true if the vector's elements are all duplicates in
   range -16 ~ 15 integer or 0.0 floating-point.  */

bool
valid_vec_immediate_p (rtx x)
{
  return (satisfies_constraint_vi (x) || satisfies_constraint_Wc0 (x));
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
      return expand_vector_broadcast (mode, c);
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
   compile-time unknown). ZVL means that the vector-length is specific
   (compile-time known by march like zvl*b). Both SCALABLE and ZVL are doing
   auto-vectorization using VLMAX vsetvl configuration.  */
static bool
autovec_use_vlmax_p (void)
{
  return rvv_vector_bits == RVV_VECTOR_BITS_SCALABLE
	  || rvv_vector_bits == RVV_VECTOR_BITS_ZVL;
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
  if (const_vec_duplicate_p (sel, &elt))
    {
      icode = code_for_pred_gather_scalar (data_mode);
      sel = elt;
    }
  else if (maybe_ne (GET_MODE_SIZE (data_mode), GET_MODE_SIZE (sel_mode)))
    icode = code_for_pred_gatherei16 (data_mode);
  else
    icode = code_for_pred_gather (data_mode);
  rtx ops[] = {target, op, sel};
  emit_vlmax_insn (icode, BINARY_OP, ops);
}

static void
emit_vlmax_masked_gather_mu_insn (rtx target, rtx op, rtx sel, rtx mask)
{
  rtx elt;
  insn_code icode;
  machine_mode data_mode = GET_MODE (target);
  machine_mode sel_mode = GET_MODE (sel);
  if (const_vec_duplicate_p (sel, &elt))
    {
      icode = code_for_pred_gather_scalar (data_mode);
      sel = elt;
    }
  else if (maybe_ne (GET_MODE_SIZE (data_mode), GET_MODE_SIZE (sel_mode)))
    icode = code_for_pred_gatherei16 (data_mode);
  else
    icode = code_for_pred_gather (data_mode);
  rtx ops[] = {target, mask, target, op, sel};
  emit_vlmax_insn (icode, BINARY_OP_TAMU, ops);
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
  emit_vlmax_insn (code_for_pred_iota (sel_mode), UNARY_OP, iota_ops);
  emit_vlmax_gather_insn (target, op0, sel);
  emit_vlmax_masked_gather_mu_insn (target, op1, sel, mask);
}

/* Emit merge instruction.  */

static machine_mode
get_repeating_sequence_dup_machine_mode (const rvv_builder &builder,
					 machine_mode mask_bit_mode)
{
  unsigned mask_precision = GET_MODE_PRECISION (mask_bit_mode).to_constant ();
  unsigned mask_scalar_size = mask_precision > builder.inner_bits_size ()
    ? builder.inner_bits_size () : mask_precision;

  scalar_mode inner_mode;
  unsigned minimal_bits_size;

  switch (mask_scalar_size)
    {
      case 8:
	inner_mode = QImode;
	minimal_bits_size = TARGET_MIN_VLEN / 8; /* AKA RVVMF8.  */
	break;
      case 16:
	inner_mode = HImode;
	minimal_bits_size = TARGET_MIN_VLEN / 4; /* AKA RVVMF4.  */
	break;
      case 32:
	inner_mode = SImode;
	minimal_bits_size = TARGET_MIN_VLEN / 2; /* AKA RVVMF2.  */
	break;
      case 64:
	inner_mode = DImode;
	minimal_bits_size = TARGET_MIN_VLEN / 1; /* AKA RVVM1.  */
	break;
      default:
	gcc_unreachable ();
	break;
    }

  gcc_assert (mask_precision % mask_scalar_size == 0);

  uint64_t dup_nunit = mask_precision > mask_scalar_size
    ? mask_precision / mask_scalar_size : minimal_bits_size / mask_scalar_size;

  return get_vector_mode (inner_mode, dup_nunit).require ();
}

/* Expand series const vector.  If VID is NULL_RTX, we use vid.v
   instructions to generate sequence for VID:

     VID = { 0, 1, 2, 3, ... }

   Otherwise, we use the VID argument directly.  */

void
expand_vec_series (rtx dest, rtx base, rtx step, rtx vid)
{
  machine_mode mode = GET_MODE (dest);
  poly_int64 nunits_m1 = GET_MODE_NUNITS (mode) - 1;
  poly_int64 value;
  rtx result = register_operand (dest, mode) ? dest : gen_reg_rtx (mode);

  /* VECT_IV = BASE + I * STEP.  */

  /* Step 1: Generate I = { 0, 1, 2, ... } by vid.v.  */
  bool reverse_p = !vid && rtx_equal_p (step, constm1_rtx)
		   && poly_int_rtx_p (base, &value)
		   && known_eq (nunits_m1, value);
  if (!vid)
    {
      vid = gen_reg_rtx (mode);
      rtx op[] = {vid};
      emit_vlmax_insn (code_for_pred_series (mode), NULLARY_OP, op);
    }

  rtx step_adj;
  if (reverse_p)
    {
      /* Special case:
	   {nunits - 1, nunits - 2, ... , 0}.
	   nunits can be either const_int or const_poly_int.

	 Code sequence:
	   vid.v v
	   vrsub nunits - 1, v.  */
      rtx ops[]
	= {result, vid, gen_int_mode (nunits_m1, GET_MODE_INNER (mode))};
      insn_code icode = code_for_pred_sub_reverse_scalar (mode);
      emit_vlmax_insn (icode, BINARY_OP, ops);
    }
  else
    {
      /* Step 2: Generate I * STEP.
	 - STEP is 1, we don't emit any instructions.
	 - STEP is power of 2, we use vsll.vi/vsll.vx.
	 - STEP is non-power of 2, we use vmul.vx.  */
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
	      emit_vlmax_insn (icode, BINARY_OP, ops);
	    }
	  else
	    {
	      insn_code icode = code_for_pred_scalar (MULT, mode);
	      rtx ops[] = {step_adj, vid, step};
	      emit_vlmax_insn (icode, BINARY_OP, ops);
	    }
	}

      /* Step 3: Generate BASE + I * STEP.
	  - BASE is 0, use result of vid.
	  - BASE is not 0, we use vadd.vx/vadd.vi.  */
      if (rtx_equal_p (base, const0_rtx))
	emit_move_insn (result, step_adj);
      else
	{
	  insn_code icode = code_for_pred_scalar (PLUS, mode);
	  rtx ops[] = {result, step_adj, base};
	  emit_vlmax_insn (icode, BINARY_OP, ops);
	}
    }

  if (result != dest)
    emit_move_insn (dest, result);
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
      emit_vlmax_insn (icode, BINARY_OP, ops);
    }
}

/* Subroutine of expand_vec_init to handle case
   when all trailing elements of builder are same.
   This works as follows:
   (a) Use expand_insn interface to broadcast last vector element in TARGET.
   (b) Insert remaining elements in TARGET using insr.

   ??? The heuristic used is to do above if number of same trailing elements
   is greater than leading_ndups, loosely based on
   heuristic from mostly_zeros_p.  May need fine-tuning.  */

static bool
expand_vector_init_trailing_same_elem (rtx target,
				       const rtx_vector_builder &builder,
				       int nelts_reqd)
{
  int leading_ndups = builder.count_dups (0, nelts_reqd - 1, 1);
  int trailing_ndups = builder.count_dups (nelts_reqd - 1, -1, -1);
  machine_mode mode = GET_MODE (target);

  if (trailing_ndups > leading_ndups)
    {
      rtx dup = expand_vector_broadcast (mode, builder.elt (nelts_reqd - 1));
      for (int i = nelts_reqd - trailing_ndups - 1; i >= 0; i--)
	{
	  unsigned int unspec
	    = FLOAT_MODE_P (mode) ? UNSPEC_VFSLIDE1UP : UNSPEC_VSLIDE1UP;
	  insn_code icode = code_for_pred_slide (unspec, mode);
	  rtx tmp = gen_reg_rtx (mode);
	  rtx ops[] = {tmp, dup, builder.elt (i)};
	  emit_vlmax_insn (icode, BINARY_OP, ops);
	  /* slide1up need source and dest to be different REG.  */
	  dup = tmp;
	}

      emit_move_insn (target, dup);
      return true;
    }

  return false;
}

static void
expand_const_vector (rtx target, rtx src)
{
  machine_mode mode = GET_MODE (target);
  rtx result = register_operand (target, mode) ? target : gen_reg_rtx (mode);
  rtx elt;
  if (const_vec_duplicate_p (src, &elt))
    {
      if (GET_MODE_CLASS (mode) == MODE_VECTOR_BOOL)
	{
	  gcc_assert (rtx_equal_p (elt, const0_rtx)
		      || rtx_equal_p (elt, const1_rtx));
	  rtx ops[] = {result, src};
	  emit_vlmax_insn (code_for_pred_mov (mode), UNARY_MASK_OP, ops);
	}
      /* Element in range -16 ~ 15 integer or 0.0 floating-point,
	 we use vmv.v.i instruction.  */
      else if (valid_vec_immediate_p (src))
	{
	  rtx ops[] = {result, src};
	  emit_vlmax_insn (code_for_pred_mov (mode), UNARY_OP, ops);
	}
      else
	{
	  /* Emit vec_duplicate<mode> split pattern before RA so that
	     we could have a better optimization opportunity in LICM
	     which will hoist vmv.v.x outside the loop and in fwprop && combine
	     which will transform 'vv' into 'vx' instruction.

	     The reason we don't emit vec_duplicate<mode> split pattern during
	     RA since the split stage after RA is a too late stage to generate
	     RVV instruction which need an additional register (We can't
	     allocate a new register after RA) for VL operand of vsetvl
	     instruction (vsetvl a5, zero).  */
	  if (lra_in_progress)
	    {
	      rtx ops[] = {result, elt};
	      emit_vlmax_insn (code_for_pred_broadcast (mode), UNARY_OP, ops);
	    }
	  else
	    {
	      struct expand_operand ops[2];
	      enum insn_code icode = optab_handler (vec_duplicate_optab, mode);
	      gcc_assert (icode != CODE_FOR_nothing);
	      create_output_operand (&ops[0], result, mode);
	      create_input_operand (&ops[1], elt, GET_MODE_INNER (mode));
	      expand_insn (icode, 2, ops);
	      result = ops[0].value;
	    }
	}

      if (result != target)
	emit_move_insn (target, result);
      return;
    }

  /* Support scalable const series vector.  */
  rtx base, step;
  if (const_vec_series_p (src, &base, &step))
    {
      expand_vec_series (result, base, step);

      if (result != target)
	emit_move_insn (target, result);
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
	  /* We handle the case that we can find a vector container to hold
	     element bitsize = NPATTERNS * ele_bitsize.

	       NPATTERNS = 8, element width = 8
		 v = { 0, 1, 2, 3, 4, 5, 6, 7, ... }
	       In this case, we can combine NPATTERNS element into a larger
	       element. Use element width = 64 and broadcast a vector with
	       all element equal to 0x0706050403020100.  */
	  rtx ele = builder.get_merged_repeating_sequence ();
	  rtx dup = expand_vector_broadcast (builder.new_mode (), ele);
	  emit_move_insn (result, gen_lowpart (mode, dup));
	}
      else
	{
	  /* We handle the case that we can't find a vector container to hold
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
			    NULLARY_OP, op);

	  /* Generate vid_repeat = { 0, 1, ... nbits, ... }  */
	  rtx vid_repeat = gen_reg_rtx (builder.int_mode ());
	  rtx and_ops[] = {vid_repeat, vid,
			   gen_int_mode (nbits, builder.inner_int_mode ())};
	  emit_vlmax_insn (code_for_pred_scalar (AND, builder.int_mode ()),
			    BINARY_OP, and_ops);

	  rtx tmp1 = gen_reg_rtx (builder.mode ());
	  rtx dup_ops[] = {tmp1, builder.elt (0)};
	  emit_vlmax_insn (code_for_pred_broadcast (builder.mode ()), UNARY_OP,
			    dup_ops);
	  for (unsigned int i = 1; i < builder.npatterns (); i++)
	    {
	      /* Generate mask according to i.  */
	      rtx mask = gen_reg_rtx (builder.mask_mode ());
	      rtx const_vec = gen_const_vector_dup (builder.int_mode (), i);
	      expand_vec_cmp (mask, EQ, vid_repeat, const_vec);

	      /* Merge scalar to each i.  */
	      rtx tmp2 = gen_reg_rtx (builder.mode ());
	      rtx merge_ops[] = {tmp2, tmp1, builder.elt (i), mask};
	      insn_code icode = code_for_pred_merge_scalar (builder.mode ());
	      emit_vlmax_insn (icode, MERGE_OP, merge_ops);
	      tmp1 = tmp2;
	    }
	  emit_move_insn (result, tmp1);
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
	  emit_vlmax_insn (icode, NULLARY_OP, vid_ops);

	  if (builder.npatterns_all_equal_p ())
	    {
	      /* Generate the variable-length vector following this rule:
		 { a, a, a + step, a + step, a + step * 2, a + step * 2, ...}
		   E.g. { 0, 0, 8, 8, 16, 16, ... } */

	      /* We want to create a pattern where value[idx] = floor (idx /
		 NPATTERNS). As NPATTERNS is always a power of two we can
		 rewrite this as = idx & -NPATTERNS.  */
	      /* Step 2: VID AND -NPATTERNS:
		 { 0&-4, 1&-4, 2&-4, 3 &-4, 4 &-4, 5 &-4, 6 &-4, 7 &-4, ... }
	      */
	      rtx imm
		= gen_int_mode (-builder.npatterns (), builder.inner_mode ());
	      rtx tmp1 = gen_reg_rtx (builder.mode ());
	      rtx and_ops[] = {tmp1, vid, imm};
	      icode = code_for_pred_scalar (AND, builder.mode ());
	      emit_vlmax_insn (icode, BINARY_OP, and_ops);

	      /* Step 3: Convert to step size 1.  */
	      rtx tmp2 = gen_reg_rtx (builder.mode ());
	      /* log2 (npatterns) to get the shift amount to convert
		 Eg.  { 0, 0, 0, 0, 4, 4, ... }
		 into { 0, 0, 0, 0, 1, 1, ... }.  */
	      HOST_WIDE_INT shift_amt = exact_log2 (builder.npatterns ()) ;
	      rtx shift = gen_int_mode (shift_amt, builder.inner_mode ());
	      rtx shift_ops[] = {tmp2, tmp1, shift};
	      icode = code_for_pred_scalar (ASHIFTRT, builder.mode ());
	      emit_vlmax_insn (icode, BINARY_OP, shift_ops);

	      /* Step 4: Multiply to step size n.  */
	      HOST_WIDE_INT step_size =
		INTVAL (builder.elt (builder.npatterns ()))
		- INTVAL (builder.elt (0));
	      rtx tmp3 = gen_reg_rtx (builder.mode ());
	      if (pow2p_hwi (step_size))
		{
		  /* Power of 2 can be handled with a left shift.  */
		  HOST_WIDE_INT shift = exact_log2 (step_size);
		  rtx shift_amount = gen_int_mode (shift, Pmode);
		  insn_code icode = code_for_pred_scalar (ASHIFT, mode);
		  rtx ops[] = {tmp3, tmp2, shift_amount};
		  emit_vlmax_insn (icode, BINARY_OP, ops);
		}
	      else
		{
		  rtx mult_amt = gen_int_mode (step_size, builder.inner_mode ());
		  insn_code icode = code_for_pred_scalar (MULT, builder.mode ());
		  rtx ops[] = {tmp3, tmp2, mult_amt};
		  emit_vlmax_insn (icode, BINARY_OP, ops);
		}

	      /* Step 5: Add starting value to all elements.  */
	      HOST_WIDE_INT init_val = INTVAL (builder.elt (0));
	      if (init_val == 0)
		emit_move_insn (result, tmp3);
	      else
		{
		  rtx dup = gen_const_vector_dup (builder.mode (), init_val);
		  rtx add_ops[] = {result, tmp3, dup};
		  icode = code_for_pred (PLUS, builder.mode ());
		  emit_vlmax_insn (icode, BINARY_OP, add_ops);
		}
	    }
	  else
	    {
	      /* Generate the variable-length vector following this rule:
		{ a, b, a + step, b + step, a + step*2, b + step*2, ... }  */

	      if (builder.npatterns_vid_diff_repeated_p ())
		{
		  /* Case 1: For example as below:
		     {3, 2, 1, 0, 7, 6, 5, 4, 11, 10, 9, 8... }
		     We have 3 - 0 = 3 equals 7 - 4 = 3, the sequence is
		     repeated as below after minus vid.
		     {3, 1, -1, -3, 3, 1, -1, -3...}
		     Then we can simplify the diff code gen to at most
		     npatterns().  */
		  rvv_builder v (builder.mode (), builder.npatterns (), 1);

		  /* Step 1: Generate diff = TARGET - VID.  */
		  for (unsigned int i = 0; i < v.npatterns (); ++i)
		    {
		     poly_int64 diff = rtx_to_poly_int64 (builder.elt (i)) - i;
		     v.quick_push (gen_int_mode (diff, v.inner_mode ()));
		    }

		  /* Step 2: Generate result = VID + diff.  */
		  rtx vec = v.build ();
		  rtx add_ops[] = {result, vid, vec};
		  emit_vlmax_insn (code_for_pred (PLUS, builder.mode ()),
				   BINARY_OP, add_ops);
		}
	      else
		{
		  /* Case 2: For example as below:
		     { -4, 4, -4 + 1, 4 + 1, -4 + 2, 4 + 2, -4 + 3, 4 + 3, ... }
		   */
		  rvv_builder v (builder.mode (), builder.npatterns (), 1);

		  /* Step 1: Generate { a, b, a, b, ... }  */
		  for (unsigned int i = 0; i < v.npatterns (); ++i)
		    v.quick_push (builder.elt (i));
		  rtx new_base = v.build ();

		  /* Step 2: Generate tmp1 = VID >> LOG2 (NPATTERNS).  */
		  rtx shift_count
		    = gen_int_mode (exact_log2 (builder.npatterns ()),
				    builder.inner_mode ());
		  rtx tmp1 = gen_reg_rtx (builder.mode ());
		  rtx shift_ops[] = {tmp1, vid, shift_count};
		  emit_vlmax_insn (code_for_pred_scalar
				   (LSHIFTRT, builder.mode ()), BINARY_OP,
				   shift_ops);

		  /* Step 3: Generate tmp2 = tmp1 * step.  */
		  rtx tmp2 = gen_reg_rtx (builder.mode ());
		  rtx step
		    = simplify_binary_operation (MINUS, builder.inner_mode (),
						 builder.elt (v.npatterns()),
						 builder.elt (0));
		  expand_vec_series (tmp2, const0_rtx, step, tmp1);

		  /* Step 4: Generate result = tmp2 + new_base.  */
		  rtx add_ops[] = {result, tmp2, new_base};
		  emit_vlmax_insn (code_for_pred (PLUS, builder.mode ()),
				   BINARY_OP, add_ops);
		}
	    }
	}
      else if (builder.interleaved_stepped_npatterns_p ())
	{
	  rtx base1 = builder.elt (0);
	  rtx base2 = builder.elt (1);
	  poly_int64 step1
	    = rtx_to_poly_int64 (builder.elt (builder.npatterns ()))
	      - rtx_to_poly_int64 (base1);
	  poly_int64 step2
	    = rtx_to_poly_int64 (builder.elt (builder.npatterns () + 1))
	      - rtx_to_poly_int64 (base2);

	  /* For { 1, 0, 2, 0, ... , n - 1, 0 }, we can use larger EEW
	     integer vector mode to generate such vector efficiently.

	     E.g. EEW = 16, { 2, 0, 4, 0, ... }

	     can be interpreted into:

		  EEW = 32, { 2, 4, ... }  */
	  unsigned int new_smode_bitsize = builder.inner_bits_size () * 2;
	  scalar_int_mode new_smode;
	  machine_mode new_mode;
	  poly_uint64 new_nunits
	    = exact_div (GET_MODE_NUNITS (builder.mode ()), 2);
	  if (int_mode_for_size (new_smode_bitsize, 0).exists (&new_smode)
	      && get_vector_mode (new_smode, new_nunits).exists (&new_mode))
	    {
	      rtx tmp1 = gen_reg_rtx (new_mode);
	      base1 = gen_int_mode (rtx_to_poly_int64 (base1), new_smode);
	      expand_vec_series (tmp1, base1, gen_int_mode (step1, new_smode));

	      if (rtx_equal_p (base2, const0_rtx) && known_eq (step2, 0))
		/* { 1, 0, 2, 0, ... }.  */
		emit_move_insn (result, gen_lowpart (mode, tmp1));
	      else if (known_eq (step2, 0))
		{
		  /* { 1, 1, 2, 1, ... }.  */
		  rtx scalar = expand_simple_binop (
		    new_smode, ASHIFT,
		    gen_int_mode (rtx_to_poly_int64 (base2), new_smode),
		    gen_int_mode (builder.inner_bits_size (), new_smode),
		    NULL_RTX, false, OPTAB_DIRECT);
		  rtx tmp2 = gen_reg_rtx (new_mode);
		  rtx ior_ops[] = {tmp2, tmp1, scalar};
		  emit_vlmax_insn (code_for_pred_scalar (IOR, new_mode),
				   BINARY_OP, ior_ops);
		  emit_move_insn (result, gen_lowpart (mode, tmp2));
		}
	      else
		{
		  /* { 1, 3, 2, 6, ... }.  */
		  rtx tmp2 = gen_reg_rtx (new_mode);
		  base2 = gen_int_mode (rtx_to_poly_int64 (base2), new_smode);
		  expand_vec_series (tmp2, base2,
				     gen_int_mode (step2, new_smode));
		  rtx shifted_tmp2 = expand_simple_binop (
		    new_mode, ASHIFT, tmp2,
		    gen_int_mode (builder.inner_bits_size (), Pmode), NULL_RTX,
		    false, OPTAB_DIRECT);
		  rtx tmp3 = gen_reg_rtx (new_mode);
		  rtx ior_ops[] = {tmp3, tmp1, shifted_tmp2};
		  emit_vlmax_insn (code_for_pred (IOR, new_mode), BINARY_OP,
				   ior_ops);
		  emit_move_insn (result, gen_lowpart (mode, tmp3));
		}
	    }
	  else
	    {
	      rtx vid = gen_reg_rtx (mode);
	      expand_vec_series (vid, const0_rtx, const1_rtx);
	      /* Transform into { 0, 0, 1, 1, 2, 2, ... }.  */
	      rtx shifted_vid
		= expand_simple_binop (mode, LSHIFTRT, vid, const1_rtx,
				       NULL_RTX, false, OPTAB_DIRECT);
	      rtx tmp1 = gen_reg_rtx (mode);
	      rtx tmp2 = gen_reg_rtx (mode);
	      expand_vec_series (tmp1, base1,
				 gen_int_mode (step1, builder.inner_mode ()),
				 shifted_vid);
	      expand_vec_series (tmp2, base2,
				 gen_int_mode (step2, builder.inner_mode ()),
				 shifted_vid);

	      /* Transform into { 0, 1, 0, 1, 0, 1, ... }.  */
	      rtx and_vid = gen_reg_rtx (mode);
	      rtx and_ops[] = {and_vid, vid, const1_rtx};
	      emit_vlmax_insn (code_for_pred_scalar (AND, mode), BINARY_OP,
			       and_ops);
	      rtx mask = gen_reg_rtx (builder.mask_mode ());
	      expand_vec_cmp (mask, EQ, and_vid, CONST1_RTX (mode));

	      rtx ops[] = {result, tmp1, tmp2, mask};
	      emit_vlmax_insn (code_for_pred_merge (mode), MERGE_OP, ops);
	    }
	}
      else
	/* TODO: We will enable more variable-length vector in the future.  */
	gcc_unreachable ();
    }
  else
    gcc_unreachable ();

  if (result != target)
    emit_move_insn (target, result);
}

/* Get the frm mode with given CONST_INT rtx, the default mode is
   FRM_DYN.  */
enum floating_point_rounding_mode
get_frm_mode (rtx operand)
{
  gcc_assert (CONST_INT_P (operand));

  switch (INTVAL (operand))
    {
    case FRM_RNE:
      return FRM_RNE;
    case FRM_RTZ:
      return FRM_RTZ;
    case FRM_RDN:
      return FRM_RDN;
    case FRM_RUP:
      return FRM_RUP;
    case FRM_RMM:
      return FRM_RMM;
    case FRM_DYN:
      return FRM_DYN;
    default:
      gcc_unreachable ();
    }

  gcc_unreachable ();
}

/* Expand a pre-RA RVV data move from SRC to DEST.
   It expands move for RVV fractional vector modes.
   Return true if the move as already been emitted.  */
bool
legitimize_move (rtx dest, rtx *srcp)
{
  rtx src = *srcp;
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
	  /* For NUNITS <= 31 VLS modes, we don't need extract
	     scalar registers so we apply the naive (set (op0) (op1)) pattern. */
	  if (can_create_pseudo_p ())
	    {
	      /* Need to force register if mem <- !reg.  */
	      if (MEM_P (dest) && !REG_P (src))
		*srcp = force_reg (mode, src);

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
	    *srcp = force_reg (mode, src);

	  return false;
	}
    }

  if (register_operand (src, mode) && register_operand (dest, mode))
    {
      emit_insn (gen_rtx_SET (dest, src));
      return true;
    }

  unsigned insn_flags
    = GET_MODE_CLASS (mode) == MODE_VECTOR_BOOL ? UNARY_MASK_OP : UNARY_OP;
  if (!register_operand (src, mode) && !register_operand (dest, mode))
    {
      rtx tmp = gen_reg_rtx (mode);
      if (MEM_P (src))
	{
	  rtx ops[] = {tmp, src};
	  emit_vlmax_insn (code_for_pred_mov (mode), insn_flags, ops);
	}
      else
	emit_move_insn (tmp, src);
      src = tmp;
    }

  if (satisfies_constraint_vu (src))
    return false;

  rtx ops[] = {dest, src};
  emit_vlmax_insn (code_for_pred_mov (mode), insn_flags, ops);
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

/* Return the VLMAX rtx of vector mode MODE.  */
rtx
get_vlmax_rtx (machine_mode mode)
{
  gcc_assert (riscv_v_ext_vector_mode_p (mode));
  return gen_int_mode (GET_MODE_NUNITS (mode), Pmode);
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
  poly_int64 nunits = GET_MODE_NUNITS (mode);
  if (riscv_v_ext_tuple_mode_p (mode))
    {
      unsigned int nf = get_nf (mode);
      nunits = exact_div (nunits, nf);
    }
  return get_vector_mode (BImode, nunits).require ();
}

/* Return the appropriate LMUL mode for MODE.  */

opt_machine_mode
get_lmul_mode (scalar_mode mode, int lmul)
{
  poly_uint64 lmul_nunits;
  unsigned int bytes = GET_MODE_SIZE (mode);
  if (multiple_p (BYTES_PER_RISCV_VECTOR * lmul, bytes, &lmul_nunits))
    return get_vector_mode (mode, lmul_nunits);
  return E_VOIDmode;
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
		     void (*emit_vector_func) (rtx *, rtx), enum avl_type type)
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
    {
      if (maybe_gt (GET_MODE_SIZE (scalar_mode), GET_MODE_SIZE (Pmode)))
	*scalar_op = force_const_mem (scalar_mode, *scalar_op);
      else
	*scalar_op = force_reg (scalar_mode, *scalar_op);
    }

  rtx tmp = gen_reg_rtx (vector_mode);
  rtx ops[] = {tmp, *scalar_op};
  if (type == VLMAX)
    emit_vlmax_insn (code_for_pred_broadcast (vector_mode), UNARY_OP, ops);
  else
    emit_nonvlmax_insn (code_for_pred_broadcast (vector_mode), UNARY_OP, ops,
			vl);
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

rtx
gen_no_side_effects_vsetvl_rtx (machine_mode vmode, rtx vl, rtx avl)
{
  unsigned int sew = get_sew (vmode);
  rtx tail_policy = gen_int_mode (get_prefer_tail_policy (), Pmode);
  rtx mask_policy = gen_int_mode (get_prefer_mask_policy (), Pmode);
  return gen_vsetvl_no_side_effects (Pmode, vl, avl, gen_int_mode (sew, Pmode),
				     gen_int_mode (get_vlmul (vmode), Pmode),
				     tail_policy, mask_policy);
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

  if (!rtx_equal_p (ops[1], CONSTM1_RTX (GET_MODE (ops[1])))
      && !rtx_equal_p (ops[2], RVV_VUNDEF (GET_MODE (ops[2]))))
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

      /* Non-fractional LMUL has whole register moves that don't require a
	 vsetvl for VLMAX.  */
      if (fractional_p)
	emit_vlmax_vsetvl (subpart_mode, ops[4]);
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
		  emit_vlmax_insn_lra (code_for_pred_mov (subpart_mode),
					UNARY_OP, operands, ops[4]);
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
		  emit_vlmax_insn_lra (code_for_pred_mov (subpart_mode),
					UNARY_OP, operands, ops[4]);
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
  if (autovec_use_vlmax_p ())
    {
      /* We use LMUL = 1 as base bytesize which is BYTES_PER_RISCV_VECTOR and
	 rvv_max_lmul as multiply factor to calculate the NUNITS to
	 get the auto-vectorization mode.  */
      poly_uint64 nunits;
      poly_uint64 vector_size = BYTES_PER_RISCV_VECTOR * TARGET_MAX_LMUL;
      poly_uint64 scalar_size = GET_MODE_SIZE (mode);
      /* Disable vectorization when we can't find a RVV mode for it.
	 E.g. -march=rv64gc_zve32x doesn't have a vector mode to vectorize
	 a double (DFmode) type.  */
      if (!multiple_p (vector_size, scalar_size, &nunits))
	return word_mode;
      machine_mode rvv_mode;
      if (get_vector_mode (mode, nunits).exists (&rvv_mode))
	return rvv_mode;
    }
  return word_mode;
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
  /* We can't use BIT mode (BI) directly to generate mask = 0b01010...
     since we don't have such instruction in RVV.
     Instead, we should use INT mode (QI/HI/SI/DI) with integer move
     instruction to generate the mask data we want.  */
  machine_mode mask_bit_mode = get_mask_mode (builder.mode ());
  machine_mode mask_int_mode
    = get_repeating_sequence_dup_machine_mode (builder, mask_bit_mode);
  uint64_t full_nelts = builder.full_nelts ().to_constant ();

  /* Step 1: Broadcast the first pattern.  */
  rtx ops[] = {target, force_reg (builder.inner_mode (), builder.elt (0))};
  emit_vlmax_insn (code_for_pred_broadcast (builder.mode ()),
		    UNARY_OP, ops);
  /* Step 2: Merge the rest iteration of pattern.  */
  for (unsigned int i = 1; i < builder.npatterns (); i++)
    {
      /* Step 2-1: Generate mask register v0 for each merge.  */
      rtx merge_mask
	= builder.get_merge_scalar_mask (i, GET_MODE_INNER (mask_int_mode));
      rtx mask = gen_reg_rtx (mask_bit_mode);
      rtx dup = gen_reg_rtx (mask_int_mode);

      if (full_nelts <= builder.inner_bits_size ()) /* vmv.s.x.  */
	{
	  rtx ops[] = {dup, merge_mask};
	  emit_nonvlmax_insn (code_for_pred_broadcast (GET_MODE (dup)),
			       SCALAR_MOVE_OP, ops, CONST1_RTX (Pmode));
	}
      else /* vmv.v.x.  */
	{
	  rtx ops[] = {dup,
		       force_reg (GET_MODE_INNER (mask_int_mode), merge_mask)};
	  rtx vl = gen_int_mode (CEIL (full_nelts, builder.inner_bits_size ()),
				 Pmode);
	  emit_nonvlmax_insn (code_for_pred_broadcast (mask_int_mode), UNARY_OP,
			       ops, vl);
	}

      emit_move_insn (mask, gen_lowpart (mask_bit_mode, dup));

      /* Step 2-2: Merge pattern according to the mask.  */
      rtx ops[] = {target, target, builder.elt (i), mask};
      emit_vlmax_insn (code_for_pred_merge_scalar (GET_MODE (target)),
			MERGE_OP, ops);
    }
}

/* Use slideup approach to combine the vectors.
     v = {a, a, a, a, b, b, b, b}

   First:
     v1 = {a, a, a, a, a, a, a, a}
     v2 = {b, b, b, b, b, b, b, b}
     v = slideup (v1, v2, nelt / 2)
*/
static void
expand_vector_init_slideup_combine_sequence (rtx target,
					     const rvv_builder &builder)
{
  machine_mode mode = GET_MODE (target);
  int nelts = builder.full_nelts ().to_constant ();
  rtx first_elt = builder.elt (0);
  rtx last_elt = builder.elt (nelts - 1);
  rtx low = expand_vector_broadcast (mode, first_elt);
  rtx high = expand_vector_broadcast (mode, last_elt);
  insn_code icode = code_for_pred_slide (UNSPEC_VSLIDEUP, mode);
  rtx ops[] = {target, low, high, gen_int_mode (nelts / 2, Pmode)};
  emit_vlmax_insn (icode, SLIDEUP_OP_MERGE, ops);
}

/* Use merge approach to merge a scalar into a vector.
     v = {a, a, a, a, a, a, b, b}

     v1 = {a, a, a, a, a, a, a, a}
     scalar = b
     mask = {0, 0, 0, 0, 0, 0, 1, 1}
*/
static void
expand_vector_init_merge_combine_sequence (rtx target,
					   const rvv_builder &builder)
{
  machine_mode mode = GET_MODE (target);
  machine_mode imode = builder.int_mode ();
  machine_mode mmode = builder.mask_mode ();
  int nelts = builder.full_nelts ().to_constant ();
  int leading_ndups = builder.count_dups (0, nelts - 1, 1);
  if ((leading_ndups > 255 && GET_MODE_INNER (imode) == QImode)
      || riscv_get_v_regno_alignment (imode) > 1)
    imode = get_vector_mode (HImode, nelts).require ();

  /* Generate vid = { 0, 1, 2, ..., n }.  */
  rtx vid = gen_reg_rtx (imode);
  expand_vec_series (vid, const0_rtx, const1_rtx);

  /* Generate mask.  */
  rtx mask = gen_reg_rtx (mmode);
  insn_code icode = code_for_pred_cmp_scalar (imode);
  rtx index = gen_int_mode (leading_ndups - 1, builder.inner_int_mode ());
  rtx dup_rtx = gen_rtx_VEC_DUPLICATE (imode, index);
  /* vmsgtu.vi/vmsgtu.vx.  */
  rtx cmp = gen_rtx_fmt_ee (GTU, mmode, vid, dup_rtx);
  rtx sel = builder.elt (nelts - 1);
  rtx mask_ops[] = {mask, cmp, vid, index};
  emit_vlmax_insn (icode, COMPARE_OP, mask_ops);

  /* Duplicate the first elements.  */
  rtx dup = expand_vector_broadcast (mode, builder.elt (0));
  /* Merge scalar into vector according to mask.  */
  rtx merge_ops[] = {target, dup, sel, mask};
  icode = code_for_pred_merge_scalar (mode);
  emit_vlmax_insn (icode, MERGE_OP, merge_ops);
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

  /* If the sequence is v = { a, a, a, a } just broadcast an element.  */
  if (v.is_repeating_sequence ())
    {
      machine_mode mode = GET_MODE (target);
      rtx dup = expand_vector_broadcast (mode, v.elt (0));
      emit_move_insn (target, dup);
      return;
    }

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

      /* Case 3: Optimize combine sequence.
	 E.g. v = {a, a, a, a, a, a, a, a, b, b, b, b, b, b, b, b}.
	 We can combine:
	   v1 = {a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a}.
	 and
	   v2 = {b, b, b, b, b, b, b, b, b, b, b, b, b, b, b, b}.
	 by slideup.  */
      if (v.combine_sequence_use_slideup_profitable_p ())
	{
	  expand_vector_init_slideup_combine_sequence (target, v);
	  return;
	}

      /* Case 4: Optimize combine sequence.
	 E.g. v = {a, a, a, a, a, a, a, a, a, a, a, b, b, b, b, b}.

	 Generate vector:
	   v = {a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a}.

	 Generate mask:
	   mask = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1}.

	 Merge b into v by mask:
	   v = {a, a, a, a, a, a, a, a, a, a, a, b, b, b, b, b}.  */
      if (v.combine_sequence_use_merge_profitable_p ())
	{
	  expand_vector_init_merge_combine_sequence (target, v);
	  return;
	}
    }

  /* Optimize trailing same elements sequence:
      v = {y, y2, y3, y4, y5, x, x, x, x, x, x, x, x, x, x, x};  */
  if (!expand_vector_init_trailing_same_elem (target, v, nelts))
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
      poly_uint64 full_size = BYTES_PER_RISCV_VECTOR * TARGET_MAX_LMUL;

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
      static const int rvv_factors[] = {1, 2, 4, 8, 16, 32, 64};
      for (unsigned int i = 0; i < sizeof (rvv_factors) / sizeof (int); i++)
	{
	  poly_uint64 units;
	  machine_mode mode;
	  if (can_div_trunc_p (full_size, rvv_factors[i], &units)
	      && get_vector_mode (QImode, units).exists (&mode))
	    modes->safe_push (mode);
	}
    }
    /* Push all VLSmodes according to TARGET_MIN_VLEN.  */
    unsigned int i = 0;
    unsigned int base_size = TARGET_MIN_VLEN * TARGET_MAX_LMUL / 8;
    unsigned int size = base_size;
    machine_mode mode;
    while (size > 0 && get_vector_mode (QImode, size).exists (&mode))
     {
	if (vls_mode_valid_p (mode))
	  modes->safe_push (mode);

	i++;
	size = base_size / (1U << i);
     }
  /* Enable LOOP_VINFO comparison in COST model.  */
  return VECT_COMPARE_COSTS;
}

/* Return true if we can find the related MODE according to default LMUL. */
static bool
can_find_related_mode_p (machine_mode vector_mode, scalar_mode element_mode,
			 poly_uint64 *nunits)
{
  if (!autovec_use_vlmax_p ())
    return false;
  if (riscv_v_ext_vector_mode_p (vector_mode)
      && multiple_p (BYTES_PER_RISCV_VECTOR * TARGET_MAX_LMUL,
		     GET_MODE_SIZE (element_mode), nunits))
    return true;
  if (riscv_v_ext_vls_mode_p (vector_mode)
      && multiple_p (TARGET_MIN_VLEN * TARGET_MAX_LMUL,
		     GET_MODE_SIZE (element_mode), nunits))
    return true;
  return false;
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
  if (can_find_related_mode_p (vector_mode, element_mode, &min_units))
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
expand_vec_cmp (rtx target, rtx_code code, rtx op0, rtx op1, rtx mask,
		rtx maskoff)
{
  machine_mode mask_mode = GET_MODE (target);
  machine_mode data_mode = GET_MODE (op0);
  insn_code icode = get_cmp_insn_code (code, data_mode);

  if (code == LTGT)
    {
      rtx lt = gen_reg_rtx (mask_mode);
      rtx gt = gen_reg_rtx (mask_mode);
      expand_vec_cmp (lt, LT, op0, op1, mask, maskoff);
      expand_vec_cmp (gt, GT, op0, op1, mask, maskoff);
      icode = code_for_pred (IOR, mask_mode);
      rtx ops[] = {target, lt, gt};
      emit_vlmax_insn (icode, BINARY_MASK_OP, ops);
      return;
    }

  rtx cmp = gen_rtx_fmt_ee (code, mask_mode, op0, op1);
  if (!mask && !maskoff)
    {
      rtx ops[] = {target, cmp, op0, op1};
      emit_vlmax_insn (icode, COMPARE_OP, ops);
    }
  else
    {
      rtx ops[] = {target, mask, maskoff, cmp, op0, op1};
      emit_vlmax_insn (icode, COMPARE_OP_MU, ops);
    }
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
	  emit_vlmax_insn (icode, BINARY_MASK_OP, ops);
	}
      else
	{
	  /* vmfeq.vv    v0, vb, vb, v0.t  */
	  expand_vec_cmp (eq0, EQ, op1, op1, eq0, eq0);
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
    expand_vec_cmp (eq0, code, op0, op1, eq0, eq0);

  if (can_invert_p)
    {
      emit_move_insn (target, eq0);
      return true;
    }

  /* We use one_cmpl<mode>2 to make Combine PASS to combine mask instructions
     into: vmand.mm/vmnor.mm/vmnand.mm/vmxnor.mm.  */
  emit_insn (gen_rtx_SET (target, gen_rtx_NOT (mask_mode, eq0)));
  return false;
}

/* Modulo all SEL indices to ensure they are all in range if [0, MAX_SEL].
   MAX_SEL is nunits - 1 if rtx_equal_p (op0, op1). Otherwise, it is
   2 * nunits - 1.  */
static rtx
modulo_sel_indices (rtx op0, rtx op1, rtx sel)
{
  rtx sel_mod;
  machine_mode sel_mode = GET_MODE (sel);
  poly_uint64 nunits = GET_MODE_NUNITS (sel_mode);
  poly_uint64 max_sel = rtx_equal_p (op0, op1) ? nunits - 1 : 2 * nunits - 1;
  /* If SEL is variable-length CONST_VECTOR, we don't need to modulo it.
     Or if SEL is constant-length within [0, MAX_SEL], no need to modulo the
     indice.  */
  if (CONST_VECTOR_P (sel)
      && (!nunits.is_constant () || const_vec_all_in_range_p (sel, 0, max_sel)))
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
  rtx sel_mod = modulo_sel_indices (op0, op1, sel);

  /* Check if the two values vectors are the same.  */
  if (rtx_equal_p (op0, op1))
    {
      emit_vlmax_gather_insn (target, op0, sel_mod);
      return;
    }

  /* This following sequence is handling the case that:
     __builtin_shufflevector (vec1, vec2, index...), the index can be any
     value in range of [0, 2 * nunits - 1].  */
  machine_mode mask_mode;
  mask_mode = get_mask_mode (data_mode);
  rtx mask = gen_reg_rtx (mask_mode);
  rtx max_sel = gen_const_vector_dup (sel_mode, nunits);

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
  emit_vlmax_insn (code_for_pred (MINUS, sel_mode), BINARY_OP, ops);

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

/* Return the appropriate index mode for gather instructions.  */
opt_machine_mode
get_gather_index_mode (struct expand_vec_perm_d *d)
{
  machine_mode sel_mode = related_int_vector_mode (d->vmode).require ();
  poly_uint64 nunits = GET_MODE_NUNITS (d->vmode);

  if (GET_MODE_INNER (d->vmode) == QImode)
    {
      if (nunits.is_constant ())
	{
	  /* If indice is LMUL8 CONST_VECTOR and any element value
	     exceed the range of 0 ~ 255, Forbid such permutation
	     since we need vector HI mode to hold such indice and
	     we don't have it.  */
	  if (!d->perm.all_in_range_p (0, 255)
	      && !get_vector_mode (HImode, nunits).exists (&sel_mode))
	    return opt_machine_mode ();
	}
      else
	{
	  /* Permuting two SEW8 variable-length vectors need vrgatherei16.vv.
	     Otherwise, it could overflow the index range.  */
	  if (!get_vector_mode (HImode, nunits).exists (&sel_mode))
	    return opt_machine_mode ();
	}
    }
  else if (riscv_get_v_regno_alignment (sel_mode) > 1
	   && GET_MODE_INNER (sel_mode) != HImode)
    sel_mode = get_vector_mode (HImode, nunits).require ();
  return sel_mode;
}

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

  /* We need to use precomputed mask for such situation and such mask
     can only be computed in compile-time known size modes.  */
  bool indices_fit_selector_p
    = GET_MODE_BITSIZE (GET_MODE_INNER (vmode)) > 8 || known_lt (vec_len, 256);
  if (!indices_fit_selector_p && !vec_len.is_constant ())
    return false;

  if (d->testing_p)
    return true;

  machine_mode mask_mode = get_mask_mode (vmode);
  rtx mask = gen_reg_rtx (mask_mode);

  if (indices_fit_selector_p && vec_len.is_constant ())
    {
      /* For a constant vector length we can generate the needed mask at
	 compile time and load it as mask at runtime.
	 This saves a compare at runtime.  */
      rtx_vector_builder sel (mask_mode, d->perm.encoding ().npatterns (),
			      d->perm.encoding ().nelts_per_pattern ());
      unsigned int encoded_nelts = sel.encoded_nelts ();
      for (unsigned int i = 0; i < encoded_nelts; i++)
	sel.quick_push (gen_int_mode (d->perm[i].to_constant ()
				      < vec_len.to_constant (),
				      GET_MODE_INNER (mask_mode)));
      mask = sel.build ();
    }
  else if (indices_fit_selector_p)
    {
      /* For a dynamic vector length < 256 we keep the permutation
	 indices in the literal pool, load it at runtime and create the
	 mask by selecting either OP0 or OP1 by

	    INDICES < NUNITS ? 1 : 0.  */
      rtx sel = vec_perm_indices_to_rtx (sel_mode, d->perm);
      rtx x = gen_int_mode (vec_len, GET_MODE_INNER (sel_mode));
      insn_code icode = code_for_pred_cmp_scalar (sel_mode);
      rtx cmp = gen_rtx_fmt_ee (LTU, mask_mode, sel, x);
      rtx ops[] = {mask, cmp, sel, x};
      emit_vlmax_insn (icode, COMPARE_OP, ops);
    }
  else
    {
      /* For EEW8 and NUNITS may be larger than 255, we can't use vmsltu
	 directly to generate the selector mask, instead, we can only use
	 precomputed mask.

	 E.g. selector = <0, 257, 2, 259> for EEW8 vector with NUNITS = 256, we
	 don't have a QImode scalar register to hold larger than 255.
	 We also cannot hold that in a vector QImode register if LMUL = 8, and,
	 since there is no larger HI mode vector we cannot create a larger
	 selector.

	 As the mask is a simple {0, 1, ...} pattern and the length is known we
	 can store it in a scalar register and broadcast it to a mask register.
       */
      gcc_assert (vec_len.is_constant ());
      int size = CEIL (GET_MODE_NUNITS (mask_mode).to_constant (), 8);
      machine_mode mode = get_vector_mode (QImode, size).require ();
      rtx tmp = gen_reg_rtx (mode);
      rvv_builder v (mode, 1, size);
      for (int i = 0; i < vec_len.to_constant () / 8; i++)
	{
	  uint8_t value = 0;
	  for (int j = 0; j < 8; j++)
	    {
	      int index = i * 8 + j;
	      if (known_lt (d->perm[index], 256))
		value |= 1 << j;
	    }
	  v.quick_push (gen_int_mode (value, QImode));
	}
      emit_move_insn (tmp, v.build ());
      emit_move_insn (mask, gen_lowpart (mask_mode, tmp));
    }

  /* TARGET = MASK ? OP0 : OP1.  */
  /* swap op0 and op1 since the order is opposite to pred_merge.  */
  rtx ops2[] = {d->target, d->op1, d->op0, mask};
  emit_vlmax_insn (code_for_pred_merge (vmode), MERGE_OP, ops2);
  return true;
}

/* Recognize the consecutive index that we can use a single
   vrgather.v[x|i] to shuffle the vectors.

   e.g. short[8] = VEC_PERM_EXPR <a, a, {0,1,0,1,0,1,0,1}>
   Use SEW = 32, index = 1 vrgather.vi to get the result.  */
static bool
shuffle_consecutive_patterns (struct expand_vec_perm_d *d)
{
  machine_mode vmode = d->vmode;
  scalar_mode smode = GET_MODE_INNER (vmode);
  poly_int64 vec_len = d->perm.length ();
  HOST_WIDE_INT elt;

  if (!vec_len.is_constant () || !d->perm[0].is_constant (&elt))
    return false;
  int vlen = vec_len.to_constant ();

  /* Compute the last element index of consecutive pattern from the leading
     consecutive elements.  */
  int last_consecutive_idx = -1;
  int consecutive_num = -1;
  for (int i = 1; i < vlen; i++)
    {
      if (maybe_ne (d->perm[i], d->perm[i - 1] + 1))
	break;
      last_consecutive_idx = i;
      consecutive_num = last_consecutive_idx + 1;
    }

  int new_vlen = vlen / consecutive_num;
  if (last_consecutive_idx < 0 || consecutive_num == vlen
      || !pow2p_hwi (consecutive_num) || !pow2p_hwi (new_vlen))
    return false;
  /* VEC_PERM <..., (index, index + 1, ... index + consecutive_num - 1)>.
     All elements of index, index + 1, ... index + consecutive_num - 1 should
     locate at the same vector.  */
  if (maybe_ge (d->perm[0], vec_len)
      != maybe_ge (d->perm[last_consecutive_idx], vec_len))
    return false;
  /* If a vector has 8 elements.  We allow optimizations on consecutive
     patterns e.g. <0, 1, 2, 3, 0, 1, 2, 3> or <4, 5, 6, 7, 4, 5, 6, 7>.
     Other patterns like <2, 3, 4, 5, 2, 3, 4, 5> are not feasible patterns
     to be optimized.  */
  if (d->perm[0].to_constant () % consecutive_num != 0)
    return false;
  unsigned int container_bits = consecutive_num * GET_MODE_BITSIZE (smode);
  if (container_bits > 64)
    return false;
  else if (container_bits == 64)
    {
      if (!TARGET_VECTOR_ELEN_64)
	return false;
      else if (FLOAT_MODE_P (smode) && !TARGET_VECTOR_ELEN_FP_64)
	return false;
    }

  /* Check the rest of elements are the same consecutive pattern.  */
  for (int i = consecutive_num; i < vlen; i++)
    if (maybe_ne (d->perm[i], d->perm[i % consecutive_num]))
      return false;

  if (FLOAT_MODE_P (smode))
    smode = float_mode_for_size (container_bits).require ();
  else
    smode = int_mode_for_size (container_bits, 0).require ();
  if (!get_vector_mode (smode, new_vlen).exists (&vmode))
    return false;
  machine_mode sel_mode = related_int_vector_mode (vmode).require ();

  /* Success! */
  if (d->testing_p)
    return true;

  int index = elt / consecutive_num;
  if (index >= new_vlen)
    index = index - new_vlen;
  rtx sel = gen_const_vector_dup (sel_mode, index);
  rtx op = elt >= vlen ? d->op0 : d->op1;
  emit_vlmax_gather_insn (gen_lowpart (vmode, d->target),
			  gen_lowpart (vmode, op), sel);
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

  /* It's not worthwhile the compress pattern has elements < 4
     and we can't modulo indices for compress pattern.  */
  if (known_ge (d->perm[vlen - 1], vlen * 2) || vlen < 4)
    return false;

  /* Compress pattern doesn't work for one vector.  */
  if (d->one_vector_p)
    return false;

  /* Compress point is the point that all elements value with index i >=
     compress point of the selector are all consecutive series increasing and
     each selector value >= NUNITS. In this case, we could compress all elements
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

  /* We can only apply compress approach when all index values from 0 to
     compress point are increasing.  */
  for (int i = 1; i < compress_point; i++)
    if (maybe_le (d->perm[i], d->perm[i - 1]))
      return false;

  /* It must be series increasing from compress point.  */
  for (int i = 1 + compress_point; i < vlen; i++)
    if (maybe_ne (d->perm[i], d->perm[i - 1] + 1))
      return false;

  /* Success!  */
  if (d->testing_p)
    return true;

  /* Check whether we need to slideup op1 to apply compress approach.

       E.g. For index = { 0, 2, 6, 7}, since d->perm[i - 1] = 7 which
	    is 2 * NUNITS - 1, so we don't need to slide up.

	    For index = { 0, 2, 5, 6}, we need to slide op1 up before
	    we apply compress approach.  */
  bool need_slideup_p = maybe_ne (d->perm[vlen - 1], 2 * vec_len - 1)
			&& !const_vec_duplicate_p (d->op1);

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

  /* If we can use compress approach, the code sequence will be:
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
      merge = gen_reg_rtx (vmode);
      rtx ops[] = {merge, d->op1, gen_int_mode (slideup_cnt, Pmode)};
      insn_code icode = code_for_pred_slide (UNSPEC_VSLIDEUP, vmode);
      emit_vlmax_insn (icode, BINARY_OP, ops);
    }

  insn_code icode = code_for_pred_compress (vmode);
  rtx ops[] = {d->target, merge, d->op0, mask};
  emit_nonvlmax_insn (icode, COMPRESS_OP_MERGE, ops,
		      gen_int_mode (vlen, Pmode));
  return true;
}

/* Recognize patterns like [4 5 6 7 12 13 14 15] where either the lower
   or the higher parts of both vectors are combined into one.  */

static bool
shuffle_slide_patterns (struct expand_vec_perm_d *d)
{
  machine_mode vmode = d->vmode;
  poly_int64 vec_len = d->perm.length ();

  if (!vec_len.is_constant ())
    return false;

  int vlen = vec_len.to_constant ();
  if (vlen < 4)
    return false;

  if (d->one_vector_p)
    return false;

  /* For a slideup OP0 can stay, for a slidedown OP1 can.
     The former requires that the first element of the permutation
     is the first element of OP0, the latter that the last permutation
     element is the last element of OP1.  */
  bool slideup = false;
  bool slidedown = false;

  /* For a slideup the permutation must start at OP0's first element.  */
  if (known_eq (d->perm[0], 0))
    slideup = true;

  /* For a slidedown the permutation must end at OP1's last element.  */
  if (known_eq (d->perm[vlen - 1], 2 * vlen - 1))
    slidedown = true;

  if (slideup && slidedown)
    return false;

  if (!slideup && !slidedown)
    return false;

  /* Check for a monotonic sequence with one pivot.  */
  int pivot = -1;
  for (int i = 0; i < vlen; i++)
    {
      if (pivot == -1 && known_ge (d->perm[i], vec_len))
	pivot = i;
      if (i > 0 && i != pivot
	  && maybe_ne (d->perm[i], d->perm[i - 1] + 1))
	return false;
    }

  if (pivot == -1)
    return false;

  /* For a slideup OP1's part (to be slid up) must be a low part,
     i.e. starting with its first element.  */
  if (slideup && maybe_ne (d->perm[pivot], vlen))
      return false;

  /* For a slidedown OP0's part (to be slid down) must be a high part,
     i.e. ending with its last element.  */
  if (slidedown && maybe_ne (d->perm[pivot - 1], vlen - 1))
    return false;

  /* Success!  */
  if (d->testing_p)
    return true;

  /* PIVOT is the start of the lower/higher part of OP1 or OP2.
     For a slideup it indicates how many elements of OP1 to
     skip/slide over.  For a slidedown it indicates how long
     OP1's high part is, while VLEN - PIVOT is the amount to slide.  */
  int slide_cnt = slideup ? pivot : vlen - pivot;
  insn_code icode;
  if (slideup)
    {
      /* No need for a vector length because we slide up until the
	 end of OP1 anyway.  */
      rtx ops[] = {d->target, d->op0, d->op1, gen_int_mode (slide_cnt, Pmode)};
      icode = code_for_pred_slide (UNSPEC_VSLIDEUP, vmode);
      emit_vlmax_insn (icode, SLIDEUP_OP_MERGE, ops);
    }
  else
    {
      /* Here we need a length because we slide to the beginning of OP1
	 leaving the remaining elements undisturbed.  */
      int len = pivot;
      rtx ops[] = {d->target, d->op1, d->op0,
		   gen_int_mode (slide_cnt, Pmode)};
      icode = code_for_pred_slide (UNSPEC_VSLIDEDOWN, vmode);
      emit_nonvlmax_insn (icode, BINARY_OP_TUMA, ops,
			  gen_int_mode (len, Pmode));
    }

  return true;
}

/* Recognize interleaving patterns like [0 4 1 5].  */

static bool
shuffle_interleave_patterns (struct expand_vec_perm_d *d)
{
  machine_mode vmode = d->vmode;
  machine_mode sel_mode = related_int_vector_mode (vmode).require ();
  poly_int64 vec_len = d->perm.length ();
  int n_patterns = d->perm.encoding ().npatterns ();

  if (!vec_len.is_constant ())
    return false;

  if (n_patterns != 2)
    return false;

  unsigned vlen = vec_len.to_constant ();

  if (vlen < 4 || vlen > 64)
    return false;

  if (d->one_vector_p)
    return false;

  bool low = true;
  if (d->perm.series_p (0, 2, 0, 1)
      && d->perm.series_p (1, 2, vlen, 1))
    low = true;
  else if (d->perm.series_p (0, 2, vlen / 2, 1)
	   && d->perm.series_p (1, 2, vlen + vlen / 2, 1))
    low = false;
  else
    return false;

  vec_perm_builder sel (vlen, 2, 1);
  sel.safe_grow (vlen);
  int cnt = 0;
  for (unsigned i = 0; i < vlen; i += 2)
    {
      sel[i] = cnt;
      sel[i + 1] = cnt + vlen / 2;
      cnt++;
    }

  vec_perm_indices indices (sel, 2, vlen);

  if (vlen != indices.length ().to_constant ())
    return false;

  /* Success!  */
  if (d->testing_p)
    return true;

  int slide_cnt = vlen / 2;
  rtx tmp = gen_reg_rtx (vmode);

  if (low)
    {
      /* No need for a vector length because we slide up until the
	 end of OP1 anyway.  */
      rtx ops[] = {tmp, d->op0, d->op1, gen_int_mode (slide_cnt, Pmode)};
      insn_code icode = code_for_pred_slide (UNSPEC_VSLIDEUP, vmode);
      emit_vlmax_insn (icode, SLIDEUP_OP_MERGE, ops);
    }
  else
    {
      rtx ops[] = {tmp, d->op1, d->op0, gen_int_mode (slide_cnt, Pmode)};
      insn_code icode = code_for_pred_slide (UNSPEC_VSLIDEDOWN, vmode);
      emit_nonvlmax_insn (icode, BINARY_OP_TUMA, ops,
			  gen_int_mode (slide_cnt, Pmode));
    }

  rtx sel_rtx = vec_perm_indices_to_rtx (sel_mode, indices);
  emit_vlmax_gather_insn (gen_lowpart (vmode, d->target), tmp, sel_rtx);

  return true;
}


/* Recognize even/odd patterns like [0 2 4 6].  We use two compress
   and one slideup.  */

static bool
shuffle_even_odd_patterns (struct expand_vec_perm_d *d)
{
  machine_mode vmode = d->vmode;
  poly_int64 vec_len = d->perm.length ();
  int n_patterns = d->perm.encoding ().npatterns ();

  if (n_patterns != 1)
    return false;

  if (!vec_len.is_constant ())
    return false;

  int vlen = vec_len.to_constant ();
  if (vlen < 4 || vlen > 64)
    return false;

  if (d->one_vector_p)
    return false;

  bool even = true;
  if (!d->perm.series_p (0, 1, 0, 2))
    {
      even = false;
      if (!d->perm.series_p (0, 1, 1, 2))
	return false;
    }

  /* Success!  */
  if (d->testing_p)
    return true;

  machine_mode mask_mode = get_mask_mode (vmode);
  rvv_builder builder (mask_mode, vlen, 1);
  int bit = even ? 0 : 1;
  for (int i = 0; i < vlen; i++)
    {
      bit ^= 1;
      if (bit)
	builder.quick_push (CONST1_RTX (BImode));
      else
	builder.quick_push (CONST0_RTX (BImode));
    }
  rtx mask = force_reg (mask_mode, builder.build ());

  insn_code icode = code_for_pred_compress (vmode);
  rtx ops1[] = {d->target, d->op0, mask};
  emit_vlmax_insn (icode, COMPRESS_OP, ops1);

  rtx tmp2 = gen_reg_rtx (vmode);
  rtx ops2[] = {tmp2, d->op1, mask};
  emit_vlmax_insn (icode, COMPRESS_OP, ops2);

  rtx ops[] = {d->target, d->target, tmp2, gen_int_mode (vlen / 2, Pmode)};
  icode = code_for_pred_slide (UNSPEC_VSLIDEUP, vmode);
  emit_vlmax_insn (icode, SLIDEUP_OP_MERGE, ops);

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
      emit_vlmax_insn (icode, BINARY_OP, ops0);
      emit_vlmax_insn (icode, BINARY_OP, ops1);
    }
  /* Generate { 0, 1, .... } mask.  */
  rtx vid = gen_reg_rtx (sel_mode);
  rtx vid_repeat = gen_reg_rtx (sel_mode);
  expand_vec_series (vid, const0_rtx, const1_rtx);
  rtx and_ops[] = {vid_repeat, vid, const1_rtx};
  emit_vlmax_insn (code_for_pred_scalar (AND, sel_mode), BINARY_OP, and_ops);
  rtx const_vec = gen_const_vector_dup (sel_mode, 1);
  rtx mask = gen_reg_rtx (mask_mode);
  expand_vec_cmp (mask, EQ, vid_repeat, const_vec);
  emit_vlmax_decompress_insn (d->target, op0, op1, mask);
  return true;
}

static bool
shuffle_bswap_pattern (struct expand_vec_perm_d *d)
{
  HOST_WIDE_INT diff;
  unsigned i, size, step;

  if (!d->one_vector_p || !d->perm[0].is_constant (&diff) || !diff)
    return false;

  step = diff + 1;
  size = step * GET_MODE_UNIT_BITSIZE (d->vmode);

  switch (size)
    {
    case 16:
      break;
    case 32:
    case 64:
      /* We will have VEC_PERM_EXPR after rtl expand when invoking
	 __builtin_bswap. It will generate about 9 instructions in
	 loop as below, no matter it is bswap16, bswap32 or bswap64.
	   .L2:
	 1 vle16.v v4,0(a0)
	 2 vmv.v.x v2,a7
	 3 vand.vv v2,v6,v2
	 4 slli    a2,a5,1
	 5 vrgatherei16.vv v1,v4,v2
	 6 sub     a4,a4,a5
	 7 vse16.v v1,0(a3)
	 8 add     a0,a0,a2
	 9 add     a3,a3,a2
	   bne     a4,zero,.L2

	 But for bswap16 we may have a even simple code gen, which
	 has only 7 instructions in loop as below.
	   .L5
	 1 vle8.v  v2,0(a5)
	 2 addi    a5,a5,32
	 3 vsrl.vi v4,v2,8
	 4 vsll.vi v2,v2,8
	 5 vor.vv  v4,v4,v2
	 6 vse8.v  v4,0(a4)
	 7 addi    a4,a4,32
	   bne     a5,a6,.L5

	 Unfortunately, the instructions in loop will grow to 13 and 24
	 for bswap32 and bswap64. Thus, we will leverage vrgather (9 insn)
	 for both the bswap64 and bswap32, but take shift and or (7 insn)
	 for bswap16.
       */
    default:
      return false;
    }

  for (i = 0; i < step; i++)
    if (!d->perm.series_p (i, step, diff - i, step))
      return false;

  /* Disable when nunits < 4 since the later generic approach
     is more profitable on BSWAP.  */
  if (!known_gt (GET_MODE_NUNITS (d->vmode), 2))
    return false;

  if (d->testing_p)
    return true;

  machine_mode vhi_mode;
  poly_uint64 vhi_nunits = exact_div (GET_MODE_NUNITS (d->vmode), 2);

  if (!get_vector_mode (HImode, vhi_nunits).exists (&vhi_mode))
    return false;

  /* Step-1: Move op0 to src with VHI mode.  */
  rtx src = gen_reg_rtx (vhi_mode);
  emit_move_insn (src, gen_lowpart (vhi_mode, d->op0));

  /* Step-2: Shift right 8 bits to dest.  */
  rtx dest = expand_binop (vhi_mode, lshr_optab, src, gen_int_mode (8, Pmode),
			   NULL_RTX, 0, OPTAB_DIRECT);

  /* Step-3: Shift left 8 bits to src.  */
  src = expand_binop (vhi_mode, ashl_optab, src, gen_int_mode (8, Pmode),
		      NULL_RTX, 0, OPTAB_DIRECT);

  /* Step-4: Logic Or dest and src to dest.  */
  dest = expand_binop (vhi_mode, ior_optab, dest, src,
		       NULL_RTX, 0, OPTAB_DIRECT);

  /* Step-5: Move src to target with VQI mode.  */
  emit_move_insn (d->target, gen_lowpart (d->vmode, dest));

  return true;
}

/* Recognize patterns like [3 4 5 6] where we combine the last element
   of the first vector and the first n - 1 elements of the second vector.
   This can be implemented by slides or by extracting and re-inserting
   (slide1up) the first vector's last element.  */

static bool
shuffle_off_by_one_patterns (struct expand_vec_perm_d *d)
{
  poly_int64 nunits = GET_MODE_NUNITS (d->vmode);

  /* Recognize { nunits - 1, nunits, nunits + 1, ... }.  */
  if (!d->perm.series_p (0, 2, nunits - 1, 2)
      || !d->perm.series_p (1, 2, nunits, 2))
    return false;

  /* Disable when nunits < 4 since the later generic approach
     is more profitable on indice = { nunits - 1, nunits }.  */
  if (!known_gt (nunits, 2))
    return false;

  /* Success! */
  if (d->testing_p)
    return true;

  int scalar_cost = riscv_register_move_cost (d->vmode, V_REGS, GR_REGS)
    + riscv_register_move_cost (d->vmode, GR_REGS, V_REGS) + 2;
  int slide_cost = 2;

  if (slide_cost < scalar_cost)
    {
      /* This variant should always be preferable because we just need two
	 slides.  The extract-variant also requires two slides but additionally
	 pays the latency for register-file crossing.  */
      rtx tmp = gen_reg_rtx (d->vmode);
      rtx ops[] = {tmp, d->op1, gen_int_mode (1, Pmode)};
      insn_code icode = code_for_pred_slide (UNSPEC_VSLIDEUP, d->vmode);
      emit_vlmax_insn (icode, BINARY_OP, ops);

      rtx ops2[] = {d->target, tmp, d->op0, gen_int_mode (nunits - 1, Pmode)};
      icode = code_for_pred_slide (UNSPEC_VSLIDEDOWN, d->vmode);
      emit_nonvlmax_insn (icode, BINARY_OP_TUMA, ops2, gen_int_mode (1, Pmode));
    }
  else
    {
      /* Extract the last element of the first vector.  */
      scalar_mode smode = GET_MODE_INNER (d->vmode);
      rtx tmp = gen_reg_rtx (smode);
      emit_vec_extract (tmp, d->op0, gen_int_mode (nunits - 1, Pmode));

      /* Insert the scalar into element 0.  */
      unsigned int unspec
	= FLOAT_MODE_P (d->vmode) ? UNSPEC_VFSLIDE1UP : UNSPEC_VSLIDE1UP;
      insn_code icode = code_for_pred_slide (unspec, d->vmode);
      rtx ops[] = {d->target, d->op1, tmp};
      emit_vlmax_insn (icode, BINARY_OP, ops);
    }

  return true;
}

/* This looks for a series pattern in the provided vector permute structure D.
   If successful it emits a series insn as well as a gather to implement it.
   Return true if successful, false otherwise.  */

static bool
shuffle_series_patterns (struct expand_vec_perm_d *d)
{
  if (!d->one_vector_p || d->perm.encoding ().npatterns () != 1)
    return false;

  poly_int64 el1 = d->perm[0];
  poly_int64 el2 = d->perm[1];
  poly_int64 el3 = d->perm[2];

  poly_int64 step1 = el2 - el1;
  poly_int64 step2 = el3 - el2;

  bool need_insert = false;
  bool have_series = false;

  /* Check for a full series.  */
  if (known_ne (step1, 0) && d->perm.series_p (0, 1, el1, step1))
    have_series = true;

  /* Check for a series starting at the second element.  */
  else if (known_ne (step2, 0) && d->perm.series_p (1, 1, el2, step2))
    {
      have_series = true;
      need_insert = true;
    }

  if (!have_series)
    return false;

  /* Disable shuffle if we can't find an appropriate integer index mode for
     gather.  */
  machine_mode sel_mode;
  if (!get_gather_index_mode (d).exists (&sel_mode))
    return false;

  /* Success! */
  if (d->testing_p)
    return true;

  /* Create the series.  */
  machine_mode eltmode = Pmode;
  rtx series = gen_reg_rtx (sel_mode);
  expand_vec_series (series, gen_int_mode (need_insert ? el2 : el1, eltmode),
		     gen_int_mode (need_insert ? step2 : step1, eltmode));

  /* Insert the remaining element if necessary.  */
  if (need_insert)
    {
      insn_code icode = code_for_pred_slide (UNSPEC_VSLIDE1UP, sel_mode);
      rtx ops[]
	= {series, series, gen_int_mode (el1, GET_MODE_INNER (sel_mode))};
      emit_vlmax_insn (icode, BINARY_OP, ops);
    }

  emit_vlmax_gather_insn (d->target, d->op0, series);

  return true;
}

/* Recognize the pattern that can be shuffled by generic approach.  */

static bool
shuffle_generic_patterns (struct expand_vec_perm_d *d)
{
  machine_mode sel_mode;

  /* We don't enable SLP for non-power of 2 NPATTERNS.  */
  if (!pow2p_hwi (d->perm.encoding().npatterns ()))
    return false;

  /* Disable shuffle if we can't find an appropriate integer index mode for
     gather.  */
  if (!get_gather_index_mode (d).exists (&sel_mode))
    return false;

  /* Success! */
  if (d->testing_p)
    return true;

  rtx sel = vec_perm_indices_to_rtx (sel_mode, d->perm);
  /* Some FIXED-VLMAX/VLS vector permutation situations call targethook
     instead of expand vec_perm<mode>, we handle it directly.  */
  expand_vec_perm (d->target, d->op0, d->op1, sel);
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
	  if (shuffle_consecutive_patterns (d))
	    return true;
	  if (shuffle_slide_patterns (d))
	    return true;
	  if (shuffle_interleave_patterns (d))
	    return true;
	  if (shuffle_even_odd_patterns (d))
	    return true;
	  if (shuffle_compress_patterns (d))
	    return true;
	  if (shuffle_decompress_patterns (d))
	    return true;
	  if (shuffle_bswap_pattern (d))
	    return true;
	  if (shuffle_off_by_one_patterns (d))
	    return true;
	  if (shuffle_series_patterns (d))
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
  if (CONST_INT_P (ops[1]) && known_le (INTVAL (ops[1]), nunits))
    {
      /* If length is known <= VF, we just use the length directly instead
	 of using vsetvli.

	 E.g. _255 = .SELECT_VL (3, POLY_INT_CST [4, 4]);
	 We move 3 into _255 instead of using explicit vsetvl.  */
      emit_move_insn (ops[0], ops[1]);
      return;
    }
  /* We arbitrary picked QImode as inner scalar mode to get vector mode.
     since vsetvl only demand ratio. We let VSETVL PASS to optimize it.  */
  scalar_int_mode mode = QImode;
  machine_mode rvv_mode = get_vector_mode (mode, nunits).require ();
  emit_insn (gen_no_side_effects_vsetvl_rtx (rvv_mode, ops[0], ops[1]));
}

/* Return RVV_VUNDEF if the ELSE value is scratch rtx.  */
static rtx
get_else_operand (rtx op)
{
  return GET_CODE (op) == SCRATCH ? RVV_VUNDEF (GET_MODE (op)) : op;
}

/* Expand MASK_LEN_{LOAD,STORE}.  */
void
expand_load_store (rtx *ops, bool is_load)
{
  int idx = 2;
  rtx mask = ops[idx++];
  /* A masked load has a merge/else operand.  */
  if (is_load)
    get_else_operand (ops[idx++]);
  rtx len = ops[idx];
  machine_mode mode = GET_MODE (ops[0]);

  if (is_vlmax_len_p (mode, len))
    {
      /* If the length operand is equal to VF, it is VLMAX load/store.  */
      if (is_load)
	{
	  rtx m_ops[] = {ops[0], mask, ops[1]};
	  emit_vlmax_insn (code_for_pred_mov (mode), UNARY_OP_TAMA, m_ops);
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
	  rtx m_ops[] = {ops[0], mask, ops[1]};
	  emit_nonvlmax_insn (code_for_pred_mov (mode), UNARY_OP_TAMA, m_ops,
			       len);
	}
      else
	emit_insn (gen_pred_store (mode, ops[0], mask, ops[1], len,
				   get_avl_type_rtx (NONVLMAX)));
    }
}

/* Expand MASK_LEN_STRIDED_LOAD.  */
void
expand_strided_load (machine_mode mode, rtx *ops)
{
  rtx v_reg = ops[0];
  rtx base = ops[1];
  rtx stride = ops[2];
  rtx mask = ops[3];
  int idx = 4;
  get_else_operand (ops[idx++]);
  rtx len = ops[idx];
  poly_int64 len_val;

  insn_code icode = code_for_pred_strided_load (mode);
  rtx emit_ops[] = {v_reg, mask, gen_rtx_MEM (mode, base), stride};

  if (poly_int_rtx_p (len, &len_val)
      && known_eq (len_val, GET_MODE_NUNITS (mode)))
    emit_vlmax_insn (icode, BINARY_OP_TAMA, emit_ops);
  else
    {
      len = satisfies_constraint_K (len) ? len : force_reg (Pmode, len);
      emit_nonvlmax_insn (icode, BINARY_OP_TAMA, emit_ops, len);
    }
}

/* Expand MASK_LEN_STRIDED_STORE.  */
void
expand_strided_store (machine_mode mode, rtx *ops)
{
  rtx v_reg = ops[2];
  rtx base = ops[0];
  rtx stride = ops[1];
  rtx mask = ops[3];
  rtx len = ops[4];
  poly_int64 len_val;
  rtx vl_type;

  if (poly_int_rtx_p (len, &len_val)
      && known_eq (len_val, GET_MODE_NUNITS (mode)))
    {
      len = gen_reg_rtx (Pmode);
      emit_vlmax_vsetvl (mode, len);
      vl_type = get_avl_type_rtx (VLMAX);
    }
  else
    {
      len = satisfies_constraint_K (len) ? len : force_reg (Pmode, len);
      vl_type = get_avl_type_rtx (NONVLMAX);
    }

  emit_insn (gen_pred_strided_store (mode, gen_rtx_MEM (mode, base),
				     mask, stride, v_reg, len, vl_type));
}

/* Return true if the operation is the floating-point operation need FRM.  */
static bool
needs_fp_rounding (unsigned icode, machine_mode mode)
{
  if (!FLOAT_MODE_P (mode))
    return false;

  return icode != maybe_code_for_pred (SMIN, mode)
	 && icode != maybe_code_for_pred (UNSPEC_VFMIN, mode)
	 && icode != maybe_code_for_pred (SMAX, mode)
	 && icode != maybe_code_for_pred (UNSPEC_VFMAX, mode)
	 && icode != maybe_code_for_pred (NEG, mode)
	 && icode != maybe_code_for_pred (ABS, mode)
	 /* narrower-FP -> FP */
	 && icode != maybe_code_for_pred_extend (mode)
	 /* narrower-INT -> FP */
	 && icode != maybe_code_for_pred_widen (FLOAT, mode)
	 && icode != maybe_code_for_pred_widen (UNSIGNED_FLOAT, mode)
	 /* vfsgnj */
	 && icode != maybe_code_for_pred (UNSPEC_VCOPYSIGN, mode)
	 && icode != maybe_code_for_pred_mov (mode);
}

/* Subroutine to expand COND_LEN_* patterns.  */
static void
expand_cond_len_op (unsigned icode, insn_flags op_type, rtx *ops, rtx len)
{
  rtx dest = ops[0];
  rtx mask = ops[1];
  machine_mode mode = GET_MODE (dest);
  machine_mode mask_mode = GET_MODE (mask);
  bool is_dummy_mask = rtx_equal_p (mask, CONSTM1_RTX (mask_mode));
  bool is_vlmax_len = is_vlmax_len_p (mode, len);

  unsigned insn_flags = HAS_DEST_P | HAS_MASK_P | HAS_MERGE_P | op_type;
  /* FIXME: We don't support simplification of COND_LEN_NEG (..., dummy len,
     dummy mask) into NEG_EXPR in GIMPLE FOLD yet.  So, we do such
     simplification in RISC-V backend and may do that in middle-end in the
     future.  */
  if (is_dummy_mask && is_vlmax_len)
    insn_flags |= TDEFAULT_POLICY_P | MDEFAULT_POLICY_P;
  else if (is_dummy_mask)
    insn_flags |= TU_POLICY_P | MDEFAULT_POLICY_P;
  else if (is_vlmax_len)
    insn_flags |= TDEFAULT_POLICY_P | MU_POLICY_P;
  else
    insn_flags |= TU_POLICY_P | MU_POLICY_P;

  if (needs_fp_rounding (icode, mode))
    insn_flags |= FRM_DYN_P;

  if (is_vlmax_len)
    emit_vlmax_insn (icode, insn_flags, ops);
  else
    emit_nonvlmax_insn (icode, insn_flags, ops, len);
}

/* Expand unary ops COND_LEN_*.  */
void
expand_cond_len_unop (unsigned icode, rtx *ops)
{
  rtx dest = ops[0];
  rtx mask = ops[1];
  rtx src = ops[2];
  rtx merge = get_else_operand (ops[3]);
  rtx len = ops[4];

  rtx cond_ops[] = {dest, mask, merge, src};
  expand_cond_len_op (icode, UNARY_OP_P, cond_ops, len);
}

/* Expand unary ops COND_*.  */
void
expand_cond_unop (unsigned icode, rtx *ops)
{
  rtx dest = ops[0];
  rtx mask = ops[1];
  rtx src = ops[2];
  rtx merge = get_else_operand (ops[3]);
  rtx len = gen_int_mode (GET_MODE_NUNITS (GET_MODE (dest)), Pmode);

  rtx cond_ops[] = {dest, mask, merge, src};
  expand_cond_len_op (icode, UNARY_OP_P, cond_ops, len);
}

/* Expand binary ops COND_LEN_*.  */
void
expand_cond_len_binop (unsigned icode, rtx *ops)
{
  rtx dest = ops[0];
  rtx mask = ops[1];
  rtx src1 = ops[2];
  rtx src2 = ops[3];
  rtx merge = get_else_operand (ops[4]);
  rtx len = ops[5];

  rtx cond_ops[] = {dest, mask, merge, src1, src2};
  expand_cond_len_op (icode, BINARY_OP_P, cond_ops, len);
}

/* Expand binary ops COND_*.  */
void
expand_cond_binop (unsigned icode, rtx *ops)
{
  rtx dest = ops[0];
  rtx mask = ops[1];
  rtx src1 = ops[2];
  rtx src2 = ops[3];
  rtx merge = get_else_operand (ops[4]);
  rtx len = gen_int_mode (GET_MODE_NUNITS (GET_MODE (dest)), Pmode);

  rtx cond_ops[] = {dest, mask, merge, src1, src2};
  expand_cond_len_op (icode, BINARY_OP_P, cond_ops, len);
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
  int shift;
  rtx mask = ops[5];
  rtx len = ops[6];
  if (is_load)
    len = ops[7];
  if (is_load)
    {
      vec_reg = ops[0];
      ptr = ops[1];
      vec_offset = ops[2];
      zero_extend_p = INTVAL (ops[3]);
      shift = exact_log2 (INTVAL (ops[4]));
    }
  else
    {
      vec_reg = ops[4];
      ptr = ops[0];
      vec_offset = ops[1];
      zero_extend_p = INTVAL (ops[2]);
      shift = exact_log2 (INTVAL (ops[3]));
    }

  machine_mode vec_mode = GET_MODE (vec_reg);
  machine_mode idx_mode = GET_MODE (vec_offset);
  scalar_mode inner_idx_mode = GET_MODE_INNER (idx_mode);
  unsigned inner_offsize = GET_MODE_BITSIZE (inner_idx_mode);
  poly_int64 nunits = GET_MODE_NUNITS (vec_mode);
  bool is_vlmax = is_vlmax_len_p (vec_mode, len);

  bool use_widening_shift = false;

  /* Extend the offset element to address width.  */
  if (inner_offsize < BITS_PER_WORD)
    {
      use_widening_shift = TARGET_ZVBB && zero_extend_p && shift == 1;
      /* 7.2. Vector Load/Store Addressing Modes.
	 If the vector offset elements are narrower than XLEN, they are
	 zero-extended to XLEN before adding to the ptr effective address. If
	 the vector offset elements are wider than XLEN, the least-significant
	 XLEN bits are used in the address calculation. An implementation must
	 raise an illegal instruction exception if the EEW is not supported for
	 offset elements.

	 RVV spec only refers to the shift == 0 case.  */
      if (!zero_extend_p || shift)
	{
	  if (zero_extend_p)
	    inner_idx_mode
	      = int_mode_for_size (inner_offsize * 2, 0).require ();
	  else
	    inner_idx_mode = int_mode_for_size (BITS_PER_WORD, 0).require ();
	  machine_mode new_idx_mode
	    = get_vector_mode (inner_idx_mode, nunits).require ();
	  if (!use_widening_shift)
	    {
	      rtx tmp = gen_reg_rtx (new_idx_mode);
	      emit_insn (gen_extend_insn (tmp, vec_offset, new_idx_mode, idx_mode,
					  zero_extend_p ? true : false));
	      vec_offset = tmp;
	    }
	  idx_mode = new_idx_mode;
	}
    }

  if (shift)
    {
      rtx tmp;
      if (!use_widening_shift)
	tmp = expand_binop (idx_mode, ashl_optab, vec_offset,
			    gen_int_mode (shift, Pmode), NULL_RTX, 0,
			    OPTAB_DIRECT);
      else
	{
	  tmp = gen_reg_rtx (idx_mode);
	  insn_code icode = code_for_pred_vwsll_scalar (idx_mode);
	  rtx ops[] = {tmp, vec_offset, const1_rtx};
	  emit_vlmax_insn (icode, BINARY_OP, ops);
	}

      vec_offset = tmp;
    }

  insn_code icode = prepare_gather_scatter (vec_mode, idx_mode, is_load);
  if (is_vlmax)
    {
      if (is_load)
	{
	  rtx load_ops[]
	    = {vec_reg, mask, ptr, vec_offset};
	  emit_vlmax_insn (icode, BINARY_OP_TAMA, load_ops);
	}
      else
	{
	  rtx store_ops[] = {mask, ptr, vec_offset, vec_reg};
	  emit_vlmax_insn (icode, SCATTER_OP_M, store_ops);
	}
    }
  else
    {
      if (is_load)
	{
	  rtx load_ops[]
	    = {vec_reg, mask, ptr, vec_offset};
	  emit_nonvlmax_insn (icode, BINARY_OP_TAMA, load_ops, len);
	}
      else
	{
	  rtx store_ops[] = {mask, ptr, vec_offset, vec_reg};
	  emit_nonvlmax_insn (icode, SCATTER_OP_M, store_ops, len);
	}
    }
}

/* Expand COND_LEN_*.  */
void
expand_cond_len_ternop (unsigned icode, rtx *ops)
{
  rtx dest = ops[0];
  rtx mask = ops[1];
  rtx src1 = ops[2];
  rtx src2 = ops[3];
  rtx src3 = ops[4];
  rtx merge = get_else_operand (ops[5]);
  rtx len = ops[6];

  rtx cond_ops[] = {dest, mask, src1, src2, src3, merge};
  expand_cond_len_op (icode, TERNARY_OP_P, cond_ops, len);
}

/* Expand COND_*.  */
void
expand_cond_ternop (unsigned icode, rtx *ops)
{
  rtx dest = ops[0];
  rtx mask = ops[1];
  rtx src1 = ops[2];
  rtx src2 = ops[3];
  rtx src3 = ops[4];
  rtx merge = get_else_operand (ops[5]);
  rtx len = gen_int_mode (GET_MODE_NUNITS (GET_MODE (dest)), Pmode);

  rtx cond_ops[] = {dest, mask, src1, src2, src3, merge};
  expand_cond_len_op (icode, TERNARY_OP_P, cond_ops, len);
}

/* Expand reduction operations.
     Case 1: ops = {scalar_dest, vector_src}
     Case 2: ops = {scalar_dest, vector_src, mask, vl}
*/
void
expand_reduction (unsigned unspec, unsigned insn_flags, rtx *ops, rtx init)
{
  rtx scalar_dest = ops[0];
  rtx vector_src = ops[1];
  machine_mode vmode = GET_MODE (vector_src);
  machine_mode vel_mode = GET_MODE (scalar_dest);
  machine_mode m1_mode = get_m1_mode (vel_mode).require ();

  rtx m1_tmp = gen_reg_rtx (m1_mode);
  rtx scalar_move_ops[] = {m1_tmp, init};
  insn_code icode = code_for_pred_broadcast (m1_mode);
  if (need_mask_operand_p (insn_flags))
    emit_nonvlmax_insn (icode, SCALAR_MOVE_OP, scalar_move_ops, ops[3]);
  else
    emit_vlmax_insn (icode, SCALAR_MOVE_OP, scalar_move_ops);

  rtx m1_tmp2 = gen_reg_rtx (m1_mode);
  rtx reduc_ops[] = {m1_tmp2, vector_src, m1_tmp};
  icode = code_for_pred (unspec, vmode);

  if (need_mask_operand_p (insn_flags))
    {
      rtx mask_len_reduc_ops[] = {m1_tmp2, ops[2], vector_src, m1_tmp};
      emit_nonvlmax_insn (icode, insn_flags, mask_len_reduc_ops, ops[3]);
    }
  else
    emit_vlmax_insn (icode, insn_flags, reduc_ops);

  emit_insn (gen_pred_extract_first (m1_mode, scalar_dest, m1_tmp2));
}

/* Prepare ops for ternary operations.
   It can be called before or after RA.  */
void
prepare_ternary_operands (rtx *ops)
{
  machine_mode mode = GET_MODE (ops[0]);

  if (!rtx_equal_p (ops[5], RVV_VUNDEF (mode))
      && (VECTOR_MODE_P (GET_MODE (ops[2]))
	  && !rtx_equal_p (ops[2], ops[5]))
      && !rtx_equal_p (ops[3], ops[5])
      && !rtx_equal_p (ops[4], ops[5]))
    {
      /* RA will fail to find vector REG and report ICE, so we pre-merge
	 the ops for LMUL = 8.  */
      if (satisfies_constraint_Wc1 (ops[1]))
	{
	  emit_move_insn (ops[0], ops[5]);
	  emit_insn (gen_pred_mov (mode, ops[0], ops[1], ops[0], ops[4], ops[6],
				   ops[7], ops[8], ops[9]));
	}
      else
	emit_insn (gen_pred_merge (mode, ops[0], RVV_VUNDEF (mode), ops[5],
				   ops[4], ops[1], ops[6], ops[7], ops[9]));
      ops[5] = ops[4] = ops[0];
    }
  else
    {
      /* Swap the multiplication ops if the fallback value is the
	 second of the two.  */
      if (rtx_equal_p (ops[3], ops[5]))
	std::swap (ops[2], ops[3]);

      /* TODO: ??? Maybe we could support splitting FMA (a, 4, b)
	 into PLUS (ASHIFT (a, 2), b) according to uarchs.  */
    }
  gcc_assert (rtx_equal_p (ops[5], RVV_VUNDEF (mode))
	      || rtx_equal_p (ops[5], ops[2]) || rtx_equal_p (ops[5], ops[4]));
}

/* Expand VEC_MASK_LEN_{LOAD_LANES,STORE_LANES}.  */
void
expand_lanes_load_store (rtx *ops, bool is_load)
{
  rtx mask = ops[2];
  rtx len = ops[3];
  if (is_load)
    len = ops[4];
  rtx addr = is_load ? XEXP (ops[1], 0) : XEXP (ops[0], 0);
  rtx reg = is_load ? ops[0] : ops[1];
  machine_mode mode = GET_MODE (ops[0]);

  if (is_vlmax_len_p (mode, len))
    {
      /* If the length operand is equal to VF, it is VLMAX load/store.  */
      if (is_load)
	{
	  rtx m_ops[] = {reg, mask, addr};
	  emit_vlmax_insn (code_for_pred_unit_strided_load (mode), UNARY_OP_TAMA,
			    m_ops);
	}
      else
	{
	  len = gen_reg_rtx (Pmode);
	  emit_vlmax_vsetvl (mode, len);
	  emit_insn (gen_pred_unit_strided_store (mode, mask, addr, reg, len,
						  get_avl_type_rtx (VLMAX)));
	}
    }
  else
    {
      if (!satisfies_constraint_K (len))
	len = force_reg (Pmode, len);
      if (is_load)
	{
	  rtx m_ops[] = {reg, mask, addr};
	  emit_nonvlmax_insn (code_for_pred_unit_strided_load (mode),
			       UNARY_OP_TAMA, m_ops, len);
	}
      else
	emit_insn (gen_pred_unit_strided_store (mode, mask, addr, reg, len,
						get_avl_type_rtx (NONVLMAX)));
    }
}

/* Expand LEN_FOLD_EXTRACT_LAST.  */
void
expand_fold_extract_last (rtx *ops)
{
  rtx dst = ops[0];
  rtx default_value = ops[1];
  rtx mask = ops[2];
  rtx anchor = gen_reg_rtx (Pmode);
  rtx index = gen_reg_rtx (Pmode);
  rtx vect = ops[3];
  rtx else_label = gen_label_rtx ();
  rtx end_label = gen_label_rtx ();
  rtx len = ops[4];
  machine_mode mode = GET_MODE (vect);
  machine_mode mask_mode = GET_MODE (mask);
  rtx compress_vect = gen_reg_rtx (mode);
  rtx slide_vect = gen_reg_rtx (mode);
  insn_code icode;

  if (is_vlmax_len_p (mode, len))
    len = NULL_RTX;

  /* Calculate the number of 1-bit in mask. */
  rtx cpop_ops[] = {anchor, mask};
  if (len)
    emit_nonvlmax_insn (code_for_pred_popcount (mask_mode, Pmode), CPOP_OP,
			 cpop_ops, len);
  else
    emit_vlmax_insn (code_for_pred_popcount (mask_mode, Pmode), CPOP_OP,
		      cpop_ops);

  riscv_expand_conditional_branch (else_label, EQ, anchor, const0_rtx);
  emit_insn (gen_rtx_SET (index, gen_rtx_PLUS (Pmode, anchor, constm1_rtx)));
  /* Compress the vector.  */
  icode = code_for_pred_compress (mode);
  rtx compress_ops[] = {compress_vect, vect, mask};
  if (len)
    emit_nonvlmax_insn (icode, COMPRESS_OP, compress_ops, len);
  else
    emit_vlmax_insn (icode, COMPRESS_OP, compress_ops);
  /* Emit the slide down to index 0 in a new vector.  */
  rtx slide_ops[] = {slide_vect, compress_vect, index};
  icode = code_for_pred_slide (UNSPEC_VSLIDEDOWN, mode);
  if (len)
    emit_nonvlmax_insn (icode, BINARY_OP, slide_ops, len);
  else
    emit_vlmax_insn (icode, BINARY_OP, slide_ops);
  /* Emit v(f)mv.[xf].s.  */
  emit_insn (gen_pred_extract_first (mode, dst, slide_vect));

  emit_jump_insn (gen_jump (end_label));
  emit_barrier ();
  emit_label (else_label);
  emit_move_insn (dst, default_value);
  emit_label (end_label);
}

/* Return true if the LMUL of comparison less than or equal to one.  */
bool
cmp_lmul_le_one (machine_mode mode)
{
  if (riscv_v_ext_vector_mode_p (mode))
    return known_le (GET_MODE_SIZE (mode), BYTES_PER_RISCV_VECTOR);
  else if (riscv_v_ext_vls_mode_p (mode))
    return known_le (GET_MODE_BITSIZE (mode), TARGET_MIN_VLEN);
  return false;
}

/* Return true if the LMUL of comparison greater than one.  */
bool
cmp_lmul_gt_one (machine_mode mode)
{
  if (riscv_v_ext_vector_mode_p (mode))
    return known_gt (GET_MODE_SIZE (mode), BYTES_PER_RISCV_VECTOR);
  else if (riscv_v_ext_vls_mode_p (mode))
    return known_gt (GET_MODE_BITSIZE (mode), TARGET_MIN_VLEN);
  return false;
}

/* Return true if the VLS mode is legal. There are 2 cases here.

   1. Enable VLS modes for VLA vectorization since fixed length VLMAX mode
      is the highest priority choice and should not conflict with VLS modes.
   2. Enable VLS modes for some cases in fixed-vlmax, aka the bitsize of the
      VLS mode are smaller than the minimal vla.

   Take vlen = 2048 as example for case 2.

   Note: Below table based on vlen = 2048.
   +----------------------------------------------------+----------------------+
   | VLS mode                                           | VLA mode             |
   +----------------------------------------------------+----------------------+
   | Name       | Precision | Inner Precision | Enabled | Min mode  | Min bits |
   +------------+-----------+-----------------+---------+-----------+----------+
   | V1BI       |     1     |              1  | Yes     | RVVMF64BI |    32    |
   | V2BI       |     2     |              1  | Yes     | RVVMF64BI |    32    |
   | V4BI       |     4     |              1  | Yes     | RVVMF64BI |    32    |
   | V8BI       |     8     |              1  | Yes     | RVVMF64BI |    32    |
   | V16BI      |    16     |              1  | Yes     | RVVMF64BI |    32    |
   | V32BI      |    32     |              1  | NO      | RVVMF64BI |    32    |
   | V64BI      |    64     |              1  | NO      | RVVMF64BI |    32    |
   | ...        |   ...     |            ...  | ...     | RVVMF64BI |    32    |
   | V4096BI    |  4096     |              1  | NO      | RVVMF64BI |    32    |
   +------------+-----------+-----------------+---------+-----------+----------+
   | V1QI       |     8     |              8  | Yes     | RVVMF8QI  |   256    |
   | V2QI       |    16     |              8  | Yes     | RVVMF8QI  |   256    |
   | V4QI       |    32     |              8  | Yes     | RVVMF8QI  |   256    |
   | V8QI       |    64     |              8  | Yes     | RVVMF8QI  |   256    |
   | V16QI      |   128     |              8  | Yes     | RVVMF8QI  |   256    |
   | V32QI      |   256     |              8  | NO      | RVVMF8QI  |   256    |
   | V64QI      |   512     |              8  | NO      | RVVMF8QI  |   256    |
   | ...        |   ...     |              .. | ...     | RVVMF8QI  |   256    |
   | V4096QI    | 32768     |              8  | NO      | RVVMF8QI  |   256    |
   +------------+-----------+-----------------+---------+-----------+----------+
   | V1HI       |    16     |              16 | Yes     | RVVMF4HI  |   512    |
   | V2HI       |    32     |              16 | Yes     | RVVMF4HI  |   512    |
   | V4HI       |    64     |              16 | Yes     | RVVMF4HI  |   512    |
   | V8HI       |   128     |              16 | Yes     | RVVMF4HI  |   512    |
   | V16HI      |   256     |              16 | Yes     | RVVMF4HI  |   512    |
   | V32HI      |   512     |              16 | NO      | RVVMF4HI  |   512    |
   | V64HI      |  1024     |              16 | NO      | RVVMF4HI  |   512    |
   | ...        |   ...     |              .. | ...     | RVVMF4HI  |   512    |
   | V2048HI    | 32768     |              16 | NO      | RVVMF4HI  |   512    |
   +------------+-----------+-----------------+---------+-----------+----------+
   | V1SI/SF    |    32     |              32 | Yes     | RVVMF2SI  |  1024    |
   | V2SI/SF    |    64     |              32 | Yes     | RVVMF2SI  |  1024    |
   | V4SI/SF    |   128     |              32 | Yes     | RVVMF2SI  |  1024    |
   | V8SI/SF    |   256     |              32 | Yes     | RVVMF2SI  |  1024    |
   | V16SI/SF   |   512     |              32 | Yes     | RVVMF2SI  |  1024    |
   | V32SI/SF   |  1024     |              32 | NO      | RVVMF2SI  |  1024    |
   | V64SI/SF   |  2048     |              32 | NO      | RVVMF2SI  |  1024    |
   | ...        |   ...     |              .. | ...     | RVVMF2SI  |  1024    |
   | V1024SI/SF | 32768     |              32 | NO      | RVVMF2SI  |  1024    |
   +------------+-----------+-----------------+---------+-----------+----------+
   | V1DI/DF    |    64     |              64 | Yes     | RVVM1DI   |  2048    |
   | V2DI/DF    |   128     |              64 | Yes     | RVVM1DI   |  2048    |
   | V4DI/DF    |   256     |              64 | Yes     | RVVM1DI   |  2048    |
   | V8DI/DF    |   512     |              64 | Yes     | RVVM1DI   |  2048    |
   | V16DI/DF   |  1024     |              64 | Yes     | RVVM1DI   |  2048    |
   | V32DI/DF   |  2048     |              64 | NO      | RVVM1DI   |  2048    |
   | V64DI/DF   |  4096     |              64 | NO      | RVVM1DI   |  2048    |
   | ...        |   ...     |              .. | ...     | RVVM1DI   |  2048    |
   | V512DI/DF  | 32768     |              64 | NO      | RVVM1DI   |  2048    |
   +------------+-----------+-----------------+---------+-----------+----------+

   Then we can have the condition for VLS mode in fixed-vlmax, aka:
     PRECISION (VLSmode) < VLEN / (64 / PRECISION(VLS_inner_mode)).  */
bool
vls_mode_valid_p (machine_mode vls_mode)
{
  if (!TARGET_VECTOR || TARGET_XTHEADVECTOR)
    return false;

  if (rvv_vector_bits == RVV_VECTOR_BITS_SCALABLE)
    {
      if (GET_MODE_CLASS (vls_mode) != MODE_VECTOR_BOOL
	  && !ordered_p (TARGET_MAX_LMUL * BITS_PER_RISCV_VECTOR,
			 GET_MODE_PRECISION (vls_mode)))
	/* We enable VLS modes which are aligned with TARGET_MAX_LMUL and
	   BITS_PER_RISCV_VECTOR.

	   e.g. When TARGET_MAX_LMUL = 1 and BITS_PER_RISCV_VECTOR = (128,128).
	   We enable VLS modes have fixed size <= 128bit.  Since ordered_p is
	   false between VLA modes with size = (128, 128) bits and VLS mode
	   with size = 128 bits, we will end up with multiple ICEs in
	   middle-end generic codes.  */
	return false;
      return true;
    }

  if (rvv_vector_bits == RVV_VECTOR_BITS_ZVL)
    {
      machine_mode inner_mode = GET_MODE_INNER (vls_mode);
      int precision = GET_MODE_PRECISION (inner_mode).to_constant ();
      int min_vlmax_bitsize = TARGET_MIN_VLEN / (64 / precision);

      return GET_MODE_PRECISION (vls_mode).to_constant () < min_vlmax_bitsize;
    }

  return false;
}

/* We don't have to convert the floating point to integer when the
   mantissa is zero.  Thus, ther will be a limitation for both the
   single and double precision floating point.  There will be no
   mantissa if the floating point is greater than the limit.

   1. Half floating point.
      +-----------+---------------+
      | float     | binary layout |
      +-----------+---------------+
      | 1023.5    | 0x63ff        |
      +-----------+---------------+
      | 1024.0    | 0x6400        |
      +-----------+---------------+
      | 1025.0    | 0x6401        |
      +-----------+---------------+
      | ...       | ...           |

      All half floating point will be unchanged for ceil if it is
      greater than and equal to 1024.

   2. Single floating point.
      +-----------+---------------+
      | float     | binary layout |
      +-----------+---------------+
      | 8388607.5 | 0x4affffff    |
      +-----------+---------------+
      | 8388608.0 | 0x4b000000    |
      +-----------+---------------+
      | 8388609.0 | 0x4b000001    |
      +-----------+---------------+
      | ...       | ...           |

      All single floating point will be unchanged for ceil if it is
      greater than and equal to 8388608.

   3. Double floating point.
      +--------------------+--------------------+
      | float              | binary layout      |
      +--------------------+--------------------+
      | 4503599627370495.5 | 0X432fffffffffffff |
      +--------------------+--------------------+
      | 4503599627370496.0 | 0X4330000000000000 |
      +--------------------+--------------------+
      | 4503599627370497.0 | 0X4340000000000000 |
      +--------------------+--------------------+
      | ...                | ...                |

      All double floating point will be unchanged for ceil if it is
      greater than and equal to 4503599627370496.
 */
rtx
get_fp_rounding_coefficient (machine_mode inner_mode)
{
  REAL_VALUE_TYPE real;

  if (inner_mode == E_HFmode)
    real_from_integer (&real, inner_mode, 1024, SIGNED);
  else if (inner_mode == E_SFmode)
    real_from_integer (&real, inner_mode, 8388608, SIGNED);
  else if (inner_mode == E_DFmode)
    real_from_integer (&real, inner_mode, 4503599627370496, SIGNED);
  else
    gcc_unreachable ();

  return const_double_from_real_value (real, inner_mode);
}

static rtx
emit_vec_float_cmp_mask (rtx fp_vector, rtx_code code, rtx fp_scalar,
			 machine_mode vec_fp_mode)
{
  /* Step-1: Prepare the scalar float compare register.  */
  rtx fp_reg = gen_reg_rtx (GET_MODE_INNER (vec_fp_mode));
  emit_insn (gen_move_insn (fp_reg, fp_scalar));

  /* Step-2: Generate the mask.  */
  machine_mode mask_mode = get_mask_mode (vec_fp_mode);
  rtx mask = gen_reg_rtx (mask_mode);
  rtx cmp = gen_rtx_fmt_ee (code, mask_mode, fp_vector, fp_reg);
  rtx cmp_ops[] = {mask, cmp, fp_vector, fp_reg};
  insn_code icode = code_for_pred_cmp_scalar (vec_fp_mode);
  emit_vlmax_insn (icode, COMPARE_OP, cmp_ops);

  return mask;
}

static void
emit_vec_copysign (rtx op_dest, rtx op_src_0, rtx op_src_1,
		   machine_mode vec_mode)
{
  rtx sgnj_ops[] = {op_dest, op_src_0, op_src_1};
  insn_code icode = code_for_pred (UNSPEC_VCOPYSIGN, vec_mode);

  emit_vlmax_insn (icode, BINARY_OP, sgnj_ops);
}

static void
emit_vec_abs (rtx op_dest, rtx op_src, machine_mode vec_mode)
{
  rtx abs_ops[] = {op_dest, op_src};
  insn_code icode = code_for_pred (ABS, vec_mode);

  emit_vlmax_insn (icode, UNARY_OP, abs_ops);
}

static void
emit_vec_cvt_x_f (rtx op_dest, rtx op_src, rtx mask,
		  insn_type type, machine_mode vec_mode)
{
  insn_code icode = code_for_pred_fcvt_x_f (UNSPEC_VFCVT, vec_mode);

  if (type & USE_VUNDEF_MERGE_P)
    {
      rtx cvt_x_ops[] = {op_dest, mask, op_src};
      emit_vlmax_insn (icode, type, cvt_x_ops);
    }
  else
    {
      rtx cvt_x_ops[] = {op_dest, mask, op_dest, op_src};
      emit_vlmax_insn (icode, type, cvt_x_ops);
    }
}

static void
emit_vec_cvt_x_f (rtx op_dest, rtx op_src, insn_type type,
		  machine_mode vec_mode)
{
  rtx ops[] = {op_dest, op_src};
  insn_code icode = code_for_pred_fcvt_x_f (UNSPEC_VFCVT, vec_mode);

  emit_vlmax_insn (icode, type, ops);
}

static void
emit_vec_narrow_cvt_x_f (rtx op_dest, rtx op_src, insn_type type,
			 machine_mode vec_mode)
{
  rtx ops[] = {op_dest, op_src};
  insn_code icode = code_for_pred_narrow_fcvt_x_f (UNSPEC_VFCVT, vec_mode);

  emit_vlmax_insn (icode, type, ops);
}

static void
emit_vec_widen_cvt_x_f (rtx op_dest, rtx op_src, insn_type type,
			 machine_mode vec_mode)
{
  rtx ops[] = {op_dest, op_src};
  insn_code icode = code_for_pred_widen_fcvt_x_f (UNSPEC_VFCVT, vec_mode);

  emit_vlmax_insn (icode, type, ops);
}

static void
emit_vec_widen_cvt_f_f (rtx op_dest, rtx op_src, insn_type type,
			 machine_mode vec_mode)
{
  rtx ops[] = {op_dest, op_src};
  insn_code icode = code_for_pred_extend (vec_mode);

  emit_vlmax_insn (icode, type, ops);
}

static void
emit_vec_cvt_f_x (rtx op_dest, rtx op_src, rtx mask,
		  insn_type type, machine_mode vec_mode)
{
  rtx cvt_fp_ops[] = {op_dest, mask, op_dest, op_src};
  insn_code icode = code_for_pred (FLOAT, vec_mode);

  emit_vlmax_insn (icode, type, cvt_fp_ops);
}

static void
emit_vec_cvt_x_f_rtz (rtx op_dest, rtx op_src, rtx mask,
		      insn_type type, machine_mode vec_mode)
{
  insn_code icode = code_for_pred (FIX, vec_mode);

  if (type & USE_VUNDEF_MERGE_P)
    {
      rtx cvt_x_ops[] = {op_dest, mask, op_src};
      emit_vlmax_insn (icode, type, cvt_x_ops);
    }
  else
    {
      rtx cvt_x_ops[] = {op_dest, mask, op_dest, op_src};
      emit_vlmax_insn (icode, type, cvt_x_ops);
    }
}

static void
emit_vec_binary_alu (rtx op_dest, rtx op_1, rtx op_2, enum rtx_code rcode,
		     machine_mode vec_mode)
{
  rtx ops[] = {op_dest, op_1, op_2};
  insn_code icode = code_for_pred (rcode, vec_mode);

  emit_vlmax_insn (icode, BINARY_OP, ops);
}

void
expand_vec_ceil (rtx op_0, rtx op_1, machine_mode vec_fp_mode,
		 machine_mode vec_int_mode)
{
  /* Step-1: Get the abs float value for mask generation.  */
  emit_vec_abs (op_0, op_1, vec_fp_mode);

  /* Step-2: Generate the mask on const fp.  */
  rtx const_fp = get_fp_rounding_coefficient (GET_MODE_INNER (vec_fp_mode));
  rtx mask = emit_vec_float_cmp_mask (op_0, LT, const_fp, vec_fp_mode);

  /* Step-3: Convert to integer on mask, with rounding up (aka ceil).  */
  rtx tmp = gen_reg_rtx (vec_int_mode);
  emit_vec_cvt_x_f (tmp, op_1, mask, UNARY_OP_TAMA_FRM_RUP, vec_fp_mode);

  /* Step-4: Convert to floating-point on mask for the final result.
     To avoid unnecessary frm register access, we use RUP here and it will
     never do the rounding up because the tmp rtx comes from the float
     to int conversion.  */
  emit_vec_cvt_f_x (op_0, tmp, mask, UNARY_OP_TAMU_FRM_RUP, vec_fp_mode);

  /* Step-5: Retrieve the sign bit for -0.0.  */
  emit_vec_copysign (op_0, op_0, op_1, vec_fp_mode);
}

void
expand_vec_floor (rtx op_0, rtx op_1, machine_mode vec_fp_mode,
		  machine_mode vec_int_mode)
{
  /* Step-1: Get the abs float value for mask generation.  */
  emit_vec_abs (op_0, op_1, vec_fp_mode);

  /* Step-2: Generate the mask on const fp.  */
  rtx const_fp = get_fp_rounding_coefficient (GET_MODE_INNER (vec_fp_mode));
  rtx mask = emit_vec_float_cmp_mask (op_0, LT, const_fp, vec_fp_mode);

  /* Step-3: Convert to integer on mask, with rounding down (aka floor).  */
  rtx tmp = gen_reg_rtx (vec_int_mode);
  emit_vec_cvt_x_f (tmp, op_1, mask, UNARY_OP_TAMA_FRM_RDN, vec_fp_mode);

  /* Step-4: Convert to floating-point on mask for the floor result.  */
  emit_vec_cvt_f_x (op_0, tmp, mask, UNARY_OP_TAMU_FRM_RDN, vec_fp_mode);

  /* Step-5: Retrieve the sign bit for -0.0.  */
  emit_vec_copysign (op_0, op_0, op_1, vec_fp_mode);
}

void
expand_vec_nearbyint (rtx op_0, rtx op_1, machine_mode vec_fp_mode,
		      machine_mode vec_int_mode)
{
  /* Step-1: Get the abs float value for mask generation.  */
  emit_vec_abs (op_0, op_1, vec_fp_mode);

  /* Step-2: Generate the mask on const fp.  */
  rtx const_fp = get_fp_rounding_coefficient (GET_MODE_INNER (vec_fp_mode));
  rtx mask = emit_vec_float_cmp_mask (op_0, LT, const_fp, vec_fp_mode);

  /* Step-3: Backup FP exception flags, nearbyint never raise exceptions. */
  rtx fflags = gen_reg_rtx (SImode);
  emit_insn (gen_riscv_frflags (fflags));

  /* Step-4: Convert to integer on mask, with rounding down (aka nearbyint).  */
  rtx tmp = gen_reg_rtx (vec_int_mode);
  emit_vec_cvt_x_f (tmp, op_1, mask, UNARY_OP_TAMA_FRM_DYN, vec_fp_mode);

  /* Step-5: Convert to floating-point on mask for the nearbyint result.  */
  emit_vec_cvt_f_x (op_0, tmp, mask, UNARY_OP_TAMU_FRM_DYN, vec_fp_mode);

  /* Step-6: Restore FP exception flags. */
  emit_insn (gen_riscv_fsflags (fflags));

  /* Step-7: Retrieve the sign bit for -0.0.  */
  emit_vec_copysign (op_0, op_0, op_1, vec_fp_mode);
}

void
expand_vec_rint (rtx op_0, rtx op_1, machine_mode vec_fp_mode,
		 machine_mode vec_int_mode)
{
  /* Step-1: Get the abs float value for mask generation.  */
  emit_vec_abs (op_0, op_1, vec_fp_mode);

  /* Step-2: Generate the mask on const fp.  */
  rtx const_fp = get_fp_rounding_coefficient (GET_MODE_INNER (vec_fp_mode));
  rtx mask = emit_vec_float_cmp_mask (op_0, LT, const_fp, vec_fp_mode);

  /* Step-3: Convert to integer on mask, with dyn rounding (aka rint).  */
  rtx tmp = gen_reg_rtx (vec_int_mode);
  emit_vec_cvt_x_f (tmp, op_1, mask, UNARY_OP_TAMA_FRM_DYN, vec_fp_mode);

  /* Step-4: Convert to floating-point on mask for the rint result.  */
  emit_vec_cvt_f_x (op_0, tmp, mask, UNARY_OP_TAMU_FRM_DYN, vec_fp_mode);

  /* Step-5: Retrieve the sign bit for -0.0.  */
  emit_vec_copysign (op_0, op_0, op_1, vec_fp_mode);
}

void
expand_vec_round (rtx op_0, rtx op_1, machine_mode vec_fp_mode,
		  machine_mode vec_int_mode)
{
  /* Step-1: Get the abs float value for mask generation.  */
  emit_vec_abs (op_0, op_1, vec_fp_mode);

  /* Step-2: Generate the mask on const fp.  */
  rtx const_fp = get_fp_rounding_coefficient (GET_MODE_INNER (vec_fp_mode));
  rtx mask = emit_vec_float_cmp_mask (op_0, LT, const_fp, vec_fp_mode);

  /* Step-3: Convert to integer on mask, rounding to nearest (aka round).  */
  rtx tmp = gen_reg_rtx (vec_int_mode);
  emit_vec_cvt_x_f (tmp, op_1, mask, UNARY_OP_TAMA_FRM_RMM, vec_fp_mode);

  /* Step-4: Convert to floating-point on mask for the round result.  */
  emit_vec_cvt_f_x (op_0, tmp, mask, UNARY_OP_TAMU_FRM_RMM, vec_fp_mode);

  /* Step-5: Retrieve the sign bit for -0.0.  */
  emit_vec_copysign (op_0, op_0, op_1, vec_fp_mode);
}

void
expand_vec_trunc (rtx op_0, rtx op_1, machine_mode vec_fp_mode,
		  machine_mode vec_int_mode)
{
  /* Step-1: Get the abs float value for mask generation.  */
  emit_vec_abs (op_0, op_1, vec_fp_mode);

  /* Step-2: Generate the mask on const fp.  */
  rtx const_fp = get_fp_rounding_coefficient (GET_MODE_INNER (vec_fp_mode));
  rtx mask = emit_vec_float_cmp_mask (op_0, LT, const_fp, vec_fp_mode);

  /* Step-3: Convert to integer on mask, rounding to zero (aka truncate).  */
  rtx tmp = gen_reg_rtx (vec_int_mode);
  emit_vec_cvt_x_f_rtz (tmp, op_1, mask, UNARY_OP_TAMA, vec_fp_mode);

  /* Step-4: Convert to floating-point on mask for the rint result.  */
  emit_vec_cvt_f_x (op_0, tmp, mask, UNARY_OP_TAMU_FRM_DYN, vec_fp_mode);

  /* Step-5: Retrieve the sign bit for -0.0.  */
  emit_vec_copysign (op_0, op_0, op_1, vec_fp_mode);
}

void
expand_vec_roundeven (rtx op_0, rtx op_1, machine_mode vec_fp_mode,
		      machine_mode vec_int_mode)
{
  /* Step-1: Get the abs float value for mask generation.  */
  emit_vec_abs (op_0, op_1, vec_fp_mode);

  /* Step-2: Generate the mask on const fp.  */
  rtx const_fp = get_fp_rounding_coefficient (GET_MODE_INNER (vec_fp_mode));
  rtx mask = emit_vec_float_cmp_mask (op_0, LT, const_fp, vec_fp_mode);

  /* Step-3: Convert to integer on mask, rounding to nearest, ties to even.  */
  rtx tmp = gen_reg_rtx (vec_int_mode);
  emit_vec_cvt_x_f (tmp, op_1, mask, UNARY_OP_TAMA_FRM_RNE, vec_fp_mode);

  /* Step-4: Convert to floating-point on mask for the rint result.  */
  emit_vec_cvt_f_x (op_0, tmp, mask, UNARY_OP_TAMU_FRM_RNE, vec_fp_mode);

  /* Step-5: Retrieve the sign bit for -0.0.  */
  emit_vec_copysign (op_0, op_0, op_1, vec_fp_mode);
}

/* Handling the rounding from floating-point to int/long/long long.  */
static void
emit_vec_rounding_to_integer (rtx op_0, rtx op_1, insn_type type,
			      machine_mode vec_fp_mode,
			      machine_mode vec_int_mode,
			      machine_mode vec_bridge_mode = E_VOIDmode)
{
  poly_uint16 vec_fp_size = GET_MODE_SIZE (vec_fp_mode);
  poly_uint16 vec_int_size = GET_MODE_SIZE (vec_int_mode);

  if (known_eq (vec_fp_size, vec_int_size)) /* SF => SI, DF => DI.  */
    emit_vec_cvt_x_f (op_0, op_1, type, vec_fp_mode);
  else if (maybe_eq (vec_fp_size, vec_int_size * 2)) /* DF => SI.  */
    emit_vec_narrow_cvt_x_f (op_0, op_1, type, vec_fp_mode);
  else if (maybe_eq (vec_fp_size * 2, vec_int_size)) /* SF => DI, HF => SI.  */
    emit_vec_widen_cvt_x_f (op_0, op_1, type, vec_int_mode);
  else if (maybe_eq (vec_fp_size * 4, vec_int_size)) /* HF => DI.  */
    {
      gcc_assert (vec_bridge_mode != E_VOIDmode);

      rtx op_sf = gen_reg_rtx (vec_bridge_mode);

      /* Step-1: HF => SF, no rounding here.  */
      emit_vec_widen_cvt_f_f (op_sf, op_1, UNARY_OP, vec_bridge_mode);
      /* Step-2: SF => DI.  */
      emit_vec_widen_cvt_x_f (op_0, op_sf, type, vec_int_mode);
    }
  else
    gcc_unreachable ();
}

void
expand_vec_lrint (rtx op_0, rtx op_1, machine_mode vec_fp_mode,
		  machine_mode vec_int_mode, machine_mode vec_bridge_mode)
{
  emit_vec_rounding_to_integer (op_0, op_1, UNARY_OP_FRM_DYN, vec_fp_mode,
				vec_int_mode, vec_bridge_mode);
}

void
expand_vec_lround (rtx op_0, rtx op_1, machine_mode vec_fp_mode,
		   machine_mode vec_int_mode, machine_mode vec_bridge_mode)
{
  emit_vec_rounding_to_integer (op_0, op_1, UNARY_OP_FRM_RMM, vec_fp_mode,
				vec_int_mode, vec_bridge_mode);
}

void
expand_vec_lceil (rtx op_0, rtx op_1, machine_mode vec_fp_mode,
		  machine_mode vec_int_mode)
{
  emit_vec_rounding_to_integer (op_0, op_1, UNARY_OP_FRM_RUP, vec_fp_mode,
				vec_int_mode);
}

void
expand_vec_lfloor (rtx op_0, rtx op_1, machine_mode vec_fp_mode,
		   machine_mode vec_int_mode)
{
  emit_vec_rounding_to_integer (op_0, op_1, UNARY_OP_FRM_RDN, vec_fp_mode,
				vec_int_mode);
}

/* Expand the standard name usadd<mode>3 for vector mode,  we can leverage
   the vector fixed point vector single-width saturating add directly.  */

void
expand_vec_usadd (rtx op_0, rtx op_1, rtx op_2, machine_mode vec_mode)
{
  emit_vec_binary_alu (op_0, op_1, op_2, US_PLUS, vec_mode);
}

/* Expand the standard name ssadd<mode>3 for vector mode,  we can leverage
   the vector fixed point vector single-width saturating add directly.  */

void
expand_vec_ssadd (rtx op_0, rtx op_1, rtx op_2, machine_mode vec_mode)
{
  emit_vec_binary_alu (op_0, op_1, op_2, SS_PLUS, vec_mode);
}

/* Expand the standard name usadd<mode>3 for vector mode,  we can leverage
   the vector fixed point vector single-width saturating add directly.  */

void
expand_vec_ussub (rtx op_0, rtx op_1, rtx op_2, machine_mode vec_mode)
{
  emit_vec_binary_alu (op_0, op_1, op_2, US_MINUS, vec_mode);
}

/* Expand the standard name ssadd<mode>3 for vector mode,  we can leverage
   the vector fixed point vector single-width saturating add directly.  */

void
expand_vec_sssub (rtx op_0, rtx op_1, rtx op_2, machine_mode vec_mode)
{
  emit_vec_binary_alu (op_0, op_1, op_2, SS_MINUS, vec_mode);
}

/* Expand the standard name ustrunc<m><n>2 for double vector mode,  like
   DI => SI.  we can leverage the vector fixed point vector narrowing
   fixed-point clip directly.  */

void
expand_vec_double_ustrunc (rtx op_0, rtx op_1, machine_mode vec_mode)
{
  insn_code icode;
  rtx zero = CONST0_RTX (Xmode);
  enum unspec unspec = UNSPEC_VNCLIPU;
  rtx ops[] = {op_0, op_1, zero};

  icode = code_for_pred_narrow_clip_scalar (unspec, vec_mode);
  emit_vlmax_insn (icode, BINARY_OP_VXRM_RNU, ops);
}

/* Expand the standard name sstrunc<m><n>2 for double vector mode,  like
   DI => SI.  we can leverage the vector fixed point vector narrowing
   fixed-point clip directly.  */

void
expand_vec_double_sstrunc (rtx op_0, rtx op_1, machine_mode vec_mode)
{
  insn_code icode;
  rtx zero = CONST0_RTX (Xmode);
  enum unspec unspec = UNSPEC_VNCLIP;
  rtx ops[] = {op_0, op_1, zero};

  icode = code_for_pred_narrow_clip_scalar (unspec, vec_mode);
  emit_vlmax_insn (icode, BINARY_OP_VXRM_RNU, ops);
}

/* Expand the standard name ustrunc<m><n>2 for double vector mode,  like
   DI => HI.  we can leverage the vector fixed point vector narrowing
   fixed-point clip directly.  */

void
expand_vec_quad_ustrunc (rtx op_0, rtx op_1, machine_mode vec_mode,
			 machine_mode double_mode)
{
  rtx double_rtx = gen_reg_rtx (double_mode);

  expand_vec_double_ustrunc (double_rtx, op_1, vec_mode);
  expand_vec_double_ustrunc (op_0, double_rtx, double_mode);
}

/* Expand the standard name sstrunc<m><n>2 for quad vector mode,  like
   DI => HI.  we can leverage the vector fixed point vector narrowing
   fixed-point clip directly.  */

void
expand_vec_quad_sstrunc (rtx op_0, rtx op_1, machine_mode vec_mode,
			 machine_mode double_mode)
{
  rtx double_rtx = gen_reg_rtx (double_mode);

  expand_vec_double_sstrunc (double_rtx, op_1, vec_mode);
  expand_vec_double_sstrunc (op_0, double_rtx, double_mode);
}

/* Expand the standard name ustrunc<m><n>2 for double vector mode,  like
   DI => QI.  we can leverage the vector fixed point vector narrowing
   fixed-point clip directly.  */

void
expand_vec_oct_ustrunc (rtx op_0, rtx op_1, machine_mode vec_mode,
			machine_mode double_mode, machine_mode quad_mode)
{
  rtx double_rtx = gen_reg_rtx (double_mode);
  rtx quad_rtx = gen_reg_rtx (quad_mode);

  expand_vec_double_ustrunc (double_rtx, op_1, vec_mode);
  expand_vec_double_ustrunc (quad_rtx, double_rtx, double_mode);
  expand_vec_double_ustrunc (op_0, quad_rtx, quad_mode);
}

/* Expand the standard name sstrunc<m><n>2 for oct vector mode,  like
   DI => QI.  we can leverage the vector fixed point vector narrowing
   fixed-point clip directly.  */

void
expand_vec_oct_sstrunc (rtx op_0, rtx op_1, machine_mode vec_mode,
			machine_mode double_mode, machine_mode quad_mode)
{
  rtx double_rtx = gen_reg_rtx (double_mode);
  rtx quad_rtx = gen_reg_rtx (quad_mode);

  expand_vec_double_sstrunc (double_rtx, op_1, vec_mode);
  expand_vec_double_sstrunc (quad_rtx, double_rtx, double_mode);
  expand_vec_double_sstrunc (op_0, quad_rtx, quad_mode);
}

/* Vectorize popcount by the Wilkes-Wheeler-Gill algorithm that libgcc uses as
   well.  */
void
expand_popcount (rtx *ops)
{
  rtx dst = ops[0];
  rtx src = ops[1];
  machine_mode mode = GET_MODE (dst);
  scalar_mode imode = GET_MODE_INNER (mode);
  static const uint64_t m5 = 0x5555555555555555ULL;
  static const uint64_t m3 = 0x3333333333333333ULL;
  static const uint64_t mf = 0x0F0F0F0F0F0F0F0FULL;
  static const uint64_t m1 = 0x0101010101010101ULL;

  rtx x1 = gen_reg_rtx (mode);
  rtx x2 = gen_reg_rtx (mode);
  rtx x3 = gen_reg_rtx (mode);
  rtx x4 = gen_reg_rtx (mode);

  /* x1 = src - (src >> 1) & 0x555...);  */
  rtx shift1 = expand_binop (mode, lshr_optab, src, GEN_INT (1), NULL, true,
			     OPTAB_DIRECT);

  rtx and1 = gen_reg_rtx (mode);
  rtx ops1[] = {and1, shift1, gen_int_mode (m5, imode)};
  emit_vlmax_insn (code_for_pred_scalar (AND, mode), riscv_vector::BINARY_OP,
		   ops1);

  x1 = expand_binop (mode, sub_optab, src, and1, NULL, true, OPTAB_DIRECT);

  /* x2 = (x1 & 0x3333333333333333ULL) + ((x1 >> 2) & 0x3333333333333333ULL);
   */
  rtx and2 = gen_reg_rtx (mode);
  rtx ops2[] = {and2, x1, gen_int_mode (m3, imode)};
  emit_vlmax_insn (code_for_pred_scalar (AND, mode), riscv_vector::BINARY_OP,
		   ops2);

  rtx shift2 = expand_binop (mode, lshr_optab, x1, GEN_INT (2), NULL, true,
			     OPTAB_DIRECT);

  rtx and22 = gen_reg_rtx (mode);
  rtx ops22[] = {and22, shift2, gen_int_mode (m3, imode)};
  emit_vlmax_insn (code_for_pred_scalar (AND, mode), riscv_vector::BINARY_OP,
		   ops22);

  x2 = expand_binop (mode, add_optab, and2, and22, NULL, true, OPTAB_DIRECT);

  /* x3 = (x2 + (x2 >> 4)) & 0x0f0f0f0f0f0f0f0fULL;  */
  rtx shift3 = expand_binop (mode, lshr_optab, x2, GEN_INT (4), NULL, true,
			     OPTAB_DIRECT);

  rtx plus3
    = expand_binop (mode, add_optab, x2, shift3, NULL, true, OPTAB_DIRECT);

  rtx ops3[] = {x3, plus3, gen_int_mode (mf, imode)};
  emit_vlmax_insn (code_for_pred_scalar (AND, mode), riscv_vector::BINARY_OP,
		   ops3);

  /* dest = (x3 * 0x0101010101010101ULL) >> 56;  */
  rtx mul4 = gen_reg_rtx (mode);
  rtx ops4[] = {mul4, x3, gen_int_mode (m1, imode)};
  emit_vlmax_insn (code_for_pred_scalar (MULT, mode), riscv_vector::BINARY_OP,
		   ops4);

  x4 = expand_binop (mode, lshr_optab, mul4,
		     GEN_INT (GET_MODE_BITSIZE (imode) - 8), NULL, true,
		     OPTAB_DIRECT);

  emit_move_insn (dst, x4);
}

/* Return true if it is VLMAX AVL TYPE.  */
bool
vlmax_avl_type_p (rtx_insn *rinsn)
{
  extract_insn_cached (rinsn);
  int index = get_attr_avl_type_idx (rinsn);
  if (index == INVALID_ATTRIBUTE)
    return false;

  gcc_assert (index < recog_data.n_operands);

  rtx avl_type = recog_data.operand[index];
  return INTVAL (avl_type) == VLMAX;
}

/* Return true if it is an RVV instruction depends on VL global
   status register.  */
bool
has_vl_op (rtx_insn *rinsn)
{
  return recog_memoized (rinsn) >= 0 && get_attr_has_vl_op (rinsn);
}

/* Get default tail policy.  */
static bool
get_default_ta ()
{
  /* For the instruction that doesn't require TA, we still need a default value
     to emit vsetvl. We pick up the default value according to prefer policy. */
  return (bool) (get_prefer_tail_policy () & 0x1
		 || (get_prefer_tail_policy () >> 1 & 0x1));
}

/* Helper function to get TA operand.  */
bool
tail_agnostic_p (rtx_insn *rinsn)
{
  /* If it doesn't have TA, we return agnostic by default.  */
  extract_insn_cached (rinsn);
  int ta = get_attr_ta (rinsn);
  return ta == INVALID_ATTRIBUTE ? get_default_ta () : IS_AGNOSTIC (ta);
}

/* Change insn and Assert the change always happens.  */
void
validate_change_or_fail (rtx object, rtx *loc, rtx new_rtx, bool in_group)
{
  bool change_p = validate_change (object, loc, new_rtx, in_group);
  gcc_assert (change_p);
}

/* Return true if it is NONVLMAX AVL TYPE.  */
bool
nonvlmax_avl_type_p (rtx_insn *rinsn)
{
  extract_insn_cached (rinsn);
  int index = get_attr_avl_type_idx (rinsn);
  if (index == INVALID_ATTRIBUTE)
    return false;

  gcc_assert (index < recog_data.n_operands);

  rtx avl_type = recog_data.operand[index];
  return INTVAL (avl_type) == NONVLMAX;
}

/* Return true if RTX is RVV VLMAX AVL.  */
bool
vlmax_avl_p (rtx x)
{
  return x && rtx_equal_p (x, RVV_VLMAX);
}

/* Helper function to get SEW operand. We always have SEW value for
   all RVV instructions that have VTYPE OP.  */
uint8_t
get_sew (rtx_insn *rinsn)
{
  return get_attr_sew (rinsn);
}

/* Helper function to get VLMUL operand. We always have VLMUL value for
   all RVV instructions that have VTYPE OP. */
enum vlmul_type
get_vlmul (rtx_insn *rinsn)
{
  return (enum vlmul_type) get_attr_vlmul (rinsn);
}

/* Count the number of REGNO in RINSN.  */
int
count_regno_occurrences (rtx_insn *rinsn, unsigned int regno)
{
  int count = 0;
  extract_insn (rinsn);
  for (int i = 0; i < recog_data.n_operands; i++)
    if (refers_to_regno_p (regno, recog_data.operand[i]))
      count++;
  return count;
}

/* Return true if the OP can be directly broadcasted.  */
bool
can_be_broadcasted_p (rtx op)
{
  machine_mode mode = GET_MODE (op);
  /* We don't allow RA (register allocation) reload generate
    (vec_duplicate:DI reg) in RV32 system wheras we allow
    (vec_duplicate:DI mem) in RV32 system.  */
  if (!can_create_pseudo_p () && !FLOAT_MODE_P (mode)
      && maybe_gt (GET_MODE_SIZE (mode), GET_MODE_SIZE (Pmode))
      && !satisfies_constraint_Wdm (op))
    return false;

  if (satisfies_constraint_K (op) || register_operand (op, mode)
      || satisfies_constraint_Wdm (op) || rtx_equal_p (op, CONST0_RTX (mode)))
    return true;

  return can_create_pseudo_p () && nonmemory_operand (op, mode);
}

void
emit_vec_extract (rtx target, rtx src, rtx index)
{
  machine_mode vmode = GET_MODE (src);
  machine_mode smode = GET_MODE (target);
  class expand_operand ops[3];
  enum insn_code icode
    = convert_optab_handler (vec_extract_optab, vmode, smode);
  gcc_assert (icode != CODE_FOR_nothing);
  create_output_operand (&ops[0], target, smode);
  ops[0].target = 1;
  create_input_operand (&ops[1], src, vmode);

  poly_int64 val;
  if (poly_int_rtx_p (index, &val))
    create_integer_operand (&ops[2], val);
  else
    create_input_operand (&ops[2], index, Pmode);

  expand_insn (icode, 3, ops);
  if (ops[0].value != target)
    emit_move_insn (target, ops[0].value);
}

/* Return true if the offset mode is valid mode that we use for gather/scatter
   autovectorization.  */
bool
gather_scatter_valid_offset_p (machine_mode mode)
{
  /* If the element size of offset mode is already >= Pmode size,
     we don't need any extensions.  */
  if (known_ge (GET_MODE_SIZE (GET_MODE_INNER (mode)), UNITS_PER_WORD))
    return true;

  /* Since we are very likely extend the offset mode into vector Pmode,
     Disable gather/scatter autovectorization if we can't extend the offset
     mode into vector Pmode.  */
  if (!get_vector_mode (Pmode, GET_MODE_NUNITS (mode)).exists ())
    return false;
  return true;
}

/* Implement TARGET_ESTIMATED_POLY_VALUE.
   Look into the tuning structure for an estimate.
   KIND specifies the type of requested estimate: min, max or likely.
   For cores with a known VLA width all three estimates are the same.
   For generic VLA tuning we want to distinguish the maximum estimate from
   the minimum and likely ones.
   The likely estimate is the same as the minimum in that case to give a
   conservative behavior of auto-vectorizing with VLA when it is a win
   even for VLA vectorization.
   When VLA width information is available VAL.coeffs[1] is multiplied by
   the number of VLA chunks over the initial VLS bits.  */
HOST_WIDE_INT
estimated_poly_value (poly_int64 val, unsigned int kind)
{
  unsigned int width_source
    = BITS_PER_RISCV_VECTOR.is_constant ()
	? (unsigned int) BITS_PER_RISCV_VECTOR.to_constant ()
	: (unsigned int) RVV_VECTOR_BITS_SCALABLE;

  /* If there is no core-specific information then the minimum and likely
     values are based on TARGET_MIN_VLEN vectors and the maximum is based on
     the architectural maximum of 65536 bits.  */
  unsigned int min_vlen_bytes = TARGET_MIN_VLEN / 8 - 1;
  if (width_source == RVV_VECTOR_BITS_SCALABLE)
    switch (kind)
      {
      case POLY_VALUE_MIN:
      case POLY_VALUE_LIKELY:
	return val.coeffs[0];

      case POLY_VALUE_MAX:
	return val.coeffs[0] + val.coeffs[1] * min_vlen_bytes;
      }

  /* Allow BITS_PER_RISCV_VECTOR to be a bitmask of different VL, treating the
     lowest as likely.  This could be made more general if future -mtune
     options need it to be.  */
  if (kind == POLY_VALUE_MAX)
    width_source = 1 << floor_log2 (width_source);
  else
    width_source = least_bit_hwi (width_source);

  /* If the core provides width information, use that.  */
  HOST_WIDE_INT over_min_vlen = width_source - TARGET_MIN_VLEN;
  return val.coeffs[0] + val.coeffs[1] * over_min_vlen / TARGET_MIN_VLEN;
}

/* Return true it is whole register-register move.  */
bool
whole_reg_to_reg_move_p (rtx *ops, machine_mode mode, int avl_type_index)
{
  /* An operation is a whole-register move if either
     (1) Its vlmax operand equals VLMAX
     (2) Its vl operand equals the number of units of its mode.  */
  if (register_operand (ops[0], mode)
      && register_operand (ops[3], mode)
      && satisfies_constraint_vu (ops[2])
      && satisfies_constraint_Wc1 (ops[1]))
    {
      if (INTVAL (ops[avl_type_index]) == VLMAX)
	return true;
      /* AVL propagation PASS will transform FIXED-VLMAX with NUNITS < 32
	 into NON-VLMAX with LEN = NUNITS.  */
      else if (CONST_INT_P (ops[4])
	       && known_eq (INTVAL (ops[4]), GET_MODE_NUNITS (mode)))
	return true;
    }
  return false;
}

/* Return true if we can transform vmv.v.x/vfmv.v.f to vmv.s.x/vfmv.s.f.  */
bool
splat_to_scalar_move_p (rtx *ops)
{
  return satisfies_constraint_Wc1 (ops[1])
	 && satisfies_constraint_vu (ops[2])
	 && !MEM_P (ops[3])
	 && satisfies_constraint_k01 (ops[4])
	 && INTVAL (ops[7]) == NONVLMAX
	 && known_ge (GET_MODE_SIZE (Pmode), GET_MODE_SIZE (GET_MODE (ops[3])));
}

} // namespace riscv_vector
