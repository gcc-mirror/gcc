/* Tree-based target query functions relating to optabs
   Copyright (C) 1987-2020 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "insn-codes.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "optabs.h"
#include "optabs-tree.h"
#include "stor-layout.h"

/* Return the optab used for computing the operation given by the tree code,
   CODE and the tree EXP.  This function is not always usable (for example, it
   cannot give complete results for multiplication or division) but probably
   ought to be relied on more widely throughout the expander.  */
optab
optab_for_tree_code (enum tree_code code, const_tree type,
		     enum optab_subtype subtype)
{
  bool trapv;
  switch (code)
    {
    case BIT_AND_EXPR:
      return and_optab;

    case BIT_IOR_EXPR:
      return ior_optab;

    case BIT_NOT_EXPR:
      return one_cmpl_optab;

    case BIT_XOR_EXPR:
      return xor_optab;

    case MULT_HIGHPART_EXPR:
      return TYPE_UNSIGNED (type) ? umul_highpart_optab : smul_highpart_optab;

    case TRUNC_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
      return TYPE_UNSIGNED (type) ? umod_optab : smod_optab;

    case RDIV_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case EXACT_DIV_EXPR:
      if (TYPE_SATURATING (type))
	return TYPE_UNSIGNED (type) ? usdiv_optab : ssdiv_optab;
      return TYPE_UNSIGNED (type) ? udiv_optab : sdiv_optab;

    case LSHIFT_EXPR:
      if (TREE_CODE (type) == VECTOR_TYPE)
	{
	  if (subtype == optab_vector)
	    return TYPE_SATURATING (type) ? unknown_optab : vashl_optab;

	  gcc_assert (subtype == optab_scalar);
	}
      if (TYPE_SATURATING (type))
	return TYPE_UNSIGNED (type) ? usashl_optab : ssashl_optab;
      return ashl_optab;

    case RSHIFT_EXPR:
      if (TREE_CODE (type) == VECTOR_TYPE)
	{
	  if (subtype == optab_vector)
	    return TYPE_UNSIGNED (type) ? vlshr_optab : vashr_optab;

	  gcc_assert (subtype == optab_scalar);
	}
      return TYPE_UNSIGNED (type) ? lshr_optab : ashr_optab;

    case LROTATE_EXPR:
      if (TREE_CODE (type) == VECTOR_TYPE)
	{
	  if (subtype == optab_vector)
	    return vrotl_optab;

	  gcc_assert (subtype == optab_scalar);
	}
      return rotl_optab;

    case RROTATE_EXPR:
      if (TREE_CODE (type) == VECTOR_TYPE)
	{
	  if (subtype == optab_vector)
	    return vrotr_optab;

	  gcc_assert (subtype == optab_scalar);
	}
      return rotr_optab;

    case MAX_EXPR:
      return TYPE_UNSIGNED (type) ? umax_optab : smax_optab;

    case MIN_EXPR:
      return TYPE_UNSIGNED (type) ? umin_optab : smin_optab;

    case REALIGN_LOAD_EXPR:
      return vec_realign_load_optab;

    case WIDEN_SUM_EXPR:
      return TYPE_UNSIGNED (type) ? usum_widen_optab : ssum_widen_optab;

    case DOT_PROD_EXPR:
      return TYPE_UNSIGNED (type) ? udot_prod_optab : sdot_prod_optab;

    case SAD_EXPR:
      return TYPE_UNSIGNED (type) ? usad_optab : ssad_optab;

    case WIDEN_MULT_PLUS_EXPR:
      return (TYPE_UNSIGNED (type)
	      ? (TYPE_SATURATING (type)
		 ? usmadd_widen_optab : umadd_widen_optab)
	      : (TYPE_SATURATING (type)
		 ? ssmadd_widen_optab : smadd_widen_optab));

    case WIDEN_MULT_MINUS_EXPR:
      return (TYPE_UNSIGNED (type)
	      ? (TYPE_SATURATING (type)
		 ? usmsub_widen_optab : umsub_widen_optab)
	      : (TYPE_SATURATING (type)
		 ? ssmsub_widen_optab : smsub_widen_optab));

    case VEC_WIDEN_MULT_HI_EXPR:
      return (TYPE_UNSIGNED (type)
	      ? vec_widen_umult_hi_optab : vec_widen_smult_hi_optab);

    case VEC_WIDEN_MULT_LO_EXPR:
      return (TYPE_UNSIGNED (type)
	      ? vec_widen_umult_lo_optab : vec_widen_smult_lo_optab);

    case VEC_WIDEN_MULT_EVEN_EXPR:
      return (TYPE_UNSIGNED (type)
	      ? vec_widen_umult_even_optab : vec_widen_smult_even_optab);

    case VEC_WIDEN_MULT_ODD_EXPR:
      return (TYPE_UNSIGNED (type)
	      ? vec_widen_umult_odd_optab : vec_widen_smult_odd_optab);

    case VEC_WIDEN_LSHIFT_HI_EXPR:
      return (TYPE_UNSIGNED (type)
	      ? vec_widen_ushiftl_hi_optab : vec_widen_sshiftl_hi_optab);

    case VEC_WIDEN_LSHIFT_LO_EXPR:
      return (TYPE_UNSIGNED (type)
	      ? vec_widen_ushiftl_lo_optab : vec_widen_sshiftl_lo_optab);

    case VEC_UNPACK_HI_EXPR:
      return (TYPE_UNSIGNED (type)
	      ? vec_unpacku_hi_optab : vec_unpacks_hi_optab);

    case VEC_UNPACK_LO_EXPR:
      return (TYPE_UNSIGNED (type)
	      ? vec_unpacku_lo_optab : vec_unpacks_lo_optab);

    case VEC_UNPACK_FLOAT_HI_EXPR:
      /* The signedness is determined from input operand.  */
      return (TYPE_UNSIGNED (type)
	      ? vec_unpacku_float_hi_optab : vec_unpacks_float_hi_optab);

    case VEC_UNPACK_FLOAT_LO_EXPR:
      /* The signedness is determined from input operand.  */
      return (TYPE_UNSIGNED (type)
	      ? vec_unpacku_float_lo_optab : vec_unpacks_float_lo_optab);

    case VEC_UNPACK_FIX_TRUNC_HI_EXPR:
      /* The signedness is determined from output operand.  */
      return (TYPE_UNSIGNED (type)
	      ? vec_unpack_ufix_trunc_hi_optab
	      : vec_unpack_sfix_trunc_hi_optab);

    case VEC_UNPACK_FIX_TRUNC_LO_EXPR:
      /* The signedness is determined from output operand.  */
      return (TYPE_UNSIGNED (type)
	      ? vec_unpack_ufix_trunc_lo_optab
	      : vec_unpack_sfix_trunc_lo_optab);

    case VEC_PACK_TRUNC_EXPR:
      return vec_pack_trunc_optab;

    case VEC_PACK_SAT_EXPR:
      return TYPE_UNSIGNED (type) ? vec_pack_usat_optab : vec_pack_ssat_optab;

    case VEC_PACK_FIX_TRUNC_EXPR:
      /* The signedness is determined from output operand.  */
      return (TYPE_UNSIGNED (type)
	      ? vec_pack_ufix_trunc_optab : vec_pack_sfix_trunc_optab);

    case VEC_PACK_FLOAT_EXPR:
      /* The signedness is determined from input operand.  */
      return (TYPE_UNSIGNED (type)
	      ? vec_packu_float_optab : vec_packs_float_optab);

    case VEC_DUPLICATE_EXPR:
      return vec_duplicate_optab;

    case VEC_SERIES_EXPR:
      return vec_series_optab;

    default:
      break;
    }

  trapv = INTEGRAL_TYPE_P (type) && TYPE_OVERFLOW_TRAPS (type);
  switch (code)
    {
    case POINTER_PLUS_EXPR:
    case PLUS_EXPR:
      if (TYPE_SATURATING (type))
	return TYPE_UNSIGNED (type) ? usadd_optab : ssadd_optab;
      return trapv ? addv_optab : add_optab;

    case POINTER_DIFF_EXPR:
    case MINUS_EXPR:
      if (TYPE_SATURATING (type))
	return TYPE_UNSIGNED (type) ? ussub_optab : sssub_optab;
      return trapv ? subv_optab : sub_optab;

    case MULT_EXPR:
      if (TYPE_SATURATING (type))
	return TYPE_UNSIGNED (type) ? usmul_optab : ssmul_optab;
      return trapv ? smulv_optab : smul_optab;

    case NEGATE_EXPR:
      if (TYPE_SATURATING (type))
	return TYPE_UNSIGNED (type) ? usneg_optab : ssneg_optab;
      return trapv ? negv_optab : neg_optab;

    case ABS_EXPR:
      return trapv ? absv_optab : abs_optab;

    case ABSU_EXPR:
      return abs_optab;
    default:
      return unknown_optab;
    }
}

/* Function supportable_convert_operation

   Check whether an operation represented by the code CODE is a
   convert operation that is supported by the target platform in
   vector form (i.e., when operating on arguments of type VECTYPE_IN
   producing a result of type VECTYPE_OUT).

   Convert operations we currently support directly are FIX_TRUNC and FLOAT.
   This function checks if these operations are supported
   by the target platform directly (via vector tree-codes).

   Output:
   - CODE1 is code of vector operation to be used when
   vectorizing the operation, if available.  */

bool
supportable_convert_operation (enum tree_code code,
			       tree vectype_out, tree vectype_in,
			       enum tree_code *code1)
{
  machine_mode m1,m2;
  bool truncp;

  gcc_assert (VECTOR_TYPE_P (vectype_out) && VECTOR_TYPE_P (vectype_in));

  m1 = TYPE_MODE (vectype_out);
  m2 = TYPE_MODE (vectype_in);

  if (!VECTOR_MODE_P (m1) || !VECTOR_MODE_P (m2))
    return false;

  /* First check if we can done conversion directly.  */
  if ((code == FIX_TRUNC_EXPR
       && can_fix_p (m1,m2,TYPE_UNSIGNED (vectype_out), &truncp)
	  != CODE_FOR_nothing)
      || (code == FLOAT_EXPR
	  && can_float_p (m1,m2,TYPE_UNSIGNED (vectype_in))
	     != CODE_FOR_nothing))
    {
      *code1 = code;
      return true;
    }

  if (GET_MODE_UNIT_PRECISION (m1) > GET_MODE_UNIT_PRECISION (m2)
      && can_extend_p (m1, m2, TYPE_UNSIGNED (vectype_in)))
    {
      *code1 = code;
      return true;
    }

  if (GET_MODE_UNIT_PRECISION (m1) < GET_MODE_UNIT_PRECISION (m2)
      && convert_optab_handler (trunc_optab, m1, m2) != CODE_FOR_nothing)
    {
      *code1 = code;
      return true;
    }

  return false;
}

/* Return TRUE if appropriate vector insn is available
   for vector comparison expr with vector type VALUE_TYPE
   and resulting mask with MASK_TYPE.  */

bool
expand_vec_cmp_expr_p (tree value_type, tree mask_type, enum tree_code code)
{
  if (get_vec_cmp_icode (TYPE_MODE (value_type), TYPE_MODE (mask_type),
			 TYPE_UNSIGNED (value_type)) != CODE_FOR_nothing)
    return true;
  if ((code == EQ_EXPR || code == NE_EXPR)
      && (get_vec_cmp_eq_icode (TYPE_MODE (value_type), TYPE_MODE (mask_type))
	  != CODE_FOR_nothing))
    return true;
  return false;
}

/* Return true iff vcond_optab/vcondu_optab can handle a vector
   comparison for code CODE, comparing operands of type CMP_OP_TYPE and
   producing a result of type VALUE_TYPE.  */

static bool
vcond_icode_p (tree value_type, tree cmp_op_type, enum tree_code code)
{
  return can_vcond_compare_p (get_rtx_code (code, TYPE_UNSIGNED (cmp_op_type)),
			      TYPE_MODE (value_type), TYPE_MODE (cmp_op_type));
}

/* Return true iff vcondeq_optab can handle a vector comparison for code CODE,
   comparing operands of type CMP_OP_TYPE and producing a result of type
   VALUE_TYPE.  */

static bool
vcond_eq_icode_p (tree value_type, tree cmp_op_type, enum tree_code code)
{
  if (code != EQ_EXPR && code != NE_EXPR)
    return false;

  return get_vcond_eq_icode (TYPE_MODE (value_type), TYPE_MODE (cmp_op_type))
	 != CODE_FOR_nothing;
}

/* Return TRUE iff, appropriate vector insns are available
   for vector cond expr with vector type VALUE_TYPE and a comparison
   with operand vector types in CMP_OP_TYPE.  */

bool
expand_vec_cond_expr_p (tree value_type, tree cmp_op_type, enum tree_code code)
{
  machine_mode value_mode = TYPE_MODE (value_type);
  machine_mode cmp_op_mode = TYPE_MODE (cmp_op_type);
  if (VECTOR_BOOLEAN_TYPE_P (cmp_op_type)
      && get_vcond_mask_icode (TYPE_MODE (value_type),
			       TYPE_MODE (cmp_op_type)) != CODE_FOR_nothing)
    return true;

  if (maybe_ne (GET_MODE_SIZE (value_mode), GET_MODE_SIZE (cmp_op_mode))
      || maybe_ne (GET_MODE_NUNITS (value_mode), GET_MODE_NUNITS (cmp_op_mode)))
    return false;

  if (TREE_CODE_CLASS (code) != tcc_comparison)
    /* This may happen, for example, if code == SSA_NAME, in which case we
       cannot be certain whether a vector insn is available.  */
    return false;

  return vcond_icode_p (value_type, cmp_op_type, code)
	 || vcond_eq_icode_p (value_type, cmp_op_type, code);
}

/* Use the current target and options to initialize
   TREE_OPTIMIZATION_OPTABS (OPTNODE).  */

void
init_tree_optimization_optabs (tree optnode)
{
  /* Quick exit if we have already computed optabs for this target.  */
  if (TREE_OPTIMIZATION_BASE_OPTABS (optnode) == this_target_optabs)
    return;

  /* Forget any previous information and set up for the current target.  */
  TREE_OPTIMIZATION_BASE_OPTABS (optnode) = this_target_optabs;
  struct target_optabs *tmp_optabs = (struct target_optabs *)
    TREE_OPTIMIZATION_OPTABS (optnode);
  if (tmp_optabs)
    memset (tmp_optabs, 0, sizeof (struct target_optabs));
  else
    tmp_optabs = ggc_cleared_alloc<target_optabs> ();

  /* Generate a new set of optabs into tmp_optabs.  */
  init_all_optabs (tmp_optabs);

  /* If the optabs changed, record it.  */
  if (memcmp (tmp_optabs, this_target_optabs, sizeof (struct target_optabs)))
    TREE_OPTIMIZATION_OPTABS (optnode) = tmp_optabs;
  else
    {
      TREE_OPTIMIZATION_OPTABS (optnode) = NULL;
      ggc_free (tmp_optabs);
    }
}

/* Return TRUE if the target has support for vector right shift of an
   operand of type TYPE.  If OT_TYPE is OPTAB_DEFAULT, check for existence
   of a shift by either a scalar or a vector.  Otherwise, check only
   for a shift that matches OT_TYPE.  */

bool
target_supports_op_p (tree type, enum tree_code code,
		      enum optab_subtype ot_subtype)
{
  optab ot = optab_for_tree_code (code, type, ot_subtype);
  return (ot != unknown_optab
	  && optab_handler (ot, TYPE_MODE (type)) != CODE_FOR_nothing);
}

