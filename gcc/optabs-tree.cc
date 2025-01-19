/* Tree-based target query functions relating to optabs
   Copyright (C) 1987-2025 Free Software Foundation, Inc.

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
#include "internal-fn.h"

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

    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
      /* {s,u}mod_optab implements TRUNC_MOD_EXPR.  For scalar modes,
	 expansion has code to adjust TRUNC_MOD_EXPR into the desired other
	 modes, but for vector modes it does not.  The adjustment code
	 should be instead emitted in tree-vect-patterns.cc.  */
      if (VECTOR_TYPE_P (type))
	return unknown_optab;
      /* FALLTHRU */
    case TRUNC_MOD_EXPR:
      return TYPE_UNSIGNED (type) ? umod_optab : smod_optab;

    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
      /* {,u}{s,u}div_optab implements {TRUNC,EXACT}_DIV_EXPR or RDIV_EXPR.
	 For scalar modes, expansion has code to adjust TRUNC_DIV_EXPR
	 into the desired other modes, but for vector modes it does not.
	 The adjustment code should be instead emitted in
	 tree-vect-patterns.cc.  */
      if (VECTOR_TYPE_P (type))
	return unknown_optab;
      /* FALLTHRU */
    case RDIV_EXPR:
    case TRUNC_DIV_EXPR:
    case EXACT_DIV_EXPR:
      if (TYPE_SATURATING (type))
	return TYPE_UNSIGNED (type) ? usdiv_optab : ssdiv_optab;
      return TYPE_UNSIGNED (type) ? udiv_optab : sdiv_optab;

    case LSHIFT_EXPR:
      if (VECTOR_TYPE_P (type))
	{
	  if (subtype == optab_vector)
	    return TYPE_SATURATING (type) ? unknown_optab : vashl_optab;

	  gcc_assert (subtype == optab_scalar);
	}
      if (TYPE_SATURATING (type))
	return TYPE_UNSIGNED (type) ? usashl_optab : ssashl_optab;
      return ashl_optab;

    case RSHIFT_EXPR:
      if (VECTOR_TYPE_P (type))
	{
	  if (subtype == optab_vector)
	    return TYPE_UNSIGNED (type) ? vlshr_optab : vashr_optab;

	  gcc_assert (subtype == optab_scalar);
	}
      return TYPE_UNSIGNED (type) ? lshr_optab : ashr_optab;

    case LROTATE_EXPR:
      if (VECTOR_TYPE_P (type))
	{
	  if (subtype == optab_vector)
	    return vrotl_optab;

	  gcc_assert (subtype == optab_scalar);
	}
      return rotl_optab;

    case RROTATE_EXPR:
      if (VECTOR_TYPE_P (type))
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

    case POINTER_PLUS_EXPR:
      return add_optab;

    case POINTER_DIFF_EXPR:
      return sub_optab;

    case REALIGN_LOAD_EXPR:
      return vec_realign_load_optab;

    case WIDEN_SUM_EXPR:
      return TYPE_UNSIGNED (type) ? usum_widen_optab : ssum_widen_optab;

    case DOT_PROD_EXPR:
      {
	if (subtype == optab_vector_mixed_sign)
	  return usdot_prod_optab;

	return (TYPE_UNSIGNED (type) ? udot_prod_optab : sdot_prod_optab);
      }

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
    case PLUS_EXPR:
      if (TYPE_SATURATING (type))
	return TYPE_UNSIGNED (type) ? usadd_optab : ssadd_optab;
      return trapv ? addv_optab : add_optab;

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

/* Check whether an operation represented by CODE is a 'half' widening operation
   in which the input vector type has half the number of bits of the output
   vector type e.g. V8QI->V8HI.

   This is handled by widening the inputs using NOP_EXPRs then using a
   non-widening stmt e.g. MINUS_EXPR.  RTL fusing converts these to the widening
   hardware instructions if supported.

   The more typical case (handled in supportable_widening_operation) is where
   the input vector type has the same number of bits as the output vector type.
   In this case half the elements of the input vectors must be processed at a
   time into respective vector outputs with elements twice as wide i.e. a
   'hi'/'lo' pair using codes such as VEC_WIDEN_MINUS_HI/LO.

   Supported widening operations:
    WIDEN_MULT_EXPR
    WIDEN_LSHIFT_EXPR

   Output:
   - CODE1 - The non-widened code, which will be used after the inputs are
     converted to the wide type.  */
bool
supportable_half_widening_operation (enum tree_code code, tree vectype_out,
				     tree vectype_in, enum tree_code *code1)
{
  machine_mode m1,m2;
  enum tree_code dummy_code;
  optab op;

  gcc_assert (VECTOR_TYPE_P (vectype_out) && VECTOR_TYPE_P (vectype_in));

  m1 = TYPE_MODE (vectype_out);
  m2 = TYPE_MODE (vectype_in);

  if (!VECTOR_MODE_P (m1) || !VECTOR_MODE_P (m2))
    return false;

  if (maybe_ne (TYPE_VECTOR_SUBPARTS (vectype_in),
		  TYPE_VECTOR_SUBPARTS (vectype_out)))
    return false;

  switch (code)
    {
    case WIDEN_LSHIFT_EXPR:
      *code1 = LSHIFT_EXPR;
      break;
    case WIDEN_MULT_EXPR:
      *code1 = MULT_EXPR;
      break;
    default:
      return false;
    }

  if (!supportable_convert_operation (NOP_EXPR, vectype_out, vectype_in,
				     &dummy_code))
    return false;

  op = optab_for_tree_code (*code1, vectype_out, optab_vector);
  return (optab_handler (op, TYPE_MODE (vectype_out)) != CODE_FOR_nothing);
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

/* Return true iff vec_cmp_optab/vec_cmpu_optab can handle a vector comparison
   for code CODE, comparing operands of type VALUE_TYPE and producing a result
   of type MASK_TYPE.  */

static bool
vec_cmp_icode_p (tree value_type, tree mask_type, enum tree_code code)
{
  enum rtx_code rcode = get_rtx_code_1 (code, TYPE_UNSIGNED (value_type));
  if (rcode == UNKNOWN)
    return false;

  return can_vec_cmp_compare_p (rcode, TYPE_MODE (value_type),
				TYPE_MODE (mask_type));
}

/* Return true iff vec_cmpeq_optab can handle a vector comparison for code
   CODE, comparing operands of type VALUE_TYPE and producing a result of type
   MASK_TYPE.  */

static bool
vec_cmp_eq_icode_p (tree value_type, tree mask_type, enum tree_code code)
{
  if (code != EQ_EXPR && code != NE_EXPR)
    return false;

  return get_vec_cmp_eq_icode (TYPE_MODE (value_type), TYPE_MODE (mask_type))
	 != CODE_FOR_nothing;
}

/* Return TRUE if appropriate vector insn is available
   for vector comparison expr with vector type VALUE_TYPE
   and resulting mask with MASK_TYPE.  */

bool
expand_vec_cmp_expr_p (tree value_type, tree mask_type, enum tree_code code)
{
  return vec_cmp_icode_p (value_type, mask_type, code)
	 || vec_cmp_eq_icode_p (value_type, mask_type, code);
}

/* Return TRUE iff, appropriate vector insns are available
   for vector cond expr with vector type VALUE_TYPE and a comparison
   with operand vector types in CMP_OP_TYPE.  */

bool
expand_vec_cond_expr_p (tree value_type, tree cmp_op_type)
{
  if (VECTOR_BOOLEAN_TYPE_P (cmp_op_type)
      && get_vcond_mask_icode (TYPE_MODE (value_type),
			       TYPE_MODE (cmp_op_type)) != CODE_FOR_nothing)
    return true;

  return false;
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
  return ot != unknown_optab && can_implement_p (ot, TYPE_MODE (type));
}

/* Return true if the target has support for masked load/store.
   We can support masked load/store by either mask{load,store}
   or mask_len_{load,store}.
   This helper function checks whether target supports masked
   load/store and return corresponding IFN in the last argument
   (IFN_MASK_{LOAD,STORE} or IFN_MASK_LEN_{LOAD,STORE}).
   If there is support and ELSVALS is nonzero store the possible else values
   in the vector it points to.  */

bool
target_supports_mask_load_store_p (machine_mode mode, machine_mode mask_mode,
				   bool is_load, internal_fn *ifn,
				   vec<int> *elsvals)
{
  optab op = is_load ? maskload_optab : maskstore_optab;
  optab len_op = is_load ? mask_len_load_optab : mask_len_store_optab;
  enum insn_code icode;
  if ((icode = convert_optab_handler (op, mode, mask_mode))
      != CODE_FOR_nothing)
    {
      if (ifn)
	*ifn = is_load ? IFN_MASK_LOAD : IFN_MASK_STORE;
      if (elsvals && is_load)
	get_supported_else_vals (icode,
				 internal_fn_else_index (IFN_MASK_LOAD),
				 *elsvals);
      return true;
    }
  else if ((icode = convert_optab_handler (len_op, mode, mask_mode))
	   != CODE_FOR_nothing)
    {
      if (ifn)
	*ifn = is_load ? IFN_MASK_LEN_LOAD : IFN_MASK_LEN_STORE;
      if (elsvals && is_load)
	get_supported_else_vals (icode,
				 internal_fn_else_index (IFN_MASK_LEN_LOAD),
				 *elsvals);
      return true;
    }
  return false;
}

/* Return true if target supports vector masked load/store for mode.
   An additional output in the last argument which is the IFN pointer.
   We set IFN as MASK_{LOAD,STORE} or MASK_LEN_{LOAD,STORE} according
   which optab is supported in the target.
   If there is support and ELSVALS is nonzero store the possible else values
   in the vector it points to.  */

bool
can_vec_mask_load_store_p (machine_mode mode,
			   machine_mode mask_mode,
			   bool is_load,
			   internal_fn *ifn,
			   vec<int> *elsvals)
{
  machine_mode vmode;

  /* If mode is vector mode, check it directly.  */
  if (VECTOR_MODE_P (mode))
    return target_supports_mask_load_store_p (mode, mask_mode, is_load, ifn,
					      elsvals);

  /* Otherwise, return true if there is some vector mode with
     the mask load/store supported.  */

  /* See if there is any chance the mask load or store might be
     vectorized.  If not, punt.  */
  scalar_mode smode;
  if (!is_a <scalar_mode> (mode, &smode))
    return false;

  vmode = targetm.vectorize.preferred_simd_mode (smode);
  if (VECTOR_MODE_P (vmode)
      && targetm.vectorize.get_mask_mode (vmode).exists (&mask_mode)
      && target_supports_mask_load_store_p (vmode, mask_mode, is_load, ifn,
					    elsvals))
    return true;

  auto_vector_modes vector_modes;
  targetm.vectorize.autovectorize_vector_modes (&vector_modes, true);
  for (machine_mode base_mode : vector_modes)
    if (related_vector_mode (base_mode, smode).exists (&vmode)
	&& targetm.vectorize.get_mask_mode (vmode).exists (&mask_mode)
	&& target_supports_mask_load_store_p (vmode, mask_mode, is_load, ifn,
					      elsvals))
      return true;
  return false;
}

/* Return true if the target has support for len load/store.
   We can support len load/store by either len_{load,store}
   or mask_len_{load,store}.
   This helper function checks whether target supports len
   load/store and return corresponding IFN in the last argument
   (IFN_LEN_{LOAD,STORE} or IFN_MASK_LEN_{LOAD,STORE}).
   If there is support and ELSVALS is nonzero store thepossible
   else values in the vector it points to.  */

static bool
target_supports_len_load_store_p (machine_mode mode, bool is_load,
				  internal_fn *ifn, vec<int> *elsvals)
{
  optab op = is_load ? len_load_optab : len_store_optab;
  optab masked_op = is_load ? mask_len_load_optab : mask_len_store_optab;

  if (direct_optab_handler (op, mode))
    {
      if (ifn)
	*ifn = is_load ? IFN_LEN_LOAD : IFN_LEN_STORE;
      return true;
    }
  machine_mode mask_mode;
  enum insn_code icode;
  if (targetm.vectorize.get_mask_mode (mode).exists (&mask_mode)
      && ((icode = convert_optab_handler (masked_op, mode, mask_mode))
	  != CODE_FOR_nothing))
    {
      if (ifn)
	*ifn = is_load ? IFN_MASK_LEN_LOAD : IFN_MASK_LEN_STORE;
      if (elsvals && is_load)
	get_supported_else_vals (icode,
				 internal_fn_else_index (IFN_MASK_LEN_LOAD),
				 *elsvals);
      return true;
    }
  return false;
}

/* If target supports vector load/store with length for vector mode MODE,
   return the corresponding vector mode, otherwise return opt_machine_mode ().
   There are two flavors for vector load/store with length, one is to measure
   length with bytes, the other is to measure length with lanes.
   As len_{load,store} optabs point out, for the flavor with bytes, we use
   VnQI to wrap the other supportable same size vector modes.
   An additional output in the last argument which is the IFN pointer.
   We set IFN as LEN_{LOAD,STORE} or MASK_LEN_{LOAD,STORE} according
   which optab is supported in the target.
   If there is support and ELSVALS is nonzero store the possible else values
   in the vector it points to.  */

opt_machine_mode
get_len_load_store_mode (machine_mode mode, bool is_load, internal_fn *ifn,
			 vec<int> *elsvals)
{
  gcc_assert (VECTOR_MODE_P (mode));

  /* Check if length in lanes supported for this mode directly.  */
  if (target_supports_len_load_store_p (mode, is_load, ifn, elsvals))
    return mode;

  /* Check if length in bytes supported for same vector size VnQI.  */
  machine_mode vmode;
  poly_uint64 nunits = GET_MODE_SIZE (mode);
  if (related_vector_mode (mode, QImode, nunits).exists (&vmode)
      && target_supports_len_load_store_p (vmode, is_load, ifn, elsvals))
    return vmode;

  return opt_machine_mode ();
}
