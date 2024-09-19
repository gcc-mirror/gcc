/* ACLE support for AArch64 SVE (__ARM_FEATURE_SVE intrinsics)
   Copyright (C) 2018-2024 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "memmodel.h"
#include "insn-codes.h"
#include "optabs.h"
#include "recog.h"
#include "expr.h"
#include "basic-block.h"
#include "function.h"
#include "fold-const.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimplify.h"
#include "explow.h"
#include "emit-rtl.h"
#include "tree-vector-builder.h"
#include "rtx-vector-builder.h"
#include "vec-perm-indices.h"
#include "aarch64-sve-builtins.h"
#include "aarch64-sve-builtins-shapes.h"
#include "aarch64-sve-builtins-base.h"
#include "aarch64-sve-builtins-functions.h"
#include "aarch64-builtins.h"
#include "ssa.h"
#include "gimple-fold.h"
#include "tree-ssa.h"

using namespace aarch64_sve;

namespace {

/* Return true if VAL is an undefined value.  */
static bool
is_undef (tree val)
{
  if (TREE_CODE (val) == SSA_NAME)
    {
      if (ssa_undefined_value_p (val, false))
	return true;

      gimple *def = SSA_NAME_DEF_STMT (val);
      if (gcall *call = dyn_cast<gcall *> (def))
	if (tree fndecl = gimple_call_fndecl (call))
	  if (const function_instance *instance = lookup_fndecl (fndecl))
	    if (instance->base == functions::svundef)
	      return true;
    }
  return false;
}

/* Return the UNSPEC_CMLA* unspec for rotation amount ROT.  */
static int
unspec_cmla (int rot)
{
  switch (rot)
    {
    case 0: return UNSPEC_CMLA;
    case 90: return UNSPEC_CMLA90;
    case 180: return UNSPEC_CMLA180;
    case 270: return UNSPEC_CMLA270;
    default: gcc_unreachable ();
    }
}

/* Return the UNSPEC_FCMLA* unspec for rotation amount ROT.  */
static int
unspec_fcmla (int rot)
{
  switch (rot)
    {
    case 0: return UNSPEC_FCMLA;
    case 90: return UNSPEC_FCMLA90;
    case 180: return UNSPEC_FCMLA180;
    case 270: return UNSPEC_FCMLA270;
    default: gcc_unreachable ();
    }
}

/* Return the UNSPEC_COND_FCMLA* unspec for rotation amount ROT.  */
static int
unspec_cond_fcmla (int rot)
{
  switch (rot)
    {
    case 0: return UNSPEC_COND_FCMLA;
    case 90: return UNSPEC_COND_FCMLA90;
    case 180: return UNSPEC_COND_FCMLA180;
    case 270: return UNSPEC_COND_FCMLA270;
    default: gcc_unreachable ();
    }
}

/* Expand a call to svmad, or svmla after reordering its operands.
   Make _m forms merge with argument MERGE_ARGNO.  */
static rtx
expand_mad (function_expander &e,
	    unsigned int merge_argno = DEFAULT_MERGE_ARGNO)
{
  if (e.pred == PRED_x)
    {
      insn_code icode;
      if (e.type_suffix (0).integer_p)
	icode = code_for_aarch64_pred_fma (e.vector_mode (0));
      else
	icode = code_for_aarch64_pred (UNSPEC_COND_FMLA, e.vector_mode (0));
      return e.use_pred_x_insn (icode);
    }

  insn_code icode = e.direct_optab_handler (cond_fma_optab);
  return e.use_cond_insn (icode, merge_argno);
}

/* Expand a call to svmla_lane or svmls_lane using floating-point unspec
   UNSPEC.  */
static rtx
expand_mla_mls_lane (function_expander &e, int unspec)
{
  /* Put the operands in the normal (fma ...) order, with the accumulator
     last.  This fits naturally since that's also the unprinted operand
     in the asm output.  */
  e.rotate_inputs_left (0, 4);
  insn_code icode = code_for_aarch64_lane (unspec, e.vector_mode (0));
  return e.use_exact_insn (icode);
}

/* Expand a call to svmsb, or svmls after reordering its operands.
   Make _m forms merge with argument MERGE_ARGNO.  */
static rtx
expand_msb (function_expander &e,
	    unsigned int merge_argno = DEFAULT_MERGE_ARGNO)
{
  if (e.pred == PRED_x)
    {
      insn_code icode;
      if (e.type_suffix (0).integer_p)
	icode = code_for_aarch64_pred_fnma (e.vector_mode (0));
      else
	icode = code_for_aarch64_pred (UNSPEC_COND_FMLS, e.vector_mode (0));
      return e.use_pred_x_insn (icode);
    }

  insn_code icode = e.direct_optab_handler (cond_fnma_optab);
  return e.use_cond_insn (icode, merge_argno);
}

class svabd_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    /* The integer operations are represented as the subtraction of the
       minimum from the maximum, with the signedness of the instruction
       keyed off the signedness of the maximum operation.  */
    rtx_code max_code = e.type_suffix (0).unsigned_p ? UMAX : SMAX;
    insn_code icode;
    if (e.pred == PRED_x)
      {
	if (e.type_suffix (0).integer_p)
	  icode = code_for_aarch64_pred_abd (max_code, e.vector_mode (0));
	else
	  icode = code_for_aarch64_pred_abd (e.vector_mode (0));
	return e.use_pred_x_insn (icode);
      }

    if (e.type_suffix (0).integer_p)
      icode = code_for_aarch64_cond_abd (max_code, e.vector_mode (0));
    else
      icode = code_for_aarch64_cond_abd (e.vector_mode (0));
    return e.use_cond_insn (icode);
  }
};

/* Implements svacge, svacgt, svacle and svaclt.  */
class svac_impl : public function_base
{
public:
  CONSTEXPR svac_impl (int unspec) : m_unspec (unspec) {}

  rtx
  expand (function_expander &e) const override
  {
    e.add_ptrue_hint (0, e.gp_mode (0));
    insn_code icode = code_for_aarch64_pred_fac (m_unspec, e.vector_mode (0));
    return e.use_exact_insn (icode);
  }

  /* The unspec code for the underlying comparison.  */
  int m_unspec;
};

class svadda_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    /* Put the predicate last, as required by mask_fold_left_plus_optab.  */
    e.rotate_inputs_left (0, 3);
    machine_mode mode = e.vector_mode (0);
    insn_code icode = direct_optab_handler (mask_fold_left_plus_optab, mode);
    return e.use_exact_insn (icode);
  }
};

/* Implements svadr[bhwd].  */
class svadr_bhwd_impl : public function_base
{
public:
  CONSTEXPR svadr_bhwd_impl (unsigned int shift) : m_shift (shift) {}

  rtx
  expand (function_expander &e) const override
  {
    machine_mode mode = GET_MODE (e.args[0]);
    if (m_shift == 0)
      return e.use_exact_insn (code_for_aarch64_adr (mode));

    /* Turn the access size into an extra shift argument.  */
    rtx shift = gen_int_mode (m_shift, GET_MODE_INNER (mode));
    e.args.quick_push (expand_vector_broadcast (mode, shift));
    return e.use_exact_insn (code_for_aarch64_adr_shift (mode));
  }

  /* How many bits left to shift the vector displacement.  */
  unsigned int m_shift;
};

class svbic_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    /* Convert svbic of a constant into svand of its inverse.  */
    if (CONST_INT_P (e.args[2]))
      {
	machine_mode mode = GET_MODE_INNER (e.vector_mode (0));
	e.args[2] = simplify_unary_operation (NOT, mode, e.args[2], mode);
	return e.map_to_rtx_codes (AND, AND, -1, -1);
      }

    if (e.type_suffix_ids[0] == TYPE_SUFFIX_b)
      {
	gcc_assert (e.pred == PRED_z);
	return e.use_exact_insn (CODE_FOR_aarch64_pred_bicvnx16bi_z);
      }

    if (e.pred == PRED_x)
      return e.use_unpred_insn (e.direct_optab_handler (andn_optab));

    return e.use_cond_insn (code_for_cond_bic (e.vector_mode (0)));
  }
};

/* Implements svbrkn, svbrkpa and svbrkpb.  */
class svbrk_binary_impl : public function_base
{
public:
  CONSTEXPR svbrk_binary_impl (int unspec) : m_unspec (unspec) {}

  rtx
  expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_aarch64_brk (m_unspec));
  }

  /* The unspec code associated with the operation.  */
  int m_unspec;
};

/* Implements svbrka and svbrkb.  */
class svbrk_unary_impl : public function_base
{
public:
  CONSTEXPR svbrk_unary_impl (int unspec) : m_unspec (unspec) {}

  rtx
  expand (function_expander &e) const override
  {
    return e.use_cond_insn (code_for_aarch64_brk (m_unspec));
  }

  /* The unspec code associated with the operation.  */
  int m_unspec;
};

class svcadd_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    /* Convert the rotation amount into a specific unspec.  */
    int rot = INTVAL (e.args.pop ());
    if (rot == 90)
      return e.map_to_unspecs (UNSPEC_CADD90, UNSPEC_CADD90,
			       UNSPEC_COND_FCADD90);
    if (rot == 270)
      return e.map_to_unspecs (UNSPEC_CADD270, UNSPEC_CADD270,
			       UNSPEC_COND_FCADD270);
    gcc_unreachable ();
  }
};

/* Implements svclasta and svclastb.  */
class svclast_impl : public quiet<function_base>
{
public:
  CONSTEXPR svclast_impl (int unspec) : m_unspec (unspec) {}

  rtx
  expand (function_expander &e) const override
  {
    /* Match the fold_extract_optab order.  */
    std::swap (e.args[0], e.args[1]);
    machine_mode mode = e.vector_mode (0);
    insn_code icode;
    if (e.mode_suffix_id == MODE_n)
      icode = code_for_fold_extract (m_unspec, mode);
    else
      icode = code_for_aarch64_fold_extract_vector (m_unspec, mode);
    return e.use_exact_insn (icode);
  }

  /* The unspec code associated with the operation.  */
  int m_unspec;
};

class svcmla_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    /* Convert the rotation amount into a specific unspec.  */
    int rot = INTVAL (e.args.pop ());
    if (e.type_suffix (0).float_p)
      {
	/* Make the operand order the same as the one used by the fma optabs,
	   with the accumulator last.  */
	e.rotate_inputs_left (1, 4);
	return e.map_to_unspecs (-1, -1, unspec_cond_fcmla (rot), 3);
      }
    else
      {
	int cmla = unspec_cmla (rot);
	return e.map_to_unspecs (cmla, cmla, -1);
      }
  }
};

class svcmla_lane_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    /* Convert the rotation amount into a specific unspec.  */
    int rot = INTVAL (e.args.pop ());
    machine_mode mode = e.vector_mode (0);
    if (e.type_suffix (0).float_p)
      {
	/* Make the operand order the same as the one used by the fma optabs,
	   with the accumulator last.  */
	e.rotate_inputs_left (0, 4);
	insn_code icode = code_for_aarch64_lane (unspec_fcmla (rot), mode);
	return e.use_exact_insn (icode);
      }
    else
      {
	insn_code icode = code_for_aarch64_lane (unspec_cmla (rot), mode);
	return e.use_exact_insn (icode);
      }
  }
};

/* Implements svcmp<cc> (except svcmpuo, which is handled separately).  */
class svcmp_impl : public function_base
{
public:
  CONSTEXPR svcmp_impl (tree_code code, int unspec_for_fp)
    : m_code (code), m_unspec_for_fp (unspec_for_fp) {}

  gimple *
  fold (gimple_folder &f) const override
  {
    tree pg = gimple_call_arg (f.call, 0);
    tree rhs1 = gimple_call_arg (f.call, 1);
    tree rhs2 = gimple_call_arg (f.call, 2);

    /* Convert a ptrue-predicated integer comparison into the corresponding
       gimple-level operation.  */
    if (integer_all_onesp (pg)
	&& f.type_suffix (0).element_bytes == 1
	&& f.type_suffix (0).integer_p)
      {
	gimple_seq stmts = NULL;
	rhs2 = f.force_vector (stmts, TREE_TYPE (rhs1), rhs2);
	gsi_insert_seq_before (f.gsi, stmts, GSI_SAME_STMT);
	return gimple_build_assign (f.lhs, m_code, rhs1, rhs2);
      }

    return NULL;
  }

  rtx
  expand (function_expander &e) const override
  {
    machine_mode mode = e.vector_mode (0);

    /* Comparisons are UNSPEC_PRED_Z operations and so need a hint
       operand.  */
    e.add_ptrue_hint (0, e.gp_mode (0));

    if (e.type_suffix (0).integer_p)
      {
	bool unsigned_p = e.type_suffix (0).unsigned_p;
	rtx_code code = get_rtx_code (m_code, unsigned_p);
	return e.use_exact_insn (code_for_aarch64_pred_cmp (code, mode));
      }

    insn_code icode = code_for_aarch64_pred_fcm (m_unspec_for_fp, mode);
    return e.use_exact_insn (icode);
  }

  /* The tree code associated with the comparison.  */
  tree_code m_code;

  /* The unspec code to use for floating-point comparisons.  */
  int m_unspec_for_fp;
};

/* Implements svcmp<cc>_wide.  */
class svcmp_wide_impl : public function_base
{
public:
  CONSTEXPR svcmp_wide_impl (tree_code code, int unspec_for_sint,
			     int unspec_for_uint)
    : m_code (code), m_unspec_for_sint (unspec_for_sint),
      m_unspec_for_uint (unspec_for_uint) {}

  rtx
  expand (function_expander &e) const override
  {
    machine_mode mode = e.vector_mode (0);
    bool unsigned_p = e.type_suffix (0).unsigned_p;
    rtx_code code = get_rtx_code (m_code, unsigned_p);

    /* Comparisons are UNSPEC_PRED_Z operations and so need a hint
       operand.  */
    e.add_ptrue_hint (0, e.gp_mode (0));

    /* If the argument is a constant that the unwidened comparisons
       can handle directly, use them instead.  */
    insn_code icode = code_for_aarch64_pred_cmp (code, mode);
    rtx op2 = unwrap_const_vec_duplicate (e.args[3]);
    if (CONSTANT_P (op2)
	&& insn_data[icode].operand[4].predicate (op2, DImode))
      {
	e.args[3] = op2;
	return e.use_exact_insn (icode);
      }

    int unspec = (unsigned_p ? m_unspec_for_uint : m_unspec_for_sint);
    return e.use_exact_insn (code_for_aarch64_pred_cmp_wide (unspec, mode));
  }

  /* The tree code associated with the comparison.  */
  tree_code m_code;

  /* The unspec codes for signed and unsigned wide comparisons
     respectively.  */
  int m_unspec_for_sint;
  int m_unspec_for_uint;
};

class svcmpuo_impl : public quiet<function_base>
{
public:
  rtx
  expand (function_expander &e) const override
  {
    e.add_ptrue_hint (0, e.gp_mode (0));
    return e.use_exact_insn (code_for_aarch64_pred_fcmuo (e.vector_mode (0)));
  }
};

class svcnot_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    machine_mode mode = e.vector_mode (0);
    machine_mode pred_mode = e.gp_mode (0);
    /* The underlying _x pattern is effectively:

	 dst = src == 0 ? 1 : 0

       rather than an UNSPEC_PRED_X.  Using this form allows autovec
       constructs to be matched by combine, but it means that the
       predicate on the src == 0 comparison must be all-true.

       For simplicity, represent other _x operations as fully-defined _m
       operations rather than using a separate bespoke pattern.  */
    if (e.pred == PRED_x
	&& gen_lowpart (pred_mode, e.args[0]) == CONSTM1_RTX (pred_mode))
      return e.use_pred_x_insn (code_for_aarch64_ptrue_cnot (mode));
    return e.use_cond_insn (code_for_cond_cnot (mode),
			    e.pred == PRED_x ? 1 : 0);
  }
};

/* Implements svcnt[bhwd], which count the number of elements
   in a particular vector mode.  */
class svcnt_bhwd_impl : public function_base
{
public:
  CONSTEXPR svcnt_bhwd_impl (machine_mode ref_mode) : m_ref_mode (ref_mode) {}

  gimple *
  fold (gimple_folder &f) const override
  {
    return f.fold_to_cstu (GET_MODE_NUNITS (m_ref_mode));
  }

  rtx
  expand (function_expander &) const override
  {
    return gen_int_mode (GET_MODE_NUNITS (m_ref_mode), DImode);
  }

  /* The mode of the vector associated with the [bhwd] suffix.  */
  machine_mode m_ref_mode;
};

/* Implements svcnt[bhwd]_pat.  */
class svcnt_bhwd_pat_impl : public svcnt_bhwd_impl
{
public:
  using svcnt_bhwd_impl::svcnt_bhwd_impl;

  gimple *
  fold (gimple_folder &f) const override
  {
    tree pattern_arg = gimple_call_arg (f.call, 0);
    aarch64_svpattern pattern = (aarch64_svpattern) tree_to_shwi (pattern_arg);

    if (pattern == AARCH64_SV_ALL)
      /* svcvnt[bwhd]_pat (SV_ALL) == svcnt[bwhd] ().  */
      return svcnt_bhwd_impl::fold (f);

    /* See whether we can count the number of elements in the pattern
       at compile time.  */
    unsigned int elements_per_vq = 128 / GET_MODE_UNIT_BITSIZE (m_ref_mode);
    HOST_WIDE_INT value = aarch64_fold_sve_cnt_pat (pattern, elements_per_vq);
    if (value >= 0)
      return f.fold_to_cstu (value);

    return NULL;
  }

  rtx
  expand (function_expander &e) const override
  {
    unsigned int elements_per_vq = 128 / GET_MODE_UNIT_BITSIZE (m_ref_mode);
    e.args.quick_push (gen_int_mode (elements_per_vq, DImode));
    e.args.quick_push (const1_rtx);
    return e.use_exact_insn (CODE_FOR_aarch64_sve_cnt_pat);
  }
};

class svcntp_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    if (e.type_suffix (0).tclass == TYPE_count)
      {
	unsigned int bits = e.type_suffix (0).element_bits;
	return e.use_exact_insn (code_for_aarch64_sve_cntp_c (bits));
      }

    machine_mode mode = e.vector_mode (0);
    e.add_ptrue_hint (0, mode);
    return e.use_exact_insn (code_for_aarch64_pred_cntp (mode));
  }
};

/* Implements svcreate2, svcreate3 and svcreate4.  */
class svcreate_impl : public quiet<multi_vector_function>
{
public:
  using quiet<multi_vector_function>::quiet;

  gimple *
  fold (gimple_folder &f) const override
  {
    unsigned int nargs = gimple_call_num_args (f.call);
    tree lhs_type = TREE_TYPE (f.lhs);

    /* Replace the call with a clobber of the result (to prevent it from
       becoming upwards exposed) followed by stores into each individual
       vector of tuple.

       The fold routines expect the replacement statement to have the
       same lhs as the original call, so return the clobber statement
       rather than the final vector store.  */
    gassign *clobber = gimple_build_assign (f.lhs, build_clobber (lhs_type));

    for (unsigned int i = nargs; i-- > 0; )
      {
	tree rhs_vector = gimple_call_arg (f.call, i);
	tree field = tuple_type_field (TREE_TYPE (f.lhs));
	tree lhs_array = build3 (COMPONENT_REF, TREE_TYPE (field),
				 unshare_expr (f.lhs), field, NULL_TREE);
	tree lhs_vector = build4 (ARRAY_REF, TREE_TYPE (rhs_vector),
				  lhs_array, size_int (i),
				  NULL_TREE, NULL_TREE);
	gassign *assign = gimple_build_assign (lhs_vector, rhs_vector);
	gsi_insert_after (f.gsi, assign, GSI_SAME_STMT);
      }
    return clobber;
  }

  rtx
  expand (function_expander &e) const override
  {
    rtx lhs_tuple = e.get_nonoverlapping_reg_target ();

    /* Record that LHS_TUPLE is dead before the first store.  */
    emit_clobber (lhs_tuple);
    for (unsigned int i = 0; i < e.args.length (); ++i)
      {
	/* Use an lvalue subreg to refer to vector I in LHS_TUPLE.  */
	rtx lhs_vector = simplify_gen_subreg (GET_MODE (e.args[i]),
					      lhs_tuple, GET_MODE (lhs_tuple),
					      i * BYTES_PER_SVE_VECTOR);
	emit_move_insn (lhs_vector, e.args[i]);
      }
    return lhs_tuple;
  }
};

class svcvt_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    insn_code icode;
    if (e.pred == PRED_none)
      {
	machine_mode mode0 = e.result_mode ();
	machine_mode mode1 = GET_MODE (e.args[0]);
	convert_optab optab;
	if (e.type_suffix (0).integer_p)
	  optab = e.type_suffix (0).unsigned_p ? ufix_optab : sfix_optab;
	else if (e.type_suffix (1).integer_p)
	  optab = e.type_suffix (1).unsigned_p ? ufloat_optab : sfloat_optab;
	else
	  optab = trunc_optab;
	icode = convert_optab_handler (optab, mode0, mode1);
	gcc_assert (icode != CODE_FOR_nothing);
	return e.use_exact_insn (icode);
      }
    machine_mode mode0 = e.vector_mode (0);
    machine_mode mode1 = e.vector_mode (1);
    /* All this complication comes from the need to select four things
       simultaneously:

       (1) the kind of conversion (int<-float, float<-int, float<-float)
       (2) signed vs. unsigned integers, where relevant
       (3) the predication mode, which must be the wider of the predication
	   modes for MODE0 and MODE1
       (4) the predication type (m, x or z)

       The only supported int<->float conversions for which the integer is
       narrower than the float are SI<->DF.  It's therefore more convenient
       to handle (3) by defining two patterns for int<->float conversions:
       one in which the integer is at least as wide as the float and so
       determines the predication mode, and another single SI<->DF pattern
       in which the float's mode determines the predication mode (which is
       always VNx2BI in that case).

       The names of the patterns follow the optab convention of giving
       the source mode before the destination mode.  */
    if (e.type_suffix (1).integer_p)
      {
	int unspec = (e.type_suffix (1).unsigned_p
		      ? UNSPEC_COND_UCVTF
		      : UNSPEC_COND_SCVTF);
	if (e.type_suffix (0).element_bytes <= e.type_suffix (1).element_bytes)
	  icode = (e.pred == PRED_x
		   ? code_for_aarch64_sve_nonextend (unspec, mode1, mode0)
		   : code_for_cond_nonextend (unspec, mode1, mode0));
	else
	  icode = (e.pred == PRED_x
		   ? code_for_aarch64_sve_extend (unspec, mode1, mode0)
		   : code_for_cond_extend (unspec, mode1, mode0));
      }
    else
      {
	int unspec = (!e.type_suffix (0).integer_p ? UNSPEC_COND_FCVT
		      : e.type_suffix (0).unsigned_p ? UNSPEC_COND_FCVTZU
		      : UNSPEC_COND_FCVTZS);
	if (e.type_suffix (0).element_bytes >= e.type_suffix (1).element_bytes)
	  icode = (e.pred == PRED_x
		   ? code_for_aarch64_sve_nontrunc (unspec, mode1, mode0)
		   : code_for_cond_nontrunc (unspec, mode1, mode0));
	else
	  icode = (e.pred == PRED_x
		   ? code_for_aarch64_sve_trunc (unspec, mode1, mode0)
		   : code_for_cond_trunc (unspec, mode1, mode0));
      }

    if (e.pred == PRED_x)
      return e.use_pred_x_insn (icode);
    return e.use_cond_insn (icode);
  }
};

class svdiv_impl : public rtx_code_function
{
public:
  CONSTEXPR svdiv_impl ()
    : rtx_code_function (DIV, UDIV, UNSPEC_COND_FDIV) {}

  gimple *
  fold (gimple_folder &f) const override
  {
    if (auto *res = f.fold_const_binary (TRUNC_DIV_EXPR))
      return res;

    /* If the dividend is all zeros, fold to zero vector.  */
    tree op1 = gimple_call_arg (f.call, 1);
    if (integer_zerop (op1))
      return gimple_build_assign (f.lhs, op1);

    /* If the divisor is all zeros, fold to zero vector.  */
    tree pg = gimple_call_arg (f.call, 0);
    tree op2 = gimple_call_arg (f.call, 2);
    if (integer_zerop (op2)
	&& (f.pred != PRED_m
	    || is_ptrue (pg, f.type_suffix (0).element_bytes)))
      return gimple_build_assign (f.lhs, build_zero_cst (TREE_TYPE (f.lhs)));

    /* If the divisor is a uniform power of 2, fold to a shift
       instruction.  */
    tree op2_cst = uniform_integer_cst_p (op2);
    if (!op2_cst || !integer_pow2p (op2_cst))
      return NULL;

    tree new_divisor;
    gcall *call;

    if (f.type_suffix (0).unsigned_p && tree_to_uhwi (op2_cst) != 1)
      {
	function_instance instance ("svlsr", functions::svlsr,
				    shapes::binary_uint_opt_n, MODE_n,
				    f.type_suffix_ids, GROUP_none, f.pred);
	call = f.redirect_call (instance);
	tree d = INTEGRAL_TYPE_P (TREE_TYPE (op2)) ? op2 : op2_cst;
	new_divisor = wide_int_to_tree (TREE_TYPE (d), tree_log2 (d));
      }
    else
      {
	if (tree_int_cst_sign_bit (op2_cst)
	    || tree_to_shwi (op2_cst) == 1)
	  return NULL;

	function_instance instance ("svasrd", functions::svasrd,
				    shapes::shift_right_imm, MODE_n,
				    f.type_suffix_ids, GROUP_none, f.pred);
	call = f.redirect_call (instance);
	new_divisor = wide_int_to_tree (scalar_types[VECTOR_TYPE_svuint64_t],
					tree_log2 (op2_cst));
      }

    gimple_call_set_arg (call, 2, new_divisor);
    return call;
  }
};


class svdot_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    /* In the optab, the multiplication operands come before the accumulator
       operand.  The optab is keyed off the multiplication mode.  */
    e.rotate_inputs_left (0, 3);
    insn_code icode;
    if (e.type_suffix_ids[1] == NUM_TYPE_SUFFIXES)
      icode = e.convert_optab_handler_for_sign (sdot_prod_optab,
						udot_prod_optab,
						0, e.result_mode (),
						GET_MODE (e.args[0]));
    else
      icode = (e.type_suffix (0).float_p
	       ? CODE_FOR_aarch64_sve_fdotvnx4sfvnx8hf
	       : e.type_suffix (0).unsigned_p
	       ? CODE_FOR_udot_prodvnx4sivnx8hi
	       : CODE_FOR_sdot_prodvnx4sivnx8hi);
    return e.use_unpred_insn (icode);
  }
};

class svdotprod_lane_impl : public unspec_based_function_base
{
public:
  using unspec_based_function_base::unspec_based_function_base;

  rtx
  expand (function_expander &e) const override
  {
    machine_mode mode0 = GET_MODE (e.args[0]);
    machine_mode mode1 = GET_MODE (e.args[1]);
    /* Use the same ordering as the dot_prod_optab, with the
       accumulator last.  */
    e.rotate_inputs_left (0, 4);
    int unspec = unspec_for (e);
    insn_code icode;
    if (unspec == UNSPEC_FDOT)
      icode = CODE_FOR_aarch64_fdot_prod_lanevnx4sfvnx8hf;
    else
      icode = code_for_aarch64_dot_prod_lane (unspec, mode0, mode1);
    return e.use_exact_insn (icode);
  }
};

class svdup_impl : public quiet<function_base>
{
public:
  gimple *
  fold (gimple_folder &f) const override
  {
    tree vec_type = TREE_TYPE (f.lhs);
    tree rhs = gimple_call_arg (f.call, f.pred == PRED_none ? 0 : 1);

    if (f.pred == PRED_none || f.pred == PRED_x)
      {
	if (CONSTANT_CLASS_P (rhs))
	  {
	    if (f.type_suffix (0).bool_p)
	      return (tree_to_shwi (rhs)
		      ? f.fold_to_ptrue ()
		      : f.fold_to_pfalse ());

	    tree rhs_vector = build_vector_from_val (vec_type, rhs);
	    return gimple_build_assign (f.lhs, rhs_vector);
	  }

	/* Avoid folding _b to a VEC_DUPLICATE_EXPR, since to do that we
	   would need to introduce an extra and unwanted conversion to
	   the truth vector element type.  */
	if (!f.type_suffix (0).bool_p)
	  return gimple_build_assign (f.lhs, VEC_DUPLICATE_EXPR, rhs);
      }

    /* svdup_z (pg, x) == VEC_COND_EXPR <pg, VEC_DUPLICATE_EXPR <x>, 0>.  */
    if (f.pred == PRED_z)
      {
	gimple_seq stmts = NULL;
	tree pred = f.convert_pred (stmts, vec_type, 0);
	rhs = f.force_vector (stmts, vec_type, rhs);
	gsi_insert_seq_before (f.gsi, stmts, GSI_SAME_STMT);
	return gimple_build_assign (f.lhs, VEC_COND_EXPR, pred, rhs,
				    build_zero_cst (vec_type));
      }

    return NULL;
  }

  rtx
  expand (function_expander &e) const override
  {
    if (e.pred == PRED_none || e.pred == PRED_x)
      /* There's no benefit to using predicated instructions for _x here.  */
      return e.use_unpred_insn (e.direct_optab_handler (vec_duplicate_optab));

    /* Model predicated svdups as a SEL in which the "true" value is
       the duplicate of the function argument and the "false" value
       is the value of inactive lanes.  */
    insn_code icode;
    machine_mode mode = e.vector_mode (0);
    if (valid_for_const_vector_p (GET_MODE_INNER (mode), e.args.last ()))
      /* Duplicate the constant to fill a vector.  The pattern optimizes
	 various cases involving constant operands, falling back to SEL
	 if necessary.  */
      icode = code_for_vcond_mask (mode, mode);
    else
      /* Use the pattern for selecting between a duplicated scalar
	 variable and a vector fallback.  */
      icode = code_for_aarch64_sel_dup (mode);
    return e.use_vcond_mask_insn (icode);
  }
};

class svdup_lane_impl : public quiet<function_base>
{
public:
  rtx
  expand (function_expander &e) const override
  {
    /* The native DUP lane has an index range of 64 bytes.  */
    machine_mode mode = e.vector_mode (0);
    if (CONST_INT_P (e.args[1])
	&& IN_RANGE (INTVAL (e.args[1]) * GET_MODE_UNIT_SIZE (mode), 0, 63))
      return e.use_exact_insn (code_for_aarch64_sve_dup_lane (mode));

    /* Treat svdup_lane as if it were svtbl_n.  */
    return e.use_exact_insn (code_for_aarch64_sve_tbl (e.vector_mode (0)));
  }
};

class svdupq_impl : public quiet<function_base>
{
private:
  gimple *
  fold_nonconst_dupq (gimple_folder &f) const
  {
    /* Lower lhs = svdupq (arg0, arg1, ..., argN} into:
       tmp = {arg0, arg1, ..., arg<N-1>}
       lhs = VEC_PERM_EXPR (tmp, tmp, {0, 1, 2, N-1, ...})  */

    if (f.type_suffix (0).bool_p
	|| BYTES_BIG_ENDIAN)
      return NULL;

    tree lhs = gimple_call_lhs (f.call);
    tree lhs_type = TREE_TYPE (lhs);
    tree elt_type = TREE_TYPE (lhs_type);
    scalar_mode elt_mode = SCALAR_TYPE_MODE (elt_type);
    machine_mode vq_mode = aarch64_vq_mode (elt_mode).require ();
    tree vq_type = build_vector_type_for_mode (elt_type, vq_mode);

    unsigned nargs = gimple_call_num_args (f.call);
    vec<constructor_elt, va_gc> *v;
    vec_alloc (v, nargs);
    for (unsigned i = 0; i < nargs; i++)
      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, gimple_call_arg (f.call, i));
    tree vec = build_constructor (vq_type, v);
    tree tmp = make_ssa_name_fn (cfun, vq_type, 0);
    gimple *g = gimple_build_assign (tmp, vec);

    gimple_seq stmts = NULL;
    gimple_seq_add_stmt_without_update (&stmts, g);

    poly_uint64 lhs_len = TYPE_VECTOR_SUBPARTS (lhs_type);
    vec_perm_builder sel (lhs_len, nargs, 1);
    for (unsigned i = 0; i < nargs; i++)
      sel.quick_push (i);

    vec_perm_indices indices (sel, 1, nargs);
    tree mask_type = build_vector_type (ssizetype, lhs_len);
    tree mask = vec_perm_indices_to_tree (mask_type, indices);

    gimple *g2 = gimple_build_assign (lhs, VEC_PERM_EXPR, tmp, tmp, mask);
    gimple_seq_add_stmt_without_update (&stmts, g2);
    gsi_replace_with_seq (f.gsi, stmts, false);
    return g2;
  }

public:
  gimple *
  fold (gimple_folder &f) const override
  {
    tree vec_type = TREE_TYPE (f.lhs);
    unsigned int nargs = gimple_call_num_args (f.call);
    /* For predicates, pad out each argument so that we have one element
       per bit.  */
    unsigned int factor = (f.type_suffix (0).bool_p
			   ? f.type_suffix (0).element_bytes : 1);
    tree_vector_builder builder (vec_type, nargs * factor, 1);
    for (unsigned int i = 0; i < nargs; ++i)
      {
	tree elt = gimple_call_arg (f.call, i);
	if (!CONSTANT_CLASS_P (elt))
	  return fold_nonconst_dupq (f);
	builder.quick_push (elt);
	for (unsigned int j = 1; j < factor; ++j)
	  builder.quick_push (build_zero_cst (TREE_TYPE (vec_type)));
      }
    return gimple_build_assign (f.lhs, builder.build ());
  }

  rtx
  expand (function_expander &e) const override
  {
    machine_mode mode = e.vector_mode (0);
    unsigned int elements_per_vq = e.args.length ();
    if (GET_MODE_CLASS (mode) == MODE_VECTOR_BOOL)
      {
	/* Construct a vector of integers so that we can compare them against
	   zero below.  Zero vs. nonzero is the only distinction that
	   matters.  */
	mode = aarch64_sve_int_mode (mode);
	for (unsigned int i = 0; i < elements_per_vq; ++i)
	  e.args[i] = simplify_gen_unary (ZERO_EXTEND, GET_MODE_INNER (mode),
					  e.args[i], QImode);
      }

    /* Get the 128-bit Advanced SIMD vector for this data size.  */
    scalar_mode element_mode = GET_MODE_INNER (mode);
    machine_mode vq_mode = aarch64_vq_mode (element_mode).require ();
    gcc_assert (known_eq (elements_per_vq, GET_MODE_NUNITS (vq_mode)));

    /* Put the arguments into a 128-bit Advanced SIMD vector.  We want
       argument N to go into architectural lane N, whereas Advanced SIMD
       vectors are loaded memory lsb to register lsb.  We therefore need
       to reverse the elements for big-endian targets.  */
    rtx vq_reg = gen_reg_rtx (vq_mode);
    rtvec vec = rtvec_alloc (elements_per_vq);
    for (unsigned int i = 0; i < elements_per_vq; ++i)
      {
	unsigned int argno = BYTES_BIG_ENDIAN ? elements_per_vq - i - 1 : i;
	RTVEC_ELT (vec, i) = e.args[argno];
      }
    aarch64_expand_vector_init (vq_reg, gen_rtx_PARALLEL (vq_mode, vec));

    /* If the result is a boolean, compare the data vector against zero.  */
    if (mode != e.vector_mode (0))
      {
	rtx data_dupq = aarch64_expand_sve_dupq (NULL, mode, vq_reg);
	return aarch64_convert_sve_data_to_pred (e.possible_target,
						 e.vector_mode (0), data_dupq);
      }

    return aarch64_expand_sve_dupq (e.possible_target, mode, vq_reg);
  }
};

class svdupq_lane_impl : public quiet<function_base>
{
public:
  rtx
  expand (function_expander &e) const override
  {
    machine_mode mode = e.vector_mode (0);
    rtx index = e.args[1];
    if (CONST_INT_P (index) && IN_RANGE (INTVAL (index), 0, 3))
      {
	/* Use the .Q form of DUP, which is the native instruction for
	   this function.  */
	insn_code icode = code_for_aarch64_sve_dupq_lane (mode);
	unsigned int num_indices = e.elements_per_vq (0);
	rtx indices = aarch64_gen_stepped_int_parallel
	  (num_indices, INTVAL (index) * num_indices, 1);

	e.add_output_operand (icode);
	e.add_input_operand (icode, e.args[0]);
	e.add_fixed_operand (indices);
	return e.generate_insn (icode);
      }

    /* Build a .D TBL index for the pairs of doublewords that we want to
       duplicate.  */
    if (CONST_INT_P (index))
      {
	/* The index vector is a constant.  */
	rtx_vector_builder builder (VNx2DImode, 2, 1);
	builder.quick_push (gen_int_mode (INTVAL (index) * 2, DImode));
	builder.quick_push (gen_int_mode (INTVAL (index) * 2 + 1, DImode));
	index = builder.build ();
      }
    else
      {
	/* Duplicate INDEX * 2 to fill a DImode vector.  The ACLE spec
	   explicitly allows the top of the index to be dropped.  */
	index = force_reg (DImode, simplify_gen_binary (ASHIFT, DImode,
							index, const1_rtx));
	index = expand_vector_broadcast (VNx2DImode, index);

	/* Get an alternating 0, 1 predicate.  */
	rtx_vector_builder builder (VNx2BImode, 2, 1);
	builder.quick_push (const0_rtx);
	builder.quick_push (constm1_rtx);
	rtx pg = force_reg (VNx2BImode, builder.build ());

	/* Add one to the odd elements of the index.  */
	rtx one = force_reg (VNx2DImode, CONST1_RTX (VNx2DImode));
	rtx target = gen_reg_rtx (VNx2DImode);
	emit_insn (gen_cond_addvnx2di (target, pg, index, one, index));
	index = target;
      }

    e.args[0] = gen_lowpart (VNx2DImode, e.args[0]);
    e.args[1] = index;
    return e.use_exact_insn (CODE_FOR_aarch64_sve_tblvnx2di);
  }
};

/* Implements svextb, svexth and svextw.  */
class svext_bhw_impl : public function_base
{
public:
  CONSTEXPR svext_bhw_impl (scalar_int_mode from_mode)
    : m_from_mode (from_mode) {}

  rtx
  expand (function_expander &e) const override
  {
    if (e.type_suffix (0).unsigned_p)
      {
	/* Convert to an AND.  The widest we go is 0xffffffff, which fits
	   in a CONST_INT.  */
	e.args.quick_push (GEN_INT (GET_MODE_MASK (m_from_mode)));
	if (e.pred == PRED_m)
	  /* We now have arguments "(inactive, pg, op, mask)".  Convert this
	     to "(pg, op, mask, inactive)" so that the order matches svand_m
	     with an extra argument on the end.  Take the inactive elements
	     from this extra argument.  */
	  e.rotate_inputs_left (0, 4);
	return e.map_to_rtx_codes (AND, AND, -1, -1, 3);
      }

    machine_mode wide_mode = e.vector_mode (0);
    poly_uint64 nunits = GET_MODE_NUNITS (wide_mode);
    machine_mode narrow_mode
      = aarch64_sve_data_mode (m_from_mode, nunits).require ();
    if (e.pred == PRED_x)
      {
	insn_code icode = code_for_aarch64_pred_sxt (wide_mode, narrow_mode);
	return e.use_pred_x_insn (icode);
      }

    insn_code icode = code_for_aarch64_cond_sxt (wide_mode, narrow_mode);
    return e.use_cond_insn (icode);
  }

  /* The element mode that we're extending from.  */
  scalar_int_mode m_from_mode;
};

/* Implements svget2, svget3 and svget4.  */
class svget_impl : public quiet<multi_vector_function>
{
public:
  using quiet<multi_vector_function>::quiet;

  gimple *
  fold (gimple_folder &f) const override
  {
    /* Fold into a normal gimple component access.  */
    tree rhs_tuple = gimple_call_arg (f.call, 0);
    tree index = gimple_call_arg (f.call, 1);
    tree field = tuple_type_field (TREE_TYPE (rhs_tuple));
    tree rhs_array = build3 (COMPONENT_REF, TREE_TYPE (field),
			     rhs_tuple, field, NULL_TREE);
    tree rhs_vector = build4 (ARRAY_REF, TREE_TYPE (f.lhs),
			      rhs_array, index, NULL_TREE, NULL_TREE);
    return gimple_build_assign (f.lhs, rhs_vector);
  }

  rtx
  expand (function_expander &e) const override
  {
    /* Fold the access into a subreg rvalue.  */
    return force_subreg (e.vector_mode (0), e.args[0], GET_MODE (e.args[0]),
			 INTVAL (e.args[1]) * BYTES_PER_SVE_VECTOR);
  }
};

class svget_neonq_impl : public function_base
{
public:
  gimple *
  fold (gimple_folder &f) const override
  {
    if (BYTES_BIG_ENDIAN)
      return NULL;
    tree rhs_sve_vector = gimple_call_arg (f.call, 0);
    tree rhs_vector = build3 (BIT_FIELD_REF, TREE_TYPE (f.lhs),
			     rhs_sve_vector, bitsize_int (128), bitsize_int (0));
    return gimple_build_assign (f.lhs, rhs_vector);
  }

  rtx
  expand (function_expander &e) const override
  {
    if (BYTES_BIG_ENDIAN)
      {
	machine_mode mode = e.vector_mode (0);
	insn_code icode = code_for_aarch64_sve_get_neonq (mode);
	unsigned int nunits = 128 / GET_MODE_UNIT_BITSIZE (mode);
	rtx indices = aarch64_gen_stepped_int_parallel
	  (nunits, nunits - 1, -1);

	e.add_output_operand (icode);
	e.add_input_operand (icode, e.args[0]);
	e.add_fixed_operand (indices);
	return e.generate_insn (icode);
      }
    return force_subreg (e.result_mode (), e.args[0], GET_MODE (e.args[0]), 0);
  }
};

class svset_neonq_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    machine_mode mode = e.vector_mode (0);

    /* If the SVE argument is undefined, we just need to reinterpret the
       Advanced SIMD argument as an SVE vector.  */
    if (!BYTES_BIG_ENDIAN
	&& is_undef (CALL_EXPR_ARG (e.call_expr, 0)))
      return force_subreg (mode, e.args[1], GET_MODE (e.args[1]), 0);

    rtx_vector_builder builder (VNx16BImode, 16, 2);
    for (unsigned int i = 0; i < 16; i++)
      builder.quick_push (CONST1_RTX (BImode));
    for (unsigned int i = 0; i < 16; i++)
      builder.quick_push (CONST0_RTX (BImode));
    e.args.quick_push (builder.build ());
    if (BYTES_BIG_ENDIAN)
      return e.use_exact_insn (code_for_aarch64_sve_set_neonq (mode));
    insn_code icode = code_for_vcond_mask (mode, mode);
    e.args[1] = force_lowpart_subreg (mode, e.args[1], GET_MODE (e.args[1]));
    e.add_output_operand (icode);
    e.add_input_operand (icode, e.args[1]);
    e.add_input_operand (icode, e.args[0]);
    e.add_input_operand (icode, e.args[2]);
    return e.generate_insn (icode);
  }
};

class svdup_neonq_impl : public function_base
{
public:
  gimple *
  fold (gimple_folder &f) const override
  {
    if (BYTES_BIG_ENDIAN)
      return NULL;
    tree rhs_vector = gimple_call_arg (f.call, 0);
    unsigned HOST_WIDE_INT neon_nelts
      = TYPE_VECTOR_SUBPARTS (TREE_TYPE (rhs_vector)).to_constant ();
    poly_uint64 sve_nelts = TYPE_VECTOR_SUBPARTS (TREE_TYPE (f.lhs));
    vec_perm_builder builder (sve_nelts, neon_nelts, 1);
    for (unsigned int i = 0; i < neon_nelts; i++)
      builder.quick_push (i);
    vec_perm_indices indices (builder, 1, neon_nelts);
    tree perm_type = build_vector_type (ssizetype, sve_nelts);
    return gimple_build_assign (f.lhs, VEC_PERM_EXPR,
				rhs_vector,
				rhs_vector,
				vec_perm_indices_to_tree (perm_type, indices));
  }

  rtx
  expand (function_expander &e) const override
  {
    machine_mode mode = e.vector_mode (0);
    if (BYTES_BIG_ENDIAN)
      {
	insn_code icode = code_for_aarch64_vec_duplicate_vq_be (mode);
	unsigned int nunits = 128 / GET_MODE_UNIT_BITSIZE (mode);
	rtx indices = aarch64_gen_stepped_int_parallel
	  (nunits, nunits - 1, -1);

	e.add_output_operand (icode);
	e.add_input_operand (icode, e.args[0]);
	e.add_fixed_operand (indices);
	return e.generate_insn (icode);
      }
    insn_code icode = code_for_aarch64_vec_duplicate_vq_le (mode);
    e.add_output_operand (icode);
    e.add_input_operand (icode, e.args[0]);
    return e.generate_insn (icode);
  }
};

class svindex_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    return e.use_exact_insn (e.direct_optab_handler (vec_series_optab));
  }
};

class svinsr_impl : public quiet<function_base>
{
public:
  gimple *
  fold (gimple_folder &f) const override
  {
    gcall *new_call = gimple_build_call_internal (IFN_VEC_SHL_INSERT, 2,
						  gimple_call_arg (f.call, 0),
						  gimple_call_arg (f.call, 1));
    gimple_call_set_lhs (new_call, f.lhs);
    return new_call;
  }

  rtx
  expand (function_expander &e) const override
  {
    insn_code icode = direct_optab_handler (vec_shl_insert_optab,
					    e.vector_mode (0));
    return e.use_exact_insn (icode);
  }
};

/* Implements svlasta and svlastb.  */
class svlast_impl : public quiet<function_base>
{
public:
  CONSTEXPR svlast_impl (int unspec) : m_unspec (unspec) {}

  bool is_lasta () const { return m_unspec == UNSPEC_LASTA; }
  bool is_lastb () const { return m_unspec == UNSPEC_LASTB; }

  /* Fold a svlast{a/b} call with constant predicate to a BIT_FIELD_REF.
     BIT_FIELD_REF lowers to Advanced SIMD element extract, so we have to
     ensure the index of the element being accessed is in the range of a
     Advanced SIMD vector width.  */
  gimple *fold (gimple_folder & f) const override
  {
    tree pred = gimple_call_arg (f.call, 0);
    tree val = gimple_call_arg (f.call, 1);

    if (TREE_CODE (pred) == VECTOR_CST)
      {
	HOST_WIDE_INT pos;
	int i = 0;
	int step = f.type_suffix (0).element_bytes;
	int step_1 = gcd (step, VECTOR_CST_NPATTERNS (pred));
	int npats = VECTOR_CST_NPATTERNS (pred);
	unsigned enelts = vector_cst_encoded_nelts (pred);
	tree b = NULL_TREE;
	unsigned HOST_WIDE_INT nelts;

	/* We can optimize 2 cases common to variable and fixed-length cases
	   without a linear search of the predicate vector:
	   1.  LASTA if predicate is all true, return element 0.
	   2.  LASTA if predicate all false, return element 0.  */
	if (is_lasta () && vector_cst_all_same (pred, step_1))
	  {
	    b = build3 (BIT_FIELD_REF, TREE_TYPE (f.lhs), val,
			bitsize_int (step * BITS_PER_UNIT), bitsize_int (0));
	    return gimple_build_assign (f.lhs, b);
	  }

	/* Handle the all-false case for LASTB where SVE VL == 128b -
	   return the highest numbered element.  */
	if (is_lastb () && known_eq (BYTES_PER_SVE_VECTOR, 16)
	    && vector_cst_all_same (pred, step_1)
	    && integer_zerop (VECTOR_CST_ENCODED_ELT (pred, 0)))
	  {
	    b = build3 (BIT_FIELD_REF, TREE_TYPE (f.lhs), val,
			bitsize_int (step * BITS_PER_UNIT),
			bitsize_int ((16 - step) * BITS_PER_UNIT));

	    return gimple_build_assign (f.lhs, b);
	  }

	/* Determine if there are any repeating non-zero elements in variable
	   length vectors.  */
	if (!VECTOR_CST_NELTS (pred).is_constant (&nelts))
	  {
	   /* If VECTOR_CST_NELTS_PER_PATTERN (pred) == 2 and every multiple of
	      'step_1' in
		[VECTOR_CST_NPATTERNS .. VECTOR_CST_ENCODED_NELTS - 1]
	      is zero, then we can treat the vector as VECTOR_CST_NPATTERNS
	      elements followed by all inactive elements.  */
	    if (VECTOR_CST_NELTS_PER_PATTERN (pred) == 2)
	      {
		/* Restrict the scope of search to NPATS if vector is
		   variable-length for linear search later.  */
		nelts = npats;
		for (unsigned j = npats; j < enelts; j += step_1)
		  {
		    /* If there are active elements in the repeated pattern of a
		       variable-length vector, then return NULL as there is no
		       way to be sure statically if this falls within the
		       Advanced SIMD range.  */
		    if (!integer_zerop (VECTOR_CST_ENCODED_ELT (pred, j)))
		      return NULL;
		  }
	      }
	    else
	      /* If we're here, it means that for NELTS_PER_PATTERN != 2, there
		 is a repeating non-zero element.  */
	      return NULL;
	  }

	/* If we're here, it means either:
	   1. The vector is variable-length and there's no active element in the
	      repeated part of the pattern, or
	   2. The vector is fixed-length.

	   Fall through to finding the last active element linearly for
	   for all cases where the last active element is known to be
	   within a statically-determinable range.  */
	i = MAX ((int)nelts - step, 0);
	for (; i >= 0; i -= step)
	  if (!integer_zerop (VECTOR_CST_ELT (pred, i)))
	    break;

	if (is_lastb ())
	  {
	    /* For LASTB, the element is the last active element.  */
	    pos = i;
	  }
	else
	  {
	    /* For LASTA, the element is one after last active element.  */
	    pos = i + step;

	    /* If last active element is
	       last element, wrap-around and return first Advanced SIMD
	       element.  */
	    if (known_ge (pos, BYTES_PER_SVE_VECTOR))
	      pos = 0;
	  }

	/* Out of Advanced SIMD range.  */
	if (pos < 0 || pos > 15)
	  return NULL;

	b = build3 (BIT_FIELD_REF, TREE_TYPE (f.lhs), val,
		    bitsize_int (step * BITS_PER_UNIT),
		    bitsize_int (pos * BITS_PER_UNIT));

	return gimple_build_assign (f.lhs, b);
      }
    return NULL;
  }

  rtx
  expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_extract (m_unspec, e.vector_mode (0)));
  }

  /* The unspec code associated with the operation.  */
  int m_unspec;
};

class svld1_impl : public full_width_access
{
public:
  unsigned int
  call_properties (const function_instance &) const override
  {
    return CP_READ_MEMORY;
  }

  gimple *
  fold (gimple_folder &f) const override
  {
    if (f.vectors_per_tuple () != 1)
      return nullptr;

    tree vectype = f.vector_type (0);

    /* Get the predicate and base pointer.  */
    gimple_seq stmts = NULL;
    tree pred = f.convert_pred (stmts, vectype, 0);
    tree base = f.fold_contiguous_base (stmts, vectype);
    gsi_insert_seq_before (f.gsi, stmts, GSI_SAME_STMT);

    tree cookie = f.load_store_cookie (TREE_TYPE (vectype));
    gcall *new_call = gimple_build_call_internal (IFN_MASK_LOAD, 3,
						  base, cookie, pred);
    gimple_call_set_lhs (new_call, f.lhs);
    return new_call;
  }

  rtx
  expand (function_expander &e) const override
  {
    insn_code icode;
    if (e.vectors_per_tuple () == 1)
      icode = convert_optab_handler (maskload_optab,
				     e.vector_mode (0), e.gp_mode (0));
    else
      icode = code_for_aarch64 (UNSPEC_LD1_COUNT, e.tuple_mode (0));
    return e.use_contiguous_load_insn (icode);
  }
};

/* Implements extending contiguous forms of svld1.  */
class svld1_extend_impl : public extending_load
{
public:
  using extending_load::extending_load;

  rtx
  expand (function_expander &e) const override
  {
    insn_code icode = code_for_aarch64_load (UNSPEC_LD1_SVE, extend_rtx_code (),
					     e.vector_mode (0),
					     e.memory_vector_mode ());
    return e.use_contiguous_load_insn (icode);
  }
};

class svld1_gather_impl : public full_width_access
{
public:
  unsigned int
  call_properties (const function_instance &) const override
  {
    return CP_READ_MEMORY;
  }

  rtx
  expand (function_expander &e) const override
  {
    e.prepare_gather_address_operands (1);
    /* Put the predicate last, as required by mask_gather_load_optab.  */
    e.rotate_inputs_left (0, 5);
    machine_mode mem_mode = e.memory_vector_mode ();
    machine_mode int_mode = aarch64_sve_int_mode (mem_mode);
    insn_code icode = convert_optab_handler (mask_gather_load_optab,
					     mem_mode, int_mode);
    return e.use_exact_insn (icode);
  }
};

/* Implements extending forms of svld1_gather.  */
class svld1_gather_extend_impl : public extending_load
{
public:
  using extending_load::extending_load;

  rtx
  expand (function_expander &e) const override
  {
    e.prepare_gather_address_operands (1);
    /* Put the predicate last, since the extending gathers use the same
       operand order as mask_gather_load_optab.  */
    e.rotate_inputs_left (0, 5);
    /* Add a constant predicate for the extension rtx.  */
    e.args.quick_push (CONSTM1_RTX (VNx16BImode));
    insn_code icode = code_for_aarch64_gather_load (extend_rtx_code (),
						    e.vector_mode (0),
						    e.memory_vector_mode ());
    return e.use_exact_insn (icode);
  }
};

class load_replicate : public function_base
{
public:
  unsigned int
  call_properties (const function_instance &) const override
  {
    return CP_READ_MEMORY;
  }

  tree
  memory_scalar_type (const function_instance &fi) const override
  {
    return fi.scalar_type (0);
  }
};

class svld1rq_impl : public load_replicate
{
public:
  machine_mode
  memory_vector_mode (const function_instance &fi) const override
  {
    return aarch64_vq_mode (GET_MODE_INNER (fi.vector_mode (0))).require ();
  }

  rtx
  expand (function_expander &e) const override
  {
    insn_code icode = code_for_aarch64_sve_ld1rq (e.vector_mode (0));
    return e.use_contiguous_load_insn (icode);
  }

  gimple *
  fold (gimple_folder &f) const override
  {
    tree arg0 = gimple_call_arg (f.call, 0);
    tree arg1 = gimple_call_arg (f.call, 1);

    /* Transform:
       lhs = svld1rq ({-1, -1, ... }, arg1)
       into:
       tmp = mem_ref<vectype> [(elem * {ref-all}) arg1]
       lhs = vec_perm_expr<tmp, tmp, {0, 1, 2, 3, ...}>.
       on little endian target.
       vectype is the corresponding ADVSIMD type.  */

    if (!BYTES_BIG_ENDIAN
	&& integer_all_onesp (arg0)
	&& !flag_non_call_exceptions)
      {
	tree lhs = gimple_call_lhs (f.call);
	tree lhs_type = TREE_TYPE (lhs);
	poly_uint64 lhs_len = TYPE_VECTOR_SUBPARTS (lhs_type);
	tree eltype = TREE_TYPE (lhs_type);

	scalar_mode elmode = GET_MODE_INNER (TYPE_MODE (lhs_type));
	machine_mode vq_mode = aarch64_vq_mode (elmode).require ();
	tree vectype = build_vector_type_for_mode (eltype, vq_mode);

	tree elt_ptr_type
	  = build_pointer_type_for_mode (eltype, VOIDmode, true);
	tree zero = build_zero_cst (elt_ptr_type);

	/* Use element type alignment.  */
	tree access_type
	  = build_aligned_type (vectype, TYPE_ALIGN (eltype));

	tree mem_ref_lhs = make_ssa_name_fn (cfun, access_type, 0);
	tree mem_ref_op = fold_build2 (MEM_REF, access_type, arg1, zero);
	gimple *mem_ref_stmt
	  = gimple_build_assign (mem_ref_lhs, mem_ref_op);

	gimple_seq stmts = NULL;
	gimple_seq_add_stmt_without_update (&stmts, mem_ref_stmt);

	int source_nelts = TYPE_VECTOR_SUBPARTS (access_type).to_constant ();
	vec_perm_builder sel (lhs_len, source_nelts, 1);
	for (int i = 0; i < source_nelts; i++)
	  sel.quick_push (i);

	vec_perm_indices indices (sel, 1, source_nelts);
	gcc_checking_assert (can_vec_perm_const_p (TYPE_MODE (lhs_type),
						   TYPE_MODE (access_type),
						   indices));
	tree mask_type = build_vector_type (ssizetype, lhs_len);
	tree mask = vec_perm_indices_to_tree (mask_type, indices);
	gimple *g2 = gimple_build_assign (lhs, VEC_PERM_EXPR,
					  mem_ref_lhs, mem_ref_lhs, mask);
	gimple_seq_add_stmt_without_update (&stmts, g2);
	gsi_replace_with_seq_vops (f.gsi, stmts);
	return g2;
      }

    return NULL;
  }
};

class svld1ro_impl : public load_replicate
{
public:
  machine_mode
  memory_vector_mode (const function_instance &) const override
  {
    return OImode;
  }

  rtx
  expand (function_expander &e) const override
  {
    insn_code icode = code_for_aarch64_sve_ld1ro (e.vector_mode (0));
    return e.use_contiguous_load_insn (icode);
  }
};

/* Implements svld2, svld3 and svld4.  */
class svld234_impl : public full_width_access
{
public:
  using full_width_access::full_width_access;

  unsigned int
  call_properties (const function_instance &) const override
  {
    return CP_READ_MEMORY;
  }

  gimple *
  fold (gimple_folder &f) const override
  {
    tree tuple_type = TREE_TYPE (f.lhs);
    tree vectype = f.vector_type (0);

    /* Get the predicate and base pointer.  */
    gimple_seq stmts = NULL;
    tree pred = f.convert_pred (stmts, vectype, 0);
    tree base = f.fold_contiguous_base (stmts, vectype);
    gsi_insert_seq_before (f.gsi, stmts, GSI_SAME_STMT);

    /* Emit two statements: a clobber of the lhs, so that it isn't
       upwards exposed, and then the load itself.

       The fold routines expect the replacement statement to have the
       same lhs as the original call, so return the clobber statement
       rather than the load.  */
    gimple *clobber = gimple_build_assign (f.lhs, build_clobber (tuple_type));

    /* View the loaded data as an array of vectors.  */
    tree field = tuple_type_field (tuple_type);
    tree lhs_array = build1 (VIEW_CONVERT_EXPR, TREE_TYPE (field),
			     unshare_expr (f.lhs));

    /* Emit the load itself.  */
    tree cookie = f.load_store_cookie (TREE_TYPE (vectype));
    gcall *new_call = gimple_build_call_internal (IFN_MASK_LOAD_LANES, 3,
						  base, cookie, pred);
    gimple_call_set_lhs (new_call, lhs_array);
    gsi_insert_after (f.gsi, new_call, GSI_SAME_STMT);

    return clobber;
  }

  rtx
  expand (function_expander &e) const override
  {
    machine_mode tuple_mode = e.result_mode ();
    insn_code icode = convert_optab_handler (vec_mask_load_lanes_optab,
					     tuple_mode, e.vector_mode (0));
    return e.use_contiguous_load_insn (icode);
  }
};

class svldff1_gather_impl : public full_width_access
{
public:
  unsigned int
  call_properties (const function_instance &) const override
  {
    return CP_READ_MEMORY | CP_READ_FFR | CP_WRITE_FFR;
  }

  rtx
  expand (function_expander &e) const override
  {
    /* See the block comment in aarch64-sve.md for details about the
       FFR handling.  */
    emit_insn (gen_aarch64_update_ffr_for_load ());

    e.prepare_gather_address_operands (1);
    /* Put the predicate last, since ldff1_gather uses the same operand
       order as mask_gather_load_optab.  */
    e.rotate_inputs_left (0, 5);
    machine_mode mem_mode = e.memory_vector_mode ();
    return e.use_exact_insn (code_for_aarch64_ldff1_gather (mem_mode));
  }
};

/* Implements extending forms of svldff1_gather.  */
class svldff1_gather_extend : public extending_load
{
public:
  using extending_load::extending_load;

  rtx
  expand (function_expander &e) const override
  {
    /* See the block comment in aarch64-sve.md for details about the
       FFR handling.  */
    emit_insn (gen_aarch64_update_ffr_for_load ());

    e.prepare_gather_address_operands (1);
    /* Put the predicate last, since ldff1_gather uses the same operand
       order as mask_gather_load_optab.  */
    e.rotate_inputs_left (0, 5);
    /* Add a constant predicate for the extension rtx.  */
    e.args.quick_push (CONSTM1_RTX (VNx16BImode));
    insn_code icode = code_for_aarch64_ldff1_gather (extend_rtx_code (),
						     e.vector_mode (0),
						     e.memory_vector_mode ());
    return e.use_exact_insn (icode);
  }
};

class svldnt1_impl : public full_width_access
{
public:
  unsigned int
  call_properties (const function_instance &) const override
  {
    return CP_READ_MEMORY;
  }

  rtx
  expand (function_expander &e) const override
  {
    insn_code icode = (e.vectors_per_tuple () == 1
		       ? code_for_aarch64_ldnt1 (e.vector_mode (0))
		       : code_for_aarch64 (UNSPEC_LDNT1_COUNT,
					   e.tuple_mode (0)));
    return e.use_contiguous_load_insn (icode);
  }
};

/* Implements svldff1 and svldnf1.  */
class svldxf1_impl : public full_width_access
{
public:
  CONSTEXPR svldxf1_impl (int unspec) : m_unspec (unspec) {}

  unsigned int
  call_properties (const function_instance &) const override
  {
    return CP_READ_MEMORY | CP_READ_FFR | CP_WRITE_FFR;
  }

  rtx
  expand (function_expander &e) const override
  {
    /* See the block comment in aarch64-sve.md for details about the
       FFR handling.  */
    emit_insn (gen_aarch64_update_ffr_for_load ());

    machine_mode mode = e.vector_mode (0);
    return e.use_contiguous_load_insn (code_for_aarch64_ldf1 (m_unspec, mode));
  }

  /* The unspec associated with the load.  */
  int m_unspec;
};

/* Implements extending contiguous forms of svldff1 and svldnf1.  */
class svldxf1_extend_impl : public extending_load
{
public:
  CONSTEXPR svldxf1_extend_impl (type_suffix_index memory_type, int unspec)
    : extending_load (memory_type), m_unspec (unspec) {}

  unsigned int
  call_properties (const function_instance &) const override
  {
    return CP_READ_MEMORY | CP_READ_FFR | CP_WRITE_FFR;
  }

  rtx
  expand (function_expander &e) const override
  {
    /* See the block comment in aarch64-sve.md for details about the
       FFR handling.  */
    emit_insn (gen_aarch64_update_ffr_for_load ());

    insn_code icode = code_for_aarch64_ldf1 (m_unspec, extend_rtx_code (),
					     e.vector_mode (0),
					     e.memory_vector_mode ());
    return e.use_contiguous_load_insn (icode);
  }

  /* The unspec associated with the load.  */
  int m_unspec;
};

class svlen_impl : public quiet<function_base>
{
public:
  gimple *
  fold (gimple_folder &f) const override
  {
    /* The argument only exists for its type.  */
    tree rhs_type = TREE_TYPE (gimple_call_arg (f.call, 0));
    tree count = build_int_cstu (TREE_TYPE (f.lhs),
				 TYPE_VECTOR_SUBPARTS (rhs_type));
    return gimple_build_assign (f.lhs, count);
  }

  rtx
  expand (function_expander &e) const override
  {
    /* The argument only exists for its type.  */
    return gen_int_mode (GET_MODE_NUNITS (e.vector_mode (0)), DImode);
  }
};

class svmad_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    return expand_mad (e);
  }
};

class svmla_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    /* Put the accumulator at the end (argument 3), but keep it as the
       merge input for _m functions.  */
    e.rotate_inputs_left (1, 4);
    return expand_mad (e, 3);
  }
};

class svmla_lane_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    if (e.type_suffix (0).integer_p)
      {
	machine_mode mode = e.vector_mode (0);
	return e.use_exact_insn (code_for_aarch64_sve_add_mul_lane (mode));
      }
    return expand_mla_mls_lane (e, UNSPEC_FMLA);
  }
};

class svmls_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    /* Put the accumulator at the end (argument 3), but keep it as the
       merge input for _m functions.  */
    e.rotate_inputs_left (1, 4);
    return expand_msb (e, 3);
  }
};

class svmov_impl : public function_base
{
public:
  gimple *
  fold (gimple_folder &f) const override
  {
    return gimple_build_assign (f.lhs, BIT_AND_EXPR,
				gimple_call_arg (f.call, 0),
				gimple_call_arg (f.call, 1));
  }

  rtx
  expand (function_expander &e) const override
  {
    /* The canonical form for the assembler alias "MOV Pa.B, Pb/Z, Pc.B"
       is "AND Pa.B, Pb/Z, Pc.B, Pc.B".  */
    gcc_assert (e.pred == PRED_z);
    e.args.quick_push (e.args[1]);
    return e.use_exact_insn (CODE_FOR_aarch64_pred_andvnx16bi_z);
  }
};

class svmls_lane_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    if (e.type_suffix (0).integer_p)
      {
	machine_mode mode = e.vector_mode (0);
	return e.use_exact_insn (code_for_aarch64_sve_sub_mul_lane (mode));
      }
    return expand_mla_mls_lane (e, UNSPEC_FMLS);
  }
};

class svmmla_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    insn_code icode;
    if (e.type_suffix (0).integer_p)
      {
	if (e.type_suffix (0).unsigned_p)
	  icode = code_for_aarch64_sve_add (UNSPEC_UMATMUL, e.vector_mode (0));
	else
	  icode = code_for_aarch64_sve_add (UNSPEC_SMATMUL, e.vector_mode (0));
      }
    else
      icode = code_for_aarch64_sve (UNSPEC_FMMLA, e.vector_mode (0));
    return e.use_exact_insn (icode);
  }
};

class svmsb_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    return expand_msb (e);
  }
};

class svmul_impl : public rtx_code_function
{
public:
  CONSTEXPR svmul_impl ()
    : rtx_code_function (MULT, MULT, UNSPEC_COND_FMUL) {}

  gimple *
  fold (gimple_folder &f) const override
  {
    if (auto *res = f.fold_const_binary (MULT_EXPR))
      return res;

    /* If one of the operands is all zeros, fold to zero vector.  */
    tree op1 = gimple_call_arg (f.call, 1);
    if (integer_zerop (op1))
      return gimple_build_assign (f.lhs, op1);

    tree pg = gimple_call_arg (f.call, 0);
    tree op2 = gimple_call_arg (f.call, 2);
    if (integer_zerop (op2)
	&& (f.pred != PRED_m
	    || is_ptrue (pg, f.type_suffix (0).element_bytes)))
      return gimple_build_assign (f.lhs, build_zero_cst (TREE_TYPE (f.lhs)));

    /* If one of the operands is a uniform power of 2, fold to a left shift
       by immediate.  */
    tree op1_cst = uniform_integer_cst_p (op1);
    tree op2_cst = uniform_integer_cst_p (op2);
    tree shift_op1, shift_op2;
    if (op1_cst && integer_pow2p (op1_cst)
	&& (f.pred != PRED_m
	    || is_ptrue (pg, f.type_suffix (0).element_bytes)))
      {
	shift_op1 = op2;
	shift_op2 = op1_cst;
      }
    else if (op2_cst && integer_pow2p (op2_cst))
      {
	shift_op1 = op1;
	shift_op2 = op2_cst;
      }
    else
      return NULL;

    if (integer_onep (shift_op2))
      return NULL;

    shift_op2 = wide_int_to_tree (unsigned_type_for (TREE_TYPE (shift_op2)),
				  tree_log2 (shift_op2));
    function_instance instance ("svlsl", functions::svlsl,
				shapes::binary_uint_opt_n, MODE_n,
				f.type_suffix_ids, GROUP_none, f.pred);
    gcall *call = f.redirect_call (instance);
    gimple_call_set_arg (call, 1, shift_op1);
    gimple_call_set_arg (call, 2, shift_op2);
    return call;
  }
};

class svnand_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    gcc_assert (e.pred == PRED_z);
    return e.use_exact_insn (CODE_FOR_aarch64_pred_nandvnx16bi_z);
  }
};

class svnor_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    gcc_assert (e.pred == PRED_z);
    return e.use_exact_insn (CODE_FOR_aarch64_pred_norvnx16bi_z);
  }
};

class svnot_impl : public rtx_code_function
{
public:
  CONSTEXPR svnot_impl () : rtx_code_function (NOT, NOT, -1) {}

  rtx
  expand (function_expander &e) const override
  {
    if (e.type_suffix_ids[0] == TYPE_SUFFIX_b)
      {
	/* The canonical form for the assembler alias "NOT Pa.B, Pb/Z, Pc.B"
	   is "EOR Pa.B, Pb/Z, Pb.B, Pc.B".  */
	gcc_assert (e.pred == PRED_z);
	e.args.quick_insert (1, e.args[0]);
	return e.use_exact_insn (CODE_FOR_aarch64_pred_xorvnx16bi_z);
      }
    return rtx_code_function::expand (e);
  }
};

class svorn_impl : public function_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    gcc_assert (e.pred == PRED_z);
    return e.use_exact_insn (CODE_FOR_aarch64_pred_ornvnx16bi_z);
  }
};

class svpfalse_impl : public function_base
{
public:
  gimple *
  fold (gimple_folder &f) const override
  {
    if (f.type_suffix (0).tclass == TYPE_bool)
      return f.fold_to_pfalse ();

    return nullptr;
  }

  rtx
  expand (function_expander &) const override
  {
    return CONST0_RTX (VNx16BImode);
  }
};

/* Implements svpfirst and svpnext, which share the same .md patterns.  */
class svpfirst_svpnext_impl : public function_base
{
public:
  CONSTEXPR svpfirst_svpnext_impl (int unspec) : m_unspec (unspec) {}

  rtx
  expand (function_expander &e) const override
  {
    machine_mode mode = e.vector_mode (0);
    e.add_ptrue_hint (0, mode);
    return e.use_exact_insn (code_for_aarch64_sve (m_unspec, mode));
  }

  /* The unspec associated with the operation.  */
  int m_unspec;
};

/* Implements contiguous forms of svprf[bhwd].  */
class svprf_bhwd_impl : public function_base
{
public:
  CONSTEXPR svprf_bhwd_impl (machine_mode mode) : m_mode (mode) {}

  unsigned int
  call_properties (const function_instance &) const override
  {
    return CP_PREFETCH_MEMORY;
  }

  rtx
  expand (function_expander &e) const override
  {
    e.prepare_prefetch_operands ();
    insn_code icode = code_for_aarch64_sve_prefetch (m_mode);
    return e.use_contiguous_prefetch_insn (icode);
  }

  /* The mode that we'd use to hold one vector of prefetched data.  */
  machine_mode m_mode;
};

/* Implements svprf[bhwd]_gather.  */
class svprf_bhwd_gather_impl : public function_base
{
public:
  CONSTEXPR svprf_bhwd_gather_impl (machine_mode mode) : m_mode (mode) {}

  unsigned int
  call_properties (const function_instance &) const override
  {
    return CP_PREFETCH_MEMORY;
  }

  machine_mode
  memory_vector_mode (const function_instance &) const override
  {
    return m_mode;
  }

  rtx
  expand (function_expander &e) const override
  {
    e.prepare_prefetch_operands ();
    e.prepare_gather_address_operands (1);

    /* Insert a zero operand to identify the mode of the memory being
       accessed.  This goes between the gather operands and prefetch
       operands created above.  */
    e.args.quick_insert (5, CONST0_RTX (m_mode));

    machine_mode reg_mode = GET_MODE (e.args[2]);
    insn_code icode = code_for_aarch64_sve_gather_prefetch (m_mode, reg_mode);
    return e.use_exact_insn (icode);
  }

  /* The mode that we'd use to hold one vector of prefetched data.  */
  machine_mode m_mode;
};

/* Implements svptest_any, svptest_first and svptest_last.  */
class svptest_impl : public function_base
{
public:
  CONSTEXPR svptest_impl (rtx_code compare) : m_compare (compare) {}

  rtx
  expand (function_expander &e) const override
  {
    /* See whether GP is an exact ptrue for some predicate mode;
       i.e. whether converting the GP to that mode will not drop
       set bits and will leave all significant bits set.  */
    machine_mode wide_mode;
    int hint;
    if (aarch64_ptrue_all_mode (e.args[0]).exists (&wide_mode))
      hint = SVE_KNOWN_PTRUE;
    else
      {
	hint = SVE_MAYBE_NOT_PTRUE;
	wide_mode = VNx16BImode;
      }

    /* Generate the PTEST itself.  */
    rtx pg = force_reg (VNx16BImode, e.args[0]);
    rtx wide_pg = gen_lowpart (wide_mode, pg);
    rtx hint_rtx = gen_int_mode (hint, DImode);
    rtx op = force_reg (wide_mode, gen_lowpart (wide_mode, e.args[1]));
    emit_insn (gen_aarch64_ptestvnx16bi (pg, wide_pg, hint_rtx, op));

    /* Get the location of the boolean result.  We can provide SImode and
       DImode values directly; rely on generic code to convert others.  */
    rtx target = e.possible_target;
    if (!target
	|| !REG_P (target)
	|| (GET_MODE (target) != SImode && GET_MODE (target) != DImode))
      target = gen_reg_rtx (DImode);

    /* Generate a CSET to convert the CC result of the PTEST to a boolean.  */
    rtx cc_reg = gen_rtx_REG (CC_NZCmode, CC_REGNUM);
    rtx compare = gen_rtx_fmt_ee (m_compare, GET_MODE (target),
				  cc_reg, const0_rtx);
    emit_insn (gen_rtx_SET (target, compare));
    return target;
  }

  /* The comparison code associated with ptest condition.  */
  rtx_code m_compare;
};

class svptrue_impl : public function_base
{
public:
  gimple *
  fold (gimple_folder &f) const override
  {
    if (f.type_suffix (0).tclass == TYPE_bool)
      return f.fold_to_ptrue ();

    return nullptr;
  }

  rtx
  expand (function_expander &e) const override
  {
    if (e.type_suffix (0).tclass == TYPE_bool)
      return aarch64_ptrue_all (e.type_suffix (0).element_bytes);

    auto bits = e.type_suffix (0).element_bits;
    return e.use_exact_insn (code_for_aarch64_sve_ptrue_c (bits));
  }
};

class svptrue_pat_impl : public function_base
{
public:
  gimple *
  fold (gimple_folder &f) const override
  {
    tree pattern_arg = gimple_call_arg (f.call, 0);
    aarch64_svpattern pattern = (aarch64_svpattern) tree_to_shwi (pattern_arg);

    if (pattern == AARCH64_SV_ALL)
      /* svptrue_pat_bN (SV_ALL) == svptrue_bN ().  */
      return f.fold_to_ptrue ();

    /* See whether we can count the number of elements in the pattern
       at compile time.  If so, construct a predicate with that number
       of 1s followed by all 0s.  */
    int nelts_per_vq = f.elements_per_vq (0);
    HOST_WIDE_INT value = aarch64_fold_sve_cnt_pat (pattern, nelts_per_vq);
    if (value >= 0)
      return f.fold_to_vl_pred (value);

    return NULL;
  }

  rtx
  expand (function_expander &e) const override
  {
    /* In rtl, the predicate is represented as the constant:

         (const:V16BI (unspec:V16BI [(const_int PATTERN)
				     (const_vector:VnnBI [zeros])]
				    UNSPEC_PTRUE))

       where nn determines the element size.  */
    rtvec vec = gen_rtvec (2, e.args[0], CONST0_RTX (e.vector_mode (0)));
    return gen_rtx_CONST (VNx16BImode,
			  gen_rtx_UNSPEC (VNx16BImode, vec, UNSPEC_PTRUE));
  }
};

/* Implements svqdec[bhwd]{,_pat} and svqinc[bhwd]{,_pat}.  */
class svqdec_svqinc_bhwd_impl : public function_base
{
public:
  CONSTEXPR svqdec_svqinc_bhwd_impl (rtx_code code_for_sint,
				     rtx_code code_for_uint,
				     scalar_int_mode elem_mode)
    : m_code_for_sint (code_for_sint),
      m_code_for_uint (code_for_uint),
      m_elem_mode (elem_mode)
  {}

  rtx
  expand (function_expander &e) const override
  {
    /* Treat non-_pat functions in the same way as _pat functions with
       an SV_ALL argument.  */
    if (e.args.length () == 2)
      e.args.quick_insert (1, gen_int_mode (AARCH64_SV_ALL, DImode));

    /* Insert the number of elements per 128-bit block as a fake argument,
       between the pattern and the multiplier.  Arguments 1, 2 and 3 then
       correspond exactly with the 3 UNSPEC_SVE_CNT_PAT operands; see
       aarch64_sve_cnt_pat for details.  */
    unsigned int elements_per_vq = 128 / GET_MODE_BITSIZE (m_elem_mode);
    e.args.quick_insert (2, gen_int_mode (elements_per_vq, DImode));

    rtx_code code = (e.type_suffix (0).unsigned_p
		     ? m_code_for_uint
		     : m_code_for_sint);

    /* Choose between operating on integer scalars or integer vectors.  */
    machine_mode mode = e.vector_mode (0);
    if (e.mode_suffix_id == MODE_n)
      mode = GET_MODE_INNER (mode);
    return e.use_exact_insn (code_for_aarch64_sve_pat (code, mode));
  }

  /* The saturating addition or subtraction codes to use for signed and
     unsigned values respectively.  */
  rtx_code m_code_for_sint;
  rtx_code m_code_for_uint;

  /* The integer mode associated with the [bhwd] suffix.  */
  scalar_int_mode m_elem_mode;
};

/* Implements svqdec[bhwd]{,_pat}.  */
class svqdec_bhwd_impl : public svqdec_svqinc_bhwd_impl
{
public:
  CONSTEXPR svqdec_bhwd_impl (scalar_int_mode elem_mode)
    : svqdec_svqinc_bhwd_impl (SS_MINUS, US_MINUS, elem_mode) {}
};

/* Implements svqinc[bhwd]{,_pat}.  */
class svqinc_bhwd_impl : public svqdec_svqinc_bhwd_impl
{
public:
  CONSTEXPR svqinc_bhwd_impl (scalar_int_mode elem_mode)
    : svqdec_svqinc_bhwd_impl (SS_PLUS, US_PLUS, elem_mode) {}
};

/* Implements svqdecp and svqincp.  */
class svqdecp_svqincp_impl : public function_base
{
public:
  CONSTEXPR svqdecp_svqincp_impl (rtx_code code_for_sint,
				  rtx_code code_for_uint)
    : m_code_for_sint (code_for_sint),
      m_code_for_uint (code_for_uint)
  {}

  rtx
  expand (function_expander &e) const override
  {
    rtx_code code = (e.type_suffix (0).unsigned_p
		     ? m_code_for_uint
		     : m_code_for_sint);
    insn_code icode;
    if (e.mode_suffix_id == MODE_n)
      {
	/* Increment or decrement a scalar (whose mode is given by the first
	   type suffix) by the number of active elements in a predicate
	   (whose mode is given by the second type suffix).  */
	machine_mode mode = GET_MODE_INNER (e.vector_mode (0));
	icode = code_for_aarch64_sve_cntp (code, mode, e.vector_mode (1));
      }
    else
      /* Increment a vector by the number of active elements in a predicate,
	 with the vector mode determining the predicate mode.  */
      icode = code_for_aarch64_sve_cntp (code, e.vector_mode (0));
    return e.use_exact_insn (icode);
  }

  /* The saturating addition or subtraction codes to use for signed and
     unsigned values respectively.  */
  rtx_code m_code_for_sint;
  rtx_code m_code_for_uint;
};

class svrdffr_impl : public function_base
{
public:
  unsigned int
  call_properties (const function_instance &) const override
  {
    return CP_READ_FFR;
  }

  rtx
  expand (function_expander &e) const override
  {
    /* See the block comment in aarch64-sve.md for details about the
       FFR handling.  */
    emit_insn (gen_aarch64_copy_ffr_to_ffrt ());
    rtx result = e.use_exact_insn (e.pred == PRED_z
				   ? CODE_FOR_aarch64_rdffr_z
				   : CODE_FOR_aarch64_rdffr);
    emit_insn (gen_aarch64_update_ffrt ());
    return result;
  }
};

class svreinterpret_impl : public quiet<function_base>
{
public:
  gimple *
  fold (gimple_folder &f) const override
  {
    if (f.vectors_per_tuple () > 1)
      return NULL;

    /* Punt to rtl if the effect of the reinterpret on registers does not
       conform to GCC's endianness model.  */
    if (GET_MODE_CLASS (f.vector_mode (0)) != MODE_VECTOR_BOOL
	&& !targetm.can_change_mode_class (f.vector_mode (0),
					   f.vector_mode (1), FP_REGS))
      return NULL;

    /* Otherwise svreinterpret corresponds directly to a VIEW_CONVERT_EXPR
       reinterpretation.  */
    tree rhs = build1 (VIEW_CONVERT_EXPR, TREE_TYPE (f.lhs),
		       gimple_call_arg (f.call, 0));
    return gimple_build_assign (f.lhs, VIEW_CONVERT_EXPR, rhs);
  }

  rtx
  expand (function_expander &e) const override
  {
    machine_mode mode = e.tuple_mode (0);
    /* Handle svbool_t <-> svcount_t.  */
    if (mode == e.tuple_mode (1))
      return e.args[0];
    return e.use_exact_insn (code_for_aarch64_sve_reinterpret (mode));
  }
};

class svrev_impl : public permute
{
public:
  gimple *
  fold (gimple_folder &f) const override
  {
    /* Punt for now on _b16 and wider; we'd need more complex evpc logic
       to rerecognize the result.  */
    if (f.type_suffix (0).bool_p && f.type_suffix (0).element_bits > 8)
      return NULL;

    /* Permute as { nelts - 1, nelts - 2, nelts - 3, ... }.  */
    poly_int64 nelts = TYPE_VECTOR_SUBPARTS (TREE_TYPE (f.lhs));
    vec_perm_builder builder (nelts, 1, 3);
    for (int i = 0; i < 3; ++i)
      builder.quick_push (nelts - i - 1);
    return fold_permute (f, builder);
  }

  rtx
  expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_aarch64_sve_rev (e.vector_mode (0)));
  }
};

class svrint_impl : public function_base
{
public:
  CONSTEXPR svrint_impl (optab_tag optab, int cond_unspec)
    : m_optab (optab), m_cond_unspec (cond_unspec)
  {}

  rtx
  expand (function_expander &e) const override
  {
    if (e.pred == PRED_none)
      {
	auto icode = direct_optab_handler (m_optab, e.tuple_mode (0));
	return e.use_exact_insn (icode);
      }
    return e.map_to_unspecs (-1, -1, m_cond_unspec);
  }

  optab_tag m_optab;
  int m_cond_unspec;
};

class svsel_impl : public quiet<function_base>
{
public:
  gimple *
  fold (gimple_folder &f) const override
  {
    if (f.vectors_per_tuple () > 1)
      return nullptr;

    /* svsel corresponds exactly to VEC_COND_EXPR.  */
    gimple_seq stmts = NULL;
    tree pred = f.convert_pred (stmts, f.vector_type (0), 0);
    gsi_insert_seq_before (f.gsi, stmts, GSI_SAME_STMT);
    return gimple_build_assign (f.lhs, VEC_COND_EXPR, pred,
				gimple_call_arg (f.call, 1),
				gimple_call_arg (f.call, 2));
  }

  rtx
  expand (function_expander &e) const override
  {
    /* svsel (cond, truev, falsev) is vcond_mask (truev, falsev, cond).  */
    e.rotate_inputs_left (0, 3);
    insn_code icode = (e.vectors_per_tuple () > 1
		       ? code_for_aarch64_sve_sel (e.tuple_mode (0))
		       : convert_optab_handler (vcond_mask_optab,
						e.vector_mode (0),
						e.gp_mode (0)));
    return e.use_exact_insn (icode);
  }
};

/* Implements svset2, svset3 and svset4.  */
class svset_impl : public quiet<multi_vector_function>
{
public:
  using quiet<multi_vector_function>::quiet;

  gimple *
  fold (gimple_folder &f) const override
  {
    tree rhs_tuple = gimple_call_arg (f.call, 0);
    tree index = gimple_call_arg (f.call, 1);
    tree rhs_vector = gimple_call_arg (f.call, 2);

    /* Replace the call with two statements: a copy of the full tuple
       to the call result, followed by an update of the individual vector.

       The fold routines expect the replacement statement to have the
       same lhs as the original call, so return the copy statement
       rather than the field update.  */
    gassign *copy = gimple_build_assign (unshare_expr (f.lhs), rhs_tuple);

    /* Get a reference to the individual vector.  */
    tree field = tuple_type_field (TREE_TYPE (f.lhs));
    tree lhs_array = build3 (COMPONENT_REF, TREE_TYPE (field),
			     f.lhs, field, NULL_TREE);
    tree lhs_vector = build4 (ARRAY_REF, TREE_TYPE (rhs_vector),
			      lhs_array, index, NULL_TREE, NULL_TREE);
    gassign *update = gimple_build_assign (lhs_vector, rhs_vector);
    gsi_insert_after (f.gsi, update, GSI_SAME_STMT);

    return copy;
  }

  rtx
  expand (function_expander &e) const override
  {
    rtx rhs_tuple = e.args[0];
    unsigned int index = INTVAL (e.args[1]);
    rtx rhs_vector = e.args[2];

    /* First copy the full tuple to the target register.  */
    rtx lhs_tuple = e.get_nonoverlapping_reg_target ();
    emit_move_insn (lhs_tuple, rhs_tuple);

    /* ...then update the individual vector.  */
    rtx lhs_vector = simplify_gen_subreg (GET_MODE (rhs_vector),
					  lhs_tuple, GET_MODE (lhs_tuple),
					  index * BYTES_PER_SVE_VECTOR);
    emit_move_insn (lhs_vector, rhs_vector);
    return lhs_vector;
  }
};

class svsetffr_impl : public function_base
{
public:
  unsigned int
  call_properties (const function_instance &) const override
  {
    return CP_WRITE_FFR;
  }

  rtx
  expand (function_expander &e) const override
  {
    e.args.quick_push (CONSTM1_RTX (VNx16BImode));
    return e.use_exact_insn (CODE_FOR_aarch64_wrffr);
  }
};

class svst1_impl : public full_width_access
{
public:
  unsigned int
  call_properties (const function_instance &) const override
  {
    return CP_WRITE_MEMORY;
  }

  gimple *
  fold (gimple_folder &f) const override
  {
    if (f.vectors_per_tuple () != 1)
      return nullptr;

    tree vectype = f.vector_type (0);

    /* Get the predicate and base pointer.  */
    gimple_seq stmts = NULL;
    tree pred = f.convert_pred (stmts, vectype, 0);
    tree base = f.fold_contiguous_base (stmts, vectype);
    gsi_insert_seq_before (f.gsi, stmts, GSI_SAME_STMT);

    tree cookie = f.load_store_cookie (TREE_TYPE (vectype));
    tree rhs = gimple_call_arg (f.call, gimple_call_num_args (f.call) - 1);
    return gimple_build_call_internal (IFN_MASK_STORE, 4,
				       base, cookie, pred, rhs);
  }

  rtx
  expand (function_expander &e) const override
  {
    insn_code icode;
    if (e.vectors_per_tuple () == 1)
      icode = convert_optab_handler (maskstore_optab,
				     e.vector_mode (0), e.gp_mode (0));
    else
      icode = code_for_aarch64 (UNSPEC_ST1_COUNT, e.tuple_mode (0));
    return e.use_contiguous_store_insn (icode);
  }
};

class svst1_scatter_impl : public full_width_access
{
public:
  unsigned int
  call_properties (const function_instance &) const override
  {
    return CP_WRITE_MEMORY;
  }

  rtx
  expand (function_expander &e) const override
  {
    e.prepare_gather_address_operands (1);
    /* Put the predicate last, as required by mask_scatter_store_optab.  */
    e.rotate_inputs_left (0, 6);
    machine_mode mem_mode = e.memory_vector_mode ();
    machine_mode int_mode = aarch64_sve_int_mode (mem_mode);
    insn_code icode = convert_optab_handler (mask_scatter_store_optab,
					     mem_mode, int_mode);
    return e.use_exact_insn (icode);
  }
};

/* Implements truncating forms of svst1_scatter.  */
class svst1_scatter_truncate_impl : public truncating_store
{
public:
  using truncating_store::truncating_store;

  rtx
  expand (function_expander &e) const override
  {
    e.prepare_gather_address_operands (1);
    /* Put the predicate last, since the truncating scatters use the same
       operand order as mask_scatter_store_optab.  */
    e.rotate_inputs_left (0, 6);
    insn_code icode = code_for_aarch64_scatter_store_trunc
      (e.memory_vector_mode (), e.vector_mode (0));
    return e.use_exact_insn (icode);
  }
};

/* Implements truncating contiguous forms of svst1.  */
class svst1_truncate_impl : public truncating_store
{
public:
  using truncating_store::truncating_store;

  rtx
  expand (function_expander &e) const override
  {
    insn_code icode = code_for_aarch64_store_trunc (e.memory_vector_mode (),
						    e.vector_mode (0));
    return e.use_contiguous_store_insn (icode);
  }
};

/* Implements svst2, svst3 and svst4.  */
class svst234_impl : public full_width_access
{
public:
  using full_width_access::full_width_access;

  unsigned int
  call_properties (const function_instance &) const override
  {
    return CP_WRITE_MEMORY;
  }

  gimple *
  fold (gimple_folder &f) const override
  {
    tree vectype = f.vector_type (0);

    /* Get the predicate and base pointer.  */
    gimple_seq stmts = NULL;
    tree pred = f.convert_pred (stmts, vectype, 0);
    tree base = f.fold_contiguous_base (stmts, vectype);
    gsi_insert_seq_before (f.gsi, stmts, GSI_SAME_STMT);

    /* View the stored data as an array of vectors.  */
    unsigned int num_args = gimple_call_num_args (f.call);
    tree rhs_tuple = gimple_call_arg (f.call, num_args - 1);
    tree field = tuple_type_field (TREE_TYPE (rhs_tuple));
    tree rhs_array = build1 (VIEW_CONVERT_EXPR, TREE_TYPE (field), rhs_tuple);

    tree cookie = f.load_store_cookie (TREE_TYPE (vectype));
    return gimple_build_call_internal (IFN_MASK_STORE_LANES, 4,
				       base, cookie, pred, rhs_array);
  }

  rtx
  expand (function_expander &e) const override
  {
    machine_mode tuple_mode = GET_MODE (e.args.last ());
    insn_code icode = convert_optab_handler (vec_mask_store_lanes_optab,
					     tuple_mode, e.vector_mode (0));
    return e.use_contiguous_store_insn (icode);
  }
};

class svstnt1_impl : public full_width_access
{
public:
  unsigned int
  call_properties (const function_instance &) const override
  {
    return CP_WRITE_MEMORY;
  }

  rtx
  expand (function_expander &e) const override
  {
    insn_code icode = (e.vectors_per_tuple () == 1
		       ? code_for_aarch64_stnt1 (e.vector_mode (0))
		       : code_for_aarch64 (UNSPEC_STNT1_COUNT,
					   e.tuple_mode (0)));
    return e.use_contiguous_store_insn (icode);
  }
};

class svsub_impl : public rtx_code_function
{
public:
  CONSTEXPR svsub_impl ()
    : rtx_code_function (MINUS, MINUS, UNSPEC_COND_FSUB) {}

  rtx
  expand (function_expander &e) const override
  {
    /* Canonicalize subtractions of constants to additions.  */
    machine_mode mode = e.vector_mode (0);
    if (e.try_negating_argument (2, mode))
      return e.map_to_rtx_codes (PLUS, PLUS, UNSPEC_COND_FADD, -1);

    return rtx_code_function::expand (e);
  }
};

class svtbl_impl : public permute
{
public:
  rtx
  expand (function_expander &e) const override
  {
    return e.use_exact_insn (code_for_aarch64_sve_tbl (e.vector_mode (0)));
  }
};

/* Implements svtrn1 and svtrn2.  */
class svtrn_impl : public binary_permute
{
public:
  CONSTEXPR svtrn_impl (int base)
    : binary_permute (base ? UNSPEC_TRN2 : UNSPEC_TRN1), m_base (base) {}

  gimple *
  fold (gimple_folder &f) const override
  {
    /* svtrn1: { 0, nelts, 2, nelts + 2, 4, nelts + 4, ... }
       svtrn2: as for svtrn1, but with 1 added to each index.  */
    poly_uint64 nelts = TYPE_VECTOR_SUBPARTS (TREE_TYPE (f.lhs));
    vec_perm_builder builder (nelts, 2, 3);
    for (unsigned int i = 0; i < 3; ++i)
      {
	builder.quick_push (m_base + i * 2);
	builder.quick_push (m_base + i * 2 + nelts);
      }
    return fold_permute (f, builder);
  }

  /* 0 for svtrn1, 1 for svtrn2.  */
  unsigned int m_base;
};

/* Base class for svundef{,2,3,4}.  */
class svundef_impl : public quiet<multi_vector_function>
{
public:
  using quiet<multi_vector_function>::quiet;

  rtx
  expand (function_expander &e) const override
  {
    rtx target = e.get_reg_target ();
    emit_clobber (copy_rtx (target));
    return target;
  }
};

/* Implements svunpklo and svunpkhi.  */
class svunpk_impl : public quiet<function_base>
{
public:
  CONSTEXPR svunpk_impl (bool high_p) : m_high_p (high_p) {}

  gimple *
  fold (gimple_folder &f) const override
  {
    /* Don't fold the predicate ops, since every bit of the svbool_t
       result is significant.  */
    if (f.type_suffix_ids[0] == TYPE_SUFFIX_b)
      return NULL;

    /* The first half in memory is VEC_UNPACK_LO_EXPR for little-endian
       and VEC_UNPACK_HI_EXPR for big-endian.  */
    bool high_p = BYTES_BIG_ENDIAN ? !m_high_p : m_high_p;
    tree_code code = high_p ? VEC_UNPACK_HI_EXPR : VEC_UNPACK_LO_EXPR;
    return gimple_build_assign (f.lhs, code, gimple_call_arg (f.call, 0));
  }

  rtx
  expand (function_expander &e) const override
  {
    machine_mode mode = GET_MODE (e.args[0]);
    unsigned int unpacku = m_high_p ? UNSPEC_UNPACKUHI : UNSPEC_UNPACKULO;
    unsigned int unpacks = m_high_p ? UNSPEC_UNPACKSHI : UNSPEC_UNPACKSLO;
    insn_code icode;
    if (GET_MODE_CLASS (mode) == MODE_VECTOR_BOOL)
      icode = code_for_aarch64_sve_punpk (unpacku, mode);
    else
      {
	int unspec = e.type_suffix (0).unsigned_p ? unpacku : unpacks;
	icode = code_for_aarch64_sve_unpk (unspec, unspec, mode);
      }
    return e.use_exact_insn (icode);
  }

  /* True for svunpkhi, false for svunpklo.  */
  bool m_high_p;
};

/* Also implements svsudot.  */
class svusdot_impl : public function_base
{
public:
  CONSTEXPR svusdot_impl (bool su) : m_su (su) {}

  rtx
  expand (function_expander &e) const override
  {
    /* The implementation of the ACLE function svsudot (for the non-lane
       version) is through the USDOT instruction but with the second and third
       inputs swapped.  */
    if (m_su)
      e.rotate_inputs_left (1, 3);
    /* The ACLE function has the same order requirements as for svdot.
       While there's no requirement for the RTL pattern to have the same sort
       of order as that for <sur>dot_prod, it's easier to read.
       Hence we do the same rotation on arguments as svdot_impl does.  */
    e.rotate_inputs_left (0, 3);
    machine_mode mode = e.vector_mode (0);
    insn_code icode = code_for_dot_prod (UNSPEC_USDOT, e.result_mode (), mode);
    return e.use_exact_insn (icode);
  }

private:
  bool m_su;
};

/* Implements svuzp1 and svuzp2.  */
class svuzp_impl : public binary_permute
{
public:
  CONSTEXPR svuzp_impl (unsigned int base)
    : binary_permute (base ? UNSPEC_UZP2 : UNSPEC_UZP1), m_base (base) {}

  gimple *
  fold (gimple_folder &f) const override
  {
    /* svuzp1: { 0, 2, 4, 6, ... }
       svuzp2: { 1, 3, 5, 7, ... }.  */
    poly_uint64 nelts = TYPE_VECTOR_SUBPARTS (TREE_TYPE (f.lhs));
    vec_perm_builder builder (nelts, 1, 3);
    for (unsigned int i = 0; i < 3; ++i)
      builder.quick_push (m_base + i * 2);
    return fold_permute (f, builder);
  }

  /* 0 for svuzp1, 1 for svuzp2.  */
  unsigned int m_base;
};

/* A function_base for svwhilele and svwhilelt functions.  */
class svwhilelx_impl : public while_comparison
{
public:
  CONSTEXPR svwhilelx_impl (int unspec_for_sint, int unspec_for_uint, bool eq_p)
    : while_comparison (unspec_for_sint, unspec_for_uint), m_eq_p (eq_p)
  {}

  /* Try to fold a call by treating its arguments as constants of type T.
     We have already filtered out the degenerate cases of X .LT. MIN
     and X .LE. MAX.  */
  template<typename T>
  gimple *
  fold_type (gimple_folder &f) const
  {
    /* Only handle cases in which both operands are constant.  */
    T arg0, arg1;
    if (!poly_int_tree_p (gimple_call_arg (f.call, 0), &arg0)
	|| !poly_int_tree_p (gimple_call_arg (f.call, 1), &arg1))
      return NULL;

    /* Check whether the result is known to be all-false.  */
    if (m_eq_p ? known_gt (arg0, arg1) : known_ge (arg0, arg1))
      return f.fold_to_pfalse ();

    /* Punt if we can't tell at compile time whether the result
       is all-false.  */
    if (m_eq_p ? maybe_gt (arg0, arg1) : maybe_ge (arg0, arg1))
      return NULL;

    /* At this point we know the result has at least one set element.  */
    poly_uint64 diff = arg1 - arg0;
    poly_uint64 nelts = GET_MODE_NUNITS (f.vector_mode (0));

    /* Canonicalize the svwhilele form to the svwhilelt form.  Subtract
       from NELTS rather than adding to DIFF, to prevent overflow.  */
    if (m_eq_p)
      nelts -= 1;

    /* Check whether the result is known to be all-true.  */
    if (known_ge (diff, nelts))
      return f.fold_to_ptrue ();

    /* Punt if DIFF might not be the actual number of set elements
       in the result.  Conditional equality is fine.  */
    if (maybe_gt (diff, nelts))
      return NULL;

    /* At this point we know that the predicate will have DIFF set elements
       for svwhilelt and DIFF + 1 set elements for svwhilele (which stops
       after rather than before ARG1 is reached).  See if we can create
       the predicate at compile time.  */
    unsigned HOST_WIDE_INT vl;
    if (diff.is_constant (&vl))
      /* Overflow is no longer possible after the checks above.  */
      return f.fold_to_vl_pred (m_eq_p ? vl + 1 : vl);

    return NULL;
  }

  gimple *
  fold (gimple_folder &f) const override
  {
    if (f.vectors_per_tuple () > 1)
      return nullptr;

    /* Filter out cases where the condition is always true or always false.  */
    tree arg1 = gimple_call_arg (f.call, 1);
    if (!m_eq_p && operand_equal_p (arg1, TYPE_MIN_VALUE (TREE_TYPE (arg1))))
      return f.fold_to_pfalse ();
    if (m_eq_p && operand_equal_p (arg1, TYPE_MAX_VALUE (TREE_TYPE (arg1))))
      return f.fold_to_ptrue ();

    if (f.type_suffix (1).unsigned_p)
      return fold_type<poly_uint64> (f);
    else
      return fold_type<poly_int64> (f);
  }

  /* True svwhilele, false for svwhilelt.  */
  bool m_eq_p;
};

class svwrffr_impl : public function_base
{
public:
  unsigned int
  call_properties (const function_instance &) const override
  {
    return CP_WRITE_FFR;
  }

  rtx
  expand (function_expander &e) const override
  {
    return e.use_exact_insn (CODE_FOR_aarch64_wrffr);
  }
};

/* Implements svzip1 and svzip2.  */
class svzip_impl : public binary_permute
{
public:
  CONSTEXPR svzip_impl (unsigned int base)
    : binary_permute (base ? UNSPEC_ZIP2 : UNSPEC_ZIP1), m_base (base) {}

  gimple *
  fold (gimple_folder &f) const override
  {
    /* svzip1: { 0, nelts, 1, nelts + 1, 2, nelts + 2, ... }
       svzip2: as for svzip1, but with nelts / 2 added to each index.  */
    poly_uint64 nelts = TYPE_VECTOR_SUBPARTS (TREE_TYPE (f.lhs));
    poly_uint64 base = m_base * exact_div (nelts, 2);
    vec_perm_builder builder (nelts, 2, 3);
    for (unsigned int i = 0; i < 3; ++i)
      {
	builder.quick_push (base + i);
	builder.quick_push (base + i + nelts);
      }
    return fold_permute (f, builder);
  }

  /* 0 for svzip1, 1 for svzip2.  */
  unsigned int m_base;
};

} /* end anonymous namespace */

namespace aarch64_sve {

FUNCTION (svabd, svabd_impl,)
FUNCTION (svabs, quiet<rtx_code_function>, (ABS, ABS, UNSPEC_COND_FABS))
FUNCTION (svacge, svac_impl, (UNSPEC_COND_FCMGE))
FUNCTION (svacgt, svac_impl, (UNSPEC_COND_FCMGT))
FUNCTION (svacle, svac_impl, (UNSPEC_COND_FCMLE))
FUNCTION (svaclt, svac_impl, (UNSPEC_COND_FCMLT))
FUNCTION (svadd, rtx_code_function, (PLUS, PLUS, UNSPEC_COND_FADD))
FUNCTION (svadda, svadda_impl,)
FUNCTION (svaddv, reduction, (UNSPEC_SADDV, UNSPEC_UADDV, UNSPEC_FADDV))
FUNCTION (svadrb, svadr_bhwd_impl, (0))
FUNCTION (svadrd, svadr_bhwd_impl, (3))
FUNCTION (svadrh, svadr_bhwd_impl, (1))
FUNCTION (svadrw, svadr_bhwd_impl, (2))
FUNCTION (svamax, cond_or_uncond_unspec_function,
	  (UNSPEC_COND_FAMAX, UNSPEC_FAMAX))
FUNCTION (svamin, cond_or_uncond_unspec_function,
	  (UNSPEC_COND_FAMIN, UNSPEC_FAMIN))
FUNCTION (svand, rtx_code_function, (AND, AND))
FUNCTION (svandv, reduction, (UNSPEC_ANDV))
FUNCTION (svasr, rtx_code_function, (ASHIFTRT, ASHIFTRT))
FUNCTION (svasr_wide, shift_wide, (ASHIFTRT, UNSPEC_ASHIFTRT_WIDE))
FUNCTION (svasrd, unspec_based_function, (UNSPEC_ASRD, -1, -1))
FUNCTION (svbfdot, fixed_insn_function, (CODE_FOR_aarch64_sve_bfdotvnx4sf))
FUNCTION (svbfdot_lane, fixed_insn_function,
	  (CODE_FOR_aarch64_sve_bfdot_lanevnx4sf))
FUNCTION (svbfmlalb, fixed_insn_function, (CODE_FOR_aarch64_sve_bfmlalbvnx4sf))
FUNCTION (svbfmlalb_lane, fixed_insn_function,
	  (CODE_FOR_aarch64_sve_bfmlalb_lanevnx4sf))
FUNCTION (svbfmlalt, fixed_insn_function, (CODE_FOR_aarch64_sve_bfmlaltvnx4sf))
FUNCTION (svbfmlalt_lane, fixed_insn_function,
	  (CODE_FOR_aarch64_sve_bfmlalt_lanevnx4sf))
FUNCTION (svbfmmla, fixed_insn_function, (CODE_FOR_aarch64_sve_bfmmlavnx4sf))
FUNCTION (svbic, svbic_impl,)
FUNCTION (svbrka, svbrk_unary_impl, (UNSPEC_BRKA))
FUNCTION (svbrkb, svbrk_unary_impl, (UNSPEC_BRKB))
FUNCTION (svbrkn, svbrk_binary_impl, (UNSPEC_BRKN))
FUNCTION (svbrkpa, svbrk_binary_impl, (UNSPEC_BRKPA))
FUNCTION (svbrkpb, svbrk_binary_impl, (UNSPEC_BRKPB))
FUNCTION (svcadd, svcadd_impl,)
FUNCTION (svclasta, svclast_impl, (UNSPEC_CLASTA))
FUNCTION (svclastb, svclast_impl, (UNSPEC_CLASTB))
FUNCTION (svcls, unary_count, (CLRSB))
FUNCTION (svclz, unary_count, (CLZ))
FUNCTION (svcmla, svcmla_impl,)
FUNCTION (svcmla_lane, svcmla_lane_impl,)
FUNCTION (svcmpeq, svcmp_impl, (EQ_EXPR, UNSPEC_COND_FCMEQ))
FUNCTION (svcmpeq_wide, svcmp_wide_impl, (EQ_EXPR, UNSPEC_COND_CMPEQ_WIDE,
					  UNSPEC_COND_CMPEQ_WIDE))
FUNCTION (svcmpge, svcmp_impl, (GE_EXPR, UNSPEC_COND_FCMGE))
FUNCTION (svcmpge_wide, svcmp_wide_impl, (GE_EXPR, UNSPEC_COND_CMPGE_WIDE,
					  UNSPEC_COND_CMPHS_WIDE))
FUNCTION (svcmpgt, svcmp_impl, (GT_EXPR, UNSPEC_COND_FCMGT))
FUNCTION (svcmpgt_wide, svcmp_wide_impl, (GT_EXPR, UNSPEC_COND_CMPGT_WIDE,
					  UNSPEC_COND_CMPHI_WIDE))
FUNCTION (svcmple, svcmp_impl, (LE_EXPR, UNSPEC_COND_FCMLE))
FUNCTION (svcmple_wide, svcmp_wide_impl, (LE_EXPR, UNSPEC_COND_CMPLE_WIDE,
					  UNSPEC_COND_CMPLS_WIDE))
FUNCTION (svcmplt, svcmp_impl, (LT_EXPR, UNSPEC_COND_FCMLT))
FUNCTION (svcmplt_wide, svcmp_wide_impl, (LT_EXPR, UNSPEC_COND_CMPLT_WIDE,
					  UNSPEC_COND_CMPLO_WIDE))
FUNCTION (svcmpne, svcmp_impl, (NE_EXPR, UNSPEC_COND_FCMNE))
FUNCTION (svcmpne_wide, svcmp_wide_impl, (NE_EXPR, UNSPEC_COND_CMPNE_WIDE,
					  UNSPEC_COND_CMPNE_WIDE))
FUNCTION (svcmpuo, svcmpuo_impl,)
FUNCTION (svcnot, svcnot_impl,)
FUNCTION (svcnt, unary_count, (POPCOUNT))
FUNCTION (svcntb, svcnt_bhwd_impl, (VNx16QImode))
FUNCTION (svcntb_pat, svcnt_bhwd_pat_impl, (VNx16QImode))
FUNCTION (svcntd, svcnt_bhwd_impl, (VNx2DImode))
FUNCTION (svcntd_pat, svcnt_bhwd_pat_impl, (VNx2DImode))
FUNCTION (svcnth, svcnt_bhwd_impl, (VNx8HImode))
FUNCTION (svcnth_pat, svcnt_bhwd_pat_impl, (VNx8HImode))
FUNCTION (svcntp, svcntp_impl,)
FUNCTION (svcntw, svcnt_bhwd_impl, (VNx4SImode))
FUNCTION (svcntw_pat, svcnt_bhwd_pat_impl, (VNx4SImode))
FUNCTION (svcompact, QUIET_CODE_FOR_MODE0 (aarch64_sve_compact),)
FUNCTION (svcreate2, svcreate_impl, (2))
FUNCTION (svcreate3, svcreate_impl, (3))
FUNCTION (svcreate4, svcreate_impl, (4))
FUNCTION (svcvt, svcvt_impl,)
FUNCTION (svcvtnt, CODE_FOR_MODE0 (aarch64_sve_cvtnt),)
FUNCTION (svdiv, svdiv_impl,)
FUNCTION (svdivr, rtx_code_function_rotated, (DIV, UDIV, UNSPEC_COND_FDIV))
FUNCTION (svdot, svdot_impl,)
FUNCTION (svdot_lane, svdotprod_lane_impl, (UNSPEC_SDOT, UNSPEC_UDOT,
					    UNSPEC_FDOT))
FUNCTION (svdup, svdup_impl,)
FUNCTION (svdup_lane, svdup_lane_impl,)
FUNCTION (svdupq, svdupq_impl,)
FUNCTION (svdupq_lane, svdupq_lane_impl,)
FUNCTION (sveor, rtx_code_function, (XOR, XOR, -1))
FUNCTION (sveorv, reduction, (UNSPEC_XORV))
FUNCTION (svexpa, unspec_based_function, (-1, -1, UNSPEC_FEXPA))
FUNCTION (svext, QUIET_CODE_FOR_MODE0 (aarch64_sve_ext),)
FUNCTION (svextb, svext_bhw_impl, (QImode))
FUNCTION (svexth, svext_bhw_impl, (HImode))
FUNCTION (svextw, svext_bhw_impl, (SImode))
FUNCTION (svget2, svget_impl, (2))
FUNCTION (svget3, svget_impl, (3))
FUNCTION (svget4, svget_impl, (4))
FUNCTION (svindex, svindex_impl,)
FUNCTION (svinsr, svinsr_impl,)
FUNCTION (svlasta, svlast_impl, (UNSPEC_LASTA))
FUNCTION (svlastb, svlast_impl, (UNSPEC_LASTB))
FUNCTION (svld1, svld1_impl,)
FUNCTION (svld1_gather, svld1_gather_impl,)
FUNCTION (svld1ro, svld1ro_impl,)
FUNCTION (svld1rq, svld1rq_impl,)
FUNCTION (svld1sb, svld1_extend_impl, (TYPE_SUFFIX_s8))
FUNCTION (svld1sb_gather, svld1_gather_extend_impl, (TYPE_SUFFIX_s8))
FUNCTION (svld1sh, svld1_extend_impl, (TYPE_SUFFIX_s16))
FUNCTION (svld1sh_gather, svld1_gather_extend_impl, (TYPE_SUFFIX_s16))
FUNCTION (svld1sw, svld1_extend_impl, (TYPE_SUFFIX_s32))
FUNCTION (svld1sw_gather, svld1_gather_extend_impl, (TYPE_SUFFIX_s32))
FUNCTION (svld1ub, svld1_extend_impl, (TYPE_SUFFIX_u8))
FUNCTION (svld1ub_gather, svld1_gather_extend_impl, (TYPE_SUFFIX_u8))
FUNCTION (svld1uh, svld1_extend_impl, (TYPE_SUFFIX_u16))
FUNCTION (svld1uh_gather, svld1_gather_extend_impl, (TYPE_SUFFIX_u16))
FUNCTION (svld1uw, svld1_extend_impl, (TYPE_SUFFIX_u32))
FUNCTION (svld1uw_gather, svld1_gather_extend_impl, (TYPE_SUFFIX_u32))
FUNCTION (svld2, svld234_impl, (2))
FUNCTION (svld3, svld234_impl, (3))
FUNCTION (svld4, svld234_impl, (4))
FUNCTION (svldff1, svldxf1_impl, (UNSPEC_LDFF1))
FUNCTION (svldff1_gather, svldff1_gather_impl,)
FUNCTION (svldff1sb, svldxf1_extend_impl, (TYPE_SUFFIX_s8, UNSPEC_LDFF1))
FUNCTION (svldff1sb_gather, svldff1_gather_extend, (TYPE_SUFFIX_s8))
FUNCTION (svldff1sh, svldxf1_extend_impl, (TYPE_SUFFIX_s16, UNSPEC_LDFF1))
FUNCTION (svldff1sh_gather, svldff1_gather_extend, (TYPE_SUFFIX_s16))
FUNCTION (svldff1sw, svldxf1_extend_impl, (TYPE_SUFFIX_s32, UNSPEC_LDFF1))
FUNCTION (svldff1sw_gather, svldff1_gather_extend, (TYPE_SUFFIX_s32))
FUNCTION (svldff1ub, svldxf1_extend_impl, (TYPE_SUFFIX_u8, UNSPEC_LDFF1))
FUNCTION (svldff1ub_gather, svldff1_gather_extend, (TYPE_SUFFIX_u8))
FUNCTION (svldff1uh, svldxf1_extend_impl, (TYPE_SUFFIX_u16, UNSPEC_LDFF1))
FUNCTION (svldff1uh_gather, svldff1_gather_extend, (TYPE_SUFFIX_u16))
FUNCTION (svldff1uw, svldxf1_extend_impl, (TYPE_SUFFIX_u32, UNSPEC_LDFF1))
FUNCTION (svldff1uw_gather, svldff1_gather_extend, (TYPE_SUFFIX_u32))
FUNCTION (svldnf1, svldxf1_impl, (UNSPEC_LDNF1))
FUNCTION (svldnf1sb, svldxf1_extend_impl, (TYPE_SUFFIX_s8, UNSPEC_LDNF1))
FUNCTION (svldnf1sh, svldxf1_extend_impl, (TYPE_SUFFIX_s16, UNSPEC_LDNF1))
FUNCTION (svldnf1sw, svldxf1_extend_impl, (TYPE_SUFFIX_s32, UNSPEC_LDNF1))
FUNCTION (svldnf1ub, svldxf1_extend_impl, (TYPE_SUFFIX_u8, UNSPEC_LDNF1))
FUNCTION (svldnf1uh, svldxf1_extend_impl, (TYPE_SUFFIX_u16, UNSPEC_LDNF1))
FUNCTION (svldnf1uw, svldxf1_extend_impl, (TYPE_SUFFIX_u32, UNSPEC_LDNF1))
FUNCTION (svldnt1, svldnt1_impl,)
FUNCTION (svlen, svlen_impl,)
FUNCTION (svlsl, rtx_code_function, (ASHIFT, ASHIFT))
FUNCTION (svlsl_wide, shift_wide, (ASHIFT, UNSPEC_ASHIFT_WIDE))
FUNCTION (svlsr, rtx_code_function, (LSHIFTRT, LSHIFTRT))
FUNCTION (svlsr_wide, shift_wide, (LSHIFTRT, UNSPEC_LSHIFTRT_WIDE))
FUNCTION (svmad, svmad_impl,)
FUNCTION (svmax, rtx_code_function, (SMAX, UMAX, UNSPEC_COND_FMAX,
				     UNSPEC_FMAX))
FUNCTION (svmaxnm, cond_or_uncond_unspec_function, (UNSPEC_COND_FMAXNM,
						    UNSPEC_FMAXNM))
FUNCTION (svmaxnmv, reduction, (UNSPEC_FMAXNMV))
FUNCTION (svmaxv, reduction, (UNSPEC_SMAXV, UNSPEC_UMAXV, UNSPEC_FMAXV))
FUNCTION (svmin, rtx_code_function, (SMIN, UMIN, UNSPEC_COND_FMIN,
				     UNSPEC_FMIN))
FUNCTION (svminnm, cond_or_uncond_unspec_function, (UNSPEC_COND_FMINNM,
						    UNSPEC_FMINNM))
FUNCTION (svminnmv, reduction, (UNSPEC_FMINNMV))
FUNCTION (svminv, reduction, (UNSPEC_SMINV, UNSPEC_UMINV, UNSPEC_FMINV))
FUNCTION (svmla, svmla_impl,)
FUNCTION (svmla_lane, svmla_lane_impl,)
FUNCTION (svmls, svmls_impl,)
FUNCTION (svmls_lane, svmls_lane_impl,)
FUNCTION (svmmla, svmmla_impl,)
FUNCTION (svmov, svmov_impl,)
FUNCTION (svmsb, svmsb_impl,)
FUNCTION (svmul, svmul_impl,)
FUNCTION (svmul_lane, CODE_FOR_MODE0 (aarch64_mul_lane),)
FUNCTION (svmulh, unspec_based_function, (UNSPEC_SMUL_HIGHPART,
					  UNSPEC_UMUL_HIGHPART, -1))
FUNCTION (svmulx, unspec_based_function, (-1, -1, UNSPEC_COND_FMULX))
FUNCTION (svnand, svnand_impl,)
FUNCTION (svneg, quiet<rtx_code_function>, (NEG, NEG, UNSPEC_COND_FNEG))
FUNCTION (svnmad, unspec_based_function, (-1, -1, UNSPEC_COND_FNMLA))
FUNCTION (svnmla, unspec_based_function_rotated, (-1, -1, UNSPEC_COND_FNMLA))
FUNCTION (svnmls, unspec_based_function_rotated, (-1, -1, UNSPEC_COND_FNMLS))
FUNCTION (svnmsb, unspec_based_function, (-1, -1, UNSPEC_COND_FNMLS))
FUNCTION (svnor, svnor_impl,)
FUNCTION (svnot, svnot_impl,)
FUNCTION (svorn, svorn_impl,)
FUNCTION (svorr, rtx_code_function, (IOR, IOR))
FUNCTION (svorv, reduction, (UNSPEC_IORV))
FUNCTION (svpfalse, svpfalse_impl,)
FUNCTION (svpfirst, svpfirst_svpnext_impl, (UNSPEC_PFIRST))
FUNCTION (svpnext, svpfirst_svpnext_impl, (UNSPEC_PNEXT))
FUNCTION (svprfb, svprf_bhwd_impl, (VNx16QImode))
FUNCTION (svprfb_gather, svprf_bhwd_gather_impl, (VNx16QImode))
FUNCTION (svprfd, svprf_bhwd_impl, (VNx2DImode))
FUNCTION (svprfd_gather, svprf_bhwd_gather_impl, (VNx2DImode))
FUNCTION (svprfh, svprf_bhwd_impl, (VNx8HImode))
FUNCTION (svprfh_gather, svprf_bhwd_gather_impl, (VNx8HImode))
FUNCTION (svprfw, svprf_bhwd_impl, (VNx4SImode))
FUNCTION (svprfw_gather, svprf_bhwd_gather_impl, (VNx4SImode))
FUNCTION (svptest_any, svptest_impl, (NE))
FUNCTION (svptest_first, svptest_impl, (LT))
FUNCTION (svptest_last, svptest_impl, (LTU))
FUNCTION (svptrue, svptrue_impl,)
FUNCTION (svptrue_pat, svptrue_pat_impl,)
FUNCTION (svqadd, rtx_code_function, (SS_PLUS, US_PLUS, -1))
FUNCTION (svqdecb, svqdec_bhwd_impl, (QImode))
FUNCTION (svqdecb_pat, svqdec_bhwd_impl, (QImode))
FUNCTION (svqdecd, svqdec_bhwd_impl, (DImode))
FUNCTION (svqdecd_pat, svqdec_bhwd_impl, (DImode))
FUNCTION (svqdech, svqdec_bhwd_impl, (HImode))
FUNCTION (svqdech_pat, svqdec_bhwd_impl, (HImode))
FUNCTION (svqdecp, svqdecp_svqincp_impl, (SS_MINUS, US_MINUS))
FUNCTION (svqdecw, svqdec_bhwd_impl, (SImode))
FUNCTION (svqdecw_pat, svqdec_bhwd_impl, (SImode))
FUNCTION (svqincb, svqinc_bhwd_impl, (QImode))
FUNCTION (svqincb_pat, svqinc_bhwd_impl, (QImode))
FUNCTION (svqincd, svqinc_bhwd_impl, (DImode))
FUNCTION (svqincd_pat, svqinc_bhwd_impl, (DImode))
FUNCTION (svqinch, svqinc_bhwd_impl, (HImode))
FUNCTION (svqinch_pat, svqinc_bhwd_impl, (HImode))
FUNCTION (svqincp, svqdecp_svqincp_impl, (SS_PLUS, US_PLUS))
FUNCTION (svqincw, svqinc_bhwd_impl, (SImode))
FUNCTION (svqincw_pat, svqinc_bhwd_impl, (SImode))
FUNCTION (svqsub, rtx_code_function, (SS_MINUS, US_MINUS, -1))
FUNCTION (svrbit, rtx_code_function, (BITREVERSE, BITREVERSE, -1))
FUNCTION (svrdffr, svrdffr_impl,)
FUNCTION (svrecpe, unspec_based_function, (-1, UNSPEC_URECPE, UNSPEC_FRECPE))
FUNCTION (svrecps, unspec_based_function, (-1, -1, UNSPEC_FRECPS))
FUNCTION (svrecpx, unspec_based_function, (-1, -1, UNSPEC_COND_FRECPX))
FUNCTION (svreinterpret, svreinterpret_impl,)
FUNCTION (svrev, svrev_impl,)
FUNCTION (svrevb, unspec_based_function, (UNSPEC_REVB, UNSPEC_REVB, -1))
FUNCTION (svrevh, unspec_based_function, (UNSPEC_REVH, UNSPEC_REVH, -1))
FUNCTION (svrevw, unspec_based_function, (UNSPEC_REVW, UNSPEC_REVW, -1))
FUNCTION (svrinta, svrint_impl, (round_optab, UNSPEC_COND_FRINTA))
FUNCTION (svrinti, svrint_impl, (nearbyint_optab, UNSPEC_COND_FRINTI))
FUNCTION (svrintm, svrint_impl, (floor_optab, UNSPEC_COND_FRINTM))
FUNCTION (svrintn, svrint_impl, (roundeven_optab, UNSPEC_COND_FRINTN))
FUNCTION (svrintp, svrint_impl, (ceil_optab, UNSPEC_COND_FRINTP))
FUNCTION (svrintx, svrint_impl, (rint_optab, UNSPEC_COND_FRINTX))
FUNCTION (svrintz, svrint_impl, (btrunc_optab, UNSPEC_COND_FRINTZ))
FUNCTION (svrsqrte, unspec_based_function, (-1, UNSPEC_RSQRTE, UNSPEC_RSQRTE))
FUNCTION (svrsqrts, unspec_based_function, (-1, -1, UNSPEC_RSQRTS))
FUNCTION (svscale, unspec_based_function, (-1, -1, UNSPEC_COND_FSCALE))
FUNCTION (svsel, svsel_impl,)
FUNCTION (svset2, svset_impl, (2))
FUNCTION (svset3, svset_impl, (3))
FUNCTION (svset4, svset_impl, (4))
FUNCTION (svsetffr, svsetffr_impl,)
FUNCTION (svsplice, QUIET_CODE_FOR_MODE0 (aarch64_sve_splice),)
FUNCTION (svsqrt, rtx_code_function, (SQRT, SQRT, UNSPEC_COND_FSQRT))
FUNCTION (svst1, svst1_impl,)
FUNCTION (svst1_scatter, svst1_scatter_impl,)
FUNCTION (svst1b, svst1_truncate_impl, (QImode))
FUNCTION (svst1b_scatter, svst1_scatter_truncate_impl, (QImode))
FUNCTION (svst1h, svst1_truncate_impl, (HImode))
FUNCTION (svst1h_scatter, svst1_scatter_truncate_impl, (HImode))
FUNCTION (svst1w, svst1_truncate_impl, (SImode))
FUNCTION (svst1w_scatter, svst1_scatter_truncate_impl, (SImode))
FUNCTION (svst2, svst234_impl, (2))
FUNCTION (svst3, svst234_impl, (3))
FUNCTION (svst4, svst234_impl, (4))
FUNCTION (svstnt1, svstnt1_impl,)
FUNCTION (svsub, svsub_impl,)
FUNCTION (svsubr, rtx_code_function_rotated, (MINUS, MINUS, UNSPEC_COND_FSUB))
FUNCTION (svsudot, svusdot_impl, (true))
FUNCTION (svsudot_lane, svdotprod_lane_impl, (UNSPEC_SUDOT, -1, -1))
FUNCTION (svtbl, svtbl_impl,)
FUNCTION (svtmad, CODE_FOR_MODE0 (aarch64_sve_tmad),)
FUNCTION (svtrn1, svtrn_impl, (0))
FUNCTION (svtrn1q, unspec_based_function, (UNSPEC_TRN1Q, UNSPEC_TRN1Q,
					   UNSPEC_TRN1Q))
FUNCTION (svtrn2, svtrn_impl, (1))
FUNCTION (svtrn2q, unspec_based_function, (UNSPEC_TRN2Q, UNSPEC_TRN2Q,
					   UNSPEC_TRN2Q))
FUNCTION (svtsmul, unspec_based_function, (-1, -1, UNSPEC_FTSMUL))
FUNCTION (svtssel, unspec_based_function, (-1, -1, UNSPEC_FTSSEL))
FUNCTION (svundef, svundef_impl, (1))
FUNCTION (svundef2, svundef_impl, (2))
FUNCTION (svundef3, svundef_impl, (3))
FUNCTION (svundef4, svundef_impl, (4))
FUNCTION (svunpkhi, svunpk_impl, (true))
FUNCTION (svunpklo, svunpk_impl, (false))
FUNCTION (svusdot, svusdot_impl, (false))
FUNCTION (svusdot_lane, svdotprod_lane_impl, (UNSPEC_USDOT, -1, -1))
FUNCTION (svusmmla, unspec_based_add_function, (UNSPEC_USMATMUL, -1, -1))
FUNCTION (svuzp1, svuzp_impl, (0))
FUNCTION (svuzp1q, unspec_based_function, (UNSPEC_UZP1Q, UNSPEC_UZP1Q,
					   UNSPEC_UZP1Q))
FUNCTION (svuzp2, svuzp_impl, (1))
FUNCTION (svuzp2q, unspec_based_function, (UNSPEC_UZP2Q, UNSPEC_UZP2Q,
					   UNSPEC_UZP2Q))
FUNCTION (svwhilele, svwhilelx_impl, (UNSPEC_WHILELE, UNSPEC_WHILELS, true))
FUNCTION (svwhilelt, svwhilelx_impl, (UNSPEC_WHILELT, UNSPEC_WHILELO, false))
FUNCTION (svwrffr, svwrffr_impl,)
FUNCTION (svzip1, svzip_impl, (0))
FUNCTION (svzip1q, unspec_based_function, (UNSPEC_ZIP1Q, UNSPEC_ZIP1Q,
					   UNSPEC_ZIP1Q))
FUNCTION (svzip2, svzip_impl, (1))
FUNCTION (svzip2q, unspec_based_function, (UNSPEC_ZIP2Q, UNSPEC_ZIP2Q,
					   UNSPEC_ZIP2Q))
NEON_SVE_BRIDGE_FUNCTION (svget_neonq, svget_neonq_impl,)
NEON_SVE_BRIDGE_FUNCTION (svset_neonq, svset_neonq_impl,)
NEON_SVE_BRIDGE_FUNCTION (svdup_neonq, svdup_neonq_impl,)

} /* end namespace aarch64_sve */
