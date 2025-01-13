/* Internal functions.
   Copyright (C) 2011-2025 Free Software Foundation, Inc.

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
#define INCLUDE_MEMORY
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "predict.h"
#include "stringpool.h"
#include "tree-vrp.h"
#include "tree-ssanames.h"
#include "expmed.h"
#include "memmodel.h"
#include "optabs.h"
#include "emit-rtl.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "internal-fn.h"
#include "stor-layout.h"
#include "dojump.h"
#include "expr.h"
#include "stringpool.h"
#include "attribs.h"
#include "asan.h"
#include "ubsan.h"
#include "recog.h"
#include "builtins.h"
#include "optabs-tree.h"
#include "gimple-ssa.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "explow.h"
#include "rtl-iter.h"
#include "gimple-range.h"
#include "fold-const-call.h"
#include "tree-ssa-live.h"
#include "tree-outof-ssa.h"
#include "gcc-urlifier.h"

/* For lang_hooks.types.type_for_mode.  */
#include "langhooks.h"

/* The names of each internal function, indexed by function number.  */
const char *const internal_fn_name_array[] = {
#define DEF_INTERNAL_FN(CODE, FLAGS, FNSPEC) #CODE,
#include "internal-fn.def"
  "<invalid-fn>"
};

/* The ECF_* flags of each internal function, indexed by function number.  */
const int internal_fn_flags_array[] = {
#define DEF_INTERNAL_FN(CODE, FLAGS, FNSPEC) FLAGS,
#include "internal-fn.def"
  0
};

/* Return the internal function called NAME, or IFN_LAST if there's
   no such function.  */

internal_fn
lookup_internal_fn (const char *name)
{
  typedef hash_map<nofree_string_hash, internal_fn> name_to_fn_map_type;
  static name_to_fn_map_type *name_to_fn_map;

  if (!name_to_fn_map)
    {
      name_to_fn_map = new name_to_fn_map_type (IFN_LAST);
      for (unsigned int i = 0; i < IFN_LAST; ++i)
	name_to_fn_map->put (internal_fn_name (internal_fn (i)),
			     internal_fn (i));
    }
  internal_fn *entry = name_to_fn_map->get (name);
  return entry ? *entry : IFN_LAST;
}

/* Geven an internal_fn IFN that is a widening function, return its
   corresponding LO and HI internal_fns.  */

extern void
lookup_hilo_internal_fn (internal_fn ifn, internal_fn *lo, internal_fn *hi)
{
  gcc_assert (widening_fn_p (ifn));

  switch (ifn)
    {
    default:
      gcc_unreachable ();
#define DEF_INTERNAL_FN(NAME, FLAGS, TYPE)
#define DEF_INTERNAL_WIDENING_OPTAB_FN(NAME, F, S, SO, UO, T)	\
    case IFN_##NAME:						\
      *lo = internal_fn (IFN_##NAME##_LO);			\
      *hi = internal_fn (IFN_##NAME##_HI);			\
      break;
#include "internal-fn.def"
    }
}

/* Given an internal_fn IFN that is a widening function, return its
   corresponding _EVEN and _ODD internal_fns in *EVEN and *ODD.  */

extern void
lookup_evenodd_internal_fn (internal_fn ifn, internal_fn *even,
			    internal_fn *odd)
{
  gcc_assert (widening_fn_p (ifn));

  switch (ifn)
    {
    default:
      gcc_unreachable ();
#define DEF_INTERNAL_FN(NAME, FLAGS, TYPE)
#define DEF_INTERNAL_WIDENING_OPTAB_FN(NAME, F, S, SO, UO, T)	\
    case IFN_##NAME:						\
      *even = internal_fn (IFN_##NAME##_EVEN);			\
      *odd = internal_fn (IFN_##NAME##_ODD);			\
      break;
#include "internal-fn.def"
    }
}


/* Fnspec of each internal function, indexed by function number.  */
const_tree internal_fn_fnspec_array[IFN_LAST + 1];

void
init_internal_fns ()
{
#define DEF_INTERNAL_FN(CODE, FLAGS, FNSPEC) \
  if (FNSPEC) internal_fn_fnspec_array[IFN_##CODE] = \
    build_string ((int) sizeof (FNSPEC) - 1, FNSPEC ? FNSPEC : "");
#include "internal-fn.def"
  internal_fn_fnspec_array[IFN_LAST] = 0;
}

/* Create static initializers for the information returned by
   direct_internal_fn.  */
#define not_direct { -2, -2, false }
#define mask_load_direct { -1, 2, false }
#define load_lanes_direct { -1, -1, false }
#define mask_load_lanes_direct { -1, -1, false }
#define gather_load_direct { 3, 1, false }
#define strided_load_direct { -1, -1, false }
#define len_load_direct { -1, -1, false }
#define mask_len_load_direct { -1, 4, false }
#define mask_store_direct { 3, 2, false }
#define store_lanes_direct { 0, 0, false }
#define mask_store_lanes_direct { 0, 0, false }
#define vec_cond_mask_direct { 1, 0, false }
#define vec_cond_mask_len_direct { 1, 1, false }
#define vec_cond_direct { 2, 0, false }
#define scatter_store_direct { 3, 1, false }
#define strided_store_direct { 1, 1, false }
#define len_store_direct { 3, 3, false }
#define mask_len_store_direct { 4, 5, false }
#define vec_set_direct { 3, 3, false }
#define vec_extract_direct { 0, -1, false }
#define unary_direct { 0, 0, true }
#define unary_convert_direct { -1, 0, true }
#define binary_direct { 0, 0, true }
#define ternary_direct { 0, 0, true }
#define cond_unary_direct { 1, 1, true }
#define cond_binary_direct { 1, 1, true }
#define cond_ternary_direct { 1, 1, true }
#define cond_len_unary_direct { 1, 1, true }
#define cond_len_binary_direct { 1, 1, true }
#define cond_len_ternary_direct { 1, 1, true }
#define while_direct { 0, 2, false }
#define fold_extract_direct { 2, 2, false }
#define fold_len_extract_direct { 2, 2, false }
#define fold_left_direct { 1, 1, false }
#define mask_fold_left_direct { 1, 1, false }
#define mask_len_fold_left_direct { 1, 1, false }
#define check_ptrs_direct { 0, 0, false }
#define crc_direct { 1, -1, true }

const direct_internal_fn_info direct_internal_fn_array[IFN_LAST + 1] = {
#define DEF_INTERNAL_FN(CODE, FLAGS, FNSPEC) not_direct,
#define DEF_INTERNAL_OPTAB_FN(CODE, FLAGS, OPTAB, TYPE) TYPE##_direct,
#define DEF_INTERNAL_SIGNED_OPTAB_FN(CODE, FLAGS, SELECTOR, SIGNED_OPTAB, \
				     UNSIGNED_OPTAB, TYPE) TYPE##_direct,
#include "internal-fn.def"
  not_direct
};

/* Like create_output_operand, but for callers that will use
   assign_call_lhs afterwards.  */

static void
create_call_lhs_operand (expand_operand *op, rtx lhs_rtx, machine_mode mode)
{
  /* Do not assign directly to a promoted subreg, since there is no
     guarantee that the instruction will leave the upper bits of the
     register in the state required by SUBREG_PROMOTED_SIGN.  */
  rtx dest = lhs_rtx;
  if (dest && GET_CODE (dest) == SUBREG && SUBREG_PROMOTED_VAR_P (dest))
    dest = NULL_RTX;
  create_output_operand (op, dest, mode);
}

/* Move the result of an expanded instruction into the lhs of a gimple call.
   LHS is the lhs of the call, LHS_RTX is its expanded form, and OP is the
   result of the expanded instruction.  OP should have been set up by
   create_call_lhs_operand.  */

static void
assign_call_lhs (tree lhs, rtx lhs_rtx, expand_operand *op)
{
  if (rtx_equal_p (lhs_rtx, op->value))
    return;

  /* If the return value has an integral type, convert the instruction
     result to that type.  This is useful for things that return an
     int regardless of the size of the input.  If the instruction result
     is smaller than required, assume that it is signed.

     If the return value has a nonintegral type, its mode must match
     the instruction result.  */
  if (GET_CODE (lhs_rtx) == SUBREG && SUBREG_PROMOTED_VAR_P (lhs_rtx))
    {
      /* If this is a scalar in a register that is stored in a wider
	 mode than the declared mode, compute the result into its
	 declared mode and then convert to the wider mode.  */
      gcc_checking_assert (INTEGRAL_TYPE_P (TREE_TYPE (lhs)));
      rtx tmp = convert_to_mode (GET_MODE (lhs_rtx), op->value, 0);
      convert_move (SUBREG_REG (lhs_rtx), tmp,
		    SUBREG_PROMOTED_SIGN (lhs_rtx));
    }
  else if (GET_MODE (lhs_rtx) == GET_MODE (op->value))
    emit_move_insn (lhs_rtx, op->value);
  else
    {
      gcc_checking_assert (INTEGRAL_TYPE_P (TREE_TYPE (lhs)));
      convert_move (lhs_rtx, op->value, 0);
    }
}

/* Expand STMT using instruction ICODE.  The instruction has NOUTPUTS
   output operands and NINPUTS input operands, where NOUTPUTS is either
   0 or 1.  The output operand (if any) comes first, followed by the
   NINPUTS input operands.  */

static void
expand_fn_using_insn (gcall *stmt, insn_code icode, unsigned int noutputs,
		      unsigned int ninputs)
{
  gcc_assert (icode != CODE_FOR_nothing);

  expand_operand *ops = XALLOCAVEC (expand_operand, noutputs + ninputs);
  unsigned int opno = 0;
  rtx lhs_rtx = NULL_RTX;
  tree lhs = gimple_call_lhs (stmt);

  if (noutputs)
    {
      gcc_assert (noutputs == 1);
      if (lhs)
	lhs_rtx = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
      create_call_lhs_operand (&ops[opno], lhs_rtx,
			       insn_data[icode].operand[opno].mode);
      opno += 1;
    }
  else
    gcc_assert (!lhs);

  for (unsigned int i = 0; i < ninputs; ++i)
    {
      tree rhs = gimple_call_arg (stmt, i);
      tree rhs_type = TREE_TYPE (rhs);
      rtx rhs_rtx = expand_normal (rhs);
      if (INTEGRAL_TYPE_P (rhs_type))
	create_convert_operand_from (&ops[opno], rhs_rtx,
				     TYPE_MODE (rhs_type),
				     TYPE_UNSIGNED (rhs_type));
      else if (TREE_CODE (rhs) == SSA_NAME
	       && SSA_NAME_IS_DEFAULT_DEF (rhs)
	       && VAR_P (SSA_NAME_VAR (rhs)))
	create_undefined_input_operand (&ops[opno], TYPE_MODE (rhs_type));
      else if (VECTOR_BOOLEAN_TYPE_P (rhs_type)
	       && SCALAR_INT_MODE_P (TYPE_MODE (rhs_type))
	       && maybe_ne (GET_MODE_PRECISION (TYPE_MODE (rhs_type)),
			    TYPE_VECTOR_SUBPARTS (rhs_type).to_constant ()))
	{
	  /* Ensure that the vector bitmasks do not have excess bits.  */
	  int nunits = TYPE_VECTOR_SUBPARTS (rhs_type).to_constant ();
	  rtx tmp = expand_binop (TYPE_MODE (rhs_type), and_optab, rhs_rtx,
				  GEN_INT ((HOST_WIDE_INT_1U << nunits) - 1),
				  NULL_RTX, true, OPTAB_WIDEN);
	  create_input_operand (&ops[opno], tmp, TYPE_MODE (rhs_type));
	}
      else
	create_input_operand (&ops[opno], rhs_rtx, TYPE_MODE (rhs_type));
      opno += 1;
    }

  gcc_assert (opno == noutputs + ninputs);
  expand_insn (icode, opno, ops);
  if (lhs_rtx)
    assign_call_lhs (lhs, lhs_rtx, &ops[0]);
}

/* ARRAY_TYPE is an array of vector modes.  Return the associated insn
   for load-lanes-style optab OPTAB, or CODE_FOR_nothing if none.  */

static enum insn_code
get_multi_vector_move (tree array_type, convert_optab optab)
{
  machine_mode imode;
  machine_mode vmode;

  gcc_assert (TREE_CODE (array_type) == ARRAY_TYPE);
  imode = TYPE_MODE (array_type);
  vmode = TYPE_MODE (TREE_TYPE (array_type));

  return convert_optab_handler (optab, imode, vmode);
}

/* Add mask, else, and len arguments according to the STMT.  */

static unsigned int
add_mask_else_and_len_args (expand_operand *ops, unsigned int opno, gcall *stmt)
{
  internal_fn ifn = gimple_call_internal_fn (stmt);
  int len_index = internal_fn_len_index (ifn);
  /* BIAS is always consecutive next of LEN.  */
  int bias_index = len_index + 1;
  int mask_index = internal_fn_mask_index (ifn);

  /* The order of arguments is always {mask, else, len, bias}.  */
  if (mask_index >= 0)
    {
      tree mask = gimple_call_arg (stmt, mask_index);
      rtx mask_rtx = expand_normal (mask);

      tree mask_type = TREE_TYPE (mask);
      if (VECTOR_BOOLEAN_TYPE_P (mask_type)
	  && SCALAR_INT_MODE_P (TYPE_MODE (mask_type))
	  && maybe_ne (GET_MODE_PRECISION (TYPE_MODE (mask_type)),
		       TYPE_VECTOR_SUBPARTS (mask_type).to_constant ()))
	{
	  /* Ensure that the vector bitmasks do not have excess bits.  */
	  int nunits = TYPE_VECTOR_SUBPARTS (mask_type).to_constant ();
	  mask_rtx = expand_binop (TYPE_MODE (mask_type), and_optab, mask_rtx,
				   GEN_INT ((HOST_WIDE_INT_1U << nunits) - 1),
				   NULL_RTX, true, OPTAB_WIDEN);
	}

      create_input_operand (&ops[opno++], mask_rtx,
			    TYPE_MODE (TREE_TYPE (mask)));
    }

  int els_index = internal_fn_else_index (ifn);
  if (els_index >= 0)
    {
      tree els = gimple_call_arg (stmt, els_index);
      tree els_type = TREE_TYPE (els);
      if (TREE_CODE (els) == SSA_NAME
	  && SSA_NAME_IS_DEFAULT_DEF (els)
	  && VAR_P (SSA_NAME_VAR (els)))
	create_undefined_input_operand (&ops[opno++], TYPE_MODE (els_type));
      else
	{
	  rtx els_rtx = expand_normal (els);
	  create_input_operand (&ops[opno++], els_rtx, TYPE_MODE (els_type));
	}
    }
  if (len_index >= 0)
    {
      tree len = gimple_call_arg (stmt, len_index);
      rtx len_rtx = expand_normal (len);
      create_convert_operand_from (&ops[opno++], len_rtx,
				   TYPE_MODE (TREE_TYPE (len)),
				   TYPE_UNSIGNED (TREE_TYPE (len)));
      tree biast = gimple_call_arg (stmt, bias_index);
      rtx bias = expand_normal (biast);
      create_input_operand (&ops[opno++], bias, QImode);
    }
  return opno;
}

/* Expand LOAD_LANES call STMT using optab OPTAB.  */

static void
expand_load_lanes_optab_fn (internal_fn, gcall *stmt, convert_optab optab)
{
  class expand_operand ops[2];
  tree type, lhs, rhs;
  rtx target, mem;

  lhs = gimple_call_lhs (stmt);
  rhs = gimple_call_arg (stmt, 0);
  type = TREE_TYPE (lhs);

  target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  mem = expand_normal (rhs);

  gcc_assert (MEM_P (mem));
  PUT_MODE (mem, TYPE_MODE (type));

  create_call_lhs_operand (&ops[0], target, TYPE_MODE (type));
  create_fixed_operand (&ops[1], mem);
  expand_insn (get_multi_vector_move (type, optab), 2, ops);
  assign_call_lhs (lhs, target, &ops[0]);
}

/* Expand STORE_LANES call STMT using optab OPTAB.  */

static void
expand_store_lanes_optab_fn (internal_fn, gcall *stmt, convert_optab optab)
{
  class expand_operand ops[2];
  tree type, lhs, rhs;
  rtx target, reg;

  lhs = gimple_call_lhs (stmt);
  rhs = gimple_call_arg (stmt, 0);
  type = TREE_TYPE (rhs);

  target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  reg = expand_normal (rhs);

  gcc_assert (MEM_P (target));
  PUT_MODE (target, TYPE_MODE (type));

  create_fixed_operand (&ops[0], target);
  create_input_operand (&ops[1], reg, TYPE_MODE (type));
  expand_insn (get_multi_vector_move (type, optab), 2, ops);
}

static void
expand_ANNOTATE (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in omp_device_lower pass.  */

static void
expand_GOMP_USE_SIMT (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in omp_device_lower pass.  */

static void
expand_GOMP_SIMT_ENTER (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* Allocate per-lane storage and begin non-uniform execution region.  */

static void
expand_GOMP_SIMT_ENTER_ALLOC (internal_fn, gcall *stmt)
{
  rtx target;
  tree lhs = gimple_call_lhs (stmt);
  if (lhs)
    target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  else
    target = gen_reg_rtx (Pmode);
  rtx size = expand_normal (gimple_call_arg (stmt, 0));
  rtx align = expand_normal (gimple_call_arg (stmt, 1));
  class expand_operand ops[3];
  create_call_lhs_operand (&ops[0], target, Pmode);
  create_input_operand (&ops[1], size, Pmode);
  create_input_operand (&ops[2], align, Pmode);
  gcc_assert (targetm.have_omp_simt_enter ());
  expand_insn (targetm.code_for_omp_simt_enter, 3, ops);
  assign_call_lhs (lhs, target, &ops[0]);
}

/* Deallocate per-lane storage and leave non-uniform execution region.  */

static void
expand_GOMP_SIMT_EXIT (internal_fn, gcall *stmt)
{
  gcc_checking_assert (!gimple_call_lhs (stmt));
  rtx arg = expand_normal (gimple_call_arg (stmt, 0));
  class expand_operand ops[1];
  create_input_operand (&ops[0], arg, Pmode);
  gcc_assert (targetm.have_omp_simt_exit ());
  expand_insn (targetm.code_for_omp_simt_exit, 1, ops);
}

/* Lane index on SIMT targets: thread index in the warp on NVPTX.  On targets
   without SIMT execution this should be expanded in omp_device_lower pass.  */

static void
expand_GOMP_SIMT_LANE (internal_fn, gcall *stmt)
{
  tree lhs = gimple_call_lhs (stmt);
  if (!lhs)
    return;

  rtx target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  gcc_assert (targetm.have_omp_simt_lane ());
  emit_insn (targetm.gen_omp_simt_lane (target));
}

/* This should get expanded in omp_device_lower pass.  */

static void
expand_GOMP_SIMT_VF (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in omp_device_lower pass.  */

static void
expand_GOMP_MAX_VF (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in omp_device_lower pass.  */

static void
expand_GOMP_TARGET_REV (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* Lane index of the first SIMT lane that supplies a non-zero argument.
   This is a SIMT counterpart to GOMP_SIMD_LAST_LANE, used to represent the
   lane that executed the last iteration for handling OpenMP lastprivate.  */

static void
expand_GOMP_SIMT_LAST_LANE (internal_fn, gcall *stmt)
{
  tree lhs = gimple_call_lhs (stmt);
  if (!lhs)
    return;

  rtx target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  rtx cond = expand_normal (gimple_call_arg (stmt, 0));
  machine_mode mode = TYPE_MODE (TREE_TYPE (lhs));
  class expand_operand ops[2];
  create_call_lhs_operand (&ops[0], target, mode);
  create_input_operand (&ops[1], cond, mode);
  gcc_assert (targetm.have_omp_simt_last_lane ());
  expand_insn (targetm.code_for_omp_simt_last_lane, 2, ops);
  assign_call_lhs (lhs, target, &ops[0]);
}

/* Non-transparent predicate used in SIMT lowering of OpenMP "ordered".  */

static void
expand_GOMP_SIMT_ORDERED_PRED (internal_fn, gcall *stmt)
{
  tree lhs = gimple_call_lhs (stmt);
  if (!lhs)
    return;

  rtx target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  rtx ctr = expand_normal (gimple_call_arg (stmt, 0));
  machine_mode mode = TYPE_MODE (TREE_TYPE (lhs));
  class expand_operand ops[2];
  create_call_lhs_operand (&ops[0], target, mode);
  create_input_operand (&ops[1], ctr, mode);
  gcc_assert (targetm.have_omp_simt_ordered ());
  expand_insn (targetm.code_for_omp_simt_ordered, 2, ops);
  assign_call_lhs (lhs, target, &ops[0]);
}

/* "Or" boolean reduction across SIMT lanes: return non-zero in all lanes if
   any lane supplies a non-zero argument.  */

static void
expand_GOMP_SIMT_VOTE_ANY (internal_fn, gcall *stmt)
{
  tree lhs = gimple_call_lhs (stmt);
  if (!lhs)
    return;

  rtx target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  rtx cond = expand_normal (gimple_call_arg (stmt, 0));
  machine_mode mode = TYPE_MODE (TREE_TYPE (lhs));
  class expand_operand ops[2];
  create_call_lhs_operand (&ops[0], target, mode);
  create_input_operand (&ops[1], cond, mode);
  gcc_assert (targetm.have_omp_simt_vote_any ());
  expand_insn (targetm.code_for_omp_simt_vote_any, 2, ops);
  assign_call_lhs (lhs, target, &ops[0]);
}

/* Exchange between SIMT lanes with a "butterfly" pattern: source lane index
   is destination lane index XOR given offset.  */

static void
expand_GOMP_SIMT_XCHG_BFLY (internal_fn, gcall *stmt)
{
  tree lhs = gimple_call_lhs (stmt);
  if (!lhs)
    return;

  rtx target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  rtx src = expand_normal (gimple_call_arg (stmt, 0));
  rtx idx = expand_normal (gimple_call_arg (stmt, 1));
  machine_mode mode = TYPE_MODE (TREE_TYPE (lhs));
  class expand_operand ops[3];
  create_call_lhs_operand (&ops[0], target, mode);
  create_input_operand (&ops[1], src, mode);
  create_input_operand (&ops[2], idx, SImode);
  gcc_assert (targetm.have_omp_simt_xchg_bfly ());
  expand_insn (targetm.code_for_omp_simt_xchg_bfly, 3, ops);
  assign_call_lhs (lhs, target, &ops[0]);
}

/* Exchange between SIMT lanes according to given source lane index.  */

static void
expand_GOMP_SIMT_XCHG_IDX (internal_fn, gcall *stmt)
{
  tree lhs = gimple_call_lhs (stmt);
  if (!lhs)
    return;

  rtx target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  rtx src = expand_normal (gimple_call_arg (stmt, 0));
  rtx idx = expand_normal (gimple_call_arg (stmt, 1));
  machine_mode mode = TYPE_MODE (TREE_TYPE (lhs));
  class expand_operand ops[3];
  create_call_lhs_operand (&ops[0], target, mode);
  create_input_operand (&ops[1], src, mode);
  create_input_operand (&ops[2], idx, SImode);
  gcc_assert (targetm.have_omp_simt_xchg_idx ());
  expand_insn (targetm.code_for_omp_simt_xchg_idx, 3, ops);
  assign_call_lhs (lhs, target, &ops[0]);
}

/* This should get expanded in adjust_simduid_builtins.  */

static void
expand_GOMP_SIMD_LANE (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in adjust_simduid_builtins.  */

static void
expand_GOMP_SIMD_VF (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in adjust_simduid_builtins.  */

static void
expand_GOMP_SIMD_LAST_LANE (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in adjust_simduid_builtins.  */

static void
expand_GOMP_SIMD_ORDERED_START (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in adjust_simduid_builtins.  */

static void
expand_GOMP_SIMD_ORDERED_END (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in gimplify_omp_dispatch.  */

static void
expand_GOMP_DISPATCH (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in the sanopt pass.  */

static void
expand_UBSAN_NULL (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in the sanopt pass.  */

static void
expand_UBSAN_BOUNDS (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in the sanopt pass.  */

static void
expand_UBSAN_VPTR (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in the sanopt pass.  */

static void
expand_UBSAN_PTR (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in the sanopt pass.  */

static void
expand_UBSAN_OBJECT_SIZE (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in the sanopt pass.  */

static void
expand_HWASAN_CHECK (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* For hwasan stack tagging:
   Clear tags on the dynamically allocated space.
   For use after an object dynamically allocated on the stack goes out of
   scope.  */
static void
expand_HWASAN_ALLOCA_UNPOISON (internal_fn, gcall *gc)
{
  gcc_assert (Pmode == ptr_mode);
  tree restored_position = gimple_call_arg (gc, 0);
  rtx restored_rtx = expand_expr (restored_position, NULL_RTX, VOIDmode,
				  EXPAND_NORMAL);
  rtx func = init_one_libfunc ("__hwasan_tag_memory");
  rtx off = expand_simple_binop (Pmode, MINUS, restored_rtx,
				 stack_pointer_rtx, NULL_RTX, 0,
				 OPTAB_WIDEN);
  emit_library_call_value (func, NULL_RTX, LCT_NORMAL, VOIDmode,
			   virtual_stack_dynamic_rtx, Pmode,
			   HWASAN_STACK_BACKGROUND, QImode,
			   off, Pmode);
}

/* For hwasan stack tagging:
   Return a tag to be used for a dynamic allocation.  */
static void
expand_HWASAN_CHOOSE_TAG (internal_fn, gcall *gc)
{
  tree tag = gimple_call_lhs (gc);
  rtx target = expand_expr (tag, NULL_RTX, VOIDmode, EXPAND_NORMAL);
  machine_mode mode = GET_MODE (target);
  gcc_assert (mode == QImode);

  rtx base_tag = targetm.memtag.extract_tag (hwasan_frame_base (), NULL_RTX);
  gcc_assert (base_tag);
  rtx tag_offset = gen_int_mode (hwasan_current_frame_tag (), QImode);
  rtx chosen_tag = expand_simple_binop (QImode, PLUS, base_tag, tag_offset,
					target, /* unsignedp = */1,
					OPTAB_WIDEN);
  chosen_tag = hwasan_truncate_to_tag_size (chosen_tag, target);

  /* Really need to put the tag into the `target` RTX.  */
  if (chosen_tag != target)
    {
      rtx temp = chosen_tag;
      gcc_assert (GET_MODE (chosen_tag) == mode);
      emit_move_insn (target, temp);
    }

  hwasan_increment_frame_tag ();
}

/* For hwasan stack tagging:
   Tag a region of space in the shadow stack according to the base pointer of
   an object on the stack.  N.b. the length provided in the internal call is
   required to be aligned to HWASAN_TAG_GRANULE_SIZE.  */
static void
expand_HWASAN_MARK (internal_fn, gcall *gc)
{
  gcc_assert (ptr_mode == Pmode);
  HOST_WIDE_INT flag = tree_to_shwi (gimple_call_arg (gc, 0));
  bool is_poison = ((asan_mark_flags)flag) == ASAN_MARK_POISON;

  tree base = gimple_call_arg (gc, 1);
  gcc_checking_assert (TREE_CODE (base) == ADDR_EXPR);
  rtx base_rtx = expand_normal (base);

  rtx tag = is_poison ? HWASAN_STACK_BACKGROUND
    : targetm.memtag.extract_tag (base_rtx, NULL_RTX);
  rtx address = targetm.memtag.untagged_pointer (base_rtx, NULL_RTX);

  tree len = gimple_call_arg (gc, 2);
  rtx r_len = expand_normal (len);

  rtx func = init_one_libfunc ("__hwasan_tag_memory");
  emit_library_call (func, LCT_NORMAL, VOIDmode, address, Pmode,
		     tag, QImode, r_len, Pmode);
}

/* For hwasan stack tagging:
   Store a tag into a pointer.  */
static void
expand_HWASAN_SET_TAG (internal_fn, gcall *gc)
{
  gcc_assert (ptr_mode == Pmode);
  tree g_target = gimple_call_lhs (gc);
  tree g_ptr = gimple_call_arg (gc, 0);
  tree g_tag = gimple_call_arg (gc, 1);

  rtx ptr = expand_normal (g_ptr);
  rtx tag = expand_expr (g_tag, NULL_RTX, QImode, EXPAND_NORMAL);
  rtx target = expand_normal (g_target);

  rtx untagged = targetm.memtag.untagged_pointer (ptr, target);
  rtx tagged_value = targetm.memtag.set_tag (untagged, tag, target);
  if (tagged_value != target)
    emit_move_insn (target, tagged_value);
}

/* This should get expanded in the sanopt pass.  */

static void
expand_ASAN_CHECK (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in the sanopt pass.  */

static void
expand_ASAN_MARK (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in the sanopt pass.  */

static void
expand_ASAN_POISON (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in the sanopt pass.  */

static void
expand_ASAN_POISON_USE (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in the tsan pass.  */

static void
expand_TSAN_FUNC_EXIT (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get expanded in the lower pass.  */

static void
expand_FALLTHROUGH (internal_fn, gcall *call)
{
  auto_urlify_attributes sentinel;
  error_at (gimple_location (call),
	    "invalid use of attribute %<fallthrough%>");
}

/* Return minimum precision needed to represent all values
   of ARG in SIGNed integral type.  */

static int
get_min_precision (tree arg, signop sign)
{
  int prec = TYPE_PRECISION (TREE_TYPE (arg));
  int cnt = 0;
  signop orig_sign = sign;
  if (TREE_CODE (arg) == INTEGER_CST)
    {
      int p;
      if (TYPE_SIGN (TREE_TYPE (arg)) != sign)
	{
	  widest_int w = wi::to_widest (arg);
	  w = wi::ext (w, prec, sign);
	  p = wi::min_precision (w, sign);
	}
      else
	p = wi::min_precision (wi::to_wide (arg), sign);
      return MIN (p, prec);
    }
  while (CONVERT_EXPR_P (arg)
	 && INTEGRAL_TYPE_P (TREE_TYPE (TREE_OPERAND (arg, 0)))
	 && TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (arg, 0))) <= prec)
    {
      arg = TREE_OPERAND (arg, 0);
      if (TYPE_PRECISION (TREE_TYPE (arg)) < prec)
	{
	  if (TYPE_UNSIGNED (TREE_TYPE (arg)))
	    sign = UNSIGNED;
	  else if (sign == UNSIGNED && get_range_pos_neg (arg) != 1)
	    return prec + (orig_sign != sign);
	  prec = TYPE_PRECISION (TREE_TYPE (arg));
	}
      if (++cnt > 30)
	return prec + (orig_sign != sign);
    }
  if (CONVERT_EXPR_P (arg)
      && INTEGRAL_TYPE_P (TREE_TYPE (TREE_OPERAND (arg, 0)))
      && TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (arg, 0))) > prec)
    {
      /* We have e.g. (unsigned short) y_2 where int y_2 = (int) x_1(D);
	 If y_2's min precision is smaller than prec, return that.  */
      int oprec = get_min_precision (TREE_OPERAND (arg, 0), sign);
      if (oprec < prec)
	return oprec + (orig_sign != sign);
    }
  if (TREE_CODE (arg) != SSA_NAME)
    return prec + (orig_sign != sign);
  int_range_max r;
  while (!get_global_range_query ()->range_of_expr (r, arg)
	 || r.varying_p ()
	 || r.undefined_p ())
    {
      gimple *g = SSA_NAME_DEF_STMT (arg);
      if (is_gimple_assign (g)
	  && CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (g)))
	{
	  tree t = gimple_assign_rhs1 (g);
	  if (INTEGRAL_TYPE_P (TREE_TYPE (t))
	      && TYPE_PRECISION (TREE_TYPE (t)) <= prec)
	    {
	      arg = t;
	      if (TYPE_PRECISION (TREE_TYPE (arg)) < prec)
		{
		  if (TYPE_UNSIGNED (TREE_TYPE (arg)))
		    sign = UNSIGNED;
		  else if (sign == UNSIGNED && get_range_pos_neg (arg) != 1)
		    return prec + (orig_sign != sign);
		  prec = TYPE_PRECISION (TREE_TYPE (arg));
		}
	      if (++cnt > 30)
		return prec + (orig_sign != sign);
	      continue;
	    }
	}
      return prec + (orig_sign != sign);
    }
  if (sign == TYPE_SIGN (TREE_TYPE (arg)))
    {
      int p1 = wi::min_precision (r.lower_bound (), sign);
      int p2 = wi::min_precision (r.upper_bound (), sign);
      p1 = MAX (p1, p2);
      prec = MIN (prec, p1);
    }
  else if (sign == UNSIGNED && !wi::neg_p (r.lower_bound (), SIGNED))
    {
      int p = wi::min_precision (r.upper_bound (), UNSIGNED);
      prec = MIN (prec, p);
    }
  return prec + (orig_sign != sign);
}

/* Helper for expand_*_overflow.  Set the __imag__ part to true
   (1 except for signed:1 type, in which case store -1).  */

static void
expand_arith_set_overflow (tree lhs, rtx target)
{
  if (TYPE_PRECISION (TREE_TYPE (TREE_TYPE (lhs))) == 1
      && !TYPE_UNSIGNED (TREE_TYPE (TREE_TYPE (lhs))))
    write_complex_part (target, constm1_rtx, true, false);
  else
    write_complex_part (target, const1_rtx, true, false);
}

/* Helper for expand_*_overflow.  Store RES into the __real__ part
   of TARGET.  If RES has larger MODE than __real__ part of TARGET,
   set the __imag__ part to 1 if RES doesn't fit into it.  Similarly
   if LHS has smaller precision than its mode.  */

static void
expand_arith_overflow_result_store (tree lhs, rtx target,
				    scalar_int_mode mode, rtx res)
{
  scalar_int_mode tgtmode
    = as_a <scalar_int_mode> (GET_MODE_INNER (GET_MODE (target)));
  rtx lres = res;
  if (tgtmode != mode)
    {
      rtx_code_label *done_label = gen_label_rtx ();
      int uns = TYPE_UNSIGNED (TREE_TYPE (TREE_TYPE (lhs)));
      lres = convert_modes (tgtmode, mode, res, uns);
      gcc_assert (GET_MODE_PRECISION (tgtmode) < GET_MODE_PRECISION (mode));
      do_compare_rtx_and_jump (res, convert_modes (mode, tgtmode, lres, uns),
			       EQ, true, mode, NULL_RTX, NULL, done_label,
			       profile_probability::very_likely ());
      expand_arith_set_overflow (lhs, target);
      emit_label (done_label);
    }
  int prec = TYPE_PRECISION (TREE_TYPE (TREE_TYPE (lhs)));
  int tgtprec = GET_MODE_PRECISION (tgtmode);
  if (prec < tgtprec)
    {
      rtx_code_label *done_label = gen_label_rtx ();
      int uns = TYPE_UNSIGNED (TREE_TYPE (TREE_TYPE (lhs)));
      res = lres;
      if (uns)
	{
	  rtx mask
	    = immed_wide_int_const (wi::shifted_mask (0, prec, false, tgtprec),
				    tgtmode);
	  lres = expand_simple_binop (tgtmode, AND, res, mask, NULL_RTX,
				      true, OPTAB_LIB_WIDEN);
	}
      else
	{
	  lres = expand_shift (LSHIFT_EXPR, tgtmode, res, tgtprec - prec,
			       NULL_RTX, 1);
	  lres = expand_shift (RSHIFT_EXPR, tgtmode, lres, tgtprec - prec,
			       NULL_RTX, 0);
	}
      do_compare_rtx_and_jump (res, lres,
			       EQ, true, tgtmode, NULL_RTX, NULL, done_label,
			       profile_probability::very_likely ());
      expand_arith_set_overflow (lhs, target);
      emit_label (done_label);
    }
  write_complex_part (target, lres, false, false);
}

/* Helper for expand_*_overflow.  Store RES into TARGET.  */

static void
expand_ubsan_result_store (tree lhs, rtx target, scalar_int_mode mode,
			   rtx res, rtx_code_label *do_error)
{
  if (TREE_CODE (TREE_TYPE (lhs)) == BITINT_TYPE
      && TYPE_PRECISION (TREE_TYPE (lhs)) < GET_MODE_PRECISION (mode))
    {
      int uns = TYPE_UNSIGNED (TREE_TYPE (lhs));
      int prec = TYPE_PRECISION (TREE_TYPE (lhs));
      int tgtprec = GET_MODE_PRECISION (mode);
      rtx resc = gen_reg_rtx (mode), lres;
      emit_move_insn (resc, res);
      if (uns)
	{
	  rtx mask
	    = immed_wide_int_const (wi::shifted_mask (0, prec, false, tgtprec),
				    mode);
	  lres = expand_simple_binop (mode, AND, res, mask, NULL_RTX,
				      true, OPTAB_LIB_WIDEN);
	}
      else
	{
	  lres = expand_shift (LSHIFT_EXPR, mode, res, tgtprec - prec,
			       NULL_RTX, 1);
	  lres = expand_shift (RSHIFT_EXPR, mode, lres, tgtprec - prec,
			       NULL_RTX, 0);
	}
      if (lres != res)
	emit_move_insn (res, lres);
      do_compare_rtx_and_jump (res, resc,
			       NE, true, mode, NULL_RTX, NULL, do_error,
			       profile_probability::very_unlikely ());
    }
  if (GET_CODE (target) == SUBREG && SUBREG_PROMOTED_VAR_P (target))
    /* If this is a scalar in a register that is stored in a wider mode
       than the declared mode, compute the result into its declared mode
       and then convert to the wider mode.  Our value is the computed
       expression.  */
    convert_move (SUBREG_REG (target), res, SUBREG_PROMOTED_SIGN (target));
  else
    emit_move_insn (target, res);
}

/* Add sub/add overflow checking to the statement STMT.
   CODE says whether the operation is +, or -.  */

void
expand_addsub_overflow (location_t loc, tree_code code, tree lhs,
			tree arg0, tree arg1, bool unsr_p, bool uns0_p,
			bool uns1_p, bool is_ubsan, tree *datap)
{
  rtx res, target = NULL_RTX;
  tree fn;
  rtx_code_label *done_label = gen_label_rtx ();
  rtx_code_label *do_error = gen_label_rtx ();
  do_pending_stack_adjust ();
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  scalar_int_mode mode = SCALAR_INT_TYPE_MODE (TREE_TYPE (arg0));
  int prec = GET_MODE_PRECISION (mode);
  rtx sgn = immed_wide_int_const (wi::min_value (prec, SIGNED), mode);
  bool do_xor = false;

  if (is_ubsan)
    gcc_assert (!unsr_p && !uns0_p && !uns1_p);

  if (lhs)
    {
      target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
      if (!is_ubsan)
	write_complex_part (target, const0_rtx, true, false);
    }

  /* We assume both operands and result have the same precision
     here (GET_MODE_BITSIZE (mode)), S stands for signed type
     with that precision, U for unsigned type with that precision,
     sgn for unsigned most significant bit in that precision.
     s1 is signed first operand, u1 is unsigned first operand,
     s2 is signed second operand, u2 is unsigned second operand,
     sr is signed result, ur is unsigned result and the following
     rules say how to compute result (which is always result of
     the operands as if both were unsigned, cast to the right
     signedness) and how to compute whether operation overflowed.

     s1 + s2 -> sr
	res = (S) ((U) s1 + (U) s2)
	ovf = s2 < 0 ? res > s1 : res < s1 (or jump on overflow)
     s1 - s2 -> sr
	res = (S) ((U) s1 - (U) s2)
	ovf = s2 < 0 ? res < s1 : res > s2 (or jump on overflow)
     u1 + u2 -> ur
	res = u1 + u2
	ovf = res < u1 (or jump on carry, but RTL opts will handle it)
     u1 - u2 -> ur
	res = u1 - u2
	ovf = res > u1 (or jump on carry, but RTL opts will handle it)
     s1 + u2 -> sr
	res = (S) ((U) s1 + u2)
	ovf = ((U) res ^ sgn) < u2
     s1 + u2 -> ur
	t1 = (S) (u2 ^ sgn)
	t2 = s1 + t1
	res = (U) t2 ^ sgn
	ovf = t1 < 0 ? t2 > s1 : t2 < s1 (or jump on overflow)
     s1 - u2 -> sr
	res = (S) ((U) s1 - u2)
	ovf = u2 > ((U) s1 ^ sgn)
     s1 - u2 -> ur
	res = (U) s1 - u2
	ovf = s1 < 0 || u2 > (U) s1
     u1 - s2 -> sr
	res = u1 - (U) s2
 	ovf = u1 >= ((U) s2 ^ sgn)
     u1 - s2 -> ur
	t1 = u1 ^ sgn
	t2 = t1 - (U) s2
	res = t2 ^ sgn
	ovf = s2 < 0 ? (S) t2 < (S) t1 : (S) t2 > (S) t1 (or jump on overflow)
     s1 + s2 -> ur
	res = (U) s1 + (U) s2
	ovf = s2 < 0 ? (s1 | (S) res) < 0) : (s1 & (S) res) < 0)
     u1 + u2 -> sr
	res = (S) (u1 + u2)
	ovf = (U) res < u2 || res < 0
     u1 - u2 -> sr
	res = (S) (u1 - u2)
	ovf = u1 >= u2 ? res < 0 : res >= 0
     s1 - s2 -> ur
	res = (U) s1 - (U) s2
	ovf = s2 >= 0 ? ((s1 | (S) res) < 0) : ((s1 & (S) res) < 0)  */

  if (code == PLUS_EXPR && uns0_p && !uns1_p)
    {
      /* PLUS_EXPR is commutative, if operand signedness differs,
	 canonicalize to the first operand being signed and second
	 unsigned to simplify following code.  */
      std::swap (op0, op1);
      std::swap (arg0, arg1);
      uns0_p = false;
      uns1_p = true;
    }

  /* u1 +- u2 -> ur  */
  if (uns0_p && uns1_p && unsr_p)
    {
      insn_code icode = optab_handler (code == PLUS_EXPR ? uaddv4_optab
                                       : usubv4_optab, mode);
      if (icode != CODE_FOR_nothing)
	{
	  class expand_operand ops[4];
	  rtx_insn *last = get_last_insn ();

	  res = gen_reg_rtx (mode);
	  create_output_operand (&ops[0], res, mode);
	  create_input_operand (&ops[1], op0, mode);
	  create_input_operand (&ops[2], op1, mode);
	  create_fixed_operand (&ops[3], do_error);
	  if (maybe_expand_insn (icode, 4, ops))
	    {
	      last = get_last_insn ();
	      if (profile_status_for_fn (cfun) != PROFILE_ABSENT
		  && JUMP_P (last)
		  && any_condjump_p (last)
		  && !find_reg_note (last, REG_BR_PROB, 0))
		add_reg_br_prob_note (last,
				      profile_probability::very_unlikely ());
	      emit_jump (done_label);
	      goto do_error_label;
	    }

	  delete_insns_since (last);
	}

      /* Compute the operation.  On RTL level, the addition is always
	 unsigned.  */
      res = expand_binop (mode, code == PLUS_EXPR ? add_optab : sub_optab,
			  op0, op1, NULL_RTX, false, OPTAB_LIB_WIDEN);
      rtx tem = op0;
      /* For PLUS_EXPR, the operation is commutative, so we can pick
	 operand to compare against.  For prec <= BITS_PER_WORD, I think
	 preferring REG operand is better over CONST_INT, because
	 the CONST_INT might enlarge the instruction or CSE would need
	 to figure out we'd already loaded it into a register before.
	 For prec > BITS_PER_WORD, I think CONST_INT might be more beneficial,
	 as then the multi-word comparison can be perhaps simplified.  */
      if (code == PLUS_EXPR
	  && (prec <= BITS_PER_WORD
	      ? (CONST_SCALAR_INT_P (op0) && REG_P (op1))
	      : CONST_SCALAR_INT_P (op1)))
	tem = op1;
      do_compare_rtx_and_jump (res, tem, code == PLUS_EXPR ? GEU : LEU,
			       true, mode, NULL_RTX, NULL, done_label,
			       profile_probability::very_likely ());
      goto do_error_label;
    }

  /* s1 +- u2 -> sr  */
  if (!uns0_p && uns1_p && !unsr_p)
    {
      /* Compute the operation.  On RTL level, the addition is always
	 unsigned.  */
      res = expand_binop (mode, code == PLUS_EXPR ? add_optab : sub_optab,
			  op0, op1, NULL_RTX, false, OPTAB_LIB_WIDEN);
      rtx tem = expand_binop (mode, add_optab,
			      code == PLUS_EXPR ? res : op0, sgn,
			      NULL_RTX, false, OPTAB_LIB_WIDEN);
      do_compare_rtx_and_jump (tem, op1, GEU, true, mode, NULL_RTX, NULL,
			       done_label, profile_probability::very_likely ());
      goto do_error_label;
    }

  /* s1 + u2 -> ur  */
  if (code == PLUS_EXPR && !uns0_p && uns1_p && unsr_p)
    {
      op1 = expand_binop (mode, add_optab, op1, sgn, NULL_RTX, false,
			  OPTAB_LIB_WIDEN);
      /* As we've changed op1, we have to avoid using the value range
	 for the original argument.  */
      arg1 = error_mark_node;
      do_xor = true;
      goto do_signed;
    }

  /* u1 - s2 -> ur  */
  if (code == MINUS_EXPR && uns0_p && !uns1_p && unsr_p)
    {
      op0 = expand_binop (mode, add_optab, op0, sgn, NULL_RTX, false,
			  OPTAB_LIB_WIDEN);
      /* As we've changed op0, we have to avoid using the value range
	 for the original argument.  */
      arg0 = error_mark_node;
      do_xor = true;
      goto do_signed;
    }

  /* s1 - u2 -> ur  */
  if (code == MINUS_EXPR && !uns0_p && uns1_p && unsr_p)
    {
      /* Compute the operation.  On RTL level, the addition is always
	 unsigned.  */
      res = expand_binop (mode, sub_optab, op0, op1, NULL_RTX, false,
			  OPTAB_LIB_WIDEN);
      int pos_neg = get_range_pos_neg (arg0);
      if (pos_neg == 2)
	/* If ARG0 is known to be always negative, this is always overflow.  */
	emit_jump (do_error);
      else if (pos_neg == 3)
	/* If ARG0 is not known to be always positive, check at runtime.  */
	do_compare_rtx_and_jump (op0, const0_rtx, LT, false, mode, NULL_RTX,
				 NULL, do_error, profile_probability::very_unlikely ());
      do_compare_rtx_and_jump (op1, op0, LEU, true, mode, NULL_RTX, NULL,
			       done_label, profile_probability::very_likely ());
      goto do_error_label;
    }

  /* u1 - s2 -> sr  */
  if (code == MINUS_EXPR && uns0_p && !uns1_p && !unsr_p)
    {
      /* Compute the operation.  On RTL level, the addition is always
	 unsigned.  */
      res = expand_binop (mode, sub_optab, op0, op1, NULL_RTX, false,
			  OPTAB_LIB_WIDEN);
      rtx tem = expand_binop (mode, add_optab, op1, sgn, NULL_RTX, false,
			      OPTAB_LIB_WIDEN);
      do_compare_rtx_and_jump (op0, tem, LTU, true, mode, NULL_RTX, NULL,
			       done_label, profile_probability::very_likely ());
      goto do_error_label;
    }

  /* u1 + u2 -> sr  */
  if (code == PLUS_EXPR && uns0_p && uns1_p && !unsr_p)
    {
      /* Compute the operation.  On RTL level, the addition is always
	 unsigned.  */
      res = expand_binop (mode, add_optab, op0, op1, NULL_RTX, false,
			  OPTAB_LIB_WIDEN);
      do_compare_rtx_and_jump (res, const0_rtx, LT, false, mode, NULL_RTX,
			       NULL, do_error, profile_probability::very_unlikely ());
      rtx tem = op1;
      /* The operation is commutative, so we can pick operand to compare
	 against.  For prec <= BITS_PER_WORD, I think preferring REG operand
	 is better over CONST_INT, because the CONST_INT might enlarge the
	 instruction or CSE would need to figure out we'd already loaded it
	 into a register before.  For prec > BITS_PER_WORD, I think CONST_INT
	 might be more beneficial, as then the multi-word comparison can be
	 perhaps simplified.  */
      if (prec <= BITS_PER_WORD
	  ? (CONST_SCALAR_INT_P (op1) && REG_P (op0))
	  : CONST_SCALAR_INT_P (op0))
	tem = op0;
      do_compare_rtx_and_jump (res, tem, GEU, true, mode, NULL_RTX, NULL,
			       done_label, profile_probability::very_likely ());
      goto do_error_label;
    }

  /* s1 +- s2 -> ur  */
  if (!uns0_p && !uns1_p && unsr_p)
    {
      /* Compute the operation.  On RTL level, the addition is always
	 unsigned.  */
      res = expand_binop (mode, code == PLUS_EXPR ? add_optab : sub_optab,
			  op0, op1, NULL_RTX, false, OPTAB_LIB_WIDEN);
      int pos_neg = get_range_pos_neg (arg1);
      if (code == PLUS_EXPR)
	{
	  int pos_neg0 = get_range_pos_neg (arg0);
	  if (pos_neg0 != 3 && pos_neg == 3)
	    {
	      std::swap (op0, op1);
	      pos_neg = pos_neg0;
	    }
	}
      rtx tem;
      if (pos_neg != 3)
	{
	  tem = expand_binop (mode, ((pos_neg == 1) ^ (code == MINUS_EXPR))
				    ? and_optab : ior_optab,
			      op0, res, NULL_RTX, false, OPTAB_LIB_WIDEN);
	  do_compare_rtx_and_jump (tem, const0_rtx, GE, false, mode, NULL,
				   NULL, done_label, profile_probability::very_likely ());
	}
      else
	{
	  rtx_code_label *do_ior_label = gen_label_rtx ();
	  do_compare_rtx_and_jump (op1, const0_rtx,
				   code == MINUS_EXPR ? GE : LT, false, mode,
				   NULL_RTX, NULL, do_ior_label,
				   profile_probability::even ());
	  tem = expand_binop (mode, and_optab, op0, res, NULL_RTX, false,
			      OPTAB_LIB_WIDEN);
	  do_compare_rtx_and_jump (tem, const0_rtx, GE, false, mode, NULL_RTX,
				   NULL, done_label, profile_probability::very_likely ());
	  emit_jump (do_error);
	  emit_label (do_ior_label);
	  tem = expand_binop (mode, ior_optab, op0, res, NULL_RTX, false,
			      OPTAB_LIB_WIDEN);
	  do_compare_rtx_and_jump (tem, const0_rtx, GE, false, mode, NULL_RTX,
				   NULL, done_label, profile_probability::very_likely ());
	}
      goto do_error_label;
    }

  /* u1 - u2 -> sr  */
  if (code == MINUS_EXPR && uns0_p && uns1_p && !unsr_p)
    {
      /* Compute the operation.  On RTL level, the addition is always
	 unsigned.  */
      res = expand_binop (mode, sub_optab, op0, op1, NULL_RTX, false,
			  OPTAB_LIB_WIDEN);
      rtx_code_label *op0_geu_op1 = gen_label_rtx ();
      do_compare_rtx_and_jump (op0, op1, GEU, true, mode, NULL_RTX, NULL,
			       op0_geu_op1, profile_probability::even ());
      do_compare_rtx_and_jump (res, const0_rtx, LT, false, mode, NULL_RTX,
			       NULL, done_label, profile_probability::very_likely ());
      emit_jump (do_error);
      emit_label (op0_geu_op1);
      do_compare_rtx_and_jump (res, const0_rtx, GE, false, mode, NULL_RTX,
			       NULL, done_label, profile_probability::very_likely ());
      goto do_error_label;
    }

  gcc_assert (!uns0_p && !uns1_p && !unsr_p);

  /* s1 +- s2 -> sr  */
 do_signed:
  {
    insn_code icode = optab_handler (code == PLUS_EXPR ? addv4_optab
				     : subv4_optab, mode);
    if (icode != CODE_FOR_nothing)
      {
	class expand_operand ops[4];
	rtx_insn *last = get_last_insn ();

	res = gen_reg_rtx (mode);
	create_output_operand (&ops[0], res, mode);
	create_input_operand (&ops[1], op0, mode);
	create_input_operand (&ops[2], op1, mode);
	create_fixed_operand (&ops[3], do_error);
	if (maybe_expand_insn (icode, 4, ops))
	  {
	    last = get_last_insn ();
	    if (profile_status_for_fn (cfun) != PROFILE_ABSENT
		&& JUMP_P (last)
		&& any_condjump_p (last)
		&& !find_reg_note (last, REG_BR_PROB, 0))
	      add_reg_br_prob_note (last,
				    profile_probability::very_unlikely ());
	    emit_jump (done_label);
	    goto do_error_label;
	  }

	delete_insns_since (last);
      }

    /* Compute the operation.  On RTL level, the addition is always
       unsigned.  */
    res = expand_binop (mode, code == PLUS_EXPR ? add_optab : sub_optab,
			op0, op1, NULL_RTX, false, OPTAB_LIB_WIDEN);

    /* If we can prove that one of the arguments (for MINUS_EXPR only
       the second operand, as subtraction is not commutative) is always
       non-negative or always negative, we can do just one comparison
       and conditional jump.  */
    int pos_neg = get_range_pos_neg (arg1);
    if (code == PLUS_EXPR)
      {
	int pos_neg0 = get_range_pos_neg (arg0);
	if (pos_neg0 != 3 && pos_neg == 3)
	  {
	    std::swap (op0, op1);
	    pos_neg = pos_neg0;
	  }
      }

    /* Addition overflows if and only if the two operands have the same sign,
       and the result has the opposite sign.  Subtraction overflows if and
       only if the two operands have opposite sign, and the subtrahend has
       the same sign as the result.  Here 0 is counted as positive.  */
    if (pos_neg == 3)
      {
	/* Compute op0 ^ op1 (operands have opposite sign).  */
        rtx op_xor = expand_binop (mode, xor_optab, op0, op1, NULL_RTX, false,
				   OPTAB_LIB_WIDEN);

	/* Compute res ^ op1 (result and 2nd operand have opposite sign).  */
	rtx res_xor = expand_binop (mode, xor_optab, res, op1, NULL_RTX, false,
				    OPTAB_LIB_WIDEN);

	rtx tem;
	if (code == PLUS_EXPR)
	  {
	    /* Compute (res ^ op1) & ~(op0 ^ op1).  */
	    tem = expand_unop (mode, one_cmpl_optab, op_xor, NULL_RTX, false);
	    tem = expand_binop (mode, and_optab, res_xor, tem, NULL_RTX, false,
				OPTAB_LIB_WIDEN);
	  }
	else
	  {
	    /* Compute (op0 ^ op1) & ~(res ^ op1).  */
	    tem = expand_unop (mode, one_cmpl_optab, res_xor, NULL_RTX, false);
	    tem = expand_binop (mode, and_optab, op_xor, tem, NULL_RTX, false,
				OPTAB_LIB_WIDEN);
	  }

	/* No overflow if the result has bit sign cleared.  */
	do_compare_rtx_and_jump (tem, const0_rtx, GE, false, mode, NULL_RTX,
				 NULL, done_label, profile_probability::very_likely ());
      }

    /* Compare the result of the operation with the first operand.
       No overflow for addition if second operand is positive and result
       is larger or second operand is negative and result is smaller.
       Likewise for subtraction with sign of second operand flipped.  */
    else
      do_compare_rtx_and_jump (res, op0,
			       (pos_neg == 1) ^ (code == MINUS_EXPR) ? GE : LE,
			       false, mode, NULL_RTX, NULL, done_label,
			       profile_probability::very_likely ());
  }

 do_error_label:
  emit_label (do_error);
  if (is_ubsan)
    {
      /* Expand the ubsan builtin call.  */
      push_temp_slots ();
      fn = ubsan_build_overflow_builtin (code, loc, TREE_TYPE (arg0),
					 arg0, arg1, datap);
      expand_normal (fn);
      pop_temp_slots ();
      do_pending_stack_adjust ();
    }
  else if (lhs)
    expand_arith_set_overflow (lhs, target);

  /* We're done.  */
  emit_label (done_label);

  if (lhs)
    {
      if (is_ubsan)
	expand_ubsan_result_store (lhs, target, mode, res, do_error);
      else
	{
	  if (do_xor)
	    res = expand_binop (mode, add_optab, res, sgn, NULL_RTX, false,
				OPTAB_LIB_WIDEN);

	  expand_arith_overflow_result_store (lhs, target, mode, res);
	}
    }
}

/* Add negate overflow checking to the statement STMT.  */

static void
expand_neg_overflow (location_t loc, tree lhs, tree arg1, bool is_ubsan,
		     tree *datap)
{
  rtx res, op1;
  tree fn;
  rtx_code_label *done_label, *do_error;
  rtx target = NULL_RTX;

  done_label = gen_label_rtx ();
  do_error = gen_label_rtx ();

  do_pending_stack_adjust ();
  op1 = expand_normal (arg1);

  scalar_int_mode mode = SCALAR_INT_TYPE_MODE (TREE_TYPE (arg1));
  if (lhs)
    {
      target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
      if (!is_ubsan)
	write_complex_part (target, const0_rtx, true, false);
    }

  enum insn_code icode = optab_handler (negv3_optab, mode);
  if (icode != CODE_FOR_nothing)
    {
      class expand_operand ops[3];
      rtx_insn *last = get_last_insn ();

      res = gen_reg_rtx (mode);
      create_output_operand (&ops[0], res, mode);
      create_input_operand (&ops[1], op1, mode);
      create_fixed_operand (&ops[2], do_error);
      if (maybe_expand_insn (icode, 3, ops))
	{
	  last = get_last_insn ();
	  if (profile_status_for_fn (cfun) != PROFILE_ABSENT
	      && JUMP_P (last)
	      && any_condjump_p (last)
	      && !find_reg_note (last, REG_BR_PROB, 0))
	    add_reg_br_prob_note (last,
				  profile_probability::very_unlikely ());
	  emit_jump (done_label);
        }
      else
	{
	  delete_insns_since (last);
	  icode = CODE_FOR_nothing;
	}
    }

  if (icode == CODE_FOR_nothing)
    {
      /* Compute the operation.  On RTL level, the addition is always
	 unsigned.  */
      res = expand_unop (mode, neg_optab, op1, NULL_RTX, false);

      /* Compare the operand with the most negative value.  */
      rtx minv = expand_normal (TYPE_MIN_VALUE (TREE_TYPE (arg1)));
      do_compare_rtx_and_jump (op1, minv, NE, true, mode, NULL_RTX, NULL,
			       done_label, profile_probability::very_likely ());
    }

  emit_label (do_error);
  if (is_ubsan)
    {
      /* Expand the ubsan builtin call.  */
      push_temp_slots ();
      fn = ubsan_build_overflow_builtin (NEGATE_EXPR, loc, TREE_TYPE (arg1),
					 arg1, NULL_TREE, datap);
      expand_normal (fn);
      pop_temp_slots ();
      do_pending_stack_adjust ();
    }
  else if (lhs)
    expand_arith_set_overflow (lhs, target);

  /* We're done.  */
  emit_label (done_label);

  if (lhs)
    {
      if (is_ubsan)
	expand_ubsan_result_store (lhs, target, mode, res, do_error);
      else
	expand_arith_overflow_result_store (lhs, target, mode, res);
    }
}

/* Return true if UNS WIDEN_MULT_EXPR with result mode WMODE and operand
   mode MODE can be expanded without using a libcall.  */

static bool
can_widen_mult_without_libcall (scalar_int_mode wmode, scalar_int_mode mode,
				rtx op0, rtx op1, bool uns)
{
  if (find_widening_optab_handler (umul_widen_optab, wmode, mode)
      != CODE_FOR_nothing)
    return true;

  if (find_widening_optab_handler (smul_widen_optab, wmode, mode)
      != CODE_FOR_nothing)
    return true;

  rtx_insn *last = get_last_insn ();
  if (CONSTANT_P (op0))
    op0 = convert_modes (wmode, mode, op0, uns);
  else
    op0 = gen_raw_REG (wmode, LAST_VIRTUAL_REGISTER + 1);
  if (CONSTANT_P (op1))
    op1 = convert_modes (wmode, mode, op1, uns);
  else
    op1 = gen_raw_REG (wmode, LAST_VIRTUAL_REGISTER + 2);
  rtx ret = expand_mult (wmode, op0, op1, NULL_RTX, uns, true);
  delete_insns_since (last);
  return ret != NULL_RTX;
}

/* Add mul overflow checking to the statement STMT.  */

static void
expand_mul_overflow (location_t loc, tree lhs, tree arg0, tree arg1,
		     bool unsr_p, bool uns0_p, bool uns1_p, bool is_ubsan,
		     tree *datap)
{
  rtx res, op0, op1;
  tree fn, type;
  rtx_code_label *done_label, *do_error;
  rtx target = NULL_RTX;
  signop sign;
  enum insn_code icode;
  int save_flag_trapv = flag_trapv;

  /* We don't want any __mulv?i3 etc. calls from the expansion of
     these internal functions, so disable -ftrapv temporarily.  */
  flag_trapv = 0;
  done_label = gen_label_rtx ();
  do_error = gen_label_rtx ();

  do_pending_stack_adjust ();
  op0 = expand_normal (arg0);
  op1 = expand_normal (arg1);

  scalar_int_mode mode = SCALAR_INT_TYPE_MODE (TREE_TYPE (arg0));
  bool uns = unsr_p;
  if (lhs)
    {
      target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
      if (!is_ubsan)
	write_complex_part (target, const0_rtx, true, false);
    }

  if (is_ubsan)
    gcc_assert (!unsr_p && !uns0_p && !uns1_p);

  /* We assume both operands and result have the same precision
     here (GET_MODE_BITSIZE (mode)), S stands for signed type
     with that precision, U for unsigned type with that precision,
     sgn for unsigned most significant bit in that precision.
     s1 is signed first operand, u1 is unsigned first operand,
     s2 is signed second operand, u2 is unsigned second operand,
     sr is signed result, ur is unsigned result and the following
     rules say how to compute result (which is always result of
     the operands as if both were unsigned, cast to the right
     signedness) and how to compute whether operation overflowed.
     main_ovf (false) stands for jump on signed multiplication
     overflow or the main algorithm with uns == false.
     main_ovf (true) stands for jump on unsigned multiplication
     overflow or the main algorithm with uns == true.

     s1 * s2 -> sr
	res = (S) ((U) s1 * (U) s2)
	ovf = main_ovf (false)
     u1 * u2 -> ur
	res = u1 * u2
	ovf = main_ovf (true)
     s1 * u2 -> ur
	res = (U) s1 * u2
	ovf = (s1 < 0 && u2) || main_ovf (true)
     u1 * u2 -> sr
	res = (S) (u1 * u2)
	ovf = res < 0 || main_ovf (true)
     s1 * u2 -> sr
	res = (S) ((U) s1 * u2)
	ovf = (S) u2 >= 0 ? main_ovf (false)
			  : (s1 != 0 && (s1 != -1 || u2 != (U) res))
     s1 * s2 -> ur
	t1 = (s1 & s2) < 0 ? (-(U) s1) : ((U) s1)
	t2 = (s1 & s2) < 0 ? (-(U) s2) : ((U) s2)
	res = t1 * t2
	ovf = (s1 ^ s2) < 0 ? (s1 && s2) : main_ovf (true)  */

  if (uns0_p && !uns1_p)
    {
      /* Multiplication is commutative, if operand signedness differs,
	 canonicalize to the first operand being signed and second
	 unsigned to simplify following code.  */
      std::swap (op0, op1);
      std::swap (arg0, arg1);
      uns0_p = false;
      uns1_p = true;
    }

  int pos_neg0 = get_range_pos_neg (arg0);
  int pos_neg1 = get_range_pos_neg (arg1);
  /* Unsigned types with smaller than mode precision, even if they have most
     significant bit set, are still zero-extended.  */
  if (uns0_p && TYPE_PRECISION (TREE_TYPE (arg0)) < GET_MODE_PRECISION (mode))
    pos_neg0 = 1;
  if (uns1_p && TYPE_PRECISION (TREE_TYPE (arg1)) < GET_MODE_PRECISION (mode))
    pos_neg1 = 1;

  /* s1 * u2 -> ur  */
  if (!uns0_p && uns1_p && unsr_p)
    {
      switch (pos_neg0)
	{
	case 1:
	  /* If s1 is non-negative, just perform normal u1 * u2 -> ur.  */
	  goto do_main;
	case 2:
	  /* If s1 is negative, avoid the main code, just multiply and
	     signal overflow if op1 is not 0.  */
	  struct separate_ops ops;
	  ops.code = MULT_EXPR;
	  ops.type = TREE_TYPE (arg1);
	  ops.op0 = make_tree (ops.type, op0);
	  ops.op1 = make_tree (ops.type, op1);
	  ops.op2 = NULL_TREE;
	  ops.location = loc;
	  res = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);
	  do_compare_rtx_and_jump (op1, const0_rtx, EQ, true, mode, NULL_RTX,
				   NULL, done_label, profile_probability::very_likely ());
	  goto do_error_label;
	case 3:
	  if (get_min_precision (arg1, UNSIGNED)
	      + get_min_precision (arg0, SIGNED) <= GET_MODE_PRECISION (mode))
	    {
	      /* If the first operand is sign extended from narrower type, the
		 second operand is zero extended from narrower type and
		 the sum of the two precisions is smaller or equal to the
		 result precision: if the first argument is at runtime
		 non-negative, maximum result will be 0x7e81 or 0x7f..fe80..01
		 and there will be no overflow, if the first argument is
		 negative and the second argument zero, the result will be
		 0 and there will be no overflow, if the first argument is
		 negative and the second argument positive, the result when
		 treated as signed will be negative (minimum -0x7f80 or
		 -0x7f..f80..0) there will be always overflow.  So, do
		 res = (U) (s1 * u2)
		 ovf = (S) res < 0  */
	      struct separate_ops ops;
	      ops.code = MULT_EXPR;
	      ops.type
		= build_nonstandard_integer_type (GET_MODE_PRECISION (mode),
						  1);
	      ops.op0 = make_tree (ops.type, op0);
	      ops.op1 = make_tree (ops.type, op1);
	      ops.op2 = NULL_TREE;
	      ops.location = loc;
	      res = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);
	      do_compare_rtx_and_jump (res, const0_rtx, GE, false,
				       mode, NULL_RTX, NULL, done_label,
				       profile_probability::very_likely ());
	      goto do_error_label;
	    }
	  rtx_code_label *do_main_label;
	  do_main_label = gen_label_rtx ();
	  do_compare_rtx_and_jump (op0, const0_rtx, GE, false, mode, NULL_RTX,
				   NULL, do_main_label, profile_probability::very_likely ());
	  do_compare_rtx_and_jump (op1, const0_rtx, EQ, true, mode, NULL_RTX,
				   NULL, do_main_label, profile_probability::very_likely ());
	  expand_arith_set_overflow (lhs, target);
	  emit_label (do_main_label);
	  goto do_main;
	default:
	  gcc_unreachable ();
	}
    }

  /* u1 * u2 -> sr  */
  if (uns0_p && uns1_p && !unsr_p)
    {
      if ((pos_neg0 | pos_neg1) == 1)
	{
	  /* If both arguments are zero extended from narrower types,
	     the MSB will be clear on both and so we can pretend it is
	     a normal s1 * s2 -> sr multiplication.  */
	  uns0_p = false;
	  uns1_p = false;
	}
      else
	uns = true;
      /* Rest of handling of this case after res is computed.  */
      goto do_main;
    }

  /* s1 * u2 -> sr  */
  if (!uns0_p && uns1_p && !unsr_p)
    {
      switch (pos_neg1)
	{
	case 1:
	  goto do_main;
	case 2:
	  /* If (S) u2 is negative (i.e. u2 is larger than maximum of S,
	     avoid the main code, just multiply and signal overflow
	     unless 0 * u2 or -1 * ((U) Smin).  */
	  struct separate_ops ops;
	  ops.code = MULT_EXPR;
	  ops.type = TREE_TYPE (arg1);
	  ops.op0 = make_tree (ops.type, op0);
	  ops.op1 = make_tree (ops.type, op1);
	  ops.op2 = NULL_TREE;
	  ops.location = loc;
	  res = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);
	  do_compare_rtx_and_jump (op0, const0_rtx, EQ, true, mode, NULL_RTX,
				   NULL, done_label, profile_probability::very_likely ());
	  do_compare_rtx_and_jump (op0, constm1_rtx, NE, true, mode, NULL_RTX,
				   NULL, do_error, profile_probability::very_unlikely ());
	  int prec;
	  prec = GET_MODE_PRECISION (mode);
	  rtx sgn;
	  sgn = immed_wide_int_const (wi::min_value (prec, SIGNED), mode);
	  do_compare_rtx_and_jump (op1, sgn, EQ, true, mode, NULL_RTX,
				   NULL, done_label, profile_probability::very_likely ());
	  goto do_error_label;
	case 3:
	  /* Rest of handling of this case after res is computed.  */
	  goto do_main;
	default:
	  gcc_unreachable ();
	}
    }

  /* s1 * s2 -> ur  */
  if (!uns0_p && !uns1_p && unsr_p)
    {
      rtx tem;
      switch (pos_neg0 | pos_neg1)
	{
	case 1: /* Both operands known to be non-negative.  */
	  goto do_main;
	case 2: /* Both operands known to be negative.  */
	  op0 = expand_unop (mode, neg_optab, op0, NULL_RTX, false);
	  op1 = expand_unop (mode, neg_optab, op1, NULL_RTX, false);
	  /* Avoid looking at arg0/arg1 ranges, as we've changed
	     the arguments.  */
	  arg0 = error_mark_node;
	  arg1 = error_mark_node;
	  goto do_main;
	case 3:
	  if ((pos_neg0 ^ pos_neg1) == 3)
	    {
	      /* If one operand is known to be negative and the other
		 non-negative, this overflows always, unless the non-negative
		 one is 0.  Just do normal multiply and set overflow
		 unless one of the operands is 0.  */
	      struct separate_ops ops;
	      ops.code = MULT_EXPR;
	      ops.type
		= build_nonstandard_integer_type (GET_MODE_PRECISION (mode),
						  1);
	      ops.op0 = make_tree (ops.type, op0);
	      ops.op1 = make_tree (ops.type, op1);
	      ops.op2 = NULL_TREE;
	      ops.location = loc;
	      res = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);
	      do_compare_rtx_and_jump (pos_neg0 == 1 ? op0 : op1, const0_rtx, EQ,
				       true, mode, NULL_RTX, NULL, done_label,
				       profile_probability::very_likely ());
	      goto do_error_label;
	    }
	  if (get_min_precision (arg0, SIGNED)
	      + get_min_precision (arg1, SIGNED) <= GET_MODE_PRECISION (mode))
	    {
	      /* If both operands are sign extended from narrower types and
		 the sum of the two precisions is smaller or equal to the
		 result precision: if both arguments are at runtime
		 non-negative, maximum result will be 0x3f01 or 0x3f..f0..01
		 and there will be no overflow, if both arguments are negative,
		 maximum result will be 0x40..00 and there will be no overflow
		 either, if one argument is positive and the other argument
		 negative, the result when treated as signed will be negative
		 and there will be always overflow, and if one argument is
		 zero and the other negative the result will be zero and no
		 overflow.  So, do
		 res = (U) (s1 * s2)
		 ovf = (S) res < 0  */
	      struct separate_ops ops;
	      ops.code = MULT_EXPR;
	      ops.type
		= build_nonstandard_integer_type (GET_MODE_PRECISION (mode),
						  1);
	      ops.op0 = make_tree (ops.type, op0);
	      ops.op1 = make_tree (ops.type, op1);
	      ops.op2 = NULL_TREE;
	      ops.location = loc;
	      res = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);
	      do_compare_rtx_and_jump (res, const0_rtx, GE, false,
				       mode, NULL_RTX, NULL, done_label,
				       profile_probability::very_likely ());
	      goto do_error_label;
	    }
	  /* The general case, do all the needed comparisons at runtime.  */
	  rtx_code_label *do_main_label, *after_negate_label;
	  rtx rop0, rop1;
	  rop0 = gen_reg_rtx (mode);
	  rop1 = gen_reg_rtx (mode);
	  emit_move_insn (rop0, op0);
	  emit_move_insn (rop1, op1);
	  op0 = rop0;
	  op1 = rop1;
	  do_main_label = gen_label_rtx ();
	  after_negate_label = gen_label_rtx ();
	  tem = expand_binop (mode, and_optab, op0, op1, NULL_RTX, false,
			      OPTAB_LIB_WIDEN);
	  do_compare_rtx_and_jump (tem, const0_rtx, GE, false, mode, NULL_RTX,
				   NULL, after_negate_label, profile_probability::very_likely ());
	  /* Both arguments negative here, negate them and continue with
	     normal unsigned overflow checking multiplication.  */
	  emit_move_insn (op0, expand_unop (mode, neg_optab, op0,
					    NULL_RTX, false));
	  emit_move_insn (op1, expand_unop (mode, neg_optab, op1,
					    NULL_RTX, false));
	  /* Avoid looking at arg0/arg1 ranges, as we might have changed
	     the arguments.  */
	  arg0 = error_mark_node;
	  arg1 = error_mark_node;
	  emit_jump (do_main_label);
	  emit_label (after_negate_label);
	  tem = expand_binop (mode, xor_optab, op0, op1, NULL_RTX, false,
			      OPTAB_LIB_WIDEN);
	  do_compare_rtx_and_jump (tem, const0_rtx, GE, false, mode, NULL_RTX,
				   NULL, do_main_label,
				   profile_probability::very_likely ());
	  /* One argument is negative here, the other positive.  This
	     overflows always, unless one of the arguments is 0.  But
	     if e.g. s2 is 0, (U) s1 * 0 doesn't overflow, whatever s1
	     is, thus we can keep do_main code oring in overflow as is.  */
	  if (pos_neg0 != 2)
	    do_compare_rtx_and_jump (op0, const0_rtx, EQ, true, mode, NULL_RTX,
				     NULL, do_main_label,
				     profile_probability::very_unlikely ());
	  if (pos_neg1 != 2)
	    do_compare_rtx_and_jump (op1, const0_rtx, EQ, true, mode, NULL_RTX,
				     NULL, do_main_label,
				     profile_probability::very_unlikely ());
	  expand_arith_set_overflow (lhs, target);
	  emit_label (do_main_label);
	  goto do_main;
	default:
	  gcc_unreachable ();
	}
    }

 do_main:
  type = build_nonstandard_integer_type (GET_MODE_PRECISION (mode), uns);
  sign = uns ? UNSIGNED : SIGNED;
  icode = optab_handler (uns ? umulv4_optab : mulv4_optab, mode);
  if (uns
      && (integer_pow2p (arg0) || integer_pow2p (arg1))
      && (optimize_insn_for_speed_p () || icode == CODE_FOR_nothing))
    {
      /* Optimize unsigned multiplication by power of 2 constant
	 using 2 shifts, one for result, one to extract the shifted
	 out bits to see if they are all zero.
	 Don't do this if optimizing for size and we have umulv4_optab,
	 in that case assume multiplication will be shorter.
	 This is heuristics based on the single target that provides
	 umulv4 right now (i?86/x86_64), if further targets add it, this
	 might need to be revisited.
	 Cases where both operands are constant should be folded already
	 during GIMPLE, and cases where one operand is constant but not
	 power of 2 are questionable, either the WIDEN_MULT_EXPR case
	 below can be done without multiplication, just by shifts and adds,
	 or we'd need to divide the result (and hope it actually doesn't
	 really divide nor multiply) and compare the result of the division
	 with the original operand.  */
      rtx opn0 = op0;
      rtx opn1 = op1;
      tree argn0 = arg0;
      tree argn1 = arg1;
      if (integer_pow2p (arg0))
	{
	  std::swap (opn0, opn1);
	  std::swap (argn0, argn1);
	}
      int cnt = tree_log2 (argn1);
      if (cnt >= 0 && cnt < GET_MODE_PRECISION (mode))
	{
	  rtx upper = const0_rtx;
	  res = expand_shift (LSHIFT_EXPR, mode, opn0, cnt, NULL_RTX, uns);
	  if (cnt != 0)
	    upper = expand_shift (RSHIFT_EXPR, mode, opn0,
				  GET_MODE_PRECISION (mode) - cnt,
				  NULL_RTX, uns);
	  do_compare_rtx_and_jump (upper, const0_rtx, EQ, true, mode,
				   NULL_RTX, NULL, done_label,
				   profile_probability::very_likely ());
	  goto do_error_label;
	}
    }
  if (icode != CODE_FOR_nothing)
    {
      class expand_operand ops[4];
      rtx_insn *last = get_last_insn ();

      res = gen_reg_rtx (mode);
      create_output_operand (&ops[0], res, mode);
      create_input_operand (&ops[1], op0, mode);
      create_input_operand (&ops[2], op1, mode);
      create_fixed_operand (&ops[3], do_error);
      if (maybe_expand_insn (icode, 4, ops))
	{
	  last = get_last_insn ();
	  if (profile_status_for_fn (cfun) != PROFILE_ABSENT
	      && JUMP_P (last)
	      && any_condjump_p (last)
	      && !find_reg_note (last, REG_BR_PROB, 0))
	    add_reg_br_prob_note (last,
				  profile_probability::very_unlikely ());
	  emit_jump (done_label);
        }
      else
	{
	  delete_insns_since (last);
	  icode = CODE_FOR_nothing;
	}
    }

  if (icode == CODE_FOR_nothing)
    {
      struct separate_ops ops;
      int prec = GET_MODE_PRECISION (mode);
      scalar_int_mode hmode, wmode;
      ops.op0 = make_tree (type, op0);
      ops.op1 = make_tree (type, op1);
      ops.op2 = NULL_TREE;
      ops.location = loc;

      /* Optimize unsigned overflow check where we don't use the
	 multiplication result, just whether overflow happened.
	 If we can do MULT_HIGHPART_EXPR, that followed by
	 comparison of the result against zero is cheapest.
	 We'll still compute res, but it should be DCEd later.  */
      use_operand_p use;
      gimple *use_stmt;
      if (!is_ubsan
	  && lhs
	  && uns
	  && !(uns0_p && uns1_p && !unsr_p)
	  && can_mult_highpart_p (mode, uns) == 1
	  && single_imm_use (lhs, &use, &use_stmt)
	  && is_gimple_assign (use_stmt)
	  && gimple_assign_rhs_code (use_stmt) == IMAGPART_EXPR)
	goto highpart;

      if (GET_MODE_2XWIDER_MODE (mode).exists (&wmode)
	  && targetm.scalar_mode_supported_p (wmode)
	  && can_widen_mult_without_libcall (wmode, mode, op0, op1, uns))
	{
	twoxwider:
	  ops.code = WIDEN_MULT_EXPR;
	  ops.type
	    = build_nonstandard_integer_type (GET_MODE_PRECISION (wmode), uns);

	  res = expand_expr_real_2 (&ops, NULL_RTX, wmode, EXPAND_NORMAL);
	  rtx hipart = expand_shift (RSHIFT_EXPR, wmode, res, prec,
				     NULL_RTX, uns);
	  hipart = convert_modes (mode, wmode, hipart, uns);
	  res = convert_modes (mode, wmode, res, uns);
	  if (uns)
	    /* For the unsigned multiplication, there was overflow if
	       HIPART is non-zero.  */
	    do_compare_rtx_and_jump (hipart, const0_rtx, EQ, true, mode,
				     NULL_RTX, NULL, done_label,
				     profile_probability::very_likely ());
	  else
	    {
	      /* RES is used more than once, place it in a pseudo.  */
	      res = force_reg (mode, res);

	      rtx signbit = expand_shift (RSHIFT_EXPR, mode, res, prec - 1,
					  NULL_RTX, 0);
	      /* RES is low half of the double width result, HIPART
		 the high half.  There was overflow if
		 HIPART is different from RES < 0 ? -1 : 0.  */
	      do_compare_rtx_and_jump (signbit, hipart, EQ, true, mode,
				       NULL_RTX, NULL, done_label,
				       profile_probability::very_likely ());
	    }
	}
      else if (can_mult_highpart_p (mode, uns) == 1)
	{
	highpart:
	  ops.code = MULT_HIGHPART_EXPR;
	  ops.type = type;

	  rtx hipart = expand_expr_real_2 (&ops, NULL_RTX, mode,
					   EXPAND_NORMAL);
	  ops.code = MULT_EXPR;
	  res = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);
	  if (uns)
	    /* For the unsigned multiplication, there was overflow if
	       HIPART is non-zero.  */
	    do_compare_rtx_and_jump (hipart, const0_rtx, EQ, true, mode,
				     NULL_RTX, NULL, done_label,
				     profile_probability::very_likely ());
	  else
	    {
	      rtx signbit = expand_shift (RSHIFT_EXPR, mode, res, prec - 1,
					  NULL_RTX, 0);
	      /* RES is low half of the double width result, HIPART
		 the high half.  There was overflow if
		 HIPART is different from RES < 0 ? -1 : 0.  */
	      do_compare_rtx_and_jump (signbit, hipart, EQ, true, mode,
				       NULL_RTX, NULL, done_label,
				       profile_probability::very_likely ());
	    }

	}
      else if (int_mode_for_size (prec / 2, 1).exists (&hmode)
	       && 2 * GET_MODE_PRECISION (hmode) == prec)
	{
	  rtx_code_label *large_op0 = gen_label_rtx ();
	  rtx_code_label *small_op0_large_op1 = gen_label_rtx ();
	  rtx_code_label *one_small_one_large = gen_label_rtx ();
	  rtx_code_label *both_ops_large = gen_label_rtx ();
	  rtx_code_label *after_hipart_neg = uns ? NULL : gen_label_rtx ();
	  rtx_code_label *after_lopart_neg = uns ? NULL : gen_label_rtx ();
	  rtx_code_label *do_overflow = gen_label_rtx ();
	  rtx_code_label *hipart_different = uns ? NULL : gen_label_rtx ();

	  unsigned int hprec = GET_MODE_PRECISION (hmode);
	  rtx hipart0 = expand_shift (RSHIFT_EXPR, mode, op0, hprec,
				      NULL_RTX, uns);
	  hipart0 = convert_modes (hmode, mode, hipart0, uns);
	  rtx lopart0 = convert_modes (hmode, mode, op0, uns);
	  rtx signbit0 = const0_rtx;
	  if (!uns)
	    signbit0 = expand_shift (RSHIFT_EXPR, hmode, lopart0, hprec - 1,
				     NULL_RTX, 0);
	  rtx hipart1 = expand_shift (RSHIFT_EXPR, mode, op1, hprec,
				      NULL_RTX, uns);
	  hipart1 = convert_modes (hmode, mode, hipart1, uns);
	  rtx lopart1 = convert_modes (hmode, mode, op1, uns);
	  rtx signbit1 = const0_rtx;
	  if (!uns)
	    signbit1 = expand_shift (RSHIFT_EXPR, hmode, lopart1, hprec - 1,
				     NULL_RTX, 0);

	  res = gen_reg_rtx (mode);

	  /* True if op0 resp. op1 are known to be in the range of
	     halfstype.  */
	  bool op0_small_p = false;
	  bool op1_small_p = false;
	  /* True if op0 resp. op1 are known to have all zeros or all ones
	     in the upper half of bits, but are not known to be
	     op{0,1}_small_p.  */
	  bool op0_medium_p = false;
	  bool op1_medium_p = false;
	  /* -1 if op{0,1} is known to be negative, 0 if it is known to be
	     nonnegative, 1 if unknown.  */
	  int op0_sign = 1;
	  int op1_sign = 1;

	  if (pos_neg0 == 1)
	    op0_sign = 0;
	  else if (pos_neg0 == 2)
	    op0_sign = -1;
	  if (pos_neg1 == 1)
	    op1_sign = 0;
	  else if (pos_neg1 == 2)
	    op1_sign = -1;

	  unsigned int mprec0 = prec;
	  if (arg0 != error_mark_node)
	    mprec0 = get_min_precision (arg0, sign);
	  if (mprec0 <= hprec)
	    op0_small_p = true;
	  else if (!uns && mprec0 <= hprec + 1)
	    op0_medium_p = true;
	  unsigned int mprec1 = prec;
	  if (arg1 != error_mark_node)
	    mprec1 = get_min_precision (arg1, sign);
	  if (mprec1 <= hprec)
	    op1_small_p = true;
	  else if (!uns && mprec1 <= hprec + 1)
	    op1_medium_p = true;

	  int smaller_sign = 1;
	  int larger_sign = 1;
	  if (op0_small_p)
	    {
	      smaller_sign = op0_sign;
	      larger_sign = op1_sign;
	    }
	  else if (op1_small_p)
	    {
	      smaller_sign = op1_sign;
	      larger_sign = op0_sign;
	    }
	  else if (op0_sign == op1_sign)
	    {
	      smaller_sign = op0_sign;
	      larger_sign = op0_sign;
	    }

	  if (!op0_small_p)
	    do_compare_rtx_and_jump (signbit0, hipart0, NE, true, hmode,
				     NULL_RTX, NULL, large_op0,
				     profile_probability::unlikely ());

	  if (!op1_small_p)
	    do_compare_rtx_and_jump (signbit1, hipart1, NE, true, hmode,
				     NULL_RTX, NULL, small_op0_large_op1,
				     profile_probability::unlikely ());

	  /* If both op0 and op1 are sign (!uns) or zero (uns) extended from
	     hmode to mode, the multiplication will never overflow.  We can
	     do just one hmode x hmode => mode widening multiplication.  */
	  tree halfstype = build_nonstandard_integer_type (hprec, uns);
	  ops.op0 = make_tree (halfstype, lopart0);
	  ops.op1 = make_tree (halfstype, lopart1);
	  ops.code = WIDEN_MULT_EXPR;
	  ops.type = type;
	  rtx thisres
	    = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);
	  emit_move_insn (res, thisres);
	  emit_jump (done_label);

	  emit_label (small_op0_large_op1);

	  /* If op0 is sign (!uns) or zero (uns) extended from hmode to mode,
	     but op1 is not, just swap the arguments and handle it as op1
	     sign/zero extended, op0 not.  */
	  rtx larger = gen_reg_rtx (mode);
	  rtx hipart = gen_reg_rtx (hmode);
	  rtx lopart = gen_reg_rtx (hmode);
	  emit_move_insn (larger, op1);
	  emit_move_insn (hipart, hipart1);
	  emit_move_insn (lopart, lopart0);
	  emit_jump (one_small_one_large);

	  emit_label (large_op0);

	  if (!op1_small_p)
	    do_compare_rtx_and_jump (signbit1, hipart1, NE, true, hmode,
				     NULL_RTX, NULL, both_ops_large,
				     profile_probability::unlikely ());

	  /* If op1 is sign (!uns) or zero (uns) extended from hmode to mode,
	     but op0 is not, prepare larger, hipart and lopart pseudos and
	     handle it together with small_op0_large_op1.  */
	  emit_move_insn (larger, op0);
	  emit_move_insn (hipart, hipart0);
	  emit_move_insn (lopart, lopart1);

	  emit_label (one_small_one_large);

	  /* lopart is the low part of the operand that is sign extended
	     to mode, larger is the other operand, hipart is the
	     high part of larger and lopart0 and lopart1 are the low parts
	     of both operands.
	     We perform lopart0 * lopart1 and lopart * hipart widening
	     multiplications.  */
	  tree halfutype = build_nonstandard_integer_type (hprec, 1);
	  ops.op0 = make_tree (halfutype, lopart0);
	  ops.op1 = make_tree (halfutype, lopart1);
	  rtx lo0xlo1
	    = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);

	  ops.op0 = make_tree (halfutype, lopart);
	  ops.op1 = make_tree (halfutype, hipart);
	  rtx loxhi = gen_reg_rtx (mode);
	  rtx tem = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);
	  emit_move_insn (loxhi, tem);

	  if (!uns)
	    {
	      /* if (hipart < 0) loxhi -= lopart << (bitsize / 2);  */
	      if (larger_sign == 0)
		emit_jump (after_hipart_neg);
	      else if (larger_sign != -1)
		do_compare_rtx_and_jump (hipart, const0_rtx, GE, false, hmode,
					 NULL_RTX, NULL, after_hipart_neg,
					 profile_probability::even ());

	      tem = convert_modes (mode, hmode, lopart, 1);
	      tem = expand_shift (LSHIFT_EXPR, mode, tem, hprec, NULL_RTX, 1);
	      tem = expand_simple_binop (mode, MINUS, loxhi, tem, NULL_RTX,
					 1, OPTAB_WIDEN);
	      emit_move_insn (loxhi, tem);

	      emit_label (after_hipart_neg);

	      /* if (lopart < 0) loxhi -= larger;  */
	      if (smaller_sign == 0)
		emit_jump (after_lopart_neg);
	      else if (smaller_sign != -1)
		do_compare_rtx_and_jump (lopart, const0_rtx, GE, false, hmode,
					 NULL_RTX, NULL, after_lopart_neg,
					 profile_probability::even ());

	      tem = expand_simple_binop (mode, MINUS, loxhi, larger, NULL_RTX,
					 1, OPTAB_WIDEN);
	      emit_move_insn (loxhi, tem);

	      emit_label (after_lopart_neg);
	    }

	  /* loxhi += (uns) lo0xlo1 >> (bitsize / 2);  */
	  tem = expand_shift (RSHIFT_EXPR, mode, lo0xlo1, hprec, NULL_RTX, 1);
	  tem = expand_simple_binop (mode, PLUS, loxhi, tem, NULL_RTX,
				     1, OPTAB_WIDEN);
	  emit_move_insn (loxhi, tem);

	  /* if (loxhi >> (bitsize / 2)
		 == (hmode) loxhi >> (bitsize / 2 - 1))  (if !uns)
	     if (loxhi >> (bitsize / 2) == 0		 (if uns).  */
	  rtx hipartloxhi = expand_shift (RSHIFT_EXPR, mode, loxhi, hprec,
					  NULL_RTX, 0);
	  hipartloxhi = convert_modes (hmode, mode, hipartloxhi, 0);
	  rtx signbitloxhi = const0_rtx;
	  if (!uns)
	    signbitloxhi = expand_shift (RSHIFT_EXPR, hmode,
					 convert_modes (hmode, mode,
							loxhi, 0),
					 hprec - 1, NULL_RTX, 0);

	  do_compare_rtx_and_jump (signbitloxhi, hipartloxhi, NE, true, hmode,
				   NULL_RTX, NULL, do_overflow,
				   profile_probability::very_unlikely ());

	  /* res = (loxhi << (bitsize / 2)) | (hmode) lo0xlo1;  */
	  rtx loxhishifted = expand_shift (LSHIFT_EXPR, mode, loxhi, hprec,
					   NULL_RTX, 1);
	  tem = convert_modes (mode, hmode,
			       convert_modes (hmode, mode, lo0xlo1, 1), 1);

	  tem = expand_simple_binop (mode, IOR, loxhishifted, tem, res,
				     1, OPTAB_WIDEN);
	  if (tem != res)
	    emit_move_insn (res, tem);
	  emit_jump (done_label);

	  emit_label (both_ops_large);

	  /* If both operands are large (not sign (!uns) or zero (uns)
	     extended from hmode), then perform the full multiplication
	     which will be the result of the operation.
	     The only cases which don't overflow are for signed multiplication
	     some cases where both hipart0 and highpart1 are 0 or -1.
	     For unsigned multiplication when high parts are both non-zero
	     this overflows always.  */
	  ops.code = MULT_EXPR;
	  ops.op0 = make_tree (type, op0);
	  ops.op1 = make_tree (type, op1);
	  tem = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);
	  emit_move_insn (res, tem);

	  if (!uns)
	    {
	      if (!op0_medium_p)
		{
		  tem = expand_simple_binop (hmode, PLUS, hipart0, const1_rtx,
					     NULL_RTX, 1, OPTAB_WIDEN);
		  do_compare_rtx_and_jump (tem, const1_rtx, GTU, true, hmode,
					   NULL_RTX, NULL, do_error,
					   profile_probability::very_unlikely ());
		}

	      if (!op1_medium_p)
		{
		  tem = expand_simple_binop (hmode, PLUS, hipart1, const1_rtx,
					     NULL_RTX, 1, OPTAB_WIDEN);
		  do_compare_rtx_and_jump (tem, const1_rtx, GTU, true, hmode,
					   NULL_RTX, NULL, do_error,
					   profile_probability::very_unlikely ());
		}

	      /* At this point hipart{0,1} are both in [-1, 0].  If they are
		 the same, overflow happened if res is non-positive, if they
		 are different, overflow happened if res is positive.  */
	      if (op0_sign != 1 && op1_sign != 1 && op0_sign != op1_sign)
		emit_jump (hipart_different);
	      else if (op0_sign == 1 || op1_sign == 1)
		do_compare_rtx_and_jump (hipart0, hipart1, NE, true, hmode,
					 NULL_RTX, NULL, hipart_different,
					 profile_probability::even ());

	      do_compare_rtx_and_jump (res, const0_rtx, LE, false, mode,
				       NULL_RTX, NULL, do_error,
				       profile_probability::very_unlikely ());
	      emit_jump (done_label);

	      emit_label (hipart_different);

	      do_compare_rtx_and_jump (res, const0_rtx, GE, false, mode,
				       NULL_RTX, NULL, do_error,
				       profile_probability::very_unlikely ());
	      emit_jump (done_label);
	    }

	  emit_label (do_overflow);

	  /* Overflow, do full multiplication and fallthru into do_error.  */
	  ops.op0 = make_tree (type, op0);
	  ops.op1 = make_tree (type, op1);
	  tem = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);
	  emit_move_insn (res, tem);
	}
      else if (GET_MODE_2XWIDER_MODE (mode).exists (&wmode)
	       && targetm.scalar_mode_supported_p (wmode))
	/* Even emitting a libcall is better than not detecting overflow
	   at all.  */
	goto twoxwider;
      else
	{
	  gcc_assert (!is_ubsan);
	  ops.code = MULT_EXPR;
	  ops.type = type;
	  res = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);
	  emit_jump (done_label);
	}
    }

 do_error_label:
  emit_label (do_error);
  if (is_ubsan)
    {
      /* Expand the ubsan builtin call.  */
      push_temp_slots ();
      fn = ubsan_build_overflow_builtin (MULT_EXPR, loc, TREE_TYPE (arg0),
					 arg0, arg1, datap);
      expand_normal (fn);
      pop_temp_slots ();
      do_pending_stack_adjust ();
    }
  else if (lhs)
    expand_arith_set_overflow (lhs, target);

  /* We're done.  */
  emit_label (done_label);

  /* u1 * u2 -> sr  */
  if (uns0_p && uns1_p && !unsr_p)
    {
      rtx_code_label *all_done_label = gen_label_rtx ();
      do_compare_rtx_and_jump (res, const0_rtx, GE, false, mode, NULL_RTX,
			       NULL, all_done_label, profile_probability::very_likely ());
      expand_arith_set_overflow (lhs, target);
      emit_label (all_done_label);
    }

  /* s1 * u2 -> sr  */
  if (!uns0_p && uns1_p && !unsr_p && pos_neg1 == 3)
    {
      rtx_code_label *all_done_label = gen_label_rtx ();
      rtx_code_label *set_noovf = gen_label_rtx ();
      do_compare_rtx_and_jump (op1, const0_rtx, GE, false, mode, NULL_RTX,
			       NULL, all_done_label, profile_probability::very_likely ());
      expand_arith_set_overflow (lhs, target);
      do_compare_rtx_and_jump (op0, const0_rtx, EQ, true, mode, NULL_RTX,
			       NULL, set_noovf, profile_probability::very_likely ());
      do_compare_rtx_and_jump (op0, constm1_rtx, NE, true, mode, NULL_RTX,
			       NULL, all_done_label, profile_probability::very_unlikely ());
      do_compare_rtx_and_jump (op1, res, NE, true, mode, NULL_RTX, NULL,
			       all_done_label, profile_probability::very_unlikely ());
      emit_label (set_noovf);
      write_complex_part (target, const0_rtx, true, false);
      emit_label (all_done_label);
    }

  if (lhs)
    {
      if (is_ubsan)
	expand_ubsan_result_store (lhs, target, mode, res, do_error);
      else
	expand_arith_overflow_result_store (lhs, target, mode, res);
    }
  flag_trapv = save_flag_trapv;
}

/* Expand UBSAN_CHECK_* internal function if it has vector operands.  */

static void
expand_vector_ubsan_overflow (location_t loc, enum tree_code code, tree lhs,
			      tree arg0, tree arg1)
{
  poly_uint64 cnt = TYPE_VECTOR_SUBPARTS (TREE_TYPE (arg0));
  rtx_code_label *loop_lab = NULL;
  rtx cntvar = NULL_RTX;
  tree cntv = NULL_TREE;
  tree eltype = TREE_TYPE (TREE_TYPE (arg0));
  tree sz = TYPE_SIZE (eltype);
  tree data = NULL_TREE;
  tree resv = NULL_TREE;
  rtx lhsr = NULL_RTX;
  rtx resvr = NULL_RTX;
  unsigned HOST_WIDE_INT const_cnt = 0;
  bool use_loop_p = (!cnt.is_constant (&const_cnt) || const_cnt > 4);
  int save_flag_trapv = flag_trapv;

  /* We don't want any __mulv?i3 etc. calls from the expansion of
     these internal functions, so disable -ftrapv temporarily.  */
  flag_trapv = 0;
  if (lhs)
    {
      optab op;
      lhsr = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
      if (!VECTOR_MODE_P (GET_MODE (lhsr))
	  || (op = optab_for_tree_code (code, TREE_TYPE (arg0),
					optab_default)) == unknown_optab
	  || (optab_handler (op, TYPE_MODE (TREE_TYPE (arg0)))
	      == CODE_FOR_nothing))
	{
	  if (MEM_P (lhsr))
	    resv = make_tree (TREE_TYPE (lhs), lhsr);
	  else
	    {
	      resvr = assign_temp (TREE_TYPE (lhs), 1, 1);
	      resv = make_tree (TREE_TYPE (lhs), resvr);
	    }
	}
    }
  if (use_loop_p)
    {
      do_pending_stack_adjust ();
      loop_lab = gen_label_rtx ();
      cntvar = gen_reg_rtx (TYPE_MODE (sizetype));
      cntv = make_tree (sizetype, cntvar);
      emit_move_insn (cntvar, const0_rtx);
      emit_label (loop_lab);
    }
  if (TREE_CODE (arg0) != VECTOR_CST)
    {
      rtx arg0r = expand_normal (arg0);
      arg0 = make_tree (TREE_TYPE (arg0), arg0r);
    }
  if (TREE_CODE (arg1) != VECTOR_CST)
    {
      rtx arg1r = expand_normal (arg1);
      arg1 = make_tree (TREE_TYPE (arg1), arg1r);
    }
  for (unsigned int i = 0; i < (use_loop_p ? 1 : const_cnt); i++)
    {
      tree op0, op1, res = NULL_TREE;
      if (use_loop_p)
	{
	  tree atype = build_array_type_nelts (eltype, cnt);
	  op0 = uniform_vector_p (arg0);
	  if (op0 == NULL_TREE)
	    {
	      op0 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, atype, arg0);
	      op0 = build4_loc (loc, ARRAY_REF, eltype, op0, cntv,
				NULL_TREE, NULL_TREE);
	    }
	  op1 = uniform_vector_p (arg1);
	  if (op1 == NULL_TREE)
	    {
	      op1 = fold_build1_loc (loc, VIEW_CONVERT_EXPR, atype, arg1);
	      op1 = build4_loc (loc, ARRAY_REF, eltype, op1, cntv,
				NULL_TREE, NULL_TREE);
	    }
	  if (resv)
	    {
	      res = fold_build1_loc (loc, VIEW_CONVERT_EXPR, atype, resv);
	      res = build4_loc (loc, ARRAY_REF, eltype, res, cntv,
				NULL_TREE, NULL_TREE);
	    }
	}
      else
	{
	  tree bitpos = bitsize_int (tree_to_uhwi (sz) * i);
	  op0 = fold_build3_loc (loc, BIT_FIELD_REF, eltype, arg0, sz, bitpos);
	  op1 = fold_build3_loc (loc, BIT_FIELD_REF, eltype, arg1, sz, bitpos);
	  if (resv)
	    res = fold_build3_loc (loc, BIT_FIELD_REF, eltype, resv, sz,
				   bitpos);
	}
      switch (code)
	{
	case PLUS_EXPR:
	  expand_addsub_overflow (loc, PLUS_EXPR, res, op0, op1,
				  false, false, false, true, &data);
	  break;
	case MINUS_EXPR:
	  if (use_loop_p ? integer_zerop (arg0) : integer_zerop (op0))
	    expand_neg_overflow (loc, res, op1, true, &data);
	  else
	    expand_addsub_overflow (loc, MINUS_EXPR, res, op0, op1,
				    false, false, false, true, &data);
	  break;
	case MULT_EXPR:
	  expand_mul_overflow (loc, res, op0, op1, false, false, false,
			       true, &data);
	  break;
	default:
	  gcc_unreachable ();
	}
    }
  if (use_loop_p)
    {
      struct separate_ops ops;
      ops.code = PLUS_EXPR;
      ops.type = TREE_TYPE (cntv);
      ops.op0 = cntv;
      ops.op1 = build_int_cst (TREE_TYPE (cntv), 1);
      ops.op2 = NULL_TREE;
      ops.location = loc;
      rtx ret = expand_expr_real_2 (&ops, cntvar, TYPE_MODE (sizetype),
				    EXPAND_NORMAL);
      if (ret != cntvar)
	emit_move_insn (cntvar, ret);
      rtx cntrtx = gen_int_mode (cnt, TYPE_MODE (sizetype));
      do_compare_rtx_and_jump (cntvar, cntrtx, NE, false,
			       TYPE_MODE (sizetype), NULL_RTX, NULL, loop_lab,
			       profile_probability::very_likely ());
    }
  if (lhs && resv == NULL_TREE)
    {
      struct separate_ops ops;
      ops.code = code;
      ops.type = TREE_TYPE (arg0);
      ops.op0 = arg0;
      ops.op1 = arg1;
      ops.op2 = NULL_TREE;
      ops.location = loc;
      rtx ret = expand_expr_real_2 (&ops, lhsr, TYPE_MODE (TREE_TYPE (arg0)),
				    EXPAND_NORMAL);
      if (ret != lhsr)
	emit_move_insn (lhsr, ret);
    }
  else if (resvr)
    emit_move_insn (lhsr, resvr);
  flag_trapv = save_flag_trapv;
}

/* Expand UBSAN_CHECK_ADD call STMT.  */

static void
expand_UBSAN_CHECK_ADD (internal_fn, gcall *stmt)
{
  location_t loc = gimple_location (stmt);
  tree lhs = gimple_call_lhs (stmt);
  tree arg0 = gimple_call_arg (stmt, 0);
  tree arg1 = gimple_call_arg (stmt, 1);
  if (VECTOR_TYPE_P (TREE_TYPE (arg0)))
    expand_vector_ubsan_overflow (loc, PLUS_EXPR, lhs, arg0, arg1);
  else
    expand_addsub_overflow (loc, PLUS_EXPR, lhs, arg0, arg1,
			    false, false, false, true, NULL);
}

/* Expand UBSAN_CHECK_SUB call STMT.  */

static void
expand_UBSAN_CHECK_SUB (internal_fn, gcall *stmt)
{
  location_t loc = gimple_location (stmt);
  tree lhs = gimple_call_lhs (stmt);
  tree arg0 = gimple_call_arg (stmt, 0);
  tree arg1 = gimple_call_arg (stmt, 1);
  if (VECTOR_TYPE_P (TREE_TYPE (arg0)))
    expand_vector_ubsan_overflow (loc, MINUS_EXPR, lhs, arg0, arg1);
  else if (integer_zerop (arg0))
    expand_neg_overflow (loc, lhs, arg1, true, NULL);
  else
    expand_addsub_overflow (loc, MINUS_EXPR, lhs, arg0, arg1,
			    false, false, false, true, NULL);
}

/* Expand UBSAN_CHECK_MUL call STMT.  */

static void
expand_UBSAN_CHECK_MUL (internal_fn, gcall *stmt)
{
  location_t loc = gimple_location (stmt);
  tree lhs = gimple_call_lhs (stmt);
  tree arg0 = gimple_call_arg (stmt, 0);
  tree arg1 = gimple_call_arg (stmt, 1);
  if (VECTOR_TYPE_P (TREE_TYPE (arg0)))
    expand_vector_ubsan_overflow (loc, MULT_EXPR, lhs, arg0, arg1);
  else
    expand_mul_overflow (loc, lhs, arg0, arg1, false, false, false, true,
			 NULL);
}

/* Helper function for {ADD,SUB,MUL}_OVERFLOW call stmt expansion.  */

static void
expand_arith_overflow (enum tree_code code, gimple *stmt)
{
  tree lhs = gimple_call_lhs (stmt);
  if (lhs == NULL_TREE)
    return;
  tree arg0 = gimple_call_arg (stmt, 0);
  tree arg1 = gimple_call_arg (stmt, 1);
  tree type = TREE_TYPE (TREE_TYPE (lhs));
  int uns0_p = TYPE_UNSIGNED (TREE_TYPE (arg0));
  int uns1_p = TYPE_UNSIGNED (TREE_TYPE (arg1));
  int unsr_p = TYPE_UNSIGNED (type);
  int prec0 = TYPE_PRECISION (TREE_TYPE (arg0));
  int prec1 = TYPE_PRECISION (TREE_TYPE (arg1));
  int precres = TYPE_PRECISION (type);
  location_t loc = gimple_location (stmt);
  if (!uns0_p && get_range_pos_neg (arg0) == 1)
    uns0_p = true;
  if (!uns1_p && get_range_pos_neg (arg1) == 1)
    uns1_p = true;
  int pr = get_min_precision (arg0, uns0_p ? UNSIGNED : SIGNED);
  prec0 = MIN (prec0, pr);
  pr = get_min_precision (arg1, uns1_p ? UNSIGNED : SIGNED);
  prec1 = MIN (prec1, pr);
  int save_flag_trapv = flag_trapv;

  /* We don't want any __mulv?i3 etc. calls from the expansion of
     these internal functions, so disable -ftrapv temporarily.  */
  flag_trapv = 0;
  /* If uns0_p && uns1_p, precop is minimum needed precision
     of unsigned type to hold the exact result, otherwise
     precop is minimum needed precision of signed type to
     hold the exact result.  */
  int precop;
  if (code == MULT_EXPR)
    precop = prec0 + prec1 + (uns0_p != uns1_p);
  else
    {
      if (uns0_p == uns1_p)
	precop = MAX (prec0, prec1) + 1;
      else if (uns0_p)
	precop = MAX (prec0 + 1, prec1) + 1;
      else
	precop = MAX (prec0, prec1 + 1) + 1;
    }
  int orig_precres = precres;

  do
    {
      if ((uns0_p && uns1_p)
	  ? ((precop + !unsr_p) <= precres
	     /* u1 - u2 -> ur can overflow, no matter what precision
		the result has.  */
	     && (code != MINUS_EXPR || !unsr_p))
	  : (!unsr_p && precop <= precres))
	{
	  /* The infinity precision result will always fit into result.  */
	  rtx target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
	  write_complex_part (target, const0_rtx, true, false);
	  scalar_int_mode mode = SCALAR_INT_TYPE_MODE (type);
	  struct separate_ops ops;
	  ops.code = code;
	  ops.type = type;
	  ops.op0 = fold_convert_loc (loc, type, arg0);
	  ops.op1 = fold_convert_loc (loc, type, arg1);
	  ops.op2 = NULL_TREE;
	  ops.location = loc;
	  rtx tem = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);
	  expand_arith_overflow_result_store (lhs, target, mode, tem);
	  flag_trapv = save_flag_trapv;
	  return;
	}

      /* For operations with low precision, if target doesn't have them, start
	 with precres widening right away, otherwise do it only if the most
	 simple cases can't be used.  */
      const int min_precision = targetm.min_arithmetic_precision ();
      if (orig_precres == precres && precres < min_precision)
	;
      else if ((uns0_p && uns1_p && unsr_p && prec0 <= precres
		&& prec1 <= precres)
	  || ((!uns0_p || !uns1_p) && !unsr_p
	      && prec0 + uns0_p <= precres
	      && prec1 + uns1_p <= precres))
	{
	  arg0 = fold_convert_loc (loc, type, arg0);
	  arg1 = fold_convert_loc (loc, type, arg1);
	  switch (code)
	    {
	    case MINUS_EXPR:
	      if (integer_zerop (arg0) && !unsr_p)
		{
		  expand_neg_overflow (loc, lhs, arg1, false, NULL);
		  flag_trapv = save_flag_trapv;
		  return;
		}
	      /* FALLTHRU */
	    case PLUS_EXPR:
	      expand_addsub_overflow (loc, code, lhs, arg0, arg1, unsr_p,
				      unsr_p, unsr_p, false, NULL);
	      flag_trapv = save_flag_trapv;
	      return;
	    case MULT_EXPR:
	      expand_mul_overflow (loc, lhs, arg0, arg1, unsr_p,
				   unsr_p, unsr_p, false, NULL);
	      flag_trapv = save_flag_trapv;
	      return;
	    default:
	      gcc_unreachable ();
	    }
	}

      /* For sub-word operations, retry with a wider type first.  */
      if (orig_precres == precres && precop <= BITS_PER_WORD)
	{
	  int p = MAX (min_precision, precop);
	  scalar_int_mode m = smallest_int_mode_for_size (p).require ();
	  tree optype = build_nonstandard_integer_type (GET_MODE_PRECISION (m),
							uns0_p && uns1_p
							&& unsr_p);
	  p = TYPE_PRECISION (optype);
	  if (p > precres)
	    {
	      precres = p;
	      unsr_p = TYPE_UNSIGNED (optype);
	      type = optype;
	      continue;
	    }
	}

      if (prec0 <= precres && prec1 <= precres)
	{
	  tree types[2];
	  if (unsr_p)
	    {
	      types[0] = build_nonstandard_integer_type (precres, 0);
	      types[1] = type;
	    }
	  else
	    {
	      types[0] = type;
	      types[1] = build_nonstandard_integer_type (precres, 1);
	    }
	  arg0 = fold_convert_loc (loc, types[uns0_p], arg0);
	  arg1 = fold_convert_loc (loc, types[uns1_p], arg1);
	  if (code != MULT_EXPR)
	    expand_addsub_overflow (loc, code, lhs, arg0, arg1, unsr_p,
				    uns0_p, uns1_p, false, NULL);
	  else
	    expand_mul_overflow (loc, lhs, arg0, arg1, unsr_p,
				 uns0_p, uns1_p, false, NULL);
	  flag_trapv = save_flag_trapv;
	  return;
	}

      /* Retry with a wider type.  */
      if (orig_precres == precres)
	{
	  int p = MAX (prec0, prec1);
	  scalar_int_mode m = smallest_int_mode_for_size (p).require ();
	  tree optype = build_nonstandard_integer_type (GET_MODE_PRECISION (m),
							uns0_p && uns1_p
							&& unsr_p);
	  p = TYPE_PRECISION (optype);
	  if (p > precres)
	    {
	      precres = p;
	      unsr_p = TYPE_UNSIGNED (optype);
	      type = optype;
	      continue;
	    }
	}

      gcc_unreachable ();
    }
  while (1);
}

/* Expand ADD_OVERFLOW STMT.  */

static void
expand_ADD_OVERFLOW (internal_fn, gcall *stmt)
{
  expand_arith_overflow (PLUS_EXPR, stmt);
}

/* Expand SUB_OVERFLOW STMT.  */

static void
expand_SUB_OVERFLOW (internal_fn, gcall *stmt)
{
  expand_arith_overflow (MINUS_EXPR, stmt);
}

/* Expand MUL_OVERFLOW STMT.  */

static void
expand_MUL_OVERFLOW (internal_fn, gcall *stmt)
{
  expand_arith_overflow (MULT_EXPR, stmt);
}

/* Expand UADDC STMT.  */

static void
expand_UADDC (internal_fn ifn, gcall *stmt)
{
  tree lhs = gimple_call_lhs (stmt);
  tree arg1 = gimple_call_arg (stmt, 0);
  tree arg2 = gimple_call_arg (stmt, 1);
  tree arg3 = gimple_call_arg (stmt, 2);
  tree type = TREE_TYPE (arg1);
  machine_mode mode = TYPE_MODE (type);
  insn_code icode = optab_handler (ifn == IFN_UADDC
				   ? uaddc5_optab : usubc5_optab, mode);
  rtx op1 = expand_normal (arg1);
  rtx op2 = expand_normal (arg2);
  rtx op3 = expand_normal (arg3);
  rtx target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  rtx re = gen_reg_rtx (mode);
  rtx im = gen_reg_rtx (mode);
  class expand_operand ops[5];
  create_output_operand (&ops[0], re, mode);
  create_output_operand (&ops[1], im, mode);
  create_input_operand (&ops[2], op1, mode);
  create_input_operand (&ops[3], op2, mode);
  create_input_operand (&ops[4], op3, mode);
  expand_insn (icode, 5, ops);
  write_complex_part (target, re, false, false);
  write_complex_part (target, im, true, false);
}

/* Expand USUBC STMT.  */

static void
expand_USUBC (internal_fn ifn, gcall *stmt)
{
  expand_UADDC (ifn, stmt);
}

/* This should get folded in tree-vectorizer.cc.  */

static void
expand_LOOP_VECTORIZED (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This should get folded in tree-vectorizer.cc.  */

static void
expand_LOOP_DIST_ALIAS (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* Return a memory reference of type TYPE for argument INDEX of STMT.
   Use argument INDEX + 1 to derive the second (TBAA) operand.  */

static tree
expand_call_mem_ref (tree type, gcall *stmt, int index)
{
  tree addr = gimple_call_arg (stmt, index);
  tree alias_ptr_type = TREE_TYPE (gimple_call_arg (stmt, index + 1));
  unsigned int align = tree_to_shwi (gimple_call_arg (stmt, index + 1));
  if (TYPE_ALIGN (type) != align)
    type = build_aligned_type (type, align);

  tree tmp = addr;
  if (TREE_CODE (tmp) == SSA_NAME)
    {
      gimple *def = get_gimple_for_ssa_name (tmp);
      if (def && gimple_assign_single_p (def))
	tmp = gimple_assign_rhs1 (def);
    }

  if (TREE_CODE (tmp) == ADDR_EXPR)
    {
      tree mem = TREE_OPERAND (tmp, 0);
      if (TREE_CODE (mem) == TARGET_MEM_REF
	  && types_compatible_p (TREE_TYPE (mem), type))
	{
	  tree offset = TMR_OFFSET (mem);
	  if (type != TREE_TYPE (mem)
	      || alias_ptr_type != TREE_TYPE (offset)
	      || !integer_zerop (offset))
	    {
	      mem = copy_node (mem);
	      TMR_OFFSET (mem) = wide_int_to_tree (alias_ptr_type,
						   wi::to_poly_wide (offset));
	      TREE_TYPE (mem) = type;
	    }
	  return mem;
	}
    }

  return fold_build2 (MEM_REF, type, addr, build_int_cst (alias_ptr_type, 0));
}

/* Expand MASK_LOAD{,_LANES}, MASK_LEN_LOAD or LEN_LOAD call STMT using optab
 * OPTAB.  */

static void
expand_partial_load_optab_fn (internal_fn ifn, gcall *stmt, convert_optab optab)
{
  int i = 0;
  class expand_operand ops[6];
  tree type, lhs, rhs, maskt;
  rtx mem, target;
  insn_code icode;

  maskt = gimple_call_arg (stmt, internal_fn_mask_index (ifn));
  lhs = gimple_call_lhs (stmt);
  if (lhs == NULL_TREE)
    return;
  type = TREE_TYPE (lhs);
  rhs = expand_call_mem_ref (type, stmt, 0);

  if (optab == vec_mask_load_lanes_optab
      || optab == vec_mask_len_load_lanes_optab)
    icode = get_multi_vector_move (type, optab);
  else if (optab == len_load_optab)
    icode = direct_optab_handler (optab, TYPE_MODE (type));
  else
    icode = convert_optab_handler (optab, TYPE_MODE (type),
				   TYPE_MODE (TREE_TYPE (maskt)));

  mem = expand_expr (rhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  gcc_assert (MEM_P (mem));
  /* The built MEM_REF does not accurately reflect that the load
     is only partial.  Clear it.  */
  set_mem_expr (mem, NULL_TREE);
  clear_mem_offset (mem);
  target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  create_call_lhs_operand (&ops[i++], target, TYPE_MODE (type));
  create_fixed_operand (&ops[i++], mem);
  i = add_mask_else_and_len_args (ops, i, stmt);
  expand_insn (icode, i, ops);

  assign_call_lhs (lhs, target, &ops[0]);
}

#define expand_mask_load_optab_fn expand_partial_load_optab_fn
#define expand_mask_load_lanes_optab_fn expand_mask_load_optab_fn
#define expand_len_load_optab_fn expand_partial_load_optab_fn
#define expand_mask_len_load_optab_fn expand_partial_load_optab_fn

/* Expand MASK_STORE{,_LANES}, MASK_LEN_STORE or LEN_STORE call STMT using optab
 * OPTAB.  */

static void
expand_partial_store_optab_fn (internal_fn ifn, gcall *stmt, convert_optab optab)
{
  int i = 0;
  class expand_operand ops[5];
  tree type, lhs, rhs, maskt;
  rtx mem, reg;
  insn_code icode;

  maskt = gimple_call_arg (stmt, internal_fn_mask_index (ifn));
  rhs = gimple_call_arg (stmt, internal_fn_stored_value_index (ifn));
  type = TREE_TYPE (rhs);
  lhs = expand_call_mem_ref (type, stmt, 0);

  if (optab == vec_mask_store_lanes_optab
      || optab == vec_mask_len_store_lanes_optab)
    icode = get_multi_vector_move (type, optab);
  else if (optab == len_store_optab)
    icode = direct_optab_handler (optab, TYPE_MODE (type));
  else
    icode = convert_optab_handler (optab, TYPE_MODE (type),
				   TYPE_MODE (TREE_TYPE (maskt)));

  mem = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  gcc_assert (MEM_P (mem));
  /* The built MEM_REF does not accurately reflect that the store
     is only partial.  Clear it.  */
  set_mem_expr (mem, NULL_TREE);
  clear_mem_offset (mem);
  reg = expand_normal (rhs);
  create_fixed_operand (&ops[i++], mem);
  create_input_operand (&ops[i++], reg, TYPE_MODE (type));
  i = add_mask_else_and_len_args (ops, i, stmt);
  expand_insn (icode, i, ops);
}

#define expand_mask_store_optab_fn expand_partial_store_optab_fn
#define expand_mask_store_lanes_optab_fn expand_mask_store_optab_fn
#define expand_len_store_optab_fn expand_partial_store_optab_fn
#define expand_mask_len_store_optab_fn expand_partial_store_optab_fn

/* Expand VCOND_MASK optab internal function.
   The expansion of STMT happens based on OPTAB table associated.  */

static void
expand_vec_cond_mask_optab_fn (internal_fn, gcall *stmt, convert_optab optab)
{
  class expand_operand ops[4];

  tree lhs = gimple_call_lhs (stmt);
  tree op0 = gimple_call_arg (stmt, 0);
  tree op1 = gimple_call_arg (stmt, 1);
  tree op2 = gimple_call_arg (stmt, 2);
  tree vec_cond_type = TREE_TYPE (lhs);

  machine_mode mode = TYPE_MODE (vec_cond_type);
  machine_mode mask_mode = TYPE_MODE (TREE_TYPE (op0));
  enum insn_code icode = convert_optab_handler (optab, mode, mask_mode);
  rtx mask, rtx_op1, rtx_op2;

  gcc_assert (icode != CODE_FOR_nothing);

  mask = expand_normal (op0);
  rtx_op1 = expand_normal (op1);
  rtx_op2 = expand_normal (op2);

  rtx target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  create_call_lhs_operand (&ops[0], target, mode);
  create_input_operand (&ops[1], rtx_op1, mode);
  create_input_operand (&ops[2], rtx_op2, mode);
  create_input_operand (&ops[3], mask, mask_mode);
  expand_insn (icode, 4, ops);
  assign_call_lhs (lhs, target, &ops[0]);
}

/* Expand VEC_SET internal functions.  */

static void
expand_vec_set_optab_fn (internal_fn, gcall *stmt, convert_optab optab)
{
  tree lhs = gimple_call_lhs (stmt);
  tree op0 = gimple_call_arg (stmt, 0);
  tree op1 = gimple_call_arg (stmt, 1);
  tree op2 = gimple_call_arg (stmt, 2);
  rtx target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  rtx src = expand_normal (op0);

  machine_mode outermode = TYPE_MODE (TREE_TYPE (op0));
  scalar_mode innermode = GET_MODE_INNER (outermode);

  rtx value = expand_normal (op1);
  rtx pos = expand_normal (op2);

  class expand_operand ops[3];
  enum insn_code icode = optab_handler (optab, outermode);

  if (icode != CODE_FOR_nothing)
    {
      rtx temp = gen_reg_rtx (outermode);
      emit_move_insn (temp, src);

      create_fixed_operand (&ops[0], temp);
      create_input_operand (&ops[1], value, innermode);
      create_convert_operand_from (&ops[2], pos, TYPE_MODE (TREE_TYPE (op2)),
				   true);
      if (maybe_expand_insn (icode, 3, ops))
	{
	  emit_move_insn (target, temp);
	  return;
	}
    }
  gcc_unreachable ();
}

static void
expand_ABNORMAL_DISPATCHER (internal_fn, gcall *)
{
}

static void
expand_BUILTIN_EXPECT (internal_fn, gcall *stmt)
{
  /* When guessing was done, the hints should be already stripped away.  */
  gcc_assert (!flag_guess_branch_prob || optimize == 0 || seen_error ());

  rtx target;
  tree lhs = gimple_call_lhs (stmt);
  if (lhs)
    target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  else
    target = const0_rtx;
  rtx val = expand_expr (gimple_call_arg (stmt, 0), target, VOIDmode, EXPAND_NORMAL);
  if (lhs && val != target)
    emit_move_insn (target, val);
}

/* IFN_VA_ARG is supposed to be expanded at pass_stdarg.  So this dummy function
   should never be called.  */

static void
expand_VA_ARG (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* IFN_VEC_CONVERT is supposed to be expanded at pass_lower_vector.  So this
   dummy function should never be called.  */

static void
expand_VEC_CONVERT (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* Expand IFN_RAWMEMCHR internal function.  */

void
expand_RAWMEMCHR (internal_fn, gcall *stmt)
{
  expand_operand ops[3];

  tree lhs = gimple_call_lhs (stmt);
  if (!lhs)
    return;
  machine_mode lhs_mode = TYPE_MODE (TREE_TYPE (lhs));
  rtx lhs_rtx = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  create_call_lhs_operand (&ops[0], lhs_rtx, lhs_mode);

  tree mem = gimple_call_arg (stmt, 0);
  rtx mem_rtx = get_memory_rtx (mem, NULL);
  create_fixed_operand (&ops[1], mem_rtx);

  tree pattern = gimple_call_arg (stmt, 1);
  machine_mode mode = TYPE_MODE (TREE_TYPE (pattern));
  rtx pattern_rtx = expand_normal (pattern);
  create_input_operand (&ops[2], pattern_rtx, mode);

  insn_code icode = direct_optab_handler (rawmemchr_optab, mode);

  expand_insn (icode, 3, ops);
  assign_call_lhs (lhs, lhs_rtx, &ops[0]);
}

/* Expand the IFN_UNIQUE function according to its first argument.  */

static void
expand_UNIQUE (internal_fn, gcall *stmt)
{
  rtx pattern = NULL_RTX;
  enum ifn_unique_kind kind
    = (enum ifn_unique_kind) TREE_INT_CST_LOW (gimple_call_arg (stmt, 0));

  switch (kind)
    {
    default:
      gcc_unreachable ();

    case IFN_UNIQUE_UNSPEC:
      if (targetm.have_unique ())
	pattern = targetm.gen_unique ();
      break;

    case IFN_UNIQUE_OACC_FORK:
    case IFN_UNIQUE_OACC_JOIN:
      if (targetm.have_oacc_fork () && targetm.have_oacc_join ())
	{
	  tree lhs = gimple_call_lhs (stmt);
	  rtx target = const0_rtx;

	  if (lhs)
	    target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);

	  rtx data_dep = expand_normal (gimple_call_arg (stmt, 1));
	  rtx axis = expand_normal (gimple_call_arg (stmt, 2));

	  if (kind == IFN_UNIQUE_OACC_FORK)
	    pattern = targetm.gen_oacc_fork (target, data_dep, axis);
	  else
	    pattern = targetm.gen_oacc_join (target, data_dep, axis);
	}
      else
	gcc_unreachable ();
      break;
    }

  if (pattern)
    emit_insn (pattern);
}

/* Expand the IFN_DEFERRED_INIT function:
   LHS = DEFERRED_INIT (SIZE of the DECL, INIT_TYPE, NAME of the DECL);

   Initialize the LHS with zero/pattern according to its second argument
   INIT_TYPE:
   if INIT_TYPE is AUTO_INIT_ZERO, use zeroes to initialize;
   if INIT_TYPE is AUTO_INIT_PATTERN, use 0xFE byte-repeatable pattern
     to initialize;
   The LHS variable is initialized including paddings.
   The reasons to choose 0xFE for pattern initialization are:
     1. It is a non-canonical virtual address on x86_64, and at the
	high end of the i386 kernel address space.
     2. It is a very large float value (-1.694739530317379e+38).
     3. It is also an unusual number for integers.  */
#define INIT_PATTERN_VALUE  0xFE
static void
expand_DEFERRED_INIT (internal_fn, gcall *stmt)
{
  tree lhs = gimple_call_lhs (stmt);
  tree var_size = gimple_call_arg (stmt, 0);
  enum auto_init_type init_type
    = (enum auto_init_type) TREE_INT_CST_LOW (gimple_call_arg (stmt, 1));
  bool reg_lhs = true;

  tree var_type = TREE_TYPE (lhs);
  gcc_assert (init_type > AUTO_INIT_UNINITIALIZED);

  if (TREE_CODE (lhs) == SSA_NAME)
    reg_lhs = true;
  else
    {
      tree lhs_base = lhs;
      while (handled_component_p (lhs_base))
	lhs_base = TREE_OPERAND (lhs_base, 0);
      reg_lhs = (mem_ref_refers_to_non_mem_p (lhs_base)
		 || non_mem_decl_p (lhs_base));
      /* If this expands to a register and the underlying decl is wrapped in
	 a MEM_REF that just serves as an access type change expose the decl
	 if it is of correct size.  This avoids a situation as in PR103271
	 if the target does not support a direct move to the registers mode.  */
      if (reg_lhs
	  && TREE_CODE (lhs_base) == MEM_REF
	  && TREE_CODE (TREE_OPERAND (lhs_base, 0)) == ADDR_EXPR
	  && DECL_P (TREE_OPERAND (TREE_OPERAND (lhs_base, 0), 0))
	  && integer_zerop (TREE_OPERAND (lhs_base, 1))
	  && tree_fits_uhwi_p (var_size)
	  && tree_int_cst_equal
	       (var_size,
		DECL_SIZE_UNIT (TREE_OPERAND (TREE_OPERAND (lhs_base, 0), 0))))
	{
	  lhs = TREE_OPERAND (TREE_OPERAND (lhs_base, 0), 0);
	  var_type = TREE_TYPE (lhs);
	}
    }

  if (!reg_lhs)
    {
      /* If the variable is not in register, expand to a memset
	 to initialize it.  */
      mark_addressable (lhs);
      tree var_addr = build_fold_addr_expr (lhs);

      tree value = (init_type == AUTO_INIT_PATTERN)
		    ? build_int_cst (integer_type_node,
				     INIT_PATTERN_VALUE)
		    : integer_zero_node;
      tree m_call = build_call_expr (builtin_decl_implicit (BUILT_IN_MEMSET),
				     3, var_addr, value, var_size);
      /* Expand this memset call.  */
      expand_builtin_memset (m_call, NULL_RTX, TYPE_MODE (var_type));
    }
  else
    {
      /* If this variable is in a register use expand_assignment.
	 For boolean scalars force zero-init.  */
      tree init;
      scalar_int_mode var_mode;
      if (TREE_CODE (TREE_TYPE (lhs)) != BOOLEAN_TYPE
	  && tree_fits_uhwi_p (var_size)
	  && (init_type == AUTO_INIT_PATTERN
	      || !is_gimple_reg_type (var_type))
	  && int_mode_for_size (tree_to_uhwi (var_size) * BITS_PER_UNIT,
				0).exists (&var_mode)
	  && have_insn_for (SET, var_mode))
	{
	  unsigned HOST_WIDE_INT total_bytes = tree_to_uhwi (var_size);
	  unsigned char *buf = XALLOCAVEC (unsigned char, total_bytes);
	  memset (buf, (init_type == AUTO_INIT_PATTERN
			? INIT_PATTERN_VALUE : 0), total_bytes);
	  tree itype = build_nonstandard_integer_type
			 (total_bytes * BITS_PER_UNIT, 1);
	  wide_int w = wi::from_buffer (buf, total_bytes);
	  init = wide_int_to_tree (itype, w);
	  /* Pun the LHS to make sure its type has constant size
	     unless it is an SSA name where that's already known.  */
	  if (TREE_CODE (lhs) != SSA_NAME)
	    lhs = build1 (VIEW_CONVERT_EXPR, itype, lhs);
	  else
	    init = fold_build1 (VIEW_CONVERT_EXPR, TREE_TYPE (lhs), init);
	}
      else
	/* Use zero-init also for variable-length sizes.  */
	init = build_zero_cst (var_type);

      expand_assignment (lhs, init, false);
    }
}

/* Expand the IFN_ACCESS_WITH_SIZE function:
   ACCESS_WITH_SIZE (REF_TO_OBJ, REF_TO_SIZE, CLASS_OF_SIZE,
		     TYPE_OF_SIZE, ACCESS_MODE)
   which returns the REF_TO_OBJ same as the 1st argument;

   1st argument REF_TO_OBJ: The reference to the object;
   2nd argument REF_TO_SIZE: The reference to the size of the object,
   3rd argument CLASS_OF_SIZE: The size referenced by the REF_TO_SIZE represents
     0: the number of bytes.
     1: the number of the elements of the object type;
   4th argument TYPE_OF_SIZE: A constant 0 with its TYPE being the same as the TYPE
    of the object referenced by REF_TO_SIZE
   5th argument ACCESS_MODE:
    -1: Unknown access semantics
     0: none
     1: read_only
     2: write_only
     3: read_write
   6th argument: A constant 0 with the pointer TYPE to the original flexible
     array type.

   Both the return type and the type of the first argument of this
   function have been converted from the incomplete array type to
   the corresponding pointer type.

   For each call to a .ACCESS_WITH_SIZE, replace it with its 1st argument.  */

static void
expand_ACCESS_WITH_SIZE (internal_fn, gcall *stmt)
{
  tree lhs = gimple_call_lhs (stmt);
  tree ref_to_obj = gimple_call_arg (stmt, 0);
  if (lhs)
    expand_assignment (lhs, ref_to_obj, false);
}

/* The size of an OpenACC compute dimension.  */

static void
expand_GOACC_DIM_SIZE (internal_fn, gcall *stmt)
{
  tree lhs = gimple_call_lhs (stmt);

  if (!lhs)
    return;

  rtx target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  if (targetm.have_oacc_dim_size ())
    {
      rtx dim = expand_expr (gimple_call_arg (stmt, 0), NULL_RTX,
			     VOIDmode, EXPAND_NORMAL);
      emit_insn (targetm.gen_oacc_dim_size (target, dim));
    }
  else
    emit_move_insn (target, GEN_INT (1));
}

/* The position of an OpenACC execution engine along one compute axis.  */

static void
expand_GOACC_DIM_POS (internal_fn, gcall *stmt)
{
  tree lhs = gimple_call_lhs (stmt);

  if (!lhs)
    return;

  rtx target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  if (targetm.have_oacc_dim_pos ())
    {
      rtx dim = expand_expr (gimple_call_arg (stmt, 0), NULL_RTX,
			     VOIDmode, EXPAND_NORMAL);
      emit_insn (targetm.gen_oacc_dim_pos (target, dim));
    }
  else
    emit_move_insn (target, const0_rtx);
}

/* This is expanded by oacc_device_lower pass.  */

static void
expand_GOACC_LOOP (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This is expanded by oacc_device_lower pass.  */

static void
expand_GOACC_REDUCTION (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* This is expanded by oacc_device_lower pass.  */

static void
expand_GOACC_TILE (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* Set errno to EDOM.  */

static void
expand_SET_EDOM (internal_fn, gcall *)
{
#ifdef TARGET_EDOM
#ifdef GEN_ERRNO_RTX
  rtx errno_rtx = GEN_ERRNO_RTX;
#else
  rtx errno_rtx = gen_rtx_MEM (word_mode, gen_rtx_SYMBOL_REF (Pmode, "errno"));
#endif
  emit_move_insn (errno_rtx,
		  gen_int_mode (TARGET_EDOM, GET_MODE (errno_rtx)));
#else
  gcc_unreachable ();
#endif
}

/* Expand atomic bit test and set.  */

static void
expand_ATOMIC_BIT_TEST_AND_SET (internal_fn, gcall *call)
{
  expand_ifn_atomic_bit_test_and (call);
}

/* Expand atomic bit test and complement.  */

static void
expand_ATOMIC_BIT_TEST_AND_COMPLEMENT (internal_fn, gcall *call)
{
  expand_ifn_atomic_bit_test_and (call);
}

/* Expand atomic bit test and reset.  */

static void
expand_ATOMIC_BIT_TEST_AND_RESET (internal_fn, gcall *call)
{
  expand_ifn_atomic_bit_test_and (call);
}

/* Expand atomic bit test and set.  */

static void
expand_ATOMIC_COMPARE_EXCHANGE (internal_fn, gcall *call)
{
  expand_ifn_atomic_compare_exchange (call);
}

/* Expand atomic add fetch and cmp with 0.  */

static void
expand_ATOMIC_ADD_FETCH_CMP_0 (internal_fn, gcall *call)
{
  expand_ifn_atomic_op_fetch_cmp_0 (call);
}

/* Expand atomic sub fetch and cmp with 0.  */

static void
expand_ATOMIC_SUB_FETCH_CMP_0 (internal_fn, gcall *call)
{
  expand_ifn_atomic_op_fetch_cmp_0 (call);
}

/* Expand atomic and fetch and cmp with 0.  */

static void
expand_ATOMIC_AND_FETCH_CMP_0 (internal_fn, gcall *call)
{
  expand_ifn_atomic_op_fetch_cmp_0 (call);
}

/* Expand atomic or fetch and cmp with 0.  */

static void
expand_ATOMIC_OR_FETCH_CMP_0 (internal_fn, gcall *call)
{
  expand_ifn_atomic_op_fetch_cmp_0 (call);
}

/* Expand atomic xor fetch and cmp with 0.  */

static void
expand_ATOMIC_XOR_FETCH_CMP_0 (internal_fn, gcall *call)
{
  expand_ifn_atomic_op_fetch_cmp_0 (call);
}

/* Expand LAUNDER to assignment, lhs = arg0.  */

static void
expand_LAUNDER (internal_fn, gcall *call)
{
  tree lhs = gimple_call_lhs (call);

  if (!lhs)
    return;

  expand_assignment (lhs, gimple_call_arg (call, 0), false);
}

/* Expand {MASK_,}SCATTER_STORE{S,U} call CALL using optab OPTAB.  */

static void
expand_scatter_store_optab_fn (internal_fn, gcall *stmt, direct_optab optab)
{
  internal_fn ifn = gimple_call_internal_fn (stmt);
  int rhs_index = internal_fn_stored_value_index (ifn);
  tree base = gimple_call_arg (stmt, 0);
  tree offset = gimple_call_arg (stmt, 1);
  tree scale = gimple_call_arg (stmt, 2);
  tree rhs = gimple_call_arg (stmt, rhs_index);

  rtx base_rtx = expand_normal (base);
  rtx offset_rtx = expand_normal (offset);
  HOST_WIDE_INT scale_int = tree_to_shwi (scale);
  rtx rhs_rtx = expand_normal (rhs);

  class expand_operand ops[8];
  int i = 0;
  create_address_operand (&ops[i++], base_rtx);
  create_input_operand (&ops[i++], offset_rtx, TYPE_MODE (TREE_TYPE (offset)));
  create_integer_operand (&ops[i++], TYPE_UNSIGNED (TREE_TYPE (offset)));
  create_integer_operand (&ops[i++], scale_int);
  create_input_operand (&ops[i++], rhs_rtx, TYPE_MODE (TREE_TYPE (rhs)));
  i = add_mask_else_and_len_args (ops, i, stmt);

  insn_code icode = convert_optab_handler (optab, TYPE_MODE (TREE_TYPE (rhs)),
					   TYPE_MODE (TREE_TYPE (offset)));
  expand_insn (icode, i, ops);
}

/* Expand {MASK_,}GATHER_LOAD call CALL using optab OPTAB.  */

static void
expand_gather_load_optab_fn (internal_fn, gcall *stmt, direct_optab optab)
{
  tree lhs = gimple_call_lhs (stmt);
  tree base = gimple_call_arg (stmt, 0);
  tree offset = gimple_call_arg (stmt, 1);
  tree scale = gimple_call_arg (stmt, 2);

  rtx lhs_rtx = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  rtx base_rtx = expand_normal (base);
  rtx offset_rtx = expand_normal (offset);
  HOST_WIDE_INT scale_int = tree_to_shwi (scale);

  int i = 0;
  class expand_operand ops[9];
  create_call_lhs_operand (&ops[i++], lhs_rtx, TYPE_MODE (TREE_TYPE (lhs)));
  create_address_operand (&ops[i++], base_rtx);
  create_input_operand (&ops[i++], offset_rtx, TYPE_MODE (TREE_TYPE (offset)));
  create_integer_operand (&ops[i++], TYPE_UNSIGNED (TREE_TYPE (offset)));
  create_integer_operand (&ops[i++], scale_int);
  i = add_mask_else_and_len_args (ops, i, stmt);
  insn_code icode = convert_optab_handler (optab, TYPE_MODE (TREE_TYPE (lhs)),
					   TYPE_MODE (TREE_TYPE (offset)));
  expand_insn (icode, i, ops);
  assign_call_lhs (lhs, lhs_rtx, &ops[0]);
}

/* Expand MASK_LEN_STRIDED_LOAD call CALL by optab OPTAB.  */

static void
expand_strided_load_optab_fn (ATTRIBUTE_UNUSED internal_fn, gcall *stmt,
			      direct_optab optab)
{
  tree lhs = gimple_call_lhs (stmt);
  tree base = gimple_call_arg (stmt, 0);
  tree stride = gimple_call_arg (stmt, 1);

  rtx lhs_rtx = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  rtx base_rtx = expand_normal (base);
  rtx stride_rtx = expand_normal (stride);

  unsigned i = 0;
  class expand_operand ops[7];
  machine_mode mode = TYPE_MODE (TREE_TYPE (lhs));

  create_output_operand (&ops[i++], lhs_rtx, mode);
  create_address_operand (&ops[i++], base_rtx);
  create_address_operand (&ops[i++], stride_rtx);

  i = add_mask_else_and_len_args (ops, i, stmt);
  expand_insn (direct_optab_handler (optab, mode), i, ops);

  if (!rtx_equal_p (lhs_rtx, ops[0].value))
    emit_move_insn (lhs_rtx, ops[0].value);
}

/* Expand MASK_LEN_STRIDED_STORE call CALL by optab OPTAB.  */

static void
expand_strided_store_optab_fn (ATTRIBUTE_UNUSED internal_fn, gcall *stmt,
			       direct_optab optab)
{
  internal_fn fn = gimple_call_internal_fn (stmt);
  int rhs_index = internal_fn_stored_value_index (fn);

  tree base = gimple_call_arg (stmt, 0);
  tree stride = gimple_call_arg (stmt, 1);
  tree rhs = gimple_call_arg (stmt, rhs_index);

  rtx base_rtx = expand_normal (base);
  rtx stride_rtx = expand_normal (stride);
  rtx rhs_rtx = expand_normal (rhs);

  unsigned i = 0;
  class expand_operand ops[6];
  machine_mode mode = TYPE_MODE (TREE_TYPE (rhs));

  create_address_operand (&ops[i++], base_rtx);
  create_address_operand (&ops[i++], stride_rtx);
  create_input_operand (&ops[i++], rhs_rtx, mode);

  i = add_mask_else_and_len_args (ops, i, stmt);
  expand_insn (direct_optab_handler (optab, mode), i, ops);
}

/* Helper for expand_DIVMOD.  Return true if the sequence starting with
   INSN contains any call insns or insns with {,U}{DIV,MOD} rtxes.  */

static bool
contains_call_div_mod (rtx_insn *insn)
{
  subrtx_iterator::array_type array;
  for (; insn; insn = NEXT_INSN (insn))
    if (CALL_P (insn))
      return true;
    else if (INSN_P (insn))
      FOR_EACH_SUBRTX (iter, array, PATTERN (insn), NONCONST)
	switch (GET_CODE (*iter))
	  {
	  case CALL:
	  case DIV:
	  case UDIV:
	  case MOD:
	  case UMOD:
	    return true;
	  default:
	    break;
	  }
  return false;
 }

/* Expand DIVMOD() using:
 a) optab handler for udivmod/sdivmod if it is available.
 b) If optab_handler doesn't exist, generate call to
    target-specific divmod libfunc.  */

static void
expand_DIVMOD (internal_fn, gcall *call_stmt)
{
  tree lhs = gimple_call_lhs (call_stmt);
  tree arg0 = gimple_call_arg (call_stmt, 0);
  tree arg1 = gimple_call_arg (call_stmt, 1);

  gcc_assert (TREE_CODE (TREE_TYPE (lhs)) == COMPLEX_TYPE);
  tree type = TREE_TYPE (TREE_TYPE (lhs));
  machine_mode mode = TYPE_MODE (type);
  bool unsignedp = TYPE_UNSIGNED (type);
  optab tab = (unsignedp) ? udivmod_optab : sdivmod_optab;

  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  rtx target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);

  rtx quotient = NULL_RTX, remainder = NULL_RTX;
  rtx_insn *insns = NULL;

  if (TREE_CODE (arg1) == INTEGER_CST)
    {
      /* For DIVMOD by integral constants, there could be efficient code
	 expanded inline e.g. using shifts and plus/minus.  Try to expand
	 the division and modulo and if it emits any library calls or any
	 {,U}{DIV,MOD} rtxes throw it away and use a divmod optab or
	 divmod libcall.  */
      scalar_int_mode int_mode;
      if (remainder == NULL_RTX
	  && optimize
	  && CONST_INT_P (op1)
	  && !pow2p_hwi (INTVAL (op1))
	  && is_int_mode (TYPE_MODE (type), &int_mode)
	  && GET_MODE_SIZE (int_mode) == 2 * UNITS_PER_WORD
	  && optab_handler (and_optab, word_mode) != CODE_FOR_nothing
	  && optab_handler (add_optab, word_mode) != CODE_FOR_nothing
	  && optimize_insn_for_speed_p ())
	{
	  rtx_insn *last = get_last_insn ();
	  remainder = NULL_RTX;
	  quotient = expand_doubleword_divmod (int_mode, op0, op1, &remainder,
					       TYPE_UNSIGNED (type));
	  if (quotient != NULL_RTX)
	    {
	      if (optab_handler (mov_optab, int_mode) != CODE_FOR_nothing)
		{
		  rtx_insn *move = emit_move_insn (quotient, quotient);
		  set_dst_reg_note (move, REG_EQUAL,
				    gen_rtx_fmt_ee (TYPE_UNSIGNED (type)
						    ? UDIV : DIV, int_mode,
						    copy_rtx (op0), op1),
				    quotient);
		  move = emit_move_insn (remainder, remainder);
		  set_dst_reg_note (move, REG_EQUAL,
				    gen_rtx_fmt_ee (TYPE_UNSIGNED (type)
						    ? UMOD : MOD, int_mode,
						    copy_rtx (op0), op1),
				    quotient);
		}
	    }
	  else
	    delete_insns_since (last);
	}

      if (remainder == NULL_RTX)
	{
	  struct separate_ops ops;
	  ops.code = TRUNC_DIV_EXPR;
	  ops.type = type;
	  ops.op0 = make_tree (ops.type, op0);
	  ops.op1 = arg1;
	  ops.op2 = NULL_TREE;
	  ops.location = gimple_location (call_stmt);
	  start_sequence ();
	  quotient = expand_expr_real_2 (&ops, NULL_RTX, mode, EXPAND_NORMAL);
	  if (contains_call_div_mod (get_insns ()))
	    quotient = NULL_RTX;
	  else
	    {
	      ops.code = TRUNC_MOD_EXPR;
	      remainder = expand_expr_real_2 (&ops, NULL_RTX, mode,
					      EXPAND_NORMAL);
	      if (contains_call_div_mod (get_insns ()))
		remainder = NULL_RTX;
	    }
	  if (remainder)
	    insns = get_insns ();
	  end_sequence ();
	}
    }

  if (remainder)
    emit_insn (insns);

  /* Check if optab_handler exists for divmod_optab for given mode.  */
  else if (optab_handler (tab, mode) != CODE_FOR_nothing)
    {
      quotient = gen_reg_rtx (mode);
      remainder = gen_reg_rtx (mode);
      expand_twoval_binop (tab, op0, op1, quotient, remainder, unsignedp);
    }

  /* Generate call to divmod libfunc if it exists.  */
  else if (rtx libfunc = optab_libfunc (tab, mode))
    targetm.expand_divmod_libfunc (libfunc, mode, op0, op1,
				   &quotient, &remainder);

  else
    gcc_unreachable ();

  /* Wrap the return value (quotient, remainder) within COMPLEX_EXPR.  */
  expand_expr (build2 (COMPLEX_EXPR, TREE_TYPE (lhs),
		       make_tree (TREE_TYPE (arg0), quotient),
		       make_tree (TREE_TYPE (arg1), remainder)),
	       target, VOIDmode, EXPAND_NORMAL);
}

/* Expand a NOP.  */

static void
expand_NOP (internal_fn, gcall *)
{
  /* Nothing.  But it shouldn't really prevail.  */
}

/* Coroutines, all should have been processed at this stage.  */

static void
expand_CO_FRAME (internal_fn, gcall *)
{
  gcc_unreachable ();
}

static void
expand_CO_YIELD (internal_fn, gcall *)
{
  gcc_unreachable ();
}

static void
expand_CO_SUSPN (internal_fn, gcall *)
{
  gcc_unreachable ();
}

static void
expand_CO_ACTOR (internal_fn, gcall *)
{
  gcc_unreachable ();
}

/* Expand a call to FN using the operands in STMT.  FN has a single
   output operand and NARGS input operands.  */

static void
expand_direct_optab_fn (internal_fn fn, gcall *stmt, direct_optab optab,
			unsigned int nargs)
{
  tree_pair types = direct_internal_fn_types (fn, stmt);
  insn_code icode = direct_optab_handler (optab, TYPE_MODE (types.first));
  expand_fn_using_insn (stmt, icode, 1, nargs);
}

/* Expand WHILE_ULT call STMT using optab OPTAB.  */

static void
expand_while_optab_fn (internal_fn, gcall *stmt, convert_optab optab)
{
  expand_operand ops[4];
  tree rhs_type[2];

  tree lhs = gimple_call_lhs (stmt);
  tree lhs_type = TREE_TYPE (lhs);
  rtx lhs_rtx = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  create_call_lhs_operand (&ops[0], lhs_rtx, TYPE_MODE (lhs_type));

  for (unsigned int i = 0; i < 2; ++i)
    {
      tree rhs = gimple_call_arg (stmt, i);
      rhs_type[i] = TREE_TYPE (rhs);
      rtx rhs_rtx = expand_normal (rhs);
      create_input_operand (&ops[i + 1], rhs_rtx, TYPE_MODE (rhs_type[i]));
    }

  int opcnt;
  if (!VECTOR_MODE_P (TYPE_MODE (lhs_type)))
    {
      /* When the mask is an integer mode the exact vector length may not
	 be clear to the backend, so we pass it in operand[3].
         Use the vector in arg2 for the most reliable intended size.  */
      tree type = TREE_TYPE (gimple_call_arg (stmt, 2));
      create_integer_operand (&ops[3], TYPE_VECTOR_SUBPARTS (type));
      opcnt = 4;
    }
  else
    /* The mask has a vector type so the length operand is unnecessary.  */
    opcnt = 3;

  insn_code icode = convert_optab_handler (optab, TYPE_MODE (rhs_type[0]),
					   TYPE_MODE (lhs_type));

  expand_insn (icode, opcnt, ops);
  assign_call_lhs (lhs, lhs_rtx, &ops[0]);
}

/* Expand a call to a convert-like optab using the operands in STMT.
   FN has a single output operand and NARGS input operands.  */

static void
expand_convert_optab_fn (internal_fn fn, gcall *stmt, convert_optab optab,
			 unsigned int nargs)
{
  tree_pair types = direct_internal_fn_types (fn, stmt);
  insn_code icode = convert_optab_handler (optab, TYPE_MODE (types.first),
					  TYPE_MODE (types.second));
  expand_fn_using_insn (stmt, icode, 1, nargs);
}

/* Expand CRC call STMT.  */

static void
expand_crc_optab_fn (internal_fn fn, gcall *stmt, convert_optab optab)
{
  tree lhs = gimple_call_lhs (stmt);
  tree rhs1 = gimple_call_arg (stmt, 0); // crc
  tree rhs2 = gimple_call_arg (stmt, 1); // data
  tree rhs3 = gimple_call_arg (stmt, 2); // polynomial

  tree result_type = TREE_TYPE (lhs);
  tree data_type = TREE_TYPE (rhs2);

  gcc_assert (TYPE_MODE (result_type) >= TYPE_MODE (data_type));

  rtx dest = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  rtx crc = expand_normal (rhs1);
  rtx data = expand_normal (rhs2);
  gcc_assert (TREE_CODE (rhs3) == INTEGER_CST);
  rtx polynomial = gen_rtx_CONST_INT (TYPE_MODE (result_type),
				      TREE_INT_CST_LOW (rhs3));

  /* Use target specific expansion if it exists.
     Otherwise, generate table-based CRC.  */
  if (direct_internal_fn_supported_p (fn, tree_pair (data_type, result_type),
				      OPTIMIZE_FOR_SPEED))
    {
      class expand_operand ops[4];

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file,
		   ";; using optab for crc_%u_polynomial_"
		   HOST_WIDE_INT_PRINT_HEX "\n",
		   GET_MODE_BITSIZE (GET_MODE (dest)).to_constant (),
		   TREE_INT_CST_LOW (rhs3));
	}

      create_call_lhs_operand (&ops[0], dest, TYPE_MODE (result_type));
      create_input_operand (&ops[1], crc, TYPE_MODE (result_type));
      create_input_operand (&ops[2], data, TYPE_MODE (data_type));
      create_input_operand (&ops[3], polynomial, TYPE_MODE (result_type));
      insn_code icode = convert_optab_handler (optab, TYPE_MODE (data_type),
					       TYPE_MODE (result_type));
      expand_insn (icode, 4, ops);
      assign_call_lhs (lhs, dest, &ops[0]);
    }
  else
    {
      /* We're bypassing all the operand conversions that are done in the
	 case when we get an icode, operands and pass that off to expand_insn.

	 That path has special case handling for promoted return values which
	 we must emulate here (is the same kind of special treatment ever
	 needed for input arguments here?).

	 In particular we do not want to store directly into a promoted
	 SUBREG destination, instead store into a suitably sized pseudo.  */
      rtx orig_dest = dest;
      if (SUBREG_P (dest) && SUBREG_PROMOTED_VAR_P (dest))
	dest = gen_reg_rtx (GET_MODE (dest));

      /* If it's IFN_CRC generate bit-forward CRC.  */
      if (fn == IFN_CRC)
	expand_crc_table_based (dest, crc, data, polynomial,
				TYPE_MODE (data_type));
      else
	/* If it's IFN_CRC_REV generate bit-reversed CRC.  */
	expand_reversed_crc_table_based (dest, crc, data, polynomial,
					 TYPE_MODE (data_type),
					 generate_reflecting_code_standard);

      /* Now get the return value where it needs to be, taking care to
	 ensure it's promoted appropriately if the ABI demands it.

	 Re-use assign_call_lhs to handle the details.  */
      class expand_operand ops[4];
      create_call_lhs_operand (&ops[0], dest, TYPE_MODE (result_type));
      ops[0].value = dest;
      assign_call_lhs (lhs, orig_dest, &ops[0]);
    }
}

/* Expanders for optabs that can use expand_direct_optab_fn.  */

#define expand_unary_optab_fn(FN, STMT, OPTAB) \
  expand_direct_optab_fn (FN, STMT, OPTAB, 1)

#define expand_binary_optab_fn(FN, STMT, OPTAB) \
  expand_direct_optab_fn (FN, STMT, OPTAB, 2)

#define expand_ternary_optab_fn(FN, STMT, OPTAB) \
  expand_direct_optab_fn (FN, STMT, OPTAB, 3)

#define expand_cond_unary_optab_fn(FN, STMT, OPTAB) \
  expand_direct_optab_fn (FN, STMT, OPTAB, 3)

#define expand_cond_binary_optab_fn(FN, STMT, OPTAB) \
  expand_direct_optab_fn (FN, STMT, OPTAB, 4)

#define expand_cond_ternary_optab_fn(FN, STMT, OPTAB) \
  expand_direct_optab_fn (FN, STMT, OPTAB, 5)

#define expand_cond_len_unary_optab_fn(FN, STMT, OPTAB) \
  expand_direct_optab_fn (FN, STMT, OPTAB, 5)

#define expand_cond_len_binary_optab_fn(FN, STMT, OPTAB) \
  expand_direct_optab_fn (FN, STMT, OPTAB, 6)

#define expand_cond_len_ternary_optab_fn(FN, STMT, OPTAB) \
  expand_direct_optab_fn (FN, STMT, OPTAB, 7)

#define expand_fold_extract_optab_fn(FN, STMT, OPTAB) \
  expand_direct_optab_fn (FN, STMT, OPTAB, 3)

#define expand_fold_len_extract_optab_fn(FN, STMT, OPTAB) \
  expand_direct_optab_fn (FN, STMT, OPTAB, 5)

#define expand_fold_left_optab_fn(FN, STMT, OPTAB) \
  expand_direct_optab_fn (FN, STMT, OPTAB, 2)

#define expand_mask_fold_left_optab_fn(FN, STMT, OPTAB) \
  expand_direct_optab_fn (FN, STMT, OPTAB, 3)

#define expand_mask_len_fold_left_optab_fn(FN, STMT, OPTAB) \
  expand_direct_optab_fn (FN, STMT, OPTAB, 5)

#define expand_check_ptrs_optab_fn(FN, STMT, OPTAB) \
  expand_direct_optab_fn (FN, STMT, OPTAB, 4)

/* Expanders for optabs that can use expand_convert_optab_fn.  */

#define expand_unary_convert_optab_fn(FN, STMT, OPTAB) \
  expand_convert_optab_fn (FN, STMT, OPTAB, 1)

#define expand_vec_extract_optab_fn(FN, STMT, OPTAB) \
  expand_convert_optab_fn (FN, STMT, OPTAB, 2)

/* RETURN_TYPE and ARGS are a return type and argument list that are
   in principle compatible with FN (which satisfies direct_internal_fn_p).
   Return the types that should be used to determine whether the
   target supports FN.  */

tree_pair
direct_internal_fn_types (internal_fn fn, tree return_type, tree *args)
{
  const direct_internal_fn_info &info = direct_internal_fn (fn);
  tree type0 = (info.type0 < 0 ? return_type : TREE_TYPE (args[info.type0]));
  tree type1 = (info.type1 < 0 ? return_type : TREE_TYPE (args[info.type1]));
  return tree_pair (type0, type1);
}

/* CALL is a call whose return type and arguments are in principle
   compatible with FN (which satisfies direct_internal_fn_p).  Return the
   types that should be used to determine whether the target supports FN.  */

tree_pair
direct_internal_fn_types (internal_fn fn, gcall *call)
{
  const direct_internal_fn_info &info = direct_internal_fn (fn);
  tree op0 = (info.type0 < 0
	      ? gimple_call_lhs (call)
	      : gimple_call_arg (call, info.type0));
  tree op1 = (info.type1 < 0
	      ? gimple_call_lhs (call)
	      : gimple_call_arg (call, info.type1));
  return tree_pair (TREE_TYPE (op0), TREE_TYPE (op1));
}

/* Return true if OPTAB is supported for TYPES (whose modes should be
   the same) when the optimization type is OPT_TYPE.  Used for simple
   direct optabs.  */

static bool
direct_optab_supported_p (direct_optab optab, tree_pair types,
			  optimization_type opt_type)
{
  machine_mode mode = TYPE_MODE (types.first);
  gcc_checking_assert (mode == TYPE_MODE (types.second));
  return direct_optab_handler (optab, mode, opt_type) != CODE_FOR_nothing;
}

/* Return true if OPTAB is supported for TYPES, where the first type
   is the destination and the second type is the source.  Used for
   convert optabs.  */

static bool
convert_optab_supported_p (convert_optab optab, tree_pair types,
			   optimization_type opt_type)
{
  return (convert_optab_handler (optab, TYPE_MODE (types.first),
				 TYPE_MODE (types.second), opt_type)
	  != CODE_FOR_nothing);
}

/* Return true if load/store lanes optab OPTAB is supported for
   array type TYPES.first when the optimization type is OPT_TYPE.  */

static bool
multi_vector_optab_supported_p (convert_optab optab, tree_pair types,
				optimization_type opt_type)
{
  gcc_assert (TREE_CODE (types.first) == ARRAY_TYPE);
  machine_mode imode = TYPE_MODE (types.first);
  machine_mode vmode = TYPE_MODE (TREE_TYPE (types.first));
  return (convert_optab_handler (optab, imode, vmode, opt_type)
	  != CODE_FOR_nothing);
}

#define direct_unary_optab_supported_p direct_optab_supported_p
#define direct_unary_convert_optab_supported_p convert_optab_supported_p
#define direct_binary_optab_supported_p direct_optab_supported_p
#define direct_ternary_optab_supported_p direct_optab_supported_p
#define direct_cond_unary_optab_supported_p direct_optab_supported_p
#define direct_cond_binary_optab_supported_p direct_optab_supported_p
#define direct_cond_ternary_optab_supported_p direct_optab_supported_p
#define direct_cond_len_unary_optab_supported_p direct_optab_supported_p
#define direct_cond_len_binary_optab_supported_p direct_optab_supported_p
#define direct_cond_len_ternary_optab_supported_p direct_optab_supported_p
#define direct_crc_optab_supported_p convert_optab_supported_p
#define direct_mask_load_optab_supported_p convert_optab_supported_p
#define direct_load_lanes_optab_supported_p multi_vector_optab_supported_p
#define direct_mask_load_lanes_optab_supported_p multi_vector_optab_supported_p
#define direct_gather_load_optab_supported_p convert_optab_supported_p
#define direct_strided_load_optab_supported_p direct_optab_supported_p
#define direct_len_load_optab_supported_p direct_optab_supported_p
#define direct_mask_len_load_optab_supported_p convert_optab_supported_p
#define direct_mask_store_optab_supported_p convert_optab_supported_p
#define direct_store_lanes_optab_supported_p multi_vector_optab_supported_p
#define direct_mask_store_lanes_optab_supported_p multi_vector_optab_supported_p
#define direct_vec_cond_mask_optab_supported_p convert_optab_supported_p
#define direct_vec_cond_optab_supported_p convert_optab_supported_p
#define direct_scatter_store_optab_supported_p convert_optab_supported_p
#define direct_strided_store_optab_supported_p direct_optab_supported_p
#define direct_len_store_optab_supported_p direct_optab_supported_p
#define direct_mask_len_store_optab_supported_p convert_optab_supported_p
#define direct_while_optab_supported_p convert_optab_supported_p
#define direct_fold_extract_optab_supported_p direct_optab_supported_p
#define direct_fold_len_extract_optab_supported_p direct_optab_supported_p
#define direct_fold_left_optab_supported_p direct_optab_supported_p
#define direct_mask_fold_left_optab_supported_p direct_optab_supported_p
#define direct_mask_len_fold_left_optab_supported_p direct_optab_supported_p
#define direct_check_ptrs_optab_supported_p direct_optab_supported_p
#define direct_vec_set_optab_supported_p direct_optab_supported_p
#define direct_vec_extract_optab_supported_p convert_optab_supported_p

/* Return the optab used by internal function FN.  */

optab
direct_internal_fn_optab (internal_fn fn, tree_pair types)
{
  switch (fn)
    {
#define DEF_INTERNAL_FN(CODE, FLAGS, FNSPEC) \
    case IFN_##CODE: break;
#define DEF_INTERNAL_OPTAB_FN(CODE, FLAGS, OPTAB, TYPE) \
    case IFN_##CODE: return OPTAB##_optab;
#define DEF_INTERNAL_SIGNED_OPTAB_FN(CODE, FLAGS, SELECTOR, SIGNED_OPTAB, \
				     UNSIGNED_OPTAB, TYPE)		\
    case IFN_##CODE: return (TYPE_UNSIGNED (types.SELECTOR)		\
			     ? UNSIGNED_OPTAB ## _optab			\
			     : SIGNED_OPTAB ## _optab);
#include "internal-fn.def"

    case IFN_LAST:
      break;
    }
  gcc_unreachable ();
}

/* Return the optab used by internal function FN.  */

static optab
direct_internal_fn_optab (internal_fn fn)
{
  switch (fn)
    {
#define DEF_INTERNAL_FN(CODE, FLAGS, FNSPEC) \
    case IFN_##CODE: break;
#define DEF_INTERNAL_OPTAB_FN(CODE, FLAGS, OPTAB, TYPE) \
    case IFN_##CODE: return OPTAB##_optab;
#include "internal-fn.def"

    case IFN_LAST:
      break;
    }
  gcc_unreachable ();
}

/* Return true if TYPE's mode has the same format as TYPE, and if there is
   a 1:1 correspondence between the values that the mode can store and the
   values that the type can store.  */

static bool
type_strictly_matches_mode_p (const_tree type)
{
  /* The masked vector operations have both vector data operands and vector
     boolean operands.  The vector data operands are expected to have a vector
     mode,  but the vector boolean operands can be an integer mode rather than
     a vector mode,  depending on how TARGET_VECTORIZE_GET_MASK_MODE is
     defined.  PR116103.  */
  if (VECTOR_BOOLEAN_TYPE_P (type)
      && SCALAR_INT_MODE_P (TYPE_MODE (type))
      && TYPE_PRECISION (TREE_TYPE (type)) == 1)
    return true;

  if (VECTOR_TYPE_P (type))
    return VECTOR_MODE_P (TYPE_MODE (type));

  if (INTEGRAL_TYPE_P (type))
    return type_has_mode_precision_p (type);

  if (SCALAR_FLOAT_TYPE_P (type) || COMPLEX_FLOAT_TYPE_P (type))
    return true;

  return false;
}

/* Returns true if both types of TYPE_PAIR strictly match their modes,
   else returns false.  */

static bool
type_pair_strictly_matches_mode_p (tree_pair type_pair)
{
  return type_strictly_matches_mode_p (type_pair.first)
    && type_strictly_matches_mode_p (type_pair.second);
}

/* Return true if FN is supported for the types in TYPES when the
   optimization type is OPT_TYPE.  The types are those associated with
   the "type0" and "type1" fields of FN's direct_internal_fn_info
   structure.  */

bool
direct_internal_fn_supported_p (internal_fn fn, tree_pair types,
				optimization_type opt_type)
{
  if (!type_pair_strictly_matches_mode_p (types))
    return false;

  switch (fn)
    {
#define DEF_INTERNAL_FN(CODE, FLAGS, FNSPEC) \
    case IFN_##CODE: break;
#define DEF_INTERNAL_OPTAB_FN(CODE, FLAGS, OPTAB, TYPE) \
    case IFN_##CODE: \
      return direct_##TYPE##_optab_supported_p (OPTAB##_optab, types, \
						opt_type);
#define DEF_INTERNAL_SIGNED_OPTAB_FN(CODE, FLAGS, SELECTOR, SIGNED_OPTAB, \
				     UNSIGNED_OPTAB, TYPE)		\
    case IFN_##CODE:							\
      {									\
	optab which_optab = (TYPE_UNSIGNED (types.SELECTOR)		\
			     ? UNSIGNED_OPTAB ## _optab			\
			     : SIGNED_OPTAB ## _optab);			\
	return direct_##TYPE##_optab_supported_p (which_optab, types,	\
						  opt_type);		\
      }
#include "internal-fn.def"

    case IFN_LAST:
      break;
    }
  gcc_unreachable ();
}

/* Return true if FN is supported for type TYPE when the optimization
   type is OPT_TYPE.  The caller knows that the "type0" and "type1"
   fields of FN's direct_internal_fn_info structure are the same.  */

bool
direct_internal_fn_supported_p (internal_fn fn, tree type,
				optimization_type opt_type)
{
  const direct_internal_fn_info &info = direct_internal_fn (fn);
  gcc_checking_assert (info.type0 == info.type1);
  return direct_internal_fn_supported_p (fn, tree_pair (type, type), opt_type);
}

/* Return true if the STMT is supported when the optimization type is OPT_TYPE,
   given that STMT is a call to a direct internal function.  */

bool
direct_internal_fn_supported_p (gcall *stmt, optimization_type opt_type)
{
  internal_fn fn = gimple_call_internal_fn (stmt);
  tree_pair types = direct_internal_fn_types (fn, stmt);
  return direct_internal_fn_supported_p (fn, types, opt_type);
}

/* Return true if FN is a binary operation and if FN is commutative.  */

bool
commutative_binary_fn_p (internal_fn fn)
{
  switch (fn)
    {
    case IFN_AVG_FLOOR:
    case IFN_AVG_CEIL:
    case IFN_MULH:
    case IFN_MULHS:
    case IFN_MULHRS:
    case IFN_FMIN:
    case IFN_FMAX:
    case IFN_COMPLEX_MUL:
    case IFN_UBSAN_CHECK_ADD:
    case IFN_UBSAN_CHECK_MUL:
    case IFN_ADD_OVERFLOW:
    case IFN_MUL_OVERFLOW:
    case IFN_SAT_ADD:
    case IFN_VEC_WIDEN_PLUS:
    case IFN_VEC_WIDEN_PLUS_LO:
    case IFN_VEC_WIDEN_PLUS_HI:
    case IFN_VEC_WIDEN_PLUS_EVEN:
    case IFN_VEC_WIDEN_PLUS_ODD:
      return true;

    default:
      return false;
    }
}

/* Return true if FN is a ternary operation and if its first two arguments
   are commutative.  */

bool
commutative_ternary_fn_p (internal_fn fn)
{
  switch (fn)
    {
    case IFN_FMA:
    case IFN_FMS:
    case IFN_FNMA:
    case IFN_FNMS:
    case IFN_UADDC:
      return true;

    default:
      return false;
    }
}

/* Return true if FN is an associative binary operation.  */

bool
associative_binary_fn_p (internal_fn fn)
{
  switch (fn)
    {
    case IFN_FMIN:
    case IFN_FMAX:
      return true;

    default:
      return false;
    }
}

/* If FN is commutative in two consecutive arguments, return the
   index of the first, otherwise return -1.  */

int
first_commutative_argument (internal_fn fn)
{
  switch (fn)
    {
    case IFN_COND_ADD:
    case IFN_COND_MUL:
    case IFN_COND_MIN:
    case IFN_COND_MAX:
    case IFN_COND_FMIN:
    case IFN_COND_FMAX:
    case IFN_COND_AND:
    case IFN_COND_IOR:
    case IFN_COND_XOR:
    case IFN_COND_FMA:
    case IFN_COND_FMS:
    case IFN_COND_FNMA:
    case IFN_COND_FNMS:
    case IFN_COND_LEN_ADD:
    case IFN_COND_LEN_MUL:
    case IFN_COND_LEN_MIN:
    case IFN_COND_LEN_MAX:
    case IFN_COND_LEN_FMIN:
    case IFN_COND_LEN_FMAX:
    case IFN_COND_LEN_AND:
    case IFN_COND_LEN_IOR:
    case IFN_COND_LEN_XOR:
    case IFN_COND_LEN_FMA:
    case IFN_COND_LEN_FMS:
    case IFN_COND_LEN_FNMA:
    case IFN_COND_LEN_FNMS:
      return 1;

    default:
      if (commutative_binary_fn_p (fn)
	  || commutative_ternary_fn_p (fn))
	return 0;
      return -1;
    }
}

/* Return true if this CODE describes an internal_fn that returns a vector with
   elements twice as wide as the element size of the input vectors.  */

bool
widening_fn_p (code_helper code)
{
  if (!code.is_fn_code ())
    return false;

  if (!internal_fn_p ((combined_fn) code))
    return false;

  internal_fn fn = as_internal_fn ((combined_fn) code);
  switch (fn)
    {
    #define DEF_INTERNAL_WIDENING_OPTAB_FN(NAME, F, S, SO, UO, T) \
    case IFN_##NAME:						  \
    case IFN_##NAME##_HI:					  \
    case IFN_##NAME##_LO:					  \
    case IFN_##NAME##_EVEN:					  \
    case IFN_##NAME##_ODD:					  \
      return true;
    #include "internal-fn.def"

    default:
      return false;
    }
}

/* Return true if IFN_SET_EDOM is supported.  */

bool
set_edom_supported_p (void)
{
#ifdef TARGET_EDOM
  return true;
#else
  return false;
#endif
}

#define DEF_INTERNAL_OPTAB_FN(CODE, FLAGS, OPTAB, TYPE) \
  static void						\
  expand_##CODE (internal_fn fn, gcall *stmt)		\
  {							\
    expand_##TYPE##_optab_fn (fn, stmt, OPTAB##_optab);	\
  }
#define DEF_INTERNAL_INT_EXT_FN(CODE, FLAGS, OPTAB, TYPE)
#define DEF_INTERNAL_SIGNED_OPTAB_FN(CODE, FLAGS, SELECTOR, SIGNED_OPTAB, \
				     UNSIGNED_OPTAB, TYPE)		\
  static void								\
  expand_##CODE (internal_fn fn, gcall *stmt)				\
  {									\
    tree_pair types = direct_internal_fn_types (fn, stmt);		\
    optab which_optab = direct_internal_fn_optab (fn, types);		\
    expand_##TYPE##_optab_fn (fn, stmt, which_optab);			\
  }
#include "internal-fn.def"

/* Routines to expand each internal function, indexed by function number.
   Each routine has the prototype:

       expand_<NAME> (gcall *stmt)

   where STMT is the statement that performs the call. */
static void (*const internal_fn_expanders[]) (internal_fn, gcall *) = {

#define DEF_INTERNAL_FN(CODE, FLAGS, FNSPEC) expand_##CODE,
#include "internal-fn.def"
  0
};

/* Invoke T(CODE, SUFFIX) for each conditional function IFN_COND_##SUFFIX
   that maps to a tree code CODE.  There is also an IFN_COND_LEN_##SUFFIX
   for each such IFN_COND_##SUFFIX.  */
#define FOR_EACH_CODE_MAPPING(T) \
  T (PLUS_EXPR, ADD) \
  T (MINUS_EXPR, SUB) \
  T (MULT_EXPR, MUL) \
  T (TRUNC_DIV_EXPR, DIV) \
  T (TRUNC_MOD_EXPR, MOD) \
  T (RDIV_EXPR, RDIV) \
  T (MIN_EXPR, MIN) \
  T (MAX_EXPR, MAX) \
  T (BIT_AND_EXPR, AND) \
  T (BIT_IOR_EXPR, IOR) \
  T (BIT_XOR_EXPR, XOR) \
  T (LSHIFT_EXPR, SHL) \
  T (RSHIFT_EXPR, SHR) \
  T (NEGATE_EXPR, NEG)

/* Return a function that only performs CODE when a certain condition is met
   and that uses a given fallback value otherwise.  For example, if CODE is
   a binary operation associated with conditional function FN:

     LHS = FN (COND, A, B, ELSE)

   is equivalent to the C expression:

     LHS = COND ? A CODE B : ELSE;

   operating elementwise if the operands are vectors.

   Return IFN_LAST if no such function exists.  */

internal_fn
get_conditional_internal_fn (tree_code code)
{
  switch (code)
    {
#define CASE(CODE, IFN) case CODE: return IFN_COND_##IFN;
      FOR_EACH_CODE_MAPPING(CASE)
#undef CASE
    default:
      return IFN_LAST;
    }
}

/* If IFN implements the conditional form of a tree code, return that
   tree code, otherwise return ERROR_MARK.  */

tree_code
conditional_internal_fn_code (internal_fn ifn)
{
  switch (ifn)
    {
#define CASE(CODE, IFN)                                                        \
  case IFN_COND_##IFN:                                                         \
  case IFN_COND_LEN_##IFN:                                                     \
    return CODE;
      FOR_EACH_CODE_MAPPING (CASE)
#undef CASE
      default:
	return ERROR_MARK;
    }
}

/* Like get_conditional_internal_fn, but return a function that
   additionally restricts the operation to the leading elements
   of a vector.  The number of elements to process is given by a length
   and bias pair, as for IFN_LOAD_LEN.  The values of the remaining
   elements are taken from the fallback ("else") argument.

   For example, if CODE is a binary operation associated with FN:

     LHS = FN (COND, A, B, ELSE, LEN, BIAS)

   is equivalent to the C code:

     for (int i = 0; i < NUNITS; i++)
      {
	if (i < LEN + BIAS && COND[i])
	  LHS[i] = A[i] CODE B[i];
	else
	  LHS[i] = ELSE[i];
      }
*/

internal_fn
get_conditional_len_internal_fn (tree_code code)
{
  switch (code)
    {
#define CASE(CODE, IFN) case CODE: return IFN_COND_LEN_##IFN;
      FOR_EACH_CODE_MAPPING(CASE)
#undef CASE
    default:
      return IFN_LAST;
    }
}

/* Invoke T(IFN) for each internal function IFN that also has an
   IFN_COND_* form.  */
#define FOR_EACH_COND_FN_PAIR(T) \
  T (FMAX) \
  T (FMIN) \
  T (FMA) \
  T (FMS) \
  T (FNMA) \
  T (FNMS)

/* Return a function that only performs internal function FN when a
   certain condition is met and that uses a given fallback value otherwise.
   In other words, the returned function FN' is such that:

     LHS = FN' (COND, A1, ... An, ELSE)

   is equivalent to the C expression:

     LHS = COND ? FN (A1, ..., An) : ELSE;

   operating elementwise if the operands are vectors.

   Return IFN_LAST if no such function exists.  */

internal_fn
get_conditional_internal_fn (internal_fn fn)
{
  switch (fn)
    {
#define CASE(NAME) case IFN_##NAME: return IFN_COND_##NAME;
      FOR_EACH_COND_FN_PAIR(CASE)
#undef CASE
    default:
      return IFN_LAST;
    }
}

/* If there exists an internal function like IFN that operates on vectors,
   but with additional length and bias parameters, return the internal_fn
   for that function, otherwise return IFN_LAST.  */
internal_fn
get_len_internal_fn (internal_fn fn)
{
  switch (fn)
    {
#define DEF_INTERNAL_COND_FN(NAME, ...)                                        \
  case IFN_COND_##NAME:                                                        \
    return IFN_COND_LEN_##NAME;
#define DEF_INTERNAL_SIGNED_COND_FN(NAME, ...)                                 \
  case IFN_COND_##NAME:                                                        \
    return IFN_COND_LEN_##NAME;
#include "internal-fn.def"
    default:
      break;
    }

  switch (fn)
    {
    case IFN_MASK_LOAD:
      return IFN_MASK_LEN_LOAD;
    case IFN_MASK_LOAD_LANES:
      return IFN_MASK_LEN_LOAD_LANES;
    case IFN_MASK_GATHER_LOAD:
      return IFN_MASK_LEN_GATHER_LOAD;
    default:
      return IFN_LAST;
    }
}

/* If IFN implements the conditional form of an unconditional internal
   function, return that unconditional function, otherwise return IFN_LAST.  */

internal_fn
get_unconditional_internal_fn (internal_fn ifn)
{
  switch (ifn)
    {
#define CASE(NAME)                                                             \
    case IFN_COND_##NAME:                                                      \
    case IFN_COND_LEN_##NAME:                                                  \
      return IFN_##NAME;
FOR_EACH_COND_FN_PAIR (CASE)
#undef CASE
    default:
      return IFN_LAST;
    }
}

/* Return true if STMT can be interpreted as a conditional tree code
   operation of the form:

     LHS = COND ? OP (RHS1, ...) : ELSE;

   operating elementwise if the operands are vectors.  This includes
   the case of an all-true COND, so that the operation always happens.

   There is an alternative approach to interpret the STMT when the operands
   are vectors which is the operation predicated by both conditional mask
   and loop control length, the equivalent C code:

     for (int i = 0; i < NUNTIS; i++)
      {
	if (i < LEN + BIAS && COND[i])
	  LHS[i] = A[i] CODE B[i];
	else
	  LHS[i] = ELSE[i];
      }

   When returning true, set:

   - *COND_OUT to the condition COND, or to NULL_TREE if the condition
     is known to be all-true
   - *CODE_OUT to the tree code
   - OPS[I] to operand I of *CODE_OUT
   - *ELSE_OUT to the fallback value ELSE, or to NULL_TREE if the
     condition is known to be all true.
   - *LEN to the len argument if it COND_LEN_* operations or to NULL_TREE.
   - *BIAS to the bias argument if it COND_LEN_* operations or to NULL_TREE.  */

bool
can_interpret_as_conditional_op_p (gimple *stmt, tree *cond_out,
				   tree_code *code_out,
				   tree (&ops)[3], tree *else_out,
				   tree *len, tree *bias)
{
  *len = NULL_TREE;
  *bias = NULL_TREE;
  if (gassign *assign = dyn_cast <gassign *> (stmt))
    {
      *cond_out = NULL_TREE;
      *code_out = gimple_assign_rhs_code (assign);
      ops[0] = gimple_assign_rhs1 (assign);
      ops[1] = gimple_assign_rhs2 (assign);
      ops[2] = gimple_assign_rhs3 (assign);
      *else_out = NULL_TREE;
      return true;
    }
  if (gcall *call = dyn_cast <gcall *> (stmt))
    if (gimple_call_internal_p (call))
      {
	internal_fn ifn = gimple_call_internal_fn (call);
	tree_code code = conditional_internal_fn_code (ifn);
	int len_index = internal_fn_len_index (ifn);
	int cond_nargs = len_index >= 0 ? 4 : 2;
	if (code != ERROR_MARK)
	  {
	    *cond_out = gimple_call_arg (call, 0);
	    *code_out = code;
	    unsigned int nops = gimple_call_num_args (call) - cond_nargs;
	    for (unsigned int i = 0; i < 3; ++i)
	      ops[i] = i < nops ? gimple_call_arg (call, i + 1) : NULL_TREE;
	    *else_out = gimple_call_arg (call, nops + 1);
	    if (len_index < 0)
	      {
		if (integer_truep (*cond_out))
		  {
		    *cond_out = NULL_TREE;
		    *else_out = NULL_TREE;
		  }
	      }
	    else
	      {
		*len = gimple_call_arg (call, len_index);
		*bias = gimple_call_arg (call, len_index + 1);
	      }
	    return true;
	  }
      }
  return false;
}

/* Return true if IFN is some form of load from memory.  */

bool
internal_load_fn_p (internal_fn fn)
{
  switch (fn)
    {
    case IFN_MASK_LOAD:
    case IFN_LOAD_LANES:
    case IFN_MASK_LOAD_LANES:
    case IFN_MASK_LEN_LOAD_LANES:
    case IFN_GATHER_LOAD:
    case IFN_MASK_GATHER_LOAD:
    case IFN_MASK_LEN_GATHER_LOAD:
    case IFN_LEN_LOAD:
    case IFN_MASK_LEN_LOAD:
      return true;

    default:
      return false;
    }
}

/* Return true if IFN is some form of store to memory.  */

bool
internal_store_fn_p (internal_fn fn)
{
  switch (fn)
    {
    case IFN_MASK_STORE:
    case IFN_STORE_LANES:
    case IFN_MASK_STORE_LANES:
    case IFN_MASK_LEN_STORE_LANES:
    case IFN_SCATTER_STORE:
    case IFN_MASK_SCATTER_STORE:
    case IFN_MASK_LEN_SCATTER_STORE:
    case IFN_LEN_STORE:
    case IFN_MASK_LEN_STORE:
      return true;

    default:
      return false;
    }
}

/* Return true if IFN is some form of gather load or scatter store.  */

bool
internal_gather_scatter_fn_p (internal_fn fn)
{
  switch (fn)
    {
    case IFN_GATHER_LOAD:
    case IFN_MASK_GATHER_LOAD:
    case IFN_MASK_LEN_GATHER_LOAD:
    case IFN_SCATTER_STORE:
    case IFN_MASK_SCATTER_STORE:
    case IFN_MASK_LEN_SCATTER_STORE:
      return true;

    default:
      return false;
    }
}

/* If FN takes a vector len argument, return the index of that argument,
   otherwise return -1.  */

int
internal_fn_len_index (internal_fn fn)
{
  switch (fn)
    {
    case IFN_LEN_LOAD:
    case IFN_LEN_STORE:
      return 2;

    case IFN_MASK_LEN_SCATTER_STORE:
    case IFN_MASK_LEN_STRIDED_LOAD:
      return 5;

    case IFN_MASK_LEN_GATHER_LOAD:
      return 6;

    case IFN_COND_LEN_FMA:
    case IFN_COND_LEN_FMS:
    case IFN_COND_LEN_FNMA:
    case IFN_COND_LEN_FNMS:
      return 5;

    case IFN_COND_LEN_ADD:
    case IFN_COND_LEN_SUB:
    case IFN_COND_LEN_MUL:
    case IFN_COND_LEN_DIV:
    case IFN_COND_LEN_MOD:
    case IFN_COND_LEN_RDIV:
    case IFN_COND_LEN_MIN:
    case IFN_COND_LEN_MAX:
    case IFN_COND_LEN_FMIN:
    case IFN_COND_LEN_FMAX:
    case IFN_COND_LEN_AND:
    case IFN_COND_LEN_IOR:
    case IFN_COND_LEN_XOR:
    case IFN_COND_LEN_SHL:
    case IFN_COND_LEN_SHR:
    case IFN_MASK_LEN_STRIDED_STORE:
      return 4;

    case IFN_COND_LEN_NEG:
    case IFN_MASK_LEN_STORE:
    case IFN_MASK_LEN_STORE_LANES:
    case IFN_VCOND_MASK_LEN:
      return 3;

    case IFN_MASK_LEN_LOAD:
    case IFN_MASK_LEN_LOAD_LANES:
      return 4;

    default:
      return -1;
    }
}

/* If FN is an IFN_COND_* or IFN_COND_LEN_* function, return the index of the
   argument that is used when the condition is false.  Return -1 otherwise.  */

int
internal_fn_else_index (internal_fn fn)
{
  switch (fn)
    {
    case IFN_COND_NEG:
    case IFN_COND_NOT:
    case IFN_COND_LEN_NEG:
    case IFN_COND_LEN_NOT:
      return 2;

    case IFN_COND_ADD:
    case IFN_COND_SUB:
    case IFN_COND_MUL:
    case IFN_COND_DIV:
    case IFN_COND_MOD:
    case IFN_COND_MIN:
    case IFN_COND_MAX:
    case IFN_COND_FMIN:
    case IFN_COND_FMAX:
    case IFN_COND_AND:
    case IFN_COND_IOR:
    case IFN_COND_XOR:
    case IFN_COND_SHL:
    case IFN_COND_SHR:
    case IFN_COND_LEN_ADD:
    case IFN_COND_LEN_SUB:
    case IFN_COND_LEN_MUL:
    case IFN_COND_LEN_DIV:
    case IFN_COND_LEN_MOD:
    case IFN_COND_LEN_MIN:
    case IFN_COND_LEN_MAX:
    case IFN_COND_LEN_FMIN:
    case IFN_COND_LEN_FMAX:
    case IFN_COND_LEN_AND:
    case IFN_COND_LEN_IOR:
    case IFN_COND_LEN_XOR:
    case IFN_COND_LEN_SHL:
    case IFN_COND_LEN_SHR:
      return 3;

    case IFN_MASK_LOAD:
    case IFN_MASK_LEN_LOAD:
    case IFN_MASK_LOAD_LANES:
    case IFN_MASK_LEN_LOAD_LANES:
      return 3;

    case IFN_COND_FMA:
    case IFN_COND_FMS:
    case IFN_COND_FNMA:
    case IFN_COND_FNMS:
    case IFN_COND_LEN_FMA:
    case IFN_COND_LEN_FMS:
    case IFN_COND_LEN_FNMA:
    case IFN_COND_LEN_FNMS:
    case IFN_MASK_LEN_STRIDED_LOAD:
      return 4;

    case IFN_MASK_GATHER_LOAD:
    case IFN_MASK_LEN_GATHER_LOAD:
      return 5;

    default:
      return -1;
    }

  return -1;
}

/* If FN takes a vector mask argument, return the index of that argument,
   otherwise return -1.  */

int
internal_fn_mask_index (internal_fn fn)
{
  switch (fn)
    {
    case IFN_MASK_LOAD:
    case IFN_MASK_LOAD_LANES:
    case IFN_MASK_LEN_LOAD_LANES:
    case IFN_MASK_STORE:
    case IFN_MASK_STORE_LANES:
    case IFN_MASK_LEN_STORE_LANES:
    case IFN_MASK_LEN_LOAD:
    case IFN_MASK_LEN_STORE:
      return 2;

    case IFN_MASK_LEN_STRIDED_LOAD:
    case IFN_MASK_LEN_STRIDED_STORE:
      return 3;

    case IFN_MASK_GATHER_LOAD:
    case IFN_MASK_SCATTER_STORE:
    case IFN_MASK_LEN_GATHER_LOAD:
    case IFN_MASK_LEN_SCATTER_STORE:
      return 4;

    case IFN_VCOND_MASK:
    case IFN_VCOND_MASK_LEN:
      return 0;

    default:
      return (conditional_internal_fn_code (fn) != ERROR_MARK
	      || get_unconditional_internal_fn (fn) != IFN_LAST ? 0 : -1);
    }
}

/* If FN takes a value that should be stored to memory, return the index
   of that argument, otherwise return -1.  */

int
internal_fn_stored_value_index (internal_fn fn)
{
  switch (fn)
    {
    case IFN_MASK_LEN_STRIDED_STORE:
      return 2;

    case IFN_MASK_STORE:
    case IFN_MASK_STORE_LANES:
    case IFN_SCATTER_STORE:
    case IFN_MASK_SCATTER_STORE:
    case IFN_MASK_LEN_SCATTER_STORE:
      return 3;

    case IFN_LEN_STORE:
      return 4;

    case IFN_MASK_LEN_STORE:
    case IFN_MASK_LEN_STORE_LANES:
      return 5;

    default:
      return -1;
    }
}


/* Store all supported else values for the optab referred to by ICODE
   in ELSE_VALS.  The index of the else operand must be specified in
   ELSE_INDEX.  */

void
get_supported_else_vals (enum insn_code icode, unsigned else_index,
			 vec<int> &else_vals)
{
  const struct insn_data_d *data = &insn_data[icode];
  if ((char)else_index >= data->n_operands)
    return;

  machine_mode else_mode = data->operand[else_index].mode;

  else_vals.truncate (0);

  /* For now we only support else values of 0, -1, and "undefined".  */
  if (insn_operand_matches (icode, else_index, CONST0_RTX (else_mode)))
    else_vals.safe_push (MASK_LOAD_ELSE_ZERO);

  if (insn_operand_matches (icode, else_index, gen_rtx_SCRATCH (else_mode)))
    else_vals.safe_push (MASK_LOAD_ELSE_UNDEFINED);

  if (GET_MODE_CLASS (else_mode) == MODE_VECTOR_INT
      && insn_operand_matches (icode, else_index, CONSTM1_RTX (else_mode)))
    else_vals.safe_push (MASK_LOAD_ELSE_M1);
}

/* Return true if the else value ELSE_VAL (one of MASK_LOAD_ELSE_ZERO,
   MASK_LOAD_ELSE_M1, and MASK_LOAD_ELSE_UNDEFINED) is valid fo the optab
   referred to by ICODE.  The index of the else operand must be specified
   in ELSE_INDEX.  */

bool
supported_else_val_p (enum insn_code icode, unsigned else_index, int else_val)
{
  if (else_val != MASK_LOAD_ELSE_ZERO && else_val != MASK_LOAD_ELSE_M1
      && else_val != MASK_LOAD_ELSE_UNDEFINED)
    gcc_unreachable ();

  auto_vec<int> else_vals;
  get_supported_else_vals (icode, else_index, else_vals);
  return else_vals.contains (else_val);
}

/* Return true if the target supports gather load or scatter store function
   IFN.  For loads, VECTOR_TYPE is the vector type of the load result,
   while for stores it is the vector type of the stored data argument.
   MEMORY_ELEMENT_TYPE is the type of the memory elements being loaded
   or stored.  OFFSET_VECTOR_TYPE is the vector type that holds the
   offset from the shared base address of each loaded or stored element.
   SCALE is the amount by which these offsets should be multiplied
   *after* they have been extended to address width.
   If the target supports the gather load the supported else values
   will be added to the vector ELSVAL points to if it is nonzero.  */

bool
internal_gather_scatter_fn_supported_p (internal_fn ifn, tree vector_type,
					tree memory_element_type,
					tree offset_vector_type, int scale,
					vec<int> *elsvals)
{
  if (!tree_int_cst_equal (TYPE_SIZE (TREE_TYPE (vector_type)),
			   TYPE_SIZE (memory_element_type)))
    return false;
  if (maybe_ne (TYPE_VECTOR_SUBPARTS (vector_type),
		TYPE_VECTOR_SUBPARTS (offset_vector_type)))
    return false;
  optab optab = direct_internal_fn_optab (ifn);
  insn_code icode = convert_optab_handler (optab, TYPE_MODE (vector_type),
					   TYPE_MODE (offset_vector_type));
  int output_ops = internal_load_fn_p (ifn) ? 1 : 0;
  bool unsigned_p = TYPE_UNSIGNED (TREE_TYPE (offset_vector_type));
  bool ok = icode != CODE_FOR_nothing
    && insn_operand_matches (icode, 2 + output_ops, GEN_INT (unsigned_p))
    && insn_operand_matches (icode, 3 + output_ops, GEN_INT (scale));

  /* For gather the optab's operand indices do not match the IFN's because
     the latter does not have the extension operand (operand 3).  It is
     implicitly added during expansion so we use the IFN's else index + 1.
     */
  if (ok && elsvals)
    get_supported_else_vals
      (icode, internal_fn_else_index (IFN_MASK_GATHER_LOAD) + 1, *elsvals);

  return ok;
}

/* Return true if the target supports IFN_CHECK_{RAW,WAR}_PTRS function IFN
   for pointers of type TYPE when the accesses have LENGTH bytes and their
   common byte alignment is ALIGN.  */

bool
internal_check_ptrs_fn_supported_p (internal_fn ifn, tree type,
				    poly_uint64 length, unsigned int align)
{
  machine_mode mode = TYPE_MODE (type);
  optab optab = direct_internal_fn_optab (ifn);
  insn_code icode = direct_optab_handler (optab, mode);
  if (icode == CODE_FOR_nothing)
    return false;
  rtx length_rtx = immed_wide_int_const (length, mode);
  return (insn_operand_matches (icode, 3, length_rtx)
	  && insn_operand_matches (icode, 4, GEN_INT (align)));
}

/* Return the supported bias for IFN which is either IFN_{LEN_,MASK_LEN_,}LOAD
   or IFN_{LEN_,MASK_LEN_,}STORE.  For now we only support the biases of 0 and
   -1 (in case 0 is not an allowable length for {len_,mask_len_}load or
   {len_,mask_len_}store). If none of the biases match what the backend
   provides, return VECT_PARTIAL_BIAS_UNSUPPORTED.  */

signed char
internal_len_load_store_bias (internal_fn ifn, machine_mode mode)
{
  optab optab = direct_internal_fn_optab (ifn);
  insn_code icode = direct_optab_handler (optab, mode);
  int bias_no = 3;

  if (icode == CODE_FOR_nothing)
    {
      machine_mode mask_mode;
      if (!targetm.vectorize.get_mask_mode (mode).exists (&mask_mode))
	return VECT_PARTIAL_BIAS_UNSUPPORTED;
      if (ifn == IFN_LEN_LOAD)
	{
	  /* Try MASK_LEN_LOAD.  */
	  optab = direct_internal_fn_optab (IFN_MASK_LEN_LOAD);
	}
      else
	{
	  /* Try MASK_LEN_STORE.  */
	  optab = direct_internal_fn_optab (IFN_MASK_LEN_STORE);
	}
      icode = convert_optab_handler (optab, mode, mask_mode);
      bias_no = 4;
    }

  if (icode != CODE_FOR_nothing)
    {
      /* For now we only support biases of 0 or -1.  Try both of them.  */
      if (insn_operand_matches (icode, bias_no, GEN_INT (0)))
	return 0;
      if (insn_operand_matches (icode, bias_no, GEN_INT (-1)))
	return -1;
    }

  return VECT_PARTIAL_BIAS_UNSUPPORTED;
}

/* Expand STMT as though it were a call to internal function FN.  */

void
expand_internal_call (internal_fn fn, gcall *stmt)
{
  internal_fn_expanders[fn] (fn, stmt);
}

/* Expand STMT, which is a call to internal function FN.  */

void
expand_internal_call (gcall *stmt)
{
  expand_internal_call (gimple_call_internal_fn (stmt), stmt);
}

/* If TYPE is a vector type, return true if IFN is a direct internal
   function that is supported for that type.  If TYPE is a scalar type,
   return true if IFN is a direct internal function that is supported for
   the target's preferred vector version of TYPE.  */

bool
vectorized_internal_fn_supported_p (internal_fn ifn, tree type)
{
  if (VECTOR_MODE_P (TYPE_MODE (type)))
    return direct_internal_fn_supported_p (ifn, type, OPTIMIZE_FOR_SPEED);

  scalar_mode smode;
  if (VECTOR_TYPE_P (type)
      || !is_a <scalar_mode> (TYPE_MODE (type), &smode))
    return false;

  machine_mode vmode = targetm.vectorize.preferred_simd_mode (smode);
  if (VECTOR_MODE_P (vmode))
    {
      tree vectype = build_vector_type_for_mode (type, vmode);
      if (direct_internal_fn_supported_p (ifn, vectype, OPTIMIZE_FOR_SPEED))
	return true;
    }

  auto_vector_modes vector_modes;
  targetm.vectorize.autovectorize_vector_modes (&vector_modes, true);
  for (machine_mode base_mode : vector_modes)
    if (related_vector_mode (base_mode, smode).exists (&vmode))
      {
	tree vectype = build_vector_type_for_mode (type, vmode);
	if (direct_internal_fn_supported_p (ifn, vectype, OPTIMIZE_FOR_SPEED))
	  return true;
      }

  return false;
}

void
expand_SHUFFLEVECTOR (internal_fn, gcall *)
{
  gcc_unreachable ();
}

void
expand_PHI (internal_fn, gcall *)
{
  gcc_unreachable ();
}

void
expand_SPACESHIP (internal_fn, gcall *stmt)
{
  tree lhs = gimple_call_lhs (stmt);
  tree rhs1 = gimple_call_arg (stmt, 0);
  tree rhs2 = gimple_call_arg (stmt, 1);
  tree rhs3 = gimple_call_arg (stmt, 2);
  tree type = TREE_TYPE (rhs1);

  do_pending_stack_adjust ();

  rtx target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  rtx op1 = expand_normal (rhs1);
  rtx op2 = expand_normal (rhs2);
  rtx op3 = expand_normal (rhs3);

  class expand_operand ops[4];
  create_call_lhs_operand (&ops[0], target, TYPE_MODE (TREE_TYPE (lhs)));
  create_input_operand (&ops[1], op1, TYPE_MODE (type));
  create_input_operand (&ops[2], op2, TYPE_MODE (type));
  create_input_operand (&ops[3], op3, TYPE_MODE (TREE_TYPE (rhs3)));
  insn_code icode = optab_handler (spaceship_optab, TYPE_MODE (type));
  expand_insn (icode, 4, ops);
  assign_call_lhs (lhs, target, &ops[0]);
}

void
expand_ASSUME (internal_fn, gcall *)
{
}

void
expand_MASK_CALL (internal_fn, gcall *)
{
  /* This IFN should only exist between ifcvt and vect passes.  */
  gcc_unreachable ();
}

void
expand_MULBITINT (internal_fn, gcall *stmt)
{
  rtx_mode_t args[6];
  for (int i = 0; i < 6; i++)
    args[i] = rtx_mode_t (expand_normal (gimple_call_arg (stmt, i)),
			  (i & 1) ? SImode : ptr_mode);
  rtx fun = init_one_libfunc ("__mulbitint3");
  emit_library_call_value_1 (0, fun, NULL_RTX, LCT_NORMAL, VOIDmode, 6, args);
}

void
expand_DIVMODBITINT (internal_fn, gcall *stmt)
{
  rtx_mode_t args[8];
  for (int i = 0; i < 8; i++)
    args[i] = rtx_mode_t (expand_normal (gimple_call_arg (stmt, i)),
			  (i & 1) ? SImode : ptr_mode);
  rtx fun = init_one_libfunc ("__divmodbitint4");
  emit_library_call_value_1 (0, fun, NULL_RTX, LCT_NORMAL, VOIDmode, 8, args);
}

void
expand_FLOATTOBITINT (internal_fn, gcall *stmt)
{
  machine_mode mode = TYPE_MODE (TREE_TYPE (gimple_call_arg (stmt, 2)));
  rtx arg0 = expand_normal (gimple_call_arg (stmt, 0));
  rtx arg1 = expand_normal (gimple_call_arg (stmt, 1));
  rtx arg2 = expand_normal (gimple_call_arg (stmt, 2));
  const char *mname = GET_MODE_NAME (mode);
  unsigned mname_len = strlen (mname);
  int len = 12 + mname_len;
  if (DECIMAL_FLOAT_MODE_P (mode))
    len += 4;
  char *libfunc_name = XALLOCAVEC (char, len);
  char *p = libfunc_name;
  const char *q;
  if (DECIMAL_FLOAT_MODE_P (mode))
    {
#if ENABLE_DECIMAL_BID_FORMAT
      memcpy (p, "__bid_fix", 9);
#else
      memcpy (p, "__dpd_fix", 9);
#endif
      p += 9;
    }
  else
    {
      memcpy (p, "__fix", 5);
      p += 5;
    }
  for (q = mname; *q; q++)
    *p++ = TOLOWER (*q);
  memcpy (p, "bitint", 7);
  rtx fun = init_one_libfunc (libfunc_name);
  emit_library_call (fun, LCT_NORMAL, VOIDmode, arg0, ptr_mode, arg1,
		     SImode, arg2, mode);
}

void
expand_BITINTTOFLOAT (internal_fn, gcall *stmt)
{
  tree lhs = gimple_call_lhs (stmt);
  if (!lhs)
    return;
  machine_mode mode = TYPE_MODE (TREE_TYPE (lhs));
  rtx arg0 = expand_normal (gimple_call_arg (stmt, 0));
  rtx arg1 = expand_normal (gimple_call_arg (stmt, 1));
  const char *mname = GET_MODE_NAME (mode);
  unsigned mname_len = strlen (mname);
  int len = 14 + mname_len;
  if (DECIMAL_FLOAT_MODE_P (mode))
    len += 4;
  char *libfunc_name = XALLOCAVEC (char, len);
  char *p = libfunc_name;
  const char *q;
  if (DECIMAL_FLOAT_MODE_P (mode))
    {
#if ENABLE_DECIMAL_BID_FORMAT
      memcpy (p, "__bid_floatbitint", 17);
#else
      memcpy (p, "__dpd_floatbitint", 17);
#endif
      p += 17;
    }
  else
    {
      memcpy (p, "__floatbitint", 13);
      p += 13;
    }
  for (q = mname; *q; q++)
    *p++ = TOLOWER (*q);
  *p = '\0';
  rtx fun = init_one_libfunc (libfunc_name);
  rtx target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  rtx val = emit_library_call_value (fun, target, LCT_PURE, mode,
				     arg0, ptr_mode, arg1, SImode);
  if (val != target)
    emit_move_insn (target, val);
}

static bool
expand_bitquery (internal_fn fn, gcall *stmt)
{
  tree lhs = gimple_call_lhs (stmt);
  if (lhs == NULL_TREE)
    return false;
  tree arg = gimple_call_arg (stmt, 0);
  if (TREE_CODE (arg) == INTEGER_CST)
    {
      tree ret = fold_const_call (as_combined_fn (fn), TREE_TYPE (arg), arg);
      gcc_checking_assert (ret && TREE_CODE (ret) == INTEGER_CST);
      expand_assignment (lhs, ret, false);
      return false;
    }
  return true;
}

void
expand_CLRSB (internal_fn fn, gcall *stmt)
{
  if (expand_bitquery (fn, stmt))
    expand_unary_optab_fn (fn, stmt, clrsb_optab);
}

void
expand_CLZ (internal_fn fn, gcall *stmt)
{
  if (expand_bitquery (fn, stmt))
    expand_unary_optab_fn (fn, stmt, clz_optab);
}

void
expand_CTZ (internal_fn fn, gcall *stmt)
{
  if (expand_bitquery (fn, stmt))
    expand_unary_optab_fn (fn, stmt, ctz_optab);
}

void
expand_FFS (internal_fn fn, gcall *stmt)
{
  if (expand_bitquery (fn, stmt))
    expand_unary_optab_fn (fn, stmt, ffs_optab);
}

void
expand_PARITY (internal_fn fn, gcall *stmt)
{
  if (expand_bitquery (fn, stmt))
    expand_unary_optab_fn (fn, stmt, parity_optab);
}

void
expand_POPCOUNT (internal_fn fn, gcall *stmt)
{
  if (!expand_bitquery (fn, stmt))
    return;
  if (gimple_call_num_args (stmt) == 1)
    {
      expand_unary_optab_fn (fn, stmt, popcount_optab);
      return;
    }
  /* If .POPCOUNT call has 2 arguments, match_single_bit_test marked it
     because the result is only used in an equality comparison against 1.
     Use rtx costs in that case to determine if .POPCOUNT (arg) == 1
     or (arg ^ (arg - 1)) > arg - 1 is cheaper.
     If .POPCOUNT second argument is 0, we additionally know that arg
     is non-zero, so use arg & (arg - 1) == 0 instead.
     If .POPCOUNT second argument is -1, the comparison was either `<= 1`
     or `> 1`.  */
  bool speed_p = optimize_insn_for_speed_p ();
  tree lhs = gimple_call_lhs (stmt);
  tree arg = gimple_call_arg (stmt, 0);
  bool nonzero_arg = integer_zerop (gimple_call_arg (stmt, 1));
  bool was_le = integer_minus_onep (gimple_call_arg (stmt, 1));
  if (was_le)
    nonzero_arg = true;
  tree type = TREE_TYPE (arg);
  machine_mode mode = TYPE_MODE (type);
  machine_mode lhsmode = TYPE_MODE (TREE_TYPE (lhs));
  do_pending_stack_adjust ();
  start_sequence ();
  expand_unary_optab_fn (fn, stmt, popcount_optab);
  rtx_insn *popcount_insns = get_insns ();
  end_sequence ();
  start_sequence ();
  rtx plhs = expand_normal (lhs);
  rtx pcmp = emit_store_flag (NULL_RTX, EQ, plhs, const1_rtx, lhsmode, 0, 0);
  if (pcmp == NULL_RTX)
    {
    fail:
      end_sequence ();
      emit_insn (popcount_insns);
      return;
    }
  rtx_insn *popcount_cmp_insns = get_insns ();
  end_sequence ();
  start_sequence ();
  rtx op0 = expand_normal (arg);
  rtx argm1 = expand_simple_binop (mode, PLUS, op0, constm1_rtx, NULL_RTX,
				   1, OPTAB_WIDEN);
  if (argm1 == NULL_RTX)
    goto fail;
  rtx argxorargm1 = expand_simple_binop (mode, nonzero_arg ? AND : XOR, op0,
					 argm1, NULL_RTX, 1, OPTAB_WIDEN);
  if (argxorargm1 == NULL_RTX)
    goto fail;
  rtx cmp;
  if (nonzero_arg)
    cmp = emit_store_flag (NULL_RTX, EQ, argxorargm1, const0_rtx, mode, 1, 1);
  else
    cmp = emit_store_flag (NULL_RTX, GTU, argxorargm1, argm1, mode, 1, 1);
  if (cmp == NULL_RTX)
    goto fail;
  rtx_insn *cmp_insns = get_insns ();
  end_sequence ();
  unsigned popcount_cost = (seq_cost (popcount_insns, speed_p)
			    + seq_cost (popcount_cmp_insns, speed_p));
  unsigned cmp_cost = seq_cost (cmp_insns, speed_p);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf(dump_file, "popcount == 1: popcount cost: %u; cmp cost: %u\n",
	    popcount_cost, cmp_cost);

  if (popcount_cost <= cmp_cost)
    emit_insn (popcount_insns);
  else
    {
      start_sequence ();
      emit_insn (cmp_insns);
      plhs = expand_normal (lhs);
      if (GET_MODE (cmp) != GET_MODE (plhs))
	cmp = convert_to_mode (GET_MODE (plhs), cmp, 1);
      /* For `<= 1`, we need to produce `2 - cmp` or `cmp ? 1 : 2` as that
	 then gets compared against 1 and we need the false case to be 2.  */
      if (was_le)
	{
	  cmp = expand_simple_binop (GET_MODE (cmp), MINUS, const2_rtx,
				     cmp, NULL_RTX, 1, OPTAB_WIDEN);
	  if (!cmp)
	    goto fail;
	}
      emit_move_insn (plhs, cmp);
      rtx_insn *all_insns = get_insns ();
      end_sequence ();
      emit_insn (all_insns);
    }
}
