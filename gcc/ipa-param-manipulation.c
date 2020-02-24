/* Manipulation of formal and actual parameters of functions and function
   calls.
   Copyright (C) 2017-2020 Free Software Foundation, Inc.

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
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "cgraph.h"
#include "fold-const.h"
#include "tree-eh.h"
#include "stor-layout.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "tree-cfg.h"
#include "tree-dfa.h"
#include "ipa-param-manipulation.h"
#include "print-tree.h"
#include "gimple-pretty-print.h"
#include "builtins.h"
#include "tree-ssa.h"
#include "tree-inline.h"


/* Actual prefixes of different newly synthetized parameters.  Keep in sync
   with IPA_PARAM_PREFIX_* defines.  */

static const char *ipa_param_prefixes[IPA_PARAM_PREFIX_COUNT]
  = {"SYNTH",
     "ISRA",
     "simd",
     "mask"};

/* Names of parameters for dumping.  Keep in sync with enum ipa_parm_op.  */

static const char *ipa_param_op_names[IPA_PARAM_PREFIX_COUNT]
  = {"IPA_PARAM_OP_UNDEFINED",
     "IPA_PARAM_OP_COPY",
     "IPA_PARAM_OP_NEW",
     "IPA_PARAM_OP_SPLIT"};

/* Fill an empty vector ARGS with PARM_DECLs representing formal parameters of
   FNDECL.  The function should not be called during LTO WPA phase except for
   thunks (or functions with bodies streamed in). */

void
push_function_arg_decls (vec<tree> *args, tree fndecl)
{
  int count;
  tree parm;

  /* Safety check that we do not attempt to use the function in WPA, except
     when the function is a thunk and then we have DECL_ARGUMENTS or when we
     have already explicitely loaded its body.  */
  gcc_assert (!flag_wpa
	      || DECL_ARGUMENTS (fndecl)
	      || gimple_has_body_p (fndecl));
  count = 0;
  for (parm = DECL_ARGUMENTS (fndecl); parm; parm = DECL_CHAIN (parm))
    count++;

  args->reserve_exact (count);
  for (parm = DECL_ARGUMENTS (fndecl); parm; parm = DECL_CHAIN (parm))
    args->quick_push (parm);
}

/* Fill an empty vector TYPES with trees representing formal parameters of
   function type FNTYPE.  */

void
push_function_arg_types (vec<tree> *types, tree fntype)
{
  int count = 0;
  tree t;

  for (t = TYPE_ARG_TYPES (fntype); t; t = TREE_CHAIN (t))
    count++;

  types->reserve_exact (count);
  for (t = TYPE_ARG_TYPES (fntype); t; t = TREE_CHAIN (t))
    types->quick_push (TREE_VALUE (t));
}

/* Dump the adjustments in the vector ADJUSTMENTS to dump_file in a human
   friendly way, assuming they are meant to be applied to FNDECL.  */

void
ipa_dump_adjusted_parameters (FILE *f,
			      vec<ipa_adjusted_param, va_gc> *adj_params)
{
  unsigned i, len = vec_safe_length (adj_params);
  bool first = true;

  fprintf (f, "    IPA adjusted parameters: ");
  for (i = 0; i < len; i++)
    {
      struct ipa_adjusted_param *apm;
      apm = &(*adj_params)[i];

      if (!first)
	fprintf (f, "                             ");
      else
	first = false;

      fprintf (f, "%i. %s %s", i, ipa_param_op_names[apm->op],
	       apm->prev_clone_adjustment ? "prev_clone_adjustment " : "");
      switch (apm->op)
	{
	case IPA_PARAM_OP_UNDEFINED:
	  break;

	case IPA_PARAM_OP_COPY:
	  fprintf (f, ", base_index: %u", apm->base_index);
	  fprintf (f, ", prev_clone_index: %u", apm->prev_clone_index);
	  break;

	case IPA_PARAM_OP_SPLIT:
	  fprintf (f, ", offset: %u", apm->unit_offset);
	  /* fall-through */
	case IPA_PARAM_OP_NEW:
	  fprintf (f, ", base_index: %u", apm->base_index);
	  fprintf (f, ", prev_clone_index: %u", apm->prev_clone_index);
	  print_node_brief (f, ", type: ", apm->type, 0);
	  print_node_brief (f, ", alias type: ", apm->alias_ptr_type, 0);
	  fprintf (f, " prefix: %s",
		   ipa_param_prefixes[apm->param_prefix_index]);
	  if (apm->reverse)
	    fprintf (f, ", reverse-sso");
	  break;
	}
      fprintf (f, "\n");
    }
}

/* Fill NEW_TYPES with types of a function after its current OTYPES have been
   modified as described in ADJ_PARAMS.  When USE_PREV_INDICES is true, use
   prev_clone_index from ADJ_PARAMS as opposed to base_index when the parameter
   is false.  */

static void
fill_vector_of_new_param_types (vec<tree> *new_types, vec<tree> *otypes,
				vec<ipa_adjusted_param, va_gc> *adj_params,
				bool use_prev_indices)
{
  unsigned adj_len = vec_safe_length (adj_params);
  new_types->reserve_exact (adj_len);
  for (unsigned i = 0; i < adj_len ; i++)
    {
      ipa_adjusted_param *apm = &(*adj_params)[i];
      if (apm->op == IPA_PARAM_OP_COPY)
	{
	  unsigned index
	    = use_prev_indices ? apm->prev_clone_index : apm->base_index;
	  /* The following needs to be handled gracefully because of type
	     mismatches.  This happens with LTO but apparently also in Fortran
	     with -fcoarray=lib -O2 -lcaf_single -latomic.  */
	  if (index >= otypes->length ())
	    continue;
	  new_types->quick_push ((*otypes)[index]);
	}
      else if (apm->op == IPA_PARAM_OP_NEW
	       || apm->op == IPA_PARAM_OP_SPLIT)
	{
	  tree ntype = apm->type;
	  if (is_gimple_reg_type (ntype)
	      && TYPE_MODE (ntype) != BLKmode)
	    {
	      unsigned malign = GET_MODE_ALIGNMENT (TYPE_MODE (ntype));
	      if (TYPE_ALIGN (ntype) != malign)
		ntype = build_aligned_type (ntype, malign);
	    }
	  new_types->quick_push (ntype);
	}
      else
	gcc_unreachable ();
    }
}

/* Build and return a function type just like ORIG_TYPE but with parameter
   types given in NEW_PARAM_TYPES - which can be NULL if, but only if,
   ORIG_TYPE itself has NULL TREE_ARG_TYPEs.  If METHOD2FUNC is true, also make
   it a FUNCTION_TYPE instead of FUNCTION_TYPE.  */

static tree
build_adjusted_function_type (tree orig_type, vec<tree> *new_param_types,
			      bool method2func, bool skip_return)
{
  tree new_arg_types = NULL;
  if (TYPE_ARG_TYPES (orig_type))
    {
      gcc_checking_assert (new_param_types);
      bool last_parm_void = (TREE_VALUE (tree_last (TYPE_ARG_TYPES (orig_type)))
			     == void_type_node);
      unsigned len = new_param_types->length ();
      for (unsigned i = 0; i < len; i++)
	new_arg_types = tree_cons (NULL_TREE, (*new_param_types)[i],
				   new_arg_types);

      tree new_reversed = nreverse (new_arg_types);
      if (last_parm_void)
	{
	  if (new_reversed)
	    TREE_CHAIN (new_arg_types) = void_list_node;
	  else
	    new_reversed = void_list_node;
	}
      new_arg_types = new_reversed;
    }

  /* Use build_distinct_type_copy to preserve as much as possible from original
     type (debug info, attribute lists etc.).  The one exception is
     METHOD_TYPEs which must have THIS argument and when we are asked to remove
     it, we need to build new FUNCTION_TYPE instead.  */
  tree new_type = NULL;
  if (method2func)
    {
      tree ret_type;
      if (skip_return)
	ret_type = void_type_node;
      else
	ret_type = TREE_TYPE (orig_type);

      new_type
	= build_distinct_type_copy (build_function_type (ret_type,
							 new_arg_types));
      TYPE_CONTEXT (new_type) = TYPE_CONTEXT (orig_type);
    }
  else
    {
      new_type = build_distinct_type_copy (orig_type);
      TYPE_ARG_TYPES (new_type) = new_arg_types;
      if (skip_return)
	TREE_TYPE (new_type) = void_type_node;
    }

  return new_type;
}

/* Return the maximum index in any IPA_PARAM_OP_COPY adjustment or -1 if there
   is none.  */

int
ipa_param_adjustments::get_max_base_index ()
{
  unsigned adj_len = vec_safe_length (m_adj_params);
  int max_index = -1;
  for (unsigned i = 0; i < adj_len ; i++)
    {
      ipa_adjusted_param *apm = &(*m_adj_params)[i];
      if (apm->op == IPA_PARAM_OP_COPY
	  && max_index < apm->base_index)
	max_index = apm->base_index;
    }
  return max_index;
}


/* Fill SURVIVING_PARAMS with an array of bools where each one says whether a
   parameter that originally was at that position still survives in the given
   clone or is removed/replaced.  If the final array is smaller than an index
   of an original parameter, that parameter also did not survive.  That a
   parameter survives does not mean it has the same index as before.  */

void
ipa_param_adjustments::get_surviving_params (vec<bool> *surviving_params)
{
  unsigned adj_len = vec_safe_length (m_adj_params);
  int max_index = get_max_base_index ();

  if (max_index < 0)
    return;
  surviving_params->reserve_exact (max_index + 1);
  surviving_params->quick_grow_cleared (max_index + 1);
  for (unsigned i = 0; i < adj_len ; i++)
    {
      ipa_adjusted_param *apm = &(*m_adj_params)[i];
      if (apm->op == IPA_PARAM_OP_COPY)
	(*surviving_params)[apm->base_index] = true;
    }
}

/* Fill NEW_INDICES with new indices of each surviving parameter or -1 for
   those which do not survive.  Any parameter outside of lenght of the vector
   does not survive.  There is currently no support for a parameter to be
   copied to two distinct new parameters.  */

void
ipa_param_adjustments::get_updated_indices (vec<int> *new_indices)
{
  unsigned adj_len = vec_safe_length (m_adj_params);
  int max_index = get_max_base_index ();

  if (max_index < 0)
    return;
  unsigned res_len = max_index + 1;
  new_indices->reserve_exact (res_len);
  for (unsigned i = 0; i < res_len ; i++)
    new_indices->quick_push (-1);
  for (unsigned i = 0; i < adj_len ; i++)
    {
      ipa_adjusted_param *apm = &(*m_adj_params)[i];
      if (apm->op == IPA_PARAM_OP_COPY)
	(*new_indices)[apm->base_index] = i;
    }
}

/* Return the original index for the given new parameter index.  Return a
   negative number if not available.  */

int
ipa_param_adjustments::get_original_index (int newidx)
{
  const ipa_adjusted_param *adj = &(*m_adj_params)[newidx];
  if (adj->op != IPA_PARAM_OP_COPY)
    return -1;
  return adj->base_index;
}

/* Return true if the first parameter (assuming there was one) survives the
   transformation intact and remains the first one.  */

bool
ipa_param_adjustments::first_param_intact_p ()
{
  return (!vec_safe_is_empty (m_adj_params)
	  && (*m_adj_params)[0].op == IPA_PARAM_OP_COPY
	  && (*m_adj_params)[0].base_index == 0);
}

/* Return true if we have to change what has formerly been a method into a
   function.  */

bool
ipa_param_adjustments::method2func_p (tree orig_type)
{
  return ((TREE_CODE (orig_type) == METHOD_TYPE) && !first_param_intact_p ());
}

/* Given function type OLD_TYPE, return a new type derived from it after
   performing all atored modifications.  TYPE_ORIGINAL_P should be true when
   OLD_TYPE refers to the type before any IPA transformations, as opposed to a
   type that can be an intermediate one in between various IPA
   transformations.  */

tree
ipa_param_adjustments::build_new_function_type (tree old_type,
						bool type_original_p)
{
  auto_vec<tree,16> new_param_types, *new_param_types_p;
  if (prototype_p (old_type))
    {
      auto_vec<tree, 16> otypes;
      push_function_arg_types (&otypes, old_type);
      fill_vector_of_new_param_types (&new_param_types, &otypes, m_adj_params,
				      !type_original_p);
      new_param_types_p = &new_param_types;
    }
  else
    new_param_types_p = NULL;

  return build_adjusted_function_type (old_type, new_param_types_p,
				       method2func_p (old_type), m_skip_return);
}

/* Build variant of function decl ORIG_DECL which has no return value if
   M_SKIP_RETURN is true and, if ORIG_DECL's types or parameters is known, has
   this type adjusted as indicated in M_ADJ_PARAMS. Arguments from
   DECL_ARGUMENTS list are not processed now, since they are linked by
   TREE_CHAIN directly and not accessible in LTO during WPA.  The caller is
   responsible for eliminating them when clones are properly materialized.  */

tree
ipa_param_adjustments::adjust_decl (tree orig_decl)
{
  tree new_decl = copy_node (orig_decl);
  tree orig_type = TREE_TYPE (orig_decl);
  if (prototype_p (orig_type)
      || (m_skip_return && !VOID_TYPE_P (TREE_TYPE (orig_type))))
    {
      tree new_type = build_new_function_type (orig_type, false);
      TREE_TYPE (new_decl) = new_type;
    }
  if (method2func_p (orig_type))
    DECL_VINDEX (new_decl) = NULL_TREE;

  /* When signature changes, we need to clear builtin info.  */
  if (fndecl_built_in_p (new_decl))
    set_decl_built_in_function (new_decl, NOT_BUILT_IN, 0);

  DECL_VIRTUAL_P (new_decl) = 0;
  DECL_LANG_SPECIFIC (new_decl) = NULL;

  /* Drop MALLOC attribute for a void function.  */
  if (m_skip_return)
    DECL_IS_MALLOC (new_decl) = 0;

  return new_decl;
}

/* Wrapper around get_base_ref_and_offset for cases interesting for IPA-SRA
   transformations.  Return true if EXPR has an interesting form and fill in
   *BASE_P and *UNIT_OFFSET_P with the appropriate info.  */

static bool
isra_get_ref_base_and_offset (tree expr, tree *base_p, unsigned *unit_offset_p)
{
  HOST_WIDE_INT offset, size;
  bool reverse;
  tree base
    = get_ref_base_and_extent_hwi (expr, &offset, &size, &reverse);
  if (!base || size < 0)
    return false;

  if ((offset % BITS_PER_UNIT) != 0)
    return false;

  if (TREE_CODE (base) == MEM_REF)
    {
      poly_int64 plmoff = mem_ref_offset (base).force_shwi ();
      HOST_WIDE_INT moff;
      bool is_cst = plmoff.is_constant (&moff);
      if (!is_cst)
	return false;
      offset += moff * BITS_PER_UNIT;
      base = TREE_OPERAND (base, 0);
    }

  if (offset < 0 || (offset / BITS_PER_UNIT) > UINT_MAX)
    return false;

  *base_p = base;
  *unit_offset_p = offset / BITS_PER_UNIT;
  return true;
}

/* Return true if EXPR describes a transitive split (i.e. one that happened for
   both the caller and the callee) as recorded in PERFORMED_SPLITS.  In that
   case, store index of the respective record in PERFORMED_SPLITS into
   *SM_IDX_P and the unit offset from all handled components in EXPR into
   *UNIT_OFFSET_P.  */

static bool
transitive_split_p (vec<ipa_param_performed_split, va_gc> *performed_splits,
		    tree expr, unsigned *sm_idx_p, unsigned *unit_offset_p)
{
  tree base;
  if (!isra_get_ref_base_and_offset (expr, &base, unit_offset_p))
    return false;

  if (TREE_CODE (base) == SSA_NAME)
    {
      base = SSA_NAME_VAR (base);
      if (!base)
	return false;
    }

  unsigned len = vec_safe_length (performed_splits);
  for (unsigned i = 0 ; i < len; i++)
    {
      ipa_param_performed_split *sm = &(*performed_splits)[i];
      if (sm->dummy_decl == base)
	{
	  *sm_idx_p = i;
	  return true;
	}
    }
  return false;
}

/* Structure to hold declarations representing transitive IPA-SRA splits.  In
   essence, if we need to pass UNIT_OFFSET of a parameter which originally has
   number BASE_INDEX, we should pass down REPL.  */

struct transitive_split_map
{
  tree repl;
  unsigned base_index;
  unsigned unit_offset;
};

/* If call STMT contains any parameters representing transitive splits as
   described by PERFORMED_SPLITS, return the number of extra parameters that
   were addded during clone materialization and fill in INDEX_MAP with adjusted
   indices of corresponding original parameters and TRANS_MAP with description
   of all transitive replacement descriptions.  Otherwise return zero. */

static unsigned
init_transitive_splits (vec<ipa_param_performed_split, va_gc> *performed_splits,
			gcall *stmt, vec <unsigned> *index_map,
			auto_vec <transitive_split_map> *trans_map)
{
  unsigned phony_arguments = 0;
  unsigned stmt_idx = 0, base_index = 0;
  unsigned nargs = gimple_call_num_args (stmt);
  while (stmt_idx < nargs)
    {
      unsigned unit_offset_delta;
      tree base_arg = gimple_call_arg (stmt, stmt_idx);

      if (phony_arguments > 0)
	index_map->safe_push (stmt_idx);

      unsigned sm_idx;
      stmt_idx++;
      if (transitive_split_p (performed_splits, base_arg, &sm_idx,
			      &unit_offset_delta))
	{
	  if (phony_arguments == 0)
	    /* We have optimistically avoided constructing index_map do far but
	       now it is clear it will be necessary, so let's create the easy
	       bit we skipped until now.  */
	    for (unsigned k = 0; k < stmt_idx; k++)
	      index_map->safe_push (k);

	  tree dummy = (*performed_splits)[sm_idx].dummy_decl;
	  for (unsigned j = sm_idx; j < performed_splits->length (); j++)
	    {
	      ipa_param_performed_split *caller_split
		= &(*performed_splits)[j];
	      if (caller_split->dummy_decl != dummy)
		break;

	      tree arg = gimple_call_arg (stmt, stmt_idx);
	      struct transitive_split_map tsm;
	      tsm.repl = arg;
	      tsm.base_index = base_index;
	      if (caller_split->unit_offset >= unit_offset_delta)
		{
		  tsm.unit_offset
		    = (caller_split->unit_offset - unit_offset_delta);
		  trans_map->safe_push (tsm);
		}

	      phony_arguments++;
	      stmt_idx++;
	    }
	}
      base_index++;
    }
  return phony_arguments;
}

/* Modify actual arguments of a function call in statement STMT, assuming it
   calls CALLEE_DECL.  CALLER_ADJ must be the description of parameter
   adjustments of the caller or NULL if there are none.  Return the new
   statement that replaced the old one.  When invoked, cfun and
   current_function_decl have to be set to the caller.  */

gcall *
ipa_param_adjustments::modify_call (gcall *stmt,
				    vec<ipa_param_performed_split,
				        va_gc> *performed_splits,
				    tree callee_decl, bool update_references)
{
  unsigned len = vec_safe_length (m_adj_params);
  auto_vec<tree, 16> vargs (len);
  tree old_decl = gimple_call_fndecl (stmt);
  unsigned old_nargs = gimple_call_num_args (stmt);
  auto_vec<bool, 16> kept (old_nargs);
  kept.quick_grow_cleared (old_nargs);

  auto_vec <unsigned, 16> index_map;
  auto_vec <transitive_split_map> trans_map;
  bool transitive_remapping = false;

  if (performed_splits)
    {
      unsigned removed = init_transitive_splits (performed_splits,
						 stmt, &index_map, &trans_map);
      if (removed > 0)
	{
	  transitive_remapping = true;
	  old_nargs -= removed;
	}
    }

  cgraph_node *current_node = cgraph_node::get (current_function_decl);
  if (update_references)
    current_node->remove_stmt_references (stmt);

  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  gimple_stmt_iterator prev_gsi = gsi;
  gsi_prev (&prev_gsi);
  for (unsigned i = 0; i < len; i++)
    {
      ipa_adjusted_param *apm = &(*m_adj_params)[i];
      if (apm->op == IPA_PARAM_OP_COPY)
	{
	  unsigned index = apm->base_index;
	  if (index >= old_nargs)
	    /* Can happen if the original call has argument mismatch,
	       ignore.  */
	    continue;
	  if (transitive_remapping)
	    index = index_map[apm->base_index];

	  tree arg = gimple_call_arg (stmt, index);

	  vargs.quick_push (arg);
	  kept[index] = true;
	  continue;
	}

      /* At the moment the only user of IPA_PARAM_OP_NEW modifies calls itself.
	 If we ever want to support it during WPA IPA stage, we'll need a
	 mechanism to call into the IPA passes that introduced them.  Currently
	 we simply mandate that IPA infrastructure understands all argument
	 modifications.  Remember, edge redirection/modification is done only
	 once, not in steps for each pass modifying the callee like clone
	 materialization.  */
      gcc_assert (apm->op == IPA_PARAM_OP_SPLIT);

      /* We have to handle transitive changes differently using the maps we
	 have created before.  So look into them first.  */
      tree repl = NULL_TREE;
      for (unsigned j = 0; j < trans_map.length (); j++)
	if (trans_map[j].base_index == apm->base_index
	    && trans_map[j].unit_offset == apm->unit_offset)
	  {
	    repl = trans_map[j].repl;
	    break;
	  }
      if (repl)
	{
	  vargs.quick_push (repl);
	  continue;
	}

      unsigned index = apm->base_index;
      if (index >= old_nargs)
	/* Can happen if the original call has argument mismatch, ignore.  */
	continue;
      if (transitive_remapping)
	index = index_map[apm->base_index];
      tree base = gimple_call_arg (stmt, index);

      /* We create a new parameter out of the value of the old one, we can
	 do the following kind of transformations:

	 - A scalar passed by reference, potentially as a part of a larger
	 aggregate, is converted to a scalar passed by value.

	 - A part of an aggregate is passed instead of the whole aggregate.  */

      location_t loc = gimple_location (stmt);
      tree off;
      bool deref_base = false;
      unsigned int deref_align = 0;
      if (TREE_CODE (base) != ADDR_EXPR
	  && is_gimple_reg_type (TREE_TYPE (base)))
	{
	  /* Detect type mismatches in calls in invalid programs and make a
	     poor attempt to gracefully convert them so that we don't ICE.  */
	  if (!POINTER_TYPE_P (TREE_TYPE (base)))
	    base = force_value_to_type (ptr_type_node, base);

	  off = build_int_cst (apm->alias_ptr_type, apm->unit_offset);
	}
      else
	{
	  bool addrof;
	  if (TREE_CODE (base) == ADDR_EXPR)
	    {
	      base = TREE_OPERAND (base, 0);
	      addrof = true;
	    }
	  else
	    addrof = false;

	  tree prev_base = base;
	  poly_int64 base_offset;
	  base = get_addr_base_and_unit_offset (base, &base_offset);

	  /* Aggregate arguments can have non-invariant addresses.  */
	  if (!base)
	    {
	      base = build_fold_addr_expr (prev_base);
	      off = build_int_cst (apm->alias_ptr_type, apm->unit_offset);
	    }
	  else if (TREE_CODE (base) == MEM_REF)
	    {
	      if (!addrof)
		{
		  deref_base = true;
		  deref_align = TYPE_ALIGN (TREE_TYPE (base));
		}
	      off = build_int_cst (apm->alias_ptr_type,
				   base_offset + apm->unit_offset);
	      off = int_const_binop (PLUS_EXPR, TREE_OPERAND (base, 1),
				     off);
	      base = TREE_OPERAND (base, 0);
	    }
	  else
	    {
	      off = build_int_cst (apm->alias_ptr_type,
				   base_offset + apm->unit_offset);
	      base = build_fold_addr_expr (base);
	    }
	}

      tree type = apm->type;
      unsigned int align;
      unsigned HOST_WIDE_INT misalign;

      if (deref_base)
	{
	  align = deref_align;
	  misalign = 0;
	}
      else
	{
	  get_pointer_alignment_1 (base, &align, &misalign);
	  /* All users must make sure that we can be optimistic when it
	     comes to alignment in this case (by inspecting the final users
	     of these new parameters).  */
	  if (TYPE_ALIGN (type) > align)
	    align = TYPE_ALIGN (type);
	}
      misalign
	+= (offset_int::from (wi::to_wide (off), SIGNED).to_short_addr ()
	    * BITS_PER_UNIT);
      misalign = misalign & (align - 1);
      if (misalign != 0)
	align = least_bit_hwi (misalign);
      if (align < TYPE_ALIGN (type))
	type = build_aligned_type (type, align);
      base = force_gimple_operand_gsi (&gsi, base,
				       true, NULL, true, GSI_SAME_STMT);
      tree expr = fold_build2_loc (loc, MEM_REF, type, base, off);
      REF_REVERSE_STORAGE_ORDER (expr) = apm->reverse;
      /* If expr is not a valid gimple call argument emit
	 a load into a temporary.  */
      if (is_gimple_reg_type (TREE_TYPE (expr)))
	{
	  gimple *tem = gimple_build_assign (NULL_TREE, expr);
	  if (gimple_in_ssa_p (cfun))
	    {
	      gimple_set_vuse (tem, gimple_vuse (stmt));
	      expr = make_ssa_name (TREE_TYPE (expr), tem);
	    }
	  else
	    expr = create_tmp_reg (TREE_TYPE (expr));
	  gimple_assign_set_lhs (tem, expr);
	  gsi_insert_before (&gsi, tem, GSI_SAME_STMT);
	}
      vargs.quick_push (expr);
    }

  if (m_always_copy_start >= 0)
    for (unsigned i = m_always_copy_start; i < old_nargs; i++)
      vargs.safe_push (gimple_call_arg (stmt, i));

  /* For optimized away parameters, add on the caller side
     before the call
     DEBUG D#X => parm_Y(D)
     stmts and associate D#X with parm in decl_debug_args_lookup
     vector to say for debug info that if parameter parm had been passed,
     it would have value parm_Y(D).  */
  if (MAY_HAVE_DEBUG_BIND_STMTS && old_decl && callee_decl)
    {
      vec<tree, va_gc> **debug_args = NULL;
      unsigned i = 0;
      for (tree old_parm = DECL_ARGUMENTS (old_decl);
	   old_parm && i < old_nargs && ((int) i) < m_always_copy_start;
	   old_parm = DECL_CHAIN (old_parm), i++)
	{
	  if (!is_gimple_reg (old_parm) || kept[i])
	    continue;
	  tree origin = DECL_ORIGIN (old_parm);
	  tree arg = gimple_call_arg (stmt, i);

	  if (!useless_type_conversion_p (TREE_TYPE (origin), TREE_TYPE (arg)))
	    {
	      if (!fold_convertible_p (TREE_TYPE (origin), arg))
		continue;
	      tree rhs1;
	      if (TREE_CODE (arg) == SSA_NAME
		  && gimple_assign_cast_p (SSA_NAME_DEF_STMT (arg))
		  && (rhs1
		      = gimple_assign_rhs1 (SSA_NAME_DEF_STMT (arg)))
		  && useless_type_conversion_p (TREE_TYPE (origin),
						TREE_TYPE (rhs1)))
		arg = rhs1;
	      else
		arg = fold_convert_loc (gimple_location (stmt),
					TREE_TYPE (origin), arg);
	    }
	  if (debug_args == NULL)
	    debug_args = decl_debug_args_insert (callee_decl);
	  unsigned int ix;
	  tree ddecl = NULL_TREE;
	  for (ix = 0; vec_safe_iterate (*debug_args, ix, &ddecl); ix += 2)
	    if (ddecl == origin)
	      {
		ddecl = (**debug_args)[ix + 1];
		break;
	      }
	  if (ddecl == NULL)
	    {
	      ddecl = make_node (DEBUG_EXPR_DECL);
	      DECL_ARTIFICIAL (ddecl) = 1;
	      TREE_TYPE (ddecl) = TREE_TYPE (origin);
	      SET_DECL_MODE (ddecl, DECL_MODE (origin));

	      vec_safe_push (*debug_args, origin);
	      vec_safe_push (*debug_args, ddecl);
	    }
	  gimple *def_temp = gimple_build_debug_bind (ddecl,
						      unshare_expr (arg), stmt);
	  gsi_insert_before (&gsi, def_temp, GSI_SAME_STMT);
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "replacing stmt:");
      print_gimple_stmt (dump_file, gsi_stmt (gsi), 0);
    }

  gcall *new_stmt = gimple_build_call_vec (callee_decl, vargs);

  if (tree lhs = gimple_call_lhs (stmt))
    {
      if (!m_skip_return)
	gimple_call_set_lhs (new_stmt, lhs);
      else if (TREE_CODE (lhs) == SSA_NAME)
	{
	  /* LHS should now by a default-def SSA.  Unfortunately default-def
	     SSA_NAMEs need a backing variable (or at least some code examining
	     SSAs assumes it is non-NULL).  So we either have to re-use the
	     decl we have at hand or introdice a new one.  */
	  tree repl = create_tmp_var (TREE_TYPE (lhs), "removed_return");
	  repl = get_or_create_ssa_default_def (cfun, repl);
	  SSA_NAME_IS_DEFAULT_DEF (repl) = true;
	  imm_use_iterator ui;
	  use_operand_p use_p;
	  gimple *using_stmt;
	  FOR_EACH_IMM_USE_STMT (using_stmt, ui, lhs)
	    {
	      FOR_EACH_IMM_USE_ON_STMT (use_p, ui)
		{
		  SET_USE (use_p, repl);
		}
	      update_stmt (using_stmt);
	    }
	}
    }

  gimple_set_block (new_stmt, gimple_block (stmt));
  if (gimple_has_location (stmt))
    gimple_set_location (new_stmt, gimple_location (stmt));
  gimple_call_set_chain (new_stmt, gimple_call_chain (stmt));
  gimple_call_copy_flags (new_stmt, stmt);
  if (gimple_in_ssa_p (cfun))
    gimple_move_vops (new_stmt, stmt);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "with stmt:");
      print_gimple_stmt (dump_file, new_stmt, 0);
      fprintf (dump_file, "\n");
    }
  gsi_replace (&gsi, new_stmt, true);
  if (update_references)
    do
      {
	current_node->record_stmt_references (gsi_stmt (gsi));
	gsi_prev (&gsi);
      }
    while (gsi_stmt (gsi) != gsi_stmt (prev_gsi));
  return new_stmt;
}

/* Dump information contained in the object in textual form to F.  */

void
ipa_param_adjustments::dump (FILE *f)
{
  fprintf (f, "    m_always_copy_start: %i\n", m_always_copy_start);
  ipa_dump_adjusted_parameters (f, m_adj_params);
  if (m_skip_return)
    fprintf (f, "     Will SKIP return.\n");
}

/* Dump information contained in the object in textual form to stderr.  */

void
ipa_param_adjustments::debug ()
{
  dump (stderr);
}

/* Register that REPLACEMENT should replace parameter described in APM and
   optionally as DUMMY to mark transitive splits across calls.  */

void
ipa_param_body_adjustments::register_replacement (ipa_adjusted_param *apm,
						  tree replacement,
						  tree dummy)
{
  gcc_checking_assert (apm->op == IPA_PARAM_OP_SPLIT
		       || apm->op == IPA_PARAM_OP_NEW);
  gcc_checking_assert (!apm->prev_clone_adjustment);
  ipa_param_body_replacement psr;
  psr.base = m_oparms[apm->prev_clone_index];
  psr.repl = replacement;
  psr.dummy = dummy;
  psr.unit_offset = apm->unit_offset;
  m_replacements.safe_push (psr);
}

/* Copy or not, as appropriate given m_id and decl context, a pre-existing
   PARM_DECL T so that it can be included in the parameters of the modified
   function.  */

tree
ipa_param_body_adjustments::carry_over_param (tree t)
{
  tree new_parm;
  if (m_id)
    {
      new_parm = remap_decl (t, m_id);
      if (TREE_CODE (new_parm) != PARM_DECL)
	new_parm = m_id->copy_decl (t, m_id);
    }
  else if (DECL_CONTEXT (t) != m_fndecl)
    {
      new_parm = copy_node (t);
      DECL_CONTEXT (new_parm) = m_fndecl;
    }
  else
    new_parm = t;
  return new_parm;
}

/* Common initialization performed by all ipa_param_body_adjustments
   constructors.  OLD_FNDECL is the declaration we take original arguments
   from, (it may be the same as M_FNDECL).  VARS, if non-NULL, is a pointer to
   a chained list of new local variables.  TREE_MAP is the IPA-CP produced
   mapping of trees to constants.

   The function is rather long but it really onlu initializes all data members
   of the class.  It creates new param DECLs, finds their new types,   */

void
ipa_param_body_adjustments::common_initialization (tree old_fndecl,
						   tree *vars,
						   vec<ipa_replace_map *,
						       va_gc> *tree_map)
{
  push_function_arg_decls (&m_oparms, old_fndecl);
  auto_vec<tree,16> otypes;
  if (TYPE_ARG_TYPES (TREE_TYPE (old_fndecl)) != NULL_TREE)
    push_function_arg_types (&otypes, TREE_TYPE (old_fndecl));
  else
    {
      auto_vec<tree,16> oparms;
      push_function_arg_decls (&oparms, old_fndecl);
      unsigned ocount = oparms.length ();
      otypes.reserve_exact (ocount);
      for (unsigned i = 0; i < ocount; i++)
	otypes.quick_push (TREE_TYPE (oparms[i]));
    }
  fill_vector_of_new_param_types (&m_new_types, &otypes, m_adj_params, true);

  auto_vec<bool, 16> kept;
  kept.reserve_exact (m_oparms.length ());
  kept.quick_grow_cleared (m_oparms.length ());
  auto_vec<tree, 16> isra_dummy_decls;
  isra_dummy_decls.reserve_exact (m_oparms.length ());
  isra_dummy_decls.quick_grow_cleared (m_oparms.length ());

  unsigned adj_len = vec_safe_length (m_adj_params);
  m_method2func = ((TREE_CODE (TREE_TYPE (m_fndecl)) == METHOD_TYPE)
		   && (adj_len == 0
		       || (*m_adj_params)[0].op != IPA_PARAM_OP_COPY
		       || (*m_adj_params)[0].base_index != 0));

  /* The main job of the this function is to go over the vector of adjusted
     parameters and create declarations or find corresponding old ones and push
     them to m_new_decls.  For IPA-SRA replacements it also creates
     corresponding m_id->dst_node->clone.performed_splits entries.  */

  m_new_decls.reserve_exact (adj_len);
  for (unsigned i = 0; i < adj_len ; i++)
    {
      ipa_adjusted_param *apm = &(*m_adj_params)[i];
      unsigned prev_index = apm->prev_clone_index;
      tree new_parm;
      if (apm->op == IPA_PARAM_OP_COPY
	  || apm->prev_clone_adjustment)
	{
	  kept[prev_index] = true;
	  new_parm = carry_over_param (m_oparms[prev_index]);
	  m_new_decls.quick_push (new_parm);
	}
      else if (apm->op == IPA_PARAM_OP_NEW
	       || apm->op == IPA_PARAM_OP_SPLIT)
	{
	  tree new_type = m_new_types[i];
	  gcc_checking_assert (new_type);
	  new_parm = build_decl (UNKNOWN_LOCATION, PARM_DECL, NULL_TREE,
				 new_type);
	  const char *prefix = ipa_param_prefixes[apm->param_prefix_index];
	  DECL_NAME (new_parm) = create_tmp_var_name (prefix);
	  DECL_ARTIFICIAL (new_parm) = 1;
	  DECL_ARG_TYPE (new_parm) = new_type;
	  DECL_CONTEXT (new_parm) = m_fndecl;
	  TREE_USED (new_parm) = 1;
	  DECL_IGNORED_P (new_parm) = 1;
	  /* We assume all newly created arguments are not addressable.  */
	  if (TREE_CODE (new_type) == COMPLEX_TYPE
	      || TREE_CODE (new_type) == VECTOR_TYPE)
	    DECL_GIMPLE_REG_P (new_parm) = 1;
	  layout_decl (new_parm, 0);
	  m_new_decls.quick_push (new_parm);

	  if (apm->op == IPA_PARAM_OP_SPLIT)
	    {
	      m_split_modifications_p = true;

	      if (m_id)
		{
		  tree dummy_decl;
		  if (!isra_dummy_decls[prev_index])
		    {
		      dummy_decl = copy_decl_to_var (m_oparms[prev_index],
						     m_id);
		      /* Any attempt to remap this dummy in this particular
			 instance of clone materialization should yield
			 itself.  */
		      insert_decl_map (m_id, dummy_decl, dummy_decl);

		      DECL_CHAIN (dummy_decl) = *vars;
		      *vars = dummy_decl;
		      isra_dummy_decls[prev_index] = dummy_decl;
		    }
		  else
		    dummy_decl = isra_dummy_decls[prev_index];

		  register_replacement (apm, new_parm, dummy_decl);
		  ipa_param_performed_split ps;
		  ps.dummy_decl = dummy_decl;
		  ps.unit_offset = apm->unit_offset;
		  vec_safe_push (m_id->dst_node->clone.performed_splits, ps);
		}
	      else
		register_replacement (apm, new_parm);
	    }
        }
      else
	gcc_unreachable ();
    }


  /* As part of body modifications, we will also have to replace remaining uses
     of remaining uses of removed PARM_DECLs (which do not however use the
     initial value) with their VAR_DECL copies.

     We do this differently with and without m_id.  With m_id, we rely on its
     mapping and create a replacement straight away.  Without it, we have our
     own mechanism for which we have to populate m_removed_decls vector.  Just
     don't mix them, that is why you should not call
     replace_removed_params_ssa_names or perform_cfun_body_modifications when
     you construct with ID not equal to NULL.  */

  unsigned op_len = m_oparms.length ();
  for (unsigned i = 0; i < op_len; i++)
    if (!kept[i])
      {
	if (m_id)
	  {
	    if (!m_id->decl_map->get (m_oparms[i]))
	      {
		/* TODO: Perhaps at least aggregate-type params could re-use
		   their isra_dummy_decl here?  */
		tree var = copy_decl_to_var (m_oparms[i], m_id);
		insert_decl_map (m_id, m_oparms[i], var);
		/* Declare this new variable.  */
		DECL_CHAIN (var) = *vars;
		*vars = var;
	      }
	  }
	else
	  {
	    m_removed_decls.safe_push (m_oparms[i]);
	    m_removed_map.put (m_oparms[i], m_removed_decls.length () - 1);
	  }
      }

  if (!MAY_HAVE_DEBUG_STMTS)
    return;

  /* Finally, when generating debug info, we fill vector m_reset_debug_decls
    with removed parameters declarations.  We do this in order to re-map their
    debug bind statements and create debug decls for them.  */

  if (tree_map)
    {
      /* Do not output debuginfo for parameter declarations as if they vanished
	 when they were in fact replaced by a constant.  */
      auto_vec <int, 16> index_mapping;
      bool need_remap = false;

      if (m_id && m_id->src_node->clone.param_adjustments)
	{
	  ipa_param_adjustments *prev_adjustments
	    = m_id->src_node->clone.param_adjustments;
	  prev_adjustments->get_updated_indices (&index_mapping);
	  need_remap = true;
	}

      for (unsigned i = 0; i < tree_map->length (); i++)
	{
	  int parm_num = (*tree_map)[i]->parm_num;
	  gcc_assert (parm_num >= 0);
	  if (need_remap)
	    parm_num = index_mapping[parm_num];
	  kept[parm_num] = true;
	}
    }

  for (unsigned i = 0; i < op_len; i++)
    if (!kept[i] && is_gimple_reg (m_oparms[i]))
      m_reset_debug_decls.safe_push (m_oparms[i]);
}

/* Constructor of ipa_param_body_adjustments from a simple list of
   modifications to parameters listed in ADJ_PARAMS which will prepare ground
   for modification of parameters of fndecl.  Return value of the function will
   not be removed and the object will assume it does not run as a part of
   tree-function_versioning.  */

ipa_param_body_adjustments
::ipa_param_body_adjustments (vec<ipa_adjusted_param, va_gc> *adj_params,
			      tree fndecl)
  : m_adj_params (adj_params), m_adjustments (NULL), m_reset_debug_decls (),
    m_split_modifications_p (false), m_fndecl (fndecl), m_id (NULL),
    m_oparms (), m_new_decls (), m_new_types (), m_replacements (),
    m_removed_decls (), m_removed_map (), m_method2func (false)
{
  common_initialization (fndecl, NULL, NULL);
}

/* Constructor of ipa_param_body_adjustments from ipa_param_adjustments in
   ADJUSTMENTS which will prepare ground for modification of parameters of
   fndecl.  The object will assume it does not run as a part of
   tree-function_versioning.  */

ipa_param_body_adjustments
::ipa_param_body_adjustments (ipa_param_adjustments *adjustments,
			      tree fndecl)
  : m_adj_params (adjustments->m_adj_params), m_adjustments (adjustments),
    m_reset_debug_decls (), m_split_modifications_p (false), m_fndecl (fndecl),
    m_id (NULL), m_oparms (), m_new_decls (), m_new_types (),
    m_replacements (), m_removed_decls (), m_removed_map (),
    m_method2func (false)
{
  common_initialization (fndecl, NULL, NULL);
}

/* Constructor of ipa_param_body_adjustments which sets it up as a part of
   running tree_function_versioning.  Planned modifications to the function are
   in ADJUSTMENTS.  FNDECL designates the new function clone which is being
   modified.  OLD_FNDECL is the function of which FNDECL is a clone (and which
   at the time of invocation still share DECL_ARGUMENTS).  ID is the
   copy_body_data structure driving the wholy body copying process.  VARS is a
   pointer to the head of the list of new local variables, TREE_MAP is the map
   that drives tree substitution in the cloning process.  */

ipa_param_body_adjustments
::ipa_param_body_adjustments (ipa_param_adjustments *adjustments,
			      tree fndecl, tree old_fndecl,
			      copy_body_data *id, tree *vars,
			      vec<ipa_replace_map *, va_gc> *tree_map)
  : m_adj_params (adjustments->m_adj_params), m_adjustments (adjustments),
    m_reset_debug_decls (), m_split_modifications_p (false), m_fndecl (fndecl),
    m_id (id), m_oparms (), m_new_decls (), m_new_types (), m_replacements (),
    m_removed_decls (), m_removed_map (), m_method2func (false)
{
  common_initialization (old_fndecl, vars, tree_map);
}

/* Chain new param decls up and return them.  */

tree
ipa_param_body_adjustments::get_new_param_chain ()
{
  tree result;
  tree *link = &result;

  unsigned len = vec_safe_length (m_adj_params);
  for (unsigned i = 0; i < len; i++)
    {
      tree new_decl = m_new_decls[i];
      *link = new_decl;
      link = &DECL_CHAIN (new_decl);
    }
  *link = NULL_TREE;
  return result;
}

/* Modify the function parameters FNDECL and its type according to the plan in
   ADJUSTMENTS.  This function needs to be called when the decl has not already
   been processed with ipa_param_adjustments::adjust_decl, otherwise just
   seting DECL_ARGUMENTS to whatever get_new_param_chain will do is enough.  */

void
ipa_param_body_adjustments::modify_formal_parameters ()
{
  tree orig_type = TREE_TYPE (m_fndecl);
  DECL_ARGUMENTS (m_fndecl) = get_new_param_chain ();

  /* When signature changes, we need to clear builtin info.  */
  if (fndecl_built_in_p (m_fndecl))
    set_decl_built_in_function (m_fndecl, NOT_BUILT_IN, 0);

  /* At this point, removing return value is only implemented when going
     through tree_function_versioning, not when modifying function body
     directly.  */
  gcc_assert (!m_adjustments || !m_adjustments->m_skip_return);
  tree new_type = build_adjusted_function_type (orig_type, &m_new_types,
						m_method2func, false);

  TREE_TYPE (m_fndecl) = new_type;
  DECL_VIRTUAL_P (m_fndecl) = 0;
  DECL_LANG_SPECIFIC (m_fndecl) = NULL;
  if (m_method2func)
    DECL_VINDEX (m_fndecl) = NULL_TREE;
}

/* Given BASE and UNIT_OFFSET, find the corresponding record among replacement
   structures.  */

ipa_param_body_replacement *
ipa_param_body_adjustments::lookup_replacement_1 (tree base,
						  unsigned unit_offset)
{
  unsigned int len = m_replacements.length ();
  for (unsigned i = 0; i < len; i++)
    {
      ipa_param_body_replacement *pbr = &m_replacements[i];

      if (pbr->base == base
	  && (pbr->unit_offset == unit_offset))
	return pbr;
    }
  return NULL;
}

/* Given BASE and UNIT_OFFSET, find the corresponding replacement expression
   and return it, assuming it is known it does not hold value by reference or
   in reverse storage order.  */

tree
ipa_param_body_adjustments::lookup_replacement (tree base, unsigned unit_offset)
{
  ipa_param_body_replacement *pbr = lookup_replacement_1 (base, unit_offset);
  if (!pbr)
    return NULL;
  return pbr->repl;
}

/* If T is an SSA_NAME, return NULL if it is not a default def or
   return its base variable if it is.  If IGNORE_DEFAULT_DEF is true,
   the base variable is always returned, regardless if it is a default
   def.  Return T if it is not an SSA_NAME.  */

static tree
get_ssa_base_param (tree t, bool ignore_default_def)
{
  if (TREE_CODE (t) == SSA_NAME)
    {
      if (ignore_default_def || SSA_NAME_IS_DEFAULT_DEF (t))
	return SSA_NAME_VAR (t);
      else
	return NULL_TREE;
    }
  return t;
}

/* Given an expression, return the structure describing how it should be
   replaced if it accesses a part of a split parameter or NULL otherwise.

   Do not free the result, it will be deallocated when the object is destroyed.

   If IGNORE_DEFAULT_DEF is cleared, consider only SSA_NAMEs of PARM_DECLs
   which are default definitions, if set, consider all SSA_NAMEs of
   PARM_DECLs.  */

ipa_param_body_replacement *
ipa_param_body_adjustments::get_expr_replacement (tree expr,
						  bool ignore_default_def)
{
  tree base;
  unsigned unit_offset;

  if (!isra_get_ref_base_and_offset (expr, &base, &unit_offset))
    return NULL;

  base = get_ssa_base_param (base, ignore_default_def);
  if (!base || TREE_CODE (base) != PARM_DECL)
    return NULL;
  return lookup_replacement_1 (base, unit_offset);
}

/* Given OLD_DECL, which is a PARM_DECL of a parameter that is being removed
   (which includes it being split or replaced), return a new variable that
   should be used for any SSA names that will remain in the function that
   previously belonged to OLD_DECL.  */

tree
ipa_param_body_adjustments::get_replacement_ssa_base (tree old_decl)
{
  unsigned *idx = m_removed_map.get (old_decl);
  if (!idx)
    return NULL;

  tree repl;
  if (TREE_CODE (m_removed_decls[*idx]) == PARM_DECL)
    {
      gcc_assert (m_removed_decls[*idx] == old_decl);
      repl = copy_var_decl (old_decl, DECL_NAME (old_decl),
			    TREE_TYPE (old_decl));
      m_removed_decls[*idx] = repl;
    }
  else
    repl = m_removed_decls[*idx];
  return repl;
}

/* If OLD_NAME, which is being defined by statement STMT, is an SSA_NAME of a
   parameter which is to be removed because its value is not used, create a new
   SSA_NAME relating to a replacement VAR_DECL, replace all uses of the
   original with it and return it.  If there is no need to re-map, return NULL.
   ADJUSTMENTS is a pointer to a vector of IPA-SRA adjustments.  */

tree
ipa_param_body_adjustments::replace_removed_params_ssa_names (tree old_name,
							      gimple *stmt)
{
  gcc_assert (!m_id);
  if (TREE_CODE (old_name) != SSA_NAME)
    return NULL;

  tree decl = SSA_NAME_VAR (old_name);
  if (decl == NULL_TREE
      || TREE_CODE (decl) != PARM_DECL)
    return NULL;

  tree repl = get_replacement_ssa_base (decl);
  if (!repl)
    return NULL;

  tree new_name = make_ssa_name (repl, stmt);
  SSA_NAME_OCCURS_IN_ABNORMAL_PHI (new_name)
    = SSA_NAME_OCCURS_IN_ABNORMAL_PHI (old_name);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "replacing an SSA name of a removed param ");
      print_generic_expr (dump_file, old_name);
      fprintf (dump_file, " with ");
      print_generic_expr (dump_file, new_name);
      fprintf (dump_file, "\n");
    }

  replace_uses_by (old_name, new_name);
  return new_name;
}

/* If the expression *EXPR_P should be replaced, do so.  CONVERT specifies
   whether the function should care about type incompatibility of the current
   and new expressions.  If it is false, the function will leave
   incompatibility issues to the caller - note that when the function
   encounters a BIT_FIELD_REF, IMAGPART_EXPR or REALPART_EXPR, it will modify
   their bases instead of the expressions themselves and then also performs any
   necessary conversions.  */

bool
ipa_param_body_adjustments::modify_expression (tree *expr_p, bool convert)
{
  tree expr = *expr_p;

  if (TREE_CODE (expr) == BIT_FIELD_REF
      || TREE_CODE (expr) == IMAGPART_EXPR
      || TREE_CODE (expr) == REALPART_EXPR)
    {
      expr_p = &TREE_OPERAND (expr, 0);
      expr = *expr_p;
      convert = true;
    }

  ipa_param_body_replacement *pbr = get_expr_replacement (expr, false);
  if (!pbr)
    return false;

  tree repl = pbr->repl;
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "About to replace expr ");
      print_generic_expr (dump_file, expr);
      fprintf (dump_file, " with ");
      print_generic_expr (dump_file, repl);
      fprintf (dump_file, "\n");
    }

  if (convert && !useless_type_conversion_p (TREE_TYPE (expr),
					     TREE_TYPE (repl)))
    {
      tree vce = build1 (VIEW_CONVERT_EXPR, TREE_TYPE (expr), repl);
      *expr_p = vce;
    }
  else
    *expr_p = repl;
  return true;
}

/* If the assignment statement STMT contains any expressions that need to
   replaced with a different one as noted by ADJUSTMENTS, do so.  Handle any
   potential type incompatibilities.  If any conversion sttements have to be
   pre-pended to STMT, they will be added to EXTRA_STMTS.  Return true iff the
   statement was modified.  */

bool
ipa_param_body_adjustments::modify_assignment (gimple *stmt,
					       gimple_seq *extra_stmts)
{
  tree *lhs_p, *rhs_p;
  bool any;

  if (!gimple_assign_single_p (stmt))
    return false;

  rhs_p = gimple_assign_rhs1_ptr (stmt);
  lhs_p = gimple_assign_lhs_ptr (stmt);

  any = modify_expression (lhs_p, false);
  any |= modify_expression (rhs_p, false);
  if (any
      && !useless_type_conversion_p (TREE_TYPE (*lhs_p), TREE_TYPE (*rhs_p)))
    {
      if (TREE_CODE (*rhs_p) == CONSTRUCTOR)
	{
	  /* V_C_Es of constructors can cause trouble (PR 42714).  */
	  if (is_gimple_reg_type (TREE_TYPE (*lhs_p)))
	    *rhs_p = build_zero_cst (TREE_TYPE (*lhs_p));
	  else
	    *rhs_p = build_constructor (TREE_TYPE (*lhs_p),
					NULL);
	}
      else
	{
	  tree new_rhs = fold_build1_loc (gimple_location (stmt),
					  VIEW_CONVERT_EXPR, TREE_TYPE (*lhs_p),
					  *rhs_p);
	  tree tmp = force_gimple_operand (new_rhs, extra_stmts, true,
					   NULL_TREE);
	  gimple_assign_set_rhs1 (stmt, tmp);
	}
      return true;
    }

  return any;
}

/* Data passed to remap_split_decl_to_dummy through walk_tree.  */

struct simple_tree_swap_info
{
  /* Change FROM to TO.  */
  tree from, to;
  /* And set DONE to true when doing so.  */
  bool done;
};

/* Simple remapper to remap a split parameter to the same expression based on a
   special dummy decl so that edge redirections can detect transitive splitting
   and finish them.  */

static tree
remap_split_decl_to_dummy (tree *tp, int *walk_subtrees, void *data)
{
  tree t = *tp;

  if (DECL_P (t) || TREE_CODE (t) == SSA_NAME)
    {
      struct simple_tree_swap_info *swapinfo
	= (struct simple_tree_swap_info *) data;
      if (t == swapinfo->from
	  || (TREE_CODE (t) == SSA_NAME
	      && SSA_NAME_VAR (t) == swapinfo->from))
	{
	  *tp = swapinfo->to;
	  swapinfo->done = true;
	}
      *walk_subtrees = 0;
    }
  else if (TYPE_P (t))
      *walk_subtrees = 0;
  else
    *walk_subtrees = 1;
  return NULL_TREE;
}


/* If the call statement pointed at by STMT_P contains any expressions that
   need to replaced with a different one as noted by ADJUSTMENTS, do so.  f the
   statement needs to be rebuilt, do so.  Return true if any modifications have
   been performed.

   If the method is invoked as a part of IPA clone materialization and if any
   parameter split is transitive, i.e. it applies to the functin that is being
   modified and also to the callee of the statement, replace the parameter
   passed to old callee with an equivalent expression based on a dummy decl
   followed by PARM_DECLs representing the actual replacements.  The actual
   replacements will be then converted into SSA_NAMEs and then
   ipa_param_adjustments::modify_call will find the appropriate ones and leave
   only those in the call.  */

bool
ipa_param_body_adjustments::modify_call_stmt (gcall **stmt_p)
{
  gcall *stmt = *stmt_p;
  auto_vec <unsigned, 4> pass_through_args;
  auto_vec <unsigned, 4> pass_through_pbr_indices;

  if (m_split_modifications_p && m_id)
    {
      for (unsigned i = 0; i < gimple_call_num_args (stmt); i++)
	{
	  tree t = gimple_call_arg (stmt, i);
	  gcc_assert (TREE_CODE (t) != BIT_FIELD_REF
		      && TREE_CODE (t) != IMAGPART_EXPR
		      && TREE_CODE (t) != REALPART_EXPR);

	  tree base;
	  unsigned unit_offset;
	  if (!isra_get_ref_base_and_offset (t, &base, &unit_offset))
	    continue;

	  bool by_ref = false;
	  if (TREE_CODE (base) == SSA_NAME)
	    {
	      if (!SSA_NAME_IS_DEFAULT_DEF (base))
		continue;
	      base = SSA_NAME_VAR (base);
	      gcc_checking_assert (base);
	      by_ref = true;
	    }
	  if (TREE_CODE (base) != PARM_DECL)
	    continue;

	  bool base_among_replacements = false;
	  unsigned j, repl_list_len = m_replacements.length ();
	  for (j = 0; j < repl_list_len; j++)
	    {
	      ipa_param_body_replacement *pbr = &m_replacements[j];
	      if (pbr->base == base)
		{
		  base_among_replacements = true;
		  break;
		}
	    }
	  if (!base_among_replacements)
	    continue;

	  /* We still have to distinguish between an end-use that we have to
	     transform now and a pass-through, which happens in the following
	     two cases.  */

	  /* TODO: After we adjust ptr_parm_has_nonarg_uses to also consider
	     &MEM_REF[ssa_name + offset], we will also have to detect that case
	     here.    */

	  if (TREE_CODE (t) == SSA_NAME
	      && SSA_NAME_IS_DEFAULT_DEF (t)
	      && SSA_NAME_VAR (t)
	      && TREE_CODE (SSA_NAME_VAR (t)) == PARM_DECL)
	    {
	      /* This must be a by_reference pass-through.  */
	      gcc_assert (POINTER_TYPE_P (TREE_TYPE (t)));
	      pass_through_args.safe_push (i);
	      pass_through_pbr_indices.safe_push (j);
	    }
	  else if (!by_ref && AGGREGATE_TYPE_P (TREE_TYPE (t)))
	    {
	      /* Currently IPA-SRA guarantees the aggregate access type
		 exactly matches in this case.  So if it does not match, it is
		 a pass-through argument that will be sorted out at edge
		 redirection time.  */
	      ipa_param_body_replacement *pbr
		= lookup_replacement_1 (base, unit_offset);

	      if (!pbr
		  || (TYPE_MAIN_VARIANT (TREE_TYPE (t))
		      != TYPE_MAIN_VARIANT (TREE_TYPE (pbr->repl))))
		{
		  pass_through_args.safe_push (i);
		  pass_through_pbr_indices.safe_push (j);
		}
	    }
	}
    }

  unsigned nargs = gimple_call_num_args (stmt);
  if (!pass_through_args.is_empty ())
    {
      auto_vec<tree, 16> vargs;
      unsigned pt_idx = 0;
      for (unsigned i = 0; i < nargs; i++)
	{
	  if (pt_idx < pass_through_args.length ()
	      && i == pass_through_args[pt_idx])
	    {
	      unsigned j = pass_through_pbr_indices[pt_idx];
	      pt_idx++;
	      tree base = m_replacements[j].base;

	      /* Map base will get mapped to the special transitive-isra marker
		 dummy decl. */
	      struct simple_tree_swap_info swapinfo;
	      swapinfo.from = base;
	      swapinfo.to = m_replacements[j].dummy;
	      swapinfo.done = false;
	      tree arg = gimple_call_arg (stmt, i);
	      walk_tree (&arg, remap_split_decl_to_dummy, &swapinfo, NULL);
	      gcc_assert (swapinfo.done);
	      vargs.safe_push (arg);
	      /* Now let's push all replacements pertaining to this parameter
		 so that all gimple register ones get correct SSA_NAMES.  Edge
		 redirection will weed out the dummy argument as well as all
		 unused replacements later.  */
	      unsigned int repl_list_len = m_replacements.length ();
	      for (; j < repl_list_len; j++)
		{
		  if (m_replacements[j].base != base)
		    break;
		  vargs.safe_push (m_replacements[j].repl);
		}
	    }
	  else
	    {
	      tree t = gimple_call_arg (stmt, i);
	      modify_expression (&t, true);
	      vargs.safe_push (t);
	    }
	}
      gcall *new_stmt = gimple_build_call_vec (gimple_call_fn (stmt), vargs);
      gimple_call_set_chain (new_stmt, gimple_call_chain (stmt));
      gimple_call_copy_flags (new_stmt, stmt);
      if (tree lhs = gimple_call_lhs (stmt))
	{
	  modify_expression (&lhs, false);
	  gimple_call_set_lhs (new_stmt, lhs);
	}
      *stmt_p = new_stmt;
      return true;
    }

  /* Otherwise, no need to rebuild the statement, let's just modify arguments
     and the LHS if/as appropriate.  */
  bool modified = false;
  for (unsigned i = 0; i < nargs; i++)
    {
      tree *t = gimple_call_arg_ptr (stmt, i);
      modified |= modify_expression (t, true);
    }

  if (gimple_call_lhs (stmt))
    {
      tree *t = gimple_call_lhs_ptr (stmt);
      modified |= modify_expression (t, false);
    }

  return modified;
}

/* If the statement STMT contains any expressions that need to replaced with a
   different one as noted by ADJUSTMENTS, do so.  Handle any potential type
   incompatibilities.  If any conversion sttements have to be pre-pended to
   STMT, they will be added to EXTRA_STMTS.  Return true iff the statement was
   modified.  */

bool
ipa_param_body_adjustments::modify_gimple_stmt (gimple **stmt,
						gimple_seq *extra_stmts)
{
  bool modified = false;
  tree *t;

  switch (gimple_code (*stmt))
    {
    case GIMPLE_RETURN:
      t = gimple_return_retval_ptr (as_a <greturn *> (*stmt));
      if (m_adjustments && m_adjustments->m_skip_return)
	*t = NULL_TREE;
      else if (*t != NULL_TREE)
	modified |= modify_expression (t, true);
      break;

    case GIMPLE_ASSIGN:
      modified |= modify_assignment (*stmt, extra_stmts);
      break;

    case GIMPLE_CALL:
      modified |= modify_call_stmt ((gcall **) stmt);
      break;

    case GIMPLE_ASM:
      {
	gasm *asm_stmt = as_a <gasm *> (*stmt);
	for (unsigned i = 0; i < gimple_asm_ninputs (asm_stmt); i++)
	  {
	    t = &TREE_VALUE (gimple_asm_input_op (asm_stmt, i));
	    modified |= modify_expression (t, true);
	  }
	for (unsigned i = 0; i < gimple_asm_noutputs (asm_stmt); i++)
	  {
	    t = &TREE_VALUE (gimple_asm_output_op (asm_stmt, i));
	    modified |= modify_expression (t, false);
	  }
      }
      break;

    default:
      break;
    }
  return modified;
}


/* Traverse body of the current function and perform the requested adjustments
   on its statements.  Return true iff the CFG has been changed.  */

bool
ipa_param_body_adjustments::modify_cfun_body ()
{
  bool cfg_changed = false;
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator gsi;

      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gphi *phi = as_a <gphi *> (gsi_stmt (gsi));
	  tree new_lhs, old_lhs = gimple_phi_result (phi);
	  new_lhs = replace_removed_params_ssa_names (old_lhs, phi);
	  if (new_lhs)
	    {
	      gimple_phi_set_result (phi, new_lhs);
	      release_ssa_name (old_lhs);
	    }
	}

      gsi = gsi_start_bb (bb);
      while (!gsi_end_p (gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  gimple *stmt_copy = stmt;
	  gimple_seq extra_stmts = NULL;
	  bool modified = modify_gimple_stmt (&stmt, &extra_stmts);
	  if (stmt != stmt_copy)
	    {
	      gcc_checking_assert (modified);
	      gsi_replace (&gsi, stmt, false);
	    }
	  if (!gimple_seq_empty_p (extra_stmts))
	    gsi_insert_seq_before (&gsi, extra_stmts, GSI_SAME_STMT);

	  def_operand_p defp;
	  ssa_op_iter iter;
	  FOR_EACH_SSA_DEF_OPERAND (defp, stmt, iter, SSA_OP_DEF)
	    {
	      tree old_def = DEF_FROM_PTR (defp);
	      if (tree new_def = replace_removed_params_ssa_names (old_def,
								   stmt))
		{
		  SET_DEF (defp, new_def);
		  release_ssa_name (old_def);
		  modified = true;
		}
	    }

	  if (modified)
	    {
	      update_stmt (stmt);
	      if (maybe_clean_eh_stmt (stmt)
		  && gimple_purge_dead_eh_edges (gimple_bb (stmt)))
		cfg_changed = true;
	    }
	  gsi_next (&gsi);
	}
    }

  return cfg_changed;
}

/* Call gimple_debug_bind_reset_value on all debug statements describing
   gimple register parameters that are being removed or replaced.  */

void
ipa_param_body_adjustments::reset_debug_stmts ()
{
  int i, len;
  gimple_stmt_iterator *gsip = NULL, gsi;

  if (MAY_HAVE_DEBUG_STMTS && single_succ_p (ENTRY_BLOCK_PTR_FOR_FN (cfun)))
    {
      gsi = gsi_after_labels (single_succ (ENTRY_BLOCK_PTR_FOR_FN (cfun)));
      gsip = &gsi;
    }
  len = m_reset_debug_decls.length ();
  for (i = 0; i < len; i++)
    {
      imm_use_iterator ui;
      gimple *stmt;
      gdebug *def_temp;
      tree name, vexpr, copy = NULL_TREE;
      use_operand_p use_p;
      tree decl = m_reset_debug_decls[i];

      gcc_checking_assert (is_gimple_reg (decl));
      name = ssa_default_def (cfun, decl);
      vexpr = NULL;
      if (name)
	FOR_EACH_IMM_USE_STMT (stmt, ui, name)
	  {
	    if (gimple_clobber_p (stmt))
	      {
		gimple_stmt_iterator cgsi = gsi_for_stmt (stmt);
		unlink_stmt_vdef (stmt);
		gsi_remove (&cgsi, true);
		release_defs (stmt);
		continue;
	      }
	    /* All other users must have been removed by function body
	       modification.  */
	    gcc_assert (is_gimple_debug (stmt));
	    if (vexpr == NULL && gsip != NULL)
	      {
		vexpr = make_node (DEBUG_EXPR_DECL);
		def_temp = gimple_build_debug_source_bind (vexpr, decl, NULL);
		DECL_ARTIFICIAL (vexpr) = 1;
		TREE_TYPE (vexpr) = TREE_TYPE (name);
		SET_DECL_MODE (vexpr, DECL_MODE (decl));
		gsi_insert_before (gsip, def_temp, GSI_SAME_STMT);
	      }
	    if (vexpr)
	      {
		FOR_EACH_IMM_USE_ON_STMT (use_p, ui)
		  SET_USE (use_p, vexpr);
	      }
	    else
	      gimple_debug_bind_reset_value (stmt);
	    update_stmt (stmt);
	  }
      /* Create a VAR_DECL for debug info purposes.  */
      if (!DECL_IGNORED_P (decl))
	{
	  copy = build_decl (DECL_SOURCE_LOCATION (current_function_decl),
			     VAR_DECL, DECL_NAME (decl),
			     TREE_TYPE (decl));
	  if (DECL_PT_UID_SET_P (decl))
	    SET_DECL_PT_UID (copy, DECL_PT_UID (decl));
	  TREE_ADDRESSABLE (copy) = TREE_ADDRESSABLE (decl);
	  TREE_READONLY (copy) = TREE_READONLY (decl);
	  TREE_THIS_VOLATILE (copy) = TREE_THIS_VOLATILE (decl);
	  DECL_GIMPLE_REG_P (copy) = DECL_GIMPLE_REG_P (decl);
	  DECL_ARTIFICIAL (copy) = DECL_ARTIFICIAL (decl);
	  DECL_IGNORED_P (copy) = DECL_IGNORED_P (decl);
	  DECL_ABSTRACT_ORIGIN (copy) = DECL_ORIGIN (decl);
	  DECL_SEEN_IN_BIND_EXPR_P (copy) = 1;
	  SET_DECL_RTL (copy, 0);
	  TREE_USED (copy) = 1;
	  DECL_CONTEXT (copy) = current_function_decl;
	  add_local_decl (cfun, copy);
	  DECL_CHAIN (copy)
	    = BLOCK_VARS (DECL_INITIAL (current_function_decl));
	  BLOCK_VARS (DECL_INITIAL (current_function_decl)) = copy;
	}
      if (gsip != NULL && copy && target_for_debug_bind (decl))
	{
	  gcc_assert (TREE_CODE (decl) == PARM_DECL);
	  if (vexpr)
	    def_temp = gimple_build_debug_bind (copy, vexpr, NULL);
	  else
	    def_temp = gimple_build_debug_source_bind (copy, decl,
						       NULL);
	  gsi_insert_before (gsip, def_temp, GSI_SAME_STMT);
	}
    }
}

/* Perform all necessary body changes to change signature, body and debug info
   of fun according to adjustments passed at construction.  Return true if CFG
   was changed in any way.  The main entry point for modification of standalone
   functions that is not part of IPA clone materialization.  */

bool
ipa_param_body_adjustments::perform_cfun_body_modifications ()
{
  bool cfg_changed;
  modify_formal_parameters ();
  cfg_changed = modify_cfun_body ();
  reset_debug_stmts ();

  return cfg_changed;
}

