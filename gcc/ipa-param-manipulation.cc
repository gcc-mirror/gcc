/* Manipulation of formal and actual parameters of functions and function
   calls.
   Copyright (C) 2017-2024 Free Software Foundation, Inc.

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

#define INCLUDE_ALGORITHM
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
#include "alloc-pool.h"
#include "symbol-summary.h"
#include "symtab-clones.h"
#include "tree-phinodes.h"
#include "cfgexpand.h"
#include "attribs.h"
#include "sreal.h"
#include "ipa-cp.h"
#include "ipa-prop.h"

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

/* Structure to hold declarations representing pass-through IPA-SRA splits.  In
   essence, it tells new index for a combination of original index and
   offset.  */

struct pass_through_split_map
{
  /* Original argument index.  */
  unsigned base_index;
  /* Offset of the split part in the original argument.  */
  unsigned unit_offset;
  /* Index of the split part in the call statement - where clone
     materialization put it.  */
  int new_index;
};

/* Information about some call statements that needs to be conveyed from clone
   materialization to edge redirection. */

class ipa_edge_modification_info
{
 public:
  ipa_edge_modification_info ()
    {}

  /* Mapping of original argument indices to where those arguments sit in the
     call statement now or to a negative index if they were removed.  */
  auto_vec<int> index_map;
  /* Information about ISRA replacements put into the call statement at the
     clone materialization stages.  */
  auto_vec<pass_through_split_map> pass_through_map;
  /* Necessary adjustment to ipa_param_adjustments::m_always_copy_start when
     redirecting the call.  */
  int always_copy_delta = 0;
};

/* Class for storing and retrieving summaries about cal statement
   modifications.  */

class ipa_edge_modification_sum
  : public call_summary <ipa_edge_modification_info *>
{
 public:
  ipa_edge_modification_sum (symbol_table *table)
    : call_summary<ipa_edge_modification_info *> (table)
  {
  }

  /* Hook that is called by summary when an edge is duplicated.  */

  void duplicate (cgraph_edge *,
		  cgraph_edge *,
		  ipa_edge_modification_info *old_info,
		  ipa_edge_modification_info *new_info) final override
  {
    new_info->index_map.safe_splice (old_info->index_map);
    new_info->pass_through_map.safe_splice (old_info->pass_through_map);
    new_info->always_copy_delta = old_info->always_copy_delta;
  }
};

/* Call summary to store information about edges which have had their arguments
   partially modified already.  */

static ipa_edge_modification_sum *ipa_edge_modifications;

/* Fail compilation if CS has any summary associated with it in
   ipa_edge_modifications.  */

DEBUG_FUNCTION void
ipa_verify_edge_has_no_modifications (cgraph_edge *cs)
{
  gcc_assert (!ipa_edge_modifications || !ipa_edge_modifications->get (cs));
}

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

  if (!len)
    return;

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
	    fprintf (f, ", reverse");
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

/* Return false if given attribute should prevent type adjustments.  */

bool
ipa_param_adjustments::type_attribute_allowed_p (tree name)
{
  if ((is_attribute_p ("fn spec", name) && flag_ipa_modref)
      || is_attribute_p ("access", name)
      || is_attribute_p ("returns_nonnull", name)
      || is_attribute_p ("assume_aligned", name)
      || is_attribute_p ("nocf_check", name)
      || is_attribute_p ("warn_unused_result", name))
    return true;
  return false;
}

/* Return true if attribute should be dropped if parameter changed.  */

static bool
drop_type_attribute_if_params_changed_p (tree name)
{
  if (is_attribute_p ("fn spec", name)
      || is_attribute_p ("access", name))
    return true;
  return false;
}

/* Build and return a function type just like ORIG_TYPE but with parameter
   types given in NEW_PARAM_TYPES - which can be NULL if, but only if,
   ORIG_TYPE itself has NULL TREE_ARG_TYPEs.  If METHOD2FUNC is true, also make
   it a FUNCTION_TYPE instead of FUNCTION_TYPE.
   If ARG_MODIFIED is true drop attributes that are no longer up to date.  */

static tree
build_adjusted_function_type (tree orig_type, vec<tree> *new_param_types,
			      bool method2func, bool skip_return,
			      bool args_modified)
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
  if (args_modified && TYPE_ATTRIBUTES (new_type))
    {
      tree t = TYPE_ATTRIBUTES (new_type);
      tree *last = &TYPE_ATTRIBUTES (new_type);
      TYPE_ATTRIBUTES (new_type) = NULL;
      for (;t; t = TREE_CHAIN (t))
	if (!drop_type_attribute_if_params_changed_p
		(get_attribute_name (t)))
	  {
	    *last = copy_node (t);
	    TREE_CHAIN (*last) = NULL;
	    last = &TREE_CHAIN (*last);
	  }
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

  /* Check if any params type cares about are modified.  In this case will
     need to drop some type attributes.  */
  bool modified = false;
  size_t index = 0;
  if (m_adj_params)
    for (tree t = TYPE_ARG_TYPES (old_type);
	 t && (int)index < m_always_copy_start && !modified;
	 t = TREE_CHAIN (t), index++)
      if (index >= m_adj_params->length ()
	  || get_original_index (index) != (int)index)
	modified = true;


  return build_adjusted_function_type (old_type, new_param_types_p,
				       method2func_p (old_type), m_skip_return,
				       modified);
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

/* Remove all statements that use NAME directly or indirectly.  KILLED_SSAS
   contains the SSA_NAMEs that are already being or have been processed and new
   ones need to be added to it.  The function only has to process situations
   handled by ssa_name_only_returned_p in ipa-sra.cc with the exception that it
   can assume it must never reach a use in a return statement.  */

static void
purge_all_uses (tree name, hash_set <tree> *killed_ssas)
{
  imm_use_iterator imm_iter;
  gimple *stmt;
  auto_vec <tree, 4> worklist;

  worklist.safe_push (name);
  while (!worklist.is_empty ())
    {
      tree cur_name = worklist.pop ();
      FOR_EACH_IMM_USE_STMT (stmt, imm_iter, cur_name)
	{
	  if (gimple_debug_bind_p (stmt))
	    {
	      /* When runing within tree-inline, we will never end up here but
		 adding the SSAs to killed_ssas will do the trick in this case
		 and the respective debug statements will get reset. */
	      gimple_debug_bind_reset_value (stmt);
	      update_stmt (stmt);
	      continue;
	    }

	  tree lhs = NULL_TREE;
	  if (is_gimple_assign (stmt))
	    lhs = gimple_assign_lhs (stmt);
	  else if (gimple_code (stmt) == GIMPLE_PHI)
	    lhs = gimple_phi_result (stmt);
	  gcc_assert (lhs
		      && (TREE_CODE (lhs) == SSA_NAME)
		      && !gimple_vdef (stmt));
	  if (!killed_ssas->add (lhs))
	    {
	      worklist.safe_push (lhs);
	      gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
	      gsi_remove (&gsi, true);
	    }
	}
    }
}

/* Modify actual arguments of a function call in statement currently belonging
   to CS, and make it call CS->callee->decl.  Return the new statement that
   replaced the old one.  When invoked, cfun and current_function_decl have to
   be set to the caller.  When called from within tree-inline, KILLED_SSAs has
   to contain the pointer to killed_new_ssa_names within the copy_body_data
   structure and SSAs discovered to be useless (if LHS is removed) will be
   added to it, otherwise it needs to be NULL.  */

gcall *
ipa_param_adjustments::modify_call (cgraph_edge *cs,
				    bool update_references,
				    hash_set <tree> *killed_ssas)
{
  gcall *stmt = cs->call_stmt;
  tree callee_decl = cs->callee->decl;

  ipa_edge_modification_info *mod_info
    = ipa_edge_modifications ? ipa_edge_modifications->get (cs) : NULL;
  if (mod_info && symtab->dump_file)
    {
      fprintf (symtab->dump_file, "Information about pre-exiting "
	       "modifications.\n  Index map:");
      unsigned idx_len = mod_info->index_map.length ();
      for (unsigned i = 0; i < idx_len; i++)
	fprintf (symtab->dump_file, " %i", mod_info->index_map[i]);
      fprintf (symtab->dump_file, "\n  Pass-through split map: ");
      unsigned ptm_len = mod_info->pass_through_map.length ();
      for (unsigned i = 0; i < ptm_len; i++)
	fprintf (symtab->dump_file,
		 " (base_index: %u, offset: %u, new_index: %i)",
		 mod_info->pass_through_map[i].base_index,
		 mod_info->pass_through_map[i].unit_offset,
		 mod_info->pass_through_map[i].new_index);
      fprintf (symtab->dump_file, "\n  Always-copy delta: %i\n",
	       mod_info->always_copy_delta);
    }

  unsigned len = vec_safe_length (m_adj_params);
  auto_vec<tree, 16> vargs (len);
  unsigned old_nargs = gimple_call_num_args (stmt);
  unsigned orig_nargs = mod_info ? mod_info->index_map.length () : old_nargs;
  auto_vec<bool, 16> kept (old_nargs);
  kept.quick_grow_cleared (old_nargs);

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
	  int index = apm->base_index;
	  if ((unsigned) index >= orig_nargs)
	    /* Can happen if the original call has argument mismatch,
	       ignore.  */
	    continue;
	  if (mod_info)
	    {
	      index = mod_info->index_map[apm->base_index];
	      gcc_assert (index >= 0);
	    }

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

      /* We have to handle pass-through changes differently using the map
	 clone materialziation might have left behind.  */
      tree repl = NULL_TREE;
      unsigned ptm_len = mod_info ? mod_info->pass_through_map.length () : 0;
      for (unsigned j = 0; j < ptm_len; j++)
	if (mod_info->pass_through_map[j].base_index == apm->base_index
	    && mod_info->pass_through_map[j].unit_offset == apm->unit_offset)
	  {
	    int repl_idx = mod_info->pass_through_map[j].new_index;
	    gcc_assert (repl_idx >= 0);
	    repl = gimple_call_arg (stmt, repl_idx);
	    break;
	  }
      if (repl)
	{
	  if (!useless_type_conversion_p(apm->type, repl->typed.type))
	    {
	      repl = force_value_to_type (apm->type, repl);
	      repl = force_gimple_operand_gsi (&gsi, repl,
					       true, NULL, true, GSI_SAME_STMT);
	    }
	  vargs.quick_push (repl);
	  continue;
	}

      int index = apm->base_index;
      if ((unsigned) index >= orig_nargs)
	/* Can happen if the original call has argument mismatch, ignore.  */
	continue;
      if (mod_info)
	{
	  index = mod_info->index_map[apm->base_index];
	  gcc_assert (index >= 0);
	}
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
    {
      int always_copy_start = m_always_copy_start;
      if (mod_info)
	{
	  always_copy_start += mod_info->always_copy_delta;
	  gcc_assert (always_copy_start >= 0);
	}
      for (unsigned i = always_copy_start; i < old_nargs; i++)
	vargs.safe_push (gimple_call_arg (stmt, i));
    }

  /* For optimized away parameters, add on the caller side
     before the call
     DEBUG D#X => parm_Y(D)
     stmts and associate D#X with parm in decl_debug_args_lookup
     vector to say for debug info that if parameter parm had been passed,
     it would have value parm_Y(D).  */
  tree old_decl = gimple_call_fndecl (stmt);
  if (MAY_HAVE_DEBUG_BIND_STMTS && old_decl && callee_decl)
    {
      vec<tree, va_gc> **debug_args = NULL;
      unsigned i = 0;
      cgraph_node *callee_node = cgraph_node::get (callee_decl);

      /* FIXME: we don't seem to be able to insert debug args before clone
	 is materialized.  Materializing them early leads to extra memory
	 use.  */
      if (callee_node->clone_of)
	callee_node->get_untransformed_body ();
      for (tree old_parm = DECL_ARGUMENTS (old_decl);
	   old_parm && i < old_nargs && ((int) i) < m_always_copy_start;
	   old_parm = DECL_CHAIN (old_parm), i++)
	{
	  if (!is_gimple_reg (old_parm) || kept[i])
	    continue;
	  tree arg;
	  if (mod_info)
	    {
	      if (mod_info->index_map[i] < 0)
		continue;
	      arg = gimple_call_arg (stmt, mod_info->index_map[i]);
	    }
	  else
	    arg = gimple_call_arg (stmt, i);

	  tree origin = DECL_ORIGIN (old_parm);
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
	      ddecl = build_debug_expr_decl (TREE_TYPE (origin));
	      /* FIXME: Is setting the mode really necessary? */
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

  hash_set <tree> *ssas_to_remove = NULL;
  if (tree lhs = gimple_call_lhs (stmt))
    {
      if (!m_skip_return)
	gimple_call_set_lhs (new_stmt, lhs);
      else if (TREE_CODE (lhs) == SSA_NAME)
	{
	  if (!killed_ssas)
	    {
	      ssas_to_remove = new hash_set<tree> (8);
	      killed_ssas = ssas_to_remove;
	    }
	  killed_ssas->add (lhs);
	  purge_all_uses (lhs, killed_ssas);
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
  if (ssas_to_remove)
    {
      ipa_release_ssas_in_hash (ssas_to_remove);
      delete ssas_to_remove;
    }
  if (update_references)
    do
      {
	current_node->record_stmt_references (gsi_stmt (gsi));
	gsi_prev (&gsi);
      }
    while (gsi_stmt (gsi) != gsi_stmt (prev_gsi));

  if (mod_info)
    ipa_edge_modifications->remove (cs);
  return new_stmt;
}

/* Dump information contained in the object in textual form to F.  */

void
ipa_param_adjustments::dump (FILE *f)
{
  fprintf (f, "    m_always_copy_start: %i\n", m_always_copy_start);
  ipa_dump_adjusted_parameters (f, m_adj_params);
  if (m_skip_return)
    fprintf (f, "    Will SKIP return.\n");
}

/* Dump information contained in the object in textual form to stderr.  */

void
ipa_param_adjustments::debug ()
{
  dump (stderr);
}

/* Register a REPLACEMENT for accesses to BASE at UNIT_OFFSET.  */

void
ipa_param_body_adjustments::register_replacement (tree base,
						  unsigned unit_offset,
						  tree replacement)
{
  ipa_param_body_replacement psr;
  psr.base = base;
  psr.repl = replacement;
  psr.dummy = NULL_TREE;
  psr.unit_offset = unit_offset;
  m_replacements.safe_push (psr);
  m_sorted_replacements_p = false;
}

/* Register that REPLACEMENT should replace parameter described in APM.  */

void
ipa_param_body_adjustments::register_replacement (ipa_adjusted_param *apm,
						  tree replacement)
{
  gcc_checking_assert (apm->op == IPA_PARAM_OP_SPLIT
		       || apm->op == IPA_PARAM_OP_NEW);
  gcc_checking_assert (!apm->prev_clone_adjustment);
  register_replacement (m_oparms[apm->prev_clone_index], apm->unit_offset,
			replacement);
}

/* Comparator for sorting and searching
   ipa_param_body_adjustments::m_replacements.  */

static int
compare_param_body_replacement (const void *va, const void *vb)
{
  const ipa_param_body_replacement *a = (const ipa_param_body_replacement *) va;
  const ipa_param_body_replacement *b = (const ipa_param_body_replacement *) vb;

  if (DECL_UID (a->base) < DECL_UID (b->base))
    return -1;
  if (DECL_UID (a->base) > DECL_UID (b->base))
    return 1;
  if (a->unit_offset < b->unit_offset)
    return -1;
  if (a->unit_offset > b->unit_offset)
    return 1;
  return 0;
}

/* Sort m_replacements and set m_sorted_replacements_p to true.  */

void
ipa_param_body_adjustments::sort_replacements ()
{
  if (m_sorted_replacements_p)
    return;
  m_replacements.qsort (compare_param_body_replacement);
  m_sorted_replacements_p = true;
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

/* If DECL is a gimple register that has a default definition SSA name and that
   has some uses, return the default definition, otherwise return NULL_TREE.  */

tree
ipa_param_body_adjustments::get_ddef_if_exists_and_is_used (tree decl)
{
 if (!is_gimple_reg (decl))
    return NULL_TREE;
  tree ddef = ssa_default_def (m_id->src_cfun, decl);
  if (!ddef || has_zero_uses (ddef))
    return NULL_TREE;
  return ddef;
}

/* Populate m_dead_stmts given that DEAD_PARAM is going to be removed without
   any replacement or splitting.  REPL is the replacement VAR_SECL to base any
   remaining uses of a removed parameter on.  Push all removed SSA names that
   are used within debug statements to DEBUGSTACK.  */

void
ipa_param_body_adjustments::mark_dead_statements (tree dead_param,
						  vec<tree> *debugstack)
{
  /* Current IPA analyses which remove unused parameters never remove a
     non-gimple register ones which have any use except as parameters in other
     calls, so we can safely leve them as they are.  */
  tree parm_ddef = get_ddef_if_exists_and_is_used (dead_param);
  if (!parm_ddef)
    return;

  auto_vec<tree, 4> stack;
  hash_set<tree> used_in_debug;
  m_dead_ssas.add (parm_ddef);
  stack.safe_push (parm_ddef);
  while (!stack.is_empty ())
    {
      imm_use_iterator imm_iter;
      use_operand_p use_p;
      tree t = stack.pop ();

      insert_decl_map (m_id, t, error_mark_node);
      FOR_EACH_IMM_USE_FAST (use_p, imm_iter, t)
	{
	  gimple *stmt = USE_STMT (use_p);

	  /* Calls containing dead arguments cannot be deleted,
	     modify_call_stmt will instead remove just the argument later on.
	     If isra_track_scalar_value_uses in ipa-sra.cc is extended to look
	     through const functions, we will need to do so here too.  */
	  if (is_gimple_call (stmt)
	      || (m_id->blocks_to_copy
		  && !bitmap_bit_p (m_id->blocks_to_copy,
				    gimple_bb (stmt)->index)))
	    continue;

	  if (is_gimple_debug (stmt))
	    {
	      m_dead_stmts.add (stmt);
	      gcc_assert (gimple_debug_bind_p (stmt));
	      if (!used_in_debug.contains (t))
		{
		  used_in_debug.add (t);
		  debugstack->safe_push (t);
		}
	    }
	  else if (gimple_code (stmt) == GIMPLE_PHI)
	    {
	      gphi *phi = as_a <gphi *> (stmt);
	      int ix = PHI_ARG_INDEX_FROM_USE (use_p);

	      if (!m_id->blocks_to_copy
		  || bitmap_bit_p (m_id->blocks_to_copy,
				   gimple_phi_arg_edge (phi, ix)->src->index))
		{
		  m_dead_stmts.add (phi);
		  tree res = gimple_phi_result (phi);
		  if (!m_dead_ssas.add (res))
		    stack.safe_push (res);
		}
	    }
	  else if (is_gimple_assign (stmt))
	    {
	      m_dead_stmts.add (stmt);
	      if (!gimple_clobber_p (stmt))
		{
		  tree lhs = gimple_assign_lhs (stmt);
		  gcc_assert (TREE_CODE (lhs) == SSA_NAME);
		  if (!m_dead_ssas.add (lhs))
		    stack.safe_push (lhs);
		}
	    }
	  else if (gimple_code (stmt) == GIMPLE_RETURN)
	    gcc_assert (m_adjustments && m_adjustments->m_skip_return);
	  else
	    /* IPA-SRA does not analyze other types of statements.  */
	    gcc_unreachable ();
	}
    }

  if (!MAY_HAVE_DEBUG_STMTS)
    {
      gcc_assert (debugstack->is_empty ());
      return;
    }

  tree dp_ddecl = build_debug_expr_decl (TREE_TYPE (dead_param));
  /* FIXME: Is setting the mode really necessary? */
  SET_DECL_MODE (dp_ddecl, DECL_MODE (dead_param));
  m_dead_ssa_debug_equiv.put (parm_ddef, dp_ddecl);
}

/* Put all clobbers of of dereference of default definition of PARAM into
   m_dead_stmts.  If there are returns among uses of the default definition of
   PARAM, verify they will be stripped off the return value.  */

void
ipa_param_body_adjustments::mark_clobbers_dead (tree param)
{
  if (!is_gimple_reg (param))
    return;
  tree ddef = get_ddef_if_exists_and_is_used (param);
  if (!ddef)
    return;

 imm_use_iterator imm_iter;
 use_operand_p use_p;
 FOR_EACH_IMM_USE_FAST (use_p, imm_iter, ddef)
   {
     gimple *stmt = USE_STMT (use_p);
     if (gimple_clobber_p (stmt))
       m_dead_stmts.add (stmt);
     else if (gimple_code (stmt) == GIMPLE_RETURN)
       gcc_assert (m_adjustments && m_adjustments->m_skip_return);
   }
}

/* Callback to walk_tree.  If REMAP is an SSA_NAME that is present in hash_map
   passed in DATA, replace it with unshared version of what it was mapped to.
   If an SSA argument would be remapped to NULL, the whole operation needs to
   abort which is signaled by returning error_mark_node.  */

static tree
replace_with_mapped_expr (tree *remap, int *walk_subtrees, void *data)
{
  if (TYPE_P (*remap))
    {
      *walk_subtrees = 0;
      return 0;
    }
  if (TREE_CODE (*remap) != SSA_NAME)
    return 0;

  *walk_subtrees = 0;

  hash_map<tree, tree> *equivs = (hash_map<tree, tree> *) data;
  if (tree *p = equivs->get (*remap))
    {
      if (!*p)
	return error_mark_node;
      *remap = unshare_expr (*p);
    }
  return 0;
}

/* Replace all occurances of SSAs in m_dead_ssa_debug_equiv in t with what they
   are mapped to.  */

void
ipa_param_body_adjustments::remap_with_debug_expressions (tree *t)
{
  /* If *t is an SSA_NAME which should have its debug statements reset, it is
     mapped to NULL in the hash_map.

     It is perhaps simpler to handle the SSA_NAME cases directly and only
     invoke walk_tree on more complex expressions.  When
     remap_with_debug_expressions is called from tree-inline.cc, a to-be-reset
     SSA_NAME can be an operand to such expressions and the entire debug
     variable we are remapping should be reset.  This is signaled by walk_tree
     returning error_mark_node and done by setting *t to NULL.  */
  if (TREE_CODE (*t) == SSA_NAME)
    {
      if (tree *p = m_dead_ssa_debug_equiv.get (*t))
	*t = *p;
    }
  else if (walk_tree (t, replace_with_mapped_expr,
		      &m_dead_ssa_debug_equiv, NULL) == error_mark_node)
    *t = NULL_TREE;
}

/* For an SSA_NAME DEAD_SSA which is about to be DCEd because it is based on a
   useless parameter, prepare an expression that should represent it in
   debug_binds in the cloned function and add a mapping from DEAD_SSA to
   m_dead_ssa_debug_equiv.  That mapping is to NULL when the associated
   debug_statement has to be reset instead.  In such case return false,
   ottherwise return true.  If DEAD_SSA comes from a basic block which is not
   about to be copied, ignore it and return true.  */

bool
ipa_param_body_adjustments::prepare_debug_expressions (tree dead_ssa)
{
  gcc_checking_assert (m_dead_ssas.contains (dead_ssa));
  if (tree *d = m_dead_ssa_debug_equiv.get (dead_ssa))
    return (*d != NULL_TREE);

  gcc_assert (!SSA_NAME_IS_DEFAULT_DEF (dead_ssa));
  gimple *def = SSA_NAME_DEF_STMT (dead_ssa);
  if (m_id->blocks_to_copy
      && !bitmap_bit_p (m_id->blocks_to_copy, gimple_bb (def)->index))
    return true;

  if (gimple_code (def) == GIMPLE_PHI)
    {
      /* In theory, we could ignore all SSAs coming from BBs not in
	 m_id->blocks_to_copy but at the time of the writing this code that
	 should never really be the case because only fnsplit uses that bitmap,
	 so don't bother.  */
      tree value = degenerate_phi_result (as_a <gphi *> (def));
      if (!value
	  || (m_dead_ssas.contains (value)
	      && !prepare_debug_expressions (value)))
	{
	  m_dead_ssa_debug_equiv.put (dead_ssa, NULL_TREE);
	  return false;
	}

      gcc_assert (TREE_CODE (value) == SSA_NAME);
      tree *d = m_dead_ssa_debug_equiv.get (value);
      m_dead_ssa_debug_equiv.put (dead_ssa, *d);
      return true;
    }

  bool lost = false;
  use_operand_p use_p;
  ssa_op_iter oi;
  FOR_EACH_PHI_OR_STMT_USE (use_p, def, oi, SSA_OP_USE)
    {
      tree use = USE_FROM_PTR (use_p);
      if (m_dead_ssas.contains (use)
	  && !prepare_debug_expressions (use))
	{
	  lost = true;
	  break;
	}
    }

  if (lost)
    {
      m_dead_ssa_debug_equiv.put (dead_ssa, NULL_TREE);
      return false;
    }

  if (is_gimple_assign (def))
    {
      gcc_assert (!gimple_clobber_p (def));
      if (gimple_assign_copy_p (def)
	  && TREE_CODE (gimple_assign_rhs1 (def)) == SSA_NAME)
	{
	  tree d = *m_dead_ssa_debug_equiv.get (gimple_assign_rhs1 (def));
	  gcc_assert (d);
	  m_dead_ssa_debug_equiv.put (dead_ssa, d);
	  return true;
	}

      tree val
	= unshare_expr_without_location (gimple_assign_rhs_to_tree (def));
      remap_with_debug_expressions (&val);

      tree vexpr = build_debug_expr_decl (TREE_TYPE (val));
      m_dead_stmt_debug_equiv.put (def, val);
      m_dead_ssa_debug_equiv.put (dead_ssa, vexpr);
      return true;
    }
  else
    gcc_unreachable ();
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
  auto_vec<bool, 16> split;
  split.reserve_exact (m_oparms.length ());
  split.quick_grow_cleared (m_oparms.length ());

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
	  layout_decl (new_parm, 0);
	  m_new_decls.quick_push (new_parm);

	  if (apm->op == IPA_PARAM_OP_SPLIT)
	    {
	      split[prev_index] = true;
	      register_replacement (apm, new_parm);
	    }
        }
      else
	gcc_unreachable ();
    }

  auto_vec <int, 16> index_mapping;
  bool need_remap = false;
  if (m_id)
    {
      clone_info *cinfo = clone_info::get (m_id->src_node);
      if (cinfo && cinfo->param_adjustments)
	{
	  cinfo->param_adjustments->get_updated_indices (&index_mapping);
	  need_remap = true;
	}

      if (ipcp_transformation *ipcp_ts
	  = ipcp_get_transformation_summary (m_id->src_node))
	{
	  for (const ipa_argagg_value &av : ipcp_ts->m_agg_values)
	    {
	      int parm_num = av.index;

	      if (need_remap)
		{
		  /* FIXME: We cannot handle the situation when IPA-CP
		     identified that a parameter is a pointer to a global
		     variable and at the same time the variable has some known
		     constant contents (PR 107640).  The best place to make
		     sure we don't drop such constants on the floor probably is
		     not here, but we have to make sure that it does not
		     confuse the remapping.  */
		  if (parm_num >= (int) index_mapping.length ())
		    continue;
		  parm_num = index_mapping[parm_num];
		  if (parm_num < 0)
		    continue;
		}

	      if (!kept[parm_num])
		{
		  /* IPA-CP has detected an aggregate constant in a parameter
		     that will not be kept, which means that IPA-SRA would have
		     split it if there wasn't a constant.  Because we are about
		     to remove the original, this is the last chance where we
		     can substitute the uses with a constant (for values passed
		     by reference) or do the split but initialize the
		     replacement with a constant (for split aggregates passed
		     by value).  */

		  if (split[parm_num])
		    {
		      /* We must be careful not to add a duplicate
			 replacement. */
		      sort_replacements ();
		      ipa_param_body_replacement *pbr
			= lookup_replacement_1 (m_oparms[parm_num],
						av.unit_offset);
		      if (pbr)
			{
			  /* Otherwise IPA-SRA should have bailed out.  */
			  gcc_assert (AGGREGATE_TYPE_P (TREE_TYPE (pbr->repl)));
			  continue;
			}
		    }

		  tree repl;
		  if (av.by_ref)
		    repl = av.value;
		  else
		    {
		      repl = create_tmp_var (TREE_TYPE (av.value),
					     "removed_ipa_cp");
		      gimple *init_stmt = gimple_build_assign (repl, av.value);
		      m_split_agg_csts_inits.safe_push (init_stmt);
		    }
		  register_replacement (m_oparms[parm_num], av.unit_offset,
					repl);
		  split[parm_num] = true;
		}
	    }
	}
    }
  sort_replacements ();

  if (tree_map)
    {
      /* Do not treat parameters which were replaced with a constant as
	 completely vanished.  */
      for (unsigned i = 0; i < tree_map->length (); i++)
	{
	  int parm_num = (*tree_map)[i]->parm_num;
	  gcc_assert (parm_num >= 0);
	  if (need_remap)
	    parm_num = index_mapping[parm_num];
	  kept[parm_num] = true;
	}
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

  auto_vec<tree, 8> ssas_to_process_debug;
  unsigned op_len = m_oparms.length ();
  for (unsigned i = 0; i < op_len; i++)
    if (!kept[i])
      {
	if (m_id)
	  {
	    gcc_assert (!m_id->decl_map->get (m_oparms[i]));
	    tree var = copy_decl_to_var (m_oparms[i], m_id);
	    insert_decl_map (m_id, m_oparms[i], var);
	    /* Declare this new variable.  */
	    DECL_CHAIN (var) = *vars;
	    *vars = var;

	    /* If this is not a split but a real removal, init hash sets
	       that will guide what not to copy to the new body.  */
	    if (!split[i])
	      mark_dead_statements (m_oparms[i], &ssas_to_process_debug);
	    else
	      mark_clobbers_dead (m_oparms[i]);
	    if (MAY_HAVE_DEBUG_STMTS
		&& is_gimple_reg (m_oparms[i]))
	      m_reset_debug_decls.safe_push (m_oparms[i]);
	  }
	else
	  {
	    m_removed_decls.safe_push (m_oparms[i]);
	    m_removed_map.put (m_oparms[i], m_removed_decls.length () - 1);
	    if (MAY_HAVE_DEBUG_STMTS
		&& !kept[i]
		&& is_gimple_reg (m_oparms[i]))
	      m_reset_debug_decls.safe_push (m_oparms[i]);
	  }
      }

  while (!ssas_to_process_debug.is_empty ())
    prepare_debug_expressions (ssas_to_process_debug.pop ());
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
    m_dead_stmts (), m_dead_ssas (), m_dead_ssa_debug_equiv (),
    m_dead_stmt_debug_equiv (), m_fndecl (fndecl), m_id (NULL), m_oparms (),
    m_new_decls (), m_new_types (), m_replacements (),
    m_split_agg_csts_inits (), m_removed_decls (), m_removed_map (),
    m_method2func (false), m_sorted_replacements_p (true)
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
    m_reset_debug_decls (), m_dead_stmts (), m_dead_ssas (),
    m_dead_ssa_debug_equiv (), m_dead_stmt_debug_equiv (), m_fndecl (fndecl),
    m_id (NULL), m_oparms (), m_new_decls (), m_new_types (), m_replacements (),
    m_split_agg_csts_inits (), m_removed_decls (), m_removed_map (),
    m_method2func (false), m_sorted_replacements_p (true)
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
    m_reset_debug_decls (), m_dead_stmts (), m_dead_ssas (),
    m_dead_ssa_debug_equiv (), m_dead_stmt_debug_equiv (), m_fndecl (fndecl),
    m_id (id), m_oparms (), m_new_decls (), m_new_types (), m_replacements (),
    m_split_agg_csts_inits (), m_removed_decls (), m_removed_map (),
    m_method2func (false), m_sorted_replacements_p (true)
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

  bool modified = false;
  size_t index = 0;
  if (m_adj_params)
    for (tree t = TYPE_ARG_TYPES (orig_type);
	 t && !modified;
	 t = TREE_CHAIN (t), index++)
      if (index >= m_adj_params->length ()
	  || (*m_adj_params)[index].op != IPA_PARAM_OP_COPY
	  || (*m_adj_params)[index].base_index != index)
	modified = true;

  /* At this point, removing return value is only implemented when going
     through tree_function_versioning, not when modifying function body
     directly.  */
  gcc_assert (!m_adjustments || !m_adjustments->m_skip_return);
  tree new_type = build_adjusted_function_type (orig_type, &m_new_types,
						m_method2func, false, modified);

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
  gcc_assert (m_sorted_replacements_p);
  ipa_param_body_replacement key;
  key.base = base;
  key.unit_offset = unit_offset;
  ipa_param_body_replacement *res
    = std::lower_bound (m_replacements.begin (), m_replacements.end (), key,
			[] (const ipa_param_body_replacement &elt,
			    const ipa_param_body_replacement &val)
			{
			  return (compare_param_body_replacement (&elt, &val)
				  < 0);
			});

  if (res == m_replacements.end ()
      || res->base != base
      || res->unit_offset != unit_offset)
    return NULL;
  return res;
}

/* Find the first replacement for BASE among m_replacements and return pointer
   to it, or NULL if there is none.  */

ipa_param_body_replacement *
ipa_param_body_adjustments::lookup_first_base_replacement (tree base)
{
  gcc_assert (m_sorted_replacements_p);
  ipa_param_body_replacement key;
  key.base = base;
  ipa_param_body_replacement *res
    = std::lower_bound (m_replacements.begin (), m_replacements.end (), key,
			[] (const ipa_param_body_replacement &elt,
			    const ipa_param_body_replacement &val)
			{
			  if (DECL_UID (elt.base) < DECL_UID (val.base))
			    return true;
			  return false;
			});

  if (res == m_replacements.end ()
      || res->base != base)
    return NULL;
  return res;
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
ipa_param_body_adjustments::modify_expression (tree *expr_p, bool convert,
					       gimple_seq *extra_stmts)
{
  tree expr = *expr_p;

  if (m_replacements.is_empty ())
    return false;
  if (TREE_CODE (expr) == BIT_FIELD_REF
      || TREE_CODE (expr) == IMAGPART_EXPR
      || TREE_CODE (expr) == REALPART_EXPR)
    {
      /* For a BIT_FIELD_REF do not bother to VIEW_CONVERT the base,
	 instead reference the replacement directly.  */
      convert = TREE_CODE (expr) != BIT_FIELD_REF;
      expr_p = &TREE_OPERAND (expr, 0);
      expr = *expr_p;
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
      gcc_checking_assert (tree_to_shwi (TYPE_SIZE (TREE_TYPE (expr)))
			   == tree_to_shwi (TYPE_SIZE (TREE_TYPE (repl))));
      tree vce = build1 (VIEW_CONVERT_EXPR, TREE_TYPE (expr), repl);
      if (is_gimple_reg (repl)
	  && is_gimple_reg_type (TREE_TYPE (expr)))
	{
	  gcc_assert (extra_stmts);
	  vce = force_gimple_operand (vce, extra_stmts, true, NULL_TREE);
	}
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

  if (m_replacements.is_empty () || !gimple_assign_single_p (stmt))
    return false;

  rhs_p = gimple_assign_rhs1_ptr (stmt);
  lhs_p = gimple_assign_lhs_ptr (stmt);

  any = modify_expression (lhs_p, false);
  any |= modify_expression (rhs_p, false, extra_stmts);
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
	  gcc_checking_assert (tree_to_shwi (TYPE_SIZE (TREE_TYPE (*lhs_p)))
			      == tree_to_shwi (TYPE_SIZE (TREE_TYPE (*rhs_p))));
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

/* Record information about what modifications to call arguments have already
   been done by clone materialization into a summary describing CS.  The
   information is stored in NEW_INDEX_MAP, NEW_PT_MAP and NEW_ALWAYS_COPY_DELTA
   and correspond to equivalent fields in ipa_edge_modification_info.  Return
   the edge summary.  */

static ipa_edge_modification_info *
record_argument_state_1 (cgraph_edge *cs, const vec<int> &new_index_map,
			 const vec<pass_through_split_map> &new_pt_map,
			 int new_always_copy_delta)

{
  ipa_edge_modification_info *sum = ipa_edge_modifications->get_create (cs);

  unsigned len = sum->pass_through_map.length ();
  for (unsigned i = 0; i < len; i++)
    {
      unsigned oldnew = sum->pass_through_map[i].new_index;
      sum->pass_through_map[i].new_index = new_index_map[oldnew];
    }

  len = sum->index_map.length ();
  if (len > 0)
    {
      unsigned nptlen = new_pt_map.length ();
      for (unsigned j = 0; j < nptlen; j++)
	{
	  int inverse = -1;
	  for (unsigned i = 0; i < len ; i++)
	    if ((unsigned) sum->index_map[i] == new_pt_map[j].base_index)
	    {
	      inverse = i;
	      break;
	    }
	  gcc_assert (inverse >= 0);
	  pass_through_split_map ptm_item;

	  ptm_item.base_index = inverse;
	  ptm_item.unit_offset = new_pt_map[j].unit_offset;
	  ptm_item.new_index = new_pt_map[j].new_index;
	  sum->pass_through_map.safe_push (ptm_item);
	}

      for (unsigned i = 0; i < len; i++)
	{
	  int idx = sum->index_map[i];
	  if (idx < 0)
	    continue;
	  sum->index_map[i] = new_index_map[idx];
	}
    }
  else
    {
      sum->pass_through_map.safe_splice (new_pt_map);
      sum->index_map.safe_splice (new_index_map);
    }
  sum->always_copy_delta += new_always_copy_delta;
  return sum;
}

/* Record information about what modifications to call arguments have already
   been done by clone materialization into a summary of an edge describing the
   call in this clone and all its clones.  NEW_INDEX_MAP, NEW_PT_MAP and
   NEW_ALWAYS_COPY_DELTA have the same meaning as record_argument_state_1.

   In order to associate the info with the right edge summaries, we need
   address of the ORIG_STMT in the function from which we are cloning (because
   the edges have not yet been re-assigned to the new statement that has just
   been created) and ID, the structure governing function body copying.  */

static void
record_argument_state (copy_body_data *id, gimple *orig_stmt,
		       const vec<int> &new_index_map,
		       const vec<pass_through_split_map> &new_pt_map,
		       int new_always_copy_delta)
{
  if (!ipa_edge_modifications)
    ipa_edge_modifications = new ipa_edge_modification_sum (symtab);

  struct cgraph_node *this_node = id->dst_node;
  ipa_edge_modification_info *first_sum = NULL;
  cgraph_edge *cs = this_node->get_edge (orig_stmt);
  if (cs)
    first_sum = record_argument_state_1 (cs, new_index_map, new_pt_map,
					 new_always_copy_delta);
  else
    gcc_assert (this_node->clones);

  if (!this_node->clones)
    return;
  for (cgraph_node *subclone = this_node->clones; subclone != this_node;)
    {
      cs = subclone->get_edge (orig_stmt);
      if (cs)
	{
	  if (!first_sum)
	    first_sum = record_argument_state_1 (cs, new_index_map, new_pt_map,
						 new_always_copy_delta);
	  else
	    {
	      ipa_edge_modification_info *s2
		= ipa_edge_modifications->get_create (cs);
	      s2->index_map.truncate (0);
	      s2->index_map.safe_splice (first_sum->index_map);
	      s2->pass_through_map.truncate (0);
	      s2->pass_through_map.safe_splice (first_sum->pass_through_map);
	      s2->always_copy_delta = first_sum->always_copy_delta;
	    }
	}
      else
	gcc_assert (subclone->clones);

      if (subclone->clones)
	subclone = subclone->clones;
      else if (subclone->next_sibling_clone)
	subclone = subclone->next_sibling_clone;
      else
	{
	  while (subclone != this_node && !subclone->next_sibling_clone)
	    subclone = subclone->clone_of;
	  if (subclone != this_node)
	    subclone = subclone->next_sibling_clone;
	}
    }
}

/* If the call statement pointed at by STMT_P contains any expressions that
   need to replaced with a different one as noted by ADJUSTMENTS, do so.  f the
   statement needs to be rebuilt, do so.  Return true if any modifications have
   been performed.  ORIG_STMT, if not NULL, is the original statement in the
   function that is being cloned from, which at this point can be used to look
   up call_graph edges.

   If the method is invoked as a part of IPA clone materialization and if any
   parameter split is pass-through, i.e. it applies to the functin that is
   being modified and also to the callee of the statement, replace the
   parameter passed to old callee with all of the replacement a callee might
   possibly want and record the performed argument modifications in
   ipa_edge_modifications.  Likewise if any argument has already been left out
   because it is not necessary.  */

bool
ipa_param_body_adjustments::modify_call_stmt (gcall **stmt_p,
					      gimple *orig_stmt)
{
  auto_vec <unsigned, 4> pass_through_args;
  auto_vec <unsigned, 4> pass_through_pbr_indices;
  auto_vec <HOST_WIDE_INT, 4> pass_through_offsets;
  gcall *stmt = *stmt_p;
  unsigned nargs = gimple_call_num_args (stmt);
  bool recreate = false;
  gcc_assert (m_sorted_replacements_p);

  for (unsigned i = 0; i < gimple_call_num_args (stmt); i++)
    {
      tree t = gimple_call_arg (stmt, i);
      gcc_assert (TREE_CODE (t) != BIT_FIELD_REF
		  && TREE_CODE (t) != IMAGPART_EXPR
		  && TREE_CODE (t) != REALPART_EXPR);

      if (TREE_CODE (t) == SSA_NAME
	  && m_dead_ssas.contains (t))
	recreate = true;

      if (m_replacements.is_empty ())
	continue;

      tree base;
      unsigned agg_arg_offset;
      if (!isra_get_ref_base_and_offset (t, &base, &agg_arg_offset))
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

      ipa_param_body_replacement *first_rep
	= lookup_first_base_replacement (base);
      if (!first_rep)
	continue;
      unsigned first_rep_index = first_rep - m_replacements.begin ();

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
	  recreate = true;
	  gcc_assert (POINTER_TYPE_P (TREE_TYPE (t)));
	  pass_through_args.safe_push (i);
	  pass_through_pbr_indices.safe_push (first_rep_index);
	  pass_through_offsets.safe_push (agg_arg_offset);
	}
      else if (!by_ref && AGGREGATE_TYPE_P (TREE_TYPE (t)))
	{
	  /* Currently IPA-SRA guarantees the aggregate access type
	     exactly matches in this case.  So if it does not match, it is
	     a pass-through argument that will be sorted out at edge
	     redirection time.  */
	  ipa_param_body_replacement *pbr
	    = lookup_replacement_1 (base, agg_arg_offset);

	  if (!pbr
	      || (TYPE_MAIN_VARIANT (TREE_TYPE (t))
		  != TYPE_MAIN_VARIANT (TREE_TYPE (pbr->repl))))
	    {
	      recreate = true;
	      pass_through_args.safe_push (i);
	      pass_through_pbr_indices.safe_push (first_rep_index);
	      pass_through_offsets.safe_push (agg_arg_offset);
	    }
	}
    }

  if (!recreate)
    {
      /* No need to rebuild the statement, let's just modify arguments
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

  auto_vec<int, 16> index_map;
  auto_vec<pass_through_split_map, 4> pass_through_map;
  auto_vec<tree, 16> vargs;
  int always_copy_delta = 0;
  unsigned pt_idx = 0;
  int new_arg_idx = 0;
  for (unsigned i = 0; i < nargs; i++)
    {
      if (pt_idx < pass_through_args.length ()
	  && i == pass_through_args[pt_idx])
	{
	  unsigned j = pass_through_pbr_indices[pt_idx];
	  unsigned agg_arg_offset = pass_through_offsets[pt_idx];
	  pt_idx++;
	  always_copy_delta--;
	  tree base = m_replacements[j].base;

	  /* In order to be put into SSA form, we have to push all replacements
	     pertaining to this parameter as parameters to the call statement.
	     Edge redirection will need to use edge summary to weed out the
	     unnecessary ones.  */
	  unsigned repl_list_len = m_replacements.length ();
	  for (; j < repl_list_len; j++)
	    {
	      if (m_replacements[j].base != base)
		break;
	      if (m_replacements[j].unit_offset < agg_arg_offset)
		continue;
	      pass_through_split_map pt_map;
	      pt_map.base_index = i;
	      pt_map.unit_offset
		= m_replacements[j].unit_offset - agg_arg_offset;
	      pt_map.new_index = new_arg_idx;
	      pass_through_map.safe_push (pt_map);
	      vargs.safe_push (m_replacements[j].repl);
	      new_arg_idx++;
	      always_copy_delta++;
	    }
	  index_map.safe_push (-1);
	}
      else
	{
	  tree t = gimple_call_arg (stmt, i);
	  if (TREE_CODE (t) == SSA_NAME
	      && m_dead_ssas.contains (t))
	    {
	      always_copy_delta--;
	      index_map.safe_push (-1);
	    }
	  else
	    {
	      modify_expression (&t, true);
	      vargs.safe_push (t);
	      index_map.safe_push (new_arg_idx);
	      new_arg_idx++;
	    }
	}
    }

  gcall *new_stmt = gimple_build_call_vec (gimple_call_fn (stmt), vargs);
  if (gimple_has_location (stmt))
    gimple_set_location (new_stmt, gimple_location (stmt));
  gimple_call_set_chain (new_stmt, gimple_call_chain (stmt));
  gimple_call_copy_flags (new_stmt, stmt);
  if (tree lhs = gimple_call_lhs (stmt))
    {
      modify_expression (&lhs, false);
      /* Avoid adjusting SSA_NAME_DEF_STMT of a SSA lhs, SSA names
	 have not yet been remapped.  */
      *gimple_call_lhs_ptr (new_stmt) = lhs;
    }
  *stmt_p = new_stmt;

  if (orig_stmt)
    record_argument_state (m_id, orig_stmt, index_map, pass_through_map,
			   always_copy_delta);
  return true;
}

/* If the statement STMT contains any expressions that need to replaced with a
   different one as noted by ADJUSTMENTS, do so.  Handle any potential type
   incompatibilities.  If any conversion sttements have to be pre-pended to
   STMT, they will be added to EXTRA_STMTS.  Return true iff the statement was
   modified.  */

bool
ipa_param_body_adjustments::modify_gimple_stmt (gimple **stmt,
						gimple_seq *extra_stmts,
						gimple *orig_stmt)
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
      modified |= modify_call_stmt ((gcall **) stmt, orig_stmt);
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
	  bool modified = modify_gimple_stmt (&stmt, &extra_stmts, NULL);
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
		vexpr = build_debug_expr_decl (TREE_TYPE (name));
		/* FIXME: Is setting the mode really necessary? */
		SET_DECL_MODE (vexpr, DECL_MODE (decl));
		def_temp = gimple_build_debug_source_bind (vexpr, decl, NULL);
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
	  DECL_NOT_GIMPLE_REG_P (copy) = DECL_NOT_GIMPLE_REG_P (decl);
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


/* If there are any initialization statements that need to be emitted into
   the basic block BB right at ther start of the new function, do so.  */
void
ipa_param_body_adjustments::append_init_stmts (basic_block bb)
{
  gimple_stmt_iterator si = gsi_last_bb (bb);
  while (!m_split_agg_csts_inits.is_empty ())
    gsi_insert_after (&si, m_split_agg_csts_inits.pop (), GSI_NEW_STMT);
}

/* Deallocate summaries which otherwise stay alive until the end of
   compilation.  */

void
ipa_edge_modifications_finalize ()
{
  if (!ipa_edge_modifications)
    return;
  delete ipa_edge_modifications;
  ipa_edge_modifications = NULL;
}

/* Helper used to sort a vector of SSA_NAMES. */

static int
compare_ssa_versions (const void *va, const void *vb)
{
  const_tree const a = *(const_tree const*)va;
  const_tree const b = *(const_tree const*)vb;

  if (SSA_NAME_VERSION (a) < SSA_NAME_VERSION (b))
    return -1;
  if (SSA_NAME_VERSION (a) > SSA_NAME_VERSION (b))
    return 1;
  return 0;
}

/* Call release_ssa_name on all elements in KILLED_SSAS in a defined order.  */

void
ipa_release_ssas_in_hash (hash_set <tree> *killed_ssas)
{
  auto_vec<tree, 16> ssas_to_release;
  for (tree sn : *killed_ssas)
    ssas_to_release.safe_push (sn);
  ssas_to_release.qsort (compare_ssa_versions);
  for (tree sn : ssas_to_release)
    release_ssa_name (sn);
}
