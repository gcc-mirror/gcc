/* Callgraph clones
   Copyright (C) 2003-2025 Free Software Foundation, Inc.
   Contributed by Jan Hubicka

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

/* This module provide facilities for cloning functions.  I.e. creating
   new functions based on existing functions with simple modifications,
   such as replacement of parameters.

   To allow whole program optimization without actual presence of function
   bodies, an additional infrastructure is provided for so-called virtual
   clones

   A virtual clone in the callgraph is a function that has no
   associated body, just a description of how to create its body based
   on a different function (which itself may be a virtual clone).

   The description of function modifications includes adjustments to
   the function's signature (which allows, for example, removing or
   adding function arguments), substitutions to perform on the
   function body, and, for inlined functions, a pointer to the
   function that it will be inlined into.

   It is also possible to redirect any edge of the callgraph from a
   function to its virtual clone.  This implies updating of the call
   site to adjust for the new function signature.

   Most of the transformations performed by inter-procedural
   optimizations can be represented via virtual clones.  For
   instance, a constant propagation pass can produce a virtual clone
   of the function which replaces one of its arguments by a
   constant.  The inliner can represent its decisions by producing a
   clone of a function whose body will be later integrated into
   a given function.

   Using virtual clones, the program can be easily updated
   during the Execute stage, solving most of pass interactions
   problems that would otherwise occur during Transform.

   Virtual clones are later materialized in the LTRANS stage and
   turned into real functions.  Passes executed after the virtual
   clone were introduced also perform their Transform stage
   on new functions, so for a pass there is no significant
   difference between operating on a real function or a virtual
   clone introduced before its Execute stage.

   Optimization passes then work on virtual clones introduced before
   their Execute stage as if they were real functions.  The
   only difference is that clones are not visible during the
   Generate Summary stage.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "stringpool.h"
#include "cgraph.h"
#include "lto-streamer.h"
#include "tree-eh.h"
#include "tree-cfg.h"
#include "tree-inline.h"
#include "attribs.h"
#include "dumpfile.h"
#include "gimple-pretty-print.h"
#include "alloc-pool.h"
#include "symbol-summary.h"
#include "tree-vrp.h"
#include "sreal.h"
#include "ipa-cp.h"
#include "ipa-prop.h"
#include "ipa-fnsummary.h"
#include "symtab-thunks.h"
#include "symtab-clones.h"

/* Create clone of edge in the node N represented by CALL_EXPR
   the callgraph.  */

cgraph_edge *
cgraph_edge::clone (cgraph_node *n, gcall *call_stmt, unsigned stmt_uid,
		    profile_count num, profile_count den,
		    bool update_original)
{
  cgraph_edge *new_edge;
  profile_count::adjust_for_ipa_scaling (&num, &den);
  profile_count prof_count = count.apply_scale (num, den);

  if (indirect_unknown_callee)
    {
      tree decl;

      if (call_stmt && (decl = gimple_call_fndecl (call_stmt))
	  /* When the call is speculative, we need to resolve it
	     via cgraph_resolve_speculation and not here.  */
	  && !speculative)
	{
	  cgraph_node *callee = cgraph_node::get (decl);
	  gcc_checking_assert (callee);
	  new_edge = n->create_edge (callee, call_stmt, prof_count, true);
	}
      else
	{
	  new_edge = n->create_indirect_edge (call_stmt,
					      indirect_info->ecf_flags,
					      prof_count, true);
	  *new_edge->indirect_info = *indirect_info;
	}
    }
  else
    {
      new_edge = n->create_edge (callee, call_stmt, prof_count, true);
      if (indirect_info)
	{
	  new_edge->indirect_info
	    = ggc_cleared_alloc<cgraph_indirect_call_info> ();
	  *new_edge->indirect_info = *indirect_info;
	}
    }

  new_edge->inline_failed = inline_failed;
  new_edge->indirect_inlining_edge = indirect_inlining_edge;
  if (!call_stmt)
    new_edge->lto_stmt_uid = stmt_uid;
  new_edge->speculative_id = speculative_id;
  /* Clone flags that depend on call_stmt availability manually.  */
  new_edge->can_throw_external = can_throw_external;
  new_edge->call_stmt_cannot_inline_p = call_stmt_cannot_inline_p;
  new_edge->speculative = speculative;
  new_edge->in_polymorphic_cdtor = in_polymorphic_cdtor;

  /* Update IPA profile.  Local profiles need no updating in original.  */
  if (update_original)
    count = count.combine_with_ipa_count_within (count.ipa ()
						 - new_edge->count.ipa (),
						 caller->count);
  symtab->call_edge_duplication_hooks (this, new_edge);
  return new_edge;
}

/* Set flags of NEW_NODE and its decl.  NEW_NODE is a newly created private
   clone or its thunk.  */

static void
set_new_clone_decl_and_node_flags (cgraph_node *new_node)
{
  DECL_EXTERNAL (new_node->decl) = 0;
  TREE_PUBLIC (new_node->decl) = 0;
  DECL_COMDAT (new_node->decl) = 0;
  DECL_WEAK (new_node->decl) = 0;
  DECL_VIRTUAL_P (new_node->decl) = 0;
  DECL_STATIC_CONSTRUCTOR (new_node->decl) = 0;
  DECL_STATIC_DESTRUCTOR (new_node->decl) = 0;
  DECL_SET_IS_OPERATOR_NEW (new_node->decl, 0);
  DECL_SET_IS_OPERATOR_DELETE (new_node->decl, 0);
  DECL_IS_REPLACEABLE_OPERATOR (new_node->decl) = 0;

  new_node->externally_visible = 0;
  new_node->local = 1;
  new_node->lowered = true;
  new_node->semantic_interposition = 0;
}

/* Duplicate thunk THUNK if necessary but make it to refer to NODE.
   ARGS_TO_SKIP, if non-NULL, determines which parameters should be omitted.
   Function can return NODE if no thunk is necessary, which can happen when
   thunk is this_adjusting but we are removing this parameter.  */

static cgraph_node *
duplicate_thunk_for_node (cgraph_node *thunk, cgraph_node *node)
{
  cgraph_node *new_thunk, *thunk_of;
  thunk_of = thunk->callees->callee->ultimate_alias_target ();

  if (thunk_of->thunk)
    node = duplicate_thunk_for_node (thunk_of, node);

  if (!DECL_ARGUMENTS (thunk->decl))
    thunk->get_untransformed_body ();

  thunk_info *i = thunk_info::get (thunk);
  cgraph_edge *cs;
  for (cs = node->callers; cs; cs = cs->next_caller)
    if (cs->caller->thunk)
      {
	thunk_info *i2 = thunk_info::get (cs->caller);
	if (*i2 == *i)
	  return cs->caller;
      }

  tree new_decl;
  clone_info *info = clone_info::get (node);
  if (info && info->param_adjustments)
    {
      /* We do not need to duplicate this_adjusting thunks if we have removed
	 this.  */
      if (i->this_adjusting
	  && !info->param_adjustments->first_param_intact_p ())
	return node;

      new_decl = copy_node (thunk->decl);
      ipa_param_body_adjustments body_adj (info->param_adjustments,
					   new_decl);
      body_adj.modify_formal_parameters ();
    }
  else
    {
      new_decl = copy_node (thunk->decl);
      for (tree *arg = &DECL_ARGUMENTS (new_decl);
	   *arg; arg = &DECL_CHAIN (*arg))
	{
	  tree next = DECL_CHAIN (*arg);
	  *arg = copy_node (*arg);
	  DECL_CONTEXT (*arg) = new_decl;
	  DECL_CHAIN (*arg) = next;
	}
    }

  gcc_checking_assert (!DECL_STRUCT_FUNCTION (new_decl));
  gcc_checking_assert (!DECL_INITIAL (new_decl));
  gcc_checking_assert (!DECL_RESULT (new_decl));
  gcc_checking_assert (!DECL_RTL_SET_P (new_decl));

  DECL_NAME (new_decl) = clone_function_name_numbered (thunk->decl,
						       "artificial_thunk");
  SET_DECL_ASSEMBLER_NAME (new_decl, DECL_NAME (new_decl));

  /* We need to force DECL_IGNORED_P because the new thunk is created after
     early debug was run.  */
  DECL_IGNORED_P (new_decl) = 1;

  new_thunk = cgraph_node::create (new_decl);
  set_new_clone_decl_and_node_flags (new_thunk);
  new_thunk->definition = true;
  new_thunk->can_change_signature = node->can_change_signature;
  new_thunk->thunk = thunk->thunk;
  new_thunk->unique_name = in_lto_p;
  new_thunk->former_clone_of = thunk->decl;
  if (info && info->param_adjustments)
    clone_info::get_create (new_thunk)->param_adjustments
	   = info->param_adjustments;
  new_thunk->unit_id = thunk->unit_id;
  new_thunk->merged_comdat = thunk->merged_comdat;
  new_thunk->merged_extern_inline = thunk->merged_extern_inline;

  cgraph_edge *e = new_thunk->create_edge (node, NULL, new_thunk->count);
  symtab->call_edge_duplication_hooks (thunk->callees, e);
  symtab->call_cgraph_duplication_hooks (thunk, new_thunk);
  return new_thunk;
}

/* If E does not lead to a thunk, simply redirect it to N.  Otherwise create
   one or more equivalent thunks for N and redirect E to the first in the
   chain.  Note that it is then necessary to call
   n->expand_all_artificial_thunks once all callers are redirected.  */

void
cgraph_edge::redirect_callee_duplicating_thunks (cgraph_node *n)
{
  cgraph_node *orig_to = callee->ultimate_alias_target ();
  if (orig_to->thunk)
    n = duplicate_thunk_for_node (orig_to, n);

  redirect_callee (n);
}

/* Call expand_thunk on all callers that are thunks and if analyze those nodes
   that were expanded.  */

void
cgraph_node::expand_all_artificial_thunks ()
{
  cgraph_edge *e;
  for (e = callers; e;)
    if (e->caller->thunk)
      {
	cgraph_node *thunk = e->caller;

	e = e->next_caller;
	if (expand_thunk (thunk, false, false))
	  {
	    thunk->thunk = false;
	    thunk->analyze ();
	    ipa_analyze_node (thunk);
	    inline_analyze_function (thunk);
	  }
	thunk->expand_all_artificial_thunks ();
      }
    else
      e = e->next_caller;
}

void
dump_callgraph_transformation (const cgraph_node *original,
			       const cgraph_node *clone,
			       const char *suffix)
{
  if (symtab->ipa_clones_dump_file)
    {
      fprintf (symtab->ipa_clones_dump_file,
	       "Callgraph clone;%s;%d;%s;%d;%d;%s;%d;%s;%d;%d;%s\n",
	       original->asm_name (), original->order,
	       DECL_SOURCE_FILE (original->decl),
	       DECL_SOURCE_LINE (original->decl),
	       DECL_SOURCE_COLUMN (original->decl), clone->asm_name (),
	       clone->order, DECL_SOURCE_FILE (clone->decl),
	       DECL_SOURCE_LINE (clone->decl), DECL_SOURCE_COLUMN (clone->decl),
	       suffix);

      symtab->cloned_nodes.add (original);
      symtab->cloned_nodes.add (clone);
    }
}

/* Turn profile of N to local profile.   */

static void
localize_profile (cgraph_node *n)
{
  n->count = n->count.guessed_local ();
  for (cgraph_edge *e = n->callees; e; e=e->next_callee)
    {
      e->count = e->count.guessed_local ();
      if (!e->inline_failed)
	localize_profile (e->callee);
    }
  for (cgraph_edge *e = n->indirect_calls; e; e=e->next_callee)
    e->count = e->count.guessed_local ();
}

/* Create node representing clone of N executed COUNT times.  Decrease
   the execution counts from original node too.
   The new clone will have decl set to DECL that may or may not be the same
   as decl of N.

   When UPDATE_ORIGINAL is true, the counts are subtracted from the original
   function's profile to reflect the fact that part of execution is handled
   by node.
   When CALL_DUPLICATION_HOOK is true, the ipa passes are acknowledged about
   the new clone. Otherwise the caller is responsible for doing so later.

   If the new node is being inlined into another one, NEW_INLINED_TO should be
   the outline function the new one is (even indirectly) inlined to.  All hooks
   will see this in node's inlined_to, when invoked.  Can be NULL if the
   node is not inlined.

   If PARAM_ADJUSTMENTS is non-NULL, the parameter manipulation information
   will be overwritten by the new structure.  Otherwise the new node will
   share parameter manipulation information with the original node.  */

cgraph_node *
cgraph_node::create_clone (tree new_decl, profile_count prof_count,
			   bool update_original,
			   vec<cgraph_edge *> redirect_callers,
			   bool call_duplication_hook,
			   cgraph_node *new_inlined_to,
			   ipa_param_adjustments *param_adjustments,
			   const char *suffix)
{
  cgraph_node *new_node = symtab->create_empty ();
  cgraph_edge *e;
  unsigned i;
  profile_count old_count = count;
  bool nonzero = count.ipa ().nonzero_p ();

  if (new_inlined_to)
    dump_callgraph_transformation (this, new_inlined_to, "inlining to");

  /* When inlining we scale precisely to prof_count, when cloning we can
     preserve local profile.  */
  if (!new_inlined_to)
    prof_count = count.combine_with_ipa_count (prof_count);
  new_node->count = prof_count;
  new_node->has_omp_variant_constructs = this->has_omp_variant_constructs;

  /* Update IPA profile.  Local profiles need no updating in original.  */
  if (update_original)
    {
      if (inlined_to)
        count = count.combine_with_ipa_count_within (count.ipa ()
						     - prof_count.ipa (),
						     inlined_to->count);
      else
        count = count.combine_with_ipa_count (count.ipa () - prof_count.ipa ());
    }
  new_node->decl = new_decl;
  new_node->order = order;
  new_node->register_symbol ();
  new_node->lto_file_data = lto_file_data;
  new_node->analyzed = analyzed;
  new_node->definition = definition;
  new_node->versionable = versionable;
  new_node->can_change_signature = can_change_signature;
  new_node->redefined_extern_inline = redefined_extern_inline;
  new_node->semantic_interposition = semantic_interposition;
  new_node->tm_may_enter_irr = tm_may_enter_irr;
  new_node->externally_visible = false;
  new_node->no_reorder = no_reorder;
  new_node->local = true;
  new_node->inlined_to = new_inlined_to;
  new_node->rtl = rtl;
  new_node->frequency = frequency;
  new_node->tp_first_run = tp_first_run;
  new_node->tm_clone = tm_clone;
  new_node->icf_merged = icf_merged;
  new_node->thunk = thunk;
  new_node->unit_id = unit_id;
  new_node->merged_comdat = merged_comdat;
  new_node->merged_extern_inline = merged_extern_inline;
  clone_info *info = clone_info::get (this);

  if (param_adjustments)
    clone_info::get_create (new_node)->param_adjustments = param_adjustments;
  else if (info && info->param_adjustments)
    clone_info::get_create (new_node)->param_adjustments
	 = info->param_adjustments;
  new_node->split_part = split_part;

  FOR_EACH_VEC_ELT (redirect_callers, i, e)
    {
      /* Redirect calls to the old version node to point to its new
	 version.  The only exception is when the edge was proved to
	 be unreachable during the cloning procedure.  */
      if (!e->callee
	  || !fndecl_built_in_p (e->callee->decl, BUILT_IN_UNREACHABLE,
						  BUILT_IN_UNREACHABLE_TRAP))
        e->redirect_callee_duplicating_thunks (new_node);
    }
  new_node->expand_all_artificial_thunks ();

  for (e = callees;e; e=e->next_callee)
    e->clone (new_node, e->call_stmt, e->lto_stmt_uid, new_node->count, old_count,
	      update_original);

  for (e = indirect_calls; e; e = e->next_callee)
    e->clone (new_node, e->call_stmt, e->lto_stmt_uid,
	      new_node->count, old_count, update_original);
  new_node->clone_references (this);

  new_node->next_sibling_clone = clones;
  if (clones)
    clones->prev_sibling_clone = new_node;
  clones = new_node;
  new_node->clone_of = this;

  if (call_duplication_hook)
    symtab->call_cgraph_duplication_hooks (this, new_node);
  /* With partial train run we do not want to assume that original's
     count is zero whenever we redurect all executed edges to clone.
     Simply drop profile to local one in this case.  */
  if (update_original
      && opt_for_fn (decl, flag_profile_partial_training)
      && nonzero
      && count.ipa_p ()
      && !count.ipa ().nonzero_p ()
      && !inlined_to)
    localize_profile (this);

  if (!new_inlined_to)
    dump_callgraph_transformation (this, new_node, suffix);

  return new_node;
}

static GTY(()) hash_map<const char *, unsigned> *clone_fn_ids;

/* Return a new assembler name for a clone of decl named NAME.  Apart
   from the string SUFFIX, the new name will end with a unique (for
   each NAME) unspecified number.  If clone numbering is not needed
   then the two argument clone_function_name should be used instead.
   Should not be called directly except for by
   lto-partition.cc:privatize_symbol_name_1.  */

tree
clone_function_name_numbered (const char *name, const char *suffix)
{
  /* Initialize the function->counter mapping the first time it's
     needed.  */
  if (!clone_fn_ids)
    clone_fn_ids = hash_map<const char *, unsigned int>::create_ggc (64);
  unsigned int &suffix_counter = clone_fn_ids->get_or_insert (
				   IDENTIFIER_POINTER (get_identifier (name)));
  return clone_function_name (name, suffix, suffix_counter++);
}

/* Return a new assembler name for a clone of DECL.  Apart from string
   SUFFIX, the new name will end with a unique (for each DECL
   assembler name) unspecified number.  If clone numbering is not
   needed then the two argument clone_function_name should be used
   instead.  */

tree
clone_function_name_numbered (tree decl, const char *suffix)
{
  tree name = DECL_ASSEMBLER_NAME (decl);
  return clone_function_name_numbered (IDENTIFIER_POINTER (name),
				       suffix);
}

/* Return a new assembler name for a clone of decl named NAME.  Apart
   from the string SUFFIX, the new name will end with the specified
   NUMBER.  If clone numbering is not needed then the two argument
   clone_function_name should be used instead.  */

tree
clone_function_name (const char *name, const char *suffix,
		     unsigned long number)
{
  size_t len = strlen (name);
  char *tmp_name, *prefix;

  prefix = XALLOCAVEC (char, len + strlen (suffix) + 2);
  memcpy (prefix, name, len);
  strcpy (prefix + len + 1, suffix);
  prefix[len] = symbol_table::symbol_suffix_separator ();
  ASM_FORMAT_PRIVATE_NAME (tmp_name, prefix, number);
  return get_identifier (tmp_name);
}

/* Return a new assembler name for a clone of DECL.  Apart from the
   string SUFFIX, the new name will end with the specified NUMBER.  If
   clone numbering is not needed then the two argument
   clone_function_name should be used instead.  */

tree
clone_function_name (tree decl, const char *suffix,
		     unsigned long number)
{
  return clone_function_name (
	   IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)), suffix, number);
}

/* Return a new assembler name ending with the string SUFFIX for a
   clone of DECL.  */

tree
clone_function_name (tree decl, const char *suffix)
{
  tree identifier = DECL_ASSEMBLER_NAME (decl);
  /* For consistency this needs to behave the same way as
     ASM_FORMAT_PRIVATE_NAME does, but without the final number
     suffix.  */
  char *separator = XALLOCAVEC (char, 2);
  separator[0] = symbol_table::symbol_suffix_separator ();
  separator[1] = 0;
#if defined (NO_DOT_IN_LABEL) && defined (NO_DOLLAR_IN_LABEL)
  const char *prefix = "__";
#else
  const char *prefix = "";
#endif
  char *result = ACONCAT ((prefix,
			   IDENTIFIER_POINTER (identifier),
			   separator,
			   suffix,
			   (char*)0));
  return get_identifier (result);
}


/* Create callgraph node clone with new declaration.  The actual body will be
   copied later at compilation stage.  The name of the new clone will be
   constructed from the name of the original node, SUFFIX and NUM_SUFFIX.

   TODO: after merging in ipa-sra use function call notes instead of args_to_skip
   bitmap interface.
   */
cgraph_node *
cgraph_node::create_virtual_clone (const vec<cgraph_edge *> &redirect_callers,
				   vec<ipa_replace_map *, va_gc> *tree_map,
				   ipa_param_adjustments *param_adjustments,
				   const char * suffix, unsigned num_suffix)
{
  tree old_decl = decl;
  cgraph_node *new_node = NULL;
  tree new_decl;
  size_t len, i;
  ipa_replace_map *map;
  char *name;

  gcc_checking_assert (versionable);
  /* TODO: It would be nice if we could recognize that param_adjustments do not
     actually perform any changes, but at the moment let's require it simply
     does not exist.  */
  gcc_assert (can_change_signature || !param_adjustments);

  /* Make a new FUNCTION_DECL tree node */
  if (!param_adjustments)
    new_decl = copy_node (old_decl);
  else
    new_decl = param_adjustments->adjust_decl (old_decl);

  /* These pointers represent function body and will be populated only when clone
     is materialized.  */
  gcc_assert (new_decl != old_decl);
  DECL_STRUCT_FUNCTION (new_decl) = NULL;
  DECL_ARGUMENTS (new_decl) = NULL;
  DECL_INITIAL (new_decl) = NULL;
  DECL_RESULT (new_decl) = NULL;
  /* We cannot do DECL_RESULT (new_decl) = NULL; here because of LTO partitioning
     sometimes storing only clone decl instead of original.  */

  /* Generate a new name for the new version. */
  len = IDENTIFIER_LENGTH (DECL_NAME (old_decl));
  name = XALLOCAVEC (char, len + strlen (suffix) + 2);
  memcpy (name, IDENTIFIER_POINTER (DECL_NAME (old_decl)), len);
  strcpy (name + len + 1, suffix);
  name[len] = '.';
  DECL_NAME (new_decl) = get_identifier (name);
  SET_DECL_ASSEMBLER_NAME (new_decl,
			   clone_function_name (old_decl, suffix, num_suffix));
  SET_DECL_RTL (new_decl, NULL);

  new_node = create_clone (new_decl, count, false,
			   redirect_callers, false, NULL, param_adjustments,
			   suffix);

  /* Update the properties.
     Make clone visible only within this translation unit.  Make sure
     that is not weak also.
     ??? We cannot use COMDAT linkage because there is no
     ABI support for this.  */
  set_new_clone_decl_and_node_flags (new_node);
  new_node->ipcp_clone = ipcp_clone;
  if (tree_map)
    clone_info::get_create (new_node)->tree_map = tree_map;
  if (!implicit_section)
    new_node->set_section (*this);

  /* Clones of global symbols or symbols with unique names are unique.  */
  if ((TREE_PUBLIC (old_decl)
       && !DECL_EXTERNAL (old_decl)
       && !DECL_WEAK (old_decl)
       && !DECL_COMDAT (old_decl))
      || in_lto_p)
    new_node->unique_name = true;
  FOR_EACH_VEC_SAFE_ELT (tree_map, i, map)
    {
      tree repl = map->new_tree;
      if (map->force_load_ref)
	{
	  gcc_assert (TREE_CODE (repl) == ADDR_EXPR);
	  repl = get_base_address (TREE_OPERAND (repl, 0));
	}
      new_node->maybe_create_reference (repl, NULL);
    }

  if (ipa_transforms_to_apply.exists ())
    new_node->ipa_transforms_to_apply
      = ipa_transforms_to_apply.copy ();

  symtab->call_cgraph_duplication_hooks (this, new_node);

  return new_node;
}

/* callgraph node being removed from symbol table; see if its entry can be
   replaced by other inline clone.
   INFO is clone info to attach to the new root.  */
cgraph_node *
cgraph_node::find_replacement (clone_info *info)
{
  cgraph_node *next_inline_clone, *replacement;

  for (next_inline_clone = clones;
       next_inline_clone
       && next_inline_clone->decl != decl;
       next_inline_clone = next_inline_clone->next_sibling_clone)
    ;

  /* If there is inline clone of the node being removed, we need
     to put it into the position of removed node and reorganize all
     other clones to be based on it.  */
  if (next_inline_clone)
    {
      cgraph_node *n;
      cgraph_node *new_clones;

      replacement = next_inline_clone;

      /* Unlink inline clone from the list of clones of removed node.  */
      if (next_inline_clone->next_sibling_clone)
	next_inline_clone->next_sibling_clone->prev_sibling_clone
	  = next_inline_clone->prev_sibling_clone;
      if (next_inline_clone->prev_sibling_clone)
	{
	  gcc_assert (clones != next_inline_clone);
	  next_inline_clone->prev_sibling_clone->next_sibling_clone
	    = next_inline_clone->next_sibling_clone;
	}
      else
	{
	  gcc_assert (clones == next_inline_clone);
	  clones = next_inline_clone->next_sibling_clone;
	}

      new_clones = clones;
      clones = NULL;

      /* Copy clone info.  */
      if (info)
	*clone_info::get_create (next_inline_clone) = *info;

      /* Now place it into clone tree at same level at NODE.  */
      next_inline_clone->clone_of = clone_of;
      next_inline_clone->prev_sibling_clone = NULL;
      next_inline_clone->next_sibling_clone = NULL;
      if (clone_of)
	{
	  if (clone_of->clones)
	    clone_of->clones->prev_sibling_clone = next_inline_clone;
	  next_inline_clone->next_sibling_clone = clone_of->clones;
	  clone_of->clones = next_inline_clone;
	}

      /* Merge the clone list.  */
      if (new_clones)
	{
	  if (!next_inline_clone->clones)
	    next_inline_clone->clones = new_clones;
	  else
	    {
	      n = next_inline_clone->clones;
	      while (n->next_sibling_clone)
		n = n->next_sibling_clone;
	      n->next_sibling_clone = new_clones;
	      new_clones->prev_sibling_clone = n;
	    }
	}

      /* Update clone_of pointers.  */
      n = new_clones;
      while (n)
	{
	  n->clone_of = next_inline_clone;
	  n = n->next_sibling_clone;
	}

      /* Update order in order to be able to find a LTO section
	 with function body.  */
      replacement->order = order;

      return replacement;
    }
  else
    return NULL;
}

/* Like cgraph_set_call_stmt but walk the clone tree and update all
   clones sharing the same function body.
   When WHOLE_SPECULATIVE_EDGES is true, all three components of
   speculative edge gets updated.  Otherwise we update only direct
   call.  */

void
cgraph_node::set_call_stmt_including_clones (gimple *old_stmt,
					     gcall *new_stmt,
					     bool update_speculative)
{
  cgraph_node *node;
  cgraph_edge *master_edge = get_edge (old_stmt);

  if (master_edge)
    cgraph_edge::set_call_stmt (master_edge, new_stmt, update_speculative);

  node = clones;
  if (node)
    while (node != this)
      {
	cgraph_edge *edge = node->get_edge (old_stmt);
	if (edge)
	  {
	    edge = cgraph_edge::set_call_stmt (edge, new_stmt,
					       update_speculative);
	    /* If UPDATE_SPECULATIVE is false, it means that we are turning
	       speculative call into a real code sequence.  Update the
	       callgraph edges.  */
	    if (edge->speculative && !update_speculative)
	      {
		cgraph_edge *indirect = edge->speculative_call_indirect_edge ();

		for (cgraph_edge *next, *direct
			= edge->first_speculative_call_target ();
		     direct;
		     direct = next)
		  {
		    next = direct->next_speculative_call_target ();
		    direct->speculative_call_target_ref ()->speculative = false;
		    direct->speculative = false;
		  }
		indirect->speculative = false;
	      }
	  }
	if (node->clones)
	  node = node->clones;
	else if (node->next_sibling_clone)
	  node = node->next_sibling_clone;
	else
	  {
	    while (node != this && !node->next_sibling_clone)
	      node = node->clone_of;
	    if (node != this)
	      node = node->next_sibling_clone;
	  }
      }
}

/* Like cgraph_create_edge walk the clone tree and update all clones sharing
   same function body.  If clones already have edge for OLD_STMT; only
   update the edge same way as cgraph_set_call_stmt_including_clones does.

   TODO: COUNT and LOOP_DEPTH should be properly distributed based on relative
   frequencies of the clones.  */

void
cgraph_node::create_edge_including_clones (cgraph_node *callee,
					   gimple *old_stmt, gcall *stmt,
					   profile_count count,
					   cgraph_inline_failed_t reason)
{
  cgraph_node *node;

  if (!get_edge (stmt))
    {
      cgraph_edge *edge = create_edge (callee, stmt, count);
      edge->inline_failed = reason;
    }

  node = clones;
  if (node)
    while (node != this)
      /* Thunk clones do not get updated while copying inline function body.  */
      if (!node->thunk)
	{
	  cgraph_edge *edge = node->get_edge (old_stmt);

	  /* It is possible that clones already contain the edge while
	     master didn't.  Either we promoted indirect call into direct
	     call in the clone or we are processing clones of unreachable
	     master where edges has been removed.  */
	  if (edge)
	    edge = cgraph_edge::set_call_stmt (edge, stmt);
	  else if (! node->get_edge (stmt))
	    {
	      edge = node->create_edge (callee, stmt, count);
	      edge->inline_failed = reason;
	    }

	  if (node->clones)
	    node = node->clones;
	  else if (node->next_sibling_clone)
	    node = node->next_sibling_clone;
	  else
	    {
	      while (node != this && !node->next_sibling_clone)
		node = node->clone_of;
	      if (node != this)
		node = node->next_sibling_clone;
	    }
	}
}

/* Remove the node from cgraph and all inline clones inlined into it.
   Skip however removal of FORBIDDEN_NODE and return true if it needs to be
   removed.  This allows to call the function from outer loop walking clone
   tree.  */

bool
cgraph_node::remove_symbol_and_inline_clones (cgraph_node *forbidden_node)
{
  cgraph_edge *e, *next;
  bool found = false;

  if (this == forbidden_node)
    {
      cgraph_edge::remove (callers);
      return true;
    }
  for (e = callees; e; e = next)
    {
      next = e->next_callee;
      if (!e->inline_failed)
	found |= e->callee->remove_symbol_and_inline_clones (forbidden_node);
    }
  remove ();
  return found;
}

/* The edges representing the callers of the NEW_VERSION node were
   fixed by cgraph_function_versioning (), now the call_expr in their
   respective tree code should be updated to call the NEW_VERSION.  */

static void
update_call_expr (cgraph_node *new_version)
{
  cgraph_edge *e;

  gcc_assert (new_version);

  /* Update the call expr on the edges to call the new version.  */
  for (e = new_version->callers; e; e = e->next_caller)
    {
      function *inner_function = DECL_STRUCT_FUNCTION (e->caller->decl);
      gimple_call_set_fndecl (e->call_stmt, new_version->decl);
      maybe_clean_eh_stmt_fn (inner_function, e->call_stmt);
    }
}


/* Create a new cgraph node which is the new version of
   callgraph node.  REDIRECT_CALLERS holds the callers
   edges which should be redirected to point to
   NEW_VERSION.  ALL the callees edges of the node
   are cloned to the new version node.  Return the new
   version node.

   If non-NULL BLOCK_TO_COPY determine what basic blocks
   was copied to prevent duplications of calls that are dead
   in the clone.  */

cgraph_node *
cgraph_node::create_version_clone (tree new_decl,
				  vec<cgraph_edge *> redirect_callers,
				  bitmap bbs_to_copy,
				  const char *suffix)
 {
   cgraph_node *new_version;
   cgraph_edge *e;
   unsigned i;

   new_version = cgraph_node::create (new_decl);

   new_version->analyzed = analyzed;
   new_version->definition = definition;
   new_version->local = local;
   new_version->externally_visible = false;
   new_version->no_reorder = no_reorder;
   new_version->local = new_version->definition;
   new_version->inlined_to = inlined_to;
   new_version->rtl = rtl;
   new_version->count = count;
   new_version->unit_id = unit_id;
   new_version->merged_comdat = merged_comdat;
   new_version->merged_extern_inline = merged_extern_inline;

   for (e = callees; e; e=e->next_callee)
     if (!bbs_to_copy
	 || bitmap_bit_p (bbs_to_copy, gimple_bb (e->call_stmt)->index))
       e->clone (new_version, e->call_stmt,
		 e->lto_stmt_uid, count, count,
		 true);
   for (e = indirect_calls; e; e=e->next_callee)
     if (!bbs_to_copy
	 || bitmap_bit_p (bbs_to_copy, gimple_bb (e->call_stmt)->index))
       e->clone (new_version, e->call_stmt,
		 e->lto_stmt_uid, count, count,
		 true);
   FOR_EACH_VEC_ELT (redirect_callers, i, e)
     {
       /* Redirect calls to the old version node to point to its new
	  version.  */
       e->redirect_callee (new_version);
     }

   dump_callgraph_transformation (this, new_version, suffix);

   return new_version;
 }

/* Perform function versioning.
   Function versioning includes copying of the tree and
   a callgraph update (creating a new cgraph node and updating
   its callees and callers).

   REDIRECT_CALLERS varray includes the edges to be redirected
   to the new version.

   TREE_MAP is a mapping of tree nodes we want to replace with
   new ones (according to results of prior analysis).

   If non-NULL ARGS_TO_SKIP determine function parameters to remove
   from new version.
   If SKIP_RETURN is true, the new version will return void.
   If non-NULL BLOCK_TO_COPY determine what basic blocks to copy.
   If non_NULL NEW_ENTRY determine new entry BB of the clone.

   If TARGET_ATTRIBUTES is non-null, when creating a new declaration,
   add the attributes to DECL_ATTRIBUTES.  And call valid_attribute_p
   that will promote value of the attribute DECL_FUNCTION_SPECIFIC_TARGET
   of the declaration.

   If VERSION_DECL is set true, use clone_function_name_numbered for the
   function clone.  Otherwise, use clone_function_name.

   Return the new version's cgraph node.  */

cgraph_node *
cgraph_node::create_version_clone_with_body
  (vec<cgraph_edge *> redirect_callers,
   vec<ipa_replace_map *, va_gc> *tree_map,
   ipa_param_adjustments *param_adjustments,
   bitmap bbs_to_copy, basic_block new_entry_block, const char *suffix,
   tree target_attributes, bool version_decl)
{
  tree old_decl = decl;
  cgraph_node *new_version_node = NULL;
  tree new_decl;

  if (!tree_versionable_function_p (old_decl))
    return NULL;

  /* TODO: Restore an assert that we do not change signature if
     can_change_signature is false.  We cannot just check that
     param_adjustments is NULL because unfortunately ipa-split removes return
     values from such functions.  */

  /* Make a new FUNCTION_DECL tree node for the new version. */
  if (param_adjustments)
    new_decl = param_adjustments->adjust_decl (old_decl);
  else
    new_decl = copy_node (old_decl);

  /* Generate a new name for the new version. */
  tree fnname = (version_decl ? clone_function_name_numbered (old_decl, suffix)
		: clone_function_name (old_decl, suffix));
  DECL_NAME (new_decl) = fnname;
  SET_DECL_ASSEMBLER_NAME (new_decl, fnname);
  SET_DECL_RTL (new_decl, NULL);

  DECL_VIRTUAL_P (new_decl) = 0;

  if (target_attributes)
    {
      DECL_ATTRIBUTES (new_decl) = target_attributes;

      location_t saved_loc = input_location;
      tree v = TREE_VALUE (target_attributes);
      input_location = DECL_SOURCE_LOCATION (new_decl);
      bool r;
      tree name_id = get_attribute_name (target_attributes);
      const char *name_str = IDENTIFIER_POINTER (name_id);
      if (strcmp (name_str, "target") == 0)
	r = targetm.target_option.valid_attribute_p (new_decl, name_id, v, 1);
      else if (strcmp (name_str, "target_version") == 0)
	r = targetm.target_option.valid_version_attribute_p (new_decl, name_id,
							     v, 1);
      else
	gcc_unreachable();

      input_location = saved_loc;
      if (!r)
	return NULL;
    }

  /* When the old decl was a con-/destructor make sure the clone isn't.  */
  DECL_STATIC_CONSTRUCTOR (new_decl) = 0;
  DECL_STATIC_DESTRUCTOR (new_decl) = 0;
  DECL_SET_IS_OPERATOR_NEW (new_decl, 0);
  DECL_SET_IS_OPERATOR_DELETE (new_decl, 0);
  DECL_IS_REPLACEABLE_OPERATOR (new_decl) = 0;

  /* Create the new version's call-graph node.
     and update the edges of the new node. */
  new_version_node = create_version_clone (new_decl, redirect_callers,
					  bbs_to_copy, suffix);

  if (ipa_transforms_to_apply.exists ())
    new_version_node->ipa_transforms_to_apply
      = ipa_transforms_to_apply.copy ();
  /* Copy the OLD_VERSION_NODE function tree to the new version.  */
  tree_function_versioning (old_decl, new_decl, tree_map, param_adjustments,
			    false, bbs_to_copy, new_entry_block);

  /* Update the new version's properties.
     Make The new version visible only within this translation unit.  Make sure
     that is not weak also.
     ??? We cannot use COMDAT linkage because there is no
     ABI support for this.  */
  new_version_node->make_decl_local ();
  DECL_VIRTUAL_P (new_version_node->decl) = 0;
  new_version_node->externally_visible = 0;
  new_version_node->local = 1;
  new_version_node->lowered = true;
  if (!implicit_section)
    new_version_node->set_section (*this);
  /* Clones of global symbols or symbols with unique names are unique.  */
  if ((TREE_PUBLIC (old_decl)
       && !DECL_EXTERNAL (old_decl)
       && !DECL_WEAK (old_decl)
       && !DECL_COMDAT (old_decl))
      || in_lto_p)
    new_version_node->unique_name = true;

  /* Update the call_expr on the edges to call the new version node. */
  update_call_expr (new_version_node);

  symtab->call_cgraph_insertion_hooks (new_version_node);
  return new_version_node;
}

/* Remove the node from the tree of virtual and inline clones and make it a
   standalone node - not a clone any more.  */

void cgraph_node::remove_from_clone_tree ()
{
  if (next_sibling_clone)
    next_sibling_clone->prev_sibling_clone = prev_sibling_clone;
  if (prev_sibling_clone)
    prev_sibling_clone->next_sibling_clone = next_sibling_clone;
  else
    clone_of->clones = next_sibling_clone;
  next_sibling_clone = NULL;
  prev_sibling_clone = NULL;
  clone_of = NULL;
}

/* Given virtual clone, turn it into actual clone.  */

void
cgraph_node::materialize_clone ()
{
  clone_info *info = clone_info::get (this);
  clone_of->get_untransformed_body ();
  former_clone_of = clone_of->decl;
  if (clone_of->former_clone_of)
    former_clone_of = clone_of->former_clone_of;
  if (symtab->dump_file)
    {
      fprintf (symtab->dump_file, "cloning %s to %s\n",
	       clone_of->dump_name (),
	       dump_name ());
      if (info && info->tree_map)
        {
	  fprintf (symtab->dump_file, "    replace map:");
	  for (unsigned int i = 0;
	       i < vec_safe_length (info->tree_map);
	       i++)
	    {
	      ipa_replace_map *replace_info;
	      replace_info = (*info->tree_map)[i];
	      fprintf (symtab->dump_file, "%s %i -> ",
		       i ? "," : "", replace_info->parm_num);
	      print_generic_expr (symtab->dump_file,
				  replace_info->new_tree);
	    }
	  fprintf (symtab->dump_file, "\n");
	}
      if (info && info->param_adjustments)
	info->param_adjustments->dump (symtab->dump_file);
    }
  clear_stmts_in_references ();
  /* Copy the OLD_VERSION_NODE function tree to the new version.  */
  tree_function_versioning (clone_of->decl, decl,
			    info ? info->tree_map : NULL,
			    info ? info->param_adjustments : NULL,
			    true, NULL, NULL);
  if (symtab->dump_file)
    {
      dump_function_to_file (clone_of->decl, symtab->dump_file,
			     dump_flags);
      dump_function_to_file (decl, symtab->dump_file, dump_flags);
    }

  cgraph_node *this_clone_of = clone_of;
  /* Function is no longer clone.  */
  remove_from_clone_tree ();
  if (!this_clone_of->analyzed && !this_clone_of->clones)
    this_clone_of->release_body ();
}

#include "gt-cgraphclones.h"
