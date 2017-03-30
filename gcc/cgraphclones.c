/* Callgraph clones
   Copyright (C) 2003-2017 Free Software Foundation, Inc.
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

/* This module provide facilities for clonning functions. I.e. creating
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
#include "tree-dump.h"
#include "gimple-pretty-print.h"

/* Create clone of edge in the node N represented by CALL_EXPR
   the callgraph.  */

cgraph_edge *
cgraph_edge::clone (cgraph_node *n, gcall *call_stmt, unsigned stmt_uid,
		    gcov_type count_scale, int freq_scale, bool update_original)
{
  cgraph_edge *new_edge;
  gcov_type gcov_count = apply_probability (count, count_scale);
  gcov_type freq;

  /* We do not want to ignore loop nest after frequency drops to 0.  */
  if (!freq_scale)
    freq_scale = 1;
  freq = frequency * (gcov_type) freq_scale / CGRAPH_FREQ_BASE;
  if (freq > CGRAPH_FREQ_MAX)
    freq = CGRAPH_FREQ_MAX;

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
	  new_edge = n->create_edge (callee, call_stmt, gcov_count, freq);
	}
      else
	{
	  new_edge = n->create_indirect_edge (call_stmt,
					      indirect_info->ecf_flags,
					      count, freq, false);
	  *new_edge->indirect_info = *indirect_info;
	}
    }
  else
    {
      new_edge = n->create_edge (callee, call_stmt, gcov_count, freq);
      if (indirect_info)
	{
	  new_edge->indirect_info
	    = ggc_cleared_alloc<cgraph_indirect_call_info> ();
	  *new_edge->indirect_info = *indirect_info;
	}
    }

  new_edge->inline_failed = inline_failed;
  new_edge->indirect_inlining_edge = indirect_inlining_edge;
  new_edge->lto_stmt_uid = stmt_uid;
  /* Clone flags that depend on call_stmt availability manually.  */
  new_edge->can_throw_external = can_throw_external;
  new_edge->call_stmt_cannot_inline_p = call_stmt_cannot_inline_p;
  new_edge->speculative = speculative;
  new_edge->in_polymorphic_cdtor = in_polymorphic_cdtor;
  if (update_original)
    {
      count -= new_edge->count;
      if (count < 0)
	count = 0;
    }
  symtab->call_edge_duplication_hooks (this, new_edge);
  return new_edge;
}

/* Build variant of function type ORIG_TYPE skipping ARGS_TO_SKIP and the
   return value if SKIP_RETURN is true.  */

tree
cgraph_build_function_type_skip_args (tree orig_type, bitmap args_to_skip,
				      bool skip_return)
{
  tree new_type = NULL;
  tree args, new_args = NULL;
  tree new_reversed;
  int i = 0;

  for (args = TYPE_ARG_TYPES (orig_type); args && args != void_list_node;
       args = TREE_CHAIN (args), i++)
    if (!args_to_skip || !bitmap_bit_p (args_to_skip, i))
      new_args = tree_cons (NULL_TREE, TREE_VALUE (args), new_args);

  new_reversed = nreverse (new_args);
  if (args)
    {
      if (new_reversed)
        TREE_CHAIN (new_args) = void_list_node;
      else
	new_reversed = void_list_node;
    }

  /* Use copy_node to preserve as much as possible from original type
     (debug info, attribute lists etc.)
     Exception is METHOD_TYPEs must have THIS argument.
     When we are asked to remove it, we need to build new FUNCTION_TYPE
     instead.  */
  if (TREE_CODE (orig_type) != METHOD_TYPE
      || !args_to_skip
      || !bitmap_bit_p (args_to_skip, 0))
    {
      new_type = build_distinct_type_copy (orig_type);
      TYPE_ARG_TYPES (new_type) = new_reversed;
    }
  else
    {
      new_type
        = build_distinct_type_copy (build_function_type (TREE_TYPE (orig_type),
							 new_reversed));
      TYPE_CONTEXT (new_type) = TYPE_CONTEXT (orig_type);
    }

  if (skip_return)
    TREE_TYPE (new_type) = void_type_node;

  return new_type;
}

/* Build variant of function decl ORIG_DECL skipping ARGS_TO_SKIP and the
   return value if SKIP_RETURN is true.

   Arguments from DECL_ARGUMENTS list can't be removed now, since they are
   linked by TREE_CHAIN directly.  The caller is responsible for eliminating
   them when they are being duplicated (i.e. copy_arguments_for_versioning).  */

static tree
build_function_decl_skip_args (tree orig_decl, bitmap args_to_skip,
			       bool skip_return)
{
  tree new_decl = copy_node (orig_decl);
  tree new_type;

  new_type = TREE_TYPE (orig_decl);
  if (prototype_p (new_type)
      || (skip_return && !VOID_TYPE_P (TREE_TYPE (new_type))))
    new_type
      = cgraph_build_function_type_skip_args (new_type, args_to_skip,
					      skip_return);
  TREE_TYPE (new_decl) = new_type;

  /* For declarations setting DECL_VINDEX (i.e. methods)
     we expect first argument to be THIS pointer.   */
  if (args_to_skip && bitmap_bit_p (args_to_skip, 0))
    DECL_VINDEX (new_decl) = NULL_TREE;

  /* When signature changes, we need to clear builtin info.  */
  if (DECL_BUILT_IN (new_decl)
      && args_to_skip
      && !bitmap_empty_p (args_to_skip))
    {
      DECL_BUILT_IN_CLASS (new_decl) = NOT_BUILT_IN;
      DECL_FUNCTION_CODE (new_decl) = (enum built_in_function) 0;
    }
  /* The FE might have information and assumptions about the other
     arguments.  */
  DECL_LANG_SPECIFIC (new_decl) = NULL;
  return new_decl;
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

  new_node->externally_visible = 0;
  new_node->local.local = 1;
  new_node->lowered = true;
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

  if (thunk_of->thunk.thunk_p)
    node = duplicate_thunk_for_node (thunk_of, node);

  if (!DECL_ARGUMENTS (thunk->decl))
    thunk->get_untransformed_body ();

  cgraph_edge *cs;
  for (cs = node->callers; cs; cs = cs->next_caller)
    if (cs->caller->thunk.thunk_p
	&& cs->caller->thunk.this_adjusting == thunk->thunk.this_adjusting
	&& cs->caller->thunk.fixed_offset == thunk->thunk.fixed_offset
	&& cs->caller->thunk.virtual_offset_p == thunk->thunk.virtual_offset_p
	&& cs->caller->thunk.virtual_value == thunk->thunk.virtual_value)
      return cs->caller;

  tree new_decl;
  if (!node->clone.args_to_skip)
    new_decl = copy_node (thunk->decl);
  else
    {
      /* We do not need to duplicate this_adjusting thunks if we have removed
	 this.  */
      if (thunk->thunk.this_adjusting
	  && bitmap_bit_p (node->clone.args_to_skip, 0))
	return node;

      new_decl = build_function_decl_skip_args (thunk->decl,
						node->clone.args_to_skip,
						false);
    }

  tree *link = &DECL_ARGUMENTS (new_decl);
  int i = 0;
  for (tree pd = DECL_ARGUMENTS (thunk->decl); pd; pd = DECL_CHAIN (pd), i++)
    {
      if (!node->clone.args_to_skip
	  || !bitmap_bit_p (node->clone.args_to_skip, i))
	{
	  tree nd = copy_node (pd);
	  DECL_CONTEXT (nd) = new_decl;
	  *link = nd;
	  link = &DECL_CHAIN (nd);
	}
    }
  *link = NULL_TREE;

  gcc_checking_assert (!DECL_STRUCT_FUNCTION (new_decl));
  gcc_checking_assert (!DECL_INITIAL (new_decl));
  gcc_checking_assert (!DECL_RESULT (new_decl));
  gcc_checking_assert (!DECL_RTL_SET_P (new_decl));

  DECL_NAME (new_decl) = clone_function_name (thunk->decl, "artificial_thunk");
  SET_DECL_ASSEMBLER_NAME (new_decl, DECL_NAME (new_decl));

  new_thunk = cgraph_node::create (new_decl);
  set_new_clone_decl_and_node_flags (new_thunk);
  new_thunk->definition = true;
  new_thunk->local.can_change_signature = node->local.can_change_signature;
  new_thunk->thunk = thunk->thunk;
  new_thunk->unique_name = in_lto_p;
  new_thunk->former_clone_of = thunk->decl;
  new_thunk->clone.args_to_skip = node->clone.args_to_skip;
  new_thunk->clone.combined_args_to_skip = node->clone.combined_args_to_skip;

  cgraph_edge *e = new_thunk->create_edge (node, NULL, 0,
						  CGRAPH_FREQ_BASE);
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
  if (orig_to->thunk.thunk_p)
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
    if (e->caller->thunk.thunk_p)
      {
	cgraph_node *thunk = e->caller;

	e = e->next_caller;
	if (thunk->expand_thunk (false, false))
	  {
	    thunk->thunk.thunk_p = false;
	    thunk->analyze ();
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

/* Create node representing clone of N executed COUNT times.  Decrease
   the execution counts from original node too.
   The new clone will have decl set to DECL that may or may not be the same
   as decl of N.

   When UPDATE_ORIGINAL is true, the counts are subtracted from the original
   function's profile to reflect the fact that part of execution is handled
   by node.  
   When CALL_DUPLICATOIN_HOOK is true, the ipa passes are acknowledged about
   the new clone. Otherwise the caller is responsible for doing so later.

   If the new node is being inlined into another one, NEW_INLINED_TO should be
   the outline function the new one is (even indirectly) inlined to.  All hooks
   will see this in node's global.inlined_to, when invoked.  Can be NULL if the
   node is not inlined.  */

cgraph_node *
cgraph_node::create_clone (tree new_decl, gcov_type gcov_count, int freq,
			   bool update_original,
			   vec<cgraph_edge *> redirect_callers,
			   bool call_duplication_hook,
			   cgraph_node *new_inlined_to,
			   bitmap args_to_skip, const char *suffix)
{
  cgraph_node *new_node = symtab->create_empty ();
  cgraph_edge *e;
  gcov_type count_scale;
  unsigned i;

  if (new_inlined_to)
    dump_callgraph_transformation (this, new_inlined_to, "inlining to");

  new_node->decl = new_decl;
  new_node->register_symbol ();
  new_node->origin = origin;
  new_node->lto_file_data = lto_file_data;
  if (new_node->origin)
    {
      new_node->next_nested = new_node->origin->nested;
      new_node->origin->nested = new_node;
    }
  new_node->analyzed = analyzed;
  new_node->definition = definition;
  new_node->local = local;
  new_node->externally_visible = false;
  new_node->no_reorder = no_reorder;
  new_node->local.local = true;
  new_node->global = global;
  new_node->global.inlined_to = new_inlined_to;
  new_node->rtl = rtl;
  new_node->count = count;
  new_node->frequency = frequency;
  new_node->tp_first_run = tp_first_run;
  new_node->tm_clone = tm_clone;
  new_node->icf_merged = icf_merged;
  new_node->merged_comdat = merged_comdat;
  new_node->thunk = thunk;

  new_node->clone.tree_map = NULL;
  new_node->clone.args_to_skip = args_to_skip;
  new_node->split_part = split_part;
  if (!args_to_skip)
    new_node->clone.combined_args_to_skip = clone.combined_args_to_skip;
  else if (clone.combined_args_to_skip)
    {
      new_node->clone.combined_args_to_skip = BITMAP_GGC_ALLOC ();
      bitmap_ior (new_node->clone.combined_args_to_skip,
		  clone.combined_args_to_skip, args_to_skip);
    }
  else
    new_node->clone.combined_args_to_skip = args_to_skip;

  if (count)
    {
      if (new_node->count > count)
        count_scale = REG_BR_PROB_BASE;
      else
	count_scale = GCOV_COMPUTE_SCALE (new_node->count, count);
    }
  else
    count_scale = 0;
  if (update_original)
    {
      count -= gcov_count;
      if (count < 0)
	count = 0;
    }

  FOR_EACH_VEC_ELT (redirect_callers, i, e)
    {
      /* Redirect calls to the old version node to point to its new
	 version.  The only exception is when the edge was proved to
	 be unreachable during the clonning procedure.  */
      if (!e->callee
	  || DECL_BUILT_IN_CLASS (e->callee->decl) != BUILT_IN_NORMAL
	  || DECL_FUNCTION_CODE (e->callee->decl) != BUILT_IN_UNREACHABLE)
        e->redirect_callee_duplicating_thunks (new_node);
    }
  new_node->expand_all_artificial_thunks ();

  for (e = callees;e; e=e->next_callee)
    e->clone (new_node, e->call_stmt, e->lto_stmt_uid, count_scale,
	      freq, update_original);

  for (e = indirect_calls; e; e = e->next_callee)
    e->clone (new_node, e->call_stmt, e->lto_stmt_uid,
	      count_scale, freq, update_original);
  new_node->clone_references (this);

  new_node->next_sibling_clone = clones;
  if (clones)
    clones->prev_sibling_clone = new_node;
  clones = new_node;
  new_node->clone_of = this;

  if (call_duplication_hook)
    symtab->call_cgraph_duplication_hooks (this, new_node);

  if (!new_inlined_to)
    dump_callgraph_transformation (this, new_node, suffix);

  return new_node;
}

static GTY(()) unsigned int clone_fn_id_num;

/* Return a new assembler name for a clone with SUFFIX of a decl named
   NAME.  */

tree
clone_function_name_1 (const char *name, const char *suffix)
{
  size_t len = strlen (name);
  char *tmp_name, *prefix;

  prefix = XALLOCAVEC (char, len + strlen (suffix) + 2);
  memcpy (prefix, name, len);
  strcpy (prefix + len + 1, suffix);
  prefix[len] = symbol_table::symbol_suffix_separator ();
  ASM_FORMAT_PRIVATE_NAME (tmp_name, prefix, clone_fn_id_num++);
  return get_identifier (tmp_name);
}

/* Return a new assembler name for a clone of DECL with SUFFIX.  */

tree
clone_function_name (tree decl, const char *suffix)
{
  tree name = DECL_ASSEMBLER_NAME (decl);
  return clone_function_name_1 (IDENTIFIER_POINTER (name), suffix);
}


/* Create callgraph node clone with new declaration.  The actual body will
   be copied later at compilation stage.

   TODO: after merging in ipa-sra use function call notes instead of args_to_skip
   bitmap interface.
   */
cgraph_node *
cgraph_node::create_virtual_clone (vec<cgraph_edge *> redirect_callers,
				   vec<ipa_replace_map *, va_gc> *tree_map,
				   bitmap args_to_skip, const char * suffix)
{
  tree old_decl = decl;
  cgraph_node *new_node = NULL;
  tree new_decl;
  size_t len, i;
  ipa_replace_map *map;
  char *name;

  gcc_checking_assert (local.versionable);
  gcc_assert (local.can_change_signature || !args_to_skip);

  /* Make a new FUNCTION_DECL tree node */
  if (!args_to_skip)
    new_decl = copy_node (old_decl);
  else
    new_decl = build_function_decl_skip_args (old_decl, args_to_skip, false);

  /* These pointers represent function body and will be populated only when clone
     is materialized.  */
  gcc_assert (new_decl != old_decl);
  DECL_STRUCT_FUNCTION (new_decl) = NULL;
  DECL_ARGUMENTS (new_decl) = NULL;
  DECL_INITIAL (new_decl) = NULL;
  DECL_RESULT (new_decl) = NULL; 
  /* We can not do DECL_RESULT (new_decl) = NULL; here because of LTO partitioning
     sometimes storing only clone decl instead of original.  */

  /* Generate a new name for the new version. */
  len = IDENTIFIER_LENGTH (DECL_NAME (old_decl));
  name = XALLOCAVEC (char, len + strlen (suffix) + 2);
  memcpy (name, IDENTIFIER_POINTER (DECL_NAME (old_decl)), len);
  strcpy (name + len + 1, suffix);
  name[len] = '.';
  DECL_NAME (new_decl) = get_identifier (name);
  SET_DECL_ASSEMBLER_NAME (new_decl, clone_function_name (old_decl, suffix));
  SET_DECL_RTL (new_decl, NULL);

  new_node = create_clone (new_decl, count, CGRAPH_FREQ_BASE, false,
			   redirect_callers, false, NULL, args_to_skip, suffix);

  /* Update the properties.
     Make clone visible only within this translation unit.  Make sure
     that is not weak also.
     ??? We cannot use COMDAT linkage because there is no
     ABI support for this.  */
  set_new_clone_decl_and_node_flags (new_node);
  new_node->clone.tree_map = tree_map;
  if (!implicit_section)
    new_node->set_section (get_section ());

  /* Clones of global symbols or symbols with unique names are unique.  */
  if ((TREE_PUBLIC (old_decl)
       && !DECL_EXTERNAL (old_decl)
       && !DECL_WEAK (old_decl)
       && !DECL_COMDAT (old_decl))
      || in_lto_p)
    new_node->unique_name = true;
  FOR_EACH_VEC_SAFE_ELT (tree_map, i, map)
    new_node->maybe_create_reference (map->new_tree, NULL);

  if (ipa_transforms_to_apply.exists ())
    new_node->ipa_transforms_to_apply
      = ipa_transforms_to_apply.copy ();

  symtab->call_cgraph_duplication_hooks (this, new_node);

  return new_node;
}

/* callgraph node being removed from symbol table; see if its entry can be
   replaced by other inline clone.  */
cgraph_node *
cgraph_node::find_replacement (void)
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
      next_inline_clone->clone = clone;

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
  cgraph_edge *edge = get_edge (old_stmt);

  if (edge)
    edge->set_call_stmt (new_stmt, update_speculative);

  node = clones;
  if (node)
    while (node != this)
      {
	cgraph_edge *edge = node->get_edge (old_stmt);
	if (edge)
	  {
	    edge->set_call_stmt (new_stmt, update_speculative);
	    /* If UPDATE_SPECULATIVE is false, it means that we are turning
	       speculative call into a real code sequence.  Update the
	       callgraph edges.  */
	    if (edge->speculative && !update_speculative)
	      {
		cgraph_edge *direct, *indirect;
		ipa_ref *ref;

		gcc_assert (!edge->indirect_unknown_callee);
		edge->speculative_call_info (direct, indirect, ref);
		direct->speculative = false;
		indirect->speculative = false;
		ref->speculative = false;
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
					   gcov_type count,
					   int freq,
					   cgraph_inline_failed_t reason)
{
  cgraph_node *node;
  cgraph_edge *edge;

  if (!get_edge (stmt))
    {
      edge = create_edge (callee, stmt, count, freq);
      edge->inline_failed = reason;
    }

  node = clones;
  if (node)
    while (node != this)
      /* Thunk clones do not get updated while copying inline function body.  */
      if (!node->thunk.thunk_p)
	{
	  cgraph_edge *edge = node->get_edge (old_stmt);

	  /* It is possible that clones already contain the edge while
	     master didn't.  Either we promoted indirect call into direct
	     call in the clone or we are processing clones of unreachable
	     master where edges has been removed.  */
	  if (edge)
	    edge->set_call_stmt (stmt);
	  else if (! node->get_edge (stmt))
	    {
	      edge = node->create_edge (callee, stmt, count, freq);
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
      callers->remove ();
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
   new_version->local.local = new_version->definition;
   new_version->global = global;
   new_version->rtl = rtl;
   new_version->count = count;

   for (e = callees; e; e=e->next_callee)
     if (!bbs_to_copy
	 || bitmap_bit_p (bbs_to_copy, gimple_bb (e->call_stmt)->index))
       e->clone (new_version, e->call_stmt,
		 e->lto_stmt_uid, REG_BR_PROB_BASE,
		 CGRAPH_FREQ_BASE,
		 true);
   for (e = indirect_calls; e; e=e->next_callee)
     if (!bbs_to_copy
	 || bitmap_bit_p (bbs_to_copy, gimple_bb (e->call_stmt)->index))
       e->clone (new_version, e->call_stmt,
		 e->lto_stmt_uid, REG_BR_PROB_BASE,
		 CGRAPH_FREQ_BASE,
		 true);
   FOR_EACH_VEC_ELT (redirect_callers, i, e)
     {
       /* Redirect calls to the old version node to point to its new
	  version.  */
       e->redirect_callee (new_version);
     }

   symtab->call_cgraph_duplication_hooks (this, new_version);

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

   Return the new version's cgraph node.  */

cgraph_node *
cgraph_node::create_version_clone_with_body
  (vec<cgraph_edge *> redirect_callers,
   vec<ipa_replace_map *, va_gc> *tree_map, bitmap args_to_skip,
   bool skip_return, bitmap bbs_to_copy, basic_block new_entry_block,
   const char *suffix)
{
  tree old_decl = decl;
  cgraph_node *new_version_node = NULL;
  tree new_decl;

  if (!tree_versionable_function_p (old_decl))
    return NULL;

  gcc_assert (local.can_change_signature || !args_to_skip);

  /* Make a new FUNCTION_DECL tree node for the new version. */
  if (!args_to_skip && !skip_return)
    new_decl = copy_node (old_decl);
  else
    new_decl
      = build_function_decl_skip_args (old_decl, args_to_skip, skip_return);

  /* Generate a new name for the new version. */
  DECL_NAME (new_decl) = clone_function_name (old_decl, suffix);
  SET_DECL_ASSEMBLER_NAME (new_decl, DECL_NAME (new_decl));
  SET_DECL_RTL (new_decl, NULL);

  /* When the old decl was a con-/destructor make sure the clone isn't.  */
  DECL_STATIC_CONSTRUCTOR (new_decl) = 0;
  DECL_STATIC_DESTRUCTOR (new_decl) = 0;

  /* Create the new version's call-graph node.
     and update the edges of the new node. */
  new_version_node = create_version_clone (new_decl, redirect_callers,
					  bbs_to_copy, suffix);

  if (ipa_transforms_to_apply.exists ())
    new_version_node->ipa_transforms_to_apply
      = ipa_transforms_to_apply.copy ();
  /* Copy the OLD_VERSION_NODE function tree to the new version.  */
  tree_function_versioning (old_decl, new_decl, tree_map, false, args_to_skip,
			    skip_return, bbs_to_copy, new_entry_block);

  /* Update the new version's properties.
     Make The new version visible only within this translation unit.  Make sure
     that is not weak also.
     ??? We cannot use COMDAT linkage because there is no
     ABI support for this.  */
  new_version_node->make_decl_local ();
  DECL_VIRTUAL_P (new_version_node->decl) = 0;
  new_version_node->externally_visible = 0;
  new_version_node->local.local = 1;
  new_version_node->lowered = true;
  if (!implicit_section)
    new_version_node->set_section (get_section ());
  /* Clones of global symbols or symbols with unique names are unique.  */
  if ((TREE_PUBLIC (old_decl)
       && !DECL_EXTERNAL (old_decl)
       && !DECL_WEAK (old_decl)
       && !DECL_COMDAT (old_decl))
      || in_lto_p)
    new_version_node->unique_name = true;

  /* Update the call_expr on the edges to call the new version node. */
  update_call_expr (new_version_node);

  symtab->call_cgraph_insertion_hooks (this);
  return new_version_node;
}

/* Given virtual clone, turn it into actual clone.  */

static void
cgraph_materialize_clone (cgraph_node *node)
{
  bitmap_obstack_initialize (NULL);
  node->former_clone_of = node->clone_of->decl;
  if (node->clone_of->former_clone_of)
    node->former_clone_of = node->clone_of->former_clone_of;
  /* Copy the OLD_VERSION_NODE function tree to the new version.  */
  tree_function_versioning (node->clone_of->decl, node->decl,
  			    node->clone.tree_map, true,
			    node->clone.args_to_skip, false,
			    NULL, NULL);
  if (symtab->dump_file)
    {
      dump_function_to_file (node->clone_of->decl, symtab->dump_file,
			     dump_flags);
      dump_function_to_file (node->decl, symtab->dump_file, dump_flags);
    }

  /* Function is no longer clone.  */
  if (node->next_sibling_clone)
    node->next_sibling_clone->prev_sibling_clone = node->prev_sibling_clone;
  if (node->prev_sibling_clone)
    node->prev_sibling_clone->next_sibling_clone = node->next_sibling_clone;
  else
    node->clone_of->clones = node->next_sibling_clone;
  node->next_sibling_clone = NULL;
  node->prev_sibling_clone = NULL;
  if (!node->clone_of->analyzed && !node->clone_of->clones)
    {
      node->clone_of->release_body ();
      node->clone_of->remove_callees ();
      node->clone_of->remove_all_references ();
    }
  node->clone_of = NULL;
  bitmap_obstack_release (NULL);
}

/* Once all functions from compilation unit are in memory, produce all clones
   and update all calls.  We might also do this on demand if we don't want to
   bring all functions to memory prior compilation, but current WHOPR
   implementation does that and it is a bit easier to keep everything right in
   this order.  */

void
symbol_table::materialize_all_clones (void)
{
  cgraph_node *node;
  bool stabilized = false;
  

  if (symtab->dump_file)
    fprintf (symtab->dump_file, "Materializing clones\n");

  cgraph_node::checking_verify_cgraph_nodes ();

  /* We can also do topological order, but number of iterations should be
     bounded by number of IPA passes since single IPA pass is probably not
     going to create clones of clones it created itself.  */
  while (!stabilized)
    {
      stabilized = true;
      FOR_EACH_FUNCTION (node)
        {
	  if (node->clone_of && node->decl != node->clone_of->decl
	      && !gimple_has_body_p (node->decl))
	    {
	      if (!node->clone_of->clone_of)
		node->clone_of->get_untransformed_body ();
	      if (gimple_has_body_p (node->clone_of->decl))
	        {
		  if (symtab->dump_file)
		    {
		      fprintf (symtab->dump_file, "cloning %s to %s\n",
			       xstrdup_for_dump (node->clone_of->name ()),
			       xstrdup_for_dump (node->name ()));
		      if (node->clone.tree_map)
		        {
			  unsigned int i;
			  fprintf (symtab->dump_file, "   replace map: ");
			  for (i = 0;
			       i < vec_safe_length (node->clone.tree_map);
			       i++)
			    {
			      ipa_replace_map *replace_info;
			      replace_info = (*node->clone.tree_map)[i];
			      print_generic_expr (symtab->dump_file, replace_info->old_tree, 0);
			      fprintf (symtab->dump_file, " -> ");
			      print_generic_expr (symtab->dump_file, replace_info->new_tree, 0);
			      fprintf (symtab->dump_file, "%s%s;",
			      	       replace_info->replace_p ? "(replace)":"",
				       replace_info->ref_p ? "(ref)":"");
			    }
			  fprintf (symtab->dump_file, "\n");
			}
		      if (node->clone.args_to_skip)
			{
			  fprintf (symtab->dump_file, "   args_to_skip: ");
			  dump_bitmap (symtab->dump_file,
				       node->clone.args_to_skip);
			}
		      if (node->clone.args_to_skip)
			{
			  fprintf (symtab->dump_file, "   combined_args_to_skip:");
			  dump_bitmap (symtab->dump_file, node->clone.combined_args_to_skip);
			}
		    }
		  cgraph_materialize_clone (node);
		  stabilized = false;
	        }
	    }
	}
    }
  FOR_EACH_FUNCTION (node)
    if (!node->analyzed && node->callees)
      {
	node->remove_callees ();
	node->remove_all_references ();
      }
    else
      node->clear_stmts_in_references ();
  if (symtab->dump_file)
    fprintf (symtab->dump_file, "Materialization Call site updates done.\n");

  cgraph_node::checking_verify_cgraph_nodes ();

  symtab->remove_unreachable_nodes (symtab->dump_file);
}

#include "gt-cgraphclones.h"
