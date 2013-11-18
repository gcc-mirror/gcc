/* Callgraph clones
   Copyright (C) 2003-2013 Free Software Foundation, Inc.
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
#include "tm.h"
#include "tree.h"
#include "gimple.h"
#include "rtl.h"
#include "bitmap.h"
#include "tree-cfg.h"
#include "tree-inline.h"
#include "langhooks.h"
#include "pointer-set.h"
#include "toplev.h"
#include "flags.h"
#include "ggc.h"
#include "debug.h"
#include "target.h"
#include "diagnostic.h"
#include "params.h"
#include "intl.h"
#include "function.h"
#include "ipa-prop.h"
#include "tree-iterator.h"
#include "tree-dump.h"
#include "gimple-pretty-print.h"
#include "coverage.h"
#include "ipa-inline.h"
#include "ipa-utils.h"
#include "lto-streamer.h"
#include "except.h"

/* Create clone of E in the node N represented by CALL_EXPR the callgraph.  */
struct cgraph_edge *
cgraph_clone_edge (struct cgraph_edge *e, struct cgraph_node *n,
		   gimple call_stmt, unsigned stmt_uid, gcov_type count_scale,
		   int freq_scale, bool update_original)
{
  struct cgraph_edge *new_edge;
  gcov_type count = apply_probability (e->count, count_scale);
  gcov_type freq;

  /* We do not want to ignore loop nest after frequency drops to 0.  */
  if (!freq_scale)
    freq_scale = 1;
  freq = e->frequency * (gcov_type) freq_scale / CGRAPH_FREQ_BASE;
  if (freq > CGRAPH_FREQ_MAX)
    freq = CGRAPH_FREQ_MAX;

  if (e->indirect_unknown_callee)
    {
      tree decl;

      if (call_stmt && (decl = gimple_call_fndecl (call_stmt)))
	{
	  struct cgraph_node *callee = cgraph_get_node (decl);
	  gcc_checking_assert (callee);
	  new_edge = cgraph_create_edge (n, callee, call_stmt, count, freq);
	}
      else
	{
	  new_edge = cgraph_create_indirect_edge (n, call_stmt,
						  e->indirect_info->ecf_flags,
						  count, freq);
	  *new_edge->indirect_info = *e->indirect_info;
	}
    }
  else
    {
      new_edge = cgraph_create_edge (n, e->callee, call_stmt, count, freq);
      if (e->indirect_info)
	{
	  new_edge->indirect_info
	    = ggc_alloc_cleared_cgraph_indirect_call_info ();
	  *new_edge->indirect_info = *e->indirect_info;
	}
    }

  new_edge->inline_failed = e->inline_failed;
  new_edge->indirect_inlining_edge = e->indirect_inlining_edge;
  new_edge->lto_stmt_uid = stmt_uid;
  /* Clone flags that depend on call_stmt availability manually.  */
  new_edge->can_throw_external = e->can_throw_external;
  new_edge->call_stmt_cannot_inline_p = e->call_stmt_cannot_inline_p;
  new_edge->speculative = e->speculative;
  if (update_original)
    {
      e->count -= new_edge->count;
      if (e->count < 0)
	e->count = 0;
    }
  cgraph_call_edge_duplication_hooks (e, new_edge);
  return new_edge;
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

struct cgraph_node *
cgraph_clone_node (struct cgraph_node *n, tree decl, gcov_type count, int freq,
		   bool update_original,
		   vec<cgraph_edge_p> redirect_callers,
		   bool call_duplication_hook,
		   struct cgraph_node *new_inlined_to)
{
  struct cgraph_node *new_node = cgraph_create_empty_node ();
  struct cgraph_edge *e;
  gcov_type count_scale;
  unsigned i;

  new_node->decl = decl;
  symtab_register_node (new_node);
  new_node->origin = n->origin;
  new_node->lto_file_data = n->lto_file_data;
  if (new_node->origin)
    {
      new_node->next_nested = new_node->origin->nested;
      new_node->origin->nested = new_node;
    }
  new_node->analyzed = n->analyzed;
  new_node->definition = n->definition;
  new_node->local = n->local;
  new_node->externally_visible = false;
  new_node->local.local = true;
  new_node->global = n->global;
  new_node->global.inlined_to = new_inlined_to;
  new_node->rtl = n->rtl;
  new_node->count = count;
  new_node->frequency = n->frequency;
  new_node->clone = n->clone;
  new_node->clone.tree_map = NULL;
  new_node->tp_first_run = n->tp_first_run;
  if (n->count)
    {
      if (new_node->count > n->count)
        count_scale = REG_BR_PROB_BASE;
      else
        count_scale = GCOV_COMPUTE_SCALE (new_node->count, n->count);
    }
  else
    count_scale = 0;
  if (update_original)
    {
      n->count -= count;
      if (n->count < 0)
	n->count = 0;
    }

  FOR_EACH_VEC_ELT (redirect_callers, i, e)
    {
      /* Redirect calls to the old version node to point to its new
	 version.  */
      cgraph_redirect_edge_callee (e, new_node);
    }


  for (e = n->callees;e; e=e->next_callee)
    cgraph_clone_edge (e, new_node, e->call_stmt, e->lto_stmt_uid,
		       count_scale, freq, update_original);

  for (e = n->indirect_calls; e; e = e->next_callee)
    cgraph_clone_edge (e, new_node, e->call_stmt, e->lto_stmt_uid,
		       count_scale, freq, update_original);
  ipa_clone_references (new_node, &n->ref_list);

  new_node->next_sibling_clone = n->clones;
  if (n->clones)
    n->clones->prev_sibling_clone = new_node;
  n->clones = new_node;
  new_node->clone_of = n;

  if (call_duplication_hook)
    cgraph_call_node_duplication_hooks (n, new_node);
  return new_node;
}

/* Return a new assembler name for a clone of DECL with SUFFIX.  */

static GTY(()) unsigned int clone_fn_id_num;

tree
clone_function_name (tree decl, const char *suffix)
{
  tree name = DECL_ASSEMBLER_NAME (decl);
  size_t len = IDENTIFIER_LENGTH (name);
  char *tmp_name, *prefix;

  prefix = XALLOCAVEC (char, len + strlen (suffix) + 2);
  memcpy (prefix, IDENTIFIER_POINTER (name), len);
  strcpy (prefix + len + 1, suffix);
#ifndef NO_DOT_IN_LABEL
  prefix[len] = '.';
#elif !defined NO_DOLLAR_IN_LABEL
  prefix[len] = '$';
#else
  prefix[len] = '_';
#endif
  ASM_FORMAT_PRIVATE_NAME (tmp_name, prefix, clone_fn_id_num++);
  return get_identifier (tmp_name);
}

/* Build variant of function type ORIG_TYPE skipping ARGS_TO_SKIP and the
   return value if SKIP_RETURN is true.  */

static tree
build_function_type_skip_args (tree orig_type, bitmap args_to_skip,
			       bool skip_return)
{
  tree new_type = NULL;
  tree args, new_args = NULL, t;
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

  /* This is a new type, not a copy of an old type.  Need to reassociate
     variants.  We can handle everything except the main variant lazily.  */
  t = TYPE_MAIN_VARIANT (orig_type);
  if (t != orig_type)
    {
      t = build_function_type_skip_args (t, args_to_skip, skip_return);
      TYPE_MAIN_VARIANT (new_type) = t;
      TYPE_NEXT_VARIANT (new_type) = TYPE_NEXT_VARIANT (t);
      TYPE_NEXT_VARIANT (t) = new_type;
    }
  else
    {
      TYPE_MAIN_VARIANT (new_type) = new_type;
      TYPE_NEXT_VARIANT (new_type) = NULL;
    }

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
      = build_function_type_skip_args (new_type, args_to_skip, skip_return);
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
  return new_decl;
}

/* Create callgraph node clone with new declaration.  The actual body will
   be copied later at compilation stage.

   TODO: after merging in ipa-sra use function call notes instead of args_to_skip
   bitmap interface.
   */
struct cgraph_node *
cgraph_create_virtual_clone (struct cgraph_node *old_node,
			     vec<cgraph_edge_p> redirect_callers,
			     vec<ipa_replace_map_p, va_gc> *tree_map,
			     bitmap args_to_skip,
			     const char * suffix)
{
  tree old_decl = old_node->decl;
  struct cgraph_node *new_node = NULL;
  tree new_decl;
  size_t len, i;
  struct ipa_replace_map *map;
  char *name;

  if (!in_lto_p)
    gcc_checking_assert  (tree_versionable_function_p (old_decl));

  gcc_assert (old_node->local.can_change_signature || !args_to_skip);

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

  new_node = cgraph_clone_node (old_node, new_decl, old_node->count,
				CGRAPH_FREQ_BASE, false,
				redirect_callers, false, NULL);
  /* Update the properties.
     Make clone visible only within this translation unit.  Make sure
     that is not weak also.
     ??? We cannot use COMDAT linkage because there is no
     ABI support for this.  */
  DECL_EXTERNAL (new_node->decl) = 0;
  if (DECL_ONE_ONLY (old_decl))
    DECL_SECTION_NAME (new_node->decl) = NULL;
  DECL_COMDAT_GROUP (new_node->decl) = 0;
  TREE_PUBLIC (new_node->decl) = 0;
  DECL_COMDAT (new_node->decl) = 0;
  DECL_WEAK (new_node->decl) = 0;
  DECL_VIRTUAL_P (new_node->decl) = 0;
  DECL_STATIC_CONSTRUCTOR (new_node->decl) = 0;
  DECL_STATIC_DESTRUCTOR (new_node->decl) = 0;
  new_node->clone.tree_map = tree_map;
  new_node->clone.args_to_skip = args_to_skip;

  /* Clones of global symbols or symbols with unique names are unique.  */
  if ((TREE_PUBLIC (old_decl)
       && !DECL_EXTERNAL (old_decl)
       && !DECL_WEAK (old_decl)
       && !DECL_COMDAT (old_decl))
      || in_lto_p)
    new_node->unique_name = true;
  FOR_EACH_VEC_SAFE_ELT (tree_map, i, map)
    ipa_maybe_record_reference (new_node, map->new_tree,
				IPA_REF_ADDR, NULL);
  if (!args_to_skip)
    new_node->clone.combined_args_to_skip = old_node->clone.combined_args_to_skip;
  else if (old_node->clone.combined_args_to_skip)
    {
      int newi = 0, oldi = 0;
      tree arg;
      bitmap new_args_to_skip = BITMAP_GGC_ALLOC ();
      struct cgraph_node *orig_node;
      for (orig_node = old_node; orig_node->clone_of; orig_node = orig_node->clone_of)
        ;
      for (arg = DECL_ARGUMENTS (orig_node->decl);
	   arg; arg = DECL_CHAIN (arg), oldi++)
	{
	  if (bitmap_bit_p (old_node->clone.combined_args_to_skip, oldi))
	    {
	      bitmap_set_bit (new_args_to_skip, oldi);
	      continue;
	    }
	  if (bitmap_bit_p (args_to_skip, newi))
	    bitmap_set_bit (new_args_to_skip, oldi);
	  newi++;
	}
      new_node->clone.combined_args_to_skip = new_args_to_skip;
    }
  else
    new_node->clone.combined_args_to_skip = args_to_skip;
  new_node->externally_visible = 0;
  new_node->local.local = 1;
  new_node->lowered = true;

  cgraph_call_node_duplication_hooks (old_node, new_node);


  return new_node;
}

/* NODE is being removed from symbol table; see if its entry can be replaced by
   other inline clone.  */
struct cgraph_node *
cgraph_find_replacement_node (struct cgraph_node *node)
{
  struct cgraph_node *next_inline_clone, *replacement;

  for (next_inline_clone = node->clones;
       next_inline_clone
       && next_inline_clone->decl != node->decl;
       next_inline_clone = next_inline_clone->next_sibling_clone)
    ;

  /* If there is inline clone of the node being removed, we need
     to put it into the position of removed node and reorganize all
     other clones to be based on it.  */
  if (next_inline_clone)
    {
      struct cgraph_node *n;
      struct cgraph_node *new_clones;

      replacement = next_inline_clone;

      /* Unlink inline clone from the list of clones of removed node.  */
      if (next_inline_clone->next_sibling_clone)
	next_inline_clone->next_sibling_clone->prev_sibling_clone
	  = next_inline_clone->prev_sibling_clone;
      if (next_inline_clone->prev_sibling_clone)
	{
	  gcc_assert (node->clones != next_inline_clone);
	  next_inline_clone->prev_sibling_clone->next_sibling_clone
	    = next_inline_clone->next_sibling_clone;
	}
      else
	{
	  gcc_assert (node->clones == next_inline_clone);
	  node->clones = next_inline_clone->next_sibling_clone;
	}

      new_clones = node->clones;
      node->clones = NULL;

      /* Copy clone info.  */
      next_inline_clone->clone = node->clone;

      /* Now place it into clone tree at same level at NODE.  */
      next_inline_clone->clone_of = node->clone_of;
      next_inline_clone->prev_sibling_clone = NULL;
      next_inline_clone->next_sibling_clone = NULL;
      if (node->clone_of)
	{
	  if (node->clone_of->clones)
	    node->clone_of->clones->prev_sibling_clone = next_inline_clone;
	  next_inline_clone->next_sibling_clone = node->clone_of->clones;
	  node->clone_of->clones = next_inline_clone;
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
		n =  n->next_sibling_clone;
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
cgraph_set_call_stmt_including_clones (struct cgraph_node *orig,
				       gimple old_stmt, gimple new_stmt,
				       bool update_speculative)
{
  struct cgraph_node *node;
  struct cgraph_edge *edge = cgraph_edge (orig, old_stmt);

  if (edge)
    cgraph_set_call_stmt (edge, new_stmt, update_speculative);

  node = orig->clones;
  if (node)
    while (node != orig)
      {
	struct cgraph_edge *edge = cgraph_edge (node, old_stmt);
	if (edge)
	  {
	    cgraph_set_call_stmt (edge, new_stmt, update_speculative);
	    /* If UPDATE_SPECULATIVE is false, it means that we are turning
	       speculative call into a real code sequence.  Update the
	       callgraph edges.  */
	    if (edge->speculative && !update_speculative)
	      {
		struct cgraph_edge *direct, *indirect;
		struct ipa_ref *ref;

		gcc_assert (!edge->indirect_unknown_callee);
		cgraph_speculative_call_info (edge, direct, indirect, ref);
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
	    while (node != orig && !node->next_sibling_clone)
	      node = node->clone_of;
	    if (node != orig)
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
cgraph_create_edge_including_clones (struct cgraph_node *orig,
				     struct cgraph_node *callee,
				     gimple old_stmt,
				     gimple stmt, gcov_type count,
				     int freq,
				     cgraph_inline_failed_t reason)
{
  struct cgraph_node *node;
  struct cgraph_edge *edge;

  if (!cgraph_edge (orig, stmt))
    {
      edge = cgraph_create_edge (orig, callee, stmt, count, freq);
      edge->inline_failed = reason;
    }

  node = orig->clones;
  if (node)
    while (node != orig)
      {
	struct cgraph_edge *edge = cgraph_edge (node, old_stmt);

        /* It is possible that clones already contain the edge while
	   master didn't.  Either we promoted indirect call into direct
	   call in the clone or we are processing clones of unreachable
	   master where edges has been removed.  */
	if (edge)
	  cgraph_set_call_stmt (edge, stmt);
	else if (!cgraph_edge (node, stmt))
	  {
	    edge = cgraph_create_edge (node, callee, stmt, count,
				       freq);
	    edge->inline_failed = reason;
	  }

	if (node->clones)
	  node = node->clones;
	else if (node->next_sibling_clone)
	  node = node->next_sibling_clone;
	else
	  {
	    while (node != orig && !node->next_sibling_clone)
	      node = node->clone_of;
	    if (node != orig)
	      node = node->next_sibling_clone;
	  }
      }
}

/* Remove the node from cgraph and all inline clones inlined into it.
   Skip however removal of FORBIDDEN_NODE and return true if it needs to be
   removed.  This allows to call the function from outer loop walking clone
   tree.  */

bool
cgraph_remove_node_and_inline_clones (struct cgraph_node *node, struct cgraph_node *forbidden_node)
{
  struct cgraph_edge *e, *next;
  bool found = false;

  if (node == forbidden_node)
    {
      cgraph_remove_edge (node->callers);
      return true;
    }
  for (e = node->callees; e; e = next)
    {
      next = e->next_callee;
      if (!e->inline_failed)
        found |= cgraph_remove_node_and_inline_clones (e->callee, forbidden_node);
    }
  cgraph_remove_node (node);
  return found;
}

/* The edges representing the callers of the NEW_VERSION node were
   fixed by cgraph_function_versioning (), now the call_expr in their
   respective tree code should be updated to call the NEW_VERSION.  */

static void
update_call_expr (struct cgraph_node *new_version)
{
  struct cgraph_edge *e;

  gcc_assert (new_version);

  /* Update the call expr on the edges to call the new version.  */
  for (e = new_version->callers; e; e = e->next_caller)
    {
      struct function *inner_function = DECL_STRUCT_FUNCTION (e->caller->decl);
      gimple_call_set_fndecl (e->call_stmt, new_version->decl);
      maybe_clean_eh_stmt_fn (inner_function, e->call_stmt);
    }
}


/* Create a new cgraph node which is the new version of
   OLD_VERSION node.  REDIRECT_CALLERS holds the callers
   edges which should be redirected to point to
   NEW_VERSION.  ALL the callees edges of OLD_VERSION
   are cloned to the new version node.  Return the new
   version node. 

   If non-NULL BLOCK_TO_COPY determine what basic blocks 
   was copied to prevent duplications of calls that are dead
   in the clone.  */

struct cgraph_node *
cgraph_copy_node_for_versioning (struct cgraph_node *old_version,
				 tree new_decl,
				 vec<cgraph_edge_p> redirect_callers,
				 bitmap bbs_to_copy)
 {
   struct cgraph_node *new_version;
   struct cgraph_edge *e;
   unsigned i;

   gcc_assert (old_version);

   new_version = cgraph_create_node (new_decl);

   new_version->analyzed = old_version->analyzed;
   new_version->definition = old_version->definition;
   new_version->local = old_version->local;
   new_version->externally_visible = false;
   new_version->local.local = new_version->definition;
   new_version->global = old_version->global;
   new_version->rtl = old_version->rtl;
   new_version->count = old_version->count;

   for (e = old_version->callees; e; e=e->next_callee)
     if (!bbs_to_copy
	 || bitmap_bit_p (bbs_to_copy, gimple_bb (e->call_stmt)->index))
       cgraph_clone_edge (e, new_version, e->call_stmt,
			  e->lto_stmt_uid, REG_BR_PROB_BASE,
			  CGRAPH_FREQ_BASE,
			  true);
   for (e = old_version->indirect_calls; e; e=e->next_callee)
     if (!bbs_to_copy
	 || bitmap_bit_p (bbs_to_copy, gimple_bb (e->call_stmt)->index))
       cgraph_clone_edge (e, new_version, e->call_stmt,
			  e->lto_stmt_uid, REG_BR_PROB_BASE,
			  CGRAPH_FREQ_BASE,
			  true);
   FOR_EACH_VEC_ELT (redirect_callers, i, e)
     {
       /* Redirect calls to the old version node to point to its new
	  version.  */
       cgraph_redirect_edge_callee (e, new_version);
     }

   cgraph_call_node_duplication_hooks (old_version, new_version);

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
   OLD_VERSION_NODE is the node that is versioned.

   If non-NULL ARGS_TO_SKIP determine function parameters to remove
   from new version.
   If SKIP_RETURN is true, the new version will return void.
   If non-NULL BLOCK_TO_COPY determine what basic blocks to copy.
   If non_NULL NEW_ENTRY determine new entry BB of the clone.

   Return the new version's cgraph node.  */

struct cgraph_node *
cgraph_function_versioning (struct cgraph_node *old_version_node,
			    vec<cgraph_edge_p> redirect_callers,
			    vec<ipa_replace_map_p, va_gc> *tree_map,
			    bitmap args_to_skip,
			    bool skip_return,
			    bitmap bbs_to_copy,
			    basic_block new_entry_block,
			    const char *clone_name)
{
  tree old_decl = old_version_node->decl;
  struct cgraph_node *new_version_node = NULL;
  tree new_decl;

  if (!tree_versionable_function_p (old_decl))
    return NULL;

  gcc_assert (old_version_node->local.can_change_signature || !args_to_skip);

  /* Make a new FUNCTION_DECL tree node for the new version. */
  if (!args_to_skip && !skip_return)
    new_decl = copy_node (old_decl);
  else
    new_decl
      = build_function_decl_skip_args (old_decl, args_to_skip, skip_return);

  /* Generate a new name for the new version. */
  DECL_NAME (new_decl) = clone_function_name (old_decl, clone_name);
  SET_DECL_ASSEMBLER_NAME (new_decl, DECL_NAME (new_decl));
  SET_DECL_RTL (new_decl, NULL);

  /* When the old decl was a con-/destructor make sure the clone isn't.  */
  DECL_STATIC_CONSTRUCTOR (new_decl) = 0;
  DECL_STATIC_DESTRUCTOR (new_decl) = 0;

  /* Create the new version's call-graph node.
     and update the edges of the new node. */
  new_version_node =
    cgraph_copy_node_for_versioning (old_version_node, new_decl,
				     redirect_callers, bbs_to_copy);

  /* Copy the OLD_VERSION_NODE function tree to the new version.  */
  tree_function_versioning (old_decl, new_decl, tree_map, false, args_to_skip,
			    skip_return, bbs_to_copy, new_entry_block);

  /* Update the new version's properties.
     Make The new version visible only within this translation unit.  Make sure
     that is not weak also.
     ??? We cannot use COMDAT linkage because there is no
     ABI support for this.  */
  symtab_make_decl_local (new_version_node->decl);
  DECL_VIRTUAL_P (new_version_node->decl) = 0;
  new_version_node->externally_visible = 0;
  new_version_node->local.local = 1;
  new_version_node->lowered = true;
  /* Clones of global symbols or symbols with unique names are unique.  */
  if ((TREE_PUBLIC (old_decl)
       && !DECL_EXTERNAL (old_decl)
       && !DECL_WEAK (old_decl)
       && !DECL_COMDAT (old_decl))
      || in_lto_p)
    new_version_node->unique_name = true;

  /* Update the call_expr on the edges to call the new version node. */
  update_call_expr (new_version_node);

  cgraph_call_function_insertion_hooks (new_version_node);
  return new_version_node;
}

/* Given virtual clone, turn it into actual clone.  */

static void
cgraph_materialize_clone (struct cgraph_node *node)
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
  if (cgraph_dump_file)
    {
      dump_function_to_file (node->clone_of->decl, cgraph_dump_file, dump_flags);
      dump_function_to_file (node->decl, cgraph_dump_file, dump_flags);
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
      cgraph_release_function_body (node->clone_of);
      cgraph_node_remove_callees (node->clone_of);
      ipa_remove_all_references (&node->clone_of->ref_list);
    }
  node->clone_of = NULL;
  bitmap_obstack_release (NULL);
}

/* Once all functions from compilation unit are in memory, produce all clones
   and update all calls.  We might also do this on demand if we don't want to
   bring all functions to memory prior compilation, but current WHOPR
   implementation does that and it is is bit easier to keep everything right in
   this order.  */

void
cgraph_materialize_all_clones (void)
{
  struct cgraph_node *node;
  bool stabilized = false;
  

  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "Materializing clones\n");
#ifdef ENABLE_CHECKING
  verify_cgraph ();
#endif

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
		cgraph_get_body (node->clone_of);
	      if (gimple_has_body_p (node->clone_of->decl))
	        {
		  if (cgraph_dump_file)
		    {
		      fprintf (cgraph_dump_file, "cloning %s to %s\n",
			       xstrdup (node->clone_of->name ()),
			       xstrdup (node->name ()));
		      if (node->clone.tree_map)
		        {
			  unsigned int i;
		          fprintf (cgraph_dump_file, "   replace map: ");
			  for (i = 0;
			       i < vec_safe_length (node->clone.tree_map);
			       i++)
			    {
			      struct ipa_replace_map *replace_info;
			      replace_info = (*node->clone.tree_map)[i];
			      print_generic_expr (cgraph_dump_file, replace_info->old_tree, 0);
			      fprintf (cgraph_dump_file, " -> ");
			      print_generic_expr (cgraph_dump_file, replace_info->new_tree, 0);
			      fprintf (cgraph_dump_file, "%s%s;",
			      	       replace_info->replace_p ? "(replace)":"",
				       replace_info->ref_p ? "(ref)":"");
			    }
			  fprintf (cgraph_dump_file, "\n");
			}
		      if (node->clone.args_to_skip)
			{
		          fprintf (cgraph_dump_file, "   args_to_skip: ");
		          dump_bitmap (cgraph_dump_file, node->clone.args_to_skip);
			}
		      if (node->clone.args_to_skip)
			{
		          fprintf (cgraph_dump_file, "   combined_args_to_skip:");
		          dump_bitmap (cgraph_dump_file, node->clone.combined_args_to_skip);
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
        cgraph_node_remove_callees (node);
	ipa_remove_all_references (&node->ref_list);
      }
    else
      ipa_clear_stmts_in_references (node);
  if (cgraph_dump_file)
    fprintf (cgraph_dump_file, "Materialization Call site updates done.\n");
#ifdef ENABLE_CHECKING
  verify_cgraph ();
#endif
  symtab_remove_unreachable_nodes (false, cgraph_dump_file);
}

#include "gt-cgraphclones.h"
