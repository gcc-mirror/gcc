/* Nested function decomposition for GIMPLE.
   Copyright (C) 2004-2021 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "memmodel.h"
#include "tm_p.h"
#include "stringpool.h"
#include "cgraph.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "dumpfile.h"
#include "tree-inline.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "tree-cfg.h"
#include "explow.h"
#include "langhooks.h"
#include "gimple-low.h"
#include "gomp-constants.h"
#include "diagnostic.h"
#include "alloc-pool.h"
#include "tree-nested.h"
#include "symbol-summary.h"
#include "symtab-thunks.h"

/* Summary of nested functions.  */
static function_summary <nested_function_info *>
   *nested_function_sum = NULL;

/* Return nested_function_info, if available.  */
nested_function_info *
nested_function_info::get (cgraph_node *node)
{
  if (!nested_function_sum)
    return NULL;
  return nested_function_sum->get (node);
}

/* Return nested_function_info possibly creating new one.  */
nested_function_info *
nested_function_info::get_create (cgraph_node *node)
{
  if (!nested_function_sum)
    {
      nested_function_sum = new function_summary <nested_function_info *>
				   (symtab);
      nested_function_sum->disable_insertion_hook ();
    }
  return nested_function_sum->get_create (node);
}

/* cgraph_node is no longer nested function; update cgraph accordingly.  */
void
unnest_function (cgraph_node *node)
{
  nested_function_info *info = nested_function_info::get (node);
  cgraph_node **node2 = &nested_function_info::get
		(nested_function_origin (node))->nested;

  gcc_checking_assert (info->origin);
  while (*node2 != node)
    node2 = &nested_function_info::get (*node2)->next_nested;
  *node2 = info->next_nested;
  info->next_nested = NULL;
  info->origin = NULL;
  nested_function_sum->remove (node);
}

/* Destructor: unlink function from nested function lists.  */
nested_function_info::~nested_function_info ()
{
  cgraph_node *next;
  for (cgraph_node *n = nested; n; n = next)
    {
      nested_function_info *info = nested_function_info::get (n);
      next = info->next_nested;
      info->origin = NULL;
      info->next_nested = NULL;
    }
  nested = NULL;
  if (origin)
    {
      cgraph_node **node2
	     = &nested_function_info::get (origin)->nested;

      nested_function_info *info;
      while ((info = nested_function_info::get (*node2)) != this && info)
	node2 = &info->next_nested;
      *node2 = next_nested;
    }
}

/* Free nested function info summaries.  */
void
nested_function_info::release ()
{
  if (nested_function_sum)
    delete (nested_function_sum);
  nested_function_sum = NULL;
}

/* If NODE is nested function, record it.  */
void
maybe_record_nested_function (cgraph_node *node)
{
  /* All nested functions gets lowered during the construction of symtab.  */
  if (symtab->state > CONSTRUCTION)
    return;
  if (DECL_CONTEXT (node->decl)
      && TREE_CODE (DECL_CONTEXT (node->decl)) == FUNCTION_DECL)
    {
      cgraph_node *origin = cgraph_node::get_create (DECL_CONTEXT (node->decl));
      nested_function_info *info = nested_function_info::get_create (node);
      nested_function_info *origin_info
		 = nested_function_info::get_create (origin);

      info->origin = origin;
      info->next_nested = origin_info->nested;
      origin_info->nested = node;
    }
}

/* The object of this pass is to lower the representation of a set of nested
   functions in order to expose all of the gory details of the various
   nonlocal references.  We want to do this sooner rather than later, in
   order to give us more freedom in emitting all of the functions in question.

   Back in olden times, when gcc was young, we developed an insanely
   complicated scheme whereby variables which were referenced nonlocally
   were forced to live in the stack of the declaring function, and then
   the nested functions magically discovered where these variables were
   placed.  In order for this scheme to function properly, it required
   that the outer function be partially expanded, then we switch to
   compiling the inner function, and once done with those we switch back
   to compiling the outer function.  Such delicate ordering requirements
   makes it difficult to do whole translation unit optimizations
   involving such functions.

   The implementation here is much more direct.  Everything that can be
   referenced by an inner function is a member of an explicitly created
   structure herein called the "nonlocal frame struct".  The incoming
   static chain for a nested function is a pointer to this struct in
   the parent.  In this way, we settle on known offsets from a known
   base, and so are decoupled from the logic that places objects in the
   function's stack frame.  More importantly, we don't have to wait for
   that to happen -- since the compilation of the inner function is no
   longer tied to a real stack frame, the nonlocal frame struct can be
   allocated anywhere.  Which means that the outer function is now
   inlinable.

   Theory of operation here is very simple.  Iterate over all the
   statements in all the functions (depth first) several times,
   allocating structures and fields on demand.  In general we want to
   examine inner functions first, so that we can avoid making changes
   to outer functions which are unnecessary.

   The order of the passes matters a bit, in that later passes will be
   skipped if it is discovered that the functions don't actually interact
   at all.  That is, they're nested in the lexical sense but could have
   been written as independent functions without change.  */


struct nesting_info
{
  struct nesting_info *outer;
  struct nesting_info *inner;
  struct nesting_info *next;

  hash_map<tree, tree> *field_map;
  hash_map<tree, tree> *var_map;
  hash_set<tree *> *mem_refs;
  bitmap suppress_expansion;

  tree context;
  tree new_local_var_chain;
  tree debug_var_chain;
  tree frame_type;
  tree frame_decl;
  tree chain_field;
  tree chain_decl;
  tree nl_goto_field;

  bool thunk_p;
  bool any_parm_remapped;
  bool any_tramp_created;
  bool any_descr_created;
  char static_chain_added;
};


/* Iterate over the nesting tree, starting with ROOT, depth first.  */

static inline struct nesting_info *
iter_nestinfo_start (struct nesting_info *root)
{
  while (root->inner)
    root = root->inner;
  return root;
}

static inline struct nesting_info *
iter_nestinfo_next (struct nesting_info *node)
{
  if (node->next)
    return iter_nestinfo_start (node->next);
  return node->outer;
}

#define FOR_EACH_NEST_INFO(I, ROOT) \
  for ((I) = iter_nestinfo_start (ROOT); (I); (I) = iter_nestinfo_next (I))

/* Obstack used for the bitmaps in the struct above.  */
static struct bitmap_obstack nesting_info_bitmap_obstack;


/* We're working in so many different function contexts simultaneously,
   that create_tmp_var is dangerous.  Prevent mishap.  */
#define create_tmp_var cant_use_create_tmp_var_here_dummy

/* Like create_tmp_var, except record the variable for registration at
   the given nesting level.  */

static tree
create_tmp_var_for (struct nesting_info *info, tree type, const char *prefix)
{
  tree tmp_var;

  /* If the type is of variable size or a type which must be created by the
     frontend, something is wrong.  Note that we explicitly allow
     incomplete types here, since we create them ourselves here.  */
  gcc_assert (!TREE_ADDRESSABLE (type));
  gcc_assert (!TYPE_SIZE_UNIT (type)
	      || TREE_CODE (TYPE_SIZE_UNIT (type)) == INTEGER_CST);

  tmp_var = create_tmp_var_raw (type, prefix);
  DECL_CONTEXT (tmp_var) = info->context;
  DECL_CHAIN (tmp_var) = info->new_local_var_chain;
  DECL_SEEN_IN_BIND_EXPR_P (tmp_var) = 1;

  info->new_local_var_chain = tmp_var;

  return tmp_var;
}

/* Like build_simple_mem_ref, but set TREE_THIS_NOTRAP on the result.  */

static tree
build_simple_mem_ref_notrap (tree ptr)
{
  tree t = build_simple_mem_ref (ptr);
  TREE_THIS_NOTRAP (t) = 1;
  return t;
}

/* Take the address of EXP to be used within function CONTEXT.
   Mark it for addressability as necessary.  */

tree
build_addr (tree exp)
{
  mark_addressable (exp);
  return build_fold_addr_expr (exp);
}

/* Insert FIELD into TYPE, sorted by alignment requirements.  */

void
insert_field_into_struct (tree type, tree field)
{
  tree *p;

  DECL_CONTEXT (field) = type;

  for (p = &TYPE_FIELDS (type); *p ; p = &DECL_CHAIN (*p))
    if (DECL_ALIGN (field) >= DECL_ALIGN (*p))
      break;

  DECL_CHAIN (field) = *p;
  *p = field;

  /* Set correct alignment for frame struct type.  */
  if (TYPE_ALIGN (type) < DECL_ALIGN (field))
    SET_TYPE_ALIGN (type, DECL_ALIGN (field));
}

/* Build or return the RECORD_TYPE that describes the frame state that is
   shared between INFO->CONTEXT and its nested functions.  This record will
   not be complete until finalize_nesting_tree; up until that point we'll
   be adding fields as necessary.

   We also build the DECL that represents this frame in the function.  */

static tree
get_frame_type (struct nesting_info *info)
{
  tree type = info->frame_type;
  if (!type)
    {
      char *name;

      type = make_node (RECORD_TYPE);

      name = concat ("FRAME.",
		     IDENTIFIER_POINTER (DECL_NAME (info->context)),
		     NULL);
      TYPE_NAME (type) = get_identifier (name);
      free (name);

      info->frame_type = type;

      /* Do not put info->frame_decl on info->new_local_var_chain,
	 so that we can declare it in the lexical blocks, which
	 makes sure virtual regs that end up appearing in its RTL
	 expression get substituted in instantiate_virtual_regs.  */
      info->frame_decl = create_tmp_var_raw (type, "FRAME");
      DECL_CONTEXT (info->frame_decl) = info->context;
      DECL_NONLOCAL_FRAME (info->frame_decl) = 1;
      DECL_SEEN_IN_BIND_EXPR_P (info->frame_decl) = 1;

      /* ??? Always make it addressable for now, since it is meant to
	 be pointed to by the static chain pointer.  This pessimizes
	 when it turns out that no static chains are needed because
	 the nested functions referencing non-local variables are not
	 reachable, but the true pessimization is to create the non-
	 local frame structure in the first place.  */
      TREE_ADDRESSABLE (info->frame_decl) = 1;
    }

  return type;
}

/* Return true if DECL should be referenced by pointer in the non-local frame
   structure.  */

static bool
use_pointer_in_frame (tree decl)
{
  if (TREE_CODE (decl) == PARM_DECL)
    {
      /* It's illegal to copy TREE_ADDRESSABLE, impossible to copy variable-
	 sized DECLs, and inefficient to copy large aggregates.  Don't bother
	 moving anything but scalar parameters.  */
      return AGGREGATE_TYPE_P (TREE_TYPE (decl));
    }
  else
    {
      /* Variable-sized DECLs can only come from OMP clauses at this point
	 since the gimplifier has already turned the regular variables into
	 pointers.  Do the same as the gimplifier.  */
      return !DECL_SIZE (decl) || TREE_CODE (DECL_SIZE (decl)) != INTEGER_CST;
    }
}

/* Given DECL, a non-locally accessed variable, find or create a field
   in the non-local frame structure for the given nesting context.  */

static tree
lookup_field_for_decl (struct nesting_info *info, tree decl,
		       enum insert_option insert)
{
  gcc_checking_assert (decl_function_context (decl) == info->context);

  if (insert == NO_INSERT)
    {
      tree *slot = info->field_map->get (decl);
      return slot ? *slot : NULL_TREE;
    }

  tree *slot = &info->field_map->get_or_insert (decl);
  if (!*slot)
    {
      tree type = get_frame_type (info);
      tree field = make_node (FIELD_DECL);
      DECL_NAME (field) = DECL_NAME (decl);

      if (use_pointer_in_frame (decl))
	{
	  TREE_TYPE (field) = build_pointer_type (TREE_TYPE (decl));
	  SET_DECL_ALIGN (field, TYPE_ALIGN (TREE_TYPE (field)));
	  DECL_NONADDRESSABLE_P (field) = 1;
	}
      else
	{
	  TREE_TYPE (field) = TREE_TYPE (decl);
	  DECL_SOURCE_LOCATION (field) = DECL_SOURCE_LOCATION (decl);
	  SET_DECL_ALIGN (field, DECL_ALIGN (decl));
	  DECL_USER_ALIGN (field) = DECL_USER_ALIGN (decl);
	  DECL_IGNORED_P (field) = DECL_IGNORED_P (decl);
	  DECL_NONADDRESSABLE_P (field) = !TREE_ADDRESSABLE (decl);
	  TREE_THIS_VOLATILE (field) = TREE_THIS_VOLATILE (decl);
	  copy_warning (field, decl);

	  /* Declare the transformation and adjust the original DECL.  For a
	     variable or for a parameter when not optimizing, we make it point
	     to the field in the frame directly.  For a parameter, we don't do
	     it when optimizing because the variable tracking pass will already
	     do the job,  */
	  if (VAR_P (decl) || !optimize)
	    {
	      tree x
		= build3 (COMPONENT_REF, TREE_TYPE (field), info->frame_decl,
			  field, NULL_TREE);

	      /* If the next declaration is a PARM_DECL pointing to the DECL,
		 we need to adjust its VALUE_EXPR directly, since chains of
		 VALUE_EXPRs run afoul of garbage collection.  This occurs
		 in Ada for Out parameters that aren't copied in.  */
	      tree next = DECL_CHAIN (decl);
	      if (next
		  && TREE_CODE (next) == PARM_DECL
		  && DECL_HAS_VALUE_EXPR_P (next)
		  && DECL_VALUE_EXPR (next) == decl)
		SET_DECL_VALUE_EXPR (next, x);

	      SET_DECL_VALUE_EXPR (decl, x);
	      DECL_HAS_VALUE_EXPR_P (decl) = 1;
	    }
	}

      insert_field_into_struct (type, field);
      *slot = field;

      if (TREE_CODE (decl) == PARM_DECL)
	info->any_parm_remapped = true;
    }

  return *slot;
}

/* Build or return the variable that holds the static chain within
   INFO->CONTEXT.  This variable may only be used within INFO->CONTEXT.  */

static tree
get_chain_decl (struct nesting_info *info)
{
  tree decl = info->chain_decl;

  if (!decl)
    {
      tree type;

      type = get_frame_type (info->outer);
      type = build_pointer_type (type);

      /* Note that this variable is *not* entered into any BIND_EXPR;
	 the construction of this variable is handled specially in
	 expand_function_start and initialize_inlined_parameters.
	 Note also that it's represented as a parameter.  This is more
	 close to the truth, since the initial value does come from
	 the caller.  */
      decl = build_decl (DECL_SOURCE_LOCATION (info->context),
			 PARM_DECL, create_tmp_var_name ("CHAIN"), type);
      DECL_ARTIFICIAL (decl) = 1;
      DECL_IGNORED_P (decl) = 1;
      TREE_USED (decl) = 1;
      DECL_CONTEXT (decl) = info->context;
      DECL_ARG_TYPE (decl) = type;

      /* Tell tree-inline.c that we never write to this variable, so
	 it can copy-prop the replacement value immediately.  */
      TREE_READONLY (decl) = 1;

      info->chain_decl = decl;

      if (dump_file
          && (dump_flags & TDF_DETAILS)
	  && !DECL_STATIC_CHAIN (info->context))
	fprintf (dump_file, "Setting static-chain for %s\n",
		 lang_hooks.decl_printable_name (info->context, 2));

      DECL_STATIC_CHAIN (info->context) = 1;
    }
  return decl;
}

/* Build or return the field within the non-local frame state that holds
   the static chain for INFO->CONTEXT.  This is the way to walk back up
   multiple nesting levels.  */

static tree
get_chain_field (struct nesting_info *info)
{
  tree field = info->chain_field;

  if (!field)
    {
      tree type = build_pointer_type (get_frame_type (info->outer));

      field = make_node (FIELD_DECL);
      DECL_NAME (field) = get_identifier ("__chain");
      TREE_TYPE (field) = type;
      SET_DECL_ALIGN (field, TYPE_ALIGN (type));
      DECL_NONADDRESSABLE_P (field) = 1;

      insert_field_into_struct (get_frame_type (info), field);

      info->chain_field = field;

      if (dump_file
          && (dump_flags & TDF_DETAILS)
	  && !DECL_STATIC_CHAIN (info->context))
	fprintf (dump_file, "Setting static-chain for %s\n",
		 lang_hooks.decl_printable_name (info->context, 2));

      DECL_STATIC_CHAIN (info->context) = 1;
    }
  return field;
}

/* Initialize a new temporary with the GIMPLE_CALL STMT.  */

static tree
init_tmp_var_with_call (struct nesting_info *info, gimple_stmt_iterator *gsi,
		        gcall *call)
{
  tree t;

  t = create_tmp_var_for (info, gimple_call_return_type (call), NULL);
  gimple_call_set_lhs (call, t);
  if (! gsi_end_p (*gsi))
    gimple_set_location (call, gimple_location (gsi_stmt (*gsi)));
  gsi_insert_before (gsi, call, GSI_SAME_STMT);

  return t;
}


/* Copy EXP into a temporary.  Allocate the temporary in the context of
   INFO and insert the initialization statement before GSI.  */

static tree
init_tmp_var (struct nesting_info *info, tree exp, gimple_stmt_iterator *gsi)
{
  tree t;
  gimple *stmt;

  t = create_tmp_var_for (info, TREE_TYPE (exp), NULL);
  stmt = gimple_build_assign (t, exp);
  if (! gsi_end_p (*gsi))
    gimple_set_location (stmt, gimple_location (gsi_stmt (*gsi)));
  gsi_insert_before_without_update (gsi, stmt, GSI_SAME_STMT);

  return t;
}


/* Similarly, but only do so to force EXP to satisfy is_gimple_val.  */

static tree
gsi_gimplify_val (struct nesting_info *info, tree exp,
		  gimple_stmt_iterator *gsi)
{
  if (is_gimple_val (exp))
    return exp;
  else
    return init_tmp_var (info, exp, gsi);
}

/* Similarly, but copy from the temporary and insert the statement
   after the iterator.  */

static tree
save_tmp_var (struct nesting_info *info, tree exp, gimple_stmt_iterator *gsi)
{
  tree t;
  gimple *stmt;

  t = create_tmp_var_for (info, TREE_TYPE (exp), NULL);
  stmt = gimple_build_assign (exp, t);
  if (! gsi_end_p (*gsi))
    gimple_set_location (stmt, gimple_location (gsi_stmt (*gsi)));
  gsi_insert_after_without_update (gsi, stmt, GSI_SAME_STMT);

  return t;
}

/* Build or return the type used to represent a nested function trampoline.  */

static GTY(()) tree trampoline_type;

static tree
get_trampoline_type (struct nesting_info *info)
{
  unsigned align, size;
  tree t;

  if (trampoline_type)
    return trampoline_type;

  align = TRAMPOLINE_ALIGNMENT;
  size = TRAMPOLINE_SIZE;

  /* If we won't be able to guarantee alignment simply via TYPE_ALIGN,
     then allocate extra space so that we can do dynamic alignment.  */
  if (align > STACK_BOUNDARY)
    {
      size += ((align/BITS_PER_UNIT) - 1) & -(STACK_BOUNDARY/BITS_PER_UNIT);
      align = STACK_BOUNDARY;
    }

  t = build_index_type (size_int (size - 1));
  t = build_array_type (char_type_node, t);
  t = build_decl (DECL_SOURCE_LOCATION (info->context),
		  FIELD_DECL, get_identifier ("__data"), t);
  SET_DECL_ALIGN (t, align);
  DECL_USER_ALIGN (t) = 1;

  trampoline_type = make_node (RECORD_TYPE);
  TYPE_NAME (trampoline_type) = get_identifier ("__builtin_trampoline");
  TYPE_FIELDS (trampoline_type) = t;
  layout_type (trampoline_type);
  DECL_CONTEXT (t) = trampoline_type;

  return trampoline_type;
}

/* Build or return the type used to represent a nested function descriptor.  */

static GTY(()) tree descriptor_type;

static tree
get_descriptor_type (struct nesting_info *info)
{
  /* The base alignment is that of a function.  */
  const unsigned align = FUNCTION_ALIGNMENT (FUNCTION_BOUNDARY);
  tree t;

  if (descriptor_type)
    return descriptor_type;

  t = build_index_type (integer_one_node);
  t = build_array_type (ptr_type_node, t);
  t = build_decl (DECL_SOURCE_LOCATION (info->context),
		  FIELD_DECL, get_identifier ("__data"), t);
  SET_DECL_ALIGN (t, MAX (TYPE_ALIGN (ptr_type_node), align));
  DECL_USER_ALIGN (t) = 1;

  descriptor_type = make_node (RECORD_TYPE);
  TYPE_NAME (descriptor_type) = get_identifier ("__builtin_descriptor");
  TYPE_FIELDS (descriptor_type) = t;
  layout_type (descriptor_type);
  DECL_CONTEXT (t) = descriptor_type;

  return descriptor_type;
}

/* Given DECL, a nested function, find or create an element in the
   var map for this function.  */

static tree
lookup_element_for_decl (struct nesting_info *info, tree decl,
			 enum insert_option insert)
{
  if (insert == NO_INSERT)
    {
      tree *slot = info->var_map->get (decl);
      return slot ? *slot : NULL_TREE;
    }

  tree *slot = &info->var_map->get_or_insert (decl);
  if (!*slot)
    *slot = build_tree_list (NULL_TREE, NULL_TREE);

  return (tree) *slot;
}

/* Given DECL, a nested function, create a field in the non-local
   frame structure for this function.  */

static tree
create_field_for_decl (struct nesting_info *info, tree decl, tree type)
{
  tree field = make_node (FIELD_DECL);
  DECL_NAME (field) = DECL_NAME (decl);
  TREE_TYPE (field) = type;
  TREE_ADDRESSABLE (field) = 1;
  insert_field_into_struct (get_frame_type (info), field);
  return field;
}

/* Given DECL, a nested function, find or create a field in the non-local
   frame structure for a trampoline for this function.  */

static tree
lookup_tramp_for_decl (struct nesting_info *info, tree decl,
		       enum insert_option insert)
{
  tree elt, field;

  elt = lookup_element_for_decl (info, decl, insert);
  if (!elt)
    return NULL_TREE;

  field = TREE_PURPOSE (elt);

  if (!field && insert == INSERT)
    {
      field = create_field_for_decl (info, decl, get_trampoline_type (info));
      TREE_PURPOSE (elt) = field;
      info->any_tramp_created = true;
    }

  return field;
}

/* Given DECL, a nested function, find or create a field in the non-local
   frame structure for a descriptor for this function.  */

static tree
lookup_descr_for_decl (struct nesting_info *info, tree decl,
		       enum insert_option insert)
{
  tree elt, field;

  elt = lookup_element_for_decl (info, decl, insert);
  if (!elt)
    return NULL_TREE;

  field = TREE_VALUE (elt);

  if (!field && insert == INSERT)
    {
      field = create_field_for_decl (info, decl, get_descriptor_type (info));
      TREE_VALUE (elt) = field;
      info->any_descr_created = true;
    }

  return field;
}

/* Build or return the field within the non-local frame state that holds
   the non-local goto "jmp_buf".  The buffer itself is maintained by the
   rtl middle-end as dynamic stack space is allocated.  */

static tree
get_nl_goto_field (struct nesting_info *info)
{
  tree field = info->nl_goto_field;
  if (!field)
    {
      unsigned size;
      tree type;

      /* For __builtin_nonlocal_goto, we need N words.  The first is the
	 frame pointer, the rest is for the target's stack pointer save
	 area.  The number of words is controlled by STACK_SAVEAREA_MODE;
	 not the best interface, but it'll do for now.  */
      if (Pmode == ptr_mode)
	type = ptr_type_node;
      else
	type = lang_hooks.types.type_for_mode (Pmode, 1);

      scalar_int_mode mode
	= as_a <scalar_int_mode> (STACK_SAVEAREA_MODE (SAVE_NONLOCAL));
      size = GET_MODE_SIZE (mode);
      size = size / GET_MODE_SIZE (Pmode);
      size = size + 1;

      type = build_array_type
	(type, build_index_type (size_int (size)));

      field = make_node (FIELD_DECL);
      DECL_NAME (field) = get_identifier ("__nl_goto_buf");
      TREE_TYPE (field) = type;
      SET_DECL_ALIGN (field, TYPE_ALIGN (type));
      TREE_ADDRESSABLE (field) = 1;

      insert_field_into_struct (get_frame_type (info), field);

      info->nl_goto_field = field;
    }

  return field;
}

/* Invoke CALLBACK on all statements of GIMPLE sequence *PSEQ.  */

static void
walk_body (walk_stmt_fn callback_stmt, walk_tree_fn callback_op,
	   struct nesting_info *info, gimple_seq *pseq)
{
  struct walk_stmt_info wi;

  memset (&wi, 0, sizeof (wi));
  wi.info = info;
  wi.val_only = true;
  walk_gimple_seq_mod (pseq, callback_stmt, callback_op, &wi);
}


/* Invoke CALLBACK_STMT/CALLBACK_OP on all statements of INFO->CONTEXT.  */

static inline void
walk_function (walk_stmt_fn callback_stmt, walk_tree_fn callback_op,
	       struct nesting_info *info)
{
  gimple_seq body = gimple_body (info->context);
  walk_body (callback_stmt, callback_op, info, &body);
  gimple_set_body (info->context, body);
}

/* Invoke CALLBACK on a GIMPLE_OMP_FOR's init, cond, incr and pre-body.  */

static void
walk_gimple_omp_for (gomp_for *for_stmt,
    		     walk_stmt_fn callback_stmt, walk_tree_fn callback_op,
    		     struct nesting_info *info)
{
  struct walk_stmt_info wi;
  gimple_seq seq;
  tree t;
  size_t i;

  walk_body (callback_stmt, callback_op, info, gimple_omp_for_pre_body_ptr (for_stmt));

  seq = NULL;
  memset (&wi, 0, sizeof (wi));
  wi.info = info;
  wi.gsi = gsi_last (seq);

  for (i = 0; i < gimple_omp_for_collapse (for_stmt); i++)
    {
      wi.val_only = false;
      walk_tree (gimple_omp_for_index_ptr (for_stmt, i), callback_op,
		 &wi, NULL);
      wi.val_only = true;
      wi.is_lhs = false;
      walk_tree (gimple_omp_for_initial_ptr (for_stmt, i), callback_op,
		 &wi, NULL);

      wi.val_only = true;
      wi.is_lhs = false;
      walk_tree (gimple_omp_for_final_ptr (for_stmt, i), callback_op,
		 &wi, NULL);

      t = gimple_omp_for_incr (for_stmt, i);
      gcc_assert (BINARY_CLASS_P (t));
      wi.val_only = false;
      walk_tree (&TREE_OPERAND (t, 0), callback_op, &wi, NULL);
      wi.val_only = true;
      wi.is_lhs = false;
      walk_tree (&TREE_OPERAND (t, 1), callback_op, &wi, NULL);
    }

  seq = gsi_seq (wi.gsi);
  if (!gimple_seq_empty_p (seq))
    {
      gimple_seq pre_body = gimple_omp_for_pre_body (for_stmt);
      annotate_all_with_location (seq, gimple_location (for_stmt));
      gimple_seq_add_seq (&pre_body, seq);
      gimple_omp_for_set_pre_body (for_stmt, pre_body);
    }
}

/* Similarly for ROOT and all functions nested underneath, depth first.  */

static void
walk_all_functions (walk_stmt_fn callback_stmt, walk_tree_fn callback_op,
		    struct nesting_info *root)
{
  struct nesting_info *n;
  FOR_EACH_NEST_INFO (n, root)
    walk_function (callback_stmt, callback_op, n);
}


/* We have to check for a fairly pathological case.  The operands of function
   nested function are to be interpreted in the context of the enclosing
   function.  So if any are variably-sized, they will get remapped when the
   enclosing function is inlined.  But that remapping would also have to be
   done in the types of the PARM_DECLs of the nested function, meaning the
   argument types of that function will disagree with the arguments in the
   calls to that function.  So we'd either have to make a copy of the nested
   function corresponding to each time the enclosing function was inlined or
   add a VIEW_CONVERT_EXPR to each such operand for each call to the nested
   function.  The former is not practical.  The latter would still require
   detecting this case to know when to add the conversions.  So, for now at
   least, we don't inline such an enclosing function.

   We have to do that check recursively, so here return indicating whether
   FNDECL has such a nested function.  ORIG_FN is the function we were
   trying to inline to use for checking whether any argument is variably
   modified by anything in it.

   It would be better to do this in tree-inline.c so that we could give
   the appropriate warning for why a function can't be inlined, but that's
   too late since the nesting structure has already been flattened and
   adding a flag just to record this fact seems a waste of a flag.  */

static bool
check_for_nested_with_variably_modified (tree fndecl, tree orig_fndecl)
{
  struct cgraph_node *cgn = cgraph_node::get (fndecl);
  tree arg;

  for (cgn = first_nested_function (cgn); cgn;
       cgn = next_nested_function (cgn))
    {
      for (arg = DECL_ARGUMENTS (cgn->decl); arg; arg = DECL_CHAIN (arg))
	if (variably_modified_type_p (TREE_TYPE (arg), orig_fndecl))
	  return true;

      if (check_for_nested_with_variably_modified (cgn->decl,
						   orig_fndecl))
	return true;
    }

  return false;
}

/* Construct our local datastructure describing the function nesting
   tree rooted by CGN.  */

static struct nesting_info *
create_nesting_tree (struct cgraph_node *cgn)
{
  struct nesting_info *info = XCNEW (struct nesting_info);
  info->field_map = new hash_map<tree, tree>;
  info->var_map = new hash_map<tree, tree>;
  info->mem_refs = new hash_set<tree *>;
  info->suppress_expansion = BITMAP_ALLOC (&nesting_info_bitmap_obstack);
  info->context = cgn->decl;
  info->thunk_p = cgn->thunk;

  for (cgn = first_nested_function (cgn); cgn;
       cgn = next_nested_function (cgn))
    {
      struct nesting_info *sub = create_nesting_tree (cgn);
      sub->outer = info;
      sub->next = info->inner;
      info->inner = sub;
    }

  /* See discussion at check_for_nested_with_variably_modified for a
     discussion of why this has to be here.  */
  if (check_for_nested_with_variably_modified (info->context, info->context))
    DECL_UNINLINABLE (info->context) = true;

  return info;
}

/* Return an expression computing the static chain for TARGET_CONTEXT
   from INFO->CONTEXT.  Insert any necessary computations before TSI.  */

static tree
get_static_chain (struct nesting_info *info, tree target_context,
		  gimple_stmt_iterator *gsi)
{
  struct nesting_info *i;
  tree x;

  if (info->context == target_context)
    {
      x = build_addr (info->frame_decl);
      info->static_chain_added |= 1;
    }
  else
    {
      x = get_chain_decl (info);
      info->static_chain_added |= 2;

      for (i = info->outer; i->context != target_context; i = i->outer)
	{
	  tree field = get_chain_field (i);

	  x = build_simple_mem_ref_notrap (x);
	  x = build3 (COMPONENT_REF, TREE_TYPE (field), x, field, NULL_TREE);
	  x = init_tmp_var (info, x, gsi);
	}
    }

  return x;
}


/* Return an expression referencing FIELD from TARGET_CONTEXT's non-local
   frame as seen from INFO->CONTEXT.  Insert any necessary computations
   before GSI.  */

static tree
get_frame_field (struct nesting_info *info, tree target_context,
		 tree field, gimple_stmt_iterator *gsi)
{
  struct nesting_info *i;
  tree x;

  if (info->context == target_context)
    {
      /* Make sure frame_decl gets created.  */
      (void) get_frame_type (info);
      x = info->frame_decl;
      info->static_chain_added |= 1;
    }
  else
    {
      x = get_chain_decl (info);
      info->static_chain_added |= 2;

      for (i = info->outer; i->context != target_context; i = i->outer)
	{
	  tree field = get_chain_field (i);

	  x = build_simple_mem_ref_notrap (x);
	  x = build3 (COMPONENT_REF, TREE_TYPE (field), x, field, NULL_TREE);
	  x = init_tmp_var (info, x, gsi);
	}

      x = build_simple_mem_ref_notrap (x);
    }

  x = build3 (COMPONENT_REF, TREE_TYPE (field), x, field, NULL_TREE);
  TREE_THIS_VOLATILE (x) = TREE_THIS_VOLATILE (field);
  return x;
}

static void note_nonlocal_vla_type (struct nesting_info *info, tree type);

/* A subroutine of convert_nonlocal_reference_op.  Create a local variable
   in the nested function with DECL_VALUE_EXPR set to reference the true
   variable in the parent function.  This is used both for debug info
   and in OMP lowering.  */

static tree
get_nonlocal_debug_decl (struct nesting_info *info, tree decl)
{
  tree target_context;
  struct nesting_info *i;
  tree x, field, new_decl;

  tree *slot = &info->var_map->get_or_insert (decl);

  if (*slot)
    return *slot;

  target_context = decl_function_context (decl);

  /* A copy of the code in get_frame_field, but without the temporaries.  */
  if (info->context == target_context)
    {
      /* Make sure frame_decl gets created.  */
      (void) get_frame_type (info);
      x = info->frame_decl;
      i = info;
      info->static_chain_added |= 1;
    }
  else
    {
      x = get_chain_decl (info);
      info->static_chain_added |= 2;
      for (i = info->outer; i->context != target_context; i = i->outer)
	{
	  field = get_chain_field (i);
	  x = build_simple_mem_ref_notrap (x);
	  x = build3 (COMPONENT_REF, TREE_TYPE (field), x, field, NULL_TREE);
	}
      x = build_simple_mem_ref_notrap (x);
    }

  field = lookup_field_for_decl (i, decl, INSERT);
  x = build3 (COMPONENT_REF, TREE_TYPE (field), x, field, NULL_TREE);
  if (use_pointer_in_frame (decl))
    x = build_simple_mem_ref_notrap (x);

  /* ??? We should be remapping types as well, surely.  */
  new_decl = build_decl (DECL_SOURCE_LOCATION (decl),
			 VAR_DECL, DECL_NAME (decl), TREE_TYPE (decl));
  DECL_CONTEXT (new_decl) = info->context;
  DECL_ARTIFICIAL (new_decl) = DECL_ARTIFICIAL (decl);
  DECL_IGNORED_P (new_decl) = DECL_IGNORED_P (decl);
  TREE_THIS_VOLATILE (new_decl) = TREE_THIS_VOLATILE (decl);
  TREE_SIDE_EFFECTS (new_decl) = TREE_SIDE_EFFECTS (decl);
  TREE_READONLY (new_decl) = TREE_READONLY (decl);
  TREE_ADDRESSABLE (new_decl) = TREE_ADDRESSABLE (decl);
  DECL_SEEN_IN_BIND_EXPR_P (new_decl) = 1;
  if ((TREE_CODE (decl) == PARM_DECL
       || TREE_CODE (decl) == RESULT_DECL
       || VAR_P (decl))
      && DECL_BY_REFERENCE (decl))
    DECL_BY_REFERENCE (new_decl) = 1;

  SET_DECL_VALUE_EXPR (new_decl, x);
  DECL_HAS_VALUE_EXPR_P (new_decl) = 1;

  *slot = new_decl;
  DECL_CHAIN (new_decl) = info->debug_var_chain;
  info->debug_var_chain = new_decl;

  if (!optimize
      && info->context != target_context
      && variably_modified_type_p (TREE_TYPE (decl), NULL))
    note_nonlocal_vla_type (info, TREE_TYPE (decl));

  return new_decl;
}


/* Callback for walk_gimple_stmt, rewrite all references to VAR
   and PARM_DECLs that belong to outer functions.

   The rewrite will involve some number of structure accesses back up
   the static chain.  E.g. for a variable FOO up one nesting level it'll
   be CHAIN->FOO.  For two levels it'll be CHAIN->__chain->FOO.  Further
   indirections apply to decls for which use_pointer_in_frame is true.  */

static tree
convert_nonlocal_reference_op (tree *tp, int *walk_subtrees, void *data)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data;
  struct nesting_info *const info = (struct nesting_info *) wi->info;
  tree t = *tp;

  *walk_subtrees = 0;
  switch (TREE_CODE (t))
    {
    case VAR_DECL:
      /* Non-automatic variables are never processed.  */
      if (TREE_STATIC (t) || DECL_EXTERNAL (t))
	break;
      /* FALLTHRU */

    case PARM_DECL:
      {
	tree x, target_context = decl_function_context (t);

	if (info->context == target_context)
	  break;

	wi->changed = true;

	if (bitmap_bit_p (info->suppress_expansion, DECL_UID (t)))
	  x = get_nonlocal_debug_decl (info, t);
	else
	  {
	    struct nesting_info *i = info;
	    while (i && i->context != target_context)
	      i = i->outer;
	    /* If none of the outer contexts is the target context, this means
	       that the VAR or PARM_DECL is referenced in a wrong context.  */
	    if (!i)
	      internal_error ("%s from %s referenced in %s",
			      IDENTIFIER_POINTER (DECL_NAME (t)),
			      IDENTIFIER_POINTER (DECL_NAME (target_context)),
			      IDENTIFIER_POINTER (DECL_NAME (info->context)));

	    x = lookup_field_for_decl (i, t, INSERT);
	    x = get_frame_field (info, target_context, x, &wi->gsi);
	    if (use_pointer_in_frame (t))
	      {
		x = init_tmp_var (info, x, &wi->gsi);
		x = build_simple_mem_ref_notrap (x);
	      }
	  }

	if (wi->val_only)
	  {
	    if (wi->is_lhs)
	      x = save_tmp_var (info, x, &wi->gsi);
	    else
	      x = init_tmp_var (info, x, &wi->gsi);
	  }

	*tp = x;
      }
      break;

    case LABEL_DECL:
      /* We're taking the address of a label from a parent function, but
	 this is not itself a non-local goto.  Mark the label such that it
	 will not be deleted, much as we would with a label address in
	 static storage.  */
      if (decl_function_context (t) != info->context)
        FORCED_LABEL (t) = 1;
      break;

    case ADDR_EXPR:
      {
	bool save_val_only = wi->val_only;

	wi->val_only = false;
	wi->is_lhs = false;
	wi->changed = false;
	walk_tree (&TREE_OPERAND (t, 0), convert_nonlocal_reference_op, wi, 0);
	wi->val_only = true;

	if (wi->changed)
	  {
	    tree save_context;

	    /* If we changed anything, we might no longer be directly
	       referencing a decl.  */
	    save_context = current_function_decl;
	    current_function_decl = info->context;
	    recompute_tree_invariant_for_addr_expr (t);

	    /* If the callback converted the address argument in a context
	       where we only accept variables (and min_invariant, presumably),
	       then compute the address into a temporary.  */
	    if (save_val_only)
	      *tp = gsi_gimplify_val ((struct nesting_info *) wi->info,
				      t, &wi->gsi);
	    current_function_decl = save_context;
	  }
      }
      break;

    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case COMPONENT_REF:
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
    case BIT_FIELD_REF:
      /* Go down this entire nest and just look at the final prefix and
	 anything that describes the references.  Otherwise, we lose track
	 of whether a NOP_EXPR or VIEW_CONVERT_EXPR needs a simple value.  */
      wi->val_only = true;
      wi->is_lhs = false;
      for (; handled_component_p (t); tp = &TREE_OPERAND (t, 0), t = *tp)
	{
	  if (TREE_CODE (t) == COMPONENT_REF)
	    walk_tree (&TREE_OPERAND (t, 2), convert_nonlocal_reference_op, wi,
		       NULL);
	  else if (TREE_CODE (t) == ARRAY_REF
		   || TREE_CODE (t) == ARRAY_RANGE_REF)
	    {
	      walk_tree (&TREE_OPERAND (t, 1), convert_nonlocal_reference_op,
			 wi, NULL);
	      walk_tree (&TREE_OPERAND (t, 2), convert_nonlocal_reference_op,
			 wi, NULL);
	      walk_tree (&TREE_OPERAND (t, 3), convert_nonlocal_reference_op,
			 wi, NULL);
	    }
	}
      wi->val_only = false;
      walk_tree (tp, convert_nonlocal_reference_op, wi, NULL);
      break;

    case VIEW_CONVERT_EXPR:
      /* Just request to look at the subtrees, leaving val_only and lhs
	 untouched.  This might actually be for !val_only + lhs, in which
	 case we don't want to force a replacement by a temporary.  */
      *walk_subtrees = 1;
      break;

    default:
      if (!IS_TYPE_OR_DECL_P (t))
	{
	  *walk_subtrees = 1;
          wi->val_only = true;
	  wi->is_lhs = false;
	}
      break;
    }

  return NULL_TREE;
}

static tree convert_nonlocal_reference_stmt (gimple_stmt_iterator *, bool *,
					     struct walk_stmt_info *);

/* Helper for convert_nonlocal_references, rewrite all references to VAR
   and PARM_DECLs that belong to outer functions.  */

static bool
convert_nonlocal_omp_clauses (tree *pclauses, struct walk_stmt_info *wi)
{
  struct nesting_info *const info = (struct nesting_info *) wi->info;
  bool need_chain = false, need_stmts = false;
  tree clause, decl, *pdecl;
  int dummy;
  bitmap new_suppress;

  new_suppress = BITMAP_GGC_ALLOC ();
  bitmap_copy (new_suppress, info->suppress_expansion);

  for (clause = *pclauses; clause ; clause = OMP_CLAUSE_CHAIN (clause))
    {
      pdecl = NULL;
      switch (OMP_CLAUSE_CODE (clause))
	{
	case OMP_CLAUSE_REDUCTION:
	case OMP_CLAUSE_IN_REDUCTION:
	case OMP_CLAUSE_TASK_REDUCTION:
	  if (OMP_CLAUSE_REDUCTION_PLACEHOLDER (clause))
	    need_stmts = true;
	  if (TREE_CODE (OMP_CLAUSE_DECL (clause)) == MEM_REF)
	    {
	      pdecl = &TREE_OPERAND (OMP_CLAUSE_DECL (clause), 0);
	      if (TREE_CODE (*pdecl) == POINTER_PLUS_EXPR)
		pdecl = &TREE_OPERAND (*pdecl, 0);
	      if (TREE_CODE (*pdecl) == INDIRECT_REF
		  || TREE_CODE (*pdecl) == ADDR_EXPR)
		pdecl = &TREE_OPERAND (*pdecl, 0);
	    }
	  goto do_decl_clause;

	case OMP_CLAUSE_LASTPRIVATE:
	  if (OMP_CLAUSE_LASTPRIVATE_GIMPLE_SEQ (clause))
	    need_stmts = true;
	  goto do_decl_clause;

	case OMP_CLAUSE_LINEAR:
	  if (OMP_CLAUSE_LINEAR_GIMPLE_SEQ (clause))
	    need_stmts = true;
	  wi->val_only = true;
	  wi->is_lhs = false;
	  convert_nonlocal_reference_op (&OMP_CLAUSE_LINEAR_STEP (clause),
					 &dummy, wi);
	  goto do_decl_clause;

	case OMP_CLAUSE_PRIVATE:
	case OMP_CLAUSE_FIRSTPRIVATE:
	case OMP_CLAUSE_COPYPRIVATE:
	case OMP_CLAUSE_SHARED:
	case OMP_CLAUSE_TO_DECLARE:
	case OMP_CLAUSE_LINK:
	case OMP_CLAUSE_USE_DEVICE_PTR:
	case OMP_CLAUSE_USE_DEVICE_ADDR:
	case OMP_CLAUSE_IS_DEVICE_PTR:
	case OMP_CLAUSE_DETACH:
	do_decl_clause:
	  if (pdecl == NULL)
	    pdecl = &OMP_CLAUSE_DECL (clause);
	  decl = *pdecl;
	  if (VAR_P (decl)
	      && (TREE_STATIC (decl) || DECL_EXTERNAL (decl)))
	    break;
	  if (decl_function_context (decl) != info->context)
	    {
	      if (OMP_CLAUSE_CODE (clause) == OMP_CLAUSE_SHARED)
		OMP_CLAUSE_SHARED_READONLY (clause) = 0;
	      bitmap_set_bit (new_suppress, DECL_UID (decl));
	      *pdecl = get_nonlocal_debug_decl (info, decl);
	      if (OMP_CLAUSE_CODE (clause) != OMP_CLAUSE_PRIVATE)
		need_chain = true;
	    }
	  break;

	case OMP_CLAUSE_SCHEDULE:
	  if (OMP_CLAUSE_SCHEDULE_CHUNK_EXPR (clause) == NULL)
	    break;
	  /* FALLTHRU */
	case OMP_CLAUSE_FINAL:
	case OMP_CLAUSE_IF:
	case OMP_CLAUSE_NUM_THREADS:
	case OMP_CLAUSE_DEPEND:
	case OMP_CLAUSE_DEVICE:
	case OMP_CLAUSE_NUM_TEAMS:
	case OMP_CLAUSE_THREAD_LIMIT:
	case OMP_CLAUSE_SAFELEN:
	case OMP_CLAUSE_SIMDLEN:
	case OMP_CLAUSE_PRIORITY:
	case OMP_CLAUSE_GRAINSIZE:
	case OMP_CLAUSE_NUM_TASKS:
	case OMP_CLAUSE_HINT:
	case OMP_CLAUSE_FILTER:
	case OMP_CLAUSE_NUM_GANGS:
	case OMP_CLAUSE_NUM_WORKERS:
	case OMP_CLAUSE_VECTOR_LENGTH:
	case OMP_CLAUSE_GANG:
	case OMP_CLAUSE_WORKER:
	case OMP_CLAUSE_VECTOR:
	case OMP_CLAUSE_ASYNC:
	case OMP_CLAUSE_WAIT:
	  /* Several OpenACC clauses have optional arguments.  Check if they
	     are present.  */
	  if (OMP_CLAUSE_OPERAND (clause, 0))
	    {
	      wi->val_only = true;
	      wi->is_lhs = false;
	      convert_nonlocal_reference_op (&OMP_CLAUSE_OPERAND (clause, 0),
					     &dummy, wi);
	    }

	  /* The gang clause accepts two arguments.  */
	  if (OMP_CLAUSE_CODE (clause) == OMP_CLAUSE_GANG
	      && OMP_CLAUSE_GANG_STATIC_EXPR (clause))
	    {
		wi->val_only = true;
		wi->is_lhs = false;
		convert_nonlocal_reference_op
		  (&OMP_CLAUSE_GANG_STATIC_EXPR (clause), &dummy, wi);
	    }
	  break;

	case OMP_CLAUSE_DIST_SCHEDULE:
	  if (OMP_CLAUSE_DIST_SCHEDULE_CHUNK_EXPR (clause) != NULL)
	    {
	      wi->val_only = true;
	      wi->is_lhs = false;
	      convert_nonlocal_reference_op (&OMP_CLAUSE_OPERAND (clause, 0),
					     &dummy, wi);
	    }
	  break;

	case OMP_CLAUSE_MAP:
	case OMP_CLAUSE_TO:
	case OMP_CLAUSE_FROM:
	  if (OMP_CLAUSE_SIZE (clause))
	    {
	      wi->val_only = true;
	      wi->is_lhs = false;
	      convert_nonlocal_reference_op (&OMP_CLAUSE_SIZE (clause),
					     &dummy, wi);
	    }
	  if (DECL_P (OMP_CLAUSE_DECL (clause)))
	    goto do_decl_clause;
	  wi->val_only = true;
	  wi->is_lhs = false;
	  walk_tree (&OMP_CLAUSE_DECL (clause), convert_nonlocal_reference_op,
		     wi, NULL);
	  break;

	case OMP_CLAUSE_ALIGNED:
	  if (OMP_CLAUSE_ALIGNED_ALIGNMENT (clause))
	    {
	      wi->val_only = true;
	      wi->is_lhs = false;
	      convert_nonlocal_reference_op
		(&OMP_CLAUSE_ALIGNED_ALIGNMENT (clause), &dummy, wi);
	    }
	  /* FALLTHRU */
	case OMP_CLAUSE_NONTEMPORAL:
	do_decl_clause_no_supp:
	  /* Like do_decl_clause, but don't add any suppression.  */
	  decl = OMP_CLAUSE_DECL (clause);
	  if (VAR_P (decl)
	      && (TREE_STATIC (decl) || DECL_EXTERNAL (decl)))
	    break;
	  if (decl_function_context (decl) != info->context)
	    {
	      OMP_CLAUSE_DECL (clause) = get_nonlocal_debug_decl (info, decl);
	      need_chain = true;
	    }
	  break;

	case OMP_CLAUSE_ALLOCATE:
	  if (OMP_CLAUSE_ALLOCATE_ALLOCATOR (clause))
	    {
	      wi->val_only = true;
	      wi->is_lhs = false;
	      convert_nonlocal_reference_op
		(&OMP_CLAUSE_ALLOCATE_ALLOCATOR (clause), &dummy, wi);
	    }
	  goto do_decl_clause_no_supp;

	case OMP_CLAUSE_NOWAIT:
	case OMP_CLAUSE_ORDERED:
	case OMP_CLAUSE_DEFAULT:
	case OMP_CLAUSE_COPYIN:
	case OMP_CLAUSE_COLLAPSE:
	case OMP_CLAUSE_TILE:
	case OMP_CLAUSE_UNTIED:
	case OMP_CLAUSE_MERGEABLE:
	case OMP_CLAUSE_PROC_BIND:
	case OMP_CLAUSE_NOGROUP:
	case OMP_CLAUSE_THREADS:
	case OMP_CLAUSE_SIMD:
	case OMP_CLAUSE_DEFAULTMAP:
	case OMP_CLAUSE_ORDER:
	case OMP_CLAUSE_SEQ:
	case OMP_CLAUSE_INDEPENDENT:
	case OMP_CLAUSE_AUTO:
	case OMP_CLAUSE_IF_PRESENT:
	case OMP_CLAUSE_FINALIZE:
	case OMP_CLAUSE_BIND:
	case OMP_CLAUSE__CONDTEMP_:
	case OMP_CLAUSE__SCANTEMP_:
	  break;

	  /* The following clause belongs to the OpenACC cache directive, which
	     is discarded during gimplification.  */
	case OMP_CLAUSE__CACHE_:
	  /* The following clauses are only allowed in the OpenMP declare simd
	     directive, so not seen here.  */
	case OMP_CLAUSE_UNIFORM:
	case OMP_CLAUSE_INBRANCH:
	case OMP_CLAUSE_NOTINBRANCH:
	  /* The following clauses are only allowed on OpenMP cancel and
	     cancellation point directives, which at this point have already
	     been lowered into a function call.  */
	case OMP_CLAUSE_FOR:
	case OMP_CLAUSE_PARALLEL:
	case OMP_CLAUSE_SECTIONS:
	case OMP_CLAUSE_TASKGROUP:
	  /* The following clauses are only added during OMP lowering; nested
	     function decomposition happens before that.  */
	case OMP_CLAUSE__LOOPTEMP_:
	case OMP_CLAUSE__REDUCTEMP_:
	case OMP_CLAUSE__SIMDUID_:
	case OMP_CLAUSE__SIMT_:
	  /* The following clauses are only allowed on OpenACC 'routine'
	     directives, not seen here.  */
	case OMP_CLAUSE_NOHOST:
	  /* Anything else.  */
	default:
	  gcc_unreachable ();
	}
    }

  info->suppress_expansion = new_suppress;

  if (need_stmts)
    for (clause = *pclauses; clause ; clause = OMP_CLAUSE_CHAIN (clause))
      switch (OMP_CLAUSE_CODE (clause))
	{
	case OMP_CLAUSE_REDUCTION:
	case OMP_CLAUSE_IN_REDUCTION:
	case OMP_CLAUSE_TASK_REDUCTION:
	  if (OMP_CLAUSE_REDUCTION_PLACEHOLDER (clause))
	    {
	      tree old_context
		= DECL_CONTEXT (OMP_CLAUSE_REDUCTION_PLACEHOLDER (clause));
	      DECL_CONTEXT (OMP_CLAUSE_REDUCTION_PLACEHOLDER (clause))
		= info->context;
	      if (OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (clause))
		DECL_CONTEXT (OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (clause))
		  = info->context;
	      tree save_local_var_chain = info->new_local_var_chain;
	      info->new_local_var_chain = NULL;
	      gimple_seq *seq = &OMP_CLAUSE_REDUCTION_GIMPLE_INIT (clause);
	      walk_body (convert_nonlocal_reference_stmt,
			 convert_nonlocal_reference_op, info, seq);
	      if (info->new_local_var_chain)
		declare_vars (info->new_local_var_chain,
			      gimple_seq_first_stmt (*seq), false);
	      info->new_local_var_chain = NULL;
	      seq = &OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (clause);
	      walk_body (convert_nonlocal_reference_stmt,
			 convert_nonlocal_reference_op, info, seq);
	      if (info->new_local_var_chain)
		declare_vars (info->new_local_var_chain,
			      gimple_seq_first_stmt (*seq), false);
	      info->new_local_var_chain = save_local_var_chain;
	      DECL_CONTEXT (OMP_CLAUSE_REDUCTION_PLACEHOLDER (clause))
		= old_context;
	      if (OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (clause))
		DECL_CONTEXT (OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (clause))
		  = old_context;
	    }
	  break;

	case OMP_CLAUSE_LASTPRIVATE:
	  {
	    tree save_local_var_chain = info->new_local_var_chain;
	    info->new_local_var_chain = NULL;
	    gimple_seq *seq = &OMP_CLAUSE_LASTPRIVATE_GIMPLE_SEQ (clause);
	    walk_body (convert_nonlocal_reference_stmt,
		       convert_nonlocal_reference_op, info, seq);
	    if (info->new_local_var_chain)
	      declare_vars (info->new_local_var_chain,
			    gimple_seq_first_stmt (*seq), false);
	    info->new_local_var_chain = save_local_var_chain;
	  }
	  break;

	case OMP_CLAUSE_LINEAR:
	  {
	    tree save_local_var_chain = info->new_local_var_chain;
	    info->new_local_var_chain = NULL;
	    gimple_seq *seq = &OMP_CLAUSE_LINEAR_GIMPLE_SEQ (clause);
	    walk_body (convert_nonlocal_reference_stmt,
		       convert_nonlocal_reference_op, info, seq);
	    if (info->new_local_var_chain)
	      declare_vars (info->new_local_var_chain,
			    gimple_seq_first_stmt (*seq), false);
	    info->new_local_var_chain = save_local_var_chain;
	  }
	  break;

	default:
	  break;
	}

  return need_chain;
}

/* Create nonlocal debug decls for nonlocal VLA array bounds.  */

static void
note_nonlocal_vla_type (struct nesting_info *info, tree type)
{
  while (POINTER_TYPE_P (type) && !TYPE_NAME (type))
    type = TREE_TYPE (type);

  if (TYPE_NAME (type)
      && TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
      && DECL_ORIGINAL_TYPE (TYPE_NAME (type)))
    type = DECL_ORIGINAL_TYPE (TYPE_NAME (type));

  while (POINTER_TYPE_P (type)
	 || TREE_CODE (type) == VECTOR_TYPE
	 || TREE_CODE (type) == FUNCTION_TYPE
	 || TREE_CODE (type) == METHOD_TYPE)
    type = TREE_TYPE (type);

  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      tree domain, t;

      note_nonlocal_vla_type (info, TREE_TYPE (type));
      domain = TYPE_DOMAIN (type);
      if (domain)
	{
	  t = TYPE_MIN_VALUE (domain);
	  if (t && (VAR_P (t) || TREE_CODE (t) == PARM_DECL)
	      && decl_function_context (t) != info->context)
	    get_nonlocal_debug_decl (info, t);
	  t = TYPE_MAX_VALUE (domain);
	  if (t && (VAR_P (t) || TREE_CODE (t) == PARM_DECL)
	      && decl_function_context (t) != info->context)
	    get_nonlocal_debug_decl (info, t);
	}
    }
}

/* Callback for walk_gimple_stmt.  Rewrite all references to VAR and
   PARM_DECLs that belong to outer functions.  This handles statements
   that are not handled via the standard recursion done in
   walk_gimple_stmt.  STMT is the statement to examine, DATA is as in
   convert_nonlocal_reference_op.  Set *HANDLED_OPS_P to true if all the
   operands of STMT have been handled by this function.  */

static tree
convert_nonlocal_reference_stmt (gimple_stmt_iterator *gsi, bool *handled_ops_p,
				 struct walk_stmt_info *wi)
{
  struct nesting_info *info = (struct nesting_info *) wi->info;
  tree save_local_var_chain;
  bitmap save_suppress;
  gimple *stmt = gsi_stmt (*gsi);

  switch (gimple_code (stmt))
    {
    case GIMPLE_GOTO:
      /* Don't walk non-local gotos for now.  */
      if (TREE_CODE (gimple_goto_dest (stmt)) != LABEL_DECL)
	{
	  wi->val_only = true;
	  wi->is_lhs = false;
	  *handled_ops_p = false;
	  return NULL_TREE;
	}
      break;

    case GIMPLE_OMP_TEAMS:
      if (!gimple_omp_teams_host (as_a <gomp_teams *> (stmt)))
	{
	  save_suppress = info->suppress_expansion;
	  convert_nonlocal_omp_clauses (gimple_omp_teams_clauses_ptr (stmt),
					wi);
	  walk_body (convert_nonlocal_reference_stmt,
		     convert_nonlocal_reference_op, info,
		     gimple_omp_body_ptr (stmt));
	  info->suppress_expansion = save_suppress;
	  break;
	}
      /* FALLTHRU */

    case GIMPLE_OMP_PARALLEL:
    case GIMPLE_OMP_TASK:
      save_suppress = info->suppress_expansion;
      if (convert_nonlocal_omp_clauses (gimple_omp_taskreg_clauses_ptr (stmt),
	                                wi))
	{
	  tree c, decl;
	  decl = get_chain_decl (info);
	  c = build_omp_clause (gimple_location (stmt),
				OMP_CLAUSE_FIRSTPRIVATE);
	  OMP_CLAUSE_DECL (c) = decl;
	  OMP_CLAUSE_CHAIN (c) = gimple_omp_taskreg_clauses (stmt);
	  gimple_omp_taskreg_set_clauses (stmt, c);
	}

      save_local_var_chain = info->new_local_var_chain;
      info->new_local_var_chain = NULL;

      walk_body (convert_nonlocal_reference_stmt, convert_nonlocal_reference_op,
	         info, gimple_omp_body_ptr (stmt));

      if (info->new_local_var_chain)
	declare_vars (info->new_local_var_chain,
	              gimple_seq_first_stmt (gimple_omp_body (stmt)),
		      false);
      info->new_local_var_chain = save_local_var_chain;
      info->suppress_expansion = save_suppress;
      break;

    case GIMPLE_OMP_FOR:
      save_suppress = info->suppress_expansion;
      convert_nonlocal_omp_clauses (gimple_omp_for_clauses_ptr (stmt), wi);
      walk_gimple_omp_for (as_a <gomp_for *> (stmt),
			   convert_nonlocal_reference_stmt,
	  		   convert_nonlocal_reference_op, info);
      walk_body (convert_nonlocal_reference_stmt,
	  	 convert_nonlocal_reference_op, info, gimple_omp_body_ptr (stmt));
      info->suppress_expansion = save_suppress;
      break;

    case GIMPLE_OMP_SECTIONS:
      save_suppress = info->suppress_expansion;
      convert_nonlocal_omp_clauses (gimple_omp_sections_clauses_ptr (stmt), wi);
      walk_body (convert_nonlocal_reference_stmt, convert_nonlocal_reference_op,
	         info, gimple_omp_body_ptr (stmt));
      info->suppress_expansion = save_suppress;
      break;

    case GIMPLE_OMP_SINGLE:
      save_suppress = info->suppress_expansion;
      convert_nonlocal_omp_clauses (gimple_omp_single_clauses_ptr (stmt), wi);
      walk_body (convert_nonlocal_reference_stmt, convert_nonlocal_reference_op,
	         info, gimple_omp_body_ptr (stmt));
      info->suppress_expansion = save_suppress;
      break;

    case GIMPLE_OMP_SCOPE:
      save_suppress = info->suppress_expansion;
      convert_nonlocal_omp_clauses (gimple_omp_scope_clauses_ptr (stmt), wi);
      walk_body (convert_nonlocal_reference_stmt, convert_nonlocal_reference_op,
		 info, gimple_omp_body_ptr (stmt));
      info->suppress_expansion = save_suppress;
      break;

    case GIMPLE_OMP_TASKGROUP:
      save_suppress = info->suppress_expansion;
      convert_nonlocal_omp_clauses (gimple_omp_taskgroup_clauses_ptr (stmt), wi);
      walk_body (convert_nonlocal_reference_stmt, convert_nonlocal_reference_op,
		 info, gimple_omp_body_ptr (stmt));
      info->suppress_expansion = save_suppress;
      break;

    case GIMPLE_OMP_TARGET:
      if (!is_gimple_omp_offloaded (stmt))
	{
	  save_suppress = info->suppress_expansion;
	  convert_nonlocal_omp_clauses (gimple_omp_target_clauses_ptr (stmt),
					wi);
	  info->suppress_expansion = save_suppress;
	  walk_body (convert_nonlocal_reference_stmt,
		     convert_nonlocal_reference_op, info,
		     gimple_omp_body_ptr (stmt));
	  break;
	}
      save_suppress = info->suppress_expansion;
      if (convert_nonlocal_omp_clauses (gimple_omp_target_clauses_ptr (stmt),
					wi))
	{
	  tree c, decl;
	  decl = get_chain_decl (info);
	  c = build_omp_clause (gimple_location (stmt), OMP_CLAUSE_MAP);
	  OMP_CLAUSE_DECL (c) = decl;
	  OMP_CLAUSE_SET_MAP_KIND (c, GOMP_MAP_TO);
	  OMP_CLAUSE_SIZE (c) = DECL_SIZE_UNIT (decl);
	  OMP_CLAUSE_CHAIN (c) = gimple_omp_target_clauses (stmt);
	  gimple_omp_target_set_clauses (as_a <gomp_target *> (stmt), c);
	}

      save_local_var_chain = info->new_local_var_chain;
      info->new_local_var_chain = NULL;

      walk_body (convert_nonlocal_reference_stmt, convert_nonlocal_reference_op,
		 info, gimple_omp_body_ptr (stmt));

      if (info->new_local_var_chain)
	declare_vars (info->new_local_var_chain,
		      gimple_seq_first_stmt (gimple_omp_body (stmt)),
		      false);
      info->new_local_var_chain = save_local_var_chain;
      info->suppress_expansion = save_suppress;
      break;

    case GIMPLE_OMP_SECTION:
    case GIMPLE_OMP_MASTER:
    case GIMPLE_OMP_MASKED:
    case GIMPLE_OMP_ORDERED:
    case GIMPLE_OMP_SCAN:
      walk_body (convert_nonlocal_reference_stmt, convert_nonlocal_reference_op,
	         info, gimple_omp_body_ptr (stmt));
      break;

    case GIMPLE_BIND:
      {
      gbind *bind_stmt = as_a <gbind *> (stmt);

      for (tree var = gimple_bind_vars (bind_stmt); var; var = DECL_CHAIN (var))
	if (TREE_CODE (var) == NAMELIST_DECL)
	  {
	    /* Adjust decls mentioned in NAMELIST_DECL.  */
	    tree decls = NAMELIST_DECL_ASSOCIATED_DECL (var);
	    tree decl;
	    unsigned int i;

	    FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (decls), i, decl)
	      {
		if (VAR_P (decl)
		    && (TREE_STATIC (decl) || DECL_EXTERNAL (decl)))
		  continue;
		if (decl_function_context (decl) != info->context)
		  CONSTRUCTOR_ELT (decls, i)->value
		    = get_nonlocal_debug_decl (info, decl);
	      }
	  }

      *handled_ops_p = false;
      return NULL_TREE;
      }
    case GIMPLE_COND:
      wi->val_only = true;
      wi->is_lhs = false;
      *handled_ops_p = false;
      return NULL_TREE;

    case GIMPLE_ASSIGN:
      if (gimple_clobber_p (stmt))
	{
	  tree lhs = gimple_assign_lhs (stmt);
	  if (DECL_P (lhs)
	      && !(TREE_STATIC (lhs) || DECL_EXTERNAL (lhs))
	      && decl_function_context (lhs) != info->context)
	    {
	      gsi_replace (gsi, gimple_build_nop (), true);
	      break;
	    }
	}
      *handled_ops_p = false;
      return NULL_TREE;

    default:
      /* For every other statement that we are not interested in
	 handling here, let the walker traverse the operands.  */
      *handled_ops_p = false;
      return NULL_TREE;
    }

  /* We have handled all of STMT operands, no need to traverse the operands.  */
  *handled_ops_p = true;
  return NULL_TREE;
}


/* A subroutine of convert_local_reference.  Create a local variable
   in the parent function with DECL_VALUE_EXPR set to reference the
   field in FRAME.  This is used both for debug info and in OMP
   lowering.  */

static tree
get_local_debug_decl (struct nesting_info *info, tree decl, tree field)
{
  tree x, new_decl;

  tree *slot = &info->var_map->get_or_insert (decl);
  if (*slot)
    return *slot;

  /* Make sure frame_decl gets created.  */
  (void) get_frame_type (info);
  x = info->frame_decl;
  x = build3 (COMPONENT_REF, TREE_TYPE (field), x, field, NULL_TREE);

  new_decl = build_decl (DECL_SOURCE_LOCATION (decl),
			 VAR_DECL, DECL_NAME (decl), TREE_TYPE (decl));
  DECL_CONTEXT (new_decl) = info->context;
  DECL_ARTIFICIAL (new_decl) = DECL_ARTIFICIAL (decl);
  DECL_IGNORED_P (new_decl) = DECL_IGNORED_P (decl);
  TREE_THIS_VOLATILE (new_decl) = TREE_THIS_VOLATILE (decl);
  TREE_SIDE_EFFECTS (new_decl) = TREE_SIDE_EFFECTS (decl);
  TREE_READONLY (new_decl) = TREE_READONLY (decl);
  TREE_ADDRESSABLE (new_decl) = TREE_ADDRESSABLE (decl);
  DECL_SEEN_IN_BIND_EXPR_P (new_decl) = 1;
  if ((TREE_CODE (decl) == PARM_DECL
       || TREE_CODE (decl) == RESULT_DECL
       || VAR_P (decl))
      && DECL_BY_REFERENCE (decl))
    DECL_BY_REFERENCE (new_decl) = 1;

  SET_DECL_VALUE_EXPR (new_decl, x);
  DECL_HAS_VALUE_EXPR_P (new_decl) = 1;
  *slot = new_decl;

  DECL_CHAIN (new_decl) = info->debug_var_chain;
  info->debug_var_chain = new_decl;

  /* Do not emit debug info twice.  */
  DECL_IGNORED_P (decl) = 1;

  return new_decl;
}


/* Called via walk_function+walk_gimple_stmt, rewrite all references to VAR
   and PARM_DECLs that were referenced by inner nested functions.
   The rewrite will be a structure reference to the local frame variable.  */

static bool convert_local_omp_clauses (tree *, struct walk_stmt_info *);

static tree
convert_local_reference_op (tree *tp, int *walk_subtrees, void *data)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data;
  struct nesting_info *const info = (struct nesting_info *) wi->info;
  tree t = *tp, field, x;
  bool save_val_only;

  *walk_subtrees = 0;
  switch (TREE_CODE (t))
    {
    case VAR_DECL:
      /* Non-automatic variables are never processed.  */
      if (TREE_STATIC (t) || DECL_EXTERNAL (t))
	break;
      /* FALLTHRU */

    case PARM_DECL:
      if (t != info->frame_decl && decl_function_context (t) == info->context)
	{
	  /* If we copied a pointer to the frame, then the original decl
	     is used unchanged in the parent function.  */
	  if (use_pointer_in_frame (t))
	    break;

	  /* No need to transform anything if no child references the
	     variable.  */
	  field = lookup_field_for_decl (info, t, NO_INSERT);
	  if (!field)
	    break;
	  wi->changed = true;

	  if (bitmap_bit_p (info->suppress_expansion, DECL_UID (t)))
	    x = get_local_debug_decl (info, t, field);
	  else
	    x = get_frame_field (info, info->context, field, &wi->gsi);

	  if (wi->val_only)
	    {
	      if (wi->is_lhs)
		x = save_tmp_var (info, x, &wi->gsi);
	      else
		x = init_tmp_var (info, x, &wi->gsi);
	    }

	  *tp = x;
	}
      break;

    case ADDR_EXPR:
      save_val_only = wi->val_only;
      wi->val_only = false;
      wi->is_lhs = false;
      wi->changed = false;
      walk_tree (&TREE_OPERAND (t, 0), convert_local_reference_op, wi, NULL);
      wi->val_only = save_val_only;

      /* If we converted anything ... */
      if (wi->changed)
	{
	  tree save_context;

	  /* Then the frame decl is now addressable.  */
	  TREE_ADDRESSABLE (info->frame_decl) = 1;

	  save_context = current_function_decl;
	  current_function_decl = info->context;
	  recompute_tree_invariant_for_addr_expr (t);

	  /* If we are in a context where we only accept values, then
	     compute the address into a temporary.  */
	  if (save_val_only)
	    *tp = gsi_gimplify_val ((struct nesting_info *) wi->info,
				    t, &wi->gsi);
	  current_function_decl = save_context;
	}
      break;

    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case COMPONENT_REF:
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
    case BIT_FIELD_REF:
      /* Go down this entire nest and just look at the final prefix and
	 anything that describes the references.  Otherwise, we lose track
	 of whether a NOP_EXPR or VIEW_CONVERT_EXPR needs a simple value.  */
      save_val_only = wi->val_only;
      wi->val_only = true;
      wi->is_lhs = false;
      for (; handled_component_p (t); tp = &TREE_OPERAND (t, 0), t = *tp)
	{
	  if (TREE_CODE (t) == COMPONENT_REF)
	    walk_tree (&TREE_OPERAND (t, 2), convert_local_reference_op, wi,
		       NULL);
	  else if (TREE_CODE (t) == ARRAY_REF
		   || TREE_CODE (t) == ARRAY_RANGE_REF)
	    {
	      walk_tree (&TREE_OPERAND (t, 1), convert_local_reference_op, wi,
			 NULL);
	      walk_tree (&TREE_OPERAND (t, 2), convert_local_reference_op, wi,
			 NULL);
	      walk_tree (&TREE_OPERAND (t, 3), convert_local_reference_op, wi,
			 NULL);
	    }
	}
      wi->val_only = false;
      walk_tree (tp, convert_local_reference_op, wi, NULL);
      wi->val_only = save_val_only;
      break;

    case MEM_REF:
      save_val_only = wi->val_only;
      wi->val_only = true;
      wi->is_lhs = false;
      walk_tree (&TREE_OPERAND (t, 0), convert_local_reference_op,
		 wi, NULL);
      /* We need to re-fold the MEM_REF as component references as
	 part of a ADDR_EXPR address are not allowed.  But we cannot
	 fold here, as the chain record type is not yet finalized.  */
      if (TREE_CODE (TREE_OPERAND (t, 0)) == ADDR_EXPR
	  && !DECL_P (TREE_OPERAND (TREE_OPERAND (t, 0), 0)))
	info->mem_refs->add (tp);
      wi->val_only = save_val_only;
      break;

    case VIEW_CONVERT_EXPR:
      /* Just request to look at the subtrees, leaving val_only and lhs
	 untouched.  This might actually be for !val_only + lhs, in which
	 case we don't want to force a replacement by a temporary.  */
      *walk_subtrees = 1;
      break;

    default:
      if (!IS_TYPE_OR_DECL_P (t))
	{
	  *walk_subtrees = 1;
	  wi->val_only = true;
	  wi->is_lhs = false;
	}
      break;
    }

  return NULL_TREE;
}

static tree convert_local_reference_stmt (gimple_stmt_iterator *, bool *,
					  struct walk_stmt_info *);

/* Helper for convert_local_reference.  Convert all the references in
   the chain of clauses at *PCLAUSES.  WI is as in convert_local_reference.  */

static bool
convert_local_omp_clauses (tree *pclauses, struct walk_stmt_info *wi)
{
  struct nesting_info *const info = (struct nesting_info *) wi->info;
  bool need_frame = false, need_stmts = false;
  tree clause, decl, *pdecl;
  int dummy;
  bitmap new_suppress;

  new_suppress = BITMAP_GGC_ALLOC ();
  bitmap_copy (new_suppress, info->suppress_expansion);

  for (clause = *pclauses; clause ; clause = OMP_CLAUSE_CHAIN (clause))
    {
      pdecl = NULL;
      switch (OMP_CLAUSE_CODE (clause))
	{
	case OMP_CLAUSE_REDUCTION:
	case OMP_CLAUSE_IN_REDUCTION:
	case OMP_CLAUSE_TASK_REDUCTION:
	  if (OMP_CLAUSE_REDUCTION_PLACEHOLDER (clause))
	    need_stmts = true;
	  if (TREE_CODE (OMP_CLAUSE_DECL (clause)) == MEM_REF)
	    {
	      pdecl = &TREE_OPERAND (OMP_CLAUSE_DECL (clause), 0);
	      if (TREE_CODE (*pdecl) == POINTER_PLUS_EXPR)
		pdecl = &TREE_OPERAND (*pdecl, 0);
	      if (TREE_CODE (*pdecl) == INDIRECT_REF
		  || TREE_CODE (*pdecl) == ADDR_EXPR)
		pdecl = &TREE_OPERAND (*pdecl, 0);
	    }
	  goto do_decl_clause;

	case OMP_CLAUSE_LASTPRIVATE:
	  if (OMP_CLAUSE_LASTPRIVATE_GIMPLE_SEQ (clause))
	    need_stmts = true;
	  goto do_decl_clause;

	case OMP_CLAUSE_LINEAR:
	  if (OMP_CLAUSE_LINEAR_GIMPLE_SEQ (clause))
	    need_stmts = true;
	  wi->val_only = true;
	  wi->is_lhs = false;
	  convert_local_reference_op (&OMP_CLAUSE_LINEAR_STEP (clause), &dummy,
				      wi);
	  goto do_decl_clause;

	case OMP_CLAUSE_PRIVATE:
	case OMP_CLAUSE_FIRSTPRIVATE:
	case OMP_CLAUSE_COPYPRIVATE:
	case OMP_CLAUSE_SHARED:
	case OMP_CLAUSE_TO_DECLARE:
	case OMP_CLAUSE_LINK:
	case OMP_CLAUSE_USE_DEVICE_PTR:
	case OMP_CLAUSE_USE_DEVICE_ADDR:
	case OMP_CLAUSE_IS_DEVICE_PTR:
	case OMP_CLAUSE_DETACH:
	do_decl_clause:
	  if (pdecl == NULL)
	    pdecl = &OMP_CLAUSE_DECL (clause);
	  decl = *pdecl;
	  if (VAR_P (decl)
	      && (TREE_STATIC (decl) || DECL_EXTERNAL (decl)))
	    break;
	  if (decl_function_context (decl) == info->context
	      && !use_pointer_in_frame (decl))
	    {
	      tree field = lookup_field_for_decl (info, decl, NO_INSERT);
	      if (field)
		{
		  if (OMP_CLAUSE_CODE (clause) == OMP_CLAUSE_SHARED)
		    OMP_CLAUSE_SHARED_READONLY (clause) = 0;
		  bitmap_set_bit (new_suppress, DECL_UID (decl));
		  *pdecl = get_local_debug_decl (info, decl, field);
		  need_frame = true;
		}
	    }
	  break;

	case OMP_CLAUSE_SCHEDULE:
	  if (OMP_CLAUSE_SCHEDULE_CHUNK_EXPR (clause) == NULL)
	    break;
	  /* FALLTHRU */
	case OMP_CLAUSE_FINAL:
	case OMP_CLAUSE_IF:
	case OMP_CLAUSE_NUM_THREADS:
	case OMP_CLAUSE_DEPEND:
	case OMP_CLAUSE_DEVICE:
	case OMP_CLAUSE_NUM_TEAMS:
	case OMP_CLAUSE_THREAD_LIMIT:
	case OMP_CLAUSE_SAFELEN:
	case OMP_CLAUSE_SIMDLEN:
	case OMP_CLAUSE_PRIORITY:
	case OMP_CLAUSE_GRAINSIZE:
	case OMP_CLAUSE_NUM_TASKS:
	case OMP_CLAUSE_HINT:
	case OMP_CLAUSE_FILTER:
	case OMP_CLAUSE_NUM_GANGS:
	case OMP_CLAUSE_NUM_WORKERS:
	case OMP_CLAUSE_VECTOR_LENGTH:
	case OMP_CLAUSE_GANG:
	case OMP_CLAUSE_WORKER:
	case OMP_CLAUSE_VECTOR:
	case OMP_CLAUSE_ASYNC:
	case OMP_CLAUSE_WAIT:
	  /* Several OpenACC clauses have optional arguments.  Check if they
	     are present.  */
	  if (OMP_CLAUSE_OPERAND (clause, 0))
	    {
	      wi->val_only = true;
	      wi->is_lhs = false;
	      convert_local_reference_op (&OMP_CLAUSE_OPERAND (clause, 0),
					  &dummy, wi);
	    }

	  /* The gang clause accepts two arguments.  */
	  if (OMP_CLAUSE_CODE (clause) == OMP_CLAUSE_GANG
	      && OMP_CLAUSE_GANG_STATIC_EXPR (clause))
	    {
		wi->val_only = true;
		wi->is_lhs = false;
		convert_nonlocal_reference_op
		  (&OMP_CLAUSE_GANG_STATIC_EXPR (clause), &dummy, wi);
	    }
	  break;

	case OMP_CLAUSE_DIST_SCHEDULE:
	  if (OMP_CLAUSE_DIST_SCHEDULE_CHUNK_EXPR (clause) != NULL)
	    {
	      wi->val_only = true;
	      wi->is_lhs = false;
	      convert_local_reference_op (&OMP_CLAUSE_OPERAND (clause, 0),
					  &dummy, wi);
	    }
	  break;

	case OMP_CLAUSE_MAP:
	case OMP_CLAUSE_TO:
	case OMP_CLAUSE_FROM:
	  if (OMP_CLAUSE_SIZE (clause))
	    {
	      wi->val_only = true;
	      wi->is_lhs = false;
	      convert_local_reference_op (&OMP_CLAUSE_SIZE (clause),
					  &dummy, wi);
	    }
	  if (DECL_P (OMP_CLAUSE_DECL (clause)))
	    goto do_decl_clause;
	  wi->val_only = true;
	  wi->is_lhs = false;
	  walk_tree (&OMP_CLAUSE_DECL (clause), convert_local_reference_op,
		     wi, NULL);
	  break;

	case OMP_CLAUSE_ALIGNED:
	  if (OMP_CLAUSE_ALIGNED_ALIGNMENT (clause))
	    {
	      wi->val_only = true;
	      wi->is_lhs = false;
	      convert_local_reference_op
		(&OMP_CLAUSE_ALIGNED_ALIGNMENT (clause), &dummy, wi);
	    }
	  /* FALLTHRU */
	case OMP_CLAUSE_NONTEMPORAL:
	do_decl_clause_no_supp:
	  /* Like do_decl_clause, but don't add any suppression.  */
	  decl = OMP_CLAUSE_DECL (clause);
	  if (VAR_P (decl)
	      && (TREE_STATIC (decl) || DECL_EXTERNAL (decl)))
	    break;
	  if (decl_function_context (decl) == info->context
	      && !use_pointer_in_frame (decl))
	    {
	      tree field = lookup_field_for_decl (info, decl, NO_INSERT);
	      if (field)
		{
		  OMP_CLAUSE_DECL (clause)
		    = get_local_debug_decl (info, decl, field);
		  need_frame = true;
		}
	    }
	  break;

	case OMP_CLAUSE_ALLOCATE:
	  if (OMP_CLAUSE_ALLOCATE_ALLOCATOR (clause))
	    {
	      wi->val_only = true;
	      wi->is_lhs = false;
	      convert_local_reference_op
		(&OMP_CLAUSE_ALLOCATE_ALLOCATOR (clause), &dummy, wi);
	    }
	  goto do_decl_clause_no_supp;

	case OMP_CLAUSE_NOWAIT:
	case OMP_CLAUSE_ORDERED:
	case OMP_CLAUSE_DEFAULT:
	case OMP_CLAUSE_COPYIN:
	case OMP_CLAUSE_COLLAPSE:
	case OMP_CLAUSE_TILE:
	case OMP_CLAUSE_UNTIED:
	case OMP_CLAUSE_MERGEABLE:
	case OMP_CLAUSE_PROC_BIND:
	case OMP_CLAUSE_NOGROUP:
	case OMP_CLAUSE_THREADS:
	case OMP_CLAUSE_SIMD:
	case OMP_CLAUSE_DEFAULTMAP:
	case OMP_CLAUSE_ORDER:
	case OMP_CLAUSE_SEQ:
	case OMP_CLAUSE_INDEPENDENT:
	case OMP_CLAUSE_AUTO:
	case OMP_CLAUSE_IF_PRESENT:
	case OMP_CLAUSE_FINALIZE:
	case OMP_CLAUSE_BIND:
	case OMP_CLAUSE__CONDTEMP_:
	case OMP_CLAUSE__SCANTEMP_:
	  break;

	  /* The following clause belongs to the OpenACC cache directive, which
	     is discarded during gimplification.  */
	case OMP_CLAUSE__CACHE_:
	  /* The following clauses are only allowed in the OpenMP declare simd
	     directive, so not seen here.  */
	case OMP_CLAUSE_UNIFORM:
	case OMP_CLAUSE_INBRANCH:
	case OMP_CLAUSE_NOTINBRANCH:
	  /* The following clauses are only allowed on OpenMP cancel and
	     cancellation point directives, which at this point have already
	     been lowered into a function call.  */
	case OMP_CLAUSE_FOR:
	case OMP_CLAUSE_PARALLEL:
	case OMP_CLAUSE_SECTIONS:
	case OMP_CLAUSE_TASKGROUP:
	  /* The following clauses are only added during OMP lowering; nested
	     function decomposition happens before that.  */
	case OMP_CLAUSE__LOOPTEMP_:
	case OMP_CLAUSE__REDUCTEMP_:
	case OMP_CLAUSE__SIMDUID_:
	case OMP_CLAUSE__SIMT_:
	  /* The following clauses are only allowed on OpenACC 'routine'
	     directives, not seen here.  */
	case OMP_CLAUSE_NOHOST:
	  /* Anything else.  */
	default:
	  gcc_unreachable ();
	}
    }

  info->suppress_expansion = new_suppress;

  if (need_stmts)
    for (clause = *pclauses; clause ; clause = OMP_CLAUSE_CHAIN (clause))
      switch (OMP_CLAUSE_CODE (clause))
	{
	case OMP_CLAUSE_REDUCTION:
	case OMP_CLAUSE_IN_REDUCTION:
	case OMP_CLAUSE_TASK_REDUCTION:
	  if (OMP_CLAUSE_REDUCTION_PLACEHOLDER (clause))
	    {
	      tree old_context
		= DECL_CONTEXT (OMP_CLAUSE_REDUCTION_PLACEHOLDER (clause));
	      DECL_CONTEXT (OMP_CLAUSE_REDUCTION_PLACEHOLDER (clause))
		= info->context;
	      if (OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (clause))
		DECL_CONTEXT (OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (clause))
		  = info->context;
	      walk_body (convert_local_reference_stmt,
			 convert_local_reference_op, info,
			 &OMP_CLAUSE_REDUCTION_GIMPLE_INIT (clause));
	      walk_body (convert_local_reference_stmt,
			 convert_local_reference_op, info,
			 &OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (clause));
	      DECL_CONTEXT (OMP_CLAUSE_REDUCTION_PLACEHOLDER (clause))
		= old_context;
	      if (OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (clause))
		DECL_CONTEXT (OMP_CLAUSE_REDUCTION_DECL_PLACEHOLDER (clause))
		  = old_context;
	    }
	  break;

	case OMP_CLAUSE_LASTPRIVATE:
	  walk_body (convert_local_reference_stmt,
		     convert_local_reference_op, info,
		     &OMP_CLAUSE_LASTPRIVATE_GIMPLE_SEQ (clause));
	  break;

	case OMP_CLAUSE_LINEAR:
	  walk_body (convert_local_reference_stmt,
		     convert_local_reference_op, info,
		     &OMP_CLAUSE_LINEAR_GIMPLE_SEQ (clause));
	  break;

	default:
	  break;
	}

  return need_frame;
}


/* Called via walk_function+walk_gimple_stmt, rewrite all references to VAR
   and PARM_DECLs that were referenced by inner nested functions.
   The rewrite will be a structure reference to the local frame variable.  */

static tree
convert_local_reference_stmt (gimple_stmt_iterator *gsi, bool *handled_ops_p,
			      struct walk_stmt_info *wi)
{
  struct nesting_info *info = (struct nesting_info *) wi->info;
  tree save_local_var_chain;
  bitmap save_suppress;
  char save_static_chain_added;
  bool frame_decl_added;
  gimple *stmt = gsi_stmt (*gsi);

  switch (gimple_code (stmt))
    {
    case GIMPLE_OMP_TEAMS:
      if (!gimple_omp_teams_host (as_a <gomp_teams *> (stmt)))
	{
	  save_suppress = info->suppress_expansion;
	  convert_local_omp_clauses (gimple_omp_teams_clauses_ptr (stmt), wi);
	  walk_body (convert_local_reference_stmt, convert_local_reference_op,
		     info, gimple_omp_body_ptr (stmt));
	  info->suppress_expansion = save_suppress;
	  break;
	}
      /* FALLTHRU */

    case GIMPLE_OMP_PARALLEL:
    case GIMPLE_OMP_TASK:
      save_suppress = info->suppress_expansion;
      frame_decl_added = false;
      if (convert_local_omp_clauses (gimple_omp_taskreg_clauses_ptr (stmt),
	                             wi))
	{
	  tree c = build_omp_clause (gimple_location (stmt),
				     OMP_CLAUSE_SHARED);
	  (void) get_frame_type (info);
	  OMP_CLAUSE_DECL (c) = info->frame_decl;
	  OMP_CLAUSE_CHAIN (c) = gimple_omp_taskreg_clauses (stmt);
	  gimple_omp_taskreg_set_clauses (stmt, c);
	  info->static_chain_added |= 4;
	  frame_decl_added = true;
	}

      save_local_var_chain = info->new_local_var_chain;
      save_static_chain_added = info->static_chain_added;
      info->new_local_var_chain = NULL;
      info->static_chain_added = 0;

      walk_body (convert_local_reference_stmt, convert_local_reference_op, info,
	         gimple_omp_body_ptr (stmt));

      if ((info->static_chain_added & 4) != 0 && !frame_decl_added)
	{
	  tree c = build_omp_clause (gimple_location (stmt),
				     OMP_CLAUSE_SHARED);
	  (void) get_frame_type (info);
	  OMP_CLAUSE_DECL (c) = info->frame_decl;
	  OMP_CLAUSE_CHAIN (c) = gimple_omp_taskreg_clauses (stmt);
	  info->static_chain_added |= 4;
	  gimple_omp_taskreg_set_clauses (stmt, c);
	}
      if (info->new_local_var_chain)
	declare_vars (info->new_local_var_chain,
		      gimple_seq_first_stmt (gimple_omp_body (stmt)), false);
      info->new_local_var_chain = save_local_var_chain;
      info->suppress_expansion = save_suppress;
      info->static_chain_added |= save_static_chain_added;
      break;

    case GIMPLE_OMP_FOR:
      save_suppress = info->suppress_expansion;
      convert_local_omp_clauses (gimple_omp_for_clauses_ptr (stmt), wi);
      walk_gimple_omp_for (as_a <gomp_for *> (stmt),
			   convert_local_reference_stmt,
			   convert_local_reference_op, info);
      walk_body (convert_local_reference_stmt, convert_local_reference_op,
		 info, gimple_omp_body_ptr (stmt));
      info->suppress_expansion = save_suppress;
      break;

    case GIMPLE_OMP_SECTIONS:
      save_suppress = info->suppress_expansion;
      convert_local_omp_clauses (gimple_omp_sections_clauses_ptr (stmt), wi);
      walk_body (convert_local_reference_stmt, convert_local_reference_op,
		 info, gimple_omp_body_ptr (stmt));
      info->suppress_expansion = save_suppress;
      break;

    case GIMPLE_OMP_SINGLE:
      save_suppress = info->suppress_expansion;
      convert_local_omp_clauses (gimple_omp_single_clauses_ptr (stmt), wi);
      walk_body (convert_local_reference_stmt, convert_local_reference_op,
		 info, gimple_omp_body_ptr (stmt));
      info->suppress_expansion = save_suppress;
      break;

    case GIMPLE_OMP_SCOPE:
      save_suppress = info->suppress_expansion;
      convert_local_omp_clauses (gimple_omp_scope_clauses_ptr (stmt), wi);
      walk_body (convert_local_reference_stmt, convert_local_reference_op,
		 info, gimple_omp_body_ptr (stmt));
      info->suppress_expansion = save_suppress;
      break;

    case GIMPLE_OMP_TASKGROUP:
      save_suppress = info->suppress_expansion;
      convert_local_omp_clauses (gimple_omp_taskgroup_clauses_ptr (stmt), wi);
      walk_body (convert_local_reference_stmt, convert_local_reference_op,
		 info, gimple_omp_body_ptr (stmt));
      info->suppress_expansion = save_suppress;
      break;

    case GIMPLE_OMP_TARGET:
      if (!is_gimple_omp_offloaded (stmt))
	{
	  save_suppress = info->suppress_expansion;
	  convert_local_omp_clauses (gimple_omp_target_clauses_ptr (stmt), wi);
	  info->suppress_expansion = save_suppress;
	  walk_body (convert_local_reference_stmt, convert_local_reference_op,
		     info, gimple_omp_body_ptr (stmt));
	  break;
	}
      save_suppress = info->suppress_expansion;
      frame_decl_added = false;
      if (convert_local_omp_clauses (gimple_omp_target_clauses_ptr (stmt), wi))
	{
	  tree c = build_omp_clause (gimple_location (stmt), OMP_CLAUSE_MAP);
	  (void) get_frame_type (info);
	  OMP_CLAUSE_DECL (c) = info->frame_decl;
	  OMP_CLAUSE_SET_MAP_KIND (c, GOMP_MAP_TOFROM);
	  OMP_CLAUSE_SIZE (c) = DECL_SIZE_UNIT (info->frame_decl);
	  OMP_CLAUSE_CHAIN (c) = gimple_omp_target_clauses (stmt);
	  gimple_omp_target_set_clauses (as_a <gomp_target *> (stmt), c);
	  info->static_chain_added |= 4;
	  frame_decl_added = true;
	}

      save_local_var_chain = info->new_local_var_chain;
      save_static_chain_added = info->static_chain_added;
      info->new_local_var_chain = NULL;
      info->static_chain_added = 0;

      walk_body (convert_local_reference_stmt, convert_local_reference_op, info,
		 gimple_omp_body_ptr (stmt));

      if ((info->static_chain_added & 4) != 0 && !frame_decl_added)
	{
	  tree c = build_omp_clause (gimple_location (stmt), OMP_CLAUSE_MAP);
	  (void) get_frame_type (info);
	  OMP_CLAUSE_DECL (c) = info->frame_decl;
	  OMP_CLAUSE_SET_MAP_KIND (c, GOMP_MAP_TOFROM);
	  OMP_CLAUSE_SIZE (c) = DECL_SIZE_UNIT (info->frame_decl);
	  OMP_CLAUSE_CHAIN (c) = gimple_omp_target_clauses (stmt);
	  gimple_omp_target_set_clauses (as_a <gomp_target *> (stmt), c);
	  info->static_chain_added |= 4;
	}

      if (info->new_local_var_chain)
	declare_vars (info->new_local_var_chain,
		      gimple_seq_first_stmt (gimple_omp_body (stmt)), false);
      info->new_local_var_chain = save_local_var_chain;
      info->suppress_expansion = save_suppress;
      info->static_chain_added |= save_static_chain_added;
      break;

    case GIMPLE_OMP_SECTION:
    case GIMPLE_OMP_MASTER:
    case GIMPLE_OMP_MASKED:
    case GIMPLE_OMP_ORDERED:
    case GIMPLE_OMP_SCAN:
      walk_body (convert_local_reference_stmt, convert_local_reference_op,
		 info, gimple_omp_body_ptr (stmt));
      break;

    case GIMPLE_COND:
      wi->val_only = true;
      wi->is_lhs = false;
      *handled_ops_p = false;
      return NULL_TREE;

    case GIMPLE_ASSIGN:
      if (gimple_clobber_p (stmt))
	{
	  tree lhs = gimple_assign_lhs (stmt);
	  if (DECL_P (lhs)
	      && decl_function_context (lhs) == info->context
	      && !use_pointer_in_frame (lhs)
	      && lookup_field_for_decl (info, lhs, NO_INSERT))
	    {
	      gsi_replace (gsi, gimple_build_nop (), true);
	      break;
	    }
	}
      *handled_ops_p = false;
      return NULL_TREE;

    case GIMPLE_BIND:
      for (tree var = gimple_bind_vars (as_a <gbind *> (stmt));
	   var;
	   var = DECL_CHAIN (var))
	if (TREE_CODE (var) == NAMELIST_DECL)
	  {
	    /* Adjust decls mentioned in NAMELIST_DECL.  */
	    tree decls = NAMELIST_DECL_ASSOCIATED_DECL (var);
	    tree decl;
	    unsigned int i;

	    FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (decls), i, decl)
	      {
		if (VAR_P (decl)
		    && (TREE_STATIC (decl) || DECL_EXTERNAL (decl)))
		  continue;
		if (decl_function_context (decl) == info->context
		    && !use_pointer_in_frame (decl))
		  {
		    tree field = lookup_field_for_decl (info, decl, NO_INSERT);
		    if (field)
		      {
			CONSTRUCTOR_ELT (decls, i)->value
			  = get_local_debug_decl (info, decl, field);
		      }
		  }
	      }
	  }

      *handled_ops_p = false;
      return NULL_TREE;

    default:
      /* For every other statement that we are not interested in
	 handling here, let the walker traverse the operands.  */
      *handled_ops_p = false;
      return NULL_TREE;
    }

  /* Indicate that we have handled all the operands ourselves.  */
  *handled_ops_p = true;
  return NULL_TREE;
}


/* Called via walk_function+walk_gimple_stmt, rewrite all GIMPLE_GOTOs
   that reference labels from outer functions.  The rewrite will be a
   call to __builtin_nonlocal_goto.  */

static tree
convert_nl_goto_reference (gimple_stmt_iterator *gsi, bool *handled_ops_p,
			   struct walk_stmt_info *wi)
{
  struct nesting_info *const info = (struct nesting_info *) wi->info, *i;
  tree label, new_label, target_context, x, field;
  gcall *call;
  gimple *stmt = gsi_stmt (*gsi);

  if (gimple_code (stmt) != GIMPLE_GOTO)
    {
      *handled_ops_p = false;
      return NULL_TREE;
    }

  label = gimple_goto_dest (stmt);
  if (TREE_CODE (label) != LABEL_DECL)
    {
      *handled_ops_p = false;
      return NULL_TREE;
    }

  target_context = decl_function_context (label);
  if (target_context == info->context)
    {
      *handled_ops_p = false;
      return NULL_TREE;
    }

  for (i = info->outer; target_context != i->context; i = i->outer)
    continue;

  /* The original user label may also be use for a normal goto, therefore
     we must create a new label that will actually receive the abnormal
     control transfer.  This new label will be marked LABEL_NONLOCAL; this
     mark will trigger proper behavior in the cfg, as well as cause the
     (hairy target-specific) non-local goto receiver code to be generated
     when we expand rtl.  Enter this association into var_map so that we
     can insert the new label into the IL during a second pass.  */
  tree *slot = &i->var_map->get_or_insert (label);
  if (*slot == NULL)
    {
      new_label = create_artificial_label (UNKNOWN_LOCATION);
      DECL_NONLOCAL (new_label) = 1;
      *slot = new_label;
    }
  else
    new_label = *slot;

  /* Build: __builtin_nl_goto(new_label, &chain->nl_goto_field).  */
  field = get_nl_goto_field (i);
  x = get_frame_field (info, target_context, field, gsi);
  x = build_addr (x);
  x = gsi_gimplify_val (info, x, gsi);
  call = gimple_build_call (builtin_decl_implicit (BUILT_IN_NONLOCAL_GOTO),
			    2, build_addr (new_label), x);
  gsi_replace (gsi, call, false);

  /* We have handled all of STMT's operands, no need to keep going.  */
  *handled_ops_p = true;
  return NULL_TREE;
}


/* Called via walk_function+walk_tree, rewrite all GIMPLE_LABELs whose labels
   are referenced via nonlocal goto from a nested function.  The rewrite
   will involve installing a newly generated DECL_NONLOCAL label, and
   (potentially) a branch around the rtl gunk that is assumed to be
   attached to such a label.  */

static tree
convert_nl_goto_receiver (gimple_stmt_iterator *gsi, bool *handled_ops_p,
			  struct walk_stmt_info *wi)
{
  struct nesting_info *const info = (struct nesting_info *) wi->info;
  tree label, new_label;
  gimple_stmt_iterator tmp_gsi;
  glabel *stmt = dyn_cast <glabel *> (gsi_stmt (*gsi));

  if (!stmt)
    {
      *handled_ops_p = false;
      return NULL_TREE;
    }

  label = gimple_label_label (stmt);

  tree *slot = info->var_map->get (label);
  if (!slot)
    {
      *handled_ops_p = false;
      return NULL_TREE;
    }

  /* If there's any possibility that the previous statement falls through,
     then we must branch around the new non-local label.  */
  tmp_gsi = wi->gsi;
  gsi_prev (&tmp_gsi);
  if (gsi_end_p (tmp_gsi) || gimple_stmt_may_fallthru (gsi_stmt (tmp_gsi)))
    {
      gimple *stmt = gimple_build_goto (label);
      gsi_insert_before (gsi, stmt, GSI_SAME_STMT);
    }

  new_label = (tree) *slot;
  stmt = gimple_build_label (new_label);
  gsi_insert_before (gsi, stmt, GSI_SAME_STMT);

  *handled_ops_p = true;
  return NULL_TREE;
}


/* Called via walk_function+walk_stmt, rewrite all references to addresses
   of nested functions that require the use of trampolines.  The rewrite
   will involve a reference a trampoline generated for the occasion.  */

static tree
convert_tramp_reference_op (tree *tp, int *walk_subtrees, void *data)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data;
  struct nesting_info *const info = (struct nesting_info *) wi->info, *i;
  tree t = *tp, decl, target_context, x, builtin;
  bool descr;
  gcall *call;

  *walk_subtrees = 0;
  switch (TREE_CODE (t))
    {
    case ADDR_EXPR:
      /* Build
	   T.1 = &CHAIN->tramp;
	   T.2 = __builtin_adjust_trampoline (T.1);
	   T.3 = (func_type)T.2;
      */

      decl = TREE_OPERAND (t, 0);
      if (TREE_CODE (decl) != FUNCTION_DECL)
	break;

      /* Only need to process nested functions.  */
      target_context = decl_function_context (decl);
      if (!target_context)
	break;

      /* If the nested function doesn't use a static chain, then
	 it doesn't need a trampoline.  */
      if (!DECL_STATIC_CHAIN (decl))
	break;

      /* If we don't want a trampoline, then don't build one.  */
      if (TREE_NO_TRAMPOLINE (t))
	break;

      /* Lookup the immediate parent of the callee, as that's where
	 we need to insert the trampoline.  */
      for (i = info; i->context != target_context; i = i->outer)
	continue;

      /* Decide whether to generate a descriptor or a trampoline. */
      descr = FUNC_ADDR_BY_DESCRIPTOR (t) && !flag_trampolines;

      if (descr)
	x = lookup_descr_for_decl (i, decl, INSERT);
      else
	x = lookup_tramp_for_decl (i, decl, INSERT);

      /* Compute the address of the field holding the trampoline.  */
      x = get_frame_field (info, target_context, x, &wi->gsi);
      x = build_addr (x);
      x = gsi_gimplify_val (info, x, &wi->gsi);

      /* Do machine-specific ugliness.  Normally this will involve
	 computing extra alignment, but it can really be anything.  */
      if (descr)
	builtin = builtin_decl_implicit (BUILT_IN_ADJUST_DESCRIPTOR);
      else
	builtin = builtin_decl_implicit (BUILT_IN_ADJUST_TRAMPOLINE);
      call = gimple_build_call (builtin, 1, x);
      x = init_tmp_var_with_call (info, &wi->gsi, call);

      /* Cast back to the proper function type.  */
      x = build1 (NOP_EXPR, TREE_TYPE (t), x);
      x = init_tmp_var (info, x, &wi->gsi);

      *tp = x;
      break;

    default:
      if (!IS_TYPE_OR_DECL_P (t))
	*walk_subtrees = 1;
      break;
    }

  return NULL_TREE;
}


/* Called via walk_function+walk_gimple_stmt, rewrite all references
   to addresses of nested functions that require the use of
   trampolines.  The rewrite will involve a reference a trampoline
   generated for the occasion.  */

static tree
convert_tramp_reference_stmt (gimple_stmt_iterator *gsi, bool *handled_ops_p,
			      struct walk_stmt_info *wi)
{
  struct nesting_info *info = (struct nesting_info *) wi->info;
  gimple *stmt = gsi_stmt (*gsi);

  switch (gimple_code (stmt))
    {
    case GIMPLE_CALL:
      {
	/* Only walk call arguments, lest we generate trampolines for
	   direct calls.  */
	unsigned long i, nargs = gimple_call_num_args (stmt);
	for (i = 0; i < nargs; i++)
	  walk_tree (gimple_call_arg_ptr (stmt, i), convert_tramp_reference_op,
		     wi, NULL);
	break;
      }

    case GIMPLE_OMP_TEAMS:
      if (!gimple_omp_teams_host (as_a <gomp_teams *> (stmt)))
	{
	  *handled_ops_p = false;
	  return NULL_TREE;
	}
      goto do_parallel;

    case GIMPLE_OMP_TARGET:
      if (!is_gimple_omp_offloaded (stmt))
	{
	  *handled_ops_p = false;
	  return NULL_TREE;
	}
      /* FALLTHRU */
    case GIMPLE_OMP_PARALLEL:
    case GIMPLE_OMP_TASK:
    do_parallel:
      {
	tree save_local_var_chain = info->new_local_var_chain;
        walk_gimple_op (stmt, convert_tramp_reference_op, wi);
	info->new_local_var_chain = NULL;
	char save_static_chain_added = info->static_chain_added;
	info->static_chain_added = 0;
        walk_body (convert_tramp_reference_stmt, convert_tramp_reference_op,
		   info, gimple_omp_body_ptr (stmt));
	if (info->new_local_var_chain)
	  declare_vars (info->new_local_var_chain,
			gimple_seq_first_stmt (gimple_omp_body (stmt)),
			false);
	for (int i = 0; i < 2; i++)
	  {
	    tree c, decl;
	    if ((info->static_chain_added & (1 << i)) == 0)
	      continue;
	    decl = i ? get_chain_decl (info) : info->frame_decl;
	    /* Don't add CHAIN.* or FRAME.* twice.  */
	    for (c = gimple_omp_taskreg_clauses (stmt);
		 c;
		 c = OMP_CLAUSE_CHAIN (c))
	      if ((OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FIRSTPRIVATE
		   || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_SHARED)
		  && OMP_CLAUSE_DECL (c) == decl)
		break;
	    if (c == NULL && gimple_code (stmt) != GIMPLE_OMP_TARGET)
	      {
		c = build_omp_clause (gimple_location (stmt),
				      i ? OMP_CLAUSE_FIRSTPRIVATE
				      : OMP_CLAUSE_SHARED);
		OMP_CLAUSE_DECL (c) = decl;
		OMP_CLAUSE_CHAIN (c) = gimple_omp_taskreg_clauses (stmt);
		gimple_omp_taskreg_set_clauses (stmt, c);
	      }
	    else if (c == NULL)
	      {
		c = build_omp_clause (gimple_location (stmt),
				      OMP_CLAUSE_MAP);
		OMP_CLAUSE_DECL (c) = decl;
		OMP_CLAUSE_SET_MAP_KIND (c,
					 i ? GOMP_MAP_TO : GOMP_MAP_TOFROM);
		OMP_CLAUSE_SIZE (c) = DECL_SIZE_UNIT (decl);
		OMP_CLAUSE_CHAIN (c) = gimple_omp_target_clauses (stmt);
		gimple_omp_target_set_clauses (as_a <gomp_target *> (stmt),
					       c);
	      }
	  }
	info->new_local_var_chain = save_local_var_chain;
	info->static_chain_added |= save_static_chain_added;
      }
      break;

    default:
      *handled_ops_p = false;
      return NULL_TREE;
    }

  *handled_ops_p = true;
  return NULL_TREE;
}



/* Called via walk_function+walk_gimple_stmt, rewrite all GIMPLE_CALLs
   that reference nested functions to make sure that the static chain
   is set up properly for the call.  */

static tree
convert_gimple_call (gimple_stmt_iterator *gsi, bool *handled_ops_p,
                     struct walk_stmt_info *wi)
{
  struct nesting_info *const info = (struct nesting_info *) wi->info;
  tree decl, target_context;
  char save_static_chain_added;
  int i;
  gimple *stmt = gsi_stmt (*gsi);

  switch (gimple_code (stmt))
    {
    case GIMPLE_CALL:
      if (gimple_call_chain (stmt))
	break;
      decl = gimple_call_fndecl (stmt);
      if (!decl)
	break;
      target_context = decl_function_context (decl);
      if (target_context && DECL_STATIC_CHAIN (decl))
	{
	  struct nesting_info *i = info;
	  while (i && i->context != target_context)
	    i = i->outer;
	  /* If none of the outer contexts is the target context, this means
	     that the function is called in a wrong context.  */
	  if (!i)
	    internal_error ("%s from %s called in %s",
			    IDENTIFIER_POINTER (DECL_NAME (decl)),
			    IDENTIFIER_POINTER (DECL_NAME (target_context)),
			    IDENTIFIER_POINTER (DECL_NAME (info->context)));

	  gimple_call_set_chain (as_a <gcall *> (stmt),
				 get_static_chain (info, target_context,
						   &wi->gsi));
	  info->static_chain_added |= (1 << (info->context != target_context));
	}
      break;

    case GIMPLE_OMP_TEAMS:
      if (!gimple_omp_teams_host (as_a <gomp_teams *> (stmt)))
	{
	  walk_body (convert_gimple_call, NULL, info,
		     gimple_omp_body_ptr (stmt));
	  break;
	}
      /* FALLTHRU */

    case GIMPLE_OMP_PARALLEL:
    case GIMPLE_OMP_TASK:
      save_static_chain_added = info->static_chain_added;
      info->static_chain_added = 0;
      walk_body (convert_gimple_call, NULL, info, gimple_omp_body_ptr (stmt));
      for (i = 0; i < 2; i++)
	{
	  tree c, decl;
	  if ((info->static_chain_added & (1 << i)) == 0)
	    continue;
	  decl = i ? get_chain_decl (info) : info->frame_decl;
	  /* Don't add CHAIN.* or FRAME.* twice.  */
	  for (c = gimple_omp_taskreg_clauses (stmt);
	       c;
	       c = OMP_CLAUSE_CHAIN (c))
	    if ((OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FIRSTPRIVATE
		 || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_SHARED)
		&& OMP_CLAUSE_DECL (c) == decl)
	      break;
	  if (c == NULL)
	    {
	      c = build_omp_clause (gimple_location (stmt),
				    i ? OMP_CLAUSE_FIRSTPRIVATE
				    : OMP_CLAUSE_SHARED);
	      OMP_CLAUSE_DECL (c) = decl;
	      OMP_CLAUSE_CHAIN (c) = gimple_omp_taskreg_clauses (stmt);
	      gimple_omp_taskreg_set_clauses (stmt, c);
	    }
	}
      info->static_chain_added |= save_static_chain_added;
      break;

    case GIMPLE_OMP_TARGET:
      if (!is_gimple_omp_offloaded (stmt))
	{
	  walk_body (convert_gimple_call, NULL, info, gimple_omp_body_ptr (stmt));
	  break;
	}
      save_static_chain_added = info->static_chain_added;
      info->static_chain_added = 0;
      walk_body (convert_gimple_call, NULL, info, gimple_omp_body_ptr (stmt));
      for (i = 0; i < 2; i++)
	{
	  tree c, decl;
	  if ((info->static_chain_added & (1 << i)) == 0)
	    continue;
	  decl = i ? get_chain_decl (info) : info->frame_decl;
	  /* Don't add CHAIN.* or FRAME.* twice.  */
	  for (c = gimple_omp_target_clauses (stmt);
	       c;
	       c = OMP_CLAUSE_CHAIN (c))
	    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_MAP
		&& OMP_CLAUSE_DECL (c) == decl)
	      break;
	  if (c == NULL)
	    {
	      c = build_omp_clause (gimple_location (stmt), OMP_CLAUSE_MAP);
	      OMP_CLAUSE_DECL (c) = decl;
	      OMP_CLAUSE_SET_MAP_KIND (c, i ? GOMP_MAP_TO : GOMP_MAP_TOFROM);
	      OMP_CLAUSE_SIZE (c) = DECL_SIZE_UNIT (decl);
	      OMP_CLAUSE_CHAIN (c) = gimple_omp_target_clauses (stmt);
	      gimple_omp_target_set_clauses (as_a <gomp_target *> (stmt),
					     c);
	    }
	}
      info->static_chain_added |= save_static_chain_added;
      break;

    case GIMPLE_OMP_FOR:
      walk_body (convert_gimple_call, NULL, info,
	  	 gimple_omp_for_pre_body_ptr (stmt));
      /* FALLTHRU */
    case GIMPLE_OMP_SECTIONS:
    case GIMPLE_OMP_SECTION:
    case GIMPLE_OMP_SINGLE:
    case GIMPLE_OMP_SCOPE:
    case GIMPLE_OMP_MASTER:
    case GIMPLE_OMP_MASKED:
    case GIMPLE_OMP_TASKGROUP:
    case GIMPLE_OMP_ORDERED:
    case GIMPLE_OMP_SCAN:
    case GIMPLE_OMP_CRITICAL:
      walk_body (convert_gimple_call, NULL, info, gimple_omp_body_ptr (stmt));
      break;

    default:
      /* Keep looking for other operands.  */
      *handled_ops_p = false;
      return NULL_TREE;
    }

  *handled_ops_p = true;
  return NULL_TREE;
}

/* Walk the nesting tree starting with ROOT.  Convert all trampolines and
   call expressions.  At the same time, determine if a nested function
   actually uses its static chain; if not, remember that.  */

static void
convert_all_function_calls (struct nesting_info *root)
{
  unsigned int chain_count = 0, old_chain_count, iter_count;
  struct nesting_info *n;

  /* First, optimistically clear static_chain for all decls that haven't
     used the static chain already for variable access.  But always create
     it if not optimizing.  This makes it possible to reconstruct the static
     nesting tree at run time and thus to resolve up-level references from
     within the debugger.  */
  FOR_EACH_NEST_INFO (n, root)
    {
      if (n->thunk_p)
	continue;
      tree decl = n->context;
      if (!optimize)
	{
	  if (n->inner)
	    (void) get_frame_type (n);
	  if (n->outer)
	    (void) get_chain_decl (n);
	}
      else if (!n->outer || (!n->chain_decl && !n->chain_field))
	{
	  DECL_STATIC_CHAIN (decl) = 0;
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Guessing no static-chain for %s\n",
		     lang_hooks.decl_printable_name (decl, 2));
	}
      else
	DECL_STATIC_CHAIN (decl) = 1;
      chain_count += DECL_STATIC_CHAIN (decl);
    }

  FOR_EACH_NEST_INFO (n, root)
    if (n->thunk_p)
      {
	tree decl = n->context;
	tree alias = thunk_info::get (cgraph_node::get (decl))->alias;
	DECL_STATIC_CHAIN (decl) = DECL_STATIC_CHAIN (alias);
      }

  /* Walk the functions and perform transformations.  Note that these
     transformations can induce new uses of the static chain, which in turn
     require re-examining all users of the decl.  */
  /* ??? It would make sense to try to use the call graph to speed this up,
     but the call graph hasn't really been built yet.  Even if it did, we
     would still need to iterate in this loop since address-of references
     wouldn't show up in the callgraph anyway.  */
  iter_count = 0;
  do
    {
      old_chain_count = chain_count;
      chain_count = 0;
      iter_count++;

      if (dump_file && (dump_flags & TDF_DETAILS))
	fputc ('\n', dump_file);

      FOR_EACH_NEST_INFO (n, root)
	{
	  if (n->thunk_p)
	    continue;
	  tree decl = n->context;
	  walk_function (convert_tramp_reference_stmt,
			 convert_tramp_reference_op, n);
	  walk_function (convert_gimple_call, NULL, n);
	  chain_count += DECL_STATIC_CHAIN (decl);
	}

      FOR_EACH_NEST_INFO (n, root)
	if (n->thunk_p)
	  {
	    tree decl = n->context;
	    tree alias = thunk_info::get (cgraph_node::get (decl))->alias;
	    DECL_STATIC_CHAIN (decl) = DECL_STATIC_CHAIN (alias);
	  }
    }
  while (chain_count != old_chain_count);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "convert_all_function_calls iterations: %u\n\n",
	     iter_count);
}

struct nesting_copy_body_data
{
  copy_body_data cb;
  struct nesting_info *root;
};

/* A helper subroutine for debug_var_chain type remapping.  */

static tree
nesting_copy_decl (tree decl, copy_body_data *id)
{
  struct nesting_copy_body_data *nid = (struct nesting_copy_body_data *) id;
  tree *slot = nid->root->var_map->get (decl);

  if (slot)
    return (tree) *slot;

  if (TREE_CODE (decl) == TYPE_DECL && DECL_ORIGINAL_TYPE (decl))
    {
      tree new_decl = copy_decl_no_change (decl, id);
      DECL_ORIGINAL_TYPE (new_decl)
	= remap_type (DECL_ORIGINAL_TYPE (decl), id);
      return new_decl;
    }

  if (VAR_P (decl)
      || TREE_CODE (decl) == PARM_DECL
      || TREE_CODE (decl) == RESULT_DECL)
    return decl;

  return copy_decl_no_change (decl, id);
}

/* A helper function for remap_vla_decls.  See if *TP contains
   some remapped variables.  */

static tree
contains_remapped_vars (tree *tp, int *walk_subtrees, void *data)
{
  struct nesting_info *root = (struct nesting_info *) data;
  tree t = *tp;

  if (DECL_P (t))
    {
      *walk_subtrees = 0;
      tree *slot = root->var_map->get (t);

      if (slot)
	return *slot;
    }
  return NULL;
}

/* Remap VLA decls in BLOCK and subblocks if remapped variables are
   involved.  */

static void
remap_vla_decls (tree block, struct nesting_info *root)
{
  tree var, subblock, val, type;
  struct nesting_copy_body_data id;

  for (subblock = BLOCK_SUBBLOCKS (block);
       subblock;
       subblock = BLOCK_CHAIN (subblock))
    remap_vla_decls (subblock, root);

  for (var = BLOCK_VARS (block); var; var = DECL_CHAIN (var))
    if (VAR_P (var) && DECL_HAS_VALUE_EXPR_P (var))
      {
	val = DECL_VALUE_EXPR (var);
	type = TREE_TYPE (var);

	if (!(TREE_CODE (val) == INDIRECT_REF
	      && TREE_CODE (TREE_OPERAND (val, 0)) == VAR_DECL
	      && variably_modified_type_p (type, NULL)))
	  continue;

	if (root->var_map->get (TREE_OPERAND (val, 0))
	    || walk_tree (&type, contains_remapped_vars, root, NULL))
	  break;
      }

  if (var == NULL_TREE)
    return;

  memset (&id, 0, sizeof (id));
  id.cb.copy_decl = nesting_copy_decl;
  id.cb.decl_map = new hash_map<tree, tree>;
  id.root = root;

  for (; var; var = DECL_CHAIN (var))
    if (VAR_P (var) && DECL_HAS_VALUE_EXPR_P (var))
      {
	struct nesting_info *i;
	tree newt, context;

	val = DECL_VALUE_EXPR (var);
	type = TREE_TYPE (var);

	if (!(TREE_CODE (val) == INDIRECT_REF
	      && TREE_CODE (TREE_OPERAND (val, 0)) == VAR_DECL
	      && variably_modified_type_p (type, NULL)))
	  continue;

	tree *slot = root->var_map->get (TREE_OPERAND (val, 0));
	if (!slot && !walk_tree (&type, contains_remapped_vars, root, NULL))
	  continue;

	context = decl_function_context (var);
	for (i = root; i; i = i->outer)
	  if (i->context == context)
	    break;

	if (i == NULL)
	  continue;

	/* Fully expand value expressions.  This avoids having debug variables
	   only referenced from them and that can be swept during GC.  */
        if (slot)
	  {
	    tree t = (tree) *slot;
	    gcc_assert (DECL_P (t) && DECL_HAS_VALUE_EXPR_P (t));
	    val = build1 (INDIRECT_REF, TREE_TYPE (val), DECL_VALUE_EXPR (t));
	  }

	id.cb.src_fn = i->context;
	id.cb.dst_fn = i->context;
	id.cb.src_cfun = DECL_STRUCT_FUNCTION (root->context);

	TREE_TYPE (var) = newt = remap_type (type, &id.cb);
	while (POINTER_TYPE_P (newt) && !TYPE_NAME (newt))
	  {
	    newt = TREE_TYPE (newt);
	    type = TREE_TYPE (type);
	  }
	if (TYPE_NAME (newt)
	    && TREE_CODE (TYPE_NAME (newt)) == TYPE_DECL
	    && DECL_ORIGINAL_TYPE (TYPE_NAME (newt))
	    && newt != type
	    && TYPE_NAME (newt) == TYPE_NAME (type))
	  TYPE_NAME (newt) = remap_decl (TYPE_NAME (newt), &id.cb);

	walk_tree (&val, copy_tree_body_r, &id.cb, NULL);
	if (val != DECL_VALUE_EXPR (var))
	  SET_DECL_VALUE_EXPR (var, val);
      }

  delete id.cb.decl_map;
}

/* Fixup VLA decls in BLOCK and subblocks if remapped variables are
   involved.  */

static void
fixup_vla_decls (tree block)
{
  for (tree var = BLOCK_VARS (block); var; var = DECL_CHAIN (var))
    if (VAR_P (var) && DECL_HAS_VALUE_EXPR_P (var))
      {
	tree val = DECL_VALUE_EXPR (var);

	if (!(TREE_CODE (val) == INDIRECT_REF
	      && VAR_P (TREE_OPERAND (val, 0))
	      && DECL_HAS_VALUE_EXPR_P (TREE_OPERAND (val, 0))))
	  continue;

	/* Fully expand value expressions.  This avoids having debug variables
	   only referenced from them and that can be swept during GC.  */
	val = build1 (INDIRECT_REF, TREE_TYPE (val),
		      DECL_VALUE_EXPR (TREE_OPERAND (val, 0)));
	SET_DECL_VALUE_EXPR (var, val);
      }

  for (tree sub = BLOCK_SUBBLOCKS (block); sub; sub = BLOCK_CHAIN (sub))
    fixup_vla_decls (sub);
}

/* Fold the MEM_REF *E.  */
bool
fold_mem_refs (tree *const &e, void *data ATTRIBUTE_UNUSED)
{
  tree *ref_p = CONST_CAST2 (tree *, const tree *, (const tree *)e);
  *ref_p = fold (*ref_p);
  return true;
}

/* Given DECL, a nested function, build an initialization call for FIELD,
   the trampoline or descriptor for DECL, using FUNC as the function.  */

static gcall *
build_init_call_stmt (struct nesting_info *info, tree decl, tree field,
		      tree func)
{
  tree arg1, arg2, arg3, x;

  gcc_assert (DECL_STATIC_CHAIN (decl));
  arg3 = build_addr (info->frame_decl);

  arg2 = build_addr (decl);

  x = build3 (COMPONENT_REF, TREE_TYPE (field),
	      info->frame_decl, field, NULL_TREE);
  arg1 = build_addr (x);

  return gimple_build_call (func, 3, arg1, arg2, arg3);
}

/* Do "everything else" to clean up or complete state collected by the various
   walking passes -- create a field to hold the frame base address, lay out the
   types and decls, generate code to initialize the frame decl, store critical
   expressions in the struct function for rtl to find.  */

static void
finalize_nesting_tree_1 (struct nesting_info *root)
{
  gimple_seq stmt_list = NULL;
  gimple *stmt;
  tree context = root->context;
  struct function *sf;

  if (root->thunk_p)
    return;

  /* If we created a non-local frame type or decl, we need to lay them
     out at this time.  */
  if (root->frame_type)
    {
      /* Debugging information needs to compute the frame base address of the
	 parent frame out of the static chain from the nested frame.

	 The static chain is the address of the FRAME record, so one could
	 imagine it would be possible to compute the frame base address just
	 adding a constant offset to this address.  Unfortunately, this is not
	 possible: if the FRAME object has alignment constraints that are
	 stronger than the stack, then the offset between the frame base and
	 the FRAME object will be dynamic.

	 What we do instead is to append a field to the FRAME object that holds
	 the frame base address: then debug info just has to fetch this
	 field.  */

      /* Debugging information will refer to the CFA as the frame base
	 address: we will do the same here.  */
      const tree frame_addr_fndecl
        = builtin_decl_explicit (BUILT_IN_DWARF_CFA);

      /* Create a field in the FRAME record to hold the frame base address for
	 this stack frame.  Since it will be used only by the debugger, put it
	 at the end of the record in order not to shift all other offsets.  */
      tree fb_decl = make_node (FIELD_DECL);

      DECL_NAME (fb_decl) = get_identifier ("FRAME_BASE.PARENT");
      TREE_TYPE (fb_decl) = ptr_type_node;
      TREE_ADDRESSABLE (fb_decl) = 1;
      DECL_CONTEXT (fb_decl) = root->frame_type;
      TYPE_FIELDS (root->frame_type) = chainon (TYPE_FIELDS (root->frame_type),
						fb_decl);

      /* In some cases the frame type will trigger the -Wpadded warning.
	 This is not helpful; suppress it. */
      int save_warn_padded = warn_padded;
      warn_padded = 0;
      layout_type (root->frame_type);
      warn_padded = save_warn_padded;
      layout_decl (root->frame_decl, 0);

      /* Initialize the frame base address field.  If the builtin we need is
	 not available, set it to NULL so that debugging information does not
	 reference junk.  */
      tree fb_ref = build3 (COMPONENT_REF, TREE_TYPE (fb_decl),
			    root->frame_decl, fb_decl, NULL_TREE);
      tree fb_tmp;

      if (frame_addr_fndecl != NULL_TREE)
	{
	  gcall *fb_gimple = gimple_build_call (frame_addr_fndecl, 1,
						integer_zero_node);
	  gimple_stmt_iterator gsi = gsi_last (stmt_list);

	  fb_tmp = init_tmp_var_with_call (root, &gsi, fb_gimple);
	}
      else
	fb_tmp = build_int_cst (TREE_TYPE (fb_ref), 0);
      gimple_seq_add_stmt (&stmt_list,
			   gimple_build_assign (fb_ref, fb_tmp));

      declare_vars (root->frame_decl,
		    gimple_seq_first_stmt (gimple_body (context)), true);
    }

  /* If any parameters were referenced non-locally, then we need to insert
     a copy or a pointer.  */
  if (root->any_parm_remapped)
    {
      tree p;
      for (p = DECL_ARGUMENTS (context); p ; p = DECL_CHAIN (p))
	{
	  tree field, x, y;

	  field = lookup_field_for_decl (root, p, NO_INSERT);
	  if (!field)
	    continue;

	  if (use_pointer_in_frame (p))
	    x = build_addr (p);
	  else
	    x = p;

	  /* If the assignment is from a non-register the stmt is
	     not valid gimple.  Make it so by using a temporary instead.  */
	  if (!is_gimple_reg (x)
	      && is_gimple_reg_type (TREE_TYPE (x)))
	    {
	      gimple_stmt_iterator gsi = gsi_last (stmt_list);
	      x = init_tmp_var (root, x, &gsi);
	    }

	  y = build3 (COMPONENT_REF, TREE_TYPE (field),
		      root->frame_decl, field, NULL_TREE);
	  stmt = gimple_build_assign (y, x);
	  gimple_seq_add_stmt (&stmt_list, stmt);
	}
    }

  /* If a chain_field was created, then it needs to be initialized
     from chain_decl.  */
  if (root->chain_field)
    {
      tree x = build3 (COMPONENT_REF, TREE_TYPE (root->chain_field),
		       root->frame_decl, root->chain_field, NULL_TREE);
      stmt = gimple_build_assign (x, get_chain_decl (root));
      gimple_seq_add_stmt (&stmt_list, stmt);
    }

  /* If trampolines were created, then we need to initialize them.  */
  if (root->any_tramp_created)
    {
      struct nesting_info *i;
      for (i = root->inner; i ; i = i->next)
	{
	  tree field, x;

	  field = lookup_tramp_for_decl (root, i->context, NO_INSERT);
	  if (!field)
	    continue;

	  x = builtin_decl_implicit (BUILT_IN_INIT_TRAMPOLINE);
	  stmt = build_init_call_stmt (root, i->context, field, x);
	  gimple_seq_add_stmt (&stmt_list, stmt);
	}
    }

  /* If descriptors were created, then we need to initialize them.  */
  if (root->any_descr_created)
    {
      struct nesting_info *i;
      for (i = root->inner; i ; i = i->next)
	{
	  tree field, x;

	  field = lookup_descr_for_decl (root, i->context, NO_INSERT);
	  if (!field)
	    continue;

	  x = builtin_decl_implicit (BUILT_IN_INIT_DESCRIPTOR);
	  stmt = build_init_call_stmt (root, i->context, field, x);
	  gimple_seq_add_stmt (&stmt_list, stmt);
	}
    }

  /* If we created initialization statements, insert them.  */
  if (stmt_list)
    {
      gbind *bind;
      annotate_all_with_location (stmt_list, DECL_SOURCE_LOCATION (context));
      bind = gimple_seq_first_stmt_as_a_bind (gimple_body (context));
      gimple_seq_add_seq (&stmt_list, gimple_bind_body (bind));
      gimple_bind_set_body (bind, stmt_list);
    }

  /* If a chain_decl was created, then it needs to be registered with
     struct function so that it gets initialized from the static chain
     register at the beginning of the function.  */
  sf = DECL_STRUCT_FUNCTION (root->context);
  sf->static_chain_decl = root->chain_decl;

  /* Similarly for the non-local goto save area.  */
  if (root->nl_goto_field)
    {
      sf->nonlocal_goto_save_area
	= get_frame_field (root, context, root->nl_goto_field, NULL);
      sf->has_nonlocal_label = 1;
    }

  /* Make sure all new local variables get inserted into the
     proper BIND_EXPR.  */
  if (root->new_local_var_chain)
    declare_vars (root->new_local_var_chain,
		  gimple_seq_first_stmt (gimple_body (root->context)),
		  false);

  if (root->debug_var_chain)
    {
      tree debug_var;
      gbind *scope;

      remap_vla_decls (DECL_INITIAL (root->context), root);

      for (debug_var = root->debug_var_chain; debug_var;
	   debug_var = DECL_CHAIN (debug_var))
	if (variably_modified_type_p (TREE_TYPE (debug_var), NULL))
	  break;

      /* If there are any debug decls with variable length types,
	 remap those types using other debug_var_chain variables.  */
      if (debug_var)
	{
	  struct nesting_copy_body_data id;

	  memset (&id, 0, sizeof (id));
	  id.cb.copy_decl = nesting_copy_decl;
	  id.cb.decl_map = new hash_map<tree, tree>;
	  id.root = root;

	  for (; debug_var; debug_var = DECL_CHAIN (debug_var))
	    if (variably_modified_type_p (TREE_TYPE (debug_var), NULL))
	      {
		tree type = TREE_TYPE (debug_var);
		tree newt, t = type;
		struct nesting_info *i;

		for (i = root; i; i = i->outer)
		  if (variably_modified_type_p (type, i->context))
		    break;

		if (i == NULL)
		  continue;

		id.cb.src_fn = i->context;
		id.cb.dst_fn = i->context;
		id.cb.src_cfun = DECL_STRUCT_FUNCTION (root->context);

		TREE_TYPE (debug_var) = newt = remap_type (type, &id.cb);
		while (POINTER_TYPE_P (newt) && !TYPE_NAME (newt))
		  {
		    newt = TREE_TYPE (newt);
		    t = TREE_TYPE (t);
		  }
		if (TYPE_NAME (newt)
		    && TREE_CODE (TYPE_NAME (newt)) == TYPE_DECL
		    && DECL_ORIGINAL_TYPE (TYPE_NAME (newt))
		    && newt != t
		    && TYPE_NAME (newt) == TYPE_NAME (t))
		  TYPE_NAME (newt) = remap_decl (TYPE_NAME (newt), &id.cb);
	      }

	  delete id.cb.decl_map;
	}

      scope = gimple_seq_first_stmt_as_a_bind (gimple_body (root->context));
      if (gimple_bind_block (scope))
	declare_vars (root->debug_var_chain, scope, true);
      else
	BLOCK_VARS (DECL_INITIAL (root->context))
	  = chainon (BLOCK_VARS (DECL_INITIAL (root->context)),
		     root->debug_var_chain);
    }
  else
    fixup_vla_decls (DECL_INITIAL (root->context));

  /* Fold the rewritten MEM_REF trees.  */
  root->mem_refs->traverse<void *, fold_mem_refs> (NULL);

  /* Dump the translated tree function.  */
  if (dump_file)
    {
      fputs ("\n\n", dump_file);
      dump_function_to_file (root->context, dump_file, dump_flags);
    }
}

static void
finalize_nesting_tree (struct nesting_info *root)
{
  struct nesting_info *n;
  FOR_EACH_NEST_INFO (n, root)
    finalize_nesting_tree_1 (n);
}

/* Unnest the nodes and pass them to cgraph.  */

static void
unnest_nesting_tree_1 (struct nesting_info *root)
{
  struct cgraph_node *node = cgraph_node::get (root->context);

  /* For nested functions update the cgraph to reflect unnesting.
     We also delay finalizing of these functions up to this point.  */
  if (nested_function_info::get (node)->origin)
    {
       unnest_function (node);
       if (!root->thunk_p)
	 cgraph_node::finalize_function (root->context, true);
    }
}

static void
unnest_nesting_tree (struct nesting_info *root)
{
  struct nesting_info *n;
  FOR_EACH_NEST_INFO (n, root)
    unnest_nesting_tree_1 (n);
}

/* Free the data structures allocated during this pass.  */

static void
free_nesting_tree (struct nesting_info *root)
{
  struct nesting_info *node, *next;

  node = iter_nestinfo_start (root);
  do
    {
      next = iter_nestinfo_next (node);
      delete node->var_map;
      delete node->field_map;
      delete node->mem_refs;
      free (node);
      node = next;
    }
  while (node);
}

/* Gimplify a function and all its nested functions.  */
static void
gimplify_all_functions (struct cgraph_node *root)
{
  struct cgraph_node *iter;
  if (!gimple_body (root->decl))
    gimplify_function_tree (root->decl);
  for (iter = first_nested_function (root); iter;
       iter = next_nested_function (iter))
    if (!iter->thunk)
      gimplify_all_functions (iter);
}

/* Main entry point for this pass.  Process FNDECL and all of its nested
   subroutines and turn them into something less tightly bound.  */

void
lower_nested_functions (tree fndecl)
{
  struct cgraph_node *cgn;
  struct nesting_info *root;

  /* If there are no nested functions, there's nothing to do.  */
  cgn = cgraph_node::get (fndecl);
  if (!first_nested_function (cgn))
    return;

  gimplify_all_functions (cgn);

  set_dump_file (dump_begin (TDI_nested, &dump_flags));
  if (dump_file)
    fprintf (dump_file, "\n;; Function %s\n\n",
	     lang_hooks.decl_printable_name (fndecl, 2));

  bitmap_obstack_initialize (&nesting_info_bitmap_obstack);
  root = create_nesting_tree (cgn);

  walk_all_functions (convert_nonlocal_reference_stmt,
                      convert_nonlocal_reference_op,
		      root);
  walk_all_functions (convert_local_reference_stmt,
                      convert_local_reference_op,
		      root);
  walk_all_functions (convert_nl_goto_reference, NULL, root);
  walk_all_functions (convert_nl_goto_receiver, NULL, root);

  convert_all_function_calls (root);
  finalize_nesting_tree (root);
  unnest_nesting_tree (root);

  free_nesting_tree (root);
  bitmap_obstack_release (&nesting_info_bitmap_obstack);

  if (dump_file)
    {
      dump_end (TDI_nested, dump_file);
      set_dump_file (NULL);
    }
}

#include "gt-tree-nested.h"
