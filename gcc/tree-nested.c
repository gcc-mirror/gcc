/* Nested function decomposition for GIMPLE.
   Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.

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
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "function.h"
#include "tree-dump.h"
#include "tree-inline.h"
#include "gimple.h"
#include "tree-iterator.h"
#include "tree-flow.h"
#include "cgraph.h"
#include "expr.h"
#include "langhooks.h"
#include "pointer-set.h"
#include "ggc.h"


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
  
  struct pointer_map_t *field_map;
  struct pointer_map_t *var_map;
  bitmap suppress_expansion;

  tree context;
  tree new_local_var_chain;
  tree debug_var_chain;
  tree frame_type;
  tree frame_decl;
  tree chain_field;
  tree chain_decl;
  tree nl_goto_field;

  bool any_parm_remapped;
  bool any_tramp_created;
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
  TREE_CHAIN (tmp_var) = info->new_local_var_chain;
  DECL_SEEN_IN_BIND_EXPR_P (tmp_var) = 1;
  if (TREE_CODE (type) == COMPLEX_TYPE
      || TREE_CODE (type) == VECTOR_TYPE)
    DECL_GIMPLE_REG_P (tmp_var) = 1;

  info->new_local_var_chain = tmp_var;

  return tmp_var;
}

/* Take the address of EXP to be used within function CONTEXT.
   Mark it for addressability as necessary.  */

tree
build_addr (tree exp, tree context)
{
  tree base = exp;
  tree save_context;
  tree retval;

  while (handled_component_p (base))
    base = TREE_OPERAND (base, 0);

  if (DECL_P (base))
    TREE_ADDRESSABLE (base) = 1;

  /* Building the ADDR_EXPR will compute a set of properties for
     that ADDR_EXPR.  Those properties are unfortunately context
     specific, i.e., they are dependent on CURRENT_FUNCTION_DECL.

     Temporarily set CURRENT_FUNCTION_DECL to the desired context,
     build the ADDR_EXPR, then restore CURRENT_FUNCTION_DECL.  That
     way the properties are for the ADDR_EXPR are computed properly.  */
  save_context = current_function_decl;
  current_function_decl = context;
  retval = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (exp)), exp);
  current_function_decl = save_context;
  return retval;
}

/* Insert FIELD into TYPE, sorted by alignment requirements.  */

void
insert_field_into_struct (tree type, tree field)
{
  tree *p;

  DECL_CONTEXT (field) = type;

  for (p = &TYPE_FIELDS (type); *p ; p = &TREE_CHAIN (*p))
    if (DECL_ALIGN (field) >= DECL_ALIGN (*p))
      break;

  TREE_CHAIN (field) = *p;
  *p = field;

  /* Set correct alignment for frame struct type.  */
  if (TYPE_ALIGN (type) < DECL_ALIGN (field))
    TYPE_ALIGN (type) = DECL_ALIGN (field);
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
      info->frame_decl = create_tmp_var_for (info, type, "FRAME");

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

/* Return true if DECL should be referenced by pointer in the non-local
   frame structure.  */

static bool
use_pointer_in_frame (tree decl)
{
  if (TREE_CODE (decl) == PARM_DECL)
    {
      /* It's illegal to copy TREE_ADDRESSABLE, impossible to copy variable
         sized decls, and inefficient to copy large aggregates.  Don't bother
         moving anything but scalar variables.  */
      return AGGREGATE_TYPE_P (TREE_TYPE (decl));
    }
  else
    {
      /* Variable sized types make things "interesting" in the frame.  */
      return DECL_SIZE (decl) == NULL || !TREE_CONSTANT (DECL_SIZE (decl));
    }
}

/* Given DECL, a non-locally accessed variable, find or create a field
   in the non-local frame structure for the given nesting context.  */

static tree
lookup_field_for_decl (struct nesting_info *info, tree decl,
		       enum insert_option insert)
{
  void **slot;

  if (insert == NO_INSERT)
    {
      slot = pointer_map_contains (info->field_map, decl);
      return slot ? (tree) *slot : NULL_TREE;
    }

  slot = pointer_map_insert (info->field_map, decl);
  if (!*slot)
    {
      tree field = make_node (FIELD_DECL);
      DECL_NAME (field) = DECL_NAME (decl);

      if (use_pointer_in_frame (decl))
	{
	  TREE_TYPE (field) = build_pointer_type (TREE_TYPE (decl));
	  DECL_ALIGN (field) = TYPE_ALIGN (TREE_TYPE (field));
	  DECL_NONADDRESSABLE_P (field) = 1;
	}
      else
	{
          TREE_TYPE (field) = TREE_TYPE (decl);
          DECL_SOURCE_LOCATION (field) = DECL_SOURCE_LOCATION (decl);
          DECL_ALIGN (field) = DECL_ALIGN (decl);
          DECL_USER_ALIGN (field) = DECL_USER_ALIGN (decl);
          TREE_ADDRESSABLE (field) = TREE_ADDRESSABLE (decl);
          DECL_NONADDRESSABLE_P (field) = !TREE_ADDRESSABLE (decl);
          TREE_THIS_VOLATILE (field) = TREE_THIS_VOLATILE (decl);
	}

      insert_field_into_struct (get_frame_type (info), field);
      *slot = field;

      if (TREE_CODE (decl) == PARM_DECL)
	info->any_parm_remapped = true;
    }

  return (tree) *slot;
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
	  && DECL_NO_STATIC_CHAIN (info->context))
	fprintf (dump_file, "Resetting no-static-chain for %s\n",
		 lang_hooks.decl_printable_name (info->context, 2));

      DECL_NO_STATIC_CHAIN (info->context) = 0;
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
      DECL_ALIGN (field) = TYPE_ALIGN (type);
      DECL_NONADDRESSABLE_P (field) = 1;

      insert_field_into_struct (get_frame_type (info), field);

      info->chain_field = field;

      if (dump_file
          && (dump_flags & TDF_DETAILS)
	  && DECL_NO_STATIC_CHAIN (info->context))
	fprintf (dump_file, "Resetting no-static-chain for %s\n",
		 lang_hooks.decl_printable_name (info->context, 2));

      DECL_NO_STATIC_CHAIN (info->context) = 0;
    }
  return field;
}

/* Initialize a new temporary with the GIMPLE_CALL STMT.  */

static tree
init_tmp_var_with_call (struct nesting_info *info, gimple_stmt_iterator *gsi,
		        gimple call)
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
  gimple stmt;

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
  gimple stmt;

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

  t = build_index_type (build_int_cst (NULL_TREE, size - 1));
  t = build_array_type (char_type_node, t);
  t = build_decl (DECL_SOURCE_LOCATION (info->context),
		  FIELD_DECL, get_identifier ("__data"), t);
  DECL_ALIGN (t) = align;
  DECL_USER_ALIGN (t) = 1;

  trampoline_type = make_node (RECORD_TYPE);
  TYPE_NAME (trampoline_type) = get_identifier ("__builtin_trampoline");
  TYPE_FIELDS (trampoline_type) = t;
  layout_type (trampoline_type);
  DECL_CONTEXT (t) = trampoline_type;

  return trampoline_type;
}

/* Given DECL, a nested function, find or create a field in the non-local
   frame structure for a trampoline for this function.  */

static tree
lookup_tramp_for_decl (struct nesting_info *info, tree decl,
		       enum insert_option insert)
{
  void **slot;

  if (insert == NO_INSERT)
    {
      slot = pointer_map_contains (info->var_map, decl);
      return slot ? (tree) *slot : NULL_TREE;
    }

  slot = pointer_map_insert (info->var_map, decl);
  if (!*slot)
    {
      tree field = make_node (FIELD_DECL);
      DECL_NAME (field) = DECL_NAME (decl);
      TREE_TYPE (field) = get_trampoline_type (info);
      TREE_ADDRESSABLE (field) = 1;

      insert_field_into_struct (get_frame_type (info), field);
      *slot = field;

      info->any_tramp_created = true;
    }

  return (tree) *slot;
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

      size = GET_MODE_SIZE (STACK_SAVEAREA_MODE (SAVE_NONLOCAL));
      size = size / GET_MODE_SIZE (Pmode);
      size = size + 1;

      type = build_array_type
	(type, build_index_type (build_int_cst (NULL_TREE, size)));

      field = make_node (FIELD_DECL);
      DECL_NAME (field) = get_identifier ("__nl_goto_buf");
      TREE_TYPE (field) = type;
      DECL_ALIGN (field) = TYPE_ALIGN (type);
      TREE_ADDRESSABLE (field) = 1;

      insert_field_into_struct (get_frame_type (info), field);

      info->nl_goto_field = field;
    }

  return field;
}

/* Invoke CALLBACK on all statements of GIMPLE sequence SEQ.  */

static void
walk_body (walk_stmt_fn callback_stmt, walk_tree_fn callback_op,
	   struct nesting_info *info, gimple_seq seq)
{
  struct walk_stmt_info wi;

  memset (&wi, 0, sizeof (wi));
  wi.info = info;
  wi.val_only = true;
  walk_gimple_seq (seq, callback_stmt, callback_op, &wi);
}


/* Invoke CALLBACK_STMT/CALLBACK_OP on all statements of INFO->CONTEXT.  */

static inline void
walk_function (walk_stmt_fn callback_stmt, walk_tree_fn callback_op,
	       struct nesting_info *info)
{
  walk_body (callback_stmt, callback_op, info, gimple_body (info->context));
}

/* Invoke CALLBACK on a GIMPLE_OMP_FOR's init, cond, incr and pre-body.  */

static void
walk_gimple_omp_for (gimple for_stmt,
    		     walk_stmt_fn callback_stmt, walk_tree_fn callback_op,
    		     struct nesting_info *info)
{
  struct walk_stmt_info wi;
  gimple_seq seq;
  tree t;
  size_t i;

  walk_body (callback_stmt, callback_op, info, gimple_omp_for_pre_body (for_stmt));

  seq = gimple_seq_alloc ();
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

  if (gimple_seq_empty_p (seq))
    gimple_seq_free (seq);
  else
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
  struct cgraph_node *cgn = cgraph_node (fndecl);
  tree arg;

  for (cgn = cgn->nested; cgn ; cgn = cgn->next_nested)
    {
      for (arg = DECL_ARGUMENTS (cgn->decl); arg; arg = TREE_CHAIN (arg))
	if (variably_modified_type_p (TREE_TYPE (arg), orig_fndecl))
	  return true;

      if (check_for_nested_with_variably_modified (cgn->decl, orig_fndecl))
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
  info->field_map = pointer_map_create ();
  info->var_map = pointer_map_create ();
  info->suppress_expansion = BITMAP_ALLOC (&nesting_info_bitmap_obstack);
  info->context = cgn->decl;

  for (cgn = cgn->nested; cgn ; cgn = cgn->next_nested)
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
      x = build_addr (info->frame_decl, target_context);
    }
  else
    {
      x = get_chain_decl (info);

      for (i = info->outer; i->context != target_context; i = i->outer)
	{
	  tree field = get_chain_field (i);

	  x = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (x)), x);
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
    }
  else
    {
      x = get_chain_decl (info);

      for (i = info->outer; i->context != target_context; i = i->outer)
	{
	  tree field = get_chain_field (i);

	  x = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (x)), x);
	  x = build3 (COMPONENT_REF, TREE_TYPE (field), x, field, NULL_TREE);
	  x = init_tmp_var (info, x, gsi);
	}

      x = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (x)), x);
    }

  x = build3 (COMPONENT_REF, TREE_TYPE (field), x, field, NULL_TREE);
  return x;
}

static void note_nonlocal_vla_type (struct nesting_info *info, tree type);

/* A subroutine of convert_nonlocal_reference_op.  Create a local variable
   in the nested function with DECL_VALUE_EXPR set to reference the true
   variable in the parent function.  This is used both for debug info 
   and in OpenMP lowering.  */

static tree
get_nonlocal_debug_decl (struct nesting_info *info, tree decl)
{
  tree target_context;
  struct nesting_info *i;
  tree x, field, new_decl;
  void **slot;

  slot = pointer_map_insert (info->var_map, decl);

  if (*slot)
    return (tree) *slot;

  target_context = decl_function_context (decl);

  /* A copy of the code in get_frame_field, but without the temporaries.  */
  if (info->context == target_context)
    {
      /* Make sure frame_decl gets created.  */
      (void) get_frame_type (info);
      x = info->frame_decl;
      i = info;
    }
  else
    {
      x = get_chain_decl (info);
      for (i = info->outer; i->context != target_context; i = i->outer)
	{
	  field = get_chain_field (i);
	  x = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (x)), x);
	  x = build3 (COMPONENT_REF, TREE_TYPE (field), x, field, NULL_TREE);
	}
      x = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (x)), x);
    }

  field = lookup_field_for_decl (i, decl, INSERT);
  x = build3 (COMPONENT_REF, TREE_TYPE (field), x, field, NULL_TREE);
  if (use_pointer_in_frame (decl))
    x = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (x)), x);

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
       || TREE_CODE (decl) == VAR_DECL)
      && DECL_BY_REFERENCE (decl))
    DECL_BY_REFERENCE (new_decl) = 1;

  SET_DECL_VALUE_EXPR (new_decl, x);
  DECL_HAS_VALUE_EXPR_P (new_decl) = 1;

  *slot = new_decl;
  TREE_CHAIN (new_decl) = info->debug_var_chain;
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
      if (decl_function_context (t) != info->context)
	{
	  tree x;
	  wi->changed = true;

	  x = get_nonlocal_debug_decl (info, t);
	  if (!bitmap_bit_p (info->suppress_expansion, DECL_UID (t)))
	    {
	      tree target_context = decl_function_context (t);
	      struct nesting_info *i;
	      for (i = info->outer; i->context != target_context; i = i->outer)
		continue;
	      x = lookup_field_for_decl (i, t, INSERT);
	      x = get_frame_field (info, target_context, x, &wi->gsi);
	      if (use_pointer_in_frame (t))
		{
		  x = init_tmp_var (info, x, &wi->gsi);
		  x = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (x)), x);
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
	    current_function_decl = save_context;

	    /* If the callback converted the address argument in a context
	       where we only accept variables (and min_invariant, presumably),
	       then compute the address into a temporary.  */
	    if (save_val_only)
	      *tp = gsi_gimplify_val ((struct nesting_info *) wi->info,
				      t, &wi->gsi);
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
	  else if (TREE_CODE (t) == BIT_FIELD_REF)
	    {
	      walk_tree (&TREE_OPERAND (t, 1), convert_nonlocal_reference_op,
			 wi, NULL);
	      walk_tree (&TREE_OPERAND (t, 2), convert_nonlocal_reference_op,
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
  tree clause, decl;
  int dummy;
  bitmap new_suppress;

  new_suppress = BITMAP_GGC_ALLOC ();
  bitmap_copy (new_suppress, info->suppress_expansion);

  for (clause = *pclauses; clause ; clause = OMP_CLAUSE_CHAIN (clause))
    {
      switch (OMP_CLAUSE_CODE (clause))
	{
	case OMP_CLAUSE_REDUCTION:
	  if (OMP_CLAUSE_REDUCTION_PLACEHOLDER (clause))
	    need_stmts = true;
	  goto do_decl_clause;

	case OMP_CLAUSE_LASTPRIVATE:
	  if (OMP_CLAUSE_LASTPRIVATE_GIMPLE_SEQ (clause))
	    need_stmts = true;
	  goto do_decl_clause;

	case OMP_CLAUSE_PRIVATE:
	case OMP_CLAUSE_FIRSTPRIVATE:
	case OMP_CLAUSE_COPYPRIVATE:
	case OMP_CLAUSE_SHARED:
	do_decl_clause:
	  decl = OMP_CLAUSE_DECL (clause);
	  if (TREE_CODE (decl) == VAR_DECL
	      && (TREE_STATIC (decl) || DECL_EXTERNAL (decl)))
	    break;
	  if (decl_function_context (decl) != info->context)
	    {
	      bitmap_set_bit (new_suppress, DECL_UID (decl));
	      OMP_CLAUSE_DECL (clause) = get_nonlocal_debug_decl (info, decl);
	      need_chain = true;
	    }
	  break;

	case OMP_CLAUSE_SCHEDULE:
	  if (OMP_CLAUSE_SCHEDULE_CHUNK_EXPR (clause) == NULL)
	    break;
	  /* FALLTHRU */
	case OMP_CLAUSE_IF:
	case OMP_CLAUSE_NUM_THREADS:
	  wi->val_only = true;
	  wi->is_lhs = false;
	  convert_nonlocal_reference_op (&OMP_CLAUSE_OPERAND (clause, 0),
	                                 &dummy, wi);
	  break;

	case OMP_CLAUSE_NOWAIT:
	case OMP_CLAUSE_ORDERED:
	case OMP_CLAUSE_DEFAULT:
	case OMP_CLAUSE_COPYIN:
	case OMP_CLAUSE_COLLAPSE:
	case OMP_CLAUSE_UNTIED:
	  break;

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
	  if (OMP_CLAUSE_REDUCTION_PLACEHOLDER (clause))
	    {
	      tree old_context
		= DECL_CONTEXT (OMP_CLAUSE_REDUCTION_PLACEHOLDER (clause));
	      DECL_CONTEXT (OMP_CLAUSE_REDUCTION_PLACEHOLDER (clause))
		= info->context;
	      walk_body (convert_nonlocal_reference_stmt,
			 convert_nonlocal_reference_op, info,
			 OMP_CLAUSE_REDUCTION_GIMPLE_INIT (clause));
	      walk_body (convert_nonlocal_reference_stmt,
			 convert_nonlocal_reference_op, info,
			 OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (clause));
	      DECL_CONTEXT (OMP_CLAUSE_REDUCTION_PLACEHOLDER (clause))
		= old_context;
	    }
	  break;

	case OMP_CLAUSE_LASTPRIVATE:
	  walk_body (convert_nonlocal_reference_stmt,
		     convert_nonlocal_reference_op, info,
		     OMP_CLAUSE_LASTPRIVATE_GIMPLE_SEQ (clause));
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
	  if (t && (TREE_CODE (t) == VAR_DECL || TREE_CODE (t) == PARM_DECL)
	      && decl_function_context (t) != info->context)
	    get_nonlocal_debug_decl (info, t);
	  t = TYPE_MAX_VALUE (domain);
	  if (t && (TREE_CODE (t) == VAR_DECL || TREE_CODE (t) == PARM_DECL)
	      && decl_function_context (t) != info->context)
	    get_nonlocal_debug_decl (info, t);
	}
    }
}

/* Create nonlocal debug decls for nonlocal VLA array bounds for VLAs
   in BLOCK.  */

static void
note_nonlocal_block_vlas (struct nesting_info *info, tree block)
{
  tree var;

  for (var = BLOCK_VARS (block); var; var = TREE_CHAIN (var))
    if (TREE_CODE (var) == VAR_DECL
	&& variably_modified_type_p (TREE_TYPE (var), NULL)
	&& DECL_HAS_VALUE_EXPR_P (var)
	&& decl_function_context (var) != info->context)
      note_nonlocal_vla_type (info, TREE_TYPE (var));
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
  gimple stmt = gsi_stmt (*gsi);

  switch (gimple_code (stmt))
    {
    case GIMPLE_GOTO:
      /* Don't walk non-local gotos for now.  */
      if (TREE_CODE (gimple_goto_dest (stmt)) != LABEL_DECL)
	{
	  wi->val_only = true;
	  wi->is_lhs = false;
	  *handled_ops_p = true;
	  return NULL_TREE;
	}
      break;

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
	         info, gimple_omp_body (stmt));

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
      walk_gimple_omp_for (stmt, convert_nonlocal_reference_stmt,
	  		   convert_nonlocal_reference_op, info);
      walk_body (convert_nonlocal_reference_stmt,
	  	 convert_nonlocal_reference_op, info, gimple_omp_body (stmt));
      info->suppress_expansion = save_suppress;
      break;

    case GIMPLE_OMP_SECTIONS:
      save_suppress = info->suppress_expansion;
      convert_nonlocal_omp_clauses (gimple_omp_sections_clauses_ptr (stmt), wi);
      walk_body (convert_nonlocal_reference_stmt, convert_nonlocal_reference_op,
	         info, gimple_omp_body (stmt));
      info->suppress_expansion = save_suppress;
      break;

    case GIMPLE_OMP_SINGLE:
      save_suppress = info->suppress_expansion;
      convert_nonlocal_omp_clauses (gimple_omp_single_clauses_ptr (stmt), wi);
      walk_body (convert_nonlocal_reference_stmt, convert_nonlocal_reference_op,
	         info, gimple_omp_body (stmt));
      info->suppress_expansion = save_suppress;
      break;

    case GIMPLE_OMP_SECTION:
    case GIMPLE_OMP_MASTER:
    case GIMPLE_OMP_ORDERED:
      walk_body (convert_nonlocal_reference_stmt, convert_nonlocal_reference_op,
	         info, gimple_omp_body (stmt));
      break;

    case GIMPLE_BIND:
      if (!optimize && gimple_bind_block (stmt))
	note_nonlocal_block_vlas (info, gimple_bind_block (stmt));

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
   field in FRAME.  This is used both for debug info and in OpenMP
   lowering.  */

static tree
get_local_debug_decl (struct nesting_info *info, tree decl, tree field)
{
  tree x, new_decl;
  void **slot;

  slot = pointer_map_insert (info->var_map, decl);
  if (*slot)
    return (tree) *slot;

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
       || TREE_CODE (decl) == VAR_DECL)
      && DECL_BY_REFERENCE (decl))
    DECL_BY_REFERENCE (new_decl) = 1;

  SET_DECL_VALUE_EXPR (new_decl, x);
  DECL_HAS_VALUE_EXPR_P (new_decl) = 1;
  *slot = new_decl;

  TREE_CHAIN (new_decl) = info->debug_var_chain;
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
      if (decl_function_context (t) == info->context)
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

	  x = get_local_debug_decl (info, t, field);
	  if (!bitmap_bit_p (info->suppress_expansion, DECL_UID (t)))
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
	  current_function_decl = save_context;

	  /* If we are in a context where we only accept values, then
	     compute the address into a temporary.  */
	  if (save_val_only)
	    *tp = gsi_gimplify_val ((struct nesting_info *) wi->info,
				    t, &wi->gsi);
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
	  else if (TREE_CODE (t) == BIT_FIELD_REF)
	    {
	      walk_tree (&TREE_OPERAND (t, 1), convert_local_reference_op, wi,
			 NULL);
	      walk_tree (&TREE_OPERAND (t, 2), convert_local_reference_op, wi,
			 NULL);
	    }
	}
      wi->val_only = false;
      walk_tree (tp, convert_local_reference_op, wi, NULL);
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
  tree clause, decl;
  int dummy;
  bitmap new_suppress;

  new_suppress = BITMAP_GGC_ALLOC ();
  bitmap_copy (new_suppress, info->suppress_expansion);

  for (clause = *pclauses; clause ; clause = OMP_CLAUSE_CHAIN (clause))
    {
      switch (OMP_CLAUSE_CODE (clause))
	{
	case OMP_CLAUSE_REDUCTION:
	  if (OMP_CLAUSE_REDUCTION_PLACEHOLDER (clause))
	    need_stmts = true;
	  goto do_decl_clause;

	case OMP_CLAUSE_LASTPRIVATE:
	  if (OMP_CLAUSE_LASTPRIVATE_GIMPLE_SEQ (clause))
	    need_stmts = true;
	  goto do_decl_clause;

	case OMP_CLAUSE_PRIVATE:
	case OMP_CLAUSE_FIRSTPRIVATE:
	case OMP_CLAUSE_COPYPRIVATE:
	case OMP_CLAUSE_SHARED:
	do_decl_clause:
	  decl = OMP_CLAUSE_DECL (clause);
	  if (TREE_CODE (decl) == VAR_DECL
	      && (TREE_STATIC (decl) || DECL_EXTERNAL (decl)))
	    break;
	  if (decl_function_context (decl) == info->context
	      && !use_pointer_in_frame (decl))
	    {
	      tree field = lookup_field_for_decl (info, decl, NO_INSERT);
	      if (field)
		{
		  bitmap_set_bit (new_suppress, DECL_UID (decl));
		  OMP_CLAUSE_DECL (clause)
		    = get_local_debug_decl (info, decl, field);
		  need_frame = true;
		}
	    }
	  break;

	case OMP_CLAUSE_SCHEDULE:
	  if (OMP_CLAUSE_SCHEDULE_CHUNK_EXPR (clause) == NULL)
	    break;
	  /* FALLTHRU */
	case OMP_CLAUSE_IF:
	case OMP_CLAUSE_NUM_THREADS:
	  wi->val_only = true;
	  wi->is_lhs = false;
	  convert_local_reference_op (&OMP_CLAUSE_OPERAND (clause, 0), &dummy,
				      wi);
	  break;

	case OMP_CLAUSE_NOWAIT:
	case OMP_CLAUSE_ORDERED:
	case OMP_CLAUSE_DEFAULT:
	case OMP_CLAUSE_COPYIN:
	case OMP_CLAUSE_COLLAPSE:
	case OMP_CLAUSE_UNTIED:
	  break;

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
	  if (OMP_CLAUSE_REDUCTION_PLACEHOLDER (clause))
	    {
	      tree old_context
		= DECL_CONTEXT (OMP_CLAUSE_REDUCTION_PLACEHOLDER (clause));
	      DECL_CONTEXT (OMP_CLAUSE_REDUCTION_PLACEHOLDER (clause))
		= info->context;
	      walk_body (convert_local_reference_stmt,
			 convert_local_reference_op, info,
			 OMP_CLAUSE_REDUCTION_GIMPLE_INIT (clause));
	      walk_body (convert_local_reference_stmt,
			 convert_local_reference_op, info,
			 OMP_CLAUSE_REDUCTION_GIMPLE_MERGE (clause));
	      DECL_CONTEXT (OMP_CLAUSE_REDUCTION_PLACEHOLDER (clause))
		= old_context;
	    }
	  break;

	case OMP_CLAUSE_LASTPRIVATE:
	  walk_body (convert_local_reference_stmt,
		     convert_local_reference_op, info,
		     OMP_CLAUSE_LASTPRIVATE_GIMPLE_SEQ (clause));
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
  gimple stmt = gsi_stmt (*gsi);

  switch (gimple_code (stmt))
    {
    case GIMPLE_OMP_PARALLEL:
    case GIMPLE_OMP_TASK:
      save_suppress = info->suppress_expansion;
      if (convert_local_omp_clauses (gimple_omp_taskreg_clauses_ptr (stmt),
	                             wi))
	{
	  tree c;
	  (void) get_frame_type (info);
	  c = build_omp_clause (gimple_location (stmt),
				OMP_CLAUSE_SHARED);
	  OMP_CLAUSE_DECL (c) = info->frame_decl;
	  OMP_CLAUSE_CHAIN (c) = gimple_omp_taskreg_clauses (stmt);
	  gimple_omp_taskreg_set_clauses (stmt, c);
	}

      save_local_var_chain = info->new_local_var_chain;
      info->new_local_var_chain = NULL;

      walk_body (convert_local_reference_stmt, convert_local_reference_op, info,
	         gimple_omp_body (stmt));

      if (info->new_local_var_chain)
	declare_vars (info->new_local_var_chain,
		      gimple_seq_first_stmt (gimple_omp_body (stmt)), false);
      info->new_local_var_chain = save_local_var_chain;
      info->suppress_expansion = save_suppress;
      break;

    case GIMPLE_OMP_FOR:
      save_suppress = info->suppress_expansion;
      convert_local_omp_clauses (gimple_omp_for_clauses_ptr (stmt), wi);
      walk_gimple_omp_for (stmt, convert_local_reference_stmt,
			   convert_local_reference_op, info);
      walk_body (convert_local_reference_stmt, convert_local_reference_op,
		 info, gimple_omp_body (stmt));
      info->suppress_expansion = save_suppress;
      break;

    case GIMPLE_OMP_SECTIONS:
      save_suppress = info->suppress_expansion;
      convert_local_omp_clauses (gimple_omp_sections_clauses_ptr (stmt), wi);
      walk_body (convert_local_reference_stmt, convert_local_reference_op,
		 info, gimple_omp_body (stmt));
      info->suppress_expansion = save_suppress;
      break;

    case GIMPLE_OMP_SINGLE:
      save_suppress = info->suppress_expansion;
      convert_local_omp_clauses (gimple_omp_single_clauses_ptr (stmt), wi);
      walk_body (convert_local_reference_stmt, convert_local_reference_op,
		 info, gimple_omp_body (stmt));
      info->suppress_expansion = save_suppress;
      break;

    case GIMPLE_OMP_SECTION:
    case GIMPLE_OMP_MASTER:
    case GIMPLE_OMP_ORDERED:
      walk_body (convert_local_reference_stmt, convert_local_reference_op,
		 info, gimple_omp_body (stmt));
      break;

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
  void **slot;
  gimple call;
  gimple stmt = gsi_stmt (*gsi);

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
  slot = pointer_map_insert (i->var_map, label);
  if (*slot == NULL)
    {
      new_label = create_artificial_label (UNKNOWN_LOCATION);
      DECL_NONLOCAL (new_label) = 1;
      *slot = new_label;
    }
  else
    new_label = (tree) *slot;
  
  /* Build: __builtin_nl_goto(new_label, &chain->nl_goto_field).  */
  field = get_nl_goto_field (i);
  x = get_frame_field (info, target_context, field, &wi->gsi);
  x = build_addr (x, target_context);
  x = gsi_gimplify_val (info, x, &wi->gsi);
  call = gimple_build_call (implicit_built_in_decls[BUILT_IN_NONLOCAL_GOTO], 2,
			    build_addr (new_label, target_context), x);
  gsi_replace (&wi->gsi, call, false);

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
  void **slot;
  gimple stmt = gsi_stmt (*gsi);

  if (gimple_code (stmt) != GIMPLE_LABEL)
    {
      *handled_ops_p = false;
      return NULL_TREE;
    }

  label = gimple_label_label (stmt);

  slot = pointer_map_contains (info->var_map, label);
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
      gimple stmt = gimple_build_goto (label);
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
  gimple call;

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
      if (DECL_NO_STATIC_CHAIN (decl))
	break;

      /* If we don't want a trampoline, then don't build one.  */
      if (TREE_NO_TRAMPOLINE (t))
	break;

      /* Lookup the immediate parent of the callee, as that's where
	 we need to insert the trampoline.  */
      for (i = info; i->context != target_context; i = i->outer)
	continue;
      x = lookup_tramp_for_decl (i, decl, INSERT);

      /* Compute the address of the field holding the trampoline.  */
      x = get_frame_field (info, target_context, x, &wi->gsi);
      x = build_addr (x, target_context);
      x = gsi_gimplify_val (info, x, &wi->gsi);

      /* Do machine-specific ugliness.  Normally this will involve
	 computing extra alignment, but it can really be anything.  */
      builtin = implicit_built_in_decls[BUILT_IN_ADJUST_TRAMPOLINE];
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
  gimple stmt = gsi_stmt (*gsi);

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

	*handled_ops_p = true;
	return NULL_TREE;
      }

    default:
      break;
    }

  *handled_ops_p = false;
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
  gimple stmt = gsi_stmt (*gsi);

  switch (gimple_code (stmt))
    {
    case GIMPLE_CALL:
      if (gimple_call_chain (stmt))
	break;
      decl = gimple_call_fndecl (stmt);
      if (!decl)
	break;
      target_context = decl_function_context (decl);
      if (target_context && !DECL_NO_STATIC_CHAIN (decl))
	{
	  gimple_call_set_chain (stmt, get_static_chain (info, target_context,
							 &wi->gsi));
	  info->static_chain_added |= (1 << (info->context != target_context));
	}
      break;

    case GIMPLE_OMP_PARALLEL:
    case GIMPLE_OMP_TASK:
      save_static_chain_added = info->static_chain_added;
      info->static_chain_added = 0;
      walk_body (convert_gimple_call, NULL, info, gimple_omp_body (stmt));
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

    case GIMPLE_OMP_FOR:
      walk_body (convert_gimple_call, NULL, info,
	  	 gimple_omp_for_pre_body (stmt));
      /* FALLTHRU */
    case GIMPLE_OMP_SECTIONS:
    case GIMPLE_OMP_SECTION:
    case GIMPLE_OMP_SINGLE:
    case GIMPLE_OMP_MASTER:
    case GIMPLE_OMP_ORDERED:
    case GIMPLE_OMP_CRITICAL:
      walk_body (convert_gimple_call, NULL, info, gimple_omp_body (stmt));
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
  struct nesting_info *n;
  int iter_count;
  bool any_changed;

  /* First, optimistically set no_static_chain for all decls that haven't
     used the static chain already for variable access.  Notice that we 
     do this pre-order, because we want inner functions to be processed
     first in the LIFO worklist.  */
  FOR_EACH_NEST_INFO (n, root)
    {
      tree decl = n->context;
      if (n->outer && !n->chain_decl && !n->chain_field)
	{
	  DECL_NO_STATIC_CHAIN (decl) = 1;
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Guessing no-static-chain for %s\n",
		     lang_hooks.decl_printable_name (decl, 2));
	}
      else
	gcc_assert (!DECL_NO_STATIC_CHAIN (decl));
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
      any_changed = false;
      iter_count++;

      if (dump_file && (dump_flags & TDF_DETAILS))
	fputc ('\n', dump_file);

      FOR_EACH_NEST_INFO (n, root)
	{
	  tree decl = n->context;
	  bool old_no_static_chain = DECL_NO_STATIC_CHAIN (decl);

	  walk_function (convert_tramp_reference_stmt,
			 convert_tramp_reference_op, n);
	  walk_function (convert_gimple_call, NULL, n);

	  /* If a call to another function created the use of a chain
	     within this function, we'll have to continue iteration.  */
	  if (old_no_static_chain && !DECL_NO_STATIC_CHAIN (decl))
	    any_changed = true;
	}
    }
  while (any_changed);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "convert_all_function_calls iterations: %d\n\n",
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
  void **slot = pointer_map_contains (nid->root->var_map, decl);

  if (slot)
    return (tree) *slot;

  if (TREE_CODE (decl) == TYPE_DECL && DECL_ORIGINAL_TYPE (decl))
    {
      tree new_decl = copy_decl_no_change (decl, id);
      DECL_ORIGINAL_TYPE (new_decl)
	= remap_type (DECL_ORIGINAL_TYPE (decl), id);
      return new_decl;
    }

  if (TREE_CODE (decl) == VAR_DECL
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
  void **slot;

  if (DECL_P (t))
    {
      *walk_subtrees = 0;
      slot = pointer_map_contains (root->var_map, t);

      if (slot)
	return (tree) *slot;
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

  for (var = BLOCK_VARS (block); var; var = TREE_CHAIN (var))
    {
      if (TREE_CODE (var) == VAR_DECL
	  && variably_modified_type_p (TREE_TYPE (var), NULL)
	  && DECL_HAS_VALUE_EXPR_P (var))
	{
	  type = TREE_TYPE (var);
	  val = DECL_VALUE_EXPR (var);
	  if (walk_tree (&type, contains_remapped_vars, root, NULL) != NULL
	      ||  walk_tree (&val, contains_remapped_vars, root, NULL) != NULL)
	    break;
	}
    }
  if (var == NULL_TREE)
    return;

  memset (&id, 0, sizeof (id));
  id.cb.copy_decl = nesting_copy_decl;
  id.cb.decl_map = pointer_map_create ();
  id.root = root;

  for (; var; var = TREE_CHAIN (var))
    if (TREE_CODE (var) == VAR_DECL
	&& variably_modified_type_p (TREE_TYPE (var), NULL)
	&& DECL_HAS_VALUE_EXPR_P (var))
      {
	struct nesting_info *i;
	tree newt, t, context;

	t = type = TREE_TYPE (var);
	val = DECL_VALUE_EXPR (var);
	if (walk_tree (&type, contains_remapped_vars, root, NULL) == NULL
	    && walk_tree (&val, contains_remapped_vars, root, NULL) == NULL)
	  continue;

	context = decl_function_context (var);
	for (i = root; i; i = i->outer)
	  if (i->context == context)
	    break;

	if (i == NULL)
	  continue;

	id.cb.src_fn = i->context;
	id.cb.dst_fn = i->context;
	id.cb.src_cfun = DECL_STRUCT_FUNCTION (root->context);

	TREE_TYPE (var) = newt = remap_type (type, &id.cb);
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

	walk_tree (&val, copy_tree_body_r, &id.cb, NULL);
	if (val != DECL_VALUE_EXPR (var))
	  SET_DECL_VALUE_EXPR (var, val);
      }

  pointer_map_destroy (id.cb.decl_map);
}

/* Do "everything else" to clean up or complete state collected by the
   various walking passes -- lay out the types and decls, generate code
   to initialize the frame decl, store critical expressions in the
   struct function for rtl to find.  */

static void
finalize_nesting_tree_1 (struct nesting_info *root)
{
  gimple_seq stmt_list;
  gimple stmt;
  tree context = root->context;
  struct function *sf;

  stmt_list = NULL;

  /* If we created a non-local frame type or decl, we need to lay them
     out at this time.  */
  if (root->frame_type)
    {
      /* In some cases the frame type will trigger the -Wpadded warning.
	 This is not helpful; suppress it. */
      int save_warn_padded = warn_padded;
      tree *adjust;

      warn_padded = 0;
      layout_type (root->frame_type);
      warn_padded = save_warn_padded;
      layout_decl (root->frame_decl, 0);

      /* Remove root->frame_decl from root->new_local_var_chain, so
	 that we can declare it also in the lexical blocks, which
	 helps ensure virtual regs that end up appearing in its RTL
	 expression get substituted in instantiate_virtual_regs().  */
      for (adjust = &root->new_local_var_chain;
	   *adjust != root->frame_decl;
	   adjust = &TREE_CHAIN (*adjust))
	gcc_assert (TREE_CHAIN (*adjust));
      *adjust = TREE_CHAIN (*adjust);

      TREE_CHAIN (root->frame_decl) = NULL_TREE;
      declare_vars (root->frame_decl,
		    gimple_seq_first_stmt (gimple_body (context)), true);
    }

  /* If any parameters were referenced non-locally, then we need to 
     insert a copy.  Likewise, if any variables were referenced by
     pointer, we need to initialize the address.  */
  if (root->any_parm_remapped)
    {
      tree p;
      for (p = DECL_ARGUMENTS (context); p ; p = TREE_CHAIN (p))
	{
	  tree field, x, y;

	  field = lookup_field_for_decl (root, p, NO_INSERT);
	  if (!field)
	    continue;

	  if (use_pointer_in_frame (p))
	    x = build_addr (p, context);
	  else
	    x = p;

	  y = build3 (COMPONENT_REF, TREE_TYPE (field),
		      root->frame_decl, field, NULL_TREE);
	  stmt = gimple_build_assign (y, x);
	  gimple_seq_add_stmt (&stmt_list, stmt);
	  /* If the assignment is from a non-register the stmt is
	     not valid gimple.  Make it so by using a temporary instead.  */
	  if (!is_gimple_reg (x)
	      && is_gimple_reg_type (TREE_TYPE (x)))
	    {
	      gimple_stmt_iterator gsi = gsi_last (stmt_list);
	      x = init_tmp_var (root, x, &gsi);
	      gimple_assign_set_rhs1 (stmt, x);
	    }
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
	  tree arg1, arg2, arg3, x, field;

	  field = lookup_tramp_for_decl (root, i->context, NO_INSERT);
	  if (!field)
	    continue;

	  gcc_assert (!DECL_NO_STATIC_CHAIN (i->context));
	  arg3 = build_addr (root->frame_decl, context);

	  arg2 = build_addr (i->context, context);

	  x = build3 (COMPONENT_REF, TREE_TYPE (field),
		      root->frame_decl, field, NULL_TREE);
	  arg1 = build_addr (x, context);

	  x = implicit_built_in_decls[BUILT_IN_INIT_TRAMPOLINE];
	  stmt = gimple_build_call (x, 3, arg1, arg2, arg3);
	  gimple_seq_add_stmt (&stmt_list, stmt);
	}
    }

  /* If we created initialization statements, insert them.  */
  if (stmt_list)
    {
      gimple bind;
      annotate_all_with_location (stmt_list, DECL_SOURCE_LOCATION (context));
      bind = gimple_seq_first_stmt (gimple_body (context));
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
      gimple scope;

      remap_vla_decls (DECL_INITIAL (root->context), root);

      for (debug_var = root->debug_var_chain; debug_var;
	   debug_var = TREE_CHAIN (debug_var))
	if (variably_modified_type_p (TREE_TYPE (debug_var), NULL))
	  break;

      /* If there are any debug decls with variable length types,
	 remap those types using other debug_var_chain variables.  */
      if (debug_var)
	{
	  struct nesting_copy_body_data id;

	  memset (&id, 0, sizeof (id));
	  id.cb.copy_decl = nesting_copy_decl;
	  id.cb.decl_map = pointer_map_create ();
	  id.root = root;

	  for (; debug_var; debug_var = TREE_CHAIN (debug_var))
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

	  pointer_map_destroy (id.cb.decl_map);
	}

      scope = gimple_seq_first_stmt (gimple_body (root->context));
      if (gimple_bind_block (scope))
	declare_vars (root->debug_var_chain, scope, true);
      else
	BLOCK_VARS (DECL_INITIAL (root->context))
	  = chainon (BLOCK_VARS (DECL_INITIAL (root->context)),
		     root->debug_var_chain);
    }

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
  struct cgraph_node *node = cgraph_node (root->context);

  /* For nested functions update the cgraph to reflect unnesting.
     We also delay finalizing of these functions up to this point.  */
  if (node->origin)
    {
       cgraph_unnest_node (cgraph_node (root->context));
       cgraph_finalize_function (root->context, true);
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
      pointer_map_destroy (node->var_map);
      pointer_map_destroy (node->field_map);
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
  for (iter = root->nested; iter; iter = iter->next_nested)
    gimplify_all_functions (iter);
}

/* Main entry point for this pass.  Process FNDECL and all of its nested
   subroutines and turn them into something less tightly bound.  */

void
lower_nested_functions (tree fndecl)
{
  struct cgraph_node *cgn;
  struct nesting_info *root;
#ifdef ENABLE_CHECKING
  struct nesting_info *n;
  bitmap orig_decl_no_static_chain;
#endif

  /* If there are no nested functions, there's nothing to do.  */
  cgn = cgraph_node (fndecl);
  if (!cgn->nested)
    return;

  gimplify_all_functions (cgn);

  dump_file = dump_begin (TDI_nested, &dump_flags);
  if (dump_file)
    fprintf (dump_file, "\n;; Function %s\n\n",
	     lang_hooks.decl_printable_name (fndecl, 2));

  bitmap_obstack_initialize (&nesting_info_bitmap_obstack);
  root = create_nesting_tree (cgn);

#ifdef ENABLE_CHECKING
  /* The C++ and Ada front ends set DECL_NO_STATIC_CHAIN in various
     instances where they expect no static chain needed.  */
  orig_decl_no_static_chain = BITMAP_ALLOC (&nesting_info_bitmap_obstack);
  FOR_EACH_NEST_INFO (n, root)
    if (DECL_NO_STATIC_CHAIN (n->context))
      bitmap_set_bit (orig_decl_no_static_chain, DECL_UID (n->context));
#endif

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

#ifdef ENABLE_CHECKING
  /* Validate the original settings of DECL_NO_STATIC_CHAIN.  */
  FOR_EACH_NEST_INFO (n, root)
    if (bitmap_bit_p (orig_decl_no_static_chain, DECL_UID (n->context)))
      gcc_assert (DECL_NO_STATIC_CHAIN (n->context));
#endif

  free_nesting_tree (root);
  bitmap_obstack_release (&nesting_info_bitmap_obstack);

  if (dump_file)
    {
      dump_end (TDI_nested, dump_file);
      dump_file = NULL;
    }
}

#include "gt-tree-nested.h"
