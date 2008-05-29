/* Nested function decomposition for trees.
   Copyright (C) 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

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
#include "tree-gimple.h"
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
     specific.  ie, they are dependent on CURRENT_FUNCTION_DECL.

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
      return slot ? *slot : NULL;
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
      decl = build_decl (PARM_DECL, create_tmp_var_name ("CHAIN"), type);
      DECL_ARTIFICIAL (decl) = 1;
      DECL_IGNORED_P (decl) = 1;
      TREE_USED (decl) = 1;
      DECL_CONTEXT (decl) = info->context;
      DECL_ARG_TYPE (decl) = type;

      /* Tell tree-inline.c that we never write to this variable, so
	 it can copy-prop the replacement value immediately.  */
      TREE_READONLY (decl) = 1;

      info->chain_decl = decl;
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
    }
  return field;
}

/* Copy EXP into a temporary.  Allocate the temporary in the context of
   INFO and insert the initialization statement before TSI.  */

static tree
init_tmp_var (struct nesting_info *info, tree exp, tree_stmt_iterator *tsi)
{
  tree t, stmt;

  t = create_tmp_var_for (info, TREE_TYPE (exp), NULL);
  stmt = build_gimple_modify_stmt (t, exp);
  SET_EXPR_LOCUS (stmt, EXPR_LOCUS (tsi_stmt (*tsi)));
  tsi_link_before (tsi, stmt, TSI_SAME_STMT);

  return t;
}

/* Similarly, but only do so to force EXP to satisfy is_gimple_val.  */

static tree
tsi_gimplify_val (struct nesting_info *info, tree exp, tree_stmt_iterator *tsi)
{
  if (is_gimple_val (exp))
    return exp;
  else
    return init_tmp_var (info, exp, tsi);
}

/* Similarly, but copy from the temporary and insert the statement
   after the iterator.  */

static tree
save_tmp_var (struct nesting_info *info, tree exp,
	      tree_stmt_iterator *tsi)
{
  tree t, stmt;

  t = create_tmp_var_for (info, TREE_TYPE (exp), NULL);
  stmt = build_gimple_modify_stmt (exp, t);
  SET_EXPR_LOCUS (stmt, EXPR_LOCUS (tsi_stmt (*tsi)));
  tsi_link_after (tsi, stmt, TSI_SAME_STMT);

  return t;
}

/* Build or return the type used to represent a nested function trampoline.  */

static GTY(()) tree trampoline_type;

static tree
get_trampoline_type (void)
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
  t = build_decl (FIELD_DECL, get_identifier ("__data"), t);
  DECL_ALIGN (t) = align;
  DECL_USER_ALIGN (t) = 1;

  trampoline_type = make_node (RECORD_TYPE);
  TYPE_NAME (trampoline_type) = get_identifier ("__builtin_trampoline");
  TYPE_FIELDS (trampoline_type) = t;
  layout_type (trampoline_type);

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
      return slot ? *slot : NULL;
    }

  slot = pointer_map_insert (info->var_map, decl);
  if (!*slot)
    {
      tree field = make_node (FIELD_DECL);
      DECL_NAME (field) = DECL_NAME (decl);
      TREE_TYPE (field) = get_trampoline_type ();
      TREE_ADDRESSABLE (field) = 1;

      insert_field_into_struct (get_frame_type (info), field);
      *slot = field;

      info->any_tramp_created = true;
    }

  return *slot;
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

/* Helper function for walk_stmts.  Walk output operands of an ASM_EXPR.  */

static void
walk_asm_expr (struct walk_stmt_info *wi, tree stmt)
{
  int noutputs = list_length (ASM_OUTPUTS (stmt));
  const char **oconstraints
    = (const char **) alloca ((noutputs) * sizeof (const char *));
  int i;
  tree link;
  const char *constraint;
  bool allows_mem, allows_reg, is_inout;

  wi->is_lhs = true;
  for (i=0, link = ASM_OUTPUTS (stmt); link; ++i, link = TREE_CHAIN (link))
    {
      constraint = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (link)));
      oconstraints[i] = constraint;
      parse_output_constraint (&constraint, i, 0, 0, &allows_mem,
			       &allows_reg, &is_inout);

      wi->val_only = (allows_reg || !allows_mem);
      walk_tree (&TREE_VALUE (link), wi->callback, wi, NULL);
    }

  for (link = ASM_INPUTS (stmt); link; link = TREE_CHAIN (link))
    {
      constraint = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (link)));
      parse_input_constraint (&constraint, 0, 0, noutputs, 0,
			      oconstraints, &allows_mem, &allows_reg);

      wi->val_only = (allows_reg || !allows_mem);
      /* Although input "m" is not really a LHS, we need a lvalue.  */
      wi->is_lhs = !wi->val_only;
      walk_tree (&TREE_VALUE (link), wi->callback, wi, NULL);
    }

  wi->is_lhs = false;
  wi->val_only = true;
}

/* Iterate over all sub-statements of *TP calling walk_tree with
   WI->CALLBACK for every sub-expression in each statement found.  */

void
walk_stmts (struct walk_stmt_info *wi, tree *tp)
{
  tree t = *tp;
  int walk_subtrees;

  if (!t)
    return;

  if (wi->want_locations && EXPR_HAS_LOCATION (t))
    input_location = EXPR_LOCATION (t);

  switch (TREE_CODE (t))
    {
    case STATEMENT_LIST:
      {
	tree_stmt_iterator i;
	for (i = tsi_start (t); !tsi_end_p (i); tsi_next (&i))
	  {
	    wi->tsi = i;
	    walk_stmts (wi, tsi_stmt_ptr (i));
	  }
      }
      break;

    case COND_EXPR:
      walk_tree (&COND_EXPR_COND (t), wi->callback, wi, NULL);
      walk_stmts (wi, &COND_EXPR_THEN (t));
      walk_stmts (wi, &COND_EXPR_ELSE (t));
      break;
    case CATCH_EXPR:
      walk_stmts (wi, &CATCH_BODY (t));
      break;
    case EH_FILTER_EXPR:
      walk_stmts (wi, &EH_FILTER_FAILURE (t));
      break;
    case TRY_CATCH_EXPR:
    case TRY_FINALLY_EXPR:
      walk_stmts (wi, &TREE_OPERAND (t, 0));
      walk_stmts (wi, &TREE_OPERAND (t, 1));
      break;

    case BIND_EXPR:
      if (wi->want_bind_expr)
	{
	  walk_subtrees = 1;
	  wi->callback (tp, &walk_subtrees, wi);
	  if (!walk_subtrees)
	    break;
	}
      walk_stmts (wi, &BIND_EXPR_BODY (t));
      break;

    case RETURN_EXPR:
      if (wi->want_return_expr)
	{
	  walk_subtrees = 1;
	  wi->callback (tp, &walk_subtrees, wi);
	  if (!walk_subtrees)
	    break;
	}
      walk_stmts (wi, &TREE_OPERAND (t, 0));
      break;

    case GIMPLE_MODIFY_STMT:
      /* A formal temporary lhs may use a COMPONENT_REF rhs.  */
      wi->val_only = !is_gimple_formal_tmp_var (GIMPLE_STMT_OPERAND (t, 0));
      walk_tree (&GIMPLE_STMT_OPERAND (t, 1), wi->callback, wi, NULL);

      /* If the rhs is appropriate for a memory, we may use a
	 COMPONENT_REF on the lhs.  */
      wi->val_only = !is_gimple_mem_rhs (GIMPLE_STMT_OPERAND (t, 1));
      wi->is_lhs = true;
      walk_tree (&GIMPLE_STMT_OPERAND (t, 0), wi->callback, wi, NULL);

      wi->val_only = true;
      wi->is_lhs = false;
      break;

    case ASM_EXPR:
      walk_asm_expr (wi, *tp);
      break;

    default:
      wi->val_only = true;
      walk_tree (tp, wi->callback, wi, NULL);
      break;
    }
}

/* Invoke CALLBACK on all statements of *STMT_P.  */

static void
walk_body (walk_tree_fn callback, struct nesting_info *info, tree *stmt_p)
{
  struct walk_stmt_info wi;

  memset (&wi, 0, sizeof (wi));
  wi.callback = callback;
  wi.info = info;
  wi.val_only = true;

  walk_stmts (&wi, stmt_p);
}

/* Invoke CALLBACK on all statements of INFO->CONTEXT.  */

static inline void
walk_function (walk_tree_fn callback, struct nesting_info *info)
{
  walk_body (callback, info, &DECL_SAVED_TREE (info->context));
}

/* Invoke CALLBACK on OMP_FOR init, cond, incr and pre-body.  */

static void
walk_omp_for (walk_tree_fn callback, struct nesting_info *info, tree for_stmt)
{
  struct walk_stmt_info wi;
  tree t, list = NULL, empty;

  walk_body (callback, info, &OMP_FOR_PRE_BODY (for_stmt));

  empty = build_empty_stmt ();
  append_to_statement_list_force (empty, &list);
  memset (&wi, 0, sizeof (wi));
  wi.callback = callback;
  wi.info = info;
  wi.tsi = tsi_last (list);

  t = OMP_FOR_INIT (for_stmt);
  gcc_assert (TREE_CODE (t) == GIMPLE_MODIFY_STMT);
  SET_EXPR_LOCUS (empty, EXPR_LOCUS (t));
  wi.val_only = false;
  walk_tree (&GIMPLE_STMT_OPERAND (t, 0), callback, &wi, NULL);
  wi.val_only = true;
  wi.is_lhs = false;
  walk_tree (&GIMPLE_STMT_OPERAND (t, 1), callback, &wi, NULL);

  t = OMP_FOR_COND (for_stmt);
  gcc_assert (COMPARISON_CLASS_P (t));
  SET_EXPR_LOCUS (empty, EXPR_LOCUS (t));
  wi.val_only = false;
  walk_tree (&TREE_OPERAND (t, 0), callback, &wi, NULL);
  wi.val_only = true;
  wi.is_lhs = false;
  walk_tree (&TREE_OPERAND (t, 1), callback, &wi, NULL);

  t = OMP_FOR_INCR (for_stmt);
  gcc_assert (TREE_CODE (t) == GIMPLE_MODIFY_STMT);
  SET_EXPR_LOCUS (empty, EXPR_LOCUS (t));
  wi.val_only = false;
  walk_tree (&GIMPLE_STMT_OPERAND (t, 0), callback, &wi, NULL);
  t = GIMPLE_STMT_OPERAND (t, 1);
  gcc_assert (BINARY_CLASS_P (t));
  wi.val_only = false;
  walk_tree (&TREE_OPERAND (t, 0), callback, &wi, NULL);
  wi.val_only = true;
  wi.is_lhs = false;
  walk_tree (&TREE_OPERAND (t, 1), callback, &wi, NULL);

  /* Remove empty statement added above from the end of statement list.  */
  tsi_delink (&wi.tsi);
  append_to_statement_list (list, &OMP_FOR_PRE_BODY (for_stmt));
}

/* Similarly for ROOT and all functions nested underneath, depth first.  */
    
static void
walk_all_functions (walk_tree_fn callback, struct nesting_info *root)
{
  do
    {
      if (root->inner)
	walk_all_functions (callback, root->inner);
      walk_function (callback, root);
      root = root->next;
    }
  while (root);
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
		  tree_stmt_iterator *tsi)
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
	  x = init_tmp_var (info, x, tsi);
	}
    }

  return x;
}

/* Return an expression referencing FIELD from TARGET_CONTEXT's non-local
   frame as seen from INFO->CONTEXT.  Insert any necessary computations
   before TSI.  */

static tree
get_frame_field (struct nesting_info *info, tree target_context,
		 tree field, tree_stmt_iterator *tsi)
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
	  x = init_tmp_var (info, x, tsi);
	}

      x = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (x)), x);
    }

  x = build3 (COMPONENT_REF, TREE_TYPE (field), x, field, NULL_TREE);
  return x;
}

/* A subroutine of convert_nonlocal_reference.  Create a local variable
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
    return *slot;

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
  new_decl = build_decl (VAR_DECL, DECL_NAME (decl), TREE_TYPE (decl));
  DECL_CONTEXT (new_decl) = info->context;
  DECL_SOURCE_LOCATION (new_decl) = DECL_SOURCE_LOCATION (decl);
  DECL_ARTIFICIAL (new_decl) = DECL_ARTIFICIAL (decl);
  DECL_IGNORED_P (new_decl) = DECL_IGNORED_P (decl);
  TREE_THIS_VOLATILE (new_decl) = TREE_THIS_VOLATILE (decl);
  TREE_SIDE_EFFECTS (new_decl) = TREE_SIDE_EFFECTS (decl);
  TREE_READONLY (new_decl) = TREE_READONLY (decl);
  TREE_ADDRESSABLE (new_decl) = TREE_ADDRESSABLE (decl);
  DECL_SEEN_IN_BIND_EXPR_P (new_decl) = 1;

  SET_DECL_VALUE_EXPR (new_decl, x);
  DECL_HAS_VALUE_EXPR_P (new_decl) = 1;

  *slot = new_decl;
  TREE_CHAIN (new_decl) = info->debug_var_chain;
  info->debug_var_chain = new_decl;

  return new_decl;
}

/* Called via walk_function+walk_tree, rewrite all references to VAR
   and PARM_DECLs that belong to outer functions.

   The rewrite will involve some number of structure accesses back up
   the static chain.  E.g. for a variable FOO up one nesting level it'll
   be CHAIN->FOO.  For two levels it'll be CHAIN->__chain->FOO.  Further
   indirections apply to decls for which use_pointer_in_frame is true.  */

static bool convert_nonlocal_omp_clauses (tree *, struct walk_stmt_info *);

static tree
convert_nonlocal_reference (tree *tp, int *walk_subtrees, void *data)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data;
  struct nesting_info *info = wi->info;
  tree t = *tp;
  tree save_local_var_chain;
  bitmap save_suppress;

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
	      x = get_frame_field (info, target_context, x, &wi->tsi);
	      if (use_pointer_in_frame (t))
		{
		  x = init_tmp_var (info, x, &wi->tsi);
		  x = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (x)), x);
		}
	    }

	  if (wi->val_only)
	    {
	      if (wi->is_lhs)
		x = save_tmp_var (info, x, &wi->tsi);
	      else
		x = init_tmp_var (info, x, &wi->tsi);
	    }

	  *tp = x;
	}
      break;

    case GOTO_EXPR:
      /* Don't walk non-local gotos for now.  */
      if (TREE_CODE (GOTO_DESTINATION (t)) != LABEL_DECL)
	{
	  *walk_subtrees = 1;
	  wi->val_only = true;
	  wi->is_lhs = false;
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
	walk_tree (&TREE_OPERAND (t, 0), convert_nonlocal_reference, wi, NULL);
	wi->val_only = true;

	if (wi->changed)
	  {
	    tree save_context;

	    /* If we changed anything, then TREE_INVARIANT is be wrong,
	       since we're no longer directly referencing a decl.  */
	    save_context = current_function_decl;
	    current_function_decl = info->context;
	    recompute_tree_invariant_for_addr_expr (t);
	    current_function_decl = save_context;

	    /* If the callback converted the address argument in a context
	       where we only accept variables (and min_invariant, presumably),
	       then compute the address into a temporary.  */
	    if (save_val_only)
	      *tp = tsi_gimplify_val (wi->info, t, &wi->tsi);
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
	    walk_tree (&TREE_OPERAND (t, 2), convert_nonlocal_reference, wi,
		       NULL);
	  else if (TREE_CODE (t) == ARRAY_REF
		   || TREE_CODE (t) == ARRAY_RANGE_REF)
	    {
	      walk_tree (&TREE_OPERAND (t, 1), convert_nonlocal_reference, wi,
			 NULL);
	      walk_tree (&TREE_OPERAND (t, 2), convert_nonlocal_reference, wi,
			 NULL);
	      walk_tree (&TREE_OPERAND (t, 3), convert_nonlocal_reference, wi,
			 NULL);
	    }
	  else if (TREE_CODE (t) == BIT_FIELD_REF)
	    {
	      walk_tree (&TREE_OPERAND (t, 1), convert_nonlocal_reference, wi,
			 NULL);
	      walk_tree (&TREE_OPERAND (t, 2), convert_nonlocal_reference, wi,
			 NULL);
	    }
	}
      wi->val_only = false;
      walk_tree (tp, convert_nonlocal_reference, wi, NULL);
      break;

    case VIEW_CONVERT_EXPR:
      /* Just request to look at the subtrees, leaving val_only and lhs
	 untouched.  This might actually be for !val_only + lhs, in which
	 case we don't want to force a replacement by a temporary.  */
      *walk_subtrees = 1;
      break;

    case OMP_PARALLEL:
      save_suppress = info->suppress_expansion;
      if (convert_nonlocal_omp_clauses (&OMP_PARALLEL_CLAUSES (t), wi))
	{
	  tree c, decl;
	  decl = get_chain_decl (info);
	  c = build_omp_clause (OMP_CLAUSE_FIRSTPRIVATE);
	  OMP_CLAUSE_DECL (c) = decl;
	  OMP_CLAUSE_CHAIN (c) = OMP_PARALLEL_CLAUSES (t);
	  OMP_PARALLEL_CLAUSES (t) = c;
	}

      save_local_var_chain = info->new_local_var_chain;
      info->new_local_var_chain = NULL;

      walk_body (convert_nonlocal_reference, info, &OMP_PARALLEL_BODY (t));

      if (info->new_local_var_chain)
	declare_vars (info->new_local_var_chain, OMP_PARALLEL_BODY (t), false);
      info->new_local_var_chain = save_local_var_chain;
      info->suppress_expansion = save_suppress;
      break;

    case OMP_FOR:
      save_suppress = info->suppress_expansion;
      convert_nonlocal_omp_clauses (&OMP_FOR_CLAUSES (t), wi);
      walk_omp_for (convert_nonlocal_reference, info, t);
      walk_body (convert_nonlocal_reference, info, &OMP_FOR_BODY (t));
      info->suppress_expansion = save_suppress;
      break;

    case OMP_SECTIONS:
    case OMP_SINGLE:
      save_suppress = info->suppress_expansion;
      convert_nonlocal_omp_clauses (&OMP_CLAUSES (t), wi);
      walk_body (convert_nonlocal_reference, info, &OMP_BODY (t));
      info->suppress_expansion = save_suppress;
      break;

    case OMP_SECTION:
    case OMP_MASTER:
    case OMP_ORDERED:
      walk_body (convert_nonlocal_reference, info, &OMP_BODY (t));
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

static bool
convert_nonlocal_omp_clauses (tree *pclauses, struct walk_stmt_info *wi)
{
  struct nesting_info *info = wi->info;
  bool need_chain = false;
  tree clause, decl;
  int dummy;
  bitmap new_suppress;

  new_suppress = BITMAP_GGC_ALLOC ();
  bitmap_copy (new_suppress, info->suppress_expansion);

  for (clause = *pclauses; clause ; clause = OMP_CLAUSE_CHAIN (clause))
    {
      switch (OMP_CLAUSE_CODE (clause))
	{
	case OMP_CLAUSE_PRIVATE:
	case OMP_CLAUSE_FIRSTPRIVATE:
	case OMP_CLAUSE_LASTPRIVATE:
	case OMP_CLAUSE_REDUCTION:
	case OMP_CLAUSE_COPYPRIVATE:
	case OMP_CLAUSE_SHARED:
	  decl = OMP_CLAUSE_DECL (clause);
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
	  convert_nonlocal_reference (&OMP_CLAUSE_OPERAND (clause, 0), &dummy,
	                              wi);
	  break;

	case OMP_CLAUSE_NOWAIT:
	case OMP_CLAUSE_ORDERED:
	case OMP_CLAUSE_DEFAULT:
	case OMP_CLAUSE_COPYIN:
	  break;

	default:
	  gcc_unreachable ();
	}
    }

  info->suppress_expansion = new_suppress;

  return need_chain;
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
    return *slot;

  /* Make sure frame_decl gets created.  */
  (void) get_frame_type (info);
  x = info->frame_decl;
  x = build3 (COMPONENT_REF, TREE_TYPE (field), x, field, NULL_TREE);

  new_decl = build_decl (VAR_DECL, DECL_NAME (decl), TREE_TYPE (decl));
  DECL_CONTEXT (new_decl) = info->context;
  DECL_SOURCE_LOCATION (new_decl) = DECL_SOURCE_LOCATION (decl);
  DECL_ARTIFICIAL (new_decl) = DECL_ARTIFICIAL (decl);
  DECL_IGNORED_P (new_decl) = DECL_IGNORED_P (decl);
  TREE_THIS_VOLATILE (new_decl) = TREE_THIS_VOLATILE (decl);
  TREE_SIDE_EFFECTS (new_decl) = TREE_SIDE_EFFECTS (decl);
  TREE_READONLY (new_decl) = TREE_READONLY (decl);
  TREE_ADDRESSABLE (new_decl) = TREE_ADDRESSABLE (decl);
  DECL_SEEN_IN_BIND_EXPR_P (new_decl) = 1;

  SET_DECL_VALUE_EXPR (new_decl, x);
  DECL_HAS_VALUE_EXPR_P (new_decl) = 1;
  *slot = new_decl;

  TREE_CHAIN (new_decl) = info->debug_var_chain;
  info->debug_var_chain = new_decl;

  /* Do not emit debug info twice.  */
  DECL_IGNORED_P (decl) = 1;

  return new_decl;
}

/* Called via walk_function+walk_tree, rewrite all references to VAR
   and PARM_DECLs that were referenced by inner nested functions.
   The rewrite will be a structure reference to the local frame variable.  */

static bool convert_local_omp_clauses (tree *, struct walk_stmt_info *);

static tree
convert_local_reference (tree *tp, int *walk_subtrees, void *data)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data;
  struct nesting_info *info = wi->info;
  tree t = *tp, field, x;
  bool save_val_only;
  tree save_local_var_chain;
  bitmap save_suppress;

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
	    x = get_frame_field (info, info->context, field, &wi->tsi);

	  if (wi->val_only)
	    {
	      if (wi->is_lhs)
		x = save_tmp_var (info, x, &wi->tsi);
	      else
		x = init_tmp_var (info, x, &wi->tsi);
	    }

	  *tp = x;
	}
      break;

    case ADDR_EXPR:
      save_val_only = wi->val_only;
      wi->val_only = false;
      wi->is_lhs = false;
      wi->changed = false;
      walk_tree (&TREE_OPERAND (t, 0), convert_local_reference, wi, NULL);
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
	    *tp = tsi_gimplify_val (wi->info, t, &wi->tsi);
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
	    walk_tree (&TREE_OPERAND (t, 2), convert_local_reference, wi,
		       NULL);
	  else if (TREE_CODE (t) == ARRAY_REF
		   || TREE_CODE (t) == ARRAY_RANGE_REF)
	    {
	      walk_tree (&TREE_OPERAND (t, 1), convert_local_reference, wi,
			 NULL);
	      walk_tree (&TREE_OPERAND (t, 2), convert_local_reference, wi,
			 NULL);
	      walk_tree (&TREE_OPERAND (t, 3), convert_local_reference, wi,
			 NULL);
	    }
	  else if (TREE_CODE (t) == BIT_FIELD_REF)
	    {
	      walk_tree (&TREE_OPERAND (t, 1), convert_local_reference, wi,
			 NULL);
	      walk_tree (&TREE_OPERAND (t, 2), convert_local_reference, wi,
			 NULL);
	    }
	}
      wi->val_only = false;
      walk_tree (tp, convert_local_reference, wi, NULL);
      wi->val_only = save_val_only;
      break;

    case VIEW_CONVERT_EXPR:
      /* Just request to look at the subtrees, leaving val_only and lhs
	 untouched.  This might actually be for !val_only + lhs, in which
	 case we don't want to force a replacement by a temporary.  */
      *walk_subtrees = 1;
      break;

    case OMP_PARALLEL:
      save_suppress = info->suppress_expansion;
      if (convert_local_omp_clauses (&OMP_PARALLEL_CLAUSES (t), wi))
	{
	  tree c;
	  (void) get_frame_type (info);
	  c = build_omp_clause (OMP_CLAUSE_SHARED);
	  OMP_CLAUSE_DECL (c) = info->frame_decl;
	  OMP_CLAUSE_CHAIN (c) = OMP_PARALLEL_CLAUSES (t);
	  OMP_PARALLEL_CLAUSES (t) = c;
	}

      save_local_var_chain = info->new_local_var_chain;
      info->new_local_var_chain = NULL;

      walk_body (convert_local_reference, info, &OMP_PARALLEL_BODY (t));

      if (info->new_local_var_chain)
	declare_vars (info->new_local_var_chain, OMP_PARALLEL_BODY (t), false);
      info->new_local_var_chain = save_local_var_chain;
      info->suppress_expansion = save_suppress;
      break;

    case OMP_FOR:
      save_suppress = info->suppress_expansion;
      convert_local_omp_clauses (&OMP_FOR_CLAUSES (t), wi);
      walk_omp_for (convert_local_reference, info, t);
      walk_body (convert_local_reference, info, &OMP_FOR_BODY (t));
      info->suppress_expansion = save_suppress;
      break;

    case OMP_SECTIONS:
    case OMP_SINGLE:
      save_suppress = info->suppress_expansion;
      convert_local_omp_clauses (&OMP_CLAUSES (t), wi);
      walk_body (convert_local_reference, info, &OMP_BODY (t));
      info->suppress_expansion = save_suppress;
      break;

    case OMP_SECTION:
    case OMP_MASTER:
    case OMP_ORDERED:
      walk_body (convert_local_reference, info, &OMP_BODY (t));
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

static bool
convert_local_omp_clauses (tree *pclauses, struct walk_stmt_info *wi)
{
  struct nesting_info *info = wi->info;
  bool need_frame = false;
  tree clause, decl;
  int dummy;
  bitmap new_suppress;

  new_suppress = BITMAP_GGC_ALLOC ();
  bitmap_copy (new_suppress, info->suppress_expansion);

  for (clause = *pclauses; clause ; clause = OMP_CLAUSE_CHAIN (clause))
    {
      switch (OMP_CLAUSE_CODE (clause))
	{
	case OMP_CLAUSE_PRIVATE:
	case OMP_CLAUSE_FIRSTPRIVATE:
	case OMP_CLAUSE_LASTPRIVATE:
	case OMP_CLAUSE_REDUCTION:
	case OMP_CLAUSE_COPYPRIVATE:
	case OMP_CLAUSE_SHARED:
	  decl = OMP_CLAUSE_DECL (clause);
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
	  convert_local_reference (&OMP_CLAUSE_OPERAND (clause, 0), &dummy, wi);
	  break;

	case OMP_CLAUSE_NOWAIT:
	case OMP_CLAUSE_ORDERED:
	case OMP_CLAUSE_DEFAULT:
	case OMP_CLAUSE_COPYIN:
	  break;

	default:
	  gcc_unreachable ();
	}
    }

  info->suppress_expansion = new_suppress;

  return need_frame;
}

/* Called via walk_function+walk_tree, rewrite all GOTO_EXPRs that 
   reference labels from outer functions.  The rewrite will be a 
   call to __builtin_nonlocal_goto.  */

static tree
convert_nl_goto_reference (tree *tp, int *walk_subtrees, void *data)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data;
  struct nesting_info *info = wi->info, *i;
  tree t = *tp, label, new_label, target_context, x, field;
  void **slot;

  *walk_subtrees = 0;
  if (TREE_CODE (t) != GOTO_EXPR)
    return NULL_TREE;
  label = GOTO_DESTINATION (t);
  if (TREE_CODE (label) != LABEL_DECL)
    return NULL_TREE;
  target_context = decl_function_context (label);
  if (target_context == info->context)
    return NULL_TREE;

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
      new_label = create_artificial_label ();
      DECL_NONLOCAL (new_label) = 1;
      *slot = new_label;
    }
  else
    new_label = *slot;
  
  /* Build: __builtin_nl_goto(new_label, &chain->nl_goto_field).  */
  field = get_nl_goto_field (i);
  x = get_frame_field (info, target_context, field, &wi->tsi);
  x = build_addr (x, target_context);
  x = tsi_gimplify_val (info, x, &wi->tsi);
  x = build_call_expr (implicit_built_in_decls[BUILT_IN_NONLOCAL_GOTO], 2,
		       build_addr (new_label, target_context), x);

  SET_EXPR_LOCUS (x, EXPR_LOCUS (tsi_stmt (wi->tsi)));
  *tsi_stmt_ptr (wi->tsi) = x;

  return NULL_TREE;
}

/* Called via walk_function+walk_tree, rewrite all LABEL_EXPRs that 
   are referenced via nonlocal goto from a nested function.  The rewrite
   will involve installing a newly generated DECL_NONLOCAL label, and
   (potentially) a branch around the rtl gunk that is assumed to be 
   attached to such a label.  */

static tree
convert_nl_goto_receiver (tree *tp, int *walk_subtrees, void *data)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data;
  struct nesting_info *info = wi->info;
  tree t = *tp, label, new_label, x;
  tree_stmt_iterator tmp_tsi;
  void **slot;

  *walk_subtrees = 0;
  if (TREE_CODE (t) != LABEL_EXPR)
    return NULL_TREE;
  label = LABEL_EXPR_LABEL (t);

  slot = pointer_map_contains (info->var_map, label);
  if (!slot)
    return NULL_TREE;

  /* If there's any possibility that the previous statement falls through,
     then we must branch around the new non-local label.  */
  tmp_tsi = wi->tsi;
  tsi_prev (&tmp_tsi);
  if (tsi_end_p (tmp_tsi) || block_may_fallthru (tsi_stmt (tmp_tsi)))
    {
      x = build1 (GOTO_EXPR, void_type_node, label);
      tsi_link_before (&wi->tsi, x, TSI_SAME_STMT);
    }

  new_label = (tree) *slot;
  x = build1 (LABEL_EXPR, void_type_node, new_label);
  tsi_link_before (&wi->tsi, x, TSI_SAME_STMT);

  return NULL_TREE;
}

/* Called via walk_function+walk_tree, rewrite all references to addresses
   of nested functions that require the use of trampolines.  The rewrite
   will involve a reference a trampoline generated for the occasion.  */

static tree
convert_tramp_reference (tree *tp, int *walk_subtrees, void *data)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data;
  struct nesting_info *info = wi->info, *i;
  tree t = *tp, decl, target_context, x;

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

      /* Lookup the immediate parent of the callee, as that's where
	 we need to insert the trampoline.  */
      for (i = info; i->context != target_context; i = i->outer)
	continue;
      x = lookup_tramp_for_decl (i, decl, INSERT);

      /* Compute the address of the field holding the trampoline.  */
      x = get_frame_field (info, target_context, x, &wi->tsi);
      x = build_addr (x, target_context);
      x = tsi_gimplify_val (info, x, &wi->tsi);

      /* Do machine-specific ugliness.  Normally this will involve
	 computing extra alignment, but it can really be anything.  */
      x = build_call_expr (implicit_built_in_decls[BUILT_IN_ADJUST_TRAMPOLINE],
			   1, x);
      x = init_tmp_var (info, x, &wi->tsi);

      /* Cast back to the proper function type.  */
      x = build1 (NOP_EXPR, TREE_TYPE (t), x);
      x = init_tmp_var (info, x, &wi->tsi);

      *tp = x;
      break;

    case CALL_EXPR:
      /* Only walk call arguments, lest we generate trampolines for
	 direct calls.  */
      {
	int nargs = call_expr_nargs (t);
	int i;
	for (i = 0; i < nargs; i++)
	  walk_tree (&CALL_EXPR_ARG (t, i), convert_tramp_reference, wi, NULL);
      }
      break;

    default:
      if (!IS_TYPE_OR_DECL_P (t))
	*walk_subtrees = 1;
      break;
    }

  return NULL_TREE;
}

/* Called via walk_function+walk_tree, rewrite all CALL_EXPRs that 
   reference nested functions to make sure that the static chain is
   set up properly for the call.  */

static tree
convert_call_expr (tree *tp, int *walk_subtrees, void *data)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data;
  struct nesting_info *info = wi->info;
  tree t = *tp, decl, target_context;
  char save_static_chain_added;
  int i;

  *walk_subtrees = 0;
  switch (TREE_CODE (t))
    {
    case CALL_EXPR:
      decl = get_callee_fndecl (t);
      if (!decl)
	break;
      target_context = decl_function_context (decl);
      if (target_context && !DECL_NO_STATIC_CHAIN (decl))
	{
	  CALL_EXPR_STATIC_CHAIN (t)
	    = get_static_chain (info, target_context, &wi->tsi);
	  info->static_chain_added
	    |= (1 << (info->context != target_context));
	}
      break;

    case RETURN_EXPR:
    case GIMPLE_MODIFY_STMT:
    case WITH_SIZE_EXPR:
      /* Only return modify and with_size_expr may contain calls.  */
      *walk_subtrees = 1;
      break;

    case OMP_PARALLEL:
      save_static_chain_added = info->static_chain_added;
      info->static_chain_added = 0;
      walk_body (convert_call_expr, info, &OMP_PARALLEL_BODY (t));
      for (i = 0; i < 2; i++)
	{
	  tree c, decl;
	  if ((info->static_chain_added & (1 << i)) == 0)
	    continue;
	  decl = i ? get_chain_decl (info) : info->frame_decl;
	  /* Don't add CHAIN.* or FRAME.* twice.  */
	  for (c = OMP_PARALLEL_CLAUSES (t); c; c = OMP_CLAUSE_CHAIN (c))
	    if ((OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FIRSTPRIVATE
		 || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_SHARED)
		&& OMP_CLAUSE_DECL (c) == decl)
	      break;
	  if (c == NULL)
	    {
	      c = build_omp_clause (i ? OMP_CLAUSE_FIRSTPRIVATE
				      : OMP_CLAUSE_SHARED);
	      OMP_CLAUSE_DECL (c) = decl;
	      OMP_CLAUSE_CHAIN (c) = OMP_PARALLEL_CLAUSES (t);
	      OMP_PARALLEL_CLAUSES (t) = c;
	    }
	}
      info->static_chain_added |= save_static_chain_added;
      break;

    case OMP_FOR:
      walk_body (convert_call_expr, info, &OMP_FOR_PRE_BODY (t));
      /* FALLTHRU */
    case OMP_SECTIONS:
    case OMP_SECTION:
    case OMP_SINGLE:
    case OMP_MASTER:
    case OMP_ORDERED:
    case OMP_CRITICAL:
      walk_body (convert_call_expr, info, &OMP_BODY (t));
      break;

    default:
      break;
    }

  return NULL_TREE;
}

/* Walk the nesting tree starting with ROOT, depth first.  Convert all
   trampolines and call expressions.  On the way back up, determine if
   a nested function actually uses its static chain; if not, remember that.  */

static void
convert_all_function_calls (struct nesting_info *root)
{
  do
    {
      if (root->inner)
	convert_all_function_calls (root->inner);

      walk_function (convert_tramp_reference, root);
      walk_function (convert_call_expr, root);

      /* If the function does not use a static chain, then remember that.  */
      if (root->outer && !root->chain_decl && !root->chain_field)
	DECL_NO_STATIC_CHAIN (root->context) = 1;
      else
	gcc_assert (!DECL_NO_STATIC_CHAIN (root->context));

      root = root->next;
    }
  while (root);
}

/* Do "everything else" to clean up or complete state collected by the
   various walking passes -- lay out the types and decls, generate code
   to initialize the frame decl, store critical expressions in the
   struct function for rtl to find.  */

static void
finalize_nesting_tree_1 (struct nesting_info *root)
{
  tree stmt_list = NULL;
  tree context = root->context;
  struct function *sf;

  /* If we created a non-local frame type or decl, we need to lay them
     out at this time.  */
  if (root->frame_type)
    {
      /* In some cases the frame type will trigger the -Wpadded warning.
	 This is not helpful; suppress it. */
      int save_warn_padded = warn_padded;
      warn_padded = 0;
      layout_type (root->frame_type);
      warn_padded = save_warn_padded;
      layout_decl (root->frame_decl, 0);
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
	  x = build_gimple_modify_stmt (y, x);
	  append_to_statement_list (x, &stmt_list);
	}
    }

  /* If a chain_field was created, then it needs to be initialized
     from chain_decl.  */
  if (root->chain_field)
    {
      tree x = build3 (COMPONENT_REF, TREE_TYPE (root->chain_field),
		       root->frame_decl, root->chain_field, NULL_TREE);
      x = build_gimple_modify_stmt (x, get_chain_decl (root));
      append_to_statement_list (x, &stmt_list);
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

	  if (DECL_NO_STATIC_CHAIN (i->context))
	    arg3 = null_pointer_node;
	  else
	    arg3 = build_addr (root->frame_decl, context);

	  arg2 = build_addr (i->context, context);

	  x = build3 (COMPONENT_REF, TREE_TYPE (field),
		      root->frame_decl, field, NULL_TREE);
	  arg1 = build_addr (x, context);

	  x = implicit_built_in_decls[BUILT_IN_INIT_TRAMPOLINE];
	  x = build_call_expr (x, 3, arg1, arg2, arg3);
	  append_to_statement_list (x, &stmt_list);
	}
    }

  /* If we created initialization statements, insert them.  */
  if (stmt_list)
    {
      annotate_all_with_locus (&stmt_list,
			       DECL_SOURCE_LOCATION (context));
      append_to_statement_list (BIND_EXPR_BODY (DECL_SAVED_TREE (context)),
				&stmt_list);
      BIND_EXPR_BODY (DECL_SAVED_TREE (context)) = stmt_list;
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
    declare_vars (root->new_local_var_chain, DECL_SAVED_TREE (root->context),
		  false);
  if (root->debug_var_chain)
    declare_vars (root->debug_var_chain, DECL_SAVED_TREE (root->context),
		  true);

  /* Dump the translated tree function.  */
  dump_function (TDI_nested, root->context);
}

static void
finalize_nesting_tree (struct nesting_info *root)
{
  do
    {
      if (root->inner)
	finalize_nesting_tree (root->inner);
      finalize_nesting_tree_1 (root);
      root = root->next;
    }
  while (root);
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
  do
    {
      if (root->inner)
	unnest_nesting_tree (root->inner);
      unnest_nesting_tree_1 (root);
      root = root->next;
    }
  while (root);
}

/* Free the data structures allocated during this pass.  */

static void
free_nesting_tree (struct nesting_info *root)
{
  struct nesting_info *next;
  do
    {
      if (root->inner)
	free_nesting_tree (root->inner);
      pointer_map_destroy (root->var_map);
      pointer_map_destroy (root->field_map);
      next = root->next;
      free (root);
      root = next;
    }
  while (root);
}

/* Main entry point for this pass.  Process FNDECL and all of its nested
   subroutines and turn them into something less tightly bound.  */

void
lower_nested_functions (tree fndecl)
{
  struct cgraph_node *cgn;
  struct nesting_info *root;

  /* If there are no nested functions, there's nothing to do.  */
  cgn = cgraph_node (fndecl);
  if (!cgn->nested)
    return;

  bitmap_obstack_initialize (&nesting_info_bitmap_obstack);
  root = create_nesting_tree (cgn);
  walk_all_functions (convert_nonlocal_reference, root);
  walk_all_functions (convert_local_reference, root);
  walk_all_functions (convert_nl_goto_reference, root);
  walk_all_functions (convert_nl_goto_receiver, root);
  convert_all_function_calls (root);
  finalize_nesting_tree (root);
  unnest_nesting_tree (root);
  free_nesting_tree (root);
  bitmap_obstack_release (&nesting_info_bitmap_obstack);
}

#include "gt-tree-nested.h"
