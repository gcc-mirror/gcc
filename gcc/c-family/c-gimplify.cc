/* Tree lowering pass.  This pass gimplifies the tree representation built
   by the C-based front ends.  The structure of gimplified, or
   language-independent, trees is dictated by the grammar described in this
   file.
   Copyright (C) 2002-2024 Free Software Foundation, Inc.
   Lowering of expressions contributed by Sebastian Pop <s.pop@laposte.net>
   Re-written to support lowering of whole function trees, documentation
   and miscellaneous cleanups by Diego Novillo <dnovillo@redhat.com>

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
#include "tm.h"
#include "function.h"
#include "basic-block.h"
#include "tree.h"
#include "tree-iterator.h"
#include "predict.h"
#include "gimple.h"
#include "cgraph.h"
#include "c-pretty-print.h"
#include "gimplify.h"
#include "langhooks.h"
#include "dumpfile.h"
#include "c-ubsan.h"
#include "tree-nested.h"
#include "context.h"
#include "tree-pass.h"
#include "internal-fn.h"

/*  The gimplification pass converts the language-dependent trees
    (ld-trees) emitted by the parser into language-independent trees
    (li-trees) that are the target of SSA analysis and transformations.

    Language-independent trees are based on the SIMPLE intermediate
    representation used in the McCAT compiler framework:

    "Designing the McCAT Compiler Based on a Family of Structured
    Intermediate Representations,"
    L. Hendren, C. Donawa, M. Emami, G. Gao, Justiani, and B. Sridharan,
    Proceedings of the 5th International Workshop on Languages and
    Compilers for Parallel Computing, no. 757 in Lecture Notes in
    Computer Science, New Haven, Connecticut, pp. 406-420,
    Springer-Verlag, August 3-5, 1992.

    http://www-acaps.cs.mcgill.ca/info/McCAT/McCAT.html

    Basically, we walk down gimplifying the nodes that we encounter.  As we
    walk back up, we check that they fit our constraints, and copy them
    into temporaries if not.  */

/* Callback for c_genericize.  */

static tree
ubsan_walk_array_refs_r (tree *tp, int *walk_subtrees, void *data)
{
  hash_set<tree> *pset = (hash_set<tree> *) data;

  if (TREE_CODE (*tp) == BIND_EXPR)
    {
      /* Since walk_tree doesn't call the callback function on the decls
	 in BIND_EXPR_VARS, we have to walk them manually, so we can avoid
	 instrumenting DECL_INITIAL of TREE_STATIC vars.  */
      *walk_subtrees = 0;
      for (tree decl = BIND_EXPR_VARS (*tp); decl; decl = DECL_CHAIN (decl))
	{
	  if (TREE_STATIC (decl))
	    continue;
	  walk_tree (&DECL_INITIAL (decl), ubsan_walk_array_refs_r, pset,
		     pset);
	  walk_tree (&DECL_SIZE (decl), ubsan_walk_array_refs_r, pset, pset);
	  walk_tree (&DECL_SIZE_UNIT (decl), ubsan_walk_array_refs_r, pset,
		     pset);
	}
      walk_tree (&BIND_EXPR_BODY (*tp), ubsan_walk_array_refs_r, pset, pset);
    }
  else if (TREE_CODE (*tp) == ADDR_EXPR
	   && TREE_CODE (TREE_OPERAND (*tp, 0)) == ARRAY_REF)
    {
      ubsan_maybe_instrument_array_ref (&TREE_OPERAND (*tp, 0), true);
      /* Make sure ubsan_maybe_instrument_array_ref is not called again
	 on the ARRAY_REF, the above call might not instrument anything
	 as the index might be constant or masked, so ensure it is not
	 walked again and walk its subtrees manually.  */
      tree aref = TREE_OPERAND (*tp, 0);
      pset->add (aref);
      *walk_subtrees = 0;
      walk_tree (&TREE_OPERAND (aref, 0), ubsan_walk_array_refs_r, pset, pset);
      walk_tree (&TREE_OPERAND (aref, 1), ubsan_walk_array_refs_r, pset, pset);
      walk_tree (&TREE_OPERAND (aref, 2), ubsan_walk_array_refs_r, pset, pset);
      walk_tree (&TREE_OPERAND (aref, 3), ubsan_walk_array_refs_r, pset, pset);
    }
  else if (TREE_CODE (*tp) == ARRAY_REF)
    ubsan_maybe_instrument_array_ref (tp, false);
  else if (TREE_CODE (*tp) == MODIFY_EXPR)
    {
      /* Since r7-1900, we gimplify RHS before LHS.  Consider
	   a[b] |= c;
	 wherein we can have a single shared tree a[b] in both LHS and RHS.
	 If we only instrument the LHS and the access is invalid, the program
	 could crash before emitting a UBSan error.  So instrument the RHS
	 first.  */
      *walk_subtrees = 0;
      walk_tree (&TREE_OPERAND (*tp, 1), ubsan_walk_array_refs_r, pset, pset);
      walk_tree (&TREE_OPERAND (*tp, 0), ubsan_walk_array_refs_r, pset, pset);
    }
  return NULL_TREE;
}

/* Gimplification of statement trees.  */

/* Local declarations.  */

enum bc_t { bc_break = 0, bc_continue = 1 };

/* Stack of labels which are targets for "break" or "continue",
   linked through TREE_CHAIN.  */
static tree bc_label[2];

/* Begin a scope which can be exited by a break or continue statement.  BC
   indicates which.

   Just creates a label with location LOCATION and pushes it into the current
   context.  */

static tree
begin_bc_block (enum bc_t bc, location_t location)
{
  tree label = create_artificial_label (location);
  DECL_CHAIN (label) = bc_label[bc];
  bc_label[bc] = label;
  if (bc == bc_break)
    LABEL_DECL_BREAK (label) = true;
  else
    LABEL_DECL_CONTINUE (label) = true;
  return label;
}

/* Finish a scope which can be exited by a break or continue statement.
   LABEL was returned from the most recent call to begin_bc_block.  BLOCK is
   an expression for the contents of the scope.

   If we saw a break (or continue) in the scope, append a LABEL_EXPR to
   BLOCK.  Otherwise, just forget the label.  */

static void
finish_bc_block (tree *block, enum bc_t bc, tree label)
{
  gcc_assert (label == bc_label[bc]);

  if (TREE_USED (label))
    append_to_statement_list (build1 (LABEL_EXPR, void_type_node, label),
			      block);

  bc_label[bc] = DECL_CHAIN (label);
  DECL_CHAIN (label) = NULL_TREE;
}

/* Allow saving and restoring break/continue state.  */

void
save_bc_state (bc_state_t *state)
{
  state->bc_label[bc_break] = bc_label[bc_break];
  state->bc_label[bc_continue] = bc_label[bc_continue];
  bc_label[bc_break] = NULL_TREE;
  bc_label[bc_continue] = NULL_TREE;
}

void
restore_bc_state (bc_state_t *state)
{
  gcc_assert (bc_label[bc_break] == NULL);
  gcc_assert (bc_label[bc_continue] == NULL);
  bc_label[bc_break] = state->bc_label[bc_break];
  bc_label[bc_continue] = state->bc_label[bc_continue];
}

/* Get the LABEL_EXPR to represent a break or continue statement
   in the current block scope.  BC indicates which.  */

static tree
get_bc_label (enum bc_t bc)
{
  tree label = bc_label[bc];
  gcc_assert (label);

  /* Mark the label used for finish_bc_block.  */
  TREE_USED (label) = 1;
  return label;
}

/* Return the location from EXPR, or OR_LOC if the former is unknown.  */

location_t
expr_loc_or_loc (const_tree expr, location_t or_loc)
{
  tree t = CONST_CAST_TREE (expr);
  location_t loc = UNKNOWN_LOCATION;
  if (t)
    loc = EXPR_LOCATION (t);
  if (loc == UNKNOWN_LOCATION)
    loc = or_loc;
  return loc;
}

/* Build a generic representation of one of the C loop forms.  COND is the
   loop condition or NULL_TREE.  BODY is the (possibly compound) statement
   controlled by the loop.  INCR is the increment expression of a for-loop,
   or NULL_TREE.  COND_IS_FIRST indicates whether the condition is
   evaluated before the loop body as in while and for loops, or after the
   loop body as in do-while loops.  */

static void
genericize_c_loop (tree *stmt_p, location_t start_locus, tree cond, tree body,
		   tree incr, bool cond_is_first, int *walk_subtrees,
		   void *data, walk_tree_fn func, walk_tree_lh lh)
{
  tree blab, clab;
  tree entry = NULL, exit = NULL, t;
  tree stmt_list = NULL;
  location_t cond_locus = expr_loc_or_loc (cond, start_locus);
  location_t incr_locus = expr_loc_or_loc (incr, start_locus);

  protected_set_expr_location_if_unset (incr, start_locus);

  walk_tree_1 (&cond, func, data, NULL, lh);
  walk_tree_1 (&incr, func, data, NULL, lh);

  blab = begin_bc_block (bc_break, start_locus);
  clab = begin_bc_block (bc_continue, start_locus);

  walk_tree_1 (&body, func, data, NULL, lh);
  *walk_subtrees = 0;

  /* If condition is zero don't generate a loop construct.  */
  if (cond && integer_zerop (cond))
    {
      if (cond_is_first)
	{
	  t = build1_loc (start_locus, GOTO_EXPR, void_type_node,
			  get_bc_label (bc_break));
	  append_to_statement_list (t, &stmt_list);
	}
    }
  else
    {
      /* Expand to gotos.  */
      tree top = build1 (LABEL_EXPR, void_type_node,
			 create_artificial_label (start_locus));

      /* If we have an exit condition, then we build an IF with gotos either
	 out of the loop, or to the top of it.  If there's no exit condition,
	 then we just build a jump back to the top.  */
      exit = build1 (GOTO_EXPR, void_type_node, LABEL_EXPR_LABEL (top));

      if (cond && !integer_nonzerop (cond))
	{
	  /* Canonicalize the loop condition to the end.  This means
	     generating a branch to the loop condition.  Reuse the
	     continue label, if there is no incr expression.  */
	  if (cond_is_first)
	    {
	      if (incr)
		{
		  entry = build1 (LABEL_EXPR, void_type_node,
				  create_artificial_label (start_locus));
		  t = build1_loc (start_locus, GOTO_EXPR, void_type_node,
				  LABEL_EXPR_LABEL (entry));
		}
	      else
		t = build1_loc (start_locus, GOTO_EXPR, void_type_node,
				get_bc_label (bc_continue));
	      append_to_statement_list (t, &stmt_list);
	    }

	  t = build1 (GOTO_EXPR, void_type_node, get_bc_label (bc_break));
	  exit = fold_build3_loc (cond_locus,
				  COND_EXPR, void_type_node, cond, exit, t);
	}
      else
	{
	  /* For the backward-goto's location of an unconditional loop
	     use the beginning of the body, or, if there is none, the
	     top of the loop.  */
	  location_t loc = expr_loc_or_loc (expr_first (body),
					    start_locus);
	  SET_EXPR_LOCATION (exit, loc);
	}
      append_to_statement_list (top, &stmt_list);
    }

  append_to_statement_list (body, &stmt_list);
  if (c_dialect_cxx ()
      && stmt_list
      && TREE_CODE (stmt_list) == STATEMENT_LIST)
    {
      tree_stmt_iterator tsi = tsi_last (stmt_list);
      if (!tsi_end_p (tsi))
	{
	  tree t = *tsi;
	  while (TREE_CODE (t) == CLEANUP_POINT_EXPR
		 || TREE_CODE (t) == EXPR_STMT
		 || CONVERT_EXPR_CODE_P (TREE_CODE (t)))
	    t = TREE_OPERAND (t, 0);
	  /* For C++, if iteration statement body ends with fallthrough
	     statement, mark it such that we diagnose it even if next
	     statement would be labeled statement with case/default label.  */
	  if (TREE_CODE (t) == CALL_EXPR
	      && !CALL_EXPR_FN (t)
	      && CALL_EXPR_IFN (t) == IFN_FALLTHROUGH)
	    TREE_NOTHROW (t) = 1;
	}
    }
  finish_bc_block (&stmt_list, bc_continue, clab);
  if (incr)
    {
      if (MAY_HAVE_DEBUG_MARKER_STMTS && incr_locus != UNKNOWN_LOCATION)
	{
	  tree d = build0 (DEBUG_BEGIN_STMT, void_type_node);
	  SET_EXPR_LOCATION (d, expr_loc_or_loc (incr, start_locus));
	  append_to_statement_list (d, &stmt_list);
	}
      append_to_statement_list (incr, &stmt_list);
    }
  append_to_statement_list (entry, &stmt_list);

  if (MAY_HAVE_DEBUG_MARKER_STMTS && cond_locus != UNKNOWN_LOCATION)
    {
      tree d = build0 (DEBUG_BEGIN_STMT, void_type_node);
      SET_EXPR_LOCATION (d, cond_locus);
      append_to_statement_list (d, &stmt_list);
    }
  append_to_statement_list (exit, &stmt_list);
  finish_bc_block (&stmt_list, bc_break, blab);
  if (!stmt_list)
    stmt_list = build_empty_stmt (start_locus);

  *stmt_p = stmt_list;
}

/* Genericize a FOR_STMT node *STMT_P.  */

static void
genericize_for_stmt (tree *stmt_p, int *walk_subtrees, void *data,
		     walk_tree_fn func, walk_tree_lh lh)
{
  tree stmt = *stmt_p;
  tree expr = NULL;
  tree loop;
  tree init = FOR_INIT_STMT (stmt);

  if (init)
    {
      walk_tree_1 (&init, func, data, NULL, lh);
      append_to_statement_list (init, &expr);
    }

  genericize_c_loop (&loop, EXPR_LOCATION (stmt), FOR_COND (stmt),
		     FOR_BODY (stmt), FOR_EXPR (stmt), 1, walk_subtrees,
		     data, func, lh);
  append_to_statement_list (loop, &expr);
  if (expr == NULL_TREE)
    expr = loop;
  *stmt_p = expr;
}

/* Genericize a WHILE_STMT node *STMT_P.  */

static void
genericize_while_stmt (tree *stmt_p, int *walk_subtrees, void *data,
		       walk_tree_fn func, walk_tree_lh lh)
{
  tree stmt = *stmt_p;
  genericize_c_loop (stmt_p, EXPR_LOCATION (stmt), WHILE_COND (stmt),
		     WHILE_BODY (stmt), NULL_TREE, 1, walk_subtrees,
		     data, func, lh);
}

/* Genericize a DO_STMT node *STMT_P.  */

static void
genericize_do_stmt (tree *stmt_p, int *walk_subtrees, void *data,
		    walk_tree_fn func, walk_tree_lh lh)
{
  tree stmt = *stmt_p;
  genericize_c_loop (stmt_p, EXPR_LOCATION (stmt), DO_COND (stmt),
		     DO_BODY (stmt), NULL_TREE, 0, walk_subtrees,
		     data, func, lh);
}

/* Genericize a SWITCH_STMT node *STMT_P by turning it into a SWITCH_EXPR.  */

static void
genericize_switch_stmt (tree *stmt_p, int *walk_subtrees, void *data,
			walk_tree_fn func, walk_tree_lh lh)
{
  tree stmt = *stmt_p;
  tree break_block, body, cond, type;
  location_t stmt_locus = EXPR_LOCATION (stmt);

  body = SWITCH_STMT_BODY (stmt);
  if (!body)
    body = build_empty_stmt (stmt_locus);
  cond = SWITCH_STMT_COND (stmt);
  type = SWITCH_STMT_TYPE (stmt);

  walk_tree_1 (&cond, func, data, NULL, lh);

  break_block = begin_bc_block (bc_break, stmt_locus);

  walk_tree_1 (&body, func, data, NULL, lh);
  walk_tree_1 (&type, func, data, NULL, lh);
  *walk_subtrees = 0;

  if (TREE_USED (break_block))
    SWITCH_BREAK_LABEL_P (break_block) = 1;
  finish_bc_block (&body, bc_break, break_block);
  *stmt_p = build2_loc (stmt_locus, SWITCH_EXPR, type, cond, body);
  SWITCH_ALL_CASES_P (*stmt_p) = SWITCH_STMT_ALL_CASES_P (stmt);
  gcc_checking_assert (!SWITCH_STMT_NO_BREAK_P (stmt)
		       || !TREE_USED (break_block));
}

/* Genericize a CONTINUE_STMT node *STMT_P.  */

static void
genericize_continue_stmt (tree *stmt_p)
{
  tree stmt_list = NULL;
  tree pred = build_predict_expr (PRED_CONTINUE, NOT_TAKEN);
  tree label = get_bc_label (bc_continue);
  location_t location = EXPR_LOCATION (*stmt_p);
  tree jump = build1_loc (location, GOTO_EXPR, void_type_node, label);
  append_to_statement_list_force (pred, &stmt_list);
  append_to_statement_list (jump, &stmt_list);
  *stmt_p = stmt_list;
}

/* Genericize a BREAK_STMT node *STMT_P.  */

static void
genericize_break_stmt (tree *stmt_p)
{
  tree label = get_bc_label (bc_break);
  location_t location = EXPR_LOCATION (*stmt_p);
  *stmt_p = build1_loc (location, GOTO_EXPR, void_type_node, label);
}

/* Genericize a OMP_FOR node *STMT_P.  */

static void
genericize_omp_for_stmt (tree *stmt_p, int *walk_subtrees, void *data,
			 walk_tree_fn func, walk_tree_lh lh)
{
  tree stmt = *stmt_p;
  location_t locus = EXPR_LOCATION (stmt);
  tree clab = begin_bc_block (bc_continue, locus);

  walk_tree_1 (&OMP_FOR_BODY (stmt), func, data, NULL, lh);
  if (TREE_CODE (stmt) != OMP_TASKLOOP)
    walk_tree_1 (&OMP_FOR_CLAUSES (stmt), func, data, NULL, lh);
  walk_tree_1 (&OMP_FOR_INIT (stmt), func, data, NULL, lh);
  walk_tree_1 (&OMP_FOR_COND (stmt), func, data, NULL, lh);
  walk_tree_1 (&OMP_FOR_INCR (stmt), func, data, NULL, lh);
  walk_tree_1 (&OMP_FOR_PRE_BODY (stmt), func, data, NULL, lh);
  *walk_subtrees = 0;

  finish_bc_block (&OMP_FOR_BODY (stmt), bc_continue, clab);
}


/* Lower structured control flow tree nodes, such as loops.  The
   STMT_P, WALK_SUBTREES, and DATA arguments are as for the walk_tree_fn
   type.  FUNC and LH are language-specific functions passed to walk_tree_1
   for node visiting and traversal, respectively; they are used to do
   subtree processing in a language-dependent way.  */

tree
c_genericize_control_stmt (tree *stmt_p, int *walk_subtrees, void *data,
			   walk_tree_fn func, walk_tree_lh lh)
{
  tree stmt = *stmt_p;

  switch (TREE_CODE (stmt))
    {
    case FOR_STMT:
      genericize_for_stmt (stmt_p, walk_subtrees, data, func, lh);
      break;

    case WHILE_STMT:
      genericize_while_stmt (stmt_p, walk_subtrees, data, func, lh);
      break;

    case DO_STMT:
      genericize_do_stmt (stmt_p, walk_subtrees, data, func, lh);
      break;

    case SWITCH_STMT:
      genericize_switch_stmt (stmt_p, walk_subtrees, data, func, lh);
      break;

    case CONTINUE_STMT:
      genericize_continue_stmt (stmt_p);
      break;

    case BREAK_STMT:
      genericize_break_stmt (stmt_p);
      break;

    case OMP_FOR:
    case OMP_SIMD:
    case OMP_DISTRIBUTE:
    case OMP_LOOP:
    case OMP_TASKLOOP:
    case OACC_LOOP:
      genericize_omp_for_stmt (stmt_p, walk_subtrees, data, func, lh);
      break;

    case STATEMENT_LIST:
      if (TREE_SIDE_EFFECTS (stmt))
	{
	  tree_stmt_iterator i;
	  int nondebug_stmts = 0;
	  bool clear_side_effects = true;
	  /* Genericization can clear TREE_SIDE_EFFECTS, e.g. when
	     transforming an IF_STMT into COND_EXPR.  If such stmt
	     appears in a STATEMENT_LIST that contains only that
	     stmt and some DEBUG_BEGIN_STMTs, without -g where the
	     STATEMENT_LIST wouldn't be present at all the resulting
	     expression wouldn't have TREE_SIDE_EFFECTS set, so make sure
	     to clear it even on the STATEMENT_LIST in such cases.  */
	  hash_set<tree> *pset = (c_dialect_cxx ()
				  ? nullptr
				  : static_cast<hash_set<tree> *>(data));
	  for (i = tsi_start (stmt); !tsi_end_p (i); tsi_next (&i))
	    {
	      tree t = tsi_stmt (i);
	      if (TREE_CODE (t) != DEBUG_BEGIN_STMT && nondebug_stmts < 2)
		nondebug_stmts++;
	      walk_tree_1 (tsi_stmt_ptr (i), func, data, pset, lh);
	      if (TREE_CODE (t) != DEBUG_BEGIN_STMT
		  && (nondebug_stmts > 1 || TREE_SIDE_EFFECTS (tsi_stmt (i))))
		clear_side_effects = false;
	    }
	  if (clear_side_effects)
	    TREE_SIDE_EFFECTS (stmt) = 0;
	  *walk_subtrees = 0;
	}
      break;

    default:
      break;
    }

  return NULL;
}


/* Wrapper for c_genericize_control_stmt to allow it to be used as a walk_tree
   callback.  This is appropriate for C; C++ calls c_genericize_control_stmt
   directly.  */

static tree
c_genericize_control_r (tree *stmt_p, int *walk_subtrees, void *data)
{
  c_genericize_control_stmt (stmt_p, walk_subtrees, data,
			     c_genericize_control_r, NULL);
  return NULL;
}

/* Convert the tree representation of FNDECL from C frontend trees to
   GENERIC.  */

void
c_genericize (tree fndecl)
{
  dump_file_info *dfi;
  FILE *dump_orig;
  dump_flags_t local_dump_flags;
  struct cgraph_node *cgn;

  if (flag_sanitize & SANITIZE_BOUNDS)
    {
      hash_set<tree> pset;
      walk_tree (&DECL_SAVED_TREE (fndecl), ubsan_walk_array_refs_r, &pset,
		 &pset);
    }

  /* Genericize loops and other structured control constructs.  The C++
     front end has already done this in lang-specific code.  */
  if (!c_dialect_cxx ())
    {
      bc_state_t save_state;
      push_cfun (DECL_STRUCT_FUNCTION (fndecl));
      save_bc_state (&save_state);
      hash_set<tree> pset;
      walk_tree (&DECL_SAVED_TREE (fndecl), c_genericize_control_r, &pset,
		 &pset);
      restore_bc_state (&save_state);
      pop_cfun ();
    }

  if (warn_duplicated_branches)
    walk_tree_without_duplicates (&DECL_SAVED_TREE (fndecl),
				  do_warn_duplicated_branches_r, NULL);

  /* Dump the C-specific tree IR.  */
  dfi = g->get_dumps ()->get_dump_file_info (TDI_original);
  dump_orig = dfi->pstream;
  local_dump_flags = dfi->pflags;
  if (dump_orig)
    {
      fprintf (dump_orig, "\n;; Function %s",
	       lang_hooks.decl_printable_name (fndecl, 2));
      fprintf (dump_orig, " (%s)\n",
	       (!DECL_ASSEMBLER_NAME_SET_P (fndecl) ? "null"
		: IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (fndecl))));
      fprintf (dump_orig, ";; enabled by -%s\n", dump_flag_name (TDI_original));
      fprintf (dump_orig, "\n");

      if (local_dump_flags & TDF_RAW)
	dump_node (DECL_SAVED_TREE (fndecl),
		   TDF_SLIM | local_dump_flags, dump_orig);
      else
	print_c_tree (dump_orig, DECL_SAVED_TREE (fndecl));
      fprintf (dump_orig, "\n");
    }

  /* Dump all nested functions now.  */
  cgn = cgraph_node::get_create (fndecl);
  for (cgn = first_nested_function (cgn);
       cgn; cgn = next_nested_function (cgn))
    c_genericize (cgn->decl);
}

static void
add_block_to_enclosing (tree block)
{
  unsigned i;
  tree enclosing;
  gbind *bind;
  vec<gbind *> stack = gimple_bind_expr_stack ();

  FOR_EACH_VEC_ELT (stack, i, bind)
    if (gimple_bind_block (bind))
      break;

  enclosing = gimple_bind_block (bind);
  BLOCK_SUBBLOCKS (enclosing) = chainon (BLOCK_SUBBLOCKS (enclosing), block);
}

/* Genericize a scope by creating a new BIND_EXPR.
   BLOCK is either a BLOCK representing the scope or a chain of _DECLs.
     In the latter case, we need to create a new BLOCK and add it to the
     BLOCK_SUBBLOCKS of the enclosing block.
   BODY is a chain of C _STMT nodes for the contents of the scope, to be
     genericized.  */

tree
c_build_bind_expr (location_t loc, tree block, tree body)
{
  tree decls, bind;

  if (block == NULL_TREE)
    decls = NULL_TREE;
  else if (TREE_CODE (block) == BLOCK)
    decls = BLOCK_VARS (block);
  else
    {
      decls = block;
      if (DECL_ARTIFICIAL (decls))
	block = NULL_TREE;
      else
	{
	  block = make_node (BLOCK);
	  BLOCK_VARS (block) = decls;
	  add_block_to_enclosing (block);
	}
    }

  if (!body)
    body = build_empty_stmt (loc);
  if (decls || block)
    {
      bind = build3 (BIND_EXPR, void_type_node, decls, body, block);
      TREE_SIDE_EFFECTS (bind) = 1;
      SET_EXPR_LOCATION (bind, loc);
    }
  else
    bind = body;

  return bind;
}

/* Helper for c_gimplify_expr: test if target supports fma-like FN.  */

static bool
fma_supported_p (enum internal_fn fn, tree type)
{
  return direct_internal_fn_supported_p (fn, type, OPTIMIZE_FOR_BOTH);
}

/* Gimplification of expression trees.  */

/* Do C-specific gimplification on *EXPR_P.  PRE_P and POST_P are as in
   gimplify_expr.  */

int
c_gimplify_expr (tree *expr_p, gimple_seq *pre_p ATTRIBUTE_UNUSED,
		 gimple_seq *post_p ATTRIBUTE_UNUSED)
{
  enum tree_code code = TREE_CODE (*expr_p);

  switch (code)
    {
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
      {
	/* We used to convert the right operand of a shift-expression
	   to an integer_type_node in the FEs.  But it is unnecessary
	   and not desirable for diagnostics and sanitizers.  We keep
	   this here to not pessimize the code, but we convert to an
	   unsigned type, because negative shift counts are undefined
	   anyway.
	   We should get rid of this conversion when we have a proper
	   type demotion/promotion pass.  */
	tree *op1_p = &TREE_OPERAND (*expr_p, 1);
	if (!VECTOR_TYPE_P (TREE_TYPE (*op1_p))
	    && !types_compatible_p (TYPE_MAIN_VARIANT (TREE_TYPE (*op1_p)),
				    unsigned_type_node)
	    && !types_compatible_p (TYPE_MAIN_VARIANT (TREE_TYPE (*op1_p)),
				    integer_type_node))
	  /* Make sure to unshare the result, tree sharing is invalid
	     during gimplification.  */
	  *op1_p = unshare_expr (convert (unsigned_type_node, *op1_p));
	break;
      }

    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      {
	tree type = TREE_TYPE (TREE_OPERAND (*expr_p, 0));
	if (INTEGRAL_TYPE_P (type) && c_promoting_integer_type_p (type))
	  {
	    if (!TYPE_OVERFLOW_WRAPS (type))
	      type = unsigned_type_for (type);
	    return gimplify_self_mod_expr (expr_p, pre_p, post_p, 1, type);
	  }
	break;
      }

    case PLUS_EXPR:
    case MINUS_EXPR:
      {
	tree type = TREE_TYPE (*expr_p);
	/* For -ffp-contract=on we need to attempt FMA contraction only
	   during initial gimplification.  Late contraction across statement
	   boundaries would violate language semantics.  */
	if (SCALAR_FLOAT_TYPE_P (type)
	    && flag_fp_contract_mode == FP_CONTRACT_ON
	    && cfun && !(cfun->curr_properties & PROP_gimple_any)
	    && fma_supported_p (IFN_FMA, type))
	  {
	    bool neg_mul = false, neg_add = code == MINUS_EXPR;

	    tree *op0_p = &TREE_OPERAND (*expr_p, 0);
	    tree *op1_p = &TREE_OPERAND (*expr_p, 1);

	    /* Look for ±(x * y) ± z, swapping operands if necessary.  */
	    if (TREE_CODE (*op0_p) == NEGATE_EXPR
		&& TREE_CODE (TREE_OPERAND (*op0_p, 0)) == MULT_EXPR)
	      /* '*EXPR_P' is '-(x * y) ± z'.  This is fine.  */;
	    else if (TREE_CODE (*op0_p) != MULT_EXPR)
	      {
		std::swap (op0_p, op1_p);
		std::swap (neg_mul, neg_add);
	      }
	    if (TREE_CODE (*op0_p) == NEGATE_EXPR)
	      {
		op0_p = &TREE_OPERAND (*op0_p, 0);
		neg_mul = !neg_mul;
	      }
	    if (TREE_CODE (*op0_p) != MULT_EXPR)
	      break;
	    auto_vec<tree, 3> ops (3);
	    ops.quick_push (TREE_OPERAND (*op0_p, 0));
	    ops.quick_push (TREE_OPERAND (*op0_p, 1));
	    ops.quick_push (*op1_p);

	    enum internal_fn ifn = IFN_FMA;
	    if (neg_mul)
	      {
		if (fma_supported_p (IFN_FNMA, type))
		  ifn = IFN_FNMA;
		else
		  ops[0] = build1 (NEGATE_EXPR, type, ops[0]);
	      }
	    if (neg_add)
	      {
		enum internal_fn ifn2 = ifn == IFN_FMA ? IFN_FMS : IFN_FNMS;
		if (fma_supported_p (ifn2, type))
		  ifn = ifn2;
		else
		  ops[2] = build1 (NEGATE_EXPR, type, ops[2]);
	      }
	    /* Avoid gimplify_arg: it emits all side effects into *PRE_P.  */
	    for (auto &&op : ops)
	      if (gimplify_expr (&op, pre_p, post_p, is_gimple_val, fb_rvalue)
		  == GS_ERROR)
		return GS_ERROR;

	    gcall *call = gimple_build_call_internal_vec (ifn, ops);
	    gimple_seq_add_stmt_without_update (pre_p, call);
	    *expr_p = create_tmp_var (type);
	    gimple_call_set_lhs (call, *expr_p);
	    return GS_ALL_DONE;
	  }
	break;
      }

    case CALL_EXPR:
      {
	tree fndecl = get_callee_fndecl (*expr_p);
	if (fndecl
	    && fndecl_built_in_p (fndecl, BUILT_IN_CLZG, BUILT_IN_CTZG)
	    && call_expr_nargs (*expr_p) == 2
	    && TREE_CODE (CALL_EXPR_ARG (*expr_p, 1)) != INTEGER_CST)
	  {
	    tree a = save_expr (CALL_EXPR_ARG (*expr_p, 0));
	    tree c = build_call_expr_loc (EXPR_LOCATION (*expr_p),
					  fndecl, 1, a);
	    *expr_p = build3_loc (EXPR_LOCATION (*expr_p), COND_EXPR,
				  integer_type_node,
				  build2_loc (EXPR_LOCATION (*expr_p),
					      NE_EXPR, boolean_type_node, a,
					      build_zero_cst (TREE_TYPE (a))),
				  c, CALL_EXPR_ARG (*expr_p, 1));
	    return GS_OK;
	  }
	break;
      }

    default:;
    }

  return GS_UNHANDLED;
}
