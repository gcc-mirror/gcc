/* Tree lowering pass.  This pass gimplifies the tree representation built
   by the C-based front ends.  The structure of gimplified, or
   language-independent, trees is dictated by the grammar described in this
   file.
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.
   Lowering of expressions contributed by Sebastian Pop <s.pop@laposte.net>
   Re-written to support lowering of whole function trees, documentation
   and miscellaneous cleanups by Diego Novillo <dnovillo@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "errors.h"
#include "varray.h"
#include "c-tree.h"
#include "c-common.h"
#include "tree-gimple.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "tree-flow.h"
#include "tree-inline.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "flags.h"
#include "rtl.h"
#include "toplev.h"
#include "tree-dump.h"
#include "c-pretty-print.h"
#include "cgraph.h"


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

/* Local declarations.  */

enum bc_t { bc_break = 0, bc_continue = 1 };

static struct c_gimplify_ctx
{
  /* For handling break and continue.  */
  tree current_bc_label;
  tree bc_id[2];
} *ctxp;

static void
push_context (void)
{
  if (ctxp)
    abort ();
  ctxp = (struct c_gimplify_ctx *) xcalloc (1, sizeof (struct c_gimplify_ctx));
  ctxp->bc_id[bc_continue] = get_identifier ("continue");
  ctxp->bc_id[bc_break] = get_identifier ("break");
}

static void
pop_context (void)
{
  if (!ctxp || ctxp->current_bc_label)
    abort ();
  free (ctxp);
  ctxp = NULL;
}

/* Gimplification of statement trees.  */

/* Convert the tree representation of FNDECL from C frontend trees to
   GENERIC.  */

void
c_genericize (tree fndecl)
{
  FILE *dump_file;
  int local_dump_flags;
  struct cgraph_node *cgn;

  /* Dump the C-specific tree IR.  */
  dump_file = dump_begin (TDI_original, &local_dump_flags);
  if (dump_file)
    {
      fprintf (dump_file, "\n;; Function %s",
	       lang_hooks.decl_printable_name (fndecl, 2));
      fprintf (dump_file, " (%s)\n",
	       IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (fndecl)));
      fprintf (dump_file, ";; enabled by -%s\n", dump_flag_name (TDI_original));
      fprintf (dump_file, "\n");

      if (local_dump_flags & TDF_RAW)
	dump_node (DECL_SAVED_TREE (fndecl),
		   TDF_SLIM | local_dump_flags, dump_file);
      else
	print_c_tree (dump_file, DECL_SAVED_TREE (fndecl));
      fprintf (dump_file, "\n");

      dump_end (TDI_original, dump_file);
    }

  /* Go ahead and gimplify for now.  */
  push_context ();
  gimplify_function_tree (fndecl);
  pop_context ();

  /* Dump the genericized tree IR.  */
  dump_function (TDI_generic, fndecl);

  /* Genericize all nested functions now.  We do things in this order so
     that items like VLA sizes are expanded properly in the context of
     the correct function.  */
  cgn = cgraph_node (fndecl);
  for (cgn = cgn->nested; cgn ; cgn = cgn->next_nested)
    c_genericize (cgn->decl);
}

static void
add_block_to_enclosing (tree block)
{
  tree enclosing;

  for (enclosing = gimple_current_bind_expr ();
       enclosing; enclosing = TREE_CHAIN (enclosing))
    if (BIND_EXPR_BLOCK (enclosing))
      break;

  enclosing = BIND_EXPR_BLOCK (enclosing);
  BLOCK_SUBBLOCKS (enclosing) = chainon (BLOCK_SUBBLOCKS (enclosing), block);
}

/* Genericize a scope by creating a new BIND_EXPR.
   BLOCK is either a BLOCK representing the scope or a chain of _DECLs.
     In the latter case, we need to create a new BLOCK and add it to the
     BLOCK_SUBBLOCKS of the enclosing block.
   BODY is a chain of C _STMT nodes for the contents of the scope, to be
     genericized.  */

tree
c_build_bind_expr (tree block, tree body)
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
    body = build_empty_stmt ();
  if (decls || block)
    {
      bind = build (BIND_EXPR, void_type_node, decls, body, block);
      TREE_SIDE_EFFECTS (bind) = 1;
    }
  else
    bind = body;

  return bind;
}

/*  Gimplify an EXPR_STMT node.

    STMT is the statement node.

    PRE_P points to the list where side effects that must happen before
	STMT should be stored.

    POST_P points to the list where side effects that must happen after
	STMT should be stored.  */

static enum gimplify_status
gimplify_expr_stmt (tree *stmt_p)
{
  tree stmt = EXPR_STMT_EXPR (*stmt_p);

  if (stmt == error_mark_node)
    stmt = NULL;

  /* Gimplification of a statement expression will nullify the
     statement if all its side effects are moved to *PRE_P and *POST_P.

     In this case we will not want to emit the gimplified statement.
     However, we may still want to emit a warning, so we do that before
     gimplification.  */
  if (stmt && (extra_warnings || warn_unused_value))
    {
      if (!TREE_SIDE_EFFECTS (stmt))
	{
	  if (!IS_EMPTY_STMT (stmt)
	      && !VOID_TYPE_P (TREE_TYPE (stmt))
	      && !TREE_NO_WARNING (stmt))
	    warning ("statement with no effect");
	}
      else if (warn_unused_value)
	{
	  /* Kludge for 20020220-2.c.  warn_if_unused_value shouldn't use
	     the stmt file location info.  */
	  set_file_and_line_for_stmt (input_location);
	  warn_if_unused_value (stmt);
	}
    }

  if (stmt == NULL_TREE)
    stmt = build_empty_stmt ();

  *stmt_p = stmt;

  return GS_OK;
}

/* Begin a scope which can be exited by a break or continue statement.  BC
   indicates which.

   Just creates a label and pushes it into the current context.  */

static tree
begin_bc_block (enum bc_t bc)
{
  tree label = create_artificial_label ();
  DECL_NAME (label) = ctxp->bc_id[bc];
  TREE_CHAIN (label) = ctxp->current_bc_label;
  ctxp->current_bc_label = label;
  return label;
}

/* Finish a scope which can be exited by a break or continue statement.
   LABEL was returned from the most recent call to begin_bc_block.  BODY is
   an expression for the contents of the scope.

   If we saw a break (or continue) in the scope, append a LABEL_EXPR to
   body.  Otherwise, just forget the label.  */

static tree
finish_bc_block (tree label, tree body)
{
  if (label != ctxp->current_bc_label)
    abort ();

  if (TREE_USED (label))
    {
      tree t, sl = NULL;

      /* Clear the name so flow can delete the label.  */
      DECL_NAME (label) = NULL_TREE;
      t = build1 (LABEL_EXPR, void_type_node, label);

      append_to_statement_list (body, &sl);
      append_to_statement_list (t, &sl);
      body = sl;
    }

  ctxp->current_bc_label = TREE_CHAIN (label);
  TREE_CHAIN (label) = NULL_TREE;
  return body;
}

/* Build a GOTO_EXPR to represent a break or continue statement.  BC
   indicates which.  */

static tree
build_bc_goto (enum bc_t bc)
{
  tree label;
  tree target_name = ctxp->bc_id[bc];

  /* Look for the appropriate type of label.  */
  for (label = ctxp->current_bc_label;
       label;
       label = TREE_CHAIN (label))
    if (DECL_NAME (label) == target_name)
      break;

  if (label == NULL_TREE)
    {
      if (bc == bc_break)
	error ("break statement not within loop or switch");
      else
	error ("continue statement not within loop or switch");

      return NULL_TREE;
    }

  /* Mark the label used for finish_bc_block.  */
  TREE_USED (label) = 1;
  return build1 (GOTO_EXPR, void_type_node, label);
}

/* Build a generic representation of one of the C loop forms.  COND is the
   loop condition or NULL_TREE.  BODY is the (possibly compound) statement
   controlled by the loop.  INCR is the increment expression of a for-loop,
   or NULL_TREE.  COND_IS_FIRST indicates whether the condition is
   evaluated before the loop body as in while and for loops, or after the
   loop body as in do-while loops.  */

static tree
gimplify_c_loop (tree cond, tree body, tree incr, bool cond_is_first)
{
  tree top, entry, exit, cont_block, break_block, stmt_list, t;
  location_t stmt_locus;

  stmt_locus = input_location;

  /* Detect do { ... } while (0) and don't generate loop construct.  */
  if (!cond_is_first && cond && integer_zerop (cond))
    top = cond = NULL;
  else
    {
      /* If we use a LOOP_EXPR here, we have to feed the whole thing
	 back through the main gimplifier to lower it.  Given that we
	 have to gimplify the loop body NOW so that we can resolve
	 break/continue stmts, seems easier to just expand to gotos.  */
      top = build1 (LABEL_EXPR, void_type_node, NULL_TREE);
    }

  break_block = begin_bc_block (bc_break);

  if (top)
    {
      /* If we have an exit condition, then we build an IF with gotos either
	 out of the loop, or to the top of it.  If there's no exit condition,
	 then we just build a jump back to the top.  */
      exit = build_and_jump (&LABEL_EXPR_LABEL (top));
      if (cond)
	{
	  t = build_bc_goto (bc_break);
	  exit = build (COND_EXPR, void_type_node, cond, exit, t);
	  exit = fold (exit);
	  gimplify_stmt (&exit);
	}
    }
  else
    exit = NULL_TREE;

  cont_block = begin_bc_block (bc_continue);

  gimplify_stmt (&body);
  gimplify_stmt (&incr);

  body = finish_bc_block (cont_block, body);

  stmt_list = NULL;

  if (cond_is_first && cond)
    {
      entry = build1 (LABEL_EXPR, void_type_node, NULL_TREE);
      t = build_and_jump (&LABEL_EXPR_LABEL (entry));
      append_to_statement_list (t, &stmt_list);
    }
  else
    entry = NULL_TREE;

  append_to_statement_list (top, &stmt_list);
  append_to_statement_list (body, &stmt_list);
  append_to_statement_list (incr, &stmt_list);
  append_to_statement_list (entry, &stmt_list);
  append_to_statement_list (exit, &stmt_list);

  annotate_all_with_locus (&stmt_list, stmt_locus);

  return finish_bc_block (break_block, stmt_list);
}

/* Gimplify a FOR_STMT node.  Move the stuff in the for-init-stmt into the
   prequeue and hand off to gimplify_c_loop.  */

static enum gimplify_status
gimplify_for_stmt (tree *stmt_p, tree *pre_p)
{
  tree stmt = *stmt_p;

  if (FOR_INIT_STMT (stmt))
    {
      gimplify_stmt (&FOR_INIT_STMT (stmt));
      append_to_statement_list (FOR_INIT_STMT (stmt), pre_p);
    }
  *stmt_p = gimplify_c_loop (FOR_COND (stmt), FOR_BODY (stmt),
			     FOR_EXPR (stmt), 1);

  return GS_ALL_DONE;
}

/* Gimplify a WHILE_STMT node.  */

static enum gimplify_status
gimplify_while_stmt (tree *stmt_p)
{
  tree stmt = *stmt_p;
  *stmt_p = gimplify_c_loop (WHILE_COND (stmt), WHILE_BODY (stmt),
			     NULL_TREE, 1);
  return GS_ALL_DONE;
}

/* Gimplify a DO_STMT node.  */

static enum gimplify_status
gimplify_do_stmt (tree *stmt_p)
{
  tree stmt = *stmt_p;
  *stmt_p = gimplify_c_loop (DO_COND (stmt), DO_BODY (stmt),
			     NULL_TREE, 0);
  return GS_ALL_DONE;
}

/* Genericize a SWITCH_STMT by turning it into a SWITCH_EXPR.  */

static enum gimplify_status
gimplify_switch_stmt (tree *stmt_p)
{
  tree stmt = *stmt_p;
  tree break_block, body;
  location_t stmt_locus = input_location;

  break_block = begin_bc_block (bc_break);

  body = SWITCH_BODY (stmt);
  if (!body)
    body = build_empty_stmt ();

  *stmt_p = build (SWITCH_EXPR, SWITCH_TYPE (stmt), SWITCH_COND (stmt),
		   body, NULL_TREE);
  annotate_with_locus (*stmt_p, stmt_locus);
  gimplify_stmt (stmt_p);

  *stmt_p = finish_bc_block (break_block, *stmt_p);
  return GS_ALL_DONE;
}

/* Genericize a RETURN_STMT by turning it into a RETURN_EXPR.  */

static enum gimplify_status
gimplify_return_stmt (tree *stmt_p)
{
  tree expr = RETURN_STMT_EXPR (*stmt_p);
  expr = build1 (RETURN_EXPR, void_type_node, expr);
  *stmt_p = expr;
  return GS_OK;
}

/* Gimplifies a DECL_STMT node *STMT_P by making any necessary allocation
   and initialization explicit.  */

static enum gimplify_status
gimplify_decl_stmt (tree *stmt_p)
{
  tree stmt = *stmt_p;
  tree decl = DECL_STMT_DECL (stmt);
  tree pre = NULL_TREE;
  tree post = NULL_TREE;

  if (TREE_TYPE (decl) == error_mark_node)
    {
      *stmt_p = NULL;
      return GS_ERROR;
    }
    
  if (TREE_CODE (decl) == TYPE_DECL)
    {
      tree type = TREE_TYPE (decl);
      if (TYPE_SIZE_UNIT (type)
          && !TREE_CONSTANT (TYPE_SIZE_UNIT (type)))
        {
          /* This is a variable-sized array type.  Simplify its size.  */
          tree temp = TYPE_SIZE_UNIT (type);
          gimplify_expr (&temp, &pre, &post, is_gimple_val, fb_rvalue);
        }
    }

  if (TREE_CODE (decl) == VAR_DECL && !DECL_EXTERNAL (decl))
    {
      tree init = DECL_INITIAL (decl);

      if (!TREE_CONSTANT (DECL_SIZE (decl)))
	{
	  tree pt_type = build_pointer_type (TREE_TYPE (decl));
	  tree alloc, size;

	  /* This is a variable-sized decl.  Simplify its size and mark it
	     for deferred expansion.  Note that mudflap depends on the format
	     of the emitted code: see mx_register_decls().  */

	  size = get_initialized_tmp_var (DECL_SIZE_UNIT (decl), &pre, &post);
	  DECL_DEFER_OUTPUT (decl) = 1;
	  alloc = build_function_call_expr
	    (implicit_built_in_decls[BUILT_IN_STACK_ALLOC],
	     tree_cons (NULL_TREE,
			build1 (ADDR_EXPR, pt_type, decl),
			tree_cons (NULL_TREE, size, NULL_TREE)));
	  append_to_compound_expr (alloc, &pre);
	}

      if (init && init != error_mark_node)
	{
	  if (!TREE_STATIC (decl))
	    {
              /* Do not warn about int x = x; as it is a GCC extension
                 to turn off this warning but only if warn_init_self
		 is zero.  */
              if (init == decl && !warn_init_self)
                TREE_NO_WARNING (decl) = 1;
              
	      DECL_INITIAL (decl) = NULL_TREE;
	      init = build (MODIFY_EXPR, void_type_node, decl, init);
	      append_to_compound_expr (init, &pre);
	    }
	  else
	    {
	      /* We must still examine initializers for static variables
		 as they may contain a label address.  */
	      walk_tree (&init, force_labels_r, NULL, NULL);
	    }
	}

      /* This decl isn't mentioned in the enclosing block, so add it to the
	 list of temps.  FIXME it seems a bit of a kludge to say that
	 anonymous artificial vars aren't pushed, but everything else is.  */
      if (DECL_ARTIFICIAL (decl) && DECL_NAME (decl) == NULL_TREE)
	gimple_add_tmp_var (decl);
    }

  append_to_compound_expr (post, &pre);
  *stmt_p = pre;
  return GS_OK;
}

/* Gimplification of expression trees.  */

/* Gimplify a C99 compound literal expression.  This just means adding the
   DECL_STMT before the current EXPR_STMT and using its anonymous decl
   instead.  */

static enum gimplify_status
gimplify_compound_literal_expr (tree *expr_p)
{
  tree decl_s = COMPOUND_LITERAL_EXPR_DECL_STMT (*expr_p);
  tree decl = DECL_STMT_DECL (decl_s);

  /* This decl isn't mentioned in the enclosing block, so add it to the
     list of temps.  FIXME it seems a bit of a kludge to say that
     anonymous artificial vars aren't pushed, but everything else is.  */
  if (DECL_NAME (decl) == NULL_TREE)
    gimple_add_tmp_var (decl);

  gimplify_decl_stmt (&decl_s);
  *expr_p = decl_s ? decl_s : decl;
  return GS_OK;
}

/* Do C-specific gimplification.  Args are as for gimplify_expr.  */

int
c_gimplify_expr (tree *expr_p, tree *pre_p, tree *post_p ATTRIBUTE_UNUSED)
{
  enum tree_code code = TREE_CODE (*expr_p);

  switch (code)
    {
    case COMPOUND_LITERAL_EXPR:
      return gimplify_compound_literal_expr (expr_p);

    case FOR_STMT:
      return gimplify_for_stmt (expr_p, pre_p);

    case WHILE_STMT:
      return gimplify_while_stmt (expr_p);

    case DO_STMT:
      return gimplify_do_stmt (expr_p);

    case SWITCH_STMT:
      return gimplify_switch_stmt (expr_p);

    case EXPR_STMT:
      return gimplify_expr_stmt (expr_p);

    case RETURN_STMT:
      return gimplify_return_stmt (expr_p);

    case DECL_STMT:
      return gimplify_decl_stmt (expr_p);

    case CONTINUE_STMT:
      *expr_p = build_bc_goto (bc_continue);
      return GS_ALL_DONE;

    case BREAK_STMT:
      *expr_p = build_bc_goto (bc_break);
      return GS_ALL_DONE;

    default:
      return GS_UNHANDLED;
    }
}
