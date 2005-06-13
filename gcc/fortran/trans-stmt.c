/* Statement translation -- generate GCC trees from gfc_code.
   Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>
   and Steven Bosscher <s.bosscher@student.tudelft.nl>

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
#include "tree.h"
#include "tree-gimple.h"
#include "ggc.h"
#include "toplev.h"
#include "real.h"
#include "gfortran.h"
#include "trans.h"
#include "trans-stmt.h"
#include "trans-types.h"
#include "trans-array.h"
#include "trans-const.h"
#include "arith.h"

typedef struct iter_info
{
  tree var;
  tree start;
  tree end;
  tree step;
  struct iter_info *next;
}
iter_info;

typedef  struct temporary_list
{
  tree temporary;
  struct temporary_list *next;
}
temporary_list;

typedef struct forall_info
{
  iter_info *this_loop;
  tree mask;
  tree pmask;
  tree maskindex;
  int nvar;
  tree size;
  struct forall_info  *outer;
  struct forall_info  *next_nest;
}
forall_info;

static void gfc_trans_where_2 (gfc_code *, tree, tree, forall_info *,
                               stmtblock_t *, temporary_list **temp);

/* Translate a F95 label number to a LABEL_EXPR.  */

tree
gfc_trans_label_here (gfc_code * code)
{
  return build1_v (LABEL_EXPR, gfc_get_label_decl (code->here));
}


/* Given a variable expression which has been ASSIGNed to, find the decl
   containing the auxiliary variables.  For variables in common blocks this
   is a field_decl.  */

void
gfc_conv_label_variable (gfc_se * se, gfc_expr * expr)
{
  gcc_assert (expr->symtree->n.sym->attr.assign == 1);
  gfc_conv_expr (se, expr);
  /* Deals with variable in common block. Get the field declaration.  */
  if (TREE_CODE (se->expr) == COMPONENT_REF)
    se->expr = TREE_OPERAND (se->expr, 1);
}

/* Translate a label assignment statement.  */

tree
gfc_trans_label_assign (gfc_code * code)
{
  tree label_tree;
  gfc_se se;
  tree len;
  tree addr;
  tree len_tree;
  char *label_str;
  int label_len;

  /* Start a new block.  */
  gfc_init_se (&se, NULL);
  gfc_start_block (&se.pre);
  gfc_conv_label_variable (&se, code->expr);

  len = GFC_DECL_STRING_LEN (se.expr);
  addr = GFC_DECL_ASSIGN_ADDR (se.expr);

  label_tree = gfc_get_label_decl (code->label);

  if (code->label->defined == ST_LABEL_TARGET)
    {
      label_tree = gfc_build_addr_expr (pvoid_type_node, label_tree);
      len_tree = integer_minus_one_node;
    }
  else
    {
      label_str = code->label->format->value.character.string;
      label_len = code->label->format->value.character.length;
      len_tree = build_int_cst (NULL_TREE, label_len);
      label_tree = gfc_build_string_const (label_len + 1, label_str);
      label_tree = gfc_build_addr_expr (pvoid_type_node, label_tree);
    }

  gfc_add_modify_expr (&se.pre, len, len_tree);
  gfc_add_modify_expr (&se.pre, addr, label_tree);

  return gfc_finish_block (&se.pre);
}

/* Translate a GOTO statement.  */

tree
gfc_trans_goto (gfc_code * code)
{
  tree assigned_goto;
  tree target;
  tree tmp;
  tree assign_error;
  tree range_error;
  gfc_se se;


  if (code->label != NULL)
    return build1_v (GOTO_EXPR, gfc_get_label_decl (code->label));

  /* ASSIGNED GOTO.  */
  gfc_init_se (&se, NULL);
  gfc_start_block (&se.pre);
  gfc_conv_label_variable (&se, code->expr);
  assign_error =
    gfc_build_cstring_const ("Assigned label is not a target label");
  tmp = GFC_DECL_STRING_LEN (se.expr);
  tmp = build2 (NE_EXPR, boolean_type_node, tmp, integer_minus_one_node);
  gfc_trans_runtime_check (tmp, assign_error, &se.pre);

  assigned_goto = GFC_DECL_ASSIGN_ADDR (se.expr);
  target = build1 (GOTO_EXPR, void_type_node, assigned_goto);

  code = code->block;
  if (code == NULL)
    {
      gfc_add_expr_to_block (&se.pre, target);
      return gfc_finish_block (&se.pre);
    }

  /* Check the label list.  */
  range_error = gfc_build_cstring_const ("Assigned label is not in the list");

  do
    {
      tmp = gfc_get_label_decl (code->label);
      tmp = gfc_build_addr_expr (pvoid_type_node, tmp);
      tmp = build2 (EQ_EXPR, boolean_type_node, tmp, assigned_goto);
      tmp = build3_v (COND_EXPR, tmp, target, build_empty_stmt ());
      gfc_add_expr_to_block (&se.pre, tmp);
      code = code->block;
    }
  while (code != NULL);
  gfc_trans_runtime_check (boolean_true_node, range_error, &se.pre);
  return gfc_finish_block (&se.pre); 
}


/* Translate an ENTRY statement.  Just adds a label for this entry point.  */
tree
gfc_trans_entry (gfc_code * code)
{
  return build1_v (LABEL_EXPR, code->ext.entry->label);
}


/* Translate the CALL statement.  Builds a call to an F95 subroutine.  */

tree
gfc_trans_call (gfc_code * code)
{
  gfc_se se;
  int has_alternate_specifier;

  /* A CALL starts a new block because the actual arguments may have to
     be evaluated first.  */
  gfc_init_se (&se, NULL);
  gfc_start_block (&se.pre);

  gcc_assert (code->resolved_sym);

  /* Translate the call.  */
  has_alternate_specifier
    = gfc_conv_function_call (&se, code->resolved_sym, code->ext.actual);

  /* A subroutine without side-effect, by definition, does nothing!  */
  TREE_SIDE_EFFECTS (se.expr) = 1;

  /* Chain the pieces together and return the block.  */
  if (has_alternate_specifier)
    {
      gfc_code *select_code;
      gfc_symbol *sym;
      select_code = code->next;
      gcc_assert(select_code->op == EXEC_SELECT);
      sym = select_code->expr->symtree->n.sym;
      se.expr = convert (gfc_typenode_for_spec (&sym->ts), se.expr);
      gfc_add_modify_expr (&se.pre, sym->backend_decl, se.expr);
    }
  else
    gfc_add_expr_to_block (&se.pre, se.expr);

  gfc_add_block_to_block (&se.pre, &se.post);
  return gfc_finish_block (&se.pre);
}


/* Translate the RETURN statement.  */

tree
gfc_trans_return (gfc_code * code ATTRIBUTE_UNUSED)
{
  if (code->expr)
    {
      gfc_se se;
      tree tmp;
      tree result;

      /* if code->expr is not NULL, this return statement must appear
         in a subroutine and current_fake_result_decl has already
	 been generated.  */

      result = gfc_get_fake_result_decl (NULL);
      if (!result)
        {
          gfc_warning ("An alternate return at %L without a * dummy argument",
                        &code->expr->where);
          return build1_v (GOTO_EXPR, gfc_get_return_label ());
        }

      /* Start a new block for this statement.  */
      gfc_init_se (&se, NULL);
      gfc_start_block (&se.pre);

      gfc_conv_expr (&se, code->expr);

      tmp = build2 (MODIFY_EXPR, TREE_TYPE (result), result, se.expr);
      gfc_add_expr_to_block (&se.pre, tmp);

      tmp = build1_v (GOTO_EXPR, gfc_get_return_label ());
      gfc_add_expr_to_block (&se.pre, tmp);
      gfc_add_block_to_block (&se.pre, &se.post);
      return gfc_finish_block (&se.pre);
    }
  else
    return build1_v (GOTO_EXPR, gfc_get_return_label ());
}


/* Translate the PAUSE statement.  We have to translate this statement
   to a runtime library call.  */

tree
gfc_trans_pause (gfc_code * code)
{
  tree gfc_int4_type_node = gfc_get_int_type (4);
  gfc_se se;
  tree args;
  tree tmp;
  tree fndecl;

  /* Start a new block for this statement.  */
  gfc_init_se (&se, NULL);
  gfc_start_block (&se.pre);


  if (code->expr == NULL)
    {
      tmp = build_int_cst (gfc_int4_type_node, code->ext.stop_code);
      args = gfc_chainon_list (NULL_TREE, tmp);
      fndecl = gfor_fndecl_pause_numeric;
    }
  else
    {
      gfc_conv_expr_reference (&se, code->expr);
      args = gfc_chainon_list (NULL_TREE, se.expr);
      args = gfc_chainon_list (args, se.string_length);
      fndecl = gfor_fndecl_pause_string;
    }

  tmp = gfc_build_function_call (fndecl, args);
  gfc_add_expr_to_block (&se.pre, tmp);

  gfc_add_block_to_block (&se.pre, &se.post);

  return gfc_finish_block (&se.pre);
}


/* Translate the STOP statement.  We have to translate this statement
   to a runtime library call.  */

tree
gfc_trans_stop (gfc_code * code)
{
  tree gfc_int4_type_node = gfc_get_int_type (4);
  gfc_se se;
  tree args;
  tree tmp;
  tree fndecl;

  /* Start a new block for this statement.  */
  gfc_init_se (&se, NULL);
  gfc_start_block (&se.pre);


  if (code->expr == NULL)
    {
      tmp = build_int_cst (gfc_int4_type_node, code->ext.stop_code);
      args = gfc_chainon_list (NULL_TREE, tmp);
      fndecl = gfor_fndecl_stop_numeric;
    }
  else
    {
      gfc_conv_expr_reference (&se, code->expr);
      args = gfc_chainon_list (NULL_TREE, se.expr);
      args = gfc_chainon_list (args, se.string_length);
      fndecl = gfor_fndecl_stop_string;
    }

  tmp = gfc_build_function_call (fndecl, args);
  gfc_add_expr_to_block (&se.pre, tmp);

  gfc_add_block_to_block (&se.pre, &se.post);

  return gfc_finish_block (&se.pre);
}


/* Generate GENERIC for the IF construct. This function also deals with
   the simple IF statement, because the front end translates the IF
   statement into an IF construct.

   We translate:

        IF (cond) THEN
           then_clause
        ELSEIF (cond2)
           elseif_clause
        ELSE
           else_clause
        ENDIF

   into:

        pre_cond_s;
        if (cond_s)
          {
            then_clause;
          }
        else
          {
            pre_cond_s
            if (cond_s)
              {
                elseif_clause
              }
            else
              {
                else_clause;
              }
          }

   where COND_S is the simplified version of the predicate. PRE_COND_S
   are the pre side-effects produced by the translation of the
   conditional.
   We need to build the chain recursively otherwise we run into
   problems with folding incomplete statements.  */

static tree
gfc_trans_if_1 (gfc_code * code)
{
  gfc_se if_se;
  tree stmt, elsestmt;

  /* Check for an unconditional ELSE clause.  */
  if (!code->expr)
    return gfc_trans_code (code->next);

  /* Initialize a statement builder for each block. Puts in NULL_TREEs.  */
  gfc_init_se (&if_se, NULL);
  gfc_start_block (&if_se.pre);

  /* Calculate the IF condition expression.  */
  gfc_conv_expr_val (&if_se, code->expr);

  /* Translate the THEN clause.  */
  stmt = gfc_trans_code (code->next);

  /* Translate the ELSE clause.  */
  if (code->block)
    elsestmt = gfc_trans_if_1 (code->block);
  else
    elsestmt = build_empty_stmt ();

  /* Build the condition expression and add it to the condition block.  */
  stmt = build3_v (COND_EXPR, if_se.expr, stmt, elsestmt);
  
  gfc_add_expr_to_block (&if_se.pre, stmt);

  /* Finish off this statement.  */
  return gfc_finish_block (&if_se.pre);
}

tree
gfc_trans_if (gfc_code * code)
{
  /* Ignore the top EXEC_IF, it only announces an IF construct. The
     actual code we must translate is in code->block.  */

  return gfc_trans_if_1 (code->block);
}


/* Translage an arithmetic IF expression.

   IF (cond) label1, label2, label3 translates to

    if (cond <= 0)
      {
        if (cond < 0)
          goto label1;
        else // cond == 0
          goto label2;
      }
    else // cond > 0
      goto label3;
*/

tree
gfc_trans_arithmetic_if (gfc_code * code)
{
  gfc_se se;
  tree tmp;
  tree branch1;
  tree branch2;
  tree zero;

  /* Start a new block.  */
  gfc_init_se (&se, NULL);
  gfc_start_block (&se.pre);

  /* Pre-evaluate COND.  */
  gfc_conv_expr_val (&se, code->expr);

  /* Build something to compare with.  */
  zero = gfc_build_const (TREE_TYPE (se.expr), integer_zero_node);

  /* If (cond < 0) take branch1 else take branch2.
     First build jumps to the COND .LT. 0 and the COND .EQ. 0 cases.  */
  branch1 = build1_v (GOTO_EXPR, gfc_get_label_decl (code->label));
  branch2 = build1_v (GOTO_EXPR, gfc_get_label_decl (code->label2));

  tmp = build2 (LT_EXPR, boolean_type_node, se.expr, zero);
  branch1 = build3_v (COND_EXPR, tmp, branch1, branch2);

  /* if (cond <= 0) take branch1 else take branch2.  */
  branch2 = build1_v (GOTO_EXPR, gfc_get_label_decl (code->label3));
  tmp = build2 (LE_EXPR, boolean_type_node, se.expr, zero);
  branch1 = build3_v (COND_EXPR, tmp, branch1, branch2);

  /* Append the COND_EXPR to the evaluation of COND, and return.  */
  gfc_add_expr_to_block (&se.pre, branch1);
  return gfc_finish_block (&se.pre);
}


/* Translate the simple DO construct.  This is where the loop variable has
   integer type and step +-1.  We can't use this in the general case
   because integer overflow and floating point errors could give incorrect
   results.
   We translate a do loop from:

   DO dovar = from, to, step
      body
   END DO

   to:

   [Evaluate loop bounds and step]
   dovar = from;
   if ((step > 0) ? (dovar <= to) : (dovar => to))
    {
      for (;;)
        {
	  body;
   cycle_label:
	  cond = (dovar == to);
	  dovar += step;
	  if (cond) goto end_label;
	}
      }
   end_label:

   This helps the optimizers by avoiding the extra induction variable
   used in the general case.  */

static tree
gfc_trans_simple_do (gfc_code * code, stmtblock_t *pblock, tree dovar,
		     tree from, tree to, tree step)
{
  stmtblock_t body;
  tree type;
  tree cond;
  tree tmp;
  tree cycle_label;
  tree exit_label;
  
  type = TREE_TYPE (dovar);

  /* Initialize the DO variable: dovar = from.  */
  gfc_add_modify_expr (pblock, dovar, from);

  /* Cycle and exit statements are implemented with gotos.  */
  cycle_label = gfc_build_label_decl (NULL_TREE);
  exit_label = gfc_build_label_decl (NULL_TREE);

  /* Put the labels where they can be found later. See gfc_trans_do().  */
  code->block->backend_decl = tree_cons (cycle_label, exit_label, NULL);

  /* Loop body.  */
  gfc_start_block (&body);

  /* Main loop body.  */
  tmp = gfc_trans_code (code->block->next);
  gfc_add_expr_to_block (&body, tmp);

  /* Label for cycle statements (if needed).  */
  if (TREE_USED (cycle_label))
    {
      tmp = build1_v (LABEL_EXPR, cycle_label);
      gfc_add_expr_to_block (&body, tmp);
    }

  /* Evaluate the loop condition.  */
  cond = build2 (EQ_EXPR, boolean_type_node, dovar, to);
  cond = gfc_evaluate_now (cond, &body);

  /* Increment the loop variable.  */
  tmp = build2 (PLUS_EXPR, type, dovar, step);
  gfc_add_modify_expr (&body, dovar, tmp);

  /* The loop exit.  */
  tmp = build1_v (GOTO_EXPR, exit_label);
  TREE_USED (exit_label) = 1;
  tmp = build3_v (COND_EXPR, cond, tmp, build_empty_stmt ());
  gfc_add_expr_to_block (&body, tmp);

  /* Finish the loop body.  */
  tmp = gfc_finish_block (&body);
  tmp = build1_v (LOOP_EXPR, tmp);

  /* Only execute the loop if the number of iterations is positive.  */
  if (tree_int_cst_sgn (step) > 0)
    cond = fold_build2 (LE_EXPR, boolean_type_node, dovar, to);
  else
    cond = fold_build2 (GE_EXPR, boolean_type_node, dovar, to);
  tmp = build3_v (COND_EXPR, cond, tmp, build_empty_stmt ());
  gfc_add_expr_to_block (pblock, tmp);

  /* Add the exit label.  */
  tmp = build1_v (LABEL_EXPR, exit_label);
  gfc_add_expr_to_block (pblock, tmp);

  return gfc_finish_block (pblock);
}

/* Translate the DO construct.  This obviously is one of the most
   important ones to get right with any compiler, but especially
   so for Fortran.

   We special case some loop forms as described in gfc_trans_simple_do.
   For other cases we implement them with a separate loop count,
   as described in the standard.

   We translate a do loop from:

   DO dovar = from, to, step
      body
   END DO

   to:

   [evaluate loop bounds and step]
   count = to + step - from;
   dovar = from;
   for (;;)
     {
       body;
cycle_label:
       dovar += step
       count--;
       if (count <=0) goto exit_label;
     }
exit_label:

   TODO: Large loop counts
   The code above assumes the loop count fits into a signed integer kind,
   i.e. Does not work for loop counts > 2^31 for integer(kind=4) variables
   We must support the full range.  */

tree
gfc_trans_do (gfc_code * code)
{
  gfc_se se;
  tree dovar;
  tree from;
  tree to;
  tree step;
  tree count;
  tree count_one;
  tree type;
  tree cond;
  tree cycle_label;
  tree exit_label;
  tree tmp;
  stmtblock_t block;
  stmtblock_t body;

  gfc_start_block (&block);

  /* Evaluate all the expressions in the iterator.  */
  gfc_init_se (&se, NULL);
  gfc_conv_expr_lhs (&se, code->ext.iterator->var);
  gfc_add_block_to_block (&block, &se.pre);
  dovar = se.expr;
  type = TREE_TYPE (dovar);

  gfc_init_se (&se, NULL);
  gfc_conv_expr_val (&se, code->ext.iterator->start);
  gfc_add_block_to_block (&block, &se.pre);
  from = gfc_evaluate_now (se.expr, &block);

  gfc_init_se (&se, NULL);
  gfc_conv_expr_val (&se, code->ext.iterator->end);
  gfc_add_block_to_block (&block, &se.pre);
  to = gfc_evaluate_now (se.expr, &block);

  gfc_init_se (&se, NULL);
  gfc_conv_expr_val (&se, code->ext.iterator->step);
  gfc_add_block_to_block (&block, &se.pre);
  step = gfc_evaluate_now (se.expr, &block);

  /* Special case simple loops.  */
  if (TREE_CODE (type) == INTEGER_TYPE
      && (integer_onep (step)
	|| tree_int_cst_equal (step, integer_minus_one_node)))
    return gfc_trans_simple_do (code, &block, dovar, from, to, step);
      
  /* Initialize loop count. This code is executed before we enter the
     loop body. We generate: count = (to + step - from) / step.  */

  tmp = fold_build2 (MINUS_EXPR, type, step, from);
  tmp = fold_build2 (PLUS_EXPR, type, to, tmp);
  if (TREE_CODE (type) == INTEGER_TYPE)
    {
      tmp = fold_build2 (TRUNC_DIV_EXPR, type, tmp, step);
      count = gfc_create_var (type, "count");
    }
  else
    {
      /* TODO: We could use the same width as the real type.
	 This would probably cause more problems that it solves
	 when we implement "long double" types.  */
      tmp = fold_build2 (RDIV_EXPR, type, tmp, step);
      tmp = fold_build1 (FIX_TRUNC_EXPR, gfc_array_index_type, tmp);
      count = gfc_create_var (gfc_array_index_type, "count");
    }
  gfc_add_modify_expr (&block, count, tmp);

  count_one = convert (TREE_TYPE (count), integer_one_node);

  /* Initialize the DO variable: dovar = from.  */
  gfc_add_modify_expr (&block, dovar, from);

  /* Loop body.  */
  gfc_start_block (&body);

  /* Cycle and exit statements are implemented with gotos.  */
  cycle_label = gfc_build_label_decl (NULL_TREE);
  exit_label = gfc_build_label_decl (NULL_TREE);

  /* Start with the loop condition.  Loop until count <= 0.  */
  cond = build2 (LE_EXPR, boolean_type_node, count,
		convert (TREE_TYPE (count), integer_zero_node));
  tmp = build1_v (GOTO_EXPR, exit_label);
  TREE_USED (exit_label) = 1;
  tmp = build3_v (COND_EXPR, cond, tmp, build_empty_stmt ());
  gfc_add_expr_to_block (&body, tmp);

  /* Put these labels where they can be found later. We put the
     labels in a TREE_LIST node (because TREE_CHAIN is already
     used). cycle_label goes in TREE_PURPOSE (backend_decl), exit
     label in TREE_VALUE (backend_decl).  */

  code->block->backend_decl = tree_cons (cycle_label, exit_label, NULL);

  /* Main loop body.  */
  tmp = gfc_trans_code (code->block->next);
  gfc_add_expr_to_block (&body, tmp);

  /* Label for cycle statements (if needed).  */
  if (TREE_USED (cycle_label))
    {
      tmp = build1_v (LABEL_EXPR, cycle_label);
      gfc_add_expr_to_block (&body, tmp);
    }

  /* Increment the loop variable.  */
  tmp = build2 (PLUS_EXPR, type, dovar, step);
  gfc_add_modify_expr (&body, dovar, tmp);

  /* Decrement the loop count.  */
  tmp = build2 (MINUS_EXPR, TREE_TYPE (count), count, count_one);
  gfc_add_modify_expr (&body, count, tmp);

  /* End of loop body.  */
  tmp = gfc_finish_block (&body);

  /* The for loop itself.  */
  tmp = build1_v (LOOP_EXPR, tmp);
  gfc_add_expr_to_block (&block, tmp);

  /* Add the exit label.  */
  tmp = build1_v (LABEL_EXPR, exit_label);
  gfc_add_expr_to_block (&block, tmp);

  return gfc_finish_block (&block);
}


/* Translate the DO WHILE construct.

   We translate

   DO WHILE (cond)
      body
   END DO

   to:

   for ( ; ; )
     {
       pre_cond;
       if (! cond) goto exit_label;
       body;
cycle_label:
     }
exit_label:

   Because the evaluation of the exit condition `cond' may have side
   effects, we can't do much for empty loop bodies.  The backend optimizers
   should be smart enough to eliminate any dead loops.  */

tree
gfc_trans_do_while (gfc_code * code)
{
  gfc_se cond;
  tree tmp;
  tree cycle_label;
  tree exit_label;
  stmtblock_t block;

  /* Everything we build here is part of the loop body.  */
  gfc_start_block (&block);

  /* Cycle and exit statements are implemented with gotos.  */
  cycle_label = gfc_build_label_decl (NULL_TREE);
  exit_label = gfc_build_label_decl (NULL_TREE);

  /* Put the labels where they can be found later. See gfc_trans_do().  */
  code->block->backend_decl = tree_cons (cycle_label, exit_label, NULL);

  /* Create a GIMPLE version of the exit condition.  */
  gfc_init_se (&cond, NULL);
  gfc_conv_expr_val (&cond, code->expr);
  gfc_add_block_to_block (&block, &cond.pre);
  cond.expr = fold_build1 (TRUTH_NOT_EXPR, boolean_type_node, cond.expr);

  /* Build "IF (! cond) GOTO exit_label".  */
  tmp = build1_v (GOTO_EXPR, exit_label);
  TREE_USED (exit_label) = 1;
  tmp = build3_v (COND_EXPR, cond.expr, tmp, build_empty_stmt ());
  gfc_add_expr_to_block (&block, tmp);

  /* The main body of the loop.  */
  tmp = gfc_trans_code (code->block->next);
  gfc_add_expr_to_block (&block, tmp);

  /* Label for cycle statements (if needed).  */
  if (TREE_USED (cycle_label))
    {
      tmp = build1_v (LABEL_EXPR, cycle_label);
      gfc_add_expr_to_block (&block, tmp);
    }

  /* End of loop body.  */
  tmp = gfc_finish_block (&block);

  gfc_init_block (&block);
  /* Build the loop.  */
  tmp = build1_v (LOOP_EXPR, tmp);
  gfc_add_expr_to_block (&block, tmp);

  /* Add the exit label.  */
  tmp = build1_v (LABEL_EXPR, exit_label);
  gfc_add_expr_to_block (&block, tmp);

  return gfc_finish_block (&block);
}


/* Translate the SELECT CASE construct for INTEGER case expressions,
   without killing all potential optimizations.  The problem is that
   Fortran allows unbounded cases, but the back-end does not, so we
   need to intercept those before we enter the equivalent SWITCH_EXPR
   we can build.

   For example, we translate this,

   SELECT CASE (expr)
      CASE (:100,101,105:115)
	 block_1
      CASE (190:199,200:)
	 block_2
      CASE (300)
	 block_3
      CASE DEFAULT
	 block_4
   END SELECT

   to the GENERIC equivalent,

     switch (expr)
       {
	 case (minimum value for typeof(expr) ... 100:
	 case 101:
	 case 105 ... 114:
	   block1:
	   goto end_label;

	 case 200 ... (maximum value for typeof(expr):
	 case 190 ... 199:
	   block2;
	   goto end_label;

	 case 300:
	   block_3;
	   goto end_label;

	 default:
	   block_4;
	   goto end_label;
       }

     end_label:  */

static tree
gfc_trans_integer_select (gfc_code * code)
{
  gfc_code *c;
  gfc_case *cp;
  tree end_label;
  tree tmp;
  gfc_se se;
  stmtblock_t block;
  stmtblock_t body;

  gfc_start_block (&block);

  /* Calculate the switch expression.  */
  gfc_init_se (&se, NULL);
  gfc_conv_expr_val (&se, code->expr);
  gfc_add_block_to_block (&block, &se.pre);

  end_label = gfc_build_label_decl (NULL_TREE);

  gfc_init_block (&body);

  for (c = code->block; c; c = c->block)
    {
      for (cp = c->ext.case_list; cp; cp = cp->next)
	{
	  tree low, high;
          tree label;

	  /* Assume it's the default case.  */
	  low = high = NULL_TREE;

	  if (cp->low)
	    {
	      low = gfc_conv_constant_to_tree (cp->low);

	      /* If there's only a lower bound, set the high bound to the
		 maximum value of the case expression.  */
	      if (!cp->high)
		high = TYPE_MAX_VALUE (TREE_TYPE (se.expr));
	    }

	  if (cp->high)
	    {
	      /* Three cases are possible here:

		 1) There is no lower bound, e.g. CASE (:N).
		 2) There is a lower bound .NE. high bound, that is
		    a case range, e.g. CASE (N:M) where M>N (we make
		    sure that M>N during type resolution).
		 3) There is a lower bound, and it has the same value
		    as the high bound, e.g. CASE (N:N).  This is our
		    internal representation of CASE(N).

		 In the first and second case, we need to set a value for
		 high.  In the thirth case, we don't because the GCC middle
		 end represents a single case value by just letting high be
		 a NULL_TREE.  We can't do that because we need to be able
		 to represent unbounded cases.  */

	      if (!cp->low
		  || (cp->low
		      && mpz_cmp (cp->low->value.integer,
				  cp->high->value.integer) != 0))
		high = gfc_conv_constant_to_tree (cp->high);

	      /* Unbounded case.  */
	      if (!cp->low)
		low = TYPE_MIN_VALUE (TREE_TYPE (se.expr));
	    }

          /* Build a label.  */
          label = gfc_build_label_decl (NULL_TREE);

	  /* Add this case label.
             Add parameter 'label', make it match GCC backend.  */
	  tmp = build3 (CASE_LABEL_EXPR, void_type_node, low, high, label);
	  gfc_add_expr_to_block (&body, tmp);
	}

      /* Add the statements for this case.  */
      tmp = gfc_trans_code (c->next);
      gfc_add_expr_to_block (&body, tmp);

      /* Break to the end of the construct.  */
      tmp = build1_v (GOTO_EXPR, end_label);
      gfc_add_expr_to_block (&body, tmp);
    }

  tmp = gfc_finish_block (&body);
  tmp = build3_v (SWITCH_EXPR, se.expr, tmp, NULL_TREE);
  gfc_add_expr_to_block (&block, tmp);

  tmp = build1_v (LABEL_EXPR, end_label);
  gfc_add_expr_to_block (&block, tmp);

  return gfc_finish_block (&block);
}


/* Translate the SELECT CASE construct for LOGICAL case expressions.

   There are only two cases possible here, even though the standard
   does allow three cases in a LOGICAL SELECT CASE construct: .TRUE.,
   .FALSE., and DEFAULT.

   We never generate more than two blocks here.  Instead, we always
   try to eliminate the DEFAULT case.  This way, we can translate this
   kind of SELECT construct to a simple

   if {} else {};

   expression in GENERIC.  */

static tree
gfc_trans_logical_select (gfc_code * code)
{
  gfc_code *c;
  gfc_code *t, *f, *d;
  gfc_case *cp;
  gfc_se se;
  stmtblock_t block;

  /* Assume we don't have any cases at all.  */
  t = f = d = NULL;

  /* Now see which ones we actually do have.  We can have at most two
     cases in a single case list: one for .TRUE. and one for .FALSE.
     The default case is always separate.  If the cases for .TRUE. and
     .FALSE. are in the same case list, the block for that case list
     always executed, and we don't generate code a COND_EXPR.  */
  for (c = code->block; c; c = c->block)
    {
      for (cp = c->ext.case_list; cp; cp = cp->next)
	{
	  if (cp->low)
	    {
	      if (cp->low->value.logical == 0) /* .FALSE.  */
		f = c;
	      else /* if (cp->value.logical != 0), thus .TRUE.  */
		t = c;
	    }
	  else
	    d = c;
	}
    }

  /* Start a new block.  */
  gfc_start_block (&block);

  /* Calculate the switch expression.  We always need to do this
     because it may have side effects.  */
  gfc_init_se (&se, NULL);
  gfc_conv_expr_val (&se, code->expr);
  gfc_add_block_to_block (&block, &se.pre);

  if (t == f && t != NULL)
    {
      /* Cases for .TRUE. and .FALSE. are in the same block.  Just
         translate the code for these cases, append it to the current
         block.  */
      gfc_add_expr_to_block (&block, gfc_trans_code (t->next));
    }
  else
    {
      tree true_tree, false_tree;

      true_tree = build_empty_stmt ();
      false_tree = build_empty_stmt ();

      /* If we have a case for .TRUE. and for .FALSE., discard the default case.
          Otherwise, if .TRUE. or .FALSE. is missing and there is a default case,
          make the missing case the default case.  */
      if (t != NULL && f != NULL)
	d = NULL;
      else if (d != NULL)
        {
	  if (t == NULL)
	    t = d;
	  else
	    f = d;
	}

      /* Translate the code for each of these blocks, and append it to
         the current block.  */
      if (t != NULL)
        true_tree = gfc_trans_code (t->next);

      if (f != NULL)
	false_tree = gfc_trans_code (f->next);

      gfc_add_expr_to_block (&block, build3_v (COND_EXPR, se.expr,
					       true_tree, false_tree));
    }

  return gfc_finish_block (&block);
}


/* Translate the SELECT CASE construct for CHARACTER case expressions.
   Instead of generating compares and jumps, it is far simpler to
   generate a data structure describing the cases in order and call a
   library subroutine that locates the right case.
   This is particularly true because this is the only case where we
   might have to dispose of a temporary.
   The library subroutine returns a pointer to jump to or NULL if no
   branches are to be taken.  */

static tree
gfc_trans_character_select (gfc_code *code)
{
  tree init, node, end_label, tmp, type, args, *labels;
  stmtblock_t block, body;
  gfc_case *cp, *d;
  gfc_code *c;
  gfc_se se;
  int i, n;

  static tree select_struct;
  static tree ss_string1, ss_string1_len;
  static tree ss_string2, ss_string2_len;
  static tree ss_target;

  if (select_struct == NULL)
    {
      tree gfc_int4_type_node = gfc_get_int_type (4);

      select_struct = make_node (RECORD_TYPE);
      TYPE_NAME (select_struct) = get_identifier ("_jump_struct");

#undef ADD_FIELD
#define ADD_FIELD(NAME, TYPE)				\
  ss_##NAME = gfc_add_field_to_struct			\
     (&(TYPE_FIELDS (select_struct)), select_struct,	\
      get_identifier (stringize(NAME)), TYPE)

      ADD_FIELD (string1, pchar_type_node);
      ADD_FIELD (string1_len, gfc_int4_type_node);

      ADD_FIELD (string2, pchar_type_node);
      ADD_FIELD (string2_len, gfc_int4_type_node);

      ADD_FIELD (target, pvoid_type_node);
#undef ADD_FIELD

      gfc_finish_type (select_struct);
    }

  cp = code->block->ext.case_list;
  while (cp->left != NULL)
    cp = cp->left;

  n = 0;
  for (d = cp; d; d = d->right)
    d->n = n++;

  if (n != 0)
    labels = gfc_getmem (n * sizeof (tree));
  else
    labels = NULL;

  for(i = 0; i < n; i++)
    {
      labels[i] = gfc_build_label_decl (NULL_TREE);
      TREE_USED (labels[i]) = 1;
      /* TODO: The gimplifier should do this for us, but it has
         inadequacies when dealing with static initializers.  */
      FORCED_LABEL (labels[i]) = 1;
    }

  end_label = gfc_build_label_decl (NULL_TREE);

  /* Generate the body */
  gfc_start_block (&block);
  gfc_init_block (&body);

  for (c = code->block; c; c = c->block)
    {
      for (d = c->ext.case_list; d; d = d->next)
        {
          tmp = build1_v (LABEL_EXPR, labels[d->n]);
          gfc_add_expr_to_block (&body, tmp);
        }

      tmp = gfc_trans_code (c->next);
      gfc_add_expr_to_block (&body, tmp);

      tmp = build1_v (GOTO_EXPR, end_label);
      gfc_add_expr_to_block (&body, tmp);
    }

  /* Generate the structure describing the branches */
  init = NULL_TREE;
  i = 0;

  for(d = cp; d; d = d->right, i++)
    {
      node = NULL_TREE;

      gfc_init_se (&se, NULL);

      if (d->low == NULL)
        {
          node = tree_cons (ss_string1, null_pointer_node, node);
          node = tree_cons (ss_string1_len, integer_zero_node, node);
        }
      else
        {
          gfc_conv_expr_reference (&se, d->low);

          node = tree_cons (ss_string1, se.expr, node);
          node = tree_cons (ss_string1_len, se.string_length, node);
        }

      if (d->high == NULL)
        {
          node = tree_cons (ss_string2, null_pointer_node, node);
          node = tree_cons (ss_string2_len, integer_zero_node, node);
        }
      else
        {
          gfc_init_se (&se, NULL);
          gfc_conv_expr_reference (&se, d->high);

          node = tree_cons (ss_string2, se.expr, node);
          node = tree_cons (ss_string2_len, se.string_length, node);
        }

      tmp = gfc_build_addr_expr (pvoid_type_node, labels[i]);
      node = tree_cons (ss_target, tmp, node);

      tmp = build1 (CONSTRUCTOR, select_struct, nreverse (node));
      init = tree_cons (NULL_TREE, tmp, init);
    }

  type = build_array_type (select_struct, build_index_type
			   (build_int_cst (NULL_TREE, n - 1)));

  init = build1 (CONSTRUCTOR, type, nreverse(init));
  TREE_CONSTANT (init) = 1;
  TREE_INVARIANT (init) = 1;
  TREE_STATIC (init) = 1;
  /* Create a static variable to hold the jump table.  */
  tmp = gfc_create_var (type, "jumptable");
  TREE_CONSTANT (tmp) = 1;
  TREE_INVARIANT (tmp) = 1;
  TREE_STATIC (tmp) = 1;
  DECL_INITIAL (tmp) = init;
  init = tmp;

  /* Build an argument list for the library call */
  init = gfc_build_addr_expr (pvoid_type_node, init);
  args = gfc_chainon_list (NULL_TREE, init);

  tmp = build_int_cst (NULL_TREE, n);
  args = gfc_chainon_list (args, tmp);

  tmp = gfc_build_addr_expr (pvoid_type_node, end_label);
  args = gfc_chainon_list (args, tmp);

  gfc_init_se (&se, NULL);
  gfc_conv_expr_reference (&se, code->expr);

  args = gfc_chainon_list (args, se.expr);
  args = gfc_chainon_list (args, se.string_length);

  gfc_add_block_to_block (&block, &se.pre);

  tmp = gfc_build_function_call (gfor_fndecl_select_string, args);
  tmp = build1 (GOTO_EXPR, void_type_node, tmp);
  gfc_add_expr_to_block (&block, tmp);

  tmp = gfc_finish_block (&body);
  gfc_add_expr_to_block (&block, tmp);
  tmp = build1_v (LABEL_EXPR, end_label);
  gfc_add_expr_to_block (&block, tmp);

  if (n != 0)
    gfc_free (labels);

  return gfc_finish_block (&block);
}


/* Translate the three variants of the SELECT CASE construct.

   SELECT CASEs with INTEGER case expressions can be translated to an
   equivalent GENERIC switch statement, and for LOGICAL case
   expressions we build one or two if-else compares.

   SELECT CASEs with CHARACTER case expressions are a whole different
   story, because they don't exist in GENERIC.  So we sort them and
   do a binary search at runtime.

   Fortran has no BREAK statement, and it does not allow jumps from
   one case block to another.  That makes things a lot easier for
   the optimizers.  */

tree
gfc_trans_select (gfc_code * code)
{
  gcc_assert (code && code->expr);

  /* Empty SELECT constructs are legal.  */
  if (code->block == NULL)
    return build_empty_stmt ();

  /* Select the correct translation function.  */
  switch (code->expr->ts.type)
    {
    case BT_LOGICAL:	return gfc_trans_logical_select (code);
    case BT_INTEGER:	return gfc_trans_integer_select (code);
    case BT_CHARACTER:	return gfc_trans_character_select (code);
    default:
      gfc_internal_error ("gfc_trans_select(): Bad type for case expr.");
      /* Not reached */
    }
}


/* Generate the loops for a FORALL block.  The normal loop format:
    count = (end - start + step) / step
    loopvar = start
    while (1)
      {
        if (count <=0 )
          goto end_of_loop
        <body>
        loopvar += step
        count --
      }
    end_of_loop:  */

static tree
gfc_trans_forall_loop (forall_info *forall_tmp, int nvar, tree body, int mask_flag)
{
  int n;
  tree tmp;
  tree cond;
  stmtblock_t block;
  tree exit_label;
  tree count;
  tree var, start, end, step, mask, maskindex;
  iter_info *iter;

  iter = forall_tmp->this_loop;
  for (n = 0; n < nvar; n++)
    {
      var = iter->var;
      start = iter->start;
      end = iter->end;
      step = iter->step;

      exit_label = gfc_build_label_decl (NULL_TREE);
      TREE_USED (exit_label) = 1;

      /* The loop counter.  */
      count = gfc_create_var (TREE_TYPE (var), "count");

      /* The body of the loop.  */
      gfc_init_block (&block);

      /* The exit condition.  */
      cond = build2 (LE_EXPR, boolean_type_node, count, integer_zero_node);
      tmp = build1_v (GOTO_EXPR, exit_label);
      tmp = build3_v (COND_EXPR, cond, tmp, build_empty_stmt ());
      gfc_add_expr_to_block (&block, tmp);

      /* The main loop body.  */
      gfc_add_expr_to_block (&block, body);

      /* Increment the loop variable.  */
      tmp = build2 (PLUS_EXPR, TREE_TYPE (var), var, step);
      gfc_add_modify_expr (&block, var, tmp);

      /* Advance to the next mask element.  Only do this for the
	 innermost loop.  */
      if (n == 0 && mask_flag)
        {
          mask = forall_tmp->mask;
          maskindex = forall_tmp->maskindex;
          if (mask)
            {
              tmp = build2 (PLUS_EXPR, gfc_array_index_type,
			    maskindex, gfc_index_one_node);
              gfc_add_modify_expr (&block, maskindex, tmp);
            }
        }
      /* Decrement the loop counter.  */
      tmp = build2 (MINUS_EXPR, TREE_TYPE (var), count, gfc_index_one_node);
      gfc_add_modify_expr (&block, count, tmp);

      body = gfc_finish_block (&block);

      /* Loop var initialization.  */
      gfc_init_block (&block);
      gfc_add_modify_expr (&block, var, start);

      /* Initialize the loop counter.  */
      tmp = fold_build2 (MINUS_EXPR, TREE_TYPE (var), step, start);
      tmp = fold_build2 (PLUS_EXPR, TREE_TYPE (var), end, tmp);
      tmp = fold_build2 (TRUNC_DIV_EXPR, TREE_TYPE (var), tmp, step);
      gfc_add_modify_expr (&block, count, tmp);

      /* The loop expression.  */
      tmp = build1_v (LOOP_EXPR, body);
      gfc_add_expr_to_block (&block, tmp);

      /* The exit label.  */
      tmp = build1_v (LABEL_EXPR, exit_label);
      gfc_add_expr_to_block (&block, tmp);

      body = gfc_finish_block (&block);
      iter = iter->next;
    }
  return body;
}


/* Generate the body and loops according to MASK_FLAG and NEST_FLAG.
   if MASK_FLAG is nonzero, the body is controlled by maskes in forall
   nest, otherwise, the body is not controlled by maskes.
   if NEST_FLAG is nonzero, generate loops for nested forall, otherwise,
   only generate loops for the current forall level.  */

static tree
gfc_trans_nested_forall_loop (forall_info * nested_forall_info, tree body,
                              int mask_flag, int nest_flag)
{
  tree tmp;
  int nvar;
  forall_info *forall_tmp;
  tree pmask, mask, maskindex;

  forall_tmp = nested_forall_info;
  /* Generate loops for nested forall.  */
  if (nest_flag)
    {
      while (forall_tmp->next_nest != NULL)
        forall_tmp = forall_tmp->next_nest;
      while (forall_tmp != NULL)
        {
          /* Generate body with masks' control.  */
          if (mask_flag)
            {
              pmask = forall_tmp->pmask;
              mask = forall_tmp->mask;
              maskindex = forall_tmp->maskindex;

              if (mask)
                {
                  /* If a mask was specified make the assignment conditional.  */
                  if (pmask)
		    tmp = gfc_build_indirect_ref (mask);
                  else
                    tmp = mask;
                  tmp = gfc_build_array_ref (tmp, maskindex);

                  body = build3_v (COND_EXPR, tmp, body, build_empty_stmt ());
                }
            }
          nvar = forall_tmp->nvar;
          body = gfc_trans_forall_loop (forall_tmp, nvar, body, mask_flag);
          forall_tmp = forall_tmp->outer;
        }
    }
  else
    {
      nvar = forall_tmp->nvar;
      body = gfc_trans_forall_loop (forall_tmp, nvar, body, mask_flag);
    }

  return body;
}


/* Allocate data for holding a temporary array.  Returns either a local
   temporary array or a pointer variable.  */

static tree
gfc_do_allocate (tree bytesize, tree size, tree * pdata, stmtblock_t * pblock,
                 tree elem_type)
{
  tree tmpvar;
  tree type;
  tree tmp;
  tree args;

  if (INTEGER_CST_P (size))
    {
      tmp = fold_build2 (MINUS_EXPR, gfc_array_index_type, size,
			 gfc_index_one_node);
    }
  else
    tmp = NULL_TREE;

  type = build_range_type (gfc_array_index_type, gfc_index_zero_node, tmp);
  type = build_array_type (elem_type, type);
  if (gfc_can_put_var_on_stack (bytesize))
    {
      gcc_assert (INTEGER_CST_P (size));
      tmpvar = gfc_create_var (type, "temp");
      *pdata = NULL_TREE;
    }
  else
    {
      tmpvar = gfc_create_var (build_pointer_type (type), "temp");
      *pdata = convert (pvoid_type_node, tmpvar);

      args = gfc_chainon_list (NULL_TREE, bytesize);
      if (gfc_index_integer_kind == 4)
	tmp = gfor_fndecl_internal_malloc;
      else if (gfc_index_integer_kind == 8)
	tmp = gfor_fndecl_internal_malloc64;
      else
	gcc_unreachable ();
      tmp = gfc_build_function_call (tmp, args);
      tmp = convert (TREE_TYPE (tmpvar), tmp);
      gfc_add_modify_expr (pblock, tmpvar, tmp);
    }
  return tmpvar;
}


/* Generate codes to copy the temporary to the actual lhs.  */

static tree
generate_loop_for_temp_to_lhs (gfc_expr *expr, tree tmp1, tree count3,
			       tree count1, tree wheremask)
{
  gfc_ss *lss;
  gfc_se lse, rse;
  stmtblock_t block, body;
  gfc_loopinfo loop1;
  tree tmp, tmp2;
  tree wheremaskexpr;

  /* Walk the lhs.  */
  lss = gfc_walk_expr (expr);

  if (lss == gfc_ss_terminator)
    {
      gfc_start_block (&block);

      gfc_init_se (&lse, NULL);

      /* Translate the expression.  */
      gfc_conv_expr (&lse, expr);

      /* Form the expression for the temporary.  */
      tmp = gfc_build_array_ref (tmp1, count1);

      /* Use the scalar assignment as is.  */
      gfc_add_block_to_block (&block, &lse.pre);
      gfc_add_modify_expr (&block, lse.expr, tmp);
      gfc_add_block_to_block (&block, &lse.post);

      /* Increment the count1.  */
      tmp = fold_build2 (PLUS_EXPR, TREE_TYPE (count1), count1,
			 gfc_index_one_node);
      gfc_add_modify_expr (&block, count1, tmp);

      tmp = gfc_finish_block (&block);
    }
  else
    {
      gfc_start_block (&block);

      gfc_init_loopinfo (&loop1);
      gfc_init_se (&rse, NULL);
      gfc_init_se (&lse, NULL);

      /* Associate the lss with the loop.  */
      gfc_add_ss_to_loop (&loop1, lss);

      /* Calculate the bounds of the scalarization.  */
      gfc_conv_ss_startstride (&loop1);
      /* Setup the scalarizing loops.  */
      gfc_conv_loop_setup (&loop1);

      gfc_mark_ss_chain_used (lss, 1);

      /* Start the scalarized loop body.  */
      gfc_start_scalarized_body (&loop1, &body);

      /* Setup the gfc_se structures.  */
      gfc_copy_loopinfo_to_se (&lse, &loop1);
      lse.ss = lss;

      /* Form the expression of the temporary.  */
      if (lss != gfc_ss_terminator)
	rse.expr = gfc_build_array_ref (tmp1, count1);
      /* Translate expr.  */
      gfc_conv_expr (&lse, expr);

      /* Use the scalar assignment.  */
      tmp = gfc_trans_scalar_assign (&lse, &rse, expr->ts.type);

     /* Form the mask expression according to the mask tree list.  */
     if (wheremask)
       {
	 wheremaskexpr = gfc_build_array_ref (wheremask, count3);
	 tmp2 = TREE_CHAIN (wheremask);
	 while (tmp2)
	   {
	     tmp1 = gfc_build_array_ref (tmp2, count3);
	     wheremaskexpr = build2 (TRUTH_AND_EXPR, TREE_TYPE (tmp1),
				     wheremaskexpr, tmp1);
	     tmp2 = TREE_CHAIN (tmp2);
	   }
	 tmp = build3_v (COND_EXPR, wheremaskexpr, tmp, build_empty_stmt ());
       }

      gfc_add_expr_to_block (&body, tmp);

      /* Increment count1.  */
      tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			 count1, gfc_index_one_node);
      gfc_add_modify_expr (&body, count1, tmp);

      /* Increment count3.  */
      if (count3)
	{
	  tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			     count3, gfc_index_one_node);
	  gfc_add_modify_expr (&body, count3, tmp);
	}

      /* Generate the copying loops.  */
      gfc_trans_scalarizing_loops (&loop1, &body);
      gfc_add_block_to_block (&block, &loop1.pre);
      gfc_add_block_to_block (&block, &loop1.post);
      gfc_cleanup_loop (&loop1);

      tmp = gfc_finish_block (&block);
    }
  return tmp;
}


/* Generate codes to copy rhs to the temporary. TMP1 is the address of temporary
   LSS and RSS are formed in function compute_inner_temp_size(), and should
   not be freed.  */

static tree
generate_loop_for_rhs_to_temp (gfc_expr *expr2, tree tmp1, tree count3,
			       tree count1, gfc_ss *lss, gfc_ss *rss,
			       tree wheremask)
{
  stmtblock_t block, body1;
  gfc_loopinfo loop;
  gfc_se lse;
  gfc_se rse;
  tree tmp, tmp2;
  tree wheremaskexpr;

  gfc_start_block (&block);

  gfc_init_se (&rse, NULL);
  gfc_init_se (&lse, NULL);

  if (lss == gfc_ss_terminator)
    {
      gfc_init_block (&body1);
      gfc_conv_expr (&rse, expr2);
      lse.expr = gfc_build_array_ref (tmp1, count1);
    }
  else
    {
      /* Initialize the loop.  */
      gfc_init_loopinfo (&loop);

      /* We may need LSS to determine the shape of the expression.  */
      gfc_add_ss_to_loop (&loop, lss);
      gfc_add_ss_to_loop (&loop, rss);

      gfc_conv_ss_startstride (&loop);
      gfc_conv_loop_setup (&loop);

      gfc_mark_ss_chain_used (rss, 1);
      /* Start the loop body.  */
      gfc_start_scalarized_body (&loop, &body1);

      /* Translate the expression.  */
      gfc_copy_loopinfo_to_se (&rse, &loop);
      rse.ss = rss;
      gfc_conv_expr (&rse, expr2);

      /* Form the expression of the temporary.  */
      lse.expr = gfc_build_array_ref (tmp1, count1);
    }

  /* Use the scalar assignment.  */
  tmp = gfc_trans_scalar_assign (&lse, &rse, expr2->ts.type);

  /* Form the mask expression according to the mask tree list.  */
  if (wheremask)
    {
      wheremaskexpr = gfc_build_array_ref (wheremask, count3);
      tmp2 = TREE_CHAIN (wheremask);
      while (tmp2)
	{
	  tmp1 = gfc_build_array_ref (tmp2, count3);
	  wheremaskexpr = build2 (TRUTH_AND_EXPR, TREE_TYPE (tmp1),
				  wheremaskexpr, tmp1);
	  tmp2 = TREE_CHAIN (tmp2);
	}
      tmp = build3_v (COND_EXPR, wheremaskexpr, tmp, build_empty_stmt ());
    }

  gfc_add_expr_to_block (&body1, tmp);

  if (lss == gfc_ss_terminator)
    {
      gfc_add_block_to_block (&block, &body1);

      /* Increment count1.  */
      tmp = fold_build2 (PLUS_EXPR, TREE_TYPE (count1), count1,
			 gfc_index_one_node);
      gfc_add_modify_expr (&block, count1, tmp);
    }
  else
    {
      /* Increment count1.  */
      tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			 count1, gfc_index_one_node);
      gfc_add_modify_expr (&body1, count1, tmp);

      /* Increment count3.  */
      if (count3)
	{
	  tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			     count3, gfc_index_one_node);
	  gfc_add_modify_expr (&body1, count3, tmp);
	}

      /* Generate the copying loops.  */
      gfc_trans_scalarizing_loops (&loop, &body1);

      gfc_add_block_to_block (&block, &loop.pre);
      gfc_add_block_to_block (&block, &loop.post);

      gfc_cleanup_loop (&loop);
      /* TODO: Reuse lss and rss when copying temp->lhs.  Need to be careful
	 as tree nodes in SS may not be valid in different scope.  */
    }

  tmp = gfc_finish_block (&block);
  return tmp;
}


/* Calculate the size of temporary needed in the assignment inside forall.
   LSS and RSS are filled in this function.  */

static tree
compute_inner_temp_size (gfc_expr *expr1, gfc_expr *expr2,
			 stmtblock_t * pblock,
                         gfc_ss **lss, gfc_ss **rss)
{
  gfc_loopinfo loop;
  tree size;
  int i;
  tree tmp;

  *lss = gfc_walk_expr (expr1);
  *rss = NULL;

  size = gfc_index_one_node;
  if (*lss != gfc_ss_terminator)
    {
      gfc_init_loopinfo (&loop);

      /* Walk the RHS of the expression.  */
      *rss = gfc_walk_expr (expr2);
      if (*rss == gfc_ss_terminator)
        {
          /* The rhs is scalar.  Add a ss for the expression.  */
          *rss = gfc_get_ss ();
          (*rss)->next = gfc_ss_terminator;
          (*rss)->type = GFC_SS_SCALAR;
          (*rss)->expr = expr2;
        }

      /* Associate the SS with the loop.  */
      gfc_add_ss_to_loop (&loop, *lss);
      /* We don't actually need to add the rhs at this point, but it might
         make guessing the loop bounds a bit easier.  */
      gfc_add_ss_to_loop (&loop, *rss);

      /* We only want the shape of the expression, not rest of the junk
         generated by the scalarizer.  */
      loop.array_parameter = 1;

      /* Calculate the bounds of the scalarization.  */
      gfc_conv_ss_startstride (&loop);
      gfc_conv_loop_setup (&loop);

      /* Figure out how many elements we need.  */
      for (i = 0; i < loop.dimen; i++)
        {
	  tmp = fold_build2 (MINUS_EXPR, gfc_array_index_type,
			     gfc_index_one_node, loop.from[i]);
          tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			     tmp, loop.to[i]);
          size = fold_build2 (MULT_EXPR, gfc_array_index_type, size, tmp);
        }
      gfc_add_block_to_block (pblock, &loop.pre);
      size = gfc_evaluate_now (size, pblock);
      gfc_add_block_to_block (pblock, &loop.post);

      /* TODO: write a function that cleans up a loopinfo without freeing
         the SS chains.  Currently a NOP.  */
    }

  return size;
}


/* Calculate the overall iterator number of the nested forall construct.  */

static tree
compute_overall_iter_number (forall_info *nested_forall_info, tree inner_size,
			     stmtblock_t *inner_size_body, stmtblock_t *block)
{
  tree tmp, number;
  stmtblock_t body;

  /* TODO: optimizing the computing process.  */
  number = gfc_create_var (gfc_array_index_type, "num");
  gfc_add_modify_expr (block, number, gfc_index_zero_node);

  gfc_start_block (&body);
  if (inner_size_body)
    gfc_add_block_to_block (&body, inner_size_body);
  if (nested_forall_info)
    tmp = build2 (PLUS_EXPR, gfc_array_index_type, number,
		  inner_size);
  else
    tmp = inner_size;
  gfc_add_modify_expr (&body, number, tmp);
  tmp = gfc_finish_block (&body);

  /* Generate loops.  */
  if (nested_forall_info != NULL)
    tmp = gfc_trans_nested_forall_loop (nested_forall_info, tmp, 0, 1);

  gfc_add_expr_to_block (block, tmp);

  return number;
}


/* Allocate temporary for forall construct.  SIZE is the size of temporary
   needed.  PTEMP1 is returned for space free.  */

static tree
allocate_temp_for_forall_nest_1 (tree type, tree size, stmtblock_t * block,
				 tree * ptemp1)
{
  tree unit;
  tree temp1;
  tree tmp;
  tree bytesize;

  unit = TYPE_SIZE_UNIT (type);
  bytesize = fold_build2 (MULT_EXPR, gfc_array_index_type, size, unit);

  *ptemp1 = NULL;
  temp1 = gfc_do_allocate (bytesize, size, ptemp1, block, type);

  if (*ptemp1)
    tmp = gfc_build_indirect_ref (temp1);
  else
    tmp = temp1;

  return tmp;
}


/* Allocate temporary for forall construct according to the information in
   nested_forall_info.  INNER_SIZE is the size of temporary needed in the
   assignment inside forall.  PTEMP1 is returned for space free.  */

static tree
allocate_temp_for_forall_nest (forall_info * nested_forall_info, tree type,
			       tree inner_size, stmtblock_t * inner_size_body,
			       stmtblock_t * block, tree * ptemp1)
{
  tree size;

  /* Calculate the total size of temporary needed in forall construct.  */
  size = compute_overall_iter_number (nested_forall_info, inner_size,
				      inner_size_body, block);

  return allocate_temp_for_forall_nest_1 (type, size, block, ptemp1);
}


/* Handle assignments inside forall which need temporary.

    forall (i=start:end:stride; maskexpr)
      e<i> = f<i>
    end forall
   (where e,f<i> are arbitrary expressions possibly involving i
    and there is a dependency between e<i> and f<i>)
   Translates to:
    masktmp(:) = maskexpr(:)

    maskindex = 0;
    count1 = 0;
    num = 0;
    for (i = start; i <= end; i += stride)
      num += SIZE (f<i>)
    count1 = 0;
    ALLOCATE (tmp(num))
    for (i = start; i <= end; i += stride)
      {
	if (masktmp[maskindex++])
	  tmp[count1++] = f<i>
      }
    maskindex = 0;
    count1 = 0;
    for (i = start; i <= end; i += stride)
      {
	if (masktmp[maskindex++])
	  e<i> = tmp[count1++]
      }
    DEALLOCATE (tmp)
  */
static void
gfc_trans_assign_need_temp (gfc_expr * expr1, gfc_expr * expr2, tree wheremask,
                            forall_info * nested_forall_info,
                            stmtblock_t * block)
{
  tree type;
  tree inner_size;
  gfc_ss *lss, *rss;
  tree count, count1;
  tree tmp, tmp1;
  tree ptemp1;
  tree mask, maskindex;
  forall_info *forall_tmp;
  stmtblock_t inner_size_body;

  /* Create vars. count1 is the current iterator number of the nested
     forall.  */
  count1 = gfc_create_var (gfc_array_index_type, "count1");

  /* Count is the wheremask index.  */
  if (wheremask)
    {
      count = gfc_create_var (gfc_array_index_type, "count");
      gfc_add_modify_expr (block, count, gfc_index_zero_node);
    }
  else
    count = NULL;

  /* Initialize count1.  */
  gfc_add_modify_expr (block, count1, gfc_index_zero_node);

  /* Calculate the size of temporary needed in the assignment. Return loop, lss
     and rss which are used in function generate_loop_for_rhs_to_temp().  */
  gfc_init_block (&inner_size_body);
  inner_size = compute_inner_temp_size (expr1, expr2, &inner_size_body,
					&lss, &rss);

  /* The type of LHS. Used in function allocate_temp_for_forall_nest */
  type = gfc_typenode_for_spec (&expr1->ts);

  /* Allocate temporary for nested forall construct according to the
     information in nested_forall_info and inner_size.  */
  tmp1 = allocate_temp_for_forall_nest (nested_forall_info, type, inner_size,
					&inner_size_body, block, &ptemp1);

  /* Initialize the maskindexes.  */
  forall_tmp = nested_forall_info;
  while (forall_tmp != NULL)
    {
      mask = forall_tmp->mask;
      maskindex = forall_tmp->maskindex;
      if (mask)
        gfc_add_modify_expr (block, maskindex, gfc_index_zero_node);
      forall_tmp = forall_tmp->next_nest;
    }

  /* Generate codes to copy rhs to the temporary .  */
  tmp = generate_loop_for_rhs_to_temp (expr2, tmp1, count, count1, lss, rss,
				       wheremask);

  /* Generate body and loops according to the information in
     nested_forall_info.  */
  tmp = gfc_trans_nested_forall_loop (nested_forall_info, tmp, 1, 1);
  gfc_add_expr_to_block (block, tmp);

  /* Reset count1.  */
  gfc_add_modify_expr (block, count1, gfc_index_zero_node);

  /* Reset maskindexed.  */
  forall_tmp = nested_forall_info;
  while (forall_tmp != NULL)
    {
      mask = forall_tmp->mask;
      maskindex = forall_tmp->maskindex;
      if (mask)
        gfc_add_modify_expr (block, maskindex, gfc_index_zero_node);
      forall_tmp = forall_tmp->next_nest;
    }

  /* Reset count.  */
  if (wheremask)
    gfc_add_modify_expr (block, count, gfc_index_zero_node);

  /* Generate codes to copy the temporary to lhs.  */
  tmp = generate_loop_for_temp_to_lhs (expr1, tmp1, count, count1, wheremask);

  /* Generate body and loops according to the information in
     nested_forall_info.  */
  tmp = gfc_trans_nested_forall_loop (nested_forall_info, tmp, 1, 1);
  gfc_add_expr_to_block (block, tmp);

  if (ptemp1)
    {
      /* Free the temporary.  */
      tmp = gfc_chainon_list (NULL_TREE, ptemp1);
      tmp = gfc_build_function_call (gfor_fndecl_internal_free, tmp);
      gfc_add_expr_to_block (block, tmp);
    }
}


/* Translate pointer assignment inside FORALL which need temporary.  */

static void
gfc_trans_pointer_assign_need_temp (gfc_expr * expr1, gfc_expr * expr2,
                                    forall_info * nested_forall_info,
                                    stmtblock_t * block)
{
  tree type;
  tree inner_size;
  gfc_ss *lss, *rss;
  gfc_se lse;
  gfc_se rse;
  gfc_ss_info *info;
  gfc_loopinfo loop;
  tree desc;
  tree parm;
  tree parmtype;
  stmtblock_t body;
  tree count;
  tree tmp, tmp1, ptemp1;
  tree mask, maskindex;
  forall_info *forall_tmp;

  count = gfc_create_var (gfc_array_index_type, "count");
  gfc_add_modify_expr (block, count, gfc_index_zero_node);

  inner_size = integer_one_node;
  lss = gfc_walk_expr (expr1);
  rss = gfc_walk_expr (expr2);
  if (lss == gfc_ss_terminator)
    {
      type = gfc_typenode_for_spec (&expr1->ts);
      type = build_pointer_type (type);

      /* Allocate temporary for nested forall construct according to the
         information in nested_forall_info and inner_size.  */
      tmp1 = allocate_temp_for_forall_nest (nested_forall_info, type,
					    inner_size, NULL, block, &ptemp1);
      gfc_start_block (&body);
      gfc_init_se (&lse, NULL);
      lse.expr = gfc_build_array_ref (tmp1, count);
      gfc_init_se (&rse, NULL);
      rse.want_pointer = 1;
      gfc_conv_expr (&rse, expr2);
      gfc_add_block_to_block (&body, &rse.pre);
      gfc_add_modify_expr (&body, lse.expr, rse.expr);
      gfc_add_block_to_block (&body, &rse.post);

      /* Increment count.  */
      tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			 count, gfc_index_one_node);
      gfc_add_modify_expr (&body, count, tmp);

      tmp = gfc_finish_block (&body);

      /* Initialize the maskindexes.  */
      forall_tmp = nested_forall_info;
      while (forall_tmp != NULL)
        {
          mask = forall_tmp->mask;
          maskindex = forall_tmp->maskindex;
          if (mask)
            gfc_add_modify_expr (block, maskindex, gfc_index_zero_node);
          forall_tmp = forall_tmp->next_nest;
        }

      /* Generate body and loops according to the information in
         nested_forall_info.  */
      tmp = gfc_trans_nested_forall_loop (nested_forall_info, tmp, 1, 1);
      gfc_add_expr_to_block (block, tmp);

      /* Reset count.  */
      gfc_add_modify_expr (block, count, gfc_index_zero_node);

      /* Reset maskindexes.  */
      forall_tmp = nested_forall_info;
      while (forall_tmp != NULL)
        {
          mask = forall_tmp->mask;
          maskindex = forall_tmp->maskindex;
          if (mask)
            gfc_add_modify_expr (block, maskindex, gfc_index_zero_node);
          forall_tmp = forall_tmp->next_nest;
        }
      gfc_start_block (&body);
      gfc_init_se (&lse, NULL);
      gfc_init_se (&rse, NULL);
      rse.expr = gfc_build_array_ref (tmp1, count);
      lse.want_pointer = 1;
      gfc_conv_expr (&lse, expr1);
      gfc_add_block_to_block (&body, &lse.pre);
      gfc_add_modify_expr (&body, lse.expr, rse.expr);
      gfc_add_block_to_block (&body, &lse.post);
      /* Increment count.  */
      tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			 count, gfc_index_one_node);
      gfc_add_modify_expr (&body, count, tmp);
      tmp = gfc_finish_block (&body);

      /* Generate body and loops according to the information in
         nested_forall_info.  */
      tmp = gfc_trans_nested_forall_loop (nested_forall_info, tmp, 1, 1);
      gfc_add_expr_to_block (block, tmp);
    }
  else
    {
      gfc_init_loopinfo (&loop);

      /* Associate the SS with the loop.  */
      gfc_add_ss_to_loop (&loop, rss);

      /* Setup the scalarizing loops and bounds.  */
      gfc_conv_ss_startstride (&loop);

      gfc_conv_loop_setup (&loop);

      info = &rss->data.info;
      desc = info->descriptor;

      /* Make a new descriptor.  */
      parmtype = gfc_get_element_type (TREE_TYPE (desc));
      parmtype = gfc_get_array_type_bounds (parmtype, loop.dimen,
                                            loop.from, loop.to, 1);

      /* Allocate temporary for nested forall construct.  */
      tmp1 = allocate_temp_for_forall_nest (nested_forall_info, parmtype,
					    inner_size, NULL, block, &ptemp1);
      gfc_start_block (&body);
      gfc_init_se (&lse, NULL);
      lse.expr = gfc_build_array_ref (tmp1, count);
      lse.direct_byref = 1;
      rss = gfc_walk_expr (expr2);
      gfc_conv_expr_descriptor (&lse, expr2, rss);

      gfc_add_block_to_block (&body, &lse.pre);
      gfc_add_block_to_block (&body, &lse.post);

      /* Increment count.  */
      tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			 count, gfc_index_one_node);
      gfc_add_modify_expr (&body, count, tmp);

      tmp = gfc_finish_block (&body);

      /* Initialize the maskindexes.  */
      forall_tmp = nested_forall_info;
      while (forall_tmp != NULL)
        {
          mask = forall_tmp->mask;
          maskindex = forall_tmp->maskindex;
          if (mask)
            gfc_add_modify_expr (block, maskindex, gfc_index_zero_node);
          forall_tmp = forall_tmp->next_nest;
        }

      /* Generate body and loops according to the information in
         nested_forall_info.  */
      tmp = gfc_trans_nested_forall_loop (nested_forall_info, tmp, 1, 1);
      gfc_add_expr_to_block (block, tmp);

      /* Reset count.  */
      gfc_add_modify_expr (block, count, gfc_index_zero_node);

      /* Reset maskindexes.  */
      forall_tmp = nested_forall_info;
      while (forall_tmp != NULL)
        {
          mask = forall_tmp->mask;
          maskindex = forall_tmp->maskindex;
          if (mask)
            gfc_add_modify_expr (block, maskindex, gfc_index_zero_node);
          forall_tmp = forall_tmp->next_nest;
        }
      parm = gfc_build_array_ref (tmp1, count);
      lss = gfc_walk_expr (expr1);
      gfc_init_se (&lse, NULL);
      gfc_conv_expr_descriptor (&lse, expr1, lss);
      gfc_add_modify_expr (&lse.pre, lse.expr, parm);
      gfc_start_block (&body);
      gfc_add_block_to_block (&body, &lse.pre);
      gfc_add_block_to_block (&body, &lse.post);

      /* Increment count.  */
      tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			 count, gfc_index_one_node);
      gfc_add_modify_expr (&body, count, tmp);

      tmp = gfc_finish_block (&body);

      tmp = gfc_trans_nested_forall_loop (nested_forall_info, tmp, 1, 1);
      gfc_add_expr_to_block (block, tmp);
    }
  /* Free the temporary.  */
  if (ptemp1)
    {
      tmp = gfc_chainon_list (NULL_TREE, ptemp1);
      tmp = gfc_build_function_call (gfor_fndecl_internal_free, tmp);
      gfc_add_expr_to_block (block, tmp);
    }
}


/* FORALL and WHERE statements are really nasty, especially when you nest
   them. All the rhs of a forall assignment must be evaluated before the
   actual assignments are performed. Presumably this also applies to all the
   assignments in an inner where statement.  */

/* Generate code for a FORALL statement.  Any temporaries are allocated as a
   linear array, relying on the fact that we process in the same order in all
   loops.

    forall (i=start:end:stride; maskexpr)
      e<i> = f<i>
      g<i> = h<i>
    end forall
   (where e,f,g,h<i> are arbitrary expressions possibly involving i)
   Translates to:
    count = ((end + 1 - start) / stride)
    masktmp(:) = maskexpr(:)

    maskindex = 0;
    for (i = start; i <= end; i += stride)
      {
        if (masktmp[maskindex++])
          e<i> = f<i>
      }
    maskindex = 0;
    for (i = start; i <= end; i += stride)
      {
        if (masktmp[maskindex++])
          g<i> = h<i>
      }

    Note that this code only works when there are no dependencies.
    Forall loop with array assignments and data dependencies are a real pain,
    because the size of the temporary cannot always be determined before the
    loop is executed.  This problem is compounded by the presence of nested
    FORALL constructs.
 */

static tree
gfc_trans_forall_1 (gfc_code * code, forall_info * nested_forall_info)
{
  stmtblock_t block;
  stmtblock_t body;
  tree *var;
  tree *start;
  tree *end;
  tree *step;
  gfc_expr **varexpr;
  tree tmp;
  tree assign;
  tree size;
  tree bytesize;
  tree tmpvar;
  tree sizevar;
  tree lenvar;
  tree maskindex;
  tree mask;
  tree pmask;
  int n;
  int nvar;
  int need_temp;
  gfc_forall_iterator *fa;
  gfc_se se;
  gfc_code *c;
  gfc_saved_var *saved_vars;
  iter_info *this_forall, *iter_tmp;
  forall_info *info, *forall_tmp;
  temporary_list *temp;

  gfc_start_block (&block);

  n = 0;
  /* Count the FORALL index number.  */
  for (fa = code->ext.forall_iterator; fa; fa = fa->next)
    n++;
  nvar = n;

  /* Allocate the space for var, start, end, step, varexpr.  */
  var = (tree *) gfc_getmem (nvar * sizeof (tree));
  start = (tree *) gfc_getmem (nvar * sizeof (tree));
  end = (tree *) gfc_getmem (nvar * sizeof (tree));
  step = (tree *) gfc_getmem (nvar * sizeof (tree));
  varexpr = (gfc_expr **) gfc_getmem (nvar * sizeof (gfc_expr *));
  saved_vars = (gfc_saved_var *) gfc_getmem (nvar * sizeof (gfc_saved_var));

  /* Allocate the space for info.  */
  info = (forall_info *) gfc_getmem (sizeof (forall_info));
  n = 0;
  for (fa = code->ext.forall_iterator; fa; fa = fa->next)
    {
      gfc_symbol *sym = fa->var->symtree->n.sym;

      /* allocate space for this_forall.  */
      this_forall = (iter_info *) gfc_getmem (sizeof (iter_info));

      /* Create a temporary variable for the FORALL index.  */
      tmp = gfc_typenode_for_spec (&sym->ts);
      var[n] = gfc_create_var (tmp, sym->name);
      gfc_shadow_sym (sym, var[n], &saved_vars[n]);

      /* Record it in this_forall.  */
      this_forall->var = var[n];

      /* Replace the index symbol's backend_decl with the temporary decl.  */
      sym->backend_decl = var[n];

      /* Work out the start, end and stride for the loop.  */
      gfc_init_se (&se, NULL);
      gfc_conv_expr_val (&se, fa->start);
      /* Record it in this_forall.  */
      this_forall->start = se.expr;
      gfc_add_block_to_block (&block, &se.pre);
      start[n] = se.expr;

      gfc_init_se (&se, NULL);
      gfc_conv_expr_val (&se, fa->end);
      /* Record it in this_forall.  */
      this_forall->end = se.expr;
      gfc_make_safe_expr (&se);
      gfc_add_block_to_block (&block, &se.pre);
      end[n] = se.expr;

      gfc_init_se (&se, NULL);
      gfc_conv_expr_val (&se, fa->stride);
      /* Record it in this_forall.  */
      this_forall->step = se.expr;
      gfc_make_safe_expr (&se);
      gfc_add_block_to_block (&block, &se.pre);
      step[n] = se.expr;

      /* Set the NEXT field of this_forall to NULL.  */
      this_forall->next = NULL;
      /* Link this_forall to the info construct.  */
      if (info->this_loop == NULL)
        info->this_loop = this_forall;
      else
        {
          iter_tmp = info->this_loop;
          while (iter_tmp->next != NULL)
            iter_tmp = iter_tmp->next;
          iter_tmp->next = this_forall;
        }

      n++;
    }
  nvar = n;

  /* Work out the number of elements in the mask array.  */
  tmpvar = NULL_TREE;
  lenvar = NULL_TREE;
  size = gfc_index_one_node;
  sizevar = NULL_TREE;

  for (n = 0; n < nvar; n++)
    {
      if (lenvar && TREE_TYPE (lenvar) != TREE_TYPE (start[n]))
	lenvar = NULL_TREE;

      /* size = (end + step - start) / step.  */
      tmp = fold_build2 (MINUS_EXPR, TREE_TYPE (start[n]), 
			 step[n], start[n]);
      tmp = fold_build2 (PLUS_EXPR, TREE_TYPE (end[n]), end[n], tmp);

      tmp = fold_build2 (FLOOR_DIV_EXPR, TREE_TYPE (tmp), tmp, step[n]);
      tmp = convert (gfc_array_index_type, tmp);

      size = fold_build2 (MULT_EXPR, gfc_array_index_type, size, tmp);
    }

  /* Record the nvar and size of current forall level.  */
  info->nvar = nvar;
  info->size = size;

  /* Link the current forall level to nested_forall_info.  */
  forall_tmp = nested_forall_info;
  if (forall_tmp == NULL)
    nested_forall_info = info;
  else
    {
      while (forall_tmp->next_nest != NULL)
        forall_tmp = forall_tmp->next_nest;
      info->outer = forall_tmp;
      forall_tmp->next_nest = info;
    }

  /* Copy the mask into a temporary variable if required.
     For now we assume a mask temporary is needed.  */
  if (code->expr)
    {
      /* Allocate the mask temporary.  */
      bytesize = fold_build2 (MULT_EXPR, gfc_array_index_type, size,
			      TYPE_SIZE_UNIT (boolean_type_node));

      mask = gfc_do_allocate (bytesize, size, &pmask, &block, boolean_type_node);

      maskindex = gfc_create_var_np (gfc_array_index_type, "mi");
      /* Record them in the info structure.  */
      info->pmask = pmask;
      info->mask = mask;
      info->maskindex = maskindex;

      gfc_add_modify_expr (&block, maskindex, gfc_index_zero_node);

      /* Start of mask assignment loop body.  */
      gfc_start_block (&body);

      /* Evaluate the mask expression.  */
      gfc_init_se (&se, NULL);
      gfc_conv_expr_val (&se, code->expr);
      gfc_add_block_to_block (&body, &se.pre);

      /* Store the mask.  */
      se.expr = convert (boolean_type_node, se.expr);

      if (pmask)
	tmp = gfc_build_indirect_ref (mask);
      else
	tmp = mask;
      tmp = gfc_build_array_ref (tmp, maskindex);
      gfc_add_modify_expr (&body, tmp, se.expr);

      /* Advance to the next mask element.  */
      tmp = build2 (PLUS_EXPR, gfc_array_index_type,
		   maskindex, gfc_index_one_node);
      gfc_add_modify_expr (&body, maskindex, tmp);

      /* Generate the loops.  */
      tmp = gfc_finish_block (&body);
      tmp = gfc_trans_nested_forall_loop (info, tmp, 0, 0);
      gfc_add_expr_to_block (&block, tmp);
    }
  else
    {
      /* No mask was specified.  */
      maskindex = NULL_TREE;
      mask = pmask = NULL_TREE;
    }

  c = code->block->next;

  /* TODO: loop merging in FORALL statements.  */
  /* Now that we've got a copy of the mask, generate the assignment loops.  */
  while (c)
    {
      switch (c->op)
	{
	case EXEC_ASSIGN:
          /* A scalar or array assignment.  */
	  need_temp = gfc_check_dependency (c->expr, c->expr2, varexpr, nvar);
          /* Temporaries due to array assignment data dependencies introduce
             no end of problems.  */
	  if (need_temp)
            gfc_trans_assign_need_temp (c->expr, c->expr2, NULL,
                                        nested_forall_info, &block);
          else
            {
              /* Use the normal assignment copying routines.  */
              assign = gfc_trans_assignment (c->expr, c->expr2);

              /* Reset the mask index.  */
              if (mask)
                gfc_add_modify_expr (&block, maskindex, gfc_index_zero_node);

              /* Generate body and loops.  */
              tmp = gfc_trans_nested_forall_loop (nested_forall_info, assign, 1, 1);
              gfc_add_expr_to_block (&block, tmp);
            }

	  break;

        case EXEC_WHERE:

	  /* Translate WHERE or WHERE construct nested in FORALL.  */
          temp = NULL;
	  gfc_trans_where_2 (c, NULL, NULL, nested_forall_info, &block, &temp);

          while (temp)
            {
              tree args;
              temporary_list *p;

              /* Free the temporary.  */
              args = gfc_chainon_list (NULL_TREE, temp->temporary);
              tmp = gfc_build_function_call (gfor_fndecl_internal_free, args);
              gfc_add_expr_to_block (&block, tmp);

              p = temp;
              temp = temp->next;
              gfc_free (p);
            }

          break;

        /* Pointer assignment inside FORALL.  */
	case EXEC_POINTER_ASSIGN:
          need_temp = gfc_check_dependency (c->expr, c->expr2, varexpr, nvar);
          if (need_temp)
            gfc_trans_pointer_assign_need_temp (c->expr, c->expr2,
                                                nested_forall_info, &block);
          else
            {
              /* Use the normal assignment copying routines.  */
              assign = gfc_trans_pointer_assignment (c->expr, c->expr2);

              /* Reset the mask index.  */
              if (mask)
                gfc_add_modify_expr (&block, maskindex, gfc_index_zero_node);

              /* Generate body and loops.  */
              tmp = gfc_trans_nested_forall_loop (nested_forall_info, assign,
                                                  1, 1);
              gfc_add_expr_to_block (&block, tmp);
            }
          break;

	case EXEC_FORALL:
	  tmp = gfc_trans_forall_1 (c, nested_forall_info);
          gfc_add_expr_to_block (&block, tmp);
          break;

	default:
	  gcc_unreachable ();
	}

      c = c->next;
    }

  /* Restore the original index variables.  */
  for (fa = code->ext.forall_iterator, n = 0; fa; fa = fa->next, n++)
    gfc_restore_sym (fa->var->symtree->n.sym, &saved_vars[n]);

  /* Free the space for var, start, end, step, varexpr.  */
  gfc_free (var);
  gfc_free (start);
  gfc_free (end);
  gfc_free (step);
  gfc_free (varexpr);
  gfc_free (saved_vars);

  if (pmask)
    {
      /* Free the temporary for the mask.  */
      tmp = gfc_chainon_list (NULL_TREE, pmask);
      tmp = gfc_build_function_call (gfor_fndecl_internal_free, tmp);
      gfc_add_expr_to_block (&block, tmp);
    }
  if (maskindex)
    pushdecl (maskindex);

  return gfc_finish_block (&block);
}


/* Translate the FORALL statement or construct.  */

tree gfc_trans_forall (gfc_code * code)
{
  return gfc_trans_forall_1 (code, NULL);
}


/* Evaluate the WHERE mask expression, copy its value to a temporary.
   If the WHERE construct is nested in FORALL, compute the overall temporary
   needed by the WHERE mask expression multiplied by the iterator number of
   the nested forall.
   ME is the WHERE mask expression.
   MASK is the temporary which value is mask's value.
   NMASK is another temporary which value is !mask.
   TEMP records the temporary's address allocated in this function in order to
   free them outside this function.
   MASK, NMASK and TEMP are all OUT arguments.  */

static tree
gfc_evaluate_where_mask (gfc_expr * me, forall_info * nested_forall_info,
                         tree * mask, tree * nmask, temporary_list ** temp,
                         stmtblock_t * block)
{
  tree tmp, tmp1;
  gfc_ss *lss, *rss;
  gfc_loopinfo loop;
  tree ptemp1, ntmp, ptemp2;
  tree inner_size, size;
  stmtblock_t body, body1, inner_size_body;
  gfc_se lse, rse;
  tree count;
  tree tmpexpr;

  gfc_init_loopinfo (&loop);

  /* Calculate the size of temporary needed by the mask-expr.  */
  gfc_init_block (&inner_size_body);
  inner_size = compute_inner_temp_size (me, me, &inner_size_body, &lss, &rss);

  /* Calculate the total size of temporary needed.  */
  size = compute_overall_iter_number (nested_forall_info, inner_size,
				      &inner_size_body, block);

  /* Allocate temporary for where mask.  */
  tmp = allocate_temp_for_forall_nest_1 (boolean_type_node, size, block,
					 &ptemp1);
  /* Record the temporary address in order to free it later.  */
  if (ptemp1)
    {
      temporary_list *tempo;
      tempo = (temporary_list *) gfc_getmem (sizeof (temporary_list));
      tempo->temporary = ptemp1;
      tempo->next = *temp;
      *temp = tempo;
    }

  /* Allocate temporary for !mask.  */
  ntmp = allocate_temp_for_forall_nest_1 (boolean_type_node, size, block,
					  &ptemp2);
  /* Record the temporary  in order to free it later.  */
  if (ptemp2)
    {
      temporary_list *tempo;
      tempo = (temporary_list *) gfc_getmem (sizeof (temporary_list));
      tempo->temporary = ptemp2;
      tempo->next = *temp;
      *temp = tempo;
    }

  /* Variable to index the temporary.  */
  count = gfc_create_var (gfc_array_index_type, "count");
  /* Initialize count.  */
  gfc_add_modify_expr (block, count, gfc_index_zero_node);

  gfc_start_block (&body);

  gfc_init_se (&rse, NULL);
  gfc_init_se (&lse, NULL);

  if (lss == gfc_ss_terminator)
    {
      gfc_init_block (&body1);
    }
  else
    {
      /* Initialize the loop.  */
      gfc_init_loopinfo (&loop);

      /* We may need LSS to determine the shape of the expression.  */
      gfc_add_ss_to_loop (&loop, lss);
      gfc_add_ss_to_loop (&loop, rss);

      gfc_conv_ss_startstride (&loop);
      gfc_conv_loop_setup (&loop);

      gfc_mark_ss_chain_used (rss, 1);
      /* Start the loop body.  */
      gfc_start_scalarized_body (&loop, &body1);

      /* Translate the expression.  */
      gfc_copy_loopinfo_to_se (&rse, &loop);
      rse.ss = rss;
      gfc_conv_expr (&rse, me);
    }
  /* Form the expression of the temporary.  */
  lse.expr = gfc_build_array_ref (tmp, count);
  tmpexpr = gfc_build_array_ref (ntmp, count);

  /* Use the scalar assignment to fill temporary TMP.  */
  tmp1 = gfc_trans_scalar_assign (&lse, &rse, me->ts.type);
  gfc_add_expr_to_block (&body1, tmp1);

  /* Fill temporary NTMP.  */
  tmp1 = build1 (TRUTH_NOT_EXPR, TREE_TYPE (lse.expr), lse.expr);
  gfc_add_modify_expr (&body1, tmpexpr, tmp1);

 if (lss == gfc_ss_terminator)
    {
      gfc_add_block_to_block (&body, &body1);
    }
  else
    {
      /* Increment count.  */
      tmp1 = fold_build2 (PLUS_EXPR, gfc_array_index_type, count,
                          gfc_index_one_node);
      gfc_add_modify_expr (&body1, count, tmp1);

      /* Generate the copying loops.  */
      gfc_trans_scalarizing_loops (&loop, &body1);

      gfc_add_block_to_block (&body, &loop.pre);
      gfc_add_block_to_block (&body, &loop.post);

      gfc_cleanup_loop (&loop);
      /* TODO: Reuse lss and rss when copying temp->lhs.  Need to be careful
         as tree nodes in SS may not be valid in different scope.  */
    }

  tmp1 = gfc_finish_block (&body);
  /* If the WHERE construct is inside FORALL, fill the full temporary.  */
  if (nested_forall_info != NULL)
    {
      forall_info *forall_tmp;
      tree maskindex;

      /* Initialize the maskindexes.  */
      forall_tmp = nested_forall_info;
      while (forall_tmp != NULL)
	{
	  maskindex = forall_tmp->maskindex;
	  if (forall_tmp->mask)
	    gfc_add_modify_expr (block, maskindex, gfc_index_zero_node);
	  forall_tmp = forall_tmp->next_nest;
	}

      tmp1 = gfc_trans_nested_forall_loop (nested_forall_info, tmp1, 1, 1);
    }

  gfc_add_expr_to_block (block, tmp1);

  *mask = tmp;
  *nmask = ntmp;

  return tmp1;
}


/* Translate an assignment statement in a WHERE statement or construct
   statement. The MASK expression is used to control which elements
   of EXPR1 shall be assigned.  */

static tree
gfc_trans_where_assign (gfc_expr *expr1, gfc_expr *expr2, tree mask,
                        tree count1, tree count2)
{
  gfc_se lse;
  gfc_se rse;
  gfc_ss *lss;
  gfc_ss *lss_section;
  gfc_ss *rss;

  gfc_loopinfo loop;
  tree tmp;
  stmtblock_t block;
  stmtblock_t body;
  tree index, maskexpr, tmp1;

#if 0
  /* TODO: handle this special case.
     Special case a single function returning an array.  */
  if (expr2->expr_type == EXPR_FUNCTION && expr2->rank > 0)
    {
      tmp = gfc_trans_arrayfunc_assign (expr1, expr2);
      if (tmp)
        return tmp;
    }
#endif

 /* Assignment of the form lhs = rhs.  */
  gfc_start_block (&block);

  gfc_init_se (&lse, NULL);
  gfc_init_se (&rse, NULL);

  /* Walk the lhs.  */
  lss = gfc_walk_expr (expr1);
  rss = NULL;

  /* In each where-assign-stmt, the mask-expr and the variable being
     defined shall be arrays of the same shape.  */
  gcc_assert (lss != gfc_ss_terminator);

  /* The assignment needs scalarization.  */
  lss_section = lss;

  /* Find a non-scalar SS from the lhs.  */
  while (lss_section != gfc_ss_terminator
         && lss_section->type != GFC_SS_SECTION)
    lss_section = lss_section->next;

  gcc_assert (lss_section != gfc_ss_terminator);

  /* Initialize the scalarizer.  */
  gfc_init_loopinfo (&loop);

  /* Walk the rhs.  */
  rss = gfc_walk_expr (expr2);
  if (rss == gfc_ss_terminator)
   {
     /* The rhs is scalar.  Add a ss for the expression.  */
     rss = gfc_get_ss ();
     rss->next = gfc_ss_terminator;
     rss->type = GFC_SS_SCALAR;
     rss->expr = expr2;
    }

  /* Associate the SS with the loop.  */
  gfc_add_ss_to_loop (&loop, lss);
  gfc_add_ss_to_loop (&loop, rss);

  /* Calculate the bounds of the scalarization.  */
  gfc_conv_ss_startstride (&loop);

  /* Resolve any data dependencies in the statement.  */
  gfc_conv_resolve_dependencies (&loop, lss_section, rss);

  /* Setup the scalarizing loops.  */
  gfc_conv_loop_setup (&loop);

  /* Setup the gfc_se structures.  */
  gfc_copy_loopinfo_to_se (&lse, &loop);
  gfc_copy_loopinfo_to_se (&rse, &loop);

  rse.ss = rss;
  gfc_mark_ss_chain_used (rss, 1);
  if (loop.temp_ss == NULL)
    {
      lse.ss = lss;
      gfc_mark_ss_chain_used (lss, 1);
    }
  else
    {
      lse.ss = loop.temp_ss;
      gfc_mark_ss_chain_used (lss, 3);
      gfc_mark_ss_chain_used (loop.temp_ss, 3);
    }

  /* Start the scalarized loop body.  */
  gfc_start_scalarized_body (&loop, &body);

  /* Translate the expression.  */
  gfc_conv_expr (&rse, expr2);
  if (lss != gfc_ss_terminator && loop.temp_ss != NULL)
    {
      gfc_conv_tmp_array_ref (&lse);
      gfc_advance_se_ss_chain (&lse);
    }
  else
    gfc_conv_expr (&lse, expr1);

  /* Form the mask expression according to the mask tree list.  */
  index = count1;
  tmp = mask;
  if (tmp != NULL)
    maskexpr = gfc_build_array_ref (tmp, index);
  else
    maskexpr = NULL;

  tmp = TREE_CHAIN (tmp);
  while (tmp)
    {
      tmp1 = gfc_build_array_ref (tmp, index);
      maskexpr = build2 (TRUTH_AND_EXPR, TREE_TYPE (tmp1), maskexpr, tmp1);
      tmp = TREE_CHAIN (tmp);
    }
  /* Use the scalar assignment as is.  */
  tmp = gfc_trans_scalar_assign (&lse, &rse, expr1->ts.type);
  tmp = build3_v (COND_EXPR, maskexpr, tmp, build_empty_stmt ());

  gfc_add_expr_to_block (&body, tmp);

  if (lss == gfc_ss_terminator)
    {
      /* Increment count1.  */
      tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			 count1, gfc_index_one_node);
      gfc_add_modify_expr (&body, count1, tmp);

      /* Use the scalar assignment as is.  */
      gfc_add_block_to_block (&block, &body);
    }
  else
    {
      gcc_assert (lse.ss == gfc_ss_terminator
		  && rse.ss == gfc_ss_terminator);

      if (loop.temp_ss != NULL)
        {
          /* Increment count1 before finish the main body of a scalarized
             expression.  */
          tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			     count1, gfc_index_one_node);
          gfc_add_modify_expr (&body, count1, tmp);
          gfc_trans_scalarized_loop_boundary (&loop, &body);

          /* We need to copy the temporary to the actual lhs.  */
          gfc_init_se (&lse, NULL);
          gfc_init_se (&rse, NULL);
          gfc_copy_loopinfo_to_se (&lse, &loop);
          gfc_copy_loopinfo_to_se (&rse, &loop);

          rse.ss = loop.temp_ss;
          lse.ss = lss;

          gfc_conv_tmp_array_ref (&rse);
          gfc_advance_se_ss_chain (&rse);
          gfc_conv_expr (&lse, expr1);

          gcc_assert (lse.ss == gfc_ss_terminator
		      && rse.ss == gfc_ss_terminator);

          /* Form the mask expression according to the mask tree list.  */
          index = count2;
          tmp = mask;
          if (tmp != NULL)
            maskexpr = gfc_build_array_ref (tmp, index);
          else
            maskexpr = NULL;

          tmp = TREE_CHAIN (tmp);
          while (tmp)
            {
              tmp1 = gfc_build_array_ref (tmp, index);
              maskexpr = build2 (TRUTH_AND_EXPR, TREE_TYPE (tmp1),
				 maskexpr, tmp1);
              tmp = TREE_CHAIN (tmp);
            }
          /* Use the scalar assignment as is.  */
          tmp = gfc_trans_scalar_assign (&lse, &rse, expr1->ts.type);
          tmp = build3_v (COND_EXPR, maskexpr, tmp, build_empty_stmt ());
          gfc_add_expr_to_block (&body, tmp);

          /* Increment count2.  */
          tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			     count2, gfc_index_one_node);
          gfc_add_modify_expr (&body, count2, tmp);
        }
      else
        {
          /* Increment count1.  */
          tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			     count1, gfc_index_one_node);
          gfc_add_modify_expr (&body, count1, tmp);
        }

      /* Generate the copying loops.  */
      gfc_trans_scalarizing_loops (&loop, &body);

      /* Wrap the whole thing up.  */
      gfc_add_block_to_block (&block, &loop.pre);
      gfc_add_block_to_block (&block, &loop.post);
      gfc_cleanup_loop (&loop);
    }

  return gfc_finish_block (&block);
}


/* Translate the WHERE construct or statement.
   This function can be called iteratively to translate the nested WHERE
   construct or statement.
   MASK is the control mask, and PMASK is the pending control mask.
   TEMP records the temporary address which must be freed later.  */

static void
gfc_trans_where_2 (gfc_code * code, tree mask, tree pmask,
                   forall_info * nested_forall_info, stmtblock_t * block,
                   temporary_list ** temp)
{
  gfc_expr *expr1;
  gfc_expr *expr2;
  gfc_code *cblock;
  gfc_code *cnext;
  tree tmp, tmp1, tmp2;
  tree count1, count2;
  tree mask_copy;
  int need_temp;

  /* the WHERE statement or the WHERE construct statement.  */
  cblock = code->block;
  while (cblock)
    {
      /* Has mask-expr.  */
      if (cblock->expr)
        {
          /* Ensure that the WHERE mask be evaluated only once.  */
          tmp2 = gfc_evaluate_where_mask (cblock->expr, nested_forall_info,
                                          &tmp, &tmp1, temp, block);

          /* Set the control mask and the pending control mask.  */
          /* It's a where-stmt.  */
          if (mask == NULL)
            {
              mask = tmp;
              pmask = tmp1;
            }
          /* It's a nested where-stmt.  */
          else if (mask && pmask == NULL)
            {
              tree tmp2;
              /* Use the TREE_CHAIN to list the masks.  */
              tmp2 = copy_list (mask);
              pmask = chainon (mask, tmp1);
              mask = chainon (tmp2, tmp);
            }
          /* It's a masked-elsewhere-stmt.  */
          else if (mask && cblock->expr)
            {
              tree tmp2;
              tmp2 = copy_list (pmask);

              mask = pmask;
              tmp2 = chainon (tmp2, tmp);
              pmask = chainon (mask, tmp1);
              mask = tmp2;
            }
        }
      /* It's a elsewhere-stmt. No mask-expr is present.  */
      else
        mask = pmask;

      /* Get the assignment statement of a WHERE statement, or the first
         statement in where-body-construct of a WHERE construct.  */
      cnext = cblock->next;
      while (cnext)
        {
          switch (cnext->op)
            {
            /* WHERE assignment statement.  */
            case EXEC_ASSIGN:
              expr1 = cnext->expr;
              expr2 = cnext->expr2;
              if (nested_forall_info != NULL)
                {
                  int nvar;
                  gfc_expr **varexpr;

                  nvar = nested_forall_info->nvar;
                  varexpr = (gfc_expr **)
                            gfc_getmem (nvar * sizeof (gfc_expr *));
                  need_temp = gfc_check_dependency (expr1, expr2, varexpr,
                                                    nvar);
                  if (need_temp)
                    gfc_trans_assign_need_temp (expr1, expr2, mask,
                                                nested_forall_info, block);
                  else
                    {
		      forall_info *forall_tmp;
		      tree maskindex;

                      /* Variables to control maskexpr.  */
                      count1 = gfc_create_var (gfc_array_index_type, "count1");
                      count2 = gfc_create_var (gfc_array_index_type, "count2");
                      gfc_add_modify_expr (block, count1, gfc_index_zero_node);
                      gfc_add_modify_expr (block, count2, gfc_index_zero_node);

                      tmp = gfc_trans_where_assign (expr1, expr2, mask, count1,
                                                    count2);

		      /* Initialize the maskindexes.  */
		      forall_tmp = nested_forall_info;
		      while (forall_tmp != NULL)
			{
			  maskindex = forall_tmp->maskindex;
			  if (forall_tmp->mask)
			    gfc_add_modify_expr (block, maskindex,
						 gfc_index_zero_node);
			  forall_tmp = forall_tmp->next_nest;
			}

                      tmp = gfc_trans_nested_forall_loop (nested_forall_info,
                                                          tmp, 1, 1);
                      gfc_add_expr_to_block (block, tmp);
                    }
                }
              else
                {
                  /* Variables to control maskexpr.  */
                  count1 = gfc_create_var (gfc_array_index_type, "count1");
                  count2 = gfc_create_var (gfc_array_index_type, "count2");
                  gfc_add_modify_expr (block, count1, gfc_index_zero_node);
                  gfc_add_modify_expr (block, count2, gfc_index_zero_node);

                  tmp = gfc_trans_where_assign (expr1, expr2, mask, count1,
                                                count2);
                  gfc_add_expr_to_block (block, tmp);

                }
              break;

            /* WHERE or WHERE construct is part of a where-body-construct.  */
            case EXEC_WHERE:
              /* Ensure that MASK is not modified by next gfc_trans_where_2.  */
              mask_copy = copy_list (mask);
              gfc_trans_where_2 (cnext, mask_copy, NULL, nested_forall_info,
                                 block, temp);
              break;

            default:
              gcc_unreachable ();
            }

         /* The next statement within the same where-body-construct.  */
         cnext = cnext->next;
       }
    /* The next masked-elsewhere-stmt, elsewhere-stmt, or end-where-stmt.  */
    cblock = cblock->block;
  }
}


/* As the WHERE or WHERE construct statement can be nested, we call
   gfc_trans_where_2 to do the translation, and pass the initial
   NULL values for both the control mask and the pending control mask.  */

tree
gfc_trans_where (gfc_code * code)
{
  stmtblock_t block;
  temporary_list *temp, *p;
  tree args;
  tree tmp;

  gfc_start_block (&block);
  temp = NULL;

  gfc_trans_where_2 (code, NULL, NULL, NULL, &block, &temp);

  /* Add calls to free temporaries which were dynamically allocated.  */
  while (temp)
    {
      args = gfc_chainon_list (NULL_TREE, temp->temporary);
      tmp = gfc_build_function_call (gfor_fndecl_internal_free, args);
      gfc_add_expr_to_block (&block, tmp);

      p = temp;
      temp = temp->next;
      gfc_free (p);
    }
  return gfc_finish_block (&block);
}


/* CYCLE a DO loop. The label decl has already been created by
   gfc_trans_do(), it's in TREE_PURPOSE (backend_decl) of the gfc_code
   node at the head of the loop. We must mark the label as used.  */

tree
gfc_trans_cycle (gfc_code * code)
{
  tree cycle_label;

  cycle_label = TREE_PURPOSE (code->ext.whichloop->backend_decl);
  TREE_USED (cycle_label) = 1;
  return build1_v (GOTO_EXPR, cycle_label);
}


/* EXIT a DO loop. Similar to CYCLE, but now the label is in
   TREE_VALUE (backend_decl) of the gfc_code node at the head of the
   loop.  */

tree
gfc_trans_exit (gfc_code * code)
{
  tree exit_label;

  exit_label = TREE_VALUE (code->ext.whichloop->backend_decl);
  TREE_USED (exit_label) = 1;
  return build1_v (GOTO_EXPR, exit_label);
}


/* Translate the ALLOCATE statement.  */

tree
gfc_trans_allocate (gfc_code * code)
{
  gfc_alloc *al;
  gfc_expr *expr;
  gfc_se se;
  tree tmp;
  tree parm;
  gfc_ref *ref;
  tree stat;
  tree pstat;
  tree error_label;
  stmtblock_t block;

  if (!code->ext.alloc_list)
    return NULL_TREE;

  gfc_start_block (&block);

  if (code->expr)
    {
      tree gfc_int4_type_node = gfc_get_int_type (4);

      stat = gfc_create_var (gfc_int4_type_node, "stat");
      pstat = gfc_build_addr_expr (NULL, stat);

      error_label = gfc_build_label_decl (NULL_TREE);
      TREE_USED (error_label) = 1;
    }
  else
    {
      pstat = integer_zero_node;
      stat = error_label = NULL_TREE;
    }


  for (al = code->ext.alloc_list; al != NULL; al = al->next)
    {
      expr = al->expr;

      gfc_init_se (&se, NULL);
      gfc_start_block (&se.pre);

      se.want_pointer = 1;
      se.descriptor_only = 1;
      gfc_conv_expr (&se, expr);

      ref = expr->ref;

      /* Find the last reference in the chain.  */
      while (ref && ref->next != NULL)
	{
	  gcc_assert (ref->type != REF_ARRAY || ref->u.ar.type == AR_ELEMENT);
	  ref = ref->next;
	}

      if (ref != NULL && ref->type == REF_ARRAY)
	{
	  /* An array.  */
	  gfc_array_allocate (&se, ref, pstat);
	}
      else
	{
	  /* A scalar or derived type.  */
	  tree val;

	  val = gfc_create_var (ppvoid_type_node, "ptr");
	  tmp = gfc_build_addr_expr (ppvoid_type_node, se.expr);
	  gfc_add_modify_expr (&se.pre, val, tmp);

	  tmp = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (se.expr)));
	  parm = gfc_chainon_list (NULL_TREE, val);
	  parm = gfc_chainon_list (parm, tmp);
	  parm = gfc_chainon_list (parm, pstat);
	  tmp = gfc_build_function_call (gfor_fndecl_allocate, parm);
	  gfc_add_expr_to_block (&se.pre, tmp);

	  if (code->expr)
	    {
	      tmp = build1_v (GOTO_EXPR, error_label);
	      parm =
		build2 (NE_EXPR, boolean_type_node, stat, integer_zero_node);
	      tmp = build3_v (COND_EXPR, parm, tmp, build_empty_stmt ());
	      gfc_add_expr_to_block (&se.pre, tmp);
	    }
	}

      tmp = gfc_finish_block (&se.pre);
      gfc_add_expr_to_block (&block, tmp);
    }

  /* Assign the value to the status variable.  */
  if (code->expr)
    {
      tmp = build1_v (LABEL_EXPR, error_label);
      gfc_add_expr_to_block (&block, tmp);

      gfc_init_se (&se, NULL);
      gfc_conv_expr_lhs (&se, code->expr);
      tmp = convert (TREE_TYPE (se.expr), stat);
      gfc_add_modify_expr (&block, se.expr, tmp);
    }

  return gfc_finish_block (&block);
}


/* Translate a DEALLOCATE statement.
   There are two cases within the for loop:
   (1) deallocate(a1, a2, a3) is translated into the following sequence
       _gfortran_deallocate(a1, 0B)
       _gfortran_deallocate(a2, 0B)
       _gfortran_deallocate(a3, 0B)
       where the STAT= variable is passed a NULL pointer.
   (2) deallocate(a1, a2, a3, stat=i) is translated into the following
       astat = 0
       _gfortran_deallocate(a1, &stat)
       astat = astat + stat
       _gfortran_deallocate(a2, &stat)
       astat = astat + stat
       _gfortran_deallocate(a3, &stat)
       astat = astat + stat
    In case (1), we simply return at the end of the for loop.  In case (2)
    we set STAT= astat.  */
tree
gfc_trans_deallocate (gfc_code * code)
{
  gfc_se se;
  gfc_alloc *al;
  gfc_expr *expr;
  tree apstat, astat, parm, pstat, stat, tmp, type, var;
  stmtblock_t block;

  gfc_start_block (&block);

  /* Set up the optional STAT= */
  if (code->expr)
    {
      tree gfc_int4_type_node = gfc_get_int_type (4);

      /* Variable used with the library call.  */
      stat = gfc_create_var (gfc_int4_type_node, "stat");
      pstat = gfc_build_addr_expr (NULL, stat);

      /* Running total of possible deallocation failures.  */
      astat = gfc_create_var (gfc_int4_type_node, "astat");
      apstat = gfc_build_addr_expr (NULL, astat);

      /* Initialize astat to 0.  */
      gfc_add_modify_expr (&block, astat, build_int_cst (TREE_TYPE (astat), 0));
    }
  else
    {
      pstat = apstat = null_pointer_node;
      stat = astat = NULL_TREE;
    }

  for (al = code->ext.alloc_list; al != NULL; al = al->next)
    {
      expr = al->expr;
      gcc_assert (expr->expr_type == EXPR_VARIABLE);

      gfc_init_se (&se, NULL);
      gfc_start_block (&se.pre);

      se.want_pointer = 1;
      se.descriptor_only = 1;
      gfc_conv_expr (&se, expr);

      if (expr->symtree->n.sym->attr.dimension)
	tmp = gfc_array_deallocate (se.expr, pstat);
      else
	{
	  type = build_pointer_type (TREE_TYPE (se.expr));
	  var = gfc_create_var (type, "ptr");
	  tmp = gfc_build_addr_expr (type, se.expr);
	  gfc_add_modify_expr (&se.pre, var, tmp);

	  parm = gfc_chainon_list (NULL_TREE, var);
	  parm = gfc_chainon_list (parm, pstat);
	  tmp = gfc_build_function_call (gfor_fndecl_deallocate, parm);
	}

      gfc_add_expr_to_block (&se.pre, tmp);

      /* Keep track of the number of failed deallocations by adding stat
	 of the last deallocation to the running total.  */
      if (code->expr)
	{
	  apstat = build2 (PLUS_EXPR, TREE_TYPE (stat), astat, stat);
	  gfc_add_modify_expr (&se.pre, astat, apstat);
	}

      tmp = gfc_finish_block (&se.pre);
      gfc_add_expr_to_block (&block, tmp);

    }

  /* Assign the value to the status variable.  */
  if (code->expr)
    {
      gfc_init_se (&se, NULL);
      gfc_conv_expr_lhs (&se, code->expr);
      tmp = convert (TREE_TYPE (se.expr), astat);
      gfc_add_modify_expr (&block, se.expr, tmp);
    }

  return gfc_finish_block (&block);
}

