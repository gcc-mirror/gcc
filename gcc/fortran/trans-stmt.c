/* Statement translation -- generate GCC trees from gfc_code.
   Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>
   and Steven Bosscher <s.bosscher@student.tudelft.nl>

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
#include "tree.h"
#include "gimple.h"
#include "ggc.h"
#include "toplev.h"
#include "real.h"
#include "gfortran.h"
#include "flags.h"
#include "trans.h"
#include "trans-stmt.h"
#include "trans-types.h"
#include "trans-array.h"
#include "trans-const.h"
#include "arith.h"
#include "dependency.h"

typedef struct iter_info
{
  tree var;
  tree start;
  tree end;
  tree step;
  struct iter_info *next;
}
iter_info;

typedef struct forall_info
{
  iter_info *this_loop;
  tree mask;
  tree maskindex;
  int nvar;
  tree size;
  struct forall_info  *prev_nest;
}
forall_info;

static void gfc_trans_where_2 (gfc_code *, tree, bool,
			       forall_info *, stmtblock_t *);

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
  /* Deals with dummy argument. Get the parameter declaration.  */
  else if (TREE_CODE (se->expr) == INDIRECT_REF)
    se->expr = TREE_OPERAND (se->expr, 0);
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
  int label_len;

  /* Start a new block.  */
  gfc_init_se (&se, NULL);
  gfc_start_block (&se.pre);
  gfc_conv_label_variable (&se, code->expr1);

  len = GFC_DECL_STRING_LEN (se.expr);
  addr = GFC_DECL_ASSIGN_ADDR (se.expr);

  label_tree = gfc_get_label_decl (code->label1);

  if (code->label1->defined == ST_LABEL_TARGET)
    {
      label_tree = gfc_build_addr_expr (pvoid_type_node, label_tree);
      len_tree = integer_minus_one_node;
    }
  else
    {
      gfc_expr *format = code->label1->format;

      label_len = format->value.character.length;
      len_tree = build_int_cst (NULL_TREE, label_len);
      label_tree = gfc_build_wide_string_const (format->ts.kind, label_len + 1,
						format->value.character.string);
      label_tree = gfc_build_addr_expr (pvoid_type_node, label_tree);
    }

  gfc_add_modify (&se.pre, len, len_tree);
  gfc_add_modify (&se.pre, addr, label_tree);

  return gfc_finish_block (&se.pre);
}

/* Translate a GOTO statement.  */

tree
gfc_trans_goto (gfc_code * code)
{
  locus loc = code->loc;
  tree assigned_goto;
  tree target;
  tree tmp;
  gfc_se se;

  if (code->label1 != NULL)
    return build1_v (GOTO_EXPR, gfc_get_label_decl (code->label1));

  /* ASSIGNED GOTO.  */
  gfc_init_se (&se, NULL);
  gfc_start_block (&se.pre);
  gfc_conv_label_variable (&se, code->expr1);
  tmp = GFC_DECL_STRING_LEN (se.expr);
  tmp = fold_build2 (NE_EXPR, boolean_type_node, tmp,
		     build_int_cst (TREE_TYPE (tmp), -1));
  gfc_trans_runtime_check (true, false, tmp, &se.pre, &loc,
			   "Assigned label is not a target label");

  assigned_goto = GFC_DECL_ASSIGN_ADDR (se.expr);

  /* We're going to ignore a label list.  It does not really change the
     statement's semantics (because it is just a further restriction on
     what's legal code); before, we were comparing label addresses here, but
     that's a very fragile business and may break with optimization.  So
     just ignore it.  */

  target = fold_build1 (GOTO_EXPR, void_type_node, assigned_goto);
  gfc_add_expr_to_block (&se.pre, target);
  return gfc_finish_block (&se.pre);
}


/* Translate an ENTRY statement.  Just adds a label for this entry point.  */
tree
gfc_trans_entry (gfc_code * code)
{
  return build1_v (LABEL_EXPR, code->ext.entry->label);
}


/* Check for dependencies between INTENT(IN) and INTENT(OUT) arguments of
   elemental subroutines.  Make temporaries for output arguments if any such
   dependencies are found.  Output arguments are chosen because internal_unpack
   can be used, as is, to copy the result back to the variable.  */
static void
gfc_conv_elemental_dependencies (gfc_se * se, gfc_se * loopse,
				 gfc_symbol * sym, gfc_actual_arglist * arg,
				 gfc_dep_check check_variable)
{
  gfc_actual_arglist *arg0;
  gfc_expr *e;
  gfc_formal_arglist *formal;
  gfc_loopinfo tmp_loop;
  gfc_se parmse;
  gfc_ss *ss;
  gfc_ss_info *info;
  gfc_symbol *fsym;
  int n;
  tree data;
  tree offset;
  tree size;
  tree tmp;

  if (loopse->ss == NULL)
    return;

  ss = loopse->ss;
  arg0 = arg;
  formal = sym->formal;

  /* Loop over all the arguments testing for dependencies.  */
  for (; arg != NULL; arg = arg->next, formal = formal ? formal->next : NULL)
    {
      e = arg->expr;
      if (e == NULL)
	continue;

      /* Obtain the info structure for the current argument.  */ 
      info = NULL;
      for (ss = loopse->ss; ss && ss != gfc_ss_terminator; ss = ss->next)
	{
	  if (ss->expr != e)
	    continue;
	  info = &ss->data.info;
	  break;
	}

      /* If there is a dependency, create a temporary and use it
	 instead of the variable.  */
      fsym = formal ? formal->sym : NULL;
      if (e->expr_type == EXPR_VARIABLE
	    && e->rank && fsym
	    && fsym->attr.intent != INTENT_IN
	    && gfc_check_fncall_dependency (e, fsym->attr.intent,
					    sym, arg0, check_variable))
	{
	  tree initial, temptype;
	  stmtblock_t temp_post;

	  /* Make a local loopinfo for the temporary creation, so that
	     none of the other ss->info's have to be renormalized.  */
	  gfc_init_loopinfo (&tmp_loop);
	  for (n = 0; n < info->dimen; n++)
	    {
	      tmp_loop.to[n] = loopse->loop->to[n];
	      tmp_loop.from[n] = loopse->loop->from[n];
	      tmp_loop.order[n] = loopse->loop->order[n];
	    }

	  /* Obtain the argument descriptor for unpacking.  */
	  gfc_init_se (&parmse, NULL);
	  parmse.want_pointer = 1;
	  gfc_conv_expr_descriptor (&parmse, e, gfc_walk_expr (e));
	  gfc_add_block_to_block (&se->pre, &parmse.pre);

	  /* If we've got INTENT(INOUT) or a derived type with INTENT(OUT),
	     initialize the array temporary with a copy of the values.  */
	  if (fsym->attr.intent == INTENT_INOUT
		|| (fsym->ts.type ==BT_DERIVED
		      && fsym->attr.intent == INTENT_OUT))
	    initial = parmse.expr;
	  else
	    initial = NULL_TREE;

	  /* Find the type of the temporary to create; we don't use the type
	     of e itself as this breaks for subcomponent-references in e (where
	     the type of e is that of the final reference, but parmse.expr's
	     type corresponds to the full derived-type).  */
	  /* TODO: Fix this somehow so we don't need a temporary of the whole
	     array but instead only the components referenced.  */
	  temptype = TREE_TYPE (parmse.expr); /* Pointer to descriptor.  */
	  gcc_assert (TREE_CODE (temptype) == POINTER_TYPE);
	  temptype = TREE_TYPE (temptype);
	  temptype = gfc_get_element_type (temptype);

	  /* Generate the temporary.  Cleaning up the temporary should be the
	     very last thing done, so we add the code to a new block and add it
	     to se->post as last instructions.  */
	  size = gfc_create_var (gfc_array_index_type, NULL);
	  data = gfc_create_var (pvoid_type_node, NULL);
	  gfc_init_block (&temp_post);
	  tmp = gfc_trans_create_temp_array (&se->pre, &temp_post,
					     &tmp_loop, info, temptype,
					     initial,
					     false, true, false,
					     &arg->expr->where);
	  gfc_add_modify (&se->pre, size, tmp);
	  tmp = fold_convert (pvoid_type_node, info->data);
	  gfc_add_modify (&se->pre, data, tmp);

	  /* Calculate the offset for the temporary.  */
	  offset = gfc_index_zero_node;
	  for (n = 0; n < info->dimen; n++)
	    {
	      tmp = gfc_conv_descriptor_stride_get (info->descriptor,
						    gfc_rank_cst[n]);
	      tmp = fold_build2 (MULT_EXPR, gfc_array_index_type,
				 loopse->loop->from[n], tmp);
	      offset = fold_build2 (MINUS_EXPR, gfc_array_index_type,
				    offset, tmp);
	    }
	  info->offset = gfc_create_var (gfc_array_index_type, NULL);	  
	  gfc_add_modify (&se->pre, info->offset, offset);

	  /* Copy the result back using unpack.  */
	  tmp = build_call_expr_loc (input_location,
				 gfor_fndecl_in_unpack, 2, parmse.expr, data);
	  gfc_add_expr_to_block (&se->post, tmp);

	  /* parmse.pre is already added above.  */
	  gfc_add_block_to_block (&se->post, &parmse.post);
	  gfc_add_block_to_block (&se->post, &temp_post);
	}
    }
}


/* Translate the CALL statement.  Builds a call to an F95 subroutine.  */

tree
gfc_trans_call (gfc_code * code, bool dependency_check,
		tree mask, tree count1, bool invert)
{
  gfc_se se;
  gfc_ss * ss;
  int has_alternate_specifier;
  gfc_dep_check check_variable;
  tree index = NULL_TREE;
  tree maskexpr = NULL_TREE;
  tree tmp;

  /* A CALL starts a new block because the actual arguments may have to
     be evaluated first.  */
  gfc_init_se (&se, NULL);
  gfc_start_block (&se.pre);

  gcc_assert (code->resolved_sym);

  ss = gfc_ss_terminator;
  if (code->resolved_sym->attr.elemental)
    ss = gfc_walk_elemental_function_args (ss, code->ext.actual, GFC_SS_REFERENCE);

  /* Is not an elemental subroutine call with array valued arguments.  */
  if (ss == gfc_ss_terminator)
    {

      /* Translate the call.  */
      has_alternate_specifier
	= gfc_conv_procedure_call (&se, code->resolved_sym, code->ext.actual,
				  code->expr1, NULL_TREE);

      /* A subroutine without side-effect, by definition, does nothing!  */
      TREE_SIDE_EFFECTS (se.expr) = 1;

      /* Chain the pieces together and return the block.  */
      if (has_alternate_specifier)
	{
	  gfc_code *select_code;
	  gfc_symbol *sym;
	  select_code = code->next;
	  gcc_assert(select_code->op == EXEC_SELECT);
	  sym = select_code->expr1->symtree->n.sym;
	  se.expr = convert (gfc_typenode_for_spec (&sym->ts), se.expr);
	  if (sym->backend_decl == NULL)
	    sym->backend_decl = gfc_get_symbol_decl (sym);
	  gfc_add_modify (&se.pre, sym->backend_decl, se.expr);
	}
      else
	gfc_add_expr_to_block (&se.pre, se.expr);

      gfc_add_block_to_block (&se.pre, &se.post);
    }

  else
    {
      /* An elemental subroutine call with array valued arguments has
	 to be scalarized.  */
      gfc_loopinfo loop;
      stmtblock_t body;
      stmtblock_t block;
      gfc_se loopse;
      gfc_se depse;

      /* gfc_walk_elemental_function_args renders the ss chain in the
	 reverse order to the actual argument order.  */
      ss = gfc_reverse_ss (ss);

      /* Initialize the loop.  */
      gfc_init_se (&loopse, NULL);
      gfc_init_loopinfo (&loop);
      gfc_add_ss_to_loop (&loop, ss);

      gfc_conv_ss_startstride (&loop);
      /* TODO: gfc_conv_loop_setup generates a temporary for vector 
	 subscripts.  This could be prevented in the elemental case  
	 as temporaries are handled separatedly 
	 (below in gfc_conv_elemental_dependencies).  */
      gfc_conv_loop_setup (&loop, &code->expr1->where);
      gfc_mark_ss_chain_used (ss, 1);

      /* Convert the arguments, checking for dependencies.  */
      gfc_copy_loopinfo_to_se (&loopse, &loop);
      loopse.ss = ss;

      /* For operator assignment, do dependency checking.  */
      if (dependency_check)
	check_variable = ELEM_CHECK_VARIABLE;
      else
	check_variable = ELEM_DONT_CHECK_VARIABLE;

      gfc_init_se (&depse, NULL);
      gfc_conv_elemental_dependencies (&depse, &loopse, code->resolved_sym,
				       code->ext.actual, check_variable);

      gfc_add_block_to_block (&loop.pre,  &depse.pre);
      gfc_add_block_to_block (&loop.post, &depse.post);

      /* Generate the loop body.  */
      gfc_start_scalarized_body (&loop, &body);
      gfc_init_block (&block);

      if (mask && count1)
	{
	  /* Form the mask expression according to the mask.  */
	  index = count1;
	  maskexpr = gfc_build_array_ref (mask, index, NULL);
	  if (invert)
	    maskexpr = fold_build1 (TRUTH_NOT_EXPR, TREE_TYPE (maskexpr),
				    maskexpr);
	}

      /* Add the subroutine call to the block.  */
      gfc_conv_procedure_call (&loopse, code->resolved_sym,
			       code->ext.actual, code->expr1,
			       NULL_TREE);

      if (mask && count1)
	{
	  tmp = build3_v (COND_EXPR, maskexpr, loopse.expr,
			  build_empty_stmt (input_location));
	  gfc_add_expr_to_block (&loopse.pre, tmp);
	  tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			     count1, gfc_index_one_node);
	  gfc_add_modify (&loopse.pre, count1, tmp);
	}
      else
	gfc_add_expr_to_block (&loopse.pre, loopse.expr);

      gfc_add_block_to_block (&block, &loopse.pre);
      gfc_add_block_to_block (&block, &loopse.post);

      /* Finish up the loop block and the loop.  */
      gfc_add_expr_to_block (&body, gfc_finish_block (&block));
      gfc_trans_scalarizing_loops (&loop, &body);
      gfc_add_block_to_block (&se.pre, &loop.pre);
      gfc_add_block_to_block (&se.pre, &loop.post);
      gfc_add_block_to_block (&se.pre, &se.post);
      gfc_cleanup_loop (&loop);
    }

  return gfc_finish_block (&se.pre);
}


/* Translate the RETURN statement.  */

tree
gfc_trans_return (gfc_code * code ATTRIBUTE_UNUSED)
{
  if (code->expr1)
    {
      gfc_se se;
      tree tmp;
      tree result;

      /* If code->expr is not NULL, this return statement must appear
         in a subroutine and current_fake_result_decl has already
	 been generated.  */

      result = gfc_get_fake_result_decl (NULL, 0);
      if (!result)
        {
          gfc_warning ("An alternate return at %L without a * dummy argument",
                        &code->expr1->where);
          return build1_v (GOTO_EXPR, gfc_get_return_label ());
        }

      /* Start a new block for this statement.  */
      gfc_init_se (&se, NULL);
      gfc_start_block (&se.pre);

      gfc_conv_expr (&se, code->expr1);

      tmp = fold_build2 (MODIFY_EXPR, TREE_TYPE (result), result,
			 fold_convert (TREE_TYPE (result), se.expr));
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
  tree tmp;

  /* Start a new block for this statement.  */
  gfc_init_se (&se, NULL);
  gfc_start_block (&se.pre);


  if (code->expr1 == NULL)
    {
      tmp = build_int_cst (gfc_int4_type_node, code->ext.stop_code);
      tmp = build_call_expr_loc (input_location,
			     gfor_fndecl_pause_numeric, 1, tmp);
    }
  else
    {
      gfc_conv_expr_reference (&se, code->expr1);
      tmp = build_call_expr_loc (input_location,
			     gfor_fndecl_pause_string, 2,
			     se.expr, se.string_length);
    }

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
  tree tmp;

  /* Start a new block for this statement.  */
  gfc_init_se (&se, NULL);
  gfc_start_block (&se.pre);


  if (code->expr1 == NULL)
    {
      tmp = build_int_cst (gfc_int4_type_node, code->ext.stop_code);
      tmp = build_call_expr_loc (input_location,
			     gfor_fndecl_stop_numeric, 1, tmp);
    }
  else
    {
      gfc_conv_expr_reference (&se, code->expr1);
      tmp = build_call_expr_loc (input_location,
			     gfor_fndecl_stop_string, 2,
			     se.expr, se.string_length);
    }

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
  if (!code->expr1)
    return gfc_trans_code (code->next);

  /* Initialize a statement builder for each block. Puts in NULL_TREEs.  */
  gfc_init_se (&if_se, NULL);
  gfc_start_block (&if_se.pre);

  /* Calculate the IF condition expression.  */
  gfc_conv_expr_val (&if_se, code->expr1);

  /* Translate the THEN clause.  */
  stmt = gfc_trans_code (code->next);

  /* Translate the ELSE clause.  */
  if (code->block)
    elsestmt = gfc_trans_if_1 (code->block);
  else
    elsestmt = build_empty_stmt (input_location);

  /* Build the condition expression and add it to the condition block.  */
  stmt = fold_build3 (COND_EXPR, void_type_node, if_se.expr, stmt, elsestmt);
  
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


/* Translate an arithmetic IF expression.

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

   An optimized version can be generated in case of equal labels.
   E.g., if label1 is equal to label2, we can translate it to

    if (cond <= 0)
      goto label1;
    else
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
  gfc_conv_expr_val (&se, code->expr1);
  se.expr = gfc_evaluate_now (se.expr, &se.pre);

  /* Build something to compare with.  */
  zero = gfc_build_const (TREE_TYPE (se.expr), integer_zero_node);

  if (code->label1->value != code->label2->value)
    {
      /* If (cond < 0) take branch1 else take branch2.
         First build jumps to the COND .LT. 0 and the COND .EQ. 0 cases.  */
      branch1 = build1_v (GOTO_EXPR, gfc_get_label_decl (code->label1));
      branch2 = build1_v (GOTO_EXPR, gfc_get_label_decl (code->label2));

      if (code->label1->value != code->label3->value)
        tmp = fold_build2 (LT_EXPR, boolean_type_node, se.expr, zero);
      else
        tmp = fold_build2 (NE_EXPR, boolean_type_node, se.expr, zero);

      branch1 = fold_build3 (COND_EXPR, void_type_node, tmp, branch1, branch2);
    }
  else
    branch1 = build1_v (GOTO_EXPR, gfc_get_label_decl (code->label1));

  if (code->label1->value != code->label3->value
      && code->label2->value != code->label3->value)
    {
      /* if (cond <= 0) take branch1 else take branch2.  */
      branch2 = build1_v (GOTO_EXPR, gfc_get_label_decl (code->label3));
      tmp = fold_build2 (LE_EXPR, boolean_type_node, se.expr, zero);
      branch1 = fold_build3 (COND_EXPR, void_type_node, tmp, branch1, branch2);
    }

  /* Append the COND_EXPR to the evaluation of COND, and return.  */
  gfc_add_expr_to_block (&se.pre, branch1);
  return gfc_finish_block (&se.pre);
}


/* Translate a BLOCK construct.  This is basically what we would do for a
   procedure body.  */

tree
gfc_trans_block_construct (gfc_code* code)
{
  gfc_namespace* ns;
  gfc_symbol* sym;
  stmtblock_t body;
  tree tmp;

  ns = code->ext.ns;
  gcc_assert (ns);
  sym = ns->proc_name;
  gcc_assert (sym);

  gcc_assert (!sym->tlink);
  sym->tlink = sym;

  gfc_start_block (&body);
  gfc_process_block_locals (ns);

  tmp = gfc_trans_code (ns->code);
  tmp = gfc_trans_deferred_vars (sym, tmp);

  gfc_add_expr_to_block (&body, tmp);
  return gfc_finish_block (&body);
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
  tree saved_dovar = NULL;
  tree cycle_label;
  tree exit_label;
  
  type = TREE_TYPE (dovar);

  /* Initialize the DO variable: dovar = from.  */
  gfc_add_modify (pblock, dovar, from);
  
  /* Save value for do-tinkering checking. */
  if (gfc_option.rtcheck & GFC_RTCHECK_DO)
    {
      saved_dovar = gfc_create_var (type, ".saved_dovar");
      gfc_add_modify (pblock, saved_dovar, dovar);
    }

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

  /* Check whether someone has modified the loop variable. */
  if (gfc_option.rtcheck & GFC_RTCHECK_DO)
    {
      tmp = fold_build2 (NE_EXPR, boolean_type_node, dovar, saved_dovar);
      gfc_trans_runtime_check (true, false, tmp, &body, &code->loc,
			       "Loop variable has been modified");
    }

  /* Evaluate the loop condition.  */
  cond = fold_build2 (EQ_EXPR, boolean_type_node, dovar, to);
  cond = gfc_evaluate_now (cond, &body);

  /* Increment the loop variable.  */
  tmp = fold_build2 (PLUS_EXPR, type, dovar, step);
  gfc_add_modify (&body, dovar, tmp);

  if (gfc_option.rtcheck & GFC_RTCHECK_DO)
    gfc_add_modify (&body, saved_dovar, dovar);

  /* The loop exit.  */
  tmp = build1_v (GOTO_EXPR, exit_label);
  TREE_USED (exit_label) = 1;
  tmp = fold_build3 (COND_EXPR, void_type_node,
		     cond, tmp, build_empty_stmt (input_location));
  gfc_add_expr_to_block (&body, tmp);

  /* Finish the loop body.  */
  tmp = gfc_finish_block (&body);
  tmp = build1_v (LOOP_EXPR, tmp);

  /* Only execute the loop if the number of iterations is positive.  */
  if (tree_int_cst_sgn (step) > 0)
    cond = fold_build2 (LE_EXPR, boolean_type_node, dovar, to);
  else
    cond = fold_build2 (GE_EXPR, boolean_type_node, dovar, to);
  tmp = fold_build3 (COND_EXPR, void_type_node,
		     cond, tmp, build_empty_stmt (input_location));
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
   empty = (step > 0 ? to < from : to > from);
   countm1 = (to - from) / step;
   dovar = from;
   if (empty) goto exit_label;
   for (;;)
     {
       body;
cycle_label:
       dovar += step
       if (countm1 ==0) goto exit_label;
       countm1--;
     }
exit_label:

   countm1 is an unsigned integer.  It is equal to the loop count minus one,
   because the loop count itself can overflow.  */

tree
gfc_trans_do (gfc_code * code)
{
  gfc_se se;
  tree dovar;
  tree saved_dovar = NULL;
  tree from;
  tree to;
  tree step;
  tree countm1;
  tree type;
  tree utype;
  tree cond;
  tree cycle_label;
  tree exit_label;
  tree tmp;
  tree pos_step;
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

  if (gfc_option.rtcheck & GFC_RTCHECK_DO)
    {
      tmp = fold_build2 (EQ_EXPR, boolean_type_node, step,
			 fold_convert (type, integer_zero_node));
      gfc_trans_runtime_check (true, false, tmp, &block, &code->loc,
			       "DO step value is zero");
    }

  /* Special case simple loops.  */
  if (TREE_CODE (type) == INTEGER_TYPE
      && (integer_onep (step)
	|| tree_int_cst_equal (step, integer_minus_one_node)))
    return gfc_trans_simple_do (code, &block, dovar, from, to, step);

  pos_step = fold_build2 (GT_EXPR, boolean_type_node, step,
			  fold_convert (type, integer_zero_node));

  if (TREE_CODE (type) == INTEGER_TYPE)
    utype = unsigned_type_for (type);
  else
    utype = unsigned_type_for (gfc_array_index_type);
  countm1 = gfc_create_var (utype, "countm1");

  /* Cycle and exit statements are implemented with gotos.  */
  cycle_label = gfc_build_label_decl (NULL_TREE);
  exit_label = gfc_build_label_decl (NULL_TREE);
  TREE_USED (exit_label) = 1;

  /* Initialize the DO variable: dovar = from.  */
  gfc_add_modify (&block, dovar, from);

  /* Save value for do-tinkering checking. */
  if (gfc_option.rtcheck & GFC_RTCHECK_DO)
    {
      saved_dovar = gfc_create_var (type, ".saved_dovar");
      gfc_add_modify (&block, saved_dovar, dovar);
    }

  /* Initialize loop count and jump to exit label if the loop is empty.
     This code is executed before we enter the loop body. We generate:
     step_sign = sign(1,step);
     if (step > 0)
       {
	 if (to < from)
	   goto exit_label;
       }
     else
       {
	 if (to > from)
	   goto exit_label;
       }
       countm1 = (to*step_sign - from*step_sign) / (step*step_sign);

  */

  if (TREE_CODE (type) == INTEGER_TYPE)
    {
      tree pos, neg, step_sign, to2, from2, step2;

      /* Calculate SIGN (1,step), as (step < 0 ? -1 : 1)  */

      tmp = fold_build2 (LT_EXPR, boolean_type_node, step, 
			 build_int_cst (TREE_TYPE (step), 0));
      step_sign = fold_build3 (COND_EXPR, type, tmp, 
			       build_int_cst (type, -1), 
			       build_int_cst (type, 1));

      tmp = fold_build2 (LT_EXPR, boolean_type_node, to, from);
      pos = fold_build3 (COND_EXPR, void_type_node, tmp,
			 build1_v (GOTO_EXPR, exit_label),
			 build_empty_stmt (input_location));

      tmp = fold_build2 (GT_EXPR, boolean_type_node, to, from);
      neg = fold_build3 (COND_EXPR, void_type_node, tmp,
			 build1_v (GOTO_EXPR, exit_label),
			 build_empty_stmt (input_location));
      tmp = fold_build3 (COND_EXPR, void_type_node, pos_step, pos, neg);

      gfc_add_expr_to_block (&block, tmp);

      /* Calculate the loop count.  to-from can overflow, so
	 we cast to unsigned.  */

      to2 = fold_build2 (MULT_EXPR, type, step_sign, to);
      from2 = fold_build2 (MULT_EXPR, type, step_sign, from);
      step2 = fold_build2 (MULT_EXPR, type, step_sign, step);
      step2 = fold_convert (utype, step2);
      tmp = fold_build2 (MINUS_EXPR, type, to2, from2);
      tmp = fold_convert (utype, tmp);
      tmp = fold_build2 (TRUNC_DIV_EXPR, utype, tmp, step2);
      tmp = fold_build2 (MODIFY_EXPR, void_type_node, countm1, tmp);
      gfc_add_expr_to_block (&block, tmp);
    }
  else
    {
      /* TODO: We could use the same width as the real type.
	 This would probably cause more problems that it solves
	 when we implement "long double" types.  */

      tmp = fold_build2 (MINUS_EXPR, type, to, from);
      tmp = fold_build2 (RDIV_EXPR, type, tmp, step);
      tmp = fold_build1 (FIX_TRUNC_EXPR, utype, tmp);
      gfc_add_modify (&block, countm1, tmp);

      /* We need a special check for empty loops:
	 empty = (step > 0 ? to < from : to > from);  */
      tmp = fold_build3 (COND_EXPR, boolean_type_node, pos_step,
			 fold_build2 (LT_EXPR, boolean_type_node, to, from),
			 fold_build2 (GT_EXPR, boolean_type_node, to, from));
      /* If the loop is empty, go directly to the exit label.  */
      tmp = fold_build3 (COND_EXPR, void_type_node, tmp,
			 build1_v (GOTO_EXPR, exit_label),
			 build_empty_stmt (input_location));
      gfc_add_expr_to_block (&block, tmp);
    }

  /* Loop body.  */
  gfc_start_block (&body);

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

  /* Check whether someone has modified the loop variable. */
  if (gfc_option.rtcheck & GFC_RTCHECK_DO)
    {
      tmp = fold_build2 (NE_EXPR, boolean_type_node, dovar, saved_dovar);
      gfc_trans_runtime_check (true, false, tmp, &body, &code->loc,
			       "Loop variable has been modified");
    }

  /* Increment the loop variable.  */
  tmp = fold_build2 (PLUS_EXPR, type, dovar, step);
  gfc_add_modify (&body, dovar, tmp);

  if (gfc_option.rtcheck & GFC_RTCHECK_DO)
    gfc_add_modify (&body, saved_dovar, dovar);

  /* End with the loop condition.  Loop until countm1 == 0.  */
  cond = fold_build2 (EQ_EXPR, boolean_type_node, countm1,
		      build_int_cst (utype, 0));
  tmp = build1_v (GOTO_EXPR, exit_label);
  tmp = fold_build3 (COND_EXPR, void_type_node,
		     cond, tmp, build_empty_stmt (input_location));
  gfc_add_expr_to_block (&body, tmp);

  /* Decrement the loop count.  */
  tmp = fold_build2 (MINUS_EXPR, utype, countm1, build_int_cst (utype, 1));
  gfc_add_modify (&body, countm1, tmp);

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
  gfc_conv_expr_val (&cond, code->expr1);
  gfc_add_block_to_block (&block, &cond.pre);
  cond.expr = fold_build1 (TRUTH_NOT_EXPR, boolean_type_node, cond.expr);

  /* Build "IF (! cond) GOTO exit_label".  */
  tmp = build1_v (GOTO_EXPR, exit_label);
  TREE_USED (exit_label) = 1;
  tmp = fold_build3 (COND_EXPR, void_type_node,
		     cond.expr, tmp, build_empty_stmt (input_location));
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
  gfc_conv_expr_val (&se, code->expr1);
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
	      low = gfc_conv_mpz_to_tree (cp->low->value.integer,
					  cp->low->ts.kind);

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
		 high.  In the third case, we don't because the GCC middle
		 end represents a single case value by just letting high be
		 a NULL_TREE.  We can't do that because we need to be able
		 to represent unbounded cases.  */

	      if (!cp->low
		  || (cp->low
		      && mpz_cmp (cp->low->value.integer,
				  cp->high->value.integer) != 0))
		high = gfc_conv_mpz_to_tree (cp->high->value.integer,
					     cp->high->ts.kind);

	      /* Unbounded case.  */
	      if (!cp->low)
		low = TYPE_MIN_VALUE (TREE_TYPE (se.expr));
	    }

          /* Build a label.  */
          label = gfc_build_label_decl (NULL_TREE);

	  /* Add this case label.
             Add parameter 'label', make it match GCC backend.  */
	  tmp = fold_build3 (CASE_LABEL_EXPR, void_type_node,
			     low, high, label);
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
  gfc_conv_expr_val (&se, code->expr1);
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
      tree true_tree, false_tree, stmt;

      true_tree = build_empty_stmt (input_location);
      false_tree = build_empty_stmt (input_location);

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

      stmt = fold_build3 (COND_EXPR, void_type_node, se.expr,
			  true_tree, false_tree);
      gfc_add_expr_to_block (&block, stmt);
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
  tree init, node, end_label, tmp, type, case_num, label, fndecl;
  stmtblock_t block, body;
  gfc_case *cp, *d;
  gfc_code *c;
  gfc_se se;
  int n, k;

  /* The jump table types are stored in static variables to avoid
     constructing them from scratch every single time.  */
  static tree select_struct[2];
  static tree ss_string1[2], ss_string1_len[2];
  static tree ss_string2[2], ss_string2_len[2];
  static tree ss_target[2];

  tree pchartype = gfc_get_pchar_type (code->expr1->ts.kind);

  if (code->expr1->ts.kind == 1)
    k = 0;
  else if (code->expr1->ts.kind == 4)
    k = 1;
  else
    gcc_unreachable ();

  if (select_struct[k] == NULL)
    {
      select_struct[k] = make_node (RECORD_TYPE);

      if (code->expr1->ts.kind == 1)
	TYPE_NAME (select_struct[k]) = get_identifier ("_jump_struct_char1");
      else if (code->expr1->ts.kind == 4)
	TYPE_NAME (select_struct[k]) = get_identifier ("_jump_struct_char4");
      else
	gcc_unreachable ();

#undef ADD_FIELD
#define ADD_FIELD(NAME, TYPE)					\
  ss_##NAME[k] = gfc_add_field_to_struct				\
     (&(TYPE_FIELDS (select_struct[k])), select_struct[k],	\
      get_identifier (stringize(NAME)), TYPE)

      ADD_FIELD (string1, pchartype);
      ADD_FIELD (string1_len, gfc_charlen_type_node);

      ADD_FIELD (string2, pchartype);
      ADD_FIELD (string2_len, gfc_charlen_type_node);

      ADD_FIELD (target, integer_type_node);
#undef ADD_FIELD

      gfc_finish_type (select_struct[k]);
    }

  cp = code->block->ext.case_list;
  while (cp->left != NULL)
    cp = cp->left;

  n = 0;
  for (d = cp; d; d = d->right)
    d->n = n++;

  end_label = gfc_build_label_decl (NULL_TREE);

  /* Generate the body */
  gfc_start_block (&block);
  gfc_init_block (&body);

  for (c = code->block; c; c = c->block)
    {
      for (d = c->ext.case_list; d; d = d->next)
        {
	  label = gfc_build_label_decl (NULL_TREE);
	  tmp = fold_build3 (CASE_LABEL_EXPR, void_type_node,
			     build_int_cst (NULL_TREE, d->n),
			     build_int_cst (NULL_TREE, d->n), label);
          gfc_add_expr_to_block (&body, tmp);
        }

      tmp = gfc_trans_code (c->next);
      gfc_add_expr_to_block (&body, tmp);

      tmp = build1_v (GOTO_EXPR, end_label);
      gfc_add_expr_to_block (&body, tmp);
    }

  /* Generate the structure describing the branches */
  init = NULL_TREE;

  for(d = cp; d; d = d->right)
    {
      node = NULL_TREE;

      gfc_init_se (&se, NULL);

      if (d->low == NULL)
        {
          node = tree_cons (ss_string1[k], null_pointer_node, node);
          node = tree_cons (ss_string1_len[k], integer_zero_node, node);
        }
      else
        {
          gfc_conv_expr_reference (&se, d->low);

          node = tree_cons (ss_string1[k], se.expr, node);
          node = tree_cons (ss_string1_len[k], se.string_length, node);
        }

      if (d->high == NULL)
        {
          node = tree_cons (ss_string2[k], null_pointer_node, node);
          node = tree_cons (ss_string2_len[k], integer_zero_node, node);
        }
      else
        {
          gfc_init_se (&se, NULL);
          gfc_conv_expr_reference (&se, d->high);

          node = tree_cons (ss_string2[k], se.expr, node);
          node = tree_cons (ss_string2_len[k], se.string_length, node);
        }

      node = tree_cons (ss_target[k], build_int_cst (integer_type_node, d->n),
			node);

      tmp = build_constructor_from_list (select_struct[k], nreverse (node));
      init = tree_cons (NULL_TREE, tmp, init);
    }

  type = build_array_type (select_struct[k],
			   build_index_type (build_int_cst (NULL_TREE, n-1)));

  init = build_constructor_from_list (type, nreverse(init));
  TREE_CONSTANT (init) = 1;
  TREE_STATIC (init) = 1;
  /* Create a static variable to hold the jump table.  */
  tmp = gfc_create_var (type, "jumptable");
  TREE_CONSTANT (tmp) = 1;
  TREE_STATIC (tmp) = 1;
  TREE_READONLY (tmp) = 1;
  DECL_INITIAL (tmp) = init;
  init = tmp;

  /* Build the library call */
  init = gfc_build_addr_expr (pvoid_type_node, init);

  gfc_init_se (&se, NULL);
  gfc_conv_expr_reference (&se, code->expr1);

  gfc_add_block_to_block (&block, &se.pre);

  if (code->expr1->ts.kind == 1)
    fndecl = gfor_fndecl_select_string;
  else if (code->expr1->ts.kind == 4)
    fndecl = gfor_fndecl_select_string_char4;
  else
    gcc_unreachable ();

  tmp = build_call_expr_loc (input_location,
			 fndecl, 4, init, build_int_cst (NULL_TREE, n),
			 se.expr, se.string_length);
  case_num = gfc_create_var (integer_type_node, "case_num");
  gfc_add_modify (&block, case_num, tmp);

  gfc_add_block_to_block (&block, &se.post);

  tmp = gfc_finish_block (&body);
  tmp = build3_v (SWITCH_EXPR, case_num, tmp, NULL_TREE);
  gfc_add_expr_to_block (&block, tmp);

  tmp = build1_v (LABEL_EXPR, end_label);
  gfc_add_expr_to_block (&block, tmp);

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
  gcc_assert (code && code->expr1);

  /* Empty SELECT constructs are legal.  */
  if (code->block == NULL)
    return build_empty_stmt (input_location);

  /* Select the correct translation function.  */
  switch (code->expr1->ts.type)
    {
    case BT_LOGICAL:	return gfc_trans_logical_select (code);
    case BT_INTEGER:	return gfc_trans_integer_select (code);
    case BT_CHARACTER:	return gfc_trans_character_select (code);
    default:
      gfc_internal_error ("gfc_trans_select(): Bad type for case expr.");
      /* Not reached */
    }
}


/* Traversal function to substitute a replacement symtree if the symbol
   in the expression is the same as that passed.  f == 2 signals that
   that variable itself is not to be checked - only the references.
   This group of functions is used when the variable expression in a
   FORALL assignment has internal references.  For example:
		FORALL (i = 1:4) p(p(i)) = i
   The only recourse here is to store a copy of 'p' for the index
   expression.  */

static gfc_symtree *new_symtree;
static gfc_symtree *old_symtree;

static bool
forall_replace (gfc_expr *expr, gfc_symbol *sym, int *f)
{
  if (expr->expr_type != EXPR_VARIABLE)
    return false;

  if (*f == 2)
    *f = 1;
  else if (expr->symtree->n.sym == sym)
    expr->symtree = new_symtree;

  return false;
}

static void
forall_replace_symtree (gfc_expr *e, gfc_symbol *sym, int f)
{
  gfc_traverse_expr (e, sym, forall_replace, f);
}

static bool
forall_restore (gfc_expr *expr,
		gfc_symbol *sym ATTRIBUTE_UNUSED,
		int *f ATTRIBUTE_UNUSED)
{
  if (expr->expr_type != EXPR_VARIABLE)
    return false;

  if (expr->symtree == new_symtree)
    expr->symtree = old_symtree;

  return false;
}

static void
forall_restore_symtree (gfc_expr *e)
{
  gfc_traverse_expr (e, NULL, forall_restore, 0);
}

static void
forall_make_variable_temp (gfc_code *c, stmtblock_t *pre, stmtblock_t *post)
{
  gfc_se tse;
  gfc_se rse;
  gfc_expr *e;
  gfc_symbol *new_sym;
  gfc_symbol *old_sym;
  gfc_symtree *root;
  tree tmp;

  /* Build a copy of the lvalue.  */
  old_symtree = c->expr1->symtree;
  old_sym = old_symtree->n.sym;
  e = gfc_lval_expr_from_sym (old_sym);
  if (old_sym->attr.dimension)
    {
      gfc_init_se (&tse, NULL);
      gfc_conv_subref_array_arg (&tse, e, 0, INTENT_IN);
      gfc_add_block_to_block (pre, &tse.pre);
      gfc_add_block_to_block (post, &tse.post);
      tse.expr = build_fold_indirect_ref_loc (input_location, tse.expr);

      if (e->ts.type != BT_CHARACTER)
	{
	  /* Use the variable offset for the temporary.  */
	  tmp = gfc_conv_array_offset (old_sym->backend_decl);
	  gfc_conv_descriptor_offset_set (pre, tse.expr, tmp);
	}
    }
  else
    {
      gfc_init_se (&tse, NULL);
      gfc_init_se (&rse, NULL);
      gfc_conv_expr (&rse, e);
      if (e->ts.type == BT_CHARACTER)
	{
	  tse.string_length = rse.string_length;
	  tmp = gfc_get_character_type_len (gfc_default_character_kind,
					    tse.string_length);
	  tse.expr = gfc_conv_string_tmp (&tse, build_pointer_type (tmp),
					  rse.string_length);
	  gfc_add_block_to_block (pre, &tse.pre);
	  gfc_add_block_to_block (post, &tse.post);
	}
      else
	{
	  tmp = gfc_typenode_for_spec (&e->ts);
	  tse.expr = gfc_create_var (tmp, "temp");
	}

      tmp = gfc_trans_scalar_assign (&tse, &rse, e->ts, true,
				     e->expr_type == EXPR_VARIABLE);
      gfc_add_expr_to_block (pre, tmp);
    }
  gfc_free_expr (e);

  /* Create a new symbol to represent the lvalue.  */
  new_sym = gfc_new_symbol (old_sym->name, NULL);
  new_sym->ts = old_sym->ts;
  new_sym->attr.referenced = 1;
  new_sym->attr.temporary = 1;
  new_sym->attr.dimension = old_sym->attr.dimension;
  new_sym->attr.flavor = old_sym->attr.flavor;

  /* Use the temporary as the backend_decl.  */
  new_sym->backend_decl = tse.expr;

  /* Create a fake symtree for it.  */
  root = NULL;
  new_symtree = gfc_new_symtree (&root, old_sym->name);
  new_symtree->n.sym = new_sym;
  gcc_assert (new_symtree == root);

  /* Go through the expression reference replacing the old_symtree
     with the new.  */
  forall_replace_symtree (c->expr1, old_sym, 2);

  /* Now we have made this temporary, we might as well use it for
  the right hand side.  */
  forall_replace_symtree (c->expr2, old_sym, 1);
}


/* Handles dependencies in forall assignments.  */
static int
check_forall_dependencies (gfc_code *c, stmtblock_t *pre, stmtblock_t *post)
{
  gfc_ref *lref;
  gfc_ref *rref;
  int need_temp;
  gfc_symbol *lsym;

  lsym = c->expr1->symtree->n.sym;
  need_temp = gfc_check_dependency (c->expr1, c->expr2, 0);

  /* Now check for dependencies within the 'variable'
     expression itself.  These are treated by making a complete
     copy of variable and changing all the references to it
     point to the copy instead.  Note that the shallow copy of
     the variable will not suffice for derived types with
     pointer components.  We therefore leave these to their
     own devices.  */
  if (lsym->ts.type == BT_DERIVED
	&& lsym->ts.u.derived->attr.pointer_comp)
    return need_temp;

  new_symtree = NULL;
  if (find_forall_index (c->expr1, lsym, 2) == SUCCESS)
    {
      forall_make_variable_temp (c, pre, post);
      need_temp = 0;
    }

  /* Substrings with dependencies are treated in the same
     way.  */
  if (c->expr1->ts.type == BT_CHARACTER
	&& c->expr1->ref
	&& c->expr2->expr_type == EXPR_VARIABLE
	&& lsym == c->expr2->symtree->n.sym)
    {
      for (lref = c->expr1->ref; lref; lref = lref->next)
	if (lref->type == REF_SUBSTRING)
	  break;
      for (rref = c->expr2->ref; rref; rref = rref->next)
	if (rref->type == REF_SUBSTRING)
	  break;

      if (rref && lref
	    && gfc_dep_compare_expr (rref->u.ss.start, lref->u.ss.start) < 0)
	{
	  forall_make_variable_temp (c, pre, post);
	  need_temp = 0;
	}
    }
  return need_temp;
}


static void
cleanup_forall_symtrees (gfc_code *c)
{
  forall_restore_symtree (c->expr1);
  forall_restore_symtree (c->expr2);
  gfc_free (new_symtree->n.sym);
  gfc_free (new_symtree);
}


/* Generate the loops for a FORALL block, specified by FORALL_TMP.  BODY
   is the contents of the FORALL block/stmt to be iterated.  MASK_FLAG
   indicates whether we should generate code to test the FORALLs mask
   array.  OUTER is the loop header to be used for initializing mask
   indices.

   The generated loop format is:
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
gfc_trans_forall_loop (forall_info *forall_tmp, tree body,
                       int mask_flag, stmtblock_t *outer)
{
  int n, nvar;
  tree tmp;
  tree cond;
  stmtblock_t block;
  tree exit_label;
  tree count;
  tree var, start, end, step;
  iter_info *iter;

  /* Initialize the mask index outside the FORALL nest.  */
  if (mask_flag && forall_tmp->mask)
    gfc_add_modify (outer, forall_tmp->maskindex, gfc_index_zero_node);

  iter = forall_tmp->this_loop;
  nvar = forall_tmp->nvar;
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
      cond = fold_build2 (LE_EXPR, boolean_type_node,
			  count, build_int_cst (TREE_TYPE (count), 0));
      tmp = build1_v (GOTO_EXPR, exit_label);
      tmp = fold_build3 (COND_EXPR, void_type_node,
			 cond, tmp, build_empty_stmt (input_location));
      gfc_add_expr_to_block (&block, tmp);

      /* The main loop body.  */
      gfc_add_expr_to_block (&block, body);

      /* Increment the loop variable.  */
      tmp = fold_build2 (PLUS_EXPR, TREE_TYPE (var), var, step);
      gfc_add_modify (&block, var, tmp);

      /* Advance to the next mask element.  Only do this for the
	 innermost loop.  */
      if (n == 0 && mask_flag && forall_tmp->mask)
	{
	  tree maskindex = forall_tmp->maskindex;
	  tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			     maskindex, gfc_index_one_node);
	  gfc_add_modify (&block, maskindex, tmp);
	}

      /* Decrement the loop counter.  */
      tmp = fold_build2 (MINUS_EXPR, TREE_TYPE (var), count,
			 build_int_cst (TREE_TYPE (var), 1));
      gfc_add_modify (&block, count, tmp);

      body = gfc_finish_block (&block);

      /* Loop var initialization.  */
      gfc_init_block (&block);
      gfc_add_modify (&block, var, start);


      /* Initialize the loop counter.  */
      tmp = fold_build2 (MINUS_EXPR, TREE_TYPE (var), step, start);
      tmp = fold_build2 (PLUS_EXPR, TREE_TYPE (var), end, tmp);
      tmp = fold_build2 (TRUNC_DIV_EXPR, TREE_TYPE (var), tmp, step);
      gfc_add_modify (&block, count, tmp);

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


/* Generate the body and loops according to MASK_FLAG.  If MASK_FLAG
   is nonzero, the body is controlled by all masks in the forall nest.
   Otherwise, the innermost loop is not controlled by it's mask.  This
   is used for initializing that mask.  */

static tree
gfc_trans_nested_forall_loop (forall_info * nested_forall_info, tree body,
                              int mask_flag)
{
  tree tmp;
  stmtblock_t header;
  forall_info *forall_tmp;
  tree mask, maskindex;

  gfc_start_block (&header);

  forall_tmp = nested_forall_info;
  while (forall_tmp != NULL)
    {
      /* Generate body with masks' control.  */
      if (mask_flag)
        {
          mask = forall_tmp->mask;
          maskindex = forall_tmp->maskindex;

          /* If a mask was specified make the assignment conditional.  */
          if (mask)
            {
              tmp = gfc_build_array_ref (mask, maskindex, NULL);
              body = build3_v (COND_EXPR, tmp, body,
			       build_empty_stmt (input_location));
            }
        }
      body = gfc_trans_forall_loop (forall_tmp, body, mask_flag, &header);
      forall_tmp = forall_tmp->prev_nest;
      mask_flag = 1;
    }

  gfc_add_expr_to_block (&header, body);
  return gfc_finish_block (&header);
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

      tmp = gfc_call_malloc (pblock, TREE_TYPE (tmpvar), bytesize);
      gfc_add_modify (pblock, tmpvar, tmp);
    }
  return tmpvar;
}


/* Generate codes to copy the temporary to the actual lhs.  */

static tree
generate_loop_for_temp_to_lhs (gfc_expr *expr, tree tmp1, tree count3,
			       tree count1, tree wheremask, bool invert)
{
  gfc_ss *lss;
  gfc_se lse, rse;
  stmtblock_t block, body;
  gfc_loopinfo loop1;
  tree tmp;
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
      tmp = gfc_build_array_ref (tmp1, count1, NULL);

      /* Use the scalar assignment as is.  */
      gfc_add_block_to_block (&block, &lse.pre);
      gfc_add_modify (&block, lse.expr, tmp);
      gfc_add_block_to_block (&block, &lse.post);

      /* Increment the count1.  */
      tmp = fold_build2 (PLUS_EXPR, TREE_TYPE (count1), count1,
			 gfc_index_one_node);
      gfc_add_modify (&block, count1, tmp);

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
      gfc_conv_loop_setup (&loop1, &expr->where);

      gfc_mark_ss_chain_used (lss, 1);

      /* Start the scalarized loop body.  */
      gfc_start_scalarized_body (&loop1, &body);

      /* Setup the gfc_se structures.  */
      gfc_copy_loopinfo_to_se (&lse, &loop1);
      lse.ss = lss;

      /* Form the expression of the temporary.  */
      if (lss != gfc_ss_terminator)
	rse.expr = gfc_build_array_ref (tmp1, count1, NULL);
      /* Translate expr.  */
      gfc_conv_expr (&lse, expr);

      /* Use the scalar assignment.  */
      rse.string_length = lse.string_length;
      tmp = gfc_trans_scalar_assign (&lse, &rse, expr->ts, false, false);

      /* Form the mask expression according to the mask tree list.  */
      if (wheremask)
	{
	  wheremaskexpr = gfc_build_array_ref (wheremask, count3, NULL);
	  if (invert)
	    wheremaskexpr = fold_build1 (TRUTH_NOT_EXPR,
					 TREE_TYPE (wheremaskexpr),
					 wheremaskexpr);
	  tmp = fold_build3 (COND_EXPR, void_type_node,
			     wheremaskexpr, tmp,
			     build_empty_stmt (input_location));
       }

      gfc_add_expr_to_block (&body, tmp);

      /* Increment count1.  */
      tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			 count1, gfc_index_one_node);
      gfc_add_modify (&body, count1, tmp);

      /* Increment count3.  */
      if (count3)
	{
	  tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			     count3, gfc_index_one_node);
	  gfc_add_modify (&body, count3, tmp);
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


/* Generate codes to copy rhs to the temporary. TMP1 is the address of
   temporary, LSS and RSS are formed in function compute_inner_temp_size(),
   and should not be freed.  WHEREMASK is the conditional execution mask
   whose sense may be inverted by INVERT.  */

static tree
generate_loop_for_rhs_to_temp (gfc_expr *expr2, tree tmp1, tree count3,
			       tree count1, gfc_ss *lss, gfc_ss *rss,
			       tree wheremask, bool invert)
{
  stmtblock_t block, body1;
  gfc_loopinfo loop;
  gfc_se lse;
  gfc_se rse;
  tree tmp;
  tree wheremaskexpr;

  gfc_start_block (&block);

  gfc_init_se (&rse, NULL);
  gfc_init_se (&lse, NULL);

  if (lss == gfc_ss_terminator)
    {
      gfc_init_block (&body1);
      gfc_conv_expr (&rse, expr2);
      lse.expr = gfc_build_array_ref (tmp1, count1, NULL);
    }
  else
    {
      /* Initialize the loop.  */
      gfc_init_loopinfo (&loop);

      /* We may need LSS to determine the shape of the expression.  */
      gfc_add_ss_to_loop (&loop, lss);
      gfc_add_ss_to_loop (&loop, rss);

      gfc_conv_ss_startstride (&loop);
      gfc_conv_loop_setup (&loop, &expr2->where);

      gfc_mark_ss_chain_used (rss, 1);
      /* Start the loop body.  */
      gfc_start_scalarized_body (&loop, &body1);

      /* Translate the expression.  */
      gfc_copy_loopinfo_to_se (&rse, &loop);
      rse.ss = rss;
      gfc_conv_expr (&rse, expr2);

      /* Form the expression of the temporary.  */
      lse.expr = gfc_build_array_ref (tmp1, count1, NULL);
    }

  /* Use the scalar assignment.  */
  lse.string_length = rse.string_length;
  tmp = gfc_trans_scalar_assign (&lse, &rse, expr2->ts, true,
				 expr2->expr_type == EXPR_VARIABLE);

  /* Form the mask expression according to the mask tree list.  */
  if (wheremask)
    {
      wheremaskexpr = gfc_build_array_ref (wheremask, count3, NULL);
      if (invert)
	wheremaskexpr = fold_build1 (TRUTH_NOT_EXPR,
				     TREE_TYPE (wheremaskexpr),
				     wheremaskexpr);
      tmp = fold_build3 (COND_EXPR, void_type_node,
			 wheremaskexpr, tmp, build_empty_stmt (input_location));
    }

  gfc_add_expr_to_block (&body1, tmp);

  if (lss == gfc_ss_terminator)
    {
      gfc_add_block_to_block (&block, &body1);

      /* Increment count1.  */
      tmp = fold_build2 (PLUS_EXPR, TREE_TYPE (count1), count1,
			 gfc_index_one_node);
      gfc_add_modify (&block, count1, tmp);
    }
  else
    {
      /* Increment count1.  */
      tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			 count1, gfc_index_one_node);
      gfc_add_modify (&body1, count1, tmp);

      /* Increment count3.  */
      if (count3)
	{
	  tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			     count3, gfc_index_one_node);
	  gfc_add_modify (&body1, count3, tmp);
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
  int save_flag;
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
      save_flag = gfc_option.rtcheck;
      gfc_option.rtcheck &= !GFC_RTCHECK_BOUNDS;
      gfc_conv_ss_startstride (&loop);
      gfc_option.rtcheck = save_flag;
      gfc_conv_loop_setup (&loop, &expr2->where);

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


/* Calculate the overall iterator number of the nested forall construct.
   This routine actually calculates the number of times the body of the
   nested forall specified by NESTED_FORALL_INFO is executed and multiplies
   that by the expression INNER_SIZE.  The BLOCK argument specifies the
   block in which to calculate the result, and the optional INNER_SIZE_BODY
   argument contains any statements that need to executed (inside the loop)
   to initialize or calculate INNER_SIZE.  */

static tree
compute_overall_iter_number (forall_info *nested_forall_info, tree inner_size,
			     stmtblock_t *inner_size_body, stmtblock_t *block)
{
  forall_info *forall_tmp = nested_forall_info;
  tree tmp, number;
  stmtblock_t body;

  /* We can eliminate the innermost unconditional loops with constant
     array bounds.  */
  if (INTEGER_CST_P (inner_size))
    {
      while (forall_tmp
	     && !forall_tmp->mask 
	     && INTEGER_CST_P (forall_tmp->size))
	{
	  inner_size = fold_build2 (MULT_EXPR, gfc_array_index_type,
				    inner_size, forall_tmp->size);
	  forall_tmp = forall_tmp->prev_nest;
	}

      /* If there are no loops left, we have our constant result.  */
      if (!forall_tmp)
	return inner_size;
    }

  /* Otherwise, create a temporary variable to compute the result.  */
  number = gfc_create_var (gfc_array_index_type, "num");
  gfc_add_modify (block, number, gfc_index_zero_node);

  gfc_start_block (&body);
  if (inner_size_body)
    gfc_add_block_to_block (&body, inner_size_body);
  if (forall_tmp)
    tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
		       number, inner_size);
  else
    tmp = inner_size;
  gfc_add_modify (&body, number, tmp);
  tmp = gfc_finish_block (&body);

  /* Generate loops.  */
  if (forall_tmp != NULL)
    tmp = gfc_trans_nested_forall_loop (forall_tmp, tmp, 1);

  gfc_add_expr_to_block (block, tmp);

  return number;
}


/* Allocate temporary for forall construct.  SIZE is the size of temporary
   needed.  PTEMP1 is returned for space free.  */

static tree
allocate_temp_for_forall_nest_1 (tree type, tree size, stmtblock_t * block,
				 tree * ptemp1)
{
  tree bytesize;
  tree unit;
  tree tmp;

  unit = fold_convert (gfc_array_index_type, TYPE_SIZE_UNIT (type));
  if (!integer_onep (unit))
    bytesize = fold_build2 (MULT_EXPR, gfc_array_index_type, size, unit);
  else
    bytesize = size;

  *ptemp1 = NULL;
  tmp = gfc_do_allocate (bytesize, size, ptemp1, block, type);

  if (*ptemp1)
    tmp = build_fold_indirect_ref_loc (input_location, tmp);
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
gfc_trans_assign_need_temp (gfc_expr * expr1, gfc_expr * expr2,
			    tree wheremask, bool invert,
                            forall_info * nested_forall_info,
                            stmtblock_t * block)
{
  tree type;
  tree inner_size;
  gfc_ss *lss, *rss;
  tree count, count1;
  tree tmp, tmp1;
  tree ptemp1;
  stmtblock_t inner_size_body;

  /* Create vars. count1 is the current iterator number of the nested
     forall.  */
  count1 = gfc_create_var (gfc_array_index_type, "count1");

  /* Count is the wheremask index.  */
  if (wheremask)
    {
      count = gfc_create_var (gfc_array_index_type, "count");
      gfc_add_modify (block, count, gfc_index_zero_node);
    }
  else
    count = NULL;

  /* Initialize count1.  */
  gfc_add_modify (block, count1, gfc_index_zero_node);

  /* Calculate the size of temporary needed in the assignment. Return loop, lss
     and rss which are used in function generate_loop_for_rhs_to_temp().  */
  gfc_init_block (&inner_size_body);
  inner_size = compute_inner_temp_size (expr1, expr2, &inner_size_body,
					&lss, &rss);

  /* The type of LHS. Used in function allocate_temp_for_forall_nest */
  if (expr1->ts.type == BT_CHARACTER && expr1->ts.u.cl->length)
    {
      if (!expr1->ts.u.cl->backend_decl)
	{
	  gfc_se tse;
	  gfc_init_se (&tse, NULL);
	  gfc_conv_expr (&tse, expr1->ts.u.cl->length);
	  expr1->ts.u.cl->backend_decl = tse.expr;
	}
      type = gfc_get_character_type_len (gfc_default_character_kind,
				         expr1->ts.u.cl->backend_decl);
    }
  else
    type = gfc_typenode_for_spec (&expr1->ts);

  /* Allocate temporary for nested forall construct according to the
     information in nested_forall_info and inner_size.  */
  tmp1 = allocate_temp_for_forall_nest (nested_forall_info, type, inner_size,
					&inner_size_body, block, &ptemp1);

  /* Generate codes to copy rhs to the temporary .  */
  tmp = generate_loop_for_rhs_to_temp (expr2, tmp1, count, count1, lss, rss,
				       wheremask, invert);

  /* Generate body and loops according to the information in
     nested_forall_info.  */
  tmp = gfc_trans_nested_forall_loop (nested_forall_info, tmp, 1);
  gfc_add_expr_to_block (block, tmp);

  /* Reset count1.  */
  gfc_add_modify (block, count1, gfc_index_zero_node);

  /* Reset count.  */
  if (wheremask)
    gfc_add_modify (block, count, gfc_index_zero_node);

  /* Generate codes to copy the temporary to lhs.  */
  tmp = generate_loop_for_temp_to_lhs (expr1, tmp1, count, count1,
				       wheremask, invert);

  /* Generate body and loops according to the information in
     nested_forall_info.  */
  tmp = gfc_trans_nested_forall_loop (nested_forall_info, tmp, 1);
  gfc_add_expr_to_block (block, tmp);

  if (ptemp1)
    {
      /* Free the temporary.  */
      tmp = gfc_call_free (ptemp1);
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

  count = gfc_create_var (gfc_array_index_type, "count");
  gfc_add_modify (block, count, gfc_index_zero_node);

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
      lse.expr = gfc_build_array_ref (tmp1, count, NULL);
      gfc_init_se (&rse, NULL);
      rse.want_pointer = 1;
      gfc_conv_expr (&rse, expr2);
      gfc_add_block_to_block (&body, &rse.pre);
      gfc_add_modify (&body, lse.expr,
			   fold_convert (TREE_TYPE (lse.expr), rse.expr));
      gfc_add_block_to_block (&body, &rse.post);

      /* Increment count.  */
      tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			 count, gfc_index_one_node);
      gfc_add_modify (&body, count, tmp);

      tmp = gfc_finish_block (&body);

      /* Generate body and loops according to the information in
         nested_forall_info.  */
      tmp = gfc_trans_nested_forall_loop (nested_forall_info, tmp, 1);
      gfc_add_expr_to_block (block, tmp);

      /* Reset count.  */
      gfc_add_modify (block, count, gfc_index_zero_node);

      gfc_start_block (&body);
      gfc_init_se (&lse, NULL);
      gfc_init_se (&rse, NULL);
      rse.expr = gfc_build_array_ref (tmp1, count, NULL);
      lse.want_pointer = 1;
      gfc_conv_expr (&lse, expr1);
      gfc_add_block_to_block (&body, &lse.pre);
      gfc_add_modify (&body, lse.expr, rse.expr);
      gfc_add_block_to_block (&body, &lse.post);
      /* Increment count.  */
      tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			 count, gfc_index_one_node);
      gfc_add_modify (&body, count, tmp);
      tmp = gfc_finish_block (&body);

      /* Generate body and loops according to the information in
         nested_forall_info.  */
      tmp = gfc_trans_nested_forall_loop (nested_forall_info, tmp, 1);
      gfc_add_expr_to_block (block, tmp);
    }
  else
    {
      gfc_init_loopinfo (&loop);

      /* Associate the SS with the loop.  */
      gfc_add_ss_to_loop (&loop, rss);

      /* Setup the scalarizing loops and bounds.  */
      gfc_conv_ss_startstride (&loop);

      gfc_conv_loop_setup (&loop, &expr2->where);

      info = &rss->data.info;
      desc = info->descriptor;

      /* Make a new descriptor.  */
      parmtype = gfc_get_element_type (TREE_TYPE (desc));
      parmtype = gfc_get_array_type_bounds (parmtype, loop.dimen,
                                            loop.from, loop.to, 1,
					    GFC_ARRAY_UNKNOWN, true);

      /* Allocate temporary for nested forall construct.  */
      tmp1 = allocate_temp_for_forall_nest (nested_forall_info, parmtype,
					    inner_size, NULL, block, &ptemp1);
      gfc_start_block (&body);
      gfc_init_se (&lse, NULL);
      lse.expr = gfc_build_array_ref (tmp1, count, NULL);
      lse.direct_byref = 1;
      rss = gfc_walk_expr (expr2);
      gfc_conv_expr_descriptor (&lse, expr2, rss);

      gfc_add_block_to_block (&body, &lse.pre);
      gfc_add_block_to_block (&body, &lse.post);

      /* Increment count.  */
      tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			 count, gfc_index_one_node);
      gfc_add_modify (&body, count, tmp);

      tmp = gfc_finish_block (&body);

      /* Generate body and loops according to the information in
         nested_forall_info.  */
      tmp = gfc_trans_nested_forall_loop (nested_forall_info, tmp, 1);
      gfc_add_expr_to_block (block, tmp);

      /* Reset count.  */
      gfc_add_modify (block, count, gfc_index_zero_node);

      parm = gfc_build_array_ref (tmp1, count, NULL);
      lss = gfc_walk_expr (expr1);
      gfc_init_se (&lse, NULL);
      gfc_conv_expr_descriptor (&lse, expr1, lss);
      gfc_add_modify (&lse.pre, lse.expr, parm);
      gfc_start_block (&body);
      gfc_add_block_to_block (&body, &lse.pre);
      gfc_add_block_to_block (&body, &lse.post);

      /* Increment count.  */
      tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			 count, gfc_index_one_node);
      gfc_add_modify (&body, count, tmp);

      tmp = gfc_finish_block (&body);

      tmp = gfc_trans_nested_forall_loop (nested_forall_info, tmp, 1);
      gfc_add_expr_to_block (block, tmp);
    }
  /* Free the temporary.  */
  if (ptemp1)
    {
      tmp = gfc_call_free (ptemp1);
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
  stmtblock_t pre;
  stmtblock_t post;
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
  iter_info *this_forall;
  forall_info *info;
  bool need_mask;

  /* Do nothing if the mask is false.  */
  if (code->expr1
      && code->expr1->expr_type == EXPR_CONSTANT
      && !code->expr1->value.logical)
    return build_empty_stmt (input_location);

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

  gfc_start_block (&pre);
  gfc_init_block (&post);
  gfc_init_block (&block);

  n = 0;
  for (fa = code->ext.forall_iterator; fa; fa = fa->next)
    {
      gfc_symbol *sym = fa->var->symtree->n.sym;

      /* Allocate space for this_forall.  */
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
      if (info->this_loop)
        {
          iter_info *iter_tmp = info->this_loop;
          while (iter_tmp->next != NULL)
            iter_tmp = iter_tmp->next;
          iter_tmp->next = this_forall;
        }
      else
        info->this_loop = this_forall;

      n++;
    }
  nvar = n;

  /* Calculate the size needed for the current forall level.  */
  size = gfc_index_one_node;
  for (n = 0; n < nvar; n++)
    {
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

  if (code->expr1)
    {
      /* If the mask is .true., consider the FORALL unconditional.  */
      if (code->expr1->expr_type == EXPR_CONSTANT
	  && code->expr1->value.logical)
	need_mask = false;
      else
	need_mask = true;
    }
  else
    need_mask = false;

  /* First we need to allocate the mask.  */
  if (need_mask)
    {
      /* As the mask array can be very big, prefer compact boolean types.  */
      tree mask_type = gfc_get_logical_type (gfc_logical_kinds[0].kind);
      mask = allocate_temp_for_forall_nest (nested_forall_info, mask_type,
					    size, NULL, &block, &pmask);
      maskindex = gfc_create_var_np (gfc_array_index_type, "mi");

      /* Record them in the info structure.  */
      info->maskindex = maskindex;
      info->mask = mask;
    }
  else
    {
      /* No mask was specified.  */
      maskindex = NULL_TREE;
      mask = pmask = NULL_TREE;
    }

  /* Link the current forall level to nested_forall_info.  */
  info->prev_nest = nested_forall_info;
  nested_forall_info = info;

  /* Copy the mask into a temporary variable if required.
     For now we assume a mask temporary is needed.  */
  if (need_mask)
    {
      /* As the mask array can be very big, prefer compact boolean types.  */
      tree mask_type = gfc_get_logical_type (gfc_logical_kinds[0].kind);

      gfc_add_modify (&block, maskindex, gfc_index_zero_node);

      /* Start of mask assignment loop body.  */
      gfc_start_block (&body);

      /* Evaluate the mask expression.  */
      gfc_init_se (&se, NULL);
      gfc_conv_expr_val (&se, code->expr1);
      gfc_add_block_to_block (&body, &se.pre);

      /* Store the mask.  */
      se.expr = convert (mask_type, se.expr);

      tmp = gfc_build_array_ref (mask, maskindex, NULL);
      gfc_add_modify (&body, tmp, se.expr);

      /* Advance to the next mask element.  */
      tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			 maskindex, gfc_index_one_node);
      gfc_add_modify (&body, maskindex, tmp);

      /* Generate the loops.  */
      tmp = gfc_finish_block (&body);
      tmp = gfc_trans_nested_forall_loop (info, tmp, 0);
      gfc_add_expr_to_block (&block, tmp);
    }

  c = code->block->next;

  /* TODO: loop merging in FORALL statements.  */
  /* Now that we've got a copy of the mask, generate the assignment loops.  */
  while (c)
    {
      switch (c->op)
	{
	case EXEC_ASSIGN:
          /* A scalar or array assignment.  DO the simple check for
	     lhs to rhs dependencies.  These make a temporary for the
	     rhs and form a second forall block to copy to variable.  */
	  need_temp = check_forall_dependencies(c, &pre, &post);

          /* Temporaries due to array assignment data dependencies introduce
             no end of problems.  */
	  if (need_temp)
            gfc_trans_assign_need_temp (c->expr1, c->expr2, NULL, false,
                                        nested_forall_info, &block);
          else
            {
              /* Use the normal assignment copying routines.  */
              assign = gfc_trans_assignment (c->expr1, c->expr2, false);

              /* Generate body and loops.  */
              tmp = gfc_trans_nested_forall_loop (nested_forall_info,
						  assign, 1);
              gfc_add_expr_to_block (&block, tmp);
            }

	  /* Cleanup any temporary symtrees that have been made to deal
	     with dependencies.  */
	  if (new_symtree)
	    cleanup_forall_symtrees (c);

	  break;

        case EXEC_WHERE:
	  /* Translate WHERE or WHERE construct nested in FORALL.  */
	  gfc_trans_where_2 (c, NULL, false, nested_forall_info, &block);
	  break;

        /* Pointer assignment inside FORALL.  */
	case EXEC_POINTER_ASSIGN:
          need_temp = gfc_check_dependency (c->expr1, c->expr2, 0);
          if (need_temp)
            gfc_trans_pointer_assign_need_temp (c->expr1, c->expr2,
                                                nested_forall_info, &block);
          else
            {
              /* Use the normal assignment copying routines.  */
              assign = gfc_trans_pointer_assignment (c->expr1, c->expr2);

              /* Generate body and loops.  */
              tmp = gfc_trans_nested_forall_loop (nested_forall_info,
						  assign, 1);
              gfc_add_expr_to_block (&block, tmp);
            }
          break;

	case EXEC_FORALL:
	  tmp = gfc_trans_forall_1 (c, nested_forall_info);
          gfc_add_expr_to_block (&block, tmp);
          break;

	/* Explicit subroutine calls are prevented by the frontend but interface
	   assignments can legitimately produce them.  */
	case EXEC_ASSIGN_CALL:
	  assign = gfc_trans_call (c, true, NULL_TREE, NULL_TREE, false);
          tmp = gfc_trans_nested_forall_loop (nested_forall_info, assign, 1);
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

  /* Free the space for this forall_info.  */
  gfc_free (info);

  if (pmask)
    {
      /* Free the temporary for the mask.  */
      tmp = gfc_call_free (pmask);
      gfc_add_expr_to_block (&block, tmp);
    }
  if (maskindex)
    pushdecl (maskindex);

  gfc_add_block_to_block (&pre, &block);
  gfc_add_block_to_block (&pre, &post);

  return gfc_finish_block (&pre);
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
   MASK is the current execution mask upon input, whose sense may or may
   not be inverted as specified by the INVERT argument.
   CMASK is the updated execution mask on output, or NULL if not required.
   PMASK is the pending execution mask on output, or NULL if not required.
   BLOCK is the block in which to place the condition evaluation loops.  */

static void
gfc_evaluate_where_mask (gfc_expr * me, forall_info * nested_forall_info,
                         tree mask, bool invert, tree cmask, tree pmask,
                         tree mask_type, stmtblock_t * block)
{
  tree tmp, tmp1;
  gfc_ss *lss, *rss;
  gfc_loopinfo loop;
  stmtblock_t body, body1;
  tree count, cond, mtmp;
  gfc_se lse, rse;

  gfc_init_loopinfo (&loop);

  lss = gfc_walk_expr (me);
  rss = gfc_walk_expr (me);

  /* Variable to index the temporary.  */
  count = gfc_create_var (gfc_array_index_type, "count");
  /* Initialize count.  */
  gfc_add_modify (block, count, gfc_index_zero_node);

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
      gfc_conv_loop_setup (&loop, &me->where);

      gfc_mark_ss_chain_used (rss, 1);
      /* Start the loop body.  */
      gfc_start_scalarized_body (&loop, &body1);

      /* Translate the expression.  */
      gfc_copy_loopinfo_to_se (&rse, &loop);
      rse.ss = rss;
      gfc_conv_expr (&rse, me);
    }

  /* Variable to evaluate mask condition.  */
  cond = gfc_create_var (mask_type, "cond");
  if (mask && (cmask || pmask))
    mtmp = gfc_create_var (mask_type, "mask");
  else mtmp = NULL_TREE;

  gfc_add_block_to_block (&body1, &lse.pre);
  gfc_add_block_to_block (&body1, &rse.pre);

  gfc_add_modify (&body1, cond, fold_convert (mask_type, rse.expr));

  if (mask && (cmask || pmask))
    {
      tmp = gfc_build_array_ref (mask, count, NULL);
      if (invert)
	tmp = fold_build1 (TRUTH_NOT_EXPR, mask_type, tmp);
      gfc_add_modify (&body1, mtmp, tmp);
    }

  if (cmask)
    {
      tmp1 = gfc_build_array_ref (cmask, count, NULL);
      tmp = cond;
      if (mask)
	tmp = fold_build2 (TRUTH_AND_EXPR, mask_type, mtmp, tmp);
      gfc_add_modify (&body1, tmp1, tmp);
    }

  if (pmask)
    {
      tmp1 = gfc_build_array_ref (pmask, count, NULL);
      tmp = fold_build1 (TRUTH_NOT_EXPR, mask_type, cond);
      if (mask)
	tmp = fold_build2 (TRUTH_AND_EXPR, mask_type, mtmp, tmp);
      gfc_add_modify (&body1, tmp1, tmp);
    }

  gfc_add_block_to_block (&body1, &lse.post);
  gfc_add_block_to_block (&body1, &rse.post);

  if (lss == gfc_ss_terminator)
    {
      gfc_add_block_to_block (&body, &body1);
    }
  else
    {
      /* Increment count.  */
      tmp1 = fold_build2 (PLUS_EXPR, gfc_array_index_type, count,
                          gfc_index_one_node);
      gfc_add_modify (&body1, count, tmp1);

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
    tmp1 = gfc_trans_nested_forall_loop (nested_forall_info, tmp1, 1);

  gfc_add_expr_to_block (block, tmp1);
}


/* Translate an assignment statement in a WHERE statement or construct
   statement. The MASK expression is used to control which elements
   of EXPR1 shall be assigned.  The sense of MASK is specified by
   INVERT.  */

static tree
gfc_trans_where_assign (gfc_expr *expr1, gfc_expr *expr2,
			tree mask, bool invert,
                        tree count1, tree count2,
			gfc_code *cnext)
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
  tree index, maskexpr;

  /* A defined assignment. */  
  if (cnext && cnext->resolved_sym)
    return gfc_trans_call (cnext, true, mask, count1, invert);

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
     rss->where = 1;
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
  gfc_conv_loop_setup (&loop, &expr2->where);

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

  /* Form the mask expression according to the mask.  */
  index = count1;
  maskexpr = gfc_build_array_ref (mask, index, NULL);
  if (invert)
    maskexpr = fold_build1 (TRUTH_NOT_EXPR, TREE_TYPE (maskexpr), maskexpr);

  /* Use the scalar assignment as is.  */
  tmp = gfc_trans_scalar_assign (&lse, &rse, expr1->ts,
				 loop.temp_ss != NULL, false);

  tmp = build3_v (COND_EXPR, maskexpr, tmp, build_empty_stmt (input_location));

  gfc_add_expr_to_block (&body, tmp);

  if (lss == gfc_ss_terminator)
    {
      /* Increment count1.  */
      tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			 count1, gfc_index_one_node);
      gfc_add_modify (&body, count1, tmp);

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
          gfc_add_modify (&body, count1, tmp);
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
          maskexpr = gfc_build_array_ref (mask, index, NULL);
	  if (invert)
	    maskexpr = fold_build1 (TRUTH_NOT_EXPR, TREE_TYPE (maskexpr),
				    maskexpr);

          /* Use the scalar assignment as is.  */
          tmp = gfc_trans_scalar_assign (&lse, &rse, expr1->ts, false, false);
          tmp = build3_v (COND_EXPR, maskexpr, tmp,
			  build_empty_stmt (input_location));
          gfc_add_expr_to_block (&body, tmp);

          /* Increment count2.  */
          tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			     count2, gfc_index_one_node);
          gfc_add_modify (&body, count2, tmp);
        }
      else
        {
          /* Increment count1.  */
          tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type,
			     count1, gfc_index_one_node);
          gfc_add_modify (&body, count1, tmp);
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
   MASK is the control mask.  */

static void
gfc_trans_where_2 (gfc_code * code, tree mask, bool invert,
		   forall_info * nested_forall_info, stmtblock_t * block)
{
  stmtblock_t inner_size_body;
  tree inner_size, size;
  gfc_ss *lss, *rss;
  tree mask_type;
  gfc_expr *expr1;
  gfc_expr *expr2;
  gfc_code *cblock;
  gfc_code *cnext;
  tree tmp;
  tree cond;
  tree count1, count2;
  bool need_cmask;
  bool need_pmask;
  int need_temp;
  tree pcmask = NULL_TREE;
  tree ppmask = NULL_TREE;
  tree cmask = NULL_TREE;
  tree pmask = NULL_TREE;
  gfc_actual_arglist *arg;

  /* the WHERE statement or the WHERE construct statement.  */
  cblock = code->block;

  /* As the mask array can be very big, prefer compact boolean types.  */
  mask_type = gfc_get_logical_type (gfc_logical_kinds[0].kind);

  /* Determine which temporary masks are needed.  */
  if (!cblock->block)
    {
      /* One clause: No ELSEWHEREs.  */
      need_cmask = (cblock->next != 0);
      need_pmask = false;
    }
  else if (cblock->block->block)
    {
      /* Three or more clauses: Conditional ELSEWHEREs.  */
      need_cmask = true;
      need_pmask = true;
    }
  else if (cblock->next)
    {
      /* Two clauses, the first non-empty.  */
      need_cmask = true;
      need_pmask = (mask != NULL_TREE
		    && cblock->block->next != 0);
    }
  else if (!cblock->block->next)
    {
      /* Two clauses, both empty.  */
      need_cmask = false;
      need_pmask = false;
    }
  /* Two clauses, the first empty, the second non-empty.  */
  else if (mask)
    {
      need_cmask = (cblock->block->expr1 != 0);
      need_pmask = true;
    }
  else
    {
      need_cmask = true;
      need_pmask = false;
    }

  if (need_cmask || need_pmask)
    {
      /* Calculate the size of temporary needed by the mask-expr.  */
      gfc_init_block (&inner_size_body);
      inner_size = compute_inner_temp_size (cblock->expr1, cblock->expr1,
					    &inner_size_body, &lss, &rss);

      /* Calculate the total size of temporary needed.  */
      size = compute_overall_iter_number (nested_forall_info, inner_size,
					  &inner_size_body, block);

      /* Check whether the size is negative.  */
      cond = fold_build2 (LE_EXPR, boolean_type_node, size,
			  gfc_index_zero_node);
      size = fold_build3 (COND_EXPR, gfc_array_index_type, cond,
			  gfc_index_zero_node, size);
      size = gfc_evaluate_now (size, block);

      /* Allocate temporary for WHERE mask if needed.  */
      if (need_cmask)
	cmask = allocate_temp_for_forall_nest_1 (mask_type, size, block,
						 &pcmask);

      /* Allocate temporary for !mask if needed.  */
      if (need_pmask)
	pmask = allocate_temp_for_forall_nest_1 (mask_type, size, block,
						 &ppmask);
    }

  while (cblock)
    {
      /* Each time around this loop, the where clause is conditional
	 on the value of mask and invert, which are updated at the
	 bottom of the loop.  */

      /* Has mask-expr.  */
      if (cblock->expr1)
        {
          /* Ensure that the WHERE mask will be evaluated exactly once.
	     If there are no statements in this WHERE/ELSEWHERE clause,
	     then we don't need to update the control mask (cmask).
	     If this is the last clause of the WHERE construct, then
	     we don't need to update the pending control mask (pmask).  */
	  if (mask)
	    gfc_evaluate_where_mask (cblock->expr1, nested_forall_info,
				     mask, invert,
				     cblock->next  ? cmask : NULL_TREE,
				     cblock->block ? pmask : NULL_TREE,
				     mask_type, block);
	  else
	    gfc_evaluate_where_mask (cblock->expr1, nested_forall_info,
				     NULL_TREE, false,
				     (cblock->next || cblock->block)
				     ? cmask : NULL_TREE,
				     NULL_TREE, mask_type, block);

	  invert = false;
        }
      /* It's a final elsewhere-stmt. No mask-expr is present.  */
      else
        cmask = mask;

      /* The body of this where clause are controlled by cmask with
	 sense specified by invert.  */

      /* Get the assignment statement of a WHERE statement, or the first
         statement in where-body-construct of a WHERE construct.  */
      cnext = cblock->next;
      while (cnext)
        {
          switch (cnext->op)
            {
            /* WHERE assignment statement.  */
	    case EXEC_ASSIGN_CALL:

	      arg = cnext->ext.actual;
	      expr1 = expr2 = NULL;
	      for (; arg; arg = arg->next)
		{
		  if (!arg->expr)
		    continue;
		  if (expr1 == NULL)
		    expr1 = arg->expr;
		  else
		    expr2 = arg->expr;
		}
	      goto evaluate;

            case EXEC_ASSIGN:
              expr1 = cnext->expr1;
              expr2 = cnext->expr2;
    evaluate:
              if (nested_forall_info != NULL)
                {
                  need_temp = gfc_check_dependency (expr1, expr2, 0);
                  if (need_temp && cnext->op != EXEC_ASSIGN_CALL)
                    gfc_trans_assign_need_temp (expr1, expr2,
						cmask, invert,
                                                nested_forall_info, block);
                  else
                    {
                      /* Variables to control maskexpr.  */
                      count1 = gfc_create_var (gfc_array_index_type, "count1");
                      count2 = gfc_create_var (gfc_array_index_type, "count2");
                      gfc_add_modify (block, count1, gfc_index_zero_node);
                      gfc_add_modify (block, count2, gfc_index_zero_node);

                      tmp = gfc_trans_where_assign (expr1, expr2,
						    cmask, invert,
						    count1, count2,
						    cnext);

                      tmp = gfc_trans_nested_forall_loop (nested_forall_info,
                                                          tmp, 1);
                      gfc_add_expr_to_block (block, tmp);
                    }
                }
              else
                {
                  /* Variables to control maskexpr.  */
                  count1 = gfc_create_var (gfc_array_index_type, "count1");
                  count2 = gfc_create_var (gfc_array_index_type, "count2");
                  gfc_add_modify (block, count1, gfc_index_zero_node);
                  gfc_add_modify (block, count2, gfc_index_zero_node);

                  tmp = gfc_trans_where_assign (expr1, expr2,
						cmask, invert,
						count1, count2,
						cnext);
                  gfc_add_expr_to_block (block, tmp);

                }
              break;

            /* WHERE or WHERE construct is part of a where-body-construct.  */
            case EXEC_WHERE:
	      gfc_trans_where_2 (cnext, cmask, invert,
				 nested_forall_info, block);
	      break;

            default:
              gcc_unreachable ();
            }

         /* The next statement within the same where-body-construct.  */
         cnext = cnext->next;
       }
    /* The next masked-elsewhere-stmt, elsewhere-stmt, or end-where-stmt.  */
    cblock = cblock->block;
    if (mask == NULL_TREE)
      {
        /* If we're the initial WHERE, we can simply invert the sense
	   of the current mask to obtain the "mask" for the remaining
	   ELSEWHEREs.  */
	invert = true;
	mask = cmask;
      }
    else
      {
	/* Otherwise, for nested WHERE's we need to use the pending mask.  */
        invert = false;
        mask = pmask;
      }
  }

  /* If we allocated a pending mask array, deallocate it now.  */
  if (ppmask)
    {
      tmp = gfc_call_free (ppmask);
      gfc_add_expr_to_block (block, tmp);
    }

  /* If we allocated a current mask array, deallocate it now.  */
  if (pcmask)
    {
      tmp = gfc_call_free (pcmask);
      gfc_add_expr_to_block (block, tmp);
    }
}

/* Translate a simple WHERE construct or statement without dependencies.
   CBLOCK is the "then" clause of the WHERE statement, where CBLOCK->EXPR
   is the mask condition, and EBLOCK if non-NULL is the "else" clause.
   Currently both CBLOCK and EBLOCK are restricted to single assignments.  */

static tree
gfc_trans_where_3 (gfc_code * cblock, gfc_code * eblock)
{
  stmtblock_t block, body;
  gfc_expr *cond, *tdst, *tsrc, *edst, *esrc;
  tree tmp, cexpr, tstmt, estmt;
  gfc_ss *css, *tdss, *tsss;
  gfc_se cse, tdse, tsse, edse, esse;
  gfc_loopinfo loop;
  gfc_ss *edss = 0;
  gfc_ss *esss = 0;

  /* Allow the scalarizer to workshare simple where loops.  */
  if (ompws_flags & OMPWS_WORKSHARE_FLAG)
    ompws_flags |= OMPWS_SCALARIZER_WS;

  cond = cblock->expr1;
  tdst = cblock->next->expr1;
  tsrc = cblock->next->expr2;
  edst = eblock ? eblock->next->expr1 : NULL;
  esrc = eblock ? eblock->next->expr2 : NULL;

  gfc_start_block (&block);
  gfc_init_loopinfo (&loop);

  /* Handle the condition.  */
  gfc_init_se (&cse, NULL);
  css = gfc_walk_expr (cond);
  gfc_add_ss_to_loop (&loop, css);

  /* Handle the then-clause.  */
  gfc_init_se (&tdse, NULL);
  gfc_init_se (&tsse, NULL);
  tdss = gfc_walk_expr (tdst);
  tsss = gfc_walk_expr (tsrc);
  if (tsss == gfc_ss_terminator)
    {
      tsss = gfc_get_ss ();
      tsss->where = 1;
      tsss->next = gfc_ss_terminator;
      tsss->type = GFC_SS_SCALAR;
      tsss->expr = tsrc;
    }
  gfc_add_ss_to_loop (&loop, tdss);
  gfc_add_ss_to_loop (&loop, tsss);

  if (eblock)
    {
      /* Handle the else clause.  */
      gfc_init_se (&edse, NULL);
      gfc_init_se (&esse, NULL);
      edss = gfc_walk_expr (edst);
      esss = gfc_walk_expr (esrc);
      if (esss == gfc_ss_terminator)
	{
	  esss = gfc_get_ss ();
	  esss->where = 1;
	  esss->next = gfc_ss_terminator;
	  esss->type = GFC_SS_SCALAR;
	  esss->expr = esrc;
	}
      gfc_add_ss_to_loop (&loop, edss);
      gfc_add_ss_to_loop (&loop, esss);
    }

  gfc_conv_ss_startstride (&loop);
  gfc_conv_loop_setup (&loop, &tdst->where);

  gfc_mark_ss_chain_used (css, 1);
  gfc_mark_ss_chain_used (tdss, 1);
  gfc_mark_ss_chain_used (tsss, 1);
  if (eblock)
    {
      gfc_mark_ss_chain_used (edss, 1);
      gfc_mark_ss_chain_used (esss, 1);
    }

  gfc_start_scalarized_body (&loop, &body);

  gfc_copy_loopinfo_to_se (&cse, &loop);
  gfc_copy_loopinfo_to_se (&tdse, &loop);
  gfc_copy_loopinfo_to_se (&tsse, &loop);
  cse.ss = css;
  tdse.ss = tdss;
  tsse.ss = tsss;
  if (eblock)
    {
      gfc_copy_loopinfo_to_se (&edse, &loop);
      gfc_copy_loopinfo_to_se (&esse, &loop);
      edse.ss = edss;
      esse.ss = esss;
    }

  gfc_conv_expr (&cse, cond);
  gfc_add_block_to_block (&body, &cse.pre);
  cexpr = cse.expr;

  gfc_conv_expr (&tsse, tsrc);
  if (tdss != gfc_ss_terminator && loop.temp_ss != NULL)
    {
      gfc_conv_tmp_array_ref (&tdse);
      gfc_advance_se_ss_chain (&tdse);
    }
  else
    gfc_conv_expr (&tdse, tdst);

  if (eblock)
    {
      gfc_conv_expr (&esse, esrc);
      if (edss != gfc_ss_terminator && loop.temp_ss != NULL)
        {
          gfc_conv_tmp_array_ref (&edse);
          gfc_advance_se_ss_chain (&edse);
        }
      else
        gfc_conv_expr (&edse, edst);
    }

  tstmt = gfc_trans_scalar_assign (&tdse, &tsse, tdst->ts, false, false);
  estmt = eblock ? gfc_trans_scalar_assign (&edse, &esse, edst->ts, false, false)
		 : build_empty_stmt (input_location);
  tmp = build3_v (COND_EXPR, cexpr, tstmt, estmt);
  gfc_add_expr_to_block (&body, tmp);
  gfc_add_block_to_block (&body, &cse.post);

  gfc_trans_scalarizing_loops (&loop, &body);
  gfc_add_block_to_block (&block, &loop.pre);
  gfc_add_block_to_block (&block, &loop.post);
  gfc_cleanup_loop (&loop);

  return gfc_finish_block (&block);
}

/* As the WHERE or WHERE construct statement can be nested, we call
   gfc_trans_where_2 to do the translation, and pass the initial
   NULL values for both the control mask and the pending control mask.  */

tree
gfc_trans_where (gfc_code * code)
{
  stmtblock_t block;
  gfc_code *cblock;
  gfc_code *eblock;

  cblock = code->block;
  if (cblock->next
      && cblock->next->op == EXEC_ASSIGN
      && !cblock->next->next)
    {
      eblock = cblock->block;
      if (!eblock)
	{
          /* A simple "WHERE (cond) x = y" statement or block is
	     dependence free if cond is not dependent upon writing x,
	     and the source y is unaffected by the destination x.  */
	  if (!gfc_check_dependency (cblock->next->expr1,
				     cblock->expr1, 0)
	      && !gfc_check_dependency (cblock->next->expr1,
					cblock->next->expr2, 0))
	    return gfc_trans_where_3 (cblock, NULL);
	}
      else if (!eblock->expr1
	       && !eblock->block
	       && eblock->next
	       && eblock->next->op == EXEC_ASSIGN
	       && !eblock->next->next)
	{
          /* A simple "WHERE (cond) x1 = y1 ELSEWHERE x2 = y2 ENDWHERE"
	     block is dependence free if cond is not dependent on writes
	     to x1 and x2, y1 is not dependent on writes to x2, and y2
	     is not dependent on writes to x1, and both y's are not
	     dependent upon their own x's.  In addition to this, the
	     final two dependency checks below exclude all but the same
	     array reference if the where and elswhere destinations
	     are the same.  In short, this is VERY conservative and this
	     is needed because the two loops, required by the standard
	     are coalesced in gfc_trans_where_3.  */
	  if (!gfc_check_dependency(cblock->next->expr1,
				    cblock->expr1, 0)
	      && !gfc_check_dependency(eblock->next->expr1,
				       cblock->expr1, 0)
	      && !gfc_check_dependency(cblock->next->expr1,
				       eblock->next->expr2, 1)
	      && !gfc_check_dependency(eblock->next->expr1,
				       cblock->next->expr2, 1)
	      && !gfc_check_dependency(cblock->next->expr1,
				       cblock->next->expr2, 1)
	      && !gfc_check_dependency(eblock->next->expr1,
				       eblock->next->expr2, 1)
	      && !gfc_check_dependency(cblock->next->expr1,
				       eblock->next->expr1, 0)
	      && !gfc_check_dependency(eblock->next->expr1,
				       cblock->next->expr1, 0))
	    return gfc_trans_where_3 (cblock, eblock);
	}
    }

  gfc_start_block (&block);

  gfc_trans_where_2 (code, NULL, false, NULL, &block);

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
  gfc_expr *expr, *init_e;
  gfc_se se;
  tree tmp;
  tree parm;
  tree stat;
  tree pstat;
  tree error_label;
  tree memsz;
  stmtblock_t block;

  if (!code->ext.alloc.list)
    return NULL_TREE;

  pstat = stat = error_label = tmp = memsz = NULL_TREE;

  gfc_start_block (&block);

  /* Either STAT= and/or ERRMSG is present.  */
  if (code->expr1 || code->expr2)
    {
      tree gfc_int4_type_node = gfc_get_int_type (4);

      stat = gfc_create_var (gfc_int4_type_node, "stat");
      pstat = gfc_build_addr_expr (NULL_TREE, stat);

      error_label = gfc_build_label_decl (NULL_TREE);
      TREE_USED (error_label) = 1;
    }

  for (al = code->ext.alloc.list; al != NULL; al = al->next)
    {
      expr = gfc_copy_expr (al->expr);

      if (expr->ts.type == BT_CLASS)
	gfc_add_component_ref (expr, "$data");

      gfc_init_se (&se, NULL);
      gfc_start_block (&se.pre);

      se.want_pointer = 1;
      se.descriptor_only = 1;
      gfc_conv_expr (&se, expr);

      if (!gfc_array_allocate (&se, expr, pstat))
	{
	  /* A scalar or derived type.  */

	  /* Determine allocate size.  */
	  if (code->expr3 && code->expr3->ts.type == BT_CLASS)
	    {
	      gfc_expr *sz;
	      gfc_se se_sz;
	      sz = gfc_copy_expr (code->expr3);
	      gfc_add_component_ref (sz, "$vptr");
	      gfc_add_component_ref (sz, "$size");
	      gfc_init_se (&se_sz, NULL);
	      gfc_conv_expr (&se_sz, sz);
	      gfc_free_expr (sz);
	      memsz = se_sz.expr;
	    }
	  else if (code->expr3 && code->expr3->ts.type != BT_CLASS)
	    memsz = TYPE_SIZE_UNIT (gfc_typenode_for_spec (&code->expr3->ts));
	  else if (code->ext.alloc.ts.type != BT_UNKNOWN)
	    memsz = TYPE_SIZE_UNIT (gfc_typenode_for_spec (&code->ext.alloc.ts));
	  else
	    memsz = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (se.expr)));

	  if (expr->ts.type == BT_CHARACTER && memsz == NULL_TREE)
	    memsz = se.string_length;

	  /* Allocate - for non-pointers with re-alloc checking.  */
	  {
	    gfc_ref *ref;
	    bool allocatable;

	    ref = expr->ref;

	    /* Find the last reference in the chain.  */
	    while (ref && ref->next != NULL)
	      {
	        gcc_assert (ref->type != REF_ARRAY || ref->u.ar.type == AR_ELEMENT);
	        ref = ref->next;
	      }

	    if (!ref)
	      allocatable = expr->symtree->n.sym->attr.allocatable;
	    else
	      allocatable = ref->u.c.component->attr.allocatable;

	    if (allocatable)
	      tmp = gfc_allocate_array_with_status (&se.pre, se.expr, memsz,
						    pstat, expr);
	    else
	      tmp = gfc_allocate_with_status (&se.pre, memsz, pstat);
	  }

	  tmp = fold_build2 (MODIFY_EXPR, void_type_node, se.expr,
			     fold_convert (TREE_TYPE (se.expr), tmp));
	  gfc_add_expr_to_block (&se.pre, tmp);

	  if (code->expr1 || code->expr2)
	    {
	      tmp = build1_v (GOTO_EXPR, error_label);
	      parm = fold_build2 (NE_EXPR, boolean_type_node,
				  stat, build_int_cst (TREE_TYPE (stat), 0));
	      tmp = fold_build3 (COND_EXPR, void_type_node,
				 parm, tmp, build_empty_stmt (input_location));
	      gfc_add_expr_to_block (&se.pre, tmp);
	    }

	  if (expr->ts.type == BT_DERIVED && expr->ts.u.derived->attr.alloc_comp)
	    {
	      tmp = build_fold_indirect_ref_loc (input_location, se.expr);
	      tmp = gfc_nullify_alloc_comp (expr->ts.u.derived, tmp, 0);
	      gfc_add_expr_to_block (&se.pre, tmp);
	    }

	}

      tmp = gfc_finish_block (&se.pre);
      gfc_add_expr_to_block (&block, tmp);

      /* Initialization via SOURCE block.  */
      if (code->expr3)
	{
	  gfc_expr *rhs = gfc_copy_expr (code->expr3);
	  if (al->expr->ts.type == BT_CLASS)
	    {
	      gfc_se dst,src;
	      if (rhs->ts.type == BT_CLASS)
		gfc_add_component_ref (rhs, "$data");
	      gfc_init_se (&dst, NULL);
	      gfc_init_se (&src, NULL);
	      gfc_conv_expr (&dst, expr);
	      gfc_conv_expr (&src, rhs);
	      gfc_add_block_to_block (&block, &src.pre);
	      tmp = gfc_build_memcpy_call (dst.expr, src.expr, memsz);
	    }
	  else
	    tmp = gfc_trans_assignment (gfc_expr_to_initialize (expr),
					rhs, false);
	  gfc_free_expr (rhs);
	  gfc_add_expr_to_block (&block, tmp);
	}
      /* Default initializer for CLASS variables.  */
      else if (al->expr->ts.type == BT_CLASS
	       && code->ext.alloc.ts.type == BT_DERIVED
	       && (init_e = gfc_default_initializer (&code->ext.alloc.ts)))
	{
	  gfc_se dst,src;
	  gfc_init_se (&dst, NULL);
	  gfc_init_se (&src, NULL);
	  gfc_conv_expr (&dst, expr);
	  gfc_conv_expr (&src, init_e);
	  gfc_add_block_to_block (&block, &src.pre);
	  tmp = gfc_build_memcpy_call (dst.expr, src.expr, memsz);
	  gfc_add_expr_to_block (&block, tmp);
	}
      /* Add default initializer for those derived types that need them.  */
      else if (expr->ts.type == BT_DERIVED
	       && (init_e = gfc_default_initializer (&expr->ts)))
	{
	  tmp = gfc_trans_assignment (gfc_expr_to_initialize (expr),
				      init_e, true);
	  gfc_add_expr_to_block (&block, tmp);
	}

      /* Allocation of CLASS entities.  */
      gfc_free_expr (expr);
      expr = al->expr;
      if (expr->ts.type == BT_CLASS)
	{
	  gfc_expr *lhs,*rhs;
	  gfc_se lse;

	  /* Initialize VPTR for CLASS objects.  */
	  lhs = gfc_expr_to_initialize (expr);
	  gfc_add_component_ref (lhs, "$vptr");
	  rhs = NULL;
	  if (code->expr3 && code->expr3->ts.type == BT_CLASS)
	    {
	      /* VPTR must be determined at run time.  */
	      rhs = gfc_copy_expr (code->expr3);
	      gfc_add_component_ref (rhs, "$vptr");
	      tmp = gfc_trans_pointer_assignment (lhs, rhs);
	      gfc_add_expr_to_block (&block, tmp);
	      gfc_free_expr (rhs);
	    }
	  else
	    {
	      /* VPTR is fixed at compile time.  */
	      gfc_symbol *vtab;
	      gfc_typespec *ts;
	      if (code->expr3)
		ts = &code->expr3->ts;
	      else if (expr->ts.type == BT_DERIVED)
		ts = &expr->ts;
	      else if (code->ext.alloc.ts.type == BT_DERIVED)
		ts = &code->ext.alloc.ts;
	      else if (expr->ts.type == BT_CLASS)
		ts = &expr->ts.u.derived->components->ts;
	      else
		ts = &expr->ts;

	      if (ts->type == BT_DERIVED)
		{
		  vtab = gfc_find_derived_vtab (ts->u.derived);
		  gcc_assert (vtab);
		  gfc_init_se (&lse, NULL);
		  lse.want_pointer = 1;
		  gfc_conv_expr (&lse, lhs);
		  tmp = gfc_build_addr_expr (NULL_TREE,
					     gfc_get_symbol_decl (vtab));
		  gfc_add_modify (&block, lse.expr,
			fold_convert (TREE_TYPE (lse.expr), tmp));
		}
	    }
	}

    }

  /* STAT block.  */
  if (code->expr1)
    {
      tmp = build1_v (LABEL_EXPR, error_label);
      gfc_add_expr_to_block (&block, tmp);

      gfc_init_se (&se, NULL);
      gfc_conv_expr_lhs (&se, code->expr1);
      tmp = convert (TREE_TYPE (se.expr), stat);
      gfc_add_modify (&block, se.expr, tmp);
    }

  /* ERRMSG block.  */
  if (code->expr2)
    {
      /* A better error message may be possible, but not required.  */
      const char *msg = "Attempt to allocate an allocated object";
      tree errmsg, slen, dlen;

      gfc_init_se (&se, NULL);
      gfc_conv_expr_lhs (&se, code->expr2);

      errmsg = gfc_create_var (pchar_type_node, "ERRMSG");

      gfc_add_modify (&block, errmsg,
		gfc_build_addr_expr (pchar_type_node,
			gfc_build_localized_cstring_const (msg)));

      slen = build_int_cst (gfc_charlen_type_node, ((int) strlen (msg)));
      dlen = gfc_get_expr_charlen (code->expr2);
      slen = fold_build2 (MIN_EXPR, TREE_TYPE (slen), dlen, slen);

      dlen = build_call_expr_loc (input_location,
			      built_in_decls[BUILT_IN_MEMCPY], 3,
		gfc_build_addr_expr (pvoid_type_node, se.expr), errmsg, slen);

      tmp = fold_build2 (NE_EXPR, boolean_type_node, stat,
			 build_int_cst (TREE_TYPE (stat), 0));

      tmp = build3_v (COND_EXPR, tmp, dlen, build_empty_stmt (input_location));

      gfc_add_expr_to_block (&block, tmp);
    }

  return gfc_finish_block (&block);
}


/* Translate a DEALLOCATE statement.  */

tree
gfc_trans_deallocate (gfc_code *code)
{
  gfc_se se;
  gfc_alloc *al;
  gfc_expr *expr;
  tree apstat, astat, pstat, stat, tmp;
  stmtblock_t block;

  pstat = apstat = stat = astat = tmp = NULL_TREE;

  gfc_start_block (&block);

  /* Count the number of failed deallocations.  If deallocate() was
     called with STAT= , then set STAT to the count.  If deallocate
     was called with ERRMSG, then set ERRMG to a string.  */
  if (code->expr1 || code->expr2)
    {
      tree gfc_int4_type_node = gfc_get_int_type (4);

      stat = gfc_create_var (gfc_int4_type_node, "stat");
      pstat = gfc_build_addr_expr (NULL_TREE, stat);

      /* Running total of possible deallocation failures.  */
      astat = gfc_create_var (gfc_int4_type_node, "astat");
      apstat = gfc_build_addr_expr (NULL_TREE, astat);

      /* Initialize astat to 0.  */
      gfc_add_modify (&block, astat, build_int_cst (TREE_TYPE (astat), 0));
    }

  for (al = code->ext.alloc.list; al != NULL; al = al->next)
    {
      expr = al->expr;
      gcc_assert (expr->expr_type == EXPR_VARIABLE);

      gfc_init_se (&se, NULL);
      gfc_start_block (&se.pre);

      se.want_pointer = 1;
      se.descriptor_only = 1;
      gfc_conv_expr (&se, expr);

      if (expr->ts.type == BT_DERIVED && expr->ts.u.derived->attr.alloc_comp)
        {
	  gfc_ref *ref;
	  gfc_ref *last = NULL;
	  for (ref = expr->ref; ref; ref = ref->next)
	    if (ref->type == REF_COMPONENT)
	      last = ref;

	  /* Do not deallocate the components of a derived type
	     ultimate pointer component.  */
	  if (!(last && last->u.c.component->attr.pointer)
		&& !(!last && expr->symtree->n.sym->attr.pointer))
	    {
	      tmp = gfc_deallocate_alloc_comp (expr->ts.u.derived, se.expr,
					       expr->rank);
	      gfc_add_expr_to_block (&se.pre, tmp);
	    }
	}

      if (expr->rank)
	tmp = gfc_array_deallocate (se.expr, pstat, expr);
      else
	{
	  tmp = gfc_deallocate_with_status (se.expr, pstat, false, expr);
	  gfc_add_expr_to_block (&se.pre, tmp);

	  tmp = fold_build2 (MODIFY_EXPR, void_type_node,
			     se.expr, build_int_cst (TREE_TYPE (se.expr), 0));
	}

      gfc_add_expr_to_block (&se.pre, tmp);

      /* Keep track of the number of failed deallocations by adding stat
	 of the last deallocation to the running total.  */
      if (code->expr1 || code->expr2)
	{
	  apstat = fold_build2 (PLUS_EXPR, TREE_TYPE (stat), astat, stat);
	  gfc_add_modify (&se.pre, astat, apstat);
	}

      tmp = gfc_finish_block (&se.pre);
      gfc_add_expr_to_block (&block, tmp);

    }

  /* Set STAT.  */
  if (code->expr1)
    {
      gfc_init_se (&se, NULL);
      gfc_conv_expr_lhs (&se, code->expr1);
      tmp = convert (TREE_TYPE (se.expr), astat);
      gfc_add_modify (&block, se.expr, tmp);
    }

  /* Set ERRMSG.  */
  if (code->expr2)
    {
      /* A better error message may be possible, but not required.  */
      const char *msg = "Attempt to deallocate an unallocated object";
      tree errmsg, slen, dlen;

      gfc_init_se (&se, NULL);
      gfc_conv_expr_lhs (&se, code->expr2);

      errmsg = gfc_create_var (pchar_type_node, "ERRMSG");

      gfc_add_modify (&block, errmsg,
		gfc_build_addr_expr (pchar_type_node,
                        gfc_build_localized_cstring_const (msg)));

      slen = build_int_cst (gfc_charlen_type_node, ((int) strlen (msg)));
      dlen = gfc_get_expr_charlen (code->expr2);
      slen = fold_build2 (MIN_EXPR, TREE_TYPE (slen), dlen, slen);

      dlen = build_call_expr_loc (input_location,
			      built_in_decls[BUILT_IN_MEMCPY], 3,
		gfc_build_addr_expr (pvoid_type_node, se.expr), errmsg, slen);

      tmp = fold_build2 (NE_EXPR, boolean_type_node, astat,
			 build_int_cst (TREE_TYPE (astat), 0));

      tmp = build3_v (COND_EXPR, tmp, dlen, build_empty_stmt (input_location));

      gfc_add_expr_to_block (&block, tmp);
    }

  return gfc_finish_block (&block);
}

