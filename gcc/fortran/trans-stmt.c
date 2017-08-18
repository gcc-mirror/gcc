/* Statement translation -- generate GCC trees from gfc_code.
   Copyright (C) 2002-2017 Free Software Foundation, Inc.
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
#include "options.h"
#include "tree.h"
#include "gfortran.h"
#include "trans.h"
#include "stringpool.h"
#include "fold-const.h"
#include "trans-stmt.h"
#include "trans-types.h"
#include "trans-array.h"
#include "trans-const.h"
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
  bool do_concurrent;
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

  if (code->label1->defined == ST_LABEL_TARGET
      || code->label1->defined == ST_LABEL_DO_TARGET)
    {
      label_tree = gfc_build_addr_expr (pvoid_type_node, label_tree);
      len_tree = integer_minus_one_node;
    }
  else
    {
      gfc_expr *format = code->label1->format;

      label_len = format->value.character.length;
      len_tree = build_int_cst (gfc_charlen_type_node, label_len);
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
  tmp = fold_build2_loc (input_location, NE_EXPR, boolean_type_node, tmp,
			 build_int_cst (TREE_TYPE (tmp), -1));
  gfc_trans_runtime_check (true, false, tmp, &se.pre, &loc,
			   "Assigned label is not a target label");

  assigned_goto = GFC_DECL_ASSIGN_ADDR (se.expr);

  /* We're going to ignore a label list.  It does not really change the
     statement's semantics (because it is just a further restriction on
     what's legal code); before, we were comparing label addresses here, but
     that's a very fragile business and may break with optimization.  So
     just ignore it.  */

  target = fold_build1_loc (input_location, GOTO_EXPR, void_type_node,
			    assigned_goto);
  gfc_add_expr_to_block (&se.pre, target);
  return gfc_finish_block (&se.pre);
}


/* Translate an ENTRY statement.  Just adds a label for this entry point.  */
tree
gfc_trans_entry (gfc_code * code)
{
  return build1_v (LABEL_EXPR, code->ext.entry->label);
}


/* Replace a gfc_ss structure by another both in the gfc_se struct
   and the gfc_loopinfo struct.  This is used in gfc_conv_elemental_dependencies
   to replace a variable ss by the corresponding temporary.  */

static void
replace_ss (gfc_se *se, gfc_ss *old_ss, gfc_ss *new_ss)
{
  gfc_ss **sess, **loopss;

  /* The old_ss is a ss for a single variable.  */
  gcc_assert (old_ss->info->type == GFC_SS_SECTION);

  for (sess = &(se->ss); *sess != gfc_ss_terminator; sess = &((*sess)->next))
    if (*sess == old_ss)
      break;
  gcc_assert (*sess != gfc_ss_terminator);

  *sess = new_ss;
  new_ss->next = old_ss->next;


  for (loopss = &(se->loop->ss); *loopss != gfc_ss_terminator;
       loopss = &((*loopss)->loop_chain))
    if (*loopss == old_ss)
      break;
  gcc_assert (*loopss != gfc_ss_terminator);

  *loopss = new_ss;
  new_ss->loop_chain = old_ss->loop_chain;
  new_ss->loop = old_ss->loop;

  gfc_free_ss (old_ss);
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
  gfc_se parmse;
  gfc_ss *ss;
  gfc_symbol *fsym;
  tree data;
  tree size;
  tree tmp;

  if (loopse->ss == NULL)
    return;

  ss = loopse->ss;
  arg0 = arg;
  formal = gfc_sym_get_dummy_args (sym);

  /* Loop over all the arguments testing for dependencies.  */
  for (; arg != NULL; arg = arg->next, formal = formal ? formal->next : NULL)
    {
      e = arg->expr;
      if (e == NULL)
	continue;

      /* Obtain the info structure for the current argument.  */
      for (ss = loopse->ss; ss && ss != gfc_ss_terminator; ss = ss->next)
	if (ss->info->expr == e)
	  break;

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
	  gfc_ss *tmp_ss;

	  tmp_ss = gfc_get_array_ss (gfc_ss_terminator, NULL, ss->dimen,
				     GFC_SS_SECTION);
	  gfc_mark_ss_chain_used (tmp_ss, 1);
	  tmp_ss->info->expr = ss->info->expr;
	  replace_ss (loopse, ss, tmp_ss);

	  /* Obtain the argument descriptor for unpacking.  */
	  gfc_init_se (&parmse, NULL);
	  parmse.want_pointer = 1;
	  gfc_conv_expr_descriptor (&parmse, e);
	  gfc_add_block_to_block (&se->pre, &parmse.pre);

	  /* If we've got INTENT(INOUT) or a derived type with INTENT(OUT),
	     initialize the array temporary with a copy of the values.  */
	  if (fsym->attr.intent == INTENT_INOUT
		|| (fsym->ts.type ==BT_DERIVED
		      && fsym->attr.intent == INTENT_OUT))
	    initial = parmse.expr;
	  /* For class expressions, we always initialize with the copy of
	     the values.  */
	  else if (e->ts.type == BT_CLASS)
	    initial = parmse.expr;
	  else
	    initial = NULL_TREE;

	  if (e->ts.type != BT_CLASS)
	    {
	     /* Find the type of the temporary to create; we don't use the type
		of e itself as this breaks for subcomponent-references in e
		(where the type of e is that of the final reference, but
		parmse.expr's type corresponds to the full derived-type).  */
	     /* TODO: Fix this somehow so we don't need a temporary of the whole
		array but instead only the components referenced.  */
	      temptype = TREE_TYPE (parmse.expr); /* Pointer to descriptor.  */
	      gcc_assert (TREE_CODE (temptype) == POINTER_TYPE);
	      temptype = TREE_TYPE (temptype);
	      temptype = gfc_get_element_type (temptype);
	    }

	  else
	    /* For class arrays signal that the size of the dynamic type has to
	       be obtained from the vtable, using the 'initial' expression.  */
	    temptype = NULL_TREE;

	  /* Generate the temporary.  Cleaning up the temporary should be the
	     very last thing done, so we add the code to a new block and add it
	     to se->post as last instructions.  */
	  size = gfc_create_var (gfc_array_index_type, NULL);
	  data = gfc_create_var (pvoid_type_node, NULL);
	  gfc_init_block (&temp_post);
	  tmp = gfc_trans_create_temp_array (&se->pre, &temp_post, tmp_ss,
					     temptype, initial, false, true,
					     false, &arg->expr->where);
	  gfc_add_modify (&se->pre, size, tmp);
	  tmp = fold_convert (pvoid_type_node, tmp_ss->info->data.array.data);
	  gfc_add_modify (&se->pre, data, tmp);

	  /* Update other ss' delta.  */
	  gfc_set_delta (loopse->loop);

	  /* Copy the result back using unpack.....  */
	  if (e->ts.type != BT_CLASS)
	    tmp = build_call_expr_loc (input_location,
			gfor_fndecl_in_unpack, 2, parmse.expr, data);
	  else
	    {
	      /* ... except for class results where the copy is
		 unconditional.  */
	      tmp = build_fold_indirect_ref_loc (input_location, parmse.expr);
	      tmp = gfc_conv_descriptor_data_get (tmp);
	      tmp = build_call_expr_loc (input_location,
					 builtin_decl_explicit (BUILT_IN_MEMCPY),
					 3, tmp, data,
					 fold_convert (size_type_node, size));
	    }
	  gfc_add_expr_to_block (&se->post, tmp);

	  /* parmse.pre is already added above.  */
	  gfc_add_block_to_block (&se->post, &parmse.post);
	  gfc_add_block_to_block (&se->post, &temp_post);
	}
    }
}


/* Get the interface symbol for the procedure corresponding to the given call.
   We can't get the procedure symbol directly as we have to handle the case
   of (deferred) type-bound procedures.  */

static gfc_symbol *
get_proc_ifc_for_call (gfc_code *c)
{
  gfc_symbol *sym;

  gcc_assert (c->op == EXEC_ASSIGN_CALL || c->op == EXEC_CALL);

  sym = gfc_get_proc_ifc_for_expr (c->expr1);

  /* Fall back/last resort try.  */
  if (sym == NULL)
    sym = c->resolved_sym;

  return sym;
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
    ss = gfc_walk_elemental_function_args (ss, code->ext.actual,
					   get_proc_ifc_for_call (code),
					   GFC_SS_REFERENCE);

  /* Is not an elemental subroutine call with array valued arguments.  */
  if (ss == gfc_ss_terminator)
    {

      /* Translate the call.  */
      has_alternate_specifier
	= gfc_conv_procedure_call (&se, code->resolved_sym, code->ext.actual,
				  code->expr1, NULL);

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
      if (code->expr1)
	gfc_conv_loop_setup (&loop, &code->expr1->where);
      else
	gfc_conv_loop_setup (&loop, &code->loc);
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
	    maskexpr = fold_build1_loc (input_location, TRUTH_NOT_EXPR,
					TREE_TYPE (maskexpr), maskexpr);
	}

      /* Add the subroutine call to the block.  */
      gfc_conv_procedure_call (&loopse, code->resolved_sym,
			       code->ext.actual, code->expr1,
			       NULL);

      if (mask && count1)
	{
	  tmp = build3_v (COND_EXPR, maskexpr, loopse.expr,
			  build_empty_stmt (input_location));
	  gfc_add_expr_to_block (&loopse.pre, tmp);
	  tmp = fold_build2_loc (input_location, PLUS_EXPR,
				 gfc_array_index_type,
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
gfc_trans_return (gfc_code * code)
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
	  gfc_warning (0,
		       "An alternate return at %L without a * dummy argument",
		       &code->expr1->where);
	  return gfc_generate_return ();
	}

      /* Start a new block for this statement.  */
      gfc_init_se (&se, NULL);
      gfc_start_block (&se.pre);

      gfc_conv_expr (&se, code->expr1);

      /* Note that the actually returned expression is a simple value and
	 does not depend on any pointers or such; thus we can clean-up with
	 se.post before returning.  */
      tmp = fold_build2_loc (input_location, MODIFY_EXPR, TREE_TYPE (result),
			     result, fold_convert (TREE_TYPE (result),
			     se.expr));
      gfc_add_expr_to_block (&se.pre, tmp);
      gfc_add_block_to_block (&se.pre, &se.post);

      tmp = gfc_generate_return ();
      gfc_add_expr_to_block (&se.pre, tmp);
      return gfc_finish_block (&se.pre);
    }

  return gfc_generate_return ();
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
      tmp = build_int_cst (gfc_int4_type_node, 0);
      tmp = build_call_expr_loc (input_location,
				 gfor_fndecl_pause_string, 2,
				 build_int_cst (pchar_type_node, 0), tmp);
    }
  else if (code->expr1->ts.type == BT_INTEGER)
    {
      gfc_conv_expr (&se, code->expr1);
      tmp = build_call_expr_loc (input_location,
				 gfor_fndecl_pause_numeric, 1,
				 fold_convert (gfc_int4_type_node, se.expr));
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
gfc_trans_stop (gfc_code *code, bool error_stop)
{
  tree gfc_int4_type_node = gfc_get_int_type (4);
  gfc_se se;
  tree tmp;

  /* Start a new block for this statement.  */
  gfc_init_se (&se, NULL);
  gfc_start_block (&se.pre);

  if (code->expr1 == NULL)
    {
      tmp = build_int_cst (gfc_int4_type_node, 0);
      tmp = build_call_expr_loc (input_location,
				 error_stop
				 ? (flag_coarray == GFC_FCOARRAY_LIB
				    ? gfor_fndecl_caf_error_stop_str
				    : gfor_fndecl_error_stop_string)
				 : (flag_coarray == GFC_FCOARRAY_LIB
				    ? gfor_fndecl_caf_stop_str
				    : gfor_fndecl_stop_string),
				 2, build_int_cst (pchar_type_node, 0), tmp);
    }
  else if (code->expr1->ts.type == BT_INTEGER)
    {
      gfc_conv_expr (&se, code->expr1);
      tmp = build_call_expr_loc (input_location,
				 error_stop
				 ? (flag_coarray == GFC_FCOARRAY_LIB
				    ? gfor_fndecl_caf_error_stop
				    : gfor_fndecl_error_stop_numeric)
				 : (flag_coarray == GFC_FCOARRAY_LIB
				    ? gfor_fndecl_caf_stop_numeric
				    : gfor_fndecl_stop_numeric), 1,
				 fold_convert (gfc_int4_type_node, se.expr));
    }
  else
    {
      gfc_conv_expr_reference (&se, code->expr1);
      tmp = build_call_expr_loc (input_location,
				 error_stop
				 ? (flag_coarray == GFC_FCOARRAY_LIB
				    ? gfor_fndecl_caf_error_stop_str
				    : gfor_fndecl_error_stop_string)
				 : (flag_coarray == GFC_FCOARRAY_LIB
				    ? gfor_fndecl_caf_stop_str
				    : gfor_fndecl_stop_string),
				 2, se.expr, se.string_length);
    }

  gfc_add_expr_to_block (&se.pre, tmp);

  gfc_add_block_to_block (&se.pre, &se.post);

  return gfc_finish_block (&se.pre);
}

/* Translate the FAIL IMAGE statement.  */

tree
gfc_trans_fail_image (gfc_code *code ATTRIBUTE_UNUSED)
{
  if (flag_coarray == GFC_FCOARRAY_LIB)
    return build_call_expr_loc (input_location,
				gfor_fndecl_caf_fail_image, 1,
				build_int_cst (pchar_type_node, 0));
  else
    {
      const char *name = gfc_get_string (PREFIX ("exit_i%d"), 4);
      gfc_symbol *exsym = gfc_get_intrinsic_sub_symbol (name);
      tree tmp = gfc_get_symbol_decl (exsym);
      return build_call_expr_loc (input_location, tmp, 1, integer_zero_node);
    }
}


tree
gfc_trans_lock_unlock (gfc_code *code, gfc_exec_op op)
{
  gfc_se se, argse;
  tree stat = NULL_TREE, stat2 = NULL_TREE;
  tree lock_acquired = NULL_TREE, lock_acquired2 = NULL_TREE;

  /* Short cut: For single images without STAT= or LOCK_ACQUIRED
     return early. (ERRMSG= is always untouched for -fcoarray=single.)  */
  if (!code->expr2 && !code->expr4 && flag_coarray != GFC_FCOARRAY_LIB)
    return NULL_TREE;

  if (code->expr2)
    {
      gcc_assert (code->expr2->expr_type == EXPR_VARIABLE);
      gfc_init_se (&argse, NULL);
      gfc_conv_expr_val (&argse, code->expr2);
      stat = argse.expr;
    }
  else if (flag_coarray == GFC_FCOARRAY_LIB)
    stat = null_pointer_node;

  if (code->expr4)
    {
      gcc_assert (code->expr4->expr_type == EXPR_VARIABLE);
      gfc_init_se (&argse, NULL);
      gfc_conv_expr_val (&argse, code->expr4);
      lock_acquired = argse.expr;
    }
  else if (flag_coarray == GFC_FCOARRAY_LIB)
    lock_acquired = null_pointer_node;

  gfc_start_block (&se.pre);
  if (flag_coarray == GFC_FCOARRAY_LIB)
    {
      tree tmp, token, image_index, errmsg, errmsg_len;
      tree index = size_zero_node;
      tree caf_decl = gfc_get_tree_for_caf_expr (code->expr1);

      if (code->expr1->symtree->n.sym->ts.type != BT_DERIVED
	  || code->expr1->symtree->n.sym->ts.u.derived->from_intmod
	     != INTMOD_ISO_FORTRAN_ENV
	  || code->expr1->symtree->n.sym->ts.u.derived->intmod_sym_id
	     != ISOFORTRAN_LOCK_TYPE)
	{
	  gfc_error ("Sorry, the lock component of derived type at %L is not "
		     "yet supported", &code->expr1->where);
	  return NULL_TREE;
	}

      gfc_get_caf_token_offset (&se, &token, NULL, caf_decl, NULL_TREE,
				code->expr1);

      if (gfc_is_coindexed (code->expr1))
	image_index = gfc_caf_get_image_index (&se.pre, code->expr1, caf_decl);
      else
	image_index = integer_zero_node;

      /* For arrays, obtain the array index.  */
      if (gfc_expr_attr (code->expr1).dimension)
	{
	  tree desc, tmp, extent, lbound, ubound;
          gfc_array_ref *ar, ar2;
          int i;

	  /* TODO: Extend this, once DT components are supported.  */
	  ar = &code->expr1->ref->u.ar;
	  ar2 = *ar;
	  memset (ar, '\0', sizeof (*ar));
	  ar->as = ar2.as;
	  ar->type = AR_FULL;

	  gfc_init_se (&argse, NULL);
	  argse.descriptor_only = 1;
	  gfc_conv_expr_descriptor (&argse, code->expr1);
	  gfc_add_block_to_block (&se.pre, &argse.pre);
	  desc = argse.expr;
	  *ar = ar2;

	  extent = integer_one_node;
	  for (i = 0; i < ar->dimen; i++)
	    {
	      gfc_init_se (&argse, NULL);
	      gfc_conv_expr_type (&argse, ar->start[i], integer_type_node);
	      gfc_add_block_to_block (&argse.pre, &argse.pre);
	      lbound = gfc_conv_descriptor_lbound_get (desc, gfc_rank_cst[i]);
	      tmp = fold_build2_loc (input_location, MINUS_EXPR,
				     integer_type_node, argse.expr,
				     fold_convert(integer_type_node, lbound));
	      tmp = fold_build2_loc (input_location, MULT_EXPR,
				     integer_type_node, extent, tmp);
	      index = fold_build2_loc (input_location, PLUS_EXPR,
				       integer_type_node, index, tmp);
	      if (i < ar->dimen - 1)
		{
		  ubound = gfc_conv_descriptor_ubound_get (desc, gfc_rank_cst[i]);
		  tmp = gfc_conv_array_extent_dim (lbound, ubound, NULL);
		  tmp = fold_convert (integer_type_node, tmp);
		  extent = fold_build2_loc (input_location, MULT_EXPR,
					    integer_type_node, extent, tmp);
		}
	    }
	}

      /* errmsg.  */
      if (code->expr3)
	{
	  gfc_init_se (&argse, NULL);
	  argse.want_pointer = 1;
	  gfc_conv_expr (&argse, code->expr3);
	  gfc_add_block_to_block (&se.pre, &argse.pre);
	  errmsg = argse.expr;
	  errmsg_len = fold_convert (integer_type_node, argse.string_length);
	}
      else
	{
	  errmsg = null_pointer_node;
	  errmsg_len = integer_zero_node;
	}

      if (stat != null_pointer_node && TREE_TYPE (stat) != integer_type_node)
	{
	  stat2 = stat;
	  stat = gfc_create_var (integer_type_node, "stat");
	}

      if (lock_acquired != null_pointer_node
	  && TREE_TYPE (lock_acquired) != integer_type_node)
	{
	  lock_acquired2 = lock_acquired;
	  lock_acquired = gfc_create_var (integer_type_node, "acquired");
	}

      if (op == EXEC_LOCK)
	tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_lock, 7,
                                   token, index, image_index,
				   lock_acquired != null_pointer_node
				   ? gfc_build_addr_expr (NULL, lock_acquired)
				   : lock_acquired,
				   stat != null_pointer_node
				   ? gfc_build_addr_expr (NULL, stat) : stat,
				   errmsg, errmsg_len);
      else
	tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_unlock, 6,
                                   token, index, image_index,
				   stat != null_pointer_node
				   ? gfc_build_addr_expr (NULL, stat) : stat,
				   errmsg, errmsg_len);
      gfc_add_expr_to_block (&se.pre, tmp);

      /* It guarantees memory consistency within the same segment */
      tmp = gfc_build_string_const (strlen ("memory")+1, "memory"),
      tmp = build5_loc (input_location, ASM_EXPR, void_type_node,
			gfc_build_string_const (1, ""), NULL_TREE, NULL_TREE,
			tree_cons (NULL_TREE, tmp, NULL_TREE), NULL_TREE);
      ASM_VOLATILE_P (tmp) = 1;

      gfc_add_expr_to_block (&se.pre, tmp);

      if (stat2 != NULL_TREE)
	gfc_add_modify (&se.pre, stat2,
			fold_convert (TREE_TYPE (stat2), stat));

      if (lock_acquired2 != NULL_TREE)
	gfc_add_modify (&se.pre, lock_acquired2,
			fold_convert (TREE_TYPE (lock_acquired2),
				      lock_acquired));

      return gfc_finish_block (&se.pre);
    }

  if (stat != NULL_TREE)
    gfc_add_modify (&se.pre, stat, build_int_cst (TREE_TYPE (stat), 0));

  if (lock_acquired != NULL_TREE)
    gfc_add_modify (&se.pre, lock_acquired,
		    fold_convert (TREE_TYPE (lock_acquired),
				  boolean_true_node));

  return gfc_finish_block (&se.pre);
}

tree
gfc_trans_event_post_wait (gfc_code *code, gfc_exec_op op)
{
  gfc_se se, argse;
  tree stat = NULL_TREE, stat2 = NULL_TREE;
  tree until_count = NULL_TREE;

  if (code->expr2)
    {
      gcc_assert (code->expr2->expr_type == EXPR_VARIABLE);
      gfc_init_se (&argse, NULL);
      gfc_conv_expr_val (&argse, code->expr2);
      stat = argse.expr;
    }
  else if (flag_coarray == GFC_FCOARRAY_LIB)
    stat = null_pointer_node;

  if (code->expr4)
    {
      gfc_init_se (&argse, NULL);
      gfc_conv_expr_val (&argse, code->expr4);
      until_count = fold_convert (integer_type_node, argse.expr);
    }
  else
    until_count = integer_one_node;

  if (flag_coarray != GFC_FCOARRAY_LIB)
    {
      gfc_start_block (&se.pre);
      gfc_init_se (&argse, NULL);
      gfc_conv_expr_val (&argse, code->expr1);

      if (op == EXEC_EVENT_POST)
	gfc_add_modify (&se.pre, argse.expr,
			fold_build2_loc (input_location, PLUS_EXPR,
				TREE_TYPE (argse.expr), argse.expr,
				build_int_cst (TREE_TYPE (argse.expr), 1)));
      else
	gfc_add_modify (&se.pre, argse.expr,
			fold_build2_loc (input_location, MINUS_EXPR,
				TREE_TYPE (argse.expr), argse.expr,
				fold_convert (TREE_TYPE (argse.expr),
					      until_count)));
      if (stat != NULL_TREE)
	gfc_add_modify (&se.pre, stat, build_int_cst (TREE_TYPE (stat), 0));

      return gfc_finish_block (&se.pre);
    }

  gfc_start_block (&se.pre);
  tree tmp, token, image_index, errmsg, errmsg_len;
  tree index = size_zero_node;
  tree caf_decl = gfc_get_tree_for_caf_expr (code->expr1);

  if (code->expr1->symtree->n.sym->ts.type != BT_DERIVED
      || code->expr1->symtree->n.sym->ts.u.derived->from_intmod
	 != INTMOD_ISO_FORTRAN_ENV
      || code->expr1->symtree->n.sym->ts.u.derived->intmod_sym_id
	 != ISOFORTRAN_EVENT_TYPE)
    {
      gfc_error ("Sorry, the event component of derived type at %L is not "
		 "yet supported", &code->expr1->where);
      return NULL_TREE;
    }

  gfc_init_se (&argse, NULL);
  gfc_get_caf_token_offset (&argse, &token, NULL, caf_decl, NULL_TREE,
			    code->expr1);
  gfc_add_block_to_block (&se.pre, &argse.pre);

  if (gfc_is_coindexed (code->expr1))
    image_index = gfc_caf_get_image_index (&se.pre, code->expr1, caf_decl);
  else
    image_index = integer_zero_node;

  /* For arrays, obtain the array index.  */
  if (gfc_expr_attr (code->expr1).dimension)
    {
      tree desc, tmp, extent, lbound, ubound;
      gfc_array_ref *ar, ar2;
      int i;

      /* TODO: Extend this, once DT components are supported.  */
      ar = &code->expr1->ref->u.ar;
      ar2 = *ar;
      memset (ar, '\0', sizeof (*ar));
      ar->as = ar2.as;
      ar->type = AR_FULL;

      gfc_init_se (&argse, NULL);
      argse.descriptor_only = 1;
      gfc_conv_expr_descriptor (&argse, code->expr1);
      gfc_add_block_to_block (&se.pre, &argse.pre);
      desc = argse.expr;
      *ar = ar2;

      extent = integer_one_node;
      for (i = 0; i < ar->dimen; i++)
	{
	  gfc_init_se (&argse, NULL);
	  gfc_conv_expr_type (&argse, ar->start[i], integer_type_node);
	  gfc_add_block_to_block (&argse.pre, &argse.pre);
	  lbound = gfc_conv_descriptor_lbound_get (desc, gfc_rank_cst[i]);
	  tmp = fold_build2_loc (input_location, MINUS_EXPR,
				 integer_type_node, argse.expr,
				 fold_convert(integer_type_node, lbound));
	  tmp = fold_build2_loc (input_location, MULT_EXPR,
				 integer_type_node, extent, tmp);
	  index = fold_build2_loc (input_location, PLUS_EXPR,
				   integer_type_node, index, tmp);
	  if (i < ar->dimen - 1)
	    {
	      ubound = gfc_conv_descriptor_ubound_get (desc, gfc_rank_cst[i]);
	      tmp = gfc_conv_array_extent_dim (lbound, ubound, NULL);
	      tmp = fold_convert (integer_type_node, tmp);
	      extent = fold_build2_loc (input_location, MULT_EXPR,
					integer_type_node, extent, tmp);
	    }
	}
    }

  /* errmsg.  */
  if (code->expr3)
    {
      gfc_init_se (&argse, NULL);
      argse.want_pointer = 1;
      gfc_conv_expr (&argse, code->expr3);
      gfc_add_block_to_block (&se.pre, &argse.pre);
      errmsg = argse.expr;
      errmsg_len = fold_convert (integer_type_node, argse.string_length);
    }
  else
    {
      errmsg = null_pointer_node;
      errmsg_len = integer_zero_node;
    }

  if (stat != null_pointer_node && TREE_TYPE (stat) != integer_type_node)
    {
      stat2 = stat;
      stat = gfc_create_var (integer_type_node, "stat");
    }

  if (op == EXEC_EVENT_POST)
    tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_event_post, 6,
			       token, index, image_index,
			       stat != null_pointer_node
			       ? gfc_build_addr_expr (NULL, stat) : stat,
			       errmsg, errmsg_len);
  else
    tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_event_wait, 6,
			       token, index, until_count,
			       stat != null_pointer_node
			       ? gfc_build_addr_expr (NULL, stat) : stat,
			       errmsg, errmsg_len);
  gfc_add_expr_to_block (&se.pre, tmp);

  /* It guarantees memory consistency within the same segment */
  tmp = gfc_build_string_const (strlen ("memory")+1, "memory"),
  tmp = build5_loc (input_location, ASM_EXPR, void_type_node,
		    gfc_build_string_const (1, ""), NULL_TREE, NULL_TREE,
		    tree_cons (NULL_TREE, tmp, NULL_TREE), NULL_TREE);
  ASM_VOLATILE_P (tmp) = 1;
  gfc_add_expr_to_block (&se.pre, tmp);

  if (stat2 != NULL_TREE)
    gfc_add_modify (&se.pre, stat2, fold_convert (TREE_TYPE (stat2), stat));

  return gfc_finish_block (&se.pre);
}

tree
gfc_trans_sync (gfc_code *code, gfc_exec_op type)
{
  gfc_se se, argse;
  tree tmp;
  tree images = NULL_TREE, stat = NULL_TREE,
       errmsg = NULL_TREE, errmsglen = NULL_TREE;

  /* Short cut: For single images without bound checking or without STAT=,
     return early. (ERRMSG= is always untouched for -fcoarray=single.)  */
  if (!code->expr2 && !(gfc_option.rtcheck & GFC_RTCHECK_BOUNDS)
      && flag_coarray != GFC_FCOARRAY_LIB)
    return NULL_TREE;

  gfc_init_se (&se, NULL);
  gfc_start_block (&se.pre);

  if (code->expr1 && code->expr1->rank == 0)
    {
      gfc_init_se (&argse, NULL);
      gfc_conv_expr_val (&argse, code->expr1);
      images = argse.expr;
    }

  if (code->expr2)
    {
      gcc_assert (code->expr2->expr_type == EXPR_VARIABLE);
      gfc_init_se (&argse, NULL);
      gfc_conv_expr_val (&argse, code->expr2);
      stat = argse.expr;
    }
  else
    stat = null_pointer_node;

  if (code->expr3 && flag_coarray == GFC_FCOARRAY_LIB)
    {
      gcc_assert (code->expr3->expr_type == EXPR_VARIABLE);
      gfc_init_se (&argse, NULL);
      argse.want_pointer = 1;
      gfc_conv_expr (&argse, code->expr3);
      gfc_conv_string_parameter (&argse);
      errmsg = gfc_build_addr_expr (NULL, argse.expr);
      errmsglen = argse.string_length;
    }
  else if (flag_coarray == GFC_FCOARRAY_LIB)
    {
      errmsg = null_pointer_node;
      errmsglen = build_int_cst (integer_type_node, 0);
    }

  /* Check SYNC IMAGES(imageset) for valid image index.
     FIXME: Add a check for image-set arrays.  */
  if (code->expr1 && (gfc_option.rtcheck & GFC_RTCHECK_BOUNDS)
      && code->expr1->rank == 0)
    {
      tree cond;
      if (flag_coarray != GFC_FCOARRAY_LIB)
	cond = fold_build2_loc (input_location, NE_EXPR, boolean_type_node,
				images, build_int_cst (TREE_TYPE (images), 1));
      else
	{
	  tree cond2;
	  tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_num_images,
				     2, integer_zero_node,
				     build_int_cst (integer_type_node, -1));
	  cond = fold_build2_loc (input_location, GT_EXPR, boolean_type_node,
				  images, tmp);
	  cond2 = fold_build2_loc (input_location, LT_EXPR, boolean_type_node,
				   images,
				   build_int_cst (TREE_TYPE (images), 1));
	  cond = fold_build2_loc (input_location, TRUTH_OR_EXPR,
				  boolean_type_node, cond, cond2);
	}
      gfc_trans_runtime_check (true, false, cond, &se.pre,
			       &code->expr1->where, "Invalid image number "
			       "%d in SYNC IMAGES",
			       fold_convert (integer_type_node, images));
    }

  /* Per F2008, 8.5.1, a SYNC MEMORY is implied by calling the
     image control statements SYNC IMAGES and SYNC ALL.  */
  if (flag_coarray == GFC_FCOARRAY_LIB)
    {
      tmp = gfc_build_string_const (strlen ("memory")+1, "memory"),
      tmp = build5_loc (input_location, ASM_EXPR, void_type_node,
			gfc_build_string_const (1, ""), NULL_TREE, NULL_TREE,
			tree_cons (NULL_TREE, tmp, NULL_TREE), NULL_TREE);
      ASM_VOLATILE_P (tmp) = 1;
      gfc_add_expr_to_block (&se.pre, tmp);
    }

  if (flag_coarray != GFC_FCOARRAY_LIB)
    {
      /* Set STAT to zero.  */
      if (code->expr2)
	gfc_add_modify (&se.pre, stat, build_int_cst (TREE_TYPE (stat), 0));
    }
  else if (type == EXEC_SYNC_ALL || type == EXEC_SYNC_MEMORY)
    {
      /* SYNC ALL           =>   stat == null_pointer_node
	 SYNC ALL(stat=s)   =>   stat has an integer type

	 If "stat" has the wrong integer type, use a temp variable of
	 the right type and later cast the result back into "stat".  */
      if (stat == null_pointer_node || TREE_TYPE (stat) == integer_type_node)
	{
	  if (TREE_TYPE (stat) == integer_type_node)
	    stat = gfc_build_addr_expr (NULL, stat);

	  if(type == EXEC_SYNC_MEMORY)
	    tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_sync_memory,
				       3, stat, errmsg, errmsglen);
	  else
	    tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_sync_all,
				       3, stat, errmsg, errmsglen);

	  gfc_add_expr_to_block (&se.pre, tmp);
	}
      else
	{
	  tree tmp_stat = gfc_create_var (integer_type_node, "stat");

	  tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_sync_all,
				     3, gfc_build_addr_expr (NULL, tmp_stat),
				     errmsg, errmsglen);
	  gfc_add_expr_to_block (&se.pre, tmp);

	  gfc_add_modify (&se.pre, stat,
			  fold_convert (TREE_TYPE (stat), tmp_stat));
	}
    }
  else
    {
      tree len;

      gcc_assert (type == EXEC_SYNC_IMAGES);

      if (!code->expr1)
	{
	  len = build_int_cst (integer_type_node, -1);
	  images = null_pointer_node;
	}
      else if (code->expr1->rank == 0)
	{
	  len = build_int_cst (integer_type_node, 1);
	  images = gfc_build_addr_expr (NULL_TREE, images);
	}
      else
	{
	  /* FIXME.  */
	  if (code->expr1->ts.kind != gfc_c_int_kind)
	    gfc_fatal_error ("Sorry, only support for integer kind %d "
			     "implemented for image-set at %L",
			     gfc_c_int_kind, &code->expr1->where);

	  gfc_conv_array_parameter (&se, code->expr1, true, NULL, NULL, &len);
	  images = se.expr;

	  tmp = gfc_typenode_for_spec (&code->expr1->ts);
	  if (GFC_ARRAY_TYPE_P (tmp) || GFC_DESCRIPTOR_TYPE_P (tmp))
	    tmp = gfc_get_element_type (tmp);

	  len = fold_build2_loc (input_location, TRUNC_DIV_EXPR,
				 TREE_TYPE (len), len,
				 fold_convert (TREE_TYPE (len),
					       TYPE_SIZE_UNIT (tmp)));
          len = fold_convert (integer_type_node, len);
	}

      /* SYNC IMAGES(imgs)        => stat == null_pointer_node
	 SYNC IMAGES(imgs,stat=s) => stat has an integer type

	 If "stat" has the wrong integer type, use a temp variable of
	 the right type and later cast the result back into "stat".  */
      if (stat == null_pointer_node || TREE_TYPE (stat) == integer_type_node)
	{
	  if (TREE_TYPE (stat) == integer_type_node)
	    stat = gfc_build_addr_expr (NULL, stat);

	  tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_sync_images,
				     5, fold_convert (integer_type_node, len),
				     images, stat, errmsg, errmsglen);
	  gfc_add_expr_to_block (&se.pre, tmp);
	}
      else
	{
	  tree tmp_stat = gfc_create_var (integer_type_node, "stat");

	  tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_sync_images,
				     5, fold_convert (integer_type_node, len),
				     images, gfc_build_addr_expr (NULL, tmp_stat),
				     errmsg, errmsglen);
	  gfc_add_expr_to_block (&se.pre, tmp);

	  gfc_add_modify (&se.pre, stat,
			  fold_convert (TREE_TYPE (stat), tmp_stat));
	}
    }

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
  locus saved_loc;
  location_t loc;

  /* Check for an unconditional ELSE clause.  */
  if (!code->expr1)
    return gfc_trans_code (code->next);

  /* Initialize a statement builder for each block. Puts in NULL_TREEs.  */
  gfc_init_se (&if_se, NULL);
  gfc_start_block (&if_se.pre);

  /* Calculate the IF condition expression.  */
  if (code->expr1->where.lb)
    {
      gfc_save_backend_locus (&saved_loc);
      gfc_set_backend_locus (&code->expr1->where);
    }

  gfc_conv_expr_val (&if_se, code->expr1);

  if (code->expr1->where.lb)
    gfc_restore_backend_locus (&saved_loc);

  /* Translate the THEN clause.  */
  stmt = gfc_trans_code (code->next);

  /* Translate the ELSE clause.  */
  if (code->block)
    elsestmt = gfc_trans_if_1 (code->block);
  else
    elsestmt = build_empty_stmt (input_location);

  /* Build the condition expression and add it to the condition block.  */
  loc = code->expr1->where.lb ? code->expr1->where.lb->location : input_location;
  stmt = fold_build3_loc (loc, COND_EXPR, void_type_node, if_se.expr, stmt,
			  elsestmt);

  gfc_add_expr_to_block (&if_se.pre, stmt);

  /* Finish off this statement.  */
  return gfc_finish_block (&if_se.pre);
}

tree
gfc_trans_if (gfc_code * code)
{
  stmtblock_t body;
  tree exit_label;

  /* Create exit label so it is available for trans'ing the body code.  */
  exit_label = gfc_build_label_decl (NULL_TREE);
  code->exit_label = exit_label;

  /* Translate the actual code in code->block.  */
  gfc_init_block (&body);
  gfc_add_expr_to_block (&body, gfc_trans_if_1 (code->block));

  /* Add exit label.  */
  gfc_add_expr_to_block (&body, build1_v (LABEL_EXPR, exit_label));

  return gfc_finish_block (&body);
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
        tmp = fold_build2_loc (input_location, LT_EXPR, boolean_type_node,
			       se.expr, zero);
      else
        tmp = fold_build2_loc (input_location, NE_EXPR, boolean_type_node,
			       se.expr, zero);

      branch1 = fold_build3_loc (input_location, COND_EXPR, void_type_node,
				 tmp, branch1, branch2);
    }
  else
    branch1 = build1_v (GOTO_EXPR, gfc_get_label_decl (code->label1));

  if (code->label1->value != code->label3->value
      && code->label2->value != code->label3->value)
    {
      /* if (cond <= 0) take branch1 else take branch2.  */
      branch2 = build1_v (GOTO_EXPR, gfc_get_label_decl (code->label3));
      tmp = fold_build2_loc (input_location, LE_EXPR, boolean_type_node,
			     se.expr, zero);
      branch1 = fold_build3_loc (input_location, COND_EXPR, void_type_node,
				 tmp, branch1, branch2);
    }

  /* Append the COND_EXPR to the evaluation of COND, and return.  */
  gfc_add_expr_to_block (&se.pre, branch1);
  return gfc_finish_block (&se.pre);
}


/* Translate a CRITICAL block.  */
tree
gfc_trans_critical (gfc_code *code)
{
  stmtblock_t block;
  tree tmp, token = NULL_TREE;

  gfc_start_block (&block);

  if (flag_coarray == GFC_FCOARRAY_LIB)
    {
      token = gfc_get_symbol_decl (code->resolved_sym);
      token = GFC_TYPE_ARRAY_CAF_TOKEN (TREE_TYPE (token));
      tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_lock, 7,
				 token, integer_zero_node, integer_one_node,
				 null_pointer_node, null_pointer_node,
				 null_pointer_node, integer_zero_node);
      gfc_add_expr_to_block (&block, tmp);

      /* It guarantees memory consistency within the same segment */
      tmp = gfc_build_string_const (strlen ("memory")+1, "memory"),
	tmp = build5_loc (input_location, ASM_EXPR, void_type_node,
			  gfc_build_string_const (1, ""),
			  NULL_TREE, NULL_TREE,
			  tree_cons (NULL_TREE, tmp, NULL_TREE),
			  NULL_TREE);
      ASM_VOLATILE_P (tmp) = 1;

      gfc_add_expr_to_block (&block, tmp);
    }

  tmp = gfc_trans_code (code->block->next);
  gfc_add_expr_to_block (&block, tmp);

  if (flag_coarray == GFC_FCOARRAY_LIB)
    {
      tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_unlock, 6,
				 token, integer_zero_node, integer_one_node,
				 null_pointer_node, null_pointer_node,
				 integer_zero_node);
      gfc_add_expr_to_block (&block, tmp);

      /* It guarantees memory consistency within the same segment */
      tmp = gfc_build_string_const (strlen ("memory")+1, "memory"),
	tmp = build5_loc (input_location, ASM_EXPR, void_type_node,
			  gfc_build_string_const (1, ""),
			  NULL_TREE, NULL_TREE,
			  tree_cons (NULL_TREE, tmp, NULL_TREE),
			  NULL_TREE);
      ASM_VOLATILE_P (tmp) = 1;

      gfc_add_expr_to_block (&block, tmp);
    }

  return gfc_finish_block (&block);
}


/* Return true, when the class has a _len component.  */

static bool
class_has_len_component (gfc_symbol *sym)
{
  gfc_component *comp = sym->ts.u.derived->components;
  while (comp)
    {
      if (strcmp (comp->name, "_len") == 0)
	return true;
      comp = comp->next;
    }
  return false;
}


/* Do proper initialization for ASSOCIATE names.  */

static void
trans_associate_var (gfc_symbol *sym, gfc_wrapped_block *block)
{
  gfc_expr *e;
  tree tmp;
  bool class_target;
  bool unlimited;
  tree desc;
  tree offset;
  tree dim;
  int n;
  tree charlen;
  bool need_len_assign;

  gcc_assert (sym->assoc);
  e = sym->assoc->target;

  class_target = (e->expr_type == EXPR_VARIABLE)
		    && (gfc_is_class_scalar_expr (e)
			|| gfc_is_class_array_ref (e, NULL));

  unlimited = UNLIMITED_POLY (e);

  /* Assignments to the string length need to be generated, when
     ( sym is a char array or
       sym has a _len component)
     and the associated expression is unlimited polymorphic, which is
     not (yet) correctly in 'unlimited', because for an already associated
     BT_DERIVED the u-poly flag is not set, i.e.,
      __tmp_CHARACTER_0_1 => w => arg
       ^ generated temp      ^ from code, the w does not have the u-poly
     flag set, where UNLIMITED_POLY(e) expects it.  */
  need_len_assign = ((unlimited || (e->ts.type == BT_DERIVED
                     && e->ts.u.derived->attr.unlimited_polymorphic))
      && (sym->ts.type == BT_CHARACTER
          || ((sym->ts.type == BT_CLASS || sym->ts.type == BT_DERIVED)
              && class_has_len_component (sym))));
  /* Do a `pointer assignment' with updated descriptor (or assign descriptor
     to array temporary) for arrays with either unknown shape or if associating
     to a variable.  */
  if (sym->attr.dimension && !class_target
      && (sym->as->type == AS_DEFERRED || sym->assoc->variable))
    {
      gfc_se se;
      tree desc;
      bool cst_array_ctor;

      desc = sym->backend_decl;
      cst_array_ctor = e->expr_type == EXPR_ARRAY
	      && gfc_constant_array_constructor_p (e->value.constructor);

      /* If association is to an expression, evaluate it and create temporary.
	 Otherwise, get descriptor of target for pointer assignment.  */
      gfc_init_se (&se, NULL);
      if (sym->assoc->variable || cst_array_ctor)
	{
	  se.direct_byref = 1;
	  se.use_offset = 1;
	  se.expr = desc;
	}

      gfc_conv_expr_descriptor (&se, e);

      /* If we didn't already do the pointer assignment, set associate-name
	 descriptor to the one generated for the temporary.  */
      if (!sym->assoc->variable && !cst_array_ctor)
	{
	  int dim;

	  gfc_add_modify (&se.pre, desc, se.expr);

	  /* The generated descriptor has lower bound zero (as array
	     temporary), shift bounds so we get lower bounds of 1.  */
	  for (dim = 0; dim < e->rank; ++dim)
	    gfc_conv_shift_descriptor_lbound (&se.pre, desc,
					      dim, gfc_index_one_node);
	}

      /* If this is a subreference array pointer associate name use the
	 associate variable element size for the value of 'span'.  */
      if (sym->attr.subref_array_pointer)
	{
	  gcc_assert (e->expr_type == EXPR_VARIABLE);
	  tmp = e->symtree->n.sym->ts.type == BT_CLASS
	      ? gfc_class_data_get (e->symtree->n.sym->backend_decl)
	      : e->symtree->n.sym->backend_decl;
	  tmp = gfc_get_element_type (TREE_TYPE (tmp));
	  tmp = fold_convert (gfc_array_index_type, size_in_bytes (tmp));
	  gfc_add_modify (&se.pre, GFC_DECL_SPAN(desc), tmp);
	}

      /* Done, register stuff as init / cleanup code.  */
      gfc_add_init_cleanup (block, gfc_finish_block (&se.pre),
			    gfc_finish_block (&se.post));
    }

  /* Temporaries, arising from TYPE IS, just need the descriptor of class
     arrays to be assigned directly.  */
  else if (class_target && sym->attr.dimension
	   && (sym->ts.type == BT_DERIVED || unlimited))
    {
      gfc_se se;

      gfc_init_se (&se, NULL);
      se.descriptor_only = 1;
      /* In a select type the (temporary) associate variable shall point to
	 a standard fortran array (lower bound == 1), but conv_expr ()
	 just maps to the input array in the class object, whose lbound may
	 be arbitrary.  conv_expr_descriptor solves this by inserting a
	 temporary array descriptor.  */
      gfc_conv_expr_descriptor (&se, e);

      gcc_assert (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (se.expr))
		  || GFC_ARRAY_TYPE_P (TREE_TYPE (se.expr)));
      gcc_assert (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (sym->backend_decl)));

      if (GFC_ARRAY_TYPE_P (TREE_TYPE (se.expr)))
	{
	  if (INDIRECT_REF_P (se.expr))
	    tmp = TREE_OPERAND (se.expr, 0);
	  else
	    tmp = se.expr;

	  gfc_add_modify (&se.pre, sym->backend_decl,
			  gfc_class_data_get (GFC_DECL_SAVED_DESCRIPTOR (tmp)));
	}
      else
	gfc_add_modify (&se.pre, sym->backend_decl, se.expr);

      if (unlimited)
	{
	  /* Recover the dtype, which has been overwritten by the
	     assignment from an unlimited polymorphic object.  */
	  tmp = gfc_conv_descriptor_dtype (sym->backend_decl);
	  gfc_add_modify (&se.pre, tmp,
			  gfc_get_dtype (TREE_TYPE (sym->backend_decl)));
	}

      gfc_add_init_cleanup (block, gfc_finish_block (&se.pre),
			    gfc_finish_block (&se.post));
    }

  /* Do a scalar pointer assignment; this is for scalar variable targets.  */
  else if (gfc_is_associate_pointer (sym))
    {
      gfc_se se;

      gcc_assert (!sym->attr.dimension);

      gfc_init_se (&se, NULL);

      /* Class associate-names come this way because they are
	 unconditionally associate pointers and the symbol is scalar.  */
      if (sym->ts.type == BT_CLASS && CLASS_DATA (sym)->attr.dimension)
	{
	  tree target_expr;
	  /* For a class array we need a descriptor for the selector.  */
	  gfc_conv_expr_descriptor (&se, e);
	  /* Needed to get/set the _len component below.  */
	  target_expr = se.expr;

	  /* Obtain a temporary class container for the result.  */
	  gfc_conv_class_to_class (&se, e, sym->ts, false, true, false, false);
	  se.expr = build_fold_indirect_ref_loc (input_location, se.expr);

	  /* Set the offset.  */
	  desc = gfc_class_data_get (se.expr);
	  offset = gfc_index_zero_node;
	  for (n = 0; n < e->rank; n++)
	    {
	      dim = gfc_rank_cst[n];
	      tmp = fold_build2_loc (input_location, MULT_EXPR,
				     gfc_array_index_type,
				     gfc_conv_descriptor_stride_get (desc, dim),
				     gfc_conv_descriptor_lbound_get (desc, dim));
	      offset = fold_build2_loc (input_location, MINUS_EXPR,
				        gfc_array_index_type,
				        offset, tmp);
	    }
	  if (need_len_assign)
	    {
	      if (e->symtree
		  && DECL_LANG_SPECIFIC (e->symtree->n.sym->backend_decl)
		 && GFC_DECL_SAVED_DESCRIPTOR (e->symtree->n.sym->backend_decl))
		/* Use the original class descriptor stored in the saved
		   descriptor to get the target_expr.  */
		target_expr =
		    GFC_DECL_SAVED_DESCRIPTOR (e->symtree->n.sym->backend_decl);
	      else
		/* Strip the _data component from the target_expr.  */
		target_expr = TREE_OPERAND (target_expr, 0);
	      /* Add a reference to the _len comp to the target expr.  */
	      tmp = gfc_class_len_get (target_expr);
	      /* Get the component-ref for the temp structure's _len comp.  */
	      charlen = gfc_class_len_get (se.expr);
	      /* Add the assign to the beginning of the block...  */
	      gfc_add_modify (&se.pre, charlen,
			      fold_convert (TREE_TYPE (charlen), tmp));
	      /* and the oposite way at the end of the block, to hand changes
		 on the string length back.  */
	      gfc_add_modify (&se.post, tmp,
			      fold_convert (TREE_TYPE (tmp), charlen));
	      /* Length assignment done, prevent adding it again below.  */
	      need_len_assign = false;
	    }
	  gfc_conv_descriptor_offset_set (&se.pre, desc, offset);
	}
      else if (sym->ts.type == BT_CLASS && e->ts.type == BT_CLASS
	       && CLASS_DATA (e)->attr.dimension)
	{
	  /* This is bound to be a class array element.  */
	  gfc_conv_expr_reference (&se, e);
	  /* Get the _vptr component of the class object.  */
	  tmp = gfc_get_vptr_from_expr (se.expr);
	  /* Obtain a temporary class container for the result.  */
	  gfc_conv_derived_to_class (&se, e, sym->ts, tmp, false, false);
	  se.expr = build_fold_indirect_ref_loc (input_location, se.expr);
	}
      else
	{
	  /* For BT_CLASS and BT_DERIVED, this boils down to a pointer assign,
	     which has the string length included.  For CHARACTERS it is still
	     needed and will be done at the end of this routine.  */
	  gfc_conv_expr (&se, e);
	  need_len_assign = need_len_assign && sym->ts.type == BT_CHARACTER;
	}

      tmp = TREE_TYPE (sym->backend_decl);
      tmp = gfc_build_addr_expr (tmp, se.expr);
      gfc_add_modify (&se.pre, sym->backend_decl, tmp);

      gfc_add_init_cleanup (block, gfc_finish_block( &se.pre),
			    gfc_finish_block (&se.post));
    }

  /* Do a simple assignment.  This is for scalar expressions, where we
     can simply use expression assignment.  */
  else
    {
      gfc_expr *lhs;

      lhs = gfc_lval_expr_from_sym (sym);
      tmp = gfc_trans_assignment (lhs, e, false, true);
      gfc_add_init_cleanup (block, tmp, NULL_TREE);
    }

  /* Set the stringlength, when needed.  */
  if (need_len_assign)
    {
      gfc_se se;
      gfc_init_se (&se, NULL);
      if (e->symtree->n.sym->ts.type == BT_CHARACTER)
	{
	  /* What about deferred strings?  */
	  gcc_assert (!e->symtree->n.sym->ts.deferred);
	  tmp = e->symtree->n.sym->ts.u.cl->backend_decl;
	}
      else
	tmp = gfc_class_len_get (gfc_get_symbol_decl (e->symtree->n.sym));
      gfc_get_symbol_decl (sym);
      charlen = sym->ts.type == BT_CHARACTER ? sym->ts.u.cl->backend_decl
					: gfc_class_len_get (sym->backend_decl);
      /* Prevent adding a noop len= len.  */
      if (tmp != charlen)
	{
	  gfc_add_modify (&se.pre, charlen,
			  fold_convert (TREE_TYPE (charlen), tmp));
	  gfc_add_init_cleanup (block, gfc_finish_block (&se.pre),
				gfc_finish_block (&se.post));
	}
    }
}


/* Translate a BLOCK construct.  This is basically what we would do for a
   procedure body.  */

tree
gfc_trans_block_construct (gfc_code* code)
{
  gfc_namespace* ns;
  gfc_symbol* sym;
  gfc_wrapped_block block;
  tree exit_label;
  stmtblock_t body;
  gfc_association_list *ass;

  ns = code->ext.block.ns;
  gcc_assert (ns);
  sym = ns->proc_name;
  gcc_assert (sym);

  /* Process local variables.  */
  gcc_assert (!sym->tlink);
  sym->tlink = sym;
  gfc_process_block_locals (ns);

  /* Generate code including exit-label.  */
  gfc_init_block (&body);
  exit_label = gfc_build_label_decl (NULL_TREE);
  code->exit_label = exit_label;

  finish_oacc_declare (ns, sym, true);

  gfc_add_expr_to_block (&body, gfc_trans_code (ns->code));
  gfc_add_expr_to_block (&body, build1_v (LABEL_EXPR, exit_label));

  /* Finish everything.  */
  gfc_start_wrapped_block (&block, gfc_finish_block (&body));
  gfc_trans_deferred_vars (sym, &block);
  for (ass = code->ext.block.assoc; ass; ass = ass->next)
    trans_associate_var (ass->st->n.sym, &block);

  return gfc_finish_wrapped_block (&block);
}

/* Translate the simple DO construct in a C-style manner.
   This is where the loop variable has integer type and step +-1.
   Following code will generate infinite loop in case where TO is INT_MAX
   (for +1 step) or INT_MIN (for -1 step)

   We translate a do loop from:

   DO dovar = from, to, step
      body
   END DO

   to:

   [Evaluate loop bounds and step]
    dovar = from;
    for (;;)
      {
	if (dovar > to)
	  goto end_label;
	body;
	cycle_label:
	dovar += step;
      }
    end_label:

   This helps the optimizers by avoiding the extra pre-header condition and
   we save a register as we just compare the updated IV (not a value in
   previous step).  */

static tree
gfc_trans_simple_do (gfc_code * code, stmtblock_t *pblock, tree dovar,
		     tree from, tree to, tree step, tree exit_cond)
{
  stmtblock_t body;
  tree type;
  tree cond;
  tree tmp;
  tree saved_dovar = NULL;
  tree cycle_label;
  tree exit_label;
  location_t loc;
  type = TREE_TYPE (dovar);
  bool is_step_positive = tree_int_cst_sgn (step) > 0;

  loc = code->ext.iterator->start->where.lb->location;

  /* Initialize the DO variable: dovar = from.  */
  gfc_add_modify_loc (loc, pblock, dovar,
		      fold_convert (TREE_TYPE (dovar), from));

  /* Save value for do-tinkering checking.  */
  if (gfc_option.rtcheck & GFC_RTCHECK_DO)
    {
      saved_dovar = gfc_create_var (type, ".saved_dovar");
      gfc_add_modify_loc (loc, pblock, saved_dovar, dovar);
    }

  /* Cycle and exit statements are implemented with gotos.  */
  cycle_label = gfc_build_label_decl (NULL_TREE);
  exit_label = gfc_build_label_decl (NULL_TREE);

  /* Put the labels where they can be found later.  See gfc_trans_do().  */
  code->cycle_label = cycle_label;
  code->exit_label = exit_label;

  /* Loop body.  */
  gfc_start_block (&body);

  /* Exit the loop if there is an I/O result condition or error.  */
  if (exit_cond)
    {
      tmp = build1_v (GOTO_EXPR, exit_label);
      tmp = fold_build3_loc (loc, COND_EXPR, void_type_node,
			     exit_cond, tmp,
			     build_empty_stmt (loc));
      gfc_add_expr_to_block (&body, tmp);
    }

  /* Evaluate the loop condition.  */
  if (is_step_positive)
    cond = fold_build2_loc (loc, GT_EXPR, boolean_type_node, dovar,
			    fold_convert (type, to));
  else
    cond = fold_build2_loc (loc, LT_EXPR, boolean_type_node, dovar,
			    fold_convert (type, to));

  cond = gfc_evaluate_now_loc (loc, cond, &body);

  /* The loop exit.  */
  tmp = fold_build1_loc (loc, GOTO_EXPR, void_type_node, exit_label);
  TREE_USED (exit_label) = 1;
  tmp = fold_build3_loc (loc, COND_EXPR, void_type_node,
			 cond, tmp, build_empty_stmt (loc));
  gfc_add_expr_to_block (&body, tmp);

  /* Check whether the induction variable is equal to INT_MAX
     (respectively to INT_MIN).  */
  if (gfc_option.rtcheck & GFC_RTCHECK_DO)
    {
      tree boundary = is_step_positive ? TYPE_MAX_VALUE (type)
	: TYPE_MIN_VALUE (type);

      tmp = fold_build2_loc (loc, EQ_EXPR, boolean_type_node,
			     dovar, boundary);
      gfc_trans_runtime_check (true, false, tmp, &body, &code->loc,
			       "Loop iterates infinitely");
    }

  /* Main loop body.  */
  tmp = gfc_trans_code_cond (code->block->next, exit_cond);
  gfc_add_expr_to_block (&body, tmp);

  /* Label for cycle statements (if needed).  */
  if (TREE_USED (cycle_label))
    {
      tmp = build1_v (LABEL_EXPR, cycle_label);
      gfc_add_expr_to_block (&body, tmp);
    }

  /* Check whether someone has modified the loop variable.  */
  if (gfc_option.rtcheck & GFC_RTCHECK_DO)
    {
      tmp = fold_build2_loc (loc, NE_EXPR, boolean_type_node,
			     dovar, saved_dovar);
      gfc_trans_runtime_check (true, false, tmp, &body, &code->loc,
			       "Loop variable has been modified");
    }

  /* Increment the loop variable.  */
  tmp = fold_build2_loc (loc, PLUS_EXPR, type, dovar, step);
  gfc_add_modify_loc (loc, &body, dovar, tmp);

  if (gfc_option.rtcheck & GFC_RTCHECK_DO)
    gfc_add_modify_loc (loc, &body, saved_dovar, dovar);

  /* Finish the loop body.  */
  tmp = gfc_finish_block (&body);
  tmp = fold_build1_loc (loc, LOOP_EXPR, void_type_node, tmp);

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
       countm1t = countm1;
       countm1--;
       if (countm1t == 0) goto exit_label;
     }
exit_label:

   countm1 is an unsigned integer.  It is equal to the loop count minus one,
   because the loop count itself can overflow.  */

tree
gfc_trans_do (gfc_code * code, tree exit_cond)
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
  stmtblock_t block;
  stmtblock_t body;
  location_t loc;

  gfc_start_block (&block);

  loc = code->ext.iterator->start->where.lb->location;

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
      tmp = fold_build2_loc (input_location, EQ_EXPR, boolean_type_node, step,
			     build_zero_cst (type));
      gfc_trans_runtime_check (true, false, tmp, &block, &code->loc,
			       "DO step value is zero");
    }

  /* Special case simple loops.  */
  if (TREE_CODE (type) == INTEGER_TYPE
      && (integer_onep (step)
	|| tree_int_cst_equal (step, integer_minus_one_node)))
    return gfc_trans_simple_do (code, &block, dovar, from, to, step,
				exit_cond);

  if (TREE_CODE (type) == INTEGER_TYPE)
    utype = unsigned_type_for (type);
  else
    utype = unsigned_type_for (gfc_array_index_type);
  countm1 = gfc_create_var (utype, "countm1");

  /* Cycle and exit statements are implemented with gotos.  */
  cycle_label = gfc_build_label_decl (NULL_TREE);
  exit_label = gfc_build_label_decl (NULL_TREE);
  TREE_USED (exit_label) = 1;

  /* Put these labels where they can be found later.  */
  code->cycle_label = cycle_label;
  code->exit_label = exit_label;

  /* Initialize the DO variable: dovar = from.  */
  gfc_add_modify (&block, dovar, from);

  /* Save value for do-tinkering checking.  */
  if (gfc_option.rtcheck & GFC_RTCHECK_DO)
    {
      saved_dovar = gfc_create_var (type, ".saved_dovar");
      gfc_add_modify_loc (loc, &block, saved_dovar, dovar);
    }

  /* Initialize loop count and jump to exit label if the loop is empty.
     This code is executed before we enter the loop body. We generate:
     if (step > 0)
       {
	 countm1 = (to - from) / step;
	 if (to < from)
	   goto exit_label;
       }
     else
       {
	 countm1 = (from - to) / -step;
	 if (to > from)
	   goto exit_label;
       }
   */

  if (TREE_CODE (type) == INTEGER_TYPE)
    {
      tree pos, neg, tou, fromu, stepu, tmp2;

      /* The distance from FROM to TO cannot always be represented in a signed
         type, thus use unsigned arithmetic, also to avoid any undefined
	 overflow issues.  */
      tou = fold_convert (utype, to);
      fromu = fold_convert (utype, from);
      stepu = fold_convert (utype, step);

      /* For a positive step, when to < from, exit, otherwise compute
         countm1 = ((unsigned)to - (unsigned)from) / (unsigned)step  */
      tmp = fold_build2_loc (loc, LT_EXPR, boolean_type_node, to, from);
      tmp2 = fold_build2_loc (loc, TRUNC_DIV_EXPR, utype,
			      fold_build2_loc (loc, MINUS_EXPR, utype,
					       tou, fromu),
			      stepu);
      pos = build2 (COMPOUND_EXPR, void_type_node,
		    fold_build2 (MODIFY_EXPR, void_type_node,
				 countm1, tmp2),
		    build3_loc (loc, COND_EXPR, void_type_node,
				gfc_unlikely (tmp, PRED_FORTRAN_LOOP_PREHEADER),
				build1_loc (loc, GOTO_EXPR, void_type_node,
					    exit_label), NULL_TREE));

      /* For a negative step, when to > from, exit, otherwise compute
         countm1 = ((unsigned)from - (unsigned)to) / -(unsigned)step  */
      tmp = fold_build2_loc (loc, GT_EXPR, boolean_type_node, to, from);
      tmp2 = fold_build2_loc (loc, TRUNC_DIV_EXPR, utype,
			      fold_build2_loc (loc, MINUS_EXPR, utype,
					       fromu, tou),
			      fold_build1_loc (loc, NEGATE_EXPR, utype, stepu));
      neg = build2 (COMPOUND_EXPR, void_type_node,
		    fold_build2 (MODIFY_EXPR, void_type_node,
				 countm1, tmp2),
		    build3_loc (loc, COND_EXPR, void_type_node,
				gfc_unlikely (tmp, PRED_FORTRAN_LOOP_PREHEADER),
				build1_loc (loc, GOTO_EXPR, void_type_node,
					    exit_label), NULL_TREE));

      tmp = fold_build2_loc (loc, LT_EXPR, boolean_type_node, step,
			     build_int_cst (TREE_TYPE (step), 0));
      tmp = fold_build3_loc (loc, COND_EXPR, void_type_node, tmp, neg, pos);

      gfc_add_expr_to_block (&block, tmp);
    }
  else
    {
      tree pos_step;

      /* TODO: We could use the same width as the real type.
	 This would probably cause more problems that it solves
	 when we implement "long double" types.  */

      tmp = fold_build2_loc (loc, MINUS_EXPR, type, to, from);
      tmp = fold_build2_loc (loc, RDIV_EXPR, type, tmp, step);
      tmp = fold_build1_loc (loc, FIX_TRUNC_EXPR, utype, tmp);
      gfc_add_modify (&block, countm1, tmp);

      /* We need a special check for empty loops:
	 empty = (step > 0 ? to < from : to > from);  */
      pos_step = fold_build2_loc (loc, GT_EXPR, boolean_type_node, step,
				  build_zero_cst (type));
      tmp = fold_build3_loc (loc, COND_EXPR, boolean_type_node, pos_step,
			     fold_build2_loc (loc, LT_EXPR,
					      boolean_type_node, to, from),
			     fold_build2_loc (loc, GT_EXPR,
					      boolean_type_node, to, from));
      /* If the loop is empty, go directly to the exit label.  */
      tmp = fold_build3_loc (loc, COND_EXPR, void_type_node, tmp,
			 build1_v (GOTO_EXPR, exit_label),
			 build_empty_stmt (input_location));
      gfc_add_expr_to_block (&block, tmp);
    }

  /* Loop body.  */
  gfc_start_block (&body);

  /* Main loop body.  */
  tmp = gfc_trans_code_cond (code->block->next, exit_cond);
  gfc_add_expr_to_block (&body, tmp);

  /* Label for cycle statements (if needed).  */
  if (TREE_USED (cycle_label))
    {
      tmp = build1_v (LABEL_EXPR, cycle_label);
      gfc_add_expr_to_block (&body, tmp);
    }

  /* Check whether someone has modified the loop variable.  */
  if (gfc_option.rtcheck & GFC_RTCHECK_DO)
    {
      tmp = fold_build2_loc (loc, NE_EXPR, boolean_type_node, dovar,
			     saved_dovar);
      gfc_trans_runtime_check (true, false, tmp, &body, &code->loc,
			       "Loop variable has been modified");
    }

  /* Exit the loop if there is an I/O result condition or error.  */
  if (exit_cond)
    {
      tmp = build1_v (GOTO_EXPR, exit_label);
      tmp = fold_build3_loc (loc, COND_EXPR, void_type_node,
			     exit_cond, tmp,
			     build_empty_stmt (input_location));
      gfc_add_expr_to_block (&body, tmp);
    }

  /* Increment the loop variable.  */
  tmp = fold_build2_loc (loc, PLUS_EXPR, type, dovar, step);
  gfc_add_modify_loc (loc, &body, dovar, tmp);

  if (gfc_option.rtcheck & GFC_RTCHECK_DO)
    gfc_add_modify_loc (loc, &body, saved_dovar, dovar);

  /* Initialize countm1t.  */
  tree countm1t = gfc_create_var (utype, "countm1t");
  gfc_add_modify_loc (loc, &body, countm1t, countm1);

  /* Decrement the loop count.  */
  tmp = fold_build2_loc (loc, MINUS_EXPR, utype, countm1,
			 build_int_cst (utype, 1));
  gfc_add_modify_loc (loc, &body, countm1, tmp);

  /* End with the loop condition.  Loop until countm1t == 0.  */
  cond = fold_build2_loc (loc, EQ_EXPR, boolean_type_node, countm1t,
			  build_int_cst (utype, 0));
  tmp = fold_build1_loc (loc, GOTO_EXPR, void_type_node, exit_label);
  tmp = fold_build3_loc (loc, COND_EXPR, void_type_node,
			 cond, tmp, build_empty_stmt (loc));
  gfc_add_expr_to_block (&body, tmp);

  /* End of loop body.  */
  tmp = gfc_finish_block (&body);

  /* The for loop itself.  */
  tmp = fold_build1_loc (loc, LOOP_EXPR, void_type_node, tmp);
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
  code->cycle_label = cycle_label;
  code->exit_label = exit_label;

  /* Create a GIMPLE version of the exit condition.  */
  gfc_init_se (&cond, NULL);
  gfc_conv_expr_val (&cond, code->expr1);
  gfc_add_block_to_block (&block, &cond.pre);
  cond.expr = fold_build1_loc (code->expr1->where.lb->location,
			       TRUTH_NOT_EXPR, TREE_TYPE (cond.expr), cond.expr);

  /* Build "IF (! cond) GOTO exit_label".  */
  tmp = build1_v (GOTO_EXPR, exit_label);
  TREE_USED (exit_label) = 1;
  tmp = fold_build3_loc (code->expr1->where.lb->location, COND_EXPR,
			 void_type_node, cond.expr, tmp,
			 build_empty_stmt (code->expr1->where.lb->location));
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
  tmp = fold_build1_loc (code->expr1->where.lb->location, LOOP_EXPR,
			 void_type_node, tmp);
  gfc_add_expr_to_block (&block, tmp);

  /* Add the exit label.  */
  tmp = build1_v (LABEL_EXPR, exit_label);
  gfc_add_expr_to_block (&block, tmp);

  return gfc_finish_block (&block);
}


/* Deal with the particular case of SELECT_TYPE, where the vtable
   addresses are used for the selection. Since these are not sorted,
   the selection has to be made by a series of if statements.  */

static tree
gfc_trans_select_type_cases (gfc_code * code)
{
  gfc_code *c;
  gfc_case *cp;
  tree tmp;
  tree cond;
  tree low;
  tree high;
  gfc_se se;
  gfc_se cse;
  stmtblock_t block;
  stmtblock_t body;
  bool def = false;
  gfc_expr *e;
  gfc_start_block (&block);

  /* Calculate the switch expression.  */
  gfc_init_se (&se, NULL);
  gfc_conv_expr_val (&se, code->expr1);
  gfc_add_block_to_block (&block, &se.pre);

  /* Generate an expression for the selector hash value, for
     use to resolve character cases.  */
  e = gfc_copy_expr (code->expr1->value.function.actual->expr);
  gfc_add_hash_component (e);

  TREE_USED (code->exit_label) = 0;

repeat:
  for (c = code->block; c; c = c->block)
    {
      cp = c->ext.block.case_list;

      /* Assume it's the default case.  */
      low = NULL_TREE;
      high = NULL_TREE;
      tmp = NULL_TREE;

      /* Put the default case at the end.  */
      if ((!def && !cp->low) || (def && cp->low))
	continue;

      if (cp->low && (cp->ts.type == BT_CLASS
		      || cp->ts.type == BT_DERIVED))
	{
	  gfc_init_se (&cse, NULL);
	  gfc_conv_expr_val (&cse, cp->low);
	  gfc_add_block_to_block (&block, &cse.pre);
	  low = cse.expr;
	}
      else if (cp->ts.type != BT_UNKNOWN)
	{
	  gcc_assert (cp->high);
	  gfc_init_se (&cse, NULL);
	  gfc_conv_expr_val (&cse, cp->high);
	  gfc_add_block_to_block (&block, &cse.pre);
	  high = cse.expr;
	}

      gfc_init_block (&body);

      /* Add the statements for this case.  */
      tmp = gfc_trans_code (c->next);
      gfc_add_expr_to_block (&body, tmp);

      /* Break to the end of the SELECT TYPE construct.  The default
	 case just falls through.  */
      if (!def)
	{
	  TREE_USED (code->exit_label) = 1;
	  tmp = build1_v (GOTO_EXPR, code->exit_label);
	  gfc_add_expr_to_block (&body, tmp);
	}

      tmp = gfc_finish_block (&body);

      if (low != NULL_TREE)
	{
	  /* Compare vtable pointers.  */
	  cond = fold_build2_loc (input_location, EQ_EXPR,
				  TREE_TYPE (se.expr), se.expr, low);
	  tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node,
				 cond, tmp,
				 build_empty_stmt (input_location));
	}
      else if (high != NULL_TREE)
	{
	  /* Compare hash values for character cases.  */
	  gfc_init_se (&cse, NULL);
	  gfc_conv_expr_val (&cse, e);
	  gfc_add_block_to_block (&block, &cse.pre);

	  cond = fold_build2_loc (input_location, EQ_EXPR,
				  TREE_TYPE (se.expr), high, cse.expr);
	  tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node,
				 cond, tmp,
				 build_empty_stmt (input_location));
	}

      gfc_add_expr_to_block (&block, tmp);
    }

  if (!def)
    {
      def = true;
      goto repeat;
    }

  gfc_free_expr (e);

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
      for (cp = c->ext.block.case_list; cp; cp = cp->next)
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
	  tmp = build_case_label (low, high, label);
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
  tmp = fold_build3_loc (input_location, SWITCH_EXPR, NULL_TREE,
			 se.expr, tmp, NULL_TREE);
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
      for (cp = c->ext.block.case_list; cp; cp = cp->next)
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

      stmt = fold_build3_loc (input_location, COND_EXPR, void_type_node,
			      se.expr, true_tree, false_tree);
      gfc_add_expr_to_block (&block, stmt);
    }

  return gfc_finish_block (&block);
}


/* The jump table types are stored in static variables to avoid
   constructing them from scratch every single time.  */
static GTY(()) tree select_struct[2];

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
  tree init, end_label, tmp, type, case_num, label, fndecl;
  stmtblock_t block, body;
  gfc_case *cp, *d;
  gfc_code *c;
  gfc_se se, expr1se;
  int n, k;
  vec<constructor_elt, va_gc> *inits = NULL;

  tree pchartype = gfc_get_pchar_type (code->expr1->ts.kind);

  /* The jump table types are stored in static variables to avoid
     constructing them from scratch every single time.  */
  static tree ss_string1[2], ss_string1_len[2];
  static tree ss_string2[2], ss_string2_len[2];
  static tree ss_target[2];

  cp = code->block->ext.block.case_list;
  while (cp->left != NULL)
    cp = cp->left;

  /* Generate the body */
  gfc_start_block (&block);
  gfc_init_se (&expr1se, NULL);
  gfc_conv_expr_reference (&expr1se, code->expr1);

  gfc_add_block_to_block (&block, &expr1se.pre);

  end_label = gfc_build_label_decl (NULL_TREE);

  gfc_init_block (&body);

  /* Attempt to optimize length 1 selects.  */
  if (integer_onep (expr1se.string_length))
    {
      for (d = cp; d; d = d->right)
	{
	  int i;
	  if (d->low)
	    {
	      gcc_assert (d->low->expr_type == EXPR_CONSTANT
			  && d->low->ts.type == BT_CHARACTER);
	      if (d->low->value.character.length > 1)
		{
		  for (i = 1; i < d->low->value.character.length; i++)
		    if (d->low->value.character.string[i] != ' ')
		      break;
		  if (i != d->low->value.character.length)
		    {
		      if (optimize && d->high && i == 1)
			{
			  gcc_assert (d->high->expr_type == EXPR_CONSTANT
				      && d->high->ts.type == BT_CHARACTER);
			  if (d->high->value.character.length > 1
			      && (d->low->value.character.string[0]
				  == d->high->value.character.string[0])
			      && d->high->value.character.string[1] != ' '
			      && ((d->low->value.character.string[1] < ' ')
				  == (d->high->value.character.string[1]
				      < ' ')))
			    continue;
			}
		      break;
		    }
		}
	    }
	  if (d->high)
	    {
	      gcc_assert (d->high->expr_type == EXPR_CONSTANT
			  && d->high->ts.type == BT_CHARACTER);
	      if (d->high->value.character.length > 1)
		{
		  for (i = 1; i < d->high->value.character.length; i++)
		    if (d->high->value.character.string[i] != ' ')
		      break;
		  if (i != d->high->value.character.length)
		    break;
		}
	    }
	}
      if (d == NULL)
	{
	  tree ctype = gfc_get_char_type (code->expr1->ts.kind);

	  for (c = code->block; c; c = c->block)
	    {
	      for (cp = c->ext.block.case_list; cp; cp = cp->next)
		{
		  tree low, high;
		  tree label;
		  gfc_char_t r;

		  /* Assume it's the default case.  */
		  low = high = NULL_TREE;

		  if (cp->low)
		    {
		      /* CASE ('ab') or CASE ('ab':'az') will never match
			 any length 1 character.  */
		      if (cp->low->value.character.length > 1
			  && cp->low->value.character.string[1] != ' ')
			continue;

		      if (cp->low->value.character.length > 0)
			r = cp->low->value.character.string[0];
		      else
			r = ' ';
		      low = build_int_cst (ctype, r);

		      /* If there's only a lower bound, set the high bound
			 to the maximum value of the case expression.  */
		      if (!cp->high)
			high = TYPE_MAX_VALUE (ctype);
		    }

		  if (cp->high)
		    {
		      if (!cp->low
			  || (cp->low->value.character.string[0]
			      != cp->high->value.character.string[0]))
			{
			  if (cp->high->value.character.length > 0)
			    r = cp->high->value.character.string[0];
			  else
			    r = ' ';
			  high = build_int_cst (ctype, r);
			}

		      /* Unbounded case.  */
		      if (!cp->low)
			low = TYPE_MIN_VALUE (ctype);
		    }

		  /* Build a label.  */
		  label = gfc_build_label_decl (NULL_TREE);

		  /* Add this case label.
		     Add parameter 'label', make it match GCC backend.  */
		  tmp = build_case_label (low, high, label);
		  gfc_add_expr_to_block (&body, tmp);
		}

	      /* Add the statements for this case.  */
	      tmp = gfc_trans_code (c->next);
	      gfc_add_expr_to_block (&body, tmp);

	      /* Break to the end of the construct.  */
	      tmp = build1_v (GOTO_EXPR, end_label);
	      gfc_add_expr_to_block (&body, tmp);
	    }

	  tmp = gfc_string_to_single_character (expr1se.string_length,
						expr1se.expr,
						code->expr1->ts.kind);
	  case_num = gfc_create_var (ctype, "case_num");
	  gfc_add_modify (&block, case_num, tmp);

	  gfc_add_block_to_block (&block, &expr1se.post);

	  tmp = gfc_finish_block (&body);
	  tmp = fold_build3_loc (input_location, SWITCH_EXPR, NULL_TREE,
				 case_num, tmp, NULL_TREE);
	  gfc_add_expr_to_block (&block, tmp);

	  tmp = build1_v (LABEL_EXPR, end_label);
	  gfc_add_expr_to_block (&block, tmp);

	  return gfc_finish_block (&block);
	}
    }

  if (code->expr1->ts.kind == 1)
    k = 0;
  else if (code->expr1->ts.kind == 4)
    k = 1;
  else
    gcc_unreachable ();

  if (select_struct[k] == NULL)
    {
      tree *chain = NULL;
      select_struct[k] = make_node (RECORD_TYPE);

      if (code->expr1->ts.kind == 1)
	TYPE_NAME (select_struct[k]) = get_identifier ("_jump_struct_char1");
      else if (code->expr1->ts.kind == 4)
	TYPE_NAME (select_struct[k]) = get_identifier ("_jump_struct_char4");
      else
	gcc_unreachable ();

#undef ADD_FIELD
#define ADD_FIELD(NAME, TYPE)						    \
  ss_##NAME[k] = gfc_add_field_to_struct (select_struct[k],		    \
					  get_identifier (stringize(NAME)), \
					  TYPE,				    \
					  &chain)

      ADD_FIELD (string1, pchartype);
      ADD_FIELD (string1_len, gfc_charlen_type_node);

      ADD_FIELD (string2, pchartype);
      ADD_FIELD (string2_len, gfc_charlen_type_node);

      ADD_FIELD (target, integer_type_node);
#undef ADD_FIELD

      gfc_finish_type (select_struct[k]);
    }

  n = 0;
  for (d = cp; d; d = d->right)
    d->n = n++;

  for (c = code->block; c; c = c->block)
    {
      for (d = c->ext.block.case_list; d; d = d->next)
        {
	  label = gfc_build_label_decl (NULL_TREE);
	  tmp = build_case_label ((d->low == NULL && d->high == NULL)
				  ? NULL
				  : build_int_cst (integer_type_node, d->n),
				  NULL, label);
          gfc_add_expr_to_block (&body, tmp);
        }

      tmp = gfc_trans_code (c->next);
      gfc_add_expr_to_block (&body, tmp);

      tmp = build1_v (GOTO_EXPR, end_label);
      gfc_add_expr_to_block (&body, tmp);
    }

  /* Generate the structure describing the branches */
  for (d = cp; d; d = d->right)
    {
      vec<constructor_elt, va_gc> *node = NULL;

      gfc_init_se (&se, NULL);

      if (d->low == NULL)
        {
          CONSTRUCTOR_APPEND_ELT (node, ss_string1[k], null_pointer_node);
          CONSTRUCTOR_APPEND_ELT (node, ss_string1_len[k], integer_zero_node);
        }
      else
        {
          gfc_conv_expr_reference (&se, d->low);

          CONSTRUCTOR_APPEND_ELT (node, ss_string1[k], se.expr);
          CONSTRUCTOR_APPEND_ELT (node, ss_string1_len[k], se.string_length);
        }

      if (d->high == NULL)
        {
          CONSTRUCTOR_APPEND_ELT (node, ss_string2[k], null_pointer_node);
          CONSTRUCTOR_APPEND_ELT (node, ss_string2_len[k], integer_zero_node);
        }
      else
        {
          gfc_init_se (&se, NULL);
          gfc_conv_expr_reference (&se, d->high);

          CONSTRUCTOR_APPEND_ELT (node, ss_string2[k], se.expr);
          CONSTRUCTOR_APPEND_ELT (node, ss_string2_len[k], se.string_length);
        }

      CONSTRUCTOR_APPEND_ELT (node, ss_target[k],
                              build_int_cst (integer_type_node, d->n));

      tmp = build_constructor (select_struct[k], node);
      CONSTRUCTOR_APPEND_ELT (inits, NULL_TREE, tmp);
    }

  type = build_array_type (select_struct[k],
			   build_index_type (size_int (n-1)));

  init = build_constructor (type, inits);
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

  if (code->expr1->ts.kind == 1)
    fndecl = gfor_fndecl_select_string;
  else if (code->expr1->ts.kind == 4)
    fndecl = gfor_fndecl_select_string_char4;
  else
    gcc_unreachable ();

  tmp = build_call_expr_loc (input_location,
			 fndecl, 4, init,
			 build_int_cst (gfc_charlen_type_node, n),
			 expr1se.expr, expr1se.string_length);
  case_num = gfc_create_var (integer_type_node, "case_num");
  gfc_add_modify (&block, case_num, tmp);

  gfc_add_block_to_block (&block, &expr1se.post);

  tmp = gfc_finish_block (&body);
  tmp = fold_build3_loc (input_location, SWITCH_EXPR, NULL_TREE,
			 case_num, tmp, NULL_TREE);
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
  stmtblock_t block;
  tree body;
  tree exit_label;

  gcc_assert (code && code->expr1);
  gfc_init_block (&block);

  /* Build the exit label and hang it in.  */
  exit_label = gfc_build_label_decl (NULL_TREE);
  code->exit_label = exit_label;

  /* Empty SELECT constructs are legal.  */
  if (code->block == NULL)
    body = build_empty_stmt (input_location);

  /* Select the correct translation function.  */
  else
    switch (code->expr1->ts.type)
      {
      case BT_LOGICAL:
	body = gfc_trans_logical_select (code);
	break;

      case BT_INTEGER:
	body = gfc_trans_integer_select (code);
	break;

      case BT_CHARACTER:
	body = gfc_trans_character_select (code);
	break;

      default:
	gfc_internal_error ("gfc_trans_select(): Bad type for case expr.");
	/* Not reached */
      }

  /* Build everything together.  */
  gfc_add_expr_to_block (&block, body);
  gfc_add_expr_to_block (&block, build1_v (LABEL_EXPR, exit_label));

  return gfc_finish_block (&block);
}

tree
gfc_trans_select_type (gfc_code * code)
{
  stmtblock_t block;
  tree body;
  tree exit_label;

  gcc_assert (code && code->expr1);
  gfc_init_block (&block);

  /* Build the exit label and hang it in.  */
  exit_label = gfc_build_label_decl (NULL_TREE);
  code->exit_label = exit_label;

  /* Empty SELECT constructs are legal.  */
  if (code->block == NULL)
    body = build_empty_stmt (input_location);
  else
    body = gfc_trans_select_type_cases (code);

  /* Build everything together.  */
  gfc_add_expr_to_block (&block, body);

  if (TREE_USED (exit_label))
    gfc_add_expr_to_block (&block, build1_v (LABEL_EXPR, exit_label));

  return gfc_finish_block (&block);
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
      gfc_conv_subref_array_arg (&tse, e, 0, INTENT_IN, false);
      gfc_add_block_to_block (pre, &tse.pre);
      gfc_add_block_to_block (post, &tse.post);
      tse.expr = build_fold_indirect_ref_loc (input_location, tse.expr);

      if (c->expr1->ref->u.ar.type != AR_SECTION)
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

      tmp = gfc_trans_scalar_assign (&tse, &rse, e->ts,
				     e->expr_type == EXPR_VARIABLE, false);
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
  if (find_forall_index (c->expr1, lsym, 2))
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
  free (new_symtree->n.sym);
  free (new_symtree);
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
      cond = fold_build2_loc (input_location, LE_EXPR, boolean_type_node,
			      count, build_int_cst (TREE_TYPE (count), 0));
      if (forall_tmp->do_concurrent)
	cond = build2 (ANNOTATE_EXPR, TREE_TYPE (cond), cond,
		       build_int_cst (integer_type_node,
				      annot_expr_ivdep_kind));

      tmp = build1_v (GOTO_EXPR, exit_label);
      tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node,
			     cond, tmp, build_empty_stmt (input_location));
      gfc_add_expr_to_block (&block, tmp);

      /* The main loop body.  */
      gfc_add_expr_to_block (&block, body);

      /* Increment the loop variable.  */
      tmp = fold_build2_loc (input_location, PLUS_EXPR, TREE_TYPE (var), var,
			     step);
      gfc_add_modify (&block, var, tmp);

      /* Advance to the next mask element.  Only do this for the
	 innermost loop.  */
      if (n == 0 && mask_flag && forall_tmp->mask)
	{
	  tree maskindex = forall_tmp->maskindex;
	  tmp = fold_build2_loc (input_location, PLUS_EXPR, gfc_array_index_type,
				 maskindex, gfc_index_one_node);
	  gfc_add_modify (&block, maskindex, tmp);
	}

      /* Decrement the loop counter.  */
      tmp = fold_build2_loc (input_location, MINUS_EXPR, TREE_TYPE (var), count,
			     build_int_cst (TREE_TYPE (var), 1));
      gfc_add_modify (&block, count, tmp);

      body = gfc_finish_block (&block);

      /* Loop var initialization.  */
      gfc_init_block (&block);
      gfc_add_modify (&block, var, start);


      /* Initialize the loop counter.  */
      tmp = fold_build2_loc (input_location, MINUS_EXPR, TREE_TYPE (var), step,
			     start);
      tmp = fold_build2_loc (input_location, PLUS_EXPR, TREE_TYPE (var), end,
			     tmp);
      tmp = fold_build2_loc (input_location, TRUNC_DIV_EXPR, TREE_TYPE (var),
			     tmp, step);
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
    tmp = fold_build2_loc (input_location, MINUS_EXPR, gfc_array_index_type,
			   size, gfc_index_one_node);
  else
    tmp = NULL_TREE;

  type = build_range_type (gfc_array_index_type, gfc_index_zero_node, tmp);
  type = build_array_type (elem_type, type);
  if (gfc_can_put_var_on_stack (bytesize) && INTEGER_CST_P (size))
    {
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
			       tree count1,
			       gfc_ss *lss, gfc_ss *rss,
			       tree wheremask, bool invert)
{
  stmtblock_t block, body1;
  gfc_loopinfo loop;
  gfc_se lse;
  gfc_se rse;
  tree tmp;
  tree wheremaskexpr;

  (void) rss; /* TODO: unused.  */

  gfc_start_block (&block);

  gfc_init_se (&rse, NULL);
  gfc_init_se (&lse, NULL);

  if (lss == gfc_ss_terminator)
    {
      gfc_init_block (&body1);
      gfc_conv_expr (&lse, expr);
      rse.expr = gfc_build_array_ref (tmp1, count1, NULL);
    }
  else
    {
      /* Initialize the loop.  */
      gfc_init_loopinfo (&loop);

      /* We may need LSS to determine the shape of the expression.  */
      gfc_add_ss_to_loop (&loop, lss);

      gfc_conv_ss_startstride (&loop);
      gfc_conv_loop_setup (&loop, &expr->where);

      gfc_mark_ss_chain_used (lss, 1);
      /* Start the loop body.  */
      gfc_start_scalarized_body (&loop, &body1);

      /* Translate the expression.  */
      gfc_copy_loopinfo_to_se (&lse, &loop);
      lse.ss = lss;
      gfc_conv_expr (&lse, expr);

      /* Form the expression of the temporary.  */
      rse.expr = gfc_build_array_ref (tmp1, count1, NULL);
    }

  /* Use the scalar assignment.  */
  rse.string_length = lse.string_length;
  tmp = gfc_trans_scalar_assign (&lse, &rse, expr->ts,
				 expr->expr_type == EXPR_VARIABLE, false);

  /* Form the mask expression according to the mask tree list.  */
  if (wheremask)
    {
      wheremaskexpr = gfc_build_array_ref (wheremask, count3, NULL);
      if (invert)
	wheremaskexpr = fold_build1_loc (input_location, TRUTH_NOT_EXPR,
					 TREE_TYPE (wheremaskexpr),
					 wheremaskexpr);
      tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node,
			     wheremaskexpr, tmp,
			     build_empty_stmt (input_location));
    }

  gfc_add_expr_to_block (&body1, tmp);

  tmp = fold_build2_loc (input_location, PLUS_EXPR, TREE_TYPE (count1),
			 count1, gfc_index_one_node);
  gfc_add_modify (&body1, count1, tmp);

  if (lss == gfc_ss_terminator)
      gfc_add_block_to_block (&block, &body1);
  else
    {
      /* Increment count3.  */
      if (count3)
	{
	  tmp = fold_build2_loc (input_location, PLUS_EXPR,
				 gfc_array_index_type,
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
  tmp = gfc_trans_scalar_assign (&lse, &rse, expr2->ts,
				 expr2->expr_type == EXPR_VARIABLE, false);

  /* Form the mask expression according to the mask tree list.  */
  if (wheremask)
    {
      wheremaskexpr = gfc_build_array_ref (wheremask, count3, NULL);
      if (invert)
	wheremaskexpr = fold_build1_loc (input_location, TRUTH_NOT_EXPR,
					 TREE_TYPE (wheremaskexpr),
					 wheremaskexpr);
      tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node,
			     wheremaskexpr, tmp,
			     build_empty_stmt (input_location));
    }

  gfc_add_expr_to_block (&body1, tmp);

  if (lss == gfc_ss_terminator)
    {
      gfc_add_block_to_block (&block, &body1);

      /* Increment count1.  */
      tmp = fold_build2_loc (input_location, PLUS_EXPR, TREE_TYPE (count1),
			     count1, gfc_index_one_node);
      gfc_add_modify (&block, count1, tmp);
    }
  else
    {
      /* Increment count1.  */
      tmp = fold_build2_loc (input_location, PLUS_EXPR, gfc_array_index_type,
			     count1, gfc_index_one_node);
      gfc_add_modify (&body1, count1, tmp);

      /* Increment count3.  */
      if (count3)
	{
	  tmp = fold_build2_loc (input_location, PLUS_EXPR,
				 gfc_array_index_type,
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
	/* The rhs is scalar.  Add a ss for the expression.  */
	*rss = gfc_get_scalar_ss (gfc_ss_terminator, expr2);

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
      gfc_option.rtcheck &= ~GFC_RTCHECK_BOUNDS;
      gfc_conv_ss_startstride (&loop);
      gfc_option.rtcheck = save_flag;
      gfc_conv_loop_setup (&loop, &expr2->where);

      /* Figure out how many elements we need.  */
      for (i = 0; i < loop.dimen; i++)
        {
	  tmp = fold_build2_loc (input_location, MINUS_EXPR,
				 gfc_array_index_type,
				 gfc_index_one_node, loop.from[i]);
          tmp = fold_build2_loc (input_location, PLUS_EXPR,
				 gfc_array_index_type, tmp, loop.to[i]);
          size = fold_build2_loc (input_location, MULT_EXPR,
				  gfc_array_index_type, size, tmp);
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
	  inner_size = fold_build2_loc (input_location, MULT_EXPR,
					gfc_array_index_type,
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
    tmp = fold_build2_loc (input_location, PLUS_EXPR,
			   gfc_array_index_type, number, inner_size);
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
    bytesize = fold_build2_loc (input_location, MULT_EXPR,
				gfc_array_index_type, size, unit);
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
  /* The type of LHS. Used in function allocate_temp_for_forall_nest */
  if (expr1->ts.type == BT_CHARACTER)
    {
      type = NULL;
      if (expr1->ref && expr1->ref->type == REF_SUBSTRING)
	{
	  gfc_se ssse;
	  gfc_init_se (&ssse, NULL);
	  gfc_conv_expr (&ssse, expr1);
	  type = gfc_get_character_type_len (gfc_default_character_kind,
					     ssse.string_length);
	}
      else
	{
	  if (!expr1->ts.u.cl->backend_decl)
	    {
	      gfc_se tse;
	      gcc_assert (expr1->ts.u.cl->length);
	      gfc_init_se (&tse, NULL);
	      gfc_conv_expr (&tse, expr1->ts.u.cl->length);
	      expr1->ts.u.cl->backend_decl = tse.expr;
	    }
	  type = gfc_get_character_type_len (gfc_default_character_kind,
					     expr1->ts.u.cl->backend_decl);
	}
    }
  else
    type = gfc_typenode_for_spec (&expr1->ts);

  gfc_init_block (&inner_size_body);
  inner_size = compute_inner_temp_size (expr1, expr2, &inner_size_body,
					&lss, &rss);

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

  /* TODO: Second call to compute_inner_temp_size to initialize lss and
     rss;  there must be a better way.  */
  inner_size = compute_inner_temp_size (expr1, expr2, &inner_size_body,
					&lss, &rss);

  /* Generate codes to copy the temporary to lhs.  */
  tmp = generate_loop_for_temp_to_lhs (expr1, tmp1, count, count1,
				       lss, rss,
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
  gfc_array_info *info;
  gfc_loopinfo loop;
  tree desc;
  tree parm;
  tree parmtype;
  stmtblock_t body;
  tree count;
  tree tmp, tmp1, ptemp1;

  count = gfc_create_var (gfc_array_index_type, "count");
  gfc_add_modify (block, count, gfc_index_zero_node);

  inner_size = gfc_index_one_node;
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
      tmp = fold_build2_loc (input_location, PLUS_EXPR, gfc_array_index_type,
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
      tmp = fold_build2_loc (input_location, PLUS_EXPR, gfc_array_index_type,
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

      info = &rss->info->data.array;
      desc = info->descriptor;

      /* Make a new descriptor.  */
      parmtype = gfc_get_element_type (TREE_TYPE (desc));
      parmtype = gfc_get_array_type_bounds (parmtype, loop.dimen, 0,
                                            loop.from, loop.to, 1,
					    GFC_ARRAY_UNKNOWN, true);

      /* Allocate temporary for nested forall construct.  */
      tmp1 = allocate_temp_for_forall_nest (nested_forall_info, parmtype,
					    inner_size, NULL, block, &ptemp1);
      gfc_start_block (&body);
      gfc_init_se (&lse, NULL);
      lse.expr = gfc_build_array_ref (tmp1, count, NULL);
      lse.direct_byref = 1;
      gfc_conv_expr_descriptor (&lse, expr2);

      gfc_add_block_to_block (&body, &lse.pre);
      gfc_add_block_to_block (&body, &lse.post);

      /* Increment count.  */
      tmp = fold_build2_loc (input_location, PLUS_EXPR, gfc_array_index_type,
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
      gfc_init_se (&lse, NULL);
      gfc_conv_expr_descriptor (&lse, expr1);
      gfc_add_modify (&lse.pre, lse.expr, parm);
      gfc_start_block (&body);
      gfc_add_block_to_block (&body, &lse.pre);
      gfc_add_block_to_block (&body, &lse.post);

      /* Increment count.  */
      tmp = fold_build2_loc (input_location, PLUS_EXPR, gfc_array_index_type,
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
  tree cycle_label = NULL_TREE;
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
  var = XCNEWVEC (tree, nvar);
  start = XCNEWVEC (tree, nvar);
  end = XCNEWVEC (tree, nvar);
  step = XCNEWVEC (tree, nvar);
  varexpr = XCNEWVEC (gfc_expr *, nvar);
  saved_vars = XCNEWVEC (gfc_saved_var, nvar);

  /* Allocate the space for info.  */
  info = XCNEW (forall_info);

  gfc_start_block (&pre);
  gfc_init_block (&post);
  gfc_init_block (&block);

  n = 0;
  for (fa = code->ext.forall_iterator; fa; fa = fa->next)
    {
      gfc_symbol *sym = fa->var->symtree->n.sym;

      /* Allocate space for this_forall.  */
      this_forall = XCNEW (iter_info);

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
      tmp = fold_build2_loc (input_location, MINUS_EXPR, TREE_TYPE (start[n]),
			     step[n], start[n]);
      tmp = fold_build2_loc (input_location, PLUS_EXPR, TREE_TYPE (end[n]),
			     end[n], tmp);
      tmp = fold_build2_loc (input_location, FLOOR_DIV_EXPR, TREE_TYPE (tmp),
			     tmp, step[n]);
      tmp = convert (gfc_array_index_type, tmp);

      size = fold_build2_loc (input_location, MULT_EXPR, gfc_array_index_type,
			      size, tmp);
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
      tmp = fold_build2_loc (input_location, PLUS_EXPR, gfc_array_index_type,
			     maskindex, gfc_index_one_node);
      gfc_add_modify (&body, maskindex, tmp);

      /* Generate the loops.  */
      tmp = gfc_finish_block (&body);
      tmp = gfc_trans_nested_forall_loop (info, tmp, 0);
      gfc_add_expr_to_block (&block, tmp);
    }

  if (code->op == EXEC_DO_CONCURRENT)
    {
      gfc_init_block (&body);
      cycle_label = gfc_build_label_decl (NULL_TREE);
      code->cycle_label = cycle_label;
      tmp = gfc_trans_code (code->block->next);
      gfc_add_expr_to_block (&body, tmp);

      if (TREE_USED (cycle_label))
	{
	  tmp = build1_v (LABEL_EXPR, cycle_label);
	  gfc_add_expr_to_block (&body, tmp);
	}

      tmp = gfc_finish_block (&body);
      nested_forall_info->do_concurrent = true;
      tmp = gfc_trans_nested_forall_loop (nested_forall_info, tmp, 1);
      gfc_add_expr_to_block (&block, tmp);
      goto done;
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
	  if (need_temp || flag_test_forall_temp)
	    gfc_trans_assign_need_temp (c->expr1, c->expr2, NULL, false,
                                        nested_forall_info, &block);
          else
            {
              /* Use the normal assignment copying routines.  */
              assign = gfc_trans_assignment (c->expr1, c->expr2, false, true);

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
	  /* Avoid cases where a temporary would never be needed and where
	     the temp code is guaranteed to fail.  */
	  if (need_temp
	      || (flag_test_forall_temp
		  && c->expr2->expr_type != EXPR_CONSTANT
		  && c->expr2->expr_type != EXPR_NULL))
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

done:
  /* Restore the original index variables.  */
  for (fa = code->ext.forall_iterator, n = 0; fa; fa = fa->next, n++)
    gfc_restore_sym (fa->var->symtree->n.sym, &saved_vars[n]);

  /* Free the space for var, start, end, step, varexpr.  */
  free (var);
  free (start);
  free (end);
  free (step);
  free (varexpr);
  free (saved_vars);

  for (this_forall = info->this_loop; this_forall;)
    {
      iter_info *next = this_forall->next;
      free (this_forall);
      this_forall = next;
    }

  /* Free the space for this forall_info.  */
  free (info);

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


/* Translate the DO CONCURRENT construct.  */

tree gfc_trans_do_concurrent (gfc_code * code)
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
	tmp = fold_build1_loc (input_location, TRUTH_NOT_EXPR, mask_type, tmp);
      gfc_add_modify (&body1, mtmp, tmp);
    }

  if (cmask)
    {
      tmp1 = gfc_build_array_ref (cmask, count, NULL);
      tmp = cond;
      if (mask)
	tmp = fold_build2_loc (input_location, TRUTH_AND_EXPR, mask_type,
			       mtmp, tmp);
      gfc_add_modify (&body1, tmp1, tmp);
    }

  if (pmask)
    {
      tmp1 = gfc_build_array_ref (pmask, count, NULL);
      tmp = fold_build1_loc (input_location, TRUTH_NOT_EXPR, mask_type, cond);
      if (mask)
	tmp = fold_build2_loc (input_location, TRUTH_AND_EXPR, mask_type, mtmp,
			       tmp);
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
      tmp1 = fold_build2_loc (input_location, PLUS_EXPR, gfc_array_index_type,
			      count, gfc_index_one_node);
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

  /* A defined assignment.  */
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
	 && lss_section->info->type != GFC_SS_SECTION)
    lss_section = lss_section->next;

  gcc_assert (lss_section != gfc_ss_terminator);

  /* Initialize the scalarizer.  */
  gfc_init_loopinfo (&loop);

  /* Walk the rhs.  */
  rss = gfc_walk_expr (expr2);
  if (rss == gfc_ss_terminator)
    {
      /* The rhs is scalar.  Add a ss for the expression.  */
      rss = gfc_get_scalar_ss (gfc_ss_terminator, expr2);
      rss->info->where = 1;
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
    gfc_conv_tmp_array_ref (&lse);
  else
    gfc_conv_expr (&lse, expr1);

  /* Form the mask expression according to the mask.  */
  index = count1;
  maskexpr = gfc_build_array_ref (mask, index, NULL);
  if (invert)
    maskexpr = fold_build1_loc (input_location, TRUTH_NOT_EXPR,
				TREE_TYPE (maskexpr), maskexpr);

  /* Use the scalar assignment as is.  */
  tmp = gfc_trans_scalar_assign (&lse, &rse, expr1->ts,
				 false, loop.temp_ss == NULL);

  tmp = build3_v (COND_EXPR, maskexpr, tmp, build_empty_stmt (input_location));

  gfc_add_expr_to_block (&body, tmp);

  if (lss == gfc_ss_terminator)
    {
      /* Increment count1.  */
      tmp = fold_build2_loc (input_location, PLUS_EXPR, gfc_array_index_type,
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
          tmp = fold_build2_loc (input_location, PLUS_EXPR,
				 gfc_array_index_type, count1, gfc_index_one_node);
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
          gfc_conv_expr (&lse, expr1);

          gcc_assert (lse.ss == gfc_ss_terminator
		      && rse.ss == gfc_ss_terminator);

          /* Form the mask expression according to the mask tree list.  */
          index = count2;
          maskexpr = gfc_build_array_ref (mask, index, NULL);
	  if (invert)
	    maskexpr = fold_build1_loc (input_location, TRUTH_NOT_EXPR,
					TREE_TYPE (maskexpr), maskexpr);

          /* Use the scalar assignment as is.  */
          tmp = gfc_trans_scalar_assign (&lse, &rse, expr1->ts, false, true);
          tmp = build3_v (COND_EXPR, maskexpr, tmp,
			  build_empty_stmt (input_location));
          gfc_add_expr_to_block (&body, tmp);

          /* Increment count2.  */
          tmp = fold_build2_loc (input_location, PLUS_EXPR,
				 gfc_array_index_type, count2,
				 gfc_index_one_node);
          gfc_add_modify (&body, count2, tmp);
        }
      else
        {
          /* Increment count1.  */
          tmp = fold_build2_loc (input_location, PLUS_EXPR,
				 gfc_array_index_type, count1,
				 gfc_index_one_node);
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

      gfc_free_ss_chain (lss);
      gfc_free_ss_chain (rss);

      /* Calculate the total size of temporary needed.  */
      size = compute_overall_iter_number (nested_forall_info, inner_size,
					  &inner_size_body, block);

      /* Check whether the size is negative.  */
      cond = fold_build2_loc (input_location, LE_EXPR, boolean_type_node, size,
			      gfc_index_zero_node);
      size = fold_build3_loc (input_location, COND_EXPR, gfc_array_index_type,
			      cond, gfc_index_zero_node, size);
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
		  if ((need_temp || flag_test_forall_temp)
		    && cnext->op != EXEC_ASSIGN_CALL)
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
  bool maybe_workshare = false;

  /* Allow the scalarizer to workshare simple where loops.  */
  if ((ompws_flags & (OMPWS_WORKSHARE_FLAG | OMPWS_SCALARIZER_BODY))
      == OMPWS_WORKSHARE_FLAG)
    {
      maybe_workshare = true;
      ompws_flags |= OMPWS_SCALARIZER_WS | OMPWS_SCALARIZER_BODY;
    }

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
      tsss = gfc_get_scalar_ss (gfc_ss_terminator, tsrc);
      tsss->info->where = 1;
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
	  esss = gfc_get_scalar_ss (gfc_ss_terminator, esrc);
	  esss->info->where = 1;
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
    gfc_conv_tmp_array_ref (&tdse);
  else
    gfc_conv_expr (&tdse, tdst);

  if (eblock)
    {
      gfc_conv_expr (&esse, esrc);
      if (edss != gfc_ss_terminator && loop.temp_ss != NULL)
	gfc_conv_tmp_array_ref (&edse);
      else
	gfc_conv_expr (&edse, edst);
    }

  tstmt = gfc_trans_scalar_assign (&tdse, &tsse, tdst->ts, false, true);
  estmt = eblock ? gfc_trans_scalar_assign (&edse, &esse, edst->ts,
					    false, true)
		 : build_empty_stmt (input_location);
  tmp = build3_v (COND_EXPR, cexpr, tstmt, estmt);
  gfc_add_expr_to_block (&body, tmp);
  gfc_add_block_to_block (&body, &cse.post);

  if (maybe_workshare)
    ompws_flags &= ~OMPWS_SCALARIZER_BODY;
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
	  if (!gfc_check_dependency (cblock->next->expr1,
				    cblock->expr1, 0)
	      && !gfc_check_dependency (eblock->next->expr1,
				       cblock->expr1, 0)
	      && !gfc_check_dependency (cblock->next->expr1,
				       eblock->next->expr2, 1)
	      && !gfc_check_dependency (eblock->next->expr1,
				       cblock->next->expr2, 1)
	      && !gfc_check_dependency (cblock->next->expr1,
				       cblock->next->expr2, 1)
	      && !gfc_check_dependency (eblock->next->expr1,
				       eblock->next->expr2, 1)
	      && !gfc_check_dependency (cblock->next->expr1,
				       eblock->next->expr1, 0)
	      && !gfc_check_dependency (eblock->next->expr1,
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

  cycle_label = code->ext.which_construct->cycle_label;
  gcc_assert (cycle_label);

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

  exit_label = code->ext.which_construct->exit_label;
  gcc_assert (exit_label);

  TREE_USED (exit_label) = 1;
  return build1_v (GOTO_EXPR, exit_label);
}


/* Get the initializer expression for the code and expr of an allocate.
   When no initializer is needed return NULL.  */

static gfc_expr *
allocate_get_initializer (gfc_code * code, gfc_expr * expr)
{
  if (!gfc_bt_struct (expr->ts.type) && expr->ts.type != BT_CLASS)
    return NULL;

  /* An explicit type was given in allocate ( T:: object).  */
  if (code->ext.alloc.ts.type == BT_DERIVED
      && (code->ext.alloc.ts.u.derived->attr.alloc_comp
	  || gfc_has_default_initializer (code->ext.alloc.ts.u.derived)))
    return gfc_default_initializer (&code->ext.alloc.ts);

  if (gfc_bt_struct (expr->ts.type)
      && (expr->ts.u.derived->attr.alloc_comp
	  || gfc_has_default_initializer (expr->ts.u.derived)))
    return gfc_default_initializer (&expr->ts);

  if (expr->ts.type == BT_CLASS
      && (CLASS_DATA (expr)->ts.u.derived->attr.alloc_comp
	  || gfc_has_default_initializer (CLASS_DATA (expr)->ts.u.derived)))
    return gfc_default_initializer (&CLASS_DATA (expr)->ts);

  return NULL;
}

/* Translate the ALLOCATE statement.  */

tree
gfc_trans_allocate (gfc_code * code)
{
  gfc_alloc *al;
  gfc_expr *expr, *e3rhs = NULL, *init_expr;
  gfc_se se, se_sz;
  tree tmp;
  tree parm;
  tree stat;
  tree errmsg;
  tree errlen;
  tree label_errmsg;
  tree label_finish;
  tree memsz;
  tree al_vptr, al_len;
  /* If an expr3 is present, then store the tree for accessing its
     _vptr, and _len components in the variables, respectively.  The
     element size, i.e. _vptr%size, is stored in expr3_esize.  Any of
     the trees may be the NULL_TREE indicating that this is not
     available for expr3's type.  */
  tree expr3, expr3_vptr, expr3_len, expr3_esize;
  /* Classify what expr3 stores.  */
  enum { E3_UNSET = 0, E3_SOURCE, E3_MOLD, E3_DESC } e3_is;
  stmtblock_t block;
  stmtblock_t post;
  tree nelems;
  bool upoly_expr, tmp_expr3_len_flag = false, al_len_needs_set, is_coarray;
  bool needs_caf_sync, caf_refs_comp;
  gfc_symtree *newsym = NULL;
  symbol_attribute caf_attr;

  if (!code->ext.alloc.list)
    return NULL_TREE;

  stat = tmp = memsz = al_vptr = al_len = NULL_TREE;
  expr3 = expr3_vptr = expr3_len = expr3_esize = NULL_TREE;
  label_errmsg = label_finish = errmsg = errlen = NULL_TREE;
  e3_is = E3_UNSET;
  is_coarray = needs_caf_sync = false;

  gfc_init_block (&block);
  gfc_init_block (&post);

  /* STAT= (and maybe ERRMSG=) is present.  */
  if (code->expr1)
    {
      /* STAT=.  */
      tree gfc_int4_type_node = gfc_get_int_type (4);
      stat = gfc_create_var (gfc_int4_type_node, "stat");

      /* ERRMSG= only makes sense with STAT=.  */
      if (code->expr2)
	{
	  gfc_init_se (&se, NULL);
	  se.want_pointer = 1;
	  gfc_conv_expr_lhs (&se, code->expr2);
	  errmsg = se.expr;
	  errlen = se.string_length;
	}
      else
	{
	  errmsg = null_pointer_node;
	  errlen = build_int_cst (gfc_charlen_type_node, 0);
	}

      /* GOTO destinations.  */
      label_errmsg = gfc_build_label_decl (NULL_TREE);
      label_finish = gfc_build_label_decl (NULL_TREE);
      TREE_USED (label_finish) = 0;
    }

  /* When an expr3 is present evaluate it only once.  The standards prevent a
     dependency of expr3 on the objects in the allocate list.  An expr3 can
     be pre-evaluated in all cases.  One just has to make sure, to use the
     correct way, i.e., to get the descriptor or to get a reference
     expression.  */
  if (code->expr3)
    {
      bool vtab_needed = false, temp_var_needed = false,
	  temp_obj_created = false;

      is_coarray = gfc_is_coarray (code->expr3);

      /* Figure whether we need the vtab from expr3.  */
      for (al = code->ext.alloc.list; !vtab_needed && al != NULL;
	   al = al->next)
	vtab_needed = (al->expr->ts.type == BT_CLASS);

      gfc_init_se (&se, NULL);
      /* When expr3 is a variable, i.e., a very simple expression,
	     then convert it once here.  */
      if (code->expr3->expr_type == EXPR_VARIABLE
	  || code->expr3->expr_type == EXPR_ARRAY
	  || code->expr3->expr_type == EXPR_CONSTANT)
	{
	  if (!code->expr3->mold
	      || code->expr3->ts.type == BT_CHARACTER
	      || vtab_needed
	      || code->ext.alloc.arr_spec_from_expr3)
	    {
	      /* Convert expr3 to a tree.  For all "simple" expression just
		 get the descriptor or the reference, respectively, depending
		 on the rank of the expr.  */
	      if (code->ext.alloc.arr_spec_from_expr3 || code->expr3->rank != 0)
		gfc_conv_expr_descriptor (&se, code->expr3);
	      else
		{
		  gfc_conv_expr_reference (&se, code->expr3);

		  /* gfc_conv_expr_reference wraps POINTER_PLUS_EXPR in a
		     NOP_EXPR, which prevents gfortran from getting the vptr
		     from the source=-expression.  Remove the NOP_EXPR and go
		     with the POINTER_PLUS_EXPR in this case.  */
		  if (code->expr3->ts.type == BT_CLASS
		      && TREE_CODE (se.expr) == NOP_EXPR
		      && (TREE_CODE (TREE_OPERAND (se.expr, 0))
							    == POINTER_PLUS_EXPR
			  || is_coarray))
		    se.expr = TREE_OPERAND (se.expr, 0);
		}
	      /* Create a temp variable only for component refs to prevent
		 having to go through the full deref-chain each time and to
		 simplfy computation of array properties.  */
	      temp_var_needed = TREE_CODE (se.expr) == COMPONENT_REF;
	    }
	}
      else
	{
	  /* In all other cases evaluate the expr3.  */
	  symbol_attribute attr;
	  /* Get the descriptor for all arrays, that are not allocatable or
	     pointer, because the latter are descriptors already.
	     The exception are function calls returning a class object:
	     The descriptor is stored in their results _data component, which
	     is easier to access, when first a temporary variable for the
	     result is created and the descriptor retrieved from there.  */
	  attr = gfc_expr_attr (code->expr3);
	  if (code->expr3->rank != 0
	      && ((!attr.allocatable && !attr.pointer)
		  || (code->expr3->expr_type == EXPR_FUNCTION
		      && (code->expr3->ts.type != BT_CLASS
			  || (code->expr3->value.function.isym
			      && code->expr3->value.function.isym
							 ->transformational)))))
	    gfc_conv_expr_descriptor (&se, code->expr3);
	  else
	    gfc_conv_expr_reference (&se, code->expr3);
	  if (code->expr3->ts.type == BT_CLASS)
	    gfc_conv_class_to_class (&se, code->expr3,
				     code->expr3->ts,
				     false, true,
				     false, false);
	  temp_obj_created = temp_var_needed = !VAR_P (se.expr);
	}
      gfc_add_block_to_block (&block, &se.pre);
      gfc_add_block_to_block (&post, &se.post);

      /* Special case when string in expr3 is zero.  */
      if (code->expr3->ts.type == BT_CHARACTER
	  && integer_zerop (se.string_length))
	{
	  gfc_init_se (&se, NULL);
	  temp_var_needed = false;
	  expr3_len = integer_zero_node;
	  e3_is = E3_MOLD;
	}
      /* Prevent aliasing, i.e., se.expr may be already a
	     variable declaration.  */
      else if (se.expr != NULL_TREE && temp_var_needed)
	{
	  tree var, desc;
	  tmp = GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (se.expr)) || is_coarray ?
		se.expr
	      : build_fold_indirect_ref_loc (input_location, se.expr);

	  /* Get the array descriptor and prepare it to be assigned to the
	     temporary variable var.  For classes the array descriptor is
	     in the _data component and the object goes into the
	     GFC_DECL_SAVED_DESCRIPTOR.  */
	  if (code->expr3->ts.type == BT_CLASS
	      && code->expr3->rank != 0)
	    {
	      /* When an array_ref was in expr3, then the descriptor is the
		 first operand.  */
	      if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (tmp)) || is_coarray)
		{
		  desc = TREE_OPERAND (tmp, 0);
		}
	      else
		{
		  desc = tmp;
		  tmp = gfc_class_data_get (tmp);
		}
	      if (code->ext.alloc.arr_spec_from_expr3)
		e3_is = E3_DESC;
	    }
	  else
	    desc = !is_coarray ? se.expr
			       : TREE_OPERAND (TREE_OPERAND (se.expr, 0), 0);
	  /* We need a regular (non-UID) symbol here, therefore give a
	     prefix.  */
	  var = gfc_create_var (TREE_TYPE (tmp), "source");
	  if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (tmp)) || is_coarray)
	    {
	      gfc_allocate_lang_decl (var);
	      GFC_DECL_SAVED_DESCRIPTOR (var) = desc;
	    }
	  gfc_add_modify_loc (input_location, &block, var, tmp);

	  expr3 = var;
	  if (se.string_length)
	    /* Evaluate it assuming that it also is complicated like expr3.  */
	    expr3_len = gfc_evaluate_now (se.string_length, &block);
	}
      else
	{
	  expr3 = se.expr;
	  expr3_len = se.string_length;
	}

      /* Deallocate any allocatable components in expressions that use a
	 temporary object, i.e. are not a simple alias of to an EXPR_VARIABLE.
	 E.g. temporaries of a function call need freeing of their components
	 here.  */
      if ((code->expr3->ts.type == BT_DERIVED
	   || code->expr3->ts.type == BT_CLASS)
	  && (code->expr3->expr_type != EXPR_VARIABLE || temp_obj_created)
	  && code->expr3->ts.u.derived->attr.alloc_comp)
	{
	  tmp = gfc_deallocate_alloc_comp (code->expr3->ts.u.derived,
					   expr3, code->expr3->rank);
	  gfc_prepend_expr_to_block (&post, tmp);
	}

      /* Store what the expr3 is to be used for.  */
      if (e3_is == E3_UNSET)
	e3_is = expr3 != NULL_TREE ?
	      (code->ext.alloc.arr_spec_from_expr3 ?
		 E3_DESC
	       : (code->expr3->mold ? E3_MOLD : E3_SOURCE))
	    : E3_UNSET;

      /* Figure how to get the _vtab entry.  This also obtains the tree
	 expression for accessing the _len component, because only
	 unlimited polymorphic objects, which are a subcategory of class
	 types, have a _len component.  */
      if (code->expr3->ts.type == BT_CLASS)
	{
	  gfc_expr *rhs;
	  tmp = expr3 != NULL_TREE && POINTER_TYPE_P (TREE_TYPE (expr3)) ?
		build_fold_indirect_ref (expr3): expr3;
	  /* Polymorphic SOURCE: VPTR must be determined at run time.
	     expr3 may be a temporary array declaration, therefore check for
	     GFC_CLASS_TYPE_P before trying to get the _vptr component.  */
	  if (tmp != NULL_TREE
	      && (e3_is == E3_DESC
		  || (GFC_CLASS_TYPE_P (TREE_TYPE (tmp))
		      && (VAR_P (tmp) || !code->expr3->ref))
		  || (VAR_P (tmp) && DECL_LANG_SPECIFIC (tmp))))
	    tmp = gfc_class_vptr_get (expr3);
	  else
	    {
	      rhs = gfc_find_and_cut_at_last_class_ref (code->expr3);
	      gfc_add_vptr_component (rhs);
	      gfc_init_se (&se, NULL);
	      se.want_pointer = 1;
	      gfc_conv_expr (&se, rhs);
	      tmp = se.expr;
	      gfc_free_expr (rhs);
	    }
	  /* Set the element size.  */
	  expr3_esize = gfc_vptr_size_get (tmp);
	  if (vtab_needed)
	    expr3_vptr = tmp;
	  /* Initialize the ref to the _len component.  */
	  if (expr3_len == NULL_TREE && UNLIMITED_POLY (code->expr3))
	    {
	      /* Same like for retrieving the _vptr.  */
	      if (expr3 != NULL_TREE && !code->expr3->ref)
		expr3_len = gfc_class_len_get (expr3);
	      else
		{
		  rhs = gfc_find_and_cut_at_last_class_ref (code->expr3);
		  gfc_add_len_component (rhs);
		  gfc_init_se (&se, NULL);
		  gfc_conv_expr (&se, rhs);
		  expr3_len = se.expr;
		  gfc_free_expr (rhs);
		}
	    }
	}
      else
	{
	  /* When the object to allocate is polymorphic type, then it
	     needs its vtab set correctly, so deduce the required _vtab
	     and _len from the source expression.  */
	  if (vtab_needed)
	    {
	      /* VPTR is fixed at compile time.  */
	      gfc_symbol *vtab;

	      vtab = gfc_find_vtab (&code->expr3->ts);
	      gcc_assert (vtab);
	      expr3_vptr = gfc_get_symbol_decl (vtab);
	      expr3_vptr = gfc_build_addr_expr (NULL_TREE,
						expr3_vptr);
	    }
	  /* _len component needs to be set, when ts is a character
	     array.  */
	  if (expr3_len == NULL_TREE
	      && code->expr3->ts.type == BT_CHARACTER)
	    {
	      if (code->expr3->ts.u.cl
		  && code->expr3->ts.u.cl->length)
		{
		  gfc_init_se (&se, NULL);
		  gfc_conv_expr (&se, code->expr3->ts.u.cl->length);
		  gfc_add_block_to_block (&block, &se.pre);
		  expr3_len = gfc_evaluate_now (se.expr, &block);
		}
	      gcc_assert (expr3_len);
	    }
	  /* For character arrays only the kind's size is needed, because
	     the array mem_size is _len * (elem_size = kind_size).
	     For all other get the element size in the normal way.  */
	  if (code->expr3->ts.type == BT_CHARACTER)
	    expr3_esize = TYPE_SIZE_UNIT (
		  gfc_get_char_type (code->expr3->ts.kind));
	  else
	    expr3_esize = TYPE_SIZE_UNIT (
		  gfc_typenode_for_spec (&code->expr3->ts));
	}
      gcc_assert (expr3_esize);
      expr3_esize = fold_convert (sizetype, expr3_esize);
      if (e3_is == E3_MOLD)
	/* The expr3 is no longer valid after this point.  */
	expr3 = NULL_TREE;
    }
  else if (code->ext.alloc.ts.type != BT_UNKNOWN)
    {
      /* Compute the explicit typespec given only once for all objects
	 to allocate.  */
      if (code->ext.alloc.ts.type != BT_CHARACTER)
	expr3_esize = TYPE_SIZE_UNIT (
	      gfc_typenode_for_spec (&code->ext.alloc.ts));
      else
	{
	  gfc_expr *sz;
	  gcc_assert (code->ext.alloc.ts.u.cl->length != NULL);
	  sz = gfc_copy_expr (code->ext.alloc.ts.u.cl->length);
	  gfc_init_se (&se_sz, NULL);
	  gfc_conv_expr (&se_sz, sz);
	  gfc_free_expr (sz);
	  tmp = gfc_get_char_type (code->ext.alloc.ts.kind);
	  tmp = TYPE_SIZE_UNIT (tmp);
	  tmp = fold_convert (TREE_TYPE (se_sz.expr), tmp);
	  gfc_add_block_to_block (&block, &se_sz.pre);
	  expr3_esize = fold_build2_loc (input_location, MULT_EXPR,
					 TREE_TYPE (se_sz.expr),
					 tmp, se_sz.expr);
	  expr3_esize = gfc_evaluate_now (expr3_esize, &block);
	}
    }

  /* The routine gfc_trans_assignment () already implements all
     techniques needed.  Unfortunately we may have a temporary
     variable for the source= expression here.  When that is the
     case convert this variable into a temporary gfc_expr of type
     EXPR_VARIABLE and used it as rhs for the assignment.  The
     advantage is, that we get scalarizer support for free,
     don't have to take care about scalar to array treatment and
     will benefit of every enhancements gfc_trans_assignment ()
     gets.
     No need to check whether e3_is is E3_UNSET, because that is
     done by expr3 != NULL_TREE.
     Exclude variables since the following block does not handle
     array sections.  In any case, there is no harm in sending
     variables to gfc_trans_assignment because there is no
     evaluation of variables.  */
  if (code->expr3)
    {
      if (code->expr3->expr_type != EXPR_VARIABLE
	  && e3_is != E3_MOLD && expr3 != NULL_TREE
	  && DECL_P (expr3) && DECL_ARTIFICIAL (expr3))
	{
	  /* Build a temporary symtree and symbol.  Do not add it to the current
	     namespace to prevent accidently modifying a colliding
	     symbol's as.  */
	  newsym = XCNEW (gfc_symtree);
	  /* The name of the symtree should be unique, because gfc_create_var ()
	     took care about generating the identifier.  */
	  newsym->name
	    = gfc_get_string ("%s", IDENTIFIER_POINTER (DECL_NAME (expr3)));
	  newsym->n.sym = gfc_new_symbol (newsym->name, NULL);
	  /* The backend_decl is known.  It is expr3, which is inserted
	     here.  */
	  newsym->n.sym->backend_decl = expr3;
	  e3rhs = gfc_get_expr ();
	  e3rhs->rank = code->expr3->rank;
	  e3rhs->symtree = newsym;
	  /* Mark the symbol referenced or gfc_trans_assignment will bug.  */
	  newsym->n.sym->attr.referenced = 1;
	  e3rhs->expr_type = EXPR_VARIABLE;
	  e3rhs->where = code->expr3->where;
	  /* Set the symbols type, upto it was BT_UNKNOWN.  */
	  if (IS_CLASS_ARRAY (code->expr3)
	      && code->expr3->expr_type == EXPR_FUNCTION
	      && code->expr3->value.function.isym
	      && code->expr3->value.function.isym->transformational)
	    {
	      e3rhs->ts = CLASS_DATA (code->expr3)->ts;
	    }
	  else if (code->expr3->ts.type == BT_CLASS
		   && !GFC_CLASS_TYPE_P (TREE_TYPE (expr3)))
	    e3rhs->ts = CLASS_DATA (code->expr3)->ts;
	  else
	    e3rhs->ts = code->expr3->ts;
	  newsym->n.sym->ts = e3rhs->ts;
	  /* Check whether the expr3 is array valued.  */
	  if (e3rhs->rank)
	    {
	      gfc_array_spec *arr;
	      arr = gfc_get_array_spec ();
	      arr->rank = e3rhs->rank;
	      arr->type = AS_DEFERRED;
	      /* Set the dimension and pointer attribute for arrays
	     to be on the safe side.  */
	      newsym->n.sym->attr.dimension = 1;
	      newsym->n.sym->attr.pointer = 1;
	      newsym->n.sym->as = arr;
	      if (IS_CLASS_ARRAY (code->expr3)
		  && code->expr3->expr_type == EXPR_FUNCTION
		  && code->expr3->value.function.isym
		  && code->expr3->value.function.isym->transformational)
		{
		  gfc_array_spec *tarr;
		  tarr = gfc_get_array_spec ();
		  *tarr = *arr;
		  e3rhs->ts.u.derived->as = tarr;
		}
	      gfc_add_full_array_ref (e3rhs, arr);
	    }
	  else if (POINTER_TYPE_P (TREE_TYPE (expr3)))
	    newsym->n.sym->attr.pointer = 1;
	  /* The string length is known, too.  Set it for char arrays.  */
	  if (e3rhs->ts.type == BT_CHARACTER)
	    newsym->n.sym->ts.u.cl->backend_decl = expr3_len;
	  gfc_commit_symbol (newsym->n.sym);
	}
      else
	e3rhs = gfc_copy_expr (code->expr3);
    }

  /* Loop over all objects to allocate.  */
  for (al = code->ext.alloc.list; al != NULL; al = al->next)
    {
      expr = gfc_copy_expr (al->expr);
      /* UNLIMITED_POLY () needs the _data component to be set, when
	 expr is a unlimited polymorphic object.  But the _data component
	 has not been set yet, so check the derived type's attr for the
	 unlimited polymorphic flag to be safe.  */
      upoly_expr = UNLIMITED_POLY (expr)
		    || (expr->ts.type == BT_DERIVED
			&& expr->ts.u.derived->attr.unlimited_polymorphic);
      gfc_init_se (&se, NULL);

      /* For class types prepare the expressions to ref the _vptr
	 and the _len component.  The latter for unlimited polymorphic
	 types only.  */
      if (expr->ts.type == BT_CLASS)
	{
	  gfc_expr *expr_ref_vptr, *expr_ref_len;
	  gfc_add_data_component (expr);
	  /* Prep the vptr handle.  */
	  expr_ref_vptr = gfc_copy_expr (al->expr);
	  gfc_add_vptr_component (expr_ref_vptr);
	  se.want_pointer = 1;
	  gfc_conv_expr (&se, expr_ref_vptr);
	  al_vptr = se.expr;
	  se.want_pointer = 0;
	  gfc_free_expr (expr_ref_vptr);
	  /* Allocated unlimited polymorphic objects always have a _len
	     component.  */
	  if (upoly_expr)
	    {
	      expr_ref_len = gfc_copy_expr (al->expr);
	      gfc_add_len_component (expr_ref_len);
	      gfc_conv_expr (&se, expr_ref_len);
	      al_len = se.expr;
	      gfc_free_expr (expr_ref_len);
	    }
	  else
	    /* In a loop ensure that all loop variable dependent variables
	       are initialized at the same spot in all execution paths.  */
	    al_len = NULL_TREE;
	}
      else
	al_vptr = al_len = NULL_TREE;

      se.want_pointer = 1;
      se.descriptor_only = 1;

      gfc_conv_expr (&se, expr);
      if (expr->ts.type == BT_CHARACTER && expr->ts.deferred)
	/* se.string_length now stores the .string_length variable of expr
	   needed to allocate character(len=:) arrays.  */
	al_len = se.string_length;

      al_len_needs_set = al_len != NULL_TREE;
      /* When allocating an array one can not use much of the
	 pre-evaluated expr3 expressions, because for most of them the
	 scalarizer is needed which is not available in the pre-evaluation
	 step.  Therefore gfc_array_allocate () is responsible (and able)
	 to handle the complete array allocation.  Only the element size
	 needs to be provided, which is done most of the time by the
	 pre-evaluation step.  */
      nelems = NULL_TREE;
      if (expr3_len && (code->expr3->ts.type == BT_CHARACTER
			|| code->expr3->ts.type == BT_CLASS))
	{
	  /* When al is an array, then the element size for each element
	     in the array is needed, which is the product of the len and
	     esize for char arrays.  For unlimited polymorphics len can be
	     zero, therefore take the maximum of len and one.  */
	  tmp = fold_build2_loc (input_location, MAX_EXPR,
				 TREE_TYPE (expr3_len),
				 expr3_len, fold_convert (TREE_TYPE (expr3_len),
							  integer_one_node));
	  tmp = fold_build2_loc (input_location, MULT_EXPR,
				 TREE_TYPE (expr3_esize), expr3_esize,
				 fold_convert (TREE_TYPE (expr3_esize), tmp));
	}
      else
	tmp = expr3_esize;
      if (!gfc_array_allocate (&se, expr, stat, errmsg, errlen,
			       label_finish, tmp, &nelems,
			       e3rhs ? e3rhs : code->expr3,
			       e3_is == E3_DESC ? expr3 : NULL_TREE,
			       code->expr3 != NULL && e3_is == E3_DESC
			       && code->expr3->expr_type == EXPR_ARRAY))
	{
	  /* A scalar or derived type.  First compute the size to
	     allocate.

	     expr3_len is set when expr3 is an unlimited polymorphic
	     object or a deferred length string.  */
	  if (expr3_len != NULL_TREE)
	    {
	      tmp = fold_convert (TREE_TYPE (expr3_esize), expr3_len);
	      tmp = fold_build2_loc (input_location, MULT_EXPR,
				     TREE_TYPE (expr3_esize),
				      expr3_esize, tmp);
	      if (code->expr3->ts.type != BT_CLASS)
		/* expr3 is a deferred length string, i.e., we are
		   done.  */
		memsz = tmp;
	      else
		{
		  /* For unlimited polymorphic enties build
			  (len > 0) ? element_size * len : element_size
		     to compute the number of bytes to allocate.
		     This allows the allocation of unlimited polymorphic
		     objects from an expr3 that is also unlimited
		     polymorphic and stores a _len dependent object,
		     e.g., a string.  */
		  memsz = fold_build2_loc (input_location, GT_EXPR,
					   boolean_type_node, expr3_len,
					   integer_zero_node);
		  memsz = fold_build3_loc (input_location, COND_EXPR,
					 TREE_TYPE (expr3_esize),
					 memsz, tmp, expr3_esize);
		}
	    }
	  else if (expr3_esize != NULL_TREE)
	    /* Any other object in expr3 just needs element size in
	       bytes.  */
	    memsz = expr3_esize;
	  else if ((expr->ts.type == BT_CHARACTER && expr->ts.deferred)
		   || (upoly_expr
		       && code->ext.alloc.ts.type == BT_CHARACTER))
	    {
	      /* Allocating deferred length char arrays need the length
		 to allocate in the alloc_type_spec.  But also unlimited
		 polymorphic objects may be allocated as char arrays.
		 Both are handled here.  */
	      gfc_init_se (&se_sz, NULL);
	      gfc_conv_expr (&se_sz, code->ext.alloc.ts.u.cl->length);
	      gfc_add_block_to_block (&se.pre, &se_sz.pre);
	      se_sz.expr = gfc_evaluate_now (se_sz.expr, &se.pre);
	      gfc_add_block_to_block (&se.pre, &se_sz.post);
	      expr3_len = se_sz.expr;
	      tmp_expr3_len_flag = true;
	      tmp = TYPE_SIZE_UNIT (
		    gfc_get_char_type (code->ext.alloc.ts.kind));
	      memsz = fold_build2_loc (input_location, MULT_EXPR,
				       TREE_TYPE (tmp),
				       fold_convert (TREE_TYPE (tmp),
						     expr3_len),
				       tmp);
	    }
	  else if (expr->ts.type == BT_CHARACTER)
	    {
	      /* Compute the number of bytes needed to allocate a fixed
		 length char array.  */
	      gcc_assert (se.string_length != NULL_TREE);
	      tmp = TYPE_SIZE_UNIT (gfc_get_char_type (expr->ts.kind));
	      memsz = fold_build2_loc (input_location, MULT_EXPR,
				       TREE_TYPE (tmp), tmp,
				       fold_convert (TREE_TYPE (tmp),
						     se.string_length));
	    }
	  else if (code->ext.alloc.ts.type != BT_UNKNOWN)
	    /* Handle all types, where the alloc_type_spec is set.  */
	    memsz = TYPE_SIZE_UNIT (gfc_typenode_for_spec (&code->ext.alloc.ts));
	  else
	    /* Handle size computation of the type declared to alloc.  */
	    memsz = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (se.expr)));

	  /* Store the caf-attributes for latter use.  */
	  if (flag_coarray == GFC_FCOARRAY_LIB
	      && (caf_attr = gfc_caf_attr (expr, true, &caf_refs_comp))
		 .codimension)
	    {
	      /* Scalar allocatable components in coarray'ed derived types make
		 it here and are treated now.  */
	      tree caf_decl, token;
	      gfc_se caf_se;

	      is_coarray = true;
	      /* Set flag, to add synchronize after the allocate.  */
	      needs_caf_sync = needs_caf_sync
		  || caf_attr.coarray_comp || !caf_refs_comp;

	      gfc_init_se (&caf_se, NULL);

	      caf_decl = gfc_get_tree_for_caf_expr (expr);
	      gfc_get_caf_token_offset (&caf_se, &token, NULL, caf_decl,
					NULL_TREE, NULL);
	      gfc_add_block_to_block (&se.pre, &caf_se.pre);
	      gfc_allocate_allocatable (&se.pre, se.expr, memsz,
					gfc_build_addr_expr (NULL_TREE, token),
					NULL_TREE, NULL_TREE, NULL_TREE,
					label_finish, expr, 1);
	    }
	  /* Allocate - for non-pointers with re-alloc checking.  */
	  else if (gfc_expr_attr (expr).allocatable)
	    gfc_allocate_allocatable (&se.pre, se.expr, memsz,
				      NULL_TREE, stat, errmsg, errlen,
				      label_finish, expr, 0);
	  else
	    gfc_allocate_using_malloc (&se.pre, se.expr, memsz, stat);
	}
      else
	{
	  /* Allocating coarrays needs a sync after the allocate executed.
	     Set the flag to add the sync after all objects are allocated.  */
	  if (flag_coarray == GFC_FCOARRAY_LIB
	      && (caf_attr = gfc_caf_attr (expr, true, &caf_refs_comp))
		 .codimension)
	    {
	      is_coarray = true;
	      needs_caf_sync = needs_caf_sync
		  || caf_attr.coarray_comp || !caf_refs_comp;
	    }

	  if (expr->ts.type == BT_CHARACTER && al_len != NULL_TREE
	      && expr3_len != NULL_TREE)
	    {
	      /* Arrays need to have a _len set before the array
		 descriptor is filled.  */
	      gfc_add_modify (&block, al_len,
			      fold_convert (TREE_TYPE (al_len), expr3_len));
	      /* Prevent setting the length twice.  */
	      al_len_needs_set = false;
	    }
	  else if (expr->ts.type == BT_CHARACTER && al_len != NULL_TREE
	      && code->ext.alloc.ts.u.cl->length)
	    {
	      /* Cover the cases where a string length is explicitly
		 specified by a type spec for deferred length character
		 arrays or unlimited polymorphic objects without a
		 source= or mold= expression.  */
	      gfc_init_se (&se_sz, NULL);
	      gfc_conv_expr (&se_sz, code->ext.alloc.ts.u.cl->length);
	      gfc_add_block_to_block (&block, &se_sz.pre);
	      gfc_add_modify (&block, al_len,
			      fold_convert (TREE_TYPE (al_len),
					    se_sz.expr));
	      al_len_needs_set = false;
	    }
	}

      gfc_add_block_to_block (&block, &se.pre);

      /* Error checking -- Note: ERRMSG only makes sense with STAT.  */
      if (code->expr1)
	{
	  tmp = build1_v (GOTO_EXPR, label_errmsg);
	  parm = fold_build2_loc (input_location, NE_EXPR,
				  boolean_type_node, stat,
				  build_int_cst (TREE_TYPE (stat), 0));
	  tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node,
				 gfc_unlikely (parm, PRED_FORTRAN_FAIL_ALLOC),
				 tmp, build_empty_stmt (input_location));
	  gfc_add_expr_to_block (&block, tmp);
	}

      /* Set the vptr only when no source= is set.  When source= is set, then
	 the trans_assignment below will set the vptr.  */
      if (al_vptr != NULL_TREE && (!code->expr3 || code->expr3->mold))
	{
	  if (expr3_vptr != NULL_TREE)
	    /* The vtab is already known, so just assign it.  */
	    gfc_add_modify (&block, al_vptr,
			    fold_convert (TREE_TYPE (al_vptr), expr3_vptr));
	  else
	    {
	      /* VPTR is fixed at compile time.  */
	      gfc_symbol *vtab;
	      gfc_typespec *ts;

	      if (code->expr3)
		/* Although expr3 is pre-evaluated above, it may happen,
		   that for arrays or in mold= cases the pre-evaluation
		   was not successful.  In these rare cases take the vtab
		   from the typespec of expr3 here.  */
		ts = &code->expr3->ts;
	      else if (code->ext.alloc.ts.type == BT_DERIVED || upoly_expr)
		/* The alloc_type_spec gives the type to allocate or the
		   al is unlimited polymorphic, which enforces the use of
		   an alloc_type_spec that is not necessarily a BT_DERIVED.  */
		ts = &code->ext.alloc.ts;
	      else
		/* Prepare for setting the vtab as declared.  */
		ts = &expr->ts;

	      vtab = gfc_find_vtab (ts);
	      gcc_assert (vtab);
	      tmp = gfc_build_addr_expr (NULL_TREE,
					 gfc_get_symbol_decl (vtab));
	      gfc_add_modify (&block, al_vptr,
			      fold_convert (TREE_TYPE (al_vptr), tmp));
	    }
	}

      /* Add assignment for string length.  */
      if (al_len != NULL_TREE && al_len_needs_set)
	{
	  if (expr3_len != NULL_TREE)
	    {
	      gfc_add_modify (&block, al_len,
			      fold_convert (TREE_TYPE (al_len),
					    expr3_len));
	      /* When tmp_expr3_len_flag is set, then expr3_len is
		 abused to carry the length information from the
		 alloc_type.  Clear it to prevent setting incorrect len
		 information in future loop iterations.  */
	      if (tmp_expr3_len_flag)
		/* No need to reset tmp_expr3_len_flag, because the
		   presence of an expr3 can not change within in the
		   loop.  */
		expr3_len = NULL_TREE;
	    }
	  else if (code->ext.alloc.ts.type == BT_CHARACTER
	      && code->ext.alloc.ts.u.cl->length)
	    {
	      /* Cover the cases where a string length is explicitly
		 specified by a type spec for deferred length character
		 arrays or unlimited polymorphic objects without a
		 source= or mold= expression.  */
	      if (expr3_esize == NULL_TREE || code->ext.alloc.ts.kind != 1)
		{
		  gfc_init_se (&se_sz, NULL);
		  gfc_conv_expr (&se_sz, code->ext.alloc.ts.u.cl->length);
		  gfc_add_block_to_block (&block, &se_sz.pre);
		  gfc_add_modify (&block, al_len,
				  fold_convert (TREE_TYPE (al_len),
						se_sz.expr));
		}
	      else
		gfc_add_modify (&block, al_len,
				fold_convert (TREE_TYPE (al_len),
					      expr3_esize));
	    }
	  else
	    /* No length information needed, because type to allocate
	       has no length.  Set _len to 0.  */
	    gfc_add_modify (&block, al_len,
			    fold_convert (TREE_TYPE (al_len),
					  integer_zero_node));
	}

      init_expr = NULL;
      if (code->expr3 && !code->expr3->mold && e3_is != E3_MOLD)
	{
	  /* Initialization via SOURCE block (or static default initializer).
	     Switch off automatic reallocation since we have just done the
	     ALLOCATE.  */
	  int realloc_lhs = flag_realloc_lhs;
	  gfc_expr *init_expr = gfc_expr_to_initialize (expr);
	  gfc_expr *rhs = e3rhs ? e3rhs : gfc_copy_expr (code->expr3);
	  flag_realloc_lhs = 0;
	  tmp = gfc_trans_assignment (init_expr, rhs, false, false, true,
				      false);
	  flag_realloc_lhs = realloc_lhs;
	  /* Free the expression allocated for init_expr.  */
	  gfc_free_expr (init_expr);
	  if (rhs != e3rhs)
	    gfc_free_expr (rhs);
	  gfc_add_expr_to_block (&block, tmp);
	}
      else if (code->expr3 && code->expr3->mold
	       && code->expr3->ts.type == BT_CLASS)
	{
	  /* Use class_init_assign to initialize expr.  */
	  gfc_code *ini;
	  ini = gfc_get_code (EXEC_INIT_ASSIGN);
	  ini->expr1 = gfc_find_and_cut_at_last_class_ref (expr);
	  tmp = gfc_trans_class_init_assign (ini);
	  gfc_free_statements (ini);
	  gfc_add_expr_to_block (&block, tmp);
	}
      else if ((init_expr = allocate_get_initializer (code, expr)))
	{
	  /* Use class_init_assign to initialize expr.  */
	  gfc_code *ini;
	  int realloc_lhs = flag_realloc_lhs;
	  ini = gfc_get_code (EXEC_INIT_ASSIGN);
	  ini->expr1 = gfc_expr_to_initialize (expr);
	  ini->expr2 = init_expr;
	  flag_realloc_lhs = 0;
	  tmp= gfc_trans_init_assign (ini);
	  flag_realloc_lhs = realloc_lhs;
	  gfc_free_statements (ini);
	  /* Init_expr is freeed by above free_statements, just need to null
	     it here.  */
	  init_expr = NULL;
	  gfc_add_expr_to_block (&block, tmp);
	}

      /* Nullify all pointers in derived type coarrays.  This registers a
	 token for them which allows their allocation.  */
      if (is_coarray)
	{
	  gfc_symbol *type = NULL;
	  symbol_attribute caf_attr;
	  int rank = 0;
	  if (code->ext.alloc.ts.type == BT_DERIVED
	      && code->ext.alloc.ts.u.derived->attr.pointer_comp)
	    {
	      type = code->ext.alloc.ts.u.derived;
	      rank = type->attr.dimension ? type->as->rank : 0;
	      gfc_clear_attr (&caf_attr);
	    }
	  else if (expr->ts.type == BT_DERIVED
		   && expr->ts.u.derived->attr.pointer_comp)
	    {
	      type = expr->ts.u.derived;
	      rank = expr->rank;
	      caf_attr = gfc_caf_attr (expr, true);
	    }

	  /* Initialize the tokens of pointer components in derived type
	     coarrays.  */
	  if (type)
	    {
	      tmp = (caf_attr.codimension && !caf_attr.dimension)
		  ? gfc_conv_descriptor_data_get (se.expr) : se.expr;
	      tmp = gfc_nullify_alloc_comp (type, tmp, rank,
					    GFC_STRUCTURE_CAF_MODE_IN_COARRAY);
	      gfc_add_expr_to_block (&block, tmp);
	    }
	}

      gfc_free_expr (expr);
    } // for-loop

  if (e3rhs)
    {
      if (newsym)
	{
	  gfc_free_symbol (newsym->n.sym);
	  XDELETE (newsym);
	}
      gfc_free_expr (e3rhs);
    }
  /* STAT.  */
  if (code->expr1)
    {
      tmp = build1_v (LABEL_EXPR, label_errmsg);
      gfc_add_expr_to_block (&block, tmp);
    }

  /* ERRMSG - only useful if STAT is present.  */
  if (code->expr1 && code->expr2)
    {
      const char *msg = "Attempt to allocate an allocated object";
      tree slen, dlen, errmsg_str;
      stmtblock_t errmsg_block;

      gfc_init_block (&errmsg_block);

      errmsg_str = gfc_create_var (pchar_type_node, "ERRMSG");
      gfc_add_modify (&errmsg_block, errmsg_str,
		gfc_build_addr_expr (pchar_type_node,
			gfc_build_localized_cstring_const (msg)));

      slen = build_int_cst (gfc_charlen_type_node, ((int) strlen (msg)));
      dlen = gfc_get_expr_charlen (code->expr2);
      slen = fold_build2_loc (input_location, MIN_EXPR,
			      TREE_TYPE (slen), dlen, slen);

      gfc_trans_string_copy (&errmsg_block, dlen, errmsg,
			     code->expr2->ts.kind,
			     slen, errmsg_str,
			     gfc_default_character_kind);
      dlen = gfc_finish_block (&errmsg_block);

      tmp = fold_build2_loc (input_location, NE_EXPR, boolean_type_node,
			     stat, build_int_cst (TREE_TYPE (stat), 0));

      tmp = build3_v (COND_EXPR, tmp,
		      dlen, build_empty_stmt (input_location));

      gfc_add_expr_to_block (&block, tmp);
    }

  /* STAT block.  */
  if (code->expr1)
    {
      if (TREE_USED (label_finish))
	{
	  tmp = build1_v (LABEL_EXPR, label_finish);
	  gfc_add_expr_to_block (&block, tmp);
	}

      gfc_init_se (&se, NULL);
      gfc_conv_expr_lhs (&se, code->expr1);
      tmp = convert (TREE_TYPE (se.expr), stat);
      gfc_add_modify (&block, se.expr, tmp);
    }

  if (needs_caf_sync)
    {
      /* Add a sync all after the allocation has been executed.  */
      tmp = build_call_expr_loc (input_location, gfor_fndecl_caf_sync_all,
				 3, null_pointer_node, null_pointer_node,
				 integer_zero_node);
      gfc_add_expr_to_block (&post, tmp);
    }

  gfc_add_block_to_block (&block, &se.post);
  gfc_add_block_to_block (&block, &post);

  return gfc_finish_block (&block);
}


/* Translate a DEALLOCATE statement.  */

tree
gfc_trans_deallocate (gfc_code *code)
{
  gfc_se se;
  gfc_alloc *al;
  tree apstat, pstat, stat, errmsg, errlen, tmp;
  tree label_finish, label_errmsg;
  stmtblock_t block;

  pstat = apstat = stat = errmsg = errlen = tmp = NULL_TREE;
  label_finish = label_errmsg = NULL_TREE;

  gfc_start_block (&block);

  /* Count the number of failed deallocations.  If deallocate() was
     called with STAT= , then set STAT to the count.  If deallocate
     was called with ERRMSG, then set ERRMG to a string.  */
  if (code->expr1)
    {
      tree gfc_int4_type_node = gfc_get_int_type (4);

      stat = gfc_create_var (gfc_int4_type_node, "stat");
      pstat = gfc_build_addr_expr (NULL_TREE, stat);

      /* GOTO destinations.  */
      label_errmsg = gfc_build_label_decl (NULL_TREE);
      label_finish = gfc_build_label_decl (NULL_TREE);
      TREE_USED (label_finish) = 0;
    }

  /* Set ERRMSG - only needed if STAT is available.  */
  if (code->expr1 && code->expr2)
    {
      gfc_init_se (&se, NULL);
      se.want_pointer = 1;
      gfc_conv_expr_lhs (&se, code->expr2);
      errmsg = se.expr;
      errlen = se.string_length;
    }

  for (al = code->ext.alloc.list; al != NULL; al = al->next)
    {
      gfc_expr *expr = gfc_copy_expr (al->expr);
      bool is_coarray = false, is_coarray_array = false;
      int caf_mode = 0;

      gcc_assert (expr->expr_type == EXPR_VARIABLE);

      if (expr->ts.type == BT_CLASS)
	gfc_add_data_component (expr);

      gfc_init_se (&se, NULL);
      gfc_start_block (&se.pre);

      se.want_pointer = 1;
      se.descriptor_only = 1;
      gfc_conv_expr (&se, expr);

      if (flag_coarray == GFC_FCOARRAY_LIB
	  || flag_coarray == GFC_FCOARRAY_SINGLE)
	{
	  bool comp_ref;
	  symbol_attribute caf_attr = gfc_caf_attr (expr, false, &comp_ref);
	  if (caf_attr.codimension)
	    {
	      is_coarray = true;
	      is_coarray_array = caf_attr.dimension || !comp_ref
		  || caf_attr.coarray_comp;

	      if (flag_coarray == GFC_FCOARRAY_LIB)
		/* When the expression to deallocate is referencing a
		   component, then only deallocate it, but do not
		   deregister.  */
		caf_mode = GFC_STRUCTURE_CAF_MODE_IN_COARRAY
		    | (comp_ref && !caf_attr.coarray_comp
		       ? GFC_STRUCTURE_CAF_MODE_DEALLOC_ONLY : 0);
	    }
	}

      if (expr->rank || is_coarray_array)
	{
	  gfc_ref *ref;

	  if (gfc_bt_struct (expr->ts.type)
	      && expr->ts.u.derived->attr.alloc_comp
	      && !gfc_is_finalizable (expr->ts.u.derived, NULL))
	    {
	      gfc_ref *last = NULL;

	      for (ref = expr->ref; ref; ref = ref->next)
		if (ref->type == REF_COMPONENT)
		  last = ref;

	      /* Do not deallocate the components of a derived type
		 ultimate pointer component.  */
	      if (!(last && last->u.c.component->attr.pointer)
		    && !(!last && expr->symtree->n.sym->attr.pointer))
		{
		  if (is_coarray && expr->rank == 0
		      && (!last || !last->u.c.component->attr.dimension)
		      && GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (se.expr)))
		    {
		      /* Add the ref to the data member only, when this is not
			 a regular array or deallocate_alloc_comp will try to
			 add another one.  */
		      tmp = gfc_conv_descriptor_data_get (se.expr);
		    }
		  else
		    tmp = se.expr;
		  tmp = gfc_deallocate_alloc_comp (expr->ts.u.derived, tmp,
						   expr->rank, caf_mode);
		  gfc_add_expr_to_block (&se.pre, tmp);
		}
	    }

	  if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (se.expr)))
	    {
	      gfc_coarray_deregtype caf_dtype;

	      if (is_coarray)
		caf_dtype = gfc_caf_is_dealloc_only (caf_mode)
		    ? GFC_CAF_COARRAY_DEALLOCATE_ONLY
		    : GFC_CAF_COARRAY_DEREGISTER;
	      else
		caf_dtype = GFC_CAF_COARRAY_NOCOARRAY;
	      tmp = gfc_deallocate_with_status (se.expr, pstat, errmsg, errlen,
						label_finish, false, expr,
						caf_dtype);
	      gfc_add_expr_to_block (&se.pre, tmp);
	    }
	  else if (TREE_CODE (se.expr) == COMPONENT_REF
		   && TREE_CODE (TREE_TYPE (se.expr)) == ARRAY_TYPE
		   && TREE_CODE (TREE_TYPE (TREE_TYPE (se.expr)))
			== RECORD_TYPE)
	    {
	      /* class.c(finalize_component) generates these, when a
		 finalizable entity has a non-allocatable derived type array
		 component, which has allocatable components. Obtain the
		 derived type of the array and deallocate the allocatable
		 components. */
	      for (ref = expr->ref; ref; ref = ref->next)
		{
		  if (ref->u.c.component->attr.dimension
		      && ref->u.c.component->ts.type == BT_DERIVED)
		    break;
		}

	      if (ref && ref->u.c.component->ts.u.derived->attr.alloc_comp
		  && !gfc_is_finalizable (ref->u.c.component->ts.u.derived,
					  NULL))
		{
		  tmp = gfc_deallocate_alloc_comp
				(ref->u.c.component->ts.u.derived,
				 se.expr, expr->rank);
		  gfc_add_expr_to_block (&se.pre, tmp);
		}
	    }

	  if (al->expr->ts.type == BT_CLASS)
	    {
	      gfc_reset_vptr (&se.pre, al->expr);
	      if (UNLIMITED_POLY (al->expr)
		  || (al->expr->ts.type == BT_DERIVED
		      && al->expr->ts.u.derived->attr.unlimited_polymorphic))
		/* Clear _len, too.  */
		gfc_reset_len (&se.pre, al->expr);
	    }
	}
      else
	{
	  tmp = gfc_deallocate_scalar_with_status (se.expr, pstat, label_finish,
						   false, al->expr,
						   al->expr->ts, is_coarray);
	  gfc_add_expr_to_block (&se.pre, tmp);

	  /* Set to zero after deallocation.  */
	  tmp = fold_build2_loc (input_location, MODIFY_EXPR, void_type_node,
				 se.expr,
				 build_int_cst (TREE_TYPE (se.expr), 0));
	  gfc_add_expr_to_block (&se.pre, tmp);

	  if (al->expr->ts.type == BT_CLASS)
	    {
	      gfc_reset_vptr (&se.pre, al->expr);
	      if (UNLIMITED_POLY (al->expr)
		  || (al->expr->ts.type == BT_DERIVED
		      && al->expr->ts.u.derived->attr.unlimited_polymorphic))
		/* Clear _len, too.  */
		gfc_reset_len (&se.pre, al->expr);
	    }
	}

      if (code->expr1)
	{
          tree cond;

	  cond = fold_build2_loc (input_location, NE_EXPR, boolean_type_node, stat,
				  build_int_cst (TREE_TYPE (stat), 0));
	  tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node,
				 gfc_unlikely (cond, PRED_FORTRAN_FAIL_ALLOC),
				 build1_v (GOTO_EXPR, label_errmsg),
				 build_empty_stmt (input_location));
	  gfc_add_expr_to_block (&se.pre, tmp);
	}

      tmp = gfc_finish_block (&se.pre);
      gfc_add_expr_to_block (&block, tmp);
      gfc_free_expr (expr);
    }

  if (code->expr1)
    {
      tmp = build1_v (LABEL_EXPR, label_errmsg);
      gfc_add_expr_to_block (&block, tmp);
    }

  /* Set ERRMSG - only needed if STAT is available.  */
  if (code->expr1 && code->expr2)
    {
      const char *msg = "Attempt to deallocate an unallocated object";
      stmtblock_t errmsg_block;
      tree errmsg_str, slen, dlen, cond;

      gfc_init_block (&errmsg_block);

      errmsg_str = gfc_create_var (pchar_type_node, "ERRMSG");
      gfc_add_modify (&errmsg_block, errmsg_str,
		gfc_build_addr_expr (pchar_type_node,
                        gfc_build_localized_cstring_const (msg)));
      slen = build_int_cst (gfc_charlen_type_node, ((int) strlen (msg)));
      dlen = gfc_get_expr_charlen (code->expr2);

      gfc_trans_string_copy (&errmsg_block, dlen, errmsg, code->expr2->ts.kind,
			     slen, errmsg_str, gfc_default_character_kind);
      tmp = gfc_finish_block (&errmsg_block);

      cond = fold_build2_loc (input_location, NE_EXPR, boolean_type_node, stat,
			     build_int_cst (TREE_TYPE (stat), 0));
      tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node,
			     gfc_unlikely (cond, PRED_FORTRAN_FAIL_ALLOC), tmp,
			     build_empty_stmt (input_location));

      gfc_add_expr_to_block (&block, tmp);
    }

  if (code->expr1 && TREE_USED (label_finish))
    {
      tmp = build1_v (LABEL_EXPR, label_finish);
      gfc_add_expr_to_block (&block, tmp);
    }

  /* Set STAT.  */
  if (code->expr1)
    {
      gfc_init_se (&se, NULL);
      gfc_conv_expr_lhs (&se, code->expr1);
      tmp = convert (TREE_TYPE (se.expr), stat);
      gfc_add_modify (&block, se.expr, tmp);
    }

  return gfc_finish_block (&block);
}

#include "gt-fortran-trans-stmt.h"
