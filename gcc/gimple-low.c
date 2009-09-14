/* GIMPLE lowering pass.  Converts High GIMPLE into Low GIMPLE.

   Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.

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
#include "tree.h"
#include "rtl.h"
#include "varray.h"
#include "gimple.h"
#include "tree-iterator.h"
#include "tree-inline.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "tree-flow.h"
#include "timevar.h"
#include "except.h"
#include "hashtab.h"
#include "flags.h"
#include "function.h"
#include "expr.h"
#include "toplev.h"
#include "tree-pass.h"

/* The differences between High GIMPLE and Low GIMPLE are the
   following:

   1- Lexical scopes are removed (i.e., GIMPLE_BIND disappears).

   2- GIMPLE_TRY and GIMPLE_CATCH are converted to abnormal control
      flow and exception regions are built as an on-the-side region
      hierarchy (See tree-eh.c:lower_eh_constructs).

   3- Multiple identical return statements are grouped into a single
      return and gotos to the unique return site.  */

/* Match a return statement with a label.  During lowering, we identify
   identical return statements and replace duplicates with a jump to
   the corresponding label.  */
struct return_statements_t
{
  tree label;
  gimple stmt;
};
typedef struct return_statements_t return_statements_t;

DEF_VEC_O(return_statements_t);
DEF_VEC_ALLOC_O(return_statements_t,heap);

struct lower_data
{
  /* Block the current statement belongs to.  */
  tree block;

  /* A vector of label and return statements to be moved to the end
     of the function.  */
  VEC(return_statements_t,heap) *return_statements;

  /* True if the function calls __builtin_setjmp.  */
  bool calls_builtin_setjmp;
};

static void lower_stmt (gimple_stmt_iterator *, struct lower_data *);
static void lower_gimple_bind (gimple_stmt_iterator *, struct lower_data *);
static void lower_gimple_return (gimple_stmt_iterator *, struct lower_data *);
static void lower_builtin_setjmp (gimple_stmt_iterator *);


/* Lower the body of current_function_decl from High GIMPLE into Low
   GIMPLE.  */

static unsigned int
lower_function_body (void)
{
  struct lower_data data;
  gimple_seq body = gimple_body (current_function_decl);
  gimple_seq lowered_body;
  gimple_stmt_iterator i;
  gimple bind;
  tree t;
  gimple x;

  /* The gimplifier should've left a body of exactly one statement,
     namely a GIMPLE_BIND.  */
  gcc_assert (gimple_seq_first (body) == gimple_seq_last (body)
	      && gimple_code (gimple_seq_first_stmt (body)) == GIMPLE_BIND);

  memset (&data, 0, sizeof (data));
  data.block = DECL_INITIAL (current_function_decl);
  BLOCK_SUBBLOCKS (data.block) = NULL_TREE;
  BLOCK_CHAIN (data.block) = NULL_TREE;
  TREE_ASM_WRITTEN (data.block) = 1;
  data.return_statements = VEC_alloc (return_statements_t, heap, 8);

  bind = gimple_seq_first_stmt (body);
  lowered_body = NULL;
  gimple_seq_add_stmt (&lowered_body, bind);
  i = gsi_start (lowered_body);
  lower_gimple_bind (&i, &data);

  /* Once the old body has been lowered, replace it with the new
     lowered sequence.  */
  gimple_set_body (current_function_decl, lowered_body);

  i = gsi_last (lowered_body);

  /* If the function falls off the end, we need a null return statement.
     If we've already got one in the return_statements vector, we don't
     need to do anything special.  Otherwise build one by hand.  */
  if (gimple_seq_may_fallthru (lowered_body)
      && (VEC_empty (return_statements_t, data.return_statements)
	  || gimple_return_retval (VEC_last (return_statements_t,
			           data.return_statements)->stmt) != NULL))
    {
      x = gimple_build_return (NULL);
      gimple_set_location (x, cfun->function_end_locus);
      gimple_set_block (x, DECL_INITIAL (current_function_decl));
      gsi_insert_after (&i, x, GSI_CONTINUE_LINKING);
    }

  /* If we lowered any return statements, emit the representative
     at the end of the function.  */
  while (!VEC_empty (return_statements_t, data.return_statements))
    {
      return_statements_t t;

      /* Unfortunately, we can't use VEC_pop because it returns void for
	 objects.  */
      t = *VEC_last (return_statements_t, data.return_statements);
      VEC_truncate (return_statements_t,
	  	    data.return_statements,
	  	    VEC_length (return_statements_t,
		      		data.return_statements) - 1);

      x = gimple_build_label (t.label);
      gsi_insert_after (&i, x, GSI_CONTINUE_LINKING);

      /* Remove the line number from the representative return statement.
	 It now fills in for many such returns.  Failure to remove this
	 will result in incorrect results for coverage analysis.  */
      gimple_set_location (t.stmt, UNKNOWN_LOCATION);
      gsi_insert_after (&i, t.stmt, GSI_CONTINUE_LINKING);
    }

  /* If the function calls __builtin_setjmp, we need to emit the computed
     goto that will serve as the unique dispatcher for all the receivers.  */
  if (data.calls_builtin_setjmp)
    {
      tree disp_label, disp_var, arg;

      /* Build 'DISP_LABEL:' and insert.  */
      disp_label = create_artificial_label (cfun->function_end_locus);
      /* This mark will create forward edges from every call site.  */
      DECL_NONLOCAL (disp_label) = 1;
      cfun->has_nonlocal_label = 1;
      x = gimple_build_label (disp_label);
      gsi_insert_after (&i, x, GSI_CONTINUE_LINKING);

      /* Build 'DISP_VAR = __builtin_setjmp_dispatcher (DISP_LABEL);'
	 and insert.  */
      disp_var = create_tmp_var (ptr_type_node, "setjmpvar");
      arg = build_addr (disp_label, current_function_decl);
      t = implicit_built_in_decls[BUILT_IN_SETJMP_DISPATCHER];
      x = gimple_build_call (t, 1, arg);
      gimple_call_set_lhs (x, disp_var);

      /* Build 'goto DISP_VAR;' and insert.  */
      gsi_insert_after (&i, x, GSI_CONTINUE_LINKING);
      x = gimple_build_goto (disp_var);
      gsi_insert_after (&i, x, GSI_CONTINUE_LINKING);
    }

  gcc_assert (data.block == DECL_INITIAL (current_function_decl));
  BLOCK_SUBBLOCKS (data.block)
    = blocks_nreverse (BLOCK_SUBBLOCKS (data.block));

  clear_block_marks (data.block);
  VEC_free(return_statements_t, heap, data.return_statements);
  return 0;
}

struct gimple_opt_pass pass_lower_cf = 
{
 {
  GIMPLE_PASS,
  "lower",				/* name */
  NULL,					/* gate */
  lower_function_body,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_NONE,				/* tv_id */
  PROP_gimple_any,			/* properties_required */
  PROP_gimple_lcf,			/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func			/* todo_flags_finish */
 }
};


/* Verify if the type of the argument matches that of the function
   declaration.  If we cannot verify this or there is a mismatch,
   return false.  */

bool
gimple_check_call_args (gimple stmt)
{
  tree fndecl, parms, p;
  unsigned int i, nargs;

  nargs = gimple_call_num_args (stmt);

  /* Get argument types for verification.  */
  fndecl = gimple_call_fndecl (stmt);
  parms = NULL_TREE;
  if (fndecl)
    parms = TYPE_ARG_TYPES (TREE_TYPE (fndecl));
  else if (POINTER_TYPE_P (TREE_TYPE (gimple_call_fn (stmt))))
    parms = TYPE_ARG_TYPES (TREE_TYPE (TREE_TYPE (gimple_call_fn (stmt))));

  /* Verify if the type of the argument matches that of the function
     declaration.  If we cannot verify this or there is a mismatch,
     return false.  */
  if (fndecl && DECL_ARGUMENTS (fndecl))
    {
      for (i = 0, p = DECL_ARGUMENTS (fndecl);
	   i < nargs;
	   i++, p = TREE_CHAIN (p))
	{
	  /* We cannot distinguish a varargs function from the case
	     of excess parameters, still deferring the inlining decision
	     to the callee is possible.  */
	  if (!p)
	    break;
	  if (p == error_mark_node
	      || gimple_call_arg (stmt, i) == error_mark_node
	      || !fold_convertible_p (DECL_ARG_TYPE (p),
				      gimple_call_arg (stmt, i)))
            return false;
	}
    }
  else if (parms)
    {
      for (i = 0, p = parms; i < nargs; i++, p = TREE_CHAIN (p))
	{
	  /* If this is a varargs function defer inlining decision
	     to callee.  */
	  if (!p)
	    break;
	  if (TREE_VALUE (p) == error_mark_node
	      || gimple_call_arg (stmt, i) == error_mark_node
	      || TREE_CODE (TREE_VALUE (p)) == VOID_TYPE
	      || !fold_convertible_p (TREE_VALUE (p),
				      gimple_call_arg (stmt, i)))
            return false;
	}
    }
  else
    {
      if (nargs != 0)
        return false;
    }
  return true;
}


/* Lower sequence SEQ.  Unlike gimplification the statements are not relowered
   when they are changed -- if this has to be done, the lowering routine must
   do it explicitly.  DATA is passed through the recursion.  */

static void
lower_sequence (gimple_seq seq, struct lower_data *data)
{
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start (seq); !gsi_end_p (gsi); )
    lower_stmt (&gsi, data);
}


/* Lower the OpenMP directive statement pointed by GSI.  DATA is
   passed through the recursion.  */

static void
lower_omp_directive (gimple_stmt_iterator *gsi, struct lower_data *data)
{
  gimple stmt;
  
  stmt = gsi_stmt (*gsi);

  lower_sequence (gimple_omp_body (stmt), data);
  gsi_insert_before (gsi, stmt, GSI_SAME_STMT);
  gsi_insert_seq_before (gsi, gimple_omp_body (stmt), GSI_SAME_STMT);
  gimple_omp_set_body (stmt, NULL);
  gsi_remove (gsi, false);
}


/* Lower statement GSI.  DATA is passed through the recursion.  */

static void
lower_stmt (gimple_stmt_iterator *gsi, struct lower_data *data)
{
  gimple stmt = gsi_stmt (*gsi);

  gimple_set_block (stmt, data->block);

  switch (gimple_code (stmt))
    {
    case GIMPLE_BIND:
      lower_gimple_bind (gsi, data);
      return;

    case GIMPLE_COND:
      /* The gimplifier has already lowered this into gotos.  */
      break;

    case GIMPLE_RETURN:
      lower_gimple_return (gsi, data);
      return;

    case GIMPLE_TRY:
      lower_sequence (gimple_try_eval (stmt), data);
      lower_sequence (gimple_try_cleanup (stmt), data);
      break;

    case GIMPLE_CATCH:
      lower_sequence (gimple_catch_handler (stmt), data);
      break;

    case GIMPLE_EH_FILTER:
      lower_sequence (gimple_eh_filter_failure (stmt), data);
      break;

    case GIMPLE_NOP:
    case GIMPLE_ASM:
    case GIMPLE_ASSIGN:
    case GIMPLE_GOTO:
    case GIMPLE_PREDICT:
    case GIMPLE_LABEL:
    case GIMPLE_SWITCH:
    case GIMPLE_EH_MUST_NOT_THROW:
    case GIMPLE_OMP_FOR:
    case GIMPLE_OMP_SECTIONS:
    case GIMPLE_OMP_SECTIONS_SWITCH:
    case GIMPLE_OMP_SECTION:
    case GIMPLE_OMP_SINGLE:
    case GIMPLE_OMP_MASTER:
    case GIMPLE_OMP_ORDERED:
    case GIMPLE_OMP_CRITICAL:
    case GIMPLE_OMP_RETURN:
    case GIMPLE_OMP_ATOMIC_LOAD:
    case GIMPLE_OMP_ATOMIC_STORE:
    case GIMPLE_OMP_CONTINUE:
      break;

    case GIMPLE_CALL:
      {
	tree decl = gimple_call_fndecl (stmt);

	if (decl
	    && DECL_BUILT_IN_CLASS (decl) == BUILT_IN_NORMAL
	    && DECL_FUNCTION_CODE (decl) == BUILT_IN_SETJMP)
	  {
	    data->calls_builtin_setjmp = true;
	    lower_builtin_setjmp (gsi);
	    return;
	  }
      }
      break;

    case GIMPLE_OMP_PARALLEL:
    case GIMPLE_OMP_TASK:
      lower_omp_directive (gsi, data);
      return;

    default:
      gcc_unreachable ();
    }

  gsi_next (gsi);
}

/* Lower a bind_expr TSI.  DATA is passed through the recursion.  */

static void
lower_gimple_bind (gimple_stmt_iterator *gsi, struct lower_data *data)
{
  tree old_block = data->block;
  gimple stmt = gsi_stmt (*gsi);
  tree new_block = gimple_bind_block (stmt);

  if (new_block)
    {
      if (new_block == old_block)
	{
	  /* The outermost block of the original function may not be the
	     outermost statement chain of the gimplified function.  So we
	     may see the outermost block just inside the function.  */
	  gcc_assert (new_block == DECL_INITIAL (current_function_decl));
	  new_block = NULL;
	}
      else
	{
	  /* We do not expect to handle duplicate blocks.  */
	  gcc_assert (!TREE_ASM_WRITTEN (new_block));
	  TREE_ASM_WRITTEN (new_block) = 1;

	  /* Block tree may get clobbered by inlining.  Normally this would
	     be fixed in rest_of_decl_compilation using block notes, but
	     since we are not going to emit them, it is up to us.  */
	  BLOCK_CHAIN (new_block) = BLOCK_SUBBLOCKS (old_block);
	  BLOCK_SUBBLOCKS (old_block) = new_block;
	  BLOCK_SUBBLOCKS (new_block) = NULL_TREE;
	  BLOCK_SUPERCONTEXT (new_block) = old_block;

	  data->block = new_block;
	}
    }

  record_vars (gimple_bind_vars (stmt));
  lower_sequence (gimple_bind_body (stmt), data);

  if (new_block)
    {
      gcc_assert (data->block == new_block);

      BLOCK_SUBBLOCKS (new_block)
	= blocks_nreverse (BLOCK_SUBBLOCKS (new_block));
      data->block = old_block;
    }

  /* The GIMPLE_BIND no longer carries any useful information -- kill it.  */
  gsi_insert_seq_before (gsi, gimple_bind_body (stmt), GSI_SAME_STMT);
  gsi_remove (gsi, false);
}

/* Try to determine whether a TRY_CATCH expression can fall through.
   This is a subroutine of block_may_fallthru.  */

static bool
try_catch_may_fallthru (const_tree stmt)
{
  tree_stmt_iterator i;

  /* If the TRY block can fall through, the whole TRY_CATCH can
     fall through.  */
  if (block_may_fallthru (TREE_OPERAND (stmt, 0)))
    return true;

  i = tsi_start (TREE_OPERAND (stmt, 1));
  switch (TREE_CODE (tsi_stmt (i)))
    {
    case CATCH_EXPR:
      /* We expect to see a sequence of CATCH_EXPR trees, each with a
	 catch expression and a body.  The whole TRY_CATCH may fall
	 through iff any of the catch bodies falls through.  */
      for (; !tsi_end_p (i); tsi_next (&i))
	{
	  if (block_may_fallthru (CATCH_BODY (tsi_stmt (i))))
	    return true;
	}
      return false;

    case EH_FILTER_EXPR:
      /* The exception filter expression only matters if there is an
	 exception.  If the exception does not match EH_FILTER_TYPES,
	 we will execute EH_FILTER_FAILURE, and we will fall through
	 if that falls through.  If the exception does match
	 EH_FILTER_TYPES, the stack unwinder will continue up the
	 stack, so we will not fall through.  We don't know whether we
	 will throw an exception which matches EH_FILTER_TYPES or not,
	 so we just ignore EH_FILTER_TYPES and assume that we might
	 throw an exception which doesn't match.  */
      return block_may_fallthru (EH_FILTER_FAILURE (tsi_stmt (i)));

    default:
      /* This case represents statements to be executed when an
	 exception occurs.  Those statements are implicitly followed
	 by a RESX statement to resume execution after the exception.
	 So in this case the TRY_CATCH never falls through.  */
      return false;
    }
}


/* Same as above, but for a GIMPLE_TRY_CATCH.  */

static bool
gimple_try_catch_may_fallthru (gimple stmt)
{
  gimple_stmt_iterator i;

  /* We don't handle GIMPLE_TRY_FINALLY.  */
  gcc_assert (gimple_try_kind (stmt) == GIMPLE_TRY_CATCH);

  /* If the TRY block can fall through, the whole TRY_CATCH can
     fall through.  */
  if (gimple_seq_may_fallthru (gimple_try_eval (stmt)))
    return true;

  i = gsi_start (gimple_try_cleanup (stmt));
  switch (gimple_code (gsi_stmt (i)))
    {
    case GIMPLE_CATCH:
      /* We expect to see a sequence of GIMPLE_CATCH stmts, each with a
	 catch expression and a body.  The whole try/catch may fall
	 through iff any of the catch bodies falls through.  */
      for (; !gsi_end_p (i); gsi_next (&i))
	{
	  if (gimple_seq_may_fallthru (gimple_catch_handler (gsi_stmt (i))))
	    return true;
	}
      return false;

    case GIMPLE_EH_FILTER:
      /* The exception filter expression only matters if there is an
	 exception.  If the exception does not match EH_FILTER_TYPES,
	 we will execute EH_FILTER_FAILURE, and we will fall through
	 if that falls through.  If the exception does match
	 EH_FILTER_TYPES, the stack unwinder will continue up the
	 stack, so we will not fall through.  We don't know whether we
	 will throw an exception which matches EH_FILTER_TYPES or not,
	 so we just ignore EH_FILTER_TYPES and assume that we might
	 throw an exception which doesn't match.  */
      return gimple_seq_may_fallthru (gimple_eh_filter_failure (gsi_stmt (i)));

    default:
      /* This case represents statements to be executed when an
	 exception occurs.  Those statements are implicitly followed
	 by a GIMPLE_RESX to resume execution after the exception.  So
	 in this case the try/catch never falls through.  */
      return false;
    }
}


/* Try to determine if we can fall out of the bottom of BLOCK.  This guess
   need not be 100% accurate; simply be conservative and return true if we
   don't know.  This is used only to avoid stupidly generating extra code.
   If we're wrong, we'll just delete the extra code later.  */

bool
block_may_fallthru (const_tree block)
{
  /* This CONST_CAST is okay because expr_last returns its argument
     unmodified and we assign it to a const_tree.  */
  const_tree stmt = expr_last (CONST_CAST_TREE(block));

  switch (stmt ? TREE_CODE (stmt) : ERROR_MARK)
    {
    case GOTO_EXPR:
    case RETURN_EXPR:
      /* Easy cases.  If the last statement of the block implies 
	 control transfer, then we can't fall through.  */
      return false;

    case SWITCH_EXPR:
      /* If SWITCH_LABELS is set, this is lowered, and represents a
	 branch to a selected label and hence can not fall through.
	 Otherwise SWITCH_BODY is set, and the switch can fall
	 through.  */
      return SWITCH_LABELS (stmt) == NULL_TREE;

    case COND_EXPR:
      if (block_may_fallthru (COND_EXPR_THEN (stmt)))
	return true;
      return block_may_fallthru (COND_EXPR_ELSE (stmt));

    case BIND_EXPR:
      return block_may_fallthru (BIND_EXPR_BODY (stmt));

    case TRY_CATCH_EXPR:
      return try_catch_may_fallthru (stmt);

    case TRY_FINALLY_EXPR:
      /* The finally clause is always executed after the try clause,
	 so if it does not fall through, then the try-finally will not
	 fall through.  Otherwise, if the try clause does not fall
	 through, then when the finally clause falls through it will
	 resume execution wherever the try clause was going.  So the
	 whole try-finally will only fall through if both the try
	 clause and the finally clause fall through.  */
      return (block_may_fallthru (TREE_OPERAND (stmt, 0))
	      && block_may_fallthru (TREE_OPERAND (stmt, 1)));

    case MODIFY_EXPR:
      if (TREE_CODE (TREE_OPERAND (stmt, 1)) == CALL_EXPR)
	stmt = TREE_OPERAND (stmt, 1);
      else
	return true;
      /* FALLTHRU */

    case CALL_EXPR:
      /* Functions that do not return do not fall through.  */
      return (call_expr_flags (stmt) & ECF_NORETURN) == 0;
    
    case CLEANUP_POINT_EXPR:
      return block_may_fallthru (TREE_OPERAND (stmt, 0));

    default:
      return true;
    }
}


/* Try to determine if we can continue executing the statement
   immediately following STMT.  This guess need not be 100% accurate;
   simply be conservative and return true if we don't know.  This is
   used only to avoid stupidly generating extra code. If we're wrong,
   we'll just delete the extra code later.  */

bool
gimple_stmt_may_fallthru (gimple stmt)
{
  if (!stmt)
    return true;

  switch (gimple_code (stmt))
    {
    case GIMPLE_GOTO:
    case GIMPLE_RETURN:
    case GIMPLE_RESX:
      /* Easy cases.  If the last statement of the seq implies 
	 control transfer, then we can't fall through.  */
      return false;

    case GIMPLE_SWITCH:
      /* Switch has already been lowered and represents a
	 branch to a selected label and hence can not fall through.  */
      return true;

    case GIMPLE_COND:
      /* GIMPLE_COND's are already lowered into a two-way branch.  They
	 can't fall through.  */
      return false;

    case GIMPLE_BIND:
      return gimple_seq_may_fallthru (gimple_bind_body (stmt));

    case GIMPLE_TRY:
      if (gimple_try_kind (stmt) == GIMPLE_TRY_CATCH)
        return gimple_try_catch_may_fallthru (stmt);

      /* It must be a GIMPLE_TRY_FINALLY.  */

      /* The finally clause is always executed after the try clause,
	 so if it does not fall through, then the try-finally will not
	 fall through.  Otherwise, if the try clause does not fall
	 through, then when the finally clause falls through it will
	 resume execution wherever the try clause was going.  So the
	 whole try-finally will only fall through if both the try
	 clause and the finally clause fall through.  */
      return (gimple_seq_may_fallthru (gimple_try_eval (stmt))
	      && gimple_seq_may_fallthru (gimple_try_cleanup (stmt)));

    case GIMPLE_ASSIGN:
      return true;

    case GIMPLE_CALL:
      /* Functions that do not return do not fall through.  */
      return (gimple_call_flags (stmt) & ECF_NORETURN) == 0;
    
    default:
      return true;
    }
}


/* Same as gimple_stmt_may_fallthru, but for the gimple sequence SEQ.  */

bool
gimple_seq_may_fallthru (gimple_seq seq)
{
  return gimple_stmt_may_fallthru (gimple_seq_last_stmt (seq));
}


/* Lower a GIMPLE_RETURN GSI.  DATA is passed through the recursion.  */

static void
lower_gimple_return (gimple_stmt_iterator *gsi, struct lower_data *data)
{
  gimple stmt = gsi_stmt (*gsi);
  gimple t;
  int i;
  return_statements_t tmp_rs;

  /* Match this up with an existing return statement that's been created.  */
  for (i = VEC_length (return_statements_t, data->return_statements) - 1;
       i >= 0; i--)
    {
      tmp_rs = *VEC_index (return_statements_t, data->return_statements, i);

      if (gimple_return_retval (stmt) == gimple_return_retval (tmp_rs.stmt))
	goto found;
    }

  /* Not found.  Create a new label and record the return statement.  */
  tmp_rs.label = create_artificial_label (cfun->function_end_locus);
  tmp_rs.stmt = stmt;
  VEC_safe_push (return_statements_t, heap, data->return_statements, &tmp_rs);

  /* Generate a goto statement and remove the return statement.  */
 found:
  t = gimple_build_goto (tmp_rs.label);
  gimple_set_location (t, gimple_location (stmt));
  gimple_set_block (t, gimple_block (stmt));
  gsi_insert_before (gsi, t, GSI_SAME_STMT);
  gsi_remove (gsi, false);
}

/* Lower a __builtin_setjmp TSI.

   __builtin_setjmp is passed a pointer to an array of five words (not
   all will be used on all machines).  It operates similarly to the C
   library function of the same name, but is more efficient.

   It is lowered into 3 other builtins, namely __builtin_setjmp_setup,
   __builtin_setjmp_dispatcher and __builtin_setjmp_receiver, but with
   __builtin_setjmp_dispatcher shared among all the instances; that's
   why it is only emitted at the end by lower_function_body.

   After full lowering, the body of the function should look like:

    {
      void * setjmpvar.0;
      int D.1844;
      int D.2844;

      [...]

      __builtin_setjmp_setup (&buf, &<D1847>);
      D.1844 = 0;
      goto <D1846>;
      <D1847>:;
      __builtin_setjmp_receiver (&<D1847>);
      D.1844 = 1;
      <D1846>:;
      if (D.1844 == 0) goto <D1848>; else goto <D1849>;

      [...]

      __builtin_setjmp_setup (&buf, &<D2847>);
      D.2844 = 0;
      goto <D2846>;
      <D2847>:;
      __builtin_setjmp_receiver (&<D2847>);
      D.2844 = 1;
      <D2846>:;
      if (D.2844 == 0) goto <D2848>; else goto <D2849>;

      [...]

      <D3850>:;
      return;
      <D3853>: [non-local];
      setjmpvar.0 = __builtin_setjmp_dispatcher (&<D3853>);
      goto setjmpvar.0;
    }

   The dispatcher block will be both the unique destination of all the
   abnormal call edges and the unique source of all the abnormal edges
   to the receivers, thus keeping the complexity explosion localized.  */

static void
lower_builtin_setjmp (gimple_stmt_iterator *gsi)
{
  gimple stmt = gsi_stmt (*gsi);
  location_t loc = gimple_location (stmt);
  tree cont_label = create_artificial_label (loc);
  tree next_label = create_artificial_label (loc);
  tree dest, t, arg;
  gimple g;

  /* NEXT_LABEL is the label __builtin_longjmp will jump to.  Its address is
     passed to both __builtin_setjmp_setup and __builtin_setjmp_receiver.  */
  FORCED_LABEL (next_label) = 1;

  dest = gimple_call_lhs (stmt);

  /* Build '__builtin_setjmp_setup (BUF, NEXT_LABEL)' and insert.  */
  arg = build_addr (next_label, current_function_decl);
  t = implicit_built_in_decls[BUILT_IN_SETJMP_SETUP];
  g = gimple_build_call (t, 2, gimple_call_arg (stmt, 0), arg);
  gimple_set_location (g, loc);
  gimple_set_block (g, gimple_block (stmt));
  gsi_insert_before (gsi, g, GSI_SAME_STMT);

  /* Build 'DEST = 0' and insert.  */
  if (dest)
    {
      g = gimple_build_assign (dest, fold_convert_loc (loc, TREE_TYPE (dest),
						       integer_zero_node));
      gimple_set_location (g, loc);
      gimple_set_block (g, gimple_block (stmt));
      gsi_insert_before (gsi, g, GSI_SAME_STMT);
    }

  /* Build 'goto CONT_LABEL' and insert.  */
  g = gimple_build_goto (cont_label);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);

  /* Build 'NEXT_LABEL:' and insert.  */
  g = gimple_build_label (next_label);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);

  /* Build '__builtin_setjmp_receiver (NEXT_LABEL)' and insert.  */
  arg = build_addr (next_label, current_function_decl);
  t = implicit_built_in_decls[BUILT_IN_SETJMP_RECEIVER];
  g = gimple_build_call (t, 1, arg);
  gimple_set_location (g, loc);
  gimple_set_block (g, gimple_block (stmt));
  gsi_insert_before (gsi, g, GSI_SAME_STMT);

  /* Build 'DEST = 1' and insert.  */
  if (dest)
    {
      g = gimple_build_assign (dest, fold_convert_loc (loc, TREE_TYPE (dest),
						       integer_one_node));
      gimple_set_location (g, loc);
      gimple_set_block (g, gimple_block (stmt));
      gsi_insert_before (gsi, g, GSI_SAME_STMT);
    }

  /* Build 'CONT_LABEL:' and insert.  */
  g = gimple_build_label (cont_label);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);

  /* Remove the call to __builtin_setjmp.  */
  gsi_remove (gsi, false);
}


/* Record the variables in VARS into function FN.  */

void
record_vars_into (tree vars, tree fn)
{
  if (fn != current_function_decl)
    push_cfun (DECL_STRUCT_FUNCTION (fn));

  for (; vars; vars = TREE_CHAIN (vars))
    {
      tree var = vars;

      /* BIND_EXPRs contains also function/type/constant declarations
         we don't need to care about.  */
      if (TREE_CODE (var) != VAR_DECL)
	continue;

      /* Nothing to do in this case.  */
      if (DECL_EXTERNAL (var))
	continue;

      /* Record the variable.  */
      cfun->local_decls = tree_cons (NULL_TREE, var,
					     cfun->local_decls);
    }

  if (fn != current_function_decl)
    pop_cfun ();
}


/* Record the variables in VARS into current_function_decl.  */

void
record_vars (tree vars)
{
  record_vars_into (vars, current_function_decl);
}
