/* GIMPLE lowering pass.  Converts High GIMPLE into Low GIMPLE.

   Copyright (C) 2003-2017 Free Software Foundation, Inc.

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
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "fold-const.h"
#include "tree-nested.h"
#include "calls.h"
#include "gimple-iterator.h"
#include "gimple-low.h"
#include "stor-layout.h"
#include "target.h"
#include "gimplify.h"

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
  greturn *stmt;
};
typedef struct return_statements_t return_statements_t;


struct lower_data
{
  /* Block the current statement belongs to.  */
  tree block;

  /* A vector of label and return statements to be moved to the end
     of the function.  */
  vec<return_statements_t> return_statements;

  /* True if the current statement cannot fall through.  */
  bool cannot_fallthru;
};

static void lower_stmt (gimple_stmt_iterator *, struct lower_data *);
static void lower_gimple_bind (gimple_stmt_iterator *, struct lower_data *);
static void lower_try_catch (gimple_stmt_iterator *, struct lower_data *);
static void lower_gimple_return (gimple_stmt_iterator *, struct lower_data *);
static void lower_builtin_setjmp (gimple_stmt_iterator *);
static void lower_builtin_fpclassify (gimple_stmt_iterator *);
static void lower_builtin_isnan (gimple_stmt_iterator *);
static void lower_builtin_isinfinite (gimple_stmt_iterator *);
static void lower_builtin_isnormal (gimple_stmt_iterator *);
static void lower_builtin_iszero (gimple_stmt_iterator *);
static void lower_builtin_issubnormal (gimple_stmt_iterator *);
static void lower_builtin_isfinite (gimple_stmt_iterator *);
static void lower_builtin_posix_memalign (gimple_stmt_iterator *);


/* Lower the body of current_function_decl from High GIMPLE into Low
   GIMPLE.  */

static unsigned int
lower_function_body (void)
{
  struct lower_data data;
  gimple_seq body = gimple_body (current_function_decl);
  gimple_seq lowered_body;
  gimple_stmt_iterator i;
  gimple *bind;
  gimple *x;

  /* The gimplifier should've left a body of exactly one statement,
     namely a GIMPLE_BIND.  */
  gcc_assert (gimple_seq_first (body) == gimple_seq_last (body)
	      && gimple_code (gimple_seq_first_stmt (body)) == GIMPLE_BIND);

  memset (&data, 0, sizeof (data));
  data.block = DECL_INITIAL (current_function_decl);
  BLOCK_SUBBLOCKS (data.block) = NULL_TREE;
  BLOCK_CHAIN (data.block) = NULL_TREE;
  TREE_ASM_WRITTEN (data.block) = 1;
  data.return_statements.create (8);

  bind = gimple_seq_first_stmt (body);
  lowered_body = NULL;
  gimple_seq_add_stmt (&lowered_body, bind);
  i = gsi_start (lowered_body);
  lower_gimple_bind (&i, &data);

  i = gsi_last (lowered_body);

  /* If the function falls off the end, we need a null return statement.
     If we've already got one in the return_statements vector, we don't
     need to do anything special.  Otherwise build one by hand.  */
  bool may_fallthru = gimple_seq_may_fallthru (lowered_body);
  if (may_fallthru
      && (data.return_statements.is_empty ()
	  || (gimple_return_retval (data.return_statements.last().stmt)
	      != NULL)))
    {
      x = gimple_build_return (NULL);
      gimple_set_location (x, cfun->function_end_locus);
      gimple_set_block (x, DECL_INITIAL (current_function_decl));
      gsi_insert_after (&i, x, GSI_CONTINUE_LINKING);
      may_fallthru = false;
    }

  /* If we lowered any return statements, emit the representative
     at the end of the function.  */
  while (!data.return_statements.is_empty ())
    {
      return_statements_t t = data.return_statements.pop ();
      x = gimple_build_label (t.label);
      gsi_insert_after (&i, x, GSI_CONTINUE_LINKING);
      gsi_insert_after (&i, t.stmt, GSI_CONTINUE_LINKING);
      if (may_fallthru)
	{
	  /* Remove the line number from the representative return statement.
	     It now fills in for the fallthru too.  Failure to remove this
	     will result in incorrect results for coverage analysis.  */
	  gimple_set_location (t.stmt, UNKNOWN_LOCATION);
	  may_fallthru = false;
	}
    }

  /* Once the old body has been lowered, replace it with the new
     lowered sequence.  */
  gimple_set_body (current_function_decl, lowered_body);

  gcc_assert (data.block == DECL_INITIAL (current_function_decl));
  BLOCK_SUBBLOCKS (data.block)
    = blocks_nreverse (BLOCK_SUBBLOCKS (data.block));

  clear_block_marks (data.block);
  data.return_statements.release ();
  return 0;
}

namespace {

const pass_data pass_data_lower_cf =
{
  GIMPLE_PASS, /* type */
  "lower", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_gimple_any, /* properties_required */
  PROP_gimple_lcf, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_lower_cf : public gimple_opt_pass
{
public:
  pass_lower_cf (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_lower_cf, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *) { return lower_function_body (); }

}; // class pass_lower_cf

} // anon namespace

gimple_opt_pass *
make_pass_lower_cf (gcc::context *ctxt)
{
  return new pass_lower_cf (ctxt);
}

/* Lower sequence SEQ.  Unlike gimplification the statements are not relowered
   when they are changed -- if this has to be done, the lowering routine must
   do it explicitly.  DATA is passed through the recursion.  */

static void
lower_sequence (gimple_seq *seq, struct lower_data *data)
{
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start (*seq); !gsi_end_p (gsi); )
    lower_stmt (&gsi, data);
}


/* Lower the OpenMP directive statement pointed by GSI.  DATA is
   passed through the recursion.  */

static void
lower_omp_directive (gimple_stmt_iterator *gsi, struct lower_data *data)
{
  gimple *stmt;

  stmt = gsi_stmt (*gsi);

  lower_sequence (gimple_omp_body_ptr (stmt), data);
  gsi_insert_seq_after (gsi, gimple_omp_body (stmt), GSI_CONTINUE_LINKING);
  gimple_omp_set_body (stmt, NULL);
  gsi_next (gsi);
}


/* Lower statement GSI.  DATA is passed through the recursion.  We try to
   track the fallthruness of statements and get rid of unreachable return
   statements in order to prevent the EH lowering pass from adding useless
   edges that can cause bogus warnings to be issued later; this guess need
   not be 100% accurate, simply be conservative and reset cannot_fallthru
   to false if we don't know.  */

static void
lower_stmt (gimple_stmt_iterator *gsi, struct lower_data *data)
{
  gimple *stmt = gsi_stmt (*gsi);

  gimple_set_block (stmt, data->block);

  switch (gimple_code (stmt))
    {
    case GIMPLE_BIND:
      lower_gimple_bind (gsi, data);
      /* Propagate fallthruness.  */
      return;

    case GIMPLE_COND:
    case GIMPLE_GOTO:
    case GIMPLE_SWITCH:
      data->cannot_fallthru = true;
      gsi_next (gsi);
      return;

    case GIMPLE_RETURN:
      if (data->cannot_fallthru)
	{
	  gsi_remove (gsi, false);
	  /* Propagate fallthruness.  */
	}
      else
	{
	  lower_gimple_return (gsi, data);
	  data->cannot_fallthru = true;
	}
      return;

    case GIMPLE_TRY:
      if (gimple_try_kind (stmt) == GIMPLE_TRY_CATCH)
	lower_try_catch (gsi, data);
      else
	{
	  /* It must be a GIMPLE_TRY_FINALLY.  */
	  bool cannot_fallthru;
	  lower_sequence (gimple_try_eval_ptr (stmt), data);
	  cannot_fallthru = data->cannot_fallthru;

	  /* The finally clause is always executed after the try clause,
	     so if it does not fall through, then the try-finally will not
	     fall through.  Otherwise, if the try clause does not fall
	     through, then when the finally clause falls through it will
	     resume execution wherever the try clause was going.  So the
	     whole try-finally will only fall through if both the try
	     clause and the finally clause fall through.  */
	  data->cannot_fallthru = false;
	  lower_sequence (gimple_try_cleanup_ptr (stmt), data);
	  data->cannot_fallthru |= cannot_fallthru;
	  gsi_next (gsi);
	}
      return;

    case GIMPLE_EH_ELSE:
      {
	geh_else *eh_else_stmt = as_a <geh_else *> (stmt);
	lower_sequence (gimple_eh_else_n_body_ptr (eh_else_stmt), data);
	lower_sequence (gimple_eh_else_e_body_ptr (eh_else_stmt), data);
      }
      break;

    case GIMPLE_NOP:
    case GIMPLE_ASM:
    case GIMPLE_ASSIGN:
    case GIMPLE_PREDICT:
    case GIMPLE_LABEL:
    case GIMPLE_EH_MUST_NOT_THROW:
    case GIMPLE_OMP_FOR:
    case GIMPLE_OMP_SECTIONS:
    case GIMPLE_OMP_SECTIONS_SWITCH:
    case GIMPLE_OMP_SECTION:
    case GIMPLE_OMP_SINGLE:
    case GIMPLE_OMP_MASTER:
    case GIMPLE_OMP_TASKGROUP:
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
	unsigned i;

	for (i = 0; i < gimple_call_num_args (stmt); i++)
	  {
	    tree arg = gimple_call_arg (stmt, i);
	    if (EXPR_P (arg))
	      TREE_SET_BLOCK (arg, data->block);
	  }

	if (decl
	    && DECL_BUILT_IN_CLASS (decl) == BUILT_IN_NORMAL)
	  {
	    switch (DECL_FUNCTION_CODE (decl))
	      {
	      case BUILT_IN_SETJMP:
		lower_builtin_setjmp (gsi);
		data->cannot_fallthru = false;
		return;

	      case BUILT_IN_POSIX_MEMALIGN:
		if (flag_tree_bit_ccp
		    && gimple_builtin_call_types_compatible_p (stmt, decl))
		  {
			lower_builtin_posix_memalign (gsi);
			return;
		  }
		break;

	      case BUILT_IN_FPCLASSIFY:
		lower_builtin_fpclassify (gsi);
		data->cannot_fallthru = false;
		return;

	      CASE_FLT_FN (BUILT_IN_ISINF):
	      case BUILT_IN_ISINFD32:
	      case BUILT_IN_ISINFD64:
	      case BUILT_IN_ISINFD128:
		lower_builtin_isinfinite (gsi);
		data->cannot_fallthru = false;
		return;

	      case BUILT_IN_ISNAND32:
	      case BUILT_IN_ISNAND64:
	      case BUILT_IN_ISNAND128:
	      CASE_FLT_FN (BUILT_IN_ISNAN):
		lower_builtin_isnan (gsi);
		data->cannot_fallthru = false;
		return;

	      case BUILT_IN_ISNORMAL:
		lower_builtin_isnormal (gsi);
		data->cannot_fallthru = false;
		return;

	      case BUILT_IN_ISZERO:
		lower_builtin_iszero (gsi);
		data->cannot_fallthru = false;
		return;

	      case BUILT_IN_ISSUBNORMAL:
		lower_builtin_issubnormal (gsi);
		data->cannot_fallthru = false;
		return;

	      CASE_FLT_FN (BUILT_IN_FINITE):
	      case BUILT_IN_FINITED32:
	      case BUILT_IN_FINITED64:
	      case BUILT_IN_FINITED128:
	      case BUILT_IN_ISFINITE:
		lower_builtin_isfinite (gsi);
		data->cannot_fallthru = false;
		return;

	      default:
		break;
	      }
	  }

	if (decl && (flags_from_decl_or_type (decl) & ECF_NORETURN))
	  {
	    data->cannot_fallthru = true;
	    gsi_next (gsi);
	    return;
	  }
      }
      break;

    case GIMPLE_OMP_PARALLEL:
    case GIMPLE_OMP_TASK:
    case GIMPLE_OMP_TARGET:
    case GIMPLE_OMP_TEAMS:
    case GIMPLE_OMP_GRID_BODY:
      data->cannot_fallthru = false;
      lower_omp_directive (gsi, data);
      data->cannot_fallthru = false;
      return;

    case GIMPLE_TRANSACTION:
      lower_sequence (gimple_transaction_body_ptr (
			as_a <gtransaction *> (stmt)),
		      data);
      break;

    default:
      gcc_unreachable ();
    }

  data->cannot_fallthru = false;
  gsi_next (gsi);
}

/* Lower a bind_expr TSI.  DATA is passed through the recursion.  */

static void
lower_gimple_bind (gimple_stmt_iterator *gsi, struct lower_data *data)
{
  tree old_block = data->block;
  gbind *stmt = as_a <gbind *> (gsi_stmt (*gsi));
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

  /* Scrap DECL_CHAIN up to BLOCK_VARS to ease GC after we no longer
     need gimple_bind_vars.  */
  tree next;
  /* BLOCK_VARS and gimple_bind_vars share a common sub-chain.  Find
     it by marking all BLOCK_VARS.  */
  if (gimple_bind_block (stmt))
    for (tree t = BLOCK_VARS (gimple_bind_block (stmt)); t; t = DECL_CHAIN (t))
      TREE_VISITED (t) = 1;
  for (tree var = gimple_bind_vars (stmt);
       var && ! TREE_VISITED (var); var = next)
    {
      next = DECL_CHAIN (var);
      DECL_CHAIN (var) = NULL_TREE;
    }
  /* Unmark BLOCK_VARS.  */
  if (gimple_bind_block (stmt))
    for (tree t = BLOCK_VARS (gimple_bind_block (stmt)); t; t = DECL_CHAIN (t))
      TREE_VISITED (t) = 0;

  lower_sequence (gimple_bind_body_ptr (stmt), data);

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

/* Same as above, but for a GIMPLE_TRY_CATCH.  */

static void
lower_try_catch (gimple_stmt_iterator *gsi, struct lower_data *data)
{
  bool cannot_fallthru;
  gimple *stmt = gsi_stmt (*gsi);
  gimple_stmt_iterator i;

  /* We don't handle GIMPLE_TRY_FINALLY.  */
  gcc_assert (gimple_try_kind (stmt) == GIMPLE_TRY_CATCH);

  lower_sequence (gimple_try_eval_ptr (stmt), data);
  cannot_fallthru = data->cannot_fallthru;

  i = gsi_start (*gimple_try_cleanup_ptr (stmt));
  switch (gimple_code (gsi_stmt (i)))
    {
    case GIMPLE_CATCH:
      /* We expect to see a sequence of GIMPLE_CATCH stmts, each with a
	 catch expression and a body.  The whole try/catch may fall
	 through iff any of the catch bodies falls through.  */
      for (; !gsi_end_p (i); gsi_next (&i))
	{
	  data->cannot_fallthru = false;
	  lower_sequence (gimple_catch_handler_ptr (
                            as_a <gcatch *> (gsi_stmt (i))),
			  data);
	  if (!data->cannot_fallthru)
	    cannot_fallthru = false;
	}
      break;

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
      data->cannot_fallthru = false;
      lower_sequence (gimple_eh_filter_failure_ptr (gsi_stmt (i)), data);
      if (!data->cannot_fallthru)
	cannot_fallthru = false;
      break;

    default:
      /* This case represents statements to be executed when an
	 exception occurs.  Those statements are implicitly followed
	 by a GIMPLE_RESX to resume execution after the exception.  So
	 in this case the try/catch never falls through.  */
      data->cannot_fallthru = false;
      lower_sequence (gimple_try_cleanup_ptr (stmt), data);
      break;
    }

  data->cannot_fallthru = cannot_fallthru;
  gsi_next (gsi);
}


/* Try to determine whether a TRY_CATCH expression can fall through.
   This is a subroutine of gimple_stmt_may_fallthru.  */

static bool
gimple_try_catch_may_fallthru (gtry *stmt)
{
  gimple_stmt_iterator i;

  /* We don't handle GIMPLE_TRY_FINALLY.  */
  gcc_assert (gimple_try_kind (stmt) == GIMPLE_TRY_CATCH);

  /* If the TRY block can fall through, the whole TRY_CATCH can
     fall through.  */
  if (gimple_seq_may_fallthru (gimple_try_eval (stmt)))
    return true;

  i = gsi_start (*gimple_try_cleanup_ptr (stmt));
  switch (gimple_code (gsi_stmt (i)))
    {
    case GIMPLE_CATCH:
      /* We expect to see a sequence of GIMPLE_CATCH stmts, each with a
	 catch expression and a body.  The whole try/catch may fall
	 through iff any of the catch bodies falls through.  */
      for (; !gsi_end_p (i); gsi_next (&i))
	{
	  if (gimple_seq_may_fallthru (gimple_catch_handler (
					 as_a <gcatch *> (gsi_stmt (i)))))
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


/* Try to determine if we can continue executing the statement
   immediately following STMT.  This guess need not be 100% accurate;
   simply be conservative and return true if we don't know.  This is
   used only to avoid stupidly generating extra code. If we're wrong,
   we'll just delete the extra code later.  */

bool
gimple_stmt_may_fallthru (gimple *stmt)
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
      /* Switch has already been lowered and represents a branch
	 to a selected label and hence can't fall through.  */
      return false;

    case GIMPLE_COND:
      /* GIMPLE_COND's are already lowered into a two-way branch.  They
	 can't fall through.  */
      return false;

    case GIMPLE_BIND:
      return gimple_seq_may_fallthru (
	       gimple_bind_body (as_a <gbind *> (stmt)));

    case GIMPLE_TRY:
      if (gimple_try_kind (stmt) == GIMPLE_TRY_CATCH)
        return gimple_try_catch_may_fallthru (as_a <gtry *> (stmt));

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

    case GIMPLE_EH_ELSE:
      {
	geh_else *eh_else_stmt = as_a <geh_else *> (stmt);
	return (gimple_seq_may_fallthru (gimple_eh_else_n_body (eh_else_stmt))
		|| gimple_seq_may_fallthru (gimple_eh_else_e_body (
					      eh_else_stmt)));
      }

    case GIMPLE_CALL:
      /* Functions that do not return do not fall through.  */
      return !gimple_call_noreturn_p (stmt);

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
  greturn *stmt = as_a <greturn *> (gsi_stmt (*gsi));
  gimple *t;
  int i;
  return_statements_t tmp_rs;

  /* Match this up with an existing return statement that's been created.  */
  for (i = data->return_statements.length () - 1;
       i >= 0; i--)
    {
      tmp_rs = data->return_statements[i];

      if (gimple_return_retval (stmt) == gimple_return_retval (tmp_rs.stmt))
	{
	  /* Remove the line number from the representative return statement.
	     It now fills in for many such returns.  Failure to remove this
	     will result in incorrect results for coverage analysis.  */
	  gimple_set_location (tmp_rs.stmt, UNKNOWN_LOCATION);

	  goto found;
	}
    }

  /* Not found.  Create a new label and record the return statement.  */
  tmp_rs.label = create_artificial_label (cfun->function_end_locus);
  tmp_rs.stmt = stmt;
  data->return_statements.safe_push (tmp_rs);

  /* Generate a goto statement and remove the return statement.  */
 found:
  /* When not optimizing, make sure user returns are preserved.  */
  if (!optimize && gimple_has_location (stmt))
    DECL_ARTIFICIAL (tmp_rs.label) = 0;
  t = gimple_build_goto (tmp_rs.label);
  gimple_set_location (t, gimple_location (stmt));
  gimple_set_block (t, gimple_block (stmt));
  gsi_insert_before (gsi, t, GSI_SAME_STMT);
  gsi_remove (gsi, false);
}

/* Lower a __builtin_setjmp GSI.

   __builtin_setjmp is passed a pointer to an array of five words (not
   all will be used on all machines).  It operates similarly to the C
   library function of the same name, but is more efficient.

   It is lowered into 2 other builtins, namely __builtin_setjmp_setup,
   __builtin_setjmp_receiver.

   After full lowering, the body of the function should look like:

    {
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
    }

   During cfg creation an extra per-function (or per-OpenMP region)
   block with ABNORMAL_DISPATCHER internal call will be added, unique
   destination of all the abnormal call edges and the unique source of
   all the abnormal edges to the receivers, thus keeping the complexity
   explosion localized.  */

static void
lower_builtin_setjmp (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  location_t loc = gimple_location (stmt);
  tree cont_label = create_artificial_label (loc);
  tree next_label = create_artificial_label (loc);
  tree dest, t, arg;
  gimple *g;

  /* __builtin_setjmp_{setup,receiver} aren't ECF_RETURNS_TWICE and for RTL
     these builtins are modelled as non-local label jumps to the label
     that is passed to these two builtins, so pretend we have a non-local
     label during GIMPLE passes too.  See PR60003.  */ 
  cfun->has_nonlocal_label = 1;

  /* NEXT_LABEL is the label __builtin_longjmp will jump to.  Its address is
     passed to both __builtin_setjmp_setup and __builtin_setjmp_receiver.  */
  FORCED_LABEL (next_label) = 1;

  tree orig_dest = dest = gimple_call_lhs (stmt);
  if (orig_dest && TREE_CODE (orig_dest) == SSA_NAME)
    dest = create_tmp_reg (TREE_TYPE (orig_dest));

  /* Build '__builtin_setjmp_setup (BUF, NEXT_LABEL)' and insert.  */
  arg = build_addr (next_label);
  t = builtin_decl_implicit (BUILT_IN_SETJMP_SETUP);
  g = gimple_build_call (t, 2, gimple_call_arg (stmt, 0), arg);
  gimple_set_location (g, loc);
  gimple_set_block (g, gimple_block (stmt));
  gsi_insert_before (gsi, g, GSI_SAME_STMT);

  /* Build 'DEST = 0' and insert.  */
  if (dest)
    {
      g = gimple_build_assign (dest, build_zero_cst (TREE_TYPE (dest)));
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
  arg = build_addr (next_label);
  t = builtin_decl_implicit (BUILT_IN_SETJMP_RECEIVER);
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

  /* Build orig_dest = dest if necessary.  */
  if (dest != orig_dest)
    {
      g = gimple_build_assign (orig_dest, dest);
      gsi_insert_before (gsi, g, GSI_SAME_STMT);
    }

  /* Remove the call to __builtin_setjmp.  */
  gsi_remove (gsi, false);
}

/* This function will if ARG is not already a variable or SSA_NAME,
   create a new temporary TMP and bind ARG to TMP.  This new binding is then
   emitted into SEQ and TMP is returned.  */
static tree
emit_tree_and_return_var (gimple_seq *seq, tree arg)
{
  if (TREE_CODE (arg) == SSA_NAME || VAR_P (arg))
    return arg;

  tree tmp = create_tmp_reg (TREE_TYPE (arg));
  gassign *stm = gimple_build_assign (tmp, arg);
  gimple_seq_add_stmt (seq, stm);
  return tmp;
}

/* This function builds an if statement that ends up using explicit branches
   instead of becoming a ternary conditional select.  This function assumes you
   will fall through to the next statements after the condition for the false
   branch.  The code emitted looks like:

   if (COND)
     RESULT_VARIABLE = TRUE_BRANCH
     GOTO EXIT_LABEL
   else
     ...

   SEQ is the gimple sequence/buffer to emit any new bindings to.
   RESULT_VARIABLE is the value to set if COND.
   EXIT_LABEL is the label to jump to in case COND.
   COND is condition to use in the conditional statement of the if.
   TRUE_BRANCH is the value to set RESULT_VARIABLE to if COND.  */
static void
emit_tree_cond (gimple_seq *seq, tree result_variable, tree exit_label,
		tree cond, tree true_branch)
{
  /* Create labels for fall through.  */
  tree true_label = create_artificial_label (UNKNOWN_LOCATION);
  tree false_label = create_artificial_label (UNKNOWN_LOCATION);
  gcond *stmt = gimple_build_cond_from_tree (cond, true_label, false_label);
  gimple_seq_add_stmt (seq, stmt);

  /* Build the true case.  */
  gimple_seq_add_stmt (seq, gimple_build_label (true_label));
  tree value = TREE_CONSTANT (true_branch)
	     ? true_branch
	     : emit_tree_and_return_var (seq, true_branch);
  gimple_seq_add_stmt (seq, gimple_build_assign (result_variable, value));
  gimple_seq_add_stmt (seq, gimple_build_goto (exit_label));

  /* Build the false case.  */
  gimple_seq_add_stmt (seq, gimple_build_label (false_label));
}

/* This function returns a variable containing an reinterpreted ARG as an
   integer.

   SEQ is the gimple sequence/buffer to write any new bindings to.
   ARG is the floating point number to reinterpret as an integer.
   LOC is the location to use when doing folding operations.  */
static tree
get_num_as_int (gimple_seq *seq, tree arg, location_t loc)
{
  tree type = TREE_TYPE (arg);

  const HOST_WIDE_INT type_width = TYPE_PRECISION (type);

  /* Re-interpret the float as an unsigned integer type
     with equal precision.  */
  tree int_arg_type = build_nonstandard_integer_type (type_width, true);
  tree conv_arg = fold_build1_loc (loc, VIEW_CONVERT_EXPR, int_arg_type, arg);
  return emit_tree_and_return_var (seq, conv_arg);
}

/* Check if ARG which is the floating point number being classified is close
   enough to IEEE 754 format to be able to go in the early exit code.  */
static bool
use_ieee_int_mode (tree arg)
{
  tree type = TREE_TYPE (arg);
  machine_mode mode = TYPE_MODE (type);

  const real_format *format = REAL_MODE_FORMAT (mode);
  machine_mode imode = int_mode_for_mode (mode);
  bool is_ibm_extended = MODE_COMPOSITE_P (mode);

  return (format->is_binary_ieee_compatible
	  && FLOAT_WORDS_BIG_ENDIAN == WORDS_BIG_ENDIAN
	  /* Check if there's a usable integer mode.  */
	  && imode != BLKmode
	  && targetm.scalar_mode_supported_p (imode)
	  && !is_ibm_extended);
}

/* Perform some IBM extended format fixups on ARG for use by FP functions.
   This is done by ignoring the lower 64 bits of the number.

   MODE is the machine mode of ARG.
   TYPE is the type of ARG.
   LOC is the location to be used in fold functions.  Usually is the location
   of the definition of ARG.  */
static bool
perform_ibm_extended_fixups (tree *arg, machine_mode *mode,
			     tree *type, location_t loc)
{
  bool is_ibm_extended = MODE_COMPOSITE_P (*mode);
  if (is_ibm_extended)
    {
      /* NaN and Inf are encoded in the high-order double value
	 only.  The low-order value is not significant.  */
      *type = double_type_node;
      *mode = DFmode;
      *arg = fold_build1_loc (loc, NOP_EXPR, *type, *arg);
    }

  return is_ibm_extended;
}

/* Generates code to check if ARG is a normal number.  For the FP case we check
   MIN_VALUE(ARG) <= ABS(ARG) > INF and for the INT value we check the exp and
   mantissa bits.  Returns a variable containing a boolean which has the result
   of the check.

   SEQ is the buffer to use to emit the gimple instructions into.
   LOC is the location to use during fold calls.  */
static tree
is_normal (gimple_seq *seq, tree arg, location_t loc)
{
  tree type = TREE_TYPE (arg);

  machine_mode mode = TYPE_MODE (type);
  const real_format *format = REAL_MODE_FORMAT (mode);
  const tree bool_type = boolean_type_node;


  /* If not using optimized route then exit early.  */
  if (!use_ieee_int_mode (arg))
    {
      tree orig_arg = arg;
      machine_mode orig_mode = mode;
      if (TREE_CODE (arg) != SSA_NAME
	  && (TREE_ADDRESSABLE (arg) != 0
	    || (TREE_CODE (arg) != PARM_DECL
	        && (!VAR_P (arg) || TREE_STATIC (arg)))))
	orig_arg = save_expr (arg);

      /* Perform IBM extended format fixups if required.  */
      bool is_ibm_extended = perform_ibm_extended_fixups (&arg, &mode,
							  &type, loc);

      REAL_VALUE_TYPE rinf, rmin;
      tree arg_p = fold_build1_loc (loc, ABS_EXPR, type, arg);

      tree const islt_fn = builtin_decl_explicit (BUILT_IN_ISLESS);
      tree const isgt_fn = builtin_decl_explicit (BUILT_IN_ISGREATER);
      tree const isge_fn = builtin_decl_explicit (BUILT_IN_ISGREATEREQUAL);

      char buf[128];
      real_inf (&rinf);
      get_min_float (REAL_MODE_FORMAT (orig_mode), buf, sizeof (buf));
      real_from_string (&rmin, buf);

      tree inf_exp = build_call_expr (islt_fn, 2, arg_p,
				      build_real (type, rinf));
      tree min_exp = build_real (type, rmin);
      if (is_ibm_extended)
	{
	  /* Testing the high end of the range is done just using
	     the high double, using the same test as isfinite().
	     For the subnormal end of the range we first test the
	     high double, then if its magnitude is equal to the
	     limit of 0x1p-969, we test whether the low double is
	     non-zero and opposite sign to the high double.  */
	  tree gt_min = build_call_expr (isgt_fn, 2, arg_p, min_exp);
	  tree eq_min = fold_build2 (EQ_EXPR, integer_type_node,
				     arg_p, min_exp);
	  tree as_complex = build1 (VIEW_CONVERT_EXPR,
				    complex_double_type_node, orig_arg);
	  tree hi_dbl = build1 (REALPART_EXPR, type, as_complex);
	  tree lo_dbl = build1 (IMAGPART_EXPR, type, as_complex);
	  tree zero = build_real (type, dconst0);
	  tree hilt = build_call_expr (islt_fn, 2, hi_dbl, zero);
	  tree lolt = build_call_expr (islt_fn, 2, lo_dbl, zero);
	  tree logt = build_call_expr (isgt_fn, 2, lo_dbl, zero);
	  tree ok_lo = fold_build1 (TRUTH_NOT_EXPR, integer_type_node,
				    fold_build3 (COND_EXPR,
						 integer_type_node,
						 hilt, logt, lolt));
	  eq_min = fold_build2 (TRUTH_ANDIF_EXPR, integer_type_node,
				eq_min, ok_lo);
	  min_exp = fold_build2 (TRUTH_ORIF_EXPR, integer_type_node,
				 gt_min, eq_min);
	}
	else
	{
	  min_exp = build_call_expr (isge_fn, 2, arg_p, min_exp);
	}

      push_gimplify_context ();
      gimplify_expr (&min_exp, seq, NULL, is_gimple_val, fb_either);
      gimplify_expr (&inf_exp, seq, NULL, is_gimple_val, fb_either);

      tree res
	= fold_build2_loc (loc, BIT_AND_EXPR, bool_type,
			   emit_tree_and_return_var (seq,
						     gimple_boolify (min_exp)),
			   emit_tree_and_return_var (seq,
						     gimple_boolify (inf_exp)));
      pop_gimplify_context (NULL);

      return emit_tree_and_return_var (seq, res);
    }

  const tree int_type = unsigned_type_node;
  const int exp_bits  = (GET_MODE_SIZE (mode) * BITS_PER_UNIT) - format->p;
  const int exp_mask  = (1 << exp_bits) - 1;

  /* Get the number reinterpreted as an integer.  */
  tree int_arg = get_num_as_int (seq, arg, loc);

  /* Extract exp bits from the float, where we expect the exponent to be.
     We create a new type because BIT_FIELD_REF does not allow you to
     extract less bits than the precision of the storage variable.  */
  tree exp_tmp
    = fold_build3_loc (loc, BIT_FIELD_REF,
		       build_nonstandard_integer_type (exp_bits, true),
		       int_arg,
		       build_int_cstu (int_type, exp_bits),
		       build_int_cstu (int_type, format->p - 1));
  tree exp_bitfield = emit_tree_and_return_var (seq, exp_tmp);

  /* Re-interpret the extracted exponent bits as a 32 bit int.
     This allows us to continue doing operations as int_type.  */
  tree exp
    = emit_tree_and_return_var (seq, fold_build1_loc (loc, NOP_EXPR, int_type,
						      exp_bitfield));

  /* exp_mask & ~1.  */
  tree mask_check
     = fold_build2_loc (loc, BIT_AND_EXPR, int_type,
			build_int_cstu (int_type, exp_mask),
			fold_build1_loc (loc, BIT_NOT_EXPR, int_type,
					 build_int_cstu (int_type, 1)));

  /* (exp + 1) & mask_check.
     Check to see if exp is not all 0 or all 1.  */
  tree exp_check
    = fold_build2_loc (loc, BIT_AND_EXPR, int_type,
		       emit_tree_and_return_var (seq,
				fold_build2_loc (loc, PLUS_EXPR, int_type, exp,
						 build_int_cstu (int_type, 1))),
		       mask_check);

  tree res = fold_build2_loc (loc, NE_EXPR, boolean_type_node,
			      build_int_cstu (int_type, 0),
			      emit_tree_and_return_var (seq, exp_check));

  return emit_tree_and_return_var (seq, res);
}

/* Generates code to check if ARG is a zero. For both the FP and INT case we
   check if ARG == 0 (modulo sign bit).  Returns a variable containing a boolean
   which has the result of the check.

   SEQ is the buffer to use to emit the gimple instructions into.
   LOC is the location to use during fold calls.  */
static tree
is_zero (gimple_seq *seq, tree arg, location_t loc)
{
  tree type = TREE_TYPE (arg);

  /* If not using optimized route then exit early.  */
  if (!use_ieee_int_mode (arg))
    {
      machine_mode mode = TYPE_MODE (type);
      /* Perform IBM extended format fixups if required.  */
      perform_ibm_extended_fixups (&arg, &mode, &type, loc);

      tree res = fold_build2_loc (loc, EQ_EXPR, boolean_type_node, arg,
				  build_real (type, dconst0));
      return emit_tree_and_return_var (seq, res);
    }

  const HOST_WIDE_INT type_width = TYPE_PRECISION (type);

  tree int_arg_type = build_nonstandard_integer_type (type_width, true);

  /* Get the number reinterpreted as an integer.
     Shift left to remove the sign.  */
  tree int_arg
    = fold_build2_loc (loc, LSHIFT_EXPR, int_arg_type,
		       get_num_as_int (seq, arg, loc),
		       build_int_cstu (int_arg_type, 1));

  /* num << 1 == 0.
     This checks to see if the number is zero.  */
  tree zero_check
    = fold_build2_loc (loc, EQ_EXPR, boolean_type_node,
		       build_int_cstu (int_arg_type, 0),
		       emit_tree_and_return_var (seq, int_arg));

  return emit_tree_and_return_var (seq, zero_check);
}

/* Generates code to check if ARG is a subnormal number.  In the FP case we test
   fabs (ARG) != 0 && fabs (ARG) < MIN_VALUE (ARG) and in the INT case we check
   the exp and mantissa bits on ARG. Returns a variable containing a boolean
   which has the result of the check.

   SEQ is the buffer to use to emit the gimple instructions into.
   LOC is the location to use during fold calls.  */
static tree
is_subnormal (gimple_seq *seq, tree arg, location_t loc)
{
  const tree bool_type = boolean_type_node;

  tree type = TREE_TYPE (arg);

  machine_mode mode = TYPE_MODE (type);
  const real_format *format = REAL_MODE_FORMAT (mode);
  const HOST_WIDE_INT type_width = TYPE_PRECISION (type);

  tree int_arg_type = build_nonstandard_integer_type (type_width, true);

  /* If not using optimized route then exit early.  */
  if (!use_ieee_int_mode (arg))
    {
      tree const islt_fn = builtin_decl_explicit (BUILT_IN_ISLESS);
      tree const isgt_fn = builtin_decl_explicit (BUILT_IN_ISGREATER);

      tree arg_p
	= emit_tree_and_return_var (seq, fold_build1_loc (loc, ABS_EXPR, type,
							  arg));
      REAL_VALUE_TYPE r;
      char buf[128];
      get_min_float (REAL_MODE_FORMAT (mode), buf, sizeof (buf));
      real_from_string (&r, buf);
      tree subnorm = build_call_expr (islt_fn, 2, arg_p, build_real (type, r));

      tree zero = build_call_expr (isgt_fn, 2, arg_p,
				   build_real (type, dconst0));

      push_gimplify_context ();
      gimplify_expr (&subnorm, seq, NULL, is_gimple_val, fb_either);
      gimplify_expr (&zero, seq, NULL, is_gimple_val, fb_either);

      tree res
	= fold_build2_loc (loc, BIT_AND_EXPR, bool_type,
			   emit_tree_and_return_var (seq,
						     gimple_boolify (subnorm)),
			   emit_tree_and_return_var (seq,
						     gimple_boolify (zero)));
      pop_gimplify_context (NULL);

      return emit_tree_and_return_var (seq, res);
  }

  /* Get the number reinterpreted as an integer.
     Shift left to remove the sign.  */
  tree int_arg
    = fold_build2_loc (loc, LSHIFT_EXPR, int_arg_type,
		       get_num_as_int (seq, arg, loc),
		       build_int_cstu (int_arg_type, 1));

  /* Check for a zero exponent and non-zero mantissa.
     This can be done with two comparisons by first apply a
     removing the sign bit and checking if the value is larger
     than the mantissa mask.  */

  /* This creates a mask to be used to check the mantissa value in the shifted
     integer representation of the fpnum.  */
  tree significant_bit = build_int_cstu (int_arg_type, format->p - 1);
  tree mantissa_mask
    = fold_build2_loc (loc, MINUS_EXPR, int_arg_type,
		       fold_build2_loc (loc, LSHIFT_EXPR, int_arg_type,
					build_int_cstu (int_arg_type, 2),
					significant_bit),
		       build_int_cstu (int_arg_type, 1));

  /* Check if exponent is zero and mantissa is not.  */
  tree subnorm_cond_tmp
    = fold_build2_loc (loc, LE_EXPR, bool_type,
		       emit_tree_and_return_var (seq, int_arg),
		       mantissa_mask);

  tree subnorm_cond = emit_tree_and_return_var (seq, subnorm_cond_tmp);

  tree zero_cond
    = fold_build2_loc (loc, GT_EXPR, boolean_type_node,
		       emit_tree_and_return_var (seq, int_arg),
		       build_int_cstu (int_arg_type, 0));

  tree subnorm_check
    = fold_build2_loc (loc, BIT_AND_EXPR, boolean_type_node,
		       emit_tree_and_return_var (seq, subnorm_cond),
		       emit_tree_and_return_var (seq, zero_cond));

  return emit_tree_and_return_var (seq, subnorm_check);
}

/* Generates code to check if ARG is an infinity.  In the FP case we test
   FABS(ARG) == INF and in the INT case we check the bits on the exp and
   mantissa.  Returns a variable containing a boolean which has the result
   of the check.

   SEQ is the buffer to use to emit the gimple instructions into.
   LOC is the location to use during fold calls.  */
static tree
is_infinity (gimple_seq *seq, tree arg, location_t loc)
{
  tree type = TREE_TYPE (arg);

  machine_mode mode = TYPE_MODE (type);
  const tree bool_type = boolean_type_node;

  if (!HONOR_INFINITIES (mode))
    {
      return build_int_cst (bool_type, false);
    }

  /* If not using optimized route then exit early.  */
  if (!use_ieee_int_mode (arg))
    {
      /* Perform IBM extended format fixups if required.  */
      perform_ibm_extended_fixups (&arg, &mode, &type, loc);

      tree arg_p
	= emit_tree_and_return_var (seq, fold_build1_loc (loc, ABS_EXPR, type,
							arg));
      REAL_VALUE_TYPE r;
      real_inf (&r);
      tree res = fold_build2_loc (loc, EQ_EXPR, bool_type, arg_p,
				  build_real (type, r));

      return emit_tree_and_return_var (seq, res);
    }

  const real_format *format = REAL_MODE_FORMAT (mode);
  const HOST_WIDE_INT type_width = TYPE_PRECISION (type);

  tree int_arg_type = build_nonstandard_integer_type (type_width, true);

  /* This creates a mask to be used to check the exp value in the shifted
     integer representation of the fpnum.  */
  const int exp_bits  = (GET_MODE_SIZE (mode) * BITS_PER_UNIT) - format->p;
  gcc_assert (format->p > 0);

  tree significant_bit = build_int_cstu (int_arg_type, format->p);
  tree exp_mask
    = fold_build2_loc (loc, MINUS_EXPR, int_arg_type,
		       fold_build2_loc (loc, LSHIFT_EXPR, int_arg_type,
					build_int_cstu (int_arg_type, 2),
					build_int_cstu (int_arg_type,
							exp_bits - 1)),
		       build_int_cstu (int_arg_type, 1));

  /* Get the number reinterpreted as an integer.
     Shift left to remove the sign.  */
  tree int_arg
    = fold_build2_loc (loc, LSHIFT_EXPR, int_arg_type,
		       get_num_as_int (seq, arg, loc),
		       build_int_cstu (int_arg_type, 1));

  /* This mask checks to see if the exp has all bits set and mantissa no
     bits set.  */
  tree inf_mask
    = fold_build2_loc (loc, LSHIFT_EXPR, int_arg_type,
		       exp_mask, significant_bit);

  /* Check if exponent has all bits set and mantissa is 0.  */
  tree inf_check
    = emit_tree_and_return_var(seq,
	fold_build2_loc (loc, EQ_EXPR, bool_type,
			 emit_tree_and_return_var(seq, int_arg),
			 inf_mask));

  return emit_tree_and_return_var (seq, inf_check);
}

/* Generates code to check if ARG is a finite number.  In the FP case we check
   if FABS(ARG) <= MAX_VALUE(ARG) and in the INT case we check the exp and
   mantissa bits.  Returns a variable containing a boolean which has the result
   of the check.

   SEQ is the buffer to use to emit the gimple instructions into.
   LOC is the location to use during fold calls.  */
static tree
is_finite (gimple_seq *seq, tree arg, location_t loc)
{
  tree type = TREE_TYPE (arg);

  machine_mode mode = TYPE_MODE (type);
  const tree bool_type = boolean_type_node;

  if (!HONOR_NANS (arg) && !HONOR_INFINITIES (arg))
    {
      return build_int_cst (bool_type, true);
    }

  /* If not using optimized route then exit early.  */
  if (!use_ieee_int_mode (arg))
    {

      /* Perform IBM extended format fixups if required.  */
      perform_ibm_extended_fixups (&arg, &mode, &type, loc);

      tree const isle_fn = builtin_decl_explicit (BUILT_IN_ISLESSEQUAL);

      tree arg_p
	= emit_tree_and_return_var (seq, fold_build1_loc (loc, ABS_EXPR, type,
							  arg));
      REAL_VALUE_TYPE rmax;
      char buf[128];
      get_max_float (REAL_MODE_FORMAT (mode), buf, sizeof (buf));
      real_from_string (&rmax, buf);

      tree res = build_call_expr (isle_fn, 2,  arg_p, build_real (type, rmax));

      push_gimplify_context ();
      gimplify_expr (&res, seq, NULL, is_gimple_val, fb_either);
      pop_gimplify_context (NULL);

      return emit_tree_and_return_var (seq, gimple_boolify(res));
    }

  const real_format *format = REAL_MODE_FORMAT (mode);
  const HOST_WIDE_INT type_width = TYPE_PRECISION (type);

  tree int_arg_type = build_nonstandard_integer_type (type_width, true);

  /* This creates a mask to be used to check the exp value in the shifted
     integer representation of the fpnum.  */
  const int exp_bits  = (GET_MODE_SIZE (mode) * BITS_PER_UNIT) - format->p;
  gcc_assert (format->p > 0);

  tree significant_bit = build_int_cstu (int_arg_type, format->p);
  tree exp_mask
    = fold_build2_loc (loc, MINUS_EXPR, int_arg_type,
		       fold_build2_loc (loc, LSHIFT_EXPR, int_arg_type,
					build_int_cstu (int_arg_type, 2),
					build_int_cstu (int_arg_type,
							exp_bits - 1)),
		       build_int_cstu (int_arg_type, 1));

  /* Get the number reinterpreted as an integer.
     Shift left to remove the sign. */
  tree int_arg
    = fold_build2_loc (loc, LSHIFT_EXPR, int_arg_type,
		       get_num_as_int (seq, arg, loc),
		       build_int_cstu (int_arg_type, 1));

  /* This mask checks to see if the exp has all bits set and mantissa no
     bits set.  */
  tree inf_mask
    = fold_build2_loc (loc, LSHIFT_EXPR, int_arg_type,
		       exp_mask, significant_bit);

  /* Check if exponent has all bits set and mantissa is 0. */
  tree inf_check_tmp
    = fold_build2_loc (loc, LT_EXPR, bool_type,
		       emit_tree_and_return_var (seq, int_arg),
		       inf_mask);

  tree inf_check = emit_tree_and_return_var (seq, inf_check_tmp);

  return emit_tree_and_return_var (seq, inf_check);
}

/* Generates code to check if ARG is a NaN. In the FP case we simply check if
   ARG != ARG and in the INT case we check the bits in the exp and mantissa.
   Returns a variable containing a boolean which has the result of the check.

   SEQ is the buffer to use to emit the gimple instructions into.
   LOC is the location to use during fold calls.  */
static tree
is_nan (gimple_seq *seq, tree arg, location_t loc)
{
  tree type = TREE_TYPE (arg);

  machine_mode mode = TYPE_MODE (type);
  const tree bool_type = boolean_type_node;

  if (!HONOR_NANS (mode))
    {
      return build_int_cst (bool_type, false);
    }

  const real_format *format = REAL_MODE_FORMAT (mode);

  /* If not using optimized route then exit early.  */
  if (!use_ieee_int_mode (arg))
    {
      /* Perform IBM extended format fixups if required.  */
      perform_ibm_extended_fixups (&arg, &mode, &type, loc);

      tree arg_p
	= emit_tree_and_return_var (seq, fold_build1_loc (loc, ABS_EXPR, type,
							  arg));
      tree res
	= fold_build2_loc (loc, UNORDERED_EXPR, bool_type,arg_p, arg_p);

      return emit_tree_and_return_var (seq, res);
  }

  const HOST_WIDE_INT type_width = TYPE_PRECISION (type);
  tree int_arg_type = build_nonstandard_integer_type (type_width, true);

  /* This creates a mask to be used to check the exp value in the shifted
     integer representation of the fpnum.  */
  const int exp_bits  = (GET_MODE_SIZE (mode) * BITS_PER_UNIT) - format->p;
  tree significant_bit = build_int_cstu (int_arg_type, format->p);
  tree exp_mask
    = fold_build2_loc (loc, MINUS_EXPR, int_arg_type,
		       fold_build2_loc (loc, LSHIFT_EXPR, int_arg_type,
					build_int_cstu (int_arg_type, 2),
					build_int_cstu (int_arg_type,
							exp_bits - 1)),
		       build_int_cstu (int_arg_type, 1));

  /* Get the number reinterpreted as an integer.
     Shift left to remove the sign.  */
  tree int_arg
    = fold_build2_loc (loc, LSHIFT_EXPR, int_arg_type,
		       get_num_as_int (seq, arg, loc),
		       build_int_cstu (int_arg_type, 1));

  /* This mask checks to see if the exp has all bits set and mantissa no
     bits set.  */
  tree inf_mask
    = fold_build2_loc (loc, LSHIFT_EXPR, int_arg_type,
		       exp_mask, significant_bit);

  /* Check if exponent has all bits set and mantissa is not 0.  */
  tree nan_check
    = emit_tree_and_return_var(seq,
	fold_build2_loc (loc, GT_EXPR, bool_type,
			 emit_tree_and_return_var(seq, int_arg),
			 inf_mask));

  return emit_tree_and_return_var (seq, nan_check);
}

/* Validates a single argument from the arguments list CALL at position INDEX.
   The extracted parameter is compared against the expected type CODE.

   A boolean is returned indicating if the parameter exist and if of the
   expected type.  */
static bool
gimple_validate_arg (gimple* call, int index, enum tree_code code)
{
  const tree arg = gimple_call_arg (call, index);
  if (!arg)
    return false;
  else if (code == POINTER_TYPE)
    return POINTER_TYPE_P (TREE_TYPE (arg));
  else if (code == INTEGER_TYPE)
    return INTEGRAL_TYPE_P (TREE_TYPE (arg));
  return code == TREE_CODE (TREE_TYPE (arg));
}

/* Lowers calls to __builtin_fpclassify to
   fpclassify (x) ->
     isnormal(x) ? FP_NORMAL :
       iszero (x) ? FP_ZERO :
	 isnan (x) ? FP_NAN :
	   isinfinite (x) ? FP_INFINITE :
	     FP_SUBNORMAL.

   The code may use integer arithmentic if it decides
   that the produced assembly would be faster. This can only be done
   for numbers that are similar to IEEE-754 in format.

   This builtin will generate code to return the appropriate floating
   point classification depending on the value of the floating point
   number passed in.  The possible return values must be supplied as
   int arguments to the call in the following order: FP_NAN, FP_INFINITE,
   FP_NORMAL, FP_SUBNORMAL and FP_ZERO.  The ellipses is for exactly
   one floating point argument which is "type generic".

   GSI is the gimple iterator containing the fpclassify call to lower.
   The call will be expanded and replaced inline in the given GSI.  */
static void
lower_builtin_fpclassify (gimple_stmt_iterator *gsi)
{
  gimple *call = gsi_stmt (*gsi);
  location_t loc = gimple_location (call);

  /* Verify the required arguments in the original call.  */
  if (gimple_call_num_args (call) != 6
      || !gimple_validate_arg (call, 0, INTEGER_TYPE)
      || !gimple_validate_arg (call, 1, INTEGER_TYPE)
      || !gimple_validate_arg (call, 2, INTEGER_TYPE)
      || !gimple_validate_arg (call, 3, INTEGER_TYPE)
      || !gimple_validate_arg (call, 4, INTEGER_TYPE)
      || !gimple_validate_arg (call, 5, REAL_TYPE))
    return;

  /* Collect the arguments from the call.  */
  tree fp_nan = gimple_call_arg (call, 0);
  tree fp_infinite = gimple_call_arg (call, 1);
  tree fp_normal = gimple_call_arg (call, 2);
  tree fp_subnormal = gimple_call_arg (call, 3);
  tree fp_zero = gimple_call_arg (call, 4);
  tree arg = gimple_call_arg (call, 5);

  gimple_seq body = NULL;

  /* Create label to jump to to exit.  */
  tree done_label = create_artificial_label (UNKNOWN_LOCATION);
  tree dest;
  tree orig_dest = dest = gimple_call_lhs (call);
  if (orig_dest && TREE_CODE (orig_dest) == SSA_NAME)
    dest = create_tmp_reg (TREE_TYPE (orig_dest));

  emit_tree_cond (&body, dest, done_label,
		  is_normal (&body, arg, loc), fp_normal);
  emit_tree_cond (&body, dest, done_label,
		  is_zero (&body, arg, loc), fp_zero);
  emit_tree_cond (&body, dest, done_label,
		  is_nan (&body, arg, loc), fp_nan);
  emit_tree_cond (&body, dest, done_label,
		  is_infinity (&body, arg, loc), fp_infinite);

  /* And finally, emit the default case if nothing else matches.
     This replaces the call to is_subnormal.  */
  gimple_seq_add_stmt (&body, gimple_build_assign (dest, fp_subnormal));
  gimple_seq_add_stmt (&body, gimple_build_label (done_label));

  /* Build orig_dest = dest if necessary.  */
  if (dest != orig_dest)
    {
      gimple_seq_add_stmt (&body, gimple_build_assign (orig_dest, dest));
    }

  gsi_insert_seq_before (gsi, body, GSI_SAME_STMT);


  /* Remove the call to __builtin_fpclassify.  */
  gsi_remove (gsi, false);
}

/* Generic wrapper for the is_nan, is_normal, is_subnormal, is_zero, etc.
   All these functions have the same setup. The wrapper validates the parameter
   and also creates the branches and labels required to properly invoke.
   This has been generalize and the function to call is passed as argument FNDECL.

   GSI is the gimple iterator containing the fpclassify call to lower.
   The call will be expanded and replaced inline in the given GSI.  */
static void
gen_call_fp_builtin (gimple_stmt_iterator *gsi,
		     tree (*fndecl)(gimple_seq *, tree, location_t))
{
  gimple *call = gsi_stmt (*gsi);
  location_t loc = gimple_location (call);

  /* Verify the required arguments in the original call.  */
  if (gimple_call_num_args (call) != 1
      || !gimple_validate_arg (call, 0, REAL_TYPE))
    return;

  tree arg = gimple_call_arg (call, 0);
  gimple_seq body = NULL;

  /* Create label to jump to to exit.  */
  tree done_label = create_artificial_label (UNKNOWN_LOCATION);
  tree dest;
  tree orig_dest = dest = gimple_call_lhs (call);
  tree type = TREE_TYPE (orig_dest);
  if (orig_dest && TREE_CODE (orig_dest) == SSA_NAME)
      dest = create_tmp_reg (type);

  tree t_true = build_int_cst (type, true);
  tree t_false = build_int_cst (type, false);

  emit_tree_cond (&body, dest, done_label,
		  fndecl (&body, arg, loc), t_true);

  /* And finally, emit the default case if nothing else matches.
     This replaces the call to false.  */
  gimple_seq_add_stmt (&body, gimple_build_assign (dest, t_false));
  gimple_seq_add_stmt (&body, gimple_build_label (done_label));

  /* Build orig_dest = dest if necessary.  */
  if (dest != orig_dest)
  {
    gimple_seq_add_stmt (&body, gimple_build_assign (orig_dest, dest));
  }

  gsi_insert_seq_before (gsi, body, GSI_SAME_STMT);

  /* Remove the call to the builtin.  */
  gsi_remove (gsi, false);
}

/* Lower and expand calls to __builtin_isnan in GSI.  */
static void
lower_builtin_isnan (gimple_stmt_iterator *gsi)
{
  gen_call_fp_builtin (gsi, &is_nan);
}

/* Lower and expand calls to __builtin_isinfinite in GSI.  */
static void
lower_builtin_isinfinite (gimple_stmt_iterator *gsi)
{
  gen_call_fp_builtin (gsi, &is_infinity);
}

/* Lower and expand calls to __builtin_isnormal in GSI.  */
static void
lower_builtin_isnormal (gimple_stmt_iterator *gsi)
{
  gen_call_fp_builtin (gsi, &is_normal);
}

/* Lower and expand calls to __builtin_iszero in GSI.  */
static void
lower_builtin_iszero (gimple_stmt_iterator *gsi)
{
  gen_call_fp_builtin (gsi, &is_zero);
}

/* Lower and expand calls to __builtin_issubnormal in GSI.  */
static void
lower_builtin_issubnormal (gimple_stmt_iterator *gsi)
{
  gen_call_fp_builtin (gsi, &is_subnormal);
}

/* Lower and expand calls to __builtin_isfinite in GSI.  */
static void
lower_builtin_isfinite (gimple_stmt_iterator *gsi)
{
  gen_call_fp_builtin (gsi, &is_finite);
}

/* Lower calls to posix_memalign to
     res = posix_memalign (ptr, align, size);
     if (res == 0)
       *ptr = __builtin_assume_aligned (*ptr, align);
   or to
     void *tem;
     res = posix_memalign (&tem, align, size);
     if (res == 0)
       ptr = __builtin_assume_aligned (tem, align);
   in case the first argument was &ptr.  That way we can get at the
   alignment of the heap pointer in CCP.  */

static void
lower_builtin_posix_memalign (gimple_stmt_iterator *gsi)
{
  gimple *stmt, *call = gsi_stmt (*gsi);
  tree pptr = gimple_call_arg (call, 0);
  tree align = gimple_call_arg (call, 1);
  tree res = gimple_call_lhs (call);
  tree ptr = create_tmp_reg (ptr_type_node);
  if (TREE_CODE (pptr) == ADDR_EXPR)
    {
      tree tem = create_tmp_var (ptr_type_node);
      TREE_ADDRESSABLE (tem) = 1;
      gimple_call_set_arg (call, 0, build_fold_addr_expr (tem));
      stmt = gimple_build_assign (ptr, tem);
    }
  else
    stmt = gimple_build_assign (ptr,
				fold_build2 (MEM_REF, ptr_type_node, pptr,
					     build_int_cst (ptr_type_node, 0)));
  if (res == NULL_TREE)
    {
      res = create_tmp_reg (integer_type_node);
      gimple_call_set_lhs (call, res);
    }
  tree align_label = create_artificial_label (UNKNOWN_LOCATION);
  tree noalign_label = create_artificial_label (UNKNOWN_LOCATION);
  gimple *cond = gimple_build_cond (EQ_EXPR, res, integer_zero_node,
				   align_label, noalign_label);
  gsi_insert_after (gsi, cond, GSI_NEW_STMT);
  gsi_insert_after (gsi, gimple_build_label (align_label), GSI_NEW_STMT);
  gsi_insert_after (gsi, stmt, GSI_NEW_STMT);
  stmt = gimple_build_call (builtin_decl_implicit (BUILT_IN_ASSUME_ALIGNED),
			    2, ptr, align);
  gimple_call_set_lhs (stmt, ptr);
  gsi_insert_after (gsi, stmt, GSI_NEW_STMT);
  stmt = gimple_build_assign (fold_build2 (MEM_REF, ptr_type_node, pptr,
					   build_int_cst (ptr_type_node, 0)),
			      ptr);
  gsi_insert_after (gsi, stmt, GSI_NEW_STMT);
  gsi_insert_after (gsi, gimple_build_label (noalign_label), GSI_NEW_STMT);
}


/* Record the variables in VARS into function FN.  */

void
record_vars_into (tree vars, tree fn)
{
  for (; vars; vars = DECL_CHAIN (vars))
    {
      tree var = vars;

      /* BIND_EXPRs contains also function/type/constant declarations
         we don't need to care about.  */
      if (!VAR_P (var))
	continue;

      /* Nothing to do in this case.  */
      if (DECL_EXTERNAL (var))
	continue;

      /* Record the variable.  */
      add_local_decl (DECL_STRUCT_FUNCTION (fn), var);
    }
}


/* Record the variables in VARS into current_function_decl.  */

void
record_vars (tree vars)
{
  record_vars_into (vars, current_function_decl);
}
