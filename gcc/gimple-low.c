/* GIMPLE lowering pass.  Converts High GIMPLE into Low GIMPLE.

   Copyright (C) 2003-2019 Free Software Foundation, Inc.

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
#include "predict.h"
#include "gimple-predict.h"
#include "gimple-fold.h"

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

  /* If we had begin stmt markers from e.g. PCH, but this compilation
     doesn't want them, lower_stmt will have cleaned them up; we can
     now clear the flag that indicates we had them.  */
  if (!MAY_HAVE_DEBUG_MARKER_STMTS && cfun->debug_nonbind_markers)
    {
      /* This counter needs not be exact, but before lowering it will
	 most certainly be.  */
      gcc_assert (cfun->debug_marker_count == 0);
      cfun->debug_nonbind_markers = false;
    }

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

    case GIMPLE_DEBUG:
      gcc_checking_assert (cfun->debug_nonbind_markers);
      /* We can't possibly have debug bind stmts before lowering, we
	 first emit them when entering SSA.  */
      gcc_checking_assert (gimple_debug_nonbind_marker_p (stmt));
      /* Propagate fallthruness.  */
      /* If the function (e.g. from PCH) had debug stmts, but they're
	 disabled for this compilation, remove them.  */
      if (!MAY_HAVE_DEBUG_MARKER_STMTS)
	gsi_remove (gsi, true);
      else
	gsi_next (gsi);
      return;

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
	    && fndecl_built_in_p (decl, BUILT_IN_NORMAL))
	  {
	    if (DECL_FUNCTION_CODE (decl) == BUILT_IN_SETJMP)
	      {
		lower_builtin_setjmp (gsi);
		data->cannot_fallthru = false;
		return;
	      }
	    else if (DECL_FUNCTION_CODE (decl) == BUILT_IN_POSIX_MEMALIGN
		     && flag_tree_bit_ccp
		     && gimple_builtin_call_types_compatible_p (stmt, decl))
	      {
		lower_builtin_posix_memalign (gsi);
		return;
	      }
	  }

	if (decl && (flags_from_decl_or_type (decl) & ECF_NORETURN))
	  {
	    data->cannot_fallthru = true;
	    gsi_next (gsi);
	    return;
	  }

	/* We delay folding of built calls from gimplification to
	   here so the IL is in consistent state for the diagnostic
	   machineries job.  */
	if (gimple_call_builtin_p (stmt))
	  fold_stmt (gsi);
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

    case GIMPLE_DEBUG:
      gcc_checking_assert (gimple_debug_begin_stmt_p (stmt));
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
  return gimple_stmt_may_fallthru (gimple_seq_last_nondebug_stmt (seq));
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
  /* location includes block.  */
  gimple_set_location (t, gimple_location (stmt));
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
  /* location includes block.  */
  gimple_set_location (g, loc);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);

  /* Build 'DEST = 0' and insert.  */
  if (dest)
    {
      g = gimple_build_assign (dest, build_zero_cst (TREE_TYPE (dest)));
      gimple_set_location (g, loc);
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
  gsi_insert_before (gsi, g, GSI_SAME_STMT);

  /* Build 'DEST = 1' and insert.  */
  if (dest)
    {
      g = gimple_build_assign (dest, fold_convert_loc (loc, TREE_TYPE (dest),
						       integer_one_node));
      gimple_set_location (g, loc);
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
