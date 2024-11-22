/* Support for C++23 ASSUME keyword functionailty.
   Copyright (C) 2023-2024 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>.

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
#include "basic-block.h"
#include "bitmap.h"
#include "options.h"
#include "function.h"
#include "cfg.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-iterator.h"
#include "gimple-range.h"
#include "tree-dfa.h"
#include "tree-cfg.h"
#include "gimple-pretty-print.h"

// An assume query utilizes the current range query to implelemtn the assume
// keyword.
// For any return value of 1 from the function, it attempts to determine
// which paths leads to a 1 value being returned. On those paths, what
// the ranges of any ssa_names listed in bitmap P (usually the parm list for
// the function) are, and combined them all.
// These ranges are then set as the global ranges for those parms in this
// function.
// Other functions which then refer to this function in an assume builtin
// will then pick up these ranges for the paramters via the inferred range
// mechanism.
//   See gimple-range-infer.cc::gimple_infer_range::check_assume_func ()
//
// my_func (int x)
// {
//   <...>
//   assume [[(x == 1 || x ==4))]]
//   if (x ==3)
//
// a small temporary assume function consisting of
// assume_f1 (int x) { return x == 1 || x == 4; }
// is constructed by the front end, and optimzed, at the very end of
// optimization, instead of generating code, we instead invoke the assume pass
// which uses this query to set the the global value of parm x to [1,1][4,4]
//
// Meanwhile., my_Fund has been rewritten to be:
//
// my_func (int x_2)
// {
//   <...>
//   assume_builtin_call  assume_f1 (x_2);
//   if (x_2 == 3)
//
// When ranger is processing the assume_builtin_call, it looks up the global
// value of the paramter in assume_f1, which is [1,1][4,4].  It then registers
// and inferred range at this statement setting the value x_2 to [1,1][4,4]
//
// Any uses of x_2 after this statement will now utilzie this inferred range.
//
// When VRP precoesses if (x_2 == 3), it picks up the inferred range, and
// determines that x_2 can never be 3, and will rewrite the branch to
//   if (0 != 0)

class assume_query
{
public:
  assume_query (function *f, bitmap p);
protected:
  inline void process_stmts (gimple *s, vrange &lhs_range)
  {
    fur_depend src (s, get_range_query (m_func));
    calculate_stmt (s, lhs_range, src);
    update_parms (src);
  }
  void update_parms (fur_source &src);
  void calculate_stmt (gimple *s, vrange &lhs_range, fur_source &src);
  void calculate_op (tree op, gimple *s, vrange &lhs, fur_source &src);
  void calculate_phi (gphi *phi, vrange &lhs_range);

  ssa_lazy_cache m_path;   // Values found on path
  ssa_lazy_cache m_parms;  // Cumulative parameter value calculated
  bitmap &m_parm_list;	   // Parameter ssa-names list.
  function *m_func;
};

// If function F returns a integral value, and has a single return
// statement, try to calculate the range of each value in P that leads
// to the return statement returning TRUE.

assume_query::assume_query (function *f, bitmap p) : m_parm_list (p),
						     m_func (f)
{
  basic_block exit_bb = EXIT_BLOCK_PTR_FOR_FN (f);
  // If there is more than one precessor to the exit block, bail.
  if (!single_pred_p (exit_bb))
    return;

  basic_block bb = single_pred (exit_bb);
  gimple_stmt_iterator gsi = gsi_last_nondebug_bb (bb);
  if (gsi_end_p (gsi))
    return;
  gimple *s = gsi_stmt (gsi);
  if (!is_a<greturn *> (s))
    return;

  // Check if the single return value is a symbolic and supported type.
  greturn *gret = as_a<greturn *> (s);
  tree op = gimple_return_retval (gret);
  if (!gimple_range_ssa_p (op))
    return;
  tree lhs_type = TREE_TYPE (op);
  if (!irange::supports_p (lhs_type))
    return;

  // Only values of interest are when the return value is 1.  The defintion
  // of the return value must be in the same block, or we have
  // complicated flow control we don't understand, and just return.
  unsigned prec = TYPE_PRECISION (lhs_type);
  int_range<2> lhs_range (lhs_type, wi::one (prec), wi::one (prec));

  gimple *def = SSA_NAME_DEF_STMT (op);
  if (!def || gimple_get_lhs (def) != op || gimple_bb (def) != bb)
    return;

  // Determine if this is a PHI or a linear sequence to deal with.
  if (is_a<gphi *> (def))
    calculate_phi (as_a<gphi *> (def), lhs_range);
  else
    process_stmts (def, lhs_range);

  if (dump_file)
    fprintf (dump_file, "\n\nAssumptions :\n--------------\n");

  // Now export any interesting values that were found.
  bitmap_iterator bi;
  unsigned x;
  EXECUTE_IF_SET_IN_BITMAP (m_parm_list, 0, x, bi)
    {
      tree name = ssa_name (x);
      tree type = TREE_TYPE (name);
      value_range assume_range (type);
      // Set the global range of NAME to anything calculated.
      if (m_parms.get_range (assume_range, name) && !assume_range.varying_p ())
	set_range_info (name, assume_range);
    }

  if (dump_file)
   {
     fputc ('\n', dump_file);
     gimple_dump_cfg (dump_file, dump_flags & ~TDF_DETAILS);
   }
}

// This function Will update all the current value of interesting parameters.
// It tries, in order:
//    a) a range found via path calculations.
//    b) range of the parm at SRC point in the IL. (either edge or stmt)
//    c) VARYING if those options fail.
//  The value is then unioned with any existing value, allowing for the
//  cumulation of all ranges leading to the return that return 1.

void
assume_query::update_parms (fur_source &src)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nupdate parameters\n");

  // Merge any parameter values.
  bitmap_iterator bi;
  unsigned x;
  EXECUTE_IF_SET_IN_BITMAP (m_parm_list, 0, x, bi)
    {
      tree name = ssa_name (x);
      tree type = TREE_TYPE (name);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "PARAMETER ");
	  print_generic_expr (dump_file, name, TDF_SLIM);
	}
      value_range glob_range (type);
      // Find a value from calculations.
      // There will be a value in m_path if GORI calculated an operand value.
      if (m_path.get_range (glob_range, name))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "\n  Calculated path range:");
	      glob_range.dump (dump_file);
	    }
	}
      // Otherwise, let ranger determine the range at the SRC location.
      else if (src.get_operand (glob_range, name))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "\n  Ranger Computes path range:");
	      glob_range.dump (dump_file);
	    }
	}
      else
	glob_range.set_varying (type);

      // Find any current saved value of parm, and combine them.
      value_range parm_range (type);
      if (m_parms.get_range (parm_range, name))
	glob_range.union_ (parm_range);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\n  Combine with previous range:");
	  parm_range.dump (dump_file);
	  fputc ('\n', dump_file);
	  print_generic_expr (dump_file, name, TDF_SLIM);
	  fprintf (dump_file, " = ");
	  glob_range.dump (dump_file);
	  fputc ('\n', dump_file);
	}
      // Set this new value.
      m_parms.set_range (name, glob_range);
    }
  // Now reset the path values for the next path.
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "---------------------\n");
  m_path.clear ();
}


// Evaluate PHI statement, using the provided LHS range.
// Only process edge that are taken and return the LHS of the PHI.

void
assume_query::calculate_phi (gphi *phi, vrange &lhs_range)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Processing PHI feeding return value:\n");
      print_gimple_stmt (dump_file, phi, 0, TDF_SLIM);
    }
  for (unsigned x= 0; x < gimple_phi_num_args (phi); x++)
    {
      tree arg = gimple_phi_arg_def (phi, x);
      value_range arg_range (TREE_TYPE (arg));
      edge e = gimple_phi_arg_edge (phi, x);
      value_range edge_range (TREE_TYPE (arg));
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "\nArgument %d (bb%d->bb%d): ", x, e->src->index,
		   e->dest->index);
	  print_generic_expr (dump_file, arg, TDF_SLIM);
	  fputc ('\n', dump_file);
	}
      // If we can't get an edge range, be conservative and assume the
      // edge can be taken.
      if (get_range_query (m_func)->range_on_edge (edge_range, e, arg))
	{
	  if (gimple_range_ssa_p (arg))
	    {
	      arg_range = lhs_range;
	      range_cast (arg_range, TREE_TYPE (arg));

	      // An SSA_NAME arg will start with the LHS value.
	      // Check the range of ARG on the edge leading here.  If that range
	      // cannot be any value from the LHS of the PHI, then this branch
	      // will not be taken to return the LHS value and can be ignored.
	      arg_range.intersect (edge_range);
	      if (arg_range.undefined_p ())
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "  IGNORE edge :  LHS range :");
		      lhs_range.dump (dump_file);
		      fprintf (dump_file, " Edge produces : ");
		      edge_range.dump (dump_file);
		      fputc ('\n', dump_file);
		    }
		  continue;
		}

	      // If the def is in the immediate preceeding block, process it
	      // with GORI to determine what values can produce this
	      // argument value.  Otherwise there is more CFG flow, so query
	      // the edge for parm ranges.  This is conservative.
	      gimple *def_stmt = SSA_NAME_DEF_STMT (arg);
	      if (def_stmt && gimple_get_lhs (def_stmt) == arg
		  && gimple_bb (def_stmt) == e->src)
		{
		  process_stmts (def_stmt, arg_range);
		  continue;
		}
	      // Fall through to process the parameter values on the edge.
	    }
	  else
	    {
	      // If this is a constant value that differs from LHS, this
	      // edge cannot be taken and we can ignore it. Otherwise fall
	      // thorugh and process the parameters on the edge.
	      edge_range.intersect (lhs_range);
	      if (edge_range.undefined_p ())
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "  IGNORE : const edge not taken\n");
		  continue;
		}
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file,
			 "  Const edge executed, compute incoming ranges.\n");

	    }
	}
      // The parameters on the edge now need calculating.
      fur_edge src (e, get_range_query (m_func));
      update_parms (src);
    }
}

// Evaluate operand OP on statement S, using the provided LHS range.
// If successful, set the range in path table, then visit OP's def stmt
// if it is in the same BB.

void
assume_query::calculate_op (tree op, gimple *s, vrange &lhs, fur_source &src)
{
  basic_block bb = gimple_bb (s);
  value_range op_range (TREE_TYPE (op));
  if (src.gori () &&
      src.gori ()->compute_operand_range (op_range, s, lhs, op, src)
      && !op_range.varying_p ())
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "  Operand ");
	  print_generic_expr (dump_file, op, TDF_SLIM);
	  fprintf (dump_file, " calculated as ");
	  op_range.dump (dump_file);
	}
      // Set the global range, merging if there is already a range.
      m_path.merge_range (op, op_range);
      m_path.get_range (op_range, op);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "  New path range :");
	  op_range.dump (dump_file);
	  fputc ('\n', dump_file);
	}
      gimple *def_stmt = SSA_NAME_DEF_STMT (op);
      // Terminate if the pathway leads to a different block as we
      // are not dealing with flow. Ranger will make those queries.
      if (def_stmt && gimple_get_lhs (def_stmt) == op
	  && gimple_bb (def_stmt) == bb)
	calculate_stmt (def_stmt, op_range, src);
    }
}

// Evaluate statement S which produces range LHS_RANGE.  Use GORI to
// determine what values the operands can have to produce the LHS,
// and set these in the M_PATH table.

void
assume_query::calculate_stmt (gimple *s, vrange &lhs_range, fur_source &src)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "  Processing stmt with LHS = ");
      lhs_range.dump (dump_file);
      fprintf (dump_file, " : ");
      print_gimple_stmt (dump_file, s, 0, TDF_SLIM);
    }
  gimple_range_op_handler handler (s);
  if (handler)
    {
      tree op = gimple_range_ssa_p (handler.operand1 ());
      if (op)
	calculate_op (op, s, lhs_range, src);
      op = gimple_range_ssa_p (handler.operand2 ());
      if (op)
	calculate_op (op, s, lhs_range, src);
    }
}

namespace {

const pass_data pass_data_assumptions =
{
  GIMPLE_PASS, /* type */
  "assumptions", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_ASSUMPTIONS, /* tv_id */
  PROP_ssa, /* properties_required */
  PROP_assumptions_done, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_end */
};


class pass_assumptions : public gimple_opt_pass
{
public:
  pass_assumptions (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_assumptions, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *fun) final override { return fun->assume_function; }
  unsigned int execute (function *fun) final override
    {
      // Create a bitmap of all the paramters in this function.
      // Invoke the assume_query to detemine what values these parameters
      // have when the function returns TRUE, and set the globals value of
      // those parameters in this function based on that.  This will later be
      // utilized by ranger when processing builtin IFN_ASSUME function calls.
      // See gimple-range-infer.cc::check_assume_func ().
      auto_bitmap decls;
      for (tree arg = DECL_ARGUMENTS (fun->decl); arg; arg = DECL_CHAIN (arg))
	{
	  tree name = ssa_default_def (fun, arg);
	  if (!name || !gimple_range_ssa_p (name))
	    continue;
	  tree type = TREE_TYPE (name);
	  if (!value_range::supports_type_p (type))
	    continue;
	  bitmap_set_bit (decls, SSA_NAME_VERSION (name));
	}
      // If there are no parameters to map, simply return;
      if (bitmap_empty_p (decls))
	return TODO_discard_function;

      enable_ranger (fun);

      // This assume query will set any global values required.
      assume_query query (fun, decls);

      disable_ranger (fun);
      return TODO_discard_function;
    }

}; // class pass_assumptions

} // anon namespace

gimple_opt_pass *
make_pass_assumptions (gcc::context *ctx)
{
  return new pass_assumptions (ctx);
}
