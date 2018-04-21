/* VRP implemented with SSA Ranger.
   Copyright (C) 2018 Free Software Foundation, Inc.
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
#include "backend.h"
#include "insn-codes.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "optabs-tree.h"
#include "gimple-pretty-print.h"
#include "diagnostic-core.h"
#include "flags.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "calls.h"
#include "cfganal.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "tree-cfg.h"
#include "wide-int.h"
#include "domwalk.h"
#include "ssa-range.h"
#include "ssa-range-global.h"


static bool
argument_ok_to_propagate (tree name)
{
  if (TREE_CODE (name) != SSA_NAME)
    return true;
  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (name))
    {
      // From may_propagate_copy
      if (SSA_NAME_IS_DEFAULT_DEF (name) && (SSA_NAME_VAR (name) == NULL_TREE
          || TREE_CODE (SSA_NAME_VAR (name)) == VAR_DECL))
      return true;
    }
  // probably dont need this.
  if (virtual_operand_p (name))
    return false;
  return true;
}

static unsigned int
execute_ranger_vrp ()
{
  // Create a temp ranger and exercise it before running in order to get a
  // listing in the dump file, and to fully exercise the code.
  { 
    path_ranger e;
    e.exercise (dump_file);
  }

  path_ranger ranger;
  basic_block bb;
  irange r;
  wide_int i;
  unsigned x;
  bitmap_iterator bi;
  bitmap touched = BITMAP_ALLOC (NULL);

  FOR_EACH_BB_FN (bb, cfun)
    {
      gcond *cond;
      gimple *stmt = last_stmt (bb);
      if (stmt && (cond = dyn_cast <gcond *> (stmt)))
        {
	  if (dump_file)
	    {
	      fprintf (dump_file, "Considering BB%d:  ", bb->index);
	      print_gimple_stmt (dump_file, cond, 0,0);
	    }
	  if (ranger.path_range_stmt (r, stmt))
	    {
	      if (dump_file)
	        {
		  fprintf (dump_file, "Found a range for the expression: ");
		  r.dump (dump_file);
		  fprintf (dump_file, "\n");
		}

	      if (r.singleton_p (i))
	        {
		  if (!argument_ok_to_propagate (gimple_cond_lhs (cond)) ||
		      !argument_ok_to_propagate (gimple_cond_rhs (cond)))
		    {
		      if (dump_file)
			{
			  fprintf (dump_file, "Found  BB%d branch",bb->index);
			  print_gimple_stmt (dump_file, stmt, 0, 0);
			  fprintf (dump_file, "cannot eliminate. proprules.\n");
			  fprintf (stderr,"\n  ***************** caught one\n");
			}
		      continue;
		    }

		  /* If either operand is an ssa_name, set the touched bit for
		     potential removal later if no uses are left.  */
		  tree t = valid_irange_ssa (gimple_cond_lhs (cond));
		  if (t)
		    bitmap_set_bit (touched, SSA_NAME_VERSION (t));
		  t = valid_irange_ssa (gimple_cond_rhs (cond));
		  if (t)
		    bitmap_set_bit (touched, SSA_NAME_VERSION (t));

		  if (dump_file)
		    {
		      fprintf (dump_file, "eliminating BB%d branch:\n",
			       bb->index);
		      print_gimple_stmt (dump_file, stmt, 0, 0);
		    }

		  /* Rewrite the condition to either true or false.  */
		  if (wi::eq_p (i, 0))
		    gimple_cond_make_false (cond);
		  else
		    gimple_cond_make_true (cond);
		  update_stmt (cond);

		  if (dump_file)
		    {
		      fprintf (dump_file, "Re-written to: \n");
		      print_gimple_stmt (dump_file, cond, 0, 0);
		      fprintf (dump_file, "\n");
		    }
		}
	    }
	}

    }

  // Now visit each name that was rewritten and see if the definiing statement
  // can be deleted due to no more uses in the program
  EXECUTE_IF_SET_IN_BITMAP (touched, 0, x, bi)
    {
      tree name = ssa_name (x);
      gimple *s = SSA_NAME_DEF_STMT (name);
      
      if (s)
        {
	  if (has_zero_uses (name))
	    {
	      if (dump_file)
		{
		  fprintf (dump_file, "Should delete");
		  print_gimple_stmt (dump_file, s, 0, 0);
		}
	    }
	  else
	    {
	      if (dump_file)
		{
		  fprintf (dump_file, "Still has uses, Could not delete ");
		  print_gimple_stmt (dump_file, s, 0, 0);
		}
	    }
	}
    }
  return 0;
}

namespace {

const pass_data pass_data_ranger_vrp =
{
  GIMPLE_PASS, /* type */
  "rvrp", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_RANGER_VRP, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_cleanup_cfg | TODO_verify_all ),
};

class pass_ranger_vrp : public gimple_opt_pass
{
public:
  pass_ranger_vrp (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_ranger_vrp, ctxt)
    {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_ranger_vrp (m_ctxt); }
  virtual bool gate (function *)
    {
      return flag_tree_rvrp != 0;
    }
  virtual unsigned int execute (function *)
    { return execute_ranger_vrp (); }

}; // class pass_vrp
} // anon namespace

gimple_opt_pass *
make_pass_ranger_vrp (gcc::context *ctxt)
{
  return new pass_ranger_vrp (ctxt);
}


