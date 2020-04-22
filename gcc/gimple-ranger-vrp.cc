/* Value range propagation pass using the ranger.
   Copyright (C) 2020 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez <aldyh@redhat.com>.

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
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "cfganal.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "tree-ssa-propagate.h"
#include "alloc-pool.h"
#include "domwalk.h"
#include "tree-cfgcleanup.h"
#include "vr-values.h"
#include "gimple-ssa-evrp-analyze.h"
#include "gimple-ranger.h"

#define DEBUG_RVRP getenv("DEBUG_RVRP")

class rvrp_ranger : public trace_ranger
{
public:
  // This is the range getter for the simplifier, but is really only
  // used for the conditional folding which still uses equivalences.
  virtual const value_range_equiv *get_value_range (const_tree expr,
						    gimple *stmt)
  {
    widest_irange r;
    if (range_of_expr (r, const_cast<tree> (expr), stmt))
      return new value_range_equiv (r); // FIXME: leaks
    return new value_range_equiv (TREE_TYPE (expr));
  }
};

class rvrp_folder : public substitute_and_fold_engine
{
public:
  rvrp_folder () : simplifier (&ranger) { }

  tree get_value (tree op, gimple *stmt)
  {
    if (disable_il_changes_p ())
      return NULL;

    widest_irange r;
    tree singleton;
    if (ranger.range_of_expr (r, op, stmt) && r.singleton_p (&singleton))
      return singleton;
    return NULL;
  }

  bool fold_stmt (gimple_stmt_iterator *gsi)
  {
    if (disable_il_changes_p ())
      return false;

    return simplifier.simplify (gsi);
  }

  bool disable_il_changes_p ()
  {
    return !flag_rvrp_changes;
  }

private:
  rvrp_ranger ranger;
  simplify_using_ranges simplifier;
};

static unsigned int
execute_ranger_vrp ()
{
  loop_optimizer_init (LOOPS_NORMAL | LOOPS_HAVE_RECORDED_EXITS);
  rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa);
  scev_initialize ();
  calculate_dominance_info (CDI_DOMINATORS);

  rvrp_folder folder;
  folder.substitute_and_fold ();

  scev_finalize ();
  loop_optimizer_finalize ();
  return 0;
}

namespace {

const pass_data pass_data_ranger_vrp =
{
 GIMPLE_PASS,			// type
 "rvrp",			// name
 OPTGROUP_NONE,			// optinfo_flags
 TV_TREE_EARLY_VRP,		// tv_id
 PROP_ssa,			// properties_required
 0,				// properties_provided
 0,				// properties_destroyed
 0,				// todo_flags_start
 ( TODO_cleanup_cfg | TODO_update_ssa | TODO_verify_all ),
};

class pass_ranger_vrp : public gimple_opt_pass
{
public:
  pass_ranger_vrp (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_ranger_vrp, ctxt)
    {}
  opt_pass * clone () { return new pass_ranger_vrp (m_ctxt); }
  virtual bool gate (function *)
    { return flag_tree_vrp != 0; }
  virtual unsigned int execute (function *)
    { return execute_ranger_vrp (); }
};

} // anon namespace

gimple_opt_pass *
make_pass_ranger_vrp (gcc::context *ctxt)
{
  return new pass_ranger_vrp (ctxt);
}
