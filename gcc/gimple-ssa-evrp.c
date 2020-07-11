/* Support routines for Value Range Propagation (VRP).
   Copyright (C) 2005-2020 Free Software Foundation, Inc.

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

class evrp_folder : public substitute_and_fold_engine
{
public:
  evrp_folder () : m_range_analyzer (/*update_global_ranges=*/true),
    m_vr_values (m_range_analyzer.get_vr_values ()),
    simplifier (m_vr_values)
  {
  }

  ~evrp_folder ()
  {
    if (dump_file)
      {
	fprintf (dump_file, "\nValue ranges after Early VRP:\n\n");
	m_range_analyzer.dump_all_value_ranges (dump_file);
	fprintf (dump_file, "\n");
      }
  }

  tree get_value (tree op, gimple *stmt ATTRIBUTE_UNUSED) OVERRIDE
  {
    return m_vr_values->op_with_constant_singleton_value_range (op);
  }

  void pre_fold_bb (basic_block bb) OVERRIDE
  {
    if (dump_file && (dump_flags & TDF_DETAILS))
      fprintf (dump_file, "evrp visiting BB%d\n", bb->index);
    m_range_analyzer.enter (bb);
  }

  void pre_fold_stmt (gimple *stmt) OVERRIDE
  {
    if (dump_file && (dump_flags & TDF_DETAILS))
      {
	fprintf (dump_file, "evrp visiting stmt ");
	print_gimple_stmt (dump_file, stmt, 0);
      }
    m_range_analyzer.record_ranges_from_stmt (stmt, false);
  }

  bool fold_stmt (gimple_stmt_iterator *gsi) OVERRIDE
  {
    return simplifier.simplify (gsi);
  }

  void post_fold_bb (basic_block bb) OVERRIDE
  {
    m_range_analyzer.leave (bb);
  }

  void post_new_stmt (gimple *stmt) OVERRIDE
  {
    m_range_analyzer.get_vr_values ()->set_defs_to_varying (stmt);
  }

private:
  DISABLE_COPY_AND_ASSIGN (evrp_folder);
  class evrp_range_analyzer m_range_analyzer;
  class vr_values *m_vr_values;

  simplify_using_ranges simplifier;
};

/* Main entry point for the early vrp pass which is a simplified non-iterative
   version of vrp where basic blocks are visited in dominance order.  Value
   ranges discovered in early vrp will also be used by ipa-vrp.  */

static unsigned int
execute_early_vrp ()
{
  /* Ideally this setup code would move into the ctor for the folder
     However, this setup can change the number of blocks which
     invalidates the internal arrays that are set up by the dominator
     walker in substitute_and_fold_engine.  */
  loop_optimizer_init (LOOPS_NORMAL | LOOPS_HAVE_RECORDED_EXITS);
  rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa);
  scev_initialize ();
  calculate_dominance_info (CDI_DOMINATORS);

  evrp_folder folder;
  folder.substitute_and_fold ();

  scev_finalize ();
  loop_optimizer_finalize ();
  return 0;
}

namespace {

const pass_data pass_data_early_vrp =
{
  GIMPLE_PASS, /* type */
  "evrp", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_EARLY_VRP, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_cleanup_cfg | TODO_update_ssa | TODO_verify_all ),
};

class pass_early_vrp : public gimple_opt_pass
{
public:
  pass_early_vrp (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_early_vrp, ctxt)
    {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_early_vrp (m_ctxt); }
  virtual bool gate (function *)
    {
      return flag_tree_vrp != 0;
    }
  virtual unsigned int execute (function *)
    { return execute_early_vrp (); }

}; // class pass_vrp
} // anon namespace

gimple_opt_pass *
make_pass_early_vrp (gcc::context *ctxt)
{
  return new pass_early_vrp (ctxt);
}
