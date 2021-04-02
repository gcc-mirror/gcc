/* Support routines for Value Range Propagation (VRP).
   Copyright (C) 2005-2021 Free Software Foundation, Inc.

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
#include "gimple-range.h"

// This is the classic EVRP folder which uses a dominator walk and pushes
// ranges into the next block if it is a single predecessor block.

class evrp_folder : public substitute_and_fold_engine
{
public:
  evrp_folder () :
    substitute_and_fold_engine (),
    m_range_analyzer (/*update_global_ranges=*/true),
    simplifier (&m_range_analyzer)
  { }

  ~evrp_folder ()
  {
    if (dump_file)
      {
	fprintf (dump_file, "\nValue ranges after Early VRP:\n\n");
	m_range_analyzer.dump_all_value_ranges (dump_file);
	fprintf (dump_file, "\n");
      }
  }

  tree value_of_expr (tree name, gimple *stmt) OVERRIDE
  {
    return m_range_analyzer.value_of_expr (name, stmt);
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
    m_range_analyzer.set_defs_to_varying (stmt);
  }

protected:
  DISABLE_COPY_AND_ASSIGN (evrp_folder);
  evrp_range_analyzer m_range_analyzer;
  simplify_using_ranges simplifier;
};

// This is a ranger based folder which continues to use the dominator
// walk to access the substitute and fold machinery.  Ranges are calculated
// on demand.

class rvrp_folder : public substitute_and_fold_engine
{
public:

  rvrp_folder () : substitute_and_fold_engine (), m_simplifier ()
  { 
    if (param_evrp_mode & EVRP_MODE_TRACE)
      m_ranger = new trace_ranger ();
    else
      m_ranger = new gimple_ranger ();
    m_simplifier.set_range_query (m_ranger);
  }
      
  ~rvrp_folder ()
  {
    if (dump_file && (dump_flags & TDF_DETAILS))
      m_ranger->dump (dump_file);
    delete m_ranger;
  }

  tree value_of_expr (tree name, gimple *s = NULL) OVERRIDE
  {
    return m_ranger->value_of_expr (name, s);
  }

  tree value_on_edge (edge e, tree name) OVERRIDE
  {
    return m_ranger->value_on_edge (e, name);
  }

  tree value_of_stmt (gimple *s, tree name = NULL) OVERRIDE
  {
    return m_ranger->value_of_stmt (s, name);
  }

  bool fold_stmt (gimple_stmt_iterator *gsi) OVERRIDE
  {
    return m_simplifier.simplify (gsi);
  }

private:
  DISABLE_COPY_AND_ASSIGN (rvrp_folder);
  gimple_ranger *m_ranger;
  simplify_using_ranges m_simplifier;
};

// In a hybrid folder, start with an EVRP folder, and add the required
// fold_stmt bits to either try the ranger first or second.
//
// The 3 value_* routines will always query both EVRP and the ranger for
// a result, and ensure they return the same value.  If either returns a value
// when the other doesn't, it is flagged in the listing, and the discoverd
// value is returned.
//
// The simplifier is unable to process 2 different sources, thus we try to 
// use one engine, and if it fails to simplify, try using the other engine.
// It is reported when the first attempt fails and the second succeeds.

class hybrid_folder : public evrp_folder
{
public:
  hybrid_folder (bool evrp_first)
  {
    if (param_evrp_mode & EVRP_MODE_TRACE)
      m_ranger = new trace_ranger ();
    else
      m_ranger = new gimple_ranger ();

    if (evrp_first)
      {
	first = &m_range_analyzer;
	second = m_ranger;
      }
     else
      {
	first = m_ranger;
	second = &m_range_analyzer;
      }
  }

  ~hybrid_folder ()
  {
    if (dump_file && (dump_flags & TDF_DETAILS))
      m_ranger->dump (dump_file);
    delete m_ranger;
  }

  bool fold_stmt (gimple_stmt_iterator *gsi) OVERRIDE
    {
      simplifier.set_range_query (first);
      if (simplifier.simplify (gsi))
	return true;

      simplifier.set_range_query (second);
      if (simplifier.simplify (gsi))
	{
	  if (dump_file)
	    fprintf (dump_file, "EVRP:hybrid: Second query simplifed stmt\n");
	  return true;
	}
      return false;
    }

  tree value_of_expr (tree name, gimple *) OVERRIDE;
  tree value_on_edge (edge, tree name) OVERRIDE;
  tree value_of_stmt (gimple *, tree name) OVERRIDE;

private:
  DISABLE_COPY_AND_ASSIGN (hybrid_folder);
  gimple_ranger *m_ranger;
  range_query *first;
  range_query *second;
  tree choose_value (tree evrp_val, tree ranger_val);
};


tree
hybrid_folder::value_of_expr (tree op, gimple *stmt)
{
  tree evrp_ret = evrp_folder::value_of_expr (op, stmt);
  tree ranger_ret = m_ranger->value_of_expr (op, stmt);
  return choose_value (evrp_ret, ranger_ret);
}

tree
hybrid_folder::value_on_edge (edge e, tree op)
{
  // Call evrp::value_of_expr directly.  Otherwise another dual call is made
  // via hybrid_folder::value_of_expr, but without an edge.
  tree evrp_ret = evrp_folder::value_of_expr (op, NULL);
  tree ranger_ret = m_ranger->value_on_edge (e, op);
  return choose_value (evrp_ret, ranger_ret);
}

tree
hybrid_folder::value_of_stmt (gimple *stmt, tree op) 
{
  // Call evrp::value_of_expr directly.  Otherwise another dual call is made
  // via hybrid_folder::value_of_expr, but without a stmt.
  tree evrp_ret;
  if (op)
    evrp_ret = evrp_folder::value_of_expr (op, NULL);
  else
    evrp_ret = NULL_TREE;

  tree ranger_ret = m_ranger->value_of_stmt (stmt, op);
  return choose_value (evrp_ret, ranger_ret);
}

// Given trees returned by EVRP and Ranger, choose/report the value to use
// by the folder.

tree
hybrid_folder::choose_value (tree evrp_val, tree ranger_val)
{
  // If both found the same value, just return it.
  if (evrp_val && ranger_val && !compare_values (evrp_val, ranger_val))
    return evrp_val;

  // If neither returned a value, return NULL_TREE.
  if (!ranger_val && !evrp_val)
    return NULL_TREE;

  // Otherwise there is a discrepancy to flag.
  if (dump_file)
    {
      if (evrp_val && ranger_val)
	fprintf (dump_file, "EVRP:hybrid: Disagreement\n");
      if (evrp_val)
	{
	  fprintf (dump_file, "EVRP:hybrid: EVRP found singleton ");
	  print_generic_expr (dump_file, evrp_val);
	  fprintf (dump_file, "\n");
	}
      if (ranger_val)
	{
	  fprintf (dump_file, "EVRP:hybrid: RVRP found singleton ");
	  print_generic_expr (dump_file, ranger_val);
	  fprintf (dump_file, "\n");
	}
    }

  // If one value was found, return it.
  if (!evrp_val)
    return ranger_val;
  if (!ranger_val)
    return evrp_val;

  // If values are different, return the first calculated value.
  if ((param_evrp_mode & EVRP_MODE_RVRP_FIRST) == EVRP_MODE_RVRP_FIRST)
    return ranger_val;
  return evrp_val;
}

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

  // Only the last 2 bits matter for choosing the folder.
  switch (param_evrp_mode & EVRP_MODE_RVRP_FIRST)
    {
    case EVRP_MODE_EVRP_ONLY:
      {
	evrp_folder folder;
	folder.substitute_and_fold ();
	break;
      }
    case EVRP_MODE_RVRP_ONLY:
      {
	rvrp_folder folder;
	folder.substitute_and_fold ();
	break;
      }
    case EVRP_MODE_EVRP_FIRST:
      {
	hybrid_folder folder (true);
	folder.substitute_and_fold ();
	break;
      }
    case EVRP_MODE_RVRP_FIRST:
      {
	hybrid_folder folder (false);
	folder.substitute_and_fold ();
	break;
      }
    default:
      gcc_unreachable ();
    }

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
