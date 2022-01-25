/* Expand an OpenMP metadirective.

   Copyright (C) 2021 Free Software Foundation, Inc.

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
#include "target.h"
#include "tree.h"
#include "langhooks.h"
#include "gimple.h"
#include "tree-pass.h"
#include "cgraph.h"
#include "fold-const.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "gomp-constants.h"
#include "omp-general.h"
#include "diagnostic-core.h"
#include "tree-cfg.h"
#include "cfganal.h"
#include "ssa.h"
#include "tree-into-ssa.h"
#include "cfghooks.h"

static void
omp_expand_metadirective (function *fun, basic_block bb)
{
  gimple *stmt = last_stmt (bb);
  vec<struct omp_metadirective_variant> candidates
    = omp_resolve_metadirective (stmt);

  /* This is the last chance for the metadirective to be resolved.  */
  if (candidates.is_empty ())
    gcc_unreachable ();

  auto_vec<tree> labels;

  for (unsigned int i = 0; i < candidates.length (); i++)
    labels.safe_push (candidates[i].directive);

  /* Delete BBs for all variants not in the candidate list.  */
  for (unsigned i = 0; i < gimple_num_ops (stmt); i++)
    {
      tree label = gimple_omp_metadirective_label (stmt, i);
      if (!labels.contains (label))
	{
	  edge e = find_edge (bb, label_to_block (fun, label));
	  remove_edge_and_dominated_blocks (e);
	}
    }

  /* Remove the metadirective statement.  */
  gimple_stmt_iterator gsi = gsi_last_bb (bb);
  gsi_remove (&gsi, true);

  if (candidates.length () == 1)
    {
      /* Special case if there is only one selector - there should be one
	 remaining edge from BB to the selected variant.  */
      edge e = find_edge (bb, label_to_block (fun,
					      candidates.last ().directive));
      e->flags |= EDGE_FALLTHRU;

      return;
    }

  basic_block cur_bb = bb;

  /* For each candidate, create a conditional that checks the dynamic
     condition, branching to the candidate directive if true, to the
     next candidate check if false.  */
  for (unsigned i = 0; i < candidates.length () - 1; i++)
    {
      basic_block next_bb = NULL;
      gcond *cond_stmt = gimple_build_cond_from_tree (candidates[i].selector,
						      NULL_TREE, NULL_TREE);
      gsi = gsi_last_bb (cur_bb);
      gsi_insert_seq_after (&gsi, cond_stmt, GSI_NEW_STMT);

      if (i < candidates.length () - 2)
	{
	  edge e_false = split_block (cur_bb, cond_stmt);
	  e_false->flags &= ~EDGE_FALLTHRU;
	  e_false->flags |= EDGE_FALSE_VALUE;
	  e_false->probability = profile_probability::uninitialized ();

	  next_bb = e_false->dest;
	}

      /* Redirect the source of the edge from BB to the candidate directive
	 to the conditional.  Reusing the edge avoids disturbing phi nodes in
	  the destination BB.  */
      edge e = find_edge (bb, label_to_block (fun, candidates[i].directive));
      redirect_edge_pred (e, cur_bb);
      e->flags |= EDGE_TRUE_VALUE;

      if (next_bb)
	cur_bb = next_bb;
    }

  /* The last of the candidates is always static.  */
  edge e = find_edge (cur_bb, label_to_block (fun,
					      candidates.last ().directive));
  e->flags |= EDGE_FALSE_VALUE;
}

namespace {

const pass_data pass_data_omp_expand_metadirective =
{
  GIMPLE_PASS, /* type */
  "omp_expand_metadirective", /* name */
  OPTGROUP_OMP, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_gimple_lcf, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa | TODO_cleanup_cfg, /* todo_flags_finish */
};

class pass_omp_expand_metadirective : public gimple_opt_pass
{
public:
  pass_omp_expand_metadirective (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_omp_expand_metadirective, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
  {
    return (flag_openmp);
  }

  virtual unsigned int execute (function *fun);
}; // class pass_omp_oacc_kernels_decompose

unsigned int
pass_omp_expand_metadirective::execute (function *fun)
{
  basic_block bb;
  auto_vec<basic_block> metadirective_bbs;

  FOR_EACH_BB_FN (bb, fun)
    {
      gimple *stmt = last_stmt (bb);
      if (stmt && is_a<gomp_metadirective *> (stmt))
	metadirective_bbs.safe_push (bb);
    }

  if (metadirective_bbs.is_empty ())
    return 0;

  calculate_dominance_info (CDI_DOMINATORS);

  for (unsigned i = 0; i < metadirective_bbs.length (); i++)
    omp_expand_metadirective (fun, metadirective_bbs[i]);

  free_dominance_info (fun, CDI_DOMINATORS);
  mark_virtual_operands_for_renaming (fun);

  return 0;
}

} // anon namespace


gimple_opt_pass *
make_pass_omp_expand_metadirective (gcc::context *ctxt)
{
  return new pass_omp_expand_metadirective (ctxt);
}
