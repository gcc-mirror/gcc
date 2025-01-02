/* If-elseif-else to switch conversion pass
   Copyright (C) 2020-2025 Free Software Foundation, Inc.

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

/* Algorithm of the pass runs in the following steps:
   a) We walk basic blocks in DOMINATOR order so that we first reach
      a first condition of a future switch.
   b) We follow false edges of a if-else-chain and we record chain
      of GIMPLE conditions.  These blocks are only used for comparison
      of a common SSA_NAME and we do not allow any side effect.
   c) We remove all basic blocks (except first) of such chain and
      GIMPLE switch replaces the condition in the first basic block.
   d) We move all GIMPLE statements in the removed blocks into the
      first one.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-dfa.h"
#include "tree-cfgcleanup.h"
#include "alias.h"
#include "tree-ssa-loop.h"
#include "diagnostic.h"
#include "cfghooks.h"
#include "tree-into-ssa.h"
#include "cfganal.h"
#include "dbgcnt.h"
#include "target.h"
#include "alloc-pool.h"
#include "tree-switch-conversion.h"
#include "tree-ssa-reassoc.h"
#include "tree-ssa.h"

using namespace tree_switch_conversion;

struct condition_info
{
  typedef auto_vec<std::pair<gphi *, tree>> mapping_vec;

  condition_info (gcond *cond, bool has_side_effect): m_cond (cond),
    m_bb (gimple_bb (cond)), m_forwarder_bb (NULL), m_ranges (),
    m_true_edge (NULL), m_false_edge (NULL),
    m_true_edge_phi_mapping (), m_false_edge_phi_mapping (),
    m_has_side_effect (has_side_effect)
  {
    m_ranges.create (0);
  }

  /* Recond PHI mapping for an original edge E and save these into
     vector VEC.  */
  void record_phi_mapping (edge e, mapping_vec *vec);

  gcond *m_cond;
  basic_block m_bb;
  basic_block m_forwarder_bb;
  auto_vec<range_entry> m_ranges;
  edge m_true_edge;
  edge m_false_edge;
  mapping_vec m_true_edge_phi_mapping;
  mapping_vec m_false_edge_phi_mapping;
  bool m_has_side_effect;
};

/* Recond PHI mapping for an original edge E and save these into vector VEC.  */

void
condition_info::record_phi_mapping (edge e, mapping_vec *vec)
{
  for (gphi_iterator gsi = gsi_start_phis (e->dest); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      tree arg = PHI_ARG_DEF_FROM_EDGE (phi, e);
      vec->safe_push (std::make_pair (phi, arg));
    }
}

/* Master structure for one if to switch conversion candidate.  */

struct if_chain
{
  /* Default constructor.  */
  if_chain (): m_entries ()
  {
    m_entries.create (2);
  }

  /* Default destructor.  */
  ~if_chain ()
  {
    m_entries.release ();
  }

  /* Verify that all case ranges do not overlap.  */
  bool check_non_overlapping_cases ();

  /* Return true when the switch can be expanded with a jump table or
     a bit test (at least partially).  */
  bool is_beneficial ();

  /* If chain entries.  */
  vec<condition_info *> m_entries;
};

/* Compare two case ranges by minimum value.  */

static int
range_cmp (const void *a, const void *b)
{
  const range_entry *re1 = *(const range_entry * const *) a;
  const range_entry *re2 = *(const range_entry * const *) b;

  return tree_int_cst_compare (re1->low, re2->low);
}

/* Verify that all case ranges do not overlap.  */

bool
if_chain::check_non_overlapping_cases ()
{
  auto_vec<range_entry *> all_ranges;
  for (unsigned i = 0; i < m_entries.length (); i++)
    for (unsigned j = 0; j < m_entries[i]->m_ranges.length (); j++)
      all_ranges.safe_push (&m_entries[i]->m_ranges[j]);

  all_ranges.qsort (range_cmp);

  for (unsigned i = 0; i < all_ranges.length () - 1; i++)
    {
      range_entry *left = all_ranges[i];
      range_entry *right = all_ranges[i + 1];
      if (tree_int_cst_le (left->low, right->low)
	  && tree_int_cst_le (right->low, left->high))
	return false;
    }

  return true;
}

/* Compare clusters by minimum value.  */

static int
cluster_cmp (const void *a, const void *b)
{
  simple_cluster *sc1 = *(simple_cluster * const *) a;
  simple_cluster *sc2 = *(simple_cluster * const *) b;

  return tree_int_cst_compare (sc1->get_low (), sc2->get_high ());
}

/* Dump constructed CLUSTERS with prefix MESSAGE.  */

static void
dump_clusters (vec<cluster *> *clusters, const char *message)
{
  if (dump_file)
    {
      fprintf (dump_file, ";; %s: ", message);
      for (unsigned i = 0; i < clusters->length (); i++)
	(*clusters)[i]->dump (dump_file, dump_flags & TDF_DETAILS);
      fprintf (dump_file, "\n");
    }
}

/* Return true when the switch can be expanded with a jump table or
   a bit test (at least partially).  */

bool
if_chain::is_beneficial ()
{
  profile_probability prob = profile_probability::uninitialized ();

  auto_vec<cluster *> clusters;
  clusters.create (m_entries.length ());

  for (unsigned i = 0; i < m_entries.length (); i++)
    {
      condition_info *info = m_entries[i];
      for (unsigned j = 0; j < info->m_ranges.length (); j++)
	{
	  range_entry *range = &info->m_ranges[j];
	  basic_block bb = info->m_true_edge->dest;
	  bool has_forwarder = !info->m_true_edge_phi_mapping.is_empty ();
	  clusters.safe_push (new simple_cluster (range->low, range->high,
						  NULL_TREE, bb, prob,
						  has_forwarder));
	}
    }

  /* Sort clusters and merge them.  */
  auto_vec<cluster *> filtered_clusters;
  filtered_clusters.create (16);
  clusters.qsort (cluster_cmp);
  simple_cluster *left = static_cast<simple_cluster *> (clusters[0]);
  filtered_clusters.safe_push (left);

  for (unsigned i = 1; i < clusters.length (); i++)
    {
      simple_cluster *right = static_cast<simple_cluster *> (clusters[i]);
      tree type = TREE_TYPE (left->get_low ());
      if (!left->m_has_forward_bb
	  && !right->m_has_forward_bb
	  && left->m_case_bb == right->m_case_bb)
	{
	  if (wi::eq_p (wi::to_wide (right->get_low ()) - wi::to_wide
			(left->get_high ()), wi::one (TYPE_PRECISION (type))))
	    {
	      left->set_high (right->get_high ());
	      delete right;
	      continue;
	    }
	}

      left = static_cast<simple_cluster *> (clusters[i]);
      filtered_clusters.safe_push (left);
    }

  dump_clusters (&filtered_clusters, "Canonical GIMPLE case clusters");

  vec<cluster *> output
    = jump_table_cluster::find_jump_tables (filtered_clusters);
  bool r = output.length () < filtered_clusters.length ();
  if (r)
    {
      dump_clusters (&output, "JT can be built");
      release_clusters (output);
      return true;
    }
  else
    output.release ();

  output = bit_test_cluster::find_bit_tests (filtered_clusters, 2);
  r = output.length () < filtered_clusters.length ();
  if (r)
    dump_clusters (&output, "BT can be built");

  release_clusters (output);
  return r;
}

/* Build case label with MIN and MAX values of a given basic block DEST.  */

static tree
build_case_label (tree index_type, tree min, tree max, basic_block dest)
{
  if (min != NULL_TREE && index_type != TREE_TYPE (min))
    min = fold_convert (index_type, min);
  if (max != NULL_TREE && index_type != TREE_TYPE (max))
    max = fold_convert (index_type, max);

  tree label = gimple_block_label (dest);
  return build_case_label (min, min == max ? NULL_TREE : max, label);
}

/* Compare two integer constants.  */

static int
label_cmp (const void *a, const void *b)
{
  const_tree l1 = *(const const_tree *) a;
  const_tree l2 = *(const const_tree *) b;

  return tree_int_cst_compare (CASE_LOW (l1), CASE_LOW (l2));
}

/* Convert a given if CHAIN into a switch GIMPLE statement.  */

static void
convert_if_conditions_to_switch (if_chain *chain)
{
  if (!dbg_cnt (if_to_switch))
    return;

  auto_vec<tree> labels;
  unsigned entries = chain->m_entries.length ();
  condition_info *first_cond = chain->m_entries[0];
  condition_info *last_cond = chain->m_entries[entries - 1];

  edge default_edge = last_cond->m_false_edge;
  basic_block default_bb = default_edge->dest;

  gimple_stmt_iterator gsi = gsi_for_stmt (first_cond->m_cond);
  tree index_type = TREE_TYPE (first_cond->m_ranges[0].exp);
  for (unsigned i = 0; i < entries; i++)
    {
      condition_info *info = chain->m_entries[i];
      basic_block case_bb = info->m_true_edge->dest;

      /* Create a forwarder block if needed.  */
      if (!info->m_true_edge_phi_mapping.is_empty ())
	{
	  info->m_forwarder_bb = split_edge (info->m_true_edge);
	  case_bb = info->m_forwarder_bb;
	}

      for (unsigned j = 0; j < info->m_ranges.length (); j++)
	labels.safe_push (build_case_label (index_type,
					    info->m_ranges[j].low,
					    info->m_ranges[j].high,
					    case_bb));
      default_bb = info->m_false_edge->dest;

      if (i == 0)
	{
	  remove_edge (first_cond->m_true_edge);
	  remove_edge (first_cond->m_false_edge);
	}
      else
	delete_basic_block (info->m_bb);

      make_edge (first_cond->m_bb, case_bb, 0);
    }

  labels.qsort (label_cmp);

  edge e = find_edge (first_cond->m_bb, default_bb);
  if (e == NULL)
    e = make_edge (first_cond->m_bb, default_bb, 0);
  gswitch *s
    = gimple_build_switch (first_cond->m_ranges[0].exp,
			   build_case_label (index_type, NULL_TREE,
					     NULL_TREE, default_bb),
			   labels);

  gsi_remove (&gsi, true);
  gsi_insert_before (&gsi, s, GSI_NEW_STMT);

  if (dump_file)
    {
      fprintf (dump_file, "Expanded into a new gimple STMT: ");
      print_gimple_stmt (dump_file, s, 0, TDF_SLIM);
      putc ('\n', dump_file);
    }

  /* Fill up missing PHI node arguments.  */
  for (unsigned i = 0; i < chain->m_entries.length (); ++i)
    {
      condition_info *info = chain->m_entries[i];
      for (unsigned j = 0; j < info->m_true_edge_phi_mapping.length (); ++j)
	{
	  std::pair<gphi *, tree> item = info->m_true_edge_phi_mapping[j];
	  add_phi_arg (item.first, item.second,
		       single_succ_edge (info->m_forwarder_bb),
		       UNKNOWN_LOCATION);
	}
    }

  /* Fill up missing PHI nodes for the default BB.  */
  for (unsigned j = 0; j < last_cond->m_false_edge_phi_mapping.length (); ++j)
    {
      std::pair<gphi *, tree> item = last_cond->m_false_edge_phi_mapping[j];
      add_phi_arg (item.first, item.second, e, UNKNOWN_LOCATION);
    }
}

/* Identify an index variable used in BB in a GIMPLE condition.
   Save information about the condition into CONDITIONS_IN_BBS.  */

static void
find_conditions (basic_block bb,
		 hash_map<basic_block, condition_info *> *conditions_in_bbs)
{
  gimple_stmt_iterator gsi = gsi_last_nondebug_bb (bb);
  if (gsi_end_p (gsi))
    return;

  gcond *cond = dyn_cast<gcond *> (gsi_stmt (gsi));
  if (cond == NULL)
    return;

  tree lhs = gimple_cond_lhs (cond);
  tree rhs = gimple_cond_rhs (cond);
  tree_code code = gimple_cond_code (cond);

  condition_info *info = new condition_info (cond, !no_side_effect_bb (bb));

  gassign *def;
  if (code == NE_EXPR
      && TREE_CODE (lhs) == SSA_NAME
      && (def = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (lhs))) != NULL
      && integer_zerop (rhs))
    {
      enum tree_code rhs_code = gimple_assign_rhs_code (def);
      if (rhs_code == BIT_IOR_EXPR)
	{
	  info->m_ranges.safe_grow (2, true);
	  init_range_entry (&info->m_ranges[0], gimple_assign_rhs1 (def), NULL);
	  init_range_entry (&info->m_ranges[1], gimple_assign_rhs2 (def), NULL);
	}
    }
  else
    {
      info->m_ranges.safe_grow (1, true);
      init_range_entry (&info->m_ranges[0], NULL_TREE, cond);
    }

  /* All identified ranges must have equal expression and IN_P flag.  */
  if (!info->m_ranges.is_empty ())
    {
      edge true_edge, false_edge;
      tree expr = info->m_ranges[0].exp;
      bool in_p = info->m_ranges[0].in_p;

      extract_true_false_edges_from_block (bb, &true_edge, &false_edge);
      info->m_true_edge = in_p ? true_edge : false_edge;
      info->m_false_edge = in_p ? false_edge : true_edge;

      for (unsigned i = 0; i < info->m_ranges.length (); ++i)
	if (info->m_ranges[i].exp == NULL_TREE
	    || !INTEGRAL_TYPE_P (TREE_TYPE (info->m_ranges[i].exp))
	    || info->m_ranges[i].low == NULL_TREE
	    || info->m_ranges[i].high == NULL_TREE
	    || (TYPE_PRECISION (TREE_TYPE (info->m_ranges[i].low))
		!= TYPE_PRECISION (TREE_TYPE (info->m_ranges[i].high))))
	  goto exit;

      for (unsigned i = 1; i < info->m_ranges.length (); ++i)
	if (info->m_ranges[i].exp != expr
	    || info->m_ranges[i].in_p != in_p)
	  goto exit;

      info->record_phi_mapping (info->m_true_edge,
				&info->m_true_edge_phi_mapping);
      info->record_phi_mapping (info->m_false_edge,
				&info->m_false_edge_phi_mapping);
      conditions_in_bbs->put (bb, info);
      return;
    }

exit:
  delete info;
}

namespace {

const pass_data pass_data_if_to_switch =
{
  GIMPLE_PASS, /* type */
  "iftoswitch", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_IF_TO_SWITCH, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa /* todo_flags_finish */
};

class pass_if_to_switch : public gimple_opt_pass
{
public:
  pass_if_to_switch (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_if_to_switch, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override
  {
    return (jump_table_cluster::is_enabled ()
	    || bit_test_cluster::is_enabled ());
  }

  unsigned int execute (function *) final override;

}; // class pass_if_to_switch

unsigned int
pass_if_to_switch::execute (function *fun)
{
  auto_vec<if_chain *> all_candidates;
  hash_map<basic_block, condition_info *> conditions_in_bbs;

  mark_ssa_maybe_undefs ();

  basic_block bb;
  FOR_EACH_BB_FN (bb, fun)
    find_conditions (bb, &conditions_in_bbs);

  if (conditions_in_bbs.is_empty ())
    return 0;

  int *rpo = XNEWVEC (int, n_basic_blocks_for_fn (fun));
  unsigned n = pre_and_rev_post_order_compute_fn (fun, NULL, rpo, false);

  auto_bitmap seen_bbs;
  for (int i = n - 1; i >= 0; --i)
    {
      basic_block bb = BASIC_BLOCK_FOR_FN (fun, rpo[i]);
      if (bitmap_bit_p (seen_bbs, bb->index))
	continue;

      bitmap_set_bit (seen_bbs, bb->index);
      condition_info **slot = conditions_in_bbs.get (bb);
      if (slot)
	{
	  condition_info *info = *slot;
	  if_chain *chain = new if_chain ();
	  chain->m_entries.safe_push (info);
	  /* Try to find a chain starting in this BB.  */
	  while (true)
	    {
	      if (!single_pred_p (gimple_bb (info->m_cond)))
		break;
	      edge e = single_pred_edge (gimple_bb (info->m_cond));
	      condition_info **info2 = conditions_in_bbs.get (e->src);
	      if (!info2 || info->m_ranges[0].exp != (*info2)->m_ranges[0].exp)
		break;

	      /* It is important that the blocks are linked through FALSE_EDGE.
		 For an expression of index != VALUE, true and false edges
		 are flipped.  */
	      if ((*info2)->m_false_edge != e)
		break;

	      /* Only the first BB in a chain can have a side effect.  */
	      if (info->m_has_side_effect)
		break;

	      chain->m_entries.safe_push (*info2);
	      bitmap_set_bit (seen_bbs, e->src->index);
	      info = *info2;
	    }

	  chain->m_entries.reverse ();
	  if (chain->m_entries.length () >= 2
	      && chain->check_non_overlapping_cases ()
	      && chain->is_beneficial ())
	    {
	      gcond *cond = chain->m_entries[0]->m_cond;
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, cond,
				 "Condition chain with %d BBs "
				 "transformed into a switch statement.\n",
				 chain->m_entries.length ());
	      all_candidates.safe_push (chain);
	    }
	  else
	    delete chain;
	}
    }

  for (unsigned i = 0; i < all_candidates.length (); i++)
    {
      convert_if_conditions_to_switch (all_candidates[i]);
      delete all_candidates[i];
    }

  free (rpo);

  for (hash_map<basic_block, condition_info *>::iterator it
       = conditions_in_bbs.begin (); it != conditions_in_bbs.end (); ++it)
    delete (*it).second;

  if (!all_candidates.is_empty ())
    {
      free_dominance_info (CDI_DOMINATORS);
      return TODO_cleanup_cfg;
    }

  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_if_to_switch (gcc::context *ctxt)
{
  return new pass_if_to_switch (ctxt);
}
