/* The analysis "engine".
   Copyright (C) 2019-2020 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "fold-const.h"
#include "gcc-rich-location.h"
#include "alloc-pool.h"
#include "fibonacci_heap.h"
#include "shortest-paths.h"
#include "diagnostic-core.h"
#include "diagnostic-event-id.h"
#include "diagnostic-path.h"
#include "function.h"
#include "pretty-print.h"
#include "sbitmap.h"
#include "bitmap.h"
#include "tristate.h"
#include "ordered-hash-map.h"
#include "selftest.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/region-model.h"
#include "analyzer/constraint-manager.h"
#include "analyzer/sm.h"
#include "analyzer/pending-diagnostic.h"
#include "analyzer/diagnostic-manager.h"
#include "cfg.h"
#include "basic-block.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-pretty-print.h"
#include "cgraph.h"
#include "digraph.h"
#include "analyzer/supergraph.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/program-state.h"
#include "analyzer/exploded-graph.h"
#include "analyzer/analysis-plan.h"
#include "analyzer/checker-path.h"
#include "analyzer/state-purge.h"
#include "analyzer/bar-chart.h"

/* For an overview, see gcc/doc/analyzer.texi.  */

#if ENABLE_ANALYZER

namespace ana {

static int readability_comparator (const void *p1, const void *p2);

/* class impl_region_model_context : public region_model_context.  */

impl_region_model_context::
impl_region_model_context (exploded_graph &eg,
			   const exploded_node *enode_for_diag,
			   const program_state *old_state,
			   program_state *new_state,
			   state_change *change,
			   const gimple *stmt,
			   stmt_finder *stmt_finder)
: m_eg (&eg), m_logger (eg.get_logger ()),
  m_enode_for_diag (enode_for_diag),
  m_old_state (old_state),
  m_new_state (new_state),
  m_change (change),
  m_stmt (stmt),
  m_stmt_finder (stmt_finder),
  m_ext_state (eg.get_ext_state ())
{
}

impl_region_model_context::
impl_region_model_context (program_state *state,
			   state_change *change,
			   const extrinsic_state &ext_state,
			   logger *logger)
: m_eg (NULL), m_logger (logger), m_enode_for_diag (NULL),
  m_old_state (NULL),
  m_new_state (state),
  m_change (change),
  m_stmt (NULL),
  m_stmt_finder (NULL),
  m_ext_state (ext_state)
{
}

void
impl_region_model_context::warn (pending_diagnostic *d)
{
  LOG_FUNC (get_logger ());
  if (m_eg)
    m_eg->get_diagnostic_manager ().add_diagnostic
      (m_enode_for_diag, m_enode_for_diag->get_supernode (),
       m_stmt, m_stmt_finder, d);
}

void
impl_region_model_context::remap_svalue_ids (const svalue_id_map &map)
{
  m_new_state->remap_svalue_ids (map);
  if (m_change)
    m_change->remap_svalue_ids (map);
}

int
impl_region_model_context::on_svalue_purge (svalue_id first_unused_sid,
					    const svalue_id_map &map)
{
  int total = 0;
  int sm_idx;
  sm_state_map *smap;
  FOR_EACH_VEC_ELT (m_new_state->m_checker_states, sm_idx, smap)
    {
      const state_machine &sm = m_ext_state.get_sm (sm_idx);
      total += smap->on_svalue_purge (sm, sm_idx, first_unused_sid,
				      map, this);
    }
  if (m_change)
    total += m_change->on_svalue_purge (first_unused_sid);
  return total;
}

void
impl_region_model_context::on_unknown_change (svalue_id sid)
{
  int sm_idx;
  sm_state_map *smap;
  FOR_EACH_VEC_ELT (m_new_state->m_checker_states, sm_idx, smap)
    smap->on_unknown_change (sid);
}

/* class setjmp_svalue : public svalue.  */

/* Compare the fields of this setjmp_svalue with OTHER, returning true
   if they are equal.
   For use by svalue::operator==.  */

bool
setjmp_svalue::compare_fields (const setjmp_svalue &other) const
{
  return m_setjmp_record == other.m_setjmp_record;
}

/* Implementation of svalue::add_to_hash vfunc for setjmp_svalue.  */

void
setjmp_svalue::add_to_hash (inchash::hash &hstate) const
{
  hstate.add_int (m_setjmp_record.m_enode->m_index);
}

/* Get the index of the stored exploded_node.  */

int
setjmp_svalue::get_enode_index () const
{
  return m_setjmp_record.m_enode->m_index;
}

/* Implementation of svalue::print_details vfunc for setjmp_svalue.  */

void
setjmp_svalue::print_details (const region_model &model ATTRIBUTE_UNUSED,
			      svalue_id this_sid ATTRIBUTE_UNUSED,
			      pretty_printer *pp) const
{
  pp_printf (pp, "setjmp: EN: %i", get_enode_index ());
}

/* Concrete implementation of sm_context, wiring it up to the rest of this
   file.  */

class impl_sm_context : public sm_context
{
public:
  impl_sm_context (exploded_graph &eg,
		   int sm_idx,
		   const state_machine &sm,
		   const exploded_node *enode_for_diag,
		   const program_state *old_state,
		   program_state *new_state,
		   state_change *change,
		   const sm_state_map *old_smap,
		   sm_state_map *new_smap,
		   stmt_finder *stmt_finder = NULL)
  : sm_context (sm_idx, sm),
    m_logger (eg.get_logger ()),
    m_eg (eg), m_enode_for_diag (enode_for_diag),
    m_old_state (old_state), m_new_state (new_state),
    m_change (change),
    m_old_smap (old_smap), m_new_smap (new_smap),
    m_stmt_finder (stmt_finder)
  {
  }

  logger *get_logger () const { return m_logger.get_logger (); }

  tree get_fndecl_for_call (const gcall *call) FINAL OVERRIDE
  {
    impl_region_model_context old_ctxt
      (m_eg, m_enode_for_diag, NULL, NULL/*m_enode->get_state ()*/,
       m_change, call);
    region_model *model = m_new_state->m_region_model;
    return model->get_fndecl_for_call (call, &old_ctxt);
  }

  void on_transition (const supernode *node  ATTRIBUTE_UNUSED,
		      const gimple *stmt  ATTRIBUTE_UNUSED,
		      tree var,
		      state_machine::state_t from,
		      state_machine::state_t to,
		      tree origin) FINAL OVERRIDE
  {
    logger * const logger = get_logger ();
    LOG_FUNC (logger);
    impl_region_model_context old_ctxt
      (m_eg, m_enode_for_diag, NULL, NULL/*m_enode->get_state ()*/,
       m_change, stmt);
    svalue_id var_old_sid
      = m_old_state->m_region_model->get_rvalue (var, &old_ctxt);

    impl_region_model_context new_ctxt (m_eg, m_enode_for_diag,
					m_old_state, m_new_state,
					m_change, NULL);
    svalue_id var_new_sid
      = m_new_state->m_region_model->get_rvalue (var, &new_ctxt);
    svalue_id origin_new_sid
      = m_new_state->m_region_model->get_rvalue (origin, &new_ctxt);

    state_machine::state_t current = m_old_smap->get_state (var_old_sid);
    if (current == from)
      {
	if (logger)
	  logger->log ("%s: state transition of %qE: %s -> %s",
		       m_sm.get_name (),
		       var,
		       m_sm.get_state_name (from),
		       m_sm.get_state_name (to));
	m_new_smap->set_state (m_new_state->m_region_model, var_new_sid,
			       to, origin_new_sid);
	if (m_change)
	  m_change->add_sm_change (m_sm_idx, var_new_sid, from, to);
      }
  }

  void warn_for_state (const supernode *snode, const gimple *stmt,
		       tree var, state_machine::state_t state,
		       pending_diagnostic *d) FINAL OVERRIDE
  {
    LOG_FUNC (get_logger ());
    gcc_assert (d); // take ownership

    impl_region_model_context old_ctxt
      (m_eg, m_enode_for_diag, m_old_state, m_new_state, m_change, NULL);
    state_machine::state_t current;
    if (var)
      {
	svalue_id var_old_sid
	  = m_old_state->m_region_model->get_rvalue (var, &old_ctxt);
	current = m_old_smap->get_state (var_old_sid);
      }
    else
      current = m_old_smap->get_global_state ();

    if (state == current)
      {
	m_eg.get_diagnostic_manager ().add_diagnostic
	  (&m_sm, m_enode_for_diag, snode, stmt, m_stmt_finder,
	   var, state, d);
      }
    else
      delete d;
  }

  /* Hook for picking more readable trees for SSA names of temporaries,
     so that rather than e.g.
       "double-free of '<unknown>'"
     we can print:
       "double-free of 'inbuf.data'".  */

  tree get_readable_tree (tree expr) FINAL OVERRIDE
  {
    /* Only for SSA_NAMEs of temporaries; otherwise, return EXPR, as it's
       likely to be the least surprising tree to report.  */
    if (TREE_CODE (expr) != SSA_NAME)
      return expr;
    if (SSA_NAME_VAR (expr) != NULL)
      return expr;

    gcc_assert (m_new_state);
    svalue_id sid = m_new_state->m_region_model->get_rvalue (expr, NULL);
    /* Find trees for all regions storing the value.  */
    auto_vec<path_var> pvs;
    m_new_state->m_region_model->get_path_vars_for_svalue (sid, &pvs);
    if (pvs.length () < 1)
      return expr;
    /* Pick the "best" such tree.  */
    // TODO: should we also consider (and consolidate) equiv classes?
    pvs.qsort (readability_comparator);
    return pvs[0].m_tree;
  }

  state_machine::state_t get_global_state () const FINAL OVERRIDE
  {
    return m_old_state->m_checker_states[m_sm_idx]->get_global_state ();
  }

  void set_global_state (state_machine::state_t state) FINAL OVERRIDE
  {
    m_new_state->m_checker_states[m_sm_idx]->set_global_state (state);
  }

  void on_custom_transition (custom_transition *transition) FINAL OVERRIDE
  {
    transition->impl_transition (&m_eg,
				 const_cast<exploded_node *> (m_enode_for_diag),
				 m_sm_idx);
  }

  log_user m_logger;
  exploded_graph &m_eg;
  const exploded_node *m_enode_for_diag;
  const program_state *m_old_state;
  program_state *m_new_state;
  state_change *m_change;
  const sm_state_map *m_old_smap;
  sm_state_map *m_new_smap;
  stmt_finder *m_stmt_finder;
};

/* Subclass of stmt_finder for finding the best stmt to report the leak at,
   given the emission path.  */

class leak_stmt_finder : public stmt_finder
{
public:
  leak_stmt_finder (const exploded_graph &eg, tree var)
  : m_eg (eg), m_var (var) {}

  stmt_finder *clone () const FINAL OVERRIDE
  {
    return new leak_stmt_finder (m_eg, m_var);
  }

  const gimple *find_stmt (const exploded_path &epath)
    FINAL OVERRIDE
  {
    logger * const logger = m_eg.get_logger ();
    LOG_FUNC (logger);

    if (TREE_CODE (m_var) == SSA_NAME)
      {
	/* Locate the final write to this SSA name in the path.  */
	const gimple *def_stmt = SSA_NAME_DEF_STMT (m_var);

	int idx_of_def_stmt;
	bool found = epath.find_stmt_backwards (def_stmt, &idx_of_def_stmt);
	if (!found)
	  goto not_found;

	/* What was the next write to the underlying var
	   after the SSA name was set? (if any).  */

	for (unsigned idx = idx_of_def_stmt + 1;
	     idx < epath.m_edges.length ();
	     ++idx)
	  {
	    const exploded_edge *eedge = epath.m_edges[idx];
	    if (logger)
	      logger->log ("eedge[%i]: EN %i -> EN %i",
			   idx,
			   eedge->m_src->m_index,
			   eedge->m_dest->m_index);
	    const exploded_node *dst_node = eedge->m_dest;
	    const program_point &dst_point = dst_node->get_point ();
	    const gimple *stmt = dst_point.get_stmt ();
	    if (!stmt)
	      continue;
	    if (const gassign *assign = dyn_cast <const gassign *> (stmt))
	      {
		tree lhs = gimple_assign_lhs (assign);
		if (TREE_CODE (lhs) == SSA_NAME
		    && SSA_NAME_VAR (lhs) == SSA_NAME_VAR (m_var))
		  return assign;
	      }
	  }
      }

  not_found:

    /* Look backwards for the first statement with a location.  */
    int i;
    const exploded_edge *eedge;
    FOR_EACH_VEC_ELT_REVERSE (epath.m_edges, i, eedge)
      {
	if (logger)
	  logger->log ("eedge[%i]: EN %i -> EN %i",
		       i,
		       eedge->m_src->m_index,
		       eedge->m_dest->m_index);
	const exploded_node *dst_node = eedge->m_dest;
	const program_point &dst_point = dst_node->get_point ();
	const gimple *stmt = dst_point.get_stmt ();
	if (stmt)
	  if (get_pure_location (stmt->location) != UNKNOWN_LOCATION)
	    return stmt;
      }

    gcc_unreachable ();
    return NULL;
  }

private:
  const exploded_graph &m_eg;
  tree m_var;
};

/* A measurement of how good EXPR is for presenting to the user, so
   that e.g. we can say prefer printing
     "leak of 'tmp.m_ptr'"
   over:
     "leak of '<unknown>'".  */

static int
readability (const_tree expr)
{
  gcc_assert (expr);
  switch (TREE_CODE (expr))
    {
    case COMPONENT_REF:
    case MEM_REF:
      /* Impose a slight readability penalty relative to that of
	 operand 0.  */
      return readability (TREE_OPERAND (expr, 0)) - 1;

    case SSA_NAME:
      {
	if (tree var = SSA_NAME_VAR (expr))
	  return readability (var);
	/* Avoid printing '<unknown>' for SSA names for temporaries.  */
	return -1;
      }
      break;

    case VAR_DECL:
      /* Arbitrarily-chosen "high readability" value.  */
      return 256;

    default:
      return 0;
    }

  return 0;
}

/* A qsort comparator for trees to sort them into most user-readable to
   least user-readable.  */

static int
readability_comparator (const void *p1, const void *p2)
{
  path_var pv1 = *(path_var const *)p1;
  path_var pv2 = *(path_var const *)p2;

  /* TODO: should we consider stack depths?  */
  int r1 = readability (pv1.m_tree);
  int r2 = readability (pv2.m_tree);

  return r2 - r1;
}

/* Create an sm_context and use it to call SM's on_leak vfunc, so that
   it can potentially complain about a leak of DST_SID (in a new region_model)
   in the given STATE, where MAP can be used to map SID back to an "old"
   region_model.  */

void
impl_region_model_context::on_state_leak (const state_machine &sm,
					  int sm_idx,
					  svalue_id dst_sid,
					  svalue_id first_unused_sid,
					  const svalue_id_map &map,
					  state_machine::state_t state)
{
  logger * const logger = get_logger ();
  LOG_SCOPE (logger);
  if (logger)
    logger->log ("considering leak of sv%i", dst_sid.as_int ());

  if (!m_eg)
    return;

  /* m_old_state also needs to be non-NULL so that the sm_ctxt can look
     up the old state of the sid.  */
  gcc_assert (m_old_state);

  /* Don't report on sid leaking if it's equal to one of the used sids.
     For example, given:
       some_non_trivial_expression = malloc (sizeof (struct foo));
     we have:
       _1 = malloc;                         (void *)
       some_non_trivial_expression = _1;    (struct foo *)
     and at leak-detection time we may have:
       sv5: {type: 'struct foo *', &r3}  (used)
       sv6: {type: 'void *', &r3}         (unused)
     where both point to the same region.  We don't want to report a
     leak of sv6, so we reject the report due to its equality with sv5.  */
  gcc_assert (m_new_state);
  gcc_assert (!first_unused_sid.null_p ());
  for (int i = 0; i < first_unused_sid.as_int (); i++)
    {
      svalue_id used_sid = svalue_id::from_int (i);

      /* Use the "_without_cm" form of eval_condition, since
	 we're half-way through purging - we don't want to introduce new
	 equivalence classes into the constraint_manager for "sid" and
	 for each of the used_sids.  */
      const region_model &rm = *m_new_state->m_region_model;
      tristate eq = rm.eval_condition_without_cm (dst_sid, EQ_EXPR, used_sid);
      if (eq.is_true ())
	{
	  if (logger)
	    logger->log ("rejecting leak of sv%i due to equality with sv%i",
			 dst_sid.as_int (), used_sid.as_int ());
	  return;
	}
    }

  /* SID has leaked within the new state: no regions use it.
     We need to convert it back to a tree, but since no regions use it, we
     have to use MAP to convert it back to an svalue_id within the old state.
     We can then look that svalue_id up to locate regions and thus tree(s)
     that use it.  */

  svalue_id old_sid = map.get_src_for_dst (dst_sid);

  auto_vec<path_var> leaked_pvs;
  m_old_state->m_region_model->get_path_vars_for_svalue (old_sid, &leaked_pvs);

  if (leaked_pvs.length () < 1)
    return;

  /* Find "best" leaked tree.
     Sort the leaks into most human-readable first, through
     to least user-readable.  Given that we only emit one
     leak per EC, this ought to ensure that we pick the most
     user-readable description of each leaking EC.
     This assumes that all vars in the EC have the same state.  */
  leaked_pvs.qsort (readability_comparator);

  tree leaked_tree = leaked_pvs[0].m_tree;
  if (logger)
    logger->log ("best leaked_tree: %qE", leaked_tree);

  leak_stmt_finder stmt_finder (*m_eg, leaked_tree);
  impl_sm_context sm_ctxt (*m_eg, sm_idx, sm, m_enode_for_diag,
			   m_old_state, m_new_state,
			   m_change,
			   m_old_state->m_checker_states[sm_idx],
			   m_new_state->m_checker_states[sm_idx],
			   &stmt_finder);
  gcc_assert (m_enode_for_diag);

  /* Don't complain about leaks when returning from "main".  */
  if (m_enode_for_diag->get_supernode ()
      && m_enode_for_diag->get_supernode ()->return_p ())
    {
      tree fndecl = m_enode_for_diag->get_function ()->decl;
      if (0 == strcmp (IDENTIFIER_POINTER (DECL_NAME (fndecl)), "main"))
	{
	  if (logger)
	    logger->log ("not reporting leak from main");
	  return;
	}
    }

  pending_diagnostic *pd = sm.on_leak (leaked_tree);
  if (pd)
    m_eg->get_diagnostic_manager ().add_diagnostic
      (&sm, m_enode_for_diag, m_enode_for_diag->get_supernode (),
       m_stmt, &stmt_finder,
       leaked_tree, state, pd);
}

/* Implementation of region_model_context::on_inherited_svalue vfunc
   for impl_region_model_context.
   Notify all checkers that CHILD_SID has been created from PARENT_SID,
   so that those state machines that inherit state can propagate the state
   from parent to child.  */

void
impl_region_model_context::on_inherited_svalue (svalue_id parent_sid,
						svalue_id child_sid)
{
  if (!m_new_state)
    return;

  int sm_idx;
  sm_state_map *smap;
  FOR_EACH_VEC_ELT (m_new_state->m_checker_states, sm_idx, smap)
    {
      const state_machine &sm = m_ext_state.get_sm (sm_idx);
      if (sm.inherited_state_p ())
	smap->on_inherited_svalue (parent_sid, child_sid);
    }
}

/* Implementation of region_model_context::on_cast vfunc
   for impl_region_model_context.
   Notify all checkers that DST_SID is a cast of SRC_SID, so that sm-state
   can be propagated from src to dst.  */

void
impl_region_model_context::on_cast (svalue_id src_sid,
				    svalue_id dst_sid)
{
  if (!m_new_state)
    return;

  int sm_idx;
  sm_state_map *smap;
  FOR_EACH_VEC_ELT (m_new_state->m_checker_states, sm_idx, smap)
    smap->on_cast (src_sid, dst_sid);
}

/* Implementation of region_model_context::on_condition vfunc.
   Notify all state machines about the condition, which could lead to
   state transitions.  */

void
impl_region_model_context::on_condition (tree lhs, enum tree_code op, tree rhs)
{
  int sm_idx;
  sm_state_map *smap;
  FOR_EACH_VEC_ELT (m_new_state->m_checker_states, sm_idx, smap)
    {
      const state_machine &sm = m_ext_state.get_sm (sm_idx);
      impl_sm_context sm_ctxt (*m_eg, sm_idx, sm, m_enode_for_diag,
			       m_old_state, m_new_state,
			       m_change,
			       m_old_state->m_checker_states[sm_idx],
			       m_new_state->m_checker_states[sm_idx]);
      sm.on_condition (&sm_ctxt,
		       m_enode_for_diag->get_supernode (), m_stmt,
		       lhs, op, rhs);
    }
}

/* Implementation of region_model_context::on_phi vfunc.
   Notify all state machines about the phi, which could lead to
   state transitions.  */

void
impl_region_model_context::on_phi (const gphi *phi, tree rhs)
{
  int sm_idx;
  sm_state_map *smap;
  FOR_EACH_VEC_ELT (m_new_state->m_checker_states, sm_idx, smap)
    {
      const state_machine &sm = m_ext_state.get_sm (sm_idx);
      impl_sm_context sm_ctxt (*m_eg, sm_idx, sm, m_enode_for_diag,
			       m_old_state, m_new_state,
			       m_change,
			       m_old_state->m_checker_states[sm_idx],
			       m_new_state->m_checker_states[sm_idx]);
      sm.on_phi (&sm_ctxt, m_enode_for_diag->get_supernode (), phi, rhs);
    }
}

/* Implementation of region_model_context::on_unexpected_tree_code vfunc.
   Mark the new state as being invalid for further exploration.
   TODO(stage1): introduce a warning for when this occurs.  */

void
impl_region_model_context::on_unexpected_tree_code (tree t,
						    const dump_location_t &loc)
{
  logger * const logger = get_logger ();
  if (logger)
    logger->log ("unhandled tree code: %qs in %qs at %s:%i",
		 get_tree_code_name (TREE_CODE (t)),
		 loc.get_impl_location ().m_function,
		 loc.get_impl_location ().m_file,
		 loc.get_impl_location ().m_line);
  if (m_new_state)
    m_new_state->m_valid = false;
}

/* struct point_and_state.  */

/* Assert that this object is sane.  */

void
point_and_state::validate (const extrinsic_state &ext_state) const
{
  /* Skip this in a release build.  */
#if !CHECKING_P
  return;
#endif

  m_point.validate ();

  m_state.validate (ext_state);

  /* Verify that the callstring's model of the stack corresponds to that
     of the region_model.  */
  /* They should have the same depth.  */
  gcc_assert (m_point.get_stack_depth ()
	      == m_state.m_region_model->get_stack_depth ());
  /* Check the functions in the callstring vs those in the frames
     at each depth.  */
  for (int depth = 0; depth < m_point.get_stack_depth (); ++depth)
    {
      gcc_assert (m_point.get_function_at_depth (depth)
		  == m_state.m_region_model->get_function_at_depth (depth));
    }
}

/* Subroutine of print_enode_indices: print a run of indices from START_IDX
   to END_IDX to PP, using and updating *FIRST_RUN.  */

static void
print_run (pretty_printer *pp, int start_idx, int end_idx,
	   bool *first_run)
{
  if (!(*first_run))
    pp_string (pp, ", ");
  *first_run = false;
  if (start_idx == end_idx)
    pp_printf (pp, "EN: %i", start_idx);
  else
    pp_printf (pp, "EN: %i-%i", start_idx, end_idx);
}

/* Print the indices within ENODES to PP, collecting them as
   runs/singletons e.g. "EN: 4-7, EN: 20-23, EN: 42".  */

static void
print_enode_indices (pretty_printer *pp,
		     const auto_vec<exploded_node *> &enodes)
{
  int cur_start_idx = -1;
  int cur_finish_idx = -1;
  bool first_run = true;
  unsigned i;
  exploded_node *enode;
  FOR_EACH_VEC_ELT (enodes, i, enode)
    {
      if (cur_start_idx == -1)
	{
	  gcc_assert (cur_finish_idx == -1);
	  cur_start_idx = cur_finish_idx = enode->m_index;
	}
      else
	{
	  if (enode->m_index == cur_finish_idx + 1)
	    /* Continuation of a run.  */
	    cur_finish_idx = enode->m_index;
	  else
	    {
	      /* Finish existing run, start a new one.  */
	      gcc_assert (cur_start_idx >= 0);
	      gcc_assert (cur_finish_idx >= 0);
	      print_run (pp, cur_start_idx, cur_finish_idx,
			 &first_run);
	      cur_start_idx = cur_finish_idx = enode->m_index;
	    }
	}
    }
  /* Finish any existing run.  */
  if (cur_start_idx >= 0)
    {
      gcc_assert (cur_finish_idx >= 0);
      print_run (pp, cur_start_idx, cur_finish_idx,
		 &first_run);
    }
}

/* class exploded_node : public dnode<eg_traits>.  */

/* exploded_node's ctor.  */

exploded_node::exploded_node (const point_and_state &ps,
			      int index)
: m_ps (ps), m_status (STATUS_WORKLIST), m_index (index)
{
  gcc_checking_assert (ps.get_state ().m_region_model->canonicalized_p ());
}

/* For use by dump_dot, get a value for the .dot "fillcolor" attribute.
   Colorize by sm-state, to make it easier to see how sm-state propagates
   through the exploded_graph.  */

const char *
exploded_node::get_dot_fillcolor () const
{
  const program_state &state = get_state ();

  /* We want to be able to easily distinguish the no-sm-state case,
     and to be able to distinguish cases where there's a single state
     from each other.

     Sum the sm_states, and use the result to choose from a table,
     modulo table-size, special-casing the "no sm-state" case.   */
  int total_sm_state = 0;
  int i;
  sm_state_map *smap;
  FOR_EACH_VEC_ELT (state.m_checker_states, i, smap)
    {
      for (sm_state_map::iterator_t iter = smap->begin ();
	 iter != smap->end ();
	   ++iter)
	total_sm_state += (*iter).second.m_state;
      total_sm_state += smap->get_global_state ();
    }

  if (total_sm_state > 0)
    {
      /* An arbitrarily-picked collection of light colors.  */
      const char * const colors[]
	= {"azure", "coral", "cornsilk", "lightblue", "yellow"};
      const int num_colors = sizeof (colors) / sizeof (colors[0]);
      return colors[total_sm_state % num_colors];
    }
  else
    /* No sm-state.   */
    return "lightgrey";
}

/* Implementation of dnode::dump_dot vfunc for exploded_node.  */

void
exploded_node::dump_dot (graphviz_out *gv, const dump_args_t &args) const
{
  pretty_printer *pp = gv->get_pp ();

  dump_dot_id (pp);
  pp_printf (pp, " [shape=none,margin=0,style=filled,fillcolor=%s,label=\"",
	     get_dot_fillcolor ());
  pp_write_text_to_stream (pp);

  pp_printf (pp, "EN: %i", m_index);
  if (m_status == STATUS_MERGER)
    pp_string (pp, " (merger)");
  pp_newline (pp);

  format f (true);
  m_ps.get_point ().print (pp, f);
  pp_newline (pp);

  const extrinsic_state &ext_state = args.m_eg.get_ext_state ();
  const program_state &state = m_ps.get_state ();
  state.dump_to_pp (ext_state, true, pp);
  pp_newline (pp);

  {
    int i;
    sm_state_map *smap;
    FOR_EACH_VEC_ELT (state.m_checker_states, i, smap)
      {
	if (!smap->is_empty_p ())
	  {
	    pp_printf (pp, "%s: ", ext_state.get_name (i));
	    smap->print (ext_state.get_sm (i), state.m_region_model, pp);
	    pp_newline (pp);
	  }
      }
  }

  /* Dump any saved_diagnostics at this enode.  */
  {
    const diagnostic_manager &dm = args.m_eg.get_diagnostic_manager ();
    for (unsigned i = 0; i < dm.get_num_diagnostics (); i++)
      {
	const saved_diagnostic *sd = dm.get_saved_diagnostic (i);
	if (sd->m_enode == this)
	  {
	    pp_printf (pp, "DIAGNOSTIC: %s", sd->m_d->get_kind ());
	    pp_newline (pp);
	  }
      }
  }

  pp_write_text_as_dot_label_to_stream (pp, /*for_record=*/true);

  pp_string (pp, "\"];\n\n");
  pp_flush (pp);
}

/* Dump this to PP in a form suitable for use as an id in .dot output.  */

void
exploded_node::dump_dot_id (pretty_printer *pp) const
{
  pp_printf (pp, "exploded_node_%i", m_index);
}

/* Dump a multiline representation of this node to PP.  */

void
exploded_node::dump_to_pp (pretty_printer *pp,
			   const extrinsic_state &ext_state) const
{
  pp_printf (pp, "EN: %i", m_index);
  pp_newline (pp);

  format f (true);
  m_ps.get_point ().print (pp, f);
  pp_newline (pp);

  m_ps.get_state ().dump_to_pp (ext_state, false, pp);
  pp_newline (pp);
}

/* Dump a multiline representation of this node to FILE.  */

void
exploded_node::dump (FILE *fp,
		     const extrinsic_state &ext_state) const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_show_color (&pp) = pp_show_color (global_dc->printer);
  pp.buffer->stream = fp;
  dump_to_pp (&pp, ext_state);
  pp_flush (&pp);
}

/* Dump a multiline representation of this node to stderr.  */

DEBUG_FUNCTION void
exploded_node::dump (const extrinsic_state &ext_state) const
{
  dump (stderr, ext_state);
}

} // namespace ana

/* Return true if FNDECL has a gimple body.  */
// TODO: is there a pre-canned way to do this?

bool
fndecl_has_gimple_body_p (tree fndecl)
{
  if (fndecl == NULL_TREE)
    return false;

  cgraph_node *n = cgraph_node::get (fndecl);
  if (!n)
    return false;

  return n->has_gimple_body_p ();
}

namespace ana {

/* A pending_diagnostic subclass for implementing "__analyzer_dump_path".  */

class dump_path_diagnostic
  : public pending_diagnostic_subclass<dump_path_diagnostic>
{
public:
  bool emit (rich_location *richloc) FINAL OVERRIDE
  {
    inform (richloc, "path");
    return true;
  }

  const char *get_kind () const FINAL OVERRIDE { return "dump_path_diagnostic"; }

  bool operator== (const dump_path_diagnostic &) const
  {
    return true;
  }
};

/* Modify STATE in place, applying the effects of the stmt at this node's
   point.  */

exploded_node::on_stmt_flags
exploded_node::on_stmt (exploded_graph &eg,
			const supernode *snode,
			const gimple *stmt,
			program_state *state,
			state_change *change) const
{
  /* Preserve the old state.  It is used here for looking
     up old checker states, for determining state transitions, and
     also within impl_region_model_context and impl_sm_context for
     going from tree to svalue_id.  */
  const program_state old_state (*state);

  impl_region_model_context ctxt (eg, this,
				  &old_state, state, change,
				  stmt);

  if (const gassign *assign = dyn_cast <const gassign *> (stmt))
    state->m_region_model->on_assignment (assign, &ctxt);

  if (const greturn *return_ = dyn_cast <const greturn *> (stmt))
    state->m_region_model->on_return (return_, &ctxt);

  /* Track whether we have a gcall to a function that's not recognized by
     anything, for which we don't have a function body, or for which we
     don't know the fndecl.  */
  bool unknown_side_effects = false;
  if (const gcall *call = dyn_cast <const gcall *> (stmt))
    {
      /* Debugging/test support.  */
      if (is_special_named_call_p (call, "__analyzer_dump", 0))
	{
	  /* Handle the builtin "__analyzer_dump" by dumping state
	     to stderr.  */
	  dump (eg.get_ext_state ());
	}
      else if (is_special_named_call_p (call, "__analyzer_dump_path", 0))
	{
	  /* Handle the builtin "__analyzer_dump_path" by queuing a
	     diagnostic at this exploded_node.  */
	  ctxt.warn (new dump_path_diagnostic ());
	}
      else if (is_special_named_call_p (call, "__analyzer_dump_region_model", 0))
	{
	  /* Handle the builtin "__analyzer_dump_region_model" by dumping
	     the region model's state to stderr.  */
	  state->m_region_model->dump (false);
	}
      else if (is_special_named_call_p (call, "__analyzer_eval", 1))
	{
	  /* Handle the builtin "__analyzer_eval" by evaluating the input
	     and dumping as a dummy warning, so that test cases can use
	     dg-warning to validate the result (and so unexpected warnings will
	     lead to DejaGnu failures).  */
	  tree t_arg = gimple_call_arg (call, 0);
	  tristate t
	    = state->m_region_model->eval_condition (t_arg,
						     NE_EXPR,
						     integer_zero_node,
						     &ctxt);
	  warning_at (call->location, 0, "%s", t.as_string ());
	}
      else if (is_special_named_call_p (call, "__analyzer_break", 0))
	{
	  /* Handle the builtin "__analyzer_break" by triggering a
	     breakpoint.  */
	  /* TODO: is there a good cross-platform way to do this?  */
	  raise (SIGINT);
	}
      else if (is_special_named_call_p (call, "__analyzer_dump_exploded_nodes",
					1))
	{
	  /* This is handled elsewhere.  */
	}
      else if (is_setjmp_call_p (call))
	state->m_region_model->on_setjmp (call, this, &ctxt);
      else if (is_longjmp_call_p (call))
	{
	  on_longjmp (eg, call, state, &ctxt);
	  return on_stmt_flags::terminate_path ();
	}
      else
	unknown_side_effects = state->m_region_model->on_call_pre (call, &ctxt);
    }

  bool any_sm_changes = false;
  int sm_idx;
  sm_state_map *smap;
  FOR_EACH_VEC_ELT (old_state.m_checker_states, sm_idx, smap)
    {
      const state_machine &sm = eg.get_ext_state ().get_sm (sm_idx);
      const sm_state_map *old_smap
	= old_state.m_checker_states[sm_idx];
      sm_state_map *new_smap = state->m_checker_states[sm_idx];
      impl_sm_context sm_ctxt (eg, sm_idx, sm, this, &old_state, state,
			       change,
			       old_smap, new_smap);
      /* Allow the state_machine to handle the stmt.  */
      if (sm.on_stmt (&sm_ctxt, snode, stmt))
	unknown_side_effects = false;
      else
	{
	  /* For those stmts that were not handled by the state machine.  */
	  if (const gcall *call = dyn_cast <const gcall *> (stmt))
	    {
	      tree callee_fndecl
		= state->m_region_model->get_fndecl_for_call (call, &ctxt);

	      if (!fndecl_has_gimple_body_p (callee_fndecl))
		new_smap->purge_for_unknown_fncall (eg, sm, call, callee_fndecl,
						    state->m_region_model,
						    &ctxt);
	    }
	}
      if (*old_smap != *new_smap)
	any_sm_changes = true;
    }

  if (const gcall *call = dyn_cast <const gcall *> (stmt))
    state->m_region_model->on_call_post (call, unknown_side_effects, &ctxt);

  return on_stmt_flags (any_sm_changes);
}

/* Consider the effect of following superedge SUCC from this node.

   Return true if it's feasible to follow the edge, or false
   if it's infeasible.

   Examples: if it's the "true" branch within
   a CFG and we know the conditional is false, we know it's infeasible.
   If it's one of multiple interprocedual "return" edges, then only
   the edge back to the most recent callsite is feasible.

   Update NEXT_STATE accordingly (e.g. to record that a condition was
   true or false, or that the NULL-ness of a pointer has been checked,
   pushing/popping stack frames, etc).

   Update NEXT_POINT accordingly (updating the call string).  */

bool
exploded_node::on_edge (exploded_graph &eg,
			const superedge *succ,
			program_point *next_point,
			program_state *next_state,
			state_change *change) const
{
  LOG_FUNC (eg.get_logger ());

  if (!next_point->on_edge (eg, succ))
    return false;

  if (!next_state->on_edge (eg, *this, succ, change))
    return false;

  return true;
}

/* Verify that the stack at LONGJMP_POINT is still valid, given a call
   to "setjmp" at SETJMP_POINT - the stack frame that "setjmp" was
   called in must still be valid.

   Caveat: this merely checks the call_strings in the points; it doesn't
   detect the case where a frame returns and is then called again.  */

static bool
valid_longjmp_stack_p (const program_point &longjmp_point,
		       const program_point &setjmp_point)
{
  const call_string &cs_at_longjmp = longjmp_point.get_call_string ();
  const call_string &cs_at_setjmp = setjmp_point.get_call_string ();

  if (cs_at_longjmp.length () < cs_at_setjmp.length ())
    return false;

  /* Check that the call strings match, up to the depth of the
     setjmp point.  */
  for (unsigned depth = 0; depth < cs_at_setjmp.length (); depth++)
    if (cs_at_longjmp[depth] != cs_at_setjmp[depth])
      return false;

  return true;
}

/* A pending_diagnostic subclass for complaining about bad longjmps,
   where the enclosing function of the "setjmp" has returned (and thus
   the stack frame no longer exists).  */

class stale_jmp_buf : public pending_diagnostic_subclass<dump_path_diagnostic>
{
public:
  stale_jmp_buf (const gcall *setjmp_call, const gcall *longjmp_call)
  : m_setjmp_call (setjmp_call), m_longjmp_call (longjmp_call)
  {}

  bool emit (rich_location *richloc) FINAL OVERRIDE
  {
    return warning_at
      (richloc, OPT_Wanalyzer_stale_setjmp_buffer,
       "%qs called after enclosing function of %qs has returned",
       get_user_facing_name (m_longjmp_call),
       get_user_facing_name (m_setjmp_call));
  }

  const char *get_kind () const FINAL OVERRIDE
  { return "stale_jmp_buf"; }

  bool operator== (const stale_jmp_buf &other) const
  {
    return (m_setjmp_call == other.m_setjmp_call
	    && m_longjmp_call == other.m_longjmp_call);
  }

private:
  const gcall *m_setjmp_call;
  const gcall *m_longjmp_call;
};

/* Handle LONGJMP_CALL, a call to longjmp or siglongjmp.

   Attempt to locate where setjmp/sigsetjmp was called on the jmp_buf and build
   an exploded_node and exploded_edge to it representing a rewind to that frame,
   handling the various kinds of failure that can occur.  */

void
exploded_node::on_longjmp (exploded_graph &eg,
			   const gcall *longjmp_call,
			   program_state *new_state,
			   region_model_context *ctxt) const
{
  tree buf_ptr = gimple_call_arg (longjmp_call, 0);

  region_model *new_region_model = new_state->m_region_model;
  region_id buf_rid = new_region_model->deref_rvalue (buf_ptr, ctxt);
  region *buf = new_region_model->get_region (buf_rid);
  if (!buf)
    return;

  svalue_id buf_content_sid
    = buf->get_value (*new_region_model, false, ctxt);
  svalue *buf_content_sval = new_region_model->get_svalue (buf_content_sid);
  if (!buf_content_sval)
    return;
  setjmp_svalue *setjmp_sval = buf_content_sval->dyn_cast_setjmp_svalue ();
  if (!setjmp_sval)
    return;

  const setjmp_record tmp_setjmp_record = setjmp_sval->get_setjmp_record ();

  /* Build a custom enode and eedge for rewinding from the longjmp/siglongjmp
     call back to the setjmp/sigsetjmp.  */
  rewind_info_t rewind_info (tmp_setjmp_record, longjmp_call);

  const gcall *setjmp_call = rewind_info.get_setjmp_call ();
  const program_point &setjmp_point = rewind_info.get_setjmp_point ();

  const program_point &longjmp_point = get_point ();

  /* Verify that the setjmp's call_stack hasn't been popped.  */
  if (!valid_longjmp_stack_p (longjmp_point, setjmp_point))
    {
      ctxt->warn (new stale_jmp_buf (setjmp_call, longjmp_call));
      return;
    }

  gcc_assert (longjmp_point.get_stack_depth ()
	      >= setjmp_point.get_stack_depth ());

  /* Update the state for use by the destination node.  */

  /* Stash the current number of diagnostics so that we can update
     any that this adds to show where the longjmp is rewinding to.  */

  diagnostic_manager *dm = &eg.get_diagnostic_manager ();
  unsigned prev_num_diagnostics = dm->get_num_diagnostics ();

  new_region_model->on_longjmp (longjmp_call, setjmp_call,
				setjmp_point.get_stack_depth (), ctxt);

  program_point next_point
    = program_point::after_supernode (setjmp_point.get_supernode (),
				      setjmp_point.get_call_string ());

  state_change change;
  exploded_node *next = eg.get_or_create_node (next_point, *new_state, &change);

  /* Create custom exploded_edge for a longjmp.  */
  if (next)
    {
      exploded_edge *eedge
	= eg.add_edge (const_cast<exploded_node *> (this), next, NULL,
		       change,
		       new rewind_info_t (tmp_setjmp_record, longjmp_call));

      /* For any diagnostics that were queued here (such as leaks) we want
	 the checker_path to show the rewinding events after the "final event"
	 so that the user sees where the longjmp is rewinding to (otherwise the
	 path is meaningless).

	 For example, we want to emit something like:
                        |   NN | {
                        |   NN |   longjmp (env, 1);
                        |      |   ~~~~~~~~~~~~~~~~
                        |      |   |
                        |      |   (10) 'ptr' leaks here; was allocated at (7)
                        |      |   (11) rewinding from 'longjmp' in 'inner'...
                        |
          <-------------+
          |
        'outer': event 12
          |
          |   NN |   i = setjmp(env);
          |      |       ^~~~~~
          |      |       |
          |      |       (12) ...to 'setjmp' in 'outer' (saved at (2))

	 where the "final" event above is event (10), but we want to append
	 events (11) and (12) afterwards.

	 Do this by setting m_trailing_eedge on any diagnostics that were
	 just saved.  */
      unsigned num_diagnostics = dm->get_num_diagnostics ();
      for (unsigned i = prev_num_diagnostics; i < num_diagnostics; i++)
	{
	  saved_diagnostic *sd = dm->get_saved_diagnostic (i);
	  sd->m_trailing_eedge = eedge;
	}
    }
}

/* Subroutine of exploded_graph::process_node for finding the successors
   of the supernode for a function exit basic block.

   Ensure that pop_frame is called, potentially queuing diagnostics about
   leaks.  */

void
exploded_node::detect_leaks (exploded_graph &eg) const
{
  LOG_FUNC_1 (eg.get_logger (), "EN: %i", m_index);

  gcc_assert (get_point ().get_supernode ()->return_p ());

  /* If we're not a "top-level" function, do nothing; pop_frame
     will be called when handling the return superedge.  */
  if (get_point ().get_stack_depth () > 1)
    return;

  /* We have a "top-level" function.  */
  gcc_assert (get_point ().get_stack_depth () == 1);

  const program_state &old_state = get_state ();

  /* Work with a temporary copy of the state: pop the frame, and see
     what leaks (via purge_unused_svalues).  */
  program_state new_state (old_state);

  gcc_assert (new_state.m_region_model);

  purge_stats stats;
  impl_region_model_context ctxt (eg, this,
				  &old_state, &new_state,
				  NULL,
				  get_stmt ());
  new_state.m_region_model->pop_frame (region_id::null (),
				       true, &stats, &ctxt);
}

/* Dump the successors and predecessors of this enode to OUTF.  */

void
exploded_node::dump_succs_and_preds (FILE *outf) const
{
  unsigned i;
  exploded_edge *e;
  {
    auto_vec<exploded_node *> preds (m_preds.length ());
    FOR_EACH_VEC_ELT (m_preds, i, e)
      preds.quick_push (e->m_src);
    pretty_printer pp;
    print_enode_indices (&pp, preds);
    fprintf (outf, "preds: %s\n",
	     pp_formatted_text (&pp));
  }
  {
    auto_vec<exploded_node *> succs (m_succs.length ());
    FOR_EACH_VEC_ELT (m_succs, i, e)
      succs.quick_push (e->m_dest);
    pretty_printer pp;
    print_enode_indices (&pp, succs);
    fprintf (outf, "succs: %s\n",
	     pp_formatted_text (&pp));
  }
}

/* class rewind_info_t : public exploded_edge::custom_info_t.  */

/* Implementation of exploded_edge::custom_info_t::update_model vfunc
   for rewind_info_t.

   Update state for the special-case of a rewind of a longjmp
   to a setjmp (which doesn't have a superedge, but does affect
   state).  */

void
rewind_info_t::update_model (region_model *model,
			     const exploded_edge &eedge)
{
  const program_point &longjmp_point = eedge.m_src->get_point ();
  const program_point &setjmp_point = eedge.m_dest->get_point ();

  gcc_assert (longjmp_point.get_stack_depth ()
	      >= setjmp_point.get_stack_depth ());

  model->on_longjmp (get_longjmp_call (),
		     get_setjmp_call (),
		     setjmp_point.get_stack_depth (), NULL);
}

/* Implementation of exploded_edge::custom_info_t::add_events_to_path vfunc
   for rewind_info_t.  */

void
rewind_info_t::add_events_to_path (checker_path *emission_path,
				   const exploded_edge &eedge)
{
  const exploded_node *src_node = eedge.m_src;
  const program_point &src_point = src_node->get_point ();
  const int src_stack_depth = src_point.get_stack_depth ();
  const exploded_node *dst_node = eedge.m_dest;
  const program_point &dst_point = dst_node->get_point ();
  const int dst_stack_depth = dst_point.get_stack_depth ();

  emission_path->add_event
    (new rewind_from_longjmp_event
     (&eedge, get_longjmp_call ()->location,
      src_point.get_fndecl (),
      src_stack_depth, this));
  emission_path->add_event
    (new rewind_to_setjmp_event
     (&eedge, get_setjmp_call ()->location,
      dst_point.get_fndecl (),
      dst_stack_depth, this));
}

/* class exploded_edge : public dedge<eg_traits>.  */

/* exploded_edge's ctor.  */

exploded_edge::exploded_edge (exploded_node *src, exploded_node *dest,
			      const extrinsic_state &ext_state,
			      const superedge *sedge,
			      const state_change &change,
			      custom_info_t *custom_info)
: dedge<eg_traits> (src, dest), m_sedge (sedge), m_change (change),
  m_custom_info (custom_info)
{
  change.validate (dest->get_state (), ext_state);
}

/* exploded_edge's dtor.  */

exploded_edge::~exploded_edge ()
{
  delete m_custom_info;
}

/* Implementation of dedge::dump_dot vfunc for exploded_edge.
   Use the label of the underlying superedge, if any.  */

void
exploded_edge::dump_dot (graphviz_out *gv, const dump_args_t &args) const
{
  pretty_printer *pp = gv->get_pp ();

  const char *style = "\"solid,bold\"";
  const char *color = "black";
  int weight = 10;
  const char *constraint = "true";

  if (m_sedge)
    switch (m_sedge->m_kind)
      {
      default:
	gcc_unreachable ();
      case SUPEREDGE_CFG_EDGE:
	break;
      case SUPEREDGE_CALL:
	color = "red";
	//constraint = "false";
	break;
      case SUPEREDGE_RETURN:
	color = "green";
	//constraint = "false";
	break;
      case SUPEREDGE_INTRAPROCEDURAL_CALL:
	style = "\"dotted\"";
	break;
      }
  if (m_custom_info)
    {
      color = "red";
      style = "\"dotted\"";
    }

  m_src->dump_dot_id (pp);
  pp_string (pp, " -> ");
  m_dest->dump_dot_id (pp);
  pp_printf (pp,
	     (" [style=%s, color=%s, weight=%d, constraint=%s,"
	      " headlabel=\""),
	     style, color, weight, constraint);

  if (m_sedge)
    m_sedge->dump_label_to_pp (pp, false);
  else if (m_custom_info)
    m_custom_info->print (pp);

  m_change.dump (pp, args.m_eg.get_ext_state ());
  //pp_write_text_as_dot_label_to_stream (pp, /*for_record=*/false);

  pp_printf (pp, "\"];\n");
}

/* struct stats.  */

/* stats' ctor.  */

stats::stats (int num_supernodes)
: m_node_reuse_count (0),
  m_node_reuse_after_merge_count (0),
  m_num_supernodes (num_supernodes)
{
  for (int i = 0; i < NUM_POINT_KINDS; i++)
    m_num_nodes[i] = 0;
}

/* Log these stats in multiline form to LOGGER.  */

void
stats::log (logger *logger) const
{
  gcc_assert (logger);
  for (int i = 0; i < NUM_POINT_KINDS; i++)
    if (m_num_nodes[i] > 0)
      logger->log ("m_num_nodes[%s]: %i",
		   point_kind_to_string (static_cast <enum point_kind> (i)),
		   m_num_nodes[i]);
  logger->log ("m_node_reuse_count: %i", m_node_reuse_count);
  logger->log ("m_node_reuse_after_merge_count: %i",
	       m_node_reuse_after_merge_count);
}

/* Dump these stats in multiline form to OUT.  */

void
stats::dump (FILE *out) const
{
  for (int i = 0; i < NUM_POINT_KINDS; i++)
    if (m_num_nodes[i] > 0)
      fprintf (out, "m_num_nodes[%s]: %i\n",
	       point_kind_to_string (static_cast <enum point_kind> (i)),
	       m_num_nodes[i]);
  fprintf (out, "m_node_reuse_count: %i\n", m_node_reuse_count);
  fprintf (out, "m_node_reuse_after_merge_count: %i\n",
	   m_node_reuse_after_merge_count);

  if (m_num_supernodes > 0)
    fprintf (out, "PK_AFTER_SUPERNODE nodes per supernode: %.2f\n",
	     (float)m_num_nodes[PK_AFTER_SUPERNODE] / (float)m_num_supernodes);
}

/* Return the total number of enodes recorded within this object.  */

int
stats::get_total_enodes () const
{
  int result = 0;
  for (int i = 0; i < NUM_POINT_KINDS; i++)
    result += m_num_nodes[i];
  return result;
}

/* strongly_connected_components's ctor.  Tarjan's SCC algorithm.  */

strongly_connected_components::
strongly_connected_components (const supergraph &sg, logger *logger)
: m_sg (sg), m_per_node (m_sg.num_nodes ())
{
  LOG_SCOPE (logger);
  auto_timevar tv (TV_ANALYZER_SCC);

  for (int i = 0; i < m_sg.num_nodes (); i++)
    m_per_node.quick_push (per_node_data ());

  for (int i = 0; i < m_sg.num_nodes (); i++)
    if (m_per_node[i].m_index == -1)
      strong_connect (i);

  if (0)
    dump ();
}

/* Dump this object to stderr.  */

DEBUG_FUNCTION void
strongly_connected_components::dump () const
{
  for (int i = 0; i < m_sg.num_nodes (); i++)
    {
      const per_node_data &v = m_per_node[i];
      fprintf (stderr, "SN %i: index: %i lowlink: %i on_stack: %i\n",
	       i, v.m_index, v.m_lowlink, v.m_on_stack);
    }
}

/* Subroutine of strongly_connected_components's ctor, part of Tarjan's
   SCC algorithm.  */

void
strongly_connected_components::strong_connect (unsigned index)
{
  supernode *v_snode = m_sg.get_node_by_index (index);

  /* Set the depth index for v to the smallest unused index.  */
  per_node_data *v = &m_per_node[index];
  v->m_index = index;
  v->m_lowlink = index;
  m_stack.safe_push (index);
  v->m_on_stack = true;
  index++;

  /* Consider successors of v.  */
  unsigned i;
  superedge *sedge;
  FOR_EACH_VEC_ELT (v_snode->m_succs, i, sedge)
    {
      supernode *w_snode = sedge->m_dest;
      per_node_data *w = &m_per_node[w_snode->m_index];
      if (w->m_index == -1)
	{
	  /* Successor w has not yet been visited; recurse on it.  */
	  strong_connect (w_snode->m_index);
	  v->m_lowlink = MIN (v->m_lowlink, w->m_lowlink);
	}
      else if (w->m_on_stack)
	{
	  /* Successor w is in stack S and hence in the current SCC
	     If w is not on stack, then (v, w) is a cross-edge in the DFS
	     tree and must be ignored.  */
	  v->m_lowlink = MIN (v->m_lowlink, w->m_index);
	}
    }

  /* If v is a root node, pop the stack and generate an SCC.  */

  if (v->m_lowlink == v->m_index)
    {
      per_node_data *w;
      do {
	int idx = m_stack.pop ();
	w = &m_per_node[idx];
	w->m_on_stack = false;
      } while (w != v);
    }
}

/* worklist's ctor.  */

worklist::worklist (const exploded_graph &eg, const analysis_plan &plan)
: m_scc (eg.get_supergraph (), eg.get_logger ()),
  m_plan (plan),
  m_queue (key_t (*this, NULL))
{
}

/* Return the number of nodes in the worklist.  */

unsigned
worklist::length () const
{
  return m_queue.nodes ();
}

/* Return the next node in the worklist, removing it.  */

exploded_node *
worklist::take_next ()
{
  return m_queue.extract_min ();
}

/* Return the next node in the worklist without removing it.  */

exploded_node *
worklist::peek_next ()
{
  return m_queue.min ();
}

/* Add ENODE to the worklist.  */

void
worklist::add_node (exploded_node *enode)
{
  gcc_assert (enode->get_status () == exploded_node::STATUS_WORKLIST);
  m_queue.insert (key_t (*this, enode), enode);
}

/* Comparator for implementing worklist::key_t comparison operators.
   Return negative if KA is before KB
   Return positive if KA is after KB
   Return 0 if they are equal.  */

int
worklist::key_t::cmp (const worklist::key_t &ka, const worklist::key_t &kb)
{
  const program_point &point_a = ka.m_enode->get_point ();
  const program_point &point_b = kb.m_enode->get_point ();
  const call_string &call_string_a = point_a.get_call_string ();
  const call_string &call_string_b = point_b.get_call_string ();

  /* Order empty-callstring points with different functions based on the
     analysis_plan, so that we generate summaries before they are used.  */
  if (flag_analyzer_call_summaries
      && call_string_a.empty_p ()
      && call_string_b.empty_p ()
      && point_a.get_function () != NULL
      && point_b.get_function () != NULL
      && point_a.get_function () != point_b.get_function ())
    {
      return ka.m_worklist.m_plan.cmp_function (point_a.get_function (),
						point_b.get_function ());
    }

  /* First, order by SCC.  */
  int scc_id_a = ka.get_scc_id (ka.m_enode);
  int scc_id_b = kb.get_scc_id (kb.m_enode);
  if (scc_id_a != scc_id_b)
    return scc_id_a - scc_id_b;

  /* If in same SCC, order by supernode index (an arbitrary but stable
     ordering).  */
  const supernode *snode_a = ka.m_enode->get_supernode ();
  const supernode *snode_b = kb.m_enode->get_supernode ();
  if (snode_a == NULL)
    {
      if (snode_b != NULL)
	/* One is NULL.  */
	return -1;
      else
	/* Both are NULL.  */
	return 0;
    }
  if (snode_b == NULL)
    /* One is NULL.  */
    return 1;
  /* Neither are NULL.  */
  gcc_assert (snode_a && snode_b);
  if (snode_a->m_index != snode_b->m_index)
    return snode_a->m_index - snode_b->m_index;

  gcc_assert (snode_a == snode_b);

  /* Order within supernode via program point.  */
  int within_snode_cmp
    = function_point::cmp_within_supernode (point_a.get_function_point (),
					    point_b.get_function_point ());
  if (within_snode_cmp)
    return within_snode_cmp;

  /* The points might vary by callstring; try sorting by callstring.  */
  int cs_cmp = call_string::cmp (call_string_a, call_string_b);
  if (cs_cmp)
    return cs_cmp;

  /* Otherwise, we ought to have the same program_point.  */
  gcc_assert (point_a == point_b);

  const program_state &state_a = ka.m_enode->get_state ();
  const program_state &state_b = kb.m_enode->get_state ();

  /* Sort by sm-state, so that identical sm-states are grouped
     together in the worklist.
     For now, sort by the hash value (might not be deterministic).  */
  for (unsigned sm_idx = 0; sm_idx < state_a.m_checker_states.length ();
       ++sm_idx)
    {
      sm_state_map *smap_a = state_a.m_checker_states[sm_idx];
      sm_state_map *smap_b = state_b.m_checker_states[sm_idx];

      hashval_t hash_a = smap_a->hash ();
      hashval_t hash_b = smap_b->hash ();
      if (hash_a < hash_b)
	return -1;
      else if (hash_a > hash_b)
	return 1;
    }

  /* Otherwise, we have two enodes at the same program point but with
     different states.  We don't have a good total ordering on states,
     so order them by enode index, so that we have at least have a
     stable sort.  */
  return ka.m_enode->m_index - kb.m_enode->m_index;
}

/* exploded_graph's ctor.  */

exploded_graph::exploded_graph (const supergraph &sg, logger *logger,
				const extrinsic_state &ext_state,
				const state_purge_map *purge_map,
				const analysis_plan &plan,
				int verbosity)
: m_sg (sg), m_logger (logger),
  m_worklist (*this, plan),
  m_ext_state (ext_state),
  m_purge_map (purge_map),
  m_plan (plan),
  m_diagnostic_manager (logger, verbosity),
  m_global_stats (m_sg.num_nodes ()),
  m_functionless_stats (m_sg.num_nodes ()),
  m_PK_AFTER_SUPERNODE_per_snode (m_sg.num_nodes ())
{
  m_origin = get_or_create_node (program_point (function_point (NULL, NULL,
								0, PK_ORIGIN),
						call_string ()),
				 program_state (ext_state), NULL);
  for (int i = 0; i < m_sg.num_nodes (); i++)
    m_PK_AFTER_SUPERNODE_per_snode.quick_push (i);
}

/* exploded_graph's dtor.  */

exploded_graph::~exploded_graph ()
{
  for (function_stat_map_t::iterator iter = m_per_function_stats.begin ();
       iter != m_per_function_stats.end ();
       ++iter)
    delete (*iter).second;

  for (point_map_t::iterator iter = m_per_point_data.begin ();
       iter != m_per_point_data.end ();
       ++iter)
    delete (*iter).second;
}

/* Ensure that there is an exploded_node representing an external call to
   FUN, adding it to the worklist if creating it.

   Add an edge from the origin exploded_node to the function entrypoint
   exploded_node.

   Return the exploded_node for the entrypoint to the function.  */

exploded_node *
exploded_graph::add_function_entry (function *fun)
{
  program_point point = program_point::from_function_entry (m_sg, fun);
  program_state state (m_ext_state);
  impl_region_model_context ctxt (&state, NULL, m_ext_state, get_logger ());
  state.m_region_model->push_frame (fun, NULL, &ctxt);

  if (!state.m_valid)
    return NULL;

  exploded_node *enode = get_or_create_node (point, state, NULL);
  /* We should never fail to add such a node.  */
  gcc_assert (enode);
  state_change change;
  add_edge (m_origin, enode, NULL, change);
  return enode;
}

/* Get or create an exploded_node for (POINT, STATE).
   If a new node is created, it is added to the worklist.
   If CHANGE is non-NULL, use it to suppress some purging of state,
   to make generation of state_change_event instances easier.  */

exploded_node *
exploded_graph::get_or_create_node (const program_point &point,
				    const program_state &state,
				    state_change *change)
{
  logger * const logger = get_logger ();
  LOG_FUNC (logger);
  if (logger)
    {
      format f (false);
      pretty_printer *pp = logger->get_printer ();
      logger->start_log_line ();
      pp_string (pp, "point: ");
      point.print (pp, f);
      logger->end_log_line ();
      logger->start_log_line ();
      pp_string (pp, "state: ");
      state.dump_to_pp (m_ext_state, true, pp);
      logger->end_log_line ();
    }

  /* Stop exploring paths for which we don't know how to effectively
     model the state.  */
  if (!state.m_valid)
    {
      if (logger)
	logger->log ("invalid state; not creating node");
      return NULL;
    }

  auto_cfun sentinel (point.get_function ());

  state.validate (get_ext_state ());

  //state.dump (get_ext_state ());

  /* Prune state to try to improve the chances of a cache hit,
     avoiding generating redundant nodes.  */
  program_state pruned_state = state.prune_for_point (*this, point, change);

  pruned_state.validate (get_ext_state ());

  //pruned_state.dump (get_ext_state ());

  if (logger)
    {
      pretty_printer *pp = logger->get_printer ();
      logger->start_log_line ();
      pp_string (pp, "pruned_state: ");
      pruned_state.dump_to_pp (m_ext_state, true, pp);
      logger->end_log_line ();
      pruned_state.m_region_model->dump_to_pp (logger->get_printer (), true);
    }

  stats *per_fn_stats = get_or_create_function_stats (point.get_function ());

  stats *per_cs_stats
    = &get_or_create_per_call_string_data (point.get_call_string ())->m_stats;

  point_and_state ps (point, pruned_state);
  ps.validate (m_ext_state);
  if (exploded_node **slot = m_point_and_state_to_node.get (&ps))
    {
      /* An exploded_node for PS already exists.  */
      if (logger)
	logger->log ("reused EN: %i", (*slot)->m_index);
      m_global_stats.m_node_reuse_count++;
      per_fn_stats->m_node_reuse_count++;
      per_cs_stats->m_node_reuse_count++;
      return *slot;
    }

  per_program_point_data *per_point_data
    = get_or_create_per_program_point_data (point);

  /* Consider merging state with another enode at this program_point.  */
  if (flag_analyzer_state_merge)
    {
      exploded_node *existing_enode;
      unsigned i;
      FOR_EACH_VEC_ELT (per_point_data->m_enodes, i, existing_enode)
	{
	  if (logger)
	    logger->log ("considering merging with existing EN: %i for point",
			 existing_enode->m_index);
	  gcc_assert (existing_enode->get_point () == point);
	  const program_state &existing_state = existing_enode->get_state ();

	  /* This merges successfully within the loop.  */

	  program_state merged_state (m_ext_state);
	  if (pruned_state.can_merge_with_p (existing_state, m_ext_state,
					     &merged_state))
	    {
	      if (logger)
		logger->log ("merging new state with that of EN: %i",
			     existing_enode->m_index);

	      /* Try again for a cache hit.
		 Whether we get one or not, merged_state's value_ids have no
		 relationship to those of the input state, and thus to those
		 of CHANGE, so we must purge any svalue_ids from *CHANGE.  */
	      ps.set_state (merged_state);
	      if (change)
		change->on_svalue_purge (svalue_id::from_int (0));

	      if (exploded_node **slot = m_point_and_state_to_node.get (&ps))
		{
		  /* An exploded_node for PS already exists.  */
		  if (logger)
		    logger->log ("reused EN: %i", (*slot)->m_index);
		  m_global_stats.m_node_reuse_after_merge_count++;
		  per_fn_stats->m_node_reuse_after_merge_count++;
		  per_cs_stats->m_node_reuse_after_merge_count++;
		  return *slot;
		}
	    }
	  else
	    if (logger)
	      logger->log ("not merging new state with that of EN: %i",
			   existing_enode->m_index);
	}
    }

  /* Impose a limit on the number of enodes per program point, and
     simply stop if we exceed it.  */
  if ((int)per_point_data->m_enodes.length ()
      > param_analyzer_max_enodes_per_program_point)
    {
      if (logger)
	logger->log ("not creating enode; too many at program point");
      warning_at (point.get_location (), OPT_Wanalyzer_too_complex,
		  "terminating analysis for this program point");
      per_point_data->m_excess_enodes++;
      return NULL;
    }

  ps.validate (m_ext_state);

  /* An exploded_node for "ps" doesn't already exist; create one.  */
  exploded_node *node = new exploded_node (ps, m_nodes.length ());
  add_node (node);
  m_point_and_state_to_node.put (node->get_ps_key (), node);

  /* Update per-program_point data.  */
  per_point_data->m_enodes.safe_push (node);

  const enum point_kind node_pk = node->get_point ().get_kind ();
  m_global_stats.m_num_nodes[node_pk]++;
  per_fn_stats->m_num_nodes[node_pk]++;
  per_cs_stats->m_num_nodes[node_pk]++;

  if (node_pk == PK_AFTER_SUPERNODE)
    m_PK_AFTER_SUPERNODE_per_snode[point.get_supernode ()->m_index]++;

  if (logger)
    {
      format f (false);
      pretty_printer *pp = logger->get_printer ();
      logger->log ("created EN: %i", node->m_index);
      logger->start_log_line ();
      pp_string (pp, "point: ");
      point.print (pp, f);
      logger->end_log_line ();
      logger->start_log_line ();
      pp_string (pp, "pruned_state: ");
      pruned_state.dump_to_pp (m_ext_state, true, pp);
      logger->end_log_line ();
    }

  /* Add the new node to the worlist.  */
  m_worklist.add_node (node);
  return node;
}

/* Add an exploded_edge from SRC to DEST, recording its association
   with SEDGE (which may be NULL), and, if non-NULL, taking ownership
   of REWIND_INFO.
   Return the newly-created eedge.  */

exploded_edge *
exploded_graph::add_edge (exploded_node *src, exploded_node *dest,
			  const superedge *sedge,
			  const state_change &change,
			  exploded_edge::custom_info_t *custom_info)
{
  exploded_edge *e = new exploded_edge (src, dest, m_ext_state,
					sedge, change, custom_info);
  digraph<eg_traits>::add_edge (e);
  return e;
}

/* Ensure that this graph has per-program_point-data for POINT;
   borrow a pointer to it.  */

per_program_point_data *
exploded_graph::
get_or_create_per_program_point_data (const program_point &point)
{
  if (per_program_point_data **slot = m_per_point_data.get (&point))
    return *slot;

  per_program_point_data *per_point_data = new per_program_point_data (point);
  m_per_point_data.put (&per_point_data->m_key, per_point_data);
  return per_point_data;
}

/* Ensure that this graph has per-call_string-data for CS;
   borrow a pointer to it.  */

per_call_string_data *
exploded_graph::get_or_create_per_call_string_data (const call_string &cs)
{
  if (per_call_string_data **slot = m_per_call_string_data.get (&cs))
    return *slot;

  per_call_string_data *data = new per_call_string_data (cs, m_sg.num_nodes ());
  m_per_call_string_data.put (&data->m_key,
			      data);
  return data;
}

/* Ensure that this graph has per-function-data for FUN;
   borrow a pointer to it.  */

per_function_data *
exploded_graph::get_or_create_per_function_data (function *fun)
{
  if (per_function_data **slot = m_per_function_data.get (fun))
    return *slot;

  per_function_data *data = new per_function_data ();
  m_per_function_data.put (fun, data);
  return data;
}

/* Get this graph's per-function-data for FUN if there is any,
   otherwise NULL.  */

per_function_data *
exploded_graph::get_per_function_data (function *fun) const
{
  if (per_function_data **slot
        = const_cast <per_function_data_t &> (m_per_function_data).get (fun))
    return *slot;

  return NULL;
}

/* Return true if NODE and FUN should be traversed directly, rather than
   called via other functions.  */

static bool
toplevel_function_p (cgraph_node *node, function *fun, logger *logger)
{
  /* TODO: better logic here
     e.g. only if more than one caller, and significantly complicated.
     Perhaps some whole-callgraph analysis to decide if it's worth summarizing
     an edge, and if so, we need summaries.  */
  if (flag_analyzer_call_summaries)
    {
      int num_call_sites = 0;
      for (cgraph_edge *edge = node->callers; edge; edge = edge->next_caller)
	++num_call_sites;

      /* For now, if there's more than one in-edge, and we want call
	 summaries, do it at the top level so that there's a chance
	 we'll have a summary when we need one.  */
      if (num_call_sites > 1)
	{
	  if (logger)
	    logger->log ("traversing %qE (%i call sites)",
			 fun->decl, num_call_sites);
	  return true;
	}
    }

  if (!TREE_PUBLIC (fun->decl))
    {
      if (logger)
	logger->log ("not traversing %qE (static)", fun->decl);
      return false;
    }

  if (logger)
    logger->log ("traversing %qE (all checks passed)", fun->decl);

  return true;
}

/* Add initial nodes to EG, with entrypoints for externally-callable
   functions.  */

void
exploded_graph::build_initial_worklist ()
{
  logger * const logger = get_logger ();
  LOG_SCOPE (logger);

  cgraph_node *node;
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (node)
  {
    function *fun = node->get_fun ();
    if (!toplevel_function_p (node, fun, logger))
      continue;
    exploded_node *enode = add_function_entry (fun);
    if (logger)
      {
	if (enode)
	  logger->log ("created EN %i for %qE entrypoint",
		       enode->m_index, fun->decl);
	else
	  logger->log ("did not create enode for %qE entrypoint", fun->decl);
      }
  }
}

/* The main loop of the analysis.
   Take freshly-created exploded_nodes from the worklist, calling
   process_node on them to explore the <point, state> graph.
   Add edges to their successors, potentially creating new successors
   (which are also added to the worklist).  */

void
exploded_graph::process_worklist ()
{
  logger * const logger = get_logger ();
  LOG_SCOPE (logger);
  auto_timevar tv (TV_ANALYZER_WORKLIST);

  while (m_worklist.length () > 0)
    {
      exploded_node *node = m_worklist.take_next ();
      gcc_assert (node->get_status () == exploded_node::STATUS_WORKLIST);
      gcc_assert (node->m_succs.length () == 0
		  || node == m_origin);

      if (logger)
	logger->log ("next to process: EN: %i", node->m_index);

      /* Avoid exponential explosions of nodes by attempting to merge
	 nodes that are at the same program point and which have
	 sufficiently similar state.  */
      if (flag_analyzer_state_merge && node != m_origin)
	if (exploded_node *node_2 = m_worklist.peek_next ())
	  {
	    gcc_assert (node_2->get_status ()
			== exploded_node::STATUS_WORKLIST);
	    gcc_assert (node->m_succs.length () == 0);
	    gcc_assert (node_2->m_succs.length () == 0);

	    gcc_assert (node != node_2);

	    if (logger)
	      logger->log ("peek worklist: EN: %i", node_2->m_index);

	    if (node->get_point () == node_2->get_point ())
	      {
		if (logger)
		  {
		    format f (false);
		    pretty_printer *pp = logger->get_printer ();
		    logger->start_log_line ();
		    logger->log_partial
		      ("got potential merge EN: %i and EN: %i at ",
		       node->m_index, node_2->m_index);
		    node->get_point ().print (pp, f);
		    logger->end_log_line ();
		  }

		const program_state &state = node->get_state ();
		const program_state &state_2 = node_2->get_state ();

		/* They shouldn't be equal, or we wouldn't have two
		   separate nodes.  */
		gcc_assert (state != state_2);

		program_state merged_state (m_ext_state);
		state_change change;
		if (state.can_merge_with_p (state_2, m_ext_state,
					    &merged_state))
		  {
		    if (logger)
		      logger->log ("merging EN: %i and EN: %i",
				   node->m_index, node_2->m_index);

		    if (merged_state == state)
		      {
			/* Then merge node_2 into node by adding an edge.  */
			add_edge (node_2, node, NULL, change);

			/* Remove node_2 from the worklist.  */
			m_worklist.take_next ();
			node_2->set_status (exploded_node::STATUS_MERGER);

			/* Continue processing "node" below.  */
		      }
		    else if (merged_state == state_2)
		      {
			/* Then merge node into node_2, and leave node_2
			   in the worklist, to be processed on the next
			   iteration.  */
			add_edge (node, node_2, NULL, change);
			node->set_status (exploded_node::STATUS_MERGER);
			continue;
		      }
		    else
		      {
			/* We have a merged state that differs from
			   both state and state_2.  */

			/* Remove node_2 from the worklist.  */
			m_worklist.take_next ();

			/* Create (or get) an exploded node for the merged
			   states, adding to the worklist.  */
			exploded_node *merged_enode
			  = get_or_create_node (node->get_point (),
						merged_state, &change);
			if (merged_enode == NULL)
			  continue;

			if (logger)
			  logger->log ("merged EN: %i and EN: %i into EN: %i",
				       node->m_index, node_2->m_index,
				       merged_enode->m_index);

			/* "node" and "node_2" have both now been removed
			   from the worklist; we should not process them.

			   "merged_enode" may be a new node; if so it will be
			   processed in a subsequent iteration.
			   Alternatively, "merged_enode" could be an existing
			   node; one way the latter can
			   happen is if we end up merging a succession of
			   similar nodes into one.  */

			/* If merged_node is one of the two we were merging,
			   add it back to the worklist to ensure it gets
			   processed.

			   Add edges from the merged nodes to it (but not a
			   self-edge).  */
			if (merged_enode == node)
			  m_worklist.add_node (merged_enode);
			else
			  {
			    add_edge (node, merged_enode, NULL, change);
			    node->set_status (exploded_node::STATUS_MERGER);
			  }

			if (merged_enode == node_2)
			  m_worklist.add_node (merged_enode);
			else
			  {
			    add_edge (node_2, merged_enode, NULL, change);
			    node_2->set_status (exploded_node::STATUS_MERGER);
			  }

			continue;
		      }
		  }

		/* TODO: should we attempt more than two nodes,
		   or just do pairs of nodes?  (and hope that we get
		   a cascade of mergers).  */
	      }
	}

      process_node (node);

      /* Impose a hard limit on the number of exploded nodes, to ensure
	 that the analysis terminates in the face of pathological state
	 explosion (or bugs).

	 Specifically, the limit is on the number of PK_AFTER_SUPERNODE
	 exploded nodes, looking at supernode exit events.

	 We use exit rather than entry since there can be multiple
	 entry ENs, one per phi; the number of PK_AFTER_SUPERNODE ought
	 to be equivalent to the number of supernodes multiplied by the
	 number of states.  */
      const int limit = m_sg.num_nodes () * param_analyzer_bb_explosion_factor;
      if (m_global_stats.m_num_nodes[PK_AFTER_SUPERNODE] > limit)
	{
	  if (logger)
	    logger->log ("bailing out; too many nodes");
	  warning_at (node->get_point ().get_location (),
		      OPT_Wanalyzer_too_complex,
		      "analysis bailed out early"
		      " (%i 'after-snode' enodes; %i enodes)",
		      m_global_stats.m_num_nodes[PK_AFTER_SUPERNODE],
		      m_nodes.length ());
	  return;
	}
    }
}

/* Return true if STMT must appear at the start of its exploded node, and
   thus we can't consolidate its effects within a run of other statements,
   where PREV_STMT was the previous statement.  */

static bool
stmt_requires_new_enode_p (const gimple *stmt,
			   const gimple *prev_stmt)
{
  /* Stop consolidating at calls to
     "__analyzer_dump_exploded_nodes", so they always appear at the
     start of an exploded_node.  */
  if (const gcall *call = dyn_cast <const gcall *> (stmt))
    if (is_special_named_call_p (call, "__analyzer_dump_exploded_nodes",
			 1))
      return true;

  /* If we had a PREV_STMT with an unknown location, and this stmt
     has a known location, then if a state change happens here, it
     could be consolidated into PREV_STMT, giving us an event with
     no location.  Ensure that STMT gets its own exploded_node to
     avoid this.  */
  if (get_pure_location (prev_stmt->location) == UNKNOWN_LOCATION
      && get_pure_location (stmt->location) != UNKNOWN_LOCATION)
    return true;

  return false;
}

/* The core of exploded_graph::process_worklist (the main analysis loop),
   handling one node in the worklist.

   Get successor <point, state> pairs for NODE, calling get_or_create on
   them, and adding an exploded_edge to each successors.

   Freshly-created nodes will be added to the worklist.  */

void
exploded_graph::process_node (exploded_node *node)
{
  logger * const logger = get_logger ();
  LOG_FUNC_1 (logger, "EN: %i", node->m_index);

  node->set_status (exploded_node::STATUS_PROCESSED);

  const program_point &point = node->get_point ();

  /* Update cfun and input_location in case of an ICE: make it easier to
     track down which source construct we're failing to handle.  */
  auto_cfun sentinel (node->get_function ());
  const gimple *stmt = point.get_stmt ();
  if (stmt)
    input_location = stmt->location;

  const program_state &state = node->get_state ();
  if (logger)
    {
      pretty_printer *pp = logger->get_printer ();
      logger->start_log_line ();
      pp_string (pp, "point: ");
      point.print (pp, format (false));
      pp_string (pp, ", state: ");
      state.dump_to_pp (m_ext_state, true, pp);
      logger->end_log_line ();
    }

  switch (point.get_kind ())
    {
    default:
      gcc_unreachable ();
    case PK_ORIGIN:
      /* This node exists to simplify finding the shortest path
	 to an exploded_node.  */
      break;

    case PK_BEFORE_SUPERNODE:
      {
	program_state next_state (state);
	state_change change;

	if (point.get_from_edge ())
	  {
	    impl_region_model_context ctxt (*this, node,
					    &state, &next_state, &change,
					    NULL);
	    const cfg_superedge *last_cfg_superedge
	      = point.get_from_edge ()->dyn_cast_cfg_superedge ();
	    if (last_cfg_superedge)
	      next_state.m_region_model->update_for_phis
		(node->get_supernode (),
		 last_cfg_superedge,
		 &ctxt);
	  }

	if (point.get_supernode ()->m_stmts.length () > 0)
	  {
	    program_point next_point
	      = program_point::before_stmt (point.get_supernode (), 0,
					    point.get_call_string ());
	    exploded_node *next
	      = get_or_create_node (next_point, next_state, &change);
	    if (next)
	      add_edge (node, next, NULL, change);
	  }
	else
	  {
	    program_point next_point
	      = program_point::after_supernode (point.get_supernode (),
						point.get_call_string ());
	    exploded_node *next = get_or_create_node (next_point, next_state,
						      &change);
	    if (next)
	      add_edge (node, next, NULL, change);
	  }
      }
      break;
    case PK_BEFORE_STMT:
      {
	/* Determine the effect of a run of one or more statements
	   within one supernode, generating an edge to the program_point
	   after the last statement that's processed.

	   Stop iterating statements and thus consolidating into one enode
	   when:
	   - reaching the end of the statements in the supernode
	   - if an sm-state-change occurs (so that it gets its own
	     exploded_node)
	   - if "-fanalyzer-fine-grained" is active
	   - encountering certain statements must appear at the start of
	   their enode (for which stmt_requires_new_enode_p returns true)

	   Update next_state in-place, to get the result of the one
	   or more stmts that are processed.  */
	program_state next_state (state);
	state_change change;
	const supernode *snode = point.get_supernode ();
	unsigned stmt_idx;
	const gimple *prev_stmt = NULL;
	for (stmt_idx = point.get_stmt_idx ();
	     stmt_idx < snode->m_stmts.length ();
	     stmt_idx++)
	  {
	    const gimple *stmt = snode->m_stmts[stmt_idx];

	    if (stmt_idx > point.get_stmt_idx ())
	      if (stmt_requires_new_enode_p (stmt, prev_stmt))
		{
		  stmt_idx--;
		  break;
		}
	    prev_stmt = stmt;

	    /* Process the stmt.  */
	    exploded_node::on_stmt_flags flags
	      = node->on_stmt (*this, snode, stmt, &next_state, &change);

	    /* If flags.m_terminate_path, stop analyzing; any nodes/edges
	       will have been added by on_stmt (e.g. for handling longjmp).  */
	    if (flags.m_terminate_path)
	      return;

	    if (flags.m_sm_changes || flag_analyzer_fine_grained)
	      break;
	  }
	unsigned next_idx = stmt_idx + 1;
	program_point next_point
	  = (next_idx < point.get_supernode ()->m_stmts.length ()
	     ? program_point::before_stmt (point.get_supernode (), next_idx,
					   point.get_call_string ())
	     : program_point::after_supernode (point.get_supernode (),
					       point.get_call_string ()));
	exploded_node *next = get_or_create_node (next_point,
						  next_state, &change);
	if (next)
	  add_edge (node, next, NULL, change);
      }
      break;
    case PK_AFTER_SUPERNODE:
      {
	/* If this is an EXIT BB, detect leaks, and potentially
	   create a function summary.  */
	if (point.get_supernode ()->return_p ())
	  {
	    node->detect_leaks (*this);
	    if (flag_analyzer_call_summaries
		&& point.get_call_string ().empty_p ())
	      {
		/* TODO: create function summary
		   There can be more than one; each corresponds to a different
		   final enode in the function.  */
		if (logger)
		  {
		    pretty_printer *pp = logger->get_printer ();
		    logger->start_log_line ();
		    logger->log_partial
		      ("would create function summary for %qE; state: ",
		       point.get_fndecl ());
		    state.dump_to_pp (m_ext_state, true, pp);
		    logger->end_log_line ();
		  }
		per_function_data *per_fn_data
		  = get_or_create_per_function_data (point.get_function ());
		per_fn_data->add_call_summary (node);
	      }
	  }
	/* Traverse into successors of the supernode.  */
	int i;
	superedge *succ;
	FOR_EACH_VEC_ELT (point.get_supernode ()->m_succs, i, succ)
	  {
	    if (logger)
	      logger->log ("considering SN: %i -> SN: %i",
			   succ->m_src->m_index, succ->m_dest->m_index);

	    state_change change;

	    program_point next_point
	      = program_point::before_supernode (succ->m_dest, succ,
						 point.get_call_string ());
	    program_state next_state (state);

	    if (!node->on_edge (*this, succ, &next_point, &next_state, &change))
	      {
		if (logger)
		  logger->log ("skipping impossible edge to SN: %i",
			       succ->m_dest->m_index);
		continue;
	      }

	    exploded_node *next = get_or_create_node (next_point, next_state,
						      &change);
	    if (next)
	      add_edge (node, next, succ, change);
	  }
      }
      break;
    }
}

/* Ensure that this graph has a stats instance for FN, return it.
   FN can be NULL, in which case a stats instances is returned covering
   "functionless" parts of the graph (the origin node).  */

stats *
exploded_graph::get_or_create_function_stats (function *fn)
{
  if (!fn)
    return &m_functionless_stats;

  if (stats **slot = m_per_function_stats.get (fn))
    return *slot;
  else
    {
      int num_supernodes = fn ? n_basic_blocks_for_fn (fn) : 0;
      /* not quite the num supernodes, but nearly.  */
      stats *new_stats = new stats (num_supernodes);
      m_per_function_stats.put (fn, new_stats);
      return new_stats;
    }
}

/* Print bar charts to PP showing:
   - the number of enodes per function, and
   - for each function:
     - the number of enodes per supernode/BB
     - the number of excess enodes per supernode/BB beyond the
       per-program-point limit, if there were any.  */

void
exploded_graph::print_bar_charts (pretty_printer *pp) const
{
  cgraph_node *cgnode;

  pp_string (pp, "enodes per function:");
  pp_newline (pp);
  bar_chart enodes_per_function;
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (cgnode)
    {
      function *fn = cgnode->get_fun ();
      const stats * const *s_ptr
	= const_cast <function_stat_map_t &> (m_per_function_stats).get (fn);
      enodes_per_function.add_item (function_name (fn),
				    s_ptr ? (*s_ptr)->get_total_enodes () : 0);
    }
  enodes_per_function.print (pp);

  /* Accumulate number of enodes per supernode.  */
  auto_vec<unsigned> enodes_per_supernode (m_sg.num_nodes ());
  for (int i = 0; i < m_sg.num_nodes (); i++)
    enodes_per_supernode.quick_push (0);
  int i;
  exploded_node *enode;
  FOR_EACH_VEC_ELT (m_nodes, i, enode)
    {
      const supernode *iter_snode = enode->get_supernode ();
      if (!iter_snode)
	continue;
      enodes_per_supernode[iter_snode->m_index]++;
    }

  /* Accumulate excess enodes per supernode.  */
  auto_vec<unsigned> excess_enodes_per_supernode (m_sg.num_nodes ());
  for (int i = 0; i < m_sg.num_nodes (); i++)
    excess_enodes_per_supernode.quick_push (0);
  for (point_map_t::iterator iter = m_per_point_data.begin ();
       iter != m_per_point_data.end (); ++iter)
    {
      const program_point *point = (*iter).first;
      const supernode *iter_snode = point->get_supernode ();
      if (!iter_snode)
	continue;
      const per_program_point_data *point_data = (*iter).second;
      excess_enodes_per_supernode[iter_snode->m_index]
	+= point_data->m_excess_enodes;
    }

  /* Show per-function bar_charts of enodes per supernode/BB.  */
  pp_string (pp, "per-function enodes per supernode/BB:");
  pp_newline (pp);
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (cgnode)
    {
      function *fn = cgnode->get_fun ();
      pp_printf (pp, "function: %qs", function_name (fn));
      pp_newline (pp);

      bar_chart enodes_per_snode;
      bar_chart excess_enodes_per_snode;
      bool have_excess_enodes = false;
      for (int i = 0; i < m_sg.num_nodes (); i++)
	{
	  const supernode *iter_snode = m_sg.get_node_by_index (i);
	  if (iter_snode->get_function () != fn)
	    continue;
	  pretty_printer tmp_pp;
	  pp_printf (&tmp_pp, "sn %i (bb %i)",
		     iter_snode->m_index, iter_snode->m_bb->index);
	  enodes_per_snode.add_item (pp_formatted_text (&tmp_pp),
				     enodes_per_supernode[iter_snode->m_index]);
	  const int num_excess
	    = excess_enodes_per_supernode[iter_snode->m_index];
	  excess_enodes_per_snode.add_item (pp_formatted_text (&tmp_pp),
					    num_excess);
	  if (num_excess)
	    have_excess_enodes = true;
	}
      enodes_per_snode.print (pp);
      if (have_excess_enodes)
	{
	  pp_printf (pp, "EXCESS ENODES:");
	  pp_newline (pp);
	  excess_enodes_per_snode.print (pp);
	}
    }
}

/* Write all stats information to this graph's logger, if any.  */

void
exploded_graph::log_stats () const
{
  logger * const logger = get_logger ();
  if (!logger)
    return;

  LOG_SCOPE (logger);

  logger->log ("m_sg.num_nodes (): %i", m_sg.num_nodes ());
  logger->log ("m_nodes.length (): %i", m_nodes.length ());
  logger->log ("m_edges.length (): %i", m_edges.length ());
  logger->log ("remaining enodes in worklist: %i", m_worklist.length ());

  logger->log ("global stats:");
  m_global_stats.log (logger);

  for (function_stat_map_t::iterator iter = m_per_function_stats.begin ();
       iter != m_per_function_stats.end ();
       ++iter)
    {
      function *fn = (*iter).first;
      log_scope s (logger, function_name (fn));
      (*iter).second->log (logger);
    }

  print_bar_charts (logger->get_printer ());
}

/* Dump all stats information to OUT.  */

void
exploded_graph::dump_stats (FILE *out) const
{
  fprintf (out, "m_sg.num_nodes (): %i\n", m_sg.num_nodes ());
  fprintf (out, "m_nodes.length (): %i\n", m_nodes.length ());
  fprintf (out, "m_edges.length (): %i\n", m_edges.length ());
  fprintf (out, "remaining enodes in worklist: %i", m_worklist.length ());

  fprintf (out, "global stats:\n");
  m_global_stats.dump (out);

  for (function_stat_map_t::iterator iter = m_per_function_stats.begin ();
       iter != m_per_function_stats.end ();
       ++iter)
    {
      function *fn = (*iter).first;
      fprintf (out, "function: %s\n", function_name (fn));
      (*iter).second->dump (out);
    }

  fprintf (out, "PK_AFTER_SUPERNODE per supernode:\n");
  for (unsigned i = 0; i < m_PK_AFTER_SUPERNODE_per_snode.length (); i++)
    fprintf (out, "  SN %i: %3i\n", i, m_PK_AFTER_SUPERNODE_per_snode[i]);
}

void
exploded_graph::dump_states_for_supernode (FILE *out,
					   const supernode *snode) const
{
  fprintf (out, "PK_AFTER_SUPERNODE nodes for SN: %i\n", snode->m_index);
  int i;
  exploded_node *enode;
  int state_idx = 0;
  FOR_EACH_VEC_ELT (m_nodes, i, enode)
    {
      const supernode *iter_snode = enode->get_supernode ();
      if (enode->get_point ().get_kind () == PK_AFTER_SUPERNODE
	  && iter_snode == snode)
	{
	  pretty_printer pp;
	  pp_format_decoder (&pp) = default_tree_printer;
	  enode->get_state ().dump_to_pp (m_ext_state, true, &pp);
	  fprintf (out, "state %i: EN: %i\n  %s\n",
		   state_idx++, enode->m_index,
		   pp_formatted_text (&pp));
	}
    }
  fprintf (out, "#exploded_node for PK_AFTER_SUPERNODE for SN: %i = %i\n",
	   snode->m_index, state_idx);
}

/* Look for the last use of SEARCH_STMT within this path.
   If found write the edge's index to *OUT_IDX and return true, otherwise
   return false.  */

bool
exploded_path::find_stmt_backwards (const gimple *search_stmt,
				    int *out_idx) const
{
  int i;
  const exploded_edge *eedge;
  FOR_EACH_VEC_ELT_REVERSE (m_edges, i, eedge)
    {
      const exploded_node *dst_node = eedge->m_dest;
      const program_point &dst_point = dst_node->get_point ();
      const gimple *stmt = dst_point.get_stmt ();
      if (stmt == search_stmt)
	{
	  *out_idx = i;
	  return true;
	}
    }
  return false;
}

/* Get the final exploded_node in this path, which must be non-empty.  */

exploded_node *
exploded_path::get_final_enode () const
{
  gcc_assert (m_edges.length () > 0);
  return m_edges[m_edges.length () - 1]->m_dest;
}

/* Check state along this path, returning true if it is feasible.
   If OUT is non-NULL, and the path is infeasible, write a new
   feasibility_problem to *OUT.  */

bool
exploded_path::feasible_p (logger *logger, feasibility_problem **out) const
{
  LOG_SCOPE (logger);

  /* Traverse the path, updating this model.  */
  region_model model;
  for (unsigned i = 0; i < m_edges.length (); i++)
    {
      const exploded_edge *eedge = m_edges[i];
      if (logger)
	logger->log ("considering edge %i: EN:%i -> EN:%i",
		     i,
		     eedge->m_src->m_index,
		     eedge->m_dest->m_index);
      const exploded_node &src_enode = *eedge->m_src;
      const program_point &src_point = src_enode.get_point ();
      if (logger)
	{
	  logger->start_log_line ();
	  src_point.print (logger->get_printer (), format (false));
	  logger->end_log_line ();
	}

      if (const gimple *stmt = src_point.get_stmt ())
	{
	  /* Update cfun and input_location in case of ICE: make it easier to
	     track down which source construct we're failing to handle.  */
	  auto_cfun sentinel (src_point.get_function ());
	  input_location = stmt->location;

	  if (const gassign *assign = dyn_cast <const gassign *> (stmt))
	    model.on_assignment (assign, NULL);
	  else if (const greturn *return_ = dyn_cast <const greturn *> (stmt))
	    model.on_return (return_, NULL);
	}

      const superedge *sedge = eedge->m_sedge;
      if (sedge)
	{
	  if (logger)
	    logger->log ("  sedge: SN:%i -> SN:%i %s",
			 sedge->m_src->m_index,
			 sedge->m_dest->m_index,
			 sedge->get_description (false));

	  const gimple *last_stmt = src_point.get_supernode ()->get_last_stmt ();
	  if (!model.maybe_update_for_edge (*sedge, last_stmt, NULL))
	    {
	      if (logger)
		{
		  logger->log ("rejecting due to region model");
		  model.dump_to_pp (logger->get_printer (), false);
		}
	      if (out)
		*out = new feasibility_problem (i, model, *eedge, last_stmt);
	      return false;
	    }
	}
      else
	{
	  /* Special-case the initial eedge from the origin node to the
	     initial function by pushing a frame for it.  */
	  if (i == 0)
	    {
	      gcc_assert (eedge->m_src->m_index == 0);
	      gcc_assert (src_point.get_kind () == PK_ORIGIN);
	      gcc_assert (eedge->m_dest->get_point ().get_kind ()
			  == PK_BEFORE_SUPERNODE);
	      function *fun = eedge->m_dest->get_function ();
	      gcc_assert (fun);
	      model.push_frame (fun, NULL, NULL);
	      if (logger)
		logger->log ("  pushing frame for %qD", fun->decl);
	    }
	  else if (eedge->m_custom_info)
	    eedge->m_custom_info->update_model (&model, *eedge);
	}

      /* Handle phi nodes on an edge leaving a PK_BEFORE_SUPERNODE (to
	 a PK_BEFORE_STMT, or a PK_AFTER_SUPERNODE if no stmts).
	 This will typically not be associated with a superedge.  */
      if (src_point.get_from_edge ())
	{
	  const cfg_superedge *last_cfg_superedge
	    = src_point.get_from_edge ()->dyn_cast_cfg_superedge ();
	  if (last_cfg_superedge)
	    {
	      if (logger)
		logger->log ("  update for phis");
	      model.update_for_phis (src_enode.get_supernode (),
				     last_cfg_superedge,
				     NULL);
	    }
	}

      if (logger)
	{
	  logger->log ("state after edge %i: EN:%i -> EN:%i",
		       i,
		       eedge->m_src->m_index,
		       eedge->m_dest->m_index);
	  logger->start_log_line ();
	  model.dump_to_pp (logger->get_printer (), true);
	  logger->end_log_line ();
	}
    }

  return true;
}

/* Dump this path in multiline form to PP.  */

void
exploded_path::dump_to_pp (pretty_printer *pp) const
{
  for (unsigned i = 0; i < m_edges.length (); i++)
    {
      const exploded_edge *eedge = m_edges[i];
      pp_printf (pp, "m_edges[%i]: EN %i -> EN %i",
		 i,
		 eedge->m_src->m_index,
		 eedge->m_dest->m_index);
      pp_newline (pp);
    }
}

/* Dump this path in multiline form to FP.  */

void
exploded_path::dump (FILE *fp) const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp_show_color (&pp) = pp_show_color (global_dc->printer);
  pp.buffer->stream = fp;
  dump_to_pp (&pp);
  pp_flush (&pp);
}

/* Dump this path in multiline form to stderr.  */

DEBUG_FUNCTION void
exploded_path::dump () const
{
  dump (stderr);
}

/* A family of cluster subclasses for use when generating .dot output for
   exploded graphs (-fdump-analyzer-exploded-graph), for grouping the
   enodes into hierarchical boxes.

   All functionless enodes appear in the top-level graph.
   Every (function, call_string) pair gets its own cluster.  Within that
   cluster, each supernode gets its own cluster.

   Hence all enodes relating to a particular function with a particular
   callstring will be in a cluster together; all enodes for the same
   function but with a different callstring will be in a different
   cluster.  */

/* Base class of cluster for clustering exploded_node instances in .dot
   output, based on various subclass-specific criteria.  */

class exploded_cluster : public cluster<eg_traits>
{
};

/* Cluster containing all exploded_node instances for one supernode.  */

class supernode_cluster : public exploded_cluster
{
public:
  supernode_cluster (const supernode *supernode) : m_supernode (supernode) {}

  // TODO: dtor?

  void dump_dot (graphviz_out *gv, const dump_args_t &args) const FINAL OVERRIDE
  {
    gv->println ("subgraph \"cluster_supernode_%p\" {",
		 (const void *)this);
    gv->indent ();
    gv->println ("style=\"dashed\";");
    gv->println ("label=\"SN: %i (bb: %i)\";",
		 m_supernode->m_index, m_supernode->m_bb->index);

    int i;
    exploded_node *enode;
    FOR_EACH_VEC_ELT (m_enodes, i, enode)
      enode->dump_dot (gv, args);

    /* Terminate subgraph.  */
    gv->outdent ();
    gv->println ("}");
  }

  void add_node (exploded_node *en) FINAL OVERRIDE
  {
    m_enodes.safe_push (en);
  }

private:
  const supernode *m_supernode;
  auto_vec <exploded_node *> m_enodes;
};

/* Cluster containing all supernode_cluster instances for one
   (function, call_string) pair.  */

class function_call_string_cluster : public exploded_cluster
{
public:
  function_call_string_cluster (function *fun, call_string cs)
  : m_fun (fun), m_cs (cs) {}

  ~function_call_string_cluster ()
  {
    for (map_t::iterator iter = m_map.begin ();
	 iter != m_map.end ();
	 ++iter)
      delete (*iter).second;
  }

  void dump_dot (graphviz_out *gv, const dump_args_t &args) const FINAL OVERRIDE
  {
    const char *funcname = function_name (m_fun);

    gv->println ("subgraph \"cluster_function_%p\" {", (const void *)this);
    gv->indent ();
    gv->write_indent ();
    gv->print ("label=\"call string: ");
    m_cs.print (gv->get_pp ());
    gv->print (" function: %s \";", funcname);
    gv->print ("\n");

    for (map_t::iterator iter = m_map.begin ();
	 iter != m_map.end ();
	 ++iter)
      (*iter).second->dump_dot (gv, args);

    /* Terminate subgraph.  */
    gv->outdent ();
    gv->println ("}");
  }

  void add_node (exploded_node *en) FINAL OVERRIDE
  {
    const supernode *supernode = en->get_supernode ();
    gcc_assert (supernode);
    supernode_cluster **slot = m_map.get (supernode);
    if (slot)
      (*slot)->add_node (en);
    else
      {
	supernode_cluster *child = new supernode_cluster (supernode);
	m_map.put (supernode, child);
	child->add_node (en);
      }
  }

private:
  function *m_fun;
  call_string m_cs;
  typedef ordered_hash_map<const supernode *, supernode_cluster *> map_t;
  map_t m_map;
};

/* Keys for root_cluster.  */

struct function_call_string
{
  function_call_string (function *fun, call_string cs)
  : m_fun (fun), m_cs (cs)
  {
    gcc_assert (fun);
  }

  function *m_fun;
  call_string m_cs;
};

} // namespace ana

template <> struct default_hash_traits<function_call_string>
: public pod_hash_traits<function_call_string>
{
  static const bool empty_zero_p = false;
};

template <>
inline hashval_t
pod_hash_traits<function_call_string>::hash (value_type v)
{
  return pointer_hash <function>::hash (v.m_fun) ^ v.m_cs.hash ();
}

template <>
inline bool
pod_hash_traits<function_call_string>::equal (const value_type &existing,
					      const value_type &candidate)
{
  return existing.m_fun == candidate.m_fun && existing.m_cs == candidate.m_cs;
}
template <>
inline void
pod_hash_traits<function_call_string>::mark_deleted (value_type &v)
{
  v.m_fun = reinterpret_cast<function *> (1);
}
template <>
inline void
pod_hash_traits<function_call_string>::mark_empty (value_type &v)
{
  v.m_fun = NULL;
}
template <>
inline bool
pod_hash_traits<function_call_string>::is_deleted (value_type v)
{
  return v.m_fun == reinterpret_cast<function *> (1);
}
template <>
inline bool
pod_hash_traits<function_call_string>::is_empty (value_type v)
{
  return v.m_fun == NULL;
}

namespace ana {

/* Top-level cluster for generating .dot output for exploded graphs,
   handling the functionless nodes, and grouping the remaining nodes by
   callstring.  */

class root_cluster : public exploded_cluster
{
public:
  ~root_cluster ()
  {
    for (map_t::iterator iter = m_map.begin ();
	 iter != m_map.end ();
	 ++iter)
      delete (*iter).second;
  }

  void dump_dot (graphviz_out *gv, const dump_args_t &args) const FINAL OVERRIDE
  {
    int i;
    exploded_node *enode;
    FOR_EACH_VEC_ELT (m_functionless_enodes, i, enode)
      enode->dump_dot (gv, args);

    for (map_t::iterator iter = m_map.begin ();
	 iter != m_map.end ();
	 ++iter)
      (*iter).second->dump_dot (gv, args);
  }

  void add_node (exploded_node *en) FINAL OVERRIDE
  {
    function *fun = en->get_function ();
    if (!fun)
      {
	m_functionless_enodes.safe_push (en);
	return;
      }

    const call_string &cs = en->get_point ().get_call_string ();
    function_call_string key (fun, cs);
    function_call_string_cluster **slot = m_map.get (key);
    if (slot)
      (*slot)->add_node (en);
    else
      {
	function_call_string_cluster *child
	  = new function_call_string_cluster (fun, cs);
	m_map.put (key, child);
	child->add_node (en);
      }
  }

private:
  /* This can't be an ordered_hash_map, as we can't store vec<call_string>,
     since it's not a POD; vec<>::quick_push has:
       *slot = obj;
     and the slot isn't initialized, so the assignment op dies when cleaning up
     un-inited *slot (within the truncate call).  */
  typedef hash_map<function_call_string, function_call_string_cluster *> map_t;
  map_t m_map;

  /* This should just be the origin exploded_node.  */
  auto_vec <exploded_node *> m_functionless_enodes;
};

/* Subclass of range_label for use within
   exploded_graph::dump_exploded_nodes for implementing
   -fdump-analyzer-exploded-nodes: a label for a specific
   exploded_node.  */

class enode_label : public range_label
{
 public:
  enode_label (const extrinsic_state &ext_state,
	       exploded_node *enode)
  : m_ext_state (ext_state), m_enode (enode) {}

  label_text get_text (unsigned) const FINAL OVERRIDE
  {
    pretty_printer pp;
    pp_format_decoder (&pp) = default_tree_printer;
    m_enode->get_state ().dump_to_pp (m_ext_state, true, &pp);
    return make_label_text (false, "EN: %i: %s",
			    m_enode->m_index, pp_formatted_text (&pp));
  }

private:
  const extrinsic_state &m_ext_state;
  exploded_node *m_enode;
};

/* Postprocessing support for dumping the exploded nodes.
   Handle -fdump-analyzer-exploded-nodes,
   -fdump-analyzer-exploded-nodes-2, and the
   "__analyzer_dump_exploded_nodes" builtin.  */

void
exploded_graph::dump_exploded_nodes () const
{
  // TODO
  /* Locate calls to __analyzer_dump_exploded_nodes.  */
  // Print how many egs there are for them?
  /* Better: log them as we go, and record the exploded nodes
     in question.  */

  /* Show every enode.  */

  /* Gather them by stmt, so that we can more clearly see the
     "hotspots" requiring numerous exploded nodes.  */

  /* Alternatively, simply throw them all into one big rich_location
     and see if the label-printing will sort it out...
     This requires them all to be in the same source file.  */

  if (flag_dump_analyzer_exploded_nodes)
    {
      auto_timevar tv (TV_ANALYZER_DUMP);
      gcc_rich_location richloc (UNKNOWN_LOCATION);
      unsigned i;
      exploded_node *enode;
      FOR_EACH_VEC_ELT (m_nodes, i, enode)
	{
	  if (const gimple *stmt = enode->get_stmt ())
	    {
	      if (get_pure_location (richloc.get_loc ()) == UNKNOWN_LOCATION)
		richloc.set_range (0, stmt->location, SHOW_RANGE_WITH_CARET);
	      else
		richloc.add_range (stmt->location,
				   SHOW_RANGE_WITHOUT_CARET,
				   new enode_label (m_ext_state, enode));
	    }
	}
      warning_at (&richloc, 0, "%i exploded nodes", m_nodes.length ());

      /* Repeat the warning without all the labels, so that message is visible
	 (the other one may well have scrolled past the terminal limit).  */
      warning_at (richloc.get_loc (), 0,
		  "%i exploded nodes", m_nodes.length ());

      if (m_worklist.length () > 0)
	warning_at (richloc.get_loc (), 0,
		    "worklist still contains %i nodes", m_worklist.length ());
    }

  /* Dump the egraph in textual form to a dump file.  */
  if (flag_dump_analyzer_exploded_nodes_2)
    {
      auto_timevar tv (TV_ANALYZER_DUMP);
      char *filename
	= concat (dump_base_name, ".eg.txt", NULL);
      FILE *outf = fopen (filename, "w");
      if (!outf)
	error_at (UNKNOWN_LOCATION, "unable to open %qs for writing", filename);
      free (filename);

      fprintf (outf, "exploded graph for %s\n", dump_base_name);
      fprintf (outf, "  nodes: %i\n", m_nodes.length ());
      fprintf (outf, "  edges: %i\n", m_edges.length ());

      unsigned i;
      exploded_node *enode;
      FOR_EACH_VEC_ELT (m_nodes, i, enode)
	{
	  fprintf (outf, "\nEN %i:\n", enode->m_index);
	  enode->dump_succs_and_preds (outf);
	  pretty_printer pp;
	  enode->get_point ().print (&pp, format (true));
	  fprintf (outf, "%s\n", pp_formatted_text (&pp));
	  enode->get_state ().dump_to_file (m_ext_state, false, outf);
	}

      fclose (outf);
    }

  /* Dump the egraph in textual form to multiple dump files, one per enode.  */
  if (flag_dump_analyzer_exploded_nodes_3)
    {
      auto_timevar tv (TV_ANALYZER_DUMP);

      unsigned i;
      exploded_node *enode;
      FOR_EACH_VEC_ELT (m_nodes, i, enode)
	{
	  char *filename
	    = xasprintf ("%s.en-%i.txt", dump_base_name, i);
	  FILE *outf = fopen (filename, "w");
	  if (!outf)
	    error_at (UNKNOWN_LOCATION, "unable to open %qs for writing", filename);
	  free (filename);

	  fprintf (outf, "EN %i:\n", enode->m_index);
	  enode->dump_succs_and_preds (outf);
	  pretty_printer pp;
	  enode->get_point ().print (&pp, format (true));
	  fprintf (outf, "%s\n", pp_formatted_text (&pp));
	  enode->get_state ().dump_to_file (m_ext_state, false, outf);

	  fclose (outf);
	}
    }

  /* Emit a warning at any call to "__analyzer_dump_exploded_nodes",
     giving the number of processed exploded nodes for "before-stmt",
     and the IDs of processed, merger, and worklist enodes.

     We highlight the count of *processed* enodes since this is of most
     interest in DejaGnu tests for ensuring that state merger has
     happened.

     We don't show the count of merger and worklist enodes, as this is
     more of an implementation detail of the merging/worklist that we
     don't want to bake into our expected DejaGnu messages.  */

  unsigned i;
  exploded_node *enode;
  hash_set<const gimple *> seen;
  FOR_EACH_VEC_ELT (m_nodes, i, enode)
    {
      if (enode->get_point ().get_kind () != PK_BEFORE_STMT)
	continue;

      if (const gimple *stmt = enode->get_stmt ())
	if (const gcall *call = dyn_cast <const gcall *> (stmt))
	  if (is_special_named_call_p (call, "__analyzer_dump_exploded_nodes",
				       1))
	    {
	      if (seen.contains (stmt))
		continue;

	      auto_vec<exploded_node *> processed_enodes;
	      auto_vec<exploded_node *> merger_enodes;
	      auto_vec<exploded_node *> worklist_enodes;
	      /* This is O(N^2).  */
	      unsigned j;
	      exploded_node *other_enode;
	      FOR_EACH_VEC_ELT (m_nodes, j, other_enode)
		{
		  if (other_enode->get_point ().get_kind () != PK_BEFORE_STMT)
		    continue;
		  if (other_enode->get_stmt () == stmt)
		    switch (other_enode->get_status ())
		      {
		      default:
			gcc_unreachable ();
		      case exploded_node::STATUS_WORKLIST:
			worklist_enodes.safe_push (other_enode);
			break;
		      case exploded_node::STATUS_PROCESSED:
			processed_enodes.safe_push (other_enode);
			break;
		      case exploded_node::STATUS_MERGER:
			merger_enodes.safe_push (other_enode);
			break;
		      }
		}

	      pretty_printer pp;
	      pp_character (&pp, '[');
	      print_enode_indices (&pp, processed_enodes);
	      if (merger_enodes.length () > 0)
		{
		  pp_string (&pp, "] merger(s): [");
		  print_enode_indices (&pp, merger_enodes);
		}
	      if (worklist_enodes.length () > 0)
		{
		  pp_string (&pp, "] worklist: [");
		  print_enode_indices (&pp, worklist_enodes);
		}
	      pp_character (&pp, ']');

	      warning_n (stmt->location, 0, processed_enodes.length (),
			 "%i processed enode: %s",
			 "%i processed enodes: %s",
			 processed_enodes.length (), pp_formatted_text (&pp));
	      seen.add (stmt);

	      /* If the argument is non-zero, then print all of the states
		 of the various enodes.  */
	      tree t_arg = fold (gimple_call_arg (call, 0));
	      if (TREE_CODE (t_arg) != INTEGER_CST)
		{
		  error_at (call->location,
			    "integer constant required for arg 1");
		  return;
		}
	      int i_arg = TREE_INT_CST_LOW (t_arg);
	      if (i_arg)
		{
		  exploded_node *other_enode;
		  FOR_EACH_VEC_ELT (processed_enodes, j, other_enode)
		    {
		      fprintf (stderr, "%i of %i: EN %i:\n",
			       j + 1, processed_enodes.length (),
			       other_enode->m_index);
		      other_enode->dump_succs_and_preds (stderr);
		      /* Dump state.  */
		      other_enode->get_state ().dump (m_ext_state, false);
		    }
		}
	    }
    }
}

/* A collection of classes for visualizing the callgraph in .dot form
   (as represented in the supergraph).  */

/* Forward decls.  */
class viz_callgraph_node;
class viz_callgraph_edge;
class viz_callgraph;
class viz_callgraph_cluster;

/* Traits for using "digraph.h" to visualize the callgraph.  */

struct viz_callgraph_traits
{
  typedef viz_callgraph_node node_t;
  typedef viz_callgraph_edge edge_t;
  typedef viz_callgraph graph_t;
  struct dump_args_t
  {
    dump_args_t (const exploded_graph *eg) : m_eg (eg) {}
    const exploded_graph *m_eg;
  };
  typedef viz_callgraph_cluster cluster_t;
};

/* Subclass of dnode representing a function within the callgraph.  */

class viz_callgraph_node : public dnode<viz_callgraph_traits>
{
  friend class viz_callgraph;

public:
  viz_callgraph_node (function *fun, int index)
  : m_fun (fun), m_index (index), m_num_supernodes (0), m_num_superedges (0)
  {
    gcc_assert (fun);
  }

  void dump_dot (graphviz_out *gv, const dump_args_t &args) const FINAL OVERRIDE
  {
    pretty_printer *pp = gv->get_pp ();

    dump_dot_id (pp);
    pp_printf (pp, " [shape=none,margin=0,style=filled,fillcolor=%s,label=<",
	       "lightgrey");
    pp_string (pp, "<TABLE BORDER=\"0\">");
    pp_write_text_to_stream (pp);

    gv->begin_trtd ();
    pp_printf (pp, "VCG: %i: %s", m_index, function_name (m_fun));
    gv->end_tdtr ();
    pp_newline (pp);

    gv->begin_trtd ();
    pp_printf (pp, "supernodes: %i\n", m_num_supernodes);
    gv->end_tdtr ();
    pp_newline (pp);

    gv->begin_trtd ();
    pp_printf (pp, "superedges: %i\n", m_num_superedges);
    gv->end_tdtr ();
    pp_newline (pp);

    if (args.m_eg)
      {
	unsigned i;
	exploded_node *enode;
	unsigned num_enodes = 0;
	FOR_EACH_VEC_ELT (args.m_eg->m_nodes, i, enode)
	  {
	    if (enode->get_point ().get_function () == m_fun)
	      num_enodes++;
	  }
	gv->begin_trtd ();
	pp_printf (pp, "enodes: %i\n", num_enodes);
	gv->end_tdtr ();
	pp_newline (pp);

	// TODO: also show the per-callstring breakdown
	const exploded_graph::call_string_data_map_t *per_cs_data
	  = args.m_eg->get_per_call_string_data ();
	for (exploded_graph::call_string_data_map_t::iterator iter
	       = per_cs_data->begin ();
	     iter != per_cs_data->end ();
	     ++iter)
	  {
	    const call_string *cs = (*iter).first;
	    //per_call_string_data *data = (*iter).second;
	    num_enodes = 0;
	    FOR_EACH_VEC_ELT (args.m_eg->m_nodes, i, enode)
	      {
		if (enode->get_point ().get_function () == m_fun
		    && enode->get_point ().get_call_string () == *cs)
		  num_enodes++;
	      }
	    if (num_enodes > 0)
	      {
		gv->begin_trtd ();
		cs->print (pp);
		pp_printf (pp, ": %i\n", num_enodes);
		pp_write_text_as_html_like_dot_to_stream (pp);
		gv->end_tdtr ();
	      }
	  }

	/* Show any summaries.  */
	per_function_data *data = args.m_eg->get_per_function_data (m_fun);
	if (data)
	  {
	    pp_newline (pp);
	    gv->begin_trtd ();
	    pp_printf (pp, "summaries: %i\n", data->m_summaries.length ());
	    pp_write_text_as_html_like_dot_to_stream (pp);
	    gv->end_tdtr ();
	  }
      }

    pp_string (pp, "</TABLE>>];\n\n");
    pp_flush (pp);
  }

  void dump_dot_id (pretty_printer *pp) const
  {
    pp_printf (pp, "vcg_%i", m_index);
  }

private:
  function *m_fun;
  int m_index;
  int m_num_supernodes;
  int m_num_superedges;
};

/* Subclass of dedge representing a callgraph edge.  */

class viz_callgraph_edge : public dedge<viz_callgraph_traits>
{
public:
  viz_callgraph_edge (viz_callgraph_node *src, viz_callgraph_node *dest)
  : dedge<viz_callgraph_traits> (src, dest)
  {}

  void dump_dot (graphviz_out *gv, const dump_args_t &) const
    FINAL OVERRIDE
  {
    pretty_printer *pp = gv->get_pp ();

    const char *style = "\"solid,bold\"";
    const char *color = "black";
    int weight = 10;
    const char *constraint = "true";

    m_src->dump_dot_id (pp);
    pp_string (pp, " -> ");
    m_dest->dump_dot_id (pp);
    pp_printf (pp,
	       (" [style=%s, color=%s, weight=%d, constraint=%s,"
		" headlabel=\""),
	       style, color, weight, constraint);
    pp_printf (pp, "\"];\n");
  }
};

/* Subclass of digraph representing the callgraph.  */

class viz_callgraph : public digraph<viz_callgraph_traits>
{
public:
  viz_callgraph (const supergraph &sg);

  viz_callgraph_node *get_vcg_node_for_function (function *fun)
  {
    return *m_map.get (fun);
  }

  viz_callgraph_node *get_vcg_node_for_snode (supernode *snode)
  {
    return get_vcg_node_for_function (snode->m_fun);
  }

private:
  hash_map<function *, viz_callgraph_node *> m_map;
};

/* Placeholder subclass of cluster.  */

class viz_callgraph_cluster : public cluster<viz_callgraph_traits>
{
};

/* viz_callgraph's ctor.  */

viz_callgraph::viz_callgraph (const supergraph &sg)
{
  cgraph_node *node;
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (node)
    {
      function *fun = node->get_fun ();
      viz_callgraph_node *vcg_node
	= new viz_callgraph_node (fun, m_nodes.length ());
      m_map.put (fun, vcg_node);
      add_node (vcg_node);
    }

  unsigned i;
  superedge *sedge;
  FOR_EACH_VEC_ELT (sg.m_edges, i, sedge)
    {
      viz_callgraph_node *vcg_src = get_vcg_node_for_snode (sedge->m_src);
      if (vcg_src->m_fun)
	get_vcg_node_for_function (vcg_src->m_fun)->m_num_superedges++;
      if (sedge->dyn_cast_call_superedge ())
	{
	  viz_callgraph_node *vcg_dest = get_vcg_node_for_snode (sedge->m_dest);
	  viz_callgraph_edge *vcg_edge
	    = new viz_callgraph_edge (vcg_src, vcg_dest);
	  add_edge (vcg_edge);
	}
    }

  supernode *snode;
  FOR_EACH_VEC_ELT (sg.m_nodes, i, snode)
    {
      if (snode->m_fun)
	get_vcg_node_for_function (snode->m_fun)->m_num_supernodes++;
    }
}

/* Dump the callgraph to FILENAME.  */

static void
dump_callgraph (const supergraph &sg, const char *filename,
		const exploded_graph *eg)
{
  FILE *outf = fopen (filename, "w");
  if (!outf)
    return;

  // TODO
  viz_callgraph vcg (sg);
  vcg.dump_dot (filename, NULL, viz_callgraph_traits::dump_args_t (eg));

  fclose (outf);
}

/* Dump the callgraph to "<srcfile>.callgraph.dot".  */

static void
dump_callgraph (const supergraph &sg, const exploded_graph *eg)
{
  auto_timevar tv (TV_ANALYZER_DUMP);
  char *filename = concat (dump_base_name, ".callgraph.dot", NULL);
  dump_callgraph (sg, filename, eg);
  free (filename);
}

/* Subclass of dot_annotator for implementing
   DUMP_BASE_NAME.supergraph-eg.dot, a post-analysis dump of the supergraph.

   Annotate the supergraph nodes by printing the exploded nodes in concise
   form within them, next to their pertinent statements where appropriate,
   colorizing the exploded nodes based on sm-state.
   Also show saved diagnostics within the exploded nodes, giving information
   on whether they were feasible, and, if infeasible, where the problem
   was.  */

class exploded_graph_annotator : public dot_annotator
{
public:
  exploded_graph_annotator (const exploded_graph &eg)
  : m_eg (eg)
  {
    /* Avoid O(N^2) by prepopulating m_enodes_per_snodes.  */
    unsigned i;
    supernode *snode;
    FOR_EACH_VEC_ELT (eg.get_supergraph ().m_nodes, i, snode)
      m_enodes_per_snodes.safe_push (new auto_vec <exploded_node *> ());
    exploded_node *enode;
    FOR_EACH_VEC_ELT (m_eg.m_nodes, i, enode)
      if (enode->get_supernode ())
	m_enodes_per_snodes[enode->get_supernode ()->m_index]->safe_push (enode);
  }

  /* Show exploded nodes for BEFORE_SUPERNODE points before N.  */
  bool add_node_annotations (graphviz_out *gv, const supernode &n,
			     bool within_table)
    const FINAL OVERRIDE
  {
    if (!within_table)
      return false;
    gv->begin_tr ();
    pretty_printer *pp = gv->get_pp ();

    gv->begin_td ();
    pp_string (pp, "BEFORE");
    gv->end_td ();

    unsigned i;
    exploded_node *enode;
    bool had_enode = false;
    FOR_EACH_VEC_ELT (*m_enodes_per_snodes[n.m_index], i, enode)
      {
	gcc_assert (enode->get_supernode () == &n);
	const program_point &point = enode->get_point ();
	if (point.get_kind () != PK_BEFORE_SUPERNODE)
	  continue;
	print_enode (gv, enode);
	had_enode = true;
      }
    if (!had_enode)
      pp_string (pp, "<TD BGCOLOR=\"red\">UNREACHED</TD>");
    pp_flush (pp);
    gv->end_tr ();
    return true;
  }

  /* Show exploded nodes for STMT.  */
  void add_stmt_annotations (graphviz_out *gv, const gimple *stmt,
			     bool within_row)
    const FINAL OVERRIDE
  {
    if (!within_row)
      return;
    pretty_printer *pp = gv->get_pp ();

    const supernode *snode
      = m_eg.get_supergraph ().get_supernode_for_stmt (stmt);
    unsigned i;
    exploded_node *enode;
    bool had_td = false;
    FOR_EACH_VEC_ELT (*m_enodes_per_snodes[snode->m_index], i, enode)
      {
	const program_point &point = enode->get_point ();
	if (point.get_kind () != PK_BEFORE_STMT)
	  continue;
	if (point.get_stmt () != stmt)
	  continue;
	print_enode (gv, enode);
	had_td = true;
      }
    pp_flush (pp);
    if (!had_td)
      {
	gv->begin_td ();
	gv->end_td ();
      }
  }

  /* Show exploded nodes for AFTER_SUPERNODE points after N.  */
  bool add_after_node_annotations (graphviz_out *gv, const supernode &n)
    const FINAL OVERRIDE
  {
    gv->begin_tr ();
    pretty_printer *pp = gv->get_pp ();

    gv->begin_td ();
    pp_string (pp, "AFTER");
    gv->end_td ();

    unsigned i;
    exploded_node *enode;
    FOR_EACH_VEC_ELT (*m_enodes_per_snodes[n.m_index], i, enode)
      {
	gcc_assert (enode->get_supernode () == &n);
	const program_point &point = enode->get_point ();
	if (point.get_kind () != PK_AFTER_SUPERNODE)
	  continue;
	print_enode (gv, enode);
      }
    pp_flush (pp);
    gv->end_tr ();
    return true;
  }

private:
  /* Concisely print a TD element for ENODE, showing the index, status,
     and any saved_diagnostics at the enode.  Colorize it to show sm-state.

     Ideally we'd dump ENODE's state here, hidden behind some kind of
     interactive disclosure method like a tooltip, so that the states
     can be explored without overwhelming the graph.
     However, I wasn't able to get graphviz/xdot to show tooltips on
     individual elements within a HTML-like label.  */
  void print_enode (graphviz_out *gv, const exploded_node *enode) const
  {
    pretty_printer *pp = gv->get_pp ();
    pp_printf (pp, "<TD BGCOLOR=\"%s\">",
	       enode->get_dot_fillcolor ());
    pp_printf (pp, "<TABLE BORDER=\"0\">");
    gv->begin_trtd ();
    pp_printf (pp, "EN: %i", enode->m_index);
    switch (enode->get_status ())
      {
      default:
	gcc_unreachable ();
      case exploded_node::STATUS_WORKLIST:
	pp_string (pp, "(W)");
	break;
      case exploded_node::STATUS_PROCESSED:
	break;
      case exploded_node::STATUS_MERGER:
	pp_string (pp, "(M)");
	break;
      }
    gv->end_tdtr ();
    /* Dump any saved_diagnostics at this enode.  */
    {
      const diagnostic_manager &dm = m_eg.get_diagnostic_manager ();
      for (unsigned i = 0; i < dm.get_num_diagnostics (); i++)
	{
	  const saved_diagnostic *sd = dm.get_saved_diagnostic (i);
	  if (sd->m_enode == enode)
	    print_saved_diagnostic (gv, sd);
	}
    }
    pp_printf (pp, "</TABLE>");
    pp_printf (pp, "</TD>");
  }

  /* Print a TABLE element for SD, showing the kind, the length of the
     exploded_path, whether the path was feasible, and if infeasible,
     what the problem was.  */
  void print_saved_diagnostic (graphviz_out *gv,
			       const saved_diagnostic *sd) const
  {
    pretty_printer *pp = gv->get_pp ();
    gv->begin_trtd ();
    pp_printf (pp, "<TABLE BORDER=\"0\">");
    gv->begin_tr ();
    pp_string (pp, "<TD BGCOLOR=\"green\">");
    pp_printf (pp, "DIAGNOSTIC: %s", sd->m_d->get_kind ());
    gv->end_tdtr ();
    gv->begin_trtd ();
    pp_printf (pp, "epath length: %i", sd->get_epath_length ());
    gv->end_tdtr ();
    switch (sd->get_status ())
      {
      default:
      case saved_diagnostic::STATUS_NEW:
	gcc_unreachable ();
	break;
      case saved_diagnostic::STATUS_INFEASIBLE_PATH:
	{
	  gv->begin_trtd ();
	  pp_printf (pp, "INFEASIBLE");
	  gv->end_tdtr ();
	  const feasibility_problem *p = sd->get_feasibility_problem ();
	  gcc_assert (p);
	  gv->begin_trtd ();
	  pp_printf (pp, "at eedge %i: EN:%i -> EN:%i",
		     p->m_eedge_idx,
		     p->m_eedge.m_src->m_index,
		     p->m_eedge.m_dest->m_index);
	  pp_write_text_as_html_like_dot_to_stream (pp);
	  gv->end_tdtr ();
	  gv->begin_trtd ();
	  p->m_eedge.m_sedge->dump (pp);
	  pp_write_text_as_html_like_dot_to_stream (pp);
	  gv->end_tdtr ();
	  gv->begin_trtd ();
	  pp_gimple_stmt_1 (pp, p->m_last_stmt, 0, (dump_flags_t)0);
	  pp_write_text_as_html_like_dot_to_stream (pp);
	  gv->end_tdtr ();
	  /* Ideally we'd print p->m_model here; see the notes above about
	     tooltips.  */
	}
	break;
      case saved_diagnostic::STATUS_FEASIBLE_PATH:
	gv->begin_trtd ();
	pp_printf (pp, "FEASIBLE");
	gv->end_tdtr ();
	break;
      }
    pp_printf (pp, "</TABLE>");
    gv->end_tdtr ();
  }

  const exploded_graph &m_eg;
  auto_delete_vec<auto_vec <exploded_node *> > m_enodes_per_snodes;
};

/* Run the analysis "engine".  */

void
impl_run_checkers (logger *logger)
{
  LOG_SCOPE (logger);

  /* If using LTO, ensure that the cgraph nodes have function bodies.  */
  cgraph_node *node;
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (node)
    node->get_untransformed_body ();

  /* Create the supergraph.  */
  supergraph sg (logger);

  state_purge_map *purge_map = NULL;

  if (flag_analyzer_state_purge)
    purge_map = new state_purge_map (sg, logger);

  if (flag_dump_analyzer_supergraph)
    {
      /* Dump supergraph pre-analysis.  */
      auto_timevar tv (TV_ANALYZER_DUMP);
      char *filename = concat (dump_base_name, ".supergraph.dot", NULL);
      supergraph::dump_args_t args ((enum supergraph_dot_flags)0, NULL);
      sg.dump_dot (filename, args);
      free (filename);
    }

  if (flag_dump_analyzer_state_purge)
    {
      auto_timevar tv (TV_ANALYZER_DUMP);
      state_purge_annotator a (purge_map);
      char *filename = concat (dump_base_name, ".state-purge.dot", NULL);
      supergraph::dump_args_t args ((enum supergraph_dot_flags)0, &a);
      sg.dump_dot (filename, args);
      free (filename);
    }

  auto_delete_vec <state_machine> checkers;
  make_checkers (checkers, logger);

  if (logger)
    {
      int i;
      state_machine *sm;
      FOR_EACH_VEC_ELT (checkers, i, sm)
	logger->log ("checkers[%i]: %s", i, sm->get_name ());
    }

  /* Extrinsic state shared by nodes in the graph.  */
  const extrinsic_state ext_state (checkers);

  const analysis_plan plan (sg, logger);

  /* The exploded graph.  */
  exploded_graph eg (sg, logger, ext_state, purge_map, plan,
		     analyzer_verbosity);

  /* Add entrypoints to the graph for externally-callable functions.  */
  eg.build_initial_worklist ();

  /* Now process the worklist, exploring the <point, state> graph.  */
  eg.process_worklist ();

  if (flag_dump_analyzer_exploded_graph)
    {
      auto_timevar tv (TV_ANALYZER_DUMP);
      char *filename
	= concat (dump_base_name, ".eg.dot", NULL);
      exploded_graph::dump_args_t args (eg);
      root_cluster c;
      eg.dump_dot (filename, &c, args);
      free (filename);
    }

  /* Now emit any saved diagnostics.  */
  eg.get_diagnostic_manager ().emit_saved_diagnostics (eg);

  eg.dump_exploded_nodes ();

  eg.log_stats ();

  if (flag_dump_analyzer_callgraph)
    dump_callgraph (sg, &eg);

  if (flag_dump_analyzer_supergraph)
    {
      /* Dump post-analysis form of supergraph.  */
      auto_timevar tv (TV_ANALYZER_DUMP);
      char *filename = concat (dump_base_name, ".supergraph-eg.dot", NULL);
      exploded_graph_annotator a (eg);
      supergraph::dump_args_t args ((enum supergraph_dot_flags)0, &a);
      sg.dump_dot (filename, args);
      free (filename);
    }

  delete purge_map;
}

/* External entrypoint to the analysis "engine".
   Set up any dumps, then call impl_run_checkers.  */

void
run_checkers ()
{
  /* Save input_location.  */
  location_t saved_input_location = input_location;

  /* Handle -fdump-analyzer and -fdump-analyzer-stderr.  */
  FILE *dump_fout = NULL;
  /* Track if we're responsible for closing dump_fout.  */
  bool owns_dump_fout = false;
  if (flag_dump_analyzer_stderr)
    dump_fout = stderr;
  else if (flag_dump_analyzer)
    {
      char *dump_filename = concat (dump_base_name, ".analyzer.txt", NULL);
      dump_fout = fopen (dump_filename, "w");
      free (dump_filename);
      if (dump_fout)
	owns_dump_fout = true;
    }

  {
    log_user the_logger (NULL);
    if (dump_fout)
      the_logger.set_logger (new logger (dump_fout, 0, 0,
					 *global_dc->printer));
    LOG_SCOPE (the_logger.get_logger ());

    impl_run_checkers (the_logger.get_logger ());

    /* end of lifetime of the_logger (so that dump file is closed after the
       various dtors run).  */
  }

  if (owns_dump_fout)
    fclose (dump_fout);

  /* Restore input_location.  Subsequent passes may assume that input_location
     is some arbitrary value *not* in the block tree, which might be violated
     if we didn't restore it.  */
  input_location = saved_input_location;
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
