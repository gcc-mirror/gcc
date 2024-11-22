/* The analysis "engine".
   Copyright (C) 2019-2024 Free Software Foundation, Inc.
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
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "make-unique.h"
#include "tree.h"
#include "fold-const.h"
#include "gcc-rich-location.h"
#include "diagnostic-core.h"
#include "diagnostic-event-id.h"
#include "diagnostic-path.h"
#include "function.h"
#include "pretty-print.h"
#include "sbitmap.h"
#include "bitmap.h"
#include "ordered-hash-map.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
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
#include "analyzer/program-state.h"
#include "analyzer/exploded-graph.h"
#include "analyzer/analysis-plan.h"
#include "analyzer/checker-path.h"
#include "analyzer/state-purge.h"
#include "analyzer/bar-chart.h"
#include "analyzer/call-info.h"
#include <zlib.h>
#include "plugin.h"
#include "target.h"
#include <memory>
#include "stringpool.h"
#include "attribs.h"
#include "tree-dfa.h"
#include "analyzer/known-function-manager.h"
#include "analyzer/call-summary.h"
#include "text-art/dump.h"

/* For an overview, see gcc/doc/analyzer.texi.  */

#if ENABLE_ANALYZER

namespace ana {

/* class impl_region_model_context : public region_model_context.  */

impl_region_model_context::
impl_region_model_context (exploded_graph &eg,
			   exploded_node *enode_for_diag,
			   const program_state *old_state,
			   program_state *new_state,
			   uncertainty_t *uncertainty,
			   path_context *path_ctxt,
			   const gimple *stmt,
			   stmt_finder *stmt_finder,
			   bool *out_could_have_done_work)
: m_eg (&eg), m_logger (eg.get_logger ()),
  m_enode_for_diag (enode_for_diag),
  m_old_state (old_state),
  m_new_state (new_state),
  m_stmt (stmt),
  m_stmt_finder (stmt_finder),
  m_ext_state (eg.get_ext_state ()),
  m_uncertainty (uncertainty),
  m_path_ctxt (path_ctxt),
  m_out_could_have_done_work (out_could_have_done_work)
{
}

impl_region_model_context::
impl_region_model_context (program_state *state,
			   const extrinsic_state &ext_state,
			   uncertainty_t *uncertainty,
			   logger *logger)
: m_eg (NULL), m_logger (logger), m_enode_for_diag (NULL),
  m_old_state (NULL),
  m_new_state (state),
  m_stmt (NULL),
  m_stmt_finder (NULL),
  m_ext_state (ext_state),
  m_uncertainty (uncertainty),
  m_path_ctxt (NULL),
  m_out_could_have_done_work (nullptr)
{
}

bool
impl_region_model_context::warn (std::unique_ptr<pending_diagnostic> d,
				 const stmt_finder *custom_finder)
{
  LOG_FUNC (get_logger ());
  auto curr_stmt_finder = custom_finder ? custom_finder : m_stmt_finder;
  if (m_stmt == NULL && curr_stmt_finder == NULL)
    {
      if (get_logger ())
	get_logger ()->log ("rejecting diagnostic: no stmt");
      return false;
    }
  if (m_eg)
    {
      bool terminate_path = d->terminate_path_p ();
      pending_location ploc (m_enode_for_diag,
			     m_enode_for_diag->get_supernode (),
			     m_stmt,
			     curr_stmt_finder);
      if (m_eg->get_diagnostic_manager ().add_diagnostic (ploc,
							  std::move (d)))
	{
	  if (m_path_ctxt
	      && terminate_path
	      && flag_analyzer_suppress_followups)
	    m_path_ctxt->terminate_path ();
	  return true;
	}
    }
  return false;
}

void
impl_region_model_context::add_note (std::unique_ptr<pending_note> pn)
{
  LOG_FUNC (get_logger ());
  if (m_eg)
    m_eg->get_diagnostic_manager ().add_note (std::move (pn));
}

void
impl_region_model_context::add_event (std::unique_ptr<checker_event> event)
{
  LOG_FUNC (get_logger ());
  if (m_eg)
    m_eg->get_diagnostic_manager ().add_event (std::move (event));
}

void
impl_region_model_context::on_svalue_leak (const svalue *sval)

{
  for (sm_state_map *smap : m_new_state->m_checker_states)
    smap->on_svalue_leak (sval, this);
}

void
impl_region_model_context::
on_liveness_change (const svalue_set &live_svalues,
		    const region_model *model)
{
  for (sm_state_map *smap : m_new_state->m_checker_states)
    smap->on_liveness_change (live_svalues, model, m_ext_state, this);
}

void
impl_region_model_context::on_unknown_change (const svalue *sval,
					      bool is_mutable)
{
  if (!sval->can_have_associated_state_p ())
    return;
  for (sm_state_map *smap : m_new_state->m_checker_states)
    smap->on_unknown_change (sval, is_mutable, m_ext_state);
}

void
impl_region_model_context::on_escaped_function (tree fndecl)
{
  m_eg->on_escaped_function (fndecl);
}

uncertainty_t *
impl_region_model_context::get_uncertainty ()
{
  return m_uncertainty;
}

/* Purge state involving SVAL.  The region_model has already been purged,
   so we only need to purge other state in the program_state:
   the sm-state.  */

void
impl_region_model_context::purge_state_involving (const svalue *sval)
{
  int i;
  sm_state_map *smap;
  FOR_EACH_VEC_ELT (m_new_state->m_checker_states, i, smap)
    smap->purge_state_involving (sval, m_ext_state);
}

void
impl_region_model_context::bifurcate (std::unique_ptr<custom_edge_info> info)
{
  if (m_path_ctxt)
    m_path_ctxt->bifurcate (std::move (info));
}

void
impl_region_model_context::terminate_path ()
{
  if (m_path_ctxt)
    return m_path_ctxt->terminate_path ();
}

/* struct setjmp_record.  */

int
setjmp_record::cmp (const setjmp_record &rec1, const setjmp_record &rec2)
{
  if (int cmp_enode = rec1.m_enode->m_index - rec2.m_enode->m_index)
    return cmp_enode;
  gcc_assert (&rec1 == &rec2);
  return 0;
}

/* class setjmp_svalue : public svalue.  */

/* Implementation of svalue::accept vfunc for setjmp_svalue.  */

void
setjmp_svalue::accept (visitor *v) const
{
  v->visit_setjmp_svalue (this);
}

/* Implementation of svalue::dump_to_pp vfunc for setjmp_svalue.  */

void
setjmp_svalue::dump_to_pp (pretty_printer *pp, bool simple) const
{
  if (simple)
    pp_printf (pp, "SETJMP(EN: %i)", get_enode_index ());
  else
    pp_printf (pp, "setjmp_svalue(EN%i)", get_enode_index ());
}

/* Implementation of svalue::print_dump_widget_label vfunc for
   setjmp_svalue.  */

void
setjmp_svalue::print_dump_widget_label (pretty_printer *pp) const
{
  pp_printf (pp, "setjmp_svalue(EN: %i)", get_enode_index ());
}

/* Implementation of svalue::add_dump_widget_children vfunc for
   setjmp_svalue.  */

void
setjmp_svalue::
add_dump_widget_children (text_art::tree_widget &,
			  const text_art::dump_widget_info &) const
{
  /* No children.  */
}

/* Get the index of the stored exploded_node.  */

int
setjmp_svalue::get_enode_index () const
{
  return m_setjmp_record.m_enode->m_index;
}

/* Concrete implementation of sm_context, wiring it up to the rest of this
   file.  */

class impl_sm_context : public sm_context
{
public:
  impl_sm_context (exploded_graph &eg,
		   int sm_idx,
		   const state_machine &sm,
		   exploded_node *enode_for_diag,
		   const program_state *old_state,
		   program_state *new_state,
		   const sm_state_map *old_smap,
		   sm_state_map *new_smap,
		   path_context *path_ctxt,
		   const stmt_finder *stmt_finder = NULL,
		   bool unknown_side_effects = false)
  : sm_context (sm_idx, sm),
    m_logger (eg.get_logger ()),
    m_eg (eg), m_enode_for_diag (enode_for_diag),
    m_old_state (old_state), m_new_state (new_state),
    m_old_smap (old_smap), m_new_smap (new_smap),
    m_path_ctxt (path_ctxt),
    m_stmt_finder (stmt_finder),
    m_unknown_side_effects (unknown_side_effects)
  {
  }

  logger *get_logger () const { return m_logger.get_logger (); }

  tree get_fndecl_for_call (const gcall *call) final override
  {
    impl_region_model_context old_ctxt
      (m_eg, m_enode_for_diag, NULL, NULL, NULL/*m_enode->get_state ()*/,
       NULL, call);
    region_model *model = m_new_state->m_region_model;
    return model->get_fndecl_for_call (call, &old_ctxt);
  }

  state_machine::state_t get_state (const gimple *stmt ATTRIBUTE_UNUSED,
				    tree var) final override
  {
    logger * const logger = get_logger ();
    LOG_FUNC (logger);
    /* Use NULL ctxt on this get_rvalue call to avoid triggering
       uninitialized value warnings.  */
    const svalue *var_old_sval
      = m_old_state->m_region_model->get_rvalue (var, NULL);

    state_machine::state_t current
      = m_old_smap->get_state (var_old_sval, m_eg.get_ext_state ());
    return current;
  }
  state_machine::state_t get_state (const gimple *stmt ATTRIBUTE_UNUSED,
				    const svalue *sval) final override
  {
    logger * const logger = get_logger ();
    LOG_FUNC (logger);
    state_machine::state_t current
      = m_old_smap->get_state (sval, m_eg.get_ext_state ());
    return current;
  }


  void set_next_state (const gimple *,
		       tree var,
		       state_machine::state_t to,
		       tree origin) final override
  {
    logger * const logger = get_logger ();
    LOG_FUNC (logger);
    const svalue *var_new_sval
      = m_new_state->m_region_model->get_rvalue (var, NULL);
    const svalue *origin_new_sval
      = m_new_state->m_region_model->get_rvalue (origin, NULL);

    /* We use the new sval here to avoid issues with uninitialized values.  */
    state_machine::state_t current
      = m_old_smap->get_state (var_new_sval, m_eg.get_ext_state ());
    if (logger)
      logger->log ("%s: state transition of %qE: %s -> %s",
		   m_sm.get_name (),
		   var,
		   current->get_name (),
		   to->get_name ());
    m_new_smap->set_state (m_new_state->m_region_model, var_new_sval,
			   to, origin_new_sval, m_eg.get_ext_state ());
  }

  void set_next_state (const gimple *stmt,
		       const svalue *sval,
		       state_machine::state_t to,
		       tree origin) final override
  {
    logger * const logger = get_logger ();
    LOG_FUNC (logger);
    impl_region_model_context old_ctxt
      (m_eg, m_enode_for_diag, NULL, NULL, NULL/*m_enode->get_state ()*/,
       NULL, stmt);

    const svalue *origin_new_sval
      = m_new_state->m_region_model->get_rvalue (origin, NULL);

    state_machine::state_t current
      = m_old_smap->get_state (sval, m_eg.get_ext_state ());
    if (logger)
      {
	logger->start_log_line ();
	logger->log_partial ("%s: state transition of ",
			     m_sm.get_name ());
	sval->dump_to_pp (logger->get_printer (), true);
	logger->log_partial (": %s -> %s",
			     current->get_name (),
			     to->get_name ());
	logger->end_log_line ();
      }
    m_new_smap->set_state (m_new_state->m_region_model, sval,
			   to, origin_new_sval, m_eg.get_ext_state ());
  }

  void warn (const supernode *snode, const gimple *stmt,
	     tree var,
	     std::unique_ptr<pending_diagnostic> d) final override
  {
    LOG_FUNC (get_logger ());
    gcc_assert (d);
    const svalue *var_old_sval
      = m_old_state->m_region_model->get_rvalue (var, NULL);
    state_machine::state_t current
      = (var
	 ? m_old_smap->get_state (var_old_sval, m_eg.get_ext_state ())
	 : m_old_smap->get_global_state ());
    bool terminate_path = d->terminate_path_p ();
    pending_location ploc (m_enode_for_diag, snode, stmt, m_stmt_finder);
    m_eg.get_diagnostic_manager ().add_diagnostic
      (&m_sm, ploc,
       var, var_old_sval, current, std::move (d));
    if (m_path_ctxt
	&& terminate_path
	&& flag_analyzer_suppress_followups)
      m_path_ctxt->terminate_path ();
  }

  void warn (const supernode *snode, const gimple *stmt,
	     const svalue *sval,
	     std::unique_ptr<pending_diagnostic> d) final override
  {
    LOG_FUNC (get_logger ());
    gcc_assert (d);
    state_machine::state_t current
      = (sval
	 ? m_old_smap->get_state (sval, m_eg.get_ext_state ())
	 : m_old_smap->get_global_state ());
    bool terminate_path = d->terminate_path_p ();
    pending_location ploc (m_enode_for_diag, snode, stmt, m_stmt_finder);
    m_eg.get_diagnostic_manager ().add_diagnostic
      (&m_sm, ploc,
       NULL_TREE, sval, current, std::move (d));
    if (m_path_ctxt
	&& terminate_path
	&& flag_analyzer_suppress_followups)
      m_path_ctxt->terminate_path ();
  }

  /* Hook for picking more readable trees for SSA names of temporaries,
     so that rather than e.g.
       "double-free of '<unknown>'"
     we can print:
       "double-free of 'inbuf.data'".  */

  tree get_diagnostic_tree (tree expr) final override
  {
    /* Only for SSA_NAMEs of temporaries; otherwise, return EXPR, as it's
       likely to be the least surprising tree to report.  */
    if (TREE_CODE (expr) != SSA_NAME)
      return expr;
    if (SSA_NAME_VAR (expr) != NULL)
      return expr;

    gcc_assert (m_new_state);
    const svalue *sval = m_new_state->m_region_model->get_rvalue (expr, NULL);
    /* Find trees for all regions storing the value.  */
    if (tree t = m_new_state->m_region_model->get_representative_tree (sval))
      return t;
    else
      return expr;
  }

  tree get_diagnostic_tree (const svalue *sval) final override
  {
    return m_new_state->m_region_model->get_representative_tree (sval);
  }

  state_machine::state_t get_global_state () const final override
  {
    return m_old_state->m_checker_states[m_sm_idx]->get_global_state ();
  }

  void set_global_state (state_machine::state_t state) final override
  {
    m_new_state->m_checker_states[m_sm_idx]->set_global_state (state);
  }

  void clear_all_per_svalue_state () final override
  {
    m_new_state->m_checker_states[m_sm_idx]->clear_all_per_svalue_state ();
  }

  void on_custom_transition (custom_transition *transition) final override
  {
    transition->impl_transition (&m_eg,
				 const_cast<exploded_node *> (m_enode_for_diag),
				 m_sm_idx);
  }

  tree is_zero_assignment (const gimple *stmt) final override
  {
    const gassign *assign_stmt = dyn_cast <const gassign *> (stmt);
    if (!assign_stmt)
     return NULL_TREE;
    impl_region_model_context old_ctxt
      (m_eg, m_enode_for_diag, m_old_state, m_new_state, NULL, NULL, stmt);
    if (const svalue *sval
	= m_new_state->m_region_model->get_gassign_result (assign_stmt,
							    &old_ctxt))
      if (tree cst = sval->maybe_get_constant ())
	if (::zerop(cst))
	  return gimple_assign_lhs (assign_stmt);
    return NULL_TREE;
  }

  path_context *get_path_context () const final override
  {
    return m_path_ctxt;
  }

  bool unknown_side_effects_p () const final override
  {
    return m_unknown_side_effects;
  }

  const program_state *get_old_program_state () const final override
  {
    return m_old_state;
  }

  const program_state *get_new_program_state () const final override
  {
    return m_new_state;
  }

  log_user m_logger;
  exploded_graph &m_eg;
  exploded_node *m_enode_for_diag;
  const program_state *m_old_state;
  program_state *m_new_state;
  const sm_state_map *m_old_smap;
  sm_state_map *m_new_smap;
  path_context *m_path_ctxt;
  const stmt_finder *m_stmt_finder;

  /* Are we handling an external function with unknown side effects?  */
  bool m_unknown_side_effects;
};

bool
impl_region_model_context::
get_state_map_by_name (const char *name,
		       sm_state_map **out_smap,
		       const state_machine **out_sm,
		       unsigned *out_sm_idx,
		       std::unique_ptr<sm_context> *out_sm_context)
{
  if (!m_new_state)
    return false;

  unsigned sm_idx;
  if (!m_ext_state.get_sm_idx_by_name (name, &sm_idx))
    return false;

  const state_machine *sm = &m_ext_state.get_sm (sm_idx);
  sm_state_map *new_smap = m_new_state->m_checker_states[sm_idx];

  *out_smap = new_smap;
  *out_sm = sm;
  if (out_sm_idx)
    *out_sm_idx = sm_idx;
  if (out_sm_context)
    {
      const sm_state_map *old_smap = m_old_state->m_checker_states[sm_idx];
      *out_sm_context
	= make_unique<impl_sm_context> (*m_eg,
					sm_idx,
					*sm,
					m_enode_for_diag,
					m_old_state,
					m_new_state,
					old_smap,
					new_smap,
					m_path_ctxt,
					m_stmt_finder,
					false);
    }
  return true;
}

/* Subclass of stmt_finder for finding the best stmt to report the leak at,
   given the emission path.  */

class leak_stmt_finder : public stmt_finder
{
public:
  leak_stmt_finder (const exploded_graph &eg, tree var)
  : m_eg (eg), m_var (var) {}

  std::unique_ptr<stmt_finder> clone () const final override
  {
    return make_unique<leak_stmt_finder> (m_eg, m_var);
  }

  const gimple *find_stmt (const exploded_path &epath)
    final override
  {
    logger * const logger = m_eg.get_logger ();
    LOG_FUNC (logger);

    if (m_var && TREE_CODE (m_var) == SSA_NAME)
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

  void update_event_loc_info (event_loc_info &) final override
  {
    /* No-op.  */
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
  /* Arbitrarily-chosen "high readability" value.  */
  const int HIGH_READABILITY = 65536;

  gcc_assert (expr);
  switch (TREE_CODE (expr))
    {
    case COMPONENT_REF:
    case MEM_REF:
      /* Impose a slight readability penalty relative to that of
	 operand 0.  */
      return readability (TREE_OPERAND (expr, 0)) - 16;

    case SSA_NAME:
      {
	if (tree var = SSA_NAME_VAR (expr))
	  {
	    if (DECL_ARTIFICIAL (var))
	      {
		/* If we have an SSA name for an artificial var,
		   only use it if it has a debug expr associated with
		   it that fixup_tree_for_diagnostic can use.  */
		if (VAR_P (var) && DECL_HAS_DEBUG_EXPR_P (var))
		  return readability (DECL_DEBUG_EXPR (var)) - 1;
	      }
	    else
	      {
		/* Slightly favor the underlying var over the SSA name to
		   avoid having them compare equal.  */
		return readability (var) - 1;
	      }
	  }
	/* Avoid printing '<unknown>' for SSA names for temporaries.  */
	return -1;
      }
      break;

    case PARM_DECL:
    case VAR_DECL:
      if (DECL_NAME (expr))
	return HIGH_READABILITY;
      else
	/* We don't want to print temporaries.  For example, the C FE
	   prints them as e.g. "<Uxxxx>" where "xxxx" is the low 16 bits
	   of the tree pointer (see pp_c_tree_decl_identifier).  */
	return -1;

    case RESULT_DECL:
      /* Printing "<return-value>" isn't ideal, but is less awful than
	 trying to print a temporary.  */
      return HIGH_READABILITY / 2;

    case NOP_EXPR:
      {
	/* Impose a moderate readability penalty for casts.  */
	const int CAST_PENALTY = 32;
	return readability (TREE_OPERAND (expr, 0)) - CAST_PENALTY;
      }

    case INTEGER_CST:
      return HIGH_READABILITY;

    default:
      return 0;
    }

  return 0;
}

/* A qsort comparator for trees to sort them into most user-readable to
   least user-readable.  */

int
readability_comparator (const void *p1, const void *p2)
{
  path_var pv1 = *(path_var const *)p1;
  path_var pv2 = *(path_var const *)p2;

  const int tree_r1 = readability (pv1.m_tree);
  const int tree_r2 = readability (pv2.m_tree);

  /* Favor items that are deeper on the stack and hence more recent;
     this also favors locals over globals.  */
  const int COST_PER_FRAME = 64;
  const int depth_r1 = pv1.m_stack_depth * COST_PER_FRAME;
  const int depth_r2 = pv2.m_stack_depth * COST_PER_FRAME;

  /* Combine the scores from the tree and from the stack depth.
     This e.g. lets us have a slightly penalized cast in the most
     recent stack frame "beat" an uncast value in an older stack frame.  */
  const int sum_r1 = tree_r1 + depth_r1;
  const int sum_r2 = tree_r2 + depth_r2;
  if (int cmp = sum_r2 - sum_r1)
    return cmp;

  /* Otherwise, more readable trees win.  */
  if (int cmp = tree_r2 - tree_r1)
    return cmp;

  /* Otherwise, if they have the same readability, then impose an
     arbitrary deterministic ordering on them.  */

  if (int cmp = TREE_CODE (pv1.m_tree) - TREE_CODE (pv2.m_tree))
    return cmp;

  switch (TREE_CODE (pv1.m_tree))
    {
    default:
      break;
    case SSA_NAME:
      if (int cmp = (SSA_NAME_VERSION (pv1.m_tree)
		     - SSA_NAME_VERSION (pv2.m_tree)))
	return cmp;
      break;
    case PARM_DECL:
    case VAR_DECL:
    case RESULT_DECL:
      if (int cmp = DECL_UID (pv1.m_tree) - DECL_UID (pv2.m_tree))
	return cmp;
      break;
    }

  /* TODO: We ought to find ways of sorting such cases.  */
  return 0;
}

/* Return true is SNODE is the EXIT node of a function, or is one
   of the final snodes within its function.

   Specifically, handle the final supernodes before the EXIT node,
   for the case of clobbers that happen immediately before exiting.
   We need a run of snodes leading to the return_p snode, where all edges are
   intraprocedural, and every snode has just one successor.

   We use this when suppressing leak reports at the end of "main".  */

static bool
returning_from_function_p (const supernode *snode)
{
  if (!snode)
    return false;

  unsigned count = 0;
  const supernode *iter = snode;
  while (true)
    {
      if (iter->return_p ())
	return true;
      if (iter->m_succs.length () != 1)
	return false;
      const superedge *sedge = iter->m_succs[0];
      if (sedge->get_kind () != SUPEREDGE_CFG_EDGE)
	return false;
      iter = sedge->m_dest;

      /* Impose a limit to ensure we terminate for pathological cases.

	 We only care about the final 3 nodes, due to cases like:
	   BB:
	     (clobber causing leak)

	   BB:
	   <label>:
	   return _val;

	   EXIT BB.*/
      if (++count > 3)
	return false;
    }
}

/* Find the best tree for SVAL and call SM's on_leak vfunc with it.
   If on_leak returns a pending_diagnostic, queue it up to be reported,
   so that we potentially complain about a leak of SVAL in the given STATE.  */

void
impl_region_model_context::on_state_leak (const state_machine &sm,
					  const svalue *sval,
					  state_machine::state_t state)
{
  logger * const logger = get_logger ();
  LOG_SCOPE (logger);
  if (logger)
    {
      logger->start_log_line ();
      logger->log_partial ("considering leak of ");
      sval->dump_to_pp (logger->get_printer (), true);
      logger->end_log_line ();
    }

  if (!m_eg)
    return;

  /* m_old_state also needs to be non-NULL so that the sm_ctxt can look
     up the old state of SVAL.  */
  gcc_assert (m_old_state);

  /* SVAL has leaked within the new state: it is not used by any reachable
     regions.
     We need to convert it back to a tree, but since it's likely no regions
     use it, we have to find the "best" tree for it in the old_state.  */
  svalue_set visited;
  path_var leaked_pv
    = m_old_state->m_region_model->get_representative_path_var (sval,
								&visited,
								nullptr);

  /* Strip off top-level casts  */
  if (leaked_pv.m_tree && TREE_CODE (leaked_pv.m_tree) == NOP_EXPR)
    leaked_pv.m_tree = TREE_OPERAND (leaked_pv.m_tree, 0);

  /* This might be NULL; the pending_diagnostic subclasses need to cope
     with this.  */
  tree leaked_tree = leaked_pv.m_tree;
  if (logger)
    {
      if (leaked_tree)
	logger->log ("best leaked_tree: %qE", leaked_tree);
      else
	logger->log ("best leaked_tree: NULL");
    }

  leak_stmt_finder stmt_finder (*m_eg, leaked_tree);
  gcc_assert (m_enode_for_diag);

  /* Don't complain about leaks when returning from "main".  */
  if (returning_from_function_p (m_enode_for_diag->get_supernode ()))
    {
      tree fndecl = m_enode_for_diag->get_function ()->decl;
      if (id_equal (DECL_NAME (fndecl), "main"))
	{
	  if (logger)
	    logger->log ("not reporting leak from main");
	  return;
	}
    }

  tree leaked_tree_for_diag = fixup_tree_for_diagnostic (leaked_tree);
  std::unique_ptr<pending_diagnostic> pd = sm.on_leak (leaked_tree_for_diag);
  if (pd)
    {
      pending_location ploc (m_enode_for_diag,
			     m_enode_for_diag->get_supernode (),
			     m_stmt,
			     &stmt_finder);
      m_eg->get_diagnostic_manager ().add_diagnostic
	(&sm, ploc,
	 leaked_tree_for_diag, sval, state, std::move (pd));
    }
}

/* Implementation of region_model_context::on_condition vfunc.
   Notify all state machines about the condition, which could lead to
   state transitions.  */

void
impl_region_model_context::on_condition (const svalue *lhs,
					 enum tree_code op,
					 const svalue *rhs)
{
  int sm_idx;
  sm_state_map *smap;
  FOR_EACH_VEC_ELT (m_new_state->m_checker_states, sm_idx, smap)
    {
      const state_machine &sm = m_ext_state.get_sm (sm_idx);
      impl_sm_context sm_ctxt (*m_eg, sm_idx, sm, m_enode_for_diag,
			       m_old_state, m_new_state,
			       m_old_state->m_checker_states[sm_idx],
			       m_new_state->m_checker_states[sm_idx],
			       m_path_ctxt);
      sm.on_condition (sm_ctxt,
		       (m_enode_for_diag
			? m_enode_for_diag->get_supernode ()
			: NULL),
		       m_stmt,
		       lhs, op, rhs);
    }
}

/* Implementation of region_model_context::on_bounded_ranges vfunc.
   Notify all state machines about the ranges, which could lead to
   state transitions.  */

void
impl_region_model_context::on_bounded_ranges (const svalue &sval,
					      const bounded_ranges &ranges)
{
  int sm_idx;
  sm_state_map *smap;
  FOR_EACH_VEC_ELT (m_new_state->m_checker_states, sm_idx, smap)
    {
      const state_machine &sm = m_ext_state.get_sm (sm_idx);
      impl_sm_context sm_ctxt (*m_eg, sm_idx, sm, m_enode_for_diag,
			       m_old_state, m_new_state,
			       m_old_state->m_checker_states[sm_idx],
			       m_new_state->m_checker_states[sm_idx],
			       m_path_ctxt);
      sm.on_bounded_ranges (sm_ctxt,
			    (m_enode_for_diag
			     ? m_enode_for_diag->get_supernode ()
			     : NULL),
			    m_stmt, sval, ranges);
    }
}

/* Implementation of region_model_context::on_pop_frame vfunc.
   Notify all state machines about the frame being popped, which
   could lead to states being discarded.  */

void
impl_region_model_context::on_pop_frame (const frame_region *frame_reg)
{
  int sm_idx;
  sm_state_map *smap;
  FOR_EACH_VEC_ELT (m_new_state->m_checker_states, sm_idx, smap)
    {
      const state_machine &sm = m_ext_state.get_sm (sm_idx);
      sm.on_pop_frame (smap, frame_reg);
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
			       m_old_state->m_checker_states[sm_idx],
			       m_new_state->m_checker_states[sm_idx],
			       m_path_ctxt);
      sm.on_phi (sm_ctxt, m_enode_for_diag->get_supernode (), phi, rhs);
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

/* Implementation of region_model_context::maybe_did_work vfunc.
   Mark that "externally visible work" has occurred, and thus we
   shouldn't report an infinite loop here.  */

void
impl_region_model_context::maybe_did_work ()
{
  if (m_out_could_have_done_work)
    *m_out_could_have_done_work = true;
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
  for (const frame_region *iter_frame
	 = m_state.m_region_model->get_current_frame ();
       iter_frame; iter_frame = iter_frame->get_calling_frame ())
    {
      int index = iter_frame->get_index ();
      gcc_assert (m_point.get_function_at_depth (index)
		  == &iter_frame->get_function ());
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

/* struct eg_traits::dump_args_t.  */

/* The <FILENAME>.eg.dot output can quickly become unwieldy if we show
   full details for all enodes (both in terms of CPU time to render it,
   and in terms of being meaningful to a human viewing it).

   If we show just the IDs then the resulting graph is usually viewable,
   but then we have to keep switching back and forth between the .dot
   view and other dumps.

   This function implements a heuristic for showing detail at the enodes
   that (we hope) matter, and just the ID at other enodes, fixing the CPU
   usage of the .dot viewer, and drawing the attention of the viewer
   to these enodes.

   Return true if ENODE should be shown in detail in .dot output.
   Return false if no detail should be shown for ENODE.  */

bool
eg_traits::dump_args_t::show_enode_details_p (const exploded_node &enode) const
{
  /* If the number of exploded nodes isn't too large, we may as well show
     all enodes in full detail in the .dot output.  */
  if (m_eg.m_nodes.length ()
	<= (unsigned) param_analyzer_max_enodes_for_full_dump)
    return true;

  /* Otherwise, assume that what's most interesting are state explosions,
     and thus the places where this happened.
     Expand enodes at program points where we hit the per-enode limit, so we
     can investigate what exploded.  */
  const per_program_point_data *per_point_data
    = m_eg.get_per_program_point_data (enode.get_point ());
  return per_point_data->m_excess_enodes > 0;
}

/* class exploded_node : public dnode<eg_traits>.  */

const char *
exploded_node::status_to_str (enum status s)
{
  switch (s)
    {
    default: gcc_unreachable ();
    case STATUS_WORKLIST: return "WORKLIST";
    case STATUS_PROCESSED: return "PROCESSED";
    case STATUS_MERGER: return "MERGER";
    case STATUS_BULK_MERGED: return "BULK_MERGED";
    }
}

/* exploded_node's ctor.  */

exploded_node::exploded_node (const point_and_state &ps,
			      int index)
: m_ps (ps), m_status (STATUS_WORKLIST), m_index (index),
  m_num_processed_stmts (0)
{
  gcc_checking_assert (ps.get_state ().m_region_model->canonicalized_p ());
}

/* Get the stmt that was processed in this enode at index IDX.
   IDX is an index within the stmts processed at this enode, rather
   than within those of the supernode.  */

const gimple *
exploded_node::get_processed_stmt (unsigned idx) const
{
  gcc_assert (idx < m_num_processed_stmts);
  const program_point &point = get_point ();
  gcc_assert (point.get_kind () == PK_BEFORE_STMT);
  const supernode *snode = get_supernode ();
  const unsigned int point_stmt_idx = point.get_stmt_idx ();
  const unsigned int idx_within_snode = point_stmt_idx + idx;
  const gimple *stmt = snode->m_stmts[idx_within_snode];
  return stmt;
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
	total_sm_state += (*iter).second.m_state->get_id ();
      total_sm_state += smap->get_global_state ()->get_id ();
    }

  if (total_sm_state > 0)
    {
      /* An arbitrarily-picked collection of light colors.  */
      const char * const colors[]
	= {"azure", "coral", "cornsilk", "lightblue", "yellow",
	   "honeydew", "lightpink", "lightsalmon", "palegreen1",
	   "wheat", "seashell"};
      const int num_colors = ARRAY_SIZE (colors);
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
  else if (m_status == STATUS_BULK_MERGED)
    pp_string (pp, " (bulk merged)");
  pp_newline (pp);

  if (args.show_enode_details_p (*this))
    {
      format f (true);
      m_ps.get_point ().print (pp, f);
      pp_newline (pp);

      const extrinsic_state &ext_state = args.m_eg.get_ext_state ();
      const program_state &state = m_ps.get_state ();
      state.dump_to_pp (ext_state, false, true, pp);
      pp_newline (pp);

      dump_processed_stmts (pp);
    }

  dump_saved_diagnostics (pp);

  args.dump_extra_info (this, pp);

  pp_write_text_as_dot_label_to_stream (pp, /*for_record=*/true);

  pp_string (pp, "\"];\n\n");

  /* It can be hard to locate the saved diagnostics as text within the
     enode nodes, so add extra nodes to the graph for each saved_diagnostic,
     highlighted in red.
     Compare with dump_saved_diagnostics.  */
  {
    unsigned i;
    const saved_diagnostic *sd;
    FOR_EACH_VEC_ELT (m_saved_diagnostics, i, sd)
      {
	sd->dump_as_dot_node (pp);

	/* Add edge connecting this enode to the saved_diagnostic.  */
	dump_dot_id (pp);
	pp_string (pp, " -> ");
	sd->dump_dot_id (pp);
	pp_string (pp, " [style=\"dotted\" arrowhead=\"none\"];");
	pp_newline (pp);
      }
  }

  pp_flush (pp);
}

/* Show any stmts that were processed within this enode,
   and their index within the supernode.  */
void
exploded_node::dump_processed_stmts (pretty_printer *pp) const
{
  if (m_num_processed_stmts > 0)
    {
      const program_point &point = get_point ();
      gcc_assert (point.get_kind () == PK_BEFORE_STMT);
      const supernode *snode = get_supernode ();
      const unsigned int point_stmt_idx = point.get_stmt_idx ();

      pp_printf (pp, "stmts: %i", m_num_processed_stmts);
      pp_newline (pp);
      for (unsigned i = 0; i < m_num_processed_stmts; i++)
	{
	  const unsigned int idx_within_snode = point_stmt_idx + i;
	  const gimple *stmt = snode->m_stmts[idx_within_snode];
	  pp_printf (pp, "  %i: ", idx_within_snode);
	  pp_gimple_stmt_1 (pp, stmt, 0, (dump_flags_t)0);
	  pp_newline (pp);
	}
    }
}

/* Dump any saved_diagnostics at this enode to PP.  */

void
exploded_node::dump_saved_diagnostics (pretty_printer *pp) const
{
  unsigned i;
  const saved_diagnostic *sd;
  FOR_EACH_VEC_ELT (m_saved_diagnostics, i, sd)
    {
      pp_printf (pp, "DIAGNOSTIC: %s (sd: %i)",
		 sd->m_d->get_kind (), sd->get_index ());
      pp_newline (pp);
    }
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

  m_ps.get_state ().dump_to_pp (ext_state, false, true, pp);
  pp_newline (pp);
}

/* Dump a multiline representation of this node to FILE.  */

void
exploded_node::dump (FILE *fp,
		     const extrinsic_state &ext_state) const
{
  tree_dump_pretty_printer pp (fp);
  dump_to_pp (&pp, ext_state);
}

/* Dump a multiline representation of this node to stderr.  */

DEBUG_FUNCTION void
exploded_node::dump (const extrinsic_state &ext_state) const
{
  dump (stderr, ext_state);
}

/* Return a new json::object of the form
   {"point"  : object for program_point,
    "state"  : object for program_state,
    "status" : str,
    "idx"    : int,
    "processed_stmts" : int}.  */

std::unique_ptr<json::object>
exploded_node::to_json (const extrinsic_state &ext_state) const
{
  auto enode_obj = ::make_unique<json::object> ();

  enode_obj->set ("point", get_point ().to_json ());
  enode_obj->set ("state", get_state ().to_json (ext_state));
  enode_obj->set_string ("status", status_to_str (m_status));
  enode_obj->set_integer ("idx", m_index);
  enode_obj->set_integer ("processed_stmts", m_num_processed_stmts);

  return enode_obj;
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

/* Modify STATE in place, applying the effects of the stmt at this node's
   point.  */

exploded_node::on_stmt_flags
exploded_node::on_stmt (exploded_graph &eg,
			const supernode *snode,
			const gimple *stmt,
			program_state *state,
			uncertainty_t *uncertainty,
			bool *out_could_have_done_work,
			path_context *path_ctxt)
{
  logger *logger = eg.get_logger ();
  LOG_SCOPE (logger);
  if (logger)
    {
      logger->start_log_line ();
      pp_gimple_stmt_1 (logger->get_printer (), stmt, 0, (dump_flags_t)0);
      logger->end_log_line ();
    }

  /* Update input_location in case of ICE: make it easier to track down which
     source construct we're failing to handle.  */
  input_location = stmt->location;

  gcc_assert (state->m_region_model);

  /* Preserve the old state.  It is used here for looking
     up old checker states, for determining state transitions, and
     also within impl_region_model_context and impl_sm_context for
     going from tree to svalue_id.  */
  const program_state old_state (*state);

  impl_region_model_context ctxt (eg, this,
				  &old_state, state, uncertainty,
				  path_ctxt, stmt, nullptr,
				  out_could_have_done_work);

  /* Handle call summaries here.  */
  if (cgraph_edge *cgedge
	  = supergraph_call_edge (snode->get_function (), stmt))
    if (eg.get_analysis_plan ().use_summary_p (cgedge))
      {
	function *called_fn = get_ultimate_function_for_cgraph_edge (cgedge);
	per_function_data *called_fn_data
	  = eg.get_per_function_data (called_fn);
	if (called_fn_data)
	  {
	    gcc_assert (called_fn);
	    return replay_call_summaries (eg,
					  snode,
					  as_a <const gcall *> (stmt),
					  state,
					  path_ctxt,
					  *called_fn,
					  *called_fn_data,
					  &ctxt);
	  }
      }

  bool unknown_side_effects = false;
  bool terminate_path = false;

  on_stmt_pre (eg, stmt, state, &terminate_path,
	       &unknown_side_effects, &ctxt);

  if (terminate_path)
    return on_stmt_flags::terminate_path ();

  int sm_idx;
  sm_state_map *smap;
  FOR_EACH_VEC_ELT (old_state.m_checker_states, sm_idx, smap)
    {
      const state_machine &sm = eg.get_ext_state ().get_sm (sm_idx);
      const sm_state_map *old_smap
	= old_state.m_checker_states[sm_idx];
      sm_state_map *new_smap = state->m_checker_states[sm_idx];
      impl_sm_context sm_ctxt (eg, sm_idx, sm, this, &old_state, state,
			       old_smap, new_smap, path_ctxt, NULL,
			       unknown_side_effects);

      /* Allow the state_machine to handle the stmt.  */
      if (sm.on_stmt (sm_ctxt, snode, stmt))
	unknown_side_effects = false;
    }

  if (path_ctxt->terminate_path_p ())
    return on_stmt_flags::terminate_path ();

  on_stmt_post (stmt, state, unknown_side_effects, &ctxt);

  return on_stmt_flags ();
}

/* Handle the pre-sm-state part of STMT, modifying STATE in-place.
   Write true to *OUT_TERMINATE_PATH if the path should be terminated.
   Write true to *OUT_UNKNOWN_SIDE_EFFECTS if the stmt has unknown
   side effects.  */

void
exploded_node::on_stmt_pre (exploded_graph &eg,
			    const gimple *stmt,
			    program_state *state,
			    bool *out_terminate_path,
			    bool *out_unknown_side_effects,
			    region_model_context *ctxt)
{
  /* Handle special-case calls that require the full program_state.  */
  if (const gcall *call = dyn_cast <const gcall *> (stmt))
    {
      if (is_special_named_call_p (call, "__analyzer_dump", 0))
	{
	  /* Handle the builtin "__analyzer_dump" by dumping state
	     to stderr.  */
	  state->dump (eg.get_ext_state (), true);
	  return;
	}
      else if (is_special_named_call_p (call, "__analyzer_dump_state", 2))
	{
	  state->impl_call_analyzer_dump_state (call, eg.get_ext_state (),
						ctxt);
	  return;
	}
      else if (is_setjmp_call_p (call))
	{
	  state->m_region_model->on_setjmp (call, this, ctxt);
	  if (ctxt)
	    ctxt->maybe_did_work ();
	  return;
	}
      else if (is_longjmp_call_p (call))
	{
	  on_longjmp (eg, call, state, ctxt);
	  *out_terminate_path = true;
	  if (ctxt)
	    ctxt->maybe_did_work ();
	  return;
	}
    }

  /* Otherwise, defer to m_region_model.  */
  state->m_region_model->on_stmt_pre (stmt,
				      out_unknown_side_effects,
				      ctxt);
}

/* Handle the post-sm-state part of STMT, modifying STATE in-place.  */

void
exploded_node::on_stmt_post (const gimple *stmt,
			     program_state *state,
			     bool unknown_side_effects,
			     region_model_context *ctxt)
{
  if (const gcall *call = dyn_cast <const gcall *> (stmt))
    state->m_region_model->on_call_post (call, unknown_side_effects, ctxt);
}

/* A concrete call_info subclass representing a replay of a call summary.  */

class call_summary_edge_info : public call_info
{
public:
  call_summary_edge_info (const call_details &cd,
			  const function &called_fn,
			  call_summary *summary,
			  const extrinsic_state &ext_state)
  : call_info (cd, called_fn),
    m_called_fn (called_fn),
    m_summary (summary),
    m_ext_state (ext_state)
  {}

  bool update_state (program_state *state,
		     const exploded_edge *,
		     region_model_context *ctxt) const final override
  {
    /* Update STATE based on summary_end_state.  */
    call_details cd (get_call_details (state->m_region_model, ctxt));
    call_summary_replay r (cd, m_called_fn, m_summary, m_ext_state);
    const program_state &summary_end_state = m_summary->get_state ();
    return state->replay_call_summary (r, summary_end_state);
  }

  bool update_model (region_model *model,
		     const exploded_edge *,
		     region_model_context *ctxt) const final override
  {
    /* Update STATE based on summary_end_state.  */
    call_details cd (get_call_details (model, ctxt));
    call_summary_replay r (cd, m_called_fn, m_summary, m_ext_state);
    const program_state &summary_end_state = m_summary->get_state ();
    model->replay_call_summary (r, *summary_end_state.m_region_model);
    return true;
  }

  void print_desc (pretty_printer &pp) const final override
  {
    pp_string (&pp, m_summary->get_desc ().get ());
  }

private:
  const function &m_called_fn;
  call_summary *m_summary;
  const extrinsic_state &m_ext_state;
};

/* Use PATH_CTXT to bifurcate, which when handled will add custom edges
   for a replay of the various feasible summaries in CALLED_FN_DATA.  */

exploded_node::on_stmt_flags
exploded_node::replay_call_summaries (exploded_graph &eg,
				      const supernode *snode,
				      const gcall *call_stmt,
				      program_state *state,
				      path_context *path_ctxt,
				      const function &called_fn,
				      per_function_data &called_fn_data,
				      region_model_context *ctxt)
{
  logger *logger = eg.get_logger ();
  LOG_SCOPE (logger);

  /* Each summary will call bifurcate on the PATH_CTXT.  */
  for (auto summary : called_fn_data.m_summaries)
    replay_call_summary (eg, snode, call_stmt, state,
			 path_ctxt, called_fn, summary, ctxt);
  path_ctxt->terminate_path ();

  return on_stmt_flags ();
}

/* Use PATH_CTXT to bifurcate, which when handled will add a
   custom edge for a replay of SUMMARY, if the summary's
   conditions are feasible based on the current state.  */

void
exploded_node::replay_call_summary (exploded_graph &eg,
				    const supernode *snode,
				    const gcall *call_stmt,
				    program_state *old_state,
				    path_context *path_ctxt,
				    const function &called_fn,
				    call_summary *summary,
				    region_model_context *ctxt)
{
  logger *logger = eg.get_logger ();
  LOG_SCOPE (logger);
  gcc_assert (snode);
  gcc_assert (call_stmt);
  gcc_assert (old_state);
  gcc_assert (summary);

  if (logger)
    logger->log ("using %s as summary for call to %qE from %qE",
		 summary->get_desc ().get (),
		 called_fn.decl,
		 snode->get_function ()->decl);
  const extrinsic_state &ext_state = eg.get_ext_state ();
  const program_state &summary_end_state = summary->get_state ();
  if (logger)
    {
      pretty_printer *pp = logger->get_printer ();

      logger->start_log_line ();
      pp_string (pp, "callsite state: ");
      old_state->dump_to_pp (ext_state, true, false, pp);
      logger->end_log_line ();

      logger->start_log_line ();
      pp_string (pp, "summary end state: ");
      summary_end_state.dump_to_pp (ext_state, true, false, pp);
      logger->end_log_line ();
    }

  program_state new_state (*old_state);

  call_details cd (call_stmt, new_state.m_region_model, ctxt);
  call_summary_replay r (cd, called_fn, summary, ext_state);

  if (path_ctxt)
    path_ctxt->bifurcate (make_unique<call_summary_edge_info> (cd,
							       called_fn,
							       summary,
							       ext_state));
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
			uncertainty_t *uncertainty)
{
  LOG_FUNC (eg.get_logger ());

  if (!next_point->on_edge (eg, succ))
    return false;

  if (!next_state->on_edge (eg, this, succ, uncertainty))
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

class stale_jmp_buf : public pending_diagnostic_subclass<stale_jmp_buf>
{
public:
  stale_jmp_buf (const gcall *setjmp_call, const gcall *longjmp_call,
		 const program_point &setjmp_point)
  : m_setjmp_call (setjmp_call), m_longjmp_call (longjmp_call),
    m_setjmp_point (setjmp_point), m_stack_pop_event (NULL)
  {}

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_stale_setjmp_buffer;
  }

  bool emit (diagnostic_emission_context &ctxt) final override
  {
    return ctxt.warn ("%qs called after enclosing function of %qs has returned",
		      get_user_facing_name (m_longjmp_call),
		      get_user_facing_name (m_setjmp_call));
  }

  const char *get_kind () const final override
  { return "stale_jmp_buf"; }

  bool operator== (const stale_jmp_buf &other) const
  {
    return (m_setjmp_call == other.m_setjmp_call
	    && m_longjmp_call == other.m_longjmp_call);
  }

  bool
  maybe_add_custom_events_for_superedge (const exploded_edge &eedge,
					 checker_path *emission_path)
    final override
  {
    /* Detect exactly when the stack first becomes invalid,
       and issue an event then.  */
    if (m_stack_pop_event)
      return false;
    const exploded_node *src_node = eedge.m_src;
    const program_point &src_point = src_node->get_point ();
    const exploded_node *dst_node = eedge.m_dest;
    const program_point &dst_point = dst_node->get_point ();
    if (valid_longjmp_stack_p (src_point, m_setjmp_point)
	&& !valid_longjmp_stack_p (dst_point, m_setjmp_point))
      {
	/* Compare with diagnostic_manager::add_events_for_superedge.  */
	const int src_stack_depth = src_point.get_stack_depth ();
	m_stack_pop_event = new precanned_custom_event
	  (event_loc_info (src_point.get_location (),
			   src_point.get_fndecl (),
			   src_stack_depth),
	   "stack frame is popped here, invalidating saved environment");
	emission_path->add_event
	  (std::unique_ptr<custom_event> (m_stack_pop_event));
	return false;
      }
    return false;
  }

  bool
  describe_final_event (pretty_printer &pp,
			const evdesc::final_event &) final override
  {
    if (m_stack_pop_event)
      pp_printf (&pp,
		 "%qs called after enclosing function of %qs returned at %@",
		 get_user_facing_name (m_longjmp_call),
		 get_user_facing_name (m_setjmp_call),
		 m_stack_pop_event->get_id_ptr ());
    else
      pp_printf (&pp,
		 "%qs called after enclosing function of %qs has returned",
		 get_user_facing_name (m_longjmp_call),
		 get_user_facing_name (m_setjmp_call));
    return true;
  }


private:
  const gcall *m_setjmp_call;
  const gcall *m_longjmp_call;
  program_point m_setjmp_point;
  custom_event *m_stack_pop_event;
};

/* Handle LONGJMP_CALL, a call to longjmp or siglongjmp.

   Attempt to locate where setjmp/sigsetjmp was called on the jmp_buf and build
   an exploded_node and exploded_edge to it representing a rewind to that frame,
   handling the various kinds of failure that can occur.  */

void
exploded_node::on_longjmp (exploded_graph &eg,
			   const gcall *longjmp_call,
			   program_state *new_state,
			   region_model_context *ctxt)
{
  tree buf_ptr = gimple_call_arg (longjmp_call, 0);
  gcc_assert (POINTER_TYPE_P (TREE_TYPE (buf_ptr)));

  region_model *new_region_model = new_state->m_region_model;
  const svalue *buf_ptr_sval = new_region_model->get_rvalue (buf_ptr, ctxt);
  const region *buf = new_region_model->deref_rvalue (buf_ptr_sval, buf_ptr,
						       ctxt);

  const svalue *buf_content_sval
    = new_region_model->get_store_value (buf, ctxt);
  const setjmp_svalue *setjmp_sval
    = buf_content_sval->dyn_cast_setjmp_svalue ();
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
      ctxt->warn (make_unique<stale_jmp_buf> (setjmp_call,
					      longjmp_call,
					      setjmp_point));
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

  /* Detect leaks in the new state relative to the old state.  */
  program_state::detect_leaks (get_state (), *new_state, NULL,
				eg.get_ext_state (), ctxt);

  program_point next_point
    = program_point::after_supernode (setjmp_point.get_supernode (),
				      setjmp_point.get_call_string ());

  exploded_node *next
    = eg.get_or_create_node (next_point, *new_state, this);

  /* Create custom exploded_edge for a longjmp.  */
  if (next)
    {
      exploded_edge *eedge
	= eg.add_edge (const_cast<exploded_node *> (this), next, NULL, true,
		       make_unique<rewind_info_t> (tmp_setjmp_record,
						   longjmp_call));

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
exploded_node::detect_leaks (exploded_graph &eg)
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

  uncertainty_t uncertainty;
  impl_region_model_context ctxt (eg, this,
				  &old_state, &new_state, &uncertainty, NULL,
				  get_stmt ());
  const svalue *result = NULL;
  new_state.m_region_model->pop_frame (NULL, &result, &ctxt, nullptr);
  program_state::detect_leaks (old_state, new_state, result,
			       eg.get_ext_state (), &ctxt);
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

/* class dynamic_call_info_t : public custom_edge_info.  */

/* Implementation of custom_edge_info::update_model vfunc
   for dynamic_call_info_t.

   Update state for a dynamically discovered call (or return), by pushing
   or popping the a frame for the appropriate function.  */

bool
dynamic_call_info_t::update_model (region_model *model,
				   const exploded_edge *eedge,
				   region_model_context *ctxt) const
{
  gcc_assert (eedge);
  if (m_is_returning_call)
    model->update_for_return_gcall (m_dynamic_call, ctxt);
  else
    {
      function *callee = eedge->m_dest->get_function ();
      model->update_for_gcall (m_dynamic_call, ctxt, callee);
    }
  return true;
}

/* Implementation of custom_edge_info::add_events_to_path vfunc
   for dynamic_call_info_t.  */

void
dynamic_call_info_t::add_events_to_path (checker_path *emission_path,
				   const exploded_edge &eedge) const
{
  const exploded_node *src_node = eedge.m_src;
  const program_point &src_point = src_node->get_point ();
  const int src_stack_depth = src_point.get_stack_depth ();
  const exploded_node *dest_node = eedge.m_dest;
  const program_point &dest_point = dest_node->get_point ();
  const int dest_stack_depth = dest_point.get_stack_depth ();

  if (m_is_returning_call)
    emission_path->add_event
      (make_unique<return_event> (eedge,
				  event_loc_info (m_dynamic_call
						  ? m_dynamic_call->location
						  : UNKNOWN_LOCATION,
						  dest_point.get_fndecl (),
						  dest_stack_depth)));
  else
    emission_path->add_event
      (make_unique<call_event> (eedge,
				event_loc_info (m_dynamic_call
						? m_dynamic_call->location
						: UNKNOWN_LOCATION,
						src_point.get_fndecl (),
						src_stack_depth)));
}

/* class rewind_info_t : public custom_edge_info.  */

/* Implementation of custom_edge_info::update_model vfunc
   for rewind_info_t.

   Update state for the special-case of a rewind of a longjmp
   to a setjmp (which doesn't have a superedge, but does affect
   state).  */

bool
rewind_info_t::update_model (region_model *model,
			     const exploded_edge *eedge,
			     region_model_context *) const
{
  gcc_assert (eedge);
  const program_point &longjmp_point = eedge->m_src->get_point ();
  const program_point &setjmp_point = eedge->m_dest->get_point ();

  gcc_assert (longjmp_point.get_stack_depth ()
	      >= setjmp_point.get_stack_depth ());

  model->on_longjmp (get_longjmp_call (),
		     get_setjmp_call (),
		     setjmp_point.get_stack_depth (), NULL);
  return true;
}

/* Implementation of custom_edge_info::add_events_to_path vfunc
   for rewind_info_t.  */

void
rewind_info_t::add_events_to_path (checker_path *emission_path,
				   const exploded_edge &eedge) const
{
  const exploded_node *src_node = eedge.m_src;
  const program_point &src_point = src_node->get_point ();
  const int src_stack_depth = src_point.get_stack_depth ();
  const exploded_node *dst_node = eedge.m_dest;
  const program_point &dst_point = dst_node->get_point ();
  const int dst_stack_depth = dst_point.get_stack_depth ();

  emission_path->add_event
    (make_unique<rewind_from_longjmp_event>
     (&eedge,
      event_loc_info (get_longjmp_call ()->location,
		      src_point.get_fndecl (),
		      src_stack_depth),
      this));
  emission_path->add_event
    (make_unique<rewind_to_setjmp_event>
     (&eedge,
      event_loc_info (get_setjmp_call ()->location,
		      dst_point.get_fndecl (),
		      dst_stack_depth),
      this));
}

/* class exploded_edge : public dedge<eg_traits>.  */

/* exploded_edge's ctor.  */

exploded_edge::exploded_edge (exploded_node *src, exploded_node *dest,
			      const superedge *sedge, bool could_do_work,
			      std::unique_ptr<custom_edge_info> custom_info)
: dedge<eg_traits> (src, dest), m_sedge (sedge),
  m_custom_info (std::move (custom_info)),
  m_could_do_work_p (could_do_work)
{
}

/* Implementation of dedge::dump_dot vfunc for exploded_edge.
   Use the label of the underlying superedge, if any.  */

void
exploded_edge::dump_dot (graphviz_out *gv, const dump_args_t &) const
{
  pretty_printer *pp = gv->get_pp ();

  m_src->dump_dot_id (pp);
  pp_string (pp, " -> ");
  m_dest->dump_dot_id (pp);
  dump_dot_label (pp);
}

/* Second half of exploded_edge::dump_dot.  This is split out
   for use by trimmed_graph::dump_dot and base_feasible_edge::dump_dot.  */

void
exploded_edge::dump_dot_label (pretty_printer *pp) const
{
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

  pp_printf (pp,
	     (" [style=%s, color=%s, weight=%d, constraint=%s,"
	      " headlabel=\""),
	     style, color, weight, constraint);

  if (m_sedge)
    m_sedge->dump_label_to_pp (pp, false);
  else if (m_custom_info)
    m_custom_info->print (pp);

  pp_printf (pp, "%s",
	     could_do_work_p () ? "(could do work)" : "DOES NO WORK");

  //pp_write_text_as_dot_label_to_stream (pp, /*for_record=*/false);

  pp_printf (pp, "\"];\n");
}

/* Return a new json::object of the form
   {"src_idx": int, the index of the source exploded edge,
    "dst_idx": int, the index of the destination exploded edge,
    "sedge": (optional) object for the superedge, if any,
    "custom": (optional) str, a description, if this is a custom edge}.  */

std::unique_ptr<json::object>
exploded_edge::to_json () const
{
  auto eedge_obj = ::make_unique<json::object> ();
  eedge_obj->set_integer ("src_idx", m_src->m_index);
  eedge_obj->set_integer ("dst_idx", m_dest->m_index);
  if (m_sedge)
    eedge_obj->set ("sedge", m_sedge->to_json ());
  if (m_custom_info)
    {
      pretty_printer pp;
      pp_format_decoder (&pp) = default_tree_printer;
      m_custom_info->print (&pp);
      eedge_obj->set_string ("custom", pp_formatted_text (&pp));
    }
  return eedge_obj;
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

/* struct per_function_data.  */

per_function_data::~per_function_data ()
{
  for (auto iter : m_summaries)
    delete iter;
}

void
per_function_data::add_call_summary (exploded_node *node)
{
  m_summaries.safe_push (new call_summary (this, node));
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

/* Return a new json::array of per-snode SCC ids.  */

std::unique_ptr<json::array>
strongly_connected_components::to_json () const
{
  auto scc_arr = ::make_unique<json::array> ();
  for (int i = 0; i < m_sg.num_nodes (); i++)
    scc_arr->append (new json::integer_number (get_scc_id (i)));
  return scc_arr;
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
      if (sedge->get_kind () != SUPEREDGE_CFG_EDGE
	  && sedge->get_kind () != SUPEREDGE_INTRAPROCEDURAL_CALL)
	continue;
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
   Return 0 if they are equal.

   The ordering of the worklist is critical for performance and for
   avoiding node explosions.  Ideally we want all enodes at a CFG join-point
   with the same callstring to be sorted next to each other in the worklist
   so that a run of consecutive enodes can be merged and processed "in bulk"
   rather than individually or pairwise, minimizing the number of new enodes
   created.  */

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
      if (int cmp = ka.m_worklist.m_plan.cmp_function (point_a.get_function (),
						       point_b.get_function ()))
	return cmp;
    }

  /* Sort by callstring, so that nodes with deeper call strings are processed
     before those with shallower call strings.
     If we have
         splitting BB
             /  \
            /    \
       fn call   no fn call
            \    /
             \  /
            join BB
     then we want the path inside the function call to be fully explored up
     to the return to the join BB before we explore on the "no fn call" path,
     so that both enodes at the join BB reach the front of the worklist at
     the same time and thus have a chance of being merged.  */
  int cs_cmp = call_string::cmp (call_string_a, call_string_b);
  if (cs_cmp)
    return cs_cmp;

  /* Order by SCC.  */
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

  /* Otherwise, we ought to have the same program_point.  */
  gcc_assert (point_a == point_b);

  const program_state &state_a = ka.m_enode->get_state ();
  const program_state &state_b = kb.m_enode->get_state ();

  /* Sort by sm-state, so that identical sm-states are grouped
     together in the worklist.  */
  for (unsigned sm_idx = 0; sm_idx < state_a.m_checker_states.length ();
       ++sm_idx)
    {
      sm_state_map *smap_a = state_a.m_checker_states[sm_idx];
      sm_state_map *smap_b = state_b.m_checker_states[sm_idx];

      if (int smap_cmp = sm_state_map::cmp (*smap_a, *smap_b))
	return smap_cmp;
    }

  /* Otherwise, we have two enodes at the same program point but with
     different states.  We don't have a good total ordering on states,
     so order them by enode index, so that we have at least have a
     stable sort.  */
  return ka.m_enode->m_index - kb.m_enode->m_index;
}

/* Return a new json::object of the form
   {"scc" : [per-snode-IDs]},  */

std::unique_ptr<json::object>
worklist::to_json () const
{
  auto worklist_obj = ::make_unique<json::object> ();

  worklist_obj->set ("scc", m_scc.to_json ());

  /* The following field isn't yet being JSONified:
     queue_t m_queue;  */

  return worklist_obj;
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
  m_diagnostic_manager (logger, ext_state.get_engine (), verbosity),
  m_global_stats (m_sg.num_nodes ()),
  m_functionless_stats (m_sg.num_nodes ()),
  m_PK_AFTER_SUPERNODE_per_snode (m_sg.num_nodes ())
{
  m_origin = get_or_create_node
    (program_point::origin (*ext_state.get_model_manager ()),
     program_state (ext_state), NULL);
  for (int i = 0; i < m_sg.num_nodes (); i++)
    m_PK_AFTER_SUPERNODE_per_snode.quick_push (i);
}

/* exploded_graph's dtor.  */

exploded_graph::~exploded_graph ()
{
  for (auto iter : m_per_point_data)
    delete iter.second;
  for (auto iter : m_per_function_data)
    delete iter.second;
  for (auto iter : m_per_function_stats)
    delete iter.second;
  for (auto iter : m_per_call_string_data)
    delete iter.second;
}

/* Subroutine for use when implementing __attribute__((tainted_args))
   on functions and on function pointer fields in structs.

   Called on STATE representing a call to FNDECL.
   Mark all params of FNDECL in STATE as "tainted".  Mark the value of all
   regions pointed to by params of FNDECL as "tainted".

   Return true if successful; return false if the "taint" state machine
   was not found.  */

static bool
mark_params_as_tainted (program_state *state, tree fndecl,
			const extrinsic_state &ext_state)
{
  unsigned taint_sm_idx;
  if (!ext_state.get_sm_idx_by_name ("taint", &taint_sm_idx))
    return false;
  sm_state_map *smap = state->m_checker_states[taint_sm_idx];

  const state_machine &sm = ext_state.get_sm (taint_sm_idx);
  state_machine::state_t tainted = sm.get_state_by_name ("tainted");

  region_model_manager *mgr = ext_state.get_model_manager ();

  function *fun = DECL_STRUCT_FUNCTION (fndecl);
  gcc_assert (fun);

  for (tree iter_parm = DECL_ARGUMENTS (fndecl); iter_parm;
       iter_parm = DECL_CHAIN (iter_parm))
    {
      tree param = iter_parm;
      if (tree parm_default_ssa = ssa_default_def (fun, iter_parm))
	param = parm_default_ssa;
      const region *param_reg = state->m_region_model->get_lvalue (param, NULL);
      const svalue *init_sval = mgr->get_or_create_initial_value (param_reg);
      smap->set_state (state->m_region_model, init_sval,
		       tainted, NULL /*origin_new_sval*/, ext_state);
      if (POINTER_TYPE_P (TREE_TYPE (param)))
	{
	  const region *pointee_reg = mgr->get_symbolic_region (init_sval);
	  /* Mark "*param" as tainted.  */
	  const svalue *init_pointee_sval
	    = mgr->get_or_create_initial_value (pointee_reg);
	  smap->set_state (state->m_region_model, init_pointee_sval,
			   tainted, NULL /*origin_new_sval*/, ext_state);
	}
    }

  return true;
}

/* Custom event for use by tainted_args_function_info when a function
   has been marked with __attribute__((tainted_args)).  */

class tainted_args_function_custom_event : public custom_event
{
public:
  tainted_args_function_custom_event (const event_loc_info &loc_info)
  : custom_event (loc_info),
    m_fndecl (loc_info.m_fndecl)
  {
  }

  void
  print_desc (pretty_printer &pp) const final override
  {
    pp_printf (&pp,
	       "function %qE marked with %<__attribute__((tainted_args))%>",
	       m_fndecl);
  }

private:
  tree m_fndecl;
};

/* Custom exploded_edge info for top-level calls to a function
   marked with __attribute__((tainted_args)).  */

class tainted_args_function_info : public custom_edge_info
{
public:
  tainted_args_function_info (tree fndecl)
  : m_fndecl (fndecl)
  {}

  void print (pretty_printer *pp) const final override
  {
    pp_string (pp, "call to tainted_args function");
  };

  bool update_model (region_model *,
		     const exploded_edge *,
		     region_model_context *) const final override
  {
    /* No-op.  */
    return true;
  }

  void add_events_to_path (checker_path *emission_path,
			   const exploded_edge &) const final override
  {
    emission_path->add_event
      (make_unique<tainted_args_function_custom_event>
       (event_loc_info (DECL_SOURCE_LOCATION (m_fndecl), m_fndecl, 0)));
  }

private:
  tree m_fndecl;
};

/* Ensure that there is an exploded_node representing an external call to
   FUN, adding it to the worklist if creating it.

   Add an edge from the origin exploded_node to the function entrypoint
   exploded_node.

   Return the exploded_node for the entrypoint to the function.  */

exploded_node *
exploded_graph::add_function_entry (const function &fun)
{
  gcc_assert (gimple_has_body_p (fun.decl));

  /* Be idempotent.  */
  function *key = const_cast<function *> (&fun);
  if (m_functions_with_enodes.contains (key))
    {
      logger * const logger = get_logger ();
       if (logger)
	logger->log ("entrypoint for %qE already exists", fun.decl);
      return NULL;
    }

  program_point point
    = program_point::from_function_entry (*m_ext_state.get_model_manager (),
					  m_sg, fun);
  program_state state (m_ext_state);
  state.push_frame (m_ext_state, fun);

  std::unique_ptr<custom_edge_info> edge_info = NULL;

  if (lookup_attribute ("tainted_args", DECL_ATTRIBUTES (fun.decl)))
    {
      if (mark_params_as_tainted (&state, fun.decl, m_ext_state))
	edge_info = make_unique<tainted_args_function_info> (fun.decl);
    }

  if (!state.m_valid)
    return NULL;

  exploded_node *enode = get_or_create_node (point, state, NULL);
  if (!enode)
    return NULL;

  add_edge (m_origin, enode, NULL, false, std::move (edge_info));

  m_functions_with_enodes.add (key);

  return enode;
}

/* Get or create an exploded_node for (POINT, STATE).
   If a new node is created, it is added to the worklist.

   Use ENODE_FOR_DIAG, a pre-existing enode, for any diagnostics
   that need to be emitted (e.g. when purging state *before* we have
   a new enode).  */

exploded_node *
exploded_graph::get_or_create_node (const program_point &point,
				    const program_state &state,
				    exploded_node *enode_for_diag)
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
      state.dump_to_pp (m_ext_state, true, false, pp);
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
  uncertainty_t uncertainty;
  program_state pruned_state
    = state.prune_for_point (*this, point, enode_for_diag, &uncertainty);

  pruned_state.validate (get_ext_state ());

  //pruned_state.dump (get_ext_state ());

  if (logger)
    {
      pretty_printer *pp = logger->get_printer ();
      logger->start_log_line ();
      pp_string (pp, "pruned_state: ");
      pruned_state.dump_to_pp (m_ext_state, true, false, pp);
      logger->end_log_line ();
      pruned_state.m_region_model->dump_to_pp (logger->get_printer (), true,
						false);
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
	  if (pruned_state.can_merge_with_p (existing_state, m_ext_state, point,
					     &merged_state))
	    {
	      merged_state.validate (m_ext_state);
	      if (logger)
		logger->log ("merging new state with that of EN: %i",
			     existing_enode->m_index);

	      /* Try again for a cache hit.
		 Whether we get one or not, merged_state's value_ids have no
		 relationship to those of the input state, and thus to those
		 of CHANGE, so we must purge any svalue_ids from *CHANGE.  */
	      ps.set_state (merged_state);

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
      >= param_analyzer_max_enodes_per_program_point)
    {
      pretty_printer pp;
      point.print (&pp, format (false));
      print_enode_indices (&pp, per_point_data->m_enodes);
      if (logger)
	logger->log ("not creating enode; too many at program point: %s",
		     pp_formatted_text (&pp));
      warning_at (point.get_location (), OPT_Wanalyzer_too_complex,
		  "terminating analysis for this program point: %s",
		  pp_formatted_text (&pp));
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
      pruned_state.dump_to_pp (m_ext_state, true, false, pp);
      logger->end_log_line ();
    }

  /* Add the new node to the worlist.  */
  m_worklist.add_node (node);
  return node;
}

/* Add an exploded_edge from SRC to DEST, recording its association
   with SEDGE (which may be NULL), and, if non-NULL, taking ownership
   of CUSTOM_INFO.  COULD_DO_WORK is used for detecting infinite loops.
   Return the newly-created eedge.  */

exploded_edge *
exploded_graph::add_edge (exploded_node *src, exploded_node *dest,
			  const superedge *sedge, bool could_do_work,
			  std::unique_ptr<custom_edge_info> custom_info)
{
  if (get_logger ())
    get_logger ()->log ("creating edge EN: %i -> EN: %i",
			src->m_index, dest->m_index);
  exploded_edge *e
    = new exploded_edge (src, dest, sedge, could_do_work,
			 std::move (custom_info));
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

/* Get this graph's per-program-point-data for POINT if there is any,
   otherwise NULL.  */

per_program_point_data *
exploded_graph::get_per_program_point_data (const program_point &point) const
{
  if (per_program_point_data **slot
      = const_cast <point_map_t &> (m_per_point_data).get (&point))
    return *slot;

  return NULL;
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

/* Return true if FUN should be traversed directly, rather than only as
   called via other functions.  */

static bool
toplevel_function_p (const function &fun, logger *logger)
{
  /* Don't directly traverse into functions that have an "__analyzer_"
     prefix.  Doing so is useful for the analyzer test suite, allowing
     us to have functions that are called in traversals, but not directly
     explored, thus testing how the analyzer handles calls and returns.
     With this, we can have DejaGnu directives that cover just the case
     of where a function is called by another function, without generating
     excess messages from the case of the first function being traversed
     directly.  */
#define ANALYZER_PREFIX "__analyzer_"
  if (!strncmp (IDENTIFIER_POINTER (DECL_NAME (fun.decl)), ANALYZER_PREFIX,
		strlen (ANALYZER_PREFIX)))
    {
      if (logger)
	logger->log ("not traversing %qE (starts with %qs)",
		     fun.decl, ANALYZER_PREFIX);
      return false;
    }

  if (logger)
    logger->log ("traversing %qE (all checks passed)", fun.decl);

  return true;
}

/* Custom event for use by tainted_call_info when a callback field has been
   marked with __attribute__((tainted_args)), for labelling the field.  */

class tainted_args_field_custom_event : public custom_event
{
public:
  tainted_args_field_custom_event (tree field)
  : custom_event (event_loc_info (DECL_SOURCE_LOCATION (field), NULL_TREE, 0)),
    m_field (field)
  {
  }

  void print_desc (pretty_printer &pp) const final override
  {
    pp_printf (&pp,
	       "field %qE of %qT"
	       " is marked with %<__attribute__((tainted_args))%>",
	       m_field, DECL_CONTEXT (m_field));
  }

private:
  tree m_field;
};

/* Custom event for use by tainted_call_info when a callback field has been
   marked with __attribute__((tainted_args)), for labelling the function used
   in that callback.  */

class tainted_args_callback_custom_event : public custom_event
{
public:
  tainted_args_callback_custom_event (const event_loc_info &loc_info,
				      tree field)
  : custom_event (loc_info),
    m_field (field)
  {
  }

  void print_desc (pretty_printer &pp) const final override
  {
    pp_printf (&pp,
	       "function %qE used as initializer for field %qE"
	       " marked with %<__attribute__((tainted_args))%>",
	       get_fndecl (), m_field);
  }

private:
  tree m_field;
};

/* Custom edge info for use when adding a function used by a callback field
   marked with '__attribute__((tainted_args))'.   */

class tainted_args_call_info : public custom_edge_info
{
public:
  tainted_args_call_info (tree field, tree fndecl, location_t loc)
  : m_field (field), m_fndecl (fndecl), m_loc (loc)
  {}

  void print (pretty_printer *pp) const final override
  {
    pp_string (pp, "call to tainted field");
  };

  bool update_model (region_model *,
		     const exploded_edge *,
		     region_model_context *) const final override
  {
    /* No-op.  */
    return true;
  }

  void add_events_to_path (checker_path *emission_path,
			   const exploded_edge &) const final override
  {
    /* Show the field in the struct declaration, e.g.
       "(1) field 'store' is marked with '__attribute__((tainted_args))'"  */
    emission_path->add_event
      (make_unique<tainted_args_field_custom_event> (m_field));

    /* Show the callback in the initializer
       e.g.
       "(2) function 'gadget_dev_desc_UDC_store' used as initializer
       for field 'store' marked with '__attribute__((tainted_args))'".  */
    emission_path->add_event
      (make_unique<tainted_args_callback_custom_event>
       (event_loc_info (m_loc, m_fndecl, 0),
	m_field));
  }

private:
  tree m_field;
  tree m_fndecl;
  location_t m_loc;
};

/* Given an initializer at LOC for FIELD marked with
   '__attribute__((tainted_args))' initialized with FNDECL, add an
   entrypoint to FNDECL to EG (and to its worklist) where the params to
   FNDECL are marked as tainted.  */

static void
add_tainted_args_callback (exploded_graph *eg, tree field, tree fndecl,
			   location_t loc)
{
  logger *logger = eg->get_logger ();

  LOG_SCOPE (logger);

  if (!gimple_has_body_p (fndecl))
    return;

  const extrinsic_state &ext_state = eg->get_ext_state ();

  function *fun = DECL_STRUCT_FUNCTION (fndecl);
  gcc_assert (fun);

  program_point point
    = program_point::from_function_entry (*ext_state.get_model_manager (),
					  eg->get_supergraph (), *fun);
  program_state state (ext_state);
  state.push_frame (ext_state, *fun);

  if (!mark_params_as_tainted (&state, fndecl, ext_state))
    return;

  if (!state.m_valid)
    return;

  exploded_node *enode = eg->get_or_create_node (point, state, NULL);
  if (logger)
    {
      if (enode)
	logger->log ("created EN %i for tainted_args %qE entrypoint",
		     enode->m_index, fndecl);
      else
	{
	  logger->log ("did not create enode for tainted_args %qE entrypoint",
		       fndecl);
	  return;
	}
    }

  eg->add_edge (eg->get_origin (), enode, NULL, false,
		make_unique<tainted_args_call_info> (field, fndecl, loc));
}

/* Callback for walk_tree for finding callbacks within initializers;
   ensure that any callback initializer where the corresponding field is
   marked with '__attribute__((tainted_args))' is treated as an entrypoint
   to the analysis, special-casing that the inputs to the callback are
   untrustworthy.  */

static tree
add_any_callbacks (tree *tp, int *, void *data)
{
  exploded_graph *eg = (exploded_graph *)data;
  if (TREE_CODE (*tp) == CONSTRUCTOR)
    {
      /* Find fields with the "tainted_args" attribute.
	 walk_tree only walks the values, not the index values;
	 look at the index values.  */
      unsigned HOST_WIDE_INT idx;
      constructor_elt *ce;

      for (idx = 0; vec_safe_iterate (CONSTRUCTOR_ELTS (*tp), idx, &ce);
	   idx++)
	if (ce->index && TREE_CODE (ce->index) == FIELD_DECL)
	  if (lookup_attribute ("tainted_args", DECL_ATTRIBUTES (ce->index)))
	    {
	      tree value = ce->value;
	      if (TREE_CODE (value) == ADDR_EXPR
		  && TREE_CODE (TREE_OPERAND (value, 0)) == FUNCTION_DECL)
		add_tainted_args_callback (eg, ce->index,
					   TREE_OPERAND (value, 0),
					   EXPR_LOCATION (value));
	    }
    }

  return NULL_TREE;
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
    gcc_assert (fun);
    if (!toplevel_function_p (*fun, logger))
      continue;
    exploded_node *enode = add_function_entry (*fun);
    if (logger)
      {
	if (enode)
	  logger->log ("created EN %i for %qE entrypoint",
		       enode->m_index, fun->decl);
	else
	  logger->log ("did not create enode for %qE entrypoint", fun->decl);
      }
  }

  /* Find callbacks that are reachable from global initializers.  */
  varpool_node *vpnode;
  FOR_EACH_VARIABLE (vpnode)
    {
      tree decl = vpnode->decl;
      tree init = DECL_INITIAL (decl);
      if (!init)
	continue;
      walk_tree (&init, add_any_callbacks, this, NULL);
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

      /* If we have a run of nodes that are before-supernode, try merging and
	 processing them together, rather than pairwise or individually.  */
      if (flag_analyzer_state_merge && node != m_origin)
	if (maybe_process_run_of_before_supernode_enodes (node))
	  goto handle_limit;

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
		const program_point &point = node->get_point ();
		if (logger)
		  {
		    format f (false);
		    pretty_printer *pp = logger->get_printer ();
		    logger->start_log_line ();
		    logger->log_partial
		      ("got potential merge EN: %i and EN: %i at ",
		       node->m_index, node_2->m_index);
		    point.print (pp, f);
		    logger->end_log_line ();
		  }
		const program_state &state = node->get_state ();
		const program_state &state_2 = node_2->get_state ();

		/* They shouldn't be equal, or we wouldn't have two
		   separate nodes.  */
		gcc_assert (state != state_2);

		program_state merged_state (m_ext_state);
		if (state.can_merge_with_p (state_2, m_ext_state,
					    point, &merged_state))
		  {
		    if (logger)
		      logger->log ("merging EN: %i and EN: %i",
				   node->m_index, node_2->m_index);

		    if (merged_state == state)
		      {
			/* Then merge node_2 into node by adding an edge.  */
			add_edge (node_2, node, NULL, false);

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
			add_edge (node, node_2, NULL, false);
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
						merged_state, node);
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
			    add_edge (node, merged_enode, NULL, false);
			    node->set_status (exploded_node::STATUS_MERGER);
			  }

			if (merged_enode == node_2)
			  m_worklist.add_node (merged_enode);
			else
			  {
			    add_edge (node_2, merged_enode, NULL, false);
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

    handle_limit:
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

/* Attempt to process a consecutive run of sufficiently-similar nodes in
   the worklist at a CFG join-point (having already popped ENODE from the
   head of the worklist).

   If ENODE's point is of the form (before-supernode, SNODE) and the next
   nodes in the worklist are a consecutive run of enodes of the same form,
   for the same supernode as ENODE (but potentially from different in-edges),
   process them all together, setting their status to STATUS_BULK_MERGED,
   and return true.
   Otherwise, return false, in which case ENODE must be processed in the
   normal way.

   When processing them all together, generate successor states based
   on phi nodes for the appropriate CFG edges, and then attempt to merge
   these states into a minimal set of merged successor states, partitioning
   the inputs by merged successor state.

   Create new exploded nodes for all of the merged states, and add edges
   connecting the input enodes to the corresponding merger exploded nodes.

   We hope we have a much smaller number of merged successor states
   compared to the number of input enodes - ideally just one,
   if all successor states can be merged.

   Processing and merging many together as one operation rather than as
   pairs avoids scaling issues where per-pair mergers could bloat the
   graph with merger nodes (especially so after switch statements).  */

bool
exploded_graph::
maybe_process_run_of_before_supernode_enodes (exploded_node *enode)
{
  /* A struct for tracking per-input state.  */
  struct item
  {
    item (exploded_node *input_enode)
    : m_input_enode (input_enode),
      m_processed_state (input_enode->get_state ()),
      m_merger_idx (-1)
    {}

    exploded_node *m_input_enode;
    program_state m_processed_state;
    int m_merger_idx;
  };

  gcc_assert (enode->get_status () == exploded_node::STATUS_WORKLIST);
  gcc_assert (enode->m_succs.length () == 0);

  const program_point &point = enode->get_point ();

  if (point.get_kind () != PK_BEFORE_SUPERNODE)
    return false;

  const supernode *snode = point.get_supernode ();

  logger * const logger = get_logger ();
  LOG_SCOPE (logger);

  /* Find a run of enodes in the worklist that are before the same supernode,
     but potentially from different in-edges.  */
  auto_vec <exploded_node *> enodes;
  enodes.safe_push (enode);
  while (exploded_node *enode_2 = m_worklist.peek_next ())
    {
      gcc_assert (enode_2->get_status ()
		  == exploded_node::STATUS_WORKLIST);
      gcc_assert (enode_2->m_succs.length () == 0);

      const program_point &point_2 = enode_2->get_point ();

      if (point_2.get_kind () == PK_BEFORE_SUPERNODE
	  && point_2.get_supernode () == snode
	  && &point_2.get_call_string () == &point.get_call_string ())
	{
	  enodes.safe_push (enode_2);
	  m_worklist.take_next ();
	}
      else
	break;
    }

  /* If the only node is ENODE, then give up.  */
  if (enodes.length () == 1)
    return false;

  if (logger)
    logger->log ("got run of %i enodes for SN: %i",
		 enodes.length (), snode->m_index);

  /* All of these enodes have a shared successor point (even if they
     were for different in-edges).  */
  program_point next_point (point.get_next ());

  /* Calculate the successor state for each enode in enodes.  */
  auto_delete_vec<item> items (enodes.length ());
  unsigned i;
  exploded_node *iter_enode;
  FOR_EACH_VEC_ELT (enodes, i, iter_enode)
    {
      item *it = new item (iter_enode);
      items.quick_push (it);
      const program_state &state = iter_enode->get_state ();
      program_state *next_state = &it->m_processed_state;
      next_state->validate (m_ext_state);
      const program_point &iter_point = iter_enode->get_point ();
      if (const superedge *iter_sedge = iter_point.get_from_edge ())
	{
	  uncertainty_t uncertainty;
	  impl_region_model_context ctxt (*this, iter_enode,
					  &state, next_state,
					  &uncertainty, NULL, NULL);
	  const cfg_superedge *last_cfg_superedge
	    = iter_sedge->dyn_cast_cfg_superedge ();
	  if (last_cfg_superedge)
	    next_state->m_region_model->update_for_phis
	      (snode, last_cfg_superedge, &ctxt);
	}
      next_state->validate (m_ext_state);
    }

  /* Attempt to partition the items into a set of merged states.
     We hope we have a much smaller number of merged states
     compared to the number of input enodes - ideally just one,
     if all can be merged.  */
  auto_delete_vec <program_state> merged_states;
  auto_vec<item *> first_item_for_each_merged_state;
  item *it;
  FOR_EACH_VEC_ELT (items, i, it)
    {
      const program_state &it_state = it->m_processed_state;
      program_state *merged_state;
      unsigned iter_merger_idx;
      FOR_EACH_VEC_ELT (merged_states, iter_merger_idx, merged_state)
	{
	  merged_state->validate (m_ext_state);
	  program_state merge (m_ext_state);
	  if (it_state.can_merge_with_p (*merged_state, m_ext_state,
					 next_point, &merge))
	    {
	      *merged_state = merge;
	      merged_state->validate (m_ext_state);
	      it->m_merger_idx = iter_merger_idx;
	      if (logger)
		logger->log ("reusing merger state %i for item %i (EN: %i)",
			     it->m_merger_idx, i, it->m_input_enode->m_index);
	      goto got_merger;
	    }
	}
      /* If it couldn't be merged with any existing merged_states,
	 create a new one.  */
      if (it->m_merger_idx == -1)
	{
	  it->m_merger_idx = merged_states.length ();
	  merged_states.safe_push (new program_state (it_state));
	  first_item_for_each_merged_state.safe_push (it);
	  if (logger)
	    logger->log ("using new merger state %i for item %i (EN: %i)",
			 it->m_merger_idx, i, it->m_input_enode->m_index);
	}
    got_merger:
      gcc_assert (it->m_merger_idx >= 0);
      gcc_assert ((unsigned)it->m_merger_idx < merged_states.length ());
    }

  /* Create merger nodes.  */
  auto_vec<exploded_node *> next_enodes (merged_states.length ());
  program_state *merged_state;
  FOR_EACH_VEC_ELT (merged_states, i, merged_state)
    {
      exploded_node *src_enode
	= first_item_for_each_merged_state[i]->m_input_enode;
      exploded_node *next
	= get_or_create_node (next_point, *merged_state, src_enode);
      /* "next" could be NULL; we handle that when adding the edges below.  */
      next_enodes.quick_push (next);
      if (logger)
	{
	  if (next)
	    logger->log ("using EN: %i for merger state %i", next->m_index, i);
	  else
	    logger->log ("using NULL enode for merger state %i", i);
	}
    }

  /* Create edges from each input enode to the appropriate successor enode.
     Update the status of the now-processed input enodes.  */
  FOR_EACH_VEC_ELT (items, i, it)
    {
      exploded_node *next = next_enodes[it->m_merger_idx];
      if (next)
	add_edge (it->m_input_enode, next, NULL,
		  false); /* no "work" is done during merger.  */
      it->m_input_enode->set_status (exploded_node::STATUS_BULK_MERGED);
    }

  if (logger)
    logger->log ("merged %i in-enodes into %i out-enode(s) at SN: %i",
		 items.length (), merged_states.length (), snode->m_index);

  return true;
}

/* Return true if STMT must appear at the start of its exploded node, and
   thus we can't consolidate its effects within a run of other statements,
   where PREV_STMT was the previous statement.  */

static bool
stmt_requires_new_enode_p (const gimple *stmt,
			   const gimple *prev_stmt)
{
  if (const gcall *call = dyn_cast <const gcall *> (stmt))
    {
      /* Stop consolidating at calls to
	 "__analyzer_dump_exploded_nodes", so they always appear at the
	 start of an exploded_node.  */
      if (is_special_named_call_p (call, "__analyzer_dump_exploded_nodes",
				   1))
	return true;

      /* sm-signal.cc injects an additional custom eedge at "signal" calls
	 from the registration enode to the handler enode, separate from the
	 regular next state, which defeats the "detect state change" logic
	 in process_node.  Work around this via special-casing, to ensure
	 we split the enode immediately before any "signal" call.  */
      if (is_special_named_call_p (call, "signal", 2, true))
	return true;
    }

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

/* Return true if OLD_STATE and NEW_STATE are sufficiently different that
   we should split enodes and create an exploded_edge separating them
   (which makes it easier to identify state changes of intereset when
   constructing checker_paths).  */

static bool
state_change_requires_new_enode_p (const program_state &old_state,
				   const program_state &new_state)
{
  /* Changes in dynamic extents signify creations of heap/alloca regions
     and resizings of heap regions; likely to be of interest in
     diagnostic paths.  */
  if (old_state.m_region_model->get_dynamic_extents ()
      != new_state.m_region_model->get_dynamic_extents ())
    return true;

  /* Changes in sm-state are of interest.  */
  int sm_idx;
  sm_state_map *smap;
  FOR_EACH_VEC_ELT (old_state.m_checker_states, sm_idx, smap)
    {
      const sm_state_map *old_smap = old_state.m_checker_states[sm_idx];
      const sm_state_map *new_smap = new_state.m_checker_states[sm_idx];
      if (*old_smap != *new_smap)
	return true;
    }

  return false;
}

/* Create enodes and eedges for the function calls that doesn't have an
   underlying call superedge.

   Such case occurs when GCC's middle end didn't know which function to
   call but the analyzer does (with the help of current state).

   Some example such calls are dynamically dispatched calls to virtual
   functions or calls that happen via function pointer.  */

bool
exploded_graph::maybe_create_dynamic_call (const gcall *call,
					   tree fn_decl,
					   exploded_node *node,
					   program_state next_state,
					   program_point &next_point,
					   uncertainty_t *uncertainty,
					   logger *logger)
{
  LOG_FUNC (logger);

  const program_point *this_point = &node->get_point ();
  function *fun = DECL_STRUCT_FUNCTION (fn_decl);
  if (fun)
    {
      const supergraph &sg = this->get_supergraph ();
      supernode *sn_entry = sg.get_node_for_function_entry (*fun);
      supernode *sn_exit = sg.get_node_for_function_exit (*fun);

      program_point new_point
	= program_point::before_supernode (sn_entry,
					   NULL,
					   this_point->get_call_string ());

      new_point.push_to_call_stack (sn_exit,
				    next_point.get_supernode());

      /* Impose a maximum recursion depth and don't analyze paths
	 that exceed it further.
	 This is something of a blunt workaround, but it only
	 applies to recursion (and mutual recursion), not to
	 general call stacks.  */
      if (new_point.get_call_string ().calc_recursion_depth ()
	  > param_analyzer_max_recursion_depth)
      {
	if (logger)
	  logger->log ("rejecting call edge: recursion limit exceeded");
	return false;
      }

      next_state.push_call (*this, node, call, uncertainty);

      if (next_state.m_valid)
	{
	  if (logger)
	    logger->log ("Discovered call to %s [SN: %i -> SN: %i]",
			 function_name(fun),
			 this_point->get_supernode ()->m_index,
			 sn_entry->m_index);

	  exploded_node *enode = get_or_create_node (new_point,
						     next_state,
						     node);
	  if (enode)
	    add_edge (node,enode, NULL,
		      false, /* No work is done by the call itself.  */
		      make_unique<dynamic_call_info_t> (call));
	  return true;
	}
    }
  return false;
}

/* Subclass of path_context for use within exploded_graph::process_node,
   so that we can split states e.g. at "realloc" calls.  */

class impl_path_context : public path_context
{
public:
  impl_path_context (const program_state *cur_state,
		     logger *logger)
  : m_cur_state (cur_state),
    m_logger (logger),
    m_terminate_path (false)
  {
  }

  bool bifurcation_p () const
  {
    return m_custom_eedge_infos.length () > 0;
  }

  const program_state &get_state_at_bifurcation () const
  {
    gcc_assert (m_state_at_bifurcation);
    return *m_state_at_bifurcation;
  }

  void
  bifurcate (std::unique_ptr<custom_edge_info> info) final override
  {
    if (m_logger)
      m_logger->log ("bifurcating path");

    if (m_state_at_bifurcation)
      /* Verify that the state at bifurcation is consistent when we
	 split into multiple out-edges.  */
      gcc_assert (*m_state_at_bifurcation == *m_cur_state);
    else
      /* Take a copy of the cur_state at the moment when bifurcation
	 happens.  */
      m_state_at_bifurcation
	= std::unique_ptr<program_state> (new program_state (*m_cur_state));

    /* Take ownership of INFO.  */
    m_custom_eedge_infos.safe_push (info.release ());
  }

  void terminate_path () final override
  {
    if (m_logger)
      m_logger->log ("terminating path");
    m_terminate_path = true;
  }

  bool terminate_path_p () const final override
  {
    return m_terminate_path;
  }

  const vec<custom_edge_info *> & get_custom_eedge_infos ()
  {
    return m_custom_eedge_infos;
  }

private:
  const program_state *m_cur_state;

  logger *m_logger;

  /* Lazily-created copy of the state before the split.  */
  std::unique_ptr<program_state> m_state_at_bifurcation;

  auto_vec <custom_edge_info *> m_custom_eedge_infos;

  bool m_terminate_path;
};

/* A subclass of pending_diagnostic for complaining about jumps through NULL
   function pointers.  */

class jump_through_null : public pending_diagnostic_subclass<jump_through_null>
{
public:
  jump_through_null (const gcall *call)
  : m_call (call)
  {}

  const char *get_kind () const final override
  {
    return "jump_through_null";
  }

  bool operator== (const jump_through_null &other) const
  {
    return m_call == other.m_call;
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_jump_through_null;
  }

  bool emit (diagnostic_emission_context &ctxt) final override
  {
    return ctxt.warn ("jump through null pointer");
  }

  bool describe_final_event (pretty_printer &pp,
			     const evdesc::final_event &) final override
  {
    pp_string (&pp, "jump through null pointer here");
    return true;
  }

private:
  const gcall *m_call;
};

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
      state.dump_to_pp (m_ext_state, true, false, pp);
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
	uncertainty_t uncertainty;

	if (point.get_from_edge ())
	  {
	    impl_region_model_context ctxt (*this, node,
					    &state, &next_state,
					    &uncertainty, NULL, NULL);
	    const cfg_superedge *last_cfg_superedge
	      = point.get_from_edge ()->dyn_cast_cfg_superedge ();
	    if (last_cfg_superedge)
	      next_state.m_region_model->update_for_phis
		(node->get_supernode (),
		 last_cfg_superedge,
		 &ctxt);
	    program_state::detect_leaks (state, next_state, NULL,
					 get_ext_state (), &ctxt);
	  }

	program_point next_point (point.get_next ());
	exploded_node *next = get_or_create_node (next_point, next_state, node);
	if (next)
	  add_edge (node, next, NULL,
		    false); /* Assume no work is done at phi nodes.  */
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
	   or more stmts that are processed.

	   Split the node in-place if an sm-state-change occurs, so that
	   the sm-state-change occurs on an edge where the src enode has
	   exactly one stmt, the one that caused the change. */
	program_state next_state (state);

	impl_path_context path_ctxt (&next_state, logger);

	bool could_have_done_work = false;
	uncertainty_t uncertainty;
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

	    program_state old_state (next_state);

	    /* Process the stmt.  */
	    exploded_node::on_stmt_flags flags
	      = node->on_stmt (*this, snode, stmt, &next_state, &uncertainty,
			       &could_have_done_work, &path_ctxt);
	    node->m_num_processed_stmts++;

	    /* If flags.m_terminate_path, stop analyzing; any nodes/edges
	       will have been added by on_stmt (e.g. for handling longjmp).  */
	    if (flags.m_terminate_path)
	      return;

	    if (next_state.m_region_model)
	      {
		impl_region_model_context ctxt (*this, node,
						&old_state, &next_state,
						&uncertainty, NULL, stmt);
		program_state::detect_leaks (old_state, next_state, NULL,
					     get_ext_state (), &ctxt);
	      }

	    unsigned next_idx = stmt_idx + 1;
	    program_point next_point
	      = (next_idx < point.get_supernode ()->m_stmts.length ()
		 ? program_point::before_stmt (point.get_supernode (), next_idx,
					       point.get_call_string ())
		 : program_point::after_supernode (point.get_supernode (),
						   point.get_call_string ()));
	    next_state = next_state.prune_for_point (*this, next_point, node,
						     &uncertainty);

	    if (flag_analyzer_fine_grained
		|| state_change_requires_new_enode_p (old_state, next_state)
		|| path_ctxt.bifurcation_p ()
		|| path_ctxt.terminate_path_p ())
	      {
		program_point split_point
		  = program_point::before_stmt (point.get_supernode (),
						stmt_idx,
						point.get_call_string ());
		if (split_point != node->get_point ())
		  {
		    /* If we're not at the start of NODE, split the enode at
		       this stmt, so we have:
			 node -> split_enode
		       so that when split_enode is processed the next edge
		       we add will be:
			 split_enode -> next
		       and any state change will effectively occur on that
		       latter edge, and split_enode will contain just stmt.  */
		    if (logger)
		      logger->log ("getting split_enode");
		    exploded_node *split_enode
		      = get_or_create_node (split_point, old_state, node);
		    if (!split_enode)
		      return;
		    /* "stmt" will be reprocessed when split_enode is
		       processed.  */
		    node->m_num_processed_stmts--;
		    if (logger)
		      logger->log ("creating edge to split_enode");
		    add_edge (node, split_enode, NULL, could_have_done_work);
		    return;
		  }
		else
		  /* If we're at the start of NODE, stop iterating,
		     so that an edge will be created from NODE to
		     (next_point, next_state) below. */
		  break;
	      }
	  }
	unsigned next_idx = stmt_idx + 1;
	program_point next_point
	  = (next_idx < point.get_supernode ()->m_stmts.length ()
	     ? program_point::before_stmt (point.get_supernode (), next_idx,
					   point.get_call_string ())
	     : program_point::after_supernode (point.get_supernode (),
					       point.get_call_string ()));
	if (path_ctxt.terminate_path_p ())
	  {
	    if (logger)
	      logger->log ("not adding node: terminating path");
	  }
	else
	  {
	    exploded_node *next
	      = get_or_create_node (next_point, next_state, node);
	    if (next)
	      add_edge (node, next, NULL, could_have_done_work);
	  }

	/* If we have custom edge infos, "bifurcate" the state
	   accordingly, potentially creating a new state/enode/eedge
	   instances.  For example, to handle a "realloc" call, we
	   might split into 3 states, for the "failure",
	   "resizing in place", and "moving to a new buffer" cases.  */
	for (auto edge_info_iter : path_ctxt.get_custom_eedge_infos ())
	  {
	    /* Take ownership of the edge infos from the path_ctxt.  */
	    std::unique_ptr<custom_edge_info> edge_info (edge_info_iter);
	    if (logger)
	      {
		logger->start_log_line ();
		logger->log_partial ("bifurcating for edge: ");
		edge_info->print (logger->get_printer ());
		logger->end_log_line ();
	      }
	    program_state bifurcated_new_state
	      (path_ctxt.get_state_at_bifurcation ());

	    /* Apply edge_info to state.  */
	    impl_region_model_context
	      bifurcation_ctxt (*this,
				node, // enode_for_diag
				&path_ctxt.get_state_at_bifurcation (),
				&bifurcated_new_state,
				NULL, // uncertainty_t *uncertainty
				NULL, // path_context *path_ctxt
				stmt);
	    if (edge_info->update_state (&bifurcated_new_state,
					 NULL, /* no exploded_edge yet.  */
					 &bifurcation_ctxt))
	      {
		exploded_node *next2
		  = get_or_create_node (next_point, bifurcated_new_state, node);
		if (next2)
		  add_edge (node, next2, NULL,
			    true /* assume that work could be done */,
			    std::move (edge_info));
	      }
	    else
	      {
		if (logger)
		  logger->log ("infeasible state, not adding node");
	      }
	  }
      }
      break;
    case PK_AFTER_SUPERNODE:
      {
	bool found_a_superedge = false;
	bool is_an_exit_block = false;
	/* If this is an EXIT BB, detect leaks, and potentially
	   create a function summary.  */
	if (point.get_supernode ()->return_p ())
	  {
	    is_an_exit_block = true;
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
		    state.dump_to_pp (m_ext_state, true, false, pp);
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
	    found_a_superedge = true;
	    if (logger)
	      {
		label_text succ_desc (succ->get_description (false));
		logger->log ("considering SN: %i -> SN: %i (%s)",
			     succ->m_src->m_index, succ->m_dest->m_index,
			     succ_desc.get ());
	      }

	    program_point next_point
	      = program_point::before_supernode (succ->m_dest, succ,
						 point.get_call_string ());
	    program_state next_state (state);
	    uncertainty_t uncertainty;

	    /* Make use the current state and try to discover and analyse
	       indirect function calls (a call that doesn't have an underlying
	       cgraph edge representing call).

	       Some examples of such calls are virtual function calls
	       and calls that happen via a function pointer.  */
	    if (succ->m_kind == SUPEREDGE_INTRAPROCEDURAL_CALL
		&& !(succ->get_any_callgraph_edge ()))
	      {
		const gcall *call
		  = point.get_supernode ()->get_final_call ();

		impl_region_model_context ctxt (*this,
						node,
						&state,
						&next_state,
						&uncertainty,
						NULL,
						point.get_stmt());

		region_model *model = state.m_region_model;
		bool call_discovered = false;

		if (tree fn_decl = model->get_fndecl_for_call (call, &ctxt))
		  call_discovered = maybe_create_dynamic_call (call,
							       fn_decl,
							       node,
							       next_state,
							       next_point,
							       &uncertainty,
							       logger);
		if (!call_discovered)
		  {
		    /* Check for jump through NULL.  */
		    if (tree fn_ptr = gimple_call_fn (call))
		      {
			const svalue *fn_ptr_sval
			  = model->get_rvalue (fn_ptr, &ctxt);
			if (fn_ptr_sval->all_zeroes_p ())
			  ctxt.warn (make_unique<jump_through_null> (call));
		      }

		    /* An unknown function or a special function was called
		       at this point, in such case, don't terminate the
		       analysis of the current function.

		       The analyzer handles calls to such functions while
		       analysing the stmt itself, so the function call
		       must have been handled by the anlyzer till now.  */
		    exploded_node *next
		      = get_or_create_node (next_point,
					    next_state,
					    node);
		    if (next)
		      add_edge (node, next, succ,
				true /* assume that work is done */);
		  }
	      }

	    if (!node->on_edge (*this, succ, &next_point, &next_state,
				&uncertainty))
	      {
		if (logger)
		  logger->log ("skipping impossible edge to SN: %i",
			       succ->m_dest->m_index);
		continue;
	      }
	    exploded_node *next = get_or_create_node (next_point, next_state,
						      node);
	    if (next)
	      {
		add_edge (node, next, succ, false);

		/* We might have a function entrypoint.  */
		detect_infinite_recursion (next);
	      }
	  }

	/* Return from the calls which doesn't have a return superedge.
	   Such case occurs when GCC's middle end didn't knew which function to
	   call but analyzer did.  */
	if ((is_an_exit_block && !found_a_superedge)
	    && (!point.get_call_string ().empty_p ()))
	  {
	    const call_string &cs = point.get_call_string ();
	    program_point next_point
	      = program_point::before_supernode (cs.get_caller_node (),
						 NULL,
						 cs);
	    program_state next_state (state);
	    uncertainty_t uncertainty;

	    const gcall *call
	      = next_point.get_supernode ()->get_returning_call ();

	    if (call)
	      next_state.returning_call (*this, node, call, &uncertainty);

	    if (next_state.m_valid)
	      {
		next_point.pop_from_call_stack ();
		exploded_node *enode = get_or_create_node (next_point,
							   next_state,
							   node);
		if (enode)
		  add_edge (node, enode, NULL, false,
			    make_unique<dynamic_call_info_t> (call, true));
	      }
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

  m_ext_state.get_engine ()->log_stats (logger);

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
	  enode->get_state ().dump_to_pp (m_ext_state, true, false, &pp);
	  fprintf (out, "state %i: EN: %i\n  %s\n",
		   state_idx++, enode->m_index,
		   pp_formatted_text (&pp));
	}
    }
  fprintf (out, "#exploded_node for PK_AFTER_SUPERNODE for SN: %i = %i\n",
	   snode->m_index, state_idx);
}

/* Return a new json::object of the form
   {"nodes" : [objs for enodes],
    "edges" : [objs for eedges],
    "ext_state": object for extrinsic_state,
    "diagnostic_manager": object for diagnostic_manager}.  */

std::unique_ptr<json::object>
exploded_graph::to_json () const
{
  auto egraph_obj = ::make_unique<json::object> ();

  /* Nodes.  */
  {
    auto nodes_arr = ::make_unique<json::array> ();
    unsigned i;
    exploded_node *n;
    FOR_EACH_VEC_ELT (m_nodes, i, n)
      nodes_arr->append (n->to_json (m_ext_state));
    egraph_obj->set ("nodes", std::move (nodes_arr));
  }

  /* Edges.  */
  {
    auto edges_arr = ::make_unique<json::array> ();
    unsigned i;
    exploded_edge *n;
    FOR_EACH_VEC_ELT (m_edges, i, n)
      edges_arr->append (n->to_json ());
    egraph_obj->set ("edges", std::move (edges_arr));
  }

  /* m_sg is JSONified at the top-level.  */

  egraph_obj->set ("ext_state", m_ext_state.to_json ());
  egraph_obj->set ("worklist", m_worklist.to_json ());
  egraph_obj->set ("diagnostic_manager", m_diagnostic_manager.to_json ());

  /* The following fields aren't yet being JSONified:
     const state_purge_map *const m_purge_map;
     const analysis_plan &m_plan;
     stats m_global_stats;
     function_stat_map_t m_per_function_stats;
     stats m_functionless_stats;
     call_string_data_map_t m_per_call_string_data;
     auto_vec<int> m_PK_AFTER_SUPERNODE_per_snode;  */

  return egraph_obj;
}

/* class exploded_path.  */

/* Copy ctor.  */

exploded_path::exploded_path (const exploded_path &other)
: m_edges (other.m_edges.length ())
{
  int i;
  const exploded_edge *eedge;
  FOR_EACH_VEC_ELT (other.m_edges, i, eedge)
    m_edges.quick_push (eedge);
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
exploded_path::feasible_p (logger *logger,
			   std::unique_ptr<feasibility_problem> *out,
			   engine *eng, const exploded_graph *eg) const
{
  LOG_SCOPE (logger);

  feasibility_state state (eng->get_model_manager (),
			   eg->get_supergraph ());

  /* Traverse the path, updating this state.  */
  for (unsigned edge_idx = 0; edge_idx < m_edges.length (); edge_idx++)
    {
      const exploded_edge *eedge = m_edges[edge_idx];
      if (logger)
	logger->log ("considering edge %i: EN:%i -> EN:%i",
		     edge_idx,
		     eedge->m_src->m_index,
		     eedge->m_dest->m_index);

      std::unique_ptr <rejected_constraint> rc;
      if (!state.maybe_update_for_edge (logger, eedge, nullptr, &rc))
	{
	  gcc_assert (rc);
	  if (out)
	    {
	      const exploded_node &src_enode = *eedge->m_src;
	      const program_point &src_point = src_enode.get_point ();
	      const gimple *last_stmt
		= src_point.get_supernode ()->get_last_stmt ();
	      *out = ::make_unique<feasibility_problem> (edge_idx, *eedge,
							 last_stmt,
							 std::move (rc));
	    }
	  return false;
	}

      if (logger)
	{
	  logger->log ("state after edge %i: EN:%i -> EN:%i",
		       edge_idx,
		       eedge->m_src->m_index,
		       eedge->m_dest->m_index);
	  logger->start_log_line ();
	  state.get_model ().dump_to_pp (logger->get_printer (), true, false);
	  logger->end_log_line ();
	}
    }

  return true;
}

/* Dump this path in multiline form to PP.
   If EXT_STATE is non-NULL, then show the nodes.  */

void
exploded_path::dump_to_pp (pretty_printer *pp,
			   const extrinsic_state *ext_state) const
{
  for (unsigned i = 0; i < m_edges.length (); i++)
    {
      const exploded_edge *eedge = m_edges[i];
      pp_printf (pp, "m_edges[%i]: EN %i -> EN %i",
		 i,
		 eedge->m_src->m_index,
		 eedge->m_dest->m_index);
      pp_newline (pp);

      if (ext_state)
	eedge->m_dest->dump_to_pp (pp, *ext_state);
    }
}

/* Dump this path in multiline form to FP.  */

void
exploded_path::dump (FILE *fp, const extrinsic_state *ext_state) const
{
  tree_dump_pretty_printer pp (fp);
  dump_to_pp (&pp, ext_state);
}

/* Dump this path in multiline form to stderr.  */

DEBUG_FUNCTION void
exploded_path::dump (const extrinsic_state *ext_state) const
{
  dump (stderr, ext_state);
}

/* Dump this path verbosely to FILENAME.  */

void
exploded_path::dump_to_file (const char *filename,
			     const extrinsic_state &ext_state) const
{
  FILE *fp = fopen (filename, "w");
  if (!fp)
    return;
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  pp.set_output_stream (fp);
  dump_to_pp (&pp, &ext_state);
  pp_flush (&pp);
  fclose (fp);
}

/* class feasibility_problem.  */

void
feasibility_problem::dump_to_pp (pretty_printer *pp) const
{
  pp_printf (pp, "edge from EN: %i to EN: %i",
	     m_eedge.m_src->m_index, m_eedge.m_dest->m_index);
  if (m_rc)
    {
      pp_string (pp, "; rejected constraint: ");
      m_rc->dump_to_pp (pp);
      pp_string (pp, "; rmodel: ");
      m_rc->get_model ().dump_to_pp (pp, true, false);
    }
}

/* class feasibility_state.  */

/* Ctor for feasibility_state, at the beginning of a path.  */

feasibility_state::feasibility_state (region_model_manager *manager,
				      const supergraph &sg)
: m_model (manager),
  m_snodes_visited (sg.m_nodes.length ())
{
  bitmap_clear (m_snodes_visited);
}

/* Copy ctor for feasibility_state, for extending a path.  */

feasibility_state::feasibility_state (const feasibility_state &other)
: m_model (other.m_model),
  m_snodes_visited (const_sbitmap (other.m_snodes_visited)->n_bits)
{
  bitmap_copy (m_snodes_visited, other.m_snodes_visited);
}

feasibility_state::feasibility_state (const region_model &model,
				      const supergraph &sg)
: m_model (model),
  m_snodes_visited (sg.m_nodes.length ())
{
  bitmap_clear (m_snodes_visited);
}

feasibility_state &
feasibility_state::operator= (const feasibility_state &other)
{
  m_model = other.m_model;
  bitmap_copy (m_snodes_visited, other.m_snodes_visited);
  return *this;
}

/* The heart of feasibility-checking.

   Attempt to update this state in-place based on traversing EEDGE
   in a path.
   Update the model for the stmts in the src enode.
   Attempt to add constraints for EEDGE.

   If feasible, return true.
   Otherwise, return false and write to *OUT_RC.  */

bool
feasibility_state::
maybe_update_for_edge (logger *logger,
		       const exploded_edge *eedge,
		       region_model_context *ctxt,
		       std::unique_ptr<rejected_constraint> *out_rc)
{
  const exploded_node &src_enode = *eedge->m_src;
  const program_point &src_point = src_enode.get_point ();
  if (logger)
    {
      logger->start_log_line ();
      src_point.print (logger->get_printer (), format (false));
      logger->end_log_line ();
    }

  /* Update state for the stmts that were processed in each enode.  */
  for (unsigned stmt_idx = 0; stmt_idx < src_enode.m_num_processed_stmts;
       stmt_idx++)
    {
      const gimple *stmt = src_enode.get_processed_stmt (stmt_idx);

      /* Update cfun and input_location in case of ICE: make it easier to
	 track down which source construct we're failing to handle.  */
      auto_cfun sentinel (src_point.get_function ());
      input_location = stmt->location;

      update_for_stmt (stmt);
    }

  const superedge *sedge = eedge->m_sedge;
  if (sedge)
    {
      if (logger)
	{
	  label_text desc (sedge->get_description (false));
	  logger->log ("  sedge: SN:%i -> SN:%i %s",
		       sedge->m_src->m_index,
		       sedge->m_dest->m_index,
		       desc.get ());
	}

      const gimple *last_stmt = src_point.get_supernode ()->get_last_stmt ();
      if (!m_model.maybe_update_for_edge (*sedge, last_stmt, ctxt, out_rc))
	{
	  if (logger)
	    {
	      logger->start_log_line ();
	      logger->log_partial ("rejecting due to region model: ");
	      m_model.dump_to_pp (logger->get_printer (), true, false);
	      logger->end_log_line ();
	    }
	  return false;
	}
    }
  else
    {
      /* Special-case the initial eedge from the origin node to the
	 initial function by pushing a frame for it.  */
      if (src_point.get_kind () == PK_ORIGIN)
	{
	  gcc_assert (eedge->m_src->m_index == 0);
	  gcc_assert (eedge->m_dest->get_point ().get_kind ()
		      == PK_BEFORE_SUPERNODE);
	  function *fun = eedge->m_dest->get_function ();
	  gcc_assert (fun);
	  m_model.push_frame (*fun, NULL, ctxt);
	  if (logger)
	    logger->log ("  pushing frame for %qD", fun->decl);
	}
      else if (eedge->m_custom_info)
	{
	  eedge->m_custom_info->update_model (&m_model, eedge, ctxt);
	}
    }

  /* Handle phi nodes on an edge leaving a PK_BEFORE_SUPERNODE (to
     a PK_BEFORE_STMT, or a PK_AFTER_SUPERNODE if no stmts).
     This will typically not be associated with a superedge.  */
  if (src_point.get_from_edge ())
    {
      const cfg_superedge *last_cfg_superedge
	= src_point.get_from_edge ()->dyn_cast_cfg_superedge ();
      const exploded_node &dst_enode = *eedge->m_dest;
      const unsigned dst_snode_idx = dst_enode.get_supernode ()->m_index;
      if (last_cfg_superedge)
	{
	  if (logger)
	    logger->log ("  update for phis");
	  m_model.update_for_phis (src_enode.get_supernode (),
				  last_cfg_superedge,
				  ctxt);
	  /* If we've entering an snode that we've already visited on this
	     epath, then we need do fix things up for loops; see the
	     comment for store::loop_replay_fixup.
	     Perhaps we should probably also verify the callstring,
	     and track program_points,  but hopefully doing it by supernode
	     is good enough.  */
	  if (bitmap_bit_p (m_snodes_visited, dst_snode_idx))
	    m_model.loop_replay_fixup (dst_enode.get_state ().m_region_model);
	}
      bitmap_set_bit (m_snodes_visited, dst_snode_idx);
    }
  return true;
}

/* Update this object for the effects of STMT.  */

void
feasibility_state::update_for_stmt (const gimple *stmt)
{
  if (const gassign *assign = dyn_cast <const gassign *> (stmt))
    m_model.on_assignment (assign, NULL);
  else if (const gasm *asm_stmt = dyn_cast <const gasm *> (stmt))
    m_model.on_asm_stmt (asm_stmt, NULL);
  else if (const gcall *call = dyn_cast <const gcall *> (stmt))
    {
      bool unknown_side_effects = m_model.on_call_pre (call, NULL);
      m_model.on_call_post (call, unknown_side_effects, NULL);
    }
  else if (const greturn *return_ = dyn_cast <const greturn *> (stmt))
    m_model.on_return (return_, NULL);
}

/* Dump this object to PP.  */

void
feasibility_state::dump_to_pp (pretty_printer *pp,
			       bool simple, bool multiline) const
{
  m_model.dump_to_pp (pp, simple, multiline);
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

  void dump_dot (graphviz_out *gv, const dump_args_t &args) const final override
  {
    gv->println ("subgraph \"cluster_supernode_%i\" {", m_supernode->m_index);
    gv->indent ();
    gv->println ("style=\"dashed\";");
    gv->println ("label=\"SN: %i (bb: %i; scc: %i)\";",
		 m_supernode->m_index, m_supernode->m_bb->index,
		 args.m_eg.get_scc_id (*m_supernode));

    int i;
    exploded_node *enode;
    FOR_EACH_VEC_ELT (m_enodes, i, enode)
      enode->dump_dot (gv, args);

    /* Terminate subgraph.  */
    gv->outdent ();
    gv->println ("}");
  }

  void add_node (exploded_node *en) final override
  {
    m_enodes.safe_push (en);
  }

  /* Comparator for use by auto_vec<supernode_cluster *>::qsort.  */

  static int cmp_ptr_ptr (const void *p1, const void *p2)
  {
    const supernode_cluster *c1
      = *(const supernode_cluster * const *)p1;
    const supernode_cluster *c2
      = *(const supernode_cluster * const *)p2;
    return c1->m_supernode->m_index - c2->m_supernode->m_index;
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
  function_call_string_cluster (function *fun, const call_string &cs)
  : m_fun (fun), m_cs (cs) {}

  ~function_call_string_cluster ()
  {
    for (map_t::iterator iter = m_map.begin ();
	 iter != m_map.end ();
	 ++iter)
      delete (*iter).second;
  }

  void dump_dot (graphviz_out *gv, const dump_args_t &args) const final override
  {
    const char *funcname = function_name (m_fun);

    gv->println ("subgraph \"cluster_function_%s\" {",
		 IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (m_fun->decl)));
    gv->indent ();
    gv->write_indent ();
    gv->print ("label=\"call string: ");
    m_cs.print (gv->get_pp ());
    gv->print (" function: %s \";", funcname);
    gv->print ("\n");

    /* Dump m_map, sorting it to avoid churn when comparing dumps.  */
    auto_vec<supernode_cluster *> child_clusters (m_map.elements ());
    for (map_t::iterator iter = m_map.begin ();
	 iter != m_map.end ();
	 ++iter)
      child_clusters.quick_push ((*iter).second);

    child_clusters.qsort (supernode_cluster::cmp_ptr_ptr);

    unsigned i;
    supernode_cluster *child_cluster;
    FOR_EACH_VEC_ELT (child_clusters, i, child_cluster)
      child_cluster->dump_dot (gv, args);

    /* Terminate subgraph.  */
    gv->outdent ();
    gv->println ("}");
  }

  void add_node (exploded_node *en) final override
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

  /* Comparator for use by auto_vec<function_call_string_cluster *>.  */

  static int cmp_ptr_ptr (const void *p1, const void *p2)
  {
    const function_call_string_cluster *c1
      = *(const function_call_string_cluster * const *)p1;
    const function_call_string_cluster *c2
      = *(const function_call_string_cluster * const *)p2;
    if (int cmp_names
	= strcmp (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (c1->m_fun->decl)),
		  IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (c2->m_fun->decl))))
      return cmp_names;
    return call_string::cmp (c1->m_cs, c2->m_cs);
  }

private:
  function *m_fun;
  const call_string &m_cs;
  typedef ordered_hash_map<const supernode *, supernode_cluster *> map_t;
  map_t m_map;
};

/* Keys for root_cluster.  */

struct function_call_string
{
  function_call_string (function *fun, const call_string *cs)
  : m_fun (fun), m_cs (cs)
  {
    gcc_assert (fun);
    gcc_assert (cs);
  }

  function *m_fun;
  const call_string *m_cs;
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
  return (pointer_hash <function>::hash (v.m_fun)
	  ^ pointer_hash <const call_string>::hash (v.m_cs));
}

template <>
inline bool
pod_hash_traits<function_call_string>::equal (const value_type &existing,
					      const value_type &candidate)
{
  return existing.m_fun == candidate.m_fun && &existing.m_cs == &candidate.m_cs;
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

  void dump_dot (graphviz_out *gv, const dump_args_t &args) const final override
  {
    int i;
    exploded_node *enode;
    FOR_EACH_VEC_ELT (m_functionless_enodes, i, enode)
      enode->dump_dot (gv, args);

    /* Dump m_map, sorting it to avoid churn when comparing dumps.  */
    auto_vec<function_call_string_cluster *> child_clusters (m_map.elements ());
    for (map_t::iterator iter = m_map.begin ();
	 iter != m_map.end ();
	 ++iter)
      child_clusters.quick_push ((*iter).second);

    child_clusters.qsort (function_call_string_cluster::cmp_ptr_ptr);

    function_call_string_cluster *child_cluster;
    FOR_EACH_VEC_ELT (child_clusters, i, child_cluster)
      child_cluster->dump_dot (gv, args);
  }

  void add_node (exploded_node *en) final override
  {
    function *fun = en->get_function ();
    if (!fun)
      {
	m_functionless_enodes.safe_push (en);
	return;
      }

    const call_string &cs = en->get_point ().get_call_string ();
    function_call_string key (fun, &cs);
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

  label_text get_text (unsigned) const final override
  {
    pretty_printer pp;
    pp_format_decoder (&pp) = default_tree_printer;
    m_enode->get_state ().dump_to_pp (m_ext_state, true, false, &pp);
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
	  text_art::dump_to_file (enode->get_state (), outf);
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
	    error_at (UNKNOWN_LOCATION, "unable to open %qs for writing",
		      filename);
	  free (filename);

	  fprintf (outf, "EN %i:\n", enode->m_index);
	  enode->dump_succs_and_preds (outf);
	  pretty_printer pp;
	  enode->get_point ().print (&pp, format (true));
	  fprintf (outf, "%s\n", pp_formatted_text (&pp));
	  text_art::dump_to_file (enode->get_state (), outf);

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

DEBUG_FUNCTION exploded_node *
exploded_graph::get_node_by_index (int idx) const
{
  exploded_node *enode = m_nodes[idx];
  gcc_assert (enode->m_index == idx);
  return enode;
}

/* Ensure that there is an exploded_node for a top-level call to FNDECL.  */

void
exploded_graph::on_escaped_function (tree fndecl)
{
  logger * const logger = get_logger ();
  LOG_FUNC_1 (logger, "%qE", fndecl);

  cgraph_node *cgnode = cgraph_node::get (fndecl);
  if (!cgnode)
    return;

  function *fun = cgnode->get_fun ();
  if (!fun)
    return;

  if (!gimple_has_body_p (fndecl))
    return;

  exploded_node *enode = add_function_entry (*fun);
  if (logger)
    {
      if (enode)
	logger->log ("created EN %i for %qE entrypoint",
		     enode->m_index, fun->decl);
      else
	logger->log ("did not create enode for %qE entrypoint", fun->decl);
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

  void dump_dot (graphviz_out *gv, const dump_args_t &args) const final override
  {
    pretty_printer *pp = gv->get_pp ();

    dump_dot_id (pp);
    pp_printf (pp, " [shape=none,margin=0,style=filled,fillcolor=%s,label=\"",
	       "lightgrey");
    pp_write_text_to_stream (pp);

    pp_printf (pp, "VCG: %i: %s", m_index, function_name (m_fun));
    pp_newline (pp);

    pp_printf (pp, "supernodes: %i\n", m_num_supernodes);
    pp_newline (pp);

    pp_printf (pp, "superedges: %i\n", m_num_superedges);
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
	pp_printf (pp, "enodes: %i\n", num_enodes);
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
		    && &enode->get_point ().get_call_string () == cs)
		  num_enodes++;
	      }
	    if (num_enodes > 0)
	      {
		cs->print (pp);
		pp_printf (pp, ": %i\n", num_enodes);
	      }
	  }

	/* Show any summaries.  */
	per_function_data *data = args.m_eg->get_per_function_data (m_fun);
	if (data)
	  {
	    pp_newline (pp);
	    pp_printf (pp, "summaries: %i\n", data->m_summaries.length ());
	    for (auto summary : data->m_summaries)
	      {
		pp_printf (pp, "\nsummary: %s:\n", summary->get_desc ().get ());
		const extrinsic_state &ext_state = args.m_eg->get_ext_state ();
		const program_state &state = summary->get_state ();
		state.dump_to_pp (ext_state, false, true, pp);
		pp_newline (pp);
	      }
	  }
      }

    pp_write_text_as_dot_label_to_stream (pp, /*for_record=*/true);
    pp_string (pp, "\"];\n\n");
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
    final override
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
    const final override
  {
    if (!within_table)
      return false;
    gv->begin_tr ();
    pretty_printer *pp = gv->get_pp ();

    gv->begin_td ();
    pp_string (pp, "BEFORE");
    pp_printf (pp, " (scc: %i)", m_eg.get_scc_id (n));
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
    const final override
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
    const final override
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
      case exploded_node::STATUS_BULK_MERGED:
	pp_string (pp, "(BM)");
	break;
      }
    gv->end_tdtr ();

    /* Dump any saved_diagnostics at this enode.  */
    for (unsigned i = 0; i < enode->get_num_diagnostics (); i++)
      {
	const saved_diagnostic *sd = enode->get_saved_diagnostic (i);
	print_saved_diagnostic (gv, sd);
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
    if (sd->get_best_epath ())
      pp_printf (pp, "epath length: %i", sd->get_epath_length ());
    else
      pp_printf (pp, "no best epath");
    gv->end_tdtr ();
    if (const feasibility_problem *p = sd->get_feasibility_problem ())
      {
	gv->begin_trtd ();
	pp_printf (pp, "INFEASIBLE at eedge %i: EN:%i -> EN:%i",
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
    pp_printf (pp, "</TABLE>");
    gv->end_tdtr ();
  }

  const exploded_graph &m_eg;
  auto_delete_vec<auto_vec <exploded_node *> > m_enodes_per_snodes;
};

/* Implement -fdump-analyzer-json.  */

static void
dump_analyzer_json (const supergraph &sg,
		    const exploded_graph &eg)
{
  auto_timevar tv (TV_ANALYZER_DUMP);
  char *filename = concat (dump_base_name, ".analyzer.json.gz", NULL);
  gzFile output = gzopen (filename, "w");
  if (!output)
    {
      error_at (UNKNOWN_LOCATION, "unable to open %qs for writing", filename);
      free (filename);
      return;
    }

  auto toplev_obj = ::make_unique<json::object> ();
  toplev_obj->set ("sgraph", sg.to_json ());
  toplev_obj->set ("egraph", eg.to_json ());

  pretty_printer pp;
  toplev_obj->print (&pp, flag_diagnostics_json_formatting);
  pp_formatted_text (&pp);

  if (gzputs (output, pp_formatted_text (&pp)) == EOF
      || gzclose (output))
    error_at (UNKNOWN_LOCATION, "error writing %qs", filename);

  free (filename);
}

/* Concrete subclass of plugin_analyzer_init_iface, allowing plugins
   to register new state machines.  */

class plugin_analyzer_init_impl : public plugin_analyzer_init_iface
{
public:
  plugin_analyzer_init_impl (auto_delete_vec <state_machine> *checkers,
			     known_function_manager *known_fn_mgr,
			     logger *logger)
  : m_checkers (checkers),
    m_known_fn_mgr (known_fn_mgr),
    m_logger (logger)
  {}

  void register_state_machine (std::unique_ptr<state_machine> sm) final override
  {
    LOG_SCOPE (m_logger);
    m_checkers->safe_push (sm.release ());
  }

  void register_known_function (const char *name,
				std::unique_ptr<known_function> kf) final override
  {
    LOG_SCOPE (m_logger);
    m_known_fn_mgr->add (name, std::move (kf));
  }

  logger *get_logger () const final override
  {
    return m_logger;
  }

private:
  auto_delete_vec <state_machine> *m_checkers;
  known_function_manager *m_known_fn_mgr;
  logger *m_logger;
};

/* Run the analysis "engine".  */

void
impl_run_checkers (logger *logger)
{
  LOG_SCOPE (logger);

  if (logger)
    {
      logger->log ("BITS_BIG_ENDIAN: %i", BITS_BIG_ENDIAN ? 1 : 0);
      logger->log ("BYTES_BIG_ENDIAN: %i", BYTES_BIG_ENDIAN ? 1 : 0);
      logger->log ("WORDS_BIG_ENDIAN: %i", WORDS_BIG_ENDIAN ? 1 : 0);
      log_stashed_constants (logger);
    }

  /* If using LTO, ensure that the cgraph nodes have function bodies.  */
  cgraph_node *node;
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (node)
    node->get_untransformed_body ();

  /* Create the supergraph.  */
  supergraph sg (logger);

  engine eng (&sg, logger);

  state_purge_map *purge_map = NULL;

  if (flag_analyzer_state_purge)
    purge_map = new state_purge_map (sg, eng.get_model_manager (), logger);

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

  register_known_functions (*eng.get_known_function_manager (),
			    *eng.get_model_manager ());

  plugin_analyzer_init_impl data (&checkers,
				  eng.get_known_function_manager (),
				  logger);
  invoke_plugin_callbacks (PLUGIN_ANALYZER_INIT, &data);

  if (logger)
    {
      int i;
      state_machine *sm;
      FOR_EACH_VEC_ELT (checkers, i, sm)
	logger->log ("checkers[%i]: %s", i, sm->get_name ());
    }

  /* Extrinsic state shared by nodes in the graph.  */
  const extrinsic_state ext_state (checkers, &eng, logger);

  const analysis_plan plan (sg, logger);

  /* The exploded graph.  */
  exploded_graph eg (sg, logger, ext_state, purge_map, plan,
		     analyzer_verbosity);

  /* Add entrypoints to the graph for externally-callable functions.  */
  eg.build_initial_worklist ();

  /* Now process the worklist, exploring the <point, state> graph.  */
  eg.process_worklist ();

  if (warn_analyzer_infinite_loop)
    eg.detect_infinite_loops ();

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

  if (flag_dump_analyzer_json)
    dump_analyzer_json (sg, eg);

  if (flag_dump_analyzer_untracked)
    eng.get_model_manager ()->dump_untracked_regions ();

  delete purge_map;

  /* Free up any dominance info that we may have created.  */
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (node)
    {
      function *fun = node->get_fun ();
      free_dominance_info (fun, CDI_DOMINATORS);
    }
}

/* Handle -fdump-analyzer and -fdump-analyzer-stderr.  */
static FILE *dump_fout = NULL;

/* Track if we're responsible for closing dump_fout.  */
static bool owns_dump_fout = false;

/* If dumping is enabled, attempt to create dump_fout if it hasn't already
   been opened.  Return it.  */

FILE *
get_or_create_any_logfile ()
{
  if (!dump_fout)
    {
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
     }
  return dump_fout;
}

/* External entrypoint to the analysis "engine".
   Set up any dumps, then call impl_run_checkers.  */

void
run_checkers ()
{
  /* Save input_location.  */
  location_t saved_input_location = input_location;

  {
    log_user the_logger (NULL);
    get_or_create_any_logfile ();
    if (dump_fout)
      the_logger.set_logger (new logger (dump_fout, 0, 0,
					 *global_dc->get_reference_printer ()));
    LOG_SCOPE (the_logger.get_logger ());

    impl_run_checkers (the_logger.get_logger ());

    /* end of lifetime of the_logger (so that dump file is closed after the
       various dtors run).  */
  }

  if (owns_dump_fout)
    {
      fclose (dump_fout);
      owns_dump_fout = false;
      dump_fout = NULL;
    }

  /* Restore input_location.  Subsequent passes may assume that input_location
     is some arbitrary value *not* in the block tree, which might be violated
     if we didn't restore it.  */
  input_location = saved_input_location;
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
