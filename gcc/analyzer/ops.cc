/* Operations within the code being analyzed.
   Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

#include "analyzer/common.h"

#include "gimple-pretty-print.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-dfa.h"
#include "fold-const.h"
#include "cgraph.h"
#include "text-art/dump.h"
#include "text-art/tree-widget.h"

#include "analyzer/ops.h"
#include "analyzer/call-details.h"
#include "analyzer/exploded-graph.h"
#include "analyzer/checker-path.h"
#include "analyzer/impl-sm-context.h"
#include "analyzer/constraint-manager.h"
#include "analyzer/call-summary.h"
#include "analyzer/call-info.h"
#include "analyzer/analysis-plan.h"

#if ENABLE_ANALYZER

namespace ana {

event_loc_info::event_loc_info (const exploded_node *enode)
{
  if (enode)
    {
      m_loc = enode->get_location ();
      m_fndecl = enode->get_point ().get_fndecl ();
      m_depth = enode->get_stack_depth ();
    }
  else
    {
      m_loc = UNKNOWN_LOCATION;
      m_fndecl = NULL_TREE;
      m_depth = 0;
    }
}

event_loc_info::event_loc_info (const program_point &point)
{
  m_loc = point.get_location ();
  m_fndecl = point.get_fndecl ();
  m_depth = point.get_stack_depth ();
}

// struct operation_context

void
operation_context::dump () const
{
  fprintf (stderr, "src enode: EN: %i\n", m_src_enode.m_index);
  m_src_enode.dump (m_eg.get_ext_state ());

  fprintf (stderr, "superedge\n");
  pretty_printer pp;
  pp.set_output_stream (stderr);
  m_sedge.dump (&pp);
}

logger *
operation_context::get_logger () const
{
  return m_eg.get_logger ();
}

const extrinsic_state &
operation_context::get_ext_state () const
{
  return m_eg.get_ext_state ();
}

const program_point &
operation_context::get_initial_point () const
{
  return m_src_enode.get_point ();
}

const program_state &
operation_context::get_initial_state () const
{
  return m_src_enode.get_state ();
}

const supergraph &
operation_context::get_supergraph () const
{
  return m_eg.get_supergraph ();
}

program_point
operation_context::get_next_intraprocedural_point () const
{
  /* All edges are intraprocedural.  */
  gcc_assert (m_sedge.m_src->get_function ()
	      == m_sedge.m_dest->get_function ());
  return program_point (m_sedge.m_dest,
			m_src_enode.get_point ().get_call_string ());
}

void
operation_context::add_outcome (const program_point &dst_point,
				program_state dst_state,
				bool could_do_work,
				uncertainty_t *uncertainty,
				std::unique_ptr<custom_edge_info> info)
{
  const program_state &src_state = get_initial_state ();
  impl_region_model_context ctxt (m_eg, &m_src_enode,
				  &src_state, &dst_state,
				  uncertainty, nullptr);
  program_state::detect_leaks (src_state, dst_state, nullptr,
			       get_ext_state (), &ctxt);

  if (exploded_node *dst_enode
      = m_eg.get_or_create_node (dst_point, dst_state, &m_src_enode))
    {
      m_eg.add_edge (&m_src_enode, dst_enode, &m_sedge, could_do_work,
		     std::move (info));
      m_eg.detect_infinite_recursion (dst_enode);
    }
}

class op_region_model_context : public impl_region_model_context
{
public:
  op_region_model_context (operation_context &op_ctxt,
			   program_state &dst_state)
  : impl_region_model_context (op_ctxt.m_eg,
			       &op_ctxt.m_src_enode,
			       &op_ctxt.get_initial_state (),
			       &dst_state,
			       nullptr,
			       &m_path_context)
  {
  }

  bool terminate_path_p () const
  {
    return m_path_context.terminate_path_p ();
  }

private:
  class op_path_context : public path_context
  {
  public:
    op_path_context ()
    : m_terminate_path (false)
    {
    }

    void bifurcate (std::unique_ptr<custom_edge_info>) final override
    {
      gcc_unreachable ();
    }

    void terminate_path () final override
    {
      m_terminate_path = true;
    }

    bool terminate_path_p () const final override
    {
      return m_terminate_path;
    }
  private:
    bool m_terminate_path;
  } m_path_context;
};

// class gimple_stmt_op : public operation

void
gimple_stmt_op::print_as_edge_label (pretty_printer *pp,
				     bool /*user_facing*/) const
{
  pp_gimple_stmt_1 (pp, &m_stmt, 0, (dump_flags_t)0);
}

bool
gimple_stmt_op::defines_ssa_name_p (const_tree ssa_name) const
{
  return &m_stmt == SSA_NAME_DEF_STMT (ssa_name);
}

bool
gimple_stmt_op::supports_bulk_merge_p () const
{
  return false;
}

/* Subclass of path_context for use within operation::execute implementations
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

DEBUG_FUNCTION void
operation::dump () const
{
  tree_dump_pretty_printer pp (stderr);
  print_as_edge_label (&pp, false);
  pp_newline (&pp);
}

void
operation::handle_on_stmt_for_state_machines (operation_context &op_ctxt,
					      program_state &dst_state,
					      path_context *path_ctxt,
					      bool &unknown_side_effects,
					      const gimple &stmt)
{
  const program_state &old_state = op_ctxt.get_initial_state ();
  int sm_idx;
  sm_state_map *smap;
  FOR_EACH_VEC_ELT (old_state.m_checker_states, sm_idx, smap)
    {
      const state_machine &sm = op_ctxt.m_eg.get_ext_state ().get_sm (sm_idx);
      const sm_state_map *old_smap
	= old_state.m_checker_states[sm_idx];
      sm_state_map *new_smap = dst_state.m_checker_states[sm_idx];
      impl_sm_context sm_ctxt (op_ctxt.m_eg, sm_idx, sm,
			       &op_ctxt.m_src_enode,
			       &old_state,
			       &dst_state,
			       old_smap, new_smap, path_ctxt,
			       unknown_side_effects);

      /* Allow the state_machine to handle the stmt.  */
      if (sm.on_stmt (sm_ctxt, &stmt))
	unknown_side_effects = false;
    }
}

void
gimple_stmt_op::
walk_load_store_addr_ops (void *data,
			  walk_stmt_load_store_addr_fn load_cb,
			  walk_stmt_load_store_addr_fn store_cb,
			  walk_stmt_load_store_addr_fn addr_cb) const
{
  walk_stmt_load_store_addr_ops (const_cast<gimple *>(&m_stmt), data,
				 load_cb, store_cb, addr_cb);
}

void
gimple_stmt_op::execute (operation_context &op_ctxt) const
{
  auto logger = op_ctxt.get_logger ();
  LOG_SCOPE (logger);
  if (logger)
    {
      logger->start_log_line ();
      pp_gimple_stmt_1 (logger->get_printer (), &get_stmt (), 0,
			(dump_flags_t)0);
      logger->end_log_line ();
    }
  execute_on_state (op_ctxt,
		    /* Pass in a copy.  */
		    op_ctxt.get_initial_state ());
}

void
gimple_stmt_op::execute_on_state (operation_context &op_ctxt,
				  program_state dst_state) const
{
  auto logger = op_ctxt.get_logger ();
  LOG_SCOPE (logger);

  auto dst_point (op_ctxt.get_next_intraprocedural_point ());
  const program_state &old_state  = op_ctxt.get_initial_state ();

  bool unknown_side_effects = false;
  bool could_have_done_work = false;

  impl_path_context path_ctxt (&dst_state, logger);
  uncertainty_t uncertainty;
  impl_region_model_context ctxt (op_ctxt.m_eg,
				  &op_ctxt.m_src_enode,
				  &old_state,
				  &dst_state,
				  &uncertainty,
				  &path_ctxt,
				  &m_stmt,
				  &could_have_done_work);

  dst_state.m_region_model->on_stmt_pre (&get_stmt (),
					 &unknown_side_effects,
					 &ctxt);

  handle_on_stmt_for_state_machines (op_ctxt,
				     dst_state,
				     &path_ctxt,
				     unknown_side_effects,
				     m_stmt);

  if (path_ctxt.terminate_path_p ())
    return;

  if (const gcall *call = dyn_cast <const gcall *> (&m_stmt))
    dst_state.m_region_model->on_call_post (*call, unknown_side_effects, &ctxt);

  if (!path_ctxt.terminate_path_p ())
    op_ctxt.add_outcome (dst_point, dst_state, could_have_done_work,
			 &uncertainty);

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
	bifurcation_ctxt (op_ctxt.m_eg,
			  &op_ctxt.m_src_enode,
			  &path_ctxt.get_state_at_bifurcation (),
			  &bifurcated_new_state,
			  nullptr, // uncertainty_t *uncertainty
			  nullptr, // path_context *path_ctxt
			  &m_stmt);
      if (edge_info->update_state (&bifurcated_new_state,
				   nullptr, /* no exploded_edge yet.  */
				   &bifurcation_ctxt))
	{
	  if (exploded_node *next2
	      = edge_info->create_enode
	      (op_ctxt.m_eg,
	       dst_point,
	       std::move (bifurcated_new_state),
	       &op_ctxt.m_src_enode,
	       &bifurcation_ctxt))
	    {
	      op_ctxt.m_eg.add_edge (&op_ctxt.m_src_enode, next2, nullptr,
				     true /* assume that work could be done */,
				     std::move (edge_info));
	    }
	}
    }
}

bool
gimple_stmt_op::
execute_for_feasibility (const exploded_edge &,
			 feasibility_state &fstate,
			 region_model_context *ctxt,
			 std::unique_ptr<rejected_constraint> */*out_rc*/) const
{
  region_model &model = fstate.get_model ();
  bool unknown_side_effects;
  model.on_stmt_pre (&m_stmt, &unknown_side_effects, ctxt);

  if (const gcall *call = dyn_cast <const gcall *> (&m_stmt))
    model.on_call_post (*call, unknown_side_effects, ctxt);

  return true;
}

/* An sm_context for adding state_change_event on assignments to NULL,
   where the default state isn't m_start.  Storing such state in the
   sm_state_map would lead to bloat of the exploded_graph, so we want
   to leave it as a default state, and inject state change events here
   when we have a diagnostic.
   Find transitions of constants, for handling on_zero_assignment.  */

struct null_assignment_sm_context : public sm_context
{
  null_assignment_sm_context (int sm_idx,
			      const state_machine &sm,
			      const program_state *old_state,
			      const program_state *new_state,
			      const gimple *stmt,
			      const program_point *point,
			      checker_path *emission_path,
			      const extrinsic_state &ext_state)
  : sm_context (sm_idx, sm), m_old_state (old_state), m_new_state (new_state),
    m_stmt (stmt), m_point (point), m_emission_path (emission_path),
    m_ext_state (ext_state)
  {
  }

  tree get_fndecl_for_call (const gcall &/*call*/) final override
  {
    return NULL_TREE;
  }

  state_machine::state_t get_state (tree var) final override
  {
    const svalue *var_old_sval
      = m_old_state->m_region_model->get_rvalue (var, nullptr);
    const sm_state_map *old_smap = m_old_state->m_checker_states[m_sm_idx];

    state_machine::state_t current
      = old_smap->get_state (var_old_sval, m_ext_state);

    return current;
  }

  state_machine::state_t get_state (const svalue *sval) final override
  {
    const sm_state_map *old_smap = m_old_state->m_checker_states[m_sm_idx];
    state_machine::state_t current = old_smap->get_state (sval, m_ext_state);
    return current;
  }

  void set_next_state (tree var,
		       state_machine::state_t to,
		       tree origin ATTRIBUTE_UNUSED) final override
  {
    state_machine::state_t from = get_state (var);
    if (from != m_sm.get_start_state ())
      return;
    if (!is_transition_to_null (to))
      return;

    const svalue *var_new_sval
      = m_new_state->m_region_model->get_rvalue (var, nullptr);

    m_emission_path->add_event
      (std::make_unique<state_change_event> (event_loc_info (*m_point),
					     m_stmt,
					     m_sm,
					     var_new_sval,
					     from, to,
					     nullptr,
					     *m_new_state,
					     nullptr));
  }

  void set_next_state (const svalue *sval,
		       state_machine::state_t to,
		       tree origin ATTRIBUTE_UNUSED) final override
  {
    state_machine::state_t from = get_state (sval);
    if (from != m_sm.get_start_state ())
      return;
    if (!is_transition_to_null (to))
      return;

    m_emission_path->add_event
      (std::make_unique<state_change_event> (event_loc_info (*m_point),
					     m_stmt,
					     m_sm,
					     sval,
					     from, to,
					     nullptr,
					     *m_new_state,
					     nullptr));
  }

  void warn (tree, std::unique_ptr<pending_diagnostic>) final override
  {
  }
  void warn (const svalue *, std::unique_ptr<pending_diagnostic>) final override
  {
  }

  tree get_diagnostic_tree (tree expr) final override
  {
    return expr;
  }

  tree get_diagnostic_tree (const svalue *sval) final override
  {
    return m_new_state->m_region_model->get_representative_tree (sval);
  }

  state_machine::state_t get_global_state () const final override
  {
    return 0;
  }

  void set_global_state (state_machine::state_t) final override
  {
    /* No-op.  */
  }

  void clear_all_per_svalue_state () final override
  {
    /* No-op.  */
  }

  void on_custom_transition (custom_transition *) final override
  {
  }

  tree is_zero_assignment (const gimple *stmt) final override
  {
    const gassign *assign_stmt = dyn_cast <const gassign *> (stmt);
    if (!assign_stmt)
     return NULL_TREE;
    if (const svalue *sval
	= m_new_state->m_region_model->get_gassign_result (assign_stmt, nullptr))
      if (tree cst = sval->maybe_get_constant ())
	if (::zerop(cst))
	  return gimple_assign_lhs (assign_stmt);
    return NULL_TREE;
  }

  const program_state *get_old_program_state () const final override
  {
    return m_old_state;
  }
  const program_state *get_new_program_state () const final override
  {
    return m_new_state;
  }

  location_t get_emission_location () const final override
  {
    return UNKNOWN_LOCATION;
  }

  /* We only care about transitions to the "null" state
     within sm-malloc.  Special-case this.  */
  static bool is_transition_to_null (state_machine::state_t s)
  {
    return !strcmp (s->get_name (), "null");
  }

  const program_state *m_old_state;
  const program_state *m_new_state;
  const gimple *m_stmt;
  const program_point *m_point;
  checker_path *m_emission_path;
  const extrinsic_state &m_ext_state;
};

void
gimple_stmt_op::add_any_events_for_eedge (const exploded_edge &eedge,
					  checker_path &out_path) const
{
  out_path.add_event
    (std::make_unique<statement_event> (&get_stmt (),
					eedge.m_dest->get_function ()->decl,
					eedge.m_dest->get_stack_depth (),
					eedge.m_dest->get_state ()));

  /* Create state change events for assignment to NULL.
     Iterate through the stmts in dst_enode, adding state change
     events for them.  */
  if (const gassign *assign = dyn_cast<const gassign *> (&m_stmt))
    {
      const program_point &src_point = eedge.m_src->get_point ();
      const extrinsic_state &ext_state = out_path.get_ext_state ();
      for (unsigned i = 0; i < ext_state.get_num_checkers (); i++)
	{
	  const state_machine &sm = ext_state.get_sm (i);
	  null_assignment_sm_context sm_ctxt (i, sm,
					      &eedge.m_src->get_state (),
					      &eedge.m_dest->get_state (),
					      assign,
					      &src_point,
					      &out_path,
					      ext_state);
	  sm.on_stmt (sm_ctxt, assign);
	  // TODO: what about phi nodes?
	}
    }
}

// class gassign_op : public gimple_stmt_op

// class greturn_op : public gimple_stmt_op

void
greturn_op::execute (operation_context &op_ctxt) const
{
  auto logger = op_ctxt.get_logger ();

  auto dst_point (op_ctxt.get_next_intraprocedural_point ());
  const program_state &old_state  = op_ctxt.get_initial_state ();
  program_state dst_state (old_state);

  impl_path_context path_ctxt (&dst_state, logger);
  uncertainty_t uncertainty;
  impl_region_model_context ctxt (op_ctxt.m_eg,
				  &op_ctxt.m_src_enode,

				  /* TODO: should we be getting the ECs from the
				     old state, rather than the new?  */
				  &op_ctxt.get_initial_state (),
				  &dst_state,
				  &uncertainty,
				  &path_ctxt,
				  nullptr,
				  nullptr);

  tree callee = op_ctxt.get_initial_point ().get_function ()->decl;
  tree lhs = DECL_RESULT (callee);

  if (lhs && get_retval ())
    {
      region_model *dst_region_model = dst_state.m_region_model;
      const svalue *sval
	= dst_region_model->get_rvalue (get_retval (), &ctxt);
      const region *ret_reg = dst_region_model->get_lvalue (lhs, &ctxt);
      dst_region_model->set_value (ret_reg, sval, &ctxt);
    }

  if (!path_ctxt.terminate_path_p ())
    op_ctxt.add_outcome (dst_point, dst_state, false, &uncertainty);
}

bool
greturn_op::
execute_for_feasibility (const exploded_edge &eedge,
			 feasibility_state &fstate,
			 region_model_context *ctxt,
			 std::unique_ptr<rejected_constraint> *) const
{
  tree callee = eedge.m_src->get_function ()->decl;
  tree lhs = DECL_RESULT (callee);

  if (lhs && get_retval ())
    {
      region_model &model = fstate.get_model ();
      const svalue *sval = model.get_rvalue (get_retval (), ctxt);
      const region *ret_reg = model.get_lvalue (lhs, ctxt);
      model.set_value (ret_reg, sval, ctxt);
    }

  return true;
}

void
greturn_op::add_any_events_for_eedge (const exploded_edge &,
				      checker_path &) const
{
  // No-op.
}

// class call_and_return_op : public gimple_stmt_op

std::unique_ptr<operation>
call_and_return_op::make (const gcall &call_stmt)
{
  if (is_special_named_call_p (call_stmt, "__analyzer_dump", 0))
    return std::make_unique<dump_op> (call_stmt, dump_op::dump_kind::state);
  else if (is_special_named_call_p (call_stmt, "__analyzer_dump_sarif", 0))
    return std::make_unique<dump_op> (call_stmt, dump_op::dump_kind::sarif);
  else if (is_special_named_call_p (call_stmt, "__analyzer_dump_dot", 0))
    return std::make_unique<dump_op> (call_stmt, dump_op::dump_kind::dot);
  else if (is_special_named_call_p (call_stmt, "__analyzer_dump_state", 2))
    return std::make_unique<dump_op> (call_stmt, dump_op::dump_kind::state_2);
  else if (is_setjmp_call_p (call_stmt))
    return std::make_unique<setjmp_op> (call_stmt);
  else if (is_longjmp_call_p (call_stmt))
    return std::make_unique<longjmp_op> (call_stmt);
  else if (is_cxa_throw_p (call_stmt))
    return std::make_unique<cxa_throw_op> (call_stmt, false);
  else if (is_cxa_rethrow_p (call_stmt))
    return std::make_unique<cxa_throw_op> (call_stmt, true);

  return std::make_unique<call_and_return_op> (call_stmt);
}

/* Resolve a function call by one of:

   (a) using a call summary to add eedges to new enodes capturing
   the states after summarized outcomes of the call

   (b) adding an interprocedural_call edge, effectively "stepping into"
   the called function, for detailed analysis of that path

   (c) simulating the effect of the call, adding an eedge to a new
   enode for the outcome of the call.  */

void
call_and_return_op::execute (operation_context &op_ctxt) const
{
  /* Can we turn this into an interprocedural call, and execute within
     the called fuction?  */
  const program_state &old_state  = op_ctxt.get_initial_state ();
  program_state dst_state (old_state);
  op_region_model_context ctxt (op_ctxt, dst_state);
  ctxt.m_stmt = &get_gcall ();
  call_details cd (get_gcall (), old_state.m_region_model, &ctxt);

  /* Regardless of how we handle the call, check any known
     preconditions.  */
  {
    /* Check for any preconditions if it's a known_function.  */
    if (auto kf = maybe_get_known_function (cd))
      kf->check_any_preconditions (cd);

    /* Check for any preconditions using sm-state.  */
    {
      int sm_idx;
      sm_state_map *smap;
      FOR_EACH_VEC_ELT (old_state.m_checker_states, sm_idx, smap)
	{
	  const state_machine &sm
	    = op_ctxt.m_eg.get_ext_state ().get_sm (sm_idx);
	  const sm_state_map *old_smap
	    = old_state.m_checker_states[sm_idx];
	  sm_state_map *new_smap = dst_state.m_checker_states[sm_idx];
	  impl_sm_context sm_ctxt (op_ctxt.m_eg, sm_idx, sm,
				   &op_ctxt.m_src_enode,
				   &old_state, &dst_state,
				   old_smap, new_smap, nullptr);
	  sm.check_call_preconditions (sm_ctxt, cd);
	}
    }
  }

  if (tree callee_fndecl = cd.get_fndecl_for_call ())
    {
      // Consider using a call summary
      if (function *called_fn = DECL_STRUCT_FUNCTION (callee_fndecl))
	if (cgraph_edge *edge = get_any_cgraph_edge (op_ctxt))
	  if (op_ctxt.m_eg.get_analysis_plan ().use_summary_p (edge))
	    {
	      per_function_data *called_fn_data
		= op_ctxt.m_eg.get_per_function_data (called_fn);
	      if (called_fn_data)
		{
		  replay_call_summaries (op_ctxt, *called_fn,
					 *called_fn_data, &ctxt);
		  return;
		}
	    }

      // Do we have an entry snode for this fndecl?
      if (auto callee_fun = DECL_STRUCT_FUNCTION (callee_fndecl))
	if (supernode *callee_entry_snode
	    = (op_ctxt.get_supergraph ()
	       .get_node_for_function_entry (*callee_fun)))
	  {
	    const call_string *dst_call_string
	      (op_ctxt.m_src_enode
	       .get_point ()
	       .get_call_string ()
	       .push_call (op_ctxt.m_sedge, *this, *callee_fun));
	    const program_point dst_point
	      (callee_entry_snode, *dst_call_string);
	    auto edge_info
	      = std::make_unique<interprocedural_call> (get_gcall (),
							*callee_fun);
	    edge_info->update_state (&dst_state, nullptr, &ctxt);
	    op_ctxt.add_outcome (dst_point, dst_state, false, nullptr,
				 std::move (edge_info));
	    return;
	  }
    }

  /* Resolve intraprocedurally: execute the gcall, but using the
     dst_state from above so that any preconditions have been applied.  */
  gimple_stmt_op::execute_on_state (op_ctxt, std::move (dst_state));
}

cgraph_edge *
call_and_return_op::get_any_cgraph_edge (operation_context &op_ctxt) const
{
  tree caller_fndecl = op_ctxt.get_initial_point ().get_fndecl ();
  gcc_assert (caller_fndecl);

  auto caller_cgnode = cgraph_node::get (caller_fndecl);
  gcc_assert (caller_cgnode);
  return caller_cgnode->get_edge (const_cast<gcall *> (&get_gcall ()));
}

void
call_and_return_op::
add_any_events_for_eedge (const exploded_edge &,
			  checker_path &) const
{
}

/* Given PARM_TO_FIND, a PARM_DECL, identify its index (writing it
   to *OUT if OUT is non-NULL), and return the corresponding argument
   at the callsite.  */

tree
call_and_return_op::get_arg_for_parm (tree callee_fndecl,
				      tree parm_to_find,
				      callsite_expr *out) const
{
  gcc_assert  (TREE_CODE (parm_to_find) == PARM_DECL);

  const gcall &call_stmt = get_gcall ();

  unsigned i = 0;
  for (tree iter_parm = DECL_ARGUMENTS (callee_fndecl); iter_parm;
       iter_parm = DECL_CHAIN (iter_parm), ++i)
    {
      if (i >= gimple_call_num_args (&call_stmt))
	return NULL_TREE;
      if (iter_parm == parm_to_find)
	{
	  if (out)
	    *out = callsite_expr::from_zero_based_param (i);
	  return gimple_call_arg (&call_stmt, i);
	}
    }

  /* Not found.  */
  return NULL_TREE;
}

/* Look for a use of ARG_TO_FIND as an argument at this callsite.
   If found, return the default SSA def of the corresponding parm within
   the callee, and if OUT is non-NULL, write the index to *OUT.
   Only the first match is handled.  */

tree
call_and_return_op::get_parm_for_arg (tree callee_fndecl,
				      tree arg_to_find,
				      callsite_expr *out) const
{
  const gcall &call_stmt = get_gcall ();

  unsigned i = 0;
  for (tree iter_parm = DECL_ARGUMENTS (callee_fndecl); iter_parm;
       iter_parm = DECL_CHAIN (iter_parm), ++i)
    {
      if (i >= gimple_call_num_args (&call_stmt))
	return NULL_TREE;
      tree param = gimple_call_arg (&call_stmt, i);
      if (arg_to_find == param)
	{
	  if (out)
	    *out = callsite_expr::from_zero_based_param (i);
	  return ssa_default_def (DECL_STRUCT_FUNCTION (callee_fndecl),
				  iter_parm);
	}
    }

  /* Not found.  */
  return NULL_TREE;
}

/* Map caller_expr back to an expr within the callee, or return NULL_TREE.
   If non-NULL is returned, populate OUT.  */

tree
call_and_return_op::map_expr_from_caller_to_callee (tree callee_fndecl,
						    tree caller_expr,
						    callsite_expr *out) const
{
  /* Is it an argument (actual param)?  If so, convert to
     parameter (formal param).  */
  tree parm = get_parm_for_arg (callee_fndecl, caller_expr, out);
  if (parm)
    return parm;
  /* Otherwise try return value.  */
  if (caller_expr == gimple_call_lhs (&get_gcall ()))
    {
      if (out)
	*out = callsite_expr::from_return_value ();
      return DECL_RESULT (callee_fndecl);
    }

  return NULL_TREE;
}

/* Map callee_expr back to an expr within the caller, or return NULL_TREE.
   If non-NULL is returned, populate OUT.  */

tree
call_and_return_op::map_expr_from_callee_to_caller (tree callee_fndecl,
						    tree callee_expr,
						    callsite_expr *out) const
{
  if (callee_expr == NULL_TREE)
    return NULL_TREE;

  /* If it's a parameter (formal param), get the argument (actual param).  */
  if (TREE_CODE (callee_expr) == PARM_DECL)
    return get_arg_for_parm (callee_fndecl, callee_expr, out);

  /* Similar for the default SSA name of the PARM_DECL.  */
  if (TREE_CODE (callee_expr) == SSA_NAME
      && SSA_NAME_IS_DEFAULT_DEF (callee_expr)
      && TREE_CODE (SSA_NAME_VAR (callee_expr)) == PARM_DECL)
    return get_arg_for_parm (callee_fndecl, SSA_NAME_VAR (callee_expr), out);

  /* Otherwise try return value.  */
  if (callee_expr == DECL_RESULT (callee_fndecl))
    {
      if (out)
	*out = callsite_expr::from_return_value ();
      return gimple_call_lhs (&get_gcall ());
    }

  return NULL_TREE;
}

const known_function *
call_and_return_op::maybe_get_known_function (const call_details &cd) const
{
  region_model_manager *mgr = cd.get_manager ();
  known_function_manager *known_fn_mgr = mgr->get_known_function_manager ();

  if (gimple_call_internal_p (&get_gcall ()))
    return known_fn_mgr->get_internal_fn
      (gimple_call_internal_fn (&get_gcall ()));

  if (tree callee_fndecl = cd.get_fndecl_for_call ())
    return known_fn_mgr->get_match (callee_fndecl, cd);

  return nullptr;
}

void
call_and_return_op::
replay_call_summaries (operation_context &op_ctxt,
		       function &called_fn,
		       per_function_data &called_fn_data,
		       region_model_context *ctxt) const
{
  logger *logger = op_ctxt.get_logger ();
  LOG_SCOPE (logger);

  for (auto summary : called_fn_data.m_summaries)
    {
      gcc_assert (summary);
      replay_call_summary (op_ctxt, called_fn, *summary, ctxt);
    }
}

/* A concrete call_info subclass representing a replay of a call summary.  */

class call_summary_edge_info : public call_info
{
public:
  call_summary_edge_info (const call_details &cd,
			  const function &called_fn,
			  call_summary &summary,
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
    const program_state &summary_end_state = m_summary.get_state ();
    return state->replay_call_summary (r, summary_end_state);
  }

  bool update_model (region_model *model,
		     const exploded_edge *,
		     region_model_context *ctxt) const final override
  {
    /* Update STATE based on summary_end_state.  */
    call_details cd (get_call_details (model, ctxt));
    call_summary_replay r (cd, m_called_fn, m_summary, m_ext_state);
    const program_state &summary_end_state = m_summary.get_state ();
    model->replay_call_summary (r, *summary_end_state.m_region_model);
    return true;
  }

  void print_desc (pretty_printer &pp) const final override
  {
    pp_string (&pp, m_summary.get_desc ().get ());
  }

private:
  const function &m_called_fn;
  call_summary &m_summary;
  const extrinsic_state &m_ext_state;
};

void
call_and_return_op::
replay_call_summary (operation_context &op_ctxt,
		     function &called_fn,
		     call_summary &summary,
		     region_model_context *ctxt) const
{
  logger *logger = op_ctxt.get_logger ();
  LOG_SCOPE (logger);
  if (logger)
    logger->log ("using %s as summary for call to %qE from %qE",
		 summary.get_desc ().get (),
		 called_fn.decl,
		 op_ctxt.get_initial_point ().get_function ()->decl);
  const extrinsic_state &ext_state = op_ctxt.get_ext_state ();
  const program_state &old_state = op_ctxt.get_initial_state ();
  const program_state &summary_end_state = summary.get_state ();
  if (logger)
    {
      pretty_printer *pp = logger->get_printer ();

      logger->start_log_line ();
      pp_string (pp, "callsite state: ");
      old_state.dump_to_pp (ext_state, true, false, pp);
      logger->end_log_line ();

      logger->start_log_line ();
      pp_string (pp, "summary end state: ");
      summary_end_state.dump_to_pp (ext_state, true, false, pp);
      logger->end_log_line ();
    }

  program_state new_state (old_state);

  call_details cd (get_gcall (), new_state.m_region_model, ctxt);
  call_summary_replay r (cd, called_fn, summary, ext_state);

  if (new_state.replay_call_summary (r, summary_end_state))
    op_ctxt.add_outcome
      (op_ctxt.get_next_intraprocedural_point (),
       new_state,
       true,
       nullptr,
       std::make_unique<call_summary_edge_info> (cd,
						 called_fn,
						 summary,
						 ext_state));
}

// class dump_op : public call_and_return_op

void
dump_op::execute (operation_context &op_ctxt) const
{
  const program_state &state = op_ctxt.get_initial_state ();
  switch (m_dump_kind)
    {
    default:
      gcc_unreachable ();
    case dump_kind::state:
      /* Handle the builtin "__analyzer_dump" by dumping state
	 to stderr.  */
      state.dump (op_ctxt.get_ext_state (), true);
      break;
    case dump_kind::sarif:
      state.dump_sarif (op_ctxt.get_ext_state ());
      break;
    case dump_kind::dot:
      state.dump_dot (op_ctxt.get_ext_state ());
      break;
    case dump_kind::state_2:
      {
	program_state dst_state (state);
	op_region_model_context ctxt (op_ctxt, dst_state);
	dst_state.impl_call_analyzer_dump_state (get_gcall (),
						 op_ctxt.get_ext_state (),
						 &ctxt);
      }
      break;
    }

  op_ctxt.add_outcome (op_ctxt.get_next_intraprocedural_point (),
		       state, false, nullptr);
}

// class setjmp_op : public call_and_return_op

void
setjmp_op::execute (operation_context &op_ctxt) const
{
  program_state dst_state (op_ctxt.get_initial_state ());
  op_region_model_context ctxt (op_ctxt, dst_state);
  dst_state.m_region_model->on_setjmp (get_gcall (),
				       op_ctxt.m_src_enode,
				       op_ctxt.m_sedge,
				       &ctxt);
  op_ctxt.add_outcome (op_ctxt.get_next_intraprocedural_point (),
		       dst_state, true, nullptr);
}

void
setjmp_op::add_any_events_for_eedge (const exploded_edge &eedge,
				     checker_path &out_path) const
{
  out_path.add_event
    (std::make_unique<setjmp_event>
       (event_loc_info (eedge.m_src),
	eedge.m_src,
	get_gcall ()));
}

// class longjmp_op : public call_and_return_op

void
longjmp_op::execute (operation_context &op_ctxt) const
{
  program_state dst_state (op_ctxt.get_initial_state ());
  op_region_model_context ctxt (op_ctxt, dst_state);
  op_ctxt.m_src_enode.on_longjmp (op_ctxt.m_eg, get_gcall (), &dst_state,
				  &ctxt);
}

// class cxa_throw_op : public call_and_return_op

void
cxa_throw_op::execute (operation_context &op_ctxt) const
{
  program_state dst_state (op_ctxt.get_initial_state ());
  op_region_model_context ctxt (op_ctxt, dst_state);
  program_point after_throw_point (op_ctxt.get_next_intraprocedural_point ());
  op_ctxt.m_src_enode.on_throw (op_ctxt.m_eg,
				get_gcall (),
				after_throw_point,
				&dst_state,
				m_is_rethrow,
				&ctxt);
  // We don't continue along op_ctxt's superedge
}

// class control_flow_op : public operation

void
control_flow_op::
walk_load_store_addr_ops (void *data,
			  walk_stmt_load_store_addr_fn load_cb,
			  walk_stmt_load_store_addr_fn store_cb,
			  walk_stmt_load_store_addr_fn addr_cb) const
{
  walk_stmt_load_store_addr_ops (const_cast <gimple *> (&m_ctrlflow_stmt),
				 data,
				 load_cb, store_cb, addr_cb);
}

void
control_flow_op::add_any_events_for_eedge (const exploded_edge &eedge,
					   checker_path &out_path) const
{
  out_path.add_event
    (std::make_unique<start_cfg_edge_event> (eedge,
					     event_loc_info (eedge.m_src),
					     this));
  out_path.add_event
     (std::make_unique<end_cfg_edge_event> (eedge,
					    event_loc_info (eedge.m_dest),
					    this));
}

/* Attempt to generate a description of any condition that holds at this edge.

   The intent is to make the user-facing messages more clear, especially for
   cases where there's a single or double-negative, such as
   when describing the false branch of an inverted condition.

   For example, rather than printing just:

      |  if (!ptr)
      |     ~
      |     |
      |     (1) following 'false' branch...

   it's clearer to spell out the condition that holds:

      |  if (!ptr)
      |     ~
      |     |
      |     (1) following 'false' branch (when 'ptr' is non-NULL)...
                                          ^^^^^^^^^^^^^^^^^^^^^^

   In the above example, this function would generate the highlighted
   string: "when 'ptr' is non-NULL".

   If the edge is not a condition, or it's not clear that a description of
   the condition would be helpful to the user, return NULL.  */

label_text
control_flow_op::maybe_describe_condition (bool ) const
{
  return label_text::borrow (nullptr);
}

void
control_flow_op::execute (operation_context &op_ctxt) const
{
  auto logger = op_ctxt.get_logger ();
  LOG_SCOPE (logger);

  program_state dst_state (op_ctxt.get_initial_state ());
  op_region_model_context ctxt (op_ctxt, dst_state);
  if (apply_constraints (&op_ctxt.m_sedge,
			 *dst_state.m_region_model,
			 &ctxt,
			 nullptr))
    {
      bool unknown_side_effects;
      handle_on_stmt_for_state_machines (op_ctxt,
					 dst_state,
					 nullptr,
					 unknown_side_effects,
					 m_ctrlflow_stmt);

      if (!ctxt.terminate_path_p ())
	{
	  auto dst_point (op_ctxt.get_next_intraprocedural_point ());
	  op_ctxt.add_outcome (dst_point, dst_state, false, nullptr);
	}
    }
}

bool
control_flow_op::
execute_for_feasibility (const exploded_edge &eedge,
			 feasibility_state &fstate,
			 region_model_context *ctxt,
			 std::unique_ptr<rejected_constraint> *out_rc) const
{
  gcc_assert (eedge.m_sedge);
  return apply_constraints (eedge.m_sedge,
			    fstate.get_model (),
			    ctxt,
			    out_rc);
}

// class gcond_edge_op : public control_flow_op

gcond_edge_op::gcond_edge_op (::edge cfg_edge,
			      const gcond &cond_stmt)
: control_flow_op (kind::cond_edge, cfg_edge, cond_stmt),
  m_true_value (get_flags () & EDGE_TRUE_VALUE)
{
  /* Exactly one of EDGE_TRUE_VALUE and EDGE_FALSE_VALUE must
     be set on CFG_EDGE.  */
  gcc_assert (static_cast<bool> (get_flags () & EDGE_TRUE_VALUE)
	      ^ static_cast<bool> (get_flags () & EDGE_FALSE_VALUE));
}

void
gcond_edge_op::print_as_edge_label (pretty_printer *pp,
				    bool user_facing) const
{
  if (!user_facing)
    pp_gimple_stmt_1 (pp, &get_ctrlflow_stmt (), 0, (dump_flags_t)0);

  if (m_true_value)
    pp_printf (pp, "true");
  else
    pp_printf (pp, "false");
}

label_text
gcond_edge_op::maybe_describe_condition (bool can_colorize) const
{
  const gcond &cond_stmt = get_gcond ();
  enum tree_code op = gimple_cond_code (&cond_stmt);
  tree lhs = gimple_cond_lhs (&cond_stmt);
  tree rhs = gimple_cond_rhs (&cond_stmt);
  if (!m_true_value)
    op = invert_tree_comparison (op, false /* honor_nans */);
  return maybe_describe_condition (can_colorize,
				   lhs, op, rhs);
}

/* Subroutine of gcond_edge_op::maybe_describe_condition above.

   Attempt to generate a user-facing description of the condition
   LHS OP RHS, but only if it is likely to make it easier for the
   user to understand a condition.  */

label_text
gcond_edge_op::maybe_describe_condition (bool can_colorize,
					 tree lhs,
					 enum tree_code op,
					 tree rhs)
{
  /* In theory we could just build a tree via
       fold_build2 (op, boolean_type_node, lhs, rhs)
     and print it with %qE on it, but this leads to warts such as
     parenthesizing vars, such as '(i) <= 9', and uses of '<unknown>'.  */

  /* Special-case: describe testing the result of strcmp, as figuring
     out what the "true" or "false" path is can be confusing to the user.  */
  if (TREE_CODE (lhs) == SSA_NAME
      && zerop (rhs))
    {
      if (gcall *call = dyn_cast <gcall *> (SSA_NAME_DEF_STMT (lhs)))
	if (is_special_named_call_p (*call, "strcmp", 2))
	  {
	    if (op == EQ_EXPR)
	      return label_text::borrow ("when the strings are equal");
	    if (op == NE_EXPR)
	      return label_text::borrow ("when the strings are non-equal");
	  }
    }

  /* Only attempt to generate text for sufficiently simple expressions.  */
  if (!should_print_expr_p (lhs))
    return label_text::borrow (nullptr);
  if (!should_print_expr_p (rhs))
    return label_text::borrow (nullptr);

  /* Special cases for pointer comparisons against NULL.  */
  if (POINTER_TYPE_P (TREE_TYPE (lhs))
      && POINTER_TYPE_P (TREE_TYPE (rhs))
      && zerop (rhs))
    {
      if (op == EQ_EXPR)
	return make_label_text (can_colorize, "when %qE is NULL",
				lhs);
      if (op == NE_EXPR)
	return make_label_text (can_colorize, "when %qE is non-NULL",
				lhs);
    }

  return make_label_text (can_colorize, "when %<%E %s %E%>",
			  lhs, op_symbol_code (op), rhs);
}

/* Subroutine of maybe_describe_condition.

   Return true if EXPR is we will get suitable user-facing output
   from %E on it.  */

bool
gcond_edge_op::should_print_expr_p (tree expr)
{
  if (TREE_CODE (expr) == SSA_NAME)
    {
      if (SSA_NAME_VAR (expr))
	return should_print_expr_p (SSA_NAME_VAR (expr));
      else
	return false;
    }

  if (DECL_P (expr))
    return true;

  if (CONSTANT_CLASS_P (expr))
    return true;

  return false;
}

bool
gcond_edge_op::
apply_constraints (const superedge *,
		   region_model &model,
		   region_model_context *ctxt,
		   std::unique_ptr<rejected_constraint> *out) const
{
  const gcond &cond_stmt = get_gcond ();
  enum tree_code op = gimple_cond_code (&cond_stmt);
  tree lhs = gimple_cond_lhs (&cond_stmt);
  tree rhs = gimple_cond_rhs (&cond_stmt);
  if (!m_true_value)
    op = invert_tree_comparison (op, false /* honor_nans */);
  return model.add_constraint (lhs, op, rhs, ctxt, out);
}

// class ggoto_edge_op : public control_flow_op

ggoto_edge_op::ggoto_edge_op (::edge cfg_edge,
			      const ggoto &goto_stmt,
			      tree dst_label)
: control_flow_op (kind::goto_edge, cfg_edge, goto_stmt),
  m_dst_label (dst_label)
{
}

void
ggoto_edge_op::print_as_edge_label (pretty_printer *pp,
				    bool user_facing) const
{
  if (!user_facing)
    pp_gimple_stmt_1 (pp, &get_ctrlflow_stmt (), 0, (dump_flags_t)0);

  if (m_dst_label)
    pp_printf (pp, "%qD", m_dst_label);
}

label_text
ggoto_edge_op::maybe_describe_condition (bool) const
{
  return label_text::borrow ("");
}

bool
ggoto_edge_op::
apply_constraints (const superedge *,
		   region_model &model,
		   region_model_context *ctxt,
		   std::unique_ptr<rejected_constraint> */*out_rc*/) const
{
  const ggoto &goto_stmt = get_ggoto ();
  tree dest = gimple_goto_dest (&goto_stmt);
  const svalue *dest_sval = model.get_rvalue (dest, ctxt);

  /* If we know we were jumping to a specific label.  */
  if (m_dst_label)
    {
      auto mgr = model.get_manager ();
      const label_region *dst_label_reg
	= mgr->get_region_for_label (m_dst_label);
      const svalue *dst_label_ptr
	= mgr->get_ptr_svalue (ptr_type_node, dst_label_reg);

      if (!model.add_constraint (dest_sval, EQ_EXPR, dst_label_ptr, ctxt))
	return false;
    }

  return true;
}

// class switch_case_op : public control_flow_op

switch_case_op::switch_case_op (function &fun,
				::edge cfg_edge,
				const gswitch &switch_stmt,
				bounded_ranges_manager &mgr)
: control_flow_op (kind::switch_edge, cfg_edge, switch_stmt)
{
  /* Populate m_case_labels with all cases which go to DST.  */
  for (unsigned i = 0; i < gimple_switch_num_labels (&switch_stmt); i++)
    {
      tree case_ = gimple_switch_label (&switch_stmt, i);
      basic_block bb = label_to_block (&fun,
				       CASE_LABEL (case_));
      if (bb == cfg_edge->dest)
	m_case_labels.push_back (case_);
    }

  auto_vec <const bounded_ranges *> case_ranges_vec
    (gimple_switch_num_labels (&switch_stmt));
  for (auto case_label : m_case_labels)
    {
      /* Get the ranges for this case label.  */
      const bounded_ranges *case_ranges
	= mgr.make_case_label_ranges (&switch_stmt, case_label);
      case_ranges_vec.quick_push (case_ranges);
    }

  m_all_cases_ranges = mgr.get_or_create_union (case_ranges_vec);
}

/* Print "case VAL:", "case LOWER ... UPPER:", or "default:" to PP.  */

void
switch_case_op::print_as_edge_label (pretty_printer *pp,
				     bool user_facing) const
{
  if (user_facing)
    {
      for (unsigned i = 0; i < m_case_labels.size (); ++i)
	{
	  if (i > 0)
	    pp_string (pp, ", ");
	  tree case_label = m_case_labels[i];
	  gcc_assert (TREE_CODE (case_label) == CASE_LABEL_EXPR);
	  tree lower_bound = CASE_LOW (case_label);
	  tree upper_bound = CASE_HIGH (case_label);
	  if (lower_bound)
	    {
	      pp_printf (pp, "case ");
	      dump_generic_node (pp, lower_bound, 0, (dump_flags_t)0, false);
	      if (upper_bound)
		{
		  pp_printf (pp, " ... ");
		  dump_generic_node (pp, upper_bound, 0, (dump_flags_t)0,
				     false);
		}
	      pp_printf (pp, ":");
	    }
	  else
	    pp_printf (pp, "default:");
	}
    }
  else
    {
      pp_character (pp, '{');
      for (unsigned i = 0; i < m_case_labels.size (); ++i)
	{
	  if (i > 0)
	    pp_string (pp, ", ");
	  tree case_label = m_case_labels[i];
	  gcc_assert (TREE_CODE (case_label) == CASE_LABEL_EXPR);
	  tree lower_bound = CASE_LOW (case_label);
	  tree upper_bound = CASE_HIGH (case_label);
	  if (lower_bound)
	    {
	      if (upper_bound)
		{
		  pp_character (pp, '[');
		  dump_generic_node (pp, lower_bound, 0, (dump_flags_t)0,
				     false);
		  pp_string (pp, ", ");
		  dump_generic_node (pp, upper_bound, 0, (dump_flags_t)0,
				     false);
		  pp_character (pp, ']');
		}
	      else
		dump_generic_node (pp, lower_bound, 0, (dump_flags_t)0, false);
	    }
	  else
	    pp_printf (pp, "default");
	}
      pp_character (pp, '}');
      if (implicitly_created_default_p ())
	{
	  pp_string (pp, " IMPLICITLY CREATED");
	}
    }
}

/* Return true iff SWITCH_STMT has a non-default label that contains
   INT_CST.  */

static bool
has_nondefault_case_for_value_p (const gswitch *switch_stmt, tree int_cst)
{
  /* We expect the initial label to be the default; skip it.  */
  gcc_assert (CASE_LOW (gimple_switch_label (switch_stmt, 0)) == NULL_TREE);
  unsigned min_idx = 1;
  unsigned max_idx = gimple_switch_num_labels (switch_stmt) - 1;

  /* Binary search: try to find the label containing INT_CST.
     This requires the cases to be sorted by CASE_LOW (done by the
     gimplifier).  */
  while (max_idx >= min_idx)
    {
      unsigned case_idx = (min_idx + max_idx) / 2;
      tree label =  gimple_switch_label (switch_stmt, case_idx);
      tree low = CASE_LOW (label);
      gcc_assert (low);
      tree high = CASE_HIGH (label);
      if (!high)
	high = low;
      if (tree_int_cst_compare (int_cst, low) < 0)
	{
	  /* INT_CST is below the range of this label.  */
	  gcc_assert (case_idx > 0);
	  max_idx = case_idx - 1;
	}
      else if (tree_int_cst_compare (int_cst, high) > 0)
	{
	  /* INT_CST is above the range of this case.  */
	  min_idx = case_idx + 1;
	}
      else
	/* This case contains INT_CST.  */
	return true;
    }
  /* Not found.  */
  return false;
}

/* Return true iff SWITCH_STMT (which must be on an enum value)
   has nondefault cases handling all values in the enum.  */

static bool
has_nondefault_cases_for_all_enum_values_p (const gswitch *switch_stmt,
					    tree type)
{
  gcc_assert (switch_stmt);
  gcc_assert (TREE_CODE (type) == ENUMERAL_TYPE);

  for (tree enum_val_iter = TYPE_VALUES (type);
       enum_val_iter;
       enum_val_iter = TREE_CHAIN (enum_val_iter))
    {
      tree enum_val = TREE_VALUE (enum_val_iter);
      gcc_assert (TREE_CODE (enum_val) == CONST_DECL);
      gcc_assert (TREE_CODE (DECL_INITIAL (enum_val)) == INTEGER_CST);
      if (!has_nondefault_case_for_value_p (switch_stmt,
					    DECL_INITIAL (enum_val)))
	return false;
    }
  return true;
}

/* Given an EDGE guarded by SWITCH_STMT, determine appropriate constraints
   for the edge to be taken.

   If they are feasible, add the constraints and return true.

   Return false if the constraints contradict existing knowledge
   (and so the edge should not be taken).
   When returning false, if OUT is non-NULL, write a new rejected_constraint
   to it.  */

bool
switch_case_op::
apply_constraints (const superedge *,
		   region_model &model,
		   region_model_context *ctxt,
		   std::unique_ptr<rejected_constraint> *out) const
{
  const gswitch *switch_stmt = &get_gswitch ();
  tree index  = gimple_switch_index (switch_stmt);
  const svalue *index_sval = model.get_rvalue (index, ctxt);
  bool check_index_type = true;

  /* With -fshort-enum, there may be a type cast.  */
  if (ctxt && index_sval->get_kind () == SK_UNARYOP
      && TREE_CODE (index_sval->get_type ()) == INTEGER_TYPE)
    {
      const unaryop_svalue *unaryop = as_a <const unaryop_svalue *> (index_sval);
      if (unaryop->get_op () == NOP_EXPR
	  && is_a <const initial_svalue *> (unaryop->get_arg ()))
	if (const initial_svalue *initvalop = (as_a <const initial_svalue *>
					       (unaryop->get_arg ())))
	  if (initvalop->get_type ()
	      && TREE_CODE (initvalop->get_type ()) == ENUMERAL_TYPE)
	    {
	      index_sval = initvalop;
	      check_index_type = false;
	    }
    }

  /* If we're switching based on an enum type, assume that the user is only
     working with values from the enum.  Hence if this is an
     implicitly-created "default", assume it doesn't get followed.
     This fixes numerous "uninitialized" false positives where we otherwise
     consider jumping past the initialization cases.  */

  if (/* Don't check during feasibility-checking (when ctxt is NULL).  */
      ctxt
      /* Must be an enum value.  */
      && index_sval->get_type ()
      && (!check_index_type
	  || TREE_CODE (TREE_TYPE (index)) == ENUMERAL_TYPE)
      && TREE_CODE (index_sval->get_type ()) == ENUMERAL_TYPE
      /* If we have a constant, then we can check it directly.  */
      && index_sval->get_kind () != SK_CONSTANT
      && implicitly_created_default_p ()
      && has_nondefault_cases_for_all_enum_values_p (switch_stmt,
						     index_sval->get_type ())
      /* Don't do this if there's a chance that the index is
	 attacker-controlled.  */
      && !ctxt->possibly_tainted_p (index_sval))
    {
      if (out)
	*out = std::make_unique <rejected_default_case> (model);
      return false;
    }

  bool sat
    = model.get_constraints ()->add_bounded_ranges (index_sval,
						    m_all_cases_ranges);
  if (!sat && out)
    *out = std::make_unique <rejected_ranges_constraint>
      (model, index, m_all_cases_ranges);
  if (sat && ctxt && !m_all_cases_ranges->empty_p ())
    ctxt->on_bounded_ranges (*index_sval, *m_all_cases_ranges);
  return sat;
}

/* Return true iff this op's edge is purely for an
   implicitly-created "default".  */

bool
switch_case_op::implicitly_created_default_p () const
{
  if (m_case_labels.size () != 1)
    return false;

  tree case_label = m_case_labels[0];
  gcc_assert (TREE_CODE (case_label) == CASE_LABEL_EXPR);
  if (CASE_LOW (case_label))
    return false;

  /* We have a single "default" case.
     Assume that it was implicitly created if it has UNKNOWN_LOCATION.  */
  return EXPR_LOCATION (case_label) == UNKNOWN_LOCATION;
}

/* Given an ERT_TRY region, get the eh_catch corresponding to
   the label of DST_SNODE, if any.  */

static eh_catch
get_catch (eh_region eh_reg, supernode *dst_snode)
{
  gcc_assert (eh_reg->type == ERT_TRY);

  tree dst_snode_label = dst_snode->get_label ();
  if (!dst_snode_label)
    return nullptr;

  for (eh_catch iter = eh_reg->u.eh_try.first_catch;
       iter;
       iter = iter->next_catch)
    if (iter->label == dst_snode_label)
      return iter;

  return nullptr;
}

class rejected_eh_dispatch : public rejected_constraint
{
public:
  rejected_eh_dispatch (const region_model &model)
  : rejected_constraint (model)
  {}

  void dump_to_pp (pretty_printer *pp) const final override
  {
    pp_printf (pp, "rejected_eh_dispatch");
  }
};

static bool
exception_matches_type_p (tree exception_type,
			  tree catch_type)
{
  if (catch_type == exception_type)
    return true;

  /* TODO (PR analyzer/119697): we should also handle subclasses etc;
     see the rules in https://en.cppreference.com/w/cpp/language/catch

     It looks like we should be calling (or emulating)
     can_convert_eh from the C++ FE, but that's specific to the C++ FE.  */

  return false;
}

static bool
matches_any_exception_type_p (eh_catch ehc, tree exception_type)
{
  if (ehc->type_list == NULL_TREE)
    /* All exceptions are caught here.  */
    return true;

  for (tree iter = ehc->type_list; iter; iter = TREE_CHAIN (iter))
    if (exception_matches_type_p (TREE_VALUE (iter),
				  exception_type))
      return true;
  return false;
}

// class eh_dispatch_edge_op : public control_flow_op

std::unique_ptr<eh_dispatch_edge_op>
eh_dispatch_edge_op::make (supernode *src_snode,
			   supernode *dst_snode,
			   ::edge cfg_edge,
			   const geh_dispatch &eh_dispatch_stmt)
{
  const eh_status *eh = src_snode->get_function ()->eh;
  gcc_assert (eh);
  int region_idx = gimple_eh_dispatch_region (&eh_dispatch_stmt);
  gcc_assert (region_idx > 0);
  gcc_assert ((*eh->region_array)[region_idx]);
  eh_region eh_reg = (*eh->region_array)[region_idx];
  gcc_assert (eh_reg);
  switch (eh_reg->type)
    {
    default:
      gcc_unreachable ();
    case ERT_CLEANUP:
      // TODO
      gcc_unreachable ();
      break;
    case ERT_TRY:
      {
	eh_catch ehc = get_catch (eh_reg, dst_snode);
	return std::make_unique<eh_dispatch_try_edge_op>
	  (src_snode, dst_snode,
	   cfg_edge, eh_dispatch_stmt,
	   eh_reg, ehc);
      }
      break;
    case ERT_ALLOWED_EXCEPTIONS:
      return std::make_unique<eh_dispatch_allowed_edge_op>
	(src_snode, dst_snode,
	 cfg_edge, eh_dispatch_stmt,
	 eh_reg);
      break;
    case ERT_MUST_NOT_THROW:
      // TODO
      gcc_unreachable ();
      break;
    }
}

eh_dispatch_edge_op::
eh_dispatch_edge_op (supernode *src_snode,
		     supernode *dst_snode,
		     enum kind kind_,
		     ::edge cfg_edge,
		     const geh_dispatch &geh_dispatch_stmt,
		     eh_region eh_reg)
: control_flow_op (kind_, cfg_edge, geh_dispatch_stmt),
  m_src_snode (src_snode),
  m_dst_snode (dst_snode),
  m_eh_region (eh_reg)
{
}

bool
eh_dispatch_edge_op::
apply_constraints (const superedge *sedge,
		   region_model &model,
		   region_model_context *ctxt,
		   std::unique_ptr<rejected_constraint> *out) const
{
  const exception_node *current_node = model.get_current_thrown_exception ();

  if (!current_node)
    return false;

  gcc_assert (current_node);
  tree curr_exception_type = current_node->maybe_get_type ();
  if (!curr_exception_type)
    /* We don't know the specific type.  */
    return true;

  return apply_eh_constraints (sedge, model, ctxt, curr_exception_type, out);
}

// class eh_dispatch_try_edge_op : public eh_dispatch_edge_op

eh_dispatch_try_edge_op::
eh_dispatch_try_edge_op (supernode *src_snode,
			 supernode *dst_snode,
			 ::edge cfg_edge,
			 const geh_dispatch &geh_dispatch_stmt,
			 eh_region eh_reg,
			 eh_catch ehc)
: eh_dispatch_edge_op (src_snode, dst_snode,
		       kind::eh_dispatch_try_edge,
		       cfg_edge, geh_dispatch_stmt, eh_reg),
  m_eh_catch (ehc)
{
  gcc_assert (eh_reg->type == ERT_TRY);
}

void
eh_dispatch_try_edge_op::print_as_edge_label (pretty_printer *pp,
					      bool user_facing) const
{
  if (!user_facing)
    pp_string (pp, "ERT_TRY: ");
  if (m_eh_catch)
    {
      bool first = true;
      for (tree iter = m_eh_catch->type_list; iter; iter = TREE_CHAIN (iter))
	{
	  if (!first)
	    pp_string (pp, ", ");
	  pp_printf (pp, "on catch %qT", TREE_VALUE (iter));
	  first = false;
	}
    }
  else
    pp_string (pp, "on uncaught exception");
}

void
eh_dispatch_try_edge_op::add_any_events_for_eedge (const exploded_edge &eedge,
						   checker_path &out_path) const
{
  if (m_eh_catch)
    {
      const region_model *model = eedge.m_src->get_state ().m_region_model;
      auto curr_thrown_exception_node
	= model->get_current_thrown_exception ();
      gcc_assert (curr_thrown_exception_node);
      tree type = curr_thrown_exception_node->maybe_get_type ();
      out_path.add_event
	(std::make_unique<catch_cfg_edge_event>
	 (eedge,
	  event_loc_info (eedge.m_dest),
	  *this,
	  type));
    }
  else
    {
      /* We have the "uncaught exception" sedge, from eh_dispatch
	 to a block containing resx.
	 Don't add any events for this, so that we can consolidate
	 adjacent stack unwinding events.  */
    }
}

bool
eh_dispatch_try_edge_op::
apply_eh_constraints (const superedge *sedge,
		      region_model &model,
		      region_model_context */*ctxt*/,
		      tree exception_type,
		      std::unique_ptr<rejected_constraint> *out) const
{
  /* TODO: can we rely on this ordering?
     or do we need to iterate through prev_catch ?  */
  /* The exception must not match any of the previous edges.  */
  for (auto sibling_sedge : get_src_snode ()->m_succs)
    {
      if (sibling_sedge == sedge)
	break;

      const eh_dispatch_try_edge_op *sibling_edge_op
	= (const eh_dispatch_try_edge_op *)sibling_sedge->get_op ();
      if (eh_catch ehc = sibling_edge_op->m_eh_catch)
	if (matches_any_exception_type_p (ehc, exception_type))
	  {
	    /* The earlier sibling matches, so the "unhandled" edge is
	       not taken.  */
	    if (out)
	      *out = std::make_unique<rejected_eh_dispatch> (model);
	    return false;
	  }
    }

  if (eh_catch ehc = m_eh_catch)
    {
      /* We have an edge that tried to match one or more types.  */

      /* The exception must not match any of the previous edges.  */

      /* It must match this type.  */
      if (matches_any_exception_type_p (ehc, exception_type))
	return true;
      else
	{
	  /* Exception type doesn't match.  */
	  if (out)
	    *out = std::make_unique<rejected_eh_dispatch> (model);
	  return false;
	}
    }
  else
    {
      /* This is the "unhandled exception" edge.
	 If we get here then no sibling edges matched;
	 we will follow this edge.  */
      return true;
    }
}

// class eh_dispatch_allowed_edge_op : public eh_dispatch_edge_op

eh_dispatch_allowed_edge_op::
eh_dispatch_allowed_edge_op (supernode *src_snode,
			     supernode *dst_snode,
			     ::edge cfg_edge,
			     const geh_dispatch &geh_dispatch_stmt,
			     eh_region eh_reg)
: eh_dispatch_edge_op (src_snode, dst_snode,
		       kind::eh_dispatch_try_edge,
		       cfg_edge, geh_dispatch_stmt, eh_reg)
{
  gcc_assert (eh_reg->type == ERT_ALLOWED_EXCEPTIONS);

  /* We expect two sibling out-edges at an eh_dispatch from such a region:

     - one to a bb without a gimple label, with a resx,
     for exceptions of expected types

     - one to a bb with a gimple label, with a call to __cxa_unexpected,
     for exceptions of unexpected types.

     Set m_kind for this edge accordingly.  */
  gcc_assert (cfg_edge->src->succs->length () == 2);
  tree label_for_unexpected_exceptions = eh_reg->u.allowed.label;
  tree label_for_dest_enode = dst_snode->get_label ();
  if (label_for_dest_enode == label_for_unexpected_exceptions)
    m_kind = eh_kind::unexpected;
  else
    {
      gcc_assert (label_for_dest_enode == nullptr);
      m_kind = eh_kind::expected;
    }
}

void
eh_dispatch_allowed_edge_op::print_as_edge_label (pretty_printer *pp,
						  bool user_facing) const
{
  if (!user_facing)
    {
      switch (m_kind)
	{
	default:
	  gcc_unreachable ();
	case eh_kind::expected:
	  pp_string (pp, "expected: ");
	  break;
	case eh_kind::unexpected:
	  pp_string (pp, "unexpected: ");
	  break;
	}
      pp_string (pp, "ERT_ALLOWED_EXCEPTIONS: ");
      eh_region eh_reg = get_eh_region ();
      bool first = true;
      for (tree iter = eh_reg->u.allowed.type_list; iter;
	   iter = TREE_CHAIN (iter))
	{
	  if (!first)
	    pp_string (pp, ", ");
	  pp_printf (pp, "%qT", TREE_VALUE (iter));
	  first = false;
	}
    }
}

bool
eh_dispatch_allowed_edge_op::
apply_eh_constraints (const superedge *,
		      region_model &model,
		      region_model_context */*ctxt*/,
		      tree exception_type,
		      std::unique_ptr<rejected_constraint> *out) const
{
  auto curr_thrown_exception_node = model.get_current_thrown_exception ();
  gcc_assert (curr_thrown_exception_node);
  tree curr_exception_type = curr_thrown_exception_node->maybe_get_type ();
  eh_region eh_reg = get_eh_region ();
  tree type_list = eh_reg->u.allowed.type_list;

  switch (get_eh_kind ())
    {
    default:
      gcc_unreachable ();
    case eh_kind::expected:
      if (!curr_exception_type)
	{
	  /* We don't know the specific type;
	     assume we have one of an expected type.  */
	  return true;
	}
      for (tree iter = type_list; iter; iter = TREE_CHAIN (iter))
	if (exception_matches_type_p (TREE_VALUE (iter),
				      exception_type))
	  return true;
      if (out)
	*out = std::make_unique<rejected_eh_dispatch> (model);
      return false;

    case eh_kind::unexpected:
      if (!curr_exception_type)
	{
	  /* We don't know the specific type;
	     assume we don't have one of an expected type.  */
	  if (out)
	    *out = std::make_unique<rejected_eh_dispatch> (model);
	  return false;
	}
      for (tree iter = type_list; iter; iter = TREE_CHAIN (iter))
	if (exception_matches_type_p (TREE_VALUE (iter),
				      exception_type))
	  {
	    if (out)
	      *out = std::make_unique<rejected_eh_dispatch> (model);
	    return false;
	  }
      return true;
    }
}

// class phis_for_edge_op : public operation

std::unique_ptr<operation>
phis_for_edge_op::maybe_make (::edge cfg_in_edge)
{
  std::vector<pair> pairs = get_pairs_for_phi_along_in_edge (cfg_in_edge);
  if (pairs.empty ())
    return nullptr;

  return std::make_unique <phis_for_edge_op> (std::move (pairs),
					      cfg_in_edge);
}

phis_for_edge_op::phis_for_edge_op (std::vector<pair> &&pairs,
				    ::edge cfg_in_edge)
: operation (kind::phis),
  m_pairs (std::move (pairs)),
  m_cfg_in_edge (cfg_in_edge)
{
}

std::vector<phis_for_edge_op::pair>
phis_for_edge_op::get_pairs_for_phi_along_in_edge (::edge cfg_in_edge)
{
  std::vector<pair> result;

  const size_t phi_arg_idx = cfg_in_edge->dest_idx;
  for (gphi_iterator gpi = gsi_start_phis (cfg_in_edge->dest);
       !gsi_end_p (gpi); gsi_next (&gpi))
    {
      gphi * const phi = gpi.phi ();
      tree dst = gimple_phi_result (phi);

      /* We don't bother tracking the .MEM SSA names.  */
      if (tree var = SSA_NAME_VAR (dst))
	if (TREE_CODE (var) == VAR_DECL)
	  if (VAR_DECL_IS_VIRTUAL_OPERAND (var))
	    continue;

      tree src = gimple_phi_arg_def (phi, phi_arg_idx);

      result.push_back ({dst, src});
    }

  return result;
}

void
phis_for_edge_op::print_as_edge_label (pretty_printer *pp,
				       bool ) const
{
  pp_printf (pp, "PHI(");
  bool first = true;
  for (auto &p : m_pairs)
    {
      if (first)
	first = false;
      else
	pp_string (pp, ", ");

      pp_printf (pp, "%E = %E", p.m_dst, p.m_src);
    }
  pp_printf (pp, ");");
}

void
phis_for_edge_op::
walk_load_store_addr_ops (void */*data*/ ,
			  walk_stmt_load_store_addr_fn /*load_cb*/,
			  walk_stmt_load_store_addr_fn /*store_cb*/,
			  walk_stmt_load_store_addr_fn /*addr_cb*/) const
{
}

bool
phis_for_edge_op::defines_ssa_name_p (const_tree ssa_name) const
{
  for (auto &p : m_pairs)
    if (p.m_dst == ssa_name)
      return true;
  return false;
}

void
phis_for_edge_op::execute (operation_context &op_ctxt) const
{
  auto logger = op_ctxt.get_logger ();
  LOG_SCOPE (logger);

  auto dst_point (op_ctxt.get_next_intraprocedural_point ());

  const program_state &src_state (op_ctxt.get_initial_state ());
  program_state dst_state (src_state);

  impl_path_context path_ctxt (&dst_state, logger);
  uncertainty_t uncertainty;
  impl_region_model_context ctxt (op_ctxt.m_eg,
				  &op_ctxt.m_src_enode,

				  /* TODO: should we be getting the ECs from the
				     old state, rather than the new?  */
				  &op_ctxt.get_initial_state (),
				  &dst_state,
				  &uncertainty,
				  &path_ctxt,
				  nullptr,
				  nullptr);

  update_state (src_state, dst_state, &ctxt);

  op_ctxt.add_outcome (dst_point, dst_state, false, &uncertainty);
}

void
phis_for_edge_op::update_state (const program_state &src_state,
				program_state &dst_state,
				region_model_context *ctxt) const
{
  const region_model &src_model = *src_state.m_region_model;
  region_model &dst_model = *dst_state.m_region_model;

  hash_set<const svalue *> svals_changing_meaning;

  /* Get state from src_state so that all of the phi stmts for an edge
     are effectively handled simultaneously.  */
  for (auto &p : m_pairs)
    {
      const svalue *src_sval = src_model.get_rvalue (p.m_src, nullptr);
      const region *dst_reg = src_model.get_lvalue (p.m_dst, nullptr);

      const svalue *old_sval = src_model.get_rvalue (p.m_dst, nullptr);
      if (old_sval->get_kind () == SK_WIDENING)
	svals_changing_meaning.add (old_sval);

      dst_model.set_value (dst_reg, src_sval, ctxt);
    }

 for (auto iter : svals_changing_meaning)
   dst_model.get_constraints ()->purge_state_involving (iter);
}

bool
phis_for_edge_op::
execute_for_feasibility (const exploded_edge &eedge,
			 feasibility_state &fstate,
			 region_model_context *ctxt,
			 std::unique_ptr<rejected_constraint> */*out_rc*/) const
{
  hash_set<const svalue *> svals_changing_meaning;
  /* Get state from src_state so that all of the phi stmts for an edge
     are effectively handled simultaneously.  */
  region_model &model = fstate.get_model ();
  region_model src_model (model);
  for (auto &p : m_pairs)
    {
      const svalue *src_sval = src_model.get_rvalue (p.m_src, ctxt);
      const region *dst_reg = model.get_lvalue (p.m_dst, ctxt);

      const svalue *sval = model.get_rvalue (p.m_dst, ctxt);
      if (sval->get_kind () == SK_WIDENING)
	svals_changing_meaning.add (sval);

      model.set_value (dst_reg, src_sval, ctxt);
    }

  for (auto iter : svals_changing_meaning)
    model.get_constraints ()->purge_state_involving (iter);

  {
    /* If we've entering an snode that we've already visited on this
       epath, then we need do fix things up for loops; see the
       comment for store::loop_replay_fixup.
       Perhaps we should probably also verify the callstring,
       and track program_points,  but hopefully doing it by supernode
       is good enough.  */
    const exploded_node &dst_enode = *eedge.m_dest;
    const unsigned dst_snode_idx = dst_enode.get_supernode ()->m_id;
    if (bitmap_bit_p (fstate.get_snodes_visited (), dst_snode_idx))
      model.loop_replay_fixup (dst_enode.get_state ().m_region_model);
  }
 
  return true;
}

void
phis_for_edge_op::
update_state_for_bulk_merger (const program_state &src_state,
			      program_state &dst_state) const
{
  update_state (src_state, dst_state, nullptr);
}

void
phis_for_edge_op::add_any_events_for_eedge (const exploded_edge &,
					    checker_path &) const
{
  // No-op
}

// class resx_op : public gimple_stmt_op

void
resx_op::execute (operation_context &op_ctxt) const
{
  auto logger = op_ctxt.get_logger ();
  LOG_SCOPE (logger);

  program_point dst_point (op_ctxt.get_next_intraprocedural_point ());
  program_state dst_state (op_ctxt.get_initial_state ());
  op_region_model_context ctxt (op_ctxt, dst_state);

  if (exploded_node *dst_enode
      = op_ctxt.m_eg.get_or_create_node (dst_point, dst_state,
					 &op_ctxt.m_src_enode,
					 // Don't add to worklist:
					 false))
    {
      op_ctxt.m_eg.add_edge (&op_ctxt.m_src_enode,
			     dst_enode,
			     &op_ctxt.m_sedge,
			     false,
			     nullptr);
      /* Try to adding eedges and enodes that unwind to the next
	 eh_dispatch statement, if any.
	 Only the final enode is added to the worklist.  */
      op_ctxt.m_eg.unwind_from_exception (*dst_enode,
					  nullptr,
					  &ctxt);
    }
}

void
resx_op::add_any_events_for_eedge (const exploded_edge &,
				   checker_path &) const
{
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
