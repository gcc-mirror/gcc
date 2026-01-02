/* Concrete implementation of sm_context.
   Copyright (C) 2019-2026 Free Software Foundation, Inc.
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

#ifndef GCC_ANALYZER_IMPL_SM_CONTEXT
#define GCC_ANALYZER_IMPL_SM_CONTEXT

namespace ana {

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
		   bool unknown_side_effects = false)
  : sm_context (sm_idx, sm),
    m_logger (eg.get_logger ()),
    m_eg (eg), m_enode_for_diag (enode_for_diag),
    m_old_state (old_state), m_new_state (new_state),
    m_old_smap (old_smap), m_new_smap (new_smap),
    m_path_ctxt (path_ctxt),
    m_unknown_side_effects (unknown_side_effects)
  {
  }

  logger *get_logger () const { return m_logger.get_logger (); }

  tree get_fndecl_for_call (const gcall &call) final override
  {
    impl_region_model_context old_ctxt
      (m_eg, m_enode_for_diag, nullptr, nullptr, nullptr/*m_enode->get_state ()*/,
       nullptr, &call);
    region_model *model = m_new_state->m_region_model;
    return model->get_fndecl_for_call (call, &old_ctxt);
  }

  state_machine::state_t get_state (tree var) final override
  {
    logger * const logger = get_logger ();
    LOG_FUNC (logger);
    /* Use nullptr ctxt on this get_rvalue call to avoid triggering
       uninitialized value warnings.  */
    const svalue *var_old_sval
      = m_old_state->m_region_model->get_rvalue (var, nullptr);

    state_machine::state_t current
      = m_old_smap->get_state (var_old_sval, m_eg.get_ext_state ());
    return current;
  }
  state_machine::state_t get_state (const svalue *sval) final override
  {
    logger * const logger = get_logger ();
    LOG_FUNC (logger);
    state_machine::state_t current
      = m_old_smap->get_state (sval, m_eg.get_ext_state ());
    return current;
  }


  void set_next_state (tree var,
		       state_machine::state_t to,
		       tree origin) final override
  {
    logger * const logger = get_logger ();
    LOG_FUNC (logger);
    const svalue *var_new_sval
      = m_new_state->m_region_model->get_rvalue (var, nullptr);
    const svalue *origin_new_sval
      = m_new_state->m_region_model->get_rvalue (origin, nullptr);

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

  void set_next_state (const svalue *sval,
		       state_machine::state_t to,
		       tree origin) final override
  {
    logger * const logger = get_logger ();
    LOG_FUNC (logger);
    impl_region_model_context old_ctxt
      (m_eg, m_enode_for_diag, nullptr, nullptr, nullptr/*m_enode->get_state ()*/,
       nullptr, nullptr);

    const svalue *origin_new_sval
      = m_new_state->m_region_model->get_rvalue (origin, nullptr);

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

  void warn (tree var,
	     std::unique_ptr<pending_diagnostic> d) final override
  {
    LOG_FUNC (get_logger ());
    gcc_assert (d);
    const svalue *var_old_sval
      = m_old_state->m_region_model->get_rvalue (var, nullptr);
    state_machine::state_t current
      = (var
	 ? m_old_smap->get_state (var_old_sval, m_eg.get_ext_state ())
	 : m_old_smap->get_global_state ());
    bool terminate_path = d->terminate_path_p ();
    pending_location ploc (m_enode_for_diag);
    m_eg.get_diagnostic_manager ().add_diagnostic
      (&m_sm, std::move (ploc),
       var, var_old_sval, current, std::move (d));
    if (m_path_ctxt
	&& terminate_path
	&& flag_analyzer_suppress_followups)
      m_path_ctxt->terminate_path ();
  }

  void warn (const svalue *sval,
	     std::unique_ptr<pending_diagnostic> d) final override
  {
    LOG_FUNC (get_logger ());
    gcc_assert (d);
    state_machine::state_t current
      = (sval
	 ? m_old_smap->get_state (sval, m_eg.get_ext_state ())
	 : m_old_smap->get_global_state ());
    bool terminate_path = d->terminate_path_p ();
    pending_location ploc (m_enode_for_diag);
    m_eg.get_diagnostic_manager ().add_diagnostic
      (&m_sm, std::move (ploc),
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
    const svalue *sval = m_new_state->m_region_model->get_rvalue (expr, nullptr);
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
      (m_eg, m_enode_for_diag, m_old_state, m_new_state, nullptr, nullptr, stmt);
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

  location_t get_emission_location () const final override
  {
    return pending_location (m_enode_for_diag).get_location ();
  }

  log_user m_logger;
  exploded_graph &m_eg;
  exploded_node *m_enode_for_diag;
  const program_state *m_old_state;
  program_state *m_new_state;
  const sm_state_map *m_old_smap;
  sm_state_map *m_new_smap;
  path_context *m_path_ctxt;

  /* Are we handling an external function with unknown side effects?  */
  bool m_unknown_side_effects;
};

} // namespace ana

#endif /* GCC_ANALYZER_IMPL_SM_CONTEXT */
