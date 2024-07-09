/* Detection of infinite recursion.
   Copyright (C) 2022-2024 Free Software Foundation, Inc.
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
#define INCLUDE_MEMORY
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
#include "json.h"
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
#include "make-unique.h"
#include "analyzer/checker-path.h"
#include "analyzer/feasible-graph.h"
#include "diagnostic-format-sarif.h"

/* A subclass of pending_diagnostic for complaining about suspected
   infinite recursion.  */

class infinite_recursion_diagnostic
: public pending_diagnostic_subclass<infinite_recursion_diagnostic>
{
public:
  infinite_recursion_diagnostic (const exploded_node *prev_entry_enode,
				 const exploded_node *new_entry_enode,
				 tree callee_fndecl)
  : m_prev_entry_enode (prev_entry_enode),
    m_new_entry_enode (new_entry_enode),
    m_callee_fndecl (callee_fndecl),
    m_prev_entry_event (NULL)
  {}

  const char *get_kind () const final override
  {
    return "infinite_recursion_diagnostic";
  }

  bool operator== (const infinite_recursion_diagnostic &other) const
  {
    return m_callee_fndecl == other.m_callee_fndecl;
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_infinite_recursion;
  }

  bool emit (diagnostic_emission_context &ctxt) final override
  {
    /* "CWE-674: Uncontrolled Recursion".  */
    ctxt.add_cwe (674);
    return ctxt.warn ("infinite recursion");
  }

  label_text describe_final_event (const evdesc::final_event &ev) final override
  {
    const int frames_consumed = (m_new_entry_enode->get_stack_depth ()
				 - m_prev_entry_enode->get_stack_depth ());
    if (frames_consumed > 1)
      return ev.formatted_print
	("apparently infinite chain of mutually-recursive function calls,"
	 " consuming %i stack frames per recursion",
	 frames_consumed);
    else
      return ev.formatted_print ("apparently infinite recursion");
  }

  void
  add_function_entry_event (const exploded_edge &eedge,
			    checker_path *emission_path) final override
  {
    /* Subclass of function_entry_event for use when reporting both
       the initial and subsequent entries to the function of interest,
       allowing for cross-referencing the first event in the description
       of the second.  */
    class recursive_function_entry_event : public function_entry_event
    {
    public:
      recursive_function_entry_event (const program_point &dst_point,
				      const infinite_recursion_diagnostic &pd,
				      bool topmost)
      : function_entry_event (dst_point),
	m_pd (pd),
	m_topmost (topmost)
      {
      }

      label_text
      get_desc (bool can_colorize) const final override
      {
	if (m_topmost)
	  {
	    if (m_pd.m_prev_entry_event
		&& m_pd.m_prev_entry_event->get_id_ptr ()->known_p ())
	      return make_label_text
		(can_colorize,
		 "recursive entry to %qE; previously entered at %@",
		 m_effective_fndecl,
		 m_pd.m_prev_entry_event->get_id_ptr ());
	    else
	      return make_label_text (can_colorize, "recursive entry to %qE",
				      m_effective_fndecl);
	  }
	else
	  return make_label_text (can_colorize, "initial entry to %qE",
				  m_effective_fndecl);
      }

    private:
      const infinite_recursion_diagnostic &m_pd;
      bool m_topmost;
    };
    const exploded_node *dst_node = eedge.m_dest;
    const program_point &dst_point = dst_node->get_point ();
    if (eedge.m_dest == m_prev_entry_enode)
      {
	gcc_assert (m_prev_entry_event == NULL);
	std::unique_ptr<checker_event> prev_entry_event
	  = make_unique <recursive_function_entry_event> (dst_point,
							  *this, false);
	m_prev_entry_event = prev_entry_event.get ();
	emission_path->add_event (std::move (prev_entry_event));
      }
    else if (eedge.m_dest == m_new_entry_enode)
      emission_path->add_event
	(make_unique<recursive_function_entry_event> (dst_point, *this, true));
    else
      pending_diagnostic::add_function_entry_event (eedge, emission_path);
  }

  /* Customize the location where the warning_event appears, putting
     it at the topmost entrypoint to the function.  */
  void add_final_event (const state_machine *,
			const exploded_node *enode,
			const gimple *,
			tree,
			state_machine::state_t,
			checker_path *emission_path) final override
  {
    gcc_assert (m_new_entry_enode);
    emission_path->add_event
      (make_unique<warning_event>
       (event_loc_info (m_new_entry_enode->get_supernode
			  ()->get_start_location (),
			m_callee_fndecl,
			m_new_entry_enode->get_stack_depth ()),
	enode,
	nullptr, nullptr, nullptr));
  }

  /* Reject paths in which conjured svalues have affected control flow
     since m_prev_entry_enode.  */

  bool check_valid_fpath_p (const feasible_node &final_fnode,
			    const gimple *)
    const final override
  {
    /* Reject paths in which calls with unknown side effects have occurred
       since m_prev_entry_enode.
       Find num calls with side effects.  Walk backward until we reach the
       pref */
    gcc_assert (final_fnode.get_inner_node () == m_new_entry_enode);

    /* FG is actually a tree.  Walk backwards from FINAL_FNODE until we
       reach the prev_entry_enode (or the origin).  */
    const feasible_node *iter_fnode = &final_fnode;
    while (iter_fnode->get_inner_node ()->m_index != 0)
      {
	gcc_assert (iter_fnode->m_preds.length () == 1);

	feasible_edge *pred_fedge
	  = static_cast <feasible_edge *> (iter_fnode->m_preds[0]);

	/* Determine if conjured svalues have affected control flow
	   since the prev entry node.  */
	if (fedge_uses_conjured_svalue_p (pred_fedge))
	  /* If so, then reject this diagnostic.  */
	  return false;
	iter_fnode = static_cast <feasible_node *> (pred_fedge->m_src);
	if (iter_fnode->get_inner_node () == m_prev_entry_enode)
	  /* Accept this diagnostic.  */
	  return true;
    }

    /* We shouldn't get here; if we do, reject the diagnostic.  */
    gcc_unreachable ();
    return false;
  }

  void maybe_add_sarif_properties (sarif_object &result_obj)
    const final override
  {
    sarif_property_bag &props = result_obj.get_or_create_properties ();
#define PROPERTY_PREFIX "gcc/analyzer/infinite_recursion_diagnostic/"
    props.set_integer (PROPERTY_PREFIX "prev_entry_enode",
		       m_prev_entry_enode->m_index);
    props.set_integer (PROPERTY_PREFIX "new_entry_enode",
		       m_new_entry_enode->m_index);
#undef PROPERTY_PREFIX
  }

private:
  /* Return true iff control flow along FEDGE was affected by
     a conjured_svalue.  */
  static bool fedge_uses_conjured_svalue_p (feasible_edge *fedge)
  {
    const exploded_edge *eedge = fedge->get_inner_edge ();
    const superedge *sedge = eedge->m_sedge;
    if (!sedge)
      return false;
    const cfg_superedge *cfg_sedge = sedge->dyn_cast_cfg_superedge ();
    if (!cfg_sedge)
      return false;
    const gimple *last_stmt = sedge->m_src->get_last_stmt ();
    if (!last_stmt)
      return false;

    const feasible_node *dst_fnode
      = static_cast<const feasible_node *> (fedge->m_dest);
    const region_model &model = dst_fnode->get_state ().get_model ();

    if (const gcond *cond_stmt = dyn_cast <const gcond *> (last_stmt))
      {
	if (expr_uses_conjured_svalue_p (model, gimple_cond_lhs (cond_stmt)))
	  return true;
	if (expr_uses_conjured_svalue_p (model, gimple_cond_rhs (cond_stmt)))
	  return true;
      }
    else if (const gswitch *switch_stmt
	       = dyn_cast <const gswitch *> (last_stmt))
      {
	if (expr_uses_conjured_svalue_p (model,
					 gimple_switch_index (switch_stmt)))
	  return true;
      }
    return false;
  }

  /* Return true iff EXPR is affected by a conjured_svalue.  */
  static bool expr_uses_conjured_svalue_p (const region_model &model,
					   tree expr)
  {
    class conjured_svalue_finder : public visitor
    {
    public:
      conjured_svalue_finder () : m_found_conjured_svalues (false)
      {
      }
      void
      visit_conjured_svalue (const conjured_svalue *) final override
      {
	m_found_conjured_svalues = true;
      }

      bool m_found_conjured_svalues;
    };

    const svalue *sval = model.get_rvalue (expr, NULL);
    conjured_svalue_finder v;
    sval->accept (&v);
    return v.m_found_conjured_svalues;
  }

  const exploded_node *m_prev_entry_enode;
  const exploded_node *m_new_entry_enode;
  tree m_callee_fndecl;
  const checker_event *m_prev_entry_event;
};

/* Return true iff ENODE is the PK_BEFORE_SUPERNODE at a function
   entrypoint.  */

static bool
is_entrypoint_p (exploded_node *enode)
{
  /* Look for an entrypoint to a function...  */
  const supernode *snode = enode->get_supernode ();
  if (!snode)
    return false;
  if (!snode->entry_p ())
    return false;;
  const program_point &point = enode->get_point ();
  if (point.get_kind () != PK_BEFORE_SUPERNODE)
    return false;
  return true;
}

/* Walk backwards through the eg, looking for the first
   enode we find that's also the entrypoint of the same function.  */

exploded_node *
exploded_graph::find_previous_entry_to (function *top_of_stack_fun,
					exploded_node *enode) const
{
  auto_vec<exploded_node *> worklist;
  hash_set<exploded_node *> visited;

  visited.add (enode);
  for (auto in_edge : enode->m_preds)
    worklist.safe_push (in_edge->m_src);

  while (worklist.length () > 0)
    {
      exploded_node *iter = worklist.pop ();

      if (is_entrypoint_p (iter)
	  && iter->get_function () == top_of_stack_fun)
	return iter;

      if (visited.contains (iter))
	continue;
      visited.add (iter);
      for (auto in_edge : iter->m_preds)
	worklist.safe_push (in_edge->m_src);
    }

  /* Not found.  */
  return NULL;
}

/* Given BASE_REG within ENCLOSING_FRAME (such as a function parameter),
   remap it to the equivalent region within EQUIV_PREV_FRAME.

   For example, given param "n" within frame "foo@3", and equiv prev frame
   "foo@1", remap it to param "n" within frame "foo@1".  */

static const region *
remap_enclosing_frame (const region *base_reg,
		       const frame_region *enclosing_frame,
		       const frame_region *equiv_prev_frame,
		       region_model_manager *mgr)
{
  gcc_assert (base_reg->get_parent_region () == enclosing_frame);
  switch (base_reg->get_kind ())
    {
    default:
      /* We should only encounter params and varargs at the topmost
	 entrypoint.  */
      gcc_unreachable ();

    case RK_VAR_ARG:
      {
	const var_arg_region *var_arg_reg = (const var_arg_region *)base_reg;
	return mgr->get_var_arg_region (equiv_prev_frame,
					var_arg_reg->get_index ());
      }
    case RK_DECL:
      {
	const decl_region *decl_reg = (const decl_region *)base_reg;
	return equiv_prev_frame->get_region_for_local (mgr,
						       decl_reg->get_decl (),
						       NULL);
      }
    }
}

/* Return true iff SVAL is unknown, or contains an unknown svalue.  */

static bool
contains_unknown_p (const svalue *sval)
{
  if (sval->get_kind () == SK_UNKNOWN)
    return true;
  if (const compound_svalue *compound_sval
	= sval->dyn_cast_compound_svalue ())
    for (auto iter : *compound_sval)
      if (iter.second->get_kind () == SK_UNKNOWN)
	return true;
  return false;
}

/* Subroutine of sufficiently_different_p.  Compare the store bindings
   for BASE_REG within NEW_ENTRY_ENODE and PREV_ENTRY_ENODE.

   Return true if the state of NEW_ENTRY_ENODE is sufficiently different
   from PREV_ENTRY_ENODE within BASE_REG to suggest that some variant is
   being modified, and thus the recursion isn't infinite.

   Return false if the states for BASE_REG are effectively the same.  */

static bool
sufficiently_different_region_binding_p (exploded_node *new_entry_enode,
					 exploded_node *prev_entry_enode,
					 const region *base_reg)
{
  /* Compare the stores of the two enodes.  */
  const region_model &new_model
    = *new_entry_enode->get_state ().m_region_model;
  const region_model &prev_model
    = *prev_entry_enode->get_state ().m_region_model;

  /* Get the value within the new frame.  */
  const svalue *new_sval
    = new_model.get_store_value (base_reg, NULL);

  /* If any part of the value is UNKNOWN (e.g. due to hitting
     complexity limits) assume that it differs from the previous
     value.  */
  if (contains_unknown_p (new_sval))
    return true;

  /* Get the equivalent value within the old enode.  */
  const svalue *prev_sval;

  if (const frame_region *enclosing_frame
      = base_reg->maybe_get_frame_region ())
    {
      /* We have a binding within a frame in the new entry enode.  */

      /* Consider changes in bindings below the original entry
	 to the recursion.  */
      const int old_stack_depth = prev_entry_enode->get_stack_depth ();
      if (enclosing_frame->get_stack_depth () < old_stack_depth)
	prev_sval = prev_model.get_store_value (base_reg, NULL);
      else
	{
	  /* Ignore bindings within frames below the new entry node.  */
	  const int new_stack_depth = new_entry_enode->get_stack_depth ();
	  if (enclosing_frame->get_stack_depth () < new_stack_depth)
	    return false;

	  /* We have a binding within the frame of the new entry node,
	     presumably a parameter.  */

	  /* Get the value within the equivalent frame of
	     the old entrypoint; typically will be the initial_svalue
	     of the parameter.  */
	  const frame_region *equiv_prev_frame
	    = prev_model.get_current_frame ();
	  const region *equiv_prev_base_reg
	    = remap_enclosing_frame (base_reg,
				     enclosing_frame,
				     equiv_prev_frame,
				     new_model.get_manager ());
	  prev_sval
	    = prev_model.get_store_value (equiv_prev_base_reg, NULL);
	}
    }
  else
    prev_sval = prev_model.get_store_value (base_reg, NULL);

  /* If the prev_sval contains UNKNOWN (e.g. due to hitting complexity limits)
     assume that it will differ from any new value.  */
  if (contains_unknown_p (prev_sval))
    return true;

  if (new_sval != prev_sval)
    return true;

  return false;
}

/* Compare the state of memory at NEW_ENTRY_ENODE and PREV_ENTRY_ENODE,
   both of which are entrypoints to the same function, where recursion has
   occurred.

   Return true if the state of NEW_ENTRY_ENODE is sufficiently different
   from PREV_ENTRY_ENODE to suggest that some variant is being modified,
   and thus the recursion isn't infinite.

   Return false if the states are effectively the same, suggesting that
   the recursion is infinite.

   For example, consider mutually recursive functions "foo" and "bar".
   At the entrypoint to a "foo" frame where we've detected recursion,
   we might have three frames on the stack: the new 'foo'@3, an inner
   'bar'@2, and the innermost 'foo'@1.

     (gdb) call enode->dump(m_ext_state)
     EN: 16
     callstring: [(SN: 9 -> SN: 3 in foo), (SN: 5 -> SN: 8 in bar)]
     before SN: 0 (NULL from-edge)

     rmodel:
     stack depth: 3
       frame (index 2): frame: ‘foo’@3
       frame (index 1): frame: ‘bar’@2
       frame (index 0): frame: ‘foo’@1
     clusters within root region
       cluster for: (*INIT_VAL(f_4(D)))
     clusters within frame: ‘bar’@2
       cluster for: b_2(D): INIT_VAL(f_4(D))
     clusters within frame: ‘foo’@3
       cluster for: f_4(D): INIT_VAL(f_4(D))
     m_called_unknown_fn: FALSE

   whereas for the previous entry node we'd have just the innermost
   'foo'@1

     (gdb) call prev_entry_enode->dump(m_ext_state)
     EN: 1
     callstring: []
     before SN: 0 (NULL from-edge)

     rmodel:
     stack depth: 1
       frame (index 0): frame: ‘foo’@1
     clusters within root region
       cluster for: (*INIT_VAL(f_4(D)))
     m_called_unknown_fn: FALSE

   We want to abstract away frames 1 and 2 in the new entry enode,
   and compare its frame 3 with the frame 1 in the previous entry
   enode, and determine if enough state changes between them to
   rule out infinite recursion.  */

static bool
sufficiently_different_p (exploded_node *new_entry_enode,
			  exploded_node *prev_entry_enode,
			  logger *logger)
{
  LOG_SCOPE (logger);
  gcc_assert (new_entry_enode);
  gcc_assert (prev_entry_enode);
  gcc_assert (is_entrypoint_p (new_entry_enode));
  gcc_assert (is_entrypoint_p (prev_entry_enode));

  /* Compare the stores of the two enodes.  */
  const region_model &new_model
    = *new_entry_enode->get_state ().m_region_model;
  const store &new_store = *new_model.get_store ();

  for (auto kv : new_store)
    {
      const region *base_reg = kv.first;
      if (sufficiently_different_region_binding_p (new_entry_enode,
						   prev_entry_enode,
						   base_reg))
	return true;
    }

  /* No significant differences found.  */
  return false;
}

/* Implementation of -Wanalyzer-infinite-recursion.

   Called when adding ENODE to the graph, after adding its first in-edge.

   For function entrypoints, see if recursion has occurred, and, if so,
   check if the state of memory changed between the recursion levels,
   which would suggest some kind of decreasing variant that leads to
   termination.

   For recursive calls where the state of memory is effectively unchanged
   between recursion levels, warn with -Wanalyzer-infinite-recursion.  */

void
exploded_graph::detect_infinite_recursion (exploded_node *enode)
{
  if (!is_entrypoint_p (enode))
    return;
  function *top_of_stack_fun = enode->get_function ();
  gcc_assert (top_of_stack_fun);

  /* ....where a call to that function is already in the call string.  */
  const call_string &call_string = enode->get_point ().get_call_string ();

  if (call_string.count_occurrences_of_function (top_of_stack_fun) < 2)
    return;

  tree fndecl = top_of_stack_fun->decl;

  log_scope s (get_logger (),
	       "checking for infinite recursion",
	       "considering recursion at EN: %i entering %qE",
	       enode->m_index, fndecl);

  /* Find enode that's the entrypoint for the previous frame for fndecl
     in the recursion.  */
  exploded_node *prev_entry_enode
    = find_previous_entry_to (top_of_stack_fun, enode);
  gcc_assert (prev_entry_enode);
  if (get_logger ())
    get_logger ()->log ("previous entrypoint to %qE is EN: %i",
			fndecl, prev_entry_enode->m_index);

  /* Look for changes to the state of memory between the recursion levels.  */
  if (sufficiently_different_p (enode, prev_entry_enode, get_logger ()))
    return;

  /* Otherwise, the state of memory is effectively the same between the two
     recursion levels; warn.  */

  const supernode *caller_snode = call_string.get_top_of_stack ().m_caller;
  const supernode *snode = enode->get_supernode ();
  gcc_assert (caller_snode->m_returning_call);
  pending_location ploc (enode,
			 snode,
			 caller_snode->m_returning_call,
			 nullptr);
  get_diagnostic_manager ().add_diagnostic
    (ploc,
     make_unique<infinite_recursion_diagnostic> (prev_entry_enode,
						 enode,
						 fndecl));
}
