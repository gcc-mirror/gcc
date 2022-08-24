/* Subclasses of diagnostic_path and diagnostic_event for analyzer diagnostics.
   Copyright (C) 2019-2022 Free Software Foundation, Inc.
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
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "diagnostic-core.h"
#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "function.h"
#include "diagnostic-path.h"
#include "options.h"
#include "cgraph.h"
#include "function.h"
#include "cfg.h"
#include "digraph.h"
#include "alloc-pool.h"
#include "fibonacci_heap.h"
#include "diagnostic-event-id.h"
#include "shortest-paths.h"
#include "json.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/sm.h"
#include "sbitmap.h"
#include "bitmap.h"
#include "tristate.h"
#include "ordered-hash-map.h"
#include "selftest.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"
#include "analyzer/program-state.h"
#include "analyzer/checker-path.h"
#include "gimple-iterator.h"
#include "inlining-iterator.h"
#include "analyzer/supergraph.h"
#include "analyzer/pending-diagnostic.h"
#include "analyzer/diagnostic-manager.h"
#include "analyzer/constraint-manager.h"
#include "analyzer/diagnostic-manager.h"
#include "analyzer/checker-path.h"
#include "analyzer/exploded-graph.h"

#if ENABLE_ANALYZER

namespace ana {

/* Get a string for EK.  */

const char *
event_kind_to_string (enum event_kind ek)
{
  switch (ek)
    {
    default:
      gcc_unreachable ();
    case EK_DEBUG:
      return "EK_DEBUG";
    case EK_CUSTOM:
      return "EK_CUSTOM";
    case EK_STMT:
      return "EK_STMT";
    case EK_REGION_CREATION:
      return "EK_REGION_CREATION";
    case EK_FUNCTION_ENTRY:
      return "EK_FUNCTION_ENTRY";
    case EK_STATE_CHANGE:
      return "EK_STATE_CHANGE";
    case EK_START_CFG_EDGE:
      return "EK_START_CFG_EDGE";
    case EK_END_CFG_EDGE:
      return "EK_END_CFG_EDGE";
    case EK_CALL_EDGE:
      return "EK_CALL_EDGE";
    case EK_RETURN_EDGE:
      return "EK_RETURN_EDGE";
    case EK_START_CONSOLIDATED_CFG_EDGES:
      return "EK_START_CONSOLIDATED_CFG_EDGES";
    case EK_END_CONSOLIDATED_CFG_EDGES:
      return "EK_END_CONSOLIDATED_CFG_EDGES";
    case EK_INLINED_CALL:
      return "EK_INLINED_CALL";
    case EK_SETJMP:
      return "EK_SETJMP";
    case EK_REWIND_FROM_LONGJMP:
      return "EK_REWIND_FROM_LONGJMP";
    case EK_REWIND_TO_SETJMP:
      return "EK_REWIND_TO_SETJMP";
    case EK_WARNING:
      return "EK_WARNING";
    }
}

/* A class for fixing up fndecls and stack depths in checker_event, based
   on inlining records.

   The early inliner runs before the analyzer, which can lead to confusing
   output.

   Tne base fndecl and depth within a checker_event are from call strings
   in program_points, which reflect the call strings after inlining.
   This class lets us offset the depth and fix up the reported fndecl and
   stack depth to better reflect the user's original code.  */

class inlining_info
{
public:
  inlining_info (location_t loc)
  {
    inlining_iterator iter (loc);
    m_inner_fndecl = iter.get_fndecl ();
    int num_frames = 0;
    while (!iter.done_p ())
      {
	m_outer_fndecl = iter.get_fndecl ();
	num_frames++;
	iter.next ();
      }
    if (num_frames > 1)
      m_extra_frames = num_frames - 1;
    else
      m_extra_frames = 0;
  }

  tree get_inner_fndecl () const { return m_inner_fndecl; }
  int get_extra_frames () const { return m_extra_frames; }

private:
  tree m_outer_fndecl;
  tree m_inner_fndecl;
  int m_extra_frames;
};

/* class checker_event : public diagnostic_event.  */

/* checker_event's ctor.  */

checker_event::checker_event (enum event_kind kind,
			      location_t loc, tree fndecl, int depth)
: m_kind (kind), m_loc (loc),
  m_original_fndecl (fndecl), m_effective_fndecl (fndecl),
  m_original_depth (depth), m_effective_depth (depth),
  m_pending_diagnostic (NULL), m_emission_id (),
  m_logical_loc (fndecl)
{
  /* Update effective fndecl and depth if inlining has been recorded.  */
  if (flag_analyzer_undo_inlining)
    {
      inlining_info info (loc);
      if (info.get_inner_fndecl ())
	{
	  m_effective_fndecl = info.get_inner_fndecl ();
	  m_effective_depth += info.get_extra_frames ();
	  m_logical_loc = tree_logical_location (m_effective_fndecl);
	}
    }
}

/* No-op implementation of diagnostic_event::get_meaning vfunc for
   checker_event: checker events have no meaning by default.  */

diagnostic_event::meaning
checker_event::get_meaning () const
{
  return meaning ();
}

/* Dump this event to PP (for debugging/logging purposes).  */

void
checker_event::dump (pretty_printer *pp) const
{
  label_text event_desc (get_desc (false));
  pp_printf (pp, "\"%s\" (depth %i",
	     event_desc.get (), m_effective_depth);

  if (m_effective_depth != m_original_depth)
    pp_printf (pp, " corrected from %i",
	       m_original_depth);
  if (m_effective_fndecl)
    {
      pp_printf (pp, ", fndecl %qE", m_effective_fndecl);
      if (m_effective_fndecl != m_original_fndecl)
	pp_printf (pp, " corrected from %qE", m_original_fndecl);
    }
  pp_printf (pp, ", m_loc=%x)",
	     get_location ());
}

/* Hook for being notified when this event has its final id EMISSION_ID
   and is about to emitted for PD.

   Base implementation of checker_event::prepare_for_emission vfunc;
   subclasses that override this should chain up to it.

   Record PD and EMISSION_ID, and call the get_desc vfunc, so that any
   side-effects of the call to get_desc take place before
   pending_diagnostic::emit is called.

   For example, state_change_event::get_desc can call
   pending_diagnostic::describe_state_change; free_of_non_heap can use this
   to tweak the message (TODO: would be neater to simply capture the
   pertinent data within the sm-state).  */

void
checker_event::prepare_for_emission (checker_path *,
				     pending_diagnostic *pd,
				     diagnostic_event_id_t emission_id)
{
  m_pending_diagnostic = pd;
  m_emission_id = emission_id;

  label_text desc = get_desc (false);
}

/* class debug_event : public checker_event.  */

/* Implementation of diagnostic_event::get_desc vfunc for
   debug_event.
   Use the saved string as the event's description.  */

label_text
debug_event::get_desc (bool) const
{
  return label_text::borrow (m_desc);
}

/* class precanned_custom_event : public custom_event.  */

/* Implementation of diagnostic_event::get_desc vfunc for
   precanned_custom_event.
   Use the saved string as the event's description.  */

label_text
precanned_custom_event::get_desc (bool) const
{
  return label_text::borrow (m_desc);
}

/* class statement_event : public checker_event.  */

/* statement_event's ctor.  */

statement_event::statement_event (const gimple *stmt, tree fndecl, int depth,
				  const program_state &dst_state)
: checker_event (EK_STMT, gimple_location (stmt), fndecl, depth),
  m_stmt (stmt),
  m_dst_state (dst_state)
{
}

/* Implementation of diagnostic_event::get_desc vfunc for
   statement_event.
   Use the statement's dump form as the event's description.  */

label_text
statement_event::get_desc (bool) const
{
  pretty_printer pp;
  pp_string (&pp, "stmt: ");
  pp_gimple_stmt_1 (&pp, m_stmt, 0, (dump_flags_t)0);
  return label_text::take (xstrdup (pp_formatted_text (&pp)));
}

/* class region_creation_event : public checker_event.  */

region_creation_event::region_creation_event (const region *reg,
					      location_t loc,
					      tree fndecl,
					      int depth)
: checker_event (EK_REGION_CREATION, loc, fndecl, depth),
  m_reg (reg)
{
}

/* Implementation of diagnostic_event::get_desc vfunc for
   region_creation_event.  */

label_text
region_creation_event::get_desc (bool can_colorize) const
{
  if (m_pending_diagnostic)
    {
      label_text custom_desc
	    = m_pending_diagnostic->describe_region_creation_event
		(evdesc::region_creation (can_colorize, m_reg));
      if (custom_desc.get ())
	return custom_desc;
    }

  switch (m_reg->get_memory_space ())
    {
    default:
      return label_text::borrow ("region created here");
    case MEMSPACE_STACK:
      return label_text::borrow ("region created on stack here");
    case MEMSPACE_HEAP:
      return label_text::borrow ("region created on heap here");
    }
}

/* class function_entry_event : public checker_event.  */

/* Implementation of diagnostic_event::get_desc vfunc for
   function_entry_event.

   Use a string such as "entry to 'foo'" as the event's description.  */

label_text
function_entry_event::get_desc (bool can_colorize) const
{
  return make_label_text (can_colorize, "entry to %qE", m_effective_fndecl);
}

/* Implementation of diagnostic_event::get_meaning vfunc for
   function entry.  */

diagnostic_event::meaning
function_entry_event::get_meaning () const
{
  return meaning (VERB_enter, NOUN_function);
}

/* class state_change_event : public checker_event.  */

/* state_change_event's ctor.  */

state_change_event::state_change_event (const supernode *node,
					const gimple *stmt,
					int stack_depth,
					const state_machine &sm,
					const svalue *sval,
					state_machine::state_t from,
					state_machine::state_t to,
					const svalue *origin,
					const program_state &dst_state)
: checker_event (EK_STATE_CHANGE,
		 stmt->location, node->m_fun->decl,
		 stack_depth),
  m_node (node), m_stmt (stmt), m_sm (sm),
  m_sval (sval), m_from (from), m_to (to),
  m_origin (origin),
  m_dst_state (dst_state)
{
}

/* Implementation of diagnostic_event::get_desc vfunc for
   state_change_event.

   Attempt to generate a nicer human-readable description.
   For greatest precision-of-wording, give the pending diagnostic
   a chance to describe this state change (in terms of the
   diagnostic).
   Note that we only have a pending_diagnostic set on the event once
   the diagnostic is about to being emitted, so the description for
   an event can change.  */

label_text
state_change_event::get_desc (bool can_colorize) const
{
  if (m_pending_diagnostic)
    {
      region_model *model = m_dst_state.m_region_model;
      tree var = model->get_representative_tree (m_sval);
      tree origin = model->get_representative_tree (m_origin);
      label_text custom_desc
	= m_pending_diagnostic->describe_state_change
	    (evdesc::state_change (can_colorize, var, origin,
				   m_from, m_to, m_emission_id, *this));
      if (custom_desc.get ())
	{
	  if (flag_analyzer_verbose_state_changes)
	    {
	      /* Get any "meaning" of event.  */
	      diagnostic_event::meaning meaning = get_meaning ();
	      pretty_printer meaning_pp;
	      meaning.dump_to_pp (&meaning_pp);

	      /* Append debug version.  */
	      if (m_origin)
		return make_label_text
		  (can_colorize,
		   "%s (state of %qE: %qs -> %qs, origin: %qE, meaning: %s)",
		   custom_desc.get (),
		   var,
		   m_from->get_name (),
		   m_to->get_name (),
		   origin,
		   pp_formatted_text (&meaning_pp));
	      else
		return make_label_text
		  (can_colorize,
		   "%s (state of %qE: %qs -> %qs, NULL origin, meaning: %s)",
		   custom_desc.get (),
		   var,
		   m_from->get_name (),
		   m_to->get_name (),
		   pp_formatted_text (&meaning_pp));
	    }
	  else
	    return custom_desc;
	}
    }

  /* Fallback description.  */
  if (m_sval)
    {
      label_text sval_desc = m_sval->get_desc ();
      if (m_origin)
	{
	  label_text origin_desc = m_origin->get_desc ();
	  return make_label_text
	    (can_colorize,
	     "state of %qs: %qs -> %qs (origin: %qs)",
	     sval_desc.get (),
	     m_from->get_name (),
	     m_to->get_name (),
	     origin_desc.get ());
	}
      else
	return make_label_text
	  (can_colorize,
	   "state of %qs: %qs -> %qs (NULL origin)",
	   sval_desc.get (),
	   m_from->get_name (),
	   m_to->get_name ());
    }
  else
    {
      gcc_assert (m_origin == NULL);
      return make_label_text
	(can_colorize,
	 "global state: %qs -> %qs",
	 m_from->get_name (),
	 m_to->get_name ());
    }
}

/* Implementation of diagnostic_event::get_meaning vfunc for
   state change events: delegate to the pending_diagnostic to
   get any meaning.  */

diagnostic_event::meaning
state_change_event::get_meaning () const
{
  if (m_pending_diagnostic)
    {
      region_model *model = m_dst_state.m_region_model;
      tree var = model->get_representative_tree (m_sval);
      tree origin = model->get_representative_tree (m_origin);
      return m_pending_diagnostic->get_meaning_for_state_change
	(evdesc::state_change (false, var, origin,
			       m_from, m_to, m_emission_id, *this));
    }
  else
    return meaning ();
}

/* class superedge_event : public checker_event.  */

/* Get the callgraph_superedge for this superedge_event, which must be
   for an interprocedural edge, rather than a CFG edge.  */

const callgraph_superedge&
superedge_event::get_callgraph_superedge () const
{
  gcc_assert (m_sedge->m_kind != SUPEREDGE_CFG_EDGE);
  return *m_sedge->dyn_cast_callgraph_superedge ();
}

/* Determine if this event should be filtered at the given verbosity
   level.  */

bool
superedge_event::should_filter_p (int verbosity) const
{
  switch (m_sedge->m_kind)
    {
    case SUPEREDGE_CFG_EDGE:
      {
	if (verbosity < 2)
	  return true;

	if (verbosity < 4)
	  {
	    /* Filter events with empty descriptions.  This ought to filter
	       FALLTHRU, but retain true/false/switch edges.  */
	    label_text desc = get_desc (false);
	    gcc_assert (desc.get ());
	    if (desc.get ()[0] == '\0')
	      return true;
	  }
      }
      break;

    default:
      break;
    }
  return false;
}

/* superedge_event's ctor.  */

superedge_event::superedge_event (enum event_kind kind,
				  const exploded_edge &eedge,
				  location_t loc, tree fndecl, int depth)
: checker_event (kind, loc, fndecl, depth),
  m_eedge (eedge), m_sedge (eedge.m_sedge),
  m_var (NULL_TREE), m_critical_state (0)
{
}

/* class cfg_edge_event : public superedge_event.  */

/* Get the cfg_superedge for this cfg_edge_event.  */

const cfg_superedge &
cfg_edge_event::get_cfg_superedge () const
{
  return *m_sedge->dyn_cast_cfg_superedge ();
}

/* cfg_edge_event's ctor.  */

cfg_edge_event::cfg_edge_event (enum event_kind kind,
				const exploded_edge &eedge,
				location_t loc, tree fndecl, int depth)
: superedge_event (kind, eedge, loc, fndecl, depth)
{
  gcc_assert (eedge.m_sedge->m_kind == SUPEREDGE_CFG_EDGE);
}

/* Implementation of diagnostic_event::get_meaning vfunc for
   CFG edge events.  */

diagnostic_event::meaning
cfg_edge_event::get_meaning () const
{
  const cfg_superedge& cfg_sedge = get_cfg_superedge ();
  if (cfg_sedge.true_value_p ())
    return meaning (VERB_branch, PROPERTY_true);
  else if (cfg_sedge.false_value_p ())
    return meaning (VERB_branch, PROPERTY_false);
  else
    return meaning ();
}

/* class start_cfg_edge_event : public cfg_edge_event.  */

/* Implementation of diagnostic_event::get_desc vfunc for
   start_cfg_edge_event.

   If -fanalyzer-verbose-edges, then generate low-level descriptions, such
   as
     "taking 'true' edge SN:7 -> SN:8".

   Otherwise, generate strings using the label of the underlying CFG if
   any, such as:
     "following 'true' branch..." or
     "following 'case 3' branch..."
     "following 'default' branch..."

   For conditionals, attempt to supply a description of the condition that
   holds, such as:
     "following 'false' branch (when 'ptr' is non-NULL)..."

   Failing that, return an empty description (which will lead to this event
   being filtered).  */

label_text
start_cfg_edge_event::get_desc (bool can_colorize) const
{
  bool user_facing = !flag_analyzer_verbose_edges;
  label_text edge_desc (m_sedge->get_description (user_facing));
  if (user_facing)
    {
      if (edge_desc.get () && strlen (edge_desc.get ()) > 0)
	{
	  label_text cond_desc = maybe_describe_condition (can_colorize);
	  label_text result;
	  if (cond_desc.get ())
	    return make_label_text (can_colorize,
				    "following %qs branch (%s)...",
				    edge_desc.get (), cond_desc.get ());
	  else
	    return make_label_text (can_colorize,
				    "following %qs branch...",
				    edge_desc.get ());
	}
      else
	return label_text::borrow ("");
    }
  else
    {
      if (strlen (edge_desc.get ()) > 0)
	return make_label_text (can_colorize,
				"taking %qs edge SN:%i -> SN:%i",
				edge_desc.get (),
				m_sedge->m_src->m_index,
				m_sedge->m_dest->m_index);
      else
	return make_label_text (can_colorize,
				"taking edge SN:%i -> SN:%i",
				m_sedge->m_src->m_index,
				m_sedge->m_dest->m_index);
    }
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
start_cfg_edge_event::maybe_describe_condition (bool can_colorize) const
{
  const cfg_superedge& cfg_sedge = get_cfg_superedge ();

  if (cfg_sedge.true_value_p () || cfg_sedge.false_value_p ())
    {
      const gimple *last_stmt = m_sedge->m_src->get_last_stmt ();
      if (const gcond *cond_stmt = dyn_cast <const gcond *> (last_stmt))
	{
	  enum tree_code op = gimple_cond_code (cond_stmt);
	  tree lhs = gimple_cond_lhs (cond_stmt);
	  tree rhs = gimple_cond_rhs (cond_stmt);
	  if (cfg_sedge.false_value_p ())
	    op = invert_tree_comparison (op, false /* honor_nans */);
	  return maybe_describe_condition (can_colorize,
					   lhs, op, rhs);
	}
    }
  return label_text::borrow (NULL);
}

/* Subroutine of maybe_describe_condition above.

   Attempt to generate a user-facing description of the condition
   LHS OP RHS, but only if it is likely to make it easier for the
   user to understand a condition.  */

label_text
start_cfg_edge_event::maybe_describe_condition (bool can_colorize,
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
	if (is_special_named_call_p (call, "strcmp", 2))
	  {
	    if (op == EQ_EXPR)
	      return label_text::borrow ("when the strings are equal");
	    if (op == NE_EXPR)
	      return label_text::borrow ("when the strings are non-equal");
	  }
    }

  /* Only attempt to generate text for sufficiently simple expressions.  */
  if (!should_print_expr_p (lhs))
    return label_text::borrow (NULL);
  if (!should_print_expr_p (rhs))
    return label_text::borrow (NULL);

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
start_cfg_edge_event::should_print_expr_p (tree expr)
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

/* class call_event : public superedge_event.  */

/* call_event's ctor.  */

call_event::call_event (const exploded_edge &eedge,
			location_t loc, tree fndecl, int depth)
: superedge_event (EK_CALL_EDGE, eedge, loc, fndecl, depth)
{
  if (eedge.m_sedge)
    gcc_assert (eedge.m_sedge->m_kind == SUPEREDGE_CALL);

   m_src_snode = eedge.m_src->get_supernode ();
   m_dest_snode = eedge.m_dest->get_supernode ();
}

/* Implementation of diagnostic_event::get_desc vfunc for
   call_event.

   If this call event passes critical state for an sm-based warning,
   allow the diagnostic to generate a precise description, such as:

     "passing freed pointer 'ptr' in call to 'foo' from 'bar'"

   Otherwise, generate a description of the form
   "calling 'foo' from 'bar'".  */

label_text
call_event::get_desc (bool can_colorize) const
{
  if (m_critical_state && m_pending_diagnostic)
    {
      gcc_assert (m_var);
      tree var = fixup_tree_for_diagnostic (m_var);
      label_text custom_desc
	= m_pending_diagnostic->describe_call_with_state
	    (evdesc::call_with_state (can_colorize,
				      m_src_snode->m_fun->decl,
				      m_dest_snode->m_fun->decl,
				      var,
				      m_critical_state));
      if (custom_desc.get ())
	return custom_desc;
    }

  return make_label_text (can_colorize,
			  "calling %qE from %qE",
			  get_callee_fndecl (),
			  get_caller_fndecl ());
}

/* Implementation of diagnostic_event::get_meaning vfunc for
   function call events.  */

diagnostic_event::meaning
call_event::get_meaning () const
{
  return meaning (VERB_call, NOUN_function);
}

/* Override of checker_event::is_call_p for calls.  */

bool
call_event::is_call_p () const
{
  return true;
}

tree
call_event::get_caller_fndecl () const
{
  return m_src_snode->m_fun->decl;
}

tree
call_event::get_callee_fndecl () const
{
  return m_dest_snode->m_fun->decl;
}

/* class return_event : public superedge_event.  */

/* return_event's ctor.  */

return_event::return_event (const exploded_edge &eedge,
			    location_t loc, tree fndecl, int depth)
: superedge_event (EK_RETURN_EDGE, eedge, loc, fndecl, depth)
{
  if (eedge.m_sedge)
    gcc_assert (eedge.m_sedge->m_kind == SUPEREDGE_RETURN);

  m_src_snode = eedge.m_src->get_supernode ();
  m_dest_snode = eedge.m_dest->get_supernode ();
}

/* Implementation of diagnostic_event::get_desc vfunc for
   return_event.

   If this return event returns critical state for an sm-based warning,
   allow the diagnostic to generate a precise description, such as:

      "possible of NULL to 'foo' from 'bar'"

   Otherwise, generate a description of the form
   "returning to 'foo' from 'bar'.  */

label_text
return_event::get_desc (bool can_colorize) const
{
  /*  For greatest precision-of-wording, if this is returning the
      state involved in the pending diagnostic, give the pending
      diagnostic a chance to describe this return (in terms of
      itself).  */
  if (m_critical_state && m_pending_diagnostic)
    {
      label_text custom_desc
	= m_pending_diagnostic->describe_return_of_state
	    (evdesc::return_of_state (can_colorize,
				      m_dest_snode->m_fun->decl,
				      m_src_snode->m_fun->decl,
				      m_critical_state));
      if (custom_desc.get ())
	return custom_desc;
    }
  return make_label_text (can_colorize,
			  "returning to %qE from %qE",
			  m_dest_snode->m_fun->decl,
			  m_src_snode->m_fun->decl);
}

/* Implementation of diagnostic_event::get_meaning vfunc for
   function return events.  */

diagnostic_event::meaning
return_event::get_meaning () const
{
  return meaning (VERB_return, NOUN_function);
}

/* Override of checker_event::is_return_p for returns.  */

bool
return_event::is_return_p () const
{
  return true;
}

/* class start_consolidated_cfg_edges_event : public checker_event.  */

label_text
start_consolidated_cfg_edges_event::get_desc (bool can_colorize) const
{
  return make_label_text (can_colorize,
			  "following %qs branch...",
			  m_edge_sense ? "true" : "false");
}

/* Implementation of diagnostic_event::get_meaning vfunc for
   start_consolidated_cfg_edges_event.  */

diagnostic_event::meaning
start_consolidated_cfg_edges_event::get_meaning () const
{
  return meaning (VERB_branch,
		  (m_edge_sense ? PROPERTY_true : PROPERTY_false));
}

/* class inlined_call_event : public checker_event.  */

label_text
inlined_call_event::get_desc (bool can_colorize) const
{
  return make_label_text (can_colorize,
			  "inlined call to %qE from %qE",
			  m_apparent_callee_fndecl,
			  m_apparent_caller_fndecl);
}

/* Implementation of diagnostic_event::get_meaning vfunc for
   reconstructed inlined function calls.  */

diagnostic_event::meaning
inlined_call_event::get_meaning () const
{
  return meaning (VERB_call, NOUN_function);
}

/* class setjmp_event : public checker_event.  */

/* Implementation of diagnostic_event::get_desc vfunc for
   setjmp_event.  */

label_text
setjmp_event::get_desc (bool can_colorize) const
{
  return make_label_text (can_colorize,
			  "%qs called here",
			  get_user_facing_name (m_setjmp_call));
}

/* Implementation of checker_event::prepare_for_emission vfunc for setjmp_event.

   Record this setjmp's event ID into the path, so that rewind events can
   use it.  */

void
setjmp_event::prepare_for_emission (checker_path *path,
				    pending_diagnostic *pd,
				    diagnostic_event_id_t emission_id)
{
  checker_event::prepare_for_emission (path, pd, emission_id);
  path->record_setjmp_event (m_enode, emission_id);
}

/* class rewind_event : public checker_event.  */

/* Get the fndecl containing the site of the longjmp call.  */

tree
rewind_event::get_longjmp_caller () const
{
  return m_eedge->m_src->get_function ()->decl;
}

/* Get the fndecl containing the site of the setjmp call.  */

tree
rewind_event::get_setjmp_caller () const
{
  return m_eedge->m_dest->get_function ()->decl;
}

/* rewind_event's ctor.  */

rewind_event::rewind_event (const exploded_edge *eedge,
			    enum event_kind kind,
			    location_t loc, tree fndecl, int depth,
			    const rewind_info_t *rewind_info)
: checker_event (kind, loc, fndecl, depth),
  m_rewind_info (rewind_info),
  m_eedge (eedge)
{
  gcc_assert (m_eedge->m_custom_info == m_rewind_info);
}

/* class rewind_from_longjmp_event : public rewind_event.  */

/* Implementation of diagnostic_event::get_desc vfunc for
   rewind_from_longjmp_event.  */

label_text
rewind_from_longjmp_event::get_desc (bool can_colorize) const
{
  const char *src_name
    = get_user_facing_name (m_rewind_info->get_longjmp_call ());

  if (get_longjmp_caller () == get_setjmp_caller ())
    /* Special-case: purely intraprocedural rewind.  */
    return make_label_text (can_colorize,
			    "rewinding within %qE from %qs...",
			    get_longjmp_caller (),
			    src_name);
  else
    return make_label_text (can_colorize,
			    "rewinding from %qs in %qE...",
			    src_name,
			    get_longjmp_caller ());
}

/* class rewind_to_setjmp_event : public rewind_event.  */

/* Implementation of diagnostic_event::get_desc vfunc for
   rewind_to_setjmp_event.  */

label_text
rewind_to_setjmp_event::get_desc (bool can_colorize) const
{
  const char *dst_name
    = get_user_facing_name (m_rewind_info->get_setjmp_call ());

  /* If we can, identify the ID of the setjmp_event.  */
  if (m_original_setjmp_event_id.known_p ())
    {
      if (get_longjmp_caller () == get_setjmp_caller ())
	/* Special-case: purely intraprocedural rewind.  */
	return make_label_text (can_colorize,
				"...to %qs (saved at %@)",
				dst_name,
				&m_original_setjmp_event_id);
      else
	return make_label_text (can_colorize,
				"...to %qs in %qE (saved at %@)",
				dst_name,
				get_setjmp_caller (),
				&m_original_setjmp_event_id);
    }
  else
    {
      if (get_longjmp_caller () == get_setjmp_caller ())
	/* Special-case: purely intraprocedural rewind.  */
	return make_label_text (can_colorize,
				"...to %qs",
				dst_name,
				get_setjmp_caller ());
      else
	return make_label_text (can_colorize,
				"...to %qs in %qE",
				dst_name,
				get_setjmp_caller ());
    }
}

/* Implementation of checker_event::prepare_for_emission vfunc for
   rewind_to_setjmp_event.

   Attempt to look up the setjmp event ID that recorded the jmp_buf
   for this rewind.  */

void
rewind_to_setjmp_event::prepare_for_emission (checker_path *path,
					      pending_diagnostic *pd,
					      diagnostic_event_id_t emission_id)
{
  checker_event::prepare_for_emission (path, pd, emission_id);
  path->get_setjmp_event (m_rewind_info->get_enode_origin (),
			  &m_original_setjmp_event_id);
}

/* class warning_event : public checker_event.  */

/* Implementation of diagnostic_event::get_desc vfunc for
   warning_event.

   If the pending diagnostic implements describe_final_event, use it,
   generating a precise description e.g.
     "second 'free' here; first 'free' was at (7)"

   Otherwise generate a generic description.  */

label_text
warning_event::get_desc (bool can_colorize) const
{
  if (m_pending_diagnostic)
    {
      tree var = fixup_tree_for_diagnostic (m_var);
      label_text ev_desc
	= m_pending_diagnostic->describe_final_event
	    (evdesc::final_event (can_colorize, var, m_state));
      if (ev_desc.get ())
	{
	  if (m_sm && flag_analyzer_verbose_state_changes)
	    {
	      if (var)
		return make_label_text (can_colorize,
					"%s (%qE is in state %qs)",
					ev_desc.get (),
					var, m_state->get_name ());
	      else
		return make_label_text (can_colorize,
					"%s (in global state %qs)",
					ev_desc.get (),
					m_state->get_name ());
	    }
	  else
	    return ev_desc;
	}
    }

  if (m_sm)
    {
      if (m_var)
	return make_label_text (can_colorize,
				"here (%qE is in state %qs)",
				m_var, m_state->get_name ());
      else
	return make_label_text (can_colorize,
				"here (in global state %qs)",
				m_state->get_name ());
    }
  else
    return label_text::borrow ("here");
}

/* Implementation of diagnostic_event::get_meaning vfunc for
   warning_event.  */

diagnostic_event::meaning
warning_event::get_meaning () const
{
  return meaning (VERB_danger, NOUN_unknown);
}

/* Print a single-line representation of this path to PP.  */

void
checker_path::dump (pretty_printer *pp) const
{
  pp_character (pp, '[');

  checker_event *e;
  int i;
  FOR_EACH_VEC_ELT (m_events, i, e)
    {
      if (i > 0)
	pp_string (pp, ", ");
      label_text event_desc (e->get_desc (false));
      pp_printf (pp, "\"%s\"", event_desc.get ());
    }
  pp_character (pp, ']');
}

/* Print a multiline form of this path to LOGGER, prefixing it with DESC.  */

void
checker_path::maybe_log (logger *logger, const char *desc) const
{
  if (!logger)
    return;
  logger->start_log_line ();
  logger->log_partial ("%s: ", desc);
  dump (logger->get_printer ());
  logger->end_log_line ();
  for (unsigned i = 0; i < m_events.length (); i++)
    {
      logger->start_log_line ();
      logger->log_partial ("%s[%i]: %s ", desc, i,
			   event_kind_to_string (m_events[i]->m_kind));
      m_events[i]->dump (logger->get_printer ());
      logger->end_log_line ();
    }
}

/* Print a multiline form of this path to STDERR.  */

DEBUG_FUNCTION void
checker_path::debug () const
{
  checker_event *e;
  int i;
  FOR_EACH_VEC_ELT (m_events, i, e)
    {
      label_text event_desc (e->get_desc (false));
      fprintf (stderr,
	       "[%i]: %s \"%s\"\n",
	       i,
	       event_kind_to_string (m_events[i]->m_kind),
	       event_desc.get ());
    }
}

/* Add region_creation_event instance to this path for REG,
   describing whether REG is on the stack or heap.  */

void
checker_path::add_region_creation_event (const region *reg,
					 location_t loc,
					 tree fndecl, int depth)
{
  add_event (new region_creation_event (reg, loc, fndecl, depth));
}

/* Add a warning_event to the end of this path.  */

void
checker_path::add_final_event (const state_machine *sm,
			       const exploded_node *enode, const gimple *stmt,
			       tree var, state_machine::state_t state)
{
  checker_event *end_of_path
    = new warning_event (get_stmt_location (stmt, enode->get_function ()),
			 enode->get_function ()->decl,
			 enode->get_stack_depth (),
			 sm, var, state);
  add_event (end_of_path);
}

void
checker_path::fixup_locations (pending_diagnostic *pd)
{
  for (checker_event *e : m_events)
    e->set_location (pd->fixup_location (e->get_location ()));
}

/* Return true if there is a (start_cfg_edge_event, end_cfg_edge_event) pair
   at (IDX, IDX + 1).  */

bool
checker_path::cfg_edge_pair_at_p (unsigned idx) const
{
  if (m_events.length () < idx + 1)
    return false;
  return (m_events[idx]->m_kind == EK_START_CFG_EDGE
	  && m_events[idx + 1]->m_kind == EK_END_CFG_EDGE);
}

/* Consider a call from "outer" to "middle" which calls "inner",
   where "inner" and "middle" have been inlined into "outer".

   We expect the stmt locations for the inlined stmts to have a
   chain like:

     [{fndecl: inner},
      {fndecl: middle, callsite: within middle to inner},
      {fndecl: outer, callsite: without outer to middle}]

   The location for the stmt will already be fixed up to reflect
   the two extra frames, so that we have e.g. this as input
   (for gcc.dg/analyzer/inlining-4.c):

    before[0]:
      EK_FUNCTION_ENTRY "entry to ‘outer’"
      (depth 1, fndecl ‘outer’, m_loc=511c4)
    before[1]:
      EK_START_CFG_EDGE "following ‘true’ branch (when ‘flag != 0’)..."
      (depth 3 corrected from 1,
       fndecl ‘inner’ corrected from ‘outer’, m_loc=8000000f)
    before[2]:
      EK_END_CFG_EDGE "...to here"
      (depth 1, fndecl ‘outer’, m_loc=0)
    before[3]:
      EK_WARNING "here (‘<unknown>’ is in state ‘null’)"
      (depth 1, fndecl ‘outer’, m_loc=80000004)

   We want to add inlined_call_events showing the calls, so that
   the above becomes:

    after[0]:
      EK_FUNCTION_ENTRY "entry to ‘outer’"
      (depth 1, fndecl ‘outer’, m_loc=511c4)
    after[1]:
      EK_INLINED_CALL "inlined call to ‘middle’ from ‘outer’"
      (depth 1, fndecl ‘outer’, m_loc=53300)
    after[2]:
      EK_INLINED_CALL "inlined call to ‘inner’ from ‘middle’"
      (depth 2, fndecl ‘middle’, m_loc=4d2e0)
    after[3]:
      EK_START_CFG_EDGE "following ‘true’ branch (when ‘flag != 0’)..."
      (depth 3 corrected from 1,
       fndecl ‘inner’ corrected from ‘outer’, m_loc=8000000f)
    after[4]: EK_END_CFG_EDGE "...to here"
      (depth 1, fndecl ‘outer’, m_loc=0)
    after[5]: EK_WARNING "here (‘<unknown>’ is in state ‘null’)"
      (depth 1, fndecl ‘outer’, m_loc=80000004)

    where we've added events between before[0] and before[1] to show
    the inlined calls leading to the effective stack depths, making
    the generated path much easier for a user to read.

    Note how in the above we have a branch (before[1] to before[2])
    where the locations were originally in different functions.
    Hence we have to add these events quite late when generating
    checker_path.  */

void
checker_path::inject_any_inlined_call_events (logger *logger)
{
  LOG_SCOPE (logger);

  if (!flag_analyzer_undo_inlining)
    return;

  /* Build a copy of m_events with the new events inserted.  */
  auto_vec<checker_event *> updated_events;

  maybe_log (logger, "before");

  hash_set<tree> blocks_in_prev_event;

  for (unsigned ev_idx = 0; ev_idx < m_events.length (); ev_idx++)
    {
      checker_event *curr_event = m_events[ev_idx];
      location_t curr_loc = curr_event->get_location ();
      hash_set<tree> blocks_in_curr_event;

      if (logger)
	{
	  logger->start_log_line ();
	  logger->log_partial ("event[%i]: %s ", ev_idx,
			       event_kind_to_string (curr_event->m_kind));
	  curr_event->dump (logger->get_printer ());
	  logger->end_log_line ();
	  for (inlining_iterator iter (curr_event->get_location ());
	       !iter.done_p (); iter.next ())
	    {
	      logger->start_log_line ();
	      logger->log_partial ("  %qE (%p), fndecl: %qE, callsite: 0x%x",
				   iter.get_block (), iter.get_block (),
				   iter.get_fndecl (), iter.get_callsite ());
	      if (iter.get_callsite ())
		dump_location (logger->get_printer (), iter.get_callsite ());
	      logger->end_log_line ();
	    }
	}

      /* We want to add events to show inlined calls.

	 We want to show changes relative to the previous event, omitting
	 the commonality between the inlining chain.

	 The chain is ordered from innermost frame to outermost frame;
	 we want to walk it backwards to show the calls, so capture it
	 in a vec.  */
      struct chain_element { tree m_block; tree m_fndecl; };
      auto_vec<chain_element> elements;
      for (inlining_iterator iter (curr_loc); !iter.done_p (); iter.next ())
	{
	  chain_element ce;
	  ce.m_block = iter.get_block ();
	  ce.m_fndecl = iter.get_fndecl ();

	  if (!blocks_in_prev_event.contains (ce.m_block))
	    elements.safe_push (ce);
	  blocks_in_curr_event.add (ce.m_block);
	}

      /* Walk from outermost to innermost.  */
      if (elements.length () > 0)
	{
	  int orig_stack_depth = curr_event->get_original_stack_depth ();
	  for (unsigned element_idx = elements.length () - 1; element_idx > 0;
	       element_idx--)
	    {
	      const chain_element &ce = elements[element_idx];
	      int stack_depth_adjustment
		= (blocks_in_curr_event.elements () - element_idx) - 1;
	      if (location_t callsite = BLOCK_SOURCE_LOCATION (ce.m_block))
		updated_events.safe_push
		  (new inlined_call_event (callsite,
					   elements[element_idx - 1].m_fndecl,
					   ce.m_fndecl,
					   orig_stack_depth,
					   stack_depth_adjustment));
	    }
	}

      /* Ideally we'd use assignment here:
	   blocks_in_prev_event = blocks_in_curr_event; */
      blocks_in_prev_event.empty ();
      for (auto iter : blocks_in_curr_event)
	blocks_in_prev_event.add (iter);

      /* Add the existing event.  */
      updated_events.safe_push (curr_event);
    }

  /* Replace m_events with updated_events.  */
  m_events.truncate (0);
  m_events.safe_splice (updated_events);

  maybe_log (logger, " after");
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
