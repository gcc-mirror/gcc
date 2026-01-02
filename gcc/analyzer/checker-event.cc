/* Subclasses of diagnostics::paths::event for analyzer diagnostics.
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

#include "analyzer/common.h"

#include "gimple-pretty-print.h"
#include "sbitmap.h"
#include "ordered-hash-map.h"
#include "fold-const.h"
#include "gimple-iterator.h"
#include "inlining-iterator.h"
#include "tree-logical-location.h"
#include "diagnostics/sarif-sink.h"
#include "diagnostics/state-graphs.h"
#include "custom-sarif-properties/state-graphs.h"

#include "analyzer/analyzer-logging.h"
#include "analyzer/sm.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"
#include "analyzer/program-state.h"
#include "analyzer/checker-path.h"
#include "analyzer/supergraph.h"
#include "analyzer/pending-diagnostic.h"
#include "analyzer/diagnostic-manager.h"
#include "analyzer/constraint-manager.h"
#include "analyzer/checker-event.h"
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
    case event_kind::debug:
      return "debug";
    case event_kind::custom:
      return "custom";
    case event_kind::stmt:
      return "stmt";
    case event_kind::region_creation:
      return "region_creation";
    case event_kind::function_entry:
      return "function_entry";
    case event_kind::state_change:
      return "state_change";
    case event_kind::start_cfg_edge:
      return "start_cfg_edge";
    case event_kind::end_cfg_edge:
      return "end_cfg_edge";
    case event_kind::catch_:
      return "catch";
    case event_kind::call_:
      return "call";
    case event_kind::return_:
      return "return";
    case event_kind::start_consolidated_cfg_edges:
      return "start_consolidated_cfg_edges";
    case event_kind::end_consolidated_cfg_edges:
      return "end_consolidated_cfg_edges";
    case event_kind::inlined_call:
      return "inlined_call";
    case event_kind::setjmp_:
      return "setjmp";
    case event_kind::rewind_from_longjmp:
      return "rewind_from_longjmp";
    case event_kind::rewind_to_setjmp:
      return "rewind_to_setjmp";
    case event_kind::throw_:
      return "throw";
    case event_kind::unwind:
      return "unwind";
    case event_kind::warning:
      return "warning";
    }
}

/* class checker_event : public diagnostics::paths::event.  */

/* checker_event's ctor.  */

checker_event::checker_event (enum event_kind kind,
			      const event_loc_info &loc_info)
: m_path (nullptr),
  m_kind (kind), m_loc (loc_info.m_loc),
  m_original_fndecl (loc_info.m_fndecl),
  m_effective_fndecl (loc_info.m_fndecl),
  m_original_depth (loc_info.m_depth),
  m_effective_depth (loc_info.m_depth),
  m_pending_diagnostic (nullptr), m_emission_id (),
  m_logical_loc
    (tree_logical_location_manager::key_from_tree (loc_info.m_fndecl))
{
  /* Update effective fndecl and depth if inlining has been recorded.  */
  if (flag_analyzer_undo_inlining)
    {
      inlining_info info (m_loc);
      if (info.get_inner_fndecl ())
	{
	  m_effective_fndecl = info.get_inner_fndecl ();
	  m_effective_depth += info.get_extra_frames ();
	  m_logical_loc
	    = tree_logical_location_manager::key_from_tree (m_effective_fndecl);
	}
    }
}

/* No-op implementation of diagnostics::paths::event::get_meaning vfunc for
   checker_event: checker events have no meaning by default.  */

diagnostics::paths::event::meaning
checker_event::get_meaning () const
{
  return diagnostics::paths::event::meaning ();
}

/* Implementation of diagnostics::paths::event::maybe_add_sarif_properties
   for checker_event.  */

void
checker_event::
maybe_add_sarif_properties (diagnostics::sarif_builder &builder,
			    diagnostics::sarif_object &thread_flow_loc_obj) const
{
  auto &props = thread_flow_loc_obj.get_or_create_properties ();
#define PROPERTY_PREFIX "gcc/analyzer/checker_event/"
  props.set (PROPERTY_PREFIX "emission_id",
	     diagnostic_event_id_to_json  (m_emission_id));
  props.set_string (PROPERTY_PREFIX "kind", event_kind_to_string (m_kind));

  if (m_original_fndecl != m_effective_fndecl)
    props.set_logical_location
      (PROPERTY_PREFIX "original_fndecl",
       builder,
       tree_logical_location_manager::key_from_tree (m_original_fndecl));

  if (m_original_depth != m_effective_depth)
    props.set_integer (PROPERTY_PREFIX "original_depth", m_original_depth);
#undef PROPERTY_PREFIX
}

/* Dump this event to PP (for debugging/logging purposes).  */

void
checker_event::dump (pretty_printer *pp) const
{
  pp_character (pp, '"');
  print_desc (*pp);
  pp_printf (pp, "\" (depth %i", m_effective_depth);

  if (m_effective_depth != m_original_depth)
    pp_printf (pp, " corrected from %i",
	       m_original_depth);
  if (m_effective_fndecl)
    {
      pp_printf (pp, ", fndecl %qE", m_effective_fndecl);
      if (m_effective_fndecl != m_original_fndecl)
	pp_printf (pp, " corrected from %qE", m_original_fndecl);
    }
  pp_printf (pp, ", m_loc=%llx)",
	     (unsigned long long) get_location ());
}

/* Dump this event to stderr (for debugging/logging purposes).  */

DEBUG_FUNCTION void
checker_event::debug () const
{
  tree_dump_pretty_printer pp (stderr);
  dump (&pp);
  pp_newline (&pp);
}

/* Hook for being notified when this event has its final id EMISSION_ID
   and is about to emitted for PD.

   Base implementation of checker_event::prepare_for_emission vfunc;
   subclasses that override this should chain up to it.

   Record PD and EMISSION_ID, and call the print_desc vfunc, so that any
   side-effects of the call to print_desc take place before
   pending_diagnostic::emit is called.

   For example, state_change_event::print_desc can call
   pending_diagnostic::describe_state_change; free_of_non_heap can use this
   to tweak the message (TODO: would be neater to simply capture the
   pertinent data within the sm-state).  */

void
checker_event::prepare_for_emission (checker_path *path,
				     pending_diagnostic *pd,
				     diagnostics::paths::event_id_t emission_id)
{
  m_path = path;
  m_pending_diagnostic = pd;
  m_emission_id = emission_id;

  auto pp = global_dc->clone_printer ();
  print_desc (*pp.get ());
}

std::unique_ptr<diagnostics::digraphs::digraph>
checker_event::maybe_make_diagnostic_state_graph (bool debug) const
{
  const program_state *state = get_program_state ();
  if (!state)
    return nullptr;

  gcc_assert (m_path);
  const extrinsic_state &ext_state = m_path->get_ext_state ();

  auto result = state->make_diagnostic_state_graph (ext_state);

  if (debug)
    {
      pretty_printer pp;
      text_art::theme *theme = global_dc->get_diagram_theme ();
      text_art::dump_to_pp (*state, theme, &pp);
      const json::string_property program_state_property
	(custom_sarif_properties::state_graphs::graph::prefix,
	 "analyzer/program_state/");
      result->set_property (program_state_property,
			    pp_formatted_text (&pp));
    }

  return result;
}

/* class debug_event : public checker_event.  */

/* Implementation of diagnostics::paths::event::print_desc vfunc for
   debug_event.
   Use the saved string as the event's description.  */

void
debug_event::print_desc (pretty_printer &pp) const
{
  pp_string (&pp, m_desc);
}

/* class precanned_custom_event : public custom_event.  */

/* Implementation of diagnostics::paths::event::print_desc vfunc for
   precanned_custom_event.
   Use the saved string as the event's description.  */

void
precanned_custom_event::print_desc (pretty_printer &pp) const
{
  pp_string (&pp, m_desc);
}

/* class statement_event : public checker_event.  */

/* statement_event's ctor.  */

statement_event::statement_event (const gimple *stmt, tree fndecl, int depth,
				  const program_state &dst_state)
: checker_event (event_kind::stmt,
		 event_loc_info (gimple_location (stmt), fndecl, depth)),
  m_stmt (stmt),
  m_dst_state (dst_state)
{
}

/* Implementation of diagnostics::paths::event::print_desc vfunc for
   statement_event.
   Use the statement's dump form as the event's description.  */

void
statement_event::print_desc (pretty_printer &pp) const
{
  pp_string (&pp, "stmt: ");
  pp_gimple_stmt_1 (&pp, m_stmt, 0, (dump_flags_t)0);
}

/* class region_creation_event : public checker_event.  */

region_creation_event::region_creation_event (const event_loc_info &loc_info)
: checker_event (event_kind::region_creation, loc_info)
{
}

/* The various region_creation_event subclasses' print_desc
   implementations.  */

void
region_creation_event_memory_space::print_desc (pretty_printer &pp) const
{
  switch (m_mem_space)
    {
    default:
      pp_string (&pp, "region created here");
      return;
    case MEMSPACE_STACK:
      pp_string (&pp, "region created on stack here");
      return;
    case MEMSPACE_HEAP:
      pp_string (&pp, "region created on heap here");
      return;
    }
}

void
region_creation_event_capacity::print_desc (pretty_printer &pp) const
{
  gcc_assert (m_capacity);
  if (TREE_CODE (m_capacity) == INTEGER_CST)
    {
      unsigned HOST_WIDE_INT hwi = tree_to_uhwi (m_capacity);
      return pp_printf_n (&pp,
			  hwi,
			  "capacity: %wu byte",
			  "capacity: %wu bytes",
			  hwi);
    }
  else
    return pp_printf (&pp, "capacity: %qE bytes", m_capacity);
}

void
region_creation_event_allocation_size::print_desc (pretty_printer &pp) const
{
  if (m_capacity)
    {
      if (TREE_CODE (m_capacity) == INTEGER_CST)
	pp_printf_n (&pp,
		     tree_to_uhwi (m_capacity),
		     "allocated %E byte here",
		     "allocated %E bytes here",
		     m_capacity);
      else
	pp_printf (&pp,
		   "allocated %qE bytes here",
		   m_capacity);
    }
  else
    pp_printf (&pp, "allocated here");
}

void
region_creation_event_debug::print_desc (pretty_printer &pp) const
{
  pp_string (&pp, "region creation: ");
  m_reg->dump_to_pp (&pp, true);
  if (m_capacity)
    pp_printf (&pp, " capacity: %qE", m_capacity);
}

/* class function_entry_event : public checker_event.  */

function_entry_event::function_entry_event (const program_point &dst_point,
					    const program_state &state)
: checker_event (event_kind::function_entry,
		 event_loc_info (dst_point.get_location (),
				 dst_point.get_fndecl (),
				 dst_point.get_stack_depth ())),
  m_state (state)
{
}

/* Implementation of diagnostics::paths::event::print_desc vfunc for
   function_entry_event.

   Use a string such as "entry to 'foo'" as the event's description.  */

void
function_entry_event::print_desc (pretty_printer &pp) const
{
  pp_printf (&pp, "entry to %qE", m_effective_fndecl);
}

/* Implementation of diagnostics::paths::event::get_meaning vfunc for
   function entry.  */

diagnostics::paths::event::meaning
function_entry_event::get_meaning () const
{
  return meaning (verb::enter, noun::function);
}

/* class state_change_event : public checker_event.  */

/* state_change_event's ctor.  */

state_change_event::state_change_event (const event_loc_info &loc_info,
					const gimple *stmt,
					const state_machine &sm,
					const svalue *sval,
					state_machine::state_t from,
					state_machine::state_t to,
					const svalue *origin,
					const program_state &dst_state,
					const exploded_node *enode)
: checker_event (event_kind::state_change, loc_info),
  m_stmt (stmt),
  m_sm (sm),
  m_sval (sval), m_from (from), m_to (to),
  m_origin (origin),
  m_dst_state (dst_state),
  m_enode (enode)
{
}

/* Implementation of diagnostics::paths::event::print_desc vfunc for
   state_change_event.

   Attempt to generate a nicer human-readable description.
   For greatest precision-of-wording, give the pending diagnostic
   a chance to describe this state change (in terms of the
   diagnostic).
   Note that we only have a pending_diagnostic set on the event once
   the diagnostic is about to being emitted, so the description for
   an event can change.  */

void
state_change_event::print_desc (pretty_printer &pp) const
{
  if (m_pending_diagnostic)
    {
      region_model *model = m_dst_state.m_region_model;
      tree var = model->get_representative_tree (m_sval);
      tree origin = model->get_representative_tree (m_origin);
      evdesc::state_change evd (var, origin,
				m_from, m_to, m_emission_id, *this);
      if (m_pending_diagnostic->describe_state_change (pp, evd))
	{
	  if (flag_analyzer_verbose_state_changes)
	    {
	      /* Append debugging information about this event.  */

	      if (var)
		pp_printf (&pp, " (state of %qE: ", var);
	      else
		pp_string (&pp, " (state: ");

	      pp_printf (&pp, "%qs -> %qs, ",
			 m_from->get_name (),
			 m_to->get_name ());

	      if (m_origin)
		pp_printf (&pp, "origin: %qE", origin);
	      else
		pp_string (&pp, "NULL origin");

	      /* Get any "meaning" of event.  */
	      diagnostics::paths::event::meaning meaning = get_meaning ();
	      pp_string (&pp, ", meaning: ");
	      meaning.dump_to_pp (&pp);
	      pp_string (&pp, ")");
	    }
	  return;
	}
    }

  /* Fallback description.  */
  if (m_sval)
    {
      label_text sval_desc = m_sval->get_desc ();
      pp_printf (&pp,
		 "state of %qs: %qs -> %qs",
		 sval_desc.get (),
		 m_from->get_name (),
		 m_to->get_name ());
      if (m_origin)
	{
	  label_text origin_desc = m_origin->get_desc ();
	  pp_printf (&pp, " (origin: %qs)",
		     origin_desc.get ());
	}
      else
	pp_string (&pp, " (NULL origin)");
    }
  else
    {
      gcc_assert (m_origin == nullptr);
      pp_printf (&pp,
		 "global state: %qs -> %qs",
		 m_from->get_name (),
		 m_to->get_name ());
    }
}

/* Implementation of diagnostics::paths::event::get_meaning vfunc for
   state change events: delegate to the pending_diagnostic to
   get any meaning.  */

diagnostics::paths::event::meaning
state_change_event::get_meaning () const
{
  if (m_pending_diagnostic)
    {
      region_model *model = m_dst_state.m_region_model;
      tree var = model->get_representative_tree (m_sval);
      tree origin = model->get_representative_tree (m_origin);
      evdesc::state_change evd (var, origin,
				m_from, m_to, m_emission_id, *this);
      return m_pending_diagnostic->get_meaning_for_state_change (evd);
    }
  else
    return meaning ();
}

/* class superedge_event : public checker_event.  */

/* Implementation of diagnostics::paths::event::maybe_add_sarif_properties
   for superedge_event.  */

void
superedge_event::
maybe_add_sarif_properties (diagnostics::sarif_builder &builder,
			    diagnostics::sarif_object &thread_flow_loc_obj)
  const
{
  checker_event::maybe_add_sarif_properties (builder, thread_flow_loc_obj);
  auto &props = thread_flow_loc_obj.get_or_create_properties ();
#define PROPERTY_PREFIX "gcc/analyzer/superedge_event/"
  if (m_sedge)
    props.set (PROPERTY_PREFIX "superedge", m_sedge->to_json ());
#undef PROPERTY_PREFIX
}

/* Determine if this event should be filtered at the given verbosity
   level.  */

bool
superedge_event::should_filter_p (int verbosity) const
{
  if (m_sedge->get_any_cfg_edge ())
    {
      if (verbosity < 2)
	return true;

      if (verbosity < 4)
	{
	  /* Filter events with empty descriptions.  This ought to filter
	     FALLTHRU, but retain true/false/switch edges.  */
	  auto pp = global_dc->clone_printer ();
	  print_desc (*pp.get ());
	  if (pp_formatted_text (pp.get ()) [0] == '\0')
	    return true;
	}
    }
  return false;
}

const program_state *
superedge_event::get_program_state () const
{
  return &m_eedge.m_dest->get_state ();
}

const call_and_return_op *
superedge_event::get_call_and_return_op () const
{
  if (m_sedge)
    if (auto base_op = m_sedge->get_op ())
      return base_op->dyn_cast_call_and_return_op ();
  return nullptr;
}

/* superedge_event's ctor.  */

superedge_event::superedge_event (enum event_kind kind,
				  const exploded_edge &eedge,
				  const event_loc_info &loc_info)
: checker_event (kind, loc_info),
  m_eedge (eedge), m_sedge (eedge.m_sedge)
{
  gcc_assert (m_sedge);
}

/* class cfg_edge_event : public superedge_event.  */

/* cfg_edge_event's ctor.  */

cfg_edge_event::cfg_edge_event (enum event_kind kind,
				const exploded_edge &eedge,
				const event_loc_info &loc_info,
				const control_flow_op *op)
: superedge_event (kind, eedge, loc_info),
  m_op (op)
{
}

/* Implementation of diagnostics::paths::event::get_meaning vfunc for
   CFG edge events.  */

diagnostics::paths::event::meaning
cfg_edge_event::get_meaning () const
{
  if (::edge e = get_cfg_edge ())
    {
      if (e->flags & EDGE_TRUE_VALUE)
	return meaning (verb::branch, property::true_);
      else if (e->flags & EDGE_FALSE_VALUE)
	return meaning (verb::branch, property::false_);
    }
  return meaning ();
}

::edge
cfg_edge_event::get_cfg_edge () const
{
  return m_sedge->get_any_cfg_edge ();
}

/* class start_cfg_edge_event : public cfg_edge_event.  */

/* Implementation of diagnostics::paths::event::print_desc vfunc for
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

   Failing that, print nothing (which will lead to this event
   being filtered).  */

void
start_cfg_edge_event::print_desc (pretty_printer &pp) const
{
  bool user_facing = !flag_analyzer_verbose_edges;
  label_text edge_desc (m_sedge->get_description (user_facing));
  if (user_facing)
    {
      if (edge_desc.get ()
	  && strlen (edge_desc.get ()) > 0
	  && m_op)
	{
	  label_text cond_desc
	    = m_op->maybe_describe_condition (pp_show_color (&pp));
	  label_text result;
	  if (cond_desc.get ())
	    pp_printf (&pp,
		       "following %qs branch (%s)...",
		       edge_desc.get (), cond_desc.get ());
	  else
	    pp_printf (&pp,
		       "following %qs branch...",
		       edge_desc.get ());
	}
    }
  else
    {
      if (strlen (edge_desc.get ()) > 0)
	return pp_printf (&pp,
			  "taking %qs edge SN:%i -> SN:%i",
			  edge_desc.get (),
			  m_sedge->m_src->m_id,
			  m_sedge->m_dest->m_id);
      else
	return pp_printf (&pp,
			  "taking edge SN:%i -> SN:%i",
			  m_sedge->m_src->m_id,
			  m_sedge->m_dest->m_id);
    }
}

/* class catch_cfg_edge_event : public cfg_edge_event.  */

diagnostics::paths::event::meaning
catch_cfg_edge_event::get_meaning () const
{
  return meaning (verb::catch_);
}

/* class call_event : public superedge_event.  */

/* call_event's ctor.  */

call_event::call_event (const exploded_edge &eedge,
			const event_loc_info &loc_info)
: superedge_event (event_kind::call_, eedge, loc_info)
{
   m_src_snode = eedge.m_src->get_supernode ();
   m_dest_snode = eedge.m_dest->get_supernode ();
}

/* Implementation of diagnostics::paths::event::print_desc vfunc for
   call_event.

   If this call event passes critical state for an sm-based warning,
   allow the diagnostic to generate a precise description, such as:

     "passing freed pointer 'ptr' in call to 'foo' from 'bar'"

   Otherwise, generate a description of the form
   "calling 'foo' from 'bar'".  */

void
call_event::print_desc (pretty_printer &pp) const
{
  if (m_critical_state.m_state && m_pending_diagnostic)
    {
      gcc_assert (m_critical_state.m_var);
      tree var = fixup_tree_for_diagnostic (m_critical_state.m_var);
      evdesc::call_with_state evd (m_src_snode->m_fun->decl,
				   m_dest_snode->m_fun->decl,
				   var,
				   m_critical_state.m_state);
      if (m_pending_diagnostic->describe_call_with_state (pp, evd))
	return;
    }

  pp_printf (&pp,
	     "calling %qE from %qE",
	     get_callee_fndecl (),
	     get_caller_fndecl ());
}

/* Implementation of diagnostics::paths::event::get_meaning vfunc for
   function call events.  */

diagnostics::paths::event::meaning
call_event::get_meaning () const
{
  return meaning (verb::call, noun::function);
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

const program_state *
call_event::get_program_state () const
{
  /* Use the state at the source (at the caller),
     rather than the one at the dest, which has a frame for the callee.  */
  return &m_eedge.m_src->get_state ();
}

/* class return_event : public checker_event.  */

/* return_event's ctor.  */

return_event::return_event (const exploded_edge &eedge,
			    const event_loc_info &loc_info)
: checker_event (event_kind::return_, loc_info),
  m_eedge (eedge)
{
  m_src_snode = eedge.m_src->get_supernode ();
  m_dest_snode = eedge.m_dest->get_supernode ();
  m_call_and_return_op
    = eedge.m_src->get_point ().get_call_string ().get_top_of_stack ().m_call_op;
}

/* Implementation of diagnostics::paths::event::print_desc vfunc for
   return_event.

   If this return event returns critical state for an sm-based warning,
   allow the diagnostic to generate a precise description, such as:

      "possible of NULL to 'foo' from 'bar'"

   Otherwise, generate a description of the form
   "returning to 'foo' from 'bar'.  */

void
return_event::print_desc (pretty_printer &pp) const
{
  /*  For greatest precision-of-wording, if this is returning the
      state involved in the pending diagnostic, give the pending
      diagnostic a chance to describe this return (in terms of
      itself).  */
  if (m_critical_state.m_state && m_pending_diagnostic)
    {
      evdesc::return_of_state evd (m_dest_snode->m_fun->decl,
				   m_src_snode->m_fun->decl,
				   m_critical_state.m_state);
      if (m_pending_diagnostic->describe_return_of_state (pp, evd))
	return;
    }
  pp_printf (&pp,
	     "returning to %qE from %qE",
	     m_dest_snode->m_fun->decl,
	     m_src_snode->m_fun->decl);
}

/* Implementation of diagnostics::paths::event::get_meaning vfunc for
   function return events.  */

diagnostics::paths::event::meaning
return_event::get_meaning () const
{
  return meaning (verb::return_, noun::function);
}

/* Override of checker_event::is_return_p for returns.  */

bool
return_event::is_return_p () const
{
  return true;
}

const program_state *
return_event::get_program_state () const
{
  return &m_eedge.m_dest->get_state ();
}

/* class start_consolidated_cfg_edges_event : public checker_event.  */

void
start_consolidated_cfg_edges_event::print_desc (pretty_printer &pp) const
{
  pp_printf (&pp,
	     "following %qs branch...",
	     m_edge_sense ? "true" : "false");
}

/* Implementation of diagnostics::paths::event::get_meaning vfunc for
   start_consolidated_cfg_edges_event.  */

diagnostics::paths::event::meaning
start_consolidated_cfg_edges_event::get_meaning () const
{
  return meaning (verb::branch,
		  (m_edge_sense ? property::true_ : property::false_));
}

/* class inlined_call_event : public checker_event.  */

void
inlined_call_event::print_desc (pretty_printer &pp) const
{
  pp_printf (&pp,
	     "inlined call to %qE from %qE",
	     m_apparent_callee_fndecl,
	     m_apparent_caller_fndecl);
}

/* Implementation of diagnostics::paths::event::get_meaning vfunc for
   reconstructed inlined function calls.  */

diagnostics::paths::event::meaning
inlined_call_event::get_meaning () const
{
  return meaning (verb::call, noun::function);
}

/* class setjmp_event : public checker_event.  */

/* Implementation of diagnostics::paths::event::print_desc vfunc for
   setjmp_event.  */

void
setjmp_event::print_desc (pretty_printer &pp) const
{
  pp_printf (&pp,
	     "%qs called here",
	     get_user_facing_name (m_setjmp_call));
}

diagnostics::paths::event::meaning
setjmp_event::get_meaning () const
{
  return meaning (verb::setjmp_);
}

/* Implementation of checker_event::prepare_for_emission vfunc for setjmp_event.

   Record this setjmp's event ID into the path, so that rewind events can
   use it.  */

void
setjmp_event::prepare_for_emission (checker_path *path,
				    pending_diagnostic *pd,
				    diagnostics::paths::event_id_t emission_id)
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

diagnostics::paths::event::meaning
rewind_event::get_meaning () const
{
  return meaning (verb::longjmp_);
}

/* rewind_event's ctor.  */

rewind_event::rewind_event (const exploded_edge *eedge,
			    enum event_kind kind,
			    const event_loc_info &loc_info,
			    const rewind_info_t *rewind_info)
: checker_event (kind, loc_info),
  m_rewind_info (rewind_info),
  m_eedge (eedge)
{
  gcc_assert (m_eedge->m_custom_info.get () == m_rewind_info);
}

/* class rewind_from_longjmp_event : public rewind_event.  */

/* Implementation of diagnostics::paths::event::print_desc vfunc for
   rewind_from_longjmp_event.  */

void
rewind_from_longjmp_event::print_desc (pretty_printer &pp) const
{
  const char *src_name
    = get_user_facing_name (m_rewind_info->get_longjmp_call ());

  if (get_longjmp_caller () == get_setjmp_caller ())
    /* Special-case: purely intraprocedural rewind.  */
    pp_printf (&pp,
	       "rewinding within %qE from %qs...",
	       get_longjmp_caller (),
	       src_name);
  else
    pp_printf (&pp,
	       "rewinding from %qs in %qE...",
	       src_name,
	       get_longjmp_caller ());
}

/* class rewind_to_setjmp_event : public rewind_event.  */

/* Implementation of diagnostics::paths::event::print_desc vfunc for
   rewind_to_setjmp_event.  */

void
rewind_to_setjmp_event::print_desc (pretty_printer &pp) const
{
  const char *dst_name
    = get_user_facing_name (m_rewind_info->get_setjmp_call ());

  /* If we can, identify the ID of the setjmp_event.  */
  if (m_original_setjmp_event_id.known_p ())
    {
      if (get_longjmp_caller () == get_setjmp_caller ())
	/* Special-case: purely intraprocedural rewind.  */
	pp_printf (&pp,
		   "...to %qs (saved at %@)",
		   dst_name,
		   &m_original_setjmp_event_id);
      else
	pp_printf (&pp,
		   "...to %qs in %qE (saved at %@)",
		   dst_name,
		   get_setjmp_caller (),
		   &m_original_setjmp_event_id);
    }
  else
    {
      if (get_longjmp_caller () == get_setjmp_caller ())
	/* Special-case: purely intraprocedural rewind.  */
	pp_printf (&pp,
		   "...to %qs",
		   dst_name);
      else
	pp_printf (&pp,
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
					      diagnostics::paths::event_id_t emission_id)
{
  checker_event::prepare_for_emission (path, pd, emission_id);
  path->get_setjmp_event (m_rewind_info->get_enode_origin (),
			  &m_original_setjmp_event_id);
}

/* class throw_event : public checker_event.  */

diagnostics::paths::event::meaning
throw_event::get_meaning () const
{
  return meaning (verb::throw_);
}

/* class explicit_throw_event : public throw_event.  */
void
explicit_throw_event::print_desc (pretty_printer &pp) const
{
  if (m_is_rethrow)
    {
      if (m_type)
	pp_printf (&pp, "rethrowing exception of type %qT here...", m_type);
      else
	pp_printf (&pp, "rethrowing exception here...");
    }
  else
    {
      if (m_type)
	pp_printf (&pp, "throwing exception of type %qT here...", m_type);
      else
	pp_printf (&pp, "throwing exception here...");
    }
}

/* class throw_from_call_to_external_fn_event : public throw_event.  */

void
throw_from_call_to_external_fn_event::print_desc (pretty_printer &pp) const
{
  if (m_fndecl)
    pp_printf (&pp, "if %qD throws an exception...", m_fndecl);
  else
    pp_printf (&pp, "if the called function throws an exception...");
}

// class unwind_event : public checker_event

void
unwind_event::print_desc (pretty_printer &pp) const
{
  if (m_num_frames > 1)
    pp_printf (&pp, "unwinding %i stack frames", m_num_frames);
  else
    pp_printf (&pp, "unwinding stack frame");
}

diagnostics::paths::event::meaning
unwind_event::get_meaning () const
{
  return meaning (verb::unwind_);
}

/* class warning_event : public checker_event.  */

/* Implementation of diagnostics::paths::event::print_desc vfunc for
   warning_event.

   If the pending diagnostic implements describe_final_event, use it,
   generating a precise description e.g.
     "second 'free' here; first 'free' was at (7)"

   Otherwise generate a generic description.  */

void
warning_event::print_desc (pretty_printer &pp) const
{
  if (m_pending_diagnostic)
    {
      tree var = fixup_tree_for_diagnostic (m_var);
      evdesc::final_event evd (var, m_state, *this);
      if (m_pending_diagnostic->describe_final_event (pp, evd))
	{
	  if (m_sm && flag_analyzer_verbose_state_changes)
	    {
	      if (var)
		pp_printf (&pp, " (%qE is in state %qs)",
			   var, m_state->get_name ());
	      else
		pp_printf (&pp, " (in global state %qs)",
			   m_state->get_name ());
	    }
	  return;
	}
    }

  if (m_sm)
    {
      if (m_var)
	pp_printf (&pp, "here (%qE is in state %qs)",
		   m_var, m_state->get_name ());
      else
	pp_printf (&pp, "here (in global state %qs)",
		   m_state->get_name ());
      return;
    }
  else
    pp_string (&pp, "here");
}

/* Implementation of diagnostics::paths::event::get_meaning vfunc for
   warning_event.  */

diagnostics::paths::event::meaning
warning_event::get_meaning () const
{
  return meaning (verb::danger, noun::unknown);
}

const program_state *
warning_event::get_program_state () const
{
  if (m_program_state)
    return m_program_state.get ();
  else
    return &m_enode->get_state ();
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
