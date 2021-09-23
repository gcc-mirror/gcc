/* An experimental state machine, for tracking bad calls from within
   signal handlers.

   Copyright (C) 2019-2021 Free Software Foundation, Inc.
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
#include "options.h"
#include "bitmap.h"
#include "diagnostic-path.h"
#include "diagnostic-metadata.h"
#include "function.h"
#include "json.h"
#include "analyzer/analyzer.h"
#include "diagnostic-event-id.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/sm.h"
#include "analyzer/pending-diagnostic.h"
#include "sbitmap.h"
#include "tristate.h"
#include "ordered-hash-map.h"
#include "selftest.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"
#include "analyzer/program-state.h"
#include "analyzer/checker-path.h"
#include "digraph.h"
#include "cfg.h"
#include "gimple-iterator.h"
#include "cgraph.h"
#include "analyzer/supergraph.h"
#include "alloc-pool.h"
#include "fibonacci_heap.h"
#include "analyzer/diagnostic-manager.h"
#include "shortest-paths.h"
#include "analyzer/exploded-graph.h"
#include "analyzer/function-set.h"
#include "analyzer/analyzer-selftests.h"

#if ENABLE_ANALYZER

namespace ana {

namespace {

/* An experimental state machine, for tracking calls to async-signal-unsafe
   functions from within signal handlers.  */

class signal_state_machine : public state_machine
{
public:
  signal_state_machine (logger *logger);

  bool inherited_state_p () const FINAL OVERRIDE { return false; }

  bool on_stmt (sm_context *sm_ctxt,
		const supernode *node,
		const gimple *stmt) const FINAL OVERRIDE;

  bool can_purge_p (state_t s) const FINAL OVERRIDE;

  /* These states are "global", rather than per-expression.  */

  /* State for when we're in a signal handler.  */
  state_t m_in_signal_handler;

  /* Stop state.  */
  state_t m_stop;
};

/* Concrete subclass for describing call to an async-signal-unsafe function
   from a signal handler.  */

class signal_unsafe_call
  : public pending_diagnostic_subclass<signal_unsafe_call>
{
public:
  signal_unsafe_call (const signal_state_machine &sm, const gcall *unsafe_call,
		      tree unsafe_fndecl)
  : m_sm (sm), m_unsafe_call (unsafe_call), m_unsafe_fndecl (unsafe_fndecl)
  {
    gcc_assert (m_unsafe_fndecl);
  }

  const char *get_kind () const FINAL OVERRIDE { return "signal_unsafe_call"; }

  bool operator== (const signal_unsafe_call &other) const
  {
    return m_unsafe_call == other.m_unsafe_call;
  }

  bool emit (rich_location *rich_loc) FINAL OVERRIDE
  {
    auto_diagnostic_group d;
    diagnostic_metadata m;
    /* CWE-479: Signal Handler Use of a Non-reentrant Function.  */
    m.add_cwe (479);
    if (warning_meta (rich_loc, m,
		      OPT_Wanalyzer_unsafe_call_within_signal_handler,
		      "call to %qD from within signal handler",
		      m_unsafe_fndecl))
      {
	/* If we know a possible alternative function, add a note
	   suggesting the replacement.  */
	if (const char *replacement = get_replacement_fn ())
	  {
	    location_t note_loc = gimple_location (m_unsafe_call);
	    /* It would be nice to add a fixit, but the gimple call
	       location covers the whole call expression.  It isn't
	       currently possible to cut this down to just the call
	       symbol.  So the fixit would replace too much.
	       note_rich_loc.add_fixit_replace (replacement); */
	    inform (note_loc,
		    "%qs is a possible signal-safe alternative for %qD",
		    replacement, m_unsafe_fndecl);
	  }
	return true;
      }
    return false;
  }

  label_text describe_state_change (const evdesc::state_change &change)
    FINAL OVERRIDE
  {
    if (change.is_global_p ()
	&& change.m_new_state == m_sm.m_in_signal_handler)
      {
	function *handler = change.m_event.get_dest_function ();
	return change.formatted_print ("registering %qD as signal handler",
				       handler->decl);
      }
    return label_text ();
  }

  label_text describe_final_event (const evdesc::final_event &ev) FINAL OVERRIDE
  {
    return ev.formatted_print ("call to %qD from within signal handler",
			       m_unsafe_fndecl);
  }

private:
  const signal_state_machine &m_sm;
  const gcall *m_unsafe_call;
  tree m_unsafe_fndecl;

  /* Returns a replacement function as text if it exists.  Currently
     only "exit" has a signal-safe replacement "_exit", which does
     slightly less, but can be used in a signal handler.  */
  const char *
  get_replacement_fn ()
  {
    gcc_assert (m_unsafe_fndecl && DECL_P (m_unsafe_fndecl));

    if (id_equal ("exit", DECL_NAME (m_unsafe_fndecl)))
      return "_exit";

    return NULL;
  }
};

/* signal_state_machine's ctor.  */

signal_state_machine::signal_state_machine (logger *logger)
: state_machine ("signal", logger)
{
  m_in_signal_handler = add_state ("in_signal_handler");
  m_stop = add_state ("stop");
}

/* Update MODEL for edges that simulate HANDLER_FUN being called as
   an signal-handler in response to a signal.  */

static void
update_model_for_signal_handler (region_model *model,
				 function *handler_fun)
{
  gcc_assert (model);
  /* Purge all state within MODEL.  */
  *model = region_model (model->get_manager ());
  model->push_frame (handler_fun, NULL, NULL);
}

/* Custom exploded_edge info: entry into a signal-handler.  */

class signal_delivery_edge_info_t : public custom_edge_info
{
public:
  void print (pretty_printer *pp) const FINAL OVERRIDE
  {
    pp_string (pp, "signal delivered");
  }

  json::object *to_json () const
  {
    json::object *custom_obj = new json::object ();
    return custom_obj;
  }

  bool update_model (region_model *model,
		     const exploded_edge *eedge,
		     region_model_context *) const FINAL OVERRIDE
  {
    gcc_assert (eedge);
    update_model_for_signal_handler (model, eedge->m_dest->get_function ());
    return true;
  }

  void add_events_to_path (checker_path *emission_path,
			   const exploded_edge &eedge ATTRIBUTE_UNUSED)
    const FINAL OVERRIDE
  {
    emission_path->add_event
      (new precanned_custom_event
       (UNKNOWN_LOCATION, NULL_TREE, 0,
	"later on,"
	" when the signal is delivered to the process"));
  }
};

/* Concrete subclass of custom_transition for modeling registration of a
   signal handler and the signal handler later being called.  */

class register_signal_handler : public custom_transition
{
public:
  register_signal_handler (const signal_state_machine &sm,
			   tree fndecl)
  : m_sm (sm), m_fndecl (fndecl) {}

  /* Model a signal-handler FNDECL being called at some later point
     by injecting an edge to a new function-entry node with an empty
     callstring, setting the 'in-signal-handler' global state
     on the node.  */
  void impl_transition (exploded_graph *eg,
			exploded_node *src_enode,
			int sm_idx) FINAL OVERRIDE
  {
    function *handler_fun = DECL_STRUCT_FUNCTION (m_fndecl);
    if (!handler_fun)
      return;
    program_point entering_handler
      = program_point::from_function_entry (eg->get_supergraph (),
					    handler_fun);

    program_state state_entering_handler (eg->get_ext_state ());
    update_model_for_signal_handler (state_entering_handler.m_region_model,
				     handler_fun);
    state_entering_handler.m_checker_states[sm_idx]->set_global_state
      (m_sm.m_in_signal_handler);

    exploded_node *dst_enode = eg->get_or_create_node (entering_handler,
						       state_entering_handler,
						       src_enode);
    if (dst_enode)
      eg->add_edge (src_enode, dst_enode, NULL, /*state_change (),*/
		    new signal_delivery_edge_info_t ());
  }

  const signal_state_machine &m_sm;
  tree m_fndecl;
};

/* Get a set of functions that are known to be unsafe to call from an
   async signal handler.  */

static function_set
get_async_signal_unsafe_fns ()
{
  // TODO: populate this list more fully
  static const char * const async_signal_unsafe_fns[] = {
    /* This array must be kept sorted.  */
    "exit",
    "fprintf",
    "free",
    "malloc",
    "printf",
    "snprintf",
    "sprintf",
    "vfprintf",
    "vprintf",
    "vsnprintf",
    "vsprintf"
  };
  const size_t count
    = sizeof(async_signal_unsafe_fns) / sizeof (async_signal_unsafe_fns[0]);
  function_set fs (async_signal_unsafe_fns, count);
  return fs;
}

/* Return true if FNDECL is known to be unsafe to call from a signal
   handler.  */

static bool
signal_unsafe_p (tree fndecl)
{
  function_set fs = get_async_signal_unsafe_fns ();
  return fs.contains_decl_p (fndecl);
}

/* Implementation of state_machine::on_stmt vfunc for signal_state_machine.  */

bool
signal_state_machine::on_stmt (sm_context *sm_ctxt,
			       const supernode *node,
			       const gimple *stmt) const
{
  const state_t global_state = sm_ctxt->get_global_state ();
  if (global_state == m_start)
    {
      if (const gcall *call = dyn_cast <const gcall *> (stmt))
	if (tree callee_fndecl = sm_ctxt->get_fndecl_for_call (call))
	  if (is_named_call_p (callee_fndecl, "signal", call, 2))
	    {
	      tree handler = gimple_call_arg (call, 1);
	      if (TREE_CODE (handler) == ADDR_EXPR
		  && TREE_CODE (TREE_OPERAND (handler, 0)) == FUNCTION_DECL)
		{
		  tree fndecl = TREE_OPERAND (handler, 0);
		  register_signal_handler rsh (*this, fndecl);
		  sm_ctxt->on_custom_transition (&rsh);
		}
	    }
    }
  else if (global_state == m_in_signal_handler)
    {
      if (const gcall *call = dyn_cast <const gcall *> (stmt))
	if (tree callee_fndecl = sm_ctxt->get_fndecl_for_call (call))
	  if (signal_unsafe_p (callee_fndecl))
	    if (sm_ctxt->get_global_state () == m_in_signal_handler)
	      sm_ctxt->warn (node, stmt, NULL_TREE,
			     new signal_unsafe_call (*this, call,
						     callee_fndecl));
    }

  return false;
}

bool
signal_state_machine::can_purge_p (state_t s ATTRIBUTE_UNUSED) const
{
  return true;
}

} // anonymous namespace

/* Internal interface to this file. */

state_machine *
make_signal_state_machine (logger *logger)
{
  return new signal_state_machine (logger);
}

#if CHECKING_P

namespace selftest {

/* Run all of the selftests within this file.  */

void
analyzer_sm_signal_cc_tests ()
{
  function_set fs = get_async_signal_unsafe_fns ();
  fs.assert_sorted ();
  fs.assert_sane ();
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
