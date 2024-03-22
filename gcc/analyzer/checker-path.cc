/* Subclass of diagnostic_path for analyzer diagnostics.
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
#define INCLUDE_MEMORY
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "diagnostic-core.h"
#include "gimple-pretty-print.h"
#include "fold-const.h"
#include "diagnostic-path.h"
#include "options.h"
#include "cgraph.h"
#include "cfg.h"
#include "digraph.h"
#include "diagnostic-event-id.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/sm.h"
#include "sbitmap.h"
#include "bitmap.h"
#include "ordered-hash-map.h"
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
#include "make-unique.h"

#if ENABLE_ANALYZER

namespace ana {

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

void
checker_path::add_event (std::unique_ptr<checker_event> event)
{
  if (m_logger)
    {
      m_logger->start_log_line ();
      m_logger->log_partial ("added event[%i]: %s ",
			     m_events.length (),
			     event_kind_to_string (event.get ()->m_kind));
      event.get ()->dump (m_logger->get_printer ());
      m_logger->end_log_line ();
    }
  m_events.safe_push (event.release ());
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

/* Add region_creation_event instances to this path for REG,
   describing whether REG is on the stack or heap and what
   its capacity is (if known).
   If DEBUG is true, also create an RCE_DEBUG event.  */

void
checker_path::add_region_creation_events (pending_diagnostic *pd,
					  const region *reg,
					  const region_model *model,
					  const event_loc_info &loc_info,
					  bool debug)
{
  tree capacity = NULL_TREE;
  if (model)
    if (const svalue *capacity_sval = model->get_capacity (reg))
      capacity = model->get_representative_tree (capacity_sval);

  pd->add_region_creation_events (reg, capacity, loc_info, *this);

  if (debug)
    add_event (make_unique<region_creation_event_debug> (reg, capacity,
							 loc_info));
}

void
checker_path::fixup_locations (pending_diagnostic *pd)
{
  for (checker_event *e : m_events)
    e->set_location (pd->fixup_location (e->get_location (), false));
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
	      logger->log_partial ("  %qE", iter.get_block ());
	      if (!flag_dump_noaddr)
		logger->log_partial (" (%p)", iter.get_block ());
	      logger->log_partial (", fndecl: %qE, callsite: 0x%x",
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
