/* Subclasses of custom_edge_info for describing outcomes of function calls.
   Copyright (C) 2021 Free Software Foundation, Inc.
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
#include "gimple-iterator.h"
#include "diagnostic-core.h"
#include "options.h"
#include "cgraph.h"
#include "tree-pretty-print.h"
#include "tristate.h"
#include "bitmap.h"
#include "selftest.h"
#include "function.h"
#include "json.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "ordered-hash-map.h"
#include "cfg.h"
#include "digraph.h"
#include "analyzer/supergraph.h"
#include "sbitmap.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"
#include "analyzer/constraint-manager.h"
#include "diagnostic-event-id.h"
#include "analyzer/sm.h"
#include "analyzer/pending-diagnostic.h"
#include "analyzer/region-model-reachability.h"
#include "analyzer/analyzer-selftests.h"
#include "analyzer/program-state.h"
#include "diagnostic-path.h"
#include "analyzer/checker-path.h"
#include "analyzer/diagnostic-manager.h"
#include "alloc-pool.h"
#include "fibonacci_heap.h"
#include "shortest-paths.h"
#include "analyzer/exploded-graph.h"
#include "analyzer/call-info.h"

#if ENABLE_ANALYZER

namespace ana {

/* class call_info : public custom_eedge_info_t.  */

/* Implementation of custom_edge_info::print vfunc for call_info:
   use get_desc to get a label_text, and print it to PP.  */

void
call_info::print (pretty_printer *pp) const
{
  label_text desc (get_desc (pp_show_color (pp)));
  pp_string (pp, desc.m_buffer);
  desc.maybe_free ();
}

/* Implementation of custom_edge_info::add_events_to_path vfunc for
   call_info: add a custom_event using call_info::get_desc as its
   description.  */

void
call_info::add_events_to_path (checker_path *emission_path,
			       const exploded_edge &eedge) const
{
  class call_event : public custom_event
  {
  public:
    call_event (location_t loc, tree fndecl, int depth,
		const call_info *call_info)
      : custom_event (loc, fndecl, depth),
	m_call_info (call_info)
    {}

    label_text get_desc (bool can_colorize) const
    {
      return m_call_info->get_desc (can_colorize);
    }

  private:
    const call_info *m_call_info;
  };

  const exploded_node *src_node = eedge.m_src;
  const program_point &src_point = src_node->get_point ();
  tree caller_fndecl = src_point.get_fndecl ();
  const int stack_depth = src_point.get_stack_depth ();

  emission_path->add_event (new call_event (get_call_stmt ()->location,
					    caller_fndecl,
					    stack_depth,
					    this));
}

/* Recreate a call_details instance from this call_info.  */

call_details
call_info::get_call_details (region_model *model,
			     region_model_context *ctxt) const
{
  return call_details (m_call_stmt, model, ctxt);
}

/* call_info's ctor.

   The call_info instance will outlive the call_details instance;
   call_details instances are typically created on the stack.  */

call_info::call_info (const call_details &cd)
: m_call_stmt (cd.get_call_stmt ()),
  m_fndecl (cd.get_fndecl_for_call ())
{
  gcc_assert (m_fndecl);
}

/* class success_call_info : public call_info.  */

/* Implementation of call_info::get_desc vfunc for success_call_info.  */

label_text
success_call_info::get_desc (bool can_colorize) const
{
  return make_label_text (can_colorize, "when %qE succeeds", get_fndecl ());
}

/* class failed_call_info : public call_info.  */

/* Implementation of call_info::get_desc vfunc for failed_call_info.  */

label_text
failed_call_info::get_desc (bool can_colorize) const
{
  return make_label_text (can_colorize, "when %qE fails", get_fndecl ());
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
