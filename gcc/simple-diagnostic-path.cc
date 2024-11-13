/* Concrete classes for implementing diagnostic paths.
   Copyright (C) 2019-2024 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


#include "config.h"
#define INCLUDE_MEMORY
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "version.h"
#include "demangle.h"
#include "intl.h"
#include "backtrace.h"
#include "diagnostic.h"
#include "simple-diagnostic-path.h"
#include "selftest.h"

/* class simple_diagnostic_path : public diagnostic_path.  */

simple_diagnostic_path::simple_diagnostic_path (pretty_printer *event_pp)
: m_event_pp (event_pp),
  m_localize_events (true)
{
  add_thread ("main");
}

/* Implementation of diagnostic_path::num_events vfunc for
   simple_diagnostic_path: simply get the number of events in the vec.  */

unsigned
simple_diagnostic_path::num_events () const
{
  return m_events.length ();
}

/* Implementation of diagnostic_path::get_event vfunc for
   simple_diagnostic_path: simply return the event in the vec.  */

const diagnostic_event &
simple_diagnostic_path::get_event (int idx) const
{
  return *m_events[idx];
}

unsigned
simple_diagnostic_path::num_threads () const
{
  return m_threads.length ();
}

const diagnostic_thread &
simple_diagnostic_path::get_thread (diagnostic_thread_id_t idx) const
{
  return *m_threads[idx];
}

bool
simple_diagnostic_path::same_function_p (int event_idx_a,
					 int event_idx_b) const
{
  return (m_events[event_idx_a]->get_fndecl ()
	  == m_events[event_idx_b]->get_fndecl ());
}

diagnostic_thread_id_t
simple_diagnostic_path::add_thread (const char *name)
{
  m_threads.safe_push (new simple_diagnostic_thread (name));
  return m_threads.length () - 1;
}

/* Add an event to this path at LOC within function FNDECL at
   stack depth DEPTH.

   Use m_context's printer to format FMT, as the text of the new
   event.  Localize FMT iff m_localize_events is set.

   Return the id of the new event.  */

diagnostic_event_id_t
simple_diagnostic_path::add_event (location_t loc, tree fndecl, int depth,
				   const char *fmt, ...)
{
  pretty_printer *pp = m_event_pp;
  pp_clear_output_area (pp);

  rich_location rich_loc (line_table, UNKNOWN_LOCATION);

  va_list ap;

  va_start (ap, fmt);

  text_info ti (m_localize_events ? _(fmt) : fmt,
		&ap, 0, nullptr, &rich_loc);
  pp_format (pp, &ti);
  pp_output_formatted_text (pp);

  va_end (ap);

  simple_diagnostic_event *new_event
    = new simple_diagnostic_event (loc, fndecl, depth, pp_formatted_text (pp));
  m_events.safe_push (new_event);

  pp_clear_output_area (pp);

  return diagnostic_event_id_t (m_events.length () - 1);
}

diagnostic_event_id_t
simple_diagnostic_path::add_thread_event (diagnostic_thread_id_t thread_id,
					  location_t loc,
					  tree fndecl,
					  int depth,
					  const char *fmt, ...)
{
  pretty_printer *pp = m_event_pp;
  pp_clear_output_area (pp);

  rich_location rich_loc (line_table, UNKNOWN_LOCATION);

  va_list ap;

  va_start (ap, fmt);

  text_info ti (_(fmt), &ap, 0, nullptr, &rich_loc);

  pp_format (pp, &ti);
  pp_output_formatted_text (pp);

  va_end (ap);

  simple_diagnostic_event *new_event
    = new simple_diagnostic_event (loc, fndecl, depth, pp_formatted_text (pp),
				   thread_id);
  m_events.safe_push (new_event);

  pp_clear_output_area (pp);

  return diagnostic_event_id_t (m_events.length () - 1);
}

/* Mark the most recent event on this path (which must exist) as being
   connected to the next one to be added.  */

void
simple_diagnostic_path::connect_to_next_event ()
{
  gcc_assert (m_events.length () > 0);
  m_events[m_events.length () - 1]->connect_to_next_event ();
}

/* struct simple_diagnostic_event.  */

/* simple_diagnostic_event's ctor.  */

simple_diagnostic_event::
simple_diagnostic_event (location_t loc,
			 tree fndecl,
			 int depth,
			 const char *desc,
			 diagnostic_thread_id_t thread_id)
: m_loc (loc), m_fndecl (fndecl), m_logical_loc (fndecl),
  m_depth (depth), m_desc (xstrdup (desc)),
  m_connected_to_next_event (false),
  m_thread_id (thread_id)
{
}

/* simple_diagnostic_event's dtor.  */

simple_diagnostic_event::~simple_diagnostic_event ()
{
  free (m_desc);
}

void
simple_diagnostic_event::print_desc (pretty_printer &pp) const
{
  pp_string (&pp, m_desc);
}

#if CHECKING_P

namespace selftest {

static void
test_intraprocedural_path (pretty_printer *event_pp)
{
  tree fntype_void_void
    = build_function_type_array (void_type_node, 0, NULL);
  tree fndecl_foo = build_fn_decl ("foo", fntype_void_void);

  simple_diagnostic_path path (event_pp);
  path.add_event (UNKNOWN_LOCATION, fndecl_foo, 0, "first %qs", "free");
  path.add_event (UNKNOWN_LOCATION, fndecl_foo, 0, "double %qs", "free");

  ASSERT_EQ (path.num_events (), 2);
  ASSERT_EQ (path.num_threads (), 1);
  ASSERT_FALSE (path.interprocedural_p ());
  ASSERT_STREQ (path.get_event (0).get_desc (*event_pp).get (),
		"first `free'");
  ASSERT_STREQ (path.get_event (1).get_desc (*event_pp).get (),
		"double `free'");
}

/* Run all of the selftests within this file.  */

void
simple_diagnostic_path_cc_tests ()
{
  /* In a few places we use the global dc's printer to determine
     colorization so ensure this off during the tests.  */
  pretty_printer *global_pp = global_dc->get_reference_printer ();
  const bool saved_show_color = pp_show_color (global_pp);
  pp_show_color (global_pp) = false;

  auto_fix_quotes fix_quotes;
  std::unique_ptr<pretty_printer> event_pp
    = std::unique_ptr<pretty_printer> (global_pp->clone ());

  test_intraprocedural_path (event_pp.get ());

  pp_show_color (global_pp) = saved_show_color;
}

} // namespace selftest

#endif /* #if CHECKING_P */
