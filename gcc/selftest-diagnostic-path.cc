/* Concrete classes for selftests involving diagnostic paths.
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
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "version.h"
#include "demangle.h"
#include "backtrace.h"
#include "diagnostic.h"
#include "selftest-diagnostic-path.h"

#if CHECKING_P

namespace selftest {

/* class test_diagnostic_path : public diagnostic_path.  */

test_diagnostic_path::test_diagnostic_path (pretty_printer *event_pp)
: m_event_pp (event_pp)
{
  add_thread ("main");
}

/* Implementation of diagnostic_path::num_events vfunc for
   test_diagnostic_path: simply get the number of events in the vec.  */

unsigned
test_diagnostic_path::num_events () const
{
  return m_events.length ();
}

/* Implementation of diagnostic_path::get_event vfunc for
   test_diagnostic_path: simply return the event in the vec.  */

const diagnostic_event &
test_diagnostic_path::get_event (int idx) const
{
  return *m_events[idx];
}

unsigned
test_diagnostic_path::num_threads () const
{
  return m_threads.length ();
}

const diagnostic_thread &
test_diagnostic_path::get_thread (diagnostic_thread_id_t idx) const
{
  return *m_threads[idx];
}

bool
test_diagnostic_path::same_function_p (int event_idx_a,
				       int event_idx_b) const
{
  const char *name_a = m_events[event_idx_a]->get_function_name ();
  const char *name_b = m_events[event_idx_b]->get_function_name ();

  if (name_a && name_b)
    return 0 == strcmp (name_a, name_b);
  return name_a == name_b;
}

diagnostic_thread_id_t
test_diagnostic_path::add_thread (const char *name)
{
  m_threads.safe_push (new test_diagnostic_thread (name));
  return m_threads.length () - 1;
}

/* Add an event to this path at LOC within function FNDECL at
   stack depth DEPTH.

   Use m_context's printer to format FMT, as the text of the new
   event.

   Return the id of the new event.  */

diagnostic_event_id_t
test_diagnostic_path::add_event (location_t loc,
				 const char *funcname,
				 int depth,
				 const char *fmt, ...)
{
  pretty_printer *pp = m_event_pp;
  pp_clear_output_area (pp);

  rich_location rich_loc (line_table, UNKNOWN_LOCATION);

  va_list ap;

  va_start (ap, fmt);

  /* No localization is done on FMT.  */
  text_info ti (fmt, &ap, 0, nullptr, &rich_loc);
  pp_format (pp, &ti);
  pp_output_formatted_text (pp);

  va_end (ap);

  test_diagnostic_event *new_event
    = new test_diagnostic_event (loc, funcname, depth, pp_formatted_text (pp));
  m_events.safe_push (new_event);

  pp_clear_output_area (pp);

  return diagnostic_event_id_t (m_events.length () - 1);
}

diagnostic_event_id_t
test_diagnostic_path::add_thread_event (diagnostic_thread_id_t thread_id,
					location_t loc,
					const char *funcname,
					int depth,
					const char *fmt, ...)
{
  pretty_printer *pp = m_event_pp;
  pp_clear_output_area (pp);

  rich_location rich_loc (line_table, UNKNOWN_LOCATION);

  va_list ap;

  va_start (ap, fmt);

  /* No localization is done on FMT.  */
  text_info ti (fmt, &ap, 0, nullptr, &rich_loc);

  pp_format (pp, &ti);
  pp_output_formatted_text (pp);

  va_end (ap);

  test_diagnostic_event *new_event
    = new test_diagnostic_event (loc, funcname, depth, pp_formatted_text (pp),
				   thread_id);
  m_events.safe_push (new_event);

  pp_clear_output_area (pp);

  return diagnostic_event_id_t (m_events.length () - 1);
}

/* Mark the most recent event on this path (which must exist) as being
   connected to the next one to be added.  */

void
test_diagnostic_path::connect_to_next_event ()
{
  gcc_assert (m_events.length () > 0);
  m_events[m_events.length () - 1]->connect_to_next_event ();
}

void
test_diagnostic_path::add_entry (const char *callee_name,
				 int stack_depth,
				 diagnostic_thread_id_t thread_id)
{
  add_thread_event (thread_id, UNKNOWN_LOCATION, callee_name, stack_depth,
		    "entering %qs", callee_name);
}

void
test_diagnostic_path::add_return (const char *caller_name,
				  int stack_depth,
				  diagnostic_thread_id_t thread_id)
{
  add_thread_event (thread_id, UNKNOWN_LOCATION, caller_name, stack_depth,
		    "returning to %qs", caller_name);
}

void
test_diagnostic_path::add_call (const char *caller_name,
				int caller_stack_depth,
				const char *callee_name,
				diagnostic_thread_id_t thread_id)
{
  add_thread_event (thread_id, UNKNOWN_LOCATION,
		    caller_name, caller_stack_depth,
		    "calling %qs", callee_name);
  add_entry (callee_name, caller_stack_depth + 1, thread_id);
}

/* struct test_diagnostic_event.  */

/* test_diagnostic_event's ctor.  */

test_diagnostic_event::
test_diagnostic_event (location_t loc,
			 const char *funcname,
			 int depth,
			 const char *desc,
			 diagnostic_thread_id_t thread_id)
: m_loc (loc),
  m_logical_loc (LOGICAL_LOCATION_KIND_FUNCTION, funcname),
  m_depth (depth), m_desc (xstrdup (desc)),
  m_connected_to_next_event (false),
  m_thread_id (thread_id)
{
}

/* test_diagnostic_event's dtor.  */

test_diagnostic_event::~test_diagnostic_event ()
{
  free (m_desc);
}

} // namespace selftest

#endif /* #if CHECKING_P */
