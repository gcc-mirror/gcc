/* Paths through the code associated with a diagnostic.
   Copyright (C) 2019-2026 Free Software Foundation, Inc.
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
#define INCLUDE_ALGORITHM
#define INCLUDE_MAP
#define INCLUDE_STRING
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "diagnostics/paths.h"
#include "diagnostics/state-graphs.h"

/* Disable warnings about missing quoting in GCC diagnostics for the print
   calls below.  */
#if __GNUC__ >= 10
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wformat-diag"
#endif

using namespace diagnostics;
using namespace diagnostics::paths;

/* class diagnostics::paths::event.  */

/* struct event::meaning.  */

void
event::meaning::dump_to_pp (pretty_printer *pp) const
{
  bool need_comma = false;
  pp_character (pp, '{');
  if (const char *verb_str = maybe_get_verb_str (m_verb))
    {
      pp_printf (pp, "verb: %qs", verb_str);
      need_comma = true;
    }
  if (const char *noun_str = maybe_get_noun_str (m_noun))
    {
      if (need_comma)
	pp_string (pp, ", ");
      pp_printf (pp, "noun: %qs", noun_str);
      need_comma = true;
    }
  if (const char *property_str = maybe_get_property_str (m_property))
    {
      if (need_comma)
	pp_string (pp, ", ");
      pp_printf (pp, "property: %qs", property_str);
      need_comma = true;
    }
  pp_character (pp, '}');
}

/* Get a string (or nullptr) for V suitable for use within a SARIF
   threadFlowLocation "kinds" property (SARIF v2.1.0 section 3.38.8).  */

const char *
event::meaning::maybe_get_verb_str (enum verb v)
{
  switch (v)
    {
    default:
      gcc_unreachable ();
    case verb::unknown:
      return nullptr;
    case verb::acquire:
      return "acquire";
    case verb::release:
      return "release";
    case verb::enter:
      return "enter";
    case verb::exit:
      return "exit";
    case verb::call:
      return "call";
    case verb::return_:
      return "return";
    case verb::branch:
      return "branch";
    case verb::danger:
      return "danger";

    /* Special control flow operations.

       These are not part of SARIF v2.1.0 section 3.38.8, but the
       spec allows other values; see
       https://github.com/oasis-tcs/sarif-spec/issues/735  */
    case verb::throw_:
      return "throw";
    case verb::catch_:
      return "catch";
    case verb::unwind_:
      return "unwind";
    case verb::setjmp_:
      return "setjmp";
    case verb::longjmp_:
      return "longjmp";
    }
}

/* Get a string (or nullptr) for N suitable for use within a SARIF
   threadFlowLocation "kinds" property (SARIF v2.1.0 section 3.38.8).  */

const char *
event::meaning::maybe_get_noun_str (enum noun n)
{
  switch (n)
    {
    default:
      gcc_unreachable ();
    case noun::unknown:
      return nullptr;
    case noun::taint:
      return "taint";
    case noun::sensitive:
      return "sensitive";
    case noun::function:
      return "function";
    case noun::lock:
      return "lock";
    case noun::memory:
      return "memory";
    case noun::resource:
      return "resource";
    }
}

/* Get a string (or nullptr) for P suitable for use within a SARIF
   threadFlowLocation "kinds" property (SARIF v2.1.0 section 3.38.8).  */

const char *
event::meaning::maybe_get_property_str (enum property p)
{
  switch (p)
    {
    default:
      gcc_unreachable ();
    case property::unknown:
      return nullptr;
    case property::true_:
      return "true";
    case property::false_:
      return "false";
    }
}

/* Generate a label_text containing the description of this event
   (for debugging/logging purposes).  */

label_text
event::get_desc (pretty_printer &ref_pp) const
{
  auto pp = ref_pp.clone ();
  pp_show_color (pp.get ()) = false;
  print_desc (*pp.get ());
  return label_text::take (xstrdup (pp_formatted_text (pp.get ())));
}

// Base implementation of event::maybe_make_diagnostic_state_graph

std::unique_ptr<digraphs::digraph>
event::maybe_make_diagnostic_state_graph (bool) const
{
  // Don't attempt to make a state graph:
  return nullptr;
}

/* class diagnostics::paths::path.  */

/* Subroutine of path::interprocedural_p.
   Look for the first event in this path that is within a function
   i.e. has a non-null logical location for which function_p is true.
   If found, write its index to *OUT_IDX and return true.
   Otherwise return false.  */

bool
path::get_first_event_in_a_function (unsigned *out_idx) const
{
  const unsigned num = num_events ();
  for (unsigned i = 0; i < num; i++)
    {
      const event &event = get_event (i);
      if (logical_locations::key logical_loc = event.get_logical_location ())
	if (m_logical_loc_mgr.function_p (logical_loc))
	  {
	    *out_idx = i;
	    return true;
	  }
    }
  return false;
}

/* Return true if the events in this path involve more than one
   function, or false if it is purely intraprocedural.  */

bool
path::interprocedural_p () const
{
  /* Ignore leading events that are outside of any function.  */
  unsigned first_fn_event_idx;
  if (!get_first_event_in_a_function (&first_fn_event_idx))
    return false;

  const event &first_fn_event = get_event (first_fn_event_idx);
  int first_fn_stack_depth = first_fn_event.get_stack_depth ();

  const unsigned num = num_events ();
  for (unsigned i = first_fn_event_idx + 1; i < num; i++)
    {
      if (!same_function_p (first_fn_event_idx, i))
	return true;
      if (get_event (i).get_stack_depth () != first_fn_stack_depth)
	return true;
    }
  return false;
}

/* Print PATH by emitting a dummy "note" associated with it.  */

DEBUG_FUNCTION
void debug (path *p)
{
  rich_location richloc (line_table, UNKNOWN_LOCATION);
  richloc.set_path (p);
  inform (&richloc, "debug path");
}

#if __GNUC__ >= 10
#  pragma GCC diagnostic pop
#endif
