/* Paths through the code associated with a diagnostic.
   Copyright (C) 2019-2020 Free Software Foundation, Inc.
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
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "diagnostic.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "tree-diagnostic.h"
#include "langhooks.h"
#include "intl.h"
#include "diagnostic-path.h"
#include "json.h"
#include "gcc-rich-location.h"
#include "diagnostic-color.h"
#include "diagnostic-event-id.h"
#include "selftest.h"
#include "selftest-diagnostic.h"

/* Anonymous namespace for path-printing code.  */

namespace {

/* Subclass of range_label for showing a particular event
   when showing a consecutive run of events within a diagnostic_path as
   labelled ranges within one gcc_rich_location.  */

class path_label : public range_label
{
 public:
  path_label (const diagnostic_path *path, unsigned start_idx)
  : m_path (path), m_start_idx (start_idx)
  {}

  label_text get_text (unsigned range_idx) const FINAL OVERRIDE
  {
    unsigned event_idx = m_start_idx + range_idx;
    const diagnostic_event &event = m_path->get_event (event_idx);

    /* Get the description of the event, perhaps with colorization:
       normally, we don't colorize within a range_label, but this
       is special-cased for diagnostic paths.  */
    bool colorize = pp_show_color (global_dc->printer);
    label_text event_text (event.get_desc (colorize));
    gcc_assert (event_text.m_buffer);
    pretty_printer pp;
    pp_show_color (&pp) = pp_show_color (global_dc->printer);
    diagnostic_event_id_t event_id (event_idx);
    pp_printf (&pp, "%@ %s", &event_id, event_text.m_buffer);
    event_text.maybe_free ();
    label_text result = label_text::take (xstrdup (pp_formatted_text (&pp)));
    return result;
  }

 private:
  const diagnostic_path *m_path;
  unsigned m_start_idx;
};

/* Return true if E1 and E2 can be consolidated into the same run of events
   when printing a diagnostic_path.  */

static bool
can_consolidate_events (const diagnostic_event &e1,
			const diagnostic_event &e2,
			bool check_locations)
{
  if (e1.get_fndecl () != e2.get_fndecl ())
    return false;

  if (e1.get_stack_depth () != e2.get_stack_depth ())
    return false;

  if (check_locations)
    {
      location_t loc1 = e1.get_location ();
      location_t loc2 = e2.get_location ();

      if (loc1 < RESERVED_LOCATION_COUNT
	  || loc2 < RESERVED_LOCATION_COUNT)
	return false;

      /* Neither can be macro-based.  */
      if (linemap_location_from_macro_expansion_p (line_table, loc1))
	return false;
      if (linemap_location_from_macro_expansion_p (line_table, loc2))
	return false;
    }

  /* Passed all the tests.  */
  return true;
}

/* A class for grouing together the events in a diagnostic_path into
   ranges of events, partitioned by stack frame (i.e. by fndecl and
   stack depth).  */

class path_summary
{
  /* A range of consecutive events within a diagnostic_path,
     all with the same fndecl and stack_depth, and which are suitable
     to print with a single call to diagnostic_show_locus.  */
  struct event_range
  {
    event_range (const diagnostic_path *path, unsigned start_idx,
		 const diagnostic_event &initial_event)
    : m_path (path),
      m_initial_event (initial_event),
      m_fndecl (initial_event.get_fndecl ()),
      m_stack_depth (initial_event.get_stack_depth ()),
      m_start_idx (start_idx), m_end_idx (start_idx),
      m_path_label (path, start_idx),
      m_richloc (initial_event.get_location (), &m_path_label)
    {}

    bool maybe_add_event (const diagnostic_event &new_ev, unsigned idx,
			  bool check_rich_locations)
    {
      if (!can_consolidate_events (m_initial_event, new_ev,
				   check_rich_locations))
	return false;
      if (check_rich_locations)
	if (!m_richloc.add_location_if_nearby (new_ev.get_location (),
					       false, &m_path_label))
	  return false;
      m_end_idx = idx;
      return true;
    }

    /* Print the events in this range to DC, typically as a single
       call to the printer's diagnostic_show_locus.  */

    void print (diagnostic_context *dc)
    {
      location_t initial_loc = m_initial_event.get_location ();

      /* Emit a span indicating the filename (and line/column) if the
	 line has changed relative to the last call to
	 diagnostic_show_locus.  */
      if (dc->show_caret)
	{
	  expanded_location exploc
	    = linemap_client_expand_location_to_spelling_point
	    (initial_loc, LOCATION_ASPECT_CARET);
	  if (exploc.file != LOCATION_FILE (dc->last_location))
	    dc->start_span (dc, exploc);
	}

      /* If we have an UNKNOWN_LOCATION (or BUILTINS_LOCATION) as the
	 primary location for an event, diagnostic_show_locus won't print
	 anything.

	 In particular the label for the event won't get printed.
	 Fail more gracefully in this case by showing the event
	 index and text, at no particular location.  */
      if (get_pure_location (initial_loc) <= BUILTINS_LOCATION)
	{
	  for (unsigned i = m_start_idx; i <= m_end_idx; i++)
	    {
	      const diagnostic_event &iter_event = m_path->get_event (i);
	      diagnostic_event_id_t event_id (i);
	      label_text event_text (iter_event.get_desc (true));
	      pretty_printer *pp = dc->printer;
	      pp_printf (pp, " %@: %s", &event_id, event_text.m_buffer);
	      pp_newline (pp);
	      event_text.maybe_free ();
	    }
	  return;
	}

      /* Call diagnostic_show_locus to show the events using labels.  */
      diagnostic_show_locus (dc, &m_richloc, DK_DIAGNOSTIC_PATH);

      /* If we have a macro expansion, show the expansion to the user.  */
      if (linemap_location_from_macro_expansion_p (line_table, initial_loc))
	{
	  gcc_assert (m_start_idx == m_end_idx);
	  maybe_unwind_expanded_macro_loc (dc, initial_loc);
	}
    }

    const diagnostic_path *m_path;
    const diagnostic_event &m_initial_event;
    tree m_fndecl;
    int m_stack_depth;
    unsigned m_start_idx;
    unsigned m_end_idx;
    path_label m_path_label;
    gcc_rich_location m_richloc;
  };

 public:
  path_summary (const diagnostic_path &path, bool check_rich_locations);

  void print (diagnostic_context *dc, bool show_depths) const;

  unsigned get_num_ranges () const { return m_ranges.length (); }

 private:
  auto_delete_vec <event_range> m_ranges;
};

/* path_summary's ctor.  */

path_summary::path_summary (const diagnostic_path &path,
			    bool check_rich_locations)
{
  const unsigned num_events = path.num_events ();

  event_range *cur_event_range = NULL;
  for (unsigned idx = 0; idx < num_events; idx++)
    {
      const diagnostic_event &event = path.get_event (idx);
      if (cur_event_range)
	if (cur_event_range->maybe_add_event (event, idx, check_rich_locations))
	  continue;

      cur_event_range = new event_range (&path, idx, event);
      m_ranges.safe_push (cur_event_range);
    }
}

/* Write SPACES to PP.  */

static void
write_indent (pretty_printer *pp, int spaces)
{
  for (int i = 0; i < spaces; i++)
    pp_space (pp);
}

/* Print FNDDECL to PP, quoting it if QUOTED is true.

   We can't use "%qE" here since we can't guarantee the capabilities
   of PP.  */

static void
print_fndecl (pretty_printer *pp, tree fndecl, bool quoted)
{
  const char *n = DECL_NAME (fndecl)
    ? identifier_to_locale (lang_hooks.decl_printable_name (fndecl, 2))
    : _("<anonymous>");
  if (quoted)
    pp_printf (pp, "%qs", n);
  else
    pp_string (pp, n);
}

/* Print this path_summary to DC, giving an overview of the interprocedural
   calls and returns.

   Print the event descriptions in a nested form, printing the event
   descriptions within calls to diagnostic_show_locus, using labels to
   show the events:

   'foo' (events 1-2)
     | NN |
     |    |
     +--> 'bar' (events 3-4)
            | NN |
            |    |
            +--> 'baz' (events 5-6)
                   | NN |
                   |    |
     <------------ +
     |
   'foo' (events 7-8)
     | NN |
     |    |
     +--> 'bar' (events 9-10)
            | NN |
            |    |
            +--> 'baz' (events 11-12)
                   | NN |
                   |    |

   If SHOW_DEPTHS is true, append " (depth N)" to the header of each run
   of events.

   For events with UNKNOWN_LOCATION, print a summary of each the event.  */

void
path_summary::print (diagnostic_context *dc, bool show_depths) const
{
  pretty_printer *pp = dc->printer;

  const int per_frame_indent = 2;

  const char *const line_color = "path";
  const char *start_line_color
    = colorize_start (pp_show_color (pp), line_color);
  const char *end_line_color = colorize_stop (pp_show_color (pp));

  /* Keep track of column numbers of existing '|' characters for
     stack depths we've already printed.  */
  const int EMPTY = -1;
  const int DELETED = -2;
  typedef int_hash <int, EMPTY, DELETED> vbar_hash;
  hash_map <vbar_hash, int> vbar_column_for_depth;

  /* Print the ranges.  */
  const int base_indent = 2;
  int cur_indent = base_indent;
  unsigned i;
  event_range *range;
  FOR_EACH_VEC_ELT (m_ranges, i, range)
    {
      write_indent (pp, cur_indent);
      if (i > 0)
	{
	  const path_summary::event_range *prev_range
	    = m_ranges[i - 1];
	  if (range->m_stack_depth > prev_range->m_stack_depth)
	    {
	      /* Show pushed stack frame(s).  */
	      const char *push_prefix = "+--> ";
	      pp_string (pp, start_line_color);
	      pp_string (pp, push_prefix);
	      pp_string (pp, end_line_color);
	      cur_indent += strlen (push_prefix);
	    }
	}
      if (range->m_fndecl)
	{
	  print_fndecl (pp, range->m_fndecl, true);
	  pp_string (pp, ": ");
	}
      if (range->m_start_idx == range->m_end_idx)
	pp_printf (pp, "event %i",
		   range->m_start_idx + 1);
      else
	pp_printf (pp, "events %i-%i",
		   range->m_start_idx + 1, range->m_end_idx + 1);
      if (show_depths)
	pp_printf (pp, " (depth %i)", range->m_stack_depth);
      pp_newline (pp);

      /* Print a run of events.  */
      {
	write_indent (pp, cur_indent + per_frame_indent);
	pp_string (pp, start_line_color);
	pp_string (pp, "|");
	pp_string (pp, end_line_color);
	pp_newline (pp);

	char *saved_prefix = pp_take_prefix (pp);
	char *prefix;
	{
	  pretty_printer tmp_pp;
	  write_indent (&tmp_pp, cur_indent + per_frame_indent);
	  pp_string (&tmp_pp, start_line_color);
	  pp_string (&tmp_pp, "|");
	  pp_string (&tmp_pp, end_line_color);
	  prefix = xstrdup (pp_formatted_text (&tmp_pp));
	}
	pp_set_prefix (pp, prefix);
	pp_prefixing_rule (pp) = DIAGNOSTICS_SHOW_PREFIX_EVERY_LINE;
	range->print (dc);
	pp_set_prefix (pp, saved_prefix);

	write_indent (pp, cur_indent + per_frame_indent);
	pp_string (pp, start_line_color);
	pp_string (pp, "|");
	pp_string (pp, end_line_color);
	pp_newline (pp);
      }

      if (i < m_ranges.length () - 1)
	{
	  const path_summary::event_range *next_range
	    = m_ranges[i + 1];

	  if (range->m_stack_depth > next_range->m_stack_depth)
	    {
	      if (vbar_column_for_depth.get (next_range->m_stack_depth))
		{
		  /* Show returning from stack frame(s), by printing
		     something like:
		     "                   |\n"
		     "     <------------ +\n"
		     "     |\n".  */
		  int vbar_for_next_frame
		    = *vbar_column_for_depth.get (next_range->m_stack_depth);

		  int indent_for_next_frame
		    = vbar_for_next_frame - per_frame_indent;
		  write_indent (pp, vbar_for_next_frame);
		  pp_string (pp, start_line_color);
		  pp_character (pp, '<');
		  for (int i = indent_for_next_frame + per_frame_indent;
		       i < cur_indent + per_frame_indent - 1; i++)
		    pp_character (pp, '-');
		  pp_character (pp, '+');
		  pp_string (pp, end_line_color);
		  pp_newline (pp);
		  cur_indent = indent_for_next_frame;

		  write_indent (pp, vbar_for_next_frame);
		  pp_string (pp, start_line_color);
		  pp_printf (pp, "|");
		  pp_string (pp, end_line_color);
		  pp_newline (pp);
		}
	      else
		{
		  /* Handle disjoint paths (e.g. a callback at some later
		     time).  */
		  cur_indent = base_indent;
		}
	    }
	  else if (range->m_stack_depth < next_range->m_stack_depth)
	    {
	      /* Prepare to show pushed stack frame.  */
	      gcc_assert (range->m_stack_depth != EMPTY);
	      gcc_assert (range->m_stack_depth != DELETED);
	      vbar_column_for_depth.put (range->m_stack_depth,
					 cur_indent + per_frame_indent);
	      cur_indent += per_frame_indent;
	    }

	}
    }
}

} /* end of anonymous namespace for path-printing code.  */

/* Print PATH to CONTEXT, according to CONTEXT's path_format.  */

void
default_tree_diagnostic_path_printer (diagnostic_context *context,
				      const diagnostic_path *path)
{
  gcc_assert (path);

  const unsigned num_events = path->num_events ();

  switch (context->path_format)
    {
    case DPF_NONE:
      /* Do nothing.  */
      return;

    case DPF_SEPARATE_EVENTS:
      {
	/* A note per event.  */
	for (unsigned i = 0; i < num_events; i++)
	  {
	    const diagnostic_event &event = path->get_event (i);
	    label_text event_text (event.get_desc (false));
	    gcc_assert (event_text.m_buffer);
	    diagnostic_event_id_t event_id (i);
	    inform (event.get_location (),
		    "%@ %s", &event_id, event_text.m_buffer);
	    event_text.maybe_free ();
	  }
      }
      break;

    case DPF_INLINE_EVENTS:
      {
	/* Consolidate related events.  */
	path_summary summary (*path, true);
	char *saved_prefix = pp_take_prefix (context->printer);
	pp_set_prefix (context->printer, NULL);
	summary.print (context, context->show_path_depths);
	pp_flush (context->printer);
	pp_set_prefix (context->printer, saved_prefix);
      }
    }
}

/* This has to be here, rather than diagnostic-format-json.cc,
   since diagnostic-format-json.o is within OBJS-libcommon and thus
   doesn't have access to trees (for m_fndecl).  */

json::value *
default_tree_make_json_for_path (diagnostic_context *context,
				 const diagnostic_path *path)
{
  json::array *path_array = new json::array ();
  for (unsigned i = 0; i < path->num_events (); i++)
    {
      const diagnostic_event &event = path->get_event (i);

      json::object *event_obj = new json::object ();
      if (event.get_location ())
	event_obj->set ("location",
			json_from_expanded_location (context,
						     event.get_location ()));
      label_text event_text (event.get_desc (false));
      event_obj->set ("description", new json::string (event_text.m_buffer));
      event_text.maybe_free ();
      if (tree fndecl = event.get_fndecl ())
	{
	  const char *function
	    = identifier_to_locale (lang_hooks.decl_printable_name (fndecl, 2));
	  event_obj->set ("function", new json::string (function));
	}
      event_obj->set ("depth",
		      new json::integer_number (event.get_stack_depth ()));
      path_array->append (event_obj);
    }
  return path_array;
}

#if CHECKING_P

namespace selftest {

/* A subclass of simple_diagnostic_path that adds member functions
   for adding test events.  */

class test_diagnostic_path : public simple_diagnostic_path
{
 public:
  test_diagnostic_path (pretty_printer *event_pp)
  : simple_diagnostic_path (event_pp)
  {
  }

  void add_entry (tree fndecl, int stack_depth)
  {
    add_event (UNKNOWN_LOCATION, fndecl, stack_depth,
	       "entering %qE", fndecl);
  }

  void add_return (tree fndecl, int stack_depth)
  {
    add_event (UNKNOWN_LOCATION, fndecl, stack_depth,
	       "returning to %qE", fndecl);
  }

  void add_call (tree caller, int caller_stack_depth, tree callee)
  {
    add_event (UNKNOWN_LOCATION, caller, caller_stack_depth,
	       "calling %qE", callee);
    add_entry (callee, caller_stack_depth + 1);
  }
};

/* Verify that empty paths are handled gracefully.  */

static void
test_empty_path (pretty_printer *event_pp)
{
  test_diagnostic_path path (event_pp);
  ASSERT_FALSE (path.interprocedural_p ());

  path_summary summary (path, false);
  ASSERT_EQ (summary.get_num_ranges (), 0);

  test_diagnostic_context dc;
  summary.print (&dc, true);
  ASSERT_STREQ ("",
		pp_formatted_text (dc.printer));
}

/* Verify that print_path_summary works on a purely intraprocedural path.  */

static void
test_intraprocedural_path (pretty_printer *event_pp)
{
  tree fntype_void_void
    = build_function_type_array (void_type_node, 0, NULL);
  tree fndecl_foo = build_fn_decl ("foo", fntype_void_void);

  test_diagnostic_path path (event_pp);
  path.add_event (UNKNOWN_LOCATION, fndecl_foo, 0, "first %qs", "free");
  path.add_event (UNKNOWN_LOCATION, fndecl_foo, 0, "double %qs", "free");

  ASSERT_FALSE (path.interprocedural_p ());

  path_summary summary (path, false);
  ASSERT_EQ (summary.get_num_ranges (), 1);

  test_diagnostic_context dc;
  summary.print (&dc, true);
  ASSERT_STREQ ("  `foo': events 1-2 (depth 0)\n"
		"    |\n"
		"    | (1): first `free'\n"
		"    | (2): double `free'\n"
		"    |\n",
		pp_formatted_text (dc.printer));
}

/* Verify that print_path_summary works on an interprocedural path.  */

static void
test_interprocedural_path_1 (pretty_printer *event_pp)
{
  /* Build fndecls.  The types aren't quite right, but that
     doesn't matter for the purposes of this test.  */
  tree fntype_void_void
    = build_function_type_array (void_type_node, 0, NULL);
  tree fndecl_test = build_fn_decl ("test", fntype_void_void);
  tree fndecl_make_boxed_int
    = build_fn_decl ("make_boxed_int", fntype_void_void);
  tree fndecl_wrapped_malloc
    = build_fn_decl ("wrapped_malloc", fntype_void_void);
  tree fndecl_free_boxed_int
    = build_fn_decl ("free_boxed_int", fntype_void_void);
  tree fndecl_wrapped_free
    = build_fn_decl ("wrapped_free", fntype_void_void);

  test_diagnostic_path path (event_pp);
  path.add_entry (fndecl_test, 0);
  path.add_call (fndecl_test, 0, fndecl_make_boxed_int);
  path.add_call (fndecl_make_boxed_int, 1, fndecl_wrapped_malloc);
  path.add_event (UNKNOWN_LOCATION, fndecl_wrapped_malloc, 2, "calling malloc");
  path.add_return (fndecl_test, 0);
  path.add_call (fndecl_test, 0, fndecl_free_boxed_int);
  path.add_call (fndecl_free_boxed_int, 1, fndecl_wrapped_free);
  path.add_event (UNKNOWN_LOCATION, fndecl_wrapped_free, 2, "calling free");
  path.add_return (fndecl_test, 0);
  path.add_call (fndecl_test, 0, fndecl_free_boxed_int);
  path.add_call (fndecl_free_boxed_int, 1, fndecl_wrapped_free);
  path.add_event (UNKNOWN_LOCATION, fndecl_wrapped_free, 2, "calling free");
  ASSERT_EQ (path.num_events (), 18);

  ASSERT_TRUE (path.interprocedural_p ());

  path_summary summary (path, false);
  ASSERT_EQ (summary.get_num_ranges (), 9);

  test_diagnostic_context dc;
  summary.print (&dc, true);
  ASSERT_STREQ
    ("  `test': events 1-2 (depth 0)\n"
     "    |\n"
     "    | (1): entering `test'\n"
     "    | (2): calling `make_boxed_int'\n"
     "    |\n"
     "    +--> `make_boxed_int': events 3-4 (depth 1)\n"
     "           |\n"
     "           | (3): entering `make_boxed_int'\n"
     "           | (4): calling `wrapped_malloc'\n"
     "           |\n"
     "           +--> `wrapped_malloc': events 5-6 (depth 2)\n"
     "                  |\n"
     "                  | (5): entering `wrapped_malloc'\n"
     "                  | (6): calling malloc\n"
     "                  |\n"
     "    <-------------+\n"
     "    |\n"
     "  `test': events 7-8 (depth 0)\n"
     "    |\n"
     "    | (7): returning to `test'\n"
     "    | (8): calling `free_boxed_int'\n"
     "    |\n"
     "    +--> `free_boxed_int': events 9-10 (depth 1)\n"
     "           |\n"
     "           | (9): entering `free_boxed_int'\n"
     "           | (10): calling `wrapped_free'\n"
     "           |\n"
     "           +--> `wrapped_free': events 11-12 (depth 2)\n"
     "                  |\n"
     "                  | (11): entering `wrapped_free'\n"
     "                  | (12): calling free\n"
     "                  |\n"
     "    <-------------+\n"
     "    |\n"
     "  `test': events 13-14 (depth 0)\n"
     "    |\n"
     "    | (13): returning to `test'\n"
     "    | (14): calling `free_boxed_int'\n"
     "    |\n"
     "    +--> `free_boxed_int': events 15-16 (depth 1)\n"
     "           |\n"
     "           | (15): entering `free_boxed_int'\n"
     "           | (16): calling `wrapped_free'\n"
     "           |\n"
     "           +--> `wrapped_free': events 17-18 (depth 2)\n"
     "                  |\n"
     "                  | (17): entering `wrapped_free'\n"
     "                  | (18): calling free\n"
     "                  |\n",
     pp_formatted_text (dc.printer));
}

/* Example where we pop the stack to an intermediate frame, rather than the
   initial one.  */

static void
test_interprocedural_path_2 (pretty_printer *event_pp)
{
  /* Build fndecls.  The types aren't quite right, but that
     doesn't matter for the purposes of this test.  */
  tree fntype_void_void
    = build_function_type_array (void_type_node, 0, NULL);
  tree fndecl_foo = build_fn_decl ("foo", fntype_void_void);
  tree fndecl_bar = build_fn_decl ("bar", fntype_void_void);
  tree fndecl_baz = build_fn_decl ("baz", fntype_void_void);

  test_diagnostic_path path (event_pp);
  path.add_entry (fndecl_foo, 0);
  path.add_call (fndecl_foo, 0, fndecl_bar);
  path.add_call (fndecl_bar, 1, fndecl_baz);
  path.add_return (fndecl_bar, 1);
  path.add_call (fndecl_bar, 1, fndecl_baz);
  ASSERT_EQ (path.num_events (), 8);

  ASSERT_TRUE (path.interprocedural_p ());

  path_summary summary (path, false);
  ASSERT_EQ (summary.get_num_ranges (), 5);

  test_diagnostic_context dc;
  summary.print (&dc, true);
  ASSERT_STREQ
    ("  `foo': events 1-2 (depth 0)\n"
     "    |\n"
     "    | (1): entering `foo'\n"
     "    | (2): calling `bar'\n"
     "    |\n"
     "    +--> `bar': events 3-4 (depth 1)\n"
     "           |\n"
     "           | (3): entering `bar'\n"
     "           | (4): calling `baz'\n"
     "           |\n"
     "           +--> `baz': event 5 (depth 2)\n"
     "                  |\n"
     "                  | (5): entering `baz'\n"
     "                  |\n"
     "           <------+\n"
     "           |\n"
     "         `bar': events 6-7 (depth 1)\n"
     "           |\n"
     "           | (6): returning to `bar'\n"
     "           | (7): calling `baz'\n"
     "           |\n"
     "           +--> `baz': event 8 (depth 2)\n"
     "                  |\n"
     "                  | (8): entering `baz'\n"
     "                  |\n",
     pp_formatted_text (dc.printer));
}

/* Verify that print_path_summary is sane in the face of a recursive
   diagnostic_path.  */

static void
test_recursion (pretty_printer *event_pp)
{
  tree fntype_void_void
    = build_function_type_array (void_type_node, 0, NULL);
  tree fndecl_factorial = build_fn_decl ("factorial", fntype_void_void);

 test_diagnostic_path path (event_pp);
  path.add_entry (fndecl_factorial, 0);
  for (int depth = 0; depth < 3; depth++)
    path.add_call (fndecl_factorial, depth, fndecl_factorial);
  ASSERT_EQ (path.num_events (), 7);

  ASSERT_TRUE (path.interprocedural_p ());

  path_summary summary (path, false);
  ASSERT_EQ (summary.get_num_ranges (), 4);

  test_diagnostic_context dc;
  summary.print (&dc, true);
  ASSERT_STREQ
    ("  `factorial': events 1-2 (depth 0)\n"
     "    |\n"
     "    | (1): entering `factorial'\n"
     "    | (2): calling `factorial'\n"
     "    |\n"
     "    +--> `factorial': events 3-4 (depth 1)\n"
     "           |\n"
     "           | (3): entering `factorial'\n"
     "           | (4): calling `factorial'\n"
     "           |\n"
     "           +--> `factorial': events 5-6 (depth 2)\n"
     "                  |\n"
     "                  | (5): entering `factorial'\n"
     "                  | (6): calling `factorial'\n"
     "                  |\n"
     "                  +--> `factorial': event 7 (depth 3)\n"
     "                         |\n"
     "                         | (7): entering `factorial'\n"
     "                         |\n",
     pp_formatted_text (dc.printer));
}

/* Run all of the selftests within this file.  */

void
tree_diagnostic_path_cc_tests ()
{
  auto_fix_quotes fix_quotes;
  pretty_printer *event_pp = global_dc->printer->clone ();
  pp_show_color (event_pp) = 0;
  test_empty_path (event_pp);
  test_intraprocedural_path (event_pp);
  test_interprocedural_path_1 (event_pp);
  test_interprocedural_path_2 (event_pp);
  test_recursion (event_pp);
  delete event_pp;
}

} // namespace selftest

#endif /* #if CHECKING_P */
