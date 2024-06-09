/* Paths through the code associated with a diagnostic.
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
#define INCLUDE_ALGORITHM
#define INCLUDE_MEMORY
#define INCLUDE_STRING
#define INCLUDE_VECTOR
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
#include "diagnostic-label-effects.h"
#include "selftest.h"
#include "selftest-diagnostic.h"
#include "text-art/theme.h"

/* Anonymous namespace for path-printing code.  */

namespace {

/* Subclass of range_label for showing a particular event
   when showing a consecutive run of events within a diagnostic_path as
   labelled ranges within one gcc_rich_location.  */

class path_label : public range_label
{
 public:
  path_label (const diagnostic_path *path, unsigned start_idx)
  : m_path (path), m_start_idx (start_idx), m_effects (*this)
  {}

  label_text get_text (unsigned range_idx) const final override
  {
    unsigned event_idx = m_start_idx + range_idx;
    const diagnostic_event &event = m_path->get_event (event_idx);

    /* Get the description of the event, perhaps with colorization:
       normally, we don't colorize within a range_label, but this
       is special-cased for diagnostic paths.  */
    const bool colorize = pp_show_color (global_dc->printer);
    label_text event_text (event.get_desc (colorize));
    gcc_assert (event_text.get ());

    const diagnostic_event::meaning meaning (event.get_meaning ());

    pretty_printer pp;
    pp_show_color (&pp) = colorize;
    diagnostic_event_id_t event_id (event_idx);

    pp_printf (&pp, "%@", &event_id);
    pp_space (&pp);

    if (meaning.m_verb == diagnostic_event::VERB_danger)
      if (text_art::theme *theme = global_dc->get_diagram_theme ())
	if (theme->emojis_p ())
	  {
	    pp_unicode_character (&pp, 0x26A0); /* U+26A0 WARNING SIGN.  */
	    /* Append U+FE0F VARIATION SELECTOR-16 to select the emoji
	       variation of the char.  */
	    pp_unicode_character (&pp, 0xFE0F);
	    /* U+26A0 WARNING SIGN has East_Asian_Width == Neutral, but in its
	       emoji variant is printed (by vte at least) with a 2nd half
	       overlapping the next char.  Hence we add two spaces here: a space
	       to be covered by this overlap, plus another space of padding.  */
	    pp_string (&pp, "  ");
	  }

    pp_printf (&pp, "%s", event_text.get ());

    label_text result = label_text::take (xstrdup (pp_formatted_text (&pp)));
    return result;
  }

  const label_effects *get_effects (unsigned /*range_idx*/) const
  {
    return &m_effects;
  }

 private:
  class path_label_effects : public label_effects
  {
  public:
    path_label_effects (const path_label &path_label)
    : m_path_label (path_label)
    {
    }
    bool has_in_edge (unsigned range_idx) const final override
    {
      if (const diagnostic_event *prev_event
	    = m_path_label.get_prev_event (range_idx))
	return prev_event->connect_to_next_event_p ();
      return false;
    }
    bool has_out_edge (unsigned range_idx) const final override
    {
      const diagnostic_event &event = m_path_label.get_event (range_idx);
      return event.connect_to_next_event_p ();
    }

  private:
    const path_label &m_path_label;
  };

  const diagnostic_event &get_event (unsigned range_idx) const
  {
    unsigned event_idx = m_start_idx + range_idx;
    return m_path->get_event (event_idx);
  }

  const diagnostic_event *get_prev_event (unsigned range_idx) const
  {
    if (m_start_idx + range_idx == 0)
      return nullptr;
    unsigned event_idx = m_start_idx + range_idx - 1;
    return &m_path->get_event (event_idx);
  }

  const diagnostic_path *m_path;
  unsigned m_start_idx;
  path_label_effects m_effects;
};

/* Return true if E1 and E2 can be consolidated into the same run of events
   when printing a diagnostic_path.  */

static bool
can_consolidate_events (const diagnostic_event &e1,
			const diagnostic_event &e2,
			bool check_locations)
{
  if (e1.get_thread_id () != e2.get_thread_id ())
    return false;

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

struct event_range;
struct path_summary;
class thread_event_printer;

/* A bundle of information about all of the events in a diagnostic_path
   relating to a specific path, for use by path_summary.  */

class per_thread_summary
{
public:
  per_thread_summary (label_text name, unsigned swimlane_idx)
  : m_name (std::move (name)),
    m_swimlane_idx (swimlane_idx),
    m_last_event (nullptr),
    m_min_depth (INT_MAX),
    m_max_depth (INT_MIN)
  {}

  void update_depth_limits (int stack_depth)
  {
    if (stack_depth < m_min_depth)
      m_min_depth = stack_depth;
    if (stack_depth > m_max_depth)
      m_max_depth = stack_depth;
  }

  const char *get_name () const { return m_name.get (); }
  unsigned get_swimlane_index () const { return m_swimlane_idx; }

  bool interprocedural_p () const;

private:
  friend struct path_summary;
  friend class thread_event_printer;
  friend struct event_range;

  const label_text m_name;

  /* The "swimlane index" is the order in which this per_thread_summary
     was created, for use when printing the events.  */
  const unsigned m_swimlane_idx;

  // The event ranges specific to this thread:
  auto_vec<event_range *> m_event_ranges;

  const diagnostic_event *m_last_event;

  int m_min_depth;
  int m_max_depth;
};

/* A range of consecutive events within a diagnostic_path, all within the
   same thread, and with the same fndecl and stack_depth, and which are suitable
   to print with a single call to diagnostic_show_locus.  */
struct event_range
{
  /* A struct for tracking the mergability of labels on a particular
     source line.  In particular, track information about links between
     labels to ensure that we only consolidate events involving links
     that the source-printing code is able to handle (splitting them
     otherwise).  */
  struct per_source_line_info
  {
    void init (int line)
    {
      m_line = line;
      m_has_in_edge = false;
      m_has_out_edge = false;
      m_min_label_source_column = INT_MAX;
      m_max_label_source_column = INT_MIN;
    }

    /* Return true if our source-printing/labelling/linking code can handle
       the events already on this source line, *and* a new event at COLUMN.  */
    bool
    can_add_label_for_event_p (bool has_in_edge,
			       const diagnostic_event *prev_event,
			       bool has_out_edge,
			       int column) const
    {
      /* Any existing in-edge has to be the left-most label on its
	 source line.  */
      if (m_has_in_edge && column < m_min_label_source_column)
	return false;
      /* Any existing out-edge has to be the right-most label on its
	 source line.  */
      if (m_has_out_edge && column > m_max_label_source_column)
	return false;
      /* Can't have more than one in-edge.  */
      if (m_has_in_edge && has_in_edge)
	return false;
      /* Can't have more than one out-edge.  */
      if (m_has_out_edge && has_out_edge)
	return false;

      if (has_in_edge)
	{
	  /* Any new in-edge needs to be the left-most label on its
	     source line.  */
	  if (column > m_min_label_source_column)
	    return false;

	  gcc_assert (prev_event);
	  const location_t prev_loc = prev_event->get_location ();
	  expanded_location prev_exploc
	    = linemap_client_expand_location_to_spelling_point
		(line_table, prev_loc, LOCATION_ASPECT_CARET);
	  /* The destination in-edge's line number has to be <= the
	     source out-edge's line number (if any).  */
	  if (prev_exploc.line >= m_line)
	    return false;
	}

      /* Any new out-edge needs to be the right-most label on its
	 source line.  */
      if (has_out_edge)
	if (column < m_max_label_source_column)
	  return false;

      /* All checks passed; we can add the new event at COLUMN.  */
      return true;
    }

    void
    add_label_for_event (bool has_in_edge, bool has_out_edge, int column)
    {
      if (has_in_edge)
	m_has_in_edge = true;
      if (has_out_edge)
	m_has_out_edge = true;
      m_min_label_source_column = std::min (m_min_label_source_column, column);
      m_max_label_source_column = std::max (m_max_label_source_column, column);
    }

    int m_line;
    bool m_has_in_edge;
    bool m_has_out_edge;
    int m_min_label_source_column;
    int m_max_label_source_column;
  };

  event_range (const diagnostic_path *path, unsigned start_idx,
	       const diagnostic_event &initial_event,
	       per_thread_summary &t,
	       bool show_event_links)
  : m_path (path),
    m_initial_event (initial_event),
    m_fndecl (initial_event.get_fndecl ()),
    m_stack_depth (initial_event.get_stack_depth ()),
    m_start_idx (start_idx), m_end_idx (start_idx),
    m_path_label (path, start_idx),
    m_richloc (initial_event.get_location (), &m_path_label),
    m_thread_id (initial_event.get_thread_id ()),
    m_per_thread_summary (t),
    m_show_event_links (show_event_links)
  {
    if (m_show_event_links)
      {
	expanded_location exploc
	  = linemap_client_expand_location_to_spelling_point
	  (line_table, initial_event.get_location (), LOCATION_ASPECT_CARET);
	per_source_line_info &source_line_info
	  = get_per_source_line_info (exploc.line);

	const diagnostic_event *prev_thread_event = t.m_last_event;
	const bool has_in_edge
	  = (prev_thread_event
	     ? prev_thread_event->connect_to_next_event_p ()
	     : false);
	const bool has_out_edge = initial_event.connect_to_next_event_p ();

	source_line_info.add_label_for_event
	  (has_in_edge, has_out_edge, exploc.column);
      }
  }

  per_source_line_info &
  get_per_source_line_info (int source_line)
  {
    bool existed = false;
    per_source_line_info &result
      = m_source_line_info_map.get_or_insert (source_line, &existed);
    if (!existed)
      result.init (source_line);
    return result;
  }

  bool maybe_add_event (const diagnostic_event &new_ev, unsigned idx,
			bool check_rich_locations)
  {
    if (!can_consolidate_events (m_initial_event, new_ev,
				 check_rich_locations))
      return false;

    /* Verify compatibility of the new label and existing labels
       with respect to the link-printing code.  */
    expanded_location exploc
      = linemap_client_expand_location_to_spelling_point
      (line_table, new_ev.get_location (), LOCATION_ASPECT_CARET);
    per_source_line_info &source_line_info
      = get_per_source_line_info (exploc.line);
    const diagnostic_event *prev_event = nullptr;
    if (idx > 0)
      prev_event = &m_path->get_event (idx - 1);
    const bool has_in_edge = (prev_event
			      ? prev_event->connect_to_next_event_p ()
			      : false);
    const bool has_out_edge = new_ev.connect_to_next_event_p ();
    if (m_show_event_links)
      if (!source_line_info.can_add_label_for_event_p
	  (has_in_edge, prev_event,
	   has_out_edge, exploc.column))
	return false;

    /* Potentially verify that the locations are sufficiently close.  */
    if (check_rich_locations)
      if (!m_richloc.add_location_if_nearby (new_ev.get_location (),
					     false, &m_path_label))
	return false;

    m_end_idx = idx;
    m_per_thread_summary.m_last_event = &new_ev;

    if (m_show_event_links)
      source_line_info.add_label_for_event
	(has_in_edge, has_out_edge, exploc.column);

    return true;
  }

  /* Print the events in this range to DC, typically as a single
     call to the printer's diagnostic_show_locus.  */

  void print (diagnostic_context *dc, pretty_printer *pp,
	      diagnostic_source_effect_info *effect_info)
  {
    location_t initial_loc = m_initial_event.get_location ();

    /* Emit a span indicating the filename (and line/column) if the
       line has changed relative to the last call to
       diagnostic_show_locus.  */
    if (dc->m_source_printing.enabled)
      {
	expanded_location exploc
	  = linemap_client_expand_location_to_spelling_point
	  (line_table, initial_loc, LOCATION_ASPECT_CARET);
	if (exploc.file != LOCATION_FILE (dc->m_last_location))
	  diagnostic_start_span (dc) (dc, exploc);
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
	    pp_printf (pp, " %@: %s", &event_id, event_text.get ());
	    pp_newline (pp);
	  }
	return;
      }

    /* Call diagnostic_show_locus to show the events using labels.  */
    diagnostic_show_locus (dc, &m_richloc, DK_DIAGNOSTIC_PATH, pp,
			   effect_info);

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
  diagnostic_thread_id_t m_thread_id;
  per_thread_summary &m_per_thread_summary;
  hash_map<int_hash<int, -1, -2>,
	   per_source_line_info> m_source_line_info_map;
  bool m_show_event_links;
};

/* A struct for grouping together the events in a diagnostic_path into
   ranges of events, partitioned by thread and by stack frame (i.e. by fndecl
   and stack depth).  */

struct path_summary
{
  path_summary (const diagnostic_path &path,
		bool check_rich_locations,
		bool show_event_links = true);

  unsigned get_num_ranges () const { return m_ranges.length (); }
  bool multithreaded_p () const { return m_per_thread_summary.length () > 1; }

  const per_thread_summary &get_events_for_thread_id (diagnostic_thread_id_t tid)
  {
    per_thread_summary **slot = m_thread_id_to_events.get (tid);
    gcc_assert (slot);
    gcc_assert (*slot);
    return **slot;
  }

  auto_delete_vec <event_range> m_ranges;
  auto_delete_vec <per_thread_summary> m_per_thread_summary;
  hash_map<int_hash<diagnostic_thread_id_t, -1, -2>,
	   per_thread_summary *> m_thread_id_to_events;

private:
  per_thread_summary &
  get_or_create_events_for_thread_id (const diagnostic_path &path,
				      diagnostic_thread_id_t tid)
  {
    if (per_thread_summary **slot = m_thread_id_to_events.get (tid))
      return **slot;

    const diagnostic_thread &thread = path.get_thread (tid);
    per_thread_summary *pts = new per_thread_summary (thread.get_name (false),
						m_per_thread_summary.length ());
    m_thread_id_to_events.put (tid, pts);
    m_per_thread_summary.safe_push (pts);
    return *pts;
  }
};

/* Return true iff there is more than one stack frame used by the events
   of this thread.  */

bool
per_thread_summary::interprocedural_p () const
{
  if (m_event_ranges.is_empty ())
    return false;
  tree first_fndecl = m_event_ranges[0]->m_fndecl;
  int first_stack_depth = m_event_ranges[0]->m_stack_depth;
  for (auto range : m_event_ranges)
    {
      if (range->m_fndecl != first_fndecl)
	return true;
      if (range->m_stack_depth != first_stack_depth)
	return true;
    }
  return false;
}

/* path_summary's ctor.  */

path_summary::path_summary (const diagnostic_path &path,
			    bool check_rich_locations,
			    bool show_event_links)
{
  const unsigned num_events = path.num_events ();

  event_range *cur_event_range = NULL;
  for (unsigned idx = 0; idx < num_events; idx++)
    {
      const diagnostic_event &event = path.get_event (idx);
      const diagnostic_thread_id_t thread_id = event.get_thread_id ();
      per_thread_summary &pts
	= get_or_create_events_for_thread_id (path, thread_id);

      pts.update_depth_limits (event.get_stack_depth ());

      if (cur_event_range)
	if (cur_event_range->maybe_add_event (event, idx, check_rich_locations))
	  continue;

      cur_event_range = new event_range (&path, idx, event, pts,
					 show_event_links);
      m_ranges.safe_push (cur_event_range);
      pts.m_event_ranges.safe_push (cur_event_range);
      pts.m_last_event = &event;
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

static const int base_indent = 2;
static const int per_frame_indent = 2;

/* A bundle of state for printing event_range instances for a particular
   thread.  */

class thread_event_printer
{
public:
  thread_event_printer (const per_thread_summary &t, bool show_depths)
  : m_per_thread_summary (t),
    m_show_depths (show_depths),
    m_cur_indent (base_indent),
    m_vbar_column_for_depth (),
    m_num_printed (0)
  {
  }

  /* Get the previous event_range within this thread, if any.  */
  const event_range *get_any_prev_range () const
  {
    if (m_num_printed > 0)
      return m_per_thread_summary.m_event_ranges[m_num_printed - 1];
    else
      return nullptr;
  }

  /* Get the next event_range within this thread, if any.  */
  const event_range *get_any_next_range () const
  {
    if (m_num_printed < m_per_thread_summary.m_event_ranges.length () - 1)
      return m_per_thread_summary.m_event_ranges[m_num_printed + 1];
    else
      return nullptr;
  }

  void
  print_swimlane_for_event_range (diagnostic_context *dc,
				  pretty_printer *pp,
				  event_range *range,
				  diagnostic_source_effect_info *effect_info)
  {
    const char *const line_color = "path";
    const char *start_line_color
      = colorize_start (pp_show_color (pp), line_color);
    const char *end_line_color = colorize_stop (pp_show_color (pp));

    text_art::ascii_theme fallback_theme;
    text_art::theme *theme = dc->get_diagram_theme ();
    if (!theme)
      theme = &fallback_theme;

    cppchar_t depth_marker_char = theme->get_cppchar
      (text_art::theme::cell_kind::INTERPROCEDURAL_DEPTH_MARKER);
    /* e.g. "|".  */

    const bool interprocedural_p = m_per_thread_summary.interprocedural_p ();

    write_indent (pp, m_cur_indent);
    if (const event_range *prev_range = get_any_prev_range ())
      {
	if (range->m_stack_depth > prev_range->m_stack_depth)
	  {
	    gcc_assert (interprocedural_p);
	    /* Show pushed stack frame(s).  */
	    cppchar_t left = theme->get_cppchar
	      (text_art::theme::cell_kind::INTERPROCEDURAL_PUSH_FRAME_LEFT);
	    cppchar_t middle = theme->get_cppchar
	      (text_art::theme::cell_kind::INTERPROCEDURAL_PUSH_FRAME_MIDDLE);
	    cppchar_t right = theme->get_cppchar
	      (text_art::theme::cell_kind::INTERPROCEDURAL_PUSH_FRAME_RIGHT);
	    /* e.g. "+--> ".  */
	    pp_string (pp, start_line_color);
	    pp_unicode_character (pp, left);
	    pp_unicode_character (pp, middle);
	    pp_unicode_character (pp, middle);
	    pp_unicode_character (pp, right);
	    pp_space (pp);
	    pp_string (pp, end_line_color);
	    m_cur_indent += 5;
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
    if (m_show_depths)
      pp_printf (pp, " (depth %i)", range->m_stack_depth);
    pp_newline (pp);

    /* Print a run of events.  */
    if (interprocedural_p)
      {
	write_indent (pp, m_cur_indent + per_frame_indent);
	pp_string (pp, start_line_color);
	pp_unicode_character (pp, depth_marker_char);
	pp_string (pp, end_line_color);
	pp_newline (pp);

	char *saved_prefix = pp_take_prefix (pp);
	char *prefix;
	{
	  pretty_printer tmp_pp;
	  write_indent (&tmp_pp, m_cur_indent + per_frame_indent);
	  pp_string (&tmp_pp, start_line_color);
	  pp_unicode_character (&tmp_pp, depth_marker_char);
	  pp_string (&tmp_pp, end_line_color);
	  prefix = xstrdup (pp_formatted_text (&tmp_pp));
	}
	pp_set_prefix (pp, prefix);
	pp_prefixing_rule (pp) = DIAGNOSTICS_SHOW_PREFIX_EVERY_LINE;
	range->print (dc, pp, effect_info);
	pp_set_prefix (pp, saved_prefix);

	write_indent (pp, m_cur_indent + per_frame_indent);
	pp_string (pp, start_line_color);
	pp_unicode_character (pp, depth_marker_char);
	pp_string (pp, end_line_color);
	pp_newline (pp);
      }
    else
      range->print (dc, pp, effect_info);

    if (const event_range *next_range = get_any_next_range ())
      {
	if (range->m_stack_depth > next_range->m_stack_depth)
	  {
	    if (m_vbar_column_for_depth.get (next_range->m_stack_depth))
	      {
		/* Show returning from stack frame(s), by printing
		   something like:
		   "                   |\n"
		   "     <-------------+\n"
		   "     |\n".  */
		gcc_assert (interprocedural_p);
		cppchar_t left = theme->get_cppchar
		  (text_art::theme::cell_kind::INTERPROCEDURAL_POP_FRAMES_LEFT);
		cppchar_t middle = theme->get_cppchar
		  (text_art::theme::cell_kind::INTERPROCEDURAL_POP_FRAMES_MIDDLE);
		cppchar_t right = theme->get_cppchar
		  (text_art::theme::cell_kind::INTERPROCEDURAL_POP_FRAMES_RIGHT);
		int vbar_for_next_frame
		  = *m_vbar_column_for_depth.get (next_range->m_stack_depth);

		int indent_for_next_frame
		  = vbar_for_next_frame - per_frame_indent;
		write_indent (pp, vbar_for_next_frame);
		pp_string (pp, start_line_color);
		pp_unicode_character (pp, left);
		for (int i = indent_for_next_frame + per_frame_indent;
		     i < m_cur_indent + per_frame_indent - 1; i++)
		  pp_unicode_character (pp, middle);
		pp_unicode_character (pp, right);
		pp_string (pp, end_line_color);
		pp_newline (pp);
		m_cur_indent = indent_for_next_frame;

		write_indent (pp, vbar_for_next_frame);
		pp_string (pp, start_line_color);
		pp_unicode_character (pp, depth_marker_char);
		pp_string (pp, end_line_color);
		pp_newline (pp);
	      }
	    else
	      {
		/* Handle disjoint paths (e.g. a callback at some later
		   time).  */
		m_cur_indent = base_indent;
	      }
	  }
	else if (range->m_stack_depth < next_range->m_stack_depth)
	  {
	    /* Prepare to show pushed stack frame.  */
	    gcc_assert (interprocedural_p);
	    gcc_assert (range->m_stack_depth != EMPTY);
	    gcc_assert (range->m_stack_depth != DELETED);
	    m_vbar_column_for_depth.put (range->m_stack_depth,
					 m_cur_indent + per_frame_indent);
	    m_cur_indent += per_frame_indent;
	  }
      }

    m_num_printed++;
  }

  int get_cur_indent () const { return m_cur_indent; }

private:
  const per_thread_summary &m_per_thread_summary;
  bool m_show_depths;

  /* Print the ranges.  */
  int m_cur_indent;

  /* Keep track of column numbers of existing '|' characters for
     stack depths we've already printed.  */
  static const int EMPTY = -1;
  static const int DELETED = -2;
  typedef int_hash <int, EMPTY, DELETED> vbar_hash;
  hash_map <vbar_hash, int> m_vbar_column_for_depth;

  /* How many event ranges within this swimlane have we printed.
     This is the index of the next event_range to print.  */
  unsigned  m_num_printed;
};

/* Print path_summary PS to DC, giving an overview of the interprocedural
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

static void
print_path_summary_as_text (const path_summary *ps, diagnostic_context *dc,
			    bool show_depths)
{
  pretty_printer *pp = dc->printer;

  std::vector<thread_event_printer> thread_event_printers;
  for (auto t : ps->m_per_thread_summary)
    thread_event_printers.push_back (thread_event_printer (*t, show_depths));

  unsigned i;
  event_range *range;
  int last_out_edge_column = -1;
  FOR_EACH_VEC_ELT (ps->m_ranges, i, range)
    {
      const int swimlane_idx
	= range->m_per_thread_summary.get_swimlane_index ();
      if (ps->multithreaded_p ())
	if (i == 0 || ps->m_ranges[i - 1]->m_thread_id != range->m_thread_id)
	  {
	    if (i > 0)
	      pp_newline (pp);
	    pp_printf (pp, "Thread: %qs",
		       range->m_per_thread_summary.get_name ());
	    pp_newline (pp);
	  }
      thread_event_printer &tep = thread_event_printers[swimlane_idx];
      /* Wire up any trailing out-edge from previous range to leading in-edge
	 of this range.  */
      diagnostic_source_effect_info effect_info;
      effect_info.m_leading_in_edge_column = last_out_edge_column;
      tep.print_swimlane_for_event_range (dc, pp, range, &effect_info);
      last_out_edge_column = effect_info.m_trailing_out_edge_column;
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

  switch (context->get_path_format ())
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
	    gcc_assert (event_text.get ());
	    diagnostic_event_id_t event_id (i);
	    if (context->show_path_depths_p ())
	      {
		int stack_depth = event.get_stack_depth ();
		tree fndecl = event.get_fndecl ();
		/* -fdiagnostics-path-format=separate-events doesn't print
		   fndecl information, so with -fdiagnostics-show-path-depths
		   print the fndecls too, if any.  */
		if (fndecl)
		  inform (event.get_location (),
			  "%@ %s (fndecl %qD, depth %i)",
			  &event_id, event_text.get (),
			  fndecl, stack_depth);
		else
		  inform (event.get_location (),
			  "%@ %s (depth %i)",
			  &event_id, event_text.get (),
			  stack_depth);
	      }
	    else
	      inform (event.get_location (),
		      "%@ %s", &event_id, event_text.get ());
	  }
      }
      break;

    case DPF_INLINE_EVENTS:
      {
	/* Consolidate related events.  */
	path_summary summary (*path, true,
			      context->m_source_printing.show_event_links_p);
	char *saved_prefix = pp_take_prefix (context->printer);
	pp_set_prefix (context->printer, NULL);
	print_path_summary_as_text (&summary, context,
				    context->show_path_depths_p ());
	pp_flush (context->printer);
	pp_set_prefix (context->printer, saved_prefix);
      }
      break;
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
      event_obj->set_string ("description", event_text.get ());
      if (tree fndecl = event.get_fndecl ())
	{
	  const char *function
	    = identifier_to_locale (lang_hooks.decl_printable_name (fndecl, 2));
	  event_obj->set_string ("function", function);
	}
      event_obj->set_integer ("depth", event.get_stack_depth ());
      path_array->append (event_obj);
    }
  return path_array;
}

#if CHECKING_P

/* Disable warnings about missing quoting in GCC diagnostics for the print
   calls in the tests below.  */
#if __GNUC__ >= 10
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wformat-diag"
#endif

namespace selftest {

/* Return true iff all events in PATH have locations for which column data
   is available, so that selftests that require precise string output can
   bail out for awkward line_table cases.  */

static bool
path_events_have_column_data_p (const diagnostic_path &path)
{
  for (unsigned idx = 0; idx < path.num_events (); idx++)
    {
      location_t event_loc = path.get_event (idx).get_location ();
      if (line_table->get_pure_location (event_loc)
	  > LINE_MAP_MAX_LOCATION_WITH_COLS)
	return false;
      if (line_table->get_start (event_loc) > LINE_MAP_MAX_LOCATION_WITH_COLS)
	return false;
      if (line_table->get_finish (event_loc) > LINE_MAP_MAX_LOCATION_WITH_COLS)
	return false;
    }
  return true;
}

/* A subclass of simple_diagnostic_path that adds member functions
   for adding test events and suppresses translation of these events.  */

class test_diagnostic_path : public simple_diagnostic_path
{
 public:
  test_diagnostic_path (pretty_printer *event_pp)
  : simple_diagnostic_path (event_pp)
  {
    disable_event_localization ();
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
  print_path_summary_as_text (&summary, &dc, true);
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
  print_path_summary_as_text (&summary, &dc, true);
  ASSERT_STREQ ("  `foo': events 1-2 (depth 0)\n"
		" (1): first `free'\n"
		" (2): double `free'\n",
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

  {
    test_diagnostic_context dc;
    dc.set_text_art_charset (DIAGNOSTICS_TEXT_ART_CHARSET_ASCII);
    print_path_summary_as_text (&summary, &dc, true);
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
  {
    test_diagnostic_context dc;
    dc.set_text_art_charset (DIAGNOSTICS_TEXT_ART_CHARSET_UNICODE);
    print_path_summary_as_text (&summary, &dc, true);
    ASSERT_STREQ
      ("  `test': events 1-2 (depth 0)\n"
       "    │\n"
       "    │ (1): entering `test'\n"
       "    │ (2): calling `make_boxed_int'\n"
       "    │\n"
       "    └──> `make_boxed_int': events 3-4 (depth 1)\n"
       "           │\n"
       "           │ (3): entering `make_boxed_int'\n"
       "           │ (4): calling `wrapped_malloc'\n"
       "           │\n"
       "           └──> `wrapped_malloc': events 5-6 (depth 2)\n"
       "                  │\n"
       "                  │ (5): entering `wrapped_malloc'\n"
       "                  │ (6): calling malloc\n"
       "                  │\n"
       "    <─────────────┘\n"
       "    │\n"
       "  `test': events 7-8 (depth 0)\n"
       "    │\n"
       "    │ (7): returning to `test'\n"
       "    │ (8): calling `free_boxed_int'\n"
       "    │\n"
       "    └──> `free_boxed_int': events 9-10 (depth 1)\n"
       "           │\n"
       "           │ (9): entering `free_boxed_int'\n"
       "           │ (10): calling `wrapped_free'\n"
       "           │\n"
       "           └──> `wrapped_free': events 11-12 (depth 2)\n"
       "                  │\n"
       "                  │ (11): entering `wrapped_free'\n"
       "                  │ (12): calling free\n"
       "                  │\n"
       "    <─────────────┘\n"
       "    │\n"
       "  `test': events 13-14 (depth 0)\n"
       "    │\n"
       "    │ (13): returning to `test'\n"
       "    │ (14): calling `free_boxed_int'\n"
       "    │\n"
       "    └──> `free_boxed_int': events 15-16 (depth 1)\n"
       "           │\n"
       "           │ (15): entering `free_boxed_int'\n"
       "           │ (16): calling `wrapped_free'\n"
       "           │\n"
       "           └──> `wrapped_free': events 17-18 (depth 2)\n"
       "                  │\n"
       "                  │ (17): entering `wrapped_free'\n"
       "                  │ (18): calling free\n"
       "                  │\n",
       pp_formatted_text (dc.printer));
  }

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

  {
    test_diagnostic_context dc;
    dc.set_text_art_charset (DIAGNOSTICS_TEXT_ART_CHARSET_ASCII);
    print_path_summary_as_text (&summary, &dc, true);
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
  {
    test_diagnostic_context dc;
    dc.set_text_art_charset (DIAGNOSTICS_TEXT_ART_CHARSET_UNICODE);
    print_path_summary_as_text (&summary, &dc, true);
    ASSERT_STREQ
      ("  `foo': events 1-2 (depth 0)\n"
       "    │\n"
       "    │ (1): entering `foo'\n"
       "    │ (2): calling `bar'\n"
       "    │\n"
       "    └──> `bar': events 3-4 (depth 1)\n"
       "           │\n"
       "           │ (3): entering `bar'\n"
       "           │ (4): calling `baz'\n"
       "           │\n"
       "           └──> `baz': event 5 (depth 2)\n"
       "                  │\n"
       "                  │ (5): entering `baz'\n"
       "                  │\n"
       "           <──────┘\n"
       "           │\n"
       "         `bar': events 6-7 (depth 1)\n"
       "           │\n"
       "           │ (6): returning to `bar'\n"
       "           │ (7): calling `baz'\n"
       "           │\n"
       "           └──> `baz': event 8 (depth 2)\n"
       "                  │\n"
       "                  │ (8): entering `baz'\n"
       "                  │\n",
       pp_formatted_text (dc.printer));
  }
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

  {
    test_diagnostic_context dc;
    dc.set_text_art_charset (DIAGNOSTICS_TEXT_ART_CHARSET_ASCII);
    print_path_summary_as_text (&summary, &dc, true);
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
  {
    test_diagnostic_context dc;
    dc.set_text_art_charset (DIAGNOSTICS_TEXT_ART_CHARSET_UNICODE);
    print_path_summary_as_text (&summary, &dc, true);
    ASSERT_STREQ
      ("  `factorial': events 1-2 (depth 0)\n"
       "    │\n"
       "    │ (1): entering `factorial'\n"
       "    │ (2): calling `factorial'\n"
       "    │\n"
       "    └──> `factorial': events 3-4 (depth 1)\n"
       "           │\n"
       "           │ (3): entering `factorial'\n"
       "           │ (4): calling `factorial'\n"
       "           │\n"
       "           └──> `factorial': events 5-6 (depth 2)\n"
       "                  │\n"
       "                  │ (5): entering `factorial'\n"
       "                  │ (6): calling `factorial'\n"
       "                  │\n"
       "                  └──> `factorial': event 7 (depth 3)\n"
       "                         │\n"
       "                         │ (7): entering `factorial'\n"
       "                         │\n",
       pp_formatted_text (dc.printer));
  }
}

/* Helper class for writing tests of control flow visualization.  */

class control_flow_test
{
public:
  control_flow_test (const location &loc,
		     const line_table_case &case_,
		     const char *content)
  : m_tmp_file (loc, ".c", content,
		/* gcc_rich_location::add_location_if_nearby implicitly
		   uses global_dc's file_cache, so we need to evict
		   tmp when we're done.  */
		&global_dc->get_file_cache ()),
    m_ltt (case_)
  {
    m_ord_map
      = linemap_check_ordinary (linemap_add (line_table, LC_ENTER, false,
					     m_tmp_file.get_filename (), 0));
    linemap_line_start (line_table, 1, 100);
  }

  location_t get_line_and_column (int line, int column)
  {
    return linemap_position_for_line_and_column (line_table, m_ord_map,
						 line, column);
  }

  location_t get_line_and_columns (int line, int first_column, int last_column)
  {
    return get_line_and_columns (line,
				 first_column, first_column, last_column);
  }

  location_t get_line_and_columns (int line,
				   int first_column,
				   int caret_column,
				   int last_column)
  {
    return make_location (get_line_and_column (line, caret_column),
			  get_line_and_column (line, first_column),
			  get_line_and_column (line, last_column));
  }

private:
  temp_source_file m_tmp_file;
  line_table_test m_ltt;
  const line_map_ordinary *m_ord_map;
};

/* Example of event edges where all events can go in the same layout,
   testing the 6 combinations of:
   - ASCII vs Unicode vs event links off
   - line numbering on and off.  */

static void
test_control_flow_1 (const line_table_case &case_,
		     pretty_printer *event_pp)
{
  /* Create a tempfile and write some text to it.
     ...000000000111111111122222222223333333333.
     ...123456789012345678901234567890123456789.  */
  const char *content
    = ("int test (int *p)\n" /* line 1.  */
       "{\n"                 /* line 2.  */
       "  if (p)\n"          /* line 3.  */
       "    return 0;\n"     /* line 4.  */
       "  return *p;\n"      /* line 5.  */
       "}\n");               /* line 6.  */

  control_flow_test t (SELFTEST_LOCATION, case_, content);

  const location_t conditional = t.get_line_and_column (3, 7);
  const location_t cfg_dest = t.get_line_and_column (5, 10);

  test_diagnostic_path path (event_pp);
  path.add_event (conditional, NULL_TREE, 0,
		  "following %qs branch (when %qs is NULL)...",
		  "false", "p");
  path.connect_to_next_event ();

  path.add_event (cfg_dest, NULL_TREE, 0,
		  "...to here");
  path.add_event (cfg_dest, NULL_TREE, 0,
		  "dereference of NULL %qs",
		  "p");

  if (!path_events_have_column_data_p (path))
    return;

  path_summary summary (path, true /*false*/);

  {
    test_diagnostic_context dc;
    dc.set_text_art_charset (DIAGNOSTICS_TEXT_ART_CHARSET_ASCII);
    dc.m_source_printing.show_event_links_p = true;
    print_path_summary_as_text (&summary, &dc, false);
    ASSERT_STREQ
      ("  events 1-3\n"
       "FILENAME:3:7:\n"
       "   if (p)\n"
       "       ^\n"
       "       |\n"
       "       (1) following `false' branch (when `p' is NULL)... ->-+\n"
       "                                                             |\n"
       "FILENAME:5:10:\n"
       "                                                             |\n"
       "+------------------------------------------------------------+\n"
       "|  return *p;\n"
       "|         ~\n"
       "|         |\n"
       "+-------->(2) ...to here\n"
       "          (3) dereference of NULL `p'\n",
       pp_formatted_text (dc.printer));
  }
  {
    test_diagnostic_context dc;
    dc.set_text_art_charset (DIAGNOSTICS_TEXT_ART_CHARSET_ASCII);
    dc.m_source_printing.show_event_links_p = false;
    print_path_summary_as_text (&summary, &dc, false);
    ASSERT_STREQ
      ("  events 1-3\n"
       "FILENAME:3:7:\n"
       "   if (p)\n"
       "       ^\n"
       "       |\n"
       "       (1) following `false' branch (when `p' is NULL)...\n"
       "FILENAME:5:10:\n"
       "   return *p;\n"
       "          ~\n"
       "          |\n"
       "          (2) ...to here\n"
       "          (3) dereference of NULL `p'\n",
       pp_formatted_text (dc.printer));
  }
  {
    test_diagnostic_context dc;
    dc.set_text_art_charset (DIAGNOSTICS_TEXT_ART_CHARSET_ASCII);
    dc.m_source_printing.show_line_numbers_p = true;
    dc.m_source_printing.show_event_links_p = true;
    print_path_summary_as_text (&summary, &dc, false);
    ASSERT_STREQ
      ("  events 1-3\n"
       "FILENAME:3:7:\n"
       "    3 |   if (p)\n"
       "      |       ^\n"
       "      |       |\n"
       "      |       (1) following `false' branch (when `p' is NULL)... ->-+\n"
       "      |                                                             |\n"
       "      |                                                             |\n"
       "      |+------------------------------------------------------------+\n"
       "    4 ||    return 0;\n"
       "    5 ||  return *p;\n"
       "      ||         ~\n"
       "      ||         |\n"
       "      |+-------->(2) ...to here\n"
       "      |          (3) dereference of NULL `p'\n",
       pp_formatted_text (dc.printer));
  }
  {
    test_diagnostic_context dc;
    dc.set_text_art_charset (DIAGNOSTICS_TEXT_ART_CHARSET_ASCII);
    dc.m_source_printing.show_line_numbers_p = true;
    dc.m_source_printing.show_event_links_p = false;
    print_path_summary_as_text (&summary, &dc, false);
    ASSERT_STREQ
      ("  events 1-3\n"
       "FILENAME:3:7:\n"
       "    3 |   if (p)\n"
       "      |       ^\n"
       "      |       |\n"
       "      |       (1) following `false' branch (when `p' is NULL)...\n"
       "    4 |     return 0;\n"
       "    5 |   return *p;\n"
       "      |          ~\n"
       "      |          |\n"
       "      |          (2) ...to here\n"
       "      |          (3) dereference of NULL `p'\n",
       pp_formatted_text (dc.printer));
  }
  {
    test_diagnostic_context dc;
    dc.set_text_art_charset (DIAGNOSTICS_TEXT_ART_CHARSET_UNICODE);
    dc.m_source_printing.show_event_links_p = true;
    print_path_summary_as_text (&summary, &dc, false);
    ASSERT_STREQ
      ("  events 1-3\n"
       "FILENAME:3:7:\n"
       "   if (p)\n"
       "       ^\n"
       "       |\n"
       "       (1) following `false' branch (when `p' is NULL)... ─>─┐\n"
       "                                                             │\n"
       "FILENAME:5:10:\n"
       "                                                             │\n"
       "┌────────────────────────────────────────────────────────────┘\n"
       "│  return *p;\n"
       "│         ~\n"
       "│         |\n"
       "└────────>(2) ...to here\n"
       "          (3) dereference of NULL `p'\n",
       pp_formatted_text (dc.printer));
  }
  {
    test_diagnostic_context dc;
    dc.set_text_art_charset (DIAGNOSTICS_TEXT_ART_CHARSET_UNICODE);
    dc.m_source_printing.show_event_links_p = true;
    dc.m_source_printing.show_line_numbers_p = true;
    print_path_summary_as_text (&summary, &dc, false);
    ASSERT_STREQ
      ("  events 1-3\n"
       "FILENAME:3:7:\n"
       "    3 |   if (p)\n"
       "      |       ^\n"
       "      |       |\n"
       "      |       (1) following `false' branch (when `p' is NULL)... ─>─┐\n"
       "      |                                                             │\n"
       "      |                                                             │\n"
       "      |┌────────────────────────────────────────────────────────────┘\n"
       "    4 |│    return 0;\n"
       "    5 |│  return *p;\n"
       "      |│         ~\n"
       "      |│         |\n"
       "      |└────────>(2) ...to here\n"
       "      |          (3) dereference of NULL `p'\n",
       pp_formatted_text (dc.printer));
  }
}

/* Complex example involving a backedge.  */

static void
test_control_flow_2 (const line_table_case &case_,
		     pretty_printer *event_pp)
{
  /* Create a tempfile and write some text to it.
     ...000000000111111111122222222223333333333.
     ...123456789012345678901234567890123456789.  */
  const char *content
    = ("int for_loop_noop_next (struct node *n)\n" /* <--------- line 1.  */
       "{\n" /* <----------------------------------------------- line 2.  */
       "  int sum = 0;\n" /* <---------------------------------- line 3.  */
       "  for (struct node *iter = n; iter; iter->next)\n" /* <- line 4.  */
       "    sum += n->val;\n" /* <------------------------------ line 5.  */
       "  return sum;\n" /* <----------------------------------- line 6.  */
       "}\n");  /* <-------------------------------------------- line 7.  */
  /* Adapted from infinite-loop-linked-list.c where
     "iter->next" should be "iter = iter->next".  */

  control_flow_test t (SELFTEST_LOCATION, case_, content);

  const location_t iter_test = t.get_line_and_columns (4, 31, 34);
  const location_t loop_body_start = t.get_line_and_columns (5, 12, 17);
  const location_t loop_body_end = t.get_line_and_columns (5, 5, 9, 17);

  test_diagnostic_path path (event_pp);
  path.add_event (iter_test, NULL_TREE, 0, "infinite loop here");

  path.add_event (iter_test, NULL_TREE, 0, "looping from here...");
  path.connect_to_next_event ();

  path.add_event (loop_body_start, NULL_TREE, 0, "...to here");

  path.add_event (loop_body_end, NULL_TREE, 0, "looping back...");
  path.connect_to_next_event ();

  path.add_event (iter_test, NULL_TREE, 0, "...to here");

  if (!path_events_have_column_data_p (path))
    return;

  path_summary summary (path, true);

  {
    test_diagnostic_context dc;
    dc.set_text_art_charset (DIAGNOSTICS_TEXT_ART_CHARSET_ASCII);
    dc.m_source_printing.show_event_links_p = true;
    dc.m_source_printing.show_line_numbers_p = true;
    print_path_summary_as_text (&summary, &dc, false);
    ASSERT_STREQ
      ("  events 1-3\n"
       "FILENAME:4:31:\n"
       "    4 |   for (struct node *iter = n; iter; iter->next)\n"
       "      |                               ^~~~\n"
       "      |                               |\n"
       "      |                               (1) infinite loop here\n"
       "      |                               (2) looping from here... ->-+\n"
       "      |                                                           |\n"
       "      |                                                           |\n"
       "      |+----------------------------------------------------------+\n"
       "    5 ||    sum += n->val;\n"
       "      ||           ~~~~~~              \n"
       "      ||           |\n"
       "      |+---------->(3) ...to here\n"
       /* We need to start an new event_range here as event (4) is to the
	  left of event (3), and thus (4) would mess up the in-edge to (3).  */
       "  event 4\n"
       "    5 |     sum += n->val;\n"
       "      |     ~~~~^~~~~~~~~\n"
       "      |         |\n"
       "      |         (4) looping back... ->-+\n"
       "      |                                |\n"
       /* We need to start an new event_range here as event (4) with an
	  out-edge is on a later line (line 5) than its destination event (5),
	  on line 4.  */
       "  event 5\n"
       "      |                                |\n"
       "      |+-------------------------------+\n"
       "    4 ||  for (struct node *iter = n; iter; iter->next)\n"
       "      ||                              ^~~~\n"
       "      ||                              |\n"
       "      |+----------------------------->(5) ...to here\n",
       pp_formatted_text (dc.printer));
  }
}

/* Complex example involving a backedge and both an in-edge and out-edge
   on the same line.  */

static void
test_control_flow_3 (const line_table_case &case_,
		     pretty_printer *event_pp)
{
  /* Create a tempfile and write some text to it.
     ...000000000111111111122222222223333333333.
     ...123456789012345678901234567890123456789.  */
  const char *content
    = ("void test_missing_comparison_in_for_condition_1 (int n)\n"
       "{\n" /* <------------------------- line 2.  */
       "  for (int i = 0; n; i++)\n" /* <- line 3.  */
       "    {\n" /* <--------------------- line 4.  */
       "    }\n" /* <--------------------- line 5.  */
       "}\n"); /* <----------------------- line 6.  */
  /* Adapted from infinite-loop-1.c where the condition should have been
     "i < n", rather than just "n".  */

  control_flow_test t (SELFTEST_LOCATION, case_, content);

  const location_t iter_test = t.get_line_and_column (3, 19);
  const location_t iter_next = t.get_line_and_columns (3, 22, 24);

  test_diagnostic_path path (event_pp);
  path.add_event (iter_test, NULL_TREE, 0, "infinite loop here");

  path.add_event (iter_test, NULL_TREE, 0, "looping from here...");
  path.connect_to_next_event ();

  path.add_event (iter_next, NULL_TREE, 0, "...to here");

  path.add_event (iter_next, NULL_TREE, 0, "looping back...");
  path.connect_to_next_event ();

  path.add_event (iter_test, NULL_TREE, 0, "...to here");

  if (!path_events_have_column_data_p (path))
    return;

  path_summary summary (path, true);

  {
    test_diagnostic_context dc;
    dc.set_text_art_charset (DIAGNOSTICS_TEXT_ART_CHARSET_ASCII);
    dc.m_source_printing.show_event_links_p = true;
    dc.m_source_printing.show_line_numbers_p = true;
    print_path_summary_as_text (&summary, &dc, false);
    ASSERT_STREQ
      ("  events 1-2\n"
       "FILENAME:3:19:\n"
       "    3 |   for (int i = 0; n; i++)\n"
       "      |                   ^\n"
       "      |                   |\n"
       "      |                   (1) infinite loop here\n"
       "      |                   (2) looping from here... ->-+\n"
       "      |                                               |\n"
       "  events 3-4\n"
       "      |                                               |\n"
       "      |+----------------------------------------------+\n"
       "    3 ||  for (int i = 0; n; i++)\n"
       "      ||                     ^~~\n"
       "      ||                     |\n"
       "      |+-------------------->(3) ...to here\n"
       "      |                      (4) looping back... ->-+\n"
       "      |                                             |\n"
       /* We need to start an new event_range here as event (4) with an
	  out-edge is on the same line as its destination event (5), but
	  to the right, which we can't handle as a single event_range. */
       "  event 5\n"
       "      |                                             |\n"
       "      |+--------------------------------------------+\n"
       "    3 ||  for (int i = 0; n; i++)\n"
       "      ||                  ^\n"
       "      ||                  |\n"
       "      |+----------------->(5) ...to here\n",
       pp_formatted_text (dc.printer));
  }
}

/* Implementation of ASSERT_CFG_EDGE_PATH_STREQ.  */

static void
assert_cfg_edge_path_streq (const location &loc,
			    pretty_printer *event_pp,
			    const location_t src_loc,
			    const location_t dst_loc,
			    const char *expected_str)
{
  test_diagnostic_path path (event_pp);
  path.add_event (src_loc, NULL_TREE, 0, "from here...");
  path.connect_to_next_event ();

  path.add_event (dst_loc, NULL_TREE, 0, "...to here");

  if (!path_events_have_column_data_p (path))
    return;

  path_summary summary (path, true);

  test_diagnostic_context dc;
  dc.set_text_art_charset (DIAGNOSTICS_TEXT_ART_CHARSET_ASCII);
  dc.m_source_printing.show_event_links_p = true;
  dc.m_source_printing.show_line_numbers_p = true;
  print_path_summary_as_text (&summary, &dc, false);
  ASSERT_STREQ_AT (loc, expected_str,
		   pp_formatted_text (dc.printer));
}

/* Assert that if we make a path with an event with "from here..." at SRC_LOC
   leading to an event "...to here" at DST_LOC that we print the path
   as EXPECTED_STR.  */

#define ASSERT_CFG_EDGE_PATH_STREQ(SRC_LOC, DST_LOC, EXPECTED_STR) \
  assert_cfg_edge_path_streq ((SELFTEST_LOCATION), (event_pp),	   \
			      (SRC_LOC), (DST_LOC), (EXPECTED_STR))

/* Various examples of edge, trying to cover all combinations of:
   - relative x positive of src label and dst label
   - relative y position of labels:
     - on same line
     - on next line
     - on line after next
     - big gap, where src is before dst
     - big gap, where src is after dst
   and other awkward cases.  */

static void
test_control_flow_4 (const line_table_case &case_,
		     pretty_printer *event_pp)
{
  std::string many_lines;
  for (int i = 1; i <= 100; i++)
    /* ............000000000111
       ............123456789012.  */
    many_lines += "LHS      RHS\n";
  control_flow_test t (SELFTEST_LOCATION, case_, many_lines.c_str ());

  /* Same line.  */
  {
    /* LHS -> RHS.  */
    ASSERT_CFG_EDGE_PATH_STREQ
      (t.get_line_and_columns (3, 1, 3),
       t.get_line_and_columns (3, 10, 12),
       ("  event 1\n"
	"FILENAME:3:1:\n"
	"    3 | LHS      RHS\n"
	"      | ^~~\n"
	"      | |\n"
	"      | (1) from here... ->-+\n"
	"      |                     |\n"
	"  event 2\n"
	"      |                     |\n"
	"      |+--------------------+\n"
	"    3 ||LHS      RHS\n"
	"      ||         ^~~\n"
	"      ||         |\n"
	"      |+-------->(2) ...to here\n"));

    /*  RHS -> LHS.  */
    ASSERT_CFG_EDGE_PATH_STREQ
      (t.get_line_and_columns (3, 10, 12),
       t.get_line_and_columns (3, 1, 3),
       ("  event 1\n"
	"FILENAME:3:10:\n"
	"    3 | LHS      RHS\n"
	"      |          ^~~\n"
	"      |          |\n"
	"      |          (1) from here... ->-+\n"
	"      |                              |\n"
	"  event 2\n"
	"      |                              |\n"
	"      |+-----------------------------+\n"
	"    3 ||LHS      RHS\n"
	"      ||^~~\n"
	"      |||\n"
	"      |+(2) ...to here\n"));
  }

  /* Next line.  */
  {
    /* LHS -> RHS.  */
    ASSERT_CFG_EDGE_PATH_STREQ
      (t.get_line_and_columns (3, 1, 3),
       t.get_line_and_columns (4, 5, 7),
       ("  events 1-2\n"
	"FILENAME:3:1:\n"
	"    3 | LHS      RHS\n"
	"      | ^~~\n"
	"      | |\n"
	"      | (1) from here... ->-+\n"
	"      |                     |\n"
	"      |                     |\n"
	"      |+--------------------+\n"
	"    4 ||LHS      RHS\n"
	"      ||    ~~~\n"
	"      ||    |\n"
	"      |+--->(2) ...to here\n"));

    /*  RHS -> LHS.  */
    ASSERT_CFG_EDGE_PATH_STREQ
      (t.get_line_and_columns (3, 10, 12),
       t.get_line_and_columns (4, 1, 3),
       ("  events 1-2\n"
	"FILENAME:3:10:\n"
	"    3 | LHS      RHS\n"
	"      |          ^~~\n"
	"      |          |\n"
	"      |          (1) from here... ->-+\n"
	"      |                              |\n"
	"      |                              |\n"
	"      |+-----------------------------+\n"
	"    4 ||LHS      RHS\n"
	"      ||~~~       \n"
	"      |||\n"
	"      |+(2) ...to here\n"));
  }

  /* Line after next.  */
  {
    /* LHS -> RHS.  */
    ASSERT_CFG_EDGE_PATH_STREQ
      (t.get_line_and_columns (3, 1, 3),
       t.get_line_and_columns (5, 10, 12),
       ("  events 1-2\n"
	"FILENAME:3:1:\n"
	"    3 | LHS      RHS\n"
	"      | ^~~\n"
	"      | |\n"
	"      | (1) from here... ->-+\n"
	"      |                     |\n"
	"      |                     |\n"
	"      |+--------------------+\n"
	"    4 ||LHS      RHS\n"
	"    5 ||LHS      RHS\n"
	"      ||         ~~~\n"
	"      ||         |\n"
	"      |+-------->(2) ...to here\n"));

    /*  RHS -> LHS.  */
    ASSERT_CFG_EDGE_PATH_STREQ
      (t.get_line_and_columns (3, 10, 12),
       t.get_line_and_columns (5, 1, 3),
       ("  events 1-2\n"
	"FILENAME:3:10:\n"
	"    3 | LHS      RHS\n"
	"      |          ^~~\n"
	"      |          |\n"
	"      |          (1) from here... ->-+\n"
	"      |                              |\n"
	"      |                              |\n"
	"      |+-----------------------------+\n"
	"    4 ||LHS      RHS\n"
	"    5 ||LHS      RHS\n"
	"      ||~~~       \n"
	"      |||\n"
	"      |+(2) ...to here\n"));
  }

  /* Big gap, increasing line number.  */
  {
    /* LHS -> RHS.  */
    ASSERT_CFG_EDGE_PATH_STREQ
      (t.get_line_and_columns (3, 1, 3),
       t.get_line_and_columns (97, 10, 12),
       ("  events 1-2\n"
	"FILENAME:3:1:\n"
	"    3 | LHS      RHS\n"
	"      | ^~~\n"
	"      | |\n"
	"      | (1) from here... ->-+\n"
	"      |                     |\n"
	"......\n"
	"      |                     |\n"
	"      |+--------------------+\n"
	"   97 ||LHS      RHS\n"
	"      ||         ~~~\n"
	"      ||         |\n"
	"      |+-------->(2) ...to here\n"));

    /*  RHS -> LHS.  */
    ASSERT_CFG_EDGE_PATH_STREQ
      (t.get_line_and_columns (3, 10, 12),
       t.get_line_and_columns (97, 1, 3),
       ("  events 1-2\n"
	"FILENAME:3:10:\n"
	"    3 | LHS      RHS\n"
	"      |          ^~~\n"
	"      |          |\n"
	"      |          (1) from here... ->-+\n"
	"      |                              |\n"
	"......\n"
	"      |                              |\n"
	"      |+-----------------------------+\n"
	"   97 ||LHS      RHS\n"
	"      ||~~~       \n"
	"      |||\n"
	"      |+(2) ...to here\n"));
  }

  /* Big gap, decreasing line number.  */
  {
    /* LHS -> RHS.  */
    ASSERT_CFG_EDGE_PATH_STREQ
      (t.get_line_and_columns (97, 1, 3),
       t.get_line_and_columns (3, 10, 12),
       ("  event 1\n"
	"FILENAME:97:1:\n"
	"   97 | LHS      RHS\n"
	"      | ^~~\n"
	"      | |\n"
	"      | (1) from here... ->-+\n"
	"      |                     |\n"
	"  event 2\n"
	"      |                     |\n"
	"      |+--------------------+\n"
	"    3 ||LHS      RHS\n"
	"      ||         ^~~\n"
	"      ||         |\n"
	"      |+-------->(2) ...to here\n"));

    /*  RHS -> LHS.  */
    ASSERT_CFG_EDGE_PATH_STREQ
      (t.get_line_and_columns (97, 10, 12),
       t.get_line_and_columns (3, 1, 3),
       ("  event 1\n"
	"FILENAME:97:10:\n"
	"   97 | LHS      RHS\n"
	"      |          ^~~\n"
	"      |          |\n"
	"      |          (1) from here... ->-+\n"
	"      |                              |\n"
	"  event 2\n"
	"      |                              |\n"
	"      |+-----------------------------+\n"
	"    3 ||LHS      RHS\n"
	"      ||^~~\n"
	"      |||\n"
	"      |+(2) ...to here\n"));
  }

  /* Unknown src.  */
  {
    ASSERT_CFG_EDGE_PATH_STREQ
      (UNKNOWN_LOCATION,
       t.get_line_and_columns (3, 10, 12),
       ("  event 1\n"
	" (1): from here...\n"
	"  event 2\n"
	"FILENAME:3:10:\n"
	"    3 | LHS      RHS\n"
	"      |          ^~~\n"
	"      |          |\n"
	"      |+-------->(2) ...to here\n"));
  }

  /* Unknown dst.  */
  {
    ASSERT_CFG_EDGE_PATH_STREQ
      (t.get_line_and_columns (3, 1, 3),
       UNKNOWN_LOCATION,
       ("  event 1\n"
	"FILENAME:3:1:\n"
	"    3 | LHS      RHS\n"
	"      | ^~~\n"
	"      | |\n"
	"      | (1) from here... ->-+\n"
	"      |                     |\n"
	"  event 2\n"
	"FILENAME:\n"
	" (2): ...to here\n"));
  }
}

/* Another complex example, adapted from data-model-20.c.  */

static void
test_control_flow_5 (const line_table_case &case_,
		     pretty_printer *event_pp)
{
  /* Create a tempfile and write some text to it.
     ...000000000111111111122222222223333333333444444444455555555556666666666.
     ...123456789012345678901234567890123456789012345678901234567890123456789.  */
  const char *content
    = ("  if ((arr = (struct foo **)malloc(n * sizeof(struct foo *))) == NULL)\n"
       "    return NULL;\n" /* <------------------------- line 2.  */
       "\n" /* <----------------------------------------- line 3.  */
       "  for (i = 0; i < n; i++) {\n" /* <-------------- line 4.  */
       "    if ((arr[i] = (struct foo *)malloc(sizeof(struct foo))) == NULL) {\n");

  control_flow_test t (SELFTEST_LOCATION, case_, content);

  test_diagnostic_path path (event_pp);
  /* (1) */
  path.add_event (t.get_line_and_column (1, 6), NULL_TREE, 0,
		  "following %qs branch (when %qs is non-NULL)...",
		  "false", "arr");
  path.connect_to_next_event ();

  /* (2) */
  path.add_event (t.get_line_and_columns (4, 8, 10, 12), NULL_TREE, 0,
		  "...to here");

  /* (3) */
  path.add_event (t.get_line_and_columns (4, 15, 17, 19), NULL_TREE, 0,
		  "following %qs branch (when %qs)...",
		  "true", "i < n");
  path.connect_to_next_event ();

  /* (4) */
  path.add_event (t.get_line_and_column (5, 13), NULL_TREE, 0,
		  "...to here");

  /* (5) */
  path.add_event (t.get_line_and_columns (5, 33, 58), NULL_TREE, 0,
		  "allocated here");

  if (!path_events_have_column_data_p (path))
    return;

  path_summary summary (path, true);

  {
    test_diagnostic_context dc;
    dc.set_text_art_charset (DIAGNOSTICS_TEXT_ART_CHARSET_ASCII);
    dc.m_source_printing.show_event_links_p = true;
    dc.m_source_printing.show_line_numbers_p = true;
    print_path_summary_as_text (&summary, &dc, false);
    ASSERT_STREQ
      ("  events 1-5\n"
       "FILENAME:1:6:\n"
       "    1 |   if ((arr = (struct foo **)malloc(n * sizeof(struct foo *))) == NULL)\n"
       "      |      ^\n"
       "      |      |\n"
       "      |      (1) following `false' branch (when `arr' is non-NULL)... ->-+\n"
       "      |                                                                  |\n"
       "......\n"
       "      |                                                                  |\n"
       "      |+-----------------------------------------------------------------+\n"
       "    4 ||  for (i = 0; i < n; i++) {\n"
       "      ||       ~~~~~  ~~~~~\n"
       "      ||         |      |\n"
       "      ||         |      (3) following `true' branch (when `i < n')... ->-+\n"
       "      |+-------->(2) ...to here                                          |\n"
       "      |                                                                  |\n"
       "      |                                                                  |\n"
       "      |+-----------------------------------------------------------------+\n"
       "    5 ||    if ((arr[i] = (struct foo *)malloc(sizeof(struct foo))) == NULL) {\n"
       "      ||            ~                   ~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
       "      ||            |                   |\n"
       "      |+----------->(4) ...to here      (5) allocated here\n",
       pp_formatted_text (dc.printer));
  }
}

/* Another complex example, adapted from loop-3.c.  */

static void
test_control_flow_6 (const line_table_case &case_,
		     pretty_printer *event_pp)
{
  /* Create a tempfile and write some text to it.
     ...000000000111111111122222222223333333.
     ...123456789012345678901234567890123456.  */
  const char *content
    = ("#include <stdlib.h>\n" /* <------------------ line 1.  */
       "\n" /* <------------------------------------- line 2.  */
       "void test(int c)\n" /* <--------------------- line 3.  */
       "{\n" /* <------------------------------------ line 4.  */
       "  int i;\n" /* <----------------------------- line 5.  */
       "  char *buffer = (char*)malloc(256);\n" /* <- line 6.  */
       "\n" /* <------------------------------------- line 7.  */
       "  for (i=0; i<255; i++) {\n" /* <------------ line 8.  */
       "    buffer[i] = c;\n" /* <------------------- line 9.  */
       "\n" /* <------------------------------------- line 10.  */
       "    free(buffer);\n" /* <-------------------- line 11.  */
       "  }\n"); /* <-------------------------------- line 12.  */

  control_flow_test t (SELFTEST_LOCATION, case_, content);

  test_diagnostic_path path (event_pp);
  /* (1) */
  path.add_event (t.get_line_and_columns (6, 25, 35), NULL_TREE, 0,
		  "allocated here");

  /* (2) */
  path.add_event (t.get_line_and_columns (8, 13, 14, 17), NULL_TREE, 0,
		  "following %qs branch (when %qs)...",
		  "true", "i <= 254");
  path.connect_to_next_event ();

  /* (3) */
  path.add_event (t.get_line_and_columns (9, 5, 15, 17), NULL_TREE, 0,
		  "...to here");

  /* (4) */
  path.add_event (t.get_line_and_columns (8, 13, 14, 17), NULL_TREE, 0,
		  "following %qs branch (when %qs)...",
		  "true", "i <= 254");
  path.connect_to_next_event ();

  /* (5) */
  path.add_event (t.get_line_and_columns (9, 5, 15, 17), NULL_TREE, 0,
		  "...to here");

  if (!path_events_have_column_data_p (path))
    return;

  path_summary summary (path, true);

  {
    test_diagnostic_context dc;
    dc.set_text_art_charset (DIAGNOSTICS_TEXT_ART_CHARSET_ASCII);
    dc.m_source_printing.show_event_links_p = true;
    dc.m_source_printing.show_line_numbers_p = true;
    print_path_summary_as_text (&summary, &dc, false);
    ASSERT_STREQ
      ("  events 1-3\n"
       "FILENAME:6:25:\n"
       "    6 |   char *buffer = (char*)malloc(256);\n"
       "      |                         ^~~~~~~~~~~\n"
       "      |                         |\n"
       "      |                         (1) allocated here\n"
       "    7 | \n"
       "    8 |   for (i=0; i<255; i++) {\n"
       "      |             ~~~~~        \n"
       "      |              |\n"
       "      |              (2) following `true' branch (when `i <= 254')... ->-+\n"
       "      |                                                                  |\n"
       "      |                                                                  |\n"
       "      |+-----------------------------------------------------------------+\n"
       "    9 ||    buffer[i] = c;\n"
       "      ||    ~~~~~~~~~~~~~        \n"
       "      ||              |\n"
       "      |+------------->(3) ...to here\n"
       "  events 4-5\n"
       "    8 |   for (i=0; i<255; i++) {\n"
       "      |             ~^~~~\n"
       "      |              |\n"
       "      |              (4) following `true' branch (when `i <= 254')... ->-+\n"
       "      |                                                                  |\n"
       "      |                                                                  |\n"
       "      |+-----------------------------------------------------------------+\n"
       "    9 ||    buffer[i] = c;\n"
       "      ||    ~~~~~~~~~~~~~\n"
       "      ||              |\n"
       "      |+------------->(5) ...to here\n",
       pp_formatted_text (dc.printer));
  }
}

static void
control_flow_tests (const line_table_case &case_)
{
  std::unique_ptr<pretty_printer> event_pp
    = std::unique_ptr<pretty_printer> (global_dc->printer->clone ());
  pp_show_color (event_pp) = 0;

  test_control_flow_1 (case_, event_pp.get ());
  test_control_flow_2 (case_, event_pp.get ());
  test_control_flow_3 (case_, event_pp.get ());
  test_control_flow_4 (case_, event_pp.get ());
  test_control_flow_5 (case_, event_pp.get ());
  test_control_flow_6 (case_, event_pp.get ());
}

/* Run all of the selftests within this file.  */

void
tree_diagnostic_path_cc_tests ()
{
  /* In a few places we use the global dc's printer to determine
     colorization so ensure this off during the tests.  */
  bool saved_show_color = pp_show_color (global_dc->printer);
  pp_show_color (global_dc->printer) = false;

  auto_fix_quotes fix_quotes;
  std::unique_ptr<pretty_printer> event_pp
    = std::unique_ptr<pretty_printer> (global_dc->printer->clone ());
  test_empty_path (event_pp.get ());
  test_intraprocedural_path (event_pp.get ());
  test_interprocedural_path_1 (event_pp.get ());
  test_interprocedural_path_2 (event_pp.get ());
  test_recursion (event_pp.get ());
  for_each_line_table_case (control_flow_tests);

  pp_show_color (global_dc->printer) = saved_show_color;
}

} // namespace selftest

#if __GNUC__ >= 10
#  pragma GCC diagnostic pop
#endif

#endif /* #if CHECKING_P */
