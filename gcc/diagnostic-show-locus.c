/* Diagnostic subroutines for printing source-code
   Copyright (C) 1999-2016 Free Software Foundation, Inc.
   Contributed by Gabriel Dos Reis <gdr@codesourcery.com>

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
#include "version.h"
#include "demangle.h"
#include "intl.h"
#include "backtrace.h"
#include "diagnostic.h"
#include "diagnostic-color.h"
#include "selftest.h"

#ifdef HAVE_TERMIOS_H
# include <termios.h>
#endif

#ifdef GWINSZ_IN_SYS_IOCTL
# include <sys/ioctl.h>
#endif

/* Classes for rendering source code and diagnostics, within an
   anonymous namespace.
   The work is done by "class layout", which embeds and uses
   "class colorizer" and "class layout_range" to get things done.  */

namespace {

/* The state at a given point of the source code, assuming that we're
   in a range: which range are we in, and whether we should draw a caret at
   this point.  */

struct point_state
{
  int range_idx;
  bool draw_caret_p;
};

/* A class to inject colorization codes when printing the diagnostic locus.

   It has one kind of colorization for each of:
     - normal text
     - range 0 (the "primary location")
     - range 1
     - range 2

   The class caches the lookup of the color codes for the above.

   The class also has responsibility for tracking which of the above is
   active, filtering out unnecessary changes.  This allows
   layout::print_source_line and layout::print_annotation_line
   to simply request a colorization code for *every* character they print,
   via this class, and have the filtering be done for them here.  */

class colorizer
{
 public:
  colorizer (diagnostic_context *context,
	     diagnostic_t diagnostic_kind);
  ~colorizer ();

  void set_range (int range_idx) { set_state (range_idx); }
  void set_normal_text () { set_state (STATE_NORMAL_TEXT); }
  void set_fixit_insert () { set_state (STATE_FIXIT_INSERT); }
  void set_fixit_delete () { set_state (STATE_FIXIT_DELETE); }

 private:
  void set_state (int state);
  void begin_state (int state);
  void finish_state (int state);
  const char *get_color_by_name (const char *);

 private:
  static const int STATE_NORMAL_TEXT = -1;
  static const int STATE_FIXIT_INSERT  = -2;
  static const int STATE_FIXIT_DELETE  = -3;

  diagnostic_context *m_context;
  diagnostic_t m_diagnostic_kind;
  int m_current_state;
  const char *m_caret;
  const char *m_range1;
  const char *m_range2;
  const char *m_fixit_insert;
  const char *m_fixit_delete;
  const char *m_stop_color;
};

/* A point within a layout_range; similar to an expanded_location,
   but after filtering on file.  */

class layout_point
{
 public:
  layout_point (const expanded_location &exploc)
  : m_line (exploc.line),
    m_column (exploc.column) {}

  int m_line;
  int m_column;
};

/* A class for use by "class layout" below: a filtered location_range.  */

class layout_range
{
 public:
  layout_range (const expanded_location *start_exploc,
		const expanded_location *finish_exploc,
		bool show_caret_p,
		const expanded_location *caret_exploc);

  bool contains_point (int row, int column) const;

  layout_point m_start;
  layout_point m_finish;
  bool m_show_caret_p;
  layout_point m_caret;
};

/* A struct for use by layout::print_source_line for telling
   layout::print_annotation_line the extents of the source line that
   it printed, so that underlines can be clipped appropriately.  */

struct line_bounds
{
  int m_first_non_ws;
  int m_last_non_ws;
};

/* A range of contiguous source lines within a layout (e.g. "lines 5-10"
   or "line 23").  During the layout ctor, layout::calculate_line_spans
   splits the pertinent source lines into a list of disjoint line_span
   instances (e.g. lines 5-10, lines 15-20, line 23).  */

struct line_span
{
  line_span (linenum_type first_line, linenum_type last_line)
    : m_first_line (first_line), m_last_line (last_line)
  {
    gcc_assert (first_line <= last_line);
  }
  linenum_type get_first_line () const { return m_first_line; }
  linenum_type get_last_line () const { return m_last_line; }

  bool contains_line_p (linenum_type line) const
  {
    return line >= m_first_line && line <= m_last_line;
  }

  static int comparator (const void *p1, const void *p2)
  {
    const line_span *ls1 = (const line_span *)p1;
    const line_span *ls2 = (const line_span *)p2;
    int first_line_diff = (int)ls1->m_first_line - (int)ls2->m_first_line;
    if (first_line_diff)
      return first_line_diff;
    return (int)ls1->m_last_line - (int)ls2->m_last_line;
  }

  linenum_type m_first_line;
  linenum_type m_last_line;
};

/* A class to control the overall layout when printing a diagnostic.

   The layout is determined within the constructor.
   It is then printed by repeatedly calling the "print_source_line",
   "print_annotation_line" and "print_any_fixits" methods.

   We assume we have disjoint ranges.  */

class layout
{
 public:
  layout (diagnostic_context *context,
	  rich_location *richloc,
	  diagnostic_t diagnostic_kind);

  int get_num_line_spans () const { return m_line_spans.length (); }
  const line_span *get_line_span (int idx) const { return &m_line_spans[idx]; }

  bool print_heading_for_line_span_index_p (int line_span_idx) const;

  expanded_location get_expanded_location (const line_span *) const;

  bool print_source_line (int row, line_bounds *lbounds_out);
  void print_annotation_line (int row, const line_bounds lbounds);
  bool annotation_line_showed_range_p (int line, int start_column,
				       int finish_column) const;
  void print_any_fixits (int row, const rich_location *richloc);

  void show_ruler (int max_column) const;

 private:
  void calculate_line_spans ();

  void print_newline ();

  bool
  get_state_at_point (/* Inputs.  */
		      int row, int column,
		      int first_non_ws, int last_non_ws,
		      /* Outputs.  */
		      point_state *out_state);

  int
  get_x_bound_for_row (int row, int caret_column,
		       int last_non_ws);

  void
  move_to_column (int *column, int dest_column);

 private:
  diagnostic_context *m_context;
  pretty_printer *m_pp;
  diagnostic_t m_diagnostic_kind;
  expanded_location m_exploc;
  colorizer m_colorizer;
  bool m_colorize_source_p;
  auto_vec <layout_range> m_layout_ranges;
  auto_vec <line_span> m_line_spans;
  int m_x_offset;
};

/* Implementation of "class colorizer".  */

/* The constructor for "colorizer".  Lookup and store color codes for the
   different kinds of things we might need to print.  */

colorizer::colorizer (diagnostic_context *context,
		      diagnostic_t diagnostic_kind) :
  m_context (context),
  m_diagnostic_kind (diagnostic_kind),
  m_current_state (STATE_NORMAL_TEXT)
{
  m_range1 = get_color_by_name ("range1");
  m_range2 = get_color_by_name ("range2");
  m_fixit_insert = get_color_by_name ("fixit-insert");
  m_fixit_delete = get_color_by_name ("fixit-delete");
  m_stop_color = colorize_stop (pp_show_color (context->printer));
}

/* The destructor for "colorize".  If colorization is on, print a code to
   turn it off.  */

colorizer::~colorizer ()
{
  finish_state (m_current_state);
}

/* Update state, printing color codes if necessary if there's a state
   change.  */

void
colorizer::set_state (int new_state)
{
  if (m_current_state != new_state)
    {
      finish_state (m_current_state);
      m_current_state = new_state;
      begin_state (new_state);
    }
}

/* Turn on any colorization for STATE.  */

void
colorizer::begin_state (int state)
{
  switch (state)
    {
    case STATE_NORMAL_TEXT:
      break;

    case STATE_FIXIT_INSERT:
      pp_string (m_context->printer, m_fixit_insert);
      break;

    case STATE_FIXIT_DELETE:
      pp_string (m_context->printer, m_fixit_delete);
      break;

    case 0:
      /* Make range 0 be the same color as the "kind" text
	 (error vs warning vs note).  */
      pp_string
	(m_context->printer,
	 colorize_start (pp_show_color (m_context->printer),
			 diagnostic_get_color_for_kind (m_diagnostic_kind)));
      break;

    case 1:
      pp_string (m_context->printer, m_range1);
      break;

    case 2:
      pp_string (m_context->printer, m_range2);
      break;

    default:
      /* We don't expect more than 3 ranges per diagnostic.  */
      gcc_unreachable ();
      break;
    }
}

/* Turn off any colorization for STATE.  */

void
colorizer::finish_state (int state)
{
  if (state != STATE_NORMAL_TEXT)
    pp_string (m_context->printer, m_stop_color);
}

/* Get the color code for NAME (or the empty string if
   colorization is disabled).  */

const char *
colorizer::get_color_by_name (const char *name)
{
  return colorize_start (pp_show_color (m_context->printer), name);
}

/* Implementation of class layout_range.  */

/* The constructor for class layout_range.
   Initialize various layout_point fields from expanded_location
   equivalents; we've already filtered on file.  */

layout_range::layout_range (const expanded_location *start_exploc,
			    const expanded_location *finish_exploc,
			    bool show_caret_p,
			    const expanded_location *caret_exploc)
: m_start (*start_exploc),
  m_finish (*finish_exploc),
  m_show_caret_p (show_caret_p),
  m_caret (*caret_exploc)
{
}

/* Is (column, row) within the given range?
   We've already filtered on the file.

   Ranges are closed (both limits are within the range).

   Example A: a single-line range:
     start:  (col=22, line=2)
     finish: (col=38, line=2)

  |00000011111111112222222222333333333344444444444
  |34567890123456789012345678901234567890123456789
--+-----------------------------------------------
01|bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
02|bbbbbbbbbbbbbbbbbbbSwwwwwwwwwwwwwwwFaaaaaaaaaaa
03|aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa

   Example B: a multiline range with
     start:  (col=14, line=3)
     finish: (col=08, line=5)

  |00000011111111112222222222333333333344444444444
  |34567890123456789012345678901234567890123456789
--+-----------------------------------------------
01|bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
02|bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
03|bbbbbbbbbbbSwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
04|wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww
05|wwwwwFaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
06|aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
--+-----------------------------------------------

   Legend:
   - 'b' indicates a point *before* the range
   - 'S' indicates the start of the range
   - 'w' indicates a point within the range
   - 'F' indicates the finish of the range (which is
	 within it).
   - 'a' indicates a subsequent point *after* the range.  */

bool
layout_range::contains_point (int row, int column) const
{
  gcc_assert (m_start.m_line <= m_finish.m_line);
  /* ...but the equivalent isn't true for the columns;
     consider example B in the comment above.  */

  if (row < m_start.m_line)
    /* Points before the first line of the range are
       outside it (corresponding to line 01 in example A
       and lines 01 and 02 in example B above).  */
    return false;

  if (row == m_start.m_line)
    /* On same line as start of range (corresponding
       to line 02 in example A and line 03 in example B).  */
    {
      if (column < m_start.m_column)
	/* Points on the starting line of the range, but
	   before the column in which it begins.  */
	return false;

      if (row < m_finish.m_line)
	/* This is a multiline range; the point
	   is within it (corresponds to line 03 in example B
	   from column 14 onwards) */
	return true;
      else
	{
	  /* This is a single-line range.  */
	  gcc_assert (row == m_finish.m_line);
	  return column <= m_finish.m_column;
	}
    }

  /* The point is in a line beyond that containing the
     start of the range: lines 03 onwards in example A,
     and lines 04 onwards in example B.  */
  gcc_assert (row > m_start.m_line);

  if (row > m_finish.m_line)
    /* The point is beyond the final line of the range
       (lines 03 onwards in example A, and lines 06 onwards
       in example B).  */
    return false;

  if (row < m_finish.m_line)
    {
      /* The point is in a line that's fully within a multiline
	 range (e.g. line 04 in example B).  */
      gcc_assert (m_start.m_line < m_finish.m_line);
      return true;
    }

  gcc_assert (row ==  m_finish.m_line);

  return column <= m_finish.m_column;
}

#if CHECKING_P

/* A helper function for testing layout_range::contains_point.  */

static layout_range
make_range (int start_line, int start_col, int end_line, int end_col)
{
  const expanded_location start_exploc
    = {"test.c", start_line, start_col, NULL, false};
  const expanded_location finish_exploc
    = {"test.c", end_line, end_col, NULL, false};
  return layout_range (&start_exploc, &finish_exploc, false,
		       &start_exploc);
}

/* Selftests for layout_range::contains_point.  */

/* Selftest for layout_range::contains_point where the layout_range
   is a range with start==end i.e. a single point.  */

static void
test_range_contains_point_for_single_point ()
{
  layout_range point = make_range (7, 10, 7, 10);

  /* Before the line. */
  ASSERT_FALSE (point.contains_point (6, 1));

  /* On the line, but before start.  */
  ASSERT_FALSE (point.contains_point (7, 9));

  /* At the point.  */
  ASSERT_TRUE (point.contains_point (7, 10));

  /* On the line, after the point.  */
  ASSERT_FALSE (point.contains_point (7, 11));

  /* After the line.  */
  ASSERT_FALSE (point.contains_point (8, 1));
}

/* Selftest for layout_range::contains_point where the layout_range
   is the single-line range shown as "Example A" above.  */

static void
test_range_contains_point_for_single_line ()
{
  layout_range example_a = make_range (2, 22, 2, 38);

  /* Before the line. */
  ASSERT_FALSE (example_a.contains_point (1, 1));

  /* On the line, but before start.  */
  ASSERT_FALSE (example_a.contains_point (2, 21));

  /* On the line, at the start.  */
  ASSERT_TRUE (example_a.contains_point (2, 22));

  /* On the line, within the range.  */
  ASSERT_TRUE (example_a.contains_point (2, 23));

  /* On the line, at the end.  */
  ASSERT_TRUE (example_a.contains_point (2, 38));

  /* On the line, after the end.  */
  ASSERT_FALSE (example_a.contains_point (2, 39));

  /* After the line.  */
  ASSERT_FALSE (example_a.contains_point (2, 39));
}

/* Selftest for layout_range::contains_point where the layout_range
   is the multi-line range shown as "Example B" above.  */

static void
test_range_contains_point_for_multiple_lines ()
{
  layout_range example_b = make_range (3, 14, 5, 8);

  /* Before first line. */
  ASSERT_FALSE (example_b.contains_point (1, 1));

  /* On the first line, but before start.  */
  ASSERT_FALSE (example_b.contains_point (3, 13));

  /* At the start.  */
  ASSERT_TRUE (example_b.contains_point (3, 14));

  /* On the first line, within the range.  */
  ASSERT_TRUE (example_b.contains_point (3, 15));

  /* On an interior line.
     The column number should not matter; try various boundary
     values.  */
  ASSERT_TRUE (example_b.contains_point (4, 1));
  ASSERT_TRUE (example_b.contains_point (4, 7));
  ASSERT_TRUE (example_b.contains_point (4, 8));
  ASSERT_TRUE (example_b.contains_point (4, 9));
  ASSERT_TRUE (example_b.contains_point (4, 13));
  ASSERT_TRUE (example_b.contains_point (4, 14));
  ASSERT_TRUE (example_b.contains_point (4, 15));

  /* On the final line, before the end.  */
  ASSERT_TRUE (example_b.contains_point (5, 7));

  /* On the final line, at the end.  */
  ASSERT_TRUE (example_b.contains_point (5, 8));

  /* On the final line, after the end.  */
  ASSERT_FALSE (example_b.contains_point (5, 9));

  /* After the line.  */
  ASSERT_FALSE (example_b.contains_point (6, 1));
}

#endif /* #if CHECKING_P */

/* Given a source line LINE of length LINE_WIDTH, determine the width
   without any trailing whitespace.  */

static int
get_line_width_without_trailing_whitespace (const char *line, int line_width)
{
  int result = line_width;
  while (result > 0)
    {
      char ch = line[result - 1];
      if (ch == ' ' || ch == '\t')
	result--;
      else
	break;
    }
  gcc_assert (result >= 0);
  gcc_assert (result <= line_width);
  gcc_assert (result == 0 ||
	      (line[result - 1] != ' '
	       && line[result -1] != '\t'));
  return result;
}

#if CHECKING_P

/* A helper function for testing get_line_width_without_trailing_whitespace.  */

static void
assert_eq (const char *line, int expected_width)
{
  int actual_value
    = get_line_width_without_trailing_whitespace (line, strlen (line));
  ASSERT_EQ (actual_value, expected_width);
}

/* Verify that get_line_width_without_trailing_whitespace is sane for
   various inputs.  It is not required to handle newlines.  */

static void
test_get_line_width_without_trailing_whitespace ()
{
  assert_eq ("", 0);
  assert_eq (" ", 0);
  assert_eq ("\t", 0);
  assert_eq ("hello world", 11);
  assert_eq ("hello world     ", 11);
  assert_eq ("hello world     \t\t  ", 11);
}

#endif /* #if CHECKING_P */

/* Helper function for layout's ctor, for sanitizing locations relative
   to the primary location within a diagnostic.

   Compare LOC_A and LOC_B to see if it makes sense to print underlines
   connecting their expanded locations.  Doing so is only guaranteed to
   make sense if the locations share the same macro expansion "history"
   i.e. they can be traced through the same macro expansions, eventually
   reaching an ordinary map.

   This may be too strong a condition, but it effectively sanitizes
   PR c++/70105, which has an example of printing an expression where the
   final location of the expression is in a different macro, which
   erroneously was leading to hundreds of lines of irrelevant source
   being printed.  */

static bool
compatible_locations_p (location_t loc_a, location_t loc_b)
{
  if (IS_ADHOC_LOC (loc_a))
    loc_a = get_location_from_adhoc_loc (line_table, loc_a);
  if (IS_ADHOC_LOC (loc_b))
    loc_b = get_location_from_adhoc_loc (line_table, loc_b);

  /* If either location is one of the special locations outside of a
     linemap, they are only compatible if they are equal.  */
  if (loc_a < RESERVED_LOCATION_COUNT
      || loc_b < RESERVED_LOCATION_COUNT)
    return loc_a == loc_b;

  const line_map *map_a = linemap_lookup (line_table, loc_a);
  linemap_assert (map_a);

  const line_map *map_b = linemap_lookup (line_table, loc_b);
  linemap_assert (map_b);

  /* Are they within the same map?  */
  if (map_a == map_b)
    {
      /* Are both within the same macro expansion?  */
      if (linemap_macro_expansion_map_p (map_a))
	{
	  /* Expand each location towards the spelling location, and
	     recurse.  */
	  const line_map_macro *macro_map = linemap_check_macro (map_a);
	  source_location loc_a_toward_spelling
	    = linemap_macro_map_loc_unwind_toward_spelling (line_table,
							    macro_map,
							    loc_a);
	  source_location loc_b_toward_spelling
	    = linemap_macro_map_loc_unwind_toward_spelling (line_table,
							    macro_map,
							    loc_b);
	  return compatible_locations_p (loc_a_toward_spelling,
					 loc_b_toward_spelling);
	}

      /* Otherwise they are within the same ordinary map.  */
      return true;
    }
  else
    {
      /* Within different maps.  */

      /* If either is within a macro expansion, they are incompatible.  */
      if (linemap_macro_expansion_map_p (map_a)
	  || linemap_macro_expansion_map_p (map_b))
	return false;

      /* Within two different ordinary maps; they are compatible iff they
	 are in the same file.  */
      const line_map_ordinary *ord_map_a = linemap_check_ordinary (map_a);
      const line_map_ordinary *ord_map_b = linemap_check_ordinary (map_b);
      return ord_map_a->to_file == ord_map_b->to_file;
    }
}

/* Implementation of class layout.  */

/* Constructor for class layout.

   Filter the ranges from the rich_location to those that we can
   sanely print, populating m_layout_ranges.
   Determine the range of lines that we will print, splitting them
   up into an ordered list of disjoint spans of contiguous line numbers.
   Determine m_x_offset, to ensure that the primary caret
   will fit within the max_width provided by the diagnostic_context.  */

layout::layout (diagnostic_context * context,
		rich_location *richloc,
		diagnostic_t diagnostic_kind)
: m_context (context),
  m_pp (context->printer),
  m_diagnostic_kind (diagnostic_kind),
  m_exploc (richloc->get_expanded_location (0)),
  m_colorizer (context, diagnostic_kind),
  m_colorize_source_p (context->colorize_source_p),
  m_layout_ranges (rich_location::MAX_RANGES),
  m_line_spans (1 + rich_location::MAX_RANGES),
  m_x_offset (0)
{
  source_location primary_loc = richloc->get_range (0)->m_loc;

  for (unsigned int idx = 0; idx < richloc->get_num_locations (); idx++)
    {
      /* This diagnostic printer can only cope with "sufficiently sane" ranges.
	 Ignore any ranges that are awkward to handle.  */
      const location_range *loc_range = richloc->get_range (idx);

      /* Split the "range" into caret and range information.  */
      source_range src_range = get_range_from_loc (line_table, loc_range->m_loc);

      /* Expand the various locations.  */
      expanded_location start
	= linemap_client_expand_location_to_spelling_point (src_range.m_start);
      expanded_location finish
	= linemap_client_expand_location_to_spelling_point (src_range.m_finish);
      expanded_location caret
	= linemap_client_expand_location_to_spelling_point (loc_range->m_loc);

      /* If any part of the range isn't in the same file as the primary
	 location of this diagnostic, ignore the range.  */
      if (start.file != m_exploc.file)
	continue;
      if (finish.file != m_exploc.file)
	continue;
      if (loc_range->m_show_caret_p)
	if (caret.file != m_exploc.file)
	  continue;

      /* Sanitize the caret location for non-primary ranges.  */
      if (m_layout_ranges.length () > 0)
	if (loc_range->m_show_caret_p)
	  if (!compatible_locations_p (loc_range->m_loc, primary_loc))
	    /* Discard any non-primary ranges that can't be printed
	       sanely relative to the primary location.  */
	    continue;

      /* Everything is now known to be in the correct source file,
	 but it may require further sanitization.  */
      layout_range ri (&start, &finish, loc_range->m_show_caret_p, &caret);

      /* If we have a range that finishes before it starts (perhaps
	 from something built via macro expansion), printing the
	 range is likely to be nonsensical.  Also, attempting to do so
	 breaks assumptions within the printing code  (PR c/68473).
	 Similarly, don't attempt to print ranges if one or both ends
	 of the range aren't sane to print relative to the
	 primary location (PR c++/70105).  */
      if (start.line > finish.line
	  || !compatible_locations_p (src_range.m_start, primary_loc)
	  || !compatible_locations_p (src_range.m_finish, primary_loc))
	{
	  /* Is this the primary location?  */
	  if (m_layout_ranges.length () == 0)
	    {
	      /* We want to print the caret for the primary location, but
		 we must sanitize away m_start and m_finish.  */
	      ri.m_start = ri.m_caret;
	      ri.m_finish = ri.m_caret;
	    }
	  else
	    /* This is a non-primary range; ignore it.  */
	    continue;
	}

      /* Passed all the tests; add the range to m_layout_ranges so that
	 it will be printed.  */
      m_layout_ranges.safe_push (ri);
    }

  /* Populate m_line_spans.  */
  calculate_line_spans ();

  /* Adjust m_x_offset.
     Center the primary caret to fit in max_width; all columns
     will be adjusted accordingly.  */
  int max_width = m_context->caret_max_width;
  int line_width;
  const char *line = location_get_source_line (m_exploc.file, m_exploc.line,
					       &line_width);
  if (line && m_exploc.column <= line_width)
    {
      int right_margin = CARET_LINE_MARGIN;
      int column = m_exploc.column;
      right_margin = MIN (line_width - column, right_margin);
      right_margin = max_width - right_margin;
      if (line_width >= max_width && column > right_margin)
	m_x_offset = column - right_margin;
      gcc_assert (m_x_offset >= 0);
    }

  if (context->show_ruler_p)
    show_ruler (m_x_offset + max_width);
}

/* Return true iff we should print a heading when starting the
   line span with the given index.  */

bool
layout::print_heading_for_line_span_index_p (int line_span_idx) const
{
  /* We print a heading for every change of line span, hence for every
     line span after the initial one.  */
  if (line_span_idx > 0)
    return true;

  /* We also do it for the initial span if the primary location of the
     diagnostic is in a different span.  */
  if (m_exploc.line > (int)get_line_span (0)->m_last_line)
    return true;

  return false;
}

/* Get an expanded_location for the first location of interest within
   the given line_span.
   Used when printing a heading to indicate a new line span.  */

expanded_location
layout::get_expanded_location (const line_span *line_span) const
{
  /* Whenever possible, use the caret location.  */
  if (line_span->contains_line_p (m_exploc.line))
    return m_exploc;

  /* Otherwise, use the start of the first range that's present
     within the line_span.  */
  for (unsigned int i = 0; i < m_layout_ranges.length (); i++)
    {
      const layout_range *lr = &m_layout_ranges[i];
      if (line_span->contains_line_p (lr->m_start.m_line))
	{
	  expanded_location exploc = m_exploc;
	  exploc.line = lr->m_start.m_line;
	  exploc.column = lr->m_start.m_column;
	  return exploc;
	}
    }

  /* It should not be possible to have a line span that didn't
     contain any of the layout_range instances.  */
  gcc_unreachable ();
  return m_exploc;
}

/* We want to print the pertinent source code at a diagnostic.  The
   rich_location can contain multiple locations.  This will have been
   filtered into m_exploc (the caret for the primary location) and
   m_layout_ranges, for those ranges within the same source file.

   We will print a subset of the lines within the source file in question,
   as a collection of "spans" of lines.

   This function populates m_line_spans with an ordered, disjoint list of
   the line spans of interest.

   For example, if the primary caret location is on line 7, with ranges
   covering lines 5-6 and lines 9-12:

     004
     005                   |RANGE 0
     006                   |RANGE 0
     007  |PRIMARY CARET
     008
     009                                |RANGE 1
     010                                |RANGE 1
     011                                |RANGE 1
     012                                |RANGE 1
     013

   then we want two spans: lines 5-7 and lines 9-12.  */

void
layout::calculate_line_spans ()
{
  /* This should only be called once, by the ctor.  */
  gcc_assert (m_line_spans.length () == 0);

  /* Populate tmp_spans with individual spans, for each of
     m_exploc, and for m_layout_ranges.  */
  auto_vec<line_span> tmp_spans (1 + rich_location::MAX_RANGES);
  tmp_spans.safe_push (line_span (m_exploc.line, m_exploc.line));
  for (unsigned int i = 0; i < m_layout_ranges.length (); i++)
    {
      const layout_range *lr = &m_layout_ranges[i];
      gcc_assert (lr->m_start.m_line <= lr->m_finish.m_line);
      tmp_spans.safe_push (line_span (lr->m_start.m_line,
				      lr->m_finish.m_line));
    }

  /* Sort them.  */
  tmp_spans.qsort(line_span::comparator);

  /* Now iterate through tmp_spans, copying into m_line_spans, and
     combining where possible.  */
  gcc_assert (tmp_spans.length () > 0);
  m_line_spans.safe_push (tmp_spans[0]);
  for (unsigned int i = 1; i < tmp_spans.length (); i++)
    {
      line_span *current = &m_line_spans[m_line_spans.length () - 1];
      const line_span *next = &tmp_spans[i];
      gcc_assert (next->m_first_line >= current->m_first_line);
      if (next->m_first_line <= current->m_last_line + 1)
	{
	  /* We can merge them. */
	  if (next->m_last_line > current->m_last_line)
	    current->m_last_line = next->m_last_line;
	}
      else
	{
	  /* No merger possible.  */
	  m_line_spans.safe_push (*next);
	}
    }

  /* Verify the result, in m_line_spans.  */
  gcc_assert (m_line_spans.length () > 0);
  for (unsigned int i = 1; i < m_line_spans.length (); i++)
    {
      const line_span *prev = &m_line_spans[i - 1];
      const line_span *next = &m_line_spans[i];
      /* The individual spans must be sane.  */
      gcc_assert (prev->m_first_line <= prev->m_last_line);
      gcc_assert (next->m_first_line <= next->m_last_line);
      /* The spans must be ordered.  */
      gcc_assert (prev->m_first_line < next->m_first_line);
      /* There must be a gap of at least one line between separate spans.  */
      gcc_assert ((prev->m_last_line + 1) < next->m_first_line);
    }
}

/* Attempt to print line ROW of source code, potentially colorized at any
   ranges.
   Return true if the line was printed, populating *LBOUNDS_OUT.
   Return false if the source line could not be read, leaving *LBOUNDS_OUT
   untouched.  */

bool
layout::print_source_line (int row, line_bounds *lbounds_out)
{
  int line_width;
  const char *line = location_get_source_line (m_exploc.file, row,
					       &line_width);
  if (!line)
    return false;

  m_colorizer.set_normal_text ();

  /* We will stop printing the source line at any trailing
     whitespace.  */
  line_width = get_line_width_without_trailing_whitespace (line,
							   line_width);
  line += m_x_offset;

  pp_space (m_pp);
  int first_non_ws = INT_MAX;
  int last_non_ws = 0;
  int column;
  for (column = 1 + m_x_offset; column <= line_width; column++)
    {
      /* Assuming colorization is enabled for the caret and underline
	 characters, we may also colorize the associated characters
	 within the source line.

	 For frontends that generate range information, we color the
	 associated characters in the source line the same as the
	 carets and underlines in the annotation line, to make it easier
	 for the reader to see the pertinent code.

	 For frontends that only generate carets, we don't colorize the
	 characters above them, since this would look strange (e.g.
	 colorizing just the first character in a token).  */
      if (m_colorize_source_p)
	{
	  bool in_range_p;
	  point_state state;
	  in_range_p = get_state_at_point (row, column,
					   0, INT_MAX,
					   &state);
	  if (in_range_p)
	    m_colorizer.set_range (state.range_idx);
	  else
	    m_colorizer.set_normal_text ();
	}
      char c = *line == '\t' ? ' ' : *line;
      if (c == '\0')
	c = ' ';
      if (c != ' ')
	{
	  last_non_ws = column;
	  if (first_non_ws == INT_MAX)
	    first_non_ws = column;
	}
      pp_character (m_pp, c);
      line++;
    }
  print_newline ();

  lbounds_out->m_first_non_ws = first_non_ws;
  lbounds_out->m_last_non_ws = last_non_ws;
  return true;
}

/* Print a line consisting of the caret/underlines for the given
   source line.  */

void
layout::print_annotation_line (int row, const line_bounds lbounds)
{
  int x_bound = get_x_bound_for_row (row, m_exploc.column,
				     lbounds.m_last_non_ws);

  pp_space (m_pp);
  for (int column = 1 + m_x_offset; column < x_bound; column++)
    {
      bool in_range_p;
      point_state state;
      in_range_p = get_state_at_point (row, column,
				       lbounds.m_first_non_ws,
				       lbounds.m_last_non_ws,
				       &state);
      if (in_range_p)
	{
	  /* Within a range.  Draw either the caret or an underline.  */
	  m_colorizer.set_range (state.range_idx);
	  if (state.draw_caret_p)
	    /* Draw the caret.  */
	    pp_character (m_pp, m_context->caret_chars[state.range_idx]);
	  else
	    pp_character (m_pp, '~');
	}
      else
	{
	  /* Not in a range.  */
	  m_colorizer.set_normal_text ();
	  pp_character (m_pp, ' ');
	}
    }
  print_newline ();
}

/* Subroutine of layout::print_any_fixits.

   Determine if the annotation line printed for LINE contained
   the exact range from START_COLUMN to FINISH_COLUMN.  */

bool
layout::annotation_line_showed_range_p (int line, int start_column,
					int finish_column) const
{
  layout_range *range;
  int i;
  FOR_EACH_VEC_ELT (m_layout_ranges, i, range)
    if (range->m_start.m_line == line
	&& range->m_start.m_column == start_column
	&& range->m_finish.m_line == line
	&& range->m_finish.m_column == finish_column)
      return true;
  return false;
}

/* If there are any fixit hints on source line ROW within RICHLOC, print them.
   They are printed in order, attempting to combine them onto lines, but
   starting new lines if necessary.  */

void
layout::print_any_fixits (int row, const rich_location *richloc)
{
  int column = 0;
  for (unsigned int i = 0; i < richloc->get_num_fixit_hints (); i++)
    {
      fixit_hint *hint = richloc->get_fixit_hint (i);
      if (hint->affects_line_p (m_exploc.file, row))
	{
	  /* For now we assume each fixit hint can only touch one line.  */
	  switch (hint->get_kind ())
	    {
	    case fixit_hint::INSERT:
	      {
		fixit_insert *insert = static_cast <fixit_insert *> (hint);
		/* This assumes the insertion just affects one line.  */
		int start_column
		  = LOCATION_COLUMN (insert->get_location ());
		move_to_column (&column, start_column);
		m_colorizer.set_fixit_insert ();
		pp_string (m_pp, insert->get_string ());
		m_colorizer.set_normal_text ();
		column += insert->get_length ();
	      }
	      break;

	    case fixit_hint::REPLACE:
	      {
		fixit_replace *replace = static_cast <fixit_replace *> (hint);
		source_range src_range = replace->get_range ();
		int line = LOCATION_LINE (src_range.m_start);
		int start_column = LOCATION_COLUMN (src_range.m_start);
		int finish_column = LOCATION_COLUMN (src_range.m_finish);

		/* If the range of the replacement wasn't printed in the
		   annotation line, then print an extra underline to
		   indicate exactly what is being replaced.
		   Always show it for removals.  */
		if (!annotation_line_showed_range_p (line, start_column,
						     finish_column)
		    || replace->get_length () == 0)
		  {
		    move_to_column (&column, start_column);
		    m_colorizer.set_fixit_delete ();
		    for (; column <= finish_column; column++)
		      pp_character (m_pp, '-');
		    m_colorizer.set_normal_text ();
		  }
		/* Print the replacement text.  REPLACE also covers
		   removals, so only do this extra work (potentially starting
		   a new line) if we have actual replacement text.  */
		if (replace->get_length () > 0)
		  {
		    move_to_column (&column, start_column);
		    m_colorizer.set_fixit_insert ();
		    pp_string (m_pp, replace->get_string ());
		    m_colorizer.set_normal_text ();
		    column += replace->get_length ();
		  }
	      }
	      break;

	    default:
	      gcc_unreachable ();
	    }
	}
    }

  /* Add a trailing newline, if necessary.  */
  move_to_column (&column, 0);
}

/* Disable any colorization and emit a newline.  */

void
layout::print_newline ()
{
  m_colorizer.set_normal_text ();
  pp_newline (m_pp);
}

/* Return true if (ROW/COLUMN) is within a range of the layout.
   If it returns true, OUT_STATE is written to, with the
   range index, and whether we should draw the caret at
   (ROW/COLUMN) (as opposed to an underline).  */

bool
layout::get_state_at_point (/* Inputs.  */
			    int row, int column,
			    int first_non_ws, int last_non_ws,
			    /* Outputs.  */
			    point_state *out_state)
{
  layout_range *range;
  int i;
  FOR_EACH_VEC_ELT (m_layout_ranges, i, range)
    {
      if (range->contains_point (row, column))
	{
	  out_state->range_idx = i;

	  /* Are we at the range's caret?  is it visible? */
	  out_state->draw_caret_p = false;
	  if (range->m_show_caret_p
	      && row == range->m_caret.m_line
	      && column == range->m_caret.m_column)
	    out_state->draw_caret_p = true;

	  /* Within a multiline range, don't display any underline
	     in any leading or trailing whitespace on a line.
	     We do display carets, however.  */
	  if (!out_state->draw_caret_p)
	    if (column < first_non_ws || column > last_non_ws)
	      return false;

	  /* We are within a range.  */
	  return true;
	}
    }

  return false;
}

/* Helper function for use by layout::print_line when printing the
   annotation line under the source line.
   Get the column beyond the rightmost one that could contain a caret or
   range marker, given that we stop rendering at trailing whitespace.
   ROW is the source line within the given file.
   CARET_COLUMN is the column of range 0's caret.
   LAST_NON_WS_COLUMN is the last column containing a non-whitespace
   character of source (as determined when printing the source line).  */

int
layout::get_x_bound_for_row (int row, int caret_column,
			     int last_non_ws_column)
{
  int result = caret_column + 1;

  layout_range *range;
  int i;
  FOR_EACH_VEC_ELT (m_layout_ranges, i, range)
    {
      if (row >= range->m_start.m_line)
	{
	  if (range->m_finish.m_line == row)
	    {
	      /* On the final line within a range; ensure that
		 we render up to the end of the range.  */
	      if (result <= range->m_finish.m_column)
		result = range->m_finish.m_column + 1;
	    }
	  else if (row < range->m_finish.m_line)
	    {
	      /* Within a multiline range; ensure that we render up to the
		 last non-whitespace column.  */
	      if (result <= last_non_ws_column)
		result = last_non_ws_column + 1;
	    }
	}
    }

  return result;
}

/* Given *COLUMN as an x-coordinate, print spaces to position
   successive output at DEST_COLUMN, printing a newline if necessary,
   and updating *COLUMN.  */

void
layout::move_to_column (int *column, int dest_column)
{
  /* Start a new line if we need to.  */
  if (*column > dest_column)
    {
      print_newline ();
      *column = 0;
    }

  while (*column < dest_column)
    {
      pp_space (m_pp);
      (*column)++;
    }
}

/* For debugging layout issues, render a ruler giving column numbers
   (after the 1-column indent).  */

void
layout::show_ruler (int max_column) const
{
  /* Hundreds.  */
  if (max_column > 99)
    {
      pp_space (m_pp);
      for (int column = 1 + m_x_offset; column <= max_column; column++)
	if (0 == column % 10)
	  pp_character (m_pp, '0' + (column / 100) % 10);
	else
	  pp_space (m_pp);
      pp_newline (m_pp);
    }

  /* Tens.  */
  pp_space (m_pp);
  for (int column = 1 + m_x_offset; column <= max_column; column++)
    if (0 == column % 10)
      pp_character (m_pp, '0' + (column / 10) % 10);
    else
      pp_space (m_pp);
  pp_newline (m_pp);

  /* Units.  */
  pp_space (m_pp);
  for (int column = 1 + m_x_offset; column <= max_column; column++)
    pp_character (m_pp, '0' + (column % 10));
  pp_newline (m_pp);
}

} /* End of anonymous namespace.  */

/* Print the physical source code corresponding to the location of
   this diagnostic, with additional annotations.  */

void
diagnostic_show_locus (diagnostic_context * context,
		       rich_location *richloc,
		       diagnostic_t diagnostic_kind)
{
  pp_newline (context->printer);

  location_t loc = richloc->get_loc ();
  /* Do nothing if source-printing has been disabled.  */
  if (!context->show_caret)
    return;

  /* Don't attempt to print source for UNKNOWN_LOCATION and for builtins.  */
  if (loc <= BUILTINS_LOCATION)
    return;

  /* Don't print the same source location twice in a row, unless we have
     fix-it hints.  */
  if (loc == context->last_location
      && richloc->get_num_fixit_hints () == 0)
    return;

  context->last_location = loc;

  const char *saved_prefix = pp_get_prefix (context->printer);
  pp_set_prefix (context->printer, NULL);

  layout layout (context, richloc, diagnostic_kind);
  for (int line_span_idx = 0; line_span_idx < layout.get_num_line_spans ();
       line_span_idx++)
    {
      const line_span *line_span = layout.get_line_span (line_span_idx);
      if (layout.print_heading_for_line_span_index_p (line_span_idx))
	{
	  expanded_location exploc = layout.get_expanded_location (line_span);
	  context->start_span (context, exploc);
	}
      int last_line = line_span->get_last_line ();
      for (int row = line_span->get_first_line (); row <= last_line; row++)
	{
	  /* Print the source line, followed by an annotation line
	     consisting of any caret/underlines, then any fixits.
	     If the source line can't be read, print nothing.  */
	  line_bounds lbounds;
	  if (layout.print_source_line (row, &lbounds))
	    {
	      layout.print_annotation_line (row, lbounds);
	      layout.print_any_fixits (row, richloc);
	    }
	}
    }

  pp_set_prefix (context->printer, saved_prefix);
}

#if CHECKING_P

namespace selftest {

/* Selftests for diagnostic_show_locus.  */

/* Convenience subclass of diagnostic_context for testing
   diagnostic_show_locus.  */

class test_diagnostic_context : public diagnostic_context
{
 public:
  test_diagnostic_context ()
  {
    diagnostic_initialize (this, 0);
    show_caret = true;
  }
  ~test_diagnostic_context ()
  {
    diagnostic_finish (this);
  }
};

/* Verify that diagnostic_show_locus works sanely on UNKNOWN_LOCATION.  */

static void
test_diagnostic_show_locus_unknown_location ()
{
  test_diagnostic_context dc;
  rich_location richloc (line_table, UNKNOWN_LOCATION);
  diagnostic_show_locus (&dc, &richloc, DK_ERROR);
  ASSERT_STREQ ("\n", pp_formatted_text (dc.printer));
}

/* Verify that diagnostic_show_locus works sanely for various
   single-line cases.

   All of these work on the following 1-line source file:
     .0000000001111111
     .1234567890123456
     "foo = bar.field;\n"
   which is set up by test_diagnostic_show_locus_one_liner and calls
   them.  */

/* Just a caret.  */

static void
test_one_liner_simple_caret ()
{
  test_diagnostic_context dc;
  location_t caret = linemap_position_for_column (line_table, 10);
  rich_location richloc (line_table, caret);
  diagnostic_show_locus (&dc, &richloc, DK_ERROR);
  ASSERT_STREQ ("\n"
		" foo = bar.field;\n"
		"          ^\n",
		pp_formatted_text (dc.printer));
}

/* Caret and range.  */

static void
test_one_liner_caret_and_range ()
{
  test_diagnostic_context dc;
  location_t caret = linemap_position_for_column (line_table, 10);
  location_t start = linemap_position_for_column (line_table, 7);
  location_t finish = linemap_position_for_column (line_table, 15);
  location_t loc = make_location (caret, start, finish);
  rich_location richloc (line_table, loc);
  diagnostic_show_locus (&dc, &richloc, DK_ERROR);
  ASSERT_STREQ ("\n"
		" foo = bar.field;\n"
		"       ~~~^~~~~~\n",
		pp_formatted_text (dc.printer));
}

/* Multiple ranges and carets.  */

static void
test_one_liner_multiple_carets_and_ranges ()
{
  test_diagnostic_context dc;
  location_t foo
    = make_location (linemap_position_for_column (line_table, 2),
		     linemap_position_for_column (line_table, 1),
		     linemap_position_for_column (line_table, 3));
  dc.caret_chars[0] = 'A';

  location_t bar
    = make_location (linemap_position_for_column (line_table, 8),
		     linemap_position_for_column (line_table, 7),
		     linemap_position_for_column (line_table, 9));
  dc.caret_chars[1] = 'B';

  location_t field
    = make_location (linemap_position_for_column (line_table, 13),
		     linemap_position_for_column (line_table, 11),
		     linemap_position_for_column (line_table, 15));
  dc.caret_chars[2] = 'C';

  rich_location richloc (line_table, foo);
  richloc.add_range (bar, true);
  richloc.add_range (field, true);
  diagnostic_show_locus (&dc, &richloc, DK_ERROR);
  ASSERT_STREQ ("\n"
		" foo = bar.field;\n"
		" ~A~   ~B~ ~~C~~\n",
		pp_formatted_text (dc.printer));
}

/* Insertion fix-it hint: adding an "&" to the front of "bar.field". */

static void
test_one_liner_fixit_insert ()
{
  test_diagnostic_context dc;
  location_t caret = linemap_position_for_column (line_table, 7);
  rich_location richloc (line_table, caret);
  richloc.add_fixit_insert (caret, "&");
  diagnostic_show_locus (&dc, &richloc, DK_ERROR);
  ASSERT_STREQ ("\n"
		" foo = bar.field;\n"
		"       ^\n"
		"       &\n",
		pp_formatted_text (dc.printer));
}

/* Removal fix-it hint: removal of the ".field". */

static void
test_one_liner_fixit_remove ()
{
  test_diagnostic_context dc;
  location_t start = linemap_position_for_column (line_table, 10);
  location_t finish = linemap_position_for_column (line_table, 15);
  location_t dot = make_location (start, start, finish);
  rich_location richloc (line_table, dot);
  source_range range;
  range.m_start = start;
  range.m_finish = finish;
  richloc.add_fixit_remove (range);
  diagnostic_show_locus (&dc, &richloc, DK_ERROR);
  ASSERT_STREQ ("\n"
		" foo = bar.field;\n"
		"          ^~~~~~\n"
		"          ------\n",
		pp_formatted_text (dc.printer));
}

/* Replace fix-it hint: replacing "field" with "m_field". */

static void
test_one_liner_fixit_replace ()
{
  test_diagnostic_context dc;
  location_t start = linemap_position_for_column (line_table, 11);
  location_t finish = linemap_position_for_column (line_table, 15);
  location_t field = make_location (start, start, finish);
  rich_location richloc (line_table, field);
  source_range range;
  range.m_start = start;
  range.m_finish = finish;
  richloc.add_fixit_replace (range, "m_field");
  diagnostic_show_locus (&dc, &richloc, DK_ERROR);
  ASSERT_STREQ ("\n"
		" foo = bar.field;\n"
		"           ^~~~~\n"
		"           m_field\n",
		pp_formatted_text (dc.printer));
}

/* Replace fix-it hint: replacing "field" with "m_field",
   but where the caret was elsewhere.  */

static void
test_one_liner_fixit_replace_non_equal_range ()
{
  test_diagnostic_context dc;
  location_t equals = linemap_position_for_column (line_table, 5);
  location_t start = linemap_position_for_column (line_table, 11);
  location_t finish = linemap_position_for_column (line_table, 15);
  rich_location richloc (line_table, equals);
  source_range range;
  range.m_start = start;
  range.m_finish = finish;
  richloc.add_fixit_replace (range, "m_field");
  diagnostic_show_locus (&dc, &richloc, DK_ERROR);
  /* The replacement range is not indicated in the annotation line, so
     it should be indicated via an additional underline.  */
  ASSERT_STREQ ("\n"
		" foo = bar.field;\n"
		"     ^\n"
		"           -----\n"
		"           m_field\n",
		pp_formatted_text (dc.printer));
}

/* Replace fix-it hint: replacing "field" with "m_field",
   where the caret was elsewhere, but where a secondary range
   exactly covers "field".  */

static void
test_one_liner_fixit_replace_equal_secondary_range ()
{
  test_diagnostic_context dc;
  location_t equals = linemap_position_for_column (line_table, 5);
  location_t start = linemap_position_for_column (line_table, 11);
  location_t finish = linemap_position_for_column (line_table, 15);
  rich_location richloc (line_table, equals);
  location_t field = make_location (start, start, finish);
  richloc.add_range (field, false);
  source_range range;
  range.m_start = start;
  range.m_finish = finish;
  richloc.add_fixit_replace (range, "m_field");
  diagnostic_show_locus (&dc, &richloc, DK_ERROR);
  /* The replacement range is indicated in the annotation line,
     so it shouldn't be indicated via an additional underline.  */
  ASSERT_STREQ ("\n"
		" foo = bar.field;\n"
		"     ^     ~~~~~\n"
		"           m_field\n",
		pp_formatted_text (dc.printer));
}

/* Run the various one-liner tests.  */

static void
test_diagnostic_show_locus_one_liner (const line_table_case &case_)
{
  /* Create a tempfile and write some text to it.
     ....................0000000001111111.
     ....................1234567890123456.  */
  const char *content = "foo = bar.field;\n";
  temp_source_file tmp (SELFTEST_LOCATION, ".c", content);
  line_table_test ltt (case_);

  linemap_add (line_table, LC_ENTER, false, tmp.get_filename (), 1);

  location_t line_end = linemap_position_for_column (line_table, 16);

  /* Don't attempt to run the tests if column data might be unavailable.  */
  if (line_end > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  ASSERT_STREQ (tmp.get_filename (), LOCATION_FILE (line_end));
  ASSERT_EQ (1, LOCATION_LINE (line_end));
  ASSERT_EQ (16, LOCATION_COLUMN (line_end));

  test_one_liner_simple_caret ();
  test_one_liner_caret_and_range ();
  test_one_liner_multiple_carets_and_ranges ();
  test_one_liner_fixit_insert ();
  test_one_liner_fixit_remove ();
  test_one_liner_fixit_replace ();
  test_one_liner_fixit_replace_non_equal_range ();
  test_one_liner_fixit_replace_equal_secondary_range ();
}

/* Verify that fix-it hints are appropriately consolidated.

   If any fix-it hints in a rich_location involve locations beyond
   LINE_MAP_MAX_LOCATION_WITH_COLS, then we can't reliably apply
   the fix-it as a whole, so there should be none.

   Otherwise, verify that consecutive "replace" and "remove" fix-its
   are merged, and that other fix-its remain separate.   */

static void
test_fixit_consolidation (const line_table_case &case_)
{
  line_table_test ltt (case_);

  linemap_add (line_table, LC_ENTER, false, "test.c", 1);

  const location_t c10 = linemap_position_for_column (line_table, 10);
  const location_t c15 = linemap_position_for_column (line_table, 15);
  const location_t c16 = linemap_position_for_column (line_table, 16);
  const location_t c17 = linemap_position_for_column (line_table, 17);
  const location_t c20 = linemap_position_for_column (line_table, 20);
  const location_t caret = c10;

  /* Insert + insert. */
  {
    rich_location richloc (line_table, caret);
    richloc.add_fixit_insert (c10, "foo");
    richloc.add_fixit_insert (c15, "bar");

    if (c15 > LINE_MAP_MAX_LOCATION_WITH_COLS)
      /* Bogus column info for 2nd fixit, so no fixits.  */
      ASSERT_EQ (0, richloc.get_num_fixit_hints ());
    else
      /* They should not have been merged.  */
      ASSERT_EQ (2, richloc.get_num_fixit_hints ());
  }

  /* Insert + replace. */
  {
    rich_location richloc (line_table, caret);
    richloc.add_fixit_insert (c10, "foo");
    richloc.add_fixit_replace (source_range::from_locations (c15, c17),
			       "bar");

    if (c17 > LINE_MAP_MAX_LOCATION_WITH_COLS)
      /* Bogus column info for 2nd fixit, so no fixits.  */
      ASSERT_EQ (0, richloc.get_num_fixit_hints ());
    else
      /* They should not have been merged.  */
      ASSERT_EQ (2, richloc.get_num_fixit_hints ());
  }

  /* Replace + non-consecutive insert. */
  {
    rich_location richloc (line_table, caret);
    richloc.add_fixit_replace (source_range::from_locations (c10, c15),
			       "bar");
    richloc.add_fixit_insert (c17, "foo");

    if (c17 > LINE_MAP_MAX_LOCATION_WITH_COLS)
      /* Bogus column info for 2nd fixit, so no fixits.  */
      ASSERT_EQ (0, richloc.get_num_fixit_hints ());
    else
      /* They should not have been merged.  */
      ASSERT_EQ (2, richloc.get_num_fixit_hints ());
  }

  /* Replace + non-consecutive replace. */
  {
    rich_location richloc (line_table, caret);
    richloc.add_fixit_replace (source_range::from_locations (c10, c15),
			       "foo");
    richloc.add_fixit_replace (source_range::from_locations (c17, c20),
			       "bar");

    if (c20 > LINE_MAP_MAX_LOCATION_WITH_COLS)
      /* Bogus column info for 2nd fixit, so no fixits.  */
      ASSERT_EQ (0, richloc.get_num_fixit_hints ());
    else
      /* They should not have been merged.  */
      ASSERT_EQ (2, richloc.get_num_fixit_hints ());
  }

  /* Replace + consecutive replace. */
  {
    rich_location richloc (line_table, caret);
    richloc.add_fixit_replace (source_range::from_locations (c10, c15),
			       "foo");
    richloc.add_fixit_replace (source_range::from_locations (c16, c20),
			       "bar");

    if (c20 > LINE_MAP_MAX_LOCATION_WITH_COLS)
      /* Bogus column info for 2nd fixit, so no fixits.  */
      ASSERT_EQ (0, richloc.get_num_fixit_hints ());
    else
      {
	/* They should have been merged into a single "replace".  */
	ASSERT_EQ (1, richloc.get_num_fixit_hints ());
	const fixit_hint *hint = richloc.get_fixit_hint (0);
	ASSERT_EQ (fixit_hint::REPLACE, hint->get_kind ());
	const fixit_replace *replace = (const fixit_replace *)hint;
	ASSERT_STREQ ("foobar", replace->get_string ());
	ASSERT_EQ (c10, replace->get_range ().m_start);
	ASSERT_EQ (c20, replace->get_range ().m_finish);
      }
  }

  /* Replace + consecutive removal. */
  {
    rich_location richloc (line_table, caret);
    richloc.add_fixit_replace (source_range::from_locations (c10, c15),
			       "foo");
    richloc.add_fixit_remove (source_range::from_locations (c16, c20));

    if (c20 > LINE_MAP_MAX_LOCATION_WITH_COLS)
      /* Bogus column info for 2nd fixit, so no fixits.  */
      ASSERT_EQ (0, richloc.get_num_fixit_hints ());
    else
      {
	/* They should have been merged into a single replace, with the
	   range extended to cover that of the removal.  */
	ASSERT_EQ (1, richloc.get_num_fixit_hints ());
	const fixit_hint *hint = richloc.get_fixit_hint (0);
	ASSERT_EQ (fixit_hint::REPLACE, hint->get_kind ());
	const fixit_replace *replace = (const fixit_replace *)hint;
	ASSERT_STREQ ("foo", replace->get_string ());
	ASSERT_EQ (c10, replace->get_range ().m_start);
	ASSERT_EQ (c20, replace->get_range ().m_finish);
      }
  }

  /* Consecutive removals. */
  {
    rich_location richloc (line_table, caret);
    richloc.add_fixit_remove (source_range::from_locations (c10, c15));
    richloc.add_fixit_remove (source_range::from_locations (c16, c20));

    if (c20 > LINE_MAP_MAX_LOCATION_WITH_COLS)
      /* Bogus column info for 2nd fixit, so no fixits.  */
      ASSERT_EQ (0, richloc.get_num_fixit_hints ());
    else
      {
	/* They should have been merged into a single "replace-with-empty".  */
	ASSERT_EQ (1, richloc.get_num_fixit_hints ());
	const fixit_hint *hint = richloc.get_fixit_hint (0);
	ASSERT_EQ (fixit_hint::REPLACE, hint->get_kind ());
	const fixit_replace *replace = (const fixit_replace *)hint;
	ASSERT_STREQ ("", replace->get_string ());
	ASSERT_EQ (c10, replace->get_range ().m_start);
	ASSERT_EQ (c20, replace->get_range ().m_finish);
      }
  }
}

/* Run all of the selftests within this file.  */

void
diagnostic_show_locus_c_tests ()
{
  test_range_contains_point_for_single_point ();
  test_range_contains_point_for_single_line ();
  test_range_contains_point_for_multiple_lines ();

  test_get_line_width_without_trailing_whitespace ();

  test_diagnostic_show_locus_unknown_location ();

  for_each_line_table_case (test_diagnostic_show_locus_one_liner);
  for_each_line_table_case (test_fixit_consolidation);
}

} // namespace selftest

#endif /* #if CHECKING_P */
