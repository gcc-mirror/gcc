/* Diagnostic subroutines for printing source-code
   Copyright (C) 1999-2024 Free Software Foundation, Inc.
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
#define INCLUDE_MEMORY
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "version.h"
#include "demangle.h"
#include "intl.h"
#include "backtrace.h"
#include "diagnostic.h"
#include "diagnostic-color.h"
#include "gcc-rich-location.h"
#include "text-range-label.h"
#include "selftest.h"
#include "selftest-diagnostic.h"
#include "selftest-diagnostic-show-locus.h"
#include "cpplib.h"
#include "text-art/types.h"
#include "text-art/theme.h"
#include "diagnostic-label-effects.h"

#ifdef HAVE_TERMIOS_H
# include <termios.h>
#endif

#ifdef GWINSZ_IN_SYS_IOCTL
# include <sys/ioctl.h>
#endif

/* Disable warnings about quoting issues in the pp_xxx calls below
   that (intentionally) don't follow GCC diagnostic conventions.  */
#if __GNUC__ >= 10
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wformat-diag"
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
   layout_printer::print_source_line and layout_printer::print_annotation_line
   to simply request a colorization code for *every* character they print,
   via this class, and have the filtering be done for them here.  */

class colorizer
{
 public:
  colorizer (pretty_printer &pp,
	     const rich_location &richloc,
	     diagnostic_t diagnostic_kind);
  ~colorizer ();

  void set_range (int range_idx)
  {
    /* If we have a specific highlight color for the range, use it.  */
    if (pp_show_highlight_colors (&m_pp))
      {
	const location_range *const loc_range = m_richloc.get_range (range_idx);
	if (loc_range->m_highlight_color)
	  {
	    set_named_color (loc_range->m_highlight_color);
	    return;
	  }
      }

    /* Otherwise, we emphasize the primary location, then alternate between
       two colors for the secondary locations.
       But if we're printing a run of events in a diagnostic path, that
       makes no sense, so print all of them with the same colorization.  */
    if (m_diagnostic_kind == DK_DIAGNOSTIC_PATH)
      set_state (0);
    else
      set_state (range_idx);
  }
  void set_cfg_edge () { set_state (0); }
  void set_normal_text () { set_state (STATE_NORMAL_TEXT); }
  void set_fixit_insert () { set_state (STATE_FIXIT_INSERT); }
  void set_fixit_delete () { set_state (STATE_FIXIT_DELETE); }
  void set_named_color (const char *color);

 private:
  void set_state (int state);
  void begin_state (int state);
  void finish_state (int state);
  const char *get_color_by_name (const char *);

 private:
  static const int STATE_NORMAL_TEXT = -1;
  static const int STATE_FIXIT_INSERT  = -2;
  static const int STATE_FIXIT_DELETE  = -3;
  static const int STATE_NAMED_COLOR  = -4;

  pretty_printer &m_pp;
  const rich_location &m_richloc;
  diagnostic_t m_diagnostic_kind;
  int m_current_state;
  const char *m_range1;
  const char *m_range2;
  const char *m_fixit_insert;
  const char *m_fixit_delete;
  const char *m_stop_color;
};

/* In order to handle multibyte sources properly, all of this logic needs to be
   aware of the distinction between the number of bytes and the number of
   display columns occupied by a character, which are not the same for non-ASCII
   characters.  For example, the Unicode pi symbol, U+03C0, is encoded in UTF-8
   as "\xcf\x80", and thus occupies 2 bytes of space while only occupying 1
   display column when it is output.  A typical emoji, such as U+1F602 (in
   UTF-8, "\xf0\x9f\x98\x82"), requires 4 bytes and has a display width of 2.

   The below example line, which is also used for selftests below, shows how the
   display column and byte column are related:

     0000000001111111111222222   display
     1234567890123456789012345   columns
     SS_foo = P_bar.SS_fieldP;
     0000000111111111222222223   byte
     1356789012456789134567891   columns

   Here SS represents the two display columns for the U+1F602 emoji, and P
   represents the one display column for the U+03C0 pi symbol.  As an example, a
   diagnostic pointing to the final P on this line is at byte column 29 and
   display column 24.  This reflects the fact that the three extended characters
   before the final P occupy cumulatively 5 more bytes than they do display
   columns (a difference of 2 for each of the two SSs, and one for the other P).

   One or the other of the two column units is more useful depending on the
   context.  For instance, in order to output the caret at the correct location,
   we need to count display columns; in order to colorize a source line, we need
   to count the bytes.  All locations are provided to us as byte counts, which
   we augment with the display column on demand so that it can be used when
   needed.  This is not the most efficient way to do things since it requires
   looping over the whole line each time, but it should be fine for the purpose
   of outputting diagnostics.

   In order to keep straight which units (byte or display) are in use at a
   given time, the following enum lets us specify that explicitly.  */

enum column_unit {
  /* Measured in raw bytes.  */
  CU_BYTES = 0,

  /* Measured in display units.  */
  CU_DISPLAY_COLS,

  /* For arrays indexed by column_unit.  */
  CU_NUM_UNITS
};

/* Utility class to augment an exploc with the corresponding display column.  */

class exploc_with_display_col : public expanded_location
{
 public:
  exploc_with_display_col (file_cache &fc,
			   const expanded_location &exploc,
			   const cpp_char_column_policy &policy,
			   enum location_aspect aspect)
  : expanded_location (exploc),
    m_display_col (location_compute_display_column (fc, exploc, policy))
  {
    if (exploc.column > 0)
      {
	/* m_display_col is now the final column of the byte.
	   If escaping has happened, we may want the first column instead.  */
	if (aspect != LOCATION_ASPECT_FINISH)
	  {
	    expanded_location prev_exploc (exploc);
	    prev_exploc.column--;
	    int prev_display_col
	      = (location_compute_display_column (fc, prev_exploc, policy));
	    m_display_col = prev_display_col + 1;
	  }
      }
  }

  int m_display_col;
};


/* A point within a layout_range; similar to an exploc_with_display_col,
   but after filtering on file.  */

class layout_point
{
 public:
  layout_point (const exploc_with_display_col &exploc)
    : m_line (exploc.line)
  {
    m_columns[CU_BYTES] = exploc.column;
    m_columns[CU_DISPLAY_COLS] = exploc.m_display_col;
  }

  linenum_type m_line;
  int m_columns[CU_NUM_UNITS];
};

/* A class for use by "class layout" below: a filtered location_range.  */

class layout_range
{
 public:
  layout_range (const exploc_with_display_col &start_exploc,
		const exploc_with_display_col &finish_exploc,
		enum range_display_kind range_display_kind,
		const exploc_with_display_col &caret_exploc,
		unsigned original_idx,
		const range_label *label);

  bool contains_point (linenum_type row, int column,
		       enum column_unit col_unit) const;
  bool intersects_line_p (linenum_type row) const;

  bool has_in_edge () const;
  bool has_out_edge () const;

  layout_point m_start;
  layout_point m_finish;
  enum range_display_kind m_range_display_kind;
  layout_point m_caret;
  unsigned m_original_idx;
  const range_label *m_label;
};

/* A struct for use by layout::print_source_line for telling
   layout::print_annotation_line the extents of the source line that
   it printed, so that underlines can be clipped appropriately.  Units
   are 1-based display columns.  */

struct line_bounds
{
  int m_first_non_ws_disp_col;
  int m_last_non_ws_disp_col;

  line_bounds ()
  {
    m_first_non_ws_disp_col = INT_MAX;
    m_last_non_ws_disp_col = 0;
  }
};

/* A range of contiguous source lines within a layout (e.g. "lines 5-10"
   or "line 23").  During the layout ctor, layout::calculate_line_spans
   splits the pertinent source lines into a list of disjoint line_span
   instances (e.g. lines 5-10, lines 15-20, line 23).  */

class line_span
{
public:
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
    int first_line_cmp = compare (ls1->m_first_line, ls2->m_first_line);
    if (first_line_cmp)
      return first_line_cmp;
    return compare (ls1->m_last_line, ls2->m_last_line);
  }

  linenum_type m_first_line;
  linenum_type m_last_line;
};

#if CHECKING_P

/* Selftests for line_span.  */

static void
test_line_span ()
{
  line_span line_one (1, 1);
  ASSERT_EQ (1, line_one.get_first_line ());
  ASSERT_EQ (1, line_one.get_last_line ());
  ASSERT_FALSE (line_one.contains_line_p (0));
  ASSERT_TRUE (line_one.contains_line_p (1));
  ASSERT_FALSE (line_one.contains_line_p (2));

  line_span lines_1_to_3 (1, 3);
  ASSERT_EQ (1, lines_1_to_3.get_first_line ());
  ASSERT_EQ (3, lines_1_to_3.get_last_line ());
  ASSERT_TRUE (lines_1_to_3.contains_line_p (1));
  ASSERT_TRUE (lines_1_to_3.contains_line_p (3));

  ASSERT_EQ (0, line_span::comparator (&line_one, &line_one));
  ASSERT_GT (line_span::comparator (&lines_1_to_3, &line_one), 0);
  ASSERT_LT (line_span::comparator (&line_one, &lines_1_to_3), 0);

  /* A linenum > 2^31.  */
  const linenum_type LARGEST_LINE = 0xffffffff;
  line_span largest_line (LARGEST_LINE, LARGEST_LINE);
  ASSERT_EQ (LARGEST_LINE, largest_line.get_first_line ());
  ASSERT_EQ (LARGEST_LINE, largest_line.get_last_line ());

  ASSERT_GT (line_span::comparator (&largest_line, &line_one), 0);
  ASSERT_LT (line_span::comparator (&line_one, &largest_line), 0);
}

#endif /* #if CHECKING_P */

/* A bundle of information containing how to print unicode
   characters and bytes when quoting source code.

   Provides a unified place to support escaping some subset
   of characters to some format.

   Extends char_column_policy; printing is split out to avoid
   libcpp having to know about pretty_printer.  */

struct char_display_policy : public cpp_char_column_policy
{
 public:
  char_display_policy (int tabstop,
		       int (*width_cb) (cppchar_t c),
		       void (*print_cb) (pretty_printer *pp,
					 const cpp_decoded_char &cp))
  : cpp_char_column_policy (tabstop, width_cb),
    m_print_cb (print_cb)
  {
  }

  void (*m_print_cb) (pretty_printer *pp,
		      const cpp_decoded_char &cp);
};

/* A class to control the overall layout when printing a diagnostic.

   The layout is determined within the constructor.

   Printing the layout is handled by class layout_printer.  This separation
   is to avoid depending on the pretty_printer in the layout.

   We assume we have disjoint ranges.  */

class layout
{
 public:
  friend class layout_printer;

  layout (const diagnostic_source_print_policy &source_policy,
	  const rich_location &richloc,
	  diagnostic_source_effect_info *effect_info = nullptr);

  bool maybe_add_location_range (const location_range *loc_range,
				 unsigned original_idx,
				 bool restrict_to_current_line_spans);

  int get_num_line_spans () const { return m_line_spans.length (); }
  const line_span *get_line_span (int idx) const { return &m_line_spans[idx]; }

  int get_linenum_width () const { return m_linenum_width; }
  int get_x_offset_display () const { return m_x_offset_display; }

  bool print_heading_for_line_span_index_p (int line_span_idx) const;

  expanded_location get_expanded_location (const line_span *) const;

  void on_bad_codepoint (const char *ptr, cppchar_t ch, size_t ch_sz);

 private:
  bool will_show_line_p (linenum_type row) const;
  bool should_print_annotation_line_p (linenum_type row) const;

  bool annotation_line_showed_range_p (linenum_type line, int start_column,
				       int finish_column) const;
  bool validate_fixit_hint_p (const fixit_hint *hint);

  void calculate_line_spans ();
  void calculate_linenum_width ();
  void calculate_x_offset_display ();

  bool
  get_state_at_point (/* Inputs.  */
		      linenum_type row, int column,
		      int first_non_ws, int last_non_ws,
		      enum column_unit col_unit,
		      /* Outputs.  */
		      point_state *out_state) const;

  int
  get_x_bound_for_row (linenum_type row, int caret_column,
		       int last_non_ws) const;

 private:
  bool compatible_locations_p (location_t loc_a, location_t loc_b) const;

  const diagnostic_source_printing_options &m_options;
  const line_maps *m_line_table;
  file_cache &m_file_cache;
  const text_art::ascii_theme m_fallback_theme;
  const text_art::theme &m_theme;
  diagnostic_source_effect_info *m_effect_info;
  char_display_policy m_char_policy;
  location_t m_primary_loc;
  exploc_with_display_col m_exploc;
  auto_vec <layout_range> m_layout_ranges;
  auto_vec <const fixit_hint *> m_fixit_hints;
  auto_vec <line_span> m_line_spans;
  int m_linenum_width;
  int m_x_offset_display;
  bool m_escape_on_output;
};

/* A bundle of state for printing a particular layout
   to a particular pretty_printer.  */

class layout_printer
{
public:
  layout_printer (pretty_printer &pp,
		  const layout &layout,
		  const rich_location &richloc,
		  diagnostic_t diagnostic_kind);

  void print (const diagnostic_source_print_policy &source_policy);

private:
  const diagnostic_source_printing_options &
  get_options () const { return m_layout.m_options; }

  const text_art::theme &
  get_theme () const { return m_layout.m_theme; }

  void show_ruler (int max_column);
  void print_gap_in_line_numbering ();
  void print_leading_fixits (linenum_type row);
  void print_line (linenum_type row);
  line_bounds print_source_line (linenum_type row, const char *line,
				 int line_bytes);
  void print_leftmost_column ();
  void start_annotation_line (char margin_char = ' ');
  void print_annotation_line (linenum_type row, const line_bounds lbounds);
  void print_any_labels (linenum_type row);
  void print_trailing_fixits (linenum_type row);
  void print_newline ();

  void
  move_to_column (int *column, int dest_column, bool add_left_margin);

  void print_any_right_to_left_edge_lines ();

private:
  pretty_printer &m_pp;
  const layout &m_layout;
  colorizer m_colorizer;
  bool m_is_diagnostic_path;

  /* Fields for handling links between labels (e.g. for showing CFG edges
     in execution paths).
     Note that the logic for printing such links makes various simplifying
     assumptions about the set of labels in the rich_location, and users
     of this code will need to split up labels into separate rich_location
     instances to respect these assumptions, or the output will look wrong.
     See the diagnostic_path-printing code for more information.  */

  /* An enum for describing the state of the leftmost column,
     used for showing links between labels.
     Consider e.g.
     .x0000000001111111111222222222233333333334444444444.
     .x1234567890123456789012345678901234567890123456789.
     |      |                                    <- none
     |      (9) following ‘false’ branch... ->-+ <- none
     |                                         | <- none
     |                                         | <- none
     |+----------------------------------------+ <- rewinding to lhs
     ||  result->i = i;                          <- at lhs
     ||  ~~~~~~~~~~^~~                           <- at lhs
     ||            |                             <- at lhs
     |+----------->(10) ...to here               <- indenting to dest
     ^^
     ||
     |leftmost column ("x" above).
     "margin".  */
  enum class link_lhs_state {
    none,
    rewinding_to_lhs,
    at_lhs,
    indenting_to_dest
  } m_link_lhs_state;

  /* The column of the current link on the RHS, if any, or
     -1 if there is none.
     Consider e.g.
     .x0000000001111111111222222222233333333334444444444.
     .x1234567890123456789012345678901234567890123456789.
     |      |                                     <- -1
     |      (10) following ‘false’ branch... ->-+ <- 42
     |                                          | <- 42
     |                                          | <- 42
     |+-----------------------------------------+ <- 42
     ||  result->i = i;                           <- -1
     ||  ~~~~~~~~~~^~~                            <- -1
     ||            |                              <- -1
     |+----------->(11) ...to here                <- -1.  */
  int m_link_rhs_column;
};

/* Implementation of "class colorizer".  */

/* The constructor for "colorizer".  Lookup and store color codes for the
   different kinds of things we might need to print.  */

colorizer::colorizer (pretty_printer &pp,
		      const rich_location &richloc,
		      diagnostic_t diagnostic_kind) :
  m_pp (pp),
  m_richloc (richloc),
  m_diagnostic_kind (diagnostic_kind),
  m_current_state (STATE_NORMAL_TEXT)
{
  m_range1 = get_color_by_name ("range1");
  m_range2 = get_color_by_name ("range2");
  m_fixit_insert = get_color_by_name ("fixit-insert");
  m_fixit_delete = get_color_by_name ("fixit-delete");
  m_stop_color = colorize_stop (pp_show_color (&m_pp));
}

/* The destructor for "colorize".  If colorization is on, print a code to
   turn it off.  */

colorizer::~colorizer ()
{
  finish_state (m_current_state);
}

/* Update state, changing to the specific named color and printing its
   color codes.  */

void
colorizer::set_named_color (const char *color)
{
  finish_state (m_current_state);
  m_current_state = STATE_NAMED_COLOR;
  pp_string (&m_pp, colorize_start (pp_show_color (&m_pp), color));
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
      pp_string (&m_pp, m_fixit_insert);
      break;

    case STATE_FIXIT_DELETE:
      pp_string (&m_pp, m_fixit_delete);
      break;

    case STATE_NAMED_COLOR:
      /* Should be handled by colorizer::set_named_color.  */
      gcc_unreachable ();

    case 0:
      /* Make range 0 be the same color as the "kind" text
	 (error vs warning vs note).  */
      pp_string
	(&m_pp,
	 colorize_start (pp_show_color (&m_pp),
			 diagnostic_get_color_for_kind (m_diagnostic_kind)));
      break;

    case 1:
      pp_string (&m_pp, m_range1);
      break;

    case 2:
      pp_string (&m_pp, m_range2);
      break;

    default:
      /* For ranges beyond 2, alternate between color 1 and color 2.  */
      {
	gcc_assert (state > 2);
	pp_string (&m_pp,
		   state % 2 ? m_range1 : m_range2);
      }
      break;
    }
}

/* Turn off any colorization for STATE.  */

void
colorizer::finish_state (int state)
{
  if (state != STATE_NORMAL_TEXT)
    pp_string (&m_pp, m_stop_color);
}

/* Get the color code for NAME (or the empty string if
   colorization is disabled).  */

const char *
colorizer::get_color_by_name (const char *name)
{
  return colorize_start (pp_show_color (&m_pp), name);
}

/* Implementation of class layout_range.  */

/* The constructor for class layout_range.
   Initialize various layout_point fields from expanded_location
   equivalents; we've already filtered on file.  */

layout_range::layout_range (const exploc_with_display_col &start_exploc,
			    const exploc_with_display_col &finish_exploc,
			    enum range_display_kind range_display_kind,
			    const exploc_with_display_col &caret_exploc,
			    unsigned original_idx,
			    const range_label *label)
: m_start (start_exploc),
  m_finish (finish_exploc),
  m_range_display_kind (range_display_kind),
  m_caret (caret_exploc),
  m_original_idx (original_idx),
  m_label (label)
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
   - 'a' indicates a subsequent point *after* the range.

   COL_UNIT controls whether we check the byte column or
   the display column; one or the other is more convenient
   depending on the context.  */

bool
layout_range::contains_point (linenum_type row, int column,
			      enum column_unit col_unit) const
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
      if (column < m_start.m_columns[col_unit])
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
	  return column <= m_finish.m_columns[col_unit];
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

  return column <= m_finish.m_columns[col_unit];
}

/* Does this layout_range contain any part of line ROW?  */

bool
layout_range::intersects_line_p (linenum_type row) const
{
  gcc_assert (m_start.m_line <= m_finish.m_line);
  if (row < m_start.m_line)
    return false;
  if (row > m_finish.m_line)
    return false;
  return true;
}

/* Return true if this layout_range should have an in-edge.  */

bool
layout_range::has_in_edge () const
{
  if (!m_label)
    return false;
  const label_effects *effects = m_label->get_effects (m_original_idx);
  if (!effects)
    return false;

  return effects->has_in_edge (m_original_idx);
}

/* Return true if this layout_range should have an out-edge.  */

bool
layout_range::has_out_edge () const
{
  if (!m_label)
    return false;
  const label_effects *effects = m_label->get_effects (m_original_idx);
  if (!effects)
    return false;

  return effects->has_out_edge (m_original_idx);
}

#if CHECKING_P

/* Default for when we don't care what the tab expansion is set to.  */
static const int def_tabstop = 8;

static cpp_char_column_policy def_policy ()
{
  return cpp_char_column_policy (def_tabstop, cpp_wcwidth);
}

/* Create some expanded locations for testing layout_range.  The filename
   member of the explocs is set to the empty string.  This member will only be
   inspected by the calls to location_compute_display_column() made from the
   layout_point constructors.  That function will check for an empty filename
   argument and not attempt to open it, rather treating the non-existent data
   as if the display width were the same as the byte count.  Tests exercising a
   real difference between byte count and display width are performed later,
   e.g. in test_diagnostic_show_locus_one_liner_utf8().  */

static layout_range
make_range (file_cache &fc,
	    int start_line, int start_col, int end_line, int end_col)
{
  const expanded_location start_exploc
    = {"", start_line, start_col, NULL, false};
  const expanded_location finish_exploc
    = {"", end_line, end_col, NULL, false};
  return layout_range (exploc_with_display_col (fc,
						start_exploc, def_policy (),
						LOCATION_ASPECT_START),
		       exploc_with_display_col (fc,
						finish_exploc, def_policy (),
						LOCATION_ASPECT_FINISH),
		       SHOW_RANGE_WITHOUT_CARET,
		       exploc_with_display_col (fc,
						start_exploc, def_policy (),
						LOCATION_ASPECT_CARET),
		       0, NULL);
}

/* Selftests for layout_range::contains_point and
   layout_range::intersects_line_p.  */

/* Selftest for layout_range, where the layout_range
   is a range with start==end i.e. a single point.  */

static void
test_layout_range_for_single_point ()
{
  file_cache fc;
  layout_range point = make_range (fc, 7, 10, 7, 10);

  /* Tests for layout_range::contains_point.  */

  for (int i = 0; i != CU_NUM_UNITS; ++i)
    {
      const enum column_unit col_unit = (enum column_unit) i;

      /* Before the line.  */
      ASSERT_FALSE (point.contains_point (6, 1, col_unit));

      /* On the line, but before start.  */
      ASSERT_FALSE (point.contains_point (7, 9, col_unit));

      /* At the point.  */
      ASSERT_TRUE (point.contains_point (7, 10, col_unit));

      /* On the line, after the point.  */
      ASSERT_FALSE (point.contains_point (7, 11, col_unit));

      /* After the line.  */
      ASSERT_FALSE (point.contains_point (8, 1, col_unit));
    }

  /* Tests for layout_range::intersects_line_p.  */
  ASSERT_FALSE (point.intersects_line_p (6));
  ASSERT_TRUE (point.intersects_line_p (7));
  ASSERT_FALSE (point.intersects_line_p (8));
}

/* Selftest for layout_range, where the layout_range
   is the single-line range shown as "Example A" above.  */

static void
test_layout_range_for_single_line ()
{
  file_cache fc;
  layout_range example_a = make_range (fc, 2, 22, 2, 38);

  /* Tests for layout_range::contains_point.  */

  for (int i = 0; i != CU_NUM_UNITS; ++i)
    {
      const enum column_unit col_unit = (enum column_unit) i;

      /* Before the line.  */
      ASSERT_FALSE (example_a.contains_point (1, 1, col_unit));

      /* On the line, but before start.  */
      ASSERT_FALSE (example_a.contains_point (2, 21, col_unit));

      /* On the line, at the start.  */
      ASSERT_TRUE (example_a.contains_point (2, 22, col_unit));

      /* On the line, within the range.  */
      ASSERT_TRUE (example_a.contains_point (2, 23, col_unit));

      /* On the line, at the end.  */
      ASSERT_TRUE (example_a.contains_point (2, 38, col_unit));

      /* On the line, after the end.  */
      ASSERT_FALSE (example_a.contains_point (2, 39, col_unit));

      /* After the line.  */
      ASSERT_FALSE (example_a.contains_point (2, 39, col_unit));
    }

  /* Tests for layout_range::intersects_line_p.  */
  ASSERT_FALSE (example_a.intersects_line_p (1));
  ASSERT_TRUE (example_a.intersects_line_p (2));
  ASSERT_FALSE (example_a.intersects_line_p (3));
}

/* Selftest for layout_range, where the layout_range
   is the multi-line range shown as "Example B" above.  */

static void
test_layout_range_for_multiple_lines ()
{
  file_cache fc;
  layout_range example_b = make_range (fc, 3, 14, 5, 8);

  /* Tests for layout_range::contains_point.  */

  for (int i = 0; i != CU_NUM_UNITS; ++i)
    {
      const enum column_unit col_unit = (enum column_unit) i;

      /* Before first line.  */
      ASSERT_FALSE (example_b.contains_point (1, 1, col_unit));

      /* On the first line, but before start.  */
      ASSERT_FALSE (example_b.contains_point (3, 13, col_unit));

      /* At the start.  */
      ASSERT_TRUE (example_b.contains_point (3, 14, col_unit));

      /* On the first line, within the range.  */
      ASSERT_TRUE (example_b.contains_point (3, 15, col_unit));

      /* On an interior line.
	 The column number should not matter; try various boundary
	 values.  */
      ASSERT_TRUE (example_b.contains_point (4, 1, col_unit));
      ASSERT_TRUE (example_b.contains_point (4, 7, col_unit));
      ASSERT_TRUE (example_b.contains_point (4, 8, col_unit));
      ASSERT_TRUE (example_b.contains_point (4, 9, col_unit));
      ASSERT_TRUE (example_b.contains_point (4, 13, col_unit));
      ASSERT_TRUE (example_b.contains_point (4, 14, col_unit));
      ASSERT_TRUE (example_b.contains_point (4, 15, col_unit));

      /* On the final line, before the end.  */
      ASSERT_TRUE (example_b.contains_point (5, 7, col_unit));

      /* On the final line, at the end.  */
      ASSERT_TRUE (example_b.contains_point (5, 8, col_unit));

      /* On the final line, after the end.  */
      ASSERT_FALSE (example_b.contains_point (5, 9, col_unit));

      /* After the line.  */
      ASSERT_FALSE (example_b.contains_point (6, 1, col_unit));
    }

  /* Tests for layout_range::intersects_line_p.  */
  ASSERT_FALSE (example_b.intersects_line_p (2));
  ASSERT_TRUE (example_b.intersects_line_p (3));
  ASSERT_TRUE (example_b.intersects_line_p (4));
  ASSERT_TRUE (example_b.intersects_line_p (5));
  ASSERT_FALSE (example_b.intersects_line_p (6));
}

#endif /* #if CHECKING_P */

/* Given a source line LINE of length LINE_BYTES bytes, determine the length
   (still in bytes, not display cols) without any trailing whitespace.  */

static int
get_line_bytes_without_trailing_whitespace (const char *line, int line_bytes)
{
  int result = line_bytes;
  while (result > 0)
    {
      char ch = line[result - 1];
      if (ch == ' ' || ch == '\t' || ch == '\r')
	result--;
      else
	break;
    }
  gcc_assert (result >= 0);
  gcc_assert (result <= line_bytes);
  gcc_assert (result == 0 ||
	      (line[result - 1] != ' '
	       && line[result -1] != '\t'
	       && line[result -1] != '\r'));
  return result;
}

#if CHECKING_P

/* A helper function for testing get_line_bytes_without_trailing_whitespace.  */

static void
assert_eq (const char *line, int expected_bytes)
{
  int actual_value
    = get_line_bytes_without_trailing_whitespace (line, strlen (line));
  ASSERT_EQ (actual_value, expected_bytes);
}

/* Verify that get_line_bytes_without_trailing_whitespace is sane for
   various inputs.  It is not required to handle newlines.  */

static void
test_get_line_bytes_without_trailing_whitespace ()
{
  assert_eq ("", 0);
  assert_eq (" ", 0);
  assert_eq ("\t", 0);
  assert_eq ("\r", 0);
  assert_eq ("hello world", 11);
  assert_eq ("hello world     ", 11);
  assert_eq ("hello world     \t\t  ", 11);
  assert_eq ("hello world\r", 11);
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

bool
layout::compatible_locations_p (location_t loc_a, location_t loc_b) const
{
  if (IS_ADHOC_LOC (loc_a))
    loc_a = get_location_from_adhoc_loc (m_line_table, loc_a);
  if (IS_ADHOC_LOC (loc_b))
    loc_b = get_location_from_adhoc_loc (m_line_table, loc_b);

  /* If either location is one of the special locations outside of a
     linemap, they are only compatible if they are equal.  */
  if (loc_a < RESERVED_LOCATION_COUNT
      || loc_b < RESERVED_LOCATION_COUNT)
    return loc_a == loc_b;

  const line_map *map_a = linemap_lookup (m_line_table, loc_a);
  linemap_assert (map_a);

  const line_map *map_b = linemap_lookup (m_line_table, loc_b);
  linemap_assert (map_b);

  /* Are they within the same map?  */
  if (map_a == map_b)
    {
      /* Are both within the same macro expansion?  */
      if (linemap_macro_expansion_map_p (map_a))
	{
	  /* If so, then they're only compatible if either both are
	     from the macro definition, or both from the macro arguments.  */
	  bool loc_a_from_defn
	    = linemap_location_from_macro_definition_p (m_line_table, loc_a);
	  bool loc_b_from_defn
	    = linemap_location_from_macro_definition_p (m_line_table, loc_b);
	  if (loc_a_from_defn != loc_b_from_defn)
	    return false;

	  /* Expand each location towards the spelling location, and
	     recurse.  */
	  const line_map_macro *macro_map = linemap_check_macro (map_a);
	  location_t loc_a_toward_spelling
	    = linemap_macro_map_loc_unwind_toward_spelling (m_line_table,
							    macro_map,
							    loc_a);
	  location_t loc_b_toward_spelling
	    = linemap_macro_map_loc_unwind_toward_spelling (m_line_table,
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

/* Comparator for sorting fix-it hints.  */

static int
fixit_cmp (const void *p_a, const void *p_b)
{
  const fixit_hint * hint_a = *static_cast<const fixit_hint * const *> (p_a);
  const fixit_hint * hint_b = *static_cast<const fixit_hint * const *> (p_b);
  return hint_a->get_start_loc () - hint_b->get_start_loc ();
}

/* Callbacks for use when not escaping the source.  */

/* The default callback for char_column_policy::m_width_cb is cpp_wcwidth.  */

/* Callback for char_display_policy::m_print_cb for printing source chars
   when not escaping the source.  */

static void
default_print_decoded_ch (pretty_printer *pp,
			  const cpp_decoded_char &decoded_ch)
{
  for (const char *ptr = decoded_ch.m_start_byte;
       ptr != decoded_ch.m_next_byte; ptr++)
    {
      if (*ptr == '\0' || *ptr == '\r')
	{
	  pp_space (pp);
	  continue;
	}

      pp_character (pp, *ptr);
    }
}

/* Callbacks for use with DIAGNOSTICS_ESCAPE_FORMAT_BYTES.  */

static const int width_per_escaped_byte = 4;

/* Callback for char_column_policy::m_width_cb for determining the
   display width when escaping with DIAGNOSTICS_ESCAPE_FORMAT_BYTES.  */

static int
escape_as_bytes_width (cppchar_t ch)
{
  if (ch < 0x80 && ISPRINT (ch))
    return cpp_wcwidth (ch);
  else
    {
      if (ch <=   0x7F) return 1 * width_per_escaped_byte;
      if (ch <=  0x7FF) return 2 * width_per_escaped_byte;
      if (ch <= 0xFFFF) return 3 * width_per_escaped_byte;
      return 4 * width_per_escaped_byte;
    }
}

/* Callback for char_display_policy::m_print_cb for printing source chars
   when escaping with DIAGNOSTICS_ESCAPE_FORMAT_BYTES.  */

static void
escape_as_bytes_print (pretty_printer *pp,
		       const cpp_decoded_char &decoded_ch)
{
  if (!decoded_ch.m_valid_ch)
    {
      for (const char *iter = decoded_ch.m_start_byte;
	   iter != decoded_ch.m_next_byte; ++iter)
	{
	  char buf[16];
	  sprintf (buf, "<%02x>", (unsigned char)*iter);
	  pp_string (pp, buf);
	}
      return;
    }

  cppchar_t ch = decoded_ch.m_ch;
  if (ch < 0x80 && ISPRINT (ch))
    pp_character (pp, ch);
  else
    {
      for (const char *iter = decoded_ch.m_start_byte;
	   iter < decoded_ch.m_next_byte; ++iter)
	{
	  char buf[16];
	  sprintf (buf, "<%02x>", (unsigned char)*iter);
	  pp_string (pp, buf);
	}
    }
}

/* Callbacks for use with DIAGNOSTICS_ESCAPE_FORMAT_UNICODE.  */

/* Callback for char_column_policy::m_width_cb for determining the
   display width when escaping with DIAGNOSTICS_ESCAPE_FORMAT_UNICODE.  */

static int
escape_as_unicode_width (cppchar_t ch)
{
  if (ch < 0x80 && ISPRINT (ch))
    return cpp_wcwidth (ch);
  else
    {
      // Width of "<U+%04x>"
      if (ch > 0xfffff)
	return 10;
      else if (ch > 0xffff)
	return 9;
      else
	return 8;
    }
}

/* Callback for char_display_policy::m_print_cb for printing source chars
   when escaping with DIAGNOSTICS_ESCAPE_FORMAT_UNICODE.  */

static void
escape_as_unicode_print (pretty_printer *pp,
			 const cpp_decoded_char &decoded_ch)
{
  if (!decoded_ch.m_valid_ch)
    {
      escape_as_bytes_print (pp, decoded_ch);
      return;
    }

  cppchar_t ch = decoded_ch.m_ch;
  if (ch < 0x80 && ISPRINT (ch))
    pp_character (pp, ch);
  else
    {
      char buf[16];
      sprintf (buf, "<U+%04X>", ch);
      pp_string (pp, buf);
    }
}

/* Populate a char_display_policy based on SOURCE_POLICY and RICHLOC.  */

static char_display_policy
make_char_policy (const diagnostic_source_print_policy &source_policy,
		  const rich_location &richloc)
{
  /* The default is to not escape non-ASCII bytes.  */
  char_display_policy result
    (source_policy.get_column_policy ().get_tabstop (),
     cpp_wcwidth,
     default_print_decoded_ch);

  /* If the diagnostic suggests escaping non-ASCII bytes, then
     use policy from user-supplied options.  */
  if (richloc.escape_on_output_p ())
    {
      result.m_undecoded_byte_width = width_per_escaped_byte;
      switch (source_policy.get_escape_format ())
	{
	default:
	  gcc_unreachable ();
	case DIAGNOSTICS_ESCAPE_FORMAT_UNICODE:
	  result.m_width_cb = escape_as_unicode_width;
	  result.m_print_cb = escape_as_unicode_print;
	  break;
	case DIAGNOSTICS_ESCAPE_FORMAT_BYTES:
	  result.m_width_cb = escape_as_bytes_width;
	  result.m_print_cb = escape_as_bytes_print;
	  break;
	}
    }

  return result;
}

/* Implementation of class layout.  */

/* Constructor for class layout.

   Filter the ranges from the rich_location to those that we can
   sanely print, populating m_layout_ranges and m_fixit_hints.
   Determine the range of lines that we will print, splitting them
   up into an ordered list of disjoint spans of contiguous line numbers.
   Determine m_x_offset_display, to ensure that the primary caret
   will fit within the max_width provided by the diagnostic_context.  */

layout::layout (const diagnostic_source_print_policy &source_policy,
		const rich_location &richloc,
		diagnostic_source_effect_info *effect_info)
: m_options (source_policy.get_options ()),
  m_line_table (richloc.get_line_table ()),
  m_file_cache (source_policy.get_file_cache ()),
  /* Ensure we have a non-null m_theme. */
  m_theme (source_policy.get_diagram_theme ()
	   ? *source_policy.get_diagram_theme ()
	   : *static_cast <const text_art::theme *> (&m_fallback_theme)),
  m_effect_info (effect_info),
  m_char_policy (make_char_policy (source_policy, richloc)),
  m_primary_loc (richloc.get_range (0)->m_loc),
  m_exploc (m_file_cache,
	    richloc.get_expanded_location (0), m_char_policy,
	    LOCATION_ASPECT_CARET),
  m_layout_ranges (richloc.get_num_locations ()),
  m_fixit_hints (richloc.get_num_fixit_hints ()),
  m_line_spans (1 + richloc.get_num_locations ()),
  m_linenum_width (0),
  m_x_offset_display (0),
  m_escape_on_output (richloc.escape_on_output_p ())
{
  for (unsigned int idx = 0; idx < richloc.get_num_locations (); idx++)
    {
      /* This diagnostic printer can only cope with "sufficiently sane" ranges.
	 Ignore any ranges that are awkward to handle.  */
      const location_range *loc_range = richloc.get_range (idx);
      maybe_add_location_range (loc_range, idx, false);
    }

  /* Populate m_fixit_hints, filtering to only those that are in the
     same file.  */
  for (unsigned int i = 0; i < richloc.get_num_fixit_hints (); i++)
    {
      const fixit_hint *hint = richloc.get_fixit_hint (i);
      if (validate_fixit_hint_p (hint))
	m_fixit_hints.safe_push (hint);
    }

  /* Sort m_fixit_hints.  */
  m_fixit_hints.qsort (fixit_cmp);

  /* Populate the indicated members.  */
  calculate_line_spans ();
  calculate_linenum_width ();
  calculate_x_offset_display ();
}


/* Attempt to add LOC_RANGE to m_layout_ranges, filtering them to
   those that we can sanely print.

   ORIGINAL_IDX is the index of LOC_RANGE within its rich_location,
   (for use as extrinsic state by label ranges).

   If RESTRICT_TO_CURRENT_LINE_SPANS is true, then LOC_RANGE is also
   filtered against this layout instance's current line spans: it
   will only be added if the location is fully within the lines
   already specified by other locations.

   Return true iff LOC_RANGE was added.  */

bool
layout::maybe_add_location_range (const location_range *loc_range,
				  unsigned original_idx,
				  bool restrict_to_current_line_spans)
{
  gcc_assert (loc_range);

  /* Split the "range" into caret and range information.  */
  source_range src_range = get_range_from_loc (m_line_table, loc_range->m_loc);

  /* Expand the various locations.  */
  expanded_location start
    = linemap_client_expand_location_to_spelling_point
    (m_line_table, src_range.m_start, LOCATION_ASPECT_START);
  expanded_location finish
    = linemap_client_expand_location_to_spelling_point
    (m_line_table, src_range.m_finish, LOCATION_ASPECT_FINISH);
  expanded_location caret
    = linemap_client_expand_location_to_spelling_point
    (m_line_table, loc_range->m_loc, LOCATION_ASPECT_CARET);

  /* If any part of the range isn't in the same file as the primary
     location of this diagnostic, ignore the range.  */
  if (start.file != m_exploc.file)
    return false;
  if (finish.file != m_exploc.file)
    return false;
  if (loc_range->m_range_display_kind == SHOW_RANGE_WITH_CARET)
    if (caret.file != m_exploc.file)
      return false;

  /* Sanitize the caret location for non-primary ranges.  */
  if (m_layout_ranges.length () > 0)
    if (loc_range->m_range_display_kind == SHOW_RANGE_WITH_CARET)
      if (!compatible_locations_p (loc_range->m_loc, m_primary_loc))
	/* Discard any non-primary ranges that can't be printed
	   sanely relative to the primary location.  */
	return false;

  /* If there's no column information, then don't try to print
     annotation lines for this range.  */
  enum range_display_kind range_display_kind
    = loc_range->m_range_display_kind;
  if (start.column == 0
      || finish.column == 0
      || caret.column == 0)
    range_display_kind = SHOW_LINES_WITHOUT_RANGE;

  /* Everything is now known to be in the correct source file,
     but it may require further sanitization.  */
  layout_range ri (exploc_with_display_col (m_file_cache,
					    start, m_char_policy,
					    LOCATION_ASPECT_START),
		   exploc_with_display_col (m_file_cache,
					    finish, m_char_policy,
					    LOCATION_ASPECT_FINISH),
		   range_display_kind,
		   exploc_with_display_col (m_file_cache,
					    caret, m_char_policy,
					    LOCATION_ASPECT_CARET),
		   original_idx, loc_range->m_label);

  /* If we have a range that finishes before it starts (perhaps
     from something built via macro expansion), printing the
     range is likely to be nonsensical.  Also, attempting to do so
     breaks assumptions within the printing code  (PR c/68473).
     Similarly, don't attempt to print ranges if one or both ends
     of the range aren't sane to print relative to the
     primary location (PR c++/70105).  */
  if (start.line > finish.line
      || !compatible_locations_p (src_range.m_start, m_primary_loc)
      || !compatible_locations_p (src_range.m_finish, m_primary_loc))
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
	return false;
    }

  /* Potentially filter to just the lines already specified by other
     locations.  This is for use by gcc_rich_location::add_location_if_nearby.
     The layout ctor doesn't use it, and can't because m_line_spans
     hasn't been set up at that point.  */
  if (restrict_to_current_line_spans)
    {
      if (!will_show_line_p (start.line))
	return false;
      if (!will_show_line_p (finish.line))
	return false;
      if (loc_range->m_range_display_kind == SHOW_RANGE_WITH_CARET)
	if (!will_show_line_p (caret.line))
	  return false;
    }

  /* Passed all the tests; add the range to m_layout_ranges so that
     it will be printed.  */
  m_layout_ranges.safe_push (ri);
  return true;
}

/* Return true iff ROW is within one of the line spans for this layout.  */

bool
layout::will_show_line_p (linenum_type row) const
{
  for (int line_span_idx = 0; line_span_idx < get_num_line_spans ();
       line_span_idx++)
    {
      const line_span *line_span = get_line_span (line_span_idx);
      if (line_span->contains_line_p (row))
	return true;
    }
  return false;
}

/* Print a line showing a gap in the line numbers, for showing the boundary
   between two line spans.  */

void
layout_printer::print_gap_in_line_numbering ()
{
  gcc_assert (m_layout.m_options.show_line_numbers_p);

  pp_emit_prefix (&m_pp);

  for (int i = 0; i < m_layout.get_linenum_width () + 1; i++)
    pp_character (&m_pp, '.');

  pp_newline (&m_pp);
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
	  exploc.column = lr->m_start.m_columns[CU_BYTES];
	  return exploc;
	}
    }

  /* Otherwise, use the location of the first fixit-hint present within
     the line_span.  */
  for (unsigned int i = 0; i < m_fixit_hints.length (); i++)
    {
      const fixit_hint *hint = m_fixit_hints[i];
      location_t loc = hint->get_start_loc ();
      expanded_location exploc = expand_location (loc);
      if (line_span->contains_line_p (exploc.line))
	return exploc;
    }

  /* It should not be possible to have a line span that didn't
     contain any of the layout_range or fixit_hint instances.  */
  gcc_unreachable ();
  return m_exploc;
}

/* Determine if HINT is meaningful to print within this layout.  */

bool
layout::validate_fixit_hint_p (const fixit_hint *hint)
{
  if (LOCATION_FILE (hint->get_start_loc ()) != m_exploc.file)
    return false;
  if (LOCATION_FILE (hint->get_next_loc ()) != m_exploc.file)
    return false;

  return true;
}

/* Determine the range of lines affected by HINT.
   This assumes that HINT has already been filtered by
   validate_fixit_hint_p, and so affects the correct source file.  */

static line_span
get_line_span_for_fixit_hint (const fixit_hint *hint)
{
  gcc_assert (hint);

  int start_line = LOCATION_LINE (hint->get_start_loc ());

  /* For line-insertion fix-it hints, add the previous line to the
     span, to give the user more context on the proposed change.  */
  if (hint->ends_with_newline_p ())
    if (start_line > 1)
      start_line--;

  return line_span (start_line,
		    LOCATION_LINE (hint->get_next_loc ()));
}

/* We want to print the pertinent source code at a diagnostic.  The
   rich_location can contain multiple locations.  This will have been
   filtered into m_exploc (the caret for the primary location) and
   m_layout_ranges, for those ranges within the same source file.

   We will print a subset of the lines within the source file in question,
   as a collection of "spans" of lines.

   This function populates m_line_spans with an ordered, disjoint list of
   the line spans of interest.

   Printing a gap between line spans takes one line, so, when printing
   line numbers, we allow a gap of up to one line between spans when
   merging, since it makes more sense to print the source line rather than a
   "gap-in-line-numbering" line.  When not printing line numbers, it's
   better to be more explicit about what's going on, so keeping them as
   separate spans is preferred.

   For example, if the primary range is on lines 8-10, with secondary ranges
   covering lines 5-6 and lines 13-15:

     004
     005                   |RANGE 1
     006                   |RANGE 1
     007
     008  |PRIMARY RANGE
     009  |PRIMARY CARET
     010  |PRIMARY RANGE
     011
     012
     013                                |RANGE 2
     014                                |RANGE 2
     015                                |RANGE 2
     016

   With line numbering on, we want two spans: lines 5-10 and lines 13-15.

   With line numbering off (with span headers), we want three spans: lines 5-6,
   lines 8-10, and lines 13-15.  */

void
layout::calculate_line_spans ()
{
  /* This should only be called once, by the ctor.  */
  gcc_assert (m_line_spans.length () == 0);

  /* Populate tmp_spans with individual spans, for each of
     m_exploc, and for m_layout_ranges.  */
  auto_vec<line_span> tmp_spans (1 + m_layout_ranges.length ());
  tmp_spans.safe_push (line_span (m_exploc.line, m_exploc.line));
  for (unsigned int i = 0; i < m_layout_ranges.length (); i++)
    {
      const layout_range *lr = &m_layout_ranges[i];
      gcc_assert (lr->m_start.m_line <= lr->m_finish.m_line);
      tmp_spans.safe_push (line_span (lr->m_start.m_line,
				      lr->m_finish.m_line));
    }

  /* Also add spans for any fix-it hints, in case they cover other lines.  */
  for (unsigned int i = 0; i < m_fixit_hints.length (); i++)
    {
      const fixit_hint *hint = m_fixit_hints[i];
      gcc_assert (hint);
      tmp_spans.safe_push (get_line_span_for_fixit_hint (hint));
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
      const int merger_distance = m_options.show_line_numbers_p ? 1 : 0;
      if ((linenum_arith_t)next->m_first_line
	  <= (linenum_arith_t)current->m_last_line + 1 + merger_distance)
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

/* Determine how many display columns need to be reserved for line numbers,
   based on the largest line number that will be needed, and populate
   m_linenum_width.  */

void
layout::calculate_linenum_width ()
{
  gcc_assert (m_line_spans.length () > 0);
  const line_span *last_span = &m_line_spans[m_line_spans.length () - 1];
  int highest_line = last_span->m_last_line;
  if (highest_line < 0)
    highest_line = 0;
  m_linenum_width = num_digits (highest_line);
  /* If we're showing jumps in the line-numbering, allow at least 3 chars.  */
  if (m_line_spans.length () > 1)
    m_linenum_width = MAX (m_linenum_width, 3);
  /* If there's a minimum margin width, apply it (subtracting 1 for the space
     after the line number.  */
  m_linenum_width = MAX (m_linenum_width, m_options.min_margin_width - 1);
}

/* Calculate m_x_offset_display, which improves readability in case the source
   line of interest is longer than the user's display.  All lines output will be
   shifted to the left (so that their beginning is no longer displayed) by
   m_x_offset_display display columns, so that the caret is in a reasonable
   location.  */

void
layout::calculate_x_offset_display ()
{
  m_x_offset_display = 0;

  const int max_width = m_options.max_width;
  if (!max_width)
    {
      /* Nothing to do, the width is not capped.  */
      return;
    }

  const char_span line = m_file_cache.get_source_line (m_exploc.file,
						       m_exploc.line);
  if (!line)
    {
      /* Nothing to do, we couldn't find the source line.  */
      return;
    }
  int caret_display_column = m_exploc.m_display_col;
  const int line_bytes
    = get_line_bytes_without_trailing_whitespace (line.get_buffer (),
						  line.length ());
  int eol_display_column
    = cpp_display_width (line.get_buffer (), line_bytes, m_char_policy);
  if (caret_display_column > eol_display_column
      || !caret_display_column)
    {
      /* This does not make sense, so don't try to do anything in this case.  */
      return;
    }

  /* Adjust caret and eol positions to include the left margin.  If we are
     outputting line numbers, then the left margin is equal to m_linenum_width
     plus three for the " | " which follows it.  Otherwise the left margin width
     is equal to 1, because layout::print_source_line() will prefix each line
     with a space.  */
  const int source_display_cols = eol_display_column;
  int left_margin_size = 1;
  if (m_options.show_line_numbers_p)
      left_margin_size = m_linenum_width + 3;
  caret_display_column += left_margin_size;
  eol_display_column += left_margin_size;

  if (eol_display_column <= max_width)
    {
      /* Nothing to do, everything fits in the display.  */
      return;
    }

  /* The line is too long for the display.  Calculate an offset such that the
     caret is not too close to the right edge of the screen.  It will be
     CARET_LINE_MARGIN display columns from the right edge, unless it is closer
     than that to the end of the source line anyway.  */
  int right_margin_size = CARET_LINE_MARGIN;
  right_margin_size = MIN (eol_display_column - caret_display_column,
			   right_margin_size);
  if (right_margin_size + left_margin_size >= max_width)
    {
      /* The max_width is very small, so anything we try to do will not be very
	 effective; just punt in this case and output with no offset.  */
      return;
    }
  const int max_caret_display_column = max_width - right_margin_size;
  if (caret_display_column > max_caret_display_column)
    {
      m_x_offset_display = caret_display_column - max_caret_display_column;
      /* Make sure we don't offset the line into oblivion.  */
      static const int min_cols_visible = 2;
      if (source_display_cols - m_x_offset_display < min_cols_visible)
	m_x_offset_display = 0;
    }
}

/* Print line ROW of source code, potentially colorized at any ranges, and
   return the line bounds.  LINE is the source line (not necessarily
   0-terminated) and LINE_BYTES is its length in bytes.  In order to handle both
   colorization and tab expansion, this function tracks the line position in
   both byte and display column units.  */

line_bounds
layout_printer::print_source_line (linenum_type row, const char *line, int line_bytes)
{
  m_colorizer.set_normal_text ();

  pp_emit_prefix (&m_pp);
  if (m_layout.m_options.show_line_numbers_p)
    {
      int width = num_digits (row);
      for (int i = 0; i < m_layout.get_linenum_width () - width; i++)
	pp_space (&m_pp);
      pp_printf (&m_pp, "%i |", row);
    }

  print_leftmost_column ();

  /* We will stop printing the source line at any trailing whitespace.  */
  line_bytes = get_line_bytes_without_trailing_whitespace (line,
							   line_bytes);

  /* This object helps to keep track of which display column we are at, which is
     necessary for computing the line bounds in display units, for doing
     tab expansion, and for implementing m_x_offset_display.  */
  cpp_display_width_computation dw (line, line_bytes, m_layout.m_char_policy);

  /* Skip the first m_x_offset_display display columns.  In case the leading
     portion that will be skipped ends with a character with wcwidth > 1, then
     it is possible we skipped too much, so account for that by padding with
     spaces.  Note that this does the right thing too in case a tab was the last
     character to be skipped over; the tab is effectively replaced by the
     correct number of trailing spaces needed to offset by the desired number of
     display columns.  */
  for (int skipped_display_cols
	 = dw.advance_display_cols (m_layout.m_x_offset_display);
       skipped_display_cols > m_layout.m_x_offset_display; --skipped_display_cols)
    pp_space (&m_pp);

  /* Print the line and compute the line_bounds.  */
  line_bounds lbounds;
  while (!dw.done ())
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
      if (m_layout.m_options.colorize_source_p)
	{
	  bool in_range_p;
	  point_state state;
	  const int start_byte_col = dw.bytes_processed () + 1;
	  in_range_p = m_layout.get_state_at_point (row, start_byte_col,
						    0, INT_MAX,
						    CU_BYTES,
						    &state);
	  if (in_range_p)
	    m_colorizer.set_range (state.range_idx);
	  else
	    m_colorizer.set_normal_text ();
	}

      /* Get the display width of the next character to be output, expanding
	 tabs and replacing some control bytes with spaces as necessary.  */
      const char *c = dw.next_byte ();
      const int start_disp_col = dw.display_cols_processed () + 1;
      cpp_decoded_char cp;
      const int this_display_width = dw.process_next_codepoint (&cp);
      if (*c == '\t')
	{
	  /* The returned display width is the number of spaces into which the
	     tab should be expanded.  */
	  for (int i = 0; i != this_display_width; ++i)
	    pp_space (&m_pp);
	  continue;
	}

      /* We have a (possibly multibyte) character to output; update the line
	 bounds if it is not whitespace.  */
      if (*c != ' ')
	{
	  lbounds.m_last_non_ws_disp_col = dw.display_cols_processed ();
	  if (lbounds.m_first_non_ws_disp_col == INT_MAX)
	    lbounds.m_first_non_ws_disp_col = start_disp_col;
	}

      /* Output the character.  */
      m_layout.m_char_policy.m_print_cb (&m_pp, cp);
      c = dw.next_byte ();
    }
  print_newline ();
  return lbounds;
}

/* Determine if we should print an annotation line for ROW.
   i.e. if any of m_layout_ranges contains ROW.  */

bool
layout::should_print_annotation_line_p (linenum_type row) const
{
  layout_range *range;
  int i;
  FOR_EACH_VEC_ELT (m_layout_ranges, i, range)
    {
      if (range->m_range_display_kind == SHOW_LINES_WITHOUT_RANGE)
	return false;
      if (range->intersects_line_p (row))
	return true;
    }
  return false;
}

/* Print the leftmost column after the margin, which is used for showing
   links between labels (e.g. for CFG edges in execution paths).  */

void
layout_printer::print_leftmost_column ()
{
  if (!get_options ().show_event_links_p)
    gcc_assert (m_link_lhs_state == link_lhs_state::none);

  switch (m_link_lhs_state)
    {
    default:
      gcc_unreachable ();
    case link_lhs_state::none:
      pp_space (&m_pp);
      break;
    case link_lhs_state::rewinding_to_lhs:
      {
	m_colorizer.set_cfg_edge ();
	const cppchar_t ch = get_theme ().get_cppchar
	  (text_art::theme::cell_kind::CFG_FROM_LEFT_TO_DOWN);
	pp_unicode_character (&m_pp, ch);
	m_colorizer.set_normal_text ();
      }
      break;
    case link_lhs_state::at_lhs:
      {
	m_colorizer.set_cfg_edge ();
	const cppchar_t ch = get_theme ().get_cppchar
	  (text_art::theme::cell_kind::CFG_DOWN);
	pp_unicode_character (&m_pp, ch);
	m_colorizer.set_normal_text ();
      }
      break;
    case link_lhs_state::indenting_to_dest:
      {
	m_colorizer.set_cfg_edge ();
	const cppchar_t ch = get_theme ().get_cppchar
	  (text_art::theme::cell_kind::CFG_FROM_DOWN_TO_RIGHT);
	pp_unicode_character (&m_pp, ch);
	m_colorizer.set_normal_text ();
      }
      break;
    }
}

/* Begin an annotation line.  If m_show_line_numbers_p, print the left
   margin, which is empty for annotation lines.
   After any left margin, print a leftmost column, which is used for
   showing links between labels (e.g. for CFG edges in execution paths).  */

void
layout_printer::start_annotation_line (char margin_char)
{
  pp_emit_prefix (&m_pp);
  if (get_options ().show_line_numbers_p)
    {
      /* Print the margin.  If MARGIN_CHAR != ' ', then print up to 3
	 of it, right-aligned, padded with spaces.  */
      int i;
      for (i = 0; i < m_layout.m_linenum_width - 3; i++)
	pp_space (&m_pp);
      for (; i < m_layout.m_linenum_width; i++)
	pp_character (&m_pp, margin_char);
      pp_string (&m_pp, " |");
    }
  if (margin_char == ' ')
    print_leftmost_column ();
  else
    pp_character (&m_pp, margin_char);
}

/* Print a line consisting of the caret/underlines for the given
   source line.  */

void
layout_printer::print_annotation_line (linenum_type row,
				       const line_bounds lbounds)
{
  int x_bound = m_layout.get_x_bound_for_row (row,
					      m_layout.m_exploc.m_display_col,
					      lbounds.m_last_non_ws_disp_col);

  start_annotation_line ();

  for (int column = 1 + m_layout.m_x_offset_display; column < x_bound; column++)
    {
      bool in_range_p;
      point_state state;
      in_range_p = m_layout.get_state_at_point (row, column,
						lbounds.m_first_non_ws_disp_col,
						lbounds.m_last_non_ws_disp_col,
						CU_DISPLAY_COLS,
						&state);
      if (in_range_p)
	{
	  /* Within a range.  Draw either the caret or an underline.  */
	  m_colorizer.set_range (state.range_idx);
	  if (state.draw_caret_p)
	    {
	      /* Draw the caret.  */
	      char caret_char;
	      if (state.range_idx < rich_location::STATICALLY_ALLOCATED_RANGES)
		caret_char = get_options ().caret_chars[state.range_idx];
	      else
		caret_char = '^';
	      pp_character (&m_pp, caret_char);
	    }
	  else
	    pp_character (&m_pp, '~');
	}
      else
	{
	  /* Not in a range.  */
	  m_colorizer.set_normal_text ();
	  pp_character (&m_pp, ' ');
	}
    }
  print_newline ();
}

/* A version of label_text that can live inside a vec.
   Requires manual cleanup via maybe_free.  */

struct pod_label_text
{
  pod_label_text ()
  : m_buffer (NULL), m_caller_owned (false)
  {}

  pod_label_text (label_text &&other)
  : m_buffer (const_cast<char*> (other.get ())),
    m_caller_owned (other.is_owner ())
  {
    other.release ();
  }

  void maybe_free ()
  {
    if (m_caller_owned)
      free (m_buffer);
  }

  char *m_buffer;
  bool m_caller_owned;
};

/* Implementation detail of layout::print_any_labels.

   A label within the given row of source.  */

class line_label
{
public:
  line_label (int state_idx, int column,
	      label_text text,
	      bool has_in_edge,
	      bool has_out_edge)
  : m_state_idx (state_idx), m_column (column),
    m_text (std::move (text)), m_label_line (0), m_has_vbar (true),
    m_has_in_edge (has_in_edge),
    m_has_out_edge (has_out_edge)
  {
    /* Using styled_string rather than cpp_display_width here
       lets us skip SGR formatting characters for color and URLs.
       It doesn't handle tabs and unicode escaping, but we don't
       expect to see either of those in labels.  */
    text_art::style_manager sm;
    text_art::styled_string str (sm, m_text.m_buffer);
    m_display_width = str.calc_canvas_width ();
  }

  /* Sorting is primarily by column, then by state index.  */
  static int comparator (const void *p1, const void *p2)
  {
    const line_label *ll1 = (const line_label *)p1;
    const line_label *ll2 = (const line_label *)p2;
    int column_cmp = compare (ll1->m_column, ll2->m_column);
    if (column_cmp)
      return column_cmp;
    /* Order by reverse state index, so that labels are printed
       in order of insertion into the rich_location when the
       sorted list is walked backwards.  */
    return -compare (ll1->m_state_idx, ll2->m_state_idx);
  }

  int m_state_idx;
  int m_column;
  pod_label_text m_text;
  size_t m_display_width;
  int m_label_line;
  bool m_has_vbar;
  bool m_has_in_edge;
  bool m_has_out_edge;
};

/* Print any labels in this row.  */
void
layout_printer::print_any_labels (linenum_type row)
{
  int i;
  auto_vec<line_label> labels;

  /* Gather the labels that are to be printed into "labels".  */
  {
    layout_range *range;
    FOR_EACH_VEC_ELT (m_layout.m_layout_ranges, i, range)
      {
	/* Most ranges don't have labels, so reject this first.  */
	if (range->m_label == NULL)
	  continue;

	/* The range's caret must be on this line.  */
	if (range->m_caret.m_line != row)
	  continue;

	/* Reject labels that aren't fully visible due to clipping
	   by m_x_offset_display.  */
	const int disp_col = range->m_caret.m_columns[CU_DISPLAY_COLS];
	if (disp_col <= m_layout.m_x_offset_display)
	  continue;

	label_text text;
	text = range->m_label->get_text (range->m_original_idx);

	/* Allow for labels that return NULL from their get_text
	   implementation (so e.g. such labels can control their own
	   visibility).  */
	if (text.get () == NULL)
	  continue;

	labels.safe_push (line_label (i, disp_col, std::move (text),
				      range->has_in_edge (),
				      range->has_out_edge ()));
      }
  }

  /* Bail out if there are no labels on this row.  */
  if (labels.length () == 0)
    return;

  /* Sort them.  */
  labels.qsort(line_label::comparator);

  /* Figure out how many "label lines" we need, and which
     one each label is printed in.

     For example, if the labels aren't too densely packed,
     we can fit them on the same line, giving two "label lines":

       foo + bar
       ~~~   ~~~
       |     |        : label line 0
       l0    l1       : label line 1

     If they would touch each other or overlap, then we need
     additional "label lines":

       foo + bar
       ~~~   ~~~
       |     |             : label line 0
       |     label 1       : label line 1
       label 0             : label line 2

     Place the final label on label line 1, and work backwards, adding
     label lines as needed.

     If multiple labels are at the same place, put them on separate
     label lines:

       foo + bar
           ^               : label line 0
           |               : label line 1
           label 0         : label line 2
           label 1         : label line 3.  */

  int max_label_line = 1;
  int label_line_with_in_edge = -1;
  {
    int next_column = INT_MAX;
    line_label *label;
    FOR_EACH_VEC_ELT_REVERSE (labels, i, label)
      {
	/* Would this label "touch" or overlap the next label?  */
	if (label->m_column + label->m_display_width >= (size_t)next_column)
	  {
	    max_label_line++;

	    /* If we've already seen labels with the same column, suppress the
	       vertical bar for subsequent ones in this backwards iteration;
	       hence only the one with the highest label_line has m_has_vbar set.  */
	    if (label->m_column == next_column)
	      label->m_has_vbar = false;
	  }

	label->m_label_line = max_label_line;
	if (get_options ().show_event_links_p)
	  if (label->m_has_in_edge)
	    label_line_with_in_edge = max_label_line;
	next_column = label->m_column;
      }
  }

  gcc_assert (labels.length () > 0);

  /* Print the "label lines".  For each label within the line, print
     either a vertical bar ('|') for the labels that are lower down, or the
     labels themselves once we've reached their line.  */
  {
    for (int label_line = 0; label_line <= max_label_line; label_line++)
      {
	if (label_line == label_line_with_in_edge)
	  {
	    gcc_assert (get_options ().show_event_links_p);
	    m_link_lhs_state = link_lhs_state::indenting_to_dest;
	  }
	start_annotation_line ();

	int column = 1 + m_layout.m_x_offset_display;
	line_label *label;
	FOR_EACH_VEC_ELT (labels, i, label)
	  {
	    if (label_line > label->m_label_line)
	      /* We've printed all the labels for this label line.  */
	      break;

	    if (label_line == label->m_label_line)
	      {
		gcc_assert (column <= label->m_column);

		if (label_line == label_line_with_in_edge)
		  {
		    /* Print a prefix showing an incoming
		       link from another label.
		       .|+----------->(10) ...to here
		       . ^~~~~~~~~~~~~
		       . this text.  */
		    gcc_assert (get_options ().show_event_links_p);
		    m_colorizer.set_cfg_edge ();
		    const cppchar_t right= get_theme ().get_cppchar
		      (text_art::theme::cell_kind::CFG_RIGHT);
		    while (column < label->m_column - 1)
		      {
			pp_unicode_character (&m_pp, right);
			column++;
		      }
		    if (column == label->m_column - 1)
		      {
			pp_character (&m_pp, '>');
			column++;
		      }
		    m_colorizer.set_normal_text ();
		    m_link_lhs_state = link_lhs_state::none;
		    label_line_with_in_edge = -1;
		  }
		else
		  move_to_column (&column, label->m_column, true);
		gcc_assert (column == label->m_column);
		/* Colorize the text, unless it's for events in a
		   diagnostic_path.  */
		if (!m_is_diagnostic_path)
		  m_colorizer.set_range (label->m_state_idx);
		pp_string (&m_pp, label->m_text.m_buffer);
		m_colorizer.set_normal_text ();
		column += label->m_display_width;
		if (get_options ().show_event_links_p && label->m_has_out_edge)
		  {
		    /* Print a suffix showing the start of a linkage
		       to another label e.g. " ->-+" which will be the
		       first part of e.g.
		       .      (9) following ‘false’ branch... ->-+ <- HERE
		       .                                         |
		       .                                         |
		       .  */
		    const cppchar_t right= get_theme ().get_cppchar
		      (text_art::theme::cell_kind::CFG_RIGHT);
		    const cppchar_t from_right_to_down= get_theme ().get_cppchar
		      (text_art::theme::cell_kind::CFG_FROM_RIGHT_TO_DOWN);
		    m_colorizer.set_cfg_edge ();
		    pp_space (&m_pp);
		    pp_unicode_character (&m_pp, right);
		    pp_unicode_character (&m_pp, '>');
		    pp_unicode_character (&m_pp, right);
		    pp_unicode_character (&m_pp, from_right_to_down);
		    m_colorizer.set_normal_text ();
		    column += 5;
		    m_link_rhs_column = column - 1;
		  }
	      }
	    else if (label->m_has_vbar)
	      {
		gcc_assert (column <= label->m_column);
		move_to_column (&column, label->m_column, true);
		m_colorizer.set_range (label->m_state_idx);
		pp_character (&m_pp, '|');
		m_colorizer.set_normal_text ();
		column++;
	      }
	  }

	/* If we have a vertical link line on the RHS, print the
	   '|' on this annotation line after the labels.  */
	if (m_link_rhs_column != -1 && column < m_link_rhs_column)
	  {
	    move_to_column (&column, m_link_rhs_column, true);
	    m_colorizer.set_cfg_edge ();
	    const cppchar_t down= get_theme ().get_cppchar
	      (text_art::theme::cell_kind::CFG_DOWN);
	    pp_unicode_character (&m_pp, down);
	    m_colorizer.set_normal_text ();
	  }

	print_newline ();
      }
    }

  /* If we have a vertical link line on the RHS, print a trailing
     annotation line showing the vertical line.  */
  if (m_link_rhs_column != -1)
    {
      int column = 1 + m_layout.m_x_offset_display;
      start_annotation_line ();
      move_to_column (&column, m_link_rhs_column, true);
      m_colorizer.set_cfg_edge ();
      const cppchar_t down= get_theme ().get_cppchar
	(text_art::theme::cell_kind::CFG_DOWN);
      pp_unicode_character (&m_pp, down);
      m_colorizer.set_normal_text ();
      print_newline ();
    }

  /* Clean up.  */
  {
    line_label *label;
    FOR_EACH_VEC_ELT (labels, i, label)
      label->m_text.maybe_free ();
  }
}

/* If there are any fixit hints inserting new lines before source line ROW,
   print them.

   They are printed on lines of their own, before the source line
   itself, with a leading '+'.  */

void
layout_printer::print_leading_fixits (linenum_type row)
{
  for (unsigned int i = 0; i < m_layout.m_fixit_hints.length (); i++)
    {
      const fixit_hint *hint = m_layout.m_fixit_hints[i];

      if (!hint->ends_with_newline_p ())
	/* Not a newline fixit; print it in print_trailing_fixits.  */
	continue;

      gcc_assert (hint->insertion_p ());

      if (hint->affects_line_p (m_layout.m_line_table,
				m_layout.m_exploc.file,
				row))
	{
	  /* Printing the '+' with normal colorization
	     and the inserted line with "insert" colorization
	     helps them stand out from each other, and from
	     the surrounding text.  */
	  m_colorizer.set_normal_text ();
	  start_annotation_line ('+');
	  m_colorizer.set_fixit_insert ();
	  /* Print all but the trailing newline of the fix-it hint.
	     We have to print the newline separately to avoid
	     getting additional pp prefixes printed.  */
	  for (size_t i = 0; i < hint->get_length () - 1; i++)
	    pp_character (&m_pp, hint->get_string ()[i]);
	  m_colorizer.set_normal_text ();
	  pp_newline (&m_pp);
	}
    }
}

/* Subroutine of layout::print_trailing_fixits.

   Determine if the annotation line printed for LINE contained
   the exact range from START_COLUMN to FINISH_COLUMN (in display units).  */

bool
layout::annotation_line_showed_range_p (linenum_type line, int start_column,
					int finish_column) const
{
  layout_range *range;
  int i;
  FOR_EACH_VEC_ELT (m_layout_ranges, i, range)
    if (range->m_start.m_line == line
	&& range->m_start.m_columns[CU_DISPLAY_COLS] == start_column
	&& range->m_finish.m_line == line
	&& range->m_finish.m_columns[CU_DISPLAY_COLS] == finish_column)
      return true;
  return false;
}

/* Classes for printing trailing fix-it hints i.e. those that
   don't add new lines.

   For insertion, these can look like:

     new_text

   For replacement, these can look like:

     ------------- : underline showing affected range
     new_text

   For deletion, these can look like:

     ------------- : underline showing affected range

   This can become confusing if they overlap, and so we need
   to do some preprocessing to decide what to print.
   We use the list of fixit_hint instances affecting the line
   to build a list of "correction" instances, and print the
   latter.

   For example, consider a set of fix-its for converting
   a C-style cast to a C++ const_cast.

   Given:

   ..000000000111111111122222222223333333333.
   ..123456789012345678901234567890123456789.
     foo *f = (foo *)ptr->field;
                          ^~~~~

   and the fix-it hints:
     - replace col 10 (the open paren) with "const_cast<"
     - replace col 16 (the close paren) with "> ("
     - insert ")" before col 27

   then we would get odd-looking output:

     foo *f = (foo *)ptr->field;
                          ^~~~~
              -
              const_cast<
                    -
                    > (        )

   It would be better to detect when fixit hints are going to
   overlap (those that require new lines), and to consolidate
   the printing of such fixits, giving something like:

     foo *f = (foo *)ptr->field;
                          ^~~~~
              -----------------
              const_cast<foo *> (ptr->field)

   This works by detecting when the printing would overlap, and
   effectively injecting no-op replace hints into the gaps between
   such fix-its, so that the printing joins up.

   In the above example, the overlap of:
     - replace col 10 (the open paren) with "const_cast<"
   and:
     - replace col 16 (the close paren) with "> ("
   is fixed by injecting a no-op:
     - replace cols 11-15 with themselves ("foo *")
   and consolidating these, making:
     - replace cols 10-16 with "const_cast<" + "foo *" + "> ("
   i.e.:
     - replace cols 10-16 with "const_cast<foo *> ("

   This overlaps with the final fix-it hint:
     - insert ")" before col 27
   and so we repeat the consolidation process, by injecting
   a no-op:
     - replace cols 17-26 with themselves ("ptr->field")
   giving:
     - replace cols 10-26 with "const_cast<foo *> (" + "ptr->field" + ")"
   i.e.:
     - replace cols 10-26 with "const_cast<foo *> (ptr->field)"

   and is thus printed as desired.  */

/* A range of (byte or display) columns within a line.  */

class column_range
{
public:
  column_range (int start_, int finish_) : start (start_), finish (finish_)
  {
    gcc_assert (valid_p (start, finish));
  }

  bool operator== (const column_range &other) const
  {
    return start == other.start && finish == other.finish;
  }

  static bool valid_p (int start, int finish)
  {
    /* We must have either a range, or an insertion.  */
    return (start <= finish || finish == start - 1);
  }

  int start;
  int finish;
};

/* Get the range of bytes or display columns that HINT would affect.  */
static column_range
get_affected_range (file_cache &fc,
		    const cpp_char_column_policy &policy,
		    const fixit_hint *hint, enum column_unit col_unit)
{
  expanded_location exploc_start = expand_location (hint->get_start_loc ());
  expanded_location exploc_finish = expand_location (hint->get_next_loc ());
  --exploc_finish.column;

  int start_column;
  int finish_column;
  if (col_unit == CU_DISPLAY_COLS)
    {
      start_column = location_compute_display_column (fc, exploc_start, policy);
      if (hint->insertion_p ())
	finish_column = start_column - 1;
      else
	finish_column
	  = location_compute_display_column (fc, exploc_finish, policy);
    }
  else
    {
      start_column = exploc_start.column;
      finish_column = exploc_finish.column;
    }
  return column_range (start_column, finish_column);
}

/* Get the range of display columns that would be printed for HINT.  */

static column_range
get_printed_columns (file_cache &fc,
		     const cpp_char_column_policy &policy,
		     const fixit_hint *hint)
{
  expanded_location exploc = expand_location (hint->get_start_loc ());
  int start_column = location_compute_display_column (fc, exploc, policy);
  int hint_width = cpp_display_width (hint->get_string (), hint->get_length (),
				      policy);
  int final_hint_column = start_column + hint_width - 1;
  if (hint->insertion_p ())
    {
      return column_range (start_column, final_hint_column);
    }
  else
    {
      exploc = expand_location (hint->get_next_loc ());
      --exploc.column;
      int finish_column = location_compute_display_column (fc, exploc, policy);
      return column_range (start_column,
			   MAX (finish_column, final_hint_column));
    }
}

/* A correction on a particular line.
   This describes a plan for how to print one or more fixit_hint
   instances that affected the line, potentially consolidating hints
   into corrections to make the result easier for the user to read.  */

class correction
{
public:
  correction (column_range affected_bytes,
	      column_range affected_columns,
	      column_range printed_columns,
	      const char *new_text, size_t new_text_len,
	      const cpp_char_column_policy &policy)
  : m_affected_bytes (affected_bytes),
    m_affected_columns (affected_columns),
    m_printed_columns (printed_columns),
    m_text (xstrdup (new_text)),
    m_byte_length (new_text_len),
    m_policy (policy),
    m_alloc_sz (new_text_len + 1)
  {
    compute_display_cols ();
  }

  ~correction () { free (m_text); }

  bool insertion_p () const
  {
    return m_affected_bytes.start == m_affected_bytes.finish + 1;
  }

  void ensure_capacity (size_t len);
  void ensure_terminated ();

  void compute_display_cols ()
  {
    m_display_cols = cpp_display_width (m_text, m_byte_length, m_policy);
  }

  void overwrite (int dst_offset, const char_span &src_span)
  {
    gcc_assert (dst_offset >= 0);
    gcc_assert (dst_offset + src_span.length () < m_alloc_sz);
    memcpy (m_text + dst_offset, src_span.get_buffer (),
	    src_span.length ());
  }

  /* If insert, then start: the column before which the text
     is to be inserted, and finish is offset by the length of
     the replacement.
     If replace, then the range of columns affected.  */
  column_range m_affected_bytes;
  column_range m_affected_columns;

  /* If insert, then start: the column before which the text
     is to be inserted, and finish is offset by the length of
     the replacement.
     If replace, then the range of columns affected.  */
  column_range m_printed_columns;

  /* The text to be inserted/used as replacement.  */
  char *m_text;
  size_t m_byte_length; /* Not including null-terminator.  */
  int m_display_cols;
  const cpp_char_column_policy &m_policy;
  size_t m_alloc_sz;
};

/* Ensure that m_text can hold a string of length LEN
   (plus 1 for 0-termination).  */

void
correction::ensure_capacity (size_t len)
{
  /* Allow 1 extra byte for 0-termination.  */
  if (m_alloc_sz < (len + 1))
    {
      size_t new_alloc_sz = (len + 1) * 2;
      m_text = (char *)xrealloc (m_text, new_alloc_sz);
      m_alloc_sz = new_alloc_sz;
    }
}

/* Ensure that m_text is 0-terminated.  */

void
correction::ensure_terminated ()
{
  /* 0-terminate the buffer.  */
  gcc_assert (m_byte_length < m_alloc_sz);
  m_text[m_byte_length] = '\0';
}

/* A list of corrections affecting a particular line.
   This is used by layout::print_trailing_fixits for planning
   how to print the fix-it hints affecting the line.  */

class line_corrections
{
public:
  line_corrections (file_cache &fc,
		    const char_display_policy &policy,
		    const char *filename,
		    linenum_type row)
  : m_file_cache (fc),
    m_policy (policy), m_filename (filename), m_row (row)
  {}
  ~line_corrections ();

  void add_hint (const fixit_hint *hint);

  file_cache &m_file_cache;
  const char_display_policy &m_policy;
  const char *m_filename;
  linenum_type m_row;
  auto_vec <correction *> m_corrections;
};

/* struct line_corrections.  */

line_corrections::~line_corrections ()
{
  unsigned i;
  correction *c;
  FOR_EACH_VEC_ELT (m_corrections, i, c)
    delete c;
}

/* A struct wrapping a particular source line, allowing
   run-time bounds-checking of accesses in a checked build.  */

class source_line
{
public:
  source_line (file_cache &fc, const char *filename, int line);

  char_span as_span () { return char_span (chars, width); }

  const char *chars;
  int width;
};

/* source_line's ctor.  */

source_line::source_line (file_cache &fc, const char *filename, int line)
{
  char_span span = fc.get_source_line (filename, line);
  chars = span.get_buffer ();
  width = span.length ();
}

/* Add HINT to the corrections for this line.
   Attempt to consolidate nearby hints so that they will not
   overlap with printed.  */

void
line_corrections::add_hint (const fixit_hint *hint)
{
  column_range affected_bytes
    = get_affected_range (m_file_cache, m_policy, hint, CU_BYTES);
  column_range affected_columns
    = get_affected_range (m_file_cache, m_policy, hint, CU_DISPLAY_COLS);
  column_range printed_columns
    = get_printed_columns (m_file_cache, m_policy, hint);

  /* Potentially consolidate.  */
  if (!m_corrections.is_empty ())
    {
      correction *last_correction
	= m_corrections[m_corrections.length () - 1];

      /* The following consolidation code assumes that the fix-it hints
	 have been sorted by start (done within layout's ctor).  */
      gcc_assert (affected_bytes.start
		  >= last_correction->m_affected_bytes.start);
      gcc_assert (printed_columns.start
		  >= last_correction->m_printed_columns.start);

      if (printed_columns.start <= last_correction->m_printed_columns.finish
	  && column_range::valid_p (last_correction->m_affected_bytes.finish + 1,
				    affected_bytes.start - 1))
	{
	  /* We have two hints for which the printed forms of the hints
	     would touch or overlap, so we need to consolidate them to avoid
	     confusing the user.
	     Attempt to inject a "replace" correction from immediately
	     after the end of the last hint to immediately before the start
	     of the next hint.  */
	  column_range between (last_correction->m_affected_bytes.finish + 1,
				affected_bytes.start - 1);

	  /* Try to read the source.  */
	  source_line line (m_file_cache, m_filename, m_row);
	  if (line.chars && between.finish < line.width)
	    {
	      /* Consolidate into the last correction:
		 add a no-op "replace" of the "between" text, and
		 add the text from the new hint.  */
	      int old_byte_len = last_correction->m_byte_length;
	      gcc_assert (old_byte_len >= 0);
	      int between_byte_len = between.finish + 1 - between.start;
	      gcc_assert (between_byte_len >= 0);
	      int new_byte_len
		= old_byte_len + between_byte_len + hint->get_length ();
	      gcc_assert (new_byte_len >= 0);
	      last_correction->ensure_capacity (new_byte_len);
	      last_correction->overwrite
		(old_byte_len,
		 line.as_span ().subspan (between.start - 1,
					  between.finish + 1 - between.start));
	      last_correction->overwrite (old_byte_len + between_byte_len,
					  char_span (hint->get_string (),
						     hint->get_length ()));
	      last_correction->m_byte_length = new_byte_len;
	      last_correction->ensure_terminated ();
	      last_correction->m_affected_bytes.finish
		= affected_bytes.finish;
	      last_correction->m_affected_columns.finish
		= affected_columns.finish;
	      int prev_display_cols = last_correction->m_display_cols;
	      last_correction->compute_display_cols ();
	      last_correction->m_printed_columns.finish
		+= last_correction->m_display_cols - prev_display_cols;
	      return;
	    }
	}
    }

  /* If no consolidation happened, add a new correction instance.  */
  m_corrections.safe_push (new correction (affected_bytes,
					   affected_columns,
					   printed_columns,
					   hint->get_string (),
					   hint->get_length (),
					   m_policy));
}

/* If there are any fixit hints on source line ROW, print them.
   They are printed in order, attempting to combine them onto lines, but
   starting new lines if necessary.
   Fix-it hints that insert new lines are handled separately,
   in layout::print_leading_fixits.  */

void
layout_printer::print_trailing_fixits (linenum_type row)
{
  /* Build a list of correction instances for the line,
     potentially consolidating hints (for the sake of readability).  */
  line_corrections corrections (m_layout.m_file_cache, m_layout.m_char_policy,
				m_layout.m_exploc.file, row);
  for (unsigned int i = 0; i < m_layout.m_fixit_hints.length (); i++)
    {
      const fixit_hint *hint = m_layout.m_fixit_hints[i];

      /* Newline fixits are handled by layout::print_leading_fixits.  */
      if (hint->ends_with_newline_p ())
	continue;

      if (hint->affects_line_p (m_layout.m_line_table,
				m_layout.m_exploc.file,
				row))
	corrections.add_hint (hint);
    }

  /* Now print the corrections.  */
  unsigned i;
  correction *c;
  int column = 1 + m_layout.m_x_offset_display;

  if (!corrections.m_corrections.is_empty ())
    start_annotation_line ();

  FOR_EACH_VEC_ELT (corrections.m_corrections, i, c)
    {
      /* For now we assume each fixit hint can only touch one line.  */
      if (c->insertion_p ())
	{
	  /* This assumes the insertion just affects one line.  */
	  int start_column = c->m_printed_columns.start;
	  move_to_column (&column, start_column, true);
	  m_colorizer.set_fixit_insert ();
	  pp_string (&m_pp, c->m_text);
	  m_colorizer.set_normal_text ();
	  column += c->m_display_cols;
	}
      else
	{
	  /* If the range of the replacement wasn't printed in the
	     annotation line, then print an extra underline to
	     indicate exactly what is being replaced.
	     Always show it for removals.  */
	  int start_column = c->m_affected_columns.start;
	  int finish_column = c->m_affected_columns.finish;
	  if (!m_layout.annotation_line_showed_range_p (row, start_column,
							finish_column)
	      || c->m_byte_length == 0)
	    {
	      move_to_column (&column, start_column, true);
	      m_colorizer.set_fixit_delete ();
	      for (; column <= finish_column; column++)
		pp_character (&m_pp, '-');
	      m_colorizer.set_normal_text ();
	    }
	  /* Print the replacement text.  REPLACE also covers
	     removals, so only do this extra work (potentially starting
	     a new line) if we have actual replacement text.  */
	  if (c->m_byte_length > 0)
	    {
	      move_to_column (&column, start_column, true);
	      m_colorizer.set_fixit_insert ();
	      pp_string (&m_pp, c->m_text);
	      m_colorizer.set_normal_text ();
	      column += c->m_display_cols;
	    }
	}
    }

  /* Add a trailing newline, if necessary.  */
  move_to_column (&column, 1 + m_layout.m_x_offset_display, false);
}

/* Disable any colorization and emit a newline.  */

void
layout_printer::print_newline ()
{
  m_colorizer.set_normal_text ();
  pp_newline (&m_pp);
}

/* Return true if (ROW/COLUMN) is within a range of the layout.
   If it returns true, OUT_STATE is written to, with the
   range index, and whether we should draw the caret at
   (ROW/COLUMN) (as opposed to an underline).  COL_UNIT controls
   whether all inputs and outputs are in bytes or display column units.  */

bool
layout::get_state_at_point (/* Inputs.  */
			    linenum_type row, int column,
			    int first_non_ws, int last_non_ws,
			    enum column_unit col_unit,
			    /* Outputs.  */
			    point_state *out_state) const
{
  layout_range *range;
  int i;
  FOR_EACH_VEC_ELT (m_layout_ranges, i, range)
    {
      if (range->m_range_display_kind == SHOW_LINES_WITHOUT_RANGE)
	/* Bail out early, so that such ranges don't affect underlining or
	   source colorization.  */
	continue;

      if (range->contains_point (row, column, col_unit))
	{
	  out_state->range_idx = i;

	  /* Are we at the range's caret?  is it visible? */
	  out_state->draw_caret_p = false;
	  if (range->m_range_display_kind == SHOW_RANGE_WITH_CARET
	      && row == range->m_caret.m_line
	      && column == range->m_caret.m_columns[col_unit])
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
   Get the display column beyond the rightmost one that could contain a caret
   or range marker, given that we stop rendering at trailing whitespace.
   ROW is the source line within the given file.
   CARET_COLUMN is the display column of range 0's caret.
   LAST_NON_WS_COLUMN is the last display column containing a non-whitespace
   character of source (as determined when printing the source line).  */

int
layout::get_x_bound_for_row (linenum_type row, int caret_column,
			     int last_non_ws_column) const
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
	      const int disp_col = range->m_finish.m_columns[CU_DISPLAY_COLS];
	      if (result <= disp_col)
		result = disp_col + 1;
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
   and updating *COLUMN.  If ADD_LEFT_MARGIN, then print the (empty)
   left margin after any newline.  */

void
layout_printer::move_to_column (int *column,
				int dest_column,
				bool add_left_margin)
{
  /* Start a new line if we need to.  */
  if (*column > dest_column)
    {
      print_newline ();
      if (add_left_margin)
	start_annotation_line ();
      *column = 1 + m_layout.m_x_offset_display;
    }

  while (*column < dest_column)
    {
      /* For debugging column issues, it can be helpful to replace this
	 pp_space call with
	   pp_character (&m_pp, '0' + (*column % 10));
	 to visualize the changing value of "*column".  */
      pp_space (&m_pp);
      (*column)++;
    }
}

/* For debugging layout issues, render a ruler giving column numbers
   (after the 1-column indent).  */

void
layout_printer::show_ruler (int max_column)
{
  /* Hundreds.  */
  if (max_column > 99)
    {
      start_annotation_line ();
      for (int column = 1 + m_layout.m_x_offset_display;
	   column <= max_column;
	   ++column)
	if (column % 10 == 0)
	  pp_character (&m_pp, '0' + (column / 100) % 10);
	else
	  pp_space (&m_pp);
      pp_newline (&m_pp);
    }

  /* Tens.  */
  start_annotation_line ();
  for (int column = 1 + m_layout.m_x_offset_display;
       column <= max_column;
       ++column)
    if (column % 10 == 0)
      pp_character (&m_pp, '0' + (column / 10) % 10);
    else
      pp_space (&m_pp);
  pp_newline (&m_pp);

  /* Units.  */
  start_annotation_line ();
  for (int column = 1 + m_layout.m_x_offset_display;
       column <= max_column;
       ++column)
    pp_character (&m_pp, '0' + (column % 10));
  pp_newline (&m_pp);
}

/* Print leading fix-its (for new lines inserted before the source line)
   then the source line, followed by an annotation line
   consisting of any caret/underlines, then any fixits.
   If the source line can't be read, print nothing.  */
void
layout_printer::print_line (linenum_type row)
{
  char_span line
    = m_layout.m_file_cache.get_source_line (m_layout.m_exploc.file, row);
  if (!line)
    return;

  print_any_right_to_left_edge_lines ();
  print_leading_fixits (row);
  const line_bounds lbounds
    = print_source_line (row, line.get_buffer (), line.length ());
  if (m_layout.should_print_annotation_line_p (row))
    print_annotation_line (row, lbounds);
  if (get_options ().show_labels_p)
    print_any_labels (row);
  print_trailing_fixits (row);
}

/* If there's a link column in the RHS, print something like this:
   "                                           │\n"
   "┌──────────────────────────────────────────┘\n"
   showing the link entering at the top right and emerging
   at the bottom left.  */

void
layout_printer::print_any_right_to_left_edge_lines ()
{
  if (m_link_rhs_column == -1)
    /* Can also happen if the out-edge had UNKNOWN_LOCATION.  */
    return;

  gcc_assert (get_options ().show_event_links_p);

  /* Print the line with "|".  */
  start_annotation_line ();
  int column = 1 + m_layout.m_x_offset_display;
  move_to_column (&column, m_link_rhs_column, true);
  m_colorizer.set_cfg_edge ();
  const cppchar_t down= get_theme ().get_cppchar
    (text_art::theme::cell_kind::CFG_DOWN);
  pp_unicode_character (&m_pp, down);
  m_colorizer.set_normal_text ();
  pp_newline (&m_pp);

  /* Print the line with "┌──────────────────────────────────────────┘".  */
  m_link_lhs_state = link_lhs_state::rewinding_to_lhs;
  start_annotation_line ();
  m_colorizer.set_cfg_edge ();
  const cppchar_t left= get_theme ().get_cppchar
    (text_art::theme::cell_kind::CFG_LEFT);
  for (int column = 1 + m_layout.m_x_offset_display;
       column < m_link_rhs_column;
       ++column)
    pp_unicode_character (&m_pp, left);
  const cppchar_t from_down_to_left = get_theme ().get_cppchar
    (text_art::theme::cell_kind::CFG_FROM_DOWN_TO_LEFT);
  pp_unicode_character (&m_pp, from_down_to_left);
  m_colorizer.set_normal_text ();
  pp_newline (&m_pp);

  /* We now have a link line on the LHS,
     and no longer have one on the RHS.  */
  m_link_lhs_state = link_lhs_state::at_lhs;
  m_link_rhs_column = -1;
}

layout_printer::layout_printer (pretty_printer &pp,
				const layout &layout,
				const rich_location &richloc,
				diagnostic_t diagnostic_kind)
: m_pp (pp),
  m_layout (layout),
  m_colorizer (m_pp, richloc, diagnostic_kind),
  m_is_diagnostic_path (diagnostic_kind == DK_DIAGNOSTIC_PATH),
  m_link_lhs_state (link_lhs_state::none),
  m_link_rhs_column (-1)
{
  if (get_options ().show_event_links_p)
    if (auto effect_info = m_layout.m_effect_info)
      if (effect_info->m_leading_in_edge_column)
	m_link_rhs_column = effect_info->m_leading_in_edge_column;
}

} /* End of anonymous namespace.  */

/* If LOC is within the spans of lines that will already be printed for
   this gcc_rich_location, then add it as a secondary location and return true.

   Otherwise return false.

   Use POLICY for determining how spans of lines would be printed.  */

bool
gcc_rich_location::
add_location_if_nearby (const diagnostic_source_print_policy &policy,
			location_t loc,
			bool restrict_to_current_line_spans,
			const range_label *label)
{
  /* Use the layout location-handling logic to sanitize LOC,
     filtering it to the current line spans within a temporary
     layout instance.  */

  layout layout (policy, *this);
  location_range loc_range;
  loc_range.m_loc = loc;
  loc_range.m_range_display_kind = SHOW_RANGE_WITHOUT_CARET;
  loc_range.m_label = nullptr;
  if (!layout.maybe_add_location_range (&loc_range, 0,
					restrict_to_current_line_spans))
    return false;

  add_range (loc, SHOW_RANGE_WITHOUT_CARET, label);
  return true;
}

bool
gcc_rich_location::
add_location_if_nearby (const diagnostic_context &dc,
			location_t loc,
			bool restrict_to_current_line_spans,
			const range_label *label)
{
  diagnostic_source_print_policy source_policy (dc);
  return add_location_if_nearby (source_policy, loc,
				 restrict_to_current_line_spans, label);
}


/* As per diagnostic_source_print_policy::print, but don't print anything
   if source printing is disabled, or if the location hasn't changed.  */

void
diagnostic_context::maybe_show_locus (const rich_location &richloc,
				      diagnostic_t diagnostic_kind,
				      pretty_printer &pp,
				      diagnostic_source_effect_info *effects)
{
  const location_t loc = richloc.get_loc ();
  /* Do nothing if source-printing has been disabled.  */
  if (!m_source_printing.enabled)
    return;

  /* Don't attempt to print source for UNKNOWN_LOCATION and for builtins.  */
  if (loc <= BUILTINS_LOCATION)
    return;

  /* Don't print the same source location twice in a row, unless we have
     fix-it hints, or multiple locations, or a label.  */
  if (loc == m_last_location
      && richloc.get_num_fixit_hints () == 0
      && richloc.get_num_locations () == 1
      && richloc.get_range (0)->m_label == NULL)
    return;

  m_last_location = loc;

  diagnostic_source_print_policy source_policy (*this);
  source_policy.print (pp, richloc, diagnostic_kind, effects);
}

diagnostic_source_print_policy::
diagnostic_source_print_policy (const diagnostic_context &dc)
: m_options (dc.m_source_printing),
  m_location_policy (dc),
  m_start_span_cb (dc.m_text_callbacks.m_start_span),
  m_file_cache (dc.get_file_cache ()),
  m_diagram_theme (dc.get_diagram_theme ()),
  m_escape_format (dc.get_escape_format ())
{
}

/* Print to PP the physical source code corresponding to the location(s)
   in RICHLOC, with additional annotations, as if for a diagnostic of the
   given DIAGNOSTIC_KIND.
   If EFFECTS is non-null, then use and update it.  */

void
diagnostic_source_print_policy::print (pretty_printer &pp,
				       const rich_location &richloc,
				       diagnostic_t diagnostic_kind,
				       diagnostic_source_effect_info *effects)
  const
{
  layout layout (*this, richloc, effects);
  layout_printer lp (pp, layout, richloc, diagnostic_kind);
  lp.print (*this);
}

void
layout_printer::print (const diagnostic_source_print_policy &source_policy)
{
  diagnostic_prefixing_rule_t saved_rule = pp_prefixing_rule (&m_pp);
  pp_prefixing_rule (&m_pp) = DIAGNOSTICS_SHOW_PREFIX_EVERY_LINE;

  if (get_options ().show_ruler_p)
    show_ruler (m_layout.m_x_offset_display + get_options ().max_width);

  for (int line_span_idx = 0; line_span_idx < m_layout.get_num_line_spans ();
       line_span_idx++)
    {
      const line_span *line_span = m_layout.get_line_span (line_span_idx);
      if (get_options ().show_line_numbers_p)
	{
	  /* With line numbers, we should show whenever the line-numbering
	     "jumps".  */
	  if (line_span_idx > 0)
	    print_gap_in_line_numbering ();
	}
      else
	{
	  /* Without line numbers, we print headings for some line spans.  */
	  if (m_layout.print_heading_for_line_span_index_p (line_span_idx))
	    {
	      expanded_location exploc
		= m_layout.get_expanded_location (line_span);
	      const diagnostic_location_print_policy &
		loc_policy = source_policy.get_location_policy ();
	      source_policy.get_start_span_fn () (loc_policy, &m_pp, exploc);
	    }
	}
      /* Iterate over the lines within this span (using linenum_arith_t to
	 avoid overflow with 0xffffffff causing an infinite loop).  */
      linenum_arith_t last_line = line_span->get_last_line ();
      for (linenum_arith_t row = line_span->get_first_line ();
	   row <= last_line; row++)
	print_line (row);
    }

  if (auto effect_info = m_layout.m_effect_info)
    effect_info->m_trailing_out_edge_column = m_link_rhs_column;

  pp_prefixing_rule (&m_pp) = saved_rule;
}

#if CHECKING_P

namespace selftest {

/* Selftests for diagnostic_show_locus.  */

diagnostic_show_locus_fixture::
diagnostic_show_locus_fixture (const line_table_case &case_,
			       const char *content)
: m_content (content),
  m_tmp_source_file (SELFTEST_LOCATION, ".c", content),
  m_ltt (case_),
  m_fc ()
{
  linemap_add (line_table, LC_ENTER, false,
	       m_tmp_source_file.get_filename (), 1);
}

/* Populate a char_display_policy based on DC and RICHLOC.  */

static char_display_policy
make_char_policy (const diagnostic_context &dc,
		  const rich_location &richloc)
{
  diagnostic_source_print_policy source_policy (dc);
  return ::make_char_policy (source_policy, richloc);
}

/* Verify that cpp_display_width correctly handles escaping.  */

static void
test_display_widths ()
{
  gcc_rich_location richloc (UNKNOWN_LOCATION);

  /* U+03C0 "GREEK SMALL LETTER PI".  */
  const char *pi = "\xCF\x80";
  /* U+1F642 "SLIGHTLY SMILING FACE".  */
  const char *emoji = "\xF0\x9F\x99\x82";
  /* Stray trailing byte of a UTF-8 character.  */
  const char *stray = "\xBF";
  /* U+10FFFF.  */
  const char *max_codepoint = "\xF4\x8F\xBF\xBF";

  /* No escaping.  */
  {
    test_diagnostic_context dc;
    char_display_policy policy (make_char_policy (dc, richloc));
    ASSERT_EQ (cpp_display_width (pi, strlen (pi), policy), 1);
    ASSERT_EQ (cpp_display_width (emoji, strlen (emoji), policy), 2);
    ASSERT_EQ (cpp_display_width (stray, strlen (stray), policy), 1);
    /* Don't check width of U+10FFFF; it's in a private use plane.  */
  }

  richloc.set_escape_on_output (true);

  {
    test_diagnostic_context dc;
    dc.set_escape_format (DIAGNOSTICS_ESCAPE_FORMAT_UNICODE);
    char_display_policy policy (make_char_policy (dc, richloc));
    ASSERT_EQ (cpp_display_width (pi, strlen (pi), policy), 8);
    ASSERT_EQ (cpp_display_width (emoji, strlen (emoji), policy), 9);
    ASSERT_EQ (cpp_display_width (stray, strlen (stray), policy), 4);
    ASSERT_EQ (cpp_display_width (max_codepoint, strlen (max_codepoint),
				  policy),
	       strlen ("<U+10FFFF>"));
  }

  {
    test_diagnostic_context dc;
    dc.set_escape_format (DIAGNOSTICS_ESCAPE_FORMAT_BYTES);
    char_display_policy policy (make_char_policy (dc, richloc));
    ASSERT_EQ (cpp_display_width (pi, strlen (pi), policy), 8);
    ASSERT_EQ (cpp_display_width (emoji, strlen (emoji), policy), 16);
    ASSERT_EQ (cpp_display_width (stray, strlen (stray), policy), 4);
    ASSERT_EQ (cpp_display_width (max_codepoint, strlen (max_codepoint),
				  policy),
	       16);
  }
}

/* For precise tests of the layout, make clear where the source line will
   start.  test_left_margin sets the total byte count from the left side of the
   screen to the start of source lines, after the line number and the separator,
   which consists of the three characters " | ".  */
static const int test_linenum_sep = 3;
static const int test_left_margin = 7;

/* Helper function for test_layout_x_offset_display_utf8().  */
static void
test_offset_impl (int caret_byte_col, int max_width,
		  int expected_x_offset_display,
		  int left_margin = test_left_margin)
{
  test_diagnostic_context dc;
  dc.m_source_printing.max_width = max_width;
  /* diagnostic_context::min_margin_width sets the minimum space reserved for
     the line number plus one space after.  */
  dc.m_source_printing.min_margin_width = left_margin - test_linenum_sep + 1;
  dc.m_source_printing.show_line_numbers_p = true;
  diagnostic_source_print_policy source_policy (dc);
  rich_location richloc (line_table,
			 linemap_position_for_column (line_table,
						      caret_byte_col));
  layout test_layout (source_policy, richloc, nullptr);
  ASSERT_EQ (left_margin - test_linenum_sep,
	     test_layout.get_linenum_width ());
  ASSERT_EQ (expected_x_offset_display,
	     test_layout.get_x_offset_display ());
}

/* Test that layout::calculate_x_offset_display() works.  */
static void
test_layout_x_offset_display_utf8 (const line_table_case &case_)
{

  const char *content
    = "This line is very long, so that we can use it to test the logic for "
      "clipping long lines.  Also this: \xf0\x9f\x98\x82\xf0\x9f\x98\x82 is a "
      "pair of emojis that occupies 8 bytes and 4 display columns, starting at "
      "column #102.\n";

  /* Number of bytes in the line, subtracting one to remove the newline.  */
  const int line_bytes = strlen (content) - 1;

  /* Number of display columns occupied by the line; each of the 2 emojis
     takes up 2 fewer display columns than it does bytes.  */
  const int line_display_cols = line_bytes - 2*2;

  /* The column of the first emoji.  Byte or display is the same as there are
     no multibyte characters earlier on the line.  */
  const int emoji_col = 102;

  diagnostic_show_locus_fixture f (case_, content);

  linemap_add (line_table, LC_ENTER, false, f.get_filename (), 1);

  location_t line_end = linemap_position_for_column (line_table, line_bytes);

  /* Don't attempt to run the tests if column data might be unavailable.  */
  if (line_end > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  ASSERT_STREQ (f.get_filename (), LOCATION_FILE (line_end));
  ASSERT_EQ (1, LOCATION_LINE (line_end));
  ASSERT_EQ (line_bytes, LOCATION_COLUMN (line_end));

  char_span lspan = f.m_fc.get_source_line (f.get_filename (), 1);
  ASSERT_EQ (line_display_cols,
	     cpp_display_width (lspan.get_buffer (), lspan.length (),
				def_policy ()));
  ASSERT_EQ (line_display_cols,
	     location_compute_display_column (f.m_fc,
					      expand_location (line_end),
					      def_policy ()));
  ASSERT_EQ (0, memcmp (lspan.get_buffer () + (emoji_col - 1),
			"\xf0\x9f\x98\x82\xf0\x9f\x98\x82", 8));

  /* (caret_byte, max_width, expected_x_offset_display, [left_margin])  */

  /* No constraint on the width -> no offset.  */
  test_offset_impl (emoji_col, 0, 0);

  /* Caret is before the beginning -> no offset.  */
  test_offset_impl (0, 100, 0);

  /* Caret is past the end of the line -> no offset.  */
  test_offset_impl (line_bytes+1, 100, 0);

  /* Line fits in the display -> no offset.  */
  test_offset_impl (line_bytes, line_display_cols + test_left_margin, 0);
  test_offset_impl (emoji_col, line_display_cols + test_left_margin, 0);

  /* Line is too long for the display but caret location is OK
     anyway -> no offset.  */
  static const int small_width = 24;
  test_offset_impl (1, small_width, 0);

  /* Width constraint is very small -> no offset.  */
  test_offset_impl (emoji_col, CARET_LINE_MARGIN, 0);

  /* Line would be offset, but due to large line numbers, offsetting
     would remove the whole line -> no offset.  */
  static const int huge_left_margin = 100;
  test_offset_impl (emoji_col, huge_left_margin, 0, huge_left_margin);

  /* Line is the same length as the display, but the line number makes it too
     long, so offset is required.  Caret is at the end so padding on the right
     is not in effect.  */
  for (int excess = 1; excess <= 3; ++excess)
    test_offset_impl (line_bytes, line_display_cols + test_left_margin - excess,
		      excess);

  /* Line is much too long for the display, caret is near the end ->
     offset should be such that the line fits in the display and caret
     remains the same distance from the end that it was.  */
  for (int caret_offset = 0, max_offset = MIN (CARET_LINE_MARGIN, 10);
       caret_offset <= max_offset; ++caret_offset)
    test_offset_impl (line_bytes - caret_offset, small_width,
		      line_display_cols + test_left_margin - small_width);

  /* As previous case but caret is closer to the middle; now we want it to end
     up CARET_LINE_MARGIN bytes from the end.  */
  ASSERT_GT (line_display_cols - emoji_col, CARET_LINE_MARGIN);
  test_offset_impl (emoji_col, small_width,
		    emoji_col + test_left_margin
		    - (small_width - CARET_LINE_MARGIN));

  /* Test that the source line is offset as expected when printed.  */
  {
    test_diagnostic_context dc;
    dc.m_source_printing.max_width = small_width - 6;
    dc.m_source_printing.min_margin_width
      = test_left_margin - test_linenum_sep + 1;
    dc.m_source_printing.show_line_numbers_p = true;
    dc.m_source_printing.show_ruler_p = true;
    diagnostic_source_print_policy policy (dc);
    rich_location richloc (line_table,
			   linemap_position_for_column (line_table,
							emoji_col));
    layout test_layout (policy, richloc, nullptr);
    layout_printer lp (*dc.get_reference_printer (), test_layout, richloc, DK_ERROR);
    lp.print (policy);
    ASSERT_STREQ ("     |         1         \n"
		  "     |         1         \n"
		  "     | 234567890123456789\n"
		  "   1 | \xf0\x9f\x98\x82\xf0\x9f\x98\x82 is a pair of emojis "
		  "that occupies 8 bytes and 4 display columns, starting at "
		  "column #102.\n"
		  "     | ^\n",
		  pp_formatted_text (dc.get_reference_printer ()));
  }

  /* Similar to the previous example, but now the offset called for would split
     the first emoji in the middle of the UTF-8 sequence.  Check that we replace
     it with a padding space in this case.  */
  {
    test_diagnostic_context dc;
    dc.m_source_printing.max_width = small_width - 5;
    dc.m_source_printing.min_margin_width
      = test_left_margin - test_linenum_sep + 1;
    dc.m_source_printing.show_line_numbers_p = true;
    dc.m_source_printing.show_ruler_p = true;
    diagnostic_source_print_policy policy (dc);
    rich_location richloc (line_table,
			   linemap_position_for_column (line_table,
							emoji_col + 2));
    layout test_layout (dc, richloc, nullptr);
    layout_printer lp (*dc.get_reference_printer (), test_layout, richloc, DK_ERROR);
    lp.print (policy);
    ASSERT_STREQ ("     |        1         1 \n"
		  "     |        1         2 \n"
		  "     | 3456789012345678901\n"
		  "   1 |  \xf0\x9f\x98\x82 is a pair of emojis "
		  "that occupies 8 bytes and 4 display columns, starting at "
		  "column #102.\n"
		  "     |  ^\n",
		  pp_formatted_text (dc.get_reference_printer ()));
  }

}

static void
test_layout_x_offset_display_tab (const line_table_case &case_)
{
  const char *content
    = "This line is very long, so that we can use it to test the logic for "
      "clipping long lines.  Also this: `\t' is a tab that occupies 1 byte and "
      "a variable number of display columns, starting at column #103.\n";

  /* Number of bytes in the line, subtracting one to remove the newline.  */
  const int line_bytes = strlen (content) - 1;

 /* The column where the tab begins.  Byte or display is the same as there are
    no multibyte characters earlier on the line.  */
  const int tab_col = 103;

  /* Effective extra size of the tab beyond what a single space would have taken
     up, indexed by tabstop.  */
  static const int num_tabstops = 11;
  int extra_width[num_tabstops];
  for (int tabstop = 1; tabstop != num_tabstops; ++tabstop)
    {
      const int this_tab_size = tabstop - (tab_col - 1) % tabstop;
      extra_width[tabstop] = this_tab_size - 1;
    }
  /* Example of this calculation: if tabstop is 10, the tab starting at column
     #103 has to expand into 8 spaces, covering columns 103-110, so that the
     next character is at column #111.  So it takes up 7 more columns than
     a space would have taken up.  */
  ASSERT_EQ (7, extra_width[10]);

  temp_source_file tmp (SELFTEST_LOCATION, ".c", content);
  file_cache fc;
  line_table_test ltt (case_);

  linemap_add (line_table, LC_ENTER, false, tmp.get_filename (), 1);

  location_t line_end = linemap_position_for_column (line_table, line_bytes);

  /* Don't attempt to run the tests if column data might be unavailable.  */
  if (line_end > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  /* Check that cpp_display_width handles the tabs as expected.  */
  char_span lspan = fc.get_source_line (tmp.get_filename (), 1);
  ASSERT_EQ ('\t', *(lspan.get_buffer () + (tab_col - 1)));
  for (int tabstop = 1; tabstop != num_tabstops; ++tabstop)
    {
      cpp_char_column_policy policy (tabstop, cpp_wcwidth);
      ASSERT_EQ (line_bytes + extra_width[tabstop],
		 cpp_display_width (lspan.get_buffer (), lspan.length (),
				    policy));
      ASSERT_EQ (line_bytes + extra_width[tabstop],
		 location_compute_display_column (fc,
						  expand_location (line_end),
						  policy));
    }

  /* Check that the tab is expanded to the expected number of spaces.  */
  rich_location richloc (line_table,
			 linemap_position_for_column (line_table,
						      tab_col + 1));
  for (int tabstop = 1; tabstop != num_tabstops; ++tabstop)
    {
      test_diagnostic_context dc;
      dc.m_tabstop = tabstop;
      diagnostic_source_print_policy policy (dc);
      layout test_layout (policy, richloc, nullptr);
      layout_printer lp (*dc.get_reference_printer (), test_layout, richloc, DK_ERROR);
      lp.print (policy);
      const char *out = pp_formatted_text (dc.get_reference_printer ());
      ASSERT_EQ (NULL, strchr (out, '\t'));
      const char *left_quote = strchr (out, '`');
      const char *right_quote = strchr (out, '\'');
      ASSERT_NE (NULL, left_quote);
      ASSERT_NE (NULL, right_quote);
      ASSERT_EQ (right_quote - left_quote, extra_width[tabstop] + 2);
    }

  /* Check that the line is offset properly and that the tab is broken up
     into the expected number of spaces when it is the last character skipped
     over.  */
  for (int tabstop = 1; tabstop != num_tabstops; ++tabstop)
    {
      test_diagnostic_context dc;
      dc.m_tabstop = tabstop;
      static const int small_width = 24;
      dc.m_source_printing.max_width = small_width - 4;
      dc.m_source_printing.min_margin_width
	= test_left_margin - test_linenum_sep + 1;
      dc.m_source_printing.show_line_numbers_p = true;
      diagnostic_source_print_policy policy (dc);
      layout test_layout (policy, richloc, nullptr);
      layout_printer lp (*dc.get_reference_printer (), test_layout, richloc, DK_ERROR);
      lp.print (policy);

      /* We have arranged things so that two columns will be printed before
	 the caret.  If the tab results in more than one space, this should
	 produce two spaces in the output; otherwise, it will be a single space
	 preceded by the opening quote before the tab character.  */
      const char *output1
	= "   1 |   ' is a tab that occupies 1 byte and a variable number of "
	  "display columns, starting at column #103.\n"
	  "     |   ^\n";
      const char *output2
	= "   1 | ` ' is a tab that occupies 1 byte and a variable number of "
	  "display columns, starting at column #103.\n"
	  "     |   ^\n";
      const char *expected_output = (extra_width[tabstop] ? output1 : output2);
      ASSERT_STREQ (expected_output, pp_formatted_text (dc.get_reference_printer ()));
    }
}


/* Verify that diagnostic_show_locus works sanely on UNKNOWN_LOCATION.  */

static void
test_diagnostic_show_locus_unknown_location ()
{
  test_diagnostic_context dc;
  rich_location richloc (line_table, UNKNOWN_LOCATION);
  ASSERT_STREQ ("", dc.test_show_locus (richloc));
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
  ASSERT_STREQ (" foo = bar.field;\n"
		"          ^\n",
		dc.test_show_locus (richloc));
}

/* No column information (column == 0).
   No annotation line should be printed.  */

static void
test_one_liner_no_column ()
{
  test_diagnostic_context dc;
  location_t caret = linemap_position_for_column (line_table, 0);
  rich_location richloc (line_table, caret);
  ASSERT_STREQ (" foo = bar.field;\n",
		dc.test_show_locus (richloc));
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
  ASSERT_STREQ (" foo = bar.field;\n"
		"       ~~~^~~~~~\n",
		dc.test_show_locus (richloc));
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
  dc.m_source_printing.caret_chars[0] = 'A';

  location_t bar
    = make_location (linemap_position_for_column (line_table, 8),
		     linemap_position_for_column (line_table, 7),
		     linemap_position_for_column (line_table, 9));
  dc.m_source_printing.caret_chars[1] = 'B';

  location_t field
    = make_location (linemap_position_for_column (line_table, 13),
		     linemap_position_for_column (line_table, 11),
		     linemap_position_for_column (line_table, 15));
  dc.m_source_printing.caret_chars[2] = 'C';

  rich_location richloc (line_table, foo);
  richloc.add_range (bar, SHOW_RANGE_WITH_CARET);
  richloc.add_range (field, SHOW_RANGE_WITH_CARET);
  ASSERT_STREQ (" foo = bar.field;\n"
		" ~A~   ~B~ ~~C~~\n",
		dc.test_show_locus (richloc));
}

/* Insertion fix-it hint: adding an "&" to the front of "bar.field". */

static void
test_one_liner_fixit_insert_before ()
{
  test_diagnostic_context dc;
  location_t caret = linemap_position_for_column (line_table, 7);
  rich_location richloc (line_table, caret);
  richloc.add_fixit_insert_before ("&");
  ASSERT_STREQ (" foo = bar.field;\n"
		"       ^\n"
		"       &\n",
		dc.test_show_locus (richloc));
}

/* Insertion fix-it hint: adding a "[0]" after "foo". */

static void
test_one_liner_fixit_insert_after ()
{
  test_diagnostic_context dc;
  location_t start = linemap_position_for_column (line_table, 1);
  location_t finish = linemap_position_for_column (line_table, 3);
  location_t foo = make_location (start, start, finish);
  rich_location richloc (line_table, foo);
  richloc.add_fixit_insert_after ("[0]");
  ASSERT_STREQ (" foo = bar.field;\n"
		" ^~~\n"
		"    [0]\n",
		dc.test_show_locus (richloc));
}

/* Removal fix-it hint: removal of the ".field".
   Also verify the interaction of pp_set_prefix with rulers and
   fix-it hints.  */

static void
test_one_liner_fixit_remove ()
{
  location_t start = linemap_position_for_column (line_table, 10);
  location_t finish = linemap_position_for_column (line_table, 15);
  location_t dot = make_location (start, start, finish);
  rich_location richloc (line_table, dot);
  richloc.add_fixit_remove ();

  /* Normal.  */
  {
    test_diagnostic_context dc;
    ASSERT_STREQ (" foo = bar.field;\n"
		  "          ^~~~~~\n"
		  "          ------\n",
		  dc.test_show_locus (richloc));
  }

  /* Test of adding a prefix.  */
  {
    test_diagnostic_context dc;
    pp_prefixing_rule (dc.get_reference_printer ()) = DIAGNOSTICS_SHOW_PREFIX_EVERY_LINE;
    pp_set_prefix (dc.get_reference_printer (), xstrdup ("TEST PREFIX:"));
    ASSERT_STREQ ("TEST PREFIX: foo = bar.field;\n"
		  "TEST PREFIX:          ^~~~~~\n"
		  "TEST PREFIX:          ------\n",
		  dc.test_show_locus (richloc));
  }

  /* Normal, with ruler.  */
  {
    test_diagnostic_context dc;
    dc.m_source_printing.show_ruler_p = true;
    dc.m_source_printing.max_width = 104;
    ASSERT_STREQ ("          0         0         0         0         0         0         0         0         0         1    \n"
		  "          1         2         3         4         5         6         7         8         9         0    \n"
		  " 12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234\n"
		  " foo = bar.field;\n"
		  "          ^~~~~~\n"
		  "          ------\n",
		  dc.test_show_locus (richloc));
  }

  /* Test of adding a prefix, with ruler.  */
  {
    test_diagnostic_context dc;
    dc.m_source_printing.show_ruler_p = true;
    dc.m_source_printing.max_width = 50;
    pp_prefixing_rule (dc.get_reference_printer ()) = DIAGNOSTICS_SHOW_PREFIX_EVERY_LINE;
    pp_set_prefix (dc.get_reference_printer (), xstrdup ("TEST PREFIX:"));
    ASSERT_STREQ ("TEST PREFIX:          1         2         3         4         5\n"
		  "TEST PREFIX: 12345678901234567890123456789012345678901234567890\n"
		  "TEST PREFIX: foo = bar.field;\n"
		  "TEST PREFIX:          ^~~~~~\n"
		  "TEST PREFIX:          ------\n",
		  dc.test_show_locus (richloc));
  }

  /* Test of adding a prefix, with ruler and line numbers.  */
  {
    test_diagnostic_context dc;
    dc.m_source_printing.show_ruler_p = true;
    dc.m_source_printing.max_width = 50;
    dc.m_source_printing.show_line_numbers_p = true;
    pp_prefixing_rule (dc.get_reference_printer ()) = DIAGNOSTICS_SHOW_PREFIX_EVERY_LINE;
    pp_set_prefix (dc.get_reference_printer (), xstrdup ("TEST PREFIX:"));
    ASSERT_STREQ ("TEST PREFIX:      |          1         2         3         4         5\n"
		  "TEST PREFIX:      | 12345678901234567890123456789012345678901234567890\n"
		  "TEST PREFIX:    1 | foo = bar.field;\n"
		  "TEST PREFIX:      |          ^~~~~~\n"
		  "TEST PREFIX:      |          ------\n",
		  dc.test_show_locus (richloc));
  }
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
  richloc.add_fixit_replace ("m_field");
  ASSERT_STREQ (" foo = bar.field;\n"
		"           ^~~~~\n"
		"           m_field\n",
		dc.test_show_locus (richloc));
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
  /* The replacement range is not indicated in the annotation line, so
     it should be indicated via an additional underline.  */
  ASSERT_STREQ (" foo = bar.field;\n"
		"     ^\n"
		"           -----\n"
		"           m_field\n",
		dc.test_show_locus (richloc));
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
  richloc.add_range (field);
  richloc.add_fixit_replace (field, "m_field");
  /* The replacement range is indicated in the annotation line,
     so it shouldn't be indicated via an additional underline.  */
  ASSERT_STREQ (" foo = bar.field;\n"
		"     ^     ~~~~~\n"
		"           m_field\n",
		dc.test_show_locus (richloc));
}

/* Verify that we can use ad-hoc locations when adding fixits to a
   rich_location.  */

static void
test_one_liner_fixit_validation_adhoc_locations ()
{
  /* Generate a range that's too long to be packed, so must
     be stored as an ad-hoc location (given the defaults
     of 5 bits or 0 bits of packed range); 41 columns > 2**5.  */
  const location_t c7 = linemap_position_for_column (line_table, 7);
  const location_t c47 = linemap_position_for_column (line_table, 47);
  const location_t loc = make_location (c7, c7, c47);

  if (c47 > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  ASSERT_TRUE (IS_ADHOC_LOC (loc));

  /* Insert.  */
  {
    rich_location richloc (line_table, loc);
    richloc.add_fixit_insert_before (loc, "test");
    /* It should not have been discarded by the validator.  */
    ASSERT_EQ (1, richloc.get_num_fixit_hints ());

    test_diagnostic_context dc;
    ASSERT_STREQ (" foo = bar.field;\n"
		  "       ^~~~~~~~~~                               \n"
		  "       test\n",
		  dc.test_show_locus (richloc));
  }

  /* Remove.  */
  {
    rich_location richloc (line_table, loc);
    source_range range = source_range::from_locations (loc, c47);
    richloc.add_fixit_remove (range);
    /* It should not have been discarded by the validator.  */
    ASSERT_EQ (1, richloc.get_num_fixit_hints ());

    test_diagnostic_context dc;
    ASSERT_STREQ (" foo = bar.field;\n"
		  "       ^~~~~~~~~~                               \n"
		  "       -----------------------------------------\n",
		  dc.test_show_locus (richloc));
  }

  /* Replace.  */
  {
    rich_location richloc (line_table, loc);
    source_range range = source_range::from_locations (loc, c47);
    richloc.add_fixit_replace (range, "test");
    /* It should not have been discarded by the validator.  */
    ASSERT_EQ (1, richloc.get_num_fixit_hints ());

    test_diagnostic_context dc;
    ASSERT_STREQ (" foo = bar.field;\n"
		  "       ^~~~~~~~~~                               \n"
		  "       test\n",
		  dc.test_show_locus (richloc));
  }
}

/* Test of consolidating insertions at the same location.  */

static void
test_one_liner_many_fixits_1 ()
{
  test_diagnostic_context dc;
  location_t equals = linemap_position_for_column (line_table, 5);
  rich_location richloc (line_table, equals);
  for (int i = 0; i < 19; i++)
    richloc.add_fixit_insert_before ("a");
  ASSERT_EQ (1, richloc.get_num_fixit_hints ());
  ASSERT_STREQ (" foo = bar.field;\n"
		"     ^\n"
		"     aaaaaaaaaaaaaaaaaaa\n",
		dc.test_show_locus (richloc));
}

/* Ensure that we can add an arbitrary number of fix-it hints to a
   rich_location, even if they are not consolidated.  */

static void
test_one_liner_many_fixits_2 ()
{
  test_diagnostic_context dc;
  location_t equals = linemap_position_for_column (line_table, 5);
  rich_location richloc (line_table, equals);
  for (int i = 0; i < 19; i++)
    {
      location_t loc = linemap_position_for_column (line_table, (i * 2) + 1);
      richloc.add_fixit_insert_before (loc, "a");
    }
  ASSERT_EQ (19, richloc.get_num_fixit_hints ());
  ASSERT_STREQ (" foo = bar.field;\n"
		"     ^\n"
		" a a a a a a a a a a a a a a a a a a a\n",
		dc.test_show_locus (richloc));
}

/* Test of labeling the ranges within a rich_location.  */

static void
test_one_liner_labels ()
{
  location_t foo
    = make_location (linemap_position_for_column (line_table, 1),
		     linemap_position_for_column (line_table, 1),
		     linemap_position_for_column (line_table, 3));
  location_t bar
    = make_location (linemap_position_for_column (line_table, 7),
		     linemap_position_for_column (line_table, 7),
		     linemap_position_for_column (line_table, 9));
  location_t field
    = make_location (linemap_position_for_column (line_table, 11),
		     linemap_position_for_column (line_table, 11),
		     linemap_position_for_column (line_table, 15));

  /* Example where all the labels fit on one line.  */
  {
    text_range_label label0 ("0");
    text_range_label label1 ("1");
    text_range_label label2 ("2");
    gcc_rich_location richloc (foo, &label0, nullptr);
    richloc.add_range (bar, SHOW_RANGE_WITHOUT_CARET, &label1);
    richloc.add_range (field, SHOW_RANGE_WITHOUT_CARET, &label2);

    {
      test_diagnostic_context dc;
      ASSERT_STREQ (" foo = bar.field;\n"
		    " ^~~   ~~~ ~~~~~\n"
		    " |     |   |\n"
		    " 0     1   2\n",
		    dc.test_show_locus (richloc));
    }

    /* Verify that we can disable label-printing.  */
    {
      test_diagnostic_context dc;
      dc.m_source_printing.show_labels_p = false;
      ASSERT_STREQ (" foo = bar.field;\n"
		    " ^~~   ~~~ ~~~~~\n",
		    dc.test_show_locus (richloc));
    }
  }

  /* Example where the labels need extra lines.  */
  {
    text_range_label label0 ("label 0");
    text_range_label label1 ("label 1");
    text_range_label label2 ("label 2");
    gcc_rich_location richloc (foo, &label0, nullptr);
    richloc.add_range (bar, SHOW_RANGE_WITHOUT_CARET, &label1);
    richloc.add_range (field, SHOW_RANGE_WITHOUT_CARET, &label2);

    test_diagnostic_context dc;
    ASSERT_STREQ (" foo = bar.field;\n"
		  " ^~~   ~~~ ~~~~~\n"
		  " |     |   |\n"
		  " |     |   label 2\n"
		  " |     label 1\n"
		  " label 0\n",
		  dc.test_show_locus (richloc));
  }

  /* Example of boundary conditions: label 0 and 1 have just enough clearance,
     but label 1 just touches label 2.  */
  {
    text_range_label label0 ("aaaaa");
    text_range_label label1 ("bbbb");
    text_range_label label2 ("c");
    gcc_rich_location richloc (foo, &label0, nullptr);
    richloc.add_range (bar, SHOW_RANGE_WITHOUT_CARET, &label1);
    richloc.add_range (field, SHOW_RANGE_WITHOUT_CARET, &label2);

    test_diagnostic_context dc;
    ASSERT_STREQ (" foo = bar.field;\n"
		  " ^~~   ~~~ ~~~~~\n"
		  " |     |   |\n"
		  " |     |   c\n"
		  " aaaaa bbbb\n",
		  dc.test_show_locus (richloc));
  }

  /* Example of out-of-order ranges (thus requiring a sort).  */
  {
    text_range_label label0 ("0");
    text_range_label label1 ("1");
    text_range_label label2 ("2");
    gcc_rich_location richloc (field, &label0, nullptr);
    richloc.add_range (bar, SHOW_RANGE_WITHOUT_CARET, &label1);
    richloc.add_range (foo, SHOW_RANGE_WITHOUT_CARET, &label2);

    test_diagnostic_context dc;
    ASSERT_STREQ (" foo = bar.field;\n"
		  " ~~~   ~~~ ^~~~~\n"
		  " |     |   |\n"
		  " 2     1   0\n",
		  dc.test_show_locus (richloc));
  }

  /* Ensure we don't ICE if multiple ranges with labels are on
     the same point.  */
  {
    text_range_label label0 ("label 0");
    text_range_label label1 ("label 1");
    text_range_label label2 ("label 2");
    gcc_rich_location richloc (bar, &label0, nullptr);
    richloc.add_range (bar, SHOW_RANGE_WITHOUT_CARET, &label1);
    richloc.add_range (bar, SHOW_RANGE_WITHOUT_CARET, &label2);

    test_diagnostic_context dc;
    ASSERT_STREQ (" foo = bar.field;\n"
		  "       ^~~\n"
		  "       |\n"
		  "       label 0\n"
		  "       label 1\n"
		  "       label 2\n",
		  dc.test_show_locus (richloc));
  }

  /* Example of out-of-order ranges (thus requiring a sort), where
     they overlap, and there are multiple ranges on the same point.  */
  {
    text_range_label label_0a ("label 0a");
    text_range_label label_1a ("label 1a");
    text_range_label label_2a ("label 2a");
    text_range_label label_0b ("label 0b");
    text_range_label label_1b ("label 1b");
    text_range_label label_2b ("label 2b");
    text_range_label label_0c ("label 0c");
    text_range_label label_1c ("label 1c");
    text_range_label label_2c ("label 2c");
    gcc_rich_location richloc (field, &label_0a, nullptr);
    richloc.add_range (bar, SHOW_RANGE_WITHOUT_CARET, &label_1a);
    richloc.add_range (foo, SHOW_RANGE_WITHOUT_CARET, &label_2a);

    richloc.add_range (field, SHOW_RANGE_WITHOUT_CARET, &label_0b);
    richloc.add_range (bar, SHOW_RANGE_WITHOUT_CARET, &label_1b);
    richloc.add_range (foo, SHOW_RANGE_WITHOUT_CARET, &label_2b);

    richloc.add_range (field, SHOW_RANGE_WITHOUT_CARET, &label_0c);
    richloc.add_range (bar, SHOW_RANGE_WITHOUT_CARET, &label_1c);
    richloc.add_range (foo, SHOW_RANGE_WITHOUT_CARET, &label_2c);

    test_diagnostic_context dc;
    ASSERT_STREQ (" foo = bar.field;\n"
		  " ~~~   ~~~ ^~~~~\n"
		  " |     |   |\n"
		  " |     |   label 0a\n"
		  " |     |   label 0b\n"
		  " |     |   label 0c\n"
		  " |     label 1a\n"
		  " |     label 1b\n"
		  " |     label 1c\n"
		  " label 2a\n"
		  " label 2b\n"
		  " label 2c\n",
		  dc.test_show_locus (richloc));
  }

  /* Verify that a NULL result from range_label::get_text is
     handled gracefully.  */
  {
    text_range_label label (NULL);
    gcc_rich_location richloc (bar, &label, nullptr);

    test_diagnostic_context dc;
    ASSERT_STREQ (" foo = bar.field;\n"
		  "       ^~~\n",
		  dc.test_show_locus (richloc));
   }

  /* TODO: example of formatted printing (needs to be in
     gcc-rich-location.cc due to Makefile.in issues).  */
}

/* Run the various one-liner tests.  */

static void
test_diagnostic_show_locus_one_liner (const line_table_case &case_)
{
  /* Create a tempfile and write some text to it.
     ....................0000000001111111.
     ....................1234567890123456.  */
  const char *content = "foo = bar.field;\n";

  diagnostic_show_locus_fixture f (case_, content);

  location_t line_end = linemap_position_for_column (line_table, 16);

  /* Don't attempt to run the tests if column data might be unavailable.  */
  if (line_end > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  ASSERT_STREQ (f.get_filename (), LOCATION_FILE (line_end));
  ASSERT_EQ (1, LOCATION_LINE (line_end));
  ASSERT_EQ (16, LOCATION_COLUMN (line_end));

  test_one_liner_simple_caret ();
  test_one_liner_no_column ();
  test_one_liner_caret_and_range ();
  test_one_liner_multiple_carets_and_ranges ();
  test_one_liner_fixit_insert_before ();
  test_one_liner_fixit_insert_after ();
  test_one_liner_fixit_remove ();
  test_one_liner_fixit_replace ();
  test_one_liner_fixit_replace_non_equal_range ();
  test_one_liner_fixit_replace_equal_secondary_range ();
  test_one_liner_fixit_validation_adhoc_locations ();
  test_one_liner_many_fixits_1 ();
  test_one_liner_many_fixits_2 ();
  test_one_liner_labels ();
}

/* Version of all one-liner tests exercising multibyte awareness.
   These are all called from test_diagnostic_show_locus_one_liner,
   which uses diagnostic_show_locus_fixture_one_liner_utf8 to create
   the test file; see the notes in diagnostic-show-locus-selftest.h.

   Note: all of the below asserts would be easier to read if we used UTF-8
   directly in the string constants, but it seems better not to demand the
   host compiler support this, when it isn't otherwise necessary.  Instead,
   whenever an extended character appears in a string, we put a line break
   after it so that all succeeding characters can appear visually at the
   correct display column.  */

/* Just a caret.  */

static void
test_one_liner_simple_caret_utf8 ()
{
  test_diagnostic_context dc;
  location_t caret = linemap_position_for_column (line_table, 18);
  rich_location richloc (line_table, caret);
  ASSERT_STREQ (" \xf0\x9f\x98\x82"
		   "_foo = \xcf\x80"
			   "_bar.\xf0\x9f\x98\x82"
				  "_field\xcf\x80"
					 ";\n"
		"               ^\n",
		dc.test_show_locus (richloc));
}

/* Caret and range.  */
static void
test_one_liner_caret_and_range_utf8 ()
{
  test_diagnostic_context dc;
  location_t caret = linemap_position_for_column (line_table, 18);
  location_t start = linemap_position_for_column (line_table, 12);
  location_t finish = linemap_position_for_column (line_table, 30);
  location_t loc = make_location (caret, start, finish);
  rich_location richloc (line_table, loc);
  ASSERT_STREQ (" \xf0\x9f\x98\x82"
		   "_foo = \xcf\x80"
			   "_bar.\xf0\x9f\x98\x82"
				  "_field\xcf\x80"
					 ";\n"
		"          ~~~~~^~~~~~~~~~\n",
		dc.test_show_locus (richloc));
}

/* Multiple ranges and carets.  */

static void
test_one_liner_multiple_carets_and_ranges_utf8 ()
{
  test_diagnostic_context dc;
  location_t foo
    = make_location (linemap_position_for_column (line_table, 7),
		     linemap_position_for_column (line_table, 1),
		     linemap_position_for_column (line_table, 8));
  dc.m_source_printing.caret_chars[0] = 'A';

  location_t bar
    = make_location (linemap_position_for_column (line_table, 16),
		     linemap_position_for_column (line_table, 12),
		     linemap_position_for_column (line_table, 17));
  dc.m_source_printing.caret_chars[1] = 'B';

  location_t field
    = make_location (linemap_position_for_column (line_table, 26),
		     linemap_position_for_column (line_table, 19),
		     linemap_position_for_column (line_table, 30));
  dc.m_source_printing.caret_chars[2] = 'C';
  rich_location richloc (line_table, foo);
  richloc.add_range (bar, SHOW_RANGE_WITH_CARET);
  richloc.add_range (field, SHOW_RANGE_WITH_CARET);
  ASSERT_STREQ (" \xf0\x9f\x98\x82"
		   "_foo = \xcf\x80"
			   "_bar.\xf0\x9f\x98\x82"
				  "_field\xcf\x80"
					 ";\n"
		" ~~~~A~   ~~~B~ ~~~~~C~~~\n",
		dc.test_show_locus (richloc));
}

/* Insertion fix-it hint: adding an "&" to the front of "P_bar.field". */

static void
test_one_liner_fixit_insert_before_utf8 ()
{
  test_diagnostic_context dc;
  location_t caret = linemap_position_for_column (line_table, 12);
  rich_location richloc (line_table, caret);
  richloc.add_fixit_insert_before ("&");
  ASSERT_STREQ (" \xf0\x9f\x98\x82"
		   "_foo = \xcf\x80"
			   "_bar.\xf0\x9f\x98\x82"
				  "_field\xcf\x80"
					 ";\n"
		"          ^\n"
		"          &\n",
		dc.test_show_locus (richloc));
}

/* Insertion fix-it hint: adding a "[0]" after "SS_foo". */

static void
test_one_liner_fixit_insert_after_utf8 ()
{
  test_diagnostic_context dc;
  location_t start = linemap_position_for_column (line_table, 1);
  location_t finish = linemap_position_for_column (line_table, 8);
  location_t foo = make_location (start, start, finish);
  rich_location richloc (line_table, foo);
  richloc.add_fixit_insert_after ("[0]");
  ASSERT_STREQ (" \xf0\x9f\x98\x82"
		   "_foo = \xcf\x80"
			   "_bar.\xf0\x9f\x98\x82"
				  "_field\xcf\x80"
					 ";\n"
		" ^~~~~~\n"
		"       [0]\n",
		dc.test_show_locus (richloc));
}

/* Removal fix-it hint: removal of the ".SS_fieldP". */

static void
test_one_liner_fixit_remove_utf8 ()
{
  test_diagnostic_context dc;
  location_t start = linemap_position_for_column (line_table, 18);
  location_t finish = linemap_position_for_column (line_table, 30);
  location_t dot = make_location (start, start, finish);
  rich_location richloc (line_table, dot);
  richloc.add_fixit_remove ();
  ASSERT_STREQ (" \xf0\x9f\x98\x82"
		   "_foo = \xcf\x80"
			   "_bar.\xf0\x9f\x98\x82"
				  "_field\xcf\x80"
					 ";\n"
		"               ^~~~~~~~~~\n"
		"               ----------\n",
		dc.test_show_locus (richloc));
}

/* Replace fix-it hint: replacing "SS_fieldP" with "m_SSfieldP". */

static void
test_one_liner_fixit_replace_utf8 ()
{
  test_diagnostic_context dc;
  location_t start = linemap_position_for_column (line_table, 19);
  location_t finish = linemap_position_for_column (line_table, 30);
  location_t field = make_location (start, start, finish);
  rich_location richloc (line_table, field);
  richloc.add_fixit_replace ("m_\xf0\x9f\x98\x82_field\xcf\x80");
  ASSERT_STREQ (" \xf0\x9f\x98\x82"
		   "_foo = \xcf\x80"
			   "_bar.\xf0\x9f\x98\x82"
				  "_field\xcf\x80"
					 ";\n"
		"                ^~~~~~~~~\n"
		"                m_\xf0\x9f\x98\x82"
				    "_field\xcf\x80\n",
		dc.test_show_locus (richloc));
}

/* Replace fix-it hint: replacing "SS_fieldP" with "m_SSfieldP",
   but where the caret was elsewhere.  */

static void
test_one_liner_fixit_replace_non_equal_range_utf8 ()
{
  test_diagnostic_context dc;
  location_t equals = linemap_position_for_column (line_table, 10);
  location_t start = linemap_position_for_column (line_table, 19);
  location_t finish = linemap_position_for_column (line_table, 30);
  rich_location richloc (line_table, equals);
  source_range range;
  range.m_start = start;
  range.m_finish = finish;
  richloc.add_fixit_replace (range, "m_\xf0\x9f\x98\x82_field\xcf\x80");
  /* The replacement range is not indicated in the annotation line, so
     it should be indicated via an additional underline.  */
  ASSERT_STREQ (" \xf0\x9f\x98\x82"
		   "_foo = \xcf\x80"
			   "_bar.\xf0\x9f\x98\x82"
				  "_field\xcf\x80"
					 ";\n"
		"        ^\n"
		"                ---------\n"
		"                m_\xf0\x9f\x98\x82"
				    "_field\xcf\x80\n",
		dc.test_show_locus (richloc));
}

/* Replace fix-it hint: replacing "SS_fieldP" with "m_SSfieldP",
   where the caret was elsewhere, but where a secondary range
   exactly covers "field".  */

static void
test_one_liner_fixit_replace_equal_secondary_range_utf8 ()
{
  test_diagnostic_context dc;
  location_t equals = linemap_position_for_column (line_table, 10);
  location_t start = linemap_position_for_column (line_table, 19);
  location_t finish = linemap_position_for_column (line_table, 30);
  rich_location richloc (line_table, equals);
  location_t field = make_location (start, start, finish);
  richloc.add_range (field);
  richloc.add_fixit_replace (field, "m_\xf0\x9f\x98\x82_field\xcf\x80");
  /* The replacement range is indicated in the annotation line,
     so it shouldn't be indicated via an additional underline.  */
  ASSERT_STREQ (" \xf0\x9f\x98\x82"
		   "_foo = \xcf\x80"
			   "_bar.\xf0\x9f\x98\x82"
				  "_field\xcf\x80"
					 ";\n"
		"        ^       ~~~~~~~~~\n"
		"                m_\xf0\x9f\x98\x82"
				    "_field\xcf\x80\n",
		dc.test_show_locus (richloc));
}

/* Verify that we can use ad-hoc locations when adding fixits to a
   rich_location.  */

static void
test_one_liner_fixit_validation_adhoc_locations_utf8 ()
{
  /* Generate a range that's too long to be packed, so must
     be stored as an ad-hoc location (given the defaults
     of 5 bits or 0 bits of packed range); 41 columns > 2**5.  */
  const location_t c12 = linemap_position_for_column (line_table, 12);
  const location_t c52 = linemap_position_for_column (line_table, 52);
  const location_t loc = make_location (c12, c12, c52);

  if (c52 > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  ASSERT_TRUE (IS_ADHOC_LOC (loc));

  /* Insert.  */
  {
    rich_location richloc (line_table, loc);
    richloc.add_fixit_insert_before (loc, "test");
    /* It should not have been discarded by the validator.  */
    ASSERT_EQ (1, richloc.get_num_fixit_hints ());

    test_diagnostic_context dc;
    ASSERT_STREQ (" \xf0\x9f\x98\x82"
		     "_foo = \xcf\x80"
			     "_bar.\xf0\x9f\x98\x82"
				    "_field\xcf\x80"
					   ";\n"
		  "          ^~~~~~~~~~~~~~~~                     \n"
		  "          test\n",
		dc.test_show_locus (richloc));
  }

  /* Remove.  */
  {
    rich_location richloc (line_table, loc);
    source_range range = source_range::from_locations (loc, c52);
    richloc.add_fixit_remove (range);
    /* It should not have been discarded by the validator.  */
    ASSERT_EQ (1, richloc.get_num_fixit_hints ());

    test_diagnostic_context dc;
    ASSERT_STREQ (" \xf0\x9f\x98\x82"
		     "_foo = \xcf\x80"
			     "_bar.\xf0\x9f\x98\x82"
				    "_field\xcf\x80"
					   ";\n"
		  "          ^~~~~~~~~~~~~~~~                     \n"
		  "          -------------------------------------\n",
		dc.test_show_locus (richloc));
  }

  /* Replace.  */
  {
    rich_location richloc (line_table, loc);
    source_range range = source_range::from_locations (loc, c52);
    richloc.add_fixit_replace (range, "test");
    /* It should not have been discarded by the validator.  */
    ASSERT_EQ (1, richloc.get_num_fixit_hints ());

    test_diagnostic_context dc;
    ASSERT_STREQ (" \xf0\x9f\x98\x82"
		     "_foo = \xcf\x80"
			     "_bar.\xf0\x9f\x98\x82"
				    "_field\xcf\x80"
					   ";\n"
		  "          ^~~~~~~~~~~~~~~~                     \n"
		  "          test\n",
		dc.test_show_locus (richloc));
  }
}

/* Test of consolidating insertions at the same location.  */

static void
test_one_liner_many_fixits_1_utf8 ()
{
  test_diagnostic_context dc;
  location_t equals = linemap_position_for_column (line_table, 10);
  rich_location richloc (line_table, equals);
  for (int i = 0; i < 19; i++)
    richloc.add_fixit_insert_before (i & 1 ? "@" : "\xcf\x80");
  ASSERT_EQ (1, richloc.get_num_fixit_hints ());
  ASSERT_STREQ (" \xf0\x9f\x98\x82"
		   "_foo = \xcf\x80"
			   "_bar.\xf0\x9f\x98\x82"
				  "_field\xcf\x80"
					 ";\n"
		"        ^\n"
		"        \xcf\x80@\xcf\x80@\xcf\x80@\xcf\x80@\xcf\x80@"
		"\xcf\x80@\xcf\x80@\xcf\x80@\xcf\x80@\xcf\x80\n",
		dc.test_show_locus (richloc));
}

/* Ensure that we can add an arbitrary number of fix-it hints to a
   rich_location, even if they are not consolidated.  */

static void
test_one_liner_many_fixits_2_utf8 ()
{
  test_diagnostic_context dc;
  location_t equals = linemap_position_for_column (line_table, 10);
  rich_location richloc (line_table, equals);
  const int nlocs = 19;
  int locs[nlocs] = {1, 5, 7, 9, 11, 14, 16, 18, 23, 25, 27, 29, 32,
		     34, 36, 38, 40, 42, 44};
  for (int i = 0; i != nlocs; ++i)
    {
      location_t loc = linemap_position_for_column (line_table, locs[i]);
      richloc.add_fixit_insert_before (loc, i & 1 ? "@" : "\xcf\x80");
    }

  ASSERT_EQ (nlocs, richloc.get_num_fixit_hints ());
  ASSERT_STREQ (" \xf0\x9f\x98\x82"
		   "_foo = \xcf\x80"
			   "_bar.\xf0\x9f\x98\x82"
				  "_field\xcf\x80"
					 ";\n"
		"        ^\n"
		" \xcf\x80 @ \xcf\x80 @ \xcf\x80 @ \xcf\x80 @  \xcf\x80 @"
		" \xcf\x80 @ \xcf\x80 @ \xcf\x80 @ \xcf\x80 @ \xcf\x80\n",
		dc.test_show_locus (richloc));
}

/* Test of labeling the ranges within a rich_location.  */

static void
test_one_liner_labels_utf8 ()
{
  location_t foo
    = make_location (linemap_position_for_column (line_table, 1),
		     linemap_position_for_column (line_table, 1),
		     linemap_position_for_column (line_table, 8));
  location_t bar
    = make_location (linemap_position_for_column (line_table, 12),
		     linemap_position_for_column (line_table, 12),
		     linemap_position_for_column (line_table, 17));
  location_t field
    = make_location (linemap_position_for_column (line_table, 19),
		     linemap_position_for_column (line_table, 19),
		     linemap_position_for_column (line_table, 30));

  /* Example where all the labels fit on one line.  */
  {
    /* These three labels contain multibyte characters such that their byte
       lengths are respectively (12, 10, 18), but their display widths are only
       (6, 5, 9).  All three fit on the line when considering the display
       widths, but not when considering the byte widths, so verify that we do
       indeed put them all on one line.  */
    text_range_label label0
      ("\xcf\x80\xcf\x80\xcf\x80\xcf\x80\xcf\x80\xcf\x80");
    text_range_label label1
      ("\xf0\x9f\x98\x82\xf0\x9f\x98\x82\xcf\x80");
    text_range_label label2
      ("\xf0\x9f\x98\x82\xcf\x80\xf0\x9f\x98\x82\xf0\x9f\x98\x82\xcf\x80"
       "\xcf\x80");
    gcc_rich_location richloc (foo, &label0, nullptr);
    richloc.add_range (bar, SHOW_RANGE_WITHOUT_CARET, &label1);
    richloc.add_range (field, SHOW_RANGE_WITHOUT_CARET, &label2);

    {
      test_diagnostic_context dc;
      ASSERT_STREQ (" \xf0\x9f\x98\x82"
		       "_foo = \xcf\x80"
			       "_bar.\xf0\x9f\x98\x82"
				      "_field\xcf\x80"
					     ";\n"
		    " ^~~~~~   ~~~~~ ~~~~~~~~~\n"
		    " |        |     |\n"
		    " \xcf\x80\xcf\x80\xcf\x80\xcf\x80\xcf\x80\xcf\x80"
			   "   \xf0\x9f\x98\x82\xf0\x9f\x98\x82\xcf\x80"
				   " \xf0\x9f\x98\x82\xcf\x80\xf0\x9f\x98\x82"
					 "\xf0\x9f\x98\x82\xcf\x80\xcf\x80\n",
		    dc.test_show_locus (richloc));
    }

  }

  /* Example where the labels need extra lines.  */
  {
    text_range_label label0 ("label 0\xf0\x9f\x98\x82");
    text_range_label label1 ("label 1\xcf\x80");
    text_range_label label2 ("label 2\xcf\x80");
    gcc_rich_location richloc (foo, &label0, nullptr);
    richloc.add_range (bar, SHOW_RANGE_WITHOUT_CARET, &label1);
    richloc.add_range (field, SHOW_RANGE_WITHOUT_CARET, &label2);

    test_diagnostic_context dc;
    ASSERT_STREQ (" \xf0\x9f\x98\x82"
		     "_foo = \xcf\x80"
			     "_bar.\xf0\x9f\x98\x82"
				    "_field\xcf\x80"
					   ";\n"
		  " ^~~~~~   ~~~~~ ~~~~~~~~~\n"
		  " |        |     |\n"
		  " |        |     label 2\xcf\x80\n"
		  " |        label 1\xcf\x80\n"
		  " label 0\xf0\x9f\x98\x82\n",
		  dc.test_show_locus (richloc));
  }

  /* Example of boundary conditions: label 0 and 1 have just enough clearance,
     but label 1 just touches label 2.  */
  {
    text_range_label label0 ("aaaaa\xf0\x9f\x98\x82\xcf\x80");
    text_range_label label1 ("bb\xf0\x9f\x98\x82\xf0\x9f\x98\x82");
    text_range_label label2 ("c");
    gcc_rich_location richloc (foo, &label0, nullptr);
    richloc.add_range (bar, SHOW_RANGE_WITHOUT_CARET, &label1);
    richloc.add_range (field, SHOW_RANGE_WITHOUT_CARET, &label2);

    test_diagnostic_context dc;
    ASSERT_STREQ (" \xf0\x9f\x98\x82"
		     "_foo = \xcf\x80"
			     "_bar.\xf0\x9f\x98\x82"
				    "_field\xcf\x80"
					   ";\n"
		  " ^~~~~~   ~~~~~ ~~~~~~~~~\n"
		  " |        |     |\n"
		  " |        |     c\n"
		  " aaaaa\xf0\x9f\x98\x82\xcf\x80"
			   " bb\xf0\x9f\x98\x82\xf0\x9f\x98\x82\n",
		  dc.test_show_locus (richloc));
  }

  /* Example of escaping the source lines.  */
  {
    text_range_label label0 ("label 0\xf0\x9f\x98\x82");
    text_range_label label1 ("label 1\xcf\x80");
    text_range_label label2 ("label 2\xcf\x80");
    gcc_rich_location richloc (foo, &label0, nullptr);
    richloc.add_range (bar, SHOW_RANGE_WITHOUT_CARET, &label1);
    richloc.add_range (field, SHOW_RANGE_WITHOUT_CARET, &label2);
    richloc.set_escape_on_output (true);

    {
      test_diagnostic_context dc;
      dc.set_escape_format (DIAGNOSTICS_ESCAPE_FORMAT_UNICODE);
      ASSERT_STREQ (" <U+1F602>_foo = <U+03C0>_bar.<U+1F602>_field<U+03C0>;\n"
		    " ^~~~~~~~~~~~~   ~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~~~~~~\n"
		    " |               |            |\n"
		    " label 0\xf0\x9f\x98\x82"
		    /* ... */ "       label 1\xcf\x80"
		    /* ...................*/ "     label 2\xcf\x80\n",
		    dc.test_show_locus (richloc));
    }
    {
      test_diagnostic_context dc;
      dc.set_escape_format (DIAGNOSTICS_ESCAPE_FORMAT_BYTES);
      ASSERT_STREQ
	(" <f0><9f><98><82>_foo = <cf><80>_bar.<f0><9f><98><82>_field<cf><80>;\n"
	 " ^~~~~~~~~~~~~~~~~~~~   ~~~~~~~~~~~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
	 " |                      |            |\n"
	 " label 0\xf0\x9f\x98\x82"
	 /* ... */ "              label 1\xcf\x80"
	 /* ..........................*/ "     label 2\xcf\x80\n",
	 dc.test_show_locus (richloc));
    }
  }
}

/* Make sure that colorization codes don't interrupt a multibyte
   sequence, which would corrupt it.  */
static void
test_one_liner_colorized_utf8 ()
{
  test_diagnostic_context dc;
  dc.m_source_printing.colorize_source_p = true;
  diagnostic_color_init (&dc, DIAGNOSTICS_COLOR_YES);
  const location_t pi = linemap_position_for_column (line_table, 12);
  rich_location richloc (line_table, pi);

  /* In order to avoid having the test depend on exactly how the colorization
     was effected, just confirm there are two pi characters in the output.  */
  const char *result = dc.test_show_locus (richloc);
  const char *null_term = result + strlen (result);
  const char *first_pi = strstr (result, "\xcf\x80");
  ASSERT_TRUE (first_pi && first_pi <= null_term - 2);
  ASSERT_STR_CONTAINS (first_pi + 2, "\xcf\x80");
}

static const char * const one_liner_utf8_content
  /* Display columns.
     0000000000000000000000011111111111111111111111111111112222222222222
     1111111122222222345678900000000123456666666677777777890123444444445  */
  = "\xf0\x9f\x98\x82_foo = \xcf\x80_bar.\xf0\x9f\x98\x82_field\xcf\x80;\n";
  /* 0000000000000000000001111111111111111111222222222222222222222233333
     1111222233334444567890122223333456789999000011112222345678999900001
     Byte columns.  */

diagnostic_show_locus_fixture_one_liner_utf8::
diagnostic_show_locus_fixture_one_liner_utf8 (const line_table_case &case_)
: diagnostic_show_locus_fixture (case_, one_liner_utf8_content)
{
}

/* Run the various one-liner tests.  */

static void
test_diagnostic_show_locus_one_liner_utf8 (const line_table_case &case_)
{
  diagnostic_show_locus_fixture_one_liner_utf8 f (case_);

  location_t line_end = linemap_position_for_column (line_table, 31);

  /* Don't attempt to run the tests if column data might be unavailable.  */
  if (line_end > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  ASSERT_STREQ (f.get_filename (), LOCATION_FILE (line_end));
  ASSERT_EQ (1, LOCATION_LINE (line_end));
  ASSERT_EQ (31, LOCATION_COLUMN (line_end));

  char_span lspan = f.m_fc.get_source_line (f.get_filename (), 1);
  ASSERT_EQ (25, cpp_display_width (lspan.get_buffer (), lspan.length (),
				    def_policy ()));
  ASSERT_EQ (25, location_compute_display_column (f.m_fc,
						  expand_location (line_end),
						  def_policy ()));

  test_one_liner_simple_caret_utf8 ();
  test_one_liner_caret_and_range_utf8 ();
  test_one_liner_multiple_carets_and_ranges_utf8 ();
  test_one_liner_fixit_insert_before_utf8 ();
  test_one_liner_fixit_insert_after_utf8 ();
  test_one_liner_fixit_remove_utf8 ();
  test_one_liner_fixit_replace_utf8 ();
  test_one_liner_fixit_replace_non_equal_range_utf8 ();
  test_one_liner_fixit_replace_equal_secondary_range_utf8 ();
  test_one_liner_fixit_validation_adhoc_locations_utf8 ();
  test_one_liner_many_fixits_1_utf8 ();
  test_one_liner_many_fixits_2_utf8 ();
  test_one_liner_labels_utf8 ();
  test_one_liner_colorized_utf8 ();
}

/* Verify that gcc_rich_location::add_location_if_nearby works.  */

static void
test_add_location_if_nearby (const line_table_case &case_)
{
  /* Create a tempfile and write some text to it.
     ...000000000111111111122222222223333333333.
     ...123456789012345678901234567890123456789.  */
  const char *content
    = ("struct same_line { double x; double y; ;\n" /* line 1.  */
       "struct different_line\n"                    /* line 2.  */
       "{\n"                                        /* line 3.  */
       "  double x;\n"                              /* line 4.  */
       "  double y;\n"                              /* line 5.  */
       ";\n");                                      /* line 6.  */
  temp_source_file tmp (SELFTEST_LOCATION, ".c", content, nullptr);
  line_table_test ltt (case_);

  const line_map_ordinary *ord_map
    = linemap_check_ordinary (linemap_add (line_table, LC_ENTER, false,
					   tmp.get_filename (), 0));

  linemap_line_start (line_table, 1, 100);

  const location_t final_line_end
    = linemap_position_for_line_and_column (line_table, ord_map, 6, 7);

  /* Don't attempt to run the tests if column data might be unavailable.  */
  if (final_line_end > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  /* Test of add_location_if_nearby on the same line as the
     primary location.  */
  {
    test_diagnostic_context dc;
    const location_t missing_close_brace_1_39
      = linemap_position_for_line_and_column (line_table, ord_map, 1, 39);
    const location_t matching_open_brace_1_18
      = linemap_position_for_line_and_column (line_table, ord_map, 1, 18);
    gcc_rich_location richloc (missing_close_brace_1_39);
    bool added = richloc.add_location_if_nearby (dc,
						 matching_open_brace_1_18);
    ASSERT_TRUE (added);
    ASSERT_EQ (2, richloc.get_num_locations ());
    ASSERT_STREQ (" struct same_line { double x; double y; ;\n"
		  "                  ~                    ^\n",
		  dc.test_show_locus (richloc));
  }

  /* Test of add_location_if_nearby on a different line to the
     primary location.  */
  {
    test_diagnostic_context dc;
    const location_t missing_close_brace_6_1
      = linemap_position_for_line_and_column (line_table, ord_map, 6, 1);
    const location_t matching_open_brace_3_1
      = linemap_position_for_line_and_column (line_table, ord_map, 3, 1);
    gcc_rich_location richloc (missing_close_brace_6_1);
    bool added = richloc.add_location_if_nearby (dc,
						 matching_open_brace_3_1);
    ASSERT_FALSE (added);
    ASSERT_EQ (1, richloc.get_num_locations ());
  }
}

/* Verify that we print fixits even if they only affect lines
   outside those covered by the ranges in the rich_location.  */

static void
test_diagnostic_show_locus_fixit_lines (const line_table_case &case_)
{
  /* Create a tempfile and write some text to it.
     ...000000000111111111122222222223333333333.
     ...123456789012345678901234567890123456789.  */
  const char *content
    = ("struct point { double x; double y; };\n" /* line 1.  */
       "struct point origin = {x: 0.0,\n"        /* line 2.  */
       "                       y\n"              /* line 3.  */
       "\n"                                      /* line 4.  */
       "\n"                                      /* line 5.  */
       "                        : 0.0};\n");     /* line 6.  */
  temp_source_file tmp (SELFTEST_LOCATION, ".c", content);
  line_table_test ltt (case_);

  const line_map_ordinary *ord_map
    = linemap_check_ordinary (linemap_add (line_table, LC_ENTER, false,
					   tmp.get_filename (), 0));

  linemap_line_start (line_table, 1, 100);

  const location_t final_line_end
    = linemap_position_for_line_and_column (line_table, ord_map, 6, 36);

  /* Don't attempt to run the tests if column data might be unavailable.  */
  if (final_line_end > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  /* A pair of tests for modernizing the initializers to C99-style.  */

  /* The one-liner case (line 2).  */
  {
    test_diagnostic_context dc;
    const location_t x
      = linemap_position_for_line_and_column (line_table, ord_map, 2, 24);
    const location_t colon
      = linemap_position_for_line_and_column (line_table, ord_map, 2, 25);
    rich_location richloc (line_table, colon);
    richloc.add_fixit_insert_before (x, ".");
    richloc.add_fixit_replace (colon, "=");
    ASSERT_STREQ (" struct point origin = {x: 0.0,\n"
		  "                         ^\n"
		  "                        .=\n",
		  dc.test_show_locus (richloc));
  }

  /* The multiline case.  The caret for the rich_location is on line 6;
     verify that insertion fixit on line 3 is still printed (and that
     span starts are printed due to the gap between the span at line 3
     and that at line 6).  */
  {
    test_diagnostic_context dc;
    const location_t y
      = linemap_position_for_line_and_column (line_table, ord_map, 3, 24);
    const location_t colon
      = linemap_position_for_line_and_column (line_table, ord_map, 6, 25);
    rich_location richloc (line_table, colon);
    richloc.add_fixit_insert_before (y, ".");
    richloc.add_fixit_replace (colon, "=");
    ASSERT_STREQ ("FILENAME:3:24:\n"
		  "                        y\n"
		  "                        .\n"
		  "FILENAME:6:25:\n"
		  "                         : 0.0};\n"
		  "                         ^\n"
		  "                         =\n",
		  dc.test_show_locus (richloc));
  }

  /* As above, but verify the behavior of multiple line spans
     with line-numbering enabled.  */
  {
    const location_t y
      = linemap_position_for_line_and_column (line_table, ord_map, 3, 24);
    const location_t colon
      = linemap_position_for_line_and_column (line_table, ord_map, 6, 25);
    rich_location richloc (line_table, colon);
    richloc.add_fixit_insert_before (y, ".");
    richloc.add_fixit_replace (colon, "=");
    test_diagnostic_context dc;
    dc.m_source_printing.show_line_numbers_p = true;
    ASSERT_STREQ ("    3 |                        y\n"
		  "      |                        .\n"
		  "......\n"
		  "    6 |                         : 0.0};\n"
		  "      |                         ^\n"
		  "      |                         =\n",
		  dc.test_show_locus (richloc));
  }
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
  const location_t c21 = linemap_position_for_column (line_table, 21);
  const location_t caret = c10;

  /* Insert + insert. */
  {
    rich_location richloc (line_table, caret);
    richloc.add_fixit_insert_before (c10, "foo");
    richloc.add_fixit_insert_before (c15, "bar");

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
    richloc.add_fixit_insert_before (c10, "foo");
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
    richloc.add_fixit_insert_before (c17, "foo");

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
	ASSERT_STREQ ("foobar", hint->get_string ());
	ASSERT_EQ (c10, hint->get_start_loc ());
	ASSERT_EQ (c21, hint->get_next_loc ());
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
	ASSERT_STREQ ("foo", hint->get_string ());
	ASSERT_EQ (c10, hint->get_start_loc ());
	ASSERT_EQ (c21, hint->get_next_loc ());
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
	ASSERT_STREQ ("", hint->get_string ());
	ASSERT_EQ (c10, hint->get_start_loc ());
	ASSERT_EQ (c21, hint->get_next_loc ());
      }
  }
}

/* Verify that the line_corrections machinery correctly prints
   overlapping fixit-hints.  */

static void
test_overlapped_fixit_printing (const line_table_case &case_)
{
  /* Create a tempfile and write some text to it.
     ...000000000111111111122222222223333333333.
     ...123456789012345678901234567890123456789.  */
  const char *content
    = ("  foo *f = (foo *)ptr->field;\n");
  temp_source_file tmp (SELFTEST_LOCATION, ".C", content);
  file_cache fc;
  line_table_test ltt (case_);

  const line_map_ordinary *ord_map
    = linemap_check_ordinary (linemap_add (line_table, LC_ENTER, false,
					   tmp.get_filename (), 0));

  linemap_line_start (line_table, 1, 100);

  const location_t final_line_end
    = linemap_position_for_line_and_column (line_table, ord_map, 6, 36);

  /* Don't attempt to run the tests if column data might be unavailable.  */
  if (final_line_end > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  /* A test for converting a C-style cast to a C++-style cast.  */
  const location_t open_paren
    = linemap_position_for_line_and_column (line_table, ord_map, 1, 12);
  const location_t close_paren
    = linemap_position_for_line_and_column (line_table, ord_map, 1, 18);
  const location_t expr_start
    = linemap_position_for_line_and_column (line_table, ord_map, 1, 19);
  const location_t expr_finish
    = linemap_position_for_line_and_column (line_table, ord_map, 1, 28);
  const location_t expr = make_location (expr_start, expr_start, expr_finish);

  /* Various examples of fix-it hints that aren't themselves consolidated,
     but for which the *printing* may need consolidation.  */

  /* Example where 3 fix-it hints are printed as one.  */
  {
    test_diagnostic_context dc;
    rich_location richloc (line_table, expr);
    richloc.add_fixit_replace (open_paren, "const_cast<");
    richloc.add_fixit_replace (close_paren, "> (");
    richloc.add_fixit_insert_after (")");

    ASSERT_STREQ ("   foo *f = (foo *)ptr->field;\n"
		  "                   ^~~~~~~~~~\n"
		  "            -----------------\n"
		  "            const_cast<foo *> (ptr->field)\n",
		  dc.test_show_locus (richloc));

    /* Unit-test the line_corrections machinery.  */
    char_display_policy policy (make_char_policy (dc, richloc));
    ASSERT_EQ (3, richloc.get_num_fixit_hints ());
    const fixit_hint *hint_0 = richloc.get_fixit_hint (0);
    ASSERT_EQ (column_range (12, 12),
	       get_affected_range (fc, policy, hint_0, CU_BYTES));
    ASSERT_EQ (column_range (12, 12),
	       get_affected_range (fc, policy, hint_0, CU_DISPLAY_COLS));
    ASSERT_EQ (column_range (12, 22), get_printed_columns (fc, policy, hint_0));
    const fixit_hint *hint_1 = richloc.get_fixit_hint (1);
    ASSERT_EQ (column_range (18, 18),
	       get_affected_range (fc, policy, hint_1, CU_BYTES));
    ASSERT_EQ (column_range (18, 18),
	       get_affected_range (fc, policy, hint_1, CU_DISPLAY_COLS));
    ASSERT_EQ (column_range (18, 20), get_printed_columns (fc, policy, hint_1));
    const fixit_hint *hint_2 = richloc.get_fixit_hint (2);
    ASSERT_EQ (column_range (29, 28),
	       get_affected_range (fc, policy, hint_2, CU_BYTES));
    ASSERT_EQ (column_range (29, 28),
	       get_affected_range (fc, policy, hint_2, CU_DISPLAY_COLS));
    ASSERT_EQ (column_range (29, 29), get_printed_columns (fc, policy, hint_2));

    /* Add each hint in turn to a line_corrections instance,
       and verify that they are consolidated into one correction instance
       as expected.  */
    line_corrections lc (fc, policy, tmp.get_filename (), 1);

    /* The first replace hint by itself.  */
    lc.add_hint (hint_0);
    ASSERT_EQ (1, lc.m_corrections.length ());
    ASSERT_EQ (column_range (12, 12), lc.m_corrections[0]->m_affected_bytes);
    ASSERT_EQ (column_range (12, 12), lc.m_corrections[0]->m_affected_columns);
    ASSERT_EQ (column_range (12, 22), lc.m_corrections[0]->m_printed_columns);
    ASSERT_STREQ ("const_cast<", lc.m_corrections[0]->m_text);

    /* After the second replacement hint, they are printed together
       as a replacement (along with the text between them).  */
    lc.add_hint (hint_1);
    ASSERT_EQ (1, lc.m_corrections.length ());
    ASSERT_STREQ ("const_cast<foo *> (", lc.m_corrections[0]->m_text);
    ASSERT_EQ (column_range (12, 18), lc.m_corrections[0]->m_affected_bytes);
    ASSERT_EQ (column_range (12, 18), lc.m_corrections[0]->m_affected_columns);
    ASSERT_EQ (column_range (12, 30), lc.m_corrections[0]->m_printed_columns);

    /* After the final insertion hint, they are all printed together
       as a replacement (along with the text between them).  */
    lc.add_hint (hint_2);
    ASSERT_STREQ ("const_cast<foo *> (ptr->field)",
		  lc.m_corrections[0]->m_text);
    ASSERT_EQ (1, lc.m_corrections.length ());
    ASSERT_EQ (column_range (12, 28), lc.m_corrections[0]->m_affected_bytes);
    ASSERT_EQ (column_range (12, 28), lc.m_corrections[0]->m_affected_columns);
    ASSERT_EQ (column_range (12, 41), lc.m_corrections[0]->m_printed_columns);
  }

  /* Example where two are consolidated during printing.  */
  {
    test_diagnostic_context dc;
    rich_location richloc (line_table, expr);
    richloc.add_fixit_replace (open_paren, "CAST (");
    richloc.add_fixit_replace (close_paren, ") (");
    richloc.add_fixit_insert_after (")");

    ASSERT_STREQ ("   foo *f = (foo *)ptr->field;\n"
		  "                   ^~~~~~~~~~\n"
		  "            -\n"
		  "            CAST (-\n"
		  "                  ) (        )\n",
		  dc.test_show_locus (richloc));
  }

  /* Example where none are consolidated during printing.  */
  {
    test_diagnostic_context dc;
    rich_location richloc (line_table, expr);
    richloc.add_fixit_replace (open_paren, "CST (");
    richloc.add_fixit_replace (close_paren, ") (");
    richloc.add_fixit_insert_after (")");

    ASSERT_STREQ ("   foo *f = (foo *)ptr->field;\n"
		  "                   ^~~~~~~~~~\n"
		  "            -\n"
		  "            CST ( -\n"
		  "                  ) (        )\n",
		  dc.test_show_locus (richloc));
  }

  /* Example of deletion fix-it hints.  */
  {
    test_diagnostic_context dc;
    rich_location richloc (line_table, expr);
    richloc.add_fixit_insert_before (open_paren, "(bar *)");
    source_range victim = {open_paren, close_paren};
    richloc.add_fixit_remove (victim);

    /* This case is actually handled by fixit-consolidation,
       rather than by line_corrections.  */
    ASSERT_EQ (1, richloc.get_num_fixit_hints ());

    ASSERT_STREQ ("   foo *f = (foo *)ptr->field;\n"
		  "                   ^~~~~~~~~~\n"
		  "            -------\n"
		  "            (bar *)\n",
		  dc.test_show_locus (richloc));
  }

  /* Example of deletion fix-it hints that would overlap.  */
  {
    test_diagnostic_context dc;
    rich_location richloc (line_table, expr);
    richloc.add_fixit_insert_before (open_paren, "(longer *)");
    source_range victim = {expr_start, expr_finish};
    richloc.add_fixit_remove (victim);

    /* These fixits are not consolidated.  */
    ASSERT_EQ (2, richloc.get_num_fixit_hints ());

    /* But the corrections are.  */
    ASSERT_STREQ ("   foo *f = (foo *)ptr->field;\n"
		  "                   ^~~~~~~~~~\n"
		  "            -----------------\n"
		  "            (longer *)(foo *)\n",
		  dc.test_show_locus (richloc));
  }

  /* Example of insertion fix-it hints that would overlap.  */
  {
    test_diagnostic_context dc;
    rich_location richloc (line_table, expr);
    richloc.add_fixit_insert_before (open_paren, "LONGER THAN THE CAST");
    richloc.add_fixit_insert_after (close_paren, "TEST");

    /* The first insertion is long enough that if printed naively,
       it would overlap with the second.
       Verify that they are printed as a single replacement.  */
    ASSERT_STREQ ("   foo *f = (foo *)ptr->field;\n"
		  "                   ^~~~~~~~~~\n"
		  "            -------\n"
		  "            LONGER THAN THE CAST(foo *)TEST\n",
		  dc.test_show_locus (richloc));
  }
}

/* Multibyte-aware version of preceding tests.  See comments above
   test_one_liner_simple_caret_utf8() too, we use the same two multibyte
   characters here.  */

static void
test_overlapped_fixit_printing_utf8 (const line_table_case &case_)
{
  /* Create a tempfile and write some text to it.  */

  const char *content
    /* Display columns.
       00000000000000000000000111111111111111111111111222222222222222223
       12344444444555555556789012344444444555555556789012345678999999990  */
    = "  f\xf0\x9f\x98\x82 *f = (f\xf0\x9f\x98\x82 *)ptr->field\xcf\x80;\n";
    /* 00000000000000000000011111111111111111111112222222222333333333333
       12344445555666677778901234566667777888899990123456789012333344445
       Byte columns.  */

  temp_source_file tmp (SELFTEST_LOCATION, ".C", content);
  line_table_test ltt (case_);

  const line_map_ordinary *ord_map
    = linemap_check_ordinary (linemap_add (line_table, LC_ENTER, false,
					   tmp.get_filename (), 0));

  linemap_line_start (line_table, 1, 100);

  const location_t final_line_end
    = linemap_position_for_line_and_column (line_table, ord_map, 6, 50);

  /* Don't attempt to run the tests if column data might be unavailable.  */
  if (final_line_end > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  /* A test for converting a C-style cast to a C++-style cast.  */
  const location_t open_paren
    = linemap_position_for_line_and_column (line_table, ord_map, 1, 14);
  const location_t close_paren
    = linemap_position_for_line_and_column (line_table, ord_map, 1, 22);
  const location_t expr_start
    = linemap_position_for_line_and_column (line_table, ord_map, 1, 23);
  const location_t expr_finish
    = linemap_position_for_line_and_column (line_table, ord_map, 1, 34);
  const location_t expr = make_location (expr_start, expr_start, expr_finish);

  /* Various examples of fix-it hints that aren't themselves consolidated,
     but for which the *printing* may need consolidation.  */

  /* Example where 3 fix-it hints are printed as one.  */
  {
    test_diagnostic_context dc;
    file_cache &fc = dc.get_file_cache ();
    rich_location richloc (line_table, expr);
    richloc.add_fixit_replace (open_paren, "const_cast<");
    richloc.add_fixit_replace (close_paren, "> (");
    richloc.add_fixit_insert_after (")");

    ASSERT_STREQ ("   f\xf0\x9f\x98\x82"
			" *f = (f\xf0\x9f\x98\x82"
				  " *)ptr->field\xcf\x80"
						";\n"
		  "                   ^~~~~~~~~~~\n"
		  "            ------------------\n"
		  "            const_cast<f\xf0\x9f\x98\x82"
					    " *> (ptr->field\xcf\x80"
							    ")\n",
		  dc.test_show_locus (richloc));

    /* Unit-test the line_corrections machinery.  */
    char_display_policy policy (make_char_policy (dc, richloc));
    ASSERT_EQ (3, richloc.get_num_fixit_hints ());
    const fixit_hint *hint_0 = richloc.get_fixit_hint (0);
    ASSERT_EQ (column_range (14, 14),
	       get_affected_range (fc, policy, hint_0, CU_BYTES));
    ASSERT_EQ (column_range (12, 12),
	       get_affected_range (fc, policy, hint_0, CU_DISPLAY_COLS));
    ASSERT_EQ (column_range (12, 22), get_printed_columns (fc, policy, hint_0));
    const fixit_hint *hint_1 = richloc.get_fixit_hint (1);
    ASSERT_EQ (column_range (22, 22),
	       get_affected_range (fc, policy, hint_1, CU_BYTES));
    ASSERT_EQ (column_range (18, 18),
	       get_affected_range (fc, policy, hint_1, CU_DISPLAY_COLS));
    ASSERT_EQ (column_range (18, 20), get_printed_columns (fc, policy, hint_1));
    const fixit_hint *hint_2 = richloc.get_fixit_hint (2);
    ASSERT_EQ (column_range (35, 34),
	       get_affected_range (fc, policy, hint_2, CU_BYTES));
    ASSERT_EQ (column_range (30, 29),
	       get_affected_range (fc, policy, hint_2, CU_DISPLAY_COLS));
    ASSERT_EQ (column_range (30, 30), get_printed_columns (fc, policy, hint_2));

    /* Add each hint in turn to a line_corrections instance,
       and verify that they are consolidated into one correction instance
       as expected.  */
    line_corrections lc (fc, policy, tmp.get_filename (), 1);

    /* The first replace hint by itself.  */
    lc.add_hint (hint_0);
    ASSERT_EQ (1, lc.m_corrections.length ());
    ASSERT_EQ (column_range (14, 14), lc.m_corrections[0]->m_affected_bytes);
    ASSERT_EQ (column_range (12, 12), lc.m_corrections[0]->m_affected_columns);
    ASSERT_EQ (column_range (12, 22), lc.m_corrections[0]->m_printed_columns);
    ASSERT_STREQ ("const_cast<", lc.m_corrections[0]->m_text);

    /* After the second replacement hint, they are printed together
       as a replacement (along with the text between them).  */
    lc.add_hint (hint_1);
    ASSERT_EQ (1, lc.m_corrections.length ());
    ASSERT_STREQ ("const_cast<f\xf0\x9f\x98\x82 *> (",
		  lc.m_corrections[0]->m_text);
    ASSERT_EQ (column_range (14, 22), lc.m_corrections[0]->m_affected_bytes);
    ASSERT_EQ (column_range (12, 18), lc.m_corrections[0]->m_affected_columns);
    ASSERT_EQ (column_range (12, 30), lc.m_corrections[0]->m_printed_columns);

    /* After the final insertion hint, they are all printed together
       as a replacement (along with the text between them).  */
    lc.add_hint (hint_2);
    ASSERT_STREQ ("const_cast<f\xf0\x9f\x98\x82 *> (ptr->field\xcf\x80)",
		  lc.m_corrections[0]->m_text);
    ASSERT_EQ (1, lc.m_corrections.length ());
    ASSERT_EQ (column_range (14, 34), lc.m_corrections[0]->m_affected_bytes);
    ASSERT_EQ (column_range (12, 29), lc.m_corrections[0]->m_affected_columns);
    ASSERT_EQ (column_range (12, 42), lc.m_corrections[0]->m_printed_columns);
  }

  /* Example where two are consolidated during printing.  */
  {
    test_diagnostic_context dc;
    rich_location richloc (line_table, expr);
    richloc.add_fixit_replace (open_paren, "CAST (");
    richloc.add_fixit_replace (close_paren, ") (");
    richloc.add_fixit_insert_after (")");

    ASSERT_STREQ ("   f\xf0\x9f\x98\x82"
			" *f = (f\xf0\x9f\x98\x82"
				  " *)ptr->field\xcf\x80"
						";\n"
		  "                   ^~~~~~~~~~~\n"
		  "            -\n"
		  "            CAST (-\n"
		  "                  ) (         )\n",
		  dc.test_show_locus (richloc));
  }

  /* Example where none are consolidated during printing.  */
  {
    test_diagnostic_context dc;
    rich_location richloc (line_table, expr);
    richloc.add_fixit_replace (open_paren, "CST (");
    richloc.add_fixit_replace (close_paren, ") (");
    richloc.add_fixit_insert_after (")");

    ASSERT_STREQ ("   f\xf0\x9f\x98\x82"
			" *f = (f\xf0\x9f\x98\x82"
				  " *)ptr->field\xcf\x80"
						";\n"
		  "                   ^~~~~~~~~~~\n"
		  "            -\n"
		  "            CST ( -\n"
		  "                  ) (         )\n",
		  dc.test_show_locus (richloc));
  }

  /* Example of deletion fix-it hints.  */
  {
    test_diagnostic_context dc;
    rich_location richloc (line_table, expr);
    richloc.add_fixit_insert_before (open_paren, "(bar\xf0\x9f\x98\x82 *)");
    source_range victim = {open_paren, close_paren};
    richloc.add_fixit_remove (victim);

    /* This case is actually handled by fixit-consolidation,
       rather than by line_corrections.  */
    ASSERT_EQ (1, richloc.get_num_fixit_hints ());

    ASSERT_STREQ ("   f\xf0\x9f\x98\x82"
			" *f = (f\xf0\x9f\x98\x82"
				  " *)ptr->field\xcf\x80"
						";\n"
		  "                   ^~~~~~~~~~~\n"
		  "            -------\n"
		  "            (bar\xf0\x9f\x98\x82"
				    " *)\n",
		  dc.test_show_locus (richloc));
  }

  /* Example of deletion fix-it hints that would overlap.  */
  {
    test_diagnostic_context dc;
    rich_location richloc (line_table, expr);
    richloc.add_fixit_insert_before (open_paren, "(long\xf0\x9f\x98\x82 *)");
    source_range victim = {expr_start, expr_finish};
    richloc.add_fixit_remove (victim);

    /* These fixits are not consolidated.  */
    ASSERT_EQ (2, richloc.get_num_fixit_hints ());

    /* But the corrections are.  */
    ASSERT_STREQ ("   f\xf0\x9f\x98\x82"
			" *f = (f\xf0\x9f\x98\x82"
				  " *)ptr->field\xcf\x80"
						";\n"
		  "                   ^~~~~~~~~~~\n"
		  "            ------------------\n"
		  "            (long\xf0\x9f\x98\x82"
				     " *)(f\xf0\x9f\x98\x82"
					    " *)\n",
		  dc.test_show_locus (richloc));
  }

  /* Example of insertion fix-it hints that would overlap.  */
  {
    test_diagnostic_context dc;
    rich_location richloc (line_table, expr);
    richloc.add_fixit_insert_before
      (open_paren, "L\xf0\x9f\x98\x82NGER THAN THE CAST");
    richloc.add_fixit_insert_after (close_paren, "TEST");

    /* The first insertion is long enough that if printed naively,
       it would overlap with the second.
       Verify that they are printed as a single replacement.  */
    ASSERT_STREQ ("   f\xf0\x9f\x98\x82"
			" *f = (f\xf0\x9f\x98\x82"
				  " *)ptr->field\xcf\x80"
						";\n"
		  "                   ^~~~~~~~~~~\n"
		  "            -------\n"
		  "            L\xf0\x9f\x98\x82"
				 "NGER THAN THE CAST(f\xf0\x9f\x98\x82"
						       " *)TEST\n",
		  dc.test_show_locus (richloc));
  }
}

/* Verify that the line_corrections machinery correctly prints
   overlapping fixit-hints that have been added in the wrong
   order.
   Adapted from PR c/81405 seen on gcc.dg/init-excess-1.c*/

static void
test_overlapped_fixit_printing_2 (const line_table_case &case_)
{
  /* Create a tempfile and write some text to it.
     ...000000000111111111122222222223333333333.
     ...123456789012345678901234567890123456789.  */
  const char *content
    = ("int a5[][0][0] = { 1, 2 };\n");
  temp_source_file tmp (SELFTEST_LOCATION, ".c", content);
  line_table_test ltt (case_);

  const line_map_ordinary *ord_map
    = linemap_check_ordinary (linemap_add (line_table, LC_ENTER, false,
					   tmp.get_filename (), 0));

  linemap_line_start (line_table, 1, 100);

  const location_t final_line_end
    = linemap_position_for_line_and_column (line_table, ord_map, 1, 100);

  /* Don't attempt to run the tests if column data might be unavailable.  */
  if (final_line_end > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  const location_t col_1
    = linemap_position_for_line_and_column (line_table, ord_map, 1, 1);
  const location_t col_20
    = linemap_position_for_line_and_column (line_table, ord_map, 1, 20);
  const location_t col_21
    = linemap_position_for_line_and_column (line_table, ord_map, 1, 21);
  const location_t col_23
    = linemap_position_for_line_and_column (line_table, ord_map, 1, 23);
  const location_t col_25
    = linemap_position_for_line_and_column (line_table, ord_map, 1, 25);

  /* Two insertions, in the wrong order.  */
  {
    test_diagnostic_context dc;
    file_cache &fc = dc.get_file_cache ();

    rich_location richloc (line_table, col_20);
    richloc.add_fixit_insert_before (col_23, "{");
    richloc.add_fixit_insert_before (col_21, "}");

    /* These fixits should be accepted; they can't be consolidated.  */
    char_display_policy policy (make_char_policy (dc, richloc));
    ASSERT_EQ (2, richloc.get_num_fixit_hints ());
    const fixit_hint *hint_0 = richloc.get_fixit_hint (0);
    ASSERT_EQ (column_range (23, 22),
	       get_affected_range (fc, policy, hint_0, CU_BYTES));
    ASSERT_EQ (column_range (23, 23), get_printed_columns (fc, policy, hint_0));
    const fixit_hint *hint_1 = richloc.get_fixit_hint (1);
    ASSERT_EQ (column_range (21, 20),
	       get_affected_range (fc, policy, hint_1, CU_BYTES));
    ASSERT_EQ (column_range (21, 21), get_printed_columns (fc, policy, hint_1));

    /* Verify that they're printed correctly.  */
    ASSERT_STREQ (" int a5[][0][0] = { 1, 2 };\n"
		  "                    ^\n"
		  "                     } {\n",
		  dc.test_show_locus (richloc));
  }

  /* Various overlapping insertions, some occurring "out of order"
     (reproducing the fix-it hints from PR c/81405).  */
  {
    test_diagnostic_context dc;
    rich_location richloc (line_table, col_20);

    richloc.add_fixit_insert_before (col_20, "{{");
    richloc.add_fixit_insert_before (col_21, "}}");
    richloc.add_fixit_insert_before (col_23, "{");
    richloc.add_fixit_insert_before (col_21, "}");
    richloc.add_fixit_insert_before (col_23, "{{");
    richloc.add_fixit_insert_before (col_25, "}");
    richloc.add_fixit_insert_before (col_21, "}");
    richloc.add_fixit_insert_before (col_1, "{");
    richloc.add_fixit_insert_before (col_25, "}");

    ASSERT_STREQ (" int a5[][0][0] = { 1, 2 };\n"
		  "                    ^\n"
		  " {                  -----\n"
		  "                    {{1}}}}, {{{2 }}\n",
		  dc.test_show_locus (richloc));
  }
}

/* Insertion fix-it hint: adding a "break;" on a line by itself.  */

static void
test_fixit_insert_containing_newline (const line_table_case &case_)
{
  /* Create a tempfile and write some text to it.
     .........................0000000001111111.
     .........................1234567890123456.  */
  const char *old_content = ("    case 'a':\n" /* line 1. */
			     "      x = a;\n"  /* line 2. */
			     "    case 'b':\n" /* line 3. */
			     "      x = b;\n");/* line 4. */

  temp_source_file tmp (SELFTEST_LOCATION, ".c", old_content);
  line_table_test ltt (case_);
  linemap_add (line_table, LC_ENTER, false, tmp.get_filename (), 3);

  location_t case_start = linemap_position_for_column (line_table, 5);
  location_t case_finish = linemap_position_for_column (line_table, 13);
  location_t case_loc = make_location (case_start, case_start, case_finish);
  location_t line_start = linemap_position_for_column (line_table, 1);

  if (case_finish > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  /* Add a "break;" on a line by itself before line 3 i.e. before
     column 1 of line 3. */
  {
    rich_location richloc (line_table, case_loc);
    richloc.add_fixit_insert_before (line_start, "      break;\n");

    /* Without line numbers.  */
    {
      test_diagnostic_context dc;
      ASSERT_STREQ ("       x = a;\n"
		    "+      break;\n"
		    "     case 'b':\n"
		    "     ^~~~~~~~~\n",
		    dc.test_show_locus (richloc));
    }

    /* With line numbers.  */
    {
      test_diagnostic_context dc;
      dc.m_source_printing.show_line_numbers_p = true;
      ASSERT_STREQ ("    2 |       x = a;\n"
		    "  +++ |+      break;\n"
		    "    3 |     case 'b':\n"
		    "      |     ^~~~~~~~~\n",
		    dc.test_show_locus (richloc));
    }
  }

  /* Verify that attempts to add text with a newline fail when the
     insertion point is *not* at the start of a line.  */
  {
    rich_location richloc (line_table, case_loc);
    richloc.add_fixit_insert_before (case_start, "break;\n");
    ASSERT_TRUE (richloc.seen_impossible_fixit_p ());
    test_diagnostic_context dc;
    ASSERT_STREQ ("     case 'b':\n"
		  "     ^~~~~~~~~\n",
		  dc.test_show_locus (richloc));
  }
}

/* Insertion fix-it hint: adding a "#include <stdio.h>\n" to the top
   of the file, where the fix-it is printed in a different line-span
   to the primary range of the diagnostic.  */

static void
test_fixit_insert_containing_newline_2 (const line_table_case &case_)
{
  /* Create a tempfile and write some text to it.
     .........................0000000001111111.
     .........................1234567890123456.  */
  const char *old_content = ("test (int ch)\n"  /* line 1. */
			     "{\n"              /* line 2. */
			     " putchar (ch);\n" /* line 3. */
			     "}\n");            /* line 4. */

  temp_source_file tmp (SELFTEST_LOCATION, ".c", old_content);
  line_table_test ltt (case_);

  const line_map_ordinary *ord_map = linemap_check_ordinary
    (linemap_add (line_table, LC_ENTER, false, tmp.get_filename (), 0));
  linemap_line_start (line_table, 1, 100);

  /* The primary range is the "putchar" token.  */
  location_t putchar_start
    = linemap_position_for_line_and_column (line_table, ord_map, 3, 2);
  location_t putchar_finish
    = linemap_position_for_line_and_column (line_table, ord_map, 3, 8);
  location_t putchar_loc
    = make_location (putchar_start, putchar_start, putchar_finish);
  rich_location richloc (line_table, putchar_loc);

  /* Add a "#include <stdio.h>" on a line by itself at the top of the file.  */
  location_t file_start
     = linemap_position_for_line_and_column (line_table, ord_map,  1, 1);
  richloc.add_fixit_insert_before (file_start, "#include <stdio.h>\n");

  if (putchar_finish > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  {
    test_diagnostic_context dc;
    ASSERT_STREQ ("FILENAME:1:1:\n"
		  "+#include <stdio.h>\n"
		  " test (int ch)\n"
		  "FILENAME:3:2:\n"
		  "  putchar (ch);\n"
		  "  ^~~~~~~\n",
		  dc.test_show_locus (richloc));
  }

  /* With line-numbering, the line spans are close enough to be
     consolidated, since it makes little sense to skip line 2.  */
  {
    test_diagnostic_context dc;
    dc.m_source_printing.show_line_numbers_p = true;
    ASSERT_STREQ ("  +++ |+#include <stdio.h>\n"
		  "    1 | test (int ch)\n"
		  "    2 | {\n"
		  "    3 |  putchar (ch);\n"
		  "      |  ^~~~~~~\n",
		  dc.test_show_locus (richloc));
  }
}

/* Replacement fix-it hint containing a newline.
   This will fail, as newlines are only supported when inserting at the
   beginning of a line.  */

static void
test_fixit_replace_containing_newline (const line_table_case &case_)
{
  /* Create a tempfile and write some text to it.
    .........................0000000001111.
    .........................1234567890123.  */
  const char *old_content = "foo = bar ();\n";

  temp_source_file tmp (SELFTEST_LOCATION, ".c", old_content);
  line_table_test ltt (case_);
  linemap_add (line_table, LC_ENTER, false, tmp.get_filename (), 1);

  /* Replace the " = " with "\n  = ", as if we were reformatting an
     overly long line.  */
  location_t start = linemap_position_for_column (line_table, 4);
  location_t finish = linemap_position_for_column (line_table, 6);
  location_t loc = linemap_position_for_column (line_table, 13);
  rich_location richloc (line_table, loc);
  source_range range = source_range::from_locations (start, finish);
  richloc.add_fixit_replace (range, "\n =");

  /* Arbitrary newlines are not yet supported within fix-it hints, so
     the fix-it should not be displayed.  */
  ASSERT_TRUE (richloc.seen_impossible_fixit_p ());

  if (finish > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  test_diagnostic_context dc;
  ASSERT_STREQ (" foo = bar ();\n"
		"             ^\n",
		dc.test_show_locus (richloc));
}

/* Fix-it hint, attempting to delete a newline.
   This will fail, as we currently only support fix-it hints that
   affect one line at a time.  */

static void
test_fixit_deletion_affecting_newline (const line_table_case &case_)
{
  /* Create a tempfile and write some text to it.
    ..........................0000000001111.
    ..........................1234567890123.  */
  const char *old_content = ("foo = bar (\n"
			     "      );\n");

  temp_source_file tmp (SELFTEST_LOCATION, ".c", old_content);
  line_table_test ltt (case_);
  const line_map_ordinary *ord_map = linemap_check_ordinary
    (linemap_add (line_table, LC_ENTER, false, tmp.get_filename (), 0));
  linemap_line_start (line_table, 1, 100);

  /* Attempt to delete the " (\n...)".  */
  location_t start
    = linemap_position_for_line_and_column (line_table, ord_map, 1, 10);
  location_t caret
    = linemap_position_for_line_and_column (line_table, ord_map, 1, 11);
  location_t finish
    = linemap_position_for_line_and_column (line_table, ord_map, 2, 7);
  location_t loc = make_location (caret, start, finish);
  rich_location richloc (line_table, loc);
  richloc. add_fixit_remove ();

  /* Fix-it hints that affect more than one line are not yet supported, so
     the fix-it should not be displayed.  */
  ASSERT_TRUE (richloc.seen_impossible_fixit_p ());

  if (finish > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  test_diagnostic_context dc;
  ASSERT_STREQ (" foo = bar (\n"
		"          ~^\n"
		"       );\n"
		"       ~    \n",
		dc.test_show_locus (richloc));
}

static void
test_tab_expansion (const line_table_case &case_)
{
  /* Create a tempfile and write some text to it.  This example uses a tabstop
     of 8, as the column numbers attempt to indicate:

    .....................000.01111111111.22222333333  display
    .....................123.90123456789.56789012345  columns  */
  const char *content = "  \t   This: `\t' is a tab.\n";
  /* ....................000 00000011111 11111222222  byte
     ....................123 45678901234 56789012345  columns  */

  const int tabstop = 8;
  cpp_char_column_policy policy (tabstop, cpp_wcwidth);
  const int first_non_ws_byte_col = 7;
  const int right_quote_byte_col = 15;
  const int last_byte_col = 25;
  ASSERT_EQ (35, cpp_display_width (content, last_byte_col, policy));

  temp_source_file tmp (SELFTEST_LOCATION, ".c", content);
  line_table_test ltt (case_);
  linemap_add (line_table, LC_ENTER, false, tmp.get_filename (), 1);

  /* Don't attempt to run the tests if column data might be unavailable.  */
  location_t line_end = linemap_position_for_column (line_table, last_byte_col);
  if (line_end > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  /* Check that the leading whitespace with mixed tabs and spaces is expanded
     into 11 spaces.  Recall that print_line() also puts one space before
     everything too.  */
  {
    test_diagnostic_context dc;
    dc.m_tabstop = tabstop;
    rich_location richloc (line_table,
			   linemap_position_for_column (line_table,
							first_non_ws_byte_col));
    ASSERT_STREQ ("            This: `      ' is a tab.\n"
		  "            ^\n",
		  dc.test_show_locus (richloc));
  }

  /* Confirm the display width was tracked correctly across the internal tab
     as well.  */
  {
    test_diagnostic_context dc;
    dc.m_tabstop = tabstop;
    rich_location richloc (line_table,
			   linemap_position_for_column (line_table,
							right_quote_byte_col));
    ASSERT_STREQ ("            This: `      ' is a tab.\n"
		  "                         ^\n",
		  dc.test_show_locus (richloc));
  }
}

/* Verify that the escaping machinery can cope with a variety of different
   invalid bytes.  */

static void
test_escaping_bytes_1 (const line_table_case &case_)
{
  const char content[] = "before\0\1\2\3\v\x80\xff""after\n";
  const size_t sz = sizeof (content);
  temp_source_file tmp (SELFTEST_LOCATION, ".c", content, sz);
  line_table_test ltt (case_);
  const line_map_ordinary *ord_map = linemap_check_ordinary
    (linemap_add (line_table, LC_ENTER, false, tmp.get_filename (), 0));
  linemap_line_start (line_table, 1, 100);

  location_t finish
    = linemap_position_for_line_and_column (line_table, ord_map, 1,
					    strlen (content));

  if (finish > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  /* Locations of the NUL and \v bytes.  */
  location_t nul_loc
    = linemap_position_for_line_and_column (line_table, ord_map, 1, 7);
  location_t v_loc
    = linemap_position_for_line_and_column (line_table, ord_map, 1, 11);
  gcc_rich_location richloc (nul_loc);
  richloc.add_range (v_loc);

  {
    test_diagnostic_context dc;
    ASSERT_STREQ (" before \1\2\3\v\x80\xff""after\n"
		  "       ^   ~\n",
		  dc.test_show_locus (richloc));
  }
  richloc.set_escape_on_output (true);
  {
    test_diagnostic_context dc;
    dc.set_escape_format (DIAGNOSTICS_ESCAPE_FORMAT_UNICODE);
    ASSERT_STREQ
      (" before<U+0000><U+0001><U+0002><U+0003><U+000B><80><ff>after\n"
       "       ^~~~~~~~                        ~~~~~~~~\n",
       dc.test_show_locus (richloc));
  }
  {
    test_diagnostic_context dc;
    dc.set_escape_format (DIAGNOSTICS_ESCAPE_FORMAT_BYTES);
    ASSERT_STREQ (" before<00><01><02><03><0b><80><ff>after\n"
		  "       ^~~~            ~~~~\n",
		  dc.test_show_locus (richloc));
  }
}

/* As above, but verify that we handle the initial byte of a line
   correctly.  */

static void
test_escaping_bytes_2 (const line_table_case &case_)
{
  const char content[]  = "\0after\n";
  const size_t sz = sizeof (content);
  temp_source_file tmp (SELFTEST_LOCATION, ".c", content, sz);
  line_table_test ltt (case_);
  const line_map_ordinary *ord_map = linemap_check_ordinary
    (linemap_add (line_table, LC_ENTER, false, tmp.get_filename (), 0));
  linemap_line_start (line_table, 1, 100);

  location_t finish
    = linemap_position_for_line_and_column (line_table, ord_map, 1,
					    strlen (content));

  if (finish > LINE_MAP_MAX_LOCATION_WITH_COLS)
    return;

  /* Location of the NUL byte.  */
  location_t nul_loc
    = linemap_position_for_line_and_column (line_table, ord_map, 1, 1);
  gcc_rich_location richloc (nul_loc);

  {
    test_diagnostic_context dc;
    ASSERT_STREQ ("  after\n"
		  " ^\n",
		  dc.test_show_locus (richloc));
  }
  richloc.set_escape_on_output (true);
  {
    test_diagnostic_context dc;
    dc.set_escape_format (DIAGNOSTICS_ESCAPE_FORMAT_UNICODE);
    ASSERT_STREQ (" <U+0000>after\n"
		  " ^~~~~~~~\n",
		  dc.test_show_locus (richloc));
  }
  {
    test_diagnostic_context dc;
    dc.set_escape_format (DIAGNOSTICS_ESCAPE_FORMAT_BYTES);
    ASSERT_STREQ (" <00>after\n"
		  " ^~~~\n",
		  dc.test_show_locus (richloc));
  }
}

/* Verify that line numbers are correctly printed for the case of
   a multiline range in which the width of the line numbers changes
   (e.g. from "9" to "10").  */

static void
test_line_numbers_multiline_range ()
{
  /* Create a tempfile and write some text to it.  */
  pretty_printer pp;
  for (int i = 0; i < 20; i++)
    /* .........0000000001111111.
   .............1234567890123456.  */
    pp_printf (&pp, "this is line %i\n", i + 1);
  temp_source_file tmp (SELFTEST_LOCATION, ".txt", pp_formatted_text (&pp));
  line_table_test ltt;

  const line_map_ordinary *ord_map = linemap_check_ordinary
    (linemap_add (line_table, LC_ENTER, false, tmp.get_filename (), 0));
  linemap_line_start (line_table, 1, 100);

  /* Create a multi-line location, starting at the "line" of line 9, with
     a caret on the "is" of line 10, finishing on the "this" line 11.  */

  location_t start
    = linemap_position_for_line_and_column (line_table, ord_map, 9, 9);
  location_t caret
    = linemap_position_for_line_and_column (line_table, ord_map, 10, 6);
  location_t finish
    = linemap_position_for_line_and_column (line_table, ord_map, 11, 4);
  location_t loc = make_location (caret, start, finish);

  test_diagnostic_context dc;
  dc.m_source_printing.show_line_numbers_p = true;
  dc.m_source_printing.min_margin_width = 0;
  gcc_rich_location richloc (loc);
  ASSERT_STREQ (" 9 | this is line 9\n"
		"   |         ~~~~~~\n"
		"10 | this is line 10\n"
		"   | ~~~~~^~~~~~~~~~\n"
		"11 | this is line 11\n"
		"   | ~~~~  \n",
		dc.test_show_locus (richloc));
}

/* Run all of the selftests within this file.  */

void
diagnostic_show_locus_cc_tests ()
{
  test_line_span ();

  test_layout_range_for_single_point ();
  test_layout_range_for_single_line ();
  test_layout_range_for_multiple_lines ();

  test_display_widths ();

  for_each_line_table_case (test_layout_x_offset_display_utf8);
  for_each_line_table_case (test_layout_x_offset_display_tab);

  test_get_line_bytes_without_trailing_whitespace ();

  test_diagnostic_show_locus_unknown_location ();

  for_each_line_table_case (test_diagnostic_show_locus_one_liner);
  for_each_line_table_case (test_diagnostic_show_locus_one_liner_utf8);
  for_each_line_table_case (test_add_location_if_nearby);
  for_each_line_table_case (test_diagnostic_show_locus_fixit_lines);
  for_each_line_table_case (test_fixit_consolidation);
  for_each_line_table_case (test_overlapped_fixit_printing);
  for_each_line_table_case (test_overlapped_fixit_printing_utf8);
  for_each_line_table_case (test_overlapped_fixit_printing_2);
  for_each_line_table_case (test_fixit_insert_containing_newline);
  for_each_line_table_case (test_fixit_insert_containing_newline_2);
  for_each_line_table_case (test_fixit_replace_containing_newline);
  for_each_line_table_case (test_fixit_deletion_affecting_newline);
  for_each_line_table_case (test_tab_expansion);
  for_each_line_table_case (test_escaping_bytes_1);
  for_each_line_table_case (test_escaping_bytes_2);

  test_line_numbers_multiline_range ();
}

} // namespace selftest

#endif /* #if CHECKING_P */

#if __GNUC__ >= 10
#  pragma GCC diagnostic pop
#endif
