/* Support for tabular/grid-based content.
   Copyright (C) 2023-2024 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

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
#include "make-unique.h"
#include "pretty-print.h"
#include "diagnostic.h"
#include "selftest.h"
#include "text-art/selftests.h"
#include "text-art/table.h"

using namespace text_art;

/* class text_art::table_cell_content.  */

table_cell_content::table_cell_content (styled_string &&s)
: m_str (std::move (s)),
  /* We assume here that the content occupies a single canvas row.  */
  m_size (m_str.calc_canvas_width (), 1)
{
}

void
table_cell_content::paint_to_canvas (canvas &canvas,
				     canvas::coord_t top_left) const
{
  canvas.paint_text (top_left, m_str);
}

/* struct text_art::table_dimension_sizes.  */

table_dimension_sizes::table_dimension_sizes (unsigned num)
: m_requirements (num, 0)
{
}

/* class text_art::table::cell_placement.  */

void
table::cell_placement::paint_cell_contents_to_canvas(canvas &canvas,
						     canvas::coord_t offset,
						     const table_geometry &tg) const
{
  const canvas::size_t req_canvas_size = get_min_canvas_size ();
  const canvas::size_t alloc_canvas_size = tg.get_canvas_size (m_rect);
  gcc_assert (req_canvas_size.w <= alloc_canvas_size.w);
  gcc_assert (req_canvas_size.h <= alloc_canvas_size.h);
  const int x_padding = alloc_canvas_size.w - req_canvas_size.w;
  const int y_padding = alloc_canvas_size.h - req_canvas_size.h;
  const table::coord_t table_top_left = m_rect.m_top_left;
  const canvas::coord_t canvas_top_left = tg.table_to_canvas (table_top_left);

  gcc_assert (x_padding >= 0);
  int x_align_offset;
  switch (m_x_align)
    {
    default:
      gcc_unreachable ();
    case x_align::LEFT:
      x_align_offset = 0;
      break;
    case x_align::CENTER:
      x_align_offset = x_padding / 2;
      break;
    case x_align::RIGHT:
      x_align_offset = x_padding;
      break;
    }

  gcc_assert (y_padding >= 0);
  int y_align_offset;
  switch (m_y_align)
    {
    default:
      gcc_unreachable ();
    case y_align::TOP:
      y_align_offset = 0;
      break;
    case y_align::CENTER:
      y_align_offset = y_padding / 2;
      break;
    case y_align::BOTTOM:
      y_align_offset = y_padding;
      break;
    }
  const canvas::coord_t content_rel_coord
    (canvas_top_left.x + 1 + x_align_offset,
     canvas_top_left.y + 1 + y_align_offset);
  m_content.paint_to_canvas (canvas, offset + content_rel_coord);
}

/* class text_art::table.  */


table::table (size_t size)
: m_size (size),
  m_placements (),
  m_occupancy (size)
{
  m_occupancy.fill (-1);
}

void
table::set_cell (coord_t coord,
		 table_cell_content &&content,
		 enum x_align x_align,
		 enum y_align y_align)
{
  set_cell_span (rect_t (coord, table::size_t (1, 1)),
		 std::move (content), x_align, y_align);
}

void
table::set_cell_span (rect_t span,
		      table_cell_content &&content,
		      enum x_align x_align,
		      enum y_align y_align)
{
  gcc_assert (span.m_size.w > 0);
  gcc_assert (span.m_size.h > 0);
  int placement_idx = m_placements.size ();
  m_placements.emplace_back (cell_placement (span, std::move (content),
					     x_align, y_align));
  for (int y = span.get_min_y (); y < span.get_next_y (); y++)
    for (int x = span.get_min_x (); x < span.get_next_x (); x++)
      {
	gcc_assert (m_occupancy.get (coord_t (x, y)) == -1);
	m_occupancy.set (coord_t (x, y), placement_idx);
      }
}

/* If SPAN is unoccuped, set it to CONTENT.
   Otherwise, discard CONTENT.  */

void
table::maybe_set_cell_span (rect_t span,
			    table_cell_content &&content,
			    enum x_align x_align,
			    enum y_align y_align)
{
  gcc_assert (span.m_size.w > 0);
  gcc_assert (span.m_size.h > 0);
  for (int y = span.get_min_y (); y < span.get_next_y (); y++)
    for (int x = span.get_min_x (); x < span.get_next_x (); x++)
      {
	if (m_occupancy.get (coord_t (x, y)) != -1)
	  return;
      }
  set_cell_span (span, std::move (content), x_align, y_align);
}

canvas
table::to_canvas (const theme &theme, const style_manager &sm) const
{
  table_dimension_sizes col_widths (m_size.w);
  table_dimension_sizes row_heights (m_size.h);
  table_cell_sizes cell_sizes (col_widths, row_heights);
  cell_sizes.pass_1 (*this);
  cell_sizes.pass_2 (*this);
  table_geometry tg (*this, cell_sizes);
  canvas canvas (tg.get_canvas_size (), sm);
  paint_to_canvas (canvas, canvas::coord_t (0, 0), tg, theme);
  return canvas;
}

void
table::paint_to_canvas (canvas &canvas,
			canvas::coord_t offset,
			const table_geometry &tg,
			const theme &theme) const
{
  canvas.fill (canvas::rect_t (offset, tg.get_canvas_size ()),
	       styled_unichar (' '));
  paint_cell_borders_to_canvas (canvas, offset, tg, theme);
  paint_cell_contents_to_canvas (canvas, offset, tg);
}

/* Print this table to stderr.  */

DEBUG_FUNCTION void
table::debug () const
{
  /* Use a temporary style manager.
     Styles in the table will be meaningless, so
     print the canvas with styling disabled.  */
  style_manager sm;
  canvas canvas (to_canvas (unicode_theme (), sm));
  canvas.debug (false);
}

/* Move OTHER's content this table, starting at OFFSET.  */

void
table::add_other_table (table &&other,
			table::coord_t offset)
{
  for (auto &&placement : other.m_placements)
    {
      set_cell_span (placement.m_rect + offset,
		     std::move (placement.m_content),
		     placement.m_x_align,
		     placement.m_y_align);
    }
}

const table::cell_placement *
table::get_placement_at (coord_t coord) const
{
  const int placement_idx = m_occupancy.get (coord);
  if (placement_idx == -1)
    return nullptr;
  return &m_placements[placement_idx];
}

int
table::get_occupancy_safe (coord_t coord) const
{
  if (coord.x < 0)
    return -1;
  if (coord.x >= m_size.w)
    return -1;
  if (coord.y < 0)
    return -1;
  if (coord.y >= m_size.h)
    return -1;
  return m_occupancy.get (coord);
}

/* Determine if the "?" edges need borders for table cell D
   in the following, for the directions relative to "X", based
   on whether each of table cell boundaries AB, CD, AC, and BD
   are boundaries between cell spans:

   #            up?
   #      +-----+-----+
   #      |           |
   #      |     ?     |
   #      |  A  ?  B  |
   #      |     ?     |
   #      |           |
   # left?+ ??? X ??? + right?
   #      |           |
   #      |     ?     |
   #      |  C  ?  D  |
   #      |     ?     |
   #      |           |
   #      +-----+-----+
   #          down?
*/

directions
table::get_connections (int table_x, int table_y) const
{
  int cell_a = get_occupancy_safe (coord_t (table_x - 1, table_y - 1));
  int cell_b = get_occupancy_safe (coord_t (table_x, table_y - 1));
  int cell_c = get_occupancy_safe (coord_t (table_x - 1, table_y));
  int cell_d = get_occupancy_safe (coord_t (table_x, table_y));
  const bool up = (cell_a != cell_b);
  const bool down = (cell_c != cell_d);
  const bool left = (cell_a != cell_c);
  const bool right = (cell_b != cell_d);
  return directions (up, down, left, right);
}

/* Paint the grid lines.

   Consider painting
   - a grid of cells,
   - plus a right-hand border
   - and a bottom border

   Then we need to paint to the canvas like this:

   #         PER-TABLE-COLUMN     R BORDER
   #      +-------------------+   +-----+
   #
   #             TABLE CELL WIDTH (in canvas units)
   #            +-------------+
   #      .     .     . .     .   .     .
   #   ...+-----+-----+.+-----+...+-----+ +
   #      |  U  |     |.|     |   |  U  | |
   #      |  U  |     |.|     |   |  U  | |
   #      |LL+RR|RRRRR|.|RRRRR|   |LL+  | |
   #      |  D  |     |.|     |   |  D  | |
   #      |  D  |     |.|     |   |  D  | |
   #   ...+-----+-----+.+-----+...+-----+ |
   #      .....................   ......  +-- PER-TABLE-ROW
   #   ...+-----+-----+.+-----+...+-----+ | +
   #      |  D  |     |.|     |   |  D  | | |
   #      |  D  |     |.|     |   |  D  | | |
   #      |  D  |     |.|     |   |  D  | | +---- TABLE CELL HEIGHT (in canvas units)
   #      |  D  |     |.|     |   |  D  | | |
   #      |  D  |     |.|     |   |  D  | | |
   #   ...+-----+-----+.+-----+...+-----+ + +
   #      .     .     .     .   .     .
   #   ...+-----+-----+.+-----+...+-----+  +
   #      |  D  |     |.|     |   |  U  |  |
   #      |  D  |     |.|     |   |  U  |  |
   #      |LL+RR|RRRRR|.|RRRRR|   |LL+  |  | BOTTOM BORDER
   #      |     |     |.|     |   |     |  |
   #      |     |     |.|     |   |     |  |
   #   ...+-----+-----+.+-----+...+-----+  +

   where each:

   #    +-----+
   #    |     |
   #    |     |
   #    |     |
   #    |     |
   #    |     |
   #    +-----+

   is a canvas cell, and the U, L, R, D express the connections
   that are present with neighboring table cells.  These affect
   the kinds of borders that we draw for a particular table cell.  */

void
table::paint_cell_borders_to_canvas (canvas &canvas,
				     canvas::coord_t offset,
				     const table_geometry &tg,
				     const theme &theme) const
{
  /* The per-table-cell left and top borders are either drawn or not,
     but if they are, they aren't affected by per-table-cell connections.  */
  const canvas::cell_t left_border
    = theme.get_line_art (directions (true, /* up */
				      true, /* down */
				      false, /* left */
				      false /* right */));
  const canvas::cell_t top_border
    = theme.get_line_art (directions (false, /* up */
				      false, /* down */
				      true, /* left */
				      true)); /* right */
  for (int table_y = 0; table_y < m_size.h; table_y++)
    {
      const int canvas_y = tg.table_y_to_canvas_y (table_y);
      for (int table_x = 0; table_x < m_size.w; table_x++)
	{
	  canvas::coord_t canvas_top_left
	    = tg.table_to_canvas(table::coord_t (table_x, table_y));

	  const directions c (get_connections (table_x, table_y));

	  /* Paint top-left corner of border, if any.  */
	  canvas.paint (offset + canvas_top_left,
			theme.get_line_art (c));

	  /* Paint remainder of left border of cell, if any.
	     We assume here that the content occupies a single canvas row.  */
	  if (c.m_down)
	    canvas.paint (offset + canvas::coord_t (canvas_top_left.x,
						    canvas_y + 1),
			  left_border);

	  /* Paint remainder of top border of cell, if any.  */
	  if (c.m_right)
	    {
	      const int col_width = tg.get_col_width (table_x);
	      for (int x_offset = 0; x_offset < col_width; x_offset++)
		{
		  const int canvas_x = canvas_top_left.x + 1 + x_offset;
		  canvas.paint (offset + canvas::coord_t (canvas_x, canvas_y),
				top_border);
		}
	    }
	}

      /* Paint right-hand border of row.  */
      const int table_x = m_size.w;
      const int canvas_x = tg.table_x_to_canvas_x (table_x);
      const directions c (get_connections (m_size.w, table_y));
      canvas.paint(offset + canvas::coord_t (canvas_x, canvas_y),
		   theme.get_line_art (directions (c.m_up,
						   c.m_down,
						   c.m_left,
						   false))); /* right */
      /* We assume here that the content occupies a single canvas row.  */
      canvas.paint(offset + canvas::coord_t (canvas_x, canvas_y + 1),
		   theme.get_line_art (directions (c.m_down, /* up */
						   c.m_down, /* down */
						   false, /* left */
						   false))); /* right */
    }

  /* Draw bottom border of table.  */
  {
    const int canvas_y = tg.get_canvas_size ().h - 1;
    for (int table_x = 0; table_x < m_size.w; table_x++)
      {
	const directions c (get_connections (table_x, m_size.h));
	const int left_canvas_x = tg.table_x_to_canvas_x (table_x);
	canvas.paint (offset + canvas::coord_t (left_canvas_x, canvas_y),
		      theme.get_line_art (directions (c.m_up,
						      false, /* down */
						      c.m_left,
						      c.m_right)));
	const int col_width = tg.get_col_width (table_x);
	for (int x_offset = 0; x_offset < col_width; x_offset++)
	  {
	    const int canvas_x = left_canvas_x + 1 + x_offset;
	    canvas.paint(offset + canvas::coord_t (canvas_x, canvas_y),
			 theme.get_line_art (directions (false, // up
							 false, // down
							 c.m_right, // left
							 c.m_right))); // right
	  }
      }

    /* Bottom-right corner of table.  */
    const int table_x = m_size.w;
    const int canvas_x = tg.table_x_to_canvas_x (table_x);
    const directions c (get_connections (m_size.w, m_size.h));
    canvas.paint (offset + canvas::coord_t (canvas_x, canvas_y),
		  theme.get_line_art (directions (c.m_up, // up
						  false, // down
						  c.m_left, // left
						  false))); // right
  }
}

void
table::paint_cell_contents_to_canvas(canvas &canvas,
				     canvas::coord_t offset,
				     const table_geometry &tg) const
{
  for (auto &placement : m_placements)
    placement.paint_cell_contents_to_canvas (canvas, offset, tg);
}

/* class table_cell_sizes.  */

/* Consider 1x1 cells.  */

void
table_cell_sizes::pass_1 (const table &table)
{
  for (auto &placement : table.m_placements)
    if (placement.one_by_one_p ())
      {
	canvas::size_t canvas_size (placement.get_min_canvas_size ());
	table::coord_t table_coord (placement.m_rect.m_top_left);
	m_col_widths.require (table_coord.x, canvas_size.w);
	m_row_heights.require (table_coord.y, canvas_size.h);
      }
}

/* Consider cells that span more than one row or column.  */

void
table_cell_sizes::pass_2 (const table &table)
{
  for (auto &placement : table.m_placements)
    if (!placement.one_by_one_p ())
      {
	const canvas::size_t req_canvas_size (placement.get_min_canvas_size ());
	const canvas::size_t current_canvas_size
	  = get_canvas_size (placement.m_rect);
	/* Grow columns as necessary.  */
	if (req_canvas_size.w > current_canvas_size.w)
	  {
	    /* Spread the deficit amongst the columns.  */
	    int deficit = req_canvas_size.w - current_canvas_size.w;
	    const int per_col = deficit / placement.m_rect.m_size.w;
	    for (int table_x = placement.m_rect.get_min_x ();
		 table_x < placement.m_rect.get_next_x ();
		 table_x++)
	    {
	      m_col_widths.m_requirements[table_x] += per_col;
	      deficit -= per_col;
	    }
	    /* Make sure we allocate all of the deficit.  */
	    if (deficit > 0)
	      {
		const int table_x = placement.m_rect.get_max_x ();
		m_col_widths.m_requirements[table_x] += deficit;
	      }
	  }
	/* Grow rows as necessary.  */
	if (req_canvas_size.h > current_canvas_size.h)
	  {
	    /* Spread the deficit amongst the rows.  */
	    int deficit = req_canvas_size.h - current_canvas_size.h;
	    const int per_row = deficit / placement.m_rect.m_size.h;
	    for (int table_y = placement.m_rect.get_min_y ();
		 table_y < placement.m_rect.get_next_y ();
		 table_y++)
	    {
	      m_row_heights.m_requirements[table_y] += per_row;
	      deficit -= per_row;
	    }
	    /* Make sure we allocate all of the deficit.  */
	    if (deficit > 0)
	      {
		const int table_y = placement.m_rect.get_max_y ();
		m_row_heights.m_requirements[table_y] += deficit;
	      }
	  }
      }
}

canvas::size_t
table_cell_sizes::get_canvas_size (const table::rect_t &rect) const
{
  canvas::size_t result (0, 0);
  for (int table_x = rect.get_min_x ();
       table_x < rect.get_next_x ();
       table_x ++)
    result.w += m_col_widths.m_requirements[table_x];
  for (int table_y = rect.get_min_y ();
       table_y < rect.get_next_y ();
       table_y ++)
    result.h += m_row_heights.m_requirements[table_y];
  /* Allow space for the borders.  */
  result.w += rect.m_size.w - 1;
  result.h += rect.m_size.h - 1;
  return result;
}

/* class text_art::table_geometry.  */

table_geometry::table_geometry (const table &table, table_cell_sizes &cell_sizes)
: m_cell_sizes (cell_sizes),
  m_canvas_size (canvas::size_t (0, 0)),
  m_col_start_x (table.get_size ().w),
  m_row_start_y (table.get_size ().h)
{
  recalc_coords ();
}

void
table_geometry::recalc_coords ()
{
  /* Start canvas column of table cell, including leading border.  */
  m_col_start_x.clear ();
  int iter_canvas_x = 0;
  for (auto w : m_cell_sizes.m_col_widths.m_requirements)
    {
      m_col_start_x.push_back (iter_canvas_x);
      iter_canvas_x += w + 1;
    }

  /* Start canvas row of table cell, including leading border.  */
  m_row_start_y.clear ();
  int iter_canvas_y = 0;
  for (auto h : m_cell_sizes.m_row_heights.m_requirements)
    {
      m_row_start_y.push_back (iter_canvas_y);
      iter_canvas_y += h + 1;
    }

  m_canvas_size = canvas::size_t (iter_canvas_x + 1,
				  iter_canvas_y + 1);
}

/* Get the TL corner of the table cell at TABLE_COORD
   in canvas coords (including the border).  */

canvas::coord_t
table_geometry::table_to_canvas (table::coord_t table_coord) const
{
  return canvas::coord_t (table_x_to_canvas_x (table_coord.x),
			  table_y_to_canvas_y (table_coord.y));
}

/* Get the left border of the table cell at column TABLE_X
   in canvas coords (including the border).  */

int
table_geometry::table_x_to_canvas_x (int table_x) const
{
  /* Allow one beyond the end, for the right-hand border of the table.  */
  if (table_x == (int)m_col_start_x.size ())
    return m_canvas_size.w - 1;
  return m_col_start_x[table_x];
}

/* Get the top border of the table cell at column TABLE_Y
   in canvas coords (including the border).  */

int
table_geometry::table_y_to_canvas_y (int table_y) const
{
  /* Allow one beyond the end, for the right-hand border of the table.  */
  if (table_y == (int)m_row_start_y.size ())
    return m_canvas_size.h - 1;
  return m_row_start_y[table_y];
}

/* class text_art::simple_table_geometry.  */

simple_table_geometry::simple_table_geometry (const table &table)
: m_col_widths (table.get_size ().w),
  m_row_heights (table.get_size ().h),
  m_cell_sizes (m_col_widths, m_row_heights),
  m_tg (table, m_cell_sizes)
{
  m_cell_sizes.pass_1 (table);
  m_cell_sizes.pass_2 (table);
  m_tg.recalc_coords ();
}

#if CHECKING_P

namespace selftest {

static void
test_tic_tac_toe ()
{
  style_manager sm;
  table t (table::size_t (3, 3));
  t.set_cell (table::coord_t (0, 0), styled_string (sm, "X"));
  t.set_cell (table::coord_t (1, 0), styled_string (sm, ""));
  t.set_cell (table::coord_t (2, 0), styled_string (sm, ""));
  t.set_cell (table::coord_t (0, 1), styled_string (sm, "O"));
  t.set_cell (table::coord_t (1, 1), styled_string (sm, "O"));
  t.set_cell (table::coord_t (2, 1), styled_string (sm, ""));
  t.set_cell (table::coord_t (0, 2), styled_string (sm, "X"));
  t.set_cell (table::coord_t (1, 2), styled_string (sm, ""));
  t.set_cell (table::coord_t (2, 2), styled_string (sm, "O"));

  {
    canvas canvas (t.to_canvas (ascii_theme (), sm));
    ASSERT_CANVAS_STREQ
      (canvas, false,
       ("+-+-+-+\n"
	"|X| | |\n"
	"+-+-+-+\n"
	"|O|O| |\n"
	"+-+-+-+\n"
	"|X| |O|\n"
	"+-+-+-+\n"));
  }

  {
    canvas canvas (t.to_canvas (unicode_theme (), sm));
    ASSERT_CANVAS_STREQ
      (canvas, false,
       // FIXME: are we allowed unicode chars in string literals in our source?
       ("┌─┬─┬─┐\n"
	"│X│ │ │\n"
	"├─┼─┼─┤\n"
	"│O│O│ │\n"
	"├─┼─┼─┤\n"
	"│X│ │O│\n"
	"└─┴─┴─┘\n"));
  }
}

static table
make_text_table ()
{
  style_manager sm;
  table t (table::size_t (3, 3));
  t.set_cell (table::coord_t (0, 0), styled_string (sm, "top left"));
  t.set_cell (table::coord_t (1, 0), styled_string (sm, "top middle"));
  t.set_cell (table::coord_t (2, 0), styled_string (sm, "top right"));
  t.set_cell (table::coord_t (0, 1), styled_string (sm, "middle left"));
  t.set_cell (table::coord_t (1, 1), styled_string (sm, "middle middle"));
  t.set_cell (table::coord_t (2, 1), styled_string (sm, "middle right"));
  t.set_cell (table::coord_t (0, 2), styled_string (sm, "bottom left"));
  t.set_cell (table::coord_t (1, 2), styled_string (sm, "bottom middle"));
  t.set_cell (table::coord_t (2, 2), styled_string (sm, "bottom right"));
  return t;
}

static void
test_text_table ()
{
  style_manager sm;
  table table = make_text_table ();
  {
    canvas canvas (table.to_canvas (ascii_theme(), sm));
    ASSERT_CANVAS_STREQ
      (canvas, false,
       ("+-----------+-------------+------------+\n"
	"| top left  | top middle  | top right  |\n"
	"+-----------+-------------+------------+\n"
	"|middle left|middle middle|middle right|\n"
	"+-----------+-------------+------------+\n"
	"|bottom left|bottom middle|bottom right|\n"
	"+-----------+-------------+------------+\n"));
  }
  {
    canvas canvas (table.to_canvas (unicode_theme(), sm));
    ASSERT_CANVAS_STREQ
      (canvas, false,
       // FIXME: are we allowed unicode chars in string literals in our source?
       ("┌───────────┬─────────────┬────────────┐\n"
	"│ top left  │ top middle  │ top right  │\n"
	"├───────────┼─────────────┼────────────┤\n"
	"│middle left│middle middle│middle right│\n"
	"├───────────┼─────────────┼────────────┤\n"
	"│bottom left│bottom middle│bottom right│\n"
	"└───────────┴─────────────┴────────────┘\n"));
  }
}

static void
test_offset_table ()
{
  style_manager sm;
  table table = make_text_table ();
  simple_table_geometry stg (table);
  const canvas::size_t tcs = stg.m_tg.get_canvas_size();
  {
    canvas canvas (canvas::size_t (tcs.w + 5, tcs.h + 5), sm);
    canvas.debug_fill ();
    table.paint_to_canvas (canvas, canvas::coord_t (3, 3),
			   stg.m_tg,
			   ascii_theme());
    ASSERT_CANVAS_STREQ
      (canvas, false,
       ("*********************************************\n"
	"*********************************************\n"
	"*********************************************\n"
	"***+-----------+-------------+------------+**\n"
	"***| top left  | top middle  | top right  |**\n"
	"***+-----------+-------------+------------+**\n"
	"***|middle left|middle middle|middle right|**\n"
	"***+-----------+-------------+------------+**\n"
	"***|bottom left|bottom middle|bottom right|**\n"
	"***+-----------+-------------+------------+**\n"
	"*********************************************\n"
	"*********************************************\n"));
  }
  {
    canvas canvas (canvas::size_t (tcs.w + 5, tcs.h + 5), sm);
    canvas.debug_fill ();
    table.paint_to_canvas (canvas, canvas::coord_t (3, 3),
			   stg.m_tg,
			   unicode_theme());
    ASSERT_CANVAS_STREQ
      (canvas, false,
       // FIXME: are we allowed unicode chars in string literals in our source?
       ("*********************************************\n"
	"*********************************************\n"
	"*********************************************\n"
	"***┌───────────┬─────────────┬────────────┐**\n"
	"***│ top left  │ top middle  │ top right  │**\n"
	"***├───────────┼─────────────┼────────────┤**\n"
	"***│middle left│middle middle│middle right│**\n"
	"***├───────────┼─────────────┼────────────┤**\n"
	"***│bottom left│bottom middle│bottom right│**\n"
	"***└───────────┴─────────────┴────────────┘**\n"
	"*********************************************\n"
	"*********************************************\n"));
  }
}

#define ASSERT_TABLE_CELL_STREQ(TABLE, TABLE_X, TABLE_Y, EXPECTED_STR)	\
  SELFTEST_BEGIN_STMT							\
    table::coord_t coord ((TABLE_X), (TABLE_Y));			\
    const table::cell_placement *cp = (TABLE).get_placement_at (coord);	\
    ASSERT_NE (cp, nullptr);						\
    ASSERT_EQ (cp->get_content (), styled_string (sm, EXPECTED_STR)); \
  SELFTEST_END_STMT

#define ASSERT_TABLE_NULL_CELL(TABLE, TABLE_X, TABLE_Y)			\
  SELFTEST_BEGIN_STMT							\
    table::coord_t coord ((TABLE_X), (TABLE_Y));			\
    const table::cell_placement *cp = (TABLE).get_placement_at (coord);	\
    ASSERT_EQ (cp, nullptr);						\
  SELFTEST_END_STMT

static void
test_spans ()
{
  style_manager sm;
  table table (table::size_t (3, 3));
  table.set_cell_span (table::rect_t (table::coord_t (0, 0),
				      table::size_t (3, 1)),
		       styled_string (sm, "ABC"));
  table.set_cell_span (table::rect_t (table::coord_t (0, 1),
				      table::size_t (2, 1)),
		       styled_string (sm, "DE"));
  table.set_cell_span (table::rect_t (table::coord_t (2, 1),
				      table::size_t (1, 1)),
		       styled_string (sm, "F"));
  table.set_cell (table::coord_t (0, 2), styled_string (sm, "G"));
  table.set_cell (table::coord_t (1, 2), styled_string (sm, "H"));
  table.set_cell (table::coord_t (2, 2), styled_string (sm, "I"));
  {
    canvas canvas (table.to_canvas (ascii_theme(), sm));
    ASSERT_CANVAS_STREQ
      (canvas, false,
       ("+-----+\n"
	"| ABC |\n"
	"+---+-+\n"
	"|DE |F|\n"
	"+-+-+-+\n"
	"|G|H|I|\n"
	"+-+-+-+\n"));
  }
  {
    canvas canvas (table.to_canvas (unicode_theme(), sm));
    ASSERT_CANVAS_STREQ
      (canvas, false,
       // FIXME: are we allowed unicode chars in string literals in our source?
       ("┌─────┐\n"
	"│ ABC │\n"
	"├───┬─┤\n"
	"│DE │F│\n"
	"├─┬─┼─┤\n"
	"│G│H│I│\n"
	"└─┴─┴─┘\n"));
  }
}

/* Verify building this 5x5 table with spans:
     |0|1|2|3|4|
     +-+-+-+-+-+
    0|A A A|B|C|0
     +     +-+ +
    1|A A A|D|C|1
     +     +-+-+
    2|A A A|E|F|2
     +-+-+-+-+-+
    3|G G|H|I I|3
     |   | +-+-+
    4|G G|H|J J|4
     +-+-+-+-+-+
     |0|1|2|3|4|
*/

static void
test_spans_2 ()
{
  style_manager sm;
  table table (table::size_t (5, 5));
  table.set_cell_span (table::rect_t (table::coord_t (0, 0),
				      table::size_t (3, 3)),
		       styled_string (sm, "A"));
  table.set_cell_span (table::rect_t (table::coord_t (3, 0),
				      table::size_t (1, 1)),
		       styled_string (sm, "B"));
  table.set_cell_span (table::rect_t (table::coord_t (4, 0),
				      table::size_t (1, 2)),
		       styled_string (sm, "C"));
  table.set_cell_span (table::rect_t (table::coord_t (3, 1),
				      table::size_t (1, 1)),
		       styled_string (sm, "D"));
  table.set_cell_span (table::rect_t (table::coord_t (3, 2),
				      table::size_t (1, 1)),
		       styled_string (sm, "E"));
  table.set_cell_span (table::rect_t (table::coord_t (4, 2),
				      table::size_t (1, 1)),
		       styled_string (sm, "F"));
  table.set_cell_span (table::rect_t (table::coord_t (0, 3),
				      table::size_t (2, 2)),
		       styled_string (sm, "G"));
  table.set_cell_span (table::rect_t (table::coord_t (2, 3),
				      table::size_t (1, 2)),
		       styled_string (sm, "H"));
  table.set_cell_span (table::rect_t (table::coord_t (3, 3),
				      table::size_t (2, 1)),
		       styled_string (sm, "I"));
  table.set_cell_span (table::rect_t (table::coord_t (3, 4),
				      table::size_t (2, 1)),
		       styled_string (sm, "J"));

  /* Check occupancy at each table coordinate.  */
  ASSERT_TABLE_CELL_STREQ (table, 0, 0, "A");
  ASSERT_TABLE_CELL_STREQ (table, 1, 0, "A");
  ASSERT_TABLE_CELL_STREQ (table, 2, 0, "A");
  ASSERT_TABLE_CELL_STREQ (table, 3, 0, "B");
  ASSERT_TABLE_CELL_STREQ (table, 4, 0, "C");

  ASSERT_TABLE_CELL_STREQ (table, 0, 1, "A");
  ASSERT_TABLE_CELL_STREQ (table, 1, 1, "A");
  ASSERT_TABLE_CELL_STREQ (table, 2, 1, "A");
  ASSERT_TABLE_CELL_STREQ (table, 3, 1, "D");
  ASSERT_TABLE_CELL_STREQ (table, 4, 1, "C");

  ASSERT_TABLE_CELL_STREQ (table, 0, 2, "A");
  ASSERT_TABLE_CELL_STREQ (table, 1, 2, "A");
  ASSERT_TABLE_CELL_STREQ (table, 2, 2, "A");
  ASSERT_TABLE_CELL_STREQ (table, 3, 2, "E");
  ASSERT_TABLE_CELL_STREQ (table, 4, 2, "F");

  ASSERT_TABLE_CELL_STREQ (table, 0, 3, "G");
  ASSERT_TABLE_CELL_STREQ (table, 1, 3, "G");
  ASSERT_TABLE_CELL_STREQ (table, 2, 3, "H");
  ASSERT_TABLE_CELL_STREQ (table, 3, 3, "I");
  ASSERT_TABLE_CELL_STREQ (table, 4, 3, "I");

  ASSERT_TABLE_CELL_STREQ (table, 0, 4, "G");
  ASSERT_TABLE_CELL_STREQ (table, 1, 4, "G");
  ASSERT_TABLE_CELL_STREQ (table, 2, 4, "H");
  ASSERT_TABLE_CELL_STREQ (table, 3, 4, "J");
  ASSERT_TABLE_CELL_STREQ (table, 4, 4, "J");

  {
    canvas canvas (table.to_canvas (ascii_theme(), sm));
    ASSERT_CANVAS_STREQ
      (canvas, false,
       ("+---+-+-+\n"
	"|   |B| |\n"
	"|   +-+C|\n"
	"| A |D| |\n"
	"|   +-+-+\n"
	"|   |E|F|\n"
	"+-+-+-+-+\n"
	"| | | I |\n"
	"|G|H+---+\n"
	"| | | J |\n"
	"+-+-+---+\n"));
  }
  {
    canvas canvas (table.to_canvas (unicode_theme(), sm));
    ASSERT_CANVAS_STREQ
      (canvas, false,
       // FIXME: are we allowed unicode chars in string literals in our source?
       ("┌───┬─┬─┐\n"
	"│   │B│ │\n"
	"│   ├─┤C│\n"
	"│ A │D│ │\n"
	"│   ├─┼─┤\n"
	"│   │E│F│\n"
	"├─┬─┼─┴─┤\n"
	"│ │ │ I │\n"
	"│G│H├───┤\n"
	"│ │ │ J │\n"
	"└─┴─┴───┘\n"));
  }
}

/* Experiment with adding a 1-table-column gap at the boundary between
   valid vs invalid for visualizing a buffer overflow.  */
static void
test_spans_3 ()
{
  const char * const str = "hello world!";
  const size_t buf_size = 10;
  const size_t str_size = strlen (str) + 1;

  style_manager sm;
  table table (table::size_t (str_size + 1, 3));

  table.set_cell_span (table::rect_t (table::coord_t (0, 0),
				      table::size_t (str_size + 1, 1)),
		       styled_string (sm, "String literal"));

  for (size_t i = 0; i < str_size; i++)
    {
      table::coord_t c (i, 1);
      if (i >= buf_size)
	c.x++;
      if (str[i] == '\0')
	table.set_cell (c, styled_string (sm, "NUL"));
      else
	table.set_cell (c, styled_string ((cppchar_t)str[i]));
    }

  table.set_cell_span (table::rect_t (table::coord_t (0, 2),
				      table::size_t (buf_size, 1)),
		       styled_string::from_fmt (sm,
						     nullptr,
						     "'buf' (char[%i])",
						     (int)buf_size));
  table.set_cell_span (table::rect_t (table::coord_t (buf_size + 1, 2),
				      table::size_t (str_size - buf_size, 1)),
		       styled_string (sm, "overflow"));

  {
    canvas canvas (table.to_canvas (ascii_theme (), sm));
    ASSERT_CANVAS_STREQ
      (canvas, false,
       "+-----------------------------+\n"
       "|       String literal        |\n"
       "+-+-+-+-+-+-+-+-+-+-++-+-+----+\n"
       "|h|e|l|l|o| |w|o|r|l||d|!|NUL |\n"
       "+-+-+-+-+-+-+-+-+-+-++-+-+----+\n"
       "| 'buf' (char[10])  ||overflow|\n"
       "+-------------------++--------+\n");
  }
  {
    canvas canvas (table.to_canvas (unicode_theme (), sm));
    ASSERT_CANVAS_STREQ
      (canvas, false,
       // FIXME: are we allowed unicode chars in string literals in our source?
       ("┌─────────────────────────────┐\n"
	"│       String literal        │\n"
	"├─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬┬─┬─┬────┤\n"
	"│h│e│l│l│o│ │w│o│r│l││d│!│NUL │\n"
	"├─┴─┴─┴─┴─┴─┴─┴─┴─┴─┤├─┴─┴────┤\n"
	"│ 'buf' (char[10])  ││overflow│\n"
	"└───────────────────┘└────────┘\n"));
  }
}

static void
test_double_width_chars ()
{
  table_cell_content tcc (styled_string ((cppchar_t)0x1f642));
  ASSERT_EQ (tcc.get_canvas_size ().w, 2);
  ASSERT_EQ (tcc.get_canvas_size ().h, 1);

  style_manager sm;
  table table (table::size_t (1, 1));
  table.set_cell (table::coord_t (0,0),
		  styled_string ((cppchar_t)0x1f642));

  canvas canvas (table.to_canvas (unicode_theme(), sm));
  ASSERT_CANVAS_STREQ
    (canvas, false,
     // FIXME: are we allowed unicode chars in string literals in our source?
     ("┌──┐\n"
      "│🙂│\n"
      "└──┘\n"));
}

static void
test_ipv4_header ()
{
  style_manager sm;
  table table (table::size_t (34, 10));
  table.set_cell (table::coord_t (0, 0), styled_string (sm, "Offsets"));
  table.set_cell (table::coord_t (1, 0), styled_string (sm, "Octet"));
  table.set_cell (table::coord_t (0, 1), styled_string (sm, "Octet"));
  for (int octet = 0; octet < 4; octet++)
    table.set_cell_span (table::rect_t (table::coord_t (2 + (octet * 8), 0),
					table::size_t (8, 1)),
			 styled_string::from_fmt (sm, nullptr, "%i", octet));
  table.set_cell (table::coord_t (1, 1), styled_string (sm, "Bit"));
  for (int bit = 0; bit < 32; bit++)
    table.set_cell (table::coord_t (bit + 2, 1),
		    styled_string::from_fmt (sm, nullptr, "%i", bit));
  for (int word = 0; word < 6; word++)
    {
      table.set_cell (table::coord_t (0, word + 2),
		      styled_string::from_fmt (sm, nullptr, "%i", word * 4));
      table.set_cell (table::coord_t (1, word + 2),
		      styled_string::from_fmt (sm, nullptr, "%i", word * 32));
    }

  table.set_cell (table::coord_t (0, 8), styled_string (sm, "..."));
  table.set_cell (table::coord_t (1, 8), styled_string (sm, "..."));
  table.set_cell (table::coord_t (0, 9), styled_string (sm, "56"));
  table.set_cell (table::coord_t (1, 9), styled_string (sm, "448"));

#define SET_BITS(FIRST, LAST, NAME)					\
  do {									\
    const int first = (FIRST);						\
    const int last = (LAST);						\
    const char *name = (NAME);						\
    const int row = first / 32;						\
    gcc_assert (last / 32 == row);					\
    table::rect_t rect (table::coord_t ((first % 32) + 2, row + 2),	\
			table::size_t (last + 1 - first , 1));		\
    table.set_cell_span (rect, styled_string (sm, name));		\
  } while (0)

  SET_BITS (0, 3, "Version");
  SET_BITS (4, 7, "IHL");
  SET_BITS (8, 13, "DSCP");
  SET_BITS (14, 15, "ECN");
  SET_BITS (16, 31, "Total Length");

  SET_BITS (32 +  0, 32 + 15, "Identification");
  SET_BITS (32 + 16, 32 + 18, "Flags");
  SET_BITS (32 + 19, 32 + 31, "Fragment Offset");

  SET_BITS (64 +  0, 64 +  7, "Time To Live");
  SET_BITS (64 +  8, 64 + 15, "Protocol");
  SET_BITS (64 + 16, 64 + 31, "Header Checksum");

  SET_BITS (96 +  0, 96 + 31, "Source IP Address");
  SET_BITS (128 +  0, 128 + 31, "Destination IP Address");

  table.set_cell_span(table::rect_t (table::coord_t (2, 7),
				     table::size_t (32, 3)),
		      styled_string (sm, "Options"));
  {
    canvas canvas (table.to_canvas (ascii_theme(), sm));
    ASSERT_CANVAS_STREQ
      (canvas, false,
       ("+-------+-----+---------------+---------------------+-----------------------+-----------------------+\n"
	"|Offsets|Octet|       0       |          1          |           2           |           3           |\n"
	"+-------+-----+-+-+-+-+-+-+-+-+-+-+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+\n"
	"| Octet | Bit |0|1|2|3|4|5|6|7|8|9|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31|\n"
	"+-------+-----+-+-+-+-+-+-+-+-+-+-+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+\n"
	"|   0   |  0  |Version|  IHL  |     DSCP      | ECN |                 Total Length                  |\n"
	"+-------+-----+-------+-------+---------------+-----+--------+--------------------------------------+\n"
	"|   4   | 32  |           Identification            | Flags  |           Fragment Offset            |\n"
	"+-------+-----+---------------+---------------------+--------+--------------------------------------+\n"
	"|   8   | 64  | Time To Live  |      Protocol       |                Header Checksum                |\n"
	"+-------+-----+---------------+---------------------+-----------------------------------------------+\n"
	"|  12   | 96  |                                  Source IP Address                                  |\n"
	"+-------+-----+-------------------------------------------------------------------------------------+\n"
	"|  16   | 128 |                               Destination IP Address                                |\n"
	"+-------+-----+-------------------------------------------------------------------------------------+\n"
	"|  20   | 160 |                                                                                     |\n"
	"+-------+-----+                                                                                     |\n"
	"|  ...  | ... |                                       Options                                       |\n"
	"+-------+-----+                                                                                     |\n"
	"|  56   | 448 |                                                                                     |\n"
	"+-------+-----+-------------------------------------------------------------------------------------+\n"));
  }
  {
    canvas canvas (table.to_canvas (unicode_theme(), sm));
    ASSERT_CANVAS_STREQ
      (canvas, false,
       // FIXME: are we allowed unicode chars in string literals in our source?
       ("┌───────┬─────┬───────────────┬─────────────────────┬───────────────────────┬───────────────────────┐\n"
	"│Offsets│Octet│       0       │          1          │           2           │           3           │\n"
	"├───────┼─────┼─┬─┬─┬─┬─┬─┬─┬─┼─┬─┬──┬──┬──┬──┬──┬──┼──┬──┬──┬──┬──┬──┬──┬──┼──┬──┬──┬──┬──┬──┬──┬──┤\n"
	"│ Octet │ Bit │0│1│2│3│4│5│6│7│8│9│10│11│12│13│14│15│16│17│18│19│20│21│22│23│24│25│26│27│28│29│30│31│\n"
	"├───────┼─────┼─┴─┴─┴─┼─┴─┴─┴─┼─┴─┴──┴──┴──┴──┼──┴──┼──┴──┴──┴──┴──┴──┴──┴──┴──┴──┴──┴──┴──┴──┴──┴──┤\n"
	"│   0   │  0  │Version│  IHL  │     DSCP      │ ECN │                 Total Length                  │\n"
	"├───────┼─────┼───────┴───────┴───────────────┴─────┼────────┬──────────────────────────────────────┤\n"
	"│   4   │ 32  │           Identification            │ Flags  │           Fragment Offset            │\n"
	"├───────┼─────┼───────────────┬─────────────────────┼────────┴──────────────────────────────────────┤\n"
	"│   8   │ 64  │ Time To Live  │      Protocol       │                Header Checksum                │\n"
	"├───────┼─────┼───────────────┴─────────────────────┴───────────────────────────────────────────────┤\n"
	"│  12   │ 96  │                                  Source IP Address                                  │\n"
	"├───────┼─────┼─────────────────────────────────────────────────────────────────────────────────────┤\n"
	"│  16   │ 128 │                               Destination IP Address                                │\n"
	"├───────┼─────┼─────────────────────────────────────────────────────────────────────────────────────┤\n"
	"│  20   │ 160 │                                                                                     │\n"
	"├───────┼─────┤                                                                                     │\n"
	"│  ...  │ ... │                                       Options                                       │\n"
	"├───────┼─────┤                                                                                     │\n"
	"│  56   │ 448 │                                                                                     │\n"
	"└───────┴─────┴─────────────────────────────────────────────────────────────────────────────────────┘\n"));
  }
}

static void
test_missing_cells ()
{
  style_manager sm;
  table table (table::size_t (3, 3));
  table.set_cell (table::coord_t (1, 0), styled_string (sm, "A"));
  table.set_cell (table::coord_t (0, 1), styled_string (sm, "B"));
  table.set_cell (table::coord_t (1, 1), styled_string (sm, "C"));
  table.set_cell (table::coord_t (2, 1), styled_string (sm, "D"));
  table.set_cell (table::coord_t (1, 2), styled_string (sm, "E"));

  ASSERT_TABLE_NULL_CELL (table, 0, 0);
  ASSERT_TABLE_CELL_STREQ (table, 1, 0, "A");
  ASSERT_TABLE_NULL_CELL (table, 2, 0);

  ASSERT_TABLE_CELL_STREQ (table, 0, 1, "B");
  ASSERT_TABLE_CELL_STREQ (table, 1, 1, "C");
  ASSERT_TABLE_CELL_STREQ (table, 2, 1, "D");

  ASSERT_TABLE_NULL_CELL (table, 0, 2);
  ASSERT_TABLE_CELL_STREQ (table, 1, 2, "E");
  ASSERT_TABLE_NULL_CELL (table, 2, 2);

  {
    canvas canvas (table.to_canvas (ascii_theme(), sm));
    ASSERT_CANVAS_STREQ
      (canvas, false,
       ("  +-+\n"
	"  |A|\n"
	"+-+-+-+\n"
	"|B|C|D|\n"
	"+-+-+-+\n"
	"  |E|\n"
	"  +-+\n"));
  }
  {
    canvas canvas (table.to_canvas (unicode_theme(), sm));
    ASSERT_CANVAS_STREQ
      (canvas, false,
       ("  ┌─┐\n"
	"  │A│\n"
	"┌─┼─┼─┐\n"
	"│B│C│D│\n"
	"└─┼─┼─┘\n"
	"  │E│\n"
	"  └─┘\n"));
  }
}

static void
test_add_row ()
{
  style_manager sm;
  table table (table::size_t (3, 0));
  for (int i = 0; i < 5; i++)
    {
      const int y = table.add_row ();
      for (int x = 0; x < 3; x++)
	table.set_cell (table::coord_t (x, y),
			styled_string::from_fmt (sm, nullptr,
						 "%i, %i", x, y));
    }
  canvas canvas (table.to_canvas (ascii_theme(), sm));
  ASSERT_CANVAS_STREQ
    (canvas, false,
     ("+----+----+----+\n"
      "|0, 0|1, 0|2, 0|\n"
      "+----+----+----+\n"
      "|0, 1|1, 1|2, 1|\n"
      "+----+----+----+\n"
      "|0, 2|1, 2|2, 2|\n"
      "+----+----+----+\n"
      "|0, 3|1, 3|2, 3|\n"
      "+----+----+----+\n"
      "|0, 4|1, 4|2, 4|\n"
      "+----+----+----+\n"));
}

static void
test_alignment ()
{
  style_manager sm;
  table table (table::size_t (9, 9));
  table.set_cell_span (table::rect_t (table::coord_t (0, 0),
				      table::size_t (3, 3)),
		       styled_string (sm, "left top"),
		      x_align::LEFT, y_align::TOP);
  table.set_cell_span (table::rect_t (table::coord_t (3, 0),
				      table::size_t (3, 3)),
		       styled_string (sm, "center top"),
		       x_align::CENTER, y_align::TOP);
  table.set_cell_span (table::rect_t (table::coord_t (6, 0),
				      table::size_t (3, 3)),
		       styled_string (sm, "right top"),
		       x_align::RIGHT, y_align::TOP);
  table.set_cell_span (table::rect_t (table::coord_t (0, 3),
				      table::size_t (3, 3)),
		       styled_string (sm, "left center"),
		       x_align::LEFT, y_align::CENTER);
  table.set_cell_span (table::rect_t (table::coord_t (3, 3),
				      table::size_t (3, 3)),
		       styled_string (sm, "center center"),
		       x_align::CENTER, y_align::CENTER);
  table.set_cell_span (table::rect_t (table::coord_t (6, 3),
				      table::size_t (3, 3)),
		       styled_string (sm, "right center"),
		       x_align::RIGHT, y_align::CENTER);
  table.set_cell_span (table::rect_t (table::coord_t (0, 6),
				      table::size_t (3, 3)),
		       styled_string (sm, "left bottom"),
		       x_align::LEFT, y_align::BOTTOM);
  table.set_cell_span (table::rect_t (table::coord_t (3, 6),
				      table::size_t (3, 3)),
		       styled_string (sm, "center bottom"),
		       x_align::CENTER, y_align::BOTTOM);
  table.set_cell_span (table::rect_t (table::coord_t (6, 6),
				      table::size_t (3, 3)),
		       styled_string (sm, "right bottom"),
		       x_align::RIGHT, y_align::BOTTOM);

  canvas canvas (table.to_canvas (ascii_theme(), sm));
  ASSERT_CANVAS_STREQ
    (canvas, false,
     ("+-----------+-------------+------------+\n"
      "|left top   | center top  |   right top|\n"
      "|           |             |            |\n"
      "+-----------+-------------+------------+\n"
      "|left center|center center|right center|\n"
      "|           |             |            |\n"
      "+-----------+-------------+------------+\n"
      "|           |             |            |\n"
      "|left bottom|center bottom|right bottom|\n"
      "+-----------+-------------+------------+\n"));
}

/* Run all selftests in this file.  */

void
text_art_table_cc_tests ()
{
  test_tic_tac_toe ();
  test_text_table ();
  test_offset_table ();
  test_spans ();
  test_spans_2 ();
  test_spans_3 ();
  test_double_width_chars ();
  test_ipv4_header ();
  test_missing_cells ();
  test_add_row ();
  test_alignment ();
}

} // namespace selftest


#endif /* #if CHECKING_P */
