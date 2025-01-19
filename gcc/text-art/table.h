/* Support for tabular/grid-based content.
   Copyright (C) 2023-2025 Free Software Foundation, Inc.
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

#ifndef GCC_TEXT_ART_TABLE_H
#define GCC_TEXT_ART_TABLE_H

#include "text-art/canvas.h"
#include "text-art/theme.h"

namespace text_art {

class table;
class table_geometry;

/* A class representing the content of a particular table cell,
   or of a span of table cells.  */

class table_cell_content
{
 public:
  table_cell_content () : m_str (), m_size (0, 0) {}
  table_cell_content (styled_string &&s);

  bool operator== (const table_cell_content &other) const
  {
    return m_str == other.m_str;
  }

  canvas::size_t get_canvas_size () const { return m_size; }

  void paint_to_canvas (canvas &canvas,
			canvas::coord_t top_left) const;

 private:
  styled_string m_str;
  canvas::size_t m_size;
};

/* A list of required sizes of table rows or columns
   in canvas units (row heights or column widths).  */

struct table_dimension_sizes
{
  table_dimension_sizes (unsigned num);

  void require (unsigned idx, int amount)
  {
    m_requirements[idx] = std::max (m_requirements[idx], amount);
  }

  std::vector<int> m_requirements;
};

/* A 2D grid of cells.  Instances of table_cell_content can be assigned
   to individual table cells, and to rectangular spans of cells.  Such
   assignments do not have to fully cover the 2D grid, but they must not
   overlap.  */

class table
{
 public:
  typedef size<class table> size_t;
  typedef coord<class table> coord_t;
  typedef range<class table> range_t;
  typedef rect<class table> rect_t;

  /* A record of how a table_cell_content was placed at a table::rect_t
     with a certain alignment.  */
  class cell_placement
  {
  public:
    cell_placement (rect_t rect,
		    table_cell_content &&content,
		    x_align x_align,
		    y_align y_align)
    : m_rect (rect),
      m_content (std::move (content)),
      m_x_align (x_align),
      m_y_align (y_align)
    {
    }

    bool one_by_one_p () const
    {
      return m_rect.m_size.w == 1 && m_rect.m_size.h == 1;
    }

    canvas::size_t get_min_canvas_size () const
    {
      // Doesn't include border
      return m_content.get_canvas_size ();
    }

    void paint_cell_contents_to_canvas(canvas &canvas,
				       canvas::coord_t offset,
				       const table_geometry &tg) const;

    const table_cell_content &get_content () const { return m_content; }

  private:
    friend class table;
    friend class table_cell_sizes;
    rect_t m_rect;
    table_cell_content m_content;
    x_align m_x_align;
    y_align m_y_align;
  };

  table (size_t size);
  ~table () = default;
  table (table &&) = default;
  table (const table &) = delete;
  table &operator= (const table &) = delete;

  const size_t &get_size () const { return m_size; }

  int add_rows (unsigned num)
  {
    int topmost_new_row = m_size.h;
    m_size.h += num;
    for (unsigned i = 0; i < num; i++)
      m_occupancy.add_row (-1);
    return topmost_new_row;
  }

  int add_row ()
  {
    return add_rows (1);
  }

  void set_cell (coord_t coord,
		 table_cell_content &&content,
		 enum x_align x_align = x_align::CENTER,
		 enum y_align y_align = y_align::CENTER);

  void set_cell_span (rect_t span,
		      table_cell_content &&content,
		      enum x_align x_align = x_align::CENTER,
		      enum y_align y_align = y_align::CENTER);

  void maybe_set_cell_span (rect_t span,
			    table_cell_content &&content,
			    enum x_align x_align = x_align::CENTER,
			    enum y_align y_align = y_align::CENTER);

  canvas to_canvas (const theme &theme, const style_manager &sm) const;

  void paint_to_canvas(canvas &canvas,
		       canvas::coord_t offset,
		       const table_geometry &tg,
		       const theme &theme) const;

  void debug () const;

  void add_other_table (table &&other, table::coord_t offset);

  /* Self-test support.  */
  const cell_placement *get_placement_at (coord_t coord) const;

 private:
  int get_occupancy_safe (coord_t coord) const;
  directions get_connections (int table_x, int table_y) const;
  void paint_cell_borders_to_canvas(canvas &canvas,
				    canvas::coord_t offset,
				    const table_geometry &tg,
				    const theme &theme) const;
  void paint_cell_contents_to_canvas(canvas &canvas,
				     canvas::coord_t offset,
				     const table_geometry &tg) const;

  friend class table_cell_sizes;

  size_t m_size;
  std::vector<cell_placement> m_placements;
  array2<int, size_t, coord_t> m_occupancy; /* indices into the m_placements vec.  */
};

/* A workspace for computing the row heights and column widths
   of a table (in canvas units).
   The col_widths and row_heights could be shared between multiple
   instances, for aligning multiple tables vertically or horizontally.  */

class table_cell_sizes
{
 public:
  table_cell_sizes (table_dimension_sizes &col_widths,
		    table_dimension_sizes &row_heights)
  : m_col_widths (col_widths),
    m_row_heights (row_heights)
  {
  }

  void pass_1 (const table &table);
  void pass_2 (const table &table);

  canvas::size_t get_canvas_size (const table::rect_t &rect) const;

  table_dimension_sizes &m_col_widths;
  table_dimension_sizes &m_row_heights;
};

/* A class responsible for mapping from table cell coords
   to canvas coords, handling column widths.
   It's the result of solving "how big are all the table cells and where
   do they go?"
   The cell_sizes are passed in, for handling aligning multiple tables,
   sharing column widths or row heights.  */

class table_geometry
{
 public:
  table_geometry (const table &table, table_cell_sizes &cell_sizes);

  void recalc_coords ();

  const canvas::size_t get_canvas_size () const { return m_canvas_size; }

  canvas::coord_t table_to_canvas (table::coord_t table_coord) const;
  int table_x_to_canvas_x (int table_x) const;
  int table_y_to_canvas_y (int table_y) const;

  int get_col_width (int table_x) const
  {
    return m_cell_sizes.m_col_widths.m_requirements[table_x];
  }

  canvas::size_t get_canvas_size (const table::rect_t &rect) const
  {
    return m_cell_sizes.get_canvas_size (rect);
  }

 private:
  table_cell_sizes &m_cell_sizes;
  canvas::size_t m_canvas_size;

  /* Start canvas column of table cell, including leading border.  */
  std::vector<int> m_col_start_x;

  /* Start canvas row of table cell, including leading border.  */
  std::vector<int> m_row_start_y;
};

/* Helper class for handling the simple case of a single table
   that doesn't need to be aligned with respect to anything else.  */

struct simple_table_geometry
{
  simple_table_geometry (const table &table);

  table_dimension_sizes m_col_widths;
  table_dimension_sizes m_row_heights;
  table_cell_sizes m_cell_sizes;
  table_geometry m_tg;
};

} // namespace text_art

#endif /* GCC_TEXT_ART_TABLE_H */
