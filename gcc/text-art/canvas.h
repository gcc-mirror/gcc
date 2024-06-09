/* Canvas for random-access procedural text art.
   Copyright (C) 2023-2024 Free Software Foundation, Inc.
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

#ifndef GCC_TEXT_ART_CANVAS_H
#define GCC_TEXT_ART_CANVAS_H

#include "text-art/types.h"

namespace text_art {

class canvas;

/* A 2 dimensional grid of text cells (a "canvas"), which
   can be written to ("painted") via random access, and then
   written out to a pretty_printer once the picture is complete.

   Each text cell can be styled independently (colorization,
   URLs, etc).  */

class canvas
{
 public:
  typedef styled_unichar cell_t;
  typedef size<class canvas> size_t;
  typedef coord<class canvas> coord_t;
  typedef range<class canvas> range_t;
  typedef rect<class canvas> rect_t;

  canvas (size_t size, const style_manager &style_mgr);

  size_t get_size () const { return m_cells.get_size (); }

  void paint (coord_t coord, cell_t c);
  void paint_text (coord_t coord, const styled_string &text);

  void fill (rect_t rect, cell_t c);
  void debug_fill ();

  void print_to_pp (pretty_printer *pp,
		    const char *per_line_prefix = NULL) const;
  void debug (bool styled) const;

  const cell_t &get (coord_t coord) const
  {
    return m_cells.get (coord);
  }

 private:
  int get_final_x_in_row (int y) const;

  array2<cell_t, size_t, coord_t> m_cells;
  const style_manager &m_style_mgr;
};

} // namespace text_art

#endif /* GCC_TEXT_ART_CANVAS_H */
