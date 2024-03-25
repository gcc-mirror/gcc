/* Classes for printing labelled rulers.
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

#ifndef GCC_TEXT_ART_RULER_H
#define GCC_TEXT_ART_RULER_H

#include "text-art/canvas.h"

namespace text_art {

/* A way to annotate a series of ranges of canvas coordinates
   with text labels either above or, in this example, below:
     ├───────┬───────┼───────┬───────┼───────┬───────┤
             │               │               │
           label A         label B          label C
   with logic to ensure that the text labels don't overlap
   when printed.  */

class x_ruler
{
 public:
  enum class label_dir { ABOVE, BELOW };
  enum class label_kind
  {
    TEXT,
    TEXT_WITH_BORDER
  };

  x_ruler (label_dir dir)
  : m_label_dir (dir),
    m_size (canvas::size_t (0, 0)),
    m_has_layout (false)
  {}

  void add_label (const canvas::range_t &r,
		  styled_string text,
		  style::id_t style_id,
		  label_kind kind = label_kind::TEXT);

  canvas::size_t get_size ()
  {
    ensure_layout ();
    return m_size;
  }

  void paint_to_canvas (canvas &canvas,
			canvas::coord_t offset,
			const theme &theme);

  void debug (const style_manager &sm);

 private:
  /* A particular label within an x_ruler.
     Consider e.g.:

     #   x:  01234567890123456789012345678901234567890123456789
     # y: 0: ├───────┬───────┼───────┬───────┼───────┬───────┤
     #    1:         │               │               │
     #    2:       label A         label B          label C
     #

     Then "label A" is:

     #               m_connector_x == 8
     #               V
     #   x:  0123456789012
     # y: 0:         ┬
     #    1:         │
     #    2:       label A
     #   x:  0123456789012
     #             ^
     #             m_text_coord.x == 6

     and m_text_coord is (2, 6).
     The y cooordinates are stored with respect to label_dir::BELOW;
     for label_dir::ABOVE we flip them when painting the ruler.  */
  class label
  {
    friend class x_ruler;
  public:
    label (const canvas::range_t &range, styled_string text, style::id_t style_id,
	   label_kind kind);

    bool operator< (const label &other) const;

  private:
    canvas::range_t m_range;
    styled_string m_text;
    style::id_t m_style_id;
    label_kind m_kind;
    canvas::rect_t m_text_rect; // includes any border
    int m_connector_x;
  };

  void ensure_layout ();
  void update_layout ();
  int get_canvas_y (int rel_y) const;

  label_dir m_label_dir;
  std::vector<label> m_labels;
  canvas::size_t m_size;
  bool m_has_layout = false;

};

} // namespace text_art

#endif /* GCC_TEXT_ART_RULER_H */
