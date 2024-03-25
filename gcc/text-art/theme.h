/* Classes for abstracting ascii vs unicode output.
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

#ifndef GCC_TEXT_ART_THEME_H
#define GCC_TEXT_ART_THEME_H

#include "text-art/canvas.h"
#include "text-art/box-drawing.h"

namespace text_art {

class theme
{
 public:
  enum class cell_kind
  {
    /* A left-hand edge of a range e.g. "├".  */
    X_RULER_LEFT_EDGE,

    /* Within a range e.g. "─".  */
    X_RULER_MIDDLE,

    /* A border between two neighboring ranges e.g. "┼".  */
    X_RULER_INTERNAL_EDGE,

    /* The connector with the text label within a range e.g. "┬".  */
    X_RULER_CONNECTOR_TO_LABEL_BELOW,

    /* As above, but when the text label is above the ruler.  */
    X_RULER_CONNECTOR_TO_LABEL_ABOVE,

    /* The vertical connection to a text label.  */
    X_RULER_VERTICAL_CONNECTOR,

    /* A right-hand edge of a range e.g. "┤".  */
    X_RULER_RIGHT_EDGE,

    TEXT_BORDER_HORIZONTAL,
    TEXT_BORDER_VERTICAL,
    TEXT_BORDER_TOP_LEFT,
    TEXT_BORDER_TOP_RIGHT,
    TEXT_BORDER_BOTTOM_LEFT,
    TEXT_BORDER_BOTTOM_RIGHT,

    Y_ARROW_UP_HEAD,
    Y_ARROW_UP_TAIL,
    Y_ARROW_DOWN_HEAD,
    Y_ARROW_DOWN_TAIL,
  };

  virtual ~theme () = default;

  virtual bool unicode_p () const = 0;
  virtual bool emojis_p () const = 0;

  virtual canvas::cell_t
  get_line_art (directions line_dirs) const = 0;

  canvas::cell_t get_cell (enum cell_kind kind, unsigned style_idx) const
  {
    return canvas::cell_t (get_cppchar (kind), false, style_idx);
  }

  virtual cppchar_t get_cppchar (enum cell_kind kind) const = 0;

  enum class y_arrow_dir { UP, DOWN };
  void paint_y_arrow (canvas &canvas,
		      int x,
		      canvas::range_t y_range,
		      y_arrow_dir dir,
		      style::id_t style_id) const;
};

class ascii_theme : public theme
{
 public:
  bool unicode_p () const final override { return false; }
  bool emojis_p () const final override { return false; }

  canvas::cell_t
  get_line_art (directions line_dirs) const final override;

  cppchar_t get_cppchar (enum cell_kind kind) const final override;
};

class unicode_theme : public theme
{
 public:
  bool unicode_p () const final override { return true; }
  bool emojis_p () const override { return false; }

  canvas::cell_t
  get_line_art (directions line_dirs) const final override;

  cppchar_t get_cppchar (enum cell_kind kind) const final override;
};

class emoji_theme : public unicode_theme
{
public:
  bool emojis_p () const final override { return true; }
};

} // namespace text_art

#endif /* GCC_TEXT_ART_THEME_H */
