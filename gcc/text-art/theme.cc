/* Classes for abstracting ascii vs unicode output.
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
#define INCLUDE_MEMORY
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "pretty-print.h"
#include "selftest.h"
#include "text-art/selftests.h"
#include "text-art/ruler.h"
#include "text-art/theme.h"

using namespace text_art;

/* class theme.  */

void
theme::paint_y_arrow (canvas &canvas,
		      int canvas_x,
		      canvas::range_t y_range,
		      y_arrow_dir dir,
		      style::id_t style_id) const
{
  int canvas_y;
  int delta_y;
  const canvas::cell_t head (get_cppchar (dir == y_arrow_dir::UP
					  ? cell_kind::Y_ARROW_UP_HEAD
					  : cell_kind::Y_ARROW_DOWN_HEAD),
			     false, style_id);
  const canvas::cell_t tail (get_cppchar (dir == y_arrow_dir::UP
					  ? cell_kind::Y_ARROW_UP_TAIL
					  : cell_kind::Y_ARROW_DOWN_TAIL),
			     false, style_id);
  if (dir == y_arrow_dir::UP)
    {
      canvas_y = y_range.get_max ();
      delta_y = -1;
    }
  else
    {
      canvas_y = y_range.get_min ();
      delta_y = 1;
    }
  for (int len = y_range.get_size (); len; len--)
    {
      const canvas::cell_t cell = (len > 1) ? tail : head;
      canvas.paint (canvas::coord_t (canvas_x, canvas_y), cell);
      canvas_y += delta_y;
    }
}

/* class ascii_theme : public theme.  */

canvas::cell_t
ascii_theme::get_line_art (directions line_dirs) const
{
  if (line_dirs.m_up
      && line_dirs.m_down
      && !(line_dirs.m_left || line_dirs.m_right))
    return canvas::cell_t ('|');
  if (line_dirs.m_left
      && line_dirs.m_right
      && !(line_dirs.m_up || line_dirs.m_down))
    return canvas::cell_t ('-');
  if (line_dirs.m_up
      || line_dirs.m_down
      || line_dirs.m_left
      || line_dirs.m_right)
    return canvas::cell_t ('+');
  return canvas::cell_t (' ');
}

cppchar_t
ascii_theme::get_cppchar (enum cell_kind kind) const
{
  switch (kind)
    {
    default:
      gcc_unreachable ();
    case cell_kind::X_RULER_LEFT_EDGE:
      return '|';
    case cell_kind::X_RULER_MIDDLE:
      return '~';
    case cell_kind::X_RULER_INTERNAL_EDGE:
      return '|';
    case cell_kind::X_RULER_CONNECTOR_TO_LABEL_BELOW:
    case cell_kind::X_RULER_CONNECTOR_TO_LABEL_ABOVE:
      return '+';
    case cell_kind::X_RULER_RIGHT_EDGE:
      return '|';
    case cell_kind::X_RULER_VERTICAL_CONNECTOR:
      return '|';

    case cell_kind::TEXT_BORDER_HORIZONTAL:
      return '-';
    case cell_kind::TEXT_BORDER_VERTICAL:
      return '|';
    case cell_kind::TEXT_BORDER_TOP_LEFT:
    case cell_kind::TEXT_BORDER_TOP_RIGHT:
    case cell_kind::TEXT_BORDER_BOTTOM_LEFT:
    case cell_kind::TEXT_BORDER_BOTTOM_RIGHT:
      return '+';

    case cell_kind::Y_ARROW_UP_HEAD: return '^';
    case cell_kind::Y_ARROW_DOWN_HEAD: return 'v';

    case cell_kind::Y_ARROW_UP_TAIL:
    case cell_kind::Y_ARROW_DOWN_TAIL:
      return '|';

    case cell_kind::INTERPROCEDURAL_PUSH_FRAME_LEFT:
      return '+';
    case cell_kind::INTERPROCEDURAL_PUSH_FRAME_MIDDLE:
      return '-';
    case cell_kind::INTERPROCEDURAL_PUSH_FRAME_RIGHT:
      return '>';
    case cell_kind::INTERPROCEDURAL_DEPTH_MARKER:
      return '|';
    case cell_kind::INTERPROCEDURAL_POP_FRAMES_LEFT:
      return '<';
    case cell_kind::INTERPROCEDURAL_POP_FRAMES_MIDDLE:
      return '-';
    case cell_kind::INTERPROCEDURAL_POP_FRAMES_RIGHT:
      return '+';

    case cell_kind::CFG_RIGHT:
      return '-';
    case cell_kind::CFG_FROM_RIGHT_TO_DOWN:
      return '+';
    case cell_kind::CFG_DOWN:
      return '|';
    case cell_kind::CFG_FROM_DOWN_TO_LEFT:
      return '+';
    case cell_kind::CFG_LEFT:
      return '-';
    case cell_kind::CFG_FROM_LEFT_TO_DOWN:
      return '+';
    case cell_kind::CFG_FROM_DOWN_TO_RIGHT:
      return '+';

    case cell_kind::TREE_CHILD_NON_FINAL:
      return '+';
    case cell_kind::TREE_CHILD_FINAL:
      return '`';
    case cell_kind::TREE_X_CONNECTOR:
      return '-';
    case cell_kind::TREE_Y_CONNECTOR:
      return '|';
    }
}

/* class unicode_theme : public theme.  */

canvas::cell_t
unicode_theme::get_line_art (directions line_dirs) const
{
  return canvas::cell_t (get_box_drawing_char (line_dirs));
}

cppchar_t
unicode_theme::get_cppchar (enum cell_kind kind) const
{
  switch (kind)
    {
    default:
      gcc_unreachable ();
    case cell_kind::X_RULER_LEFT_EDGE:
      return 0x251C; /* "├": U+251C: BOX DRAWINGS LIGHT VERTICAL AND RIGHT */
    case cell_kind::X_RULER_MIDDLE:
      return 0x2500; /* "─": U+2500: BOX DRAWINGS LIGHT HORIZONTAL */
    case cell_kind::X_RULER_INTERNAL_EDGE:
      return 0x253C; /* "┼": U+253C: BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL */
    case cell_kind::X_RULER_CONNECTOR_TO_LABEL_BELOW:
      return 0x252C; /* "┬": U+252C: BOX DRAWINGS LIGHT DOWN AND HORIZONTAL */
    case cell_kind::X_RULER_CONNECTOR_TO_LABEL_ABOVE:
      return 0x2534; /* "┴": U+2534: BOX DRAWINGS LIGHT UP AND HORIZONTAL */
    case cell_kind::X_RULER_RIGHT_EDGE:
      return 0x2524; /* "┤": U+2524: BOX DRAWINGS LIGHT VERTICAL AND LEFT */
    case cell_kind::X_RULER_VERTICAL_CONNECTOR:
      return 0x2502; /* "│": U+2502: BOX DRAWINGS LIGHT VERTICAL */

    case cell_kind::TEXT_BORDER_HORIZONTAL:
      return 0x2500; /* "─": U+2500: BOX DRAWINGS LIGHT HORIZONTAL */
    case cell_kind::TEXT_BORDER_VERTICAL:
      return 0x2502; /* "│": U+2502: BOX DRAWINGS LIGHT VERTICAL */

    /* Round corners.  */
    case cell_kind::TEXT_BORDER_TOP_LEFT:
      return 0x256D; /* "╭": U+256D BOX DRAWINGS LIGHT ARC DOWN AND RIGHT.  */
    case cell_kind::TEXT_BORDER_TOP_RIGHT:
      return 0x256E; /* "╮": U+256E BOX DRAWINGS LIGHT ARC DOWN AND LEFT.  */
    case cell_kind::TEXT_BORDER_BOTTOM_LEFT:
      return 0x2570; /* "╰": U+2570 BOX DRAWINGS LIGHT ARC UP AND RIGHT.  */
    case cell_kind::TEXT_BORDER_BOTTOM_RIGHT:
      return 0x256F; /* "╯": U+256F BOX DRAWINGS LIGHT ARC UP AND LEFT.  */

    case cell_kind::Y_ARROW_UP_HEAD:
      return '^';
    case cell_kind::Y_ARROW_DOWN_HEAD:
      return 'v';
    case cell_kind::Y_ARROW_UP_TAIL:
    case cell_kind::Y_ARROW_DOWN_TAIL:
      return 0x2502; /* "│": U+2502: BOX DRAWINGS LIGHT VERTICAL */

    case cell_kind::INTERPROCEDURAL_PUSH_FRAME_LEFT:
      return 0x2514; /* "└": U+2514: BOX DRAWINGS LIGHT UP AND RIGHT  */
    case cell_kind::INTERPROCEDURAL_PUSH_FRAME_MIDDLE:
      return 0x2500; /* "─": U+2500: BOX DRAWINGS LIGHT HORIZONTAL */
    case cell_kind::INTERPROCEDURAL_PUSH_FRAME_RIGHT:
      return '>';
    case cell_kind::INTERPROCEDURAL_DEPTH_MARKER:
      return 0x2502; /* "│": U+2502: BOX DRAWINGS LIGHT VERTICAL */
    case cell_kind::INTERPROCEDURAL_POP_FRAMES_LEFT:
      return '<';
    case cell_kind::INTERPROCEDURAL_POP_FRAMES_MIDDLE:
      return 0x2500; /* "─": U+2500: BOX DRAWINGS LIGHT HORIZONTAL */
    case cell_kind::INTERPROCEDURAL_POP_FRAMES_RIGHT:
      return 0x2518; /* "┘": U+2518: BOX DRAWINGS LIGHT UP AND LEFT.  */

    case cell_kind::CFG_RIGHT:
      return 0x2500; /* "─": U+2500: BOX DRAWINGS LIGHT HORIZONTAL */
    case cell_kind::CFG_FROM_RIGHT_TO_DOWN:
      return 0x2510; /* "┐": U+2510: BOX DRAWINGS LIGHT DOWN AND LEFT */
    case cell_kind::CFG_DOWN:
      return 0x2502; /* "│": U+2502: BOX DRAWINGS LIGHT VERTICAL */
    case cell_kind::CFG_FROM_DOWN_TO_LEFT:
      return 0x2518; /* "┘": U+2518: BOX DRAWINGS LIGHT UP AND LEFT.  */
    case cell_kind::CFG_LEFT:
      return 0x2500; /* "─": U+2500: BOX DRAWINGS LIGHT HORIZONTAL */
    case cell_kind::CFG_FROM_LEFT_TO_DOWN:
      return 0x250c; /* "┌" U+250C: BOX DRAWINGS LIGHT DOWN AND RIGHT */
    case cell_kind::CFG_FROM_DOWN_TO_RIGHT:
      return 0x2514; /* "└": U+2514: BOX DRAWINGS LIGHT UP AND RIGHT  */

    case cell_kind::TREE_CHILD_NON_FINAL:
      return 0x251C; /* "├": U+251C: BOX DRAWINGS LIGHT VERTICAL AND RIGHT */
    case cell_kind::TREE_CHILD_FINAL:
      return 0x2570; /* "╰": U+2570 BOX DRAWINGS LIGHT ARC UP AND RIGHT.  */
    case cell_kind::TREE_X_CONNECTOR:
      return 0x2500; /* "─": U+2500: BOX DRAWINGS LIGHT HORIZONTAL */
    case cell_kind::TREE_Y_CONNECTOR:
      return 0x2502; /* "│": U+2502: BOX DRAWINGS LIGHT VERTICAL */
    }
}
