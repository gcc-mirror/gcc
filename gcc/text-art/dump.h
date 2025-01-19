/* Templates for dumping objects.
   Copyright (C) 2024-2025 Free Software Foundation, Inc.
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

#ifndef GCC_TEXT_ART_DUMP_H
#define GCC_TEXT_ART_DUMP_H

#include "tree-diagnostic.h"
#include "text-art/canvas.h"
#include "text-art/widget.h"
#include "text-art/dump-widget-info.h"

/* A family of templates for dumping objects via the text_art::widget
   system.
   Any type T that has a make_dump_widget member function ought to be
   dumpable via these functions.  */

namespace text_art {

/* Dump OBJ to PP, using OBJ's make_dump_widget member function.  */

template <typename T>
void dump_to_pp (const T &obj, text_art::theme *theme, pretty_printer *pp)
{
  if (!theme)
    return;

  style_manager sm;
  style tree_style (get_style_from_color_cap_name ("note"));

  style::id_t tree_style_id (sm.get_or_create_id (tree_style));

  dump_widget_info dwi (sm, *theme, tree_style_id);
  if (std::unique_ptr<widget> w = obj.make_dump_widget (dwi))
    {
      text_art::canvas c (w->to_canvas (dwi.m_sm));
      c.print_to_pp (pp);
    }
}

/* Dump OBJ to OUTF, using OBJ's make_dump_widget member function.  */

template <typename T>
void dump_to_file (const T &obj, FILE *outf)
{
  tree_dump_pretty_printer pp (outf);
  text_art::theme *theme = global_dc->get_diagram_theme ();
  dump_to_pp (obj, theme, &pp);
}

/* Dump OBJ to stderr, using OBJ's make_dump_widget member function.  */

template <typename T>
void dump (const T &obj)
{
  dump_to_file (obj, stderr);
}

} // namespace text_art

#endif /* GCC_TEXT_ART_DUMP_H */
