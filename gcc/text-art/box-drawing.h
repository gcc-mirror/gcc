/* Procedural lookup of box drawing characters.
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

#ifndef GCC_TEXT_ART_BOX_DRAWING_H
#define GCC_TEXT_ART_BOX_DRAWING_H

#include "text-art/types.h"

namespace text_art {

extern cppchar_t get_box_drawing_char (directions line_dirs);

} // namespace text_art

#endif /* GCC_TEXT_ART_BOX_DRAWING_H */
