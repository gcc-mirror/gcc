/* UndefinedBehaviorSanitizer, undefined behavior detector.
   Copyright (C) 2013 Free Software Foundation, Inc.
   Contributed by Marek Polacek <polacek@redhat.com>

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

#ifndef GCC_C_UBSAN_H
#define GCC_C_UBSAN_H

extern tree ubsan_instrument_division (location_t, tree, tree);
extern tree ubsan_instrument_shift (location_t, enum tree_code, tree, tree);
extern tree ubsan_instrument_vla (location_t, tree);
extern tree ubsan_instrument_return (location_t);

#endif  /* GCC_C_UBSAN_H  */
