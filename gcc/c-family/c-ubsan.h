/* UndefinedBehaviorSanitizer, undefined behavior detector.
   Copyright (C) 2013-2021 Free Software Foundation, Inc.
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
extern tree ubsan_instrument_bounds (location_t, tree, tree *, bool);
extern bool ubsan_array_ref_instrumented_p (const_tree);
extern void ubsan_maybe_instrument_array_ref (tree *, bool);
extern void ubsan_maybe_instrument_reference (tree *);
extern void ubsan_maybe_instrument_member_call (tree, bool);

#endif  /* GCC_C_UBSAN_H  */
