/* AddressSanitizer, a fast memory error detector.
   Copyright (C) 2011, 2012 Free Software Foundation, Inc.
   Contributed by Kostya Serebryany <kcc@google.com>

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

#ifndef TREE_ASAN
#define TREE_ASAN

extern void asan_finish_file(void);

/* Shadow memory is found at
   (address >> ASAN_SHADOW_SHIFT) + targetm.asan_shadow_offset ().  */
#define ASAN_SHADOW_SHIFT	3

#endif /* TREE_ASAN */
