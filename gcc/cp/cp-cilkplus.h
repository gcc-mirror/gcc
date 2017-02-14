/* C++-specific tree lowering bits; see also c-gimplify.c and tree-gimple.c.

   Copyright (C) 2002-2017 Free Software Foundation, Inc.
   Contributed by Jason Merrill <jason@redhat.com>

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

#ifndef GCC_CP_CILKPLUS_H
#define GCC_CP_CILKPLUS_H

extern bool cilk_cp_detect_spawn_and_unwrap (tree *);
extern bool cpp_validate_cilk_plus_loop (tree);

#endif /* ! GCC_CP_CILKPLUS_H */
