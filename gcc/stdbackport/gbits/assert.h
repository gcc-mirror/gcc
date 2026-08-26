/* Configurable assert.  -*- C++ -*-
   Copyright (C) 2026 The GNU Toolchain Authors.

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


#ifndef GCC_BACKPORT_ASSERT_H
#define GCC_BACKPORT_ASSERT_H 1

/* Useful to override if running testsuites.  */
#ifndef gccbackport_assert
# define gccbackport_assert(x) gcc_assert (x)
#endif

#endif // GCC_BACKPORT_ASSERT_H
