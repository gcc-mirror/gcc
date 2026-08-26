/* Verify that prerequisites of files including this header have been included
   through system.h.
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


#ifdef WANT_FUNCTIONAL
# include <functional>
# ifndef INCLUDE_FUNCTIONAL
#  ifdef GCC_SYSTEM_H
#   warning "<functional> included after system.h.  " \
   "Please define INCLUDE_FUNCTIONAL before including system.h"
#  endif
# endif
# undef WANT_FUNCTIONAL
#endif
