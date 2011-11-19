/* Output variables, constants and external declarations, for GNU compiler.
   Copyright (C) 2001, 2007, 2009 Free Software Foundation, Inc.
   Contributed by Douglas Rupp (rupp@gnat.com).

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#undef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE 64

#undef POINTER_SIZE
#define POINTER_SIZE 64

/* Defaults to "long int" */
#undef SIZE_TYPE
#undef PTRDIFF_TYPE
