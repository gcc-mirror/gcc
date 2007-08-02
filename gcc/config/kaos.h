/* Definitions of target machine for GCC.
   common kaOS definitions for all architectures.
   Copyright (C) 2003, 2007 Free Software Foundation, Inc.

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

/* Specify predefined symbols in preprocessor.  */
#define TARGET_OS_CPP_BUILTINS()		\
    do {					\
	builtin_define ("__kaOS__");	\
    } while (0)


/* do not link any library implicitly for kaOS target.  */
#undef LIB_SPEC
#define LIB_SPEC ""
