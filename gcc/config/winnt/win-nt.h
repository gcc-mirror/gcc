/* winnt.h  --  operating system specific defines to be used when
   targeting GCC for Windows NT 3.x.
   Copyright (C) 1994 Free Software Foundation, Inc.
   Contributed by Douglas B. Rupp (drupp@cs.washington.edu).

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   To use this file, put in your tm.h file something like:

	#include "?????.h"
	#include "svr4.h"

   followed by any really system-specific defines (or overrides of
   defines) which you find that you need.  For example, CPP_PREDEFINES
   is defined here with only the defined -Dunix and -DSVR4.  You should
   probably override that in your target-specific ?????svr4.h file
   with a set of defines that includes these, but also contains an
   appropriate define for the type of hardware that you are targeting. */

#undef LIB_SPEC
#define LIB_SPEC "libc.lib kernel32.lib"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC ""

#undef LINK_SPEC
#define LINK_SPEC "-align:0x1000 -subsystem:console -entry:mainCRTStartup \
  -stack:1000000,1000000"

#undef STANDARD_EXEC_PREFIX
#define STANDARD_EXEC_PREFIX ""

#undef STANDARD_STARTFILE_PREFIX
#define STANDARD_STARTFILE_PREFIX ""

#undef TOOLDIR_BASE_PREFIX
#define TOOLDIR_BASE_PREFIX ""

#ifdef STANDARD_INCLUDE_DIR
#undef STANDARD_INCLUDE_DIR
#endif
#define STANDARD_INCLUDE_DIR ""

#undef LOCAL_INCLUDE_DIR
#define LOCAL_INCLUDE_DIR ""

#undef INCLUDE_DEFAULTS
#define INCLUDE_DEFAULTS				\
  {							\
    { 0, 0, 0 }						\
  }

