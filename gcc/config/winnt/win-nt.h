/* Operating system specific defines to be used when targeting GCC for
   Windows NT 3.x.
   Copyright (C) 1994, 1995 Free Software Foundation, Inc.
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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#define TARGET_MEM_FUNCTIONS

#undef STARTFILE_SPEC
#define STARTFILE_SPEC ""

#undef LINK_SPEC
#define LINK_SPEC "-stack 5000000,5000000 -noinhibit-exec %{g}"

#undef CPP_SPEC
#define CPP_SPEC "-lang-c-c++-comments"

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

#undef STDC_VALUE
#define STDC_VALUE 0
