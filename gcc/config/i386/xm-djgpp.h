/* Configuration for GNU C-compiler for Intel 80386 running DJGPP.
   Copyright (C) 1988, 1996, 1998, 1999, 2000 Free Software Foundation, Inc.

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

#define __MSDOS__ 1

#include "i386/xm-i386.h"

/* Use semicolons to separate elements of a path.  */
#define PATH_SEPARATOR ';'

#define EXECUTABLE_SUFFIX ".exe"

/* Even though we support "/", allow "\" since everybody tests both.  */
#define DIR_SEPARATOR '/'
#define DIR_SEPARATOR_2 '\\'

/* Allow test for DOS drive names.  */
#define HAVE_DOS_BASED_FILE_SYSTEM

/* System dependant initialization for collect2
   to tell system() to act like Unix.  */
#define COLLECT2_HOST_INITIALIZATION \
  do { __system_flags |= (__system_allow_multiple_cmds			\
		          | __system_emulate_chdir); } while (0)

/* Define a version appropriate for DOS.  */
#undef XREF_FILE_NAME
#define XREF_FILE_NAME(xref_file, file) \
  do { \
    const char xref_ext[] = ".gxref"; \
    strcpy (xref_file, file); \
    s = basename (xref_file); \
    t = strchr (s, '.'); \
    if (t) \
      strcpy (t, xref_ext); \
    else \
      strcat (xref_file, xref_ext); \
  while (0)

