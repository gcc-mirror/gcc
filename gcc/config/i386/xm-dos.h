/* Configuration for GNU C-compiler for Intel 80386 running DOS.
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.

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

#include "i386/xm-i386.h"

/* Use semicolons to separate elements of a path.  */
#define PATH_SEPARATOR ';'

/* Use backslashs to separate levels of directory.  */
#define DIR_SEPARATOR '\\'
#define DIR_SEPARATOR_2 '/'

/* Allow checks for drive names. */
#define HAVE_DOS_BASED_FILE_SYSTEM

/* Suffix for executable file names.  */
#define EXECUTABLE_SUFFIX ".exe"

#define MKTEMP_EACH_FILE 1

#define NO_PRECOMPILES 1
