/* Configuration for GNU C-compiler for hosting on Windows32.
   using GNU tools and the Windows32 API Library.
   Copyright (C) 1997, 1998, 1999, 2001 Free Software Foundation, Inc.

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

/* Even though we support "/", allow "\" since everybody tests both.  */
#define DIR_SEPARATOR '\\'
#define DIR_SEPARATOR_2 '/'

/* Mingw32 does not try to hide the underlying DOS-based file system
   like Cygwin does.  */
#define HAVE_DOS_BASED_FILE_SYSTEM

#define HOST_EXECUTABLE_SUFFIX ".exe"

#undef PATH_SEPARATOR
#define PATH_SEPARATOR ';'
