/* Configuration for GNU C-compiler for hosting on Windows NT.
   using a unix style C library.
   Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

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

#define HOST_EXECUTABLE_SUFFIX ".exe"

/* Even though Cygwin tries to hide the DOS based filesystem, it
   still shows though at times.  */
#define HAVE_DOS_BASED_FILE_SYSTEM

/* We support both "/" and "\" since everybody tests both but we
   default to "/".  This is important because if gcc produces Win32
   paths containing backslashes, make and configure may treat the
   backslashes as escape characters.  Many Win32 programs use forward
   slashes so using a forward slash shouldn't be problematic from the
   perspective of wanting gcc to produce native Win32 paths.  */
#undef DIR_SEPARATOR_2
#define DIR_SEPARATOR_2 '\\'
