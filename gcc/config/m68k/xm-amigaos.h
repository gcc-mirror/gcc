/* Configuration for GNU C-compiler for m68k Amiga, running AmigaOS.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998, 2003
   Free Software Foundation, Inc.  
   Contributed by Markus M. Wild (wild@amiga.physik.unizh.ch).

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef _FCNTL_H_
#include <fcntl.h>
#endif

/* AmigaOS specific headers, such as from the Native Developer Update kits,
   go in SYSTEM_INCLUDE_DIR.  STANDARD_INCLUDE_DIR is the equivalent of
   Unix "/usr/include".  All other include paths are set in Makefile.  */

#define SYSTEM_INCLUDE_DIR	"/gg/os-include"
#define STANDARD_INCLUDE_DIR	"/gg/include"

#define STANDARD_EXEC_PREFIX_1	"/gg/libexec/gcc/"
#define STANDARD_EXEC_PREFIX_2	"/gg/lib/gcc/"
#define STANDARD_STARTFILE_PREFIX_1 "/gg/lib/"
#define STANDARD_STARTFILE_PREFIX_2 "/gg/lib/"

/* The AmigaOS stores file names with regard to upper/lower case, but actions
   on existing files are case independent on the standard filesystems.

   A good example of where this causes problems is the conflict between the C
   include file <string.h> and the C++ include file <String.h>, where the C++
   include file dir is searched first and thus causes includes of <string.h>
   to include <String.h> instead.

   In order to solve this problem we define the macro OPEN_CASE_SENSITIVE as
   the name of the function that takes the same args as open() and does case
   dependent opens.  */

#define OPEN_CASE_SENSITIVE(NAME, FLAGS, MODE) open ((NAME), (FLAGS) | O_CASE, (MODE))

/* On the AmigaOS, there are two pathname separators, '/' (DIR_SEPARATOR)
   and ':' (VOL_SEPARATOR).  DIR_SEPARATOR defaults to the correct
   character, so we don't have to explicitly set it.  */

#define DIR_SEPARATOR '/'
#define VOL_SEPARATOR ':'
#define DIR_SEPARATOR_2 VOL_SEPARATOR

/* Zap PREFIX_INCLUDE_DIR, since with the AmigaOS port it is the same as
   STANDARD_INCLUDE_DIR.  */

#undef PREFIX_INCLUDE_DIR 
