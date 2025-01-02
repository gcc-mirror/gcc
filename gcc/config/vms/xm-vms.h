/* Configuration for GCC for hosting on VMS
   using a Unix style C library.
   Copyright (C) 1996-2025 Free Software Foundation, Inc.

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

/* A couple of conditionals for execution machine are controlled here.  */
#ifndef VMS
#define VMS
#endif

/* Causes exit() to be redefined to __posix_exit() and
   Posix compatible failure and success codes to be used.  */
#define _POSIX_EXIT 1

/* Open files in stream mode if not otherwise explicitly specified.  */
#define __UNIX_FOPEN 1

/* Write to stdout using fputc to avoid record terminators in pipes.  */
#define __UNIX_FWRITE 1

#define STDC_HEADERS 1

#define HOST_EXECUTABLE_SUFFIX ".exe"
#define HOST_OBJECT_SUFFIX ".obj"

#define DUMPFILE_FORMAT "_%02d_"

#define DELETE_IF_ORDINARY(NAME,ST,VERBOSE_FLAG)           \
do                                                         \
  {                                                        \
    while (stat (NAME, &ST) >= 0 && S_ISREG (ST.st_mode))  \
      if (unlink (NAME) < 0)                               \
	{                                                  \
	  if (VERBOSE_FLAG)                                \
	    perror_with_name (NAME);                       \
	  break;                                           \
	}                                                  \
  } while (0)

/* If 64 bit pointers are used, use 64 bit specifier.  */

#if (defined (__INITIAL_POINTER_SIZE) && __INITIAL_POINTER_SIZE == 64) \
  || defined (__LONG_POINTERS)
#define HOST_LONG_FORMAT "ll"
#define HOST_PTR_PRINTF "%llp"
#endif

#define STANDARD_STARTFILE_PREFIX "/gnu/lib/"
