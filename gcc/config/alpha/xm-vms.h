/* Configuration for GNU C-compiler for openVMS/Alpha.
   Copyright (C) 1996, 1997, 2001 Free Software Foundation, Inc.
   Contributed by Klaus Kaempf (kkaempf@progis.de).

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

#define HOST_WIDE_INT long long
#define HOST_BITS_PER_WIDE_INT 64

/* A couple of conditionals for execution machine are controlled here.  */
#ifndef VMS
#define VMS
#endif

/* Define a local equivalent (sort of) for unlink */
#define unlink remove

/* Causes exit() to be redefined to __posix_exit() and
   Posix compatible failure and success codes to be used */
#define _POSIX_EXIT 1

/* Open files in stream mode if not otherwise explicitly specified */
#define __UNIX_FOPEN 1

/* Write to stdout using fputc to avoid record terminators in pipes */
#define __UNIX_FWRITE 1

#define STDC_HEADERS 1

#define HOST_EXECUTABLE_SUFFIX ".exe"
#define HOST_OBJECT_SUFFIX ".obj"

#define DUMPFILE_FORMAT "_%02d_"
