/* CPP Library.
   Copyright (C) 1986, 1987, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000 Free Software Foundation, Inc.
   Contributed by Per Bothner, 1994-95.
   Based on CCCP program by Paul Rubin, June 1986
   Adapted to ANSI C, Richard Stallman, Jan 1987

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

/* This file contains data definitions shared between cpplib and
   tradcpp.  */

#include "config.h"
#include "system.h"
#include "cppdefault.h"

const struct default_include cpp_include_defaults[]
#ifdef INCLUDE_DEFAULTS
= INCLUDE_DEFAULTS;
#else
= {
#ifdef GPLUSPLUS_INCLUDE_DIR
    /* Pick up GNU C++ generic include files.  */
    { GPLUSPLUS_INCLUDE_DIR, "G++", 1, 1, 0 },
#endif
#ifdef GPLUSPLUS_TOOL_INCLUDE_DIR
    /* Pick up GNU C++ target-dependent include files.  */
    { GPLUSPLUS_TOOL_INCLUDE_DIR, "G++", 1, 1, 0 },
#endif
#ifdef GPLUSPLUS_BACKWARD_INCLUDE_DIR
    /* Pick up GNU C++ backward and deprecated include files.  */
    { GPLUSPLUS_BACKWARD_INCLUDE_DIR, "G++", 1, 1, 0 },
#endif
#ifdef LOCAL_INCLUDE_DIR
    /* /usr/local/include comes before the fixincluded header files.  */
    { LOCAL_INCLUDE_DIR, 0, 0, 1, 1 },
#endif
#ifdef PREFIX_INCLUDE_DIR
    { PREFIX_INCLUDE_DIR, 0, 0, 1, 0 },
#endif
#ifdef GCC_INCLUDE_DIR
    /* This is the dir for fixincludes and for gcc's private headers.  */
    { GCC_INCLUDE_DIR, "GCC", 0, 0, 0 },
#endif
#ifdef CROSS_INCLUDE_DIR
    /* One place the target system's headers might be.  */
    { CROSS_INCLUDE_DIR, "GCC", 0, 0, 0 },
#endif
#ifdef TOOL_INCLUDE_DIR
    /* Another place the target system's headers might be.  */
    { TOOL_INCLUDE_DIR, "BINUTILS", 0, 1, 0 },
#endif
#ifdef SYSTEM_INCLUDE_DIR
    /* Some systems have an extra dir of include files.  */
    { SYSTEM_INCLUDE_DIR, 0, 0, 0, 1 },
#endif
#ifdef STANDARD_INCLUDE_DIR
    /* /usr/include comes dead last.  */
    { STANDARD_INCLUDE_DIR, STANDARD_INCLUDE_COMPONENT, 0, 0, 1 },
#endif
    { 0, 0, 0, 0, 0 }
  };
#endif /* no INCLUDE_DEFAULTS */

#ifdef GCC_INCLUDE_DIR
const char cpp_GCC_INCLUDE_DIR[] = GCC_INCLUDE_DIR;
const size_t cpp_GCC_INCLUDE_DIR_len = sizeof GCC_INCLUDE_DIR - 8;
#else
const char cpp_GCC_INCLUDE_DIR[] = "";
const size_t cpp_GCC_INCLUDE_DIR_len = 0;
#endif

#ifdef TARGET_SYSTEM_ROOT
const char *cpp_SYSROOT = TARGET_SYSTEM_ROOT;
#else
const char *cpp_SYSROOT = "";
#endif
