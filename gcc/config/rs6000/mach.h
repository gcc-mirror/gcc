/* Definitions of target machine for GNU compiler,
   for IBM RS/6000 running MACH.
   Copyright (C) 1992, 1999 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@nyu.edu)

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

#define TARGET_AIX 0

/* Print subsidiary information on the compiler version in use.  */
#define TARGET_VERSION fprintf (stderr, " (Mach-RS/6000)");

/* We don't define AIX under MACH; instead we define `unix'.  */
#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()         \
  do                                     \
    {                                    \
      builtin_define_std ("rios");       \
      builtin_define ("_IBMR2");         \
      builtin_define_std ("unix");       \
      builtin_assert ("system=unix");    \
      builtin_assert ("system=mach");    \
      builtin_assert ("cpu=rs6000");     \
      builtin_assert ("machine=rs6000"); \
    }                                    \
  while (0)

/* Define different binder options for MACH.  */
#undef LINK_SPEC
#define LINK_SPEC \
 "-T0x10000000 -D0x20000000 -K %{!nostdlib:%{!r*:%{!e*:-e __start}}} \
  -bnoso -berrmsg -btextro -bhalt:4 -bnodelcsect"

/* MACH doesn't have atexit.  */
#define NEED_ATEXIT

/* Don't default to pcc-struct-return, because gcc is the only compiler, and
   we want to retain compatibility with older gcc versions.  */
#define DEFAULT_PCC_STRUCT_RETURN 0
