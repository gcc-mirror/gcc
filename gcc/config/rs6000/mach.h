/* Definitions of target machine for GNU compiler,
   for IBM RS/6000 running MACH.
   Copyright (C) 1992 Free Software Foundation, Inc.
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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include "rs6000.h"

/* We don't define AIX under MACH; instead we define `unix'.  */
#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Drios -D_IBMR2 -Dunix"

/* Define different binder options for MACH.  */
#undef LINK_SPEC
#define LINK_SPEC \
 "-T0x10000000 -D0x20000000 -K %{!nostdlib:%{!r*:%{!e*:-e __start}}} \
  -bnoso -berrmsg -btextro -bhalt:4 -bnodelcsect"

/* MACH doesn't have atexit.  */
#undef HAVE_ATEXIT
