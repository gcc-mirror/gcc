/* Definitions of target machine for GNU compiler.  System/370 version.
   Copyright (C) 1989, 1993, 1995, 1996, 1997 Free Software Foundation, Inc.
   Contributed by Jan Stein (jan@cd.chalmers.se).
   Modified for OS/390 OpenEdition by Dave Pitts (dpitts@cozx.com)

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

#define TARGET_VERSION printf (" (370/OpenEdition)");

/* Specify that we're generating code for the Language Environment */

#define LE370 1
#define LONGEXTERNAL 1
#define TARGET_EBCDIC 1
#define TARGET_HLASM 1

/* Options for the preprocessor for this target machine.  */

#define CPP_SPEC "-trigraphs"

/* Options for this target machine.  */

#define LIB_SPEC ""
#define LIBGCC_SPEC ""
#define STARTFILE_SPEC "/usr/local/lib/gccmain.o"

/* Target OS preprocessor built-ins.  */
#define TARGET_OS_CPP_BUILTINS()		\
    do {					\
	builtin_define_std ("unix");		\
	builtin_define_std ("UNIX");		\
	builtin_define_std ("openedition");	\
	builtin_define ("__i370__");		\
	builtin_assert ("system=openedition");	\
	builtin_assert ("system=unix");		\
    } while (0)

/* Include system common definitions */

#include "config/i370/i370.h"

