/* Definitions of target machine for GNU compiler,
   for Alpha NetBSD systems.
   Copyright (C) 1998 Free Software Foundation, Inc.

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

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_FP | MASK_FPREGS | MASK_GAS)

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-D_LONGLONG -Dnetbsd -Dunix " SUB_CPP_PREDEFINES

#undef LIB_SPEC
#define LIB_SPEC "%{pg:-lgmon} %{pg:-lc_p} %{!pg:-lc}"

/* Generate calls to memcpy, etc., not bcopy, etc. */
#define TARGET_MEM_FUNCTIONS

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)			\
	fputs ("\tlda $28,_mcount\n\tjsr $28,($28),_mcount\n", (FILE))

/* Show that we need a GP when profiling.  */
#define TARGET_PROFILING_NEEDS_GP
