/* Definitions of target machine for GNU compiler
   for Alpha Linux-based GNU systems using ECOFF.
   Copyright (C) 1996, 1997 Free Software Foundation, Inc.
   Contributed by Bob Manson.

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

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (Alpha GNU/Linux for ECOFF)");

#undef CPP_SUBTARGET_SPEC
#define CPP_SUBTARGET_SPEC "-D__ECOFF__"

#undef LINK_SPEC
#define LINK_SPEC "-G 8 %{O*:-O3} %{!O*:-O1}"

/* stabs get slurped by the assembler into a queer ecoff format.  */
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

/* We support #pragma.  */
#define HANDLE_SYSV_PRAGMA
