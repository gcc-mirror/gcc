/* Definitions of target machine for GNU compiler, for SunOS 4.x
   Copyright (C) 1994, 1999 Free Software Foundation, Inc.

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

#undef SUNOS4_SHARED_LIBRARIES
#define SUNOS4_SHARED_LIBRARIES 1

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dsparc -Dsun -Dunix -Asystem=unix -Asystem=bsd"

#define LIB_SPEC "%{!shared:%{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p} %{g:-lg}}"

/* Provide required defaults for linker -e and -d switches.  */

#define LINK_SPEC \
 "%{!shared:%{!nostdlib:%{!r*:%{!e*:-e start}}} -dc -dp} %{static:-Bstatic} \
  %{assert*} %{shared:%{!mimpure-text:-assert pure-text}}"

/* Use N_BINCL stabs.  */

#define DBX_USE_BINCL

/* The Sun as doesn't like unaligned data.  */
#define DWARF2_UNWIND_INFO 0

/* SunOS has on_exit instead of atexit.  */
/* The man page says it returns int.  */
extern int on_exit PARAMS ((void *, void *));
#define ON_EXIT(FUNC) on_exit ((FUNC), 0)
#define NEED_ATEXIT
