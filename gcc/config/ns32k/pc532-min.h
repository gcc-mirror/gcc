/* Definitions of target machine for GNU compiler.
   PC532 with National 32532, running Minix.
   Works with pc532 Minix 1.5hybrid.
   Copyright (C) 1990 Free Software Foundation, Inc.

   Derived from SEQUENT NS32000, written originally
   by Bruce Culbertson <culberts@hplabs.hp.com>,
   hacked for easier fit in gcc by Jyrki Kuoppala <jkp@cs.hut.fi>.

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

#include "pc532.h"

/* Minix has crtso.o instead of crt0.o */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC  \
  "%{pg:gcrtso.o%s}%{!pg:%{p:mcrtso.o%s}%{!p:crtso.o%s}}"

/* our setjmp doesn't save registers, so we must tell gcc to save
   call-saved-regs in a function calling setjmp */

#define NON_SAVING_SETJMP (current_function_calls_setjmp)

/* at least with estdio there's no _cleanup() but we have atexit() */

#define HAVE_ATEXIT
