/* Definitions of target machine for GNU compiler,
   for IBM RS/6000 POWER running AIX version 3.x with the fixed assembler.
   Copyright (C) 1995, 1996, 2000 Free Software Foundation, Inc.
   Contributed by Jason Merrill (jason@cygnus.com).

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


#include "rs6000/rs6000.h"
#include "rs6000/aix.h"

/* Tell the assembler to assume that all undefined names are external.  */

#undef ASM_SPEC
#define ASM_SPEC "-u %(asm_cpu)"

#undef ASM_DEFAULT_SPEC
#define ASM_DEFAULT_SPEC "-mpwr"

/* Define the options for the binder: Start text at 512, align all segments
   to 512 bytes, and warn if there is text relocation.

   The -bhalt:4 option supposedly changes the level at which ld will abort,
   but it also suppresses warnings about multiply defined symbols and is
   used by the AIX cc command.  So we use it here.

   -bnodelcsect undoes a poor choice of default relating to multiply-defined
   csects.  See AIX documentation for more information about this.

   -bM:SRE tells the linker that the output file is Shared REusable.  Note
   that to actually build a shared library you will also need to specify an
   export list with the -Wl,-bE option.

   If -mcpu=common, export the architecture dependent multiply/divide routines
   as per README.RS6000.  */

#undef	LINK_SPEC
#ifndef CROSS_COMPILE
#define LINK_SPEC "-T512 -H512 %{!r:-btextro} -bhalt:4 -bnodelcsect\
   %{static:-bnso -bI:/lib/syscalls.exp} \
   %{mcpu=common: milli.exp%s} \
   %{!shared:%{g*:-bexport:/usr/lib/libg.exp}} %{shared:-bM:SRE}"
#else
#define LINK_SPEC "-T512 -H512 %{!r:-btextro} -bhalt:4 -bnodelcsect\
   %{static:-bnso} \
   %{mcpu=common: milli.exp%s} \
   %{shared:-bM:SRE}"
#endif
