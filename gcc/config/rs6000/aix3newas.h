/* Definitions of target machine for GNU compiler,
   for IBM RS/6000 POWER running AIX version 3.x with the fixed assembler.
   Copyright (C) 1995 Free Software Foundation, Inc.
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

/* Tell the assembler to assume that all undefined names are external.  */

#undef ASM_SPEC
#define ASM_SPEC "-u \
%{!mcpu*: \
  %{mpower: %{!mpowerpc*: %{!mpower2: -mpwr}}} \
  %{mpower2: -mpwrx} \
  %{mno-power: %{mpowerpc*: -mppc}} \
  %{mno-power: %{!mpowerpc*: -mcom}} \
  %{!mno-power: %{mpowerpc*: -m601}} \
  %{!mno-power: %{!mpowerpc*: %{!mpower2: -mpwr}}}} \
%{mcpu=common: -mcom} \
%{mcpu=power: -mpwr} \
%{mcpu=powerpc: -mppc} \
%{mcpu=rios: -mpwr} \
%{mcpu=rios1: -mpwr} \
%{mcpu=rios2: -mpwrx} \
%{mcpu=rsc: -mpwr} \
%{mcpu=rsc1: -mpwr} \
%{mcpu=403: -mppc} \
%{mcpu=601: -m601} \
%{mcpu=603: -mppc} \
%{mcpu=604: -mppc}"

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
#define LINK_SPEC "-T512 -H512 %{!r:-btextro} -bhalt:4 -bnodelcsect\
   %{static:-bnso -bI:/lib/syscalls.exp} \
   %{mcpu=common: milli.exp%s} \
   %{!shared:%{g*:-bexport:/usr/lib/libg.exp}} %{shared:-bM:SRE}"

/* These are not necessary when we pass -u to the assembler, and undefining
   them saves a great deal of space in object files.  */

#undef ASM_OUTPUT_EXTERNAL
#undef ASM_OUTPUT_EXTERNAL_LIBCALL
#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME)	\
{ rtx _symref = XEXP (DECL_RTL (DECL), 0);	\
  if ((TREE_CODE (DECL) == VAR_DECL		\
       || TREE_CODE (DECL) == FUNCTION_DECL)	\
      && (NAME)[0] != '*'			\
      && (NAME)[strlen (NAME) - 1] != ']')	\
    {						\
      char *_name = (char *) permalloc (strlen (XSTR (_symref, 0)) + 5); \
      strcpy (_name, XSTR (_symref, 0));	\
      strcat (_name, TREE_CODE (DECL) == FUNCTION_DECL ? "[DS]" : "[RW]"); \
      XSTR (_symref, 0) = _name;		\
    }						\
}
