/* Definitions of target machine for GNU compiler,
   for IBM RS/6000 PowerPC running AIX version 3.2.
   Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.
   Contributed by David Edelsohn (edelsohn@npac.syr.edu).

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

#undef ASM_SPEC
#define ASM_SPEC "-u \
%{!mcpu*: \
  %{mpower2: -mpwrx} \
  %{mpowerpc*: %{!mpower: -mppc}} \
  %{mno-powerpc: %{!mpower: %{!mpower2: -mcom}}} \
  %{mno-powerpc: %{mpower: %{!mpower2: -mpwr}}} \
  %{!mno-powerpc: %{mpower: -m601}} \
  %{!mno-powerpc: %{!mpower: -mppc}}} \
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

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-D_IBMR2 -D_POWER -D_AIX -D_AIX32 \
-Asystem(unix) -Asystem(aix) -Acpu(powerpc) -Amachine(powerpc)"

#undef CPP_SPEC
#define CPP_SPEC "\
%{posix: -D_POSIX_SOURCE} \
%{!mcpu*: \
  %{mpower: %{!mpower2: -D_ARCH_PWR}} \
  %{mpower2: -D_ARCH_PWR2} \
  %{mpowerpc*: -D_ARCH_PPC} \
  %{mno-powerpc: %{!mpower: %{!mpower2: -D_ARCH_COM}}} \
  %{!mno-powerpc: -D_ARCH_PPC}} \
%{mcpu=common: -D_ARCH_COM} \
%{mcpu=power: -D_ARCH_PWR} \
%{mcpu=powerpc: -D_ARCH_PPC} \
%{mcpu=rios: -D_ARCH_PWR} \
%{mcpu=rios1: -D_ARCH_PWR} \
%{mcpu=rios2: -D_ARCH_PWR2} \
%{mcpu=rsc: -D_ARCH_PWR} \
%{mcpu=rsc1: -D_ARCH_PWR} \
%{mcpu=403: -D_ARCH_PPC} \
%{mcpu=601: -D_ARCH_PPC -D_ARCH_PWR} \
%{mcpu=603: -D_ARCH_PPC} \
%{mcpu=604: -D_ARCH_PPC}"

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_POWERPC | MASK_NEW_MNEMONICS)

#undef PROCESSOR_DEFAULT
#define PROCESSOR_DEFAULT PROCESSOR_PPC601

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
