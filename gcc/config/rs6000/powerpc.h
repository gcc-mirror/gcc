/* Definitions of target machine for GNU compiler,
   for IBM RS/6000 PowerPC running AIX version 3.2.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.
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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include "rs6000/rs6000.h"

#undef ASM_SPEC
#define ASM_SPEC "-u -mppc"

#undef CPP_SPEC
#define CPP_SPEC "\
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
%{mcpu=601: -D_ARCH_PPC -D_ARCH_PWR} \
%{mcpu=mpc601: -D_ARCH_PPC -D_ARCH_PWR} \
%{mcpu=ppc601: -D_ARCH_PPC -D_ARCH_PWR} \
%{mcpu=603: -D_ARCH_PPC} \
%{mcpu=mpc603: -D_ARCH_PPC} \
%{mcpu=ppc603: -D_ARCH_PPC} \
%{mcpu=604: -D_ARCH_PPC} \
%{mcpu=mpc604: -D_ARCH_PPC} \
%{mcpu=ppc604: -D_ARCH_PPC}"

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_POWERPC | MASK_NEW_MNEMONICS)

#undef PROCESSOR_DEFAULT
#define PROCESSOR_DEFAULT PROCESSOR_PPC601
