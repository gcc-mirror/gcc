/* Target definitions for GNU compiler for a little endian PowerPC
   running System V.4
   Copyright (C) 1995, Free Software Foundation, Inc.
   Contributed by Cygnus Support.

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

#include "rs6000/sysv4.h"

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_POWERPC | MASK_NEW_MNEMONICS | MASK_LITTLE_ENDIAN)

#undef CPP_SPEC
#define CPP_SPEC "\
%{posix: -D_POSIX_SOURCE} \
%{mrelocatable: -D_RELOCATABLE} \
%{mbig: -D_BIG_ENDIAN -Amachine(bigendian)} \
%{mbig-endian: -D_BIG_ENDIAN -Amachine(bigendian)} \
%{!mbig: %{!mbig-endian: -D_LITTLE_ENDIAN -Amachine(littleendian)}} \
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
%{mcpu=mpc403: -D_ARCH_PPC} \
%{mcpu=ppc403: -D_ARCH_PPC} \
%{mcpu=601: -D_ARCH_PPC -D_ARCH_PWR} \
%{mcpu=mpc601: -D_ARCH_PPC -D_ARCH_PWR} \
%{mcpu=ppc601: -D_ARCH_PPC -D_ARCH_PWR} \
%{mcpu=603: -D_ARCH_PPC} \
%{mcpu=mpc603: -D_ARCH_PPC} \
%{mcpu=ppc603: -D_ARCH_PPC} \
%{mcpu=604: -D_ARCH_PPC} \
%{mcpu=mpc604: -D_ARCH_PPC} \
%{mcpu=ppc604: -D_ARCH_PPC}"
