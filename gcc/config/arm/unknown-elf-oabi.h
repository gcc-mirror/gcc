/* Definitions for non-Linux based ARM systems using ELF old abi
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.
   Contributed by Catherine Moore <clm@cygnus.com>

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
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Run-time Target Specification.  */
#ifndef TARGET_VERSION
#define TARGET_VERSION	fputs (" (ARM/ELF non-Linux old abi)", stderr);
#endif

#define CPP_PREDEFINES "-Darm_oabi -Darm -Darm_elf -Acpu(arm) -Amachine(arm) -D__ELF__"

#ifndef ASM_SPEC
#define ASM_SPEC "-moabi %{mbig-endian:-EB} %{mcpu=*:-m%*} %{march=*:-m%*} \
 %{mapcs-*:-mapcs-%*} %{mthumb-interwork:-mthumb-interwork}"
#endif

/* Now get the routine arm-elf definitions.  */
#include "arm/unknown-elf.h"
#include "arm/elf.h"
