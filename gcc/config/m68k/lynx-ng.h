/* Definitions for Motorola 680x0 running LynxOS, using Lynx's old as and ld.
   Copyright (C) 1993, 1995, 1996 Free Software Foundation, Inc.

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

#include <m68k/m68k.h>
#include <m68k/coff.h>
#include <lynx-ng.h>

/* See m68k.h.  7 means 68020 with 68881.  */

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_BITFIELD|MASK_68881|MASK_68020)
#endif

/* Names to predefine in the preprocessor for this target machine.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -Dmc68000 -DM68K -DLynx -DIBITS32 -Asystem=unix -Asystem=lynx -Acpu=m68k -Amachine=m68k"

/* Provide required defaults for linker switches.  */

#undef LINK_SPEC
#define LINK_SPEC "-P1000 %{msystem-v:-V} %{mcoff:-k}"

/* Every structure or union's size must be a multiple of 2 bytes.  */

#define STRUCTURE_SIZE_BOUNDARY 16
