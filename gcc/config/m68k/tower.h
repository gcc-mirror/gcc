/* Definitions of target machine for GNU compiler.
   Copyright (C) 1990, 1994, 1996 Free Software Foundation, Inc.
   Contributed by Robert Andersson, International Systems, Oslo, ra@intsys.no.

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

/* For NCR Tower 32/4x0 and 32/6x0 running System V Release 3.  I don't have
   access to 200/700/800/850 machines, so I don't know if it works on those
   as well.  It shouldn't be far from it however.  The hardware floating point
   support is completely untested, as I do not have access to a machine with
   a 6888x FPU in it.  It does not work on the System V Release 2 based OS
   releases.  Making it work will not be easy, due to the silly way in which
   stack expansion is implemented in the OS.

   This file is included in tower-as.h.
   Do *NOT* include this file directly.  */


#include "m68k/m68k.h"

#define TARGET_DEFAULT (MASK_BITFIELD|MASK_68020)

/* Don't try using XFmode.  */
#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 64

/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Dunix -Dtower32 -Dtower32_200 -Asystem(unix) -Asystem(svr3) -Acpu(m68k) -Amachine(m68k)"

#if 0  /* It is incorrect to test these symbols.
	  They describe the host, not the target.
	  It should not matter which model is specified.  */
#ifdef tower32_600
#define CPP_PREDEFINES "-Dunix -Dtower32 -Dtower32_600 -Asystem(unix) -Asystem(svr3) -Acpu(m68k) -Amachine(m68k)"
#endif
#ifdef tower32_700
#define CPP_PREDEFINES "-Dunix -Dtower32 -Dtower32_700 -Asystem(unix) -Asystem(svr3) -Acpu(m68k) -Amachine(m68k)"
#endif
#ifdef tower32_800
#define CPP_PREDEFINES "-Dunix -Dtower32 -Dtower32_800 -Asystem(unix) -Asystem(svr3) -Acpu(m68k) -Amachine(m68k)"
#endif
#ifdef tower32_850
#define CPP_PREDEFINES "-Dunix -Dtower32 -Dtower32_850 -Asystem(unix) -Asystem(svr3) -Acpu(m68k) -Amachine(m68k)"
#endif
#endif

/* The startfiles and libraries depend on the -p and -m68881 options.
   The Tower does not support the -pg option.  */

#define LINK_SPEC                                              \
"%{p:%{m68881:-L/usr/lib/fp/libp} -L/usr/lib/libp}             \
 %{m68881:-L/usr/lib/fp}"

#define LIB_SPEC \
"%{shlib:-lc_s} -lc crtend.o%s crtn.o%s"

#define STARTFILE_SPEC \
"%{p:mcrt1.o%s} %{!p:crt1.o%s} crtbegin.o%s"

/* Use mem* functions, recognize #ident lines.  */

#define TARGET_MEM_FUNCTIONS
#define IDENT_DIRECTIVE

/* Every structure and union's size must be a multiple of two bytes.  */

#define STRUCTURE_SIZE_BOUNDARY 16

/* All register names should have a leading % character.  */

#undef REGISTER_NAMES
#define REGISTER_NAMES \
{"%d0", "%d1", "%d2", "%d3", "%d4", "%d5", "%d6", "%d7",                      \
 "%a0", "%a1", "%a2", "%a3", "%a4", "%a5", "%a6", "%sp",                      \
 "%fp0", "%fp1", "%fp2", "%fp3", "%fp4", "%fp5", "%fp6", "%fp7"};

#undef REGISTER_PREFIX
#define REGISTER_PREFIX "%"

#undef IMMEDIATE_PREFIX
#define IMMEDIATE_PREFIX "&"

/* The prefix to add to user-visible assembler symbols. */

/* We do not want leading underscores.  */

#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""
