/* Definitions of target machine for GNU compiler.
   Convergent Technologies MiniFrame version,
   using GAS and binutils with COFF encapsulation.

   Written by Ronald Cole

   Because the MiniFrame's C compiler is so completely lobotomized,
   bootstrapping this is damn near impossible!
   Write to me for information on obtaining the binaries...

   bug reports to csusac!unify!rjc@ucdavis.edu

   Copyright (C) 1990 Free Software Foundation, Inc.

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

#include "3b1g.h"

/* Names to predefine in the preprocessor for this target machine.  */
#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dmc68000 -Dmc68k -Dunix -Dctix"

/* Where to look for robotussinized startfiles.  */
#undef STANDARD_STARTFILE_PREFIX
#define STANDARD_STARTFILE_PREFIX "/usr/local/lib/gnu/"

/* Generate calls to the MiniFrame's library (for speed).  */
#define DIVSI3_LIBCALL "ldiv"
#define UDIVSI3_LIBCALL "uldiv"
#define MODSI3_LIBCALL "lrem"
#define UMODSI3_LIBCALL "ulrem"
#define MULSI3_LIBCALL "lmul"
#define UMULSI3_LIBCALL "ulmul"
