/* Definitions for SPARC running LynxOS.
   Copyright (C) 1993, 1995, 1996, 2000 Free Software Foundation, Inc.

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

#undef ASM_OUTPUT_IDENT
#undef SELECT_RTX_SECTION

#define BSS_SECTION_ASM_OP	"\t.section\t\".bss\""

/* ??? Must redefine to get sparclite and v8 defines.  Can this be done
   differently?  */

#undef CPP_SPEC
#define CPP_SPEC "%{mthreads:-D_MULTITHREADED}  \
  %{mposix:-D_POSIX_SOURCE}  \
  %{msystem-v:-I/usr/include_v}  \
  %(cpp_cpu)"

/* Names to predefine in the preprocessor for this target machine.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -Dsparc -DSPARC -DLynx -DLYNX -DIBITS32 -Asystem=unix -Asystem=lynx -Acpu=sparc -Amachine=sparc"

#undef LINK_SPEC

/* Sparc version of libc.a has references to libm.a (printf calls pow for
   instance), so we must always link both.  */

#undef LIB_SPEC
#define LIB_SPEC "%{mthreads:-L/lib/thread/}  \
  %{msystem-v:-lc_v -lm_v -lc_v}  \
  %{!msystem-v:%{mposix:-lc_p} -lc -lm -lc}"
