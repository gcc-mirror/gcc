/* Definitions for Intel 386 running system V, using dbx-in-coff encapsulation.
   Copyright (C) 1992, 1995 Free Software Foundation, Inc.

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

#include "i386/svr3gas.h"

/* We do not want to output SDB debugging information.  */

#undef SDB_DEBUGGING_INFO

/* We want to output DBX debugging information.  */

#define DBX_DEBUGGING_INFO

/* Compensate for botch in dbxout_init/dbxout_source_file which
   unconditionally drops the first character from ltext_label_name */

#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(BUF,PREFIX,NUMBER)	\
    sprintf ((BUF), "*.%s%d", (PREFIX), (NUMBER))

/* With the current gas, .align N aligns to an N-byte boundary.
   This is done to be compatible with the system assembler.
   You must specify -DOTHER_ALIGN when building gas-1.38.1.  */

#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
     if ((LOG)!=0) fprintf ((FILE), "\t.align %d\n", 1<<(LOG))

/* Align labels, etc. at 4-byte boundaries.
   For the 486, align to 16-byte boundary for sake of cache.  */

#undef LABEL_ALIGN_AFTER_BARRIER
#define LABEL_ALIGN_AFTER_BARRIER(LABEL) (i386_align_jumps)

/* Align start of loop at 4-byte boundary.  */

#undef LOOP_ALIGN
#define LOOP_ALIGN(LABEL) (i386_align_loops)


/* Additional overrides needed for dbx-in-coff gas, mostly taken from pbb.h */

/* Although the gas we use can create .ctor and .dtor sections from N_SETT
   stabs, it does not support section directives, so we need to have the loader
   define the lists.
 */
#define CTOR_LISTS_DEFINED_EXTERNALLY

/* Use crt1.o as a startup file and crtn.o as a closing file.  */
/*
 * The loader directive file svr3.ifile defines how to merge the constructor 
 * sections into the data section.  Also, since gas only puts out those 
 * sections in response to N_SETT stabs, and does not (yet) have a 
 * ".sections" directive, svr3.ifile also defines the list symbols 
 * __DTOR_LIST__ and __CTOR_LIST__.
 */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!r:%{!z:svr3.ifile%s}%{z:svr3z.ifile%s}}\
   %{pg:gcrt1.o%s}%{!pg:%{posix:%{p:mcrtp1.o%s}%{!p:crtp1.o%s}}%{!posix:%{p:mcrt1.o%s}%{!p:crt1.o%s}}} \
   %{p:-L/usr/lib/libp}%{pg:-L/usr/lib/libp}"
  
#define ENDFILE_SPEC "crtn.o%s"
  
#undef LIB_SPEC
#define LIB_SPEC "%{posix:-lcposix} %{shlib:-lc_s} -lc -lg"
