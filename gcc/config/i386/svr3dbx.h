/* Definitions for Intel 386 running system V, using dbx-in-coff encapsulation.
   Copyright (C) 1992 Free Software Foundation, Inc.

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
   You have specify -DOTHER_ALIGN wenn building gas-1.38.1. */

#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
     if ((LOG)!=0) fprintf ((FILE), "\t.align %d\n", 1<<(LOG))

/* Align labels, etc. at 4-byte boundaries.
   For the 486, align to 16-byte boundary for sake of cache.  */

#undef ASM_OUTPUT_ALIGN_CODE
#define ASM_OUTPUT_ALIGN_CODE(FILE)			\
     fprintf ((FILE), "\t.align %d,0x90\n",		\
	      TARGET_486 ? 16 : 4);  /* Use log of 16 or log of 4 as arg.  */

/* Align start of loop at 4-byte boundary.  */

#undef ASM_OUTPUT_LOOP_ALIGN
#define ASM_OUTPUT_LOOP_ALIGN(FILE) \
     fprintf ((FILE), "\t.align 4,0x90\n");


/* Additional overrides needed for dbx-in-coff gas, mostly taken from pbb.h */

/* Although the gas we use can create .ctor and .dtor sections from N_SETT
   stabs, it does not support section directives, so we need to have the loader
   define the lists.
 */
#define CTOR_LISTS_DEFINED_EXTERNALLY

/* similar to default, but allows for the table defined by ld with svr3.ifile. 
   nptrs is always 0.  So we need to instead check that __DTOR_LIST__[1] != 0.
   The old check is left in so that the same macro can be used if and when  
   a future version of gas does support section directives. */

#define DO_GLOBAL_DTORS_BODY {int nptrs = *(int *)__DTOR_LIST__; int i; \
  if (nptrs == -1 || (__DTOR_LIST__[0] == 0 && __DTOR_LIST__[1] != 0))  \
    for (nptrs = 0; __DTOR_LIST__[nptrs + 1] != 0; nptrs++); 		\
  for (i = nptrs; i >= 1; i--)						\
    __DTOR_LIST__[i] (); }

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
