/* Definitions of target machine for GNU compiler.
   Citicorp/TTI Unicom PBB version (using GAS with a %-register prefix)
   Copyright (C) 1987, 1988, 1990, 1996, 1997 Free Software Foundation, Inc.

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

/* Note:   This config uses a version of gas with a postprocessing stage that
   converts the output of gas to coff containing stab debug symbols.
   (See vasta@apollo.com or mb@soldev.tti.com) */

#include "m68k/m68k.h"

/* See m68k.h.  5 means 68020 without 68881.  */

#define TARGET_DEFAULT (MASK_BITFIELD|MASK_68020)

/* Don't try using XFmode.  */
#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 64

/* Every structure or union's size must be a multiple of 2 bytes.  */
#define STRUCTURE_SIZE_BOUNDARY 16

/* Define __HAVE_68881__ in preprocessor if -m68881 is specified.
   This will control the use of inline 68881 insns in certain macros.  */

#define CPP_SPEC "%{m68881:-D__HAVE_68881__}"

/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Dm68k -Dunix -DUnicomPBB -Dmc68k -Dmc68020 -Dmc68k32 -Asystem(unix)  -Acpu(m68k) -Amachine(m68k)"

/* We want DBX format for use with gdb under COFF.  */

#define DBX_DEBUGGING_INFO

/* Generate calls to memcpy, memcmp and memset.  */

#define TARGET_MEM_FUNCTIONS

/* -m68000 requires special flags to the assembler. */

#define ASM_SPEC \
 " %{m68000:-mc68010}%{mc68000:-mc68010}"

/* we use /lib/libp/lib*  when profiling */

#define LIB_SPEC "%{p:-L/usr/lib/libp} %{pg:-L/usr/lib/libp} -lc"


/* Use crt1.o as a startup file and crtn.o as a closing file.  */
/*
 * The loader directive file gcc.ifile defines how to merge the constructor 
 * sections into the data section.  Also, since gas only puts out those 
 * sections in response to N_SETT stabs, and does not (yet) have a 
 * ".sections" directive, gcc.ifile also defines the list symbols 
 * __DTOR_LIST__ and __CTOR_LIST__.
 * 
 * Finally, we must explicitly specify the file from libgcc.a that defines
 * exit(), otherwise if the user specifies (for example) "-lc_s" on the 
 * command line, the wrong exit() will be used and global destructors will 
 * not get called .
 */

#define STARTFILE_SPEC \
"%{!r: gcc.ifile%s} %{pg:gcrt1.o%s}%{!pg:%{p:mcrt1.o%s}%{!p:crt1.o%s}} \
%{!r:_exit.o%s}"

#define ENDFILE_SPEC "crtn.o%s"

/* cpp has to support a #sccs directive for the /usr/include files */

#define SCCS_DIRECTIVE

/* GAS register prefix assembly syntax: */

/* User labels have no prefix */
#undef  USER_LABEL_PREFIX 
#define USER_LABEL_PREFIX ""

/* local labels are prefixed with ".L" */
#undef  LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX "."

/* registers are prefixed with "%" */
#undef  REGISTER_PREFIX
#define REGISTER_PREFIX "%"

#undef REGISTER_NAMES
#define REGISTER_NAMES \
{"%d0", "%d1", "%d2", "%d3", "%d4", "%d5", "%d6", "%d7",	\
 "%a0", "%a1", "%a2", "%a3", "%a4", "%a5", "%a6", "%sp",	\
 "%fp0", "%fp1", "%fp2", "%fp3", "%fp4", "%fp5", "%fp6", "%fp7"}

#undef FUNCTION_EXTRA_EPILOGUE
#define FUNCTION_EXTRA_EPILOGUE(FILE, SIZE)				\
  { extern int current_function_returns_pointer;			\
    if ((current_function_returns_pointer) &&				\
      ! find_equiv_reg (0, get_last_insn (), 0, 0, 0, 8, Pmode))        \
      asm_fprintf (FILE, "\tmovl %Rd0,%Ra0\n"); } 

#define ASM_RETURN_CASE_JUMP \
  do {						\
    if (TARGET_5200)				\
      return "ext%.l %0\n\tjmp %%pc@(2,%0:l)";	\
    else					\
      return "jmp %%pc@(2,%0:w)";		\
  } while (0)

/* Although the gas we use can create .ctor and .dtor sections from N_SETT
   stabs, it does not support section directives, so we need to have the loader
   define the lists.
 */
#define CTOR_LISTS_DEFINED_EXTERNALLY

/* similar to default, but allows for the table defined by ld with gcc.ifile. 
   nptrs is always 0.  So we need to instead check that __DTOR_LIST__[1] != 0.
   The old check is left in so that the same macro can be used if and when  
   a future version of gas does support section directives. */

#define DO_GLOBAL_DTORS_BODY {int nptrs = *(int *)__DTOR_LIST__; int i; \
  if (nptrs == -1 || (__DTOR_LIST__[0] == 0 && __DTOR_LIST__[1] != 0))  \
    for (nptrs = 0; __DTOR_LIST__[nptrs + 1] != 0; nptrs++); 		\
  for (i = nptrs; i >= 1; i--)						\
    __DTOR_LIST__[i] (); }

/* 
 * Here is an example gcc.ifile.  I've tested it on PBB 68k and on sco 386
 * systems. The NEXT(0x200000) works on just about all 386 and m68k systems, 
 * but can be reduced to any power of 2 that is >= NBPS (0x10000 on a pbb).

   SECTIONS {
       .text BIND(0x200200) BLOCK (0x200) : 
		{ *(.init) *(.text) vfork = fork; *(.fini) }

      	GROUP BIND( NEXT(0x200000) + ADDR(.text) + SIZEOF(.text)):
	{      .data : { __CTOR_LIST__ = . ; . += 4; *(.ctor) . += 4 ;
		       	 __DTOR_LIST__ = . ; . += 4; *(.dtor) . += 4 ; }
	       .bss : { }
       }
  }
 */

/*
Local variables:
version-control: t
End:
*/
