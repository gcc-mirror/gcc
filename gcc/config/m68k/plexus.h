/* Definitions of target machine for GNU compiler.
   Copyright (C) 1990, 1994 Free Software Foundation, Inc.

   Written by Randy Welch
   Send bug reports, questions and improvements to any of the following
   addresses:

   randy@kcin.uucp
   randy@tss.com
   rwelch@netcom.com
   plx-info@wpg.com        Plexus users mailing list

   For 680X0 based Plexus Computers running SYSVR2

   The Plexus port of gcc requires you to use gas ( either 1.3X with COFF 
   patches or 2.X ),  If you use gas 2.X you have to use binutils-2.X.
      
   With using gas-2.X the Plexus gcc port is now capable of generating
   output suitable for use by gdb-4.X ( send mail to above address for
   info on getting gdb patches or other GNU items for the Plexus )

   This is configured for label output default by gas as LXXX instead of
   plexus cc/as combination requires .LXXX

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#include "m68k/m68k.h"

/* Define __HAVE_68881 in preprocessor only if -m68881 is specified.
   This will control the use of inline 68881 insns in certain macros. */

#define TARGET_DEFAULT 5                      /* set to 5 if on a '020 box */

/* Don't try using XFmode.  */
#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 64

#define CPP_SPEC "%{m68881:-D__HAVE_68881__}"
#define CPP_PREDEFINES "-Dm68 -Dunix -Dplexus -Asystem(unix)  -Acpu(m68k) -Amachine(m68k)"

#if TARGET_DEFAULT & 01
#define ASM_SPEC\
"%{m68000:-mc68000}%{mc68000:-mc68000}%{!mc68000:%{!m68000:-mc68020}}"
#undef STRICT_ALIGNMENT
#define STRICT_ALIGNMENT 0
#else
#define ASM_SPEC\
"%{m68020:-mc68020}%{mc68020:-mc68020}%{!mc68020:%{!mc68020:-mc68000}}"
#endif

/***************************************************************************/
/*  Un comment the following if you want adb to be able to follow a core   */
/*  file if you compile a program with -O                                  */
/***************************************************************************/
/* #define FRAME_POINTER_REQUIRED */

/* Let's be compatible with the Plexus C compiler by default.  Why not?  */
#define PLEXUS_CC_COMPAT

#ifdef PLEXUS_CC_COMPAT
#define STRUCTURE_SIZE_BOUNDARY 16	/* for compatibility with cc */
#undef STACK_BOUNDARY
#define STACK_BOUNDARY 32		/* ditto */
#endif

#undef NEED_PROBE
#define NEED_PROBE -132			/* plexus needs a stack probe */

/***********************************************************************/
/* if you have binutils-2.X and gas-2.X running you can generate code  */
/* that gdb can understand ( gdb support available for 4.11 )          */
/*                                                                     */
/* If you use gas-1.3X don't define this as the version of the coff    */
/* patches for gas-1.3x ( stabs in coff ) does not generate coff debug */
/* syms                                                                */
/***********************************************************************/
#define HAVE_GAS_2_X

#ifdef HAVE_GAS_2_X
#undef DBX_DEBUGGING_INFO
#define SDB_DEBUGGING_INFO

#undef ASM_FILE_START
#define ASM_FILE_START(FILE) \
  output_file_directive((FILE), main_input_filename)

#else
#undef DBX_DEBUGGING_INFO		/* no real debugger */
#undef SDB_DEBUGGING_INFO
#endif
#define TARGET_MEM_FUNCTIONS

/***********************************************************************/
/*                          items for collect2                         */
/***********************************************************************/

#define NM_FLAGS ""
#define NO_SYS_SIGLIST
#define NO_DUP2

#define SIZE_TYPE "int"
