/* Definitions of target machine for GNU compiler.
   Copyright (C) 1990 Free Software Foundation, Inc.

   Written by Randy Welch
   Send bug reports, questions and improvements to any of the following
   addresses:

   randy@kcin.alphacdc.com
   rwelch@isis.cs.du.eu
   rwelch@csn.org

   For Plexus P/60 and assumably P/35 P/75 P/95's running System V.2

   This file outputs assembler source for gas-1.38.1 with the COFF patches
   The patches for gas-1.38.1 to support COFF is on ftp.cs.umb.edu in pub/gnu 
   No debugging is supported, due to the fact that the only debugger Plexus
   had was adb *sigh*.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include "m68k.h"

/* Define __HAVE_68881 in preprocessor only if -m68881 is specified.
   This will control the use of inline 68881 insns in certain macros. */

#define TARGET_DEFAULT 5                      /* set to 5 if on a '020 box */

#define CPP_SPEC "%{m68881:-D__HAVE_68881__}"
#define CPP_PREDEFINES "-Dm68 -Dunix -Dplexus"

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
#define STRUCTURE_SIZE_BOUNDARY 16	/* for compatiblity with cc */
#undef STACK_BOUNDARY
#define STACK_BOUNDARY 32		/* ditto */
#endif

#undef NEED_PROBE
#define NEED_PROBE -132			/* plexus needs a stack probe */

#undef DBX_DEBUGGING_INFO		/* no real debugger */
#undef SDB_DEBUGGING_INFO

#define TARGET_MEM_FUNCTIONS

/***********************************************************************/
/*                          items for collect2                         */
/***********************************************************************/

#define NM_FLAGS ""
#define NO_SYS_SIGLIST
#define NO_DUP2

#define SIZE_TYPE "int"
