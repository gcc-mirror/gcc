/* Definitions for rtems targeting a Motorola m68k using elf.
   Copyright (C) 1999, 2000, National Research Council of Canada.
   Contributed by Charles-Antoine Gauthier (charles.gauthier@nrc.ca).

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


#define MOTOROLA       /* Use Motorola syntax rather than MIT.  */

#include "m68k/m68020-elf.h"

/* Specify predefined symbols in preprocessor.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dmc68000 -Drtems -D__rtems__ -D__ELF__ \
   -Asystem(rtems) -Acpu(mc68000) -Acpu(m68k) -Amachine(m68k)"

/* Generate calls to memcpy, memcmp and memset.  */
#ifndef TARGET_MEM_FUNCTIONS
#define TARGET_MEM_FUNCTIONS
#endif

/*
 *  Each RTEMS BSP provides its own crt0 and linker script.  Unfortunately
 *  this means that crt0 and the linker script are not available as
 *  each tool is configured.  Without a crt0 and linker script, m68k ELF
 *  targets do not successfully link "conftest.c" during the configuration
 *  process.  RTEMS supplies a crt0.c that provides all the symbols required
 *  to successfully link a program.  The resulting program will not run 
 *  but this is enough to satisfy the autoconf macro AC_PROG_CC.
 *  Override STARTFILE_SPEC to use the fake crt0.o supplied by rtems.
 */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC "crt0.o%s"

/*
 *  Redefine INIT_SECTION_ASM_OP and FINI_SECTION_ASM_OP. This is the easiest
 *  way to process constructors, destructors, and the exception frame
 *  information at startup.
 */
#undef INIT_SECTION_ASM_OP
#define INIT_SECTION_ASM_OP    ".section\t.init"
#undef FINI_SECTION_ASM_OP
#define FINI_SECTION_ASM_OP    ".section\t.fini"

#undef EH_FRAME_SECTION_ASM_OP
#define EH_FRAME_SECTION_ASM_OP        ".section\t.eh_frame"

/* Do I need this? */
#undef INVOKE__main

/* Get machine-independent configuration parameters for RTEMS.  */
#include <rtems.h>

/* end of m68k/rtemself.h */
