/* Definitions of target machine for GNU compiler, for SPARClite w/o FPU.
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.
   Contributed by Stan Cox (scox@cygnus.com).

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

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-D__sparc__ -D__sparclite__ -Acpu=sparc -Amachine=sparc"

/* Default to dwarf2 in ELF.  */

#undef DWARF_DEBUGGING_INFO
#define DWARF_DEBUGGING_INFO
#undef DWARF2_DEBUGGING_INFO
#define DWARF2_DEBUGGING_INFO

#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (sparclite)");

/* Enable app-regs and epilogue options.  Do not enable the fpu.  */

#undef TARGET_DEFAULT
#define TARGET_DEFAULT MASK_APP_REGS

/* US Software GOFAST library support.  */
#undef INIT_SUBTARGET_OPTABS
#define INIT_SUBTARGET_OPTABS INIT_GOFAST_OPTABS      

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "crti.o%s crtbegin.o%s"

/* Use __main method of constructor invocation.  */
#undef INIT_SECTION_ASM_OP
#undef FINI_SECTION_ASM_OP
