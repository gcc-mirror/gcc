/* Definitions of target machine for GCC, for SPARClite w/o FPU, ELF.
   Copyright (C) 1998, 1999, 2005 Free Software Foundation, Inc.
   Contributed by Stan Cox (scox@cygnus.com).

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (sparclite)");

#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
	builtin_define ("__sparclite__");	\
    }						\
  while (0)

/* Enable app-regs and epilogue options.  Do not enable the fpu.  */

#undef TARGET_DEFAULT
#define TARGET_DEFAULT MASK_APP_REGS

/* Enable US Software GOFAST library support.  */
#define US_SOFTWARE_GOFAST

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "crti.o%s crtbegin.o%s"

/* Use __main method of constructor invocation.  */
#undef INIT_SECTION_ASM_OP
#undef FINI_SECTION_ASM_OP
