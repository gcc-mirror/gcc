/* nextstep.h -- operating system specific defines to be used when
   targeting GCC for NeXTSTEP.
   Copyright (C) 1989, 1990, 1991, 1992, 1993 Free Software Foundation, Inc.

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


/* changed for NeXTStep 2.1, Ch. Kranz, 2/94, 3/94 */
#include "nextstep.h"

/* set flag_gnu_linker=0, use collect2 for linking */
#undef USE_COLLECT2
#define USE_COLLECT2

/* use this until a newer gdb for NeXTStep21 is available */
#define DEFAULT_GDB_EXTENSIONS 0

/* we need the call to __main to start all global destructors and constructors
   correctly, so undef INIT_SECTION_ASM_OP, (see libgcc2.c line 1965) 
   and define INVOKE_main */
#undef	INIT_SECTION_ASM_OP
#define INVOKE__main

/* We call the global destructors, constructors from __main */
#undef TARGET_ASM_CONSTRUCTOR
#undef TARGET_ASM_DESTRUCTOR

#undef ASM_FILE_END
#define ASM_FILE_END(FILE)					\
  do {								\
      if (strcmp (lang_hooks.name, "GNU C++") == 0)		\
      {								\
	ASM_OUTPUT_ALIGN (FILE, 1);				\
      }								\
  } while (0) 
/* deleted: destructor_section ();				\ */
/* deleted: constructor_section ();				\ */

/* Ensure correct alignment of bss data.  */
/* ASM_OUTPUT_ALIGNED_LOCAL not needed */
/* need ASM_OUTPUT_LOCAL instead for old NeXT-as */
/* look in varasm.c, line 1062 and 1476 */
#undef	ASM_OUTPUT_ALIGNED_LOCAL
#undef ASM_OUTPUT_LOCAL
#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".lcomm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (ROUNDED)))

