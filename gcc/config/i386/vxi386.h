/* Definitions of target machine for GNU compiler.  VxWorks i386 version.
   Copyright (C) 1998, 2002 Free Software Foundation, Inc.

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

#undef  TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (80386, VxWorks BSD syntax)"); 

#define TARGET_OS_CPP_BUILTINS()			\
  do							\
    {							\
      builtin_define ("__vxworks");			\
      builtin_assert ("system=unix");			\
							\
      if (TARGET_386)					\
	builtin_define ("CPU=I80386");			\
      else if (TARGET_486)				\
	builtin_define ("CPU=I80486");			\
      else if (TARGET_PENTIUM)				\
	{						\
	  builtin_define ("CPU=PENTIUM");		\
	  builtin_define ("CPU_VARIANT=PENTIUM");	\
	}						\
      else if (TARGET_PENTIUMPRO)			\
	{						\
	  builtin_define ("CPU=PENTIUM");		\
	  builtin_define ("CPU_VARIANT=PENTIUMPRO");	\
	}						\
    }							\
  while (0)

#define HANDLE_SYSV_PRAGMA 1

/* VxWorks does all the library stuff itself.  */

#undef LIB_SPEC
#define LIB_SPEC ""

/* VxWorks uses object files, not loadable images.  make linker just
   combine objects.  */

#undef LINK_SPEC
#define LINK_SPEC "-r"

/* VxWorks provides the functionality of crt0.o and friends itself.  */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC ""

#undef ENDFILE_SPEC
#define ENDFILE_SPEC ""
