/* Common VxWorks target definitions for GCC.
   Copyright (C) 1999, 2000, 2001, 2002, 2004 Free Software Foundation, Inc.
   Contributed by Wind River Systems.

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

/* Specify what to link with.  */
/* VxWorks does all the library stuff itself.  */
#undef	LIB_SPEC
#define	LIB_SPEC ""

#undef LINK_SPEC
#define LINK_SPEC "-r"

/* VxWorks provides the functionality of crt0.o and friends itself.  */
#undef  STARTFILE_SPEC
#define	STARTFILE_SPEC ""

#undef ENDFILE_SPEC
#define ENDFILE_SPEC ""

/* VxWorks cannot have dots in constructor labels, because it uses a
   mutant variation of collect2 that generates C code instead of
   assembly.  Thus each constructor label must be a legitimate C
   symbol.  FIXME: Have VxWorks use real collect2 instead.  */

#undef NO_DOLLAR_IN_LABEL
#define NO_DOT_IN_LABEL

/* We want #pragma pack(n) enabled and expect to inherit the proper
   definition of HANDLE_SYSV_PRAGMA from elfos.h for that purpose.  */

/* No underscore is prepended to any C symbol name.  */
#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""

/* VxWorks uses wchar_t == unsigned short (UCS2) on all architectures.  */
#undef WCHAR_TYPE
#define WCHAR_TYPE "short unsigned int"
#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 16

/* Dwarf2 unwind info is not supported.  */
#define DWARF2_UNWIND_INFO 0
/* Weak symbols and link-once sections are not enabled by default.  */
#define DEFAULT_USE_WEAK 0

/* Only supported debug format is Dwarf2.  */
#undef DBX_DEBUGGING_INFO
