/* Definitions for Linux for S/390.
   Copyright (C) 1999-2013 Free Software Foundation, Inc.
   Contributed by Hartmut Penner (hpenner@de.ibm.com) and
                  Ulrich Weigand (uweigand@de.ibm.com).

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef _LINUX_H
#define _LINUX_H

/* Target specific type definitions.  */

/* ??? Do we really want long as size_t on 31-bit?  */
#undef  SIZE_TYPE
#define SIZE_TYPE (TARGET_64BIT ? "long unsigned int" : "long unsigned int")
#undef  PTRDIFF_TYPE
#define PTRDIFF_TYPE (TARGET_64BIT ? "long int" : "int")

#undef  WCHAR_TYPE
#define WCHAR_TYPE "int"
#undef  WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32


/* Target specific preprocessor settings.  */

#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
      GNU_USER_TARGET_OS_CPP_BUILTINS();	\
    }						\
  while (0)


/* Target specific assembler settings.  */

#undef  ASM_SPEC
#define ASM_SPEC "%{m31&m64}%{mesa&mzarch}%{march=*}"


/* Target specific linker settings.  */

#ifdef DEFAULT_TARGET_64BIT
#define MULTILIB_DEFAULTS { "m64" }
#else
#define MULTILIB_DEFAULTS { "m31" }
#endif

#define GLIBC_DYNAMIC_LINKER32 "/lib/ld.so.1"
#define GLIBC_DYNAMIC_LINKER64 "/lib/ld64.so.1"

#undef  LINK_SPEC
#define LINK_SPEC \
  "%{m31:-m elf_s390}%{m64:-m elf64_s390} \
   %{shared:-shared} \
   %{!shared: \
      %{static:-static} \
      %{!static: \
	%{rdynamic:-export-dynamic} \
	%{m31:-dynamic-linker " GNU_USER_DYNAMIC_LINKER32 "} \
	%{m64:-dynamic-linker " GNU_USER_DYNAMIC_LINKER64 "}}}"

#define CPP_SPEC "%{posix:-D_POSIX_SOURCE} %{pthread:-D_REENTRANT}"

#define TARGET_ASM_FILE_END file_end_indicate_exec_stack

#ifdef TARGET_LIBC_PROVIDES_SSP
/* s390 glibc provides __stack_chk_guard in 0x14(tp),
   s390x glibc provides it at 0x28(tp).  */
#define TARGET_THREAD_SSP_OFFSET        (TARGET_64BIT ? 0x28 : 0x14)
#endif

/* Define if long doubles should be mangled as 'g'.  */
#define TARGET_ALTERNATE_LONG_DOUBLE_MANGLING

#endif
