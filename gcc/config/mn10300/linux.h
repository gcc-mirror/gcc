/* Definitions of taret machine for GNU compiler.
   Matsushita AM33/2.0
   Copyright (C) 2001-2025 Free Software Foundation, Inc.
   Contributed by Alexandre Oliva <aoliva@redhat.com>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#undef  PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

#define TARGET_OS_CPP_BUILTINS() GNU_USER_TARGET_OS_CPP_BUILTINS()

#undef  CPP_SPEC
#define CPP_SPEC "%{mam33:-D__AM33__} %{!mam33:-D__AM33__=2 -D__AM33_2__} \
  %{posix:-D_POSIX_SOURCE} \
  %{pthread:-D_REENTRANT -D_PTHREADS}"

#undef  ASM_SPEC
#define ASM_SPEC ""

#define GLIBC_DYNAMIC_LINKER "/lib/ld.so.1"

#undef  LINK_SPEC
#define LINK_SPEC "%{mrelax:--relax} %{shared:-shared} \
   %{!static: \
     %{rdynamic:-export-dynamic} \
     -dynamic-linker " GNU_USER_DYNAMIC_LINKER "} \
   %{static:-static}"

#undef  PROCESSOR_DEFAULT
#define PROCESSOR_DEFAULT PROCESSOR_AM33_2

extern int mn10300_protect_label;

#undef  PRINT_OPERAND
#define PRINT_OPERAND(FILE, X, CODE)		\
  do						\
    {						\
      mn10300_protect_label = 1;		\
      mn10300_print_operand ((FILE), (X), (CODE));	\
      mn10300_protect_label = 0;		\
    }						\
  while (0)

#undef  PRINT_OPERAND_ADDRESS
#define PRINT_OPERAND_ADDRESS(FILE, X)		\
  do						\
    {						\
      mn10300_protect_label = 1;		\
      mn10300_print_operand_address ((FILE), (X));	\
      mn10300_protect_label = 0;		\
    }						\
   while (0)

#undef  ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(FILE, NAME)		\
  do						\
    {						\
      const char * real_name;			\
						\
      real_name = (*targetm.strip_name_encoding) (NAME);	\
      if (mn10300_protect_label)		\
        asm_fprintf (FILE, "+");		\
      asm_fprintf (FILE, "%U%s", real_name);	\
    }						\
  while (0)

#undef SIZE_TYPE
#undef PTRDIFF_TYPE
#undef WCHAR_TYPE
#undef WCHAR_TYPE_SIZE
