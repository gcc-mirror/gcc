/* Definitions for Intel 386 running QNX/Neutrino.
   Copyright (C) 2002, 2003, 2007, 2010 Free Software Foundation, Inc.

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

#undef  DEFAULT_PCC_STRUCT_RETURN
#define DEFAULT_PCC_STRUCT_RETURN 1

#undef TARGET_VERSION
#define TARGET_VERSION	fprintf (stderr, " (QNX/Neutrino/i386 ELF)");

#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
        builtin_define ("__X86__");		\
        builtin_define ("__QNXNTO__");		\
        builtin_define ("__QNX__");		\
        builtin_define ("__ELF__");		\
        builtin_define ("__LITTLEENDIAN__");	\
        builtin_assert ("system=qnx");		\
        builtin_assert ("system=qnxnto");	\
        builtin_assert ("system=nto");		\
        builtin_assert ("system=unix");		\
    }						\
  while (0)

#undef THREAD_MODEL_SPEC
#define THREAD_MODEL_SPEC "posix"

#ifdef CROSS_DIRECTORY_STRUCTURE
#define SYSROOT_SUFFIX_SPEC "x86"
#endif

#ifndef CROSS_DIRECTORY_STRUCTURE
#undef MD_EXEC_PREFIX
#define MD_EXEC_PREFIX "/usr/ccs/bin/"

#undef MD_STARTFILE_PREFIX
#define MD_STARTFILE_PREFIX "/usr/ccs/lib/"
#endif

#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
"%{!shared: \
  %{!symbolic: \
    %{pg:mcrt1.o%s} \
    %{!pg:%{p:mcrt1.o%s} \
    %{!p:crt1.o%s}}}} \
crti.o%s \
%{fexceptions: crtbegin.o%s} \
%{!fexceptions: %R/lib/crtbegin.o}"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC \
  "crtend.o%s crtn.o%s"

#undef LINK_SPEC
#define LINK_SPEC \
  "%{h*} %{v:-V} \
   %{static:-dn -Bstatic} \
   %{shared:-G -dy -z text} \
   %{symbolic:-Bsymbolic -G -dy -z text} \
   %{G:-G} \
   %{YP,*} \
   %{!YP,*:%{p:-Y P,%R/lib} \
    %{!p:-Y P,%R/lib}} \
   %{Qy:} %{!Qn:-Qy} \
   -m i386nto \
   %{!shared: --dynamic-linker /usr/lib/ldqnx.so.2}"

#undef	LIB_SPEC
#define LIB_SPEC "%{!shared:%{!symbolic:-lc}}"

#undef  ASM_SPEC
#define ASM_SPEC ""

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "long unsigned int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD

#define NO_IMPLICIT_EXTERN_C 1

#define TARGET_POSIX_IO

#undef DBX_REGISTER_NUMBER
