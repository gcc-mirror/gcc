/* Definitions for Intel 386 running Linux
   Copyright (C) 1992, 1994 Free Software Foundation, Inc.
   Contributed by H.J. Lu (hjl@nynexst.com)

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#if 0	/* The FSF has fixed the known bugs. But ....... */

/* Linux has a hacked gas 1.38.1, which can handle repz, repnz
 and fildll.  */

#define GOOD_GAS
#endif

/* Don't assume anything about the header files. */
#define NO_IMPLICIT_EXTERN_C

/* This is tested by i386/gas.h.  */
#define YES_UNDERSCORES

#ifndef LINUX_ELF
#include "i386/gstabs.h"
#endif

/* Specify predefined symbols in preprocessor.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -Di386 -Dlinux -Asystem(unix) -Asystem(posix) -Acpu(i386) -Amachine(i386)"

#undef CPP_SPEC
#if TARGET_CPU_DEFAULT == 2
#define CPP_SPEC "%{!m386:-D__i486__} %{posix:-D_POSIX_SOURCE}"
#else
#define CPP_SPEC "%{m486:-D__i486__} %{posix:-D_POSIX_SOURCE}"
#endif

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD

#undef HAVE_ATEXIT
#define HAVE_ATEXIT

/* Linux uses ctype from glibc.a. I am not sure how complete it is.
   For now, we play safe. It may change later. */

#if 0
#undef MULTIBYTE_CHARS
#define MULTIBYTE_CHARS	1
#endif

#undef LIB_SPEC
#define LIB_SPEC \
"%{mieee-fp:-lieee} %{p:-lgmon -lc_p} %{pg:-lgmon -lc_p} \
 %{!p:%{!pg:%{!g*:-lc} %{g*:-lg}}}" 

#undef STARTFILE_SPEC
#define STARTFILE_SPEC  "%{pg:gcrt0.o%s} %{!pg:%{p:gcrt0.o%s} %{!p:crt0.o%s}} %{static:-static}"


/* There are conflicting reports about whether this system uses
   a different assembler syntax.  wilson@cygnus.com says # is right.  */
#undef COMMENT_BEGIN
#define COMMENT_BEGIN "#"

#undef ASM_APP_ON
#define ASM_APP_ON "#APP\n"

#undef ASM_APP_OFF
#define ASM_APP_OFF "#NO_APP\n"

/* Don't default to pcc-struct-return, because gcc is the only compiler, and
   we want to retain compatibility with older gcc versions.  */
#ifndef LINUX_ELF
#define DEFAULT_PCC_STRUCT_RETURN 0
#endif

/* We need that too. */
#define HANDLE_SYSV_PRAGMA

#undef LINK_SPEC

/* We want to pass -v to linker */
#if TARGET_CPU_DEFAULT == 2
#define LINK_SPEC	"%{v:-dll-verbose} %{!m386:-m486}"
#else
#define LINK_SPEC	"%{v:-dll-verbose} %{m486:-m486}"
#endif

/* Get perform_* macros to build libgcc.a.  */
#include "i386/perform.h"
