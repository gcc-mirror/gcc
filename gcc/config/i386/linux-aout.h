/* Definitions for Intel 386 running Linux-based GNU systems using a.out.
   Copyright (C) 1992, 1994, 1995, 1997, 1998, 2002 Free Software Foundation, Inc.
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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* This is tested by i386/gas.h.  */
#define YES_UNDERSCORES

#include <i386/gstabs.h>
#include <linux-aout.h>	/* some common stuff */

#undef ASM_COMMENT_START
#define ASM_COMMENT_START "#"

/* Specify predefined symbols in preprocessor.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -D__gnu_linux__ -Dlinux -Asystem=posix"

#undef CPP_SPEC
#define CPP_SPEC "%(cpp_cpu) %{fPIC:-D__PIC__ -D__pic__} %{fpic:-D__PIC__ -D__pic__} %{posix:-D_POSIX_SOURCE}"

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD

/* Don't default to pcc-struct-return, because gcc is the only compiler,
   and we want to retain compatibility with older gcc versions.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

#undef LIB_SPEC

#if 1
/* We no longer link with libc_p.a or libg.a by default. If you
   want to profile or debug the GNU/Linux C library, please add
   -lc_p or -ggdb to LDFLAGS at the link time, respectively.  */
#define LIB_SPEC \
"%{mieee-fp:-lieee} %{p:-lgmon} %{pg:-lgmon} %{!ggdb:-lc} %{ggdb:-lg}"
#else    
#define LIB_SPEC \
"%{mieee-fp:-lieee} %{p:-lgmon -lc_p} %{pg:-lgmon -lc_p} \
 %{!p:%{!pg:%{!g*:-lc} %{g*:-lg -static}}}" 
#endif


#undef LINK_SPEC
#define LINK_SPEC	"-m i386linux"
