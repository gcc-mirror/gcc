/* Definitions for SH running Linux-based GNU systems using ELF
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.
   Contributed by Kazumoto Kojima <kkojima@rr.iij4u.or.jp>

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
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Run-time Target Specification.  */
#undef TARGET_VERSION
#define TARGET_VERSION  fputs (" (SH GNU/Linux with ELF)", stderr);

/* Return to the original ELF way.  */
#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"
 
#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"
  
#undef WCHAR_TYPE
#define WCHAR_TYPE "long int"
   
#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD

#undef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC "\
   %{fPIC:-D__PIC__ -D__pic__} \
   %{fpic:-D__PIC__ -D__pic__} \
   %{posix:-D_POSIX_SOURCE} \
   %{pthread:-D_REENTRANT -D_PTHREADS} \
"

#undef SUBTARGET_CPP_ENDIAN_SPEC
#define SUBTARGET_CPP_ENDIAN_SPEC \
  "%{mb:-D__BIG_ENDIAN__} \
   %{!mb:-D__LITTLE_ENDIAN__}"

#undef CPP_DEFAULT_CPU_SPEC
#define CPP_DEFAULT_CPU_SPEC "-D__SH3__ -D__sh3__"


#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-D__ELF__ -Dunix -D__sh__ -Dlinux -Asystem=posix"

#undef ASM_SPEC
#define ASM_SPEC  "%{!mb:-little} %{mrelax:-relax}"

#undef CC1_SPEC
#define CC1_SPEC \
  "-musermode %{!mb:-ml} %{!m3e:%{!m4:-m3}}"

#undef CC1PLUS_SPEC
#define CC1PLUS_SPEC \
  "-musermode %{!mb:-ml} %{!m3e:%{!m4:-m3}}"

#undef LINK_SPEC
#define LINK_SPEC \
  "%{!mb:-m shlelf_linux} %{mrelax:-relax} \
   %{shared:-shared} \
   %{!static: \
     %{rdynamic:-export-dynamic} \
     %{!dynamic-linker:-dynamic-linker /lib/ld.so.1} \
     %{!rpath:-rpath /lib}} \
   %{static:-static}"

#undef LIB_SPEC
#define LIB_SPEC \
  "%{shared: -lc} \
   %{!shared: %{pthread:-lthread} \
     %{profile:-lc_p} %{!profile: -lc}}"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!shared: \
     %{pg:gcrt1.o%s} %{!pg:%{p:gcrt1.o%s} \
		       %{!p:%{profile:gcrt1.o%s} \
			 %{!profile:crt1.o%s}}}} \
   crti.o%s %{!shared:crtbegin.o%s} %{shared:crtbeginS.o%s}"
