/* Definitions for SH running Linux-based GNU systems using ELF
   Copyright (C) 1999, 2000, 2002 Free Software Foundation, Inc.
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

#undef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC "\
   %{posix:-D_POSIX_SOURCE} \
   %{pthread:-D_REENTRANT -D_PTHREADS} \
"

#define TARGET_OS_CPP_BUILTINS() \
do { \
  builtin_define_std ("unix"); \
  builtin_define ("__gnu_linux__"); \
  builtin_define_std ("linux"); \
  builtin_assert ("system=posix"); \
} while (0)

#undef TARGET_DEFAULT
#define TARGET_DEFAULT \
  (TARGET_CPU_DEFAULT | USERMODE_BIT | TARGET_ENDIAN_DEFAULT)

#undef SUBTARGET_LINK_EMUL_SUFFIX
#define SUBTARGET_LINK_EMUL_SUFFIX "_linux"
#undef SUBTARGET_LINK_SPEC
#define SUBTARGET_LINK_SPEC \
  "%{shared:-shared} \
   %{!static: \
     %{rdynamic:-export-dynamic} \
     %{!dynamic-linker:-dynamic-linker /lib/ld-linux.so.2} \
     %{!rpath:-rpath /lib}} \
   %{static:-static}"

/* The GNU C++ standard library requires that these macros be defined.  */
#undef CPLUSPLUS_CPP_SPEC
#define CPLUSPLUS_CPP_SPEC "-D_GNU_SOURCE %(cpp)"

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
