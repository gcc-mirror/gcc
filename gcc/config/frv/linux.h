/* Target macros for the FRV Linux port of GCC.
   Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004
   Free Software Foundation, Inc.
   Contributed by Red Hat Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.  */

#ifndef __FRV_LINUX_H__
#define __FRV_LINUX_H__

#undef SUBTARGET_DRIVER_SELF_SPECS
#define SUBTARGET_DRIVER_SELF_SPECS \
  "%{!mno-fdpic:-mfdpic}",

#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!shared: %{pg|p|profile:gcrt1.o%s;pie:Scrt1.o%s;:crt1.o%s}} \
   crti.o%s %{static:crtbeginT.o%s;shared|pie:crtbeginS.o%s;:crtbegin.o%s}"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{shared|pie:crtendS.o%s;:crtend.o%s} crtn.o%s"

#undef LINK_SPEC
#define LINK_SPEC "\
  %{mfdpic: -m elf32frvfd -z text} %{shared} %{pie} \
  %{!shared: %{!static: \
   %{rdynamic:-export-dynamic} \
   %{!dynamic-linker:-dynamic-linker /lib/ld.so.1}} \
   %{static}}"

/* Support for compile-time default CPU.  */
#define OPTION_DEFAULT_SPECS \
  {"cpu", "%{!mcpu=*:-mcpu=%(VALUE)}" }

/* Define OS-specific predefined preprocessor macros.  */
#define TARGET_OS_CPP_BUILTINS()	\
  do {					\
    builtin_define ("__gnu_linux__");	\
    builtin_define_std ("linux");	\
    builtin_define_std ("unix");	\
    builtin_assert ("system=linux");	\
  } while (0)

#define HAS_INIT_SECTION 1
#define INIT_SECTION_ASM_OP	"\t.section .init,\"ax\""
#define FINI_SECTION_ASM_OP	"\t.section .fini,\"ax\""

#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC)	\
asm (SECTION_OP); \
asm ("ldi.p @(fp,4), gr15 ! call " #FUNC); \
asm (TEXT_SECTION_ASM_OP);

#undef INVOKE__main

#undef Twrite
#define Twrite __write

/* uClibc doesn't support many of the C90-reserved C99-defined math
   functions.  Make sure we don't implicitly generate them unless C99
   support is explicitly requested.  This will affect both frv-linux
   and frv-uclinux.  Even though the glibc, the primary library for
   frv-linux, would enable better code to be generated with
   TARGET_C99_FUNCTIONS defined to 1, uClinux can be used as the
   library for frv-linux as well, and we'd better have that work
   correctly.  Maybe we move this to a uclibc.h header in the future,
   and use that for frv-uclinux and frv-linux-uclibc?  Define it here
   for now, such that we can still get exactly the same code out of
   both frv-linux-gcc and frv-uclinux-gcc, when feeding them the same
   preprocessed sources.  */
#undef TARGET_C99_FUNCTIONS
#define TARGET_C99_FUNCTIONS 0

#endif /* __FRV_LINUX_H__ */
