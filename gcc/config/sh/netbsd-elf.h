/* Definitions for SH running NetBSD using ELF
   Copyright (C) 2002 Free Software Foundation, Inc.
   Contributed by Wasabi Systems, Inc.

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
#if TARGET_ENDIAN_DEFAULT == LITTLE_ENDIAN_BIT
#define TARGET_VERSION  fputs (" (NetBSD/shle ELF)", stderr);
#else
#define TARGET_VERSION	fputs (" (NetBSD/sh ELF)", stderr);
#endif

#define TARGET_OS_CPP_BUILTINS()					\
  do									\
    {									\
      NETBSD_OS_CPP_BUILTINS_ELF();					\
      builtin_define ("__NO_LEADING_UNDERSCORES__");			\
    }									\
  while (0)

/* Provide a LINK_SPEC appropriate for a NetBSD/sh ELF target.
   This is a copy of LINK_SPEC from <netbsd-elf.h> tweaked for
   the SH target.  */

#undef LINK_SPEC
#define LINK_SPEC							\
  "%{assert*} %{R*}							\
   %{mb:-m shelf_nbsd}							\
   %{ml:-m shlelf_nbsd}							\
   %{mrelax:-relax}							\
   %{shared:-shared}							\
   %{!shared:								\
     -dc -dp								\
     %{!nostdlib:							\
       %{!r*:								\
	 %{!e*:-e __start}}}						\
     %{!static:								\
       %{rdynamic:-export-dynamic}					\
       %{!dynamic-linker:-dynamic-linker /usr/libexec/ld.elf_so}}	\
     %{static:-static}}"


/* Provide a CPP_SPEC appropriate for NetBSD.  */
#undef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC NETBSD_CPP_SPEC

/* Restore the ASM_SPEC from sh/sh.h; sh/elf.h clobbers it.  */
#undef ASM_SPEC
#define ASM_SPEC  "%(subtarget_asm_endian_spec) %{mrelax:-relax}"

#undef TARGET_DEFAULT
#define TARGET_DEFAULT \
  (SH1_BIT|SH2_BIT|SH3_BIT | USERMODE_BIT | TARGET_ENDIAN_DEFAULT)

 
#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(STREAM,LABELNO)				\
do									\
  {									\
    fprintf((STREAM), "\tmov.l\t%cLP%d,r1\n",				\
            LOCAL_LABEL_PREFIX, (LABELNO));				\
    fprintf((STREAM), "\tmova\t%cLP%dr,r0\n",				\
            LOCAL_LABEL_PREFIX, (LABELNO));				\
    fprintf((STREAM), "\tjmp\t@r1\n");					\
    fprintf((STREAM), "\tnop\n");					\
    fprintf((STREAM), "\t.align\t2\n");					\
    fprintf((STREAM), "%cLP%d:\t.long\t__mcount\n",			\
            LOCAL_LABEL_PREFIX, (LABELNO));				\
    fprintf((STREAM), "%cLP%dr:\n", LOCAL_LABEL_PREFIX, (LABELNO));	\
  }									\
while (0)
