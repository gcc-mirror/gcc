/* Definitions of target machine for GNU compiler,
   for x86-64/ELF NetBSD systems.
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
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


/* Provide a LINK_SPEC appropriate for a NetBSD/x86-64 ELF target.
   This is a copy of LINK_SPEC from <netbsd-elf.h> tweaked for
   the x86-64 target.  */

#undef LINK_SPEC
#define LINK_SPEC							\
  "%{!m32:-m elf_x86_64}						\
   %{m32:-m elf_i386}							\
   %{assert*} %{R*}							\
   %{shared:-shared}							\
   %{!shared:								\
     -dc -dp								\
     %{!nostdlib:							\
       %{!r*:								\
	  %{!e*:-e _start}}}						\
     %{!static:								\
       %{rdynamic:-export-dynamic}					\
       %{!dynamic-linker:-dynamic-linker /usr/libexec/ld.elf_so}}	\
     %{static:-static}}"


/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES							\
  "-D__NetBSD__ -D__ELF__ -Asystem=unix -Asystem=NetBSD"


/* Provide some extra CPP specs needed by NetBSD/x86_64.  */
#define CPP_LP64_SPEC "%{!m32:-D_LP64}"

#define CPP_SUBTARGET_SPEC "%(cpp_lp64)"

#undef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS						\
  { "cpp_lp64", CPP_LP64_SPEC },					\
  { "cpp_subtarget", CPP_SUBTARGET_SPEC },


/* Provide a CPP_SPEC appropriate for NetBSD.  Currently we deal with
   our subtarget specs and the GCC option `-posix'.  */

#undef CPP_SPEC
#define CPP_SPEC "%(cpp_cpu) %(cpp_subtarget) %{posix:-D_POSIX_SOURCE}"


/* Output assembler code to FILE to call the profiler.  */

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)				\
{									\
  if (TARGET_64BIT && flag_pic)						\
    fprintf (FILE, "\tcall *__mcount@PLT\n");				\
  else if (flag_pic)							\
    fprintf (FILE, "\tcall *__mcount@PLT\n");				\
  else									\
    fprintf (FILE, "\tcall __mcount\n");				\
}


#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (NetBSD/x86_64 ELF)");
