/* Definitions of target machine for GNU compiler,
   for Alpha NetBSD systems.
   Copyright (C) 1998, 2002 Free Software Foundation, Inc.

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

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_FP | MASK_FPREGS | MASK_GAS)

#undef CPP_PREDEFINES
#define CPP_PREDEFINES							\
  "-D__NetBSD__ -D__ELF__ -Asystem=unix -Asystem=NetBSD"


/* Show that we need a GP when profiling.  */
#undef TARGET_PROFILING_NEEDS_GP
#define TARGET_PROFILING_NEEDS_GP 1


/* Provide a CPP_SPEC appropriate for NetBSD/alpha.  In addition to
   the standard NetBSD specs, we also handle Alpha FP mode indications.  */

#undef CPP_SPEC
#define CPP_SPEC							\
  "%{mieee:-D_IEEE_FP}							\
   %{mieee-with-inexact:-D_IEEE_FP -D_IEEE_FP_INEXACT}			\
   %(cpp_cpu) %(cpp_subtarget)"

#undef CPP_SUBTARGET_SPEC
#define CPP_SUBTARGET_SPEC						\
  "%{posix:-D_POSIX_SOURCE}"


/* Provide a LINK_SPEC appropriate for a NetBSD/alpha ELF target.
   This is a copy of LINK_SPEC from <netbsd-elf.h> tweaked for
   the alpha target.  */

#undef LINK_SPEC
#define LINK_SPEC							\
  "%{G*} %{relax:-relax}						\
   %{O*:-O3} %{!O*:-O1}							\
   %{assert*} %{R*}							\
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


/* Provide an ENDFILE_SPEC appropriate for NetBSD/alpha ELF.  Here we
   add crtend.o, which provides part of the support for getting
   C++ file-scope static objects deconstructed after exiting "main".

   We also need to handle the GCC option `-ffast-math'.  */

#undef ENDFILE_SPEC
#define ENDFILE_SPEC		\
  "%{ffast-math|funsafe-math-optimizations:crtfm%O%s} \
   %{!shared:crtend%O%s} %{shared:crtendS%O%s}"


/* Make gcc agree with <machine/ansi.h> */

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_UNSIGNED
#define WCHAR_UNSIGNED 0

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

#undef WINT_TYPE
#define WINT_TYPE "int"

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (NetBSD/alpha ELF)");
