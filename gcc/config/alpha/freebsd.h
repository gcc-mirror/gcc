/* Definitions for DEC Alpha/AXP running FreeBSD using the ELF format
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by David E. O'Brien <obrien@FreeBSD.org> and BSDi.

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


/* Provide a CPP_SPEC appropriate for FreeBSD/alpha.  Besides the dealing with
   the GCC option `-posix', and PIC issues as on all FreeBSD platforms, we must
   deal with the Alpha's FP issues.  */

#undef  CPP_SPEC
#define CPP_SPEC "%(cpp_cpu)						\
  %{fPIC:-D__PIC__ -D__pic__} %{fpic:-D__PIC__ -D__pic__}		\
  %{posix:-D_POSIX_SOURCE}						\
  %{mieee:-D_IEEE_FP}							\
  %{mieee-with-inexact:-D_IEEE_FP -D_IEEE_FP_INEXACT}"

#undef  LINK_SPEC
#define LINK_SPEC "-m elf64alpha %{G*} %{relax:-relax}			\
  %{p:%e`-p' not supported; use `-pg' and gprof(1)}			\
  %{Wl,*:%*}								\
  %{assert*} %{R*} %{rpath*} %{defsym*}					\
  %{shared:-Bshareable %{h*} %{soname*}}				\
  %{symbolic:-Bsymbolic}						\
  %{!shared:								\
    %{!static:								\
      %{rdynamic:-export-dynamic}					\
      %{!dynamic-linker:-dynamic-linker /usr/libexec/ld-elf.so.1}}	\
    %{static:-Bstatic}}"

/* Provide an ASM_SPEC appropriate for a FreeBSD/Alpha target.  This differs
   from the generic FreeBSD ASM_SPEC in that no special handling of PIC is
   necessary on the Alpha.  */
/* Per Richard Henderson <rth@cygnus.com>, it is better to use the `.arch'
   directive in the assembly file.  alpha/elf.h gives us this in
   "ASM_FILE_START".
#undef  ASM_SPEC
#define ASM_SPEC " %| %{mcpu=*:-m%*}"
*/


/************************[  Target stuff  ]***********************************/

/* Define the actual types of some ANSI-mandated types.  
   Needs to agree with <machine/ansi.h>.  GCC defaults come from c-decl.c,
   c-common.c, and config/<arch>/<arch>.h.  */

/* alpha.h gets this wrong for FreeBSD.  We use the GCC defaults instead.  */
#undef WCHAR_TYPE

#undef  WCHAR_UNSIGNED
#define WCHAR_UNSIGNED 0

#undef  WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

#undef  TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (FreeBSD/alpha ELF)");

#define TARGET_ELF		1

#undef  TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_FP | MASK_FPREGS | MASK_GAS)

#undef HAS_INIT_SECTION

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  Under FreeBSD/Alpha, the assembler does
   nothing special with -pg. */

#undef  FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)				\
	fputs ("\tjsr $28,_mcount\n", (FILE))

/* Show that we need a GP when profiling.  */
#undef  TARGET_PROFILING_NEEDS_GP
#define TARGET_PROFILING_NEEDS_GP 1

/* This is the char to use for continuation (in case we need to turn
   continuation back on).  */

#undef  DBX_CONTIN_CHAR
#define DBX_CONTIN_CHAR	'?'
