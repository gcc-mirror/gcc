/* Definitions for DEC Alpha/AXP running FreeBSD using the ELF format
   Copyright (C) 2000, 2002 Free Software Foundation, Inc.
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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


/* Provide a FBSD_TARGET_CPU_CPP_BUILTINS and CPP_SPEC appropriate for
   FreeBSD/alpha.  Besides the dealing with
   the GCC option `-posix', and PIC issues as on all FreeBSD platforms, we must
   deal with the Alpha's FP issues.  */

#undef FBSD_TARGET_CPU_CPP_BUILTINS
#define FBSD_TARGET_CPU_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define ("__LP64__");		\
      if (flag_pic)				\
	{					\
	  builtin_define ("__PIC__");		\
	  builtin_define ("__pic__");		\
	}					\
    }						\
  while (0)

#undef  CPP_SPEC
#define CPP_SPEC "%(cpp_subtarget) %{posix:-D_POSIX_SOURCE}"

#define LINK_SPEC "%{G*} %{relax:-relax}				\
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


/************************[  Target stuff  ]***********************************/

/* Define the actual types of some ANSI-mandated types.  
   Needs to agree with <machine/ansi.h>.  GCC defaults come from c-decl.c,
   c-common.c, and config/<arch>/<arch>.h.  */

/* alpha.h gets this wrong for FreeBSD.  We use the GCC defaults instead.  */
#undef WCHAR_TYPE

#undef  WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE	32

#undef  TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (FreeBSD/alpha ELF)");

#define TARGET_ELF	1

#undef  TARGET_DEFAULT
#define TARGET_DEFAULT	(MASK_FP | MASK_FPREGS | MASK_GAS)

#undef HAS_INIT_SECTION

/* Show that we need a GP when profiling.  */
#undef  TARGET_PROFILING_NEEDS_GP
#define TARGET_PROFILING_NEEDS_GP 1

/* This is the char to use for continuation (in case we need to turn
   continuation back on).  */

#undef  DBX_CONTIN_CHAR
#define DBX_CONTIN_CHAR	'?'

/* Don't default to pcc-struct-return, we want to retain compatibility with
   older FreeBSD releases AND pcc-struct-return may not be reentrant.  */

#undef  DEFAULT_PCC_STRUCT_RETURN
#define DEFAULT_PCC_STRUCT_RETURN 0
