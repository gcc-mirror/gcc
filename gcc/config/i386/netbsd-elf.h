/* Definitions of target machine for GNU compiler,
   for i386/ELF NetBSD systems.
   Copyright (C) 2001 Free Software Foundation, Inc.
   Contributed by matthew green <mrg@eterna.com.au>

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

/* Provide a LIB_SPEC appropriate for NetBSD.  Just select the appropriate
   libc, depending on whether we're doing profiling; if `-posix' is specified,
   link against the appropriate libposix first.  */

#undef LIB_SPEC
#define LIB_SPEC							\
  "%{posix:%{!p:%{!pg:-lposix}}%{p:-lposix_p}%{pg:-lposix_p}}		\
   %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p}"

/* Provide a STARTFILE_SPEC appropriate for NetBSD ELF targets.  Here we
   provide support for the special GCC option -static.  On ELF targets,
   we also add the crtbegin.o file which provides part of the support
   for getting C++ file-scope static objects constructed before entering
   `main'.  We use the NetBSD crt0. */

#undef STARTFILE_SPEC
#define	STARTFILE_SPEC \
 "%{!shared: \
     %{pg:gcrt0%O%s} \
     %{!pg: \
        %{p:gcrt0%O%s} \
        %{!p:crt0%O%s}}} \
   %{!shared:crtbegin%O%s} %{shared:crtbeginS%O%s}"

/* Provide an ENDFILE_SPEC approrpiate for NetBSD ELF targets.  Here we
   add crtend.o, which provides part of the support for getting C++
   file-scope static objects deconstructed after exiting `main'. */

#undef ENDFILE_SPEC
#define	ENDFILE_SPEC \
 "%{!shared:crtend%O%s} %{shared:crtendS%O%s}"

/* Provide a LINK_SPEC appropriate for a NetBSD/i386 ELF target.  Only
   the linker emulation is i386-specific.  The rest are
   common to all ELF targets, except for the name of the start function. */

#undef LINK_SPEC
#define LINK_SPEC \
 "-m elf_i386 \
  %{assert*} %{R*} \
  %{shared:-shared} \
  %{!shared: \
    -dp \
    %{!nostdlib:%{!r*:%{!e*:-e __start}}} \
    %{!static: \
      -dy %{rdynamic:-export-dynamic} \
      %{!dynamic-linker:-dynamic-linker /usr/libexec/ld.elf_so}} \
    %{static:-static}}"

/* Provide a CPP_SPEC appropriate for NetBSD.  Current we just deal with
   the GCC option `-posix'.  */

#undef CPP_SPEC
#define CPP_SPEC "%{posix:-D_POSIX_SOURCE}"

/* Provide an ASM_SPEC appropriate for NetBSD.  Currently we only deal
   with the options for generating PIC code.  */

#undef ASM_SPEC
#define ASM_SPEC " %| %{fpic:-k} %{fPIC:-k -K}"

/* Provide a LIB_SPEC appropriate for NetBSD.  Just select the appropriate
   libc, depending on whether we're doing profiling.  */

#undef LIB_SPEC
#define LIB_SPEC "%{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p}"

/* This defines which switch letters take arguments. */
#undef SWITCH_TAKES_ARG
#define SWITCH_TAKES_ARG(CHAR) \
  (DEFAULT_SWITCH_TAKES_ARG(CHAR) \
   || (CHAR) == 'R')

/* Implicit library calls should use memcpy, not bcopy, etc.  */

#define TARGET_MEM_FUNCTIONS

/* Names to predefine in the preprocessor for this target machine. */

#define CPP_PREDEFINES "\
-Di386 -D__NetBSD__ -D__ELF__ -Asystem(unix) -Asystem(NetBSD)"

/* Make gcc agree with <machine/ansi.h> */

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_UNSIGNED
#define WCHAR_UNSIGNED 0

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

#undef WINT_TYPE
#define WINT_TYPE "int"

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  Under NetBSD/i386, the assembler does
   nothing special with -pg. */

#undef ASM_APP_ON
#define ASM_APP_ON "#APP\n"

#undef ASM_APP_OFF
#define ASM_APP_OFF "#NO_APP\n"

#undef ASM_FINAL_SPEC
#define ASM_FINAL_SPEC "%{pipe:-}"

#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(n)  svr4_dbx_register_map[n]

#undef  NO_PROFILE_COUNTERS
#define NO_PROFILE_COUNTERS

#undef HAS_INIT_SECTION

#undef DEFAULT_VTABLE_THUNKS
#define DEFAULT_VTABLE_THUNKS 1

/* This is how we tell the assembler that two symbols have the same value.  */

#define ASM_OUTPUT_DEF(FILE,NAME1,NAME2) \
  do { assemble_name(FILE, NAME1); 	 \
       fputs(" = ", FILE);		 \
       assemble_name(FILE, NAME2);	 \
       fputc('\n', FILE); } while (0)

/* A C statement to output to the stdio stream FILE an assembler
   command to advance the location counter to a multiple of 1<<LOG
   bytes if it is within MAX_SKIP bytes.

   This is used to align code labels according to Intel recommendations.  */

#ifdef HAVE_GAS_MAX_SKIP_P2ALIGN
#define ASM_OUTPUT_MAX_SKIP_ALIGN(FILE, LOG, MAX_SKIP)					\
  if ((LOG) != 0) {														\
    if ((MAX_SKIP) == 0) fprintf ((FILE), "\t.p2align %d\n", (LOG));	\
    else fprintf ((FILE), "\t.p2align %d,,%d\n", (LOG), (MAX_SKIP));	\
  }
#endif

/*
 * We always use gas here, so we don't worry about ECOFF assembler problems.
 */
#undef TARGET_GAS
#define TARGET_GAS	1

/* Default to pcc-struct-return, because this is the ELF abi and
   we don't care about compatibility with older gcc versions.  */
#define DEFAULT_PCC_STRUCT_RETURN 1

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (NetBSD/i386 ELF target)");
