/* Definitions for Linux-based GNU systems with ELF format
   Copyright (C) 1995, 1996, 1997, 1998 Free Software Foundation, Inc.
   Contributed by Eric Youngdale.
   Modified for stabs-in-ELF by H.J. Lu (hjl@lucon.org).

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

/* Don't assume anything about the header files. */
#define NO_IMPLICIT_EXTERN_C

#undef HAVE_ATEXIT
#define HAVE_ATEXIT

/* GNU/Linux uses ctype from glibc.a. I am not sure how complete it is.
   For now, we play safe. It may change later. */

#if 0
#undef MULTIBYTE_CHARS
#define MULTIBYTE_CHARS 1
#endif

#undef ASM_APP_ON
#define ASM_APP_ON "#APP\n"

#undef ASM_APP_OFF
#define ASM_APP_OFF "#NO_APP\n"

#define SET_ASM_OP	".set"

/* Use stabs instead of DWARF debug format.  */
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG
#include "svr4.h"

#undef MD_EXEC_PREFIX
#undef MD_STARTFILE_PREFIX

/* Output at beginning of assembler file.  */
/* The .file command should always begin the output.  */
#undef ASM_FILE_START
#define ASM_FILE_START(FILE)						\
  do {									\
	output_file_directive (FILE, main_input_filename);		\
	fprintf (FILE, "\t.version\t\"01.01\"\n");			\
  } while (0)

/* Provide a STARTFILE_SPEC appropriate for GNU/Linux.  Here we add
   the GNU/Linux magical crtbegin.o file (see crtstuff.c) which
   provides part of the support for getting C++ file-scope static
   object constructed before entering `main'. */
   
#undef	STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!shared: \
     %{pg:gcrt1.o%s} %{!pg:%{p:gcrt1.o%s} \
		       %{!p:%{profile:gcrt1.o%s} \
			 %{!profile:crt1.o%s}}}} \
   crti.o%s %{!shared:crtbegin.o%s} %{shared:crtbeginS.o%s}"

/* Provide a ENDFILE_SPEC appropriate for GNU/Linux.  Here we tack on
   the GNU/Linux magical crtend.o file (see crtstuff.c) which
   provides part of the support for getting C++ file-scope static
   object constructed before entering `main', followed by a normal
   GNU/Linux "finalizer" file, `crtn.o'.  */

#undef	ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{!shared:crtend.o%s} %{shared:crtendS.o%s} crtn.o%s"

/* This is for -profile to use -lc_p instead of -lc. */
#undef  CC1_SPEC
#define CC1_SPEC "%{profile:-p}"

#ifndef USE_GNULIBC_1
#undef DEFAULT_VTABLE_THUNKS
#define DEFAULT_VTABLE_THUNKS 1
#endif

#undef	LIB_SPEC
/* We no longer link with libc_p.a or libg.a by default. If you
   want to profile or debug the GNU/Linux C library, please add
   -profile or -ggdb to LDFLAGS at the link time, respectively. */
#if 1
#ifdef USE_GNULIBC_1
#define LIB_SPEC \
  "%{!shared: %{p:-lgmon} %{pg:-lgmon} %{profile:-lgmon -lc_p} \
     %{!profile:%{!ggdb:-lc} %{ggdb:-lg}}}"
#else
#define LIB_SPEC \
  "%{shared: -lc} \
   %{!shared: %{mieee-fp:-lieee} %{pthread:-lpthread} \
	%{profile:-lc_p} %{!profile: -lc}}"
#endif
#else
#define LIB_SPEC \
  "%{!shared: \
     %{p:-lgmon -lc_p} %{pg:-lgmon -lc_p} \
       %{!p:%{!pg:%{!g*:-lc} %{g*:-lg}}}}"
#endif
