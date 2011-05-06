/* Definitions for systems using, at least optionally, a GNU
   (glibc-based) userspace or other userspace with libc derived from
   glibc (e.g. uClibc) or for which similar specs are appropriate.
   Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2003, 2004, 2005, 2006,
   2007, 2009, 2010, 2011 Free Software Foundation, Inc.
   Contributed by Eric Youngdale.
   Modified for stabs-in-ELF by H.J. Lu (hjl@lucon.org).

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* Don't assume anything about the header files.  */
#define NO_IMPLICIT_EXTERN_C

#undef ASM_APP_ON
#define ASM_APP_ON "#APP\n"

#undef ASM_APP_OFF
#define ASM_APP_OFF "#NO_APP\n"

/* Provide a STARTFILE_SPEC appropriate for GNU userspace.  Here we add
   the GNU userspace magical crtbegin.o file (see crtstuff.c) which
   provides part of the support for getting C++ file-scope static
   object constructed before entering `main'.  */
   
#if defined HAVE_LD_PIE
#define GNU_USER_TARGET_STARTFILE_SPEC \
  "%{!shared: %{pg|p|profile:gcrt1.o%s;pie:Scrt1.o%s;:crt1.o%s}} \
   crti.o%s %{static:crtbeginT.o%s;shared|pie:crtbeginS.o%s;:crtbegin.o%s}"
#else
#define GNU_USER_TARGET_STARTFILE_SPEC \
  "%{!shared: %{pg|p|profile:gcrt1.o%s;:crt1.o%s}} \
   crti.o%s %{static:crtbeginT.o%s;shared|pie:crtbeginS.o%s;:crtbegin.o%s}"
#endif
#undef  STARTFILE_SPEC
#define STARTFILE_SPEC GNU_USER_TARGET_STARTFILE_SPEC

/* Provide a ENDFILE_SPEC appropriate for GNU userspace.  Here we tack on
   the GNU userspace magical crtend.o file (see crtstuff.c) which
   provides part of the support for getting C++ file-scope static
   object constructed before entering `main', followed by a normal
   GNU userspace "finalizer" file, `crtn.o'.  */

#define GNU_USER_TARGET_ENDFILE_SPEC \
  "%{shared|pie:crtendS.o%s;:crtend.o%s} crtn.o%s"
#undef  ENDFILE_SPEC
#define ENDFILE_SPEC GNU_USER_TARGET_ENDFILE_SPEC

/* This is for -profile to use -lc_p instead of -lc.  */
#define GNU_USER_TARGET_CC1_SPEC "%{profile:-p}"
#ifndef CC1_SPEC
#define CC1_SPEC GNU_USER_TARGET_CC1_SPEC
#endif

/* The GNU C++ standard library requires that these macros be defined.  */
#undef CPLUSPLUS_CPP_SPEC
#define CPLUSPLUS_CPP_SPEC "-D_GNU_SOURCE %(cpp)"

#define GNU_USER_TARGET_LIB_SPEC \
  "%{pthread:-lpthread} \
   %{shared:-lc} \
   %{!shared:%{mieee-fp:-lieee} %{profile:-lc_p}%{!profile:-lc}}"
#undef  LIB_SPEC
#define LIB_SPEC GNU_USER_TARGET_LIB_SPEC

#if defined(HAVE_LD_EH_FRAME_HDR)
#define LINK_EH_SPEC "%{!static:--eh-frame-hdr} "
#endif

#undef LINK_GCC_C_SEQUENCE_SPEC
#define LINK_GCC_C_SEQUENCE_SPEC \
  "%{static:--start-group} %G %L %{static:--end-group}%{!static:%G}"

/* Use --as-needed -lgcc_s for eh support.  */
#ifdef HAVE_LD_AS_NEEDED
#define USE_LD_AS_NEEDED 1
#endif

#define TARGET_POSIX_IO

#define TARGET_C99_FUNCTIONS 1
#define TARGET_HAS_SINCOS 1
