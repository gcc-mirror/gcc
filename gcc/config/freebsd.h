/* Base configuration file for all FreeBSD targets.
   Copyright (C) 1999-2020 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Common FreeBSD configuration. 
   All FreeBSD architectures should include this file, which will specify
   their commonalities.
   Adapted from gcc/config/i386/freebsd-elf.h by 
   David O'Brien <obrien@FreeBSD.org>.  
   Further work by David O'Brien <obrien@FreeBSD.org> and
   Loren J. Rittle <ljrittle@acm.org>.  */


/* In case we need to know.  */
#define USING_CONFIG_FREEBSD 1

#undef  TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS() FBSD_TARGET_OS_CPP_BUILTINS()

#undef  CPP_SPEC
#define CPP_SPEC FBSD_CPP_SPEC

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC FBSD_STARTFILE_SPEC

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC FBSD_ENDFILE_SPEC

#undef  LIB_SPEC
#define LIB_SPEC FBSD_LIB_SPEC

#if defined(HAVE_LD_EH_FRAME_HDR)
#define LINK_EH_SPEC "%{!static|static-pie:--eh-frame-hdr} "
#endif

#ifdef TARGET_LIBC_PROVIDES_SSP
#define LINK_SSP_SPEC "%{fstack-protector|fstack-protector-all" \
		       "|fstack-protector-strong|fstack-protector-explicit" \
		       ":-lssp_nonshared}"
#endif

#undef TARGET_LIBC_HAS_FUNCTION
#define TARGET_LIBC_HAS_FUNCTION no_c99_libc_has_function

/* Use --as-needed -lgcc_s for eh support.  */
#ifdef HAVE_LD_AS_NEEDED
#define USE_LD_AS_NEEDED 1
#endif

/* Link -lasan early on the command line.  For -static-libasan, don't link
   it for -shared link, the executable should be compiled with -static-libasan
   in that case, and for executable link with --{,no-}whole-archive around
   it to force everything into the executable.  And similarly for -ltsan
   and -llsan.  */
#if defined(HAVE_LD_STATIC_DYNAMIC)
#undef LIBASAN_EARLY_SPEC
#define LIBASAN_EARLY_SPEC "%{!shared:libasan_preinit%O%s} " \
  "%{static-libasan:%{!shared:" \
  LD_STATIC_OPTION " --whole-archive -lasan --no-whole-archive " \
  LD_DYNAMIC_OPTION "}}%{!static-libasan:-lasan -lpthread}"
#undef LIBTSAN_EARLY_SPEC
#define LIBTSAN_EARLY_SPEC "%{static-libtsan:%{!shared:" \
  LD_STATIC_OPTION " --whole-archive -ltsan --no-whole-archive " \
  LD_DYNAMIC_OPTION "}}%{!static-libtsan:-ltsan -lpthread}"
#undef LIBLSAN_EARLY_SPEC
#define LIBLSAN_EARLY_SPEC "%{static-liblsan:%{!shared:" \
  LD_STATIC_OPTION " --whole-archive -llsan --no-whole-archive " \
  LD_DYNAMIC_OPTION "}}%{!static-liblsan:-llsan -lpthread}"
#endif

/************************[  Target stuff  ]***********************************/

/* All FreeBSD Architectures support the ELF object file format.  */
#undef  OBJECT_FORMAT_ELF
#define OBJECT_FORMAT_ELF

/* Follow FreeBSD's standard headers (<sys/_types.h> etc...).  */

#undef  WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef  WINT_TYPE
#define WINT_TYPE "int"

#define MATH_LIBRARY_PROFILE    "m_p"

/* Code generation parameters.  */

/* Use periods rather than dollar signs in special g++ assembler names.
   This ensures the configuration knows our system correctly so we can link
   with libraries compiled with the native cc.  */
#undef NO_DOLLAR_IN_LABEL

/* Used by libgcc2.c.  We support file locking with fcntl / F_SETLKW.
   This enables the test coverage code to use file locking when exiting a
   program, which avoids race conditions if the program has forked.  */
#define TARGET_POSIX_IO
