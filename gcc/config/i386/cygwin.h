/* Operating system specific defines to be used when targeting GCC for
   hosting on Windows32, using a Unix style C library and tools.
   Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
   2007, 2008, 2009, 2010, 2011 Free Software Foundation, Inc.

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

#define EXTRA_OS_CPP_BUILTINS()  /* Nothing.  */

#undef CPP_SPEC
#define CPP_SPEC "%(cpp_cpu) %{posix:-D_POSIX_SOURCE} \
  -D__CYGWIN32__ -D__CYGWIN__ %{!ansi:-Dunix} -D__unix__ -D__unix \
  %{mwin32:-DWIN32 -D_WIN32 -D__WIN32 -D__WIN32__ %{!ansi:-DWINNT}} \
  %{!nostdinc:%{!mno-win32:-idirafter ../include/w32api%s -idirafter ../../include/w32api%s}}\
"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "\
  %{!shared: %{!mdll: crt0%O%s \
  %{pg:gcrt0%O%s}}}\
  crtbegin.o%s"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{Ofast|ffast-math|funsafe-math-optimizations:crtfastmath.o%s}\
   crtend.o%s"

/* Normally, -lgcc is not needed since everything in it is in the DLL, but we
   want to allow things to be added to it when installing new versions of
   GCC without making a new CYGWIN.DLL, so we leave it.  Profiling is handled
   by calling the init function from main.  */

#ifdef ENABLE_SHARED_LIBGCC
#define SHARED_LIBGCC_SPEC " \
 %{static|static-libgcc:-lgcc -lgcc_eh} \
 %{!static: \
   %{!static-libgcc: \
     %{!shared: \
       %{!shared-libgcc:-lgcc -lgcc_eh} \
       %{shared-libgcc:-lgcc_s -lgcc} \
      } \
     %{shared:-lgcc_s -lgcc} \
    } \
  } "
#else
#define SHARED_LIBGCC_SPEC " -lgcc "
#endif

#undef REAL_LIBGCC_SPEC
#define REAL_LIBGCC_SPEC SHARED_LIBGCC_SPEC

/* We have to dynamic link to get to the system DLLs.  All of libc, libm and
   the Unix stuff is in cygwin.dll.  The import library is called
   'libcygwin.a'.  For Windows applications, include more libraries, but
   always include kernel32.  We'd like to specific subsystem windows to
   ld, but that doesn't work just yet.  */

#undef LIB_SPEC
#define LIB_SPEC "\
  %{pg:-lgmon} \
  -lcygwin \
  %{mwindows:-lgdi32 -lcomdlg32} \
  -ladvapi32 -lshell32 -luser32 -lkernel32"

/* To implement C++ function replacement we always wrap the cxx
   malloc-like operators.  See N2800 #17.6.4.6 [replacement.functions] */
#define CXX_WRAP_SPEC_LIST " \
  --wrap _Znwj \
  --wrap _Znaj \
  --wrap _ZdlPv \
  --wrap _ZdaPv \
  --wrap _ZnwjRKSt9nothrow_t \
  --wrap _ZnajRKSt9nothrow_t \
  --wrap _ZdlPvRKSt9nothrow_t \
  --wrap _ZdaPvRKSt9nothrow_t \
"

#if defined (USE_CYGWIN_LIBSTDCXX_WRAPPERS)

#if USE_CYGWIN_LIBSTDCXX_WRAPPERS
/* Default on, only explict -mno disables.  */
#define CXX_WRAP_SPEC_OPT "!mno-use-libstdc-wrappers"
#else
/* Default off, only explict -m enables.  */
#define CXX_WRAP_SPEC_OPT "muse-libstdc-wrappers"
#endif

#define CXX_WRAP_SPEC "%{" CXX_WRAP_SPEC_OPT ":" CXX_WRAP_SPEC_LIST "}"

#else /* !defined (USE_CYGWIN_LIBSTDCXX_WRAPPERS)  */

#define CXX_WRAP_SPEC ""

#endif /* ?defined (USE_CYGWIN_LIBSTDCXX_WRAPPERS) */

#define LINK_SPEC "\
  %{mwindows:--subsystem windows} \
  %{mconsole:--subsystem console} \
  " CXX_WRAP_SPEC " \
  %{shared: %{mdll: %eshared and mdll are not compatible}} \
  %{shared: --shared} %{mdll:--dll} \
  %{static:-Bstatic} %{!static:-Bdynamic} \
  %{shared|mdll: --enable-auto-image-base -e __cygwin_dll_entry@12} \
  --dll-search-prefix=cyg -tsaware"

/* Binutils does not handle weak symbols from dlls correctly.  For now,
   do not use them unnecessarily in gthr-posix.h.  */
#define GTHREAD_USE_WEAK 0

/* Every program on cygwin links against cygwin1.dll which contains 
   the pthread routines.  There is no need to explicitly link them
   and the -pthread flag is not recognized.  */
#undef GOMP_SELF_SPECS
#define GOMP_SELF_SPECS ""
#undef GTM_SELF_SPECS
#define GTM_SELF_SPECS ""

/* This matches SHLIB_SONAME and SHLIB_SOVERSION in t-cygwin. */
#if DWARF2_UNWIND_INFO
#define LIBGCC_EH_EXTN ""
#else
#define LIBGCC_EH_EXTN "-sjlj"
#endif
#define LIBGCC_SONAME "cyggcc_s" LIBGCC_EH_EXTN "-1.dll"

/* We should find a way to not have to update this manually.  */
#define LIBGCJ_SONAME "cyggcj" /*LIBGCC_EH_EXTN*/ "-13.dll"

