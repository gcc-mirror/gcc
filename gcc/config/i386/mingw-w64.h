/* Operating system specific defines to be used when targeting GCC for
   hosting on Windows 32/64 via mingw-w64 runtime, using GNU tools and
   the Windows API Library.
   Copyright (C) 2009-2025 Free Software Foundation, Inc.

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

/* Enable -municode feature and support optional pthread support.  */

#undef CPP_SPEC
#define CPP_SPEC "%{posix:-D_POSIX_SOURCE} %{mthreads:-D_MT} " \
		 "%{municode:-DUNICODE} " \
		 "%{" SPEC_PTHREAD1 ":-D_REENTRANT} " \
		 "%{" SPEC_PTHREAD2 ":-U_REENTRANT} " \
		 "%{mcrtdll=crtdll*:-U__MSVCRT__ -D__CRTDLL__} " \
		 "%{mcrtdll=msvcrt10*:-D__MSVCRT_VERSION__=0x100} " \
		 "%{mcrtdll=msvcrt20*:-D__MSVCRT_VERSION__=0x200} " \
		 "%{mcrtdll=msvcrt40*:-D__MSVCRT_VERSION__=0x400} " \
		 "%{mcrtdll=msvcr40*:-D__MSVCRT_VERSION__=0x400} " \
		 "%{mcrtdll=msvcrtd*:-D__MSVCRT_VERSION__=0x600} " \
		 "%{mcrtdll=msvcrt-os*:-D__MSVCRT_VERSION__=0x700} " \
		 "%{mcrtdll=msvcr70*:-D__MSVCRT_VERSION__=0x700} " \
		 "%{mcrtdll=msvcr71*:-D__MSVCRT_VERSION__=0x701} " \
		 "%{mcrtdll=msvcr80*:-D__MSVCRT_VERSION__=0x800} " \
		 "%{mcrtdll=msvcr90*:-D__MSVCRT_VERSION__=0x900} " \
		 "%{mcrtdll=msvcr100*:-D__MSVCRT_VERSION__=0xA00} " \
		 "%{mcrtdll=msvcr110*:-D__MSVCRT_VERSION__=0xB00} " \
		 "%{mcrtdll=msvcr120*:-D__MSVCRT_VERSION__=0xC00} " \
		 "%{mcrtdll=ucrt*:-D_UCRT} "

#undef REAL_LIBGCC_SPEC
#define REAL_LIBGCC_SPEC \
  "%{mthreads:-lmingwthrd} -lmingw32 \
   " SHARED_LIBGCC_SPEC " \
   -lmingwex %{!mcrtdll=*:-lmsvcrt} %{mcrtdll=*:-l%*} \
   -lkernel32 " MCFGTHREAD_SPEC

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{shared|mdll:dllcrt2%O%s} \
  %{!shared:%{!mdll:%{!municode:crt2%O%s}}} \
  %{!shared:%{!mdll:%{municode:crt2u%O%s}}} \
  %{pg:gcrt2%O%s} \
  crtbegin.o%s \
  %{fvtable-verify=none:%s; \
    fvtable-verify=preinit:vtv_start.o%s; \
    fvtable-verify=std:vtv_start.o%s}"

/* Enable multilib.  */

#undef ASM_SPEC
#define ASM_SPEC "%{m32:--32} %{m64:--64}"

#undef LIB_SPEC
#define LIB_SPEC "%{pg:-lgmon} %{" SPEC_PTHREAD1 ":-lpthread} " \
		 "%{" SPEC_PTHREAD2 ": } " \
		 "%{mwindows:-lgdi32 -lcomdlg32} " \
     "%{fvtable-verify=preinit:-lvtv -lpsapi; \
        fvtable-verify=std:-lvtv -lpsapi} " \
		 "-ladvapi32 -lshell32 -luser32 -lkernel32"

#undef SPEC_32
#undef SPEC_64
#if TARGET_64BIT_DEFAULT
#define SPEC_32 "m32"
#define SPEC_64 "!m32"
#else
#define SPEC_32 "!m64"
#define SPEC_64 "m64"
#endif

#undef SUB_LINK_ENTRY32
#undef SUB_LINK_ENTRY64
#define SUB_LINK_ENTRY32 "-e _DllMainCRTStartup@12"
#if defined(USE_MINGW64_LEADING_UNDERSCORES)
#define SUB_LINK_ENTRY64 "-e _DllMainCRTStartup"
#else
#define SUB_LINK_ENTRY64 "-e DllMainCRTStartup"
#endif

#undef SUB_LINK_SPEC
#undef SUB_LINK_ENTRY
#define SUB_LINK_SPEC "%{" SPEC_64 ":-m i386pep} %{" SPEC_32 ":-m i386pe}"
#define SUB_LINK_ENTRY "%{" SPEC_64 ":" SUB_LINK_ENTRY64 "} %{" SPEC_32 ":" SUB_LINK_ENTRY32 "}"

#undef MULTILIB_DEFAULTS
#if TARGET_64BIT_DEFAULT
#define MULTILIB_DEFAULTS { "m64" }
#else
#define MULTILIB_DEFAULTS { "m32" }
#endif

#undef LINK_SPEC_LARGE_ADDR_AWARE
#if MINGW_DEFAULT_LARGE_ADDR_AWARE
# define LINK_SPEC_LARGE_ADDR_AWARE \
  "%{!shared:%{!mdll:%{" SPEC_32 ":--large-address-aware}}}"
#else
# define LINK_SPEC_LARGE_ADDR_AWARE ""
#endif

#undef LINK_SPEC_DISABLE_DYNAMICBASE
#if HAVE_LD_PE_DISABLE_DYNAMICBASE
# define LINK_SPEC_DISABLE_DYNAMICBASE \
  "%{!shared:%{!mdll:%{no-pie:--disable-dynamicbase}}}"
#else
# define LINK_SPEC_DISABLE_DYNAMICBASE ""
#endif

#undef LINK_SPEC
#define LINK_SPEC SUB_LINK_SPEC " %{mwindows:--subsystem windows} \
  %{mconsole:--subsystem console} \
  %{shared: %{mdll: %eshared and mdll are not compatible}} \
  %{shared: --shared} %{mdll:--dll} \
  %{static:-Bstatic} %{!static:-Bdynamic} \
  %{shared|mdll: " SUB_LINK_ENTRY " --enable-auto-image-base} \
  " LINK_SPEC_LARGE_ADDR_AWARE "\
  " LINK_SPEC_DISABLE_DYNAMICBASE "\
  %(shared_libgcc_undefs)"

/* Enable sincos optimization, overriding cygming.h.  sincos, sincosf
   and sincosl functions are available on mingw-w64, but not on the
   original mingw32.  */
#undef TARGET_LIBC_HAS_FUNCTION
#define TARGET_LIBC_HAS_FUNCTION gnu_libc_has_function
