/* Operating system specific defines to be used when targeting GCC for
   hosting on Windows 32/64 via mingw-w64 runtime, using GNU tools and
   the Windows API Library.
   Copyright (C) 2009,
   2009 Free Software Foundation, Inc.

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

/* Enable -municode feature.  */

#undef CPP_SPEC
#define CPP_SPEC "%{posix:-D_POSIX_SOURCE} %{mthreads:-D_MT} \
  %{municode:-DUNICODE}"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{shared|mdll:dllcrt2%O%s} \
  %{!shared:%{!mdll:%{!municode:crt2%O%s}}} \
  %{!shared:%{!mdll:%{municode:crt2u%O%s}}} \
  %{pg:gcrt2%O%s} \
  crtbegin.o%s"

/* Enable multilib.  */

#undef ASM_SPEC
#define ASM_SPEC "%{v:-V} %{n} %{T} %{Ym,*} %{Yd,*} \
 %{Wa,*:%*} %{m32:--32} %{m64:--64}"

#if TARGET_64BIT_DEFAULT
#define SPEC_32 "m32"
#define SPEC_64 "!m32"
#else
#define SPEC_32 "!m64"
#define SPEC_64 "m64"
#endif

#define SUB_LINK_SPEC "%{" SPEC_64 ":-m i386pep} %{" SPEC_32 ":-m i386pe}"

#if TARGET_64BIT_DEFAULT
#define MULTILIB_DEFAULTS { "m64" }
#else
#define MULTILIB_DEFAULTS { "m32" }
#endif

#undef LINK_SPEC
#define LINK_SPEC SUB_LINK_SPEC "%{mwindows:--subsystem windows} \
  %{mconsole:--subsystem console} \
  %{shared: %{mdll: %eshared and mdll are not compatible}} \
  %{shared: --shared} %{mdll:--dll} \
  %{static:-Bstatic} %{!static:-Bdynamic} \
  %{shared|mdll: -e _DllMainCRTStartup@12 --enable-auto-image-base} \
  %(shared_libgcc_undefs)"
