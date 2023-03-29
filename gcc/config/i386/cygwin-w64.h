/* Operating system specific defines to be used when targeting GCC for
   hosting on Windows 32/64 via Cygwin runtime, using GNU tools and
   the Windows API Library.
   Copyright (C) 2013-2023 Free Software Foundation, Inc.

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

/* Enable multilib.  */

#undef ASM_SPEC
#define ASM_SPEC "%{m32:--32} %{m64:--64}"

/* To implement C++ function replacement we always wrap the cxx
   malloc-like operators.  See N2800 #17.6.4.6 [replacement.functions] */
#undef CXX_WRAP_SPEC_LIST
#define CXX_WRAP_SPEC_LIST " \
  --wrap _Znwm \
  --wrap _Znam \
  --wrap _ZdlPv \
  --wrap _ZdaPv \
  --wrap _ZnwmRKSt9nothrow_t \
  --wrap _ZnamRKSt9nothrow_t \
  --wrap _ZdlPvRKSt9nothrow_t \
  --wrap _ZdaPvRKSt9nothrow_t \
"

#undef SPEC_32
#undef SPEC_64
#define SPEC_32 "m32"
#define SPEC_64 "!m32"

#undef SUB_LINK_ENTRY32
#undef SUB_LINK_ENTRY64
#define SUB_LINK_ENTRY32 "-e __cygwin_dll_entry@12"
#define SUB_LINK_ENTRY64 "-e _cygwin_dll_entry"

#undef SUB_LINK_SPEC
#undef SUB_LINK_ENTRY
#define SUB_LINK_SPEC "%{" SPEC_64 ":-m i386pep} %{" SPEC_32 ":-m i386pe}"
#define SUB_LINK_ENTRY "%{" SPEC_64 ":" SUB_LINK_ENTRY64 "} %{" SPEC_32 ":" SUB_LINK_ENTRY32 "}"

#undef MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS { "m64" }

#undef LINK_SPEC
#define LINK_SPEC SUB_LINK_SPEC "\
  %{mwindows:--subsystem windows} \
  %{mconsole:--subsystem console} \
  " CXX_WRAP_SPEC " \
  %{shared: %{mdll: %eshared and mdll are not compatible}} \
  %{shared: --shared} %{mdll:--dll} \
  %{static:-Bstatic} %{!static:-Bdynamic} \
  %{shared|mdll: " SUB_LINK_ENTRY " --enable-auto-image-base} \
  %(shared_libgcc_undefs) \
  --dll-search-prefix=cyg \
  %{!shared: %{!mdll: %{" SPEC_32 ":--large-address-aware} --tsaware}}"

/* Cygwin64 will have a 64-bit long type. */
#undef LONG_TYPE_SIZE
#undef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE (TARGET_64BIT ? 64 : 32)

/* Override default "long long unsigned int" from cygming.h. */
#undef SIZE_TYPE
#define SIZE_TYPE (TARGET_64BIT ? "long unsigned int" : "unsigned int")
#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE (TARGET_64BIT ? "long int" : "int")

#undef LIBGCC_SONAME
#define LIBGCC_SONAME "cyggcc_s-seh-1.dll"
