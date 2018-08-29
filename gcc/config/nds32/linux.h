/* Definitions of target machine of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2014 Free Software Foundation, Inc.
   Contributed by Andes Technology Corporation.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */


/* ------------------------------------------------------------------------ */

#define TARGET_LINUX_ABI 1

#undef  SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef  PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#ifdef TARGET_DEFAULT_TLSDESC_TRAMPOLINE
  #define NDS32_TLSDESC_TRAMPOLINE_SPEC \
    " %{!mno-tlsdesc-trampoline:--mtlsdesc-trampoline}"
#else
  #define NDS32_TLSDESC_TRAMPOLINE_SPEC ""
#endif

#define TARGET_OS_CPP_BUILTINS()                \
  do                                            \
    {                                           \
      GNU_USER_TARGET_OS_CPP_BUILTINS();           \
    }                                           \
  while (0)

#define GLIBC_DYNAMIC_LINKER "/lib/ld.so.1"

/* In the configure stage we may use options --enable-default-relax,
   --enable-Os-default-ifc and --enable-Os-default-ex9.  They effect
   the default spec of passing --relax, --mifc, and --mex9 to linker.
   We use NDS32_RELAX_SPEC, NDS32_IFC_SPEC, and NDS32_EX9_SPEC
   so that we can customize them conveniently.  */
#define LINK_SPEC \
 " %{G*}" \
 " %{mbig-endian:-EB} %{mlittle-endian:-EL}" \
 " %{shared:-shared} \
  %{!shared: \
    %{!static: \
      %{rdynamic:-export-dynamic} \
      -dynamic-linker " GNU_USER_DYNAMIC_LINKER "} \
    %{static:-static}}" \
  NDS32_RELAX_SPEC \
  NDS32_TLSDESC_TRAMPOLINE_SPEC

#define LINK_PIE_SPEC "%{pie:%{!fno-pie:%{!fno-PIE:%{!static:-pie}}}} "

#define CPP_SPEC "%{pthread:-D_REENTRANT}"

/* The SYNC operations are implemented as library functions, not
   INSN patterns.  As a result, the HAVE defines for the patterns are
   not defined.  We need to define them to generate the corresponding
   __GCC_HAVE_SYNC_COMPARE_AND_SWAP_* and __GCC_ATOMIC_*_LOCK_FREE
   defines.
   Ref: https://sourceware.org/ml/libc-alpha/2014-09/msg00322.html  */
#define HAVE_sync_compare_and_swapqi 1
#define HAVE_sync_compare_and_swaphi 1
#define HAVE_sync_compare_and_swapsi 1
