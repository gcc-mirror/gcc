/* Target definitions for x86_64 running Darwin with a 64b host supporting a
   32b multilib.
   Copyright (C) 2006-2019 Free Software Foundation, Inc.
   Contributed by Apple Computer Inc.

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

#undef  DARWIN_ARCH_SPEC
#define DARWIN_ARCH_SPEC "%{m32:i386;:x86_64}"

/* WORKAROUND pr80556:
   For x86_64 Darwin10 and later, the unwinder is in libunwind (redirected
   from libSystem).  This doesn't use the keymgr (see keymgr.c) and therefore
   the calls that libgcc makes to obtain the KEYMGR_GCC3_DW2_OBJ_LIST are not
   updated to include new images, and might not even be valid for a single
   image.
   Therefore, for 64b exes at least, we must use the libunwind implementation,
   even when static-libgcc is specified.  We put libSystem first so that
   unwinder symbols are satisfied from there. */
#undef REAL_LIBGCC_SPEC
#define REAL_LIBGCC_SPEC						   \
   "%{static-libgcc|static: 						   \
      %{!m32:%:version-compare(>= 10.6 mmacosx-version-min= -lSystem)}	   \
        -lgcc_eh -lgcc;							   \
      shared-libgcc|fexceptions|fgnu-runtime:				   \
       %:version-compare(!> 10.5 mmacosx-version-min= -lgcc_s.10.4)	   \
       %:version-compare(>< 10.5 10.6 mmacosx-version-min= -lgcc_s.10.5)   \
       %:version-compare(!> 10.5 mmacosx-version-min= -lgcc_ext.10.4)	   \
       %:version-compare(>= 10.5 mmacosx-version-min= -lgcc_ext.10.5)	   \
       -lgcc ;								   \
      :%:version-compare(>< 10.3.9 10.5 mmacosx-version-min= -lgcc_s.10.4) \
       %:version-compare(>< 10.5 10.6 mmacosx-version-min= -lgcc_s.10.5)   \
       %:version-compare(!> 10.5 mmacosx-version-min= -lgcc_ext.10.4)	   \
       %:version-compare(>= 10.5 mmacosx-version-min= -lgcc_ext.10.5)	   \
       -lgcc }"

#undef  DARWIN_SUBARCH_SPEC
#define DARWIN_SUBARCH_SPEC DARWIN_ARCH_SPEC

#undef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS                                   \
  DARWIN_EXTRA_SPECS                                            \
  { "darwin_arch", DARWIN_ARCH_SPEC },                          \
  { "darwin_crt2", "" },                                        \
  { "darwin_subarch", DARWIN_SUBARCH_SPEC },
