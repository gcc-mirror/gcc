/* Target definitions for Darwin (Mac OS X) systems.
   Copyright (C) 2009-2018 Free Software Foundation, Inc.
   Contributed by Jack Howarth <howarth@bromo.med.uc.edu>.

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

/* Fix PR41260 by passing -no_compact_unwind on darwin10 and later until
   unwinder in libSystem is fixed to digest new epilog unwinding notes.

   Fix PR47558 by linking against libSystem ahead of libgcc_ext. */
#undef  LINK_GCC_C_SEQUENCE_SPEC
#define LINK_GCC_C_SEQUENCE_SPEC \
"%:version-compare(>= 10.6 mmacosx-version-min= -no_compact_unwind) \
 %{!static:%{!static-libgcc: \
    %:version-compare(>= 10.6 mmacosx-version-min= -lSystem) } } \
 %{fno-pic|fno-PIC|fno-pie|fno-PIE|fapple-kext|mkernel|static|mdynamic-no-pic: \
   %:version-compare(>= 10.7 mmacosx-version-min= -no_pie) } \
 %{!nostdlib:%:version-compare(>< 10.6 10.7 mmacosx-version-min= -ld10-uwfef.o)} \
  %G %{!nolibc:%L}"

#undef DEF_MIN_OSX_VERSION
#define DEF_MIN_OSX_VERSION "10.6"

#ifndef LD64_VERSION
#undef DEF_LD64
#define DEF_LD64 "97.7"
#endif
