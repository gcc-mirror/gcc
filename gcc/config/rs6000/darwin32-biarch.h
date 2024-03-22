/* Target definitions for PowerPC running Darwin (Mac OS X) for a 32b host
   with a 64b miultilib.
   Copyright (C) 2019-2024 Free Software Foundation, Inc.

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

#undef DARWIN_ARCH_SPEC
#define DARWIN_ARCH_SPEC "%{m64:ppc64;:ppc}"

#undef DARWIN_SUBARCH_SPEC
#define DARWIN_SUBARCH_SPEC "			\
 %{m64: ppc64}					\
 %{!m64:					\
 %{mcpu=601:ppc601;				\
   mcpu=603:ppc603;				\
   mcpu=603e:ppc603;				\
   mcpu=604:ppc604;				\
   mcpu=604e:ppc604e;				\
   mcpu=740:ppc750;				\
   mcpu=750:ppc750;				\
   mcpu=G3:ppc750;				\
   mcpu=7400:ppc7400;				\
   mcpu=G4:ppc7400;				\
   mcpu=7450:ppc7450;				\
   mcpu=970:ppc970;				\
   mcpu=power4:ppc970;				\
   mcpu=G5:ppc970;				\
   :ppc}}"

#undef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS			\
  DARWIN_EXTRA_SPECS                            \
  { "darwin_arch", DARWIN_ARCH_SPEC },		\
  { "darwin_crt2", DARWIN_CRT2_SPEC },		\
  { "darwin_subarch", DARWIN_SUBARCH_SPEC },
