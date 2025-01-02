/* Machine description for AArch64 architecture.
   Copyright (C) 2009-2025 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_AARCH64_ERRATA_H
#define GCC_AARCH64_ERRATA_H

/* Completely ignore the option if we've explicitly specify something other than
   mcpu=cortex-a53 or march=armv8-a.  */
#define TARGET_SUPPRESS_OPT_SPEC(OPT) \
    "mcpu=*:%{%:is_local_not_armv8_base(%{mcpu=*:%*}):; " OPT \
    "}; march=*:%{%:is_local_not_armv8_base(%{march=*:%*}):;" OPT "}; " OPT

/* Explicitly turn off the option if we've explicitly specify something other
   than mcpu=cortex-a53 or march=armv8-a.  This will also erase any other usage
   of the flag making the order of the options not relevant.  */
# define TARGET_TURN_OFF_OPT_SPEC(FLAG)  \
   "mcpu=*:%{%:is_local_not_armv8_base(%{mcpu=*:%*}):%<m" FLAG " -mno-" FLAG \
   "}; march=*:%{%:is_local_not_armv8_base(%{march=*:%*}):%<m" FLAG " -mno-" FLAG "}"

/* Cortex-A53 835769 Errata.  */

#if TARGET_FIX_ERR_A53_835769_DEFAULT
#define CA53_ERR_835769_SPEC     \
  " %{" \
  TARGET_SUPPRESS_OPT_SPEC ("!mno-fix-cortex-a53-835769:--fix-cortex-a53-835769") \
  " }"
#else
#define CA53_ERR_835769_SPEC     \
  " %{" \
  TARGET_SUPPRESS_OPT_SPEC ("mfix-cortex-a53-835769:--fix-cortex-a53-835769") \
  " }"
#endif

#define CA53_ERR_835769_COMPILE_SPEC \
  " %{" TARGET_TURN_OFF_OPT_SPEC ("fix-cortex-a53-835769") "}"

/* Cortex-A53 843419 Errata.  */

#if TARGET_FIX_ERR_A53_843419_DEFAULT
#define CA53_ERR_843419_SPEC     \
  " %{" \
  TARGET_SUPPRESS_OPT_SPEC ("!mno-fix-cortex-a53-843419:--fix-cortex-a53-843419") \
  " }"
#else
#define CA53_ERR_843419_SPEC     \
  " %{" \
  TARGET_SUPPRESS_OPT_SPEC ("mfix-cortex-a53-843419:--fix-cortex-a53-843419") \
  " }"
#endif

#define CA53_ERR_843419_COMPILE_SPEC \
  " %{" TARGET_TURN_OFF_OPT_SPEC ("fix-cortex-a53-843419") "}"

/* Exports to use in SPEC files.  */

#define AARCH64_ERRATA_LINK_SPEC		\
  CA53_ERR_835769_SPEC				\
  CA53_ERR_843419_SPEC

#define AARCH64_ERRATA_COMPILE_SPEC		\
  CA53_ERR_835769_COMPILE_SPEC			\
  CA53_ERR_843419_COMPILE_SPEC

#endif /*  GCC_AARCH64_ERRATA_H */
