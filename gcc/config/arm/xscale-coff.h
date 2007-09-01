/* Definitions for XScale systems using COFF
   Copyright (C) 1999, 2000, 2001, 2007 Free Software Foundation, Inc.
   Contributed by Catherine Moore <clm@cygnus.com>

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

/* Run-time Target Specification.  */
#undef  SUBTARGET_CPU_DEFAULT
#define SUBTARGET_CPU_DEFAULT 		TARGET_CPU_xscale

#undef  SUBTARGET_EXTRA_ASM_SPEC
#define SUBTARGET_EXTRA_ASM_SPEC "%{!mcpu=*:-mcpu=xscale}"

#undef  MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS \
  { "mlittle-endian", "mno-thumb-interwork", "marm" }

#undef  TARGET_VERSION
#define TARGET_VERSION	fputs (" (XScale/COFF)", stderr);
