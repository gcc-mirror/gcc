/* Core target definitions for GNU compiler
   for PowerPC embedded targeted systems with SPE support.
   Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2011
   Free Software Foundation, Inc.
   Contributed by Aldy Hernandez (aldyh@redhat.com).

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

#undef  TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_POWERPC | MASK_NEW_MNEMONICS | MASK_EABI	\
  | MASK_STRICT_ALIGN)

#undef  SUBSUBTARGET_OVERRIDE_OPTIONS
#define SUBSUBTARGET_OVERRIDE_OPTIONS \
  if (!global_options_set.x_rs6000_cpu_index) \
    rs6000_cpu = PROCESSOR_PPC8540; \
  if (!global_options_set.x_rs6000_spe_abi) \
    rs6000_spe_abi = 1; \
  if (!global_options_set.x_rs6000_float_gprs) \
    rs6000_float_gprs = 1; \
  if (!global_options_set.x_rs6000_spe) \
    rs6000_spe = 1; \
  if (target_flags & MASK_64BIT) \
    error ("-m64 not supported in this configuration")

#undef  ASM_DEFAULT_SPEC
#define	ASM_DEFAULT_SPEC "-mppc -mspe -me500"
