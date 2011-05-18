/* Definitions for option handling for MIPS.
   Copyright (C) 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998
   1999, 2000, 2001, 2002, 2003, 2004, 2005, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.

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

#ifndef MIPS_OPTS_H
#define MIPS_OPTS_H

/* Enumerates the setting of the -mcode-readable option.  */
enum mips_code_readable_setting {
  CODE_READABLE_NO,
  CODE_READABLE_PCREL,
  CODE_READABLE_YES
};

/* Enumerates the setting of the -mr10k-cache-barrier option.  */
enum mips_r10k_cache_barrier_setting {
  R10K_CACHE_BARRIER_NONE,
  R10K_CACHE_BARRIER_STORE,
  R10K_CACHE_BARRIER_LOAD_STORE
};

/* No enumeration is defined to index the -march= values (entries in
   mips_cpu_info_table), with the type int being used instead, but we
   need to distinguish the special "from-abi" and "native" values.  */
#define MIPS_ARCH_OPTION_FROM_ABI -1
#define MIPS_ARCH_OPTION_NATIVE -2

#endif
