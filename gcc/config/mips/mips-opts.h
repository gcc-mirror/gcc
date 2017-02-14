/* Definitions for option handling for MIPS.
   Copyright (C) 1989-2017 Free Software Foundation, Inc.

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

/* Enumerates the setting of the -mabs and -mnan options.  */
enum mips_ieee_754_setting {
  MIPS_IEEE_754_DEFAULT,
  MIPS_IEEE_754_LEGACY,
  MIPS_IEEE_754_2008
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

/* Enumerates the setting of the -mcompact-branches= option.  */
enum mips_cb_setting {
  MIPS_CB_NEVER,
  MIPS_CB_OPTIMAL,
  MIPS_CB_ALWAYS
};
#endif
