/* Common hooks for IBM S/390 and zSeries.
   Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006,
   2007, 2008, 2009, 2010, 2011 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "diagnostic-core.h"
#include "tm.h"
#include "common/common-target.h"
#include "common/common-target-def.h"
#include "opts.h"
#include "flags.h"

EXPORTED_CONST int processor_flags_table[] =
  {
    /* g5 */     PF_IEEE_FLOAT,
    /* g6 */     PF_IEEE_FLOAT,
    /* z900 */   PF_IEEE_FLOAT | PF_ZARCH,
    /* z990 */   PF_IEEE_FLOAT | PF_ZARCH | PF_LONG_DISPLACEMENT,
    /* z9-109 */ PF_IEEE_FLOAT | PF_ZARCH | PF_LONG_DISPLACEMENT
                 | PF_EXTIMM,
    /* z9-ec */  PF_IEEE_FLOAT | PF_ZARCH | PF_LONG_DISPLACEMENT
                 | PF_EXTIMM | PF_DFP,
    /* z10 */    PF_IEEE_FLOAT | PF_ZARCH | PF_LONG_DISPLACEMENT
                 | PF_EXTIMM | PF_DFP | PF_Z10,
    /* z196 */   PF_IEEE_FLOAT | PF_ZARCH | PF_LONG_DISPLACEMENT
                 | PF_EXTIMM | PF_DFP | PF_Z10 | PF_Z196
  };

/* Change optimizations to be performed, depending on the
   optimization level.  */

static const struct default_options s390_option_optimization_table[] =
  {
    { OPT_LEVELS_1_PLUS, OPT_fomit_frame_pointer, NULL, 1 },

    /* Enable -fsched-pressure by default when optimizing.  */
    { OPT_LEVELS_1_PLUS, OPT_fsched_pressure, NULL, 1 },

    /* ??? There are apparently still problems with -fcaller-saves.  */
    { OPT_LEVELS_ALL, OPT_fcaller_saves, NULL, 0 },

    /* Use MVCLE instructions to decrease code size if requested.  */
    { OPT_LEVELS_SIZE, OPT_mmvcle, NULL, 1 },

    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

/* Implement TARGET_OPTION_INIT_STRUCT.  */

static void
s390_option_init_struct (struct gcc_options *opts)
{
  /* By default, always emit DWARF-2 unwind info.  This allows debugging
     without maintaining a stack frame back-chain.  */
  opts->x_flag_asynchronous_unwind_tables = 1;
}

/* Implement TARGET_HANDLE_OPTION.  */

static bool
s390_handle_option (struct gcc_options *opts,
		    struct gcc_options *opts_set ATTRIBUTE_UNUSED,
  		    const struct cl_decoded_option *decoded,
		    location_t loc)
{
  size_t code = decoded->opt_index;
  const char *arg = decoded->arg;
  int value = decoded->value;

  switch (code)
    {
    case OPT_march_:
      opts->x_s390_arch_flags = processor_flags_table[value];
      opts->x_s390_arch_string = arg;
      return true;

    case OPT_mstack_guard_:
      if (exact_log2 (value) == -1)
	error_at (loc, "stack guard value must be an exact power of 2");
      return true;

    case OPT_mstack_size_:
      if (exact_log2 (value) == -1)
	error_at (loc, "stack size must be an exact power of 2");
      return true;

    case OPT_mtune_:
      opts->x_s390_tune_flags = processor_flags_table[value];
      return true;

    case OPT_mwarn_framesize_:
      return sscanf (arg, HOST_WIDE_INT_PRINT_DEC,
		     &opts->x_s390_warn_framesize) == 1;

    default:
      return true;
    }
}

#undef TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS (TARGET_DEFAULT)

#undef TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION s390_handle_option

#undef TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE s390_option_optimization_table

#undef TARGET_OPTION_INIT_STRUCT
#define TARGET_OPTION_INIT_STRUCT s390_option_init_struct

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
