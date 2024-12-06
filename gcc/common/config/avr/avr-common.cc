/* Common hooks for AVR 8-bit microcontrollers.
   Copyright (C) 1998-2024 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "common/common-target.h"
#include "common/common-target-def.h"
#include "opts.h"
#include "diagnostic.h"

/* Implement TARGET_OPTION_OPTIMIZATION_TABLE.  */
static const struct default_options avr_option_optimization_table[] =
  {
    // The only effect of -fcaller-saves might be that it triggers
    // a frame without need when it tries to be smart around calls.
    { OPT_LEVELS_ALL, OPT_fcaller_saves, NULL, 0 },
    // Avoid large lookup tables in RAM from -foptimize-crc.
    { OPT_LEVELS_ALL, OPT_foptimize_crc, NULL, 0 },
    { OPT_LEVELS_1_PLUS_NOT_DEBUG, OPT_mgas_isr_prologues, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_mmain_is_OS_task, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_mfuse_add_, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_mfuse_add_, NULL, 2 },
    { OPT_LEVELS_1_PLUS_NOT_DEBUG, OPT_mfuse_move_, NULL, 3 },
    { OPT_LEVELS_2_PLUS, OPT_mfuse_move_, NULL, 23 },
    { OPT_LEVELS_2_PLUS, OPT_msplit_bit_shift, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_msplit_ldst, NULL, 1 },
    // Stick to the "old" placement of the subreg lowering pass.
    { OPT_LEVELS_1_PLUS, OPT_fsplit_wide_types_early, NULL, 1 },
    /* Allow optimizer to introduce store data races. This used to be the
       default -- it was changed because bigger targets did not see any
       performance decrease. For the AVR though, disallowing data races
       introduces additional code in LIM and increases reg pressure.  */
    { OPT_LEVELS_ALL, OPT_fallow_store_data_races, NULL, 1 },

#if defined (WITH_DOUBLE64)
    { OPT_LEVELS_ALL, OPT_mdouble_, NULL, 64 },
#elif defined (WITH_DOUBLE32)
    { OPT_LEVELS_ALL, OPT_mdouble_, NULL, 32 },
#else
#error "align this with config.gcc"
#endif

#if defined (WITH_LONG_DOUBLE64)
    { OPT_LEVELS_ALL, OPT_mlong_double_, NULL, 64 },
#elif defined (WITH_LONG_DOUBLE32)
    { OPT_LEVELS_ALL, OPT_mlong_double_, NULL, 32 },
#else
#error "align this with config.gcc"
#endif

    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };


/* Implement `TARGET_HANDLE_OPTION'.  */

/* This is the same logic that driver-avr.cc:avr_double_lib() applies
   during DRIVER_SELF_SPECS, but this time we complain about -mdouble=
   and -mlong-double= that are not provided by --with-double= resp.
   --with-long-double=  */

static bool
avr_handle_option (struct gcc_options *opts, struct gcc_options*,
		   const struct cl_decoded_option *decoded,
		   location_t loc ATTRIBUTE_UNUSED)
{
  int value = decoded->value;

  switch (decoded->opt_index)
    {
    case OPT_mdouble_:
      if (value == 64)
	{
#if !defined (HAVE_DOUBLE64)
	  error_at (loc, "option %<-mdouble=64%> is only available if "
		    "configured %<--with-double={64|64,32|32,64}%>");
#endif
	  opts->x_avropt_long_double = 64;
	}
      else if (value == 32)
	{
#if !defined (HAVE_DOUBLE32)
	  error_at (loc, "option %<-mdouble=32%> is only available if "
		    "configured %<--with-double={32|32,64|64,32}%>");
#endif
	}
      else
	gcc_unreachable();

#if defined (HAVE_LONG_DOUBLE_IS_DOUBLE)
      opts->x_avropt_long_double = value;
#endif
      break; // -mdouble=

    case OPT_mlong_double_:
      if (value == 64)
	{
#if !defined (HAVE_LONG_DOUBLE64)
	  error_at (loc, "option %<-mlong-double=64%> is only available if "
		    "configured %<--with-long-double={64|64,32|32,64}%>, "
		    "or %<--with-long-double=double%> together with "
		    "%<--with-double={64|64,32|32,64}%>");
#endif
	}
      else if (value == 32)
	{
#if !defined (HAVE_LONG_DOUBLE32)
	  error_at (loc, "option %<-mlong-double=32%> is only available if "
		    "configured %<--with-long-double={32|32,64|64,32}%>, "
		    "or %<--with-long-double=double%> together with "
		    "%<--with-double={32|32,64|64,32}%>");
#endif
	  opts->x_avropt_double = 32;
	}
      else
	gcc_unreachable();

#if defined (HAVE_LONG_DOUBLE_IS_DOUBLE)
      opts->x_avropt_double = value;
#endif
      break; // -mlong-double=
    }

  return true;
}


#undef TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION avr_handle_option

#undef TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE avr_option_optimization_table

#undef TARGET_EXCEPT_UNWIND_INFO
#define TARGET_EXCEPT_UNWIND_INFO sjlj_except_unwind_info

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
