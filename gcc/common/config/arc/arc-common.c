/* Common hooks for Synopsys DesignWare ARC
   Copyright (C) 1994-2016 Free Software Foundation, Inc.
   Contributor: Joern Rennecke <joern.rennecke@embecosm.com>
		on behalf of Synopsys Inc.

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
#include "diagnostic-core.h"
#include "tm.h"
#include "common/common-target.h"
#include "opts.h"
#include "flags.h"

static void
arc_option_init_struct (struct gcc_options *opts)
{
  opts->x_flag_no_common = 255; /* Mark as not user-initialized.  */

  /* Which cpu we're compiling for (ARC600, ARC601, ARC700, ARCv2).  */
  arc_cpu = PROCESSOR_NONE;
}

/* Set default optimization options.  */
/* The conditions are incomplete, so we rely on the evaluation order here,
   which goes from first to last, i.e. the last match prevails.  */
/* ??? But this trick only works for reject_negative options.  Approximate
   missing option combination.  */
#define OPT_LEVELS_3_PLUS_SPEED_ONLY OPT_LEVELS_3_PLUS
static const struct default_options arc_option_optimization_table[] =
  {
    { OPT_LEVELS_1_PLUS, OPT_fomit_frame_pointer, NULL, 1 },
    { OPT_LEVELS_ALL, OPT_mRcq, NULL, 1 },
    { OPT_LEVELS_ALL, OPT_mRcw, NULL, 1 },
    { OPT_LEVELS_ALL, OPT_msize_level_, NULL, 1 },
    { OPT_LEVELS_3_PLUS_SPEED_ONLY, OPT_msize_level_, NULL, 0 },
    { OPT_LEVELS_SIZE, OPT_msize_level_, NULL, 3 },
    { OPT_LEVELS_3_PLUS_SPEED_ONLY, OPT_malign_call, NULL, 1 },
    { OPT_LEVELS_ALL, OPT_mearly_cbranchsi, NULL, 1 },
    { OPT_LEVELS_ALL, OPT_mbbit_peephole, NULL, 1 },
    { OPT_LEVELS_SIZE, OPT_mq_class, NULL, 1 },
    { OPT_LEVELS_SIZE, OPT_mcase_vector_pcrel, NULL, 1 },
    { OPT_LEVELS_SIZE, OPT_mcompact_casesi, NULL, 1 },
    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

/*  Process options.  */
static bool
arc_handle_option (struct gcc_options *opts, struct gcc_options *opts_set,
		   const struct cl_decoded_option *decoded,
		   location_t loc)
{
  size_t code = decoded->opt_index;
  int value = decoded->value;
  const char *arg = decoded->arg;

  switch (code)
    {
      static int mcpu_seen = PROCESSOR_NONE;
    case OPT_mcpu_:
      /* N.B., at this point arc_cpu has already been set to its new value by
	 our caller, so comparing arc_cpu with PROCESSOR_NONE is pointless.  */

      if (mcpu_seen != PROCESSOR_NONE && mcpu_seen != value)
	warning_at (loc, 0, "multiple -mcpu= options specified.");
      mcpu_seen = value;

      switch (value)
	{
	case PROCESSOR_NPS400:
	  if (! (opts_set->x_TARGET_CASE_VECTOR_PC_RELATIVE) )
	    opts->x_TARGET_CASE_VECTOR_PC_RELATIVE = 1;
	  /* Fall through */
	case PROCESSOR_ARC600:
	case PROCESSOR_ARC700:
	  if (! (opts_set->x_target_flags & MASK_BARREL_SHIFTER) )
	    opts->x_target_flags |= MASK_BARREL_SHIFTER;
	  break;
	case PROCESSOR_ARC601:
	  if (! (opts_set->x_target_flags & MASK_BARREL_SHIFTER) )
	    opts->x_target_flags &= ~MASK_BARREL_SHIFTER;
	  break;
	case PROCESSOR_ARCHS:
	  if ( !(opts_set->x_target_flags & MASK_BARREL_SHIFTER))
	    opts->x_target_flags |= MASK_BARREL_SHIFTER;  /* Default: on.  */
	  if ( !(opts_set->x_target_flags & MASK_CODE_DENSITY))
	    opts->x_target_flags |= MASK_CODE_DENSITY;	  /* Default: on.  */
	  if ( !(opts_set->x_target_flags & MASK_NORM_SET))
	    opts->x_target_flags |= MASK_NORM_SET;	  /* Default: on.  */
	  if ( !(opts_set->x_target_flags & MASK_SWAP_SET))
	    opts->x_target_flags |= MASK_SWAP_SET;	  /* Default: on.  */
	  if ( !(opts_set->x_target_flags & MASK_DIVREM))
	    opts->x_target_flags |= MASK_DIVREM;	  /* Default: on.  */
	  break;

	case PROCESSOR_ARCEM:
	  if ( !(opts_set->x_target_flags & MASK_BARREL_SHIFTER))
	    opts->x_target_flags |= MASK_BARREL_SHIFTER;  /* Default: on.  */
	  if ( !(opts_set->x_target_flags & MASK_CODE_DENSITY))
	    opts->x_target_flags &= ~MASK_CODE_DENSITY;	  /* Default: off.  */
	  if ( !(opts_set->x_target_flags & MASK_NORM_SET))
	    opts->x_target_flags &= ~MASK_NORM_SET;	  /* Default: off.  */
	  if ( !(opts_set->x_target_flags & MASK_SWAP_SET))
	    opts->x_target_flags &= ~MASK_SWAP_SET;	  /* Default: off.  */
	  if ( !(opts_set->x_target_flags & MASK_DIVREM))
	    opts->x_target_flags &= ~MASK_DIVREM;	  /* Default: off.  */
	  break;
	default:
	  gcc_unreachable ();
	}
      break;

    case OPT_mmpy_option_:
      if (value < 0 || value > 9)
	error_at (loc, "bad value %qs for -mmpy-option switch", arg);
      break;
    }

  return true;
}

#define TARGET_OPTION_INIT_STRUCT arc_option_init_struct
#define TARGET_OPTION_OPTIMIZATION_TABLE arc_option_optimization_table
#define TARGET_HANDLE_OPTION arc_handle_option

#define DEFAULT_NO_SDATA (TARGET_SDATA_DEFAULT ? 0 : MASK_NO_SDATA_SET)

/* We default to ARC700, which has the barrel shifter enabled.  */
#define TARGET_DEFAULT_TARGET_FLAGS \
  (MASK_BARREL_SHIFTER|MASK_VOLATILE_CACHE_SET|DEFAULT_NO_SDATA)


#include "common/common-target-def.h"

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
