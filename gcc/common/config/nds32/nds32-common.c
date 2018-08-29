/* Common hooks of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2018 Free Software Foundation, Inc.
   Contributed by Andes Technology Corporation.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "diagnostic-core.h"
#include "tm.h"
#include "common/common-target.h"
#include "common/common-target-def.h"
#include "opts.h"
#include "flags.h"

/* ------------------------------------------------------------------------ */

/* Implement TARGET_HANDLE_OPTION.  */
static bool
nds32_handle_option (struct gcc_options *opts ATTRIBUTE_UNUSED,
		     struct gcc_options *opts_set ATTRIBUTE_UNUSED,
		     const struct cl_decoded_option *decoded,
		     location_t loc)
{
  size_t     code  = decoded->opt_index;
  int        value = decoded->value;

  switch (code)
    {
    case OPT_misr_vector_size_:
      /* Check the valid vector size: 4 or 16.  */
      if (value != 4 && value != 16)
	{
	  error_at (loc, "for the option -misr-vector-size=X, the valid X "
			 "must be: 4 or 16");
	  return false;
	}

      return true;

    case OPT_misr_secure_:
      /* Check the valid security level: 0 1 2 3.  */
      if (value < 0 || value > 3)
	{
	  error_at (loc, "for the option -misr-secure=X, the valid X "
			 "must be: 0, 1, 2, or 3");
	  return false;
	}
      return true;

    case OPT_mcache_block_size_:
      /* Check valid value: 4 8 16 32 64 128 256 512.  */
      if (exact_log2 (value) < 2 || exact_log2 (value) > 9)
	{
	  error_at (loc, "for the option -mcache-block-size=X, the valid X "
			 "must be: 4, 8, 16, 32, 64, 128, 256, or 512");
	  return false;
	}

      return true;

    default:
      return true;
    }
}

/* ------------------------------------------------------------------------ */

/* Implement TARGET_OPTION_OPTIMIZATION_TABLE.  */
static const struct default_options nds32_option_optimization_table[] =
{
#if TARGET_LINUX_ABI == 0
  /* Disable -fdelete-null-pointer-checks by default in ELF toolchain.  */
  { OPT_LEVELS_ALL,               OPT_fdelete_null_pointer_checks,
							   NULL, 0 },
#endif
  /* Enable -fsched-pressure by default at -O1 and above.  */
  { OPT_LEVELS_1_PLUS,            OPT_fsched_pressure,     NULL, 1 },
  /* Enable -fomit-frame-pointer by default at all optimization levels.  */
  { OPT_LEVELS_ALL,               OPT_fomit_frame_pointer, NULL, 1 },
  /* Enable -mrelax-hint by default at all optimization levels.  */
  { OPT_LEVELS_ALL,               OPT_mrelax_hint,         NULL, 1 },
  /* Enalbe -malways-align by default at -O1 and above, but not -Os or -Og.  */
  { OPT_LEVELS_1_PLUS_SPEED_ONLY, OPT_malways_align,       NULL, 1 },
  /* Enable -mv3push by default at -Os, but it is useless under V2 ISA.  */
  { OPT_LEVELS_SIZE,              OPT_mv3push,             NULL, 1 },

  { OPT_LEVELS_NONE,              0,                       NULL, 0 }
};

/* ------------------------------------------------------------------------ */

/* Implement TARGET_EXCEPT_UNWIND_INFO.  */
static enum unwind_info_type
nds32_except_unwind_info (struct gcc_options *opts ATTRIBUTE_UNUSED)
{
  if (TARGET_LINUX_ABI)
    return UI_DWARF2;

  return UI_SJLJ;
}

/* ------------------------------------------------------------------------ */


/* Run-time Target Specification.  */

/* The default target flags consist of
   TARGET_CPU_DEFAULT and other MASK_XXX flags.

   The value of TARGET_CPU_DEFAULT is set by
   the process of 'configure' and 'make' stage.
   Please check gcc/config.gcc for more implementation detail.

   Other MASK_XXX flags are set individually.
   By default we enable
     TARGET_16_BIT     : Generate 16/32 bit mixed length instruction.
     TARGET_EXT_PERF   : Generate performance extention instrcution.
     TARGET_EXT_PERF2  : Generate performance extention version 2 instrcution.
     TARGET_EXT_STRING : Generate string extention instrcution.
     TARGET_HW_ABS     : Generate hardware abs instruction.
     TARGET_CMOV       : Generate conditional move instruction.  */
#undef TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS		\
  (TARGET_CPU_DEFAULT				\
   | TARGET_DEFAULT_FPU_ISA			\
   | TARGET_DEFAULT_FPU_FMA			\
   | MASK_16_BIT				\
   | MASK_EXT_PERF				\
   | MASK_EXT_PERF2				\
   | MASK_EXT_STRING				\
   | MASK_HW_ABS				\
   | MASK_CMOV)

#undef TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION nds32_handle_option

#undef TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE nds32_option_optimization_table


/* Defining the Output Assembler Language.  */

#undef TARGET_EXCEPT_UNWIND_INFO
#define TARGET_EXCEPT_UNWIND_INFO nds32_except_unwind_info

/* ------------------------------------------------------------------------ */

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;

/* ------------------------------------------------------------------------ */
