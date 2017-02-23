/* Common hooks for RISC-V.
   Copyright (C) 2016 Free Software Foundation, Inc.

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
#include "flags.h"
#include "diagnostic-core.h"

/* Parse a RISC-V ISA string into an option mask.  */

static void
riscv_parse_arch_string (const char *isa, int *flags, location_t loc)
{
  const char *p = isa;

  if (strncmp (p, "rv32", 4) == 0)
    *flags &= ~MASK_64BIT, p += 4;
  else if (strncmp (p, "rv64", 4) == 0)
    *flags |= MASK_64BIT, p += 4;
  else
    {
      error_at (loc, "-march=%s: ISA string must begin with rv32 or rv64", isa);
      return;
    }

  if (*p == 'g')
    {
      p++;

      *flags |= MASK_MUL;
      *flags |= MASK_ATOMIC;
      *flags |= MASK_HARD_FLOAT;
      *flags |= MASK_DOUBLE_FLOAT;
    }
  else if (*p == 'i')
    {
      p++;

      *flags &= ~MASK_MUL;
      if (*p == 'm')
	*flags |= MASK_MUL, p++;

      *flags &= ~MASK_ATOMIC;
      if (*p == 'a')
	*flags |= MASK_ATOMIC, p++;

      *flags &= ~(MASK_HARD_FLOAT | MASK_DOUBLE_FLOAT);
      if (*p == 'f')
	{
	  *flags |= MASK_HARD_FLOAT, p++;

	  if (*p == 'd')
	    {
	      *flags |= MASK_DOUBLE_FLOAT;
	      p++;
	    }
	}
    }
  else
    {
      error_at (loc, "-march=%s: invalid ISA string", isa);
      return;
    }

  *flags &= ~MASK_RVC;
  if (*p == 'c')
    *flags |= MASK_RVC, p++;

  if (*p)
    {
      error_at (loc, "-march=%s: unsupported ISA substring %qs", isa, p);
      return;
    }
}

/* Implement TARGET_HANDLE_OPTION.  */

static bool
riscv_handle_option (struct gcc_options *opts,
		     struct gcc_options *opts_set ATTRIBUTE_UNUSED,
		     const struct cl_decoded_option *decoded,
		     location_t loc)
{
  switch (decoded->opt_index)
    {
    case OPT_march_:
      riscv_parse_arch_string (decoded->arg, &opts->x_target_flags, loc);
      return true;

    default:
      return true;
    }
}

/* Implement TARGET_OPTION_OPTIMIZATION_TABLE.  */
static const struct default_options riscv_option_optimization_table[] =
  {
    { OPT_LEVELS_1_PLUS, OPT_fsection_anchors, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fomit_frame_pointer, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_free, NULL, 1 },
    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

#undef TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE riscv_option_optimization_table

#undef TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION riscv_handle_option

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
