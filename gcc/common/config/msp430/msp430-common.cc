/* Common hooks for Texas Instruments MSP430.
   Copyright (C) 2014-2023 Free Software Foundation, Inc.

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
#include "common/common-target-def.h"
#include "opts.h"
#include "flags.h"

/* Check for generic -mmcu= names here.  If found then we
   convert to a baseline cpu name.  Otherwise we allow the option to
   be passed on to the backend where it can be checked more fully.  */

static bool
msp430_handle_option (struct gcc_options *opts ATTRIBUTE_UNUSED,
		      struct gcc_options *opts_set ATTRIBUTE_UNUSED,
		      const struct cl_decoded_option *decoded,
		      location_t loc ATTRIBUTE_UNUSED)
{
  switch (decoded->opt_index)
    {
    case OPT_mmcu_:
      /* For backwards compatibility we recognise two generic MCU
	 430X names.  However we want to be able to generate special C
	 preprocessor defines for them, which is why we set target_mcu
	 to NULL.  */
      if (strcasecmp (decoded->arg, "msp430") == 0)
	{
	  target_cpu = MSP430_CPU_MSP430;
	  target_mcu = NULL;
	}
      else if (strcasecmp (decoded->arg, "msp430x") == 0
	       || strcasecmp (decoded->arg, "msp430xv2") == 0)
	{
	  target_cpu = MSP430_CPU_MSP430X;
	  target_mcu = NULL;
	}
      break;
    }
      
  return true;
}

#undef  TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION			msp430_handle_option

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
