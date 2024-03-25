/* Functions for Linux on PowerPC.
   Copyright (C) 2013-2024 Free Software Foundation, Inc.

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

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "target.h"
#include "targhooks.h"
#include "tree.h"
#include "case-cfn-macros.h"

/* Implement TARGET_FLOAT_EXCEPTIONS_ROUNDING_SUPPORTED_P.  */

bool
rs6000_linux_float_exceptions_rounding_supported_p (void)
{
  /* glibc has support for exceptions and rounding modes for software
     floating point.  */
  if (OPTION_GLIBC)
    return true;
  else
    return TARGET_HARD_FLOAT;
}

/* Implement TARGET_LIBM_FUNCTION_MAX_ERROR.  */

unsigned
rs6000_linux_libm_function_max_error (unsigned cfn, machine_mode mode,
				      bool boundary_p)
{
  if (OPTION_GLIBC)
    {
      int rnd = flag_rounding_math ? 4 : 0;
      switch (cfn)
	{
	CASE_CFN_SQRT:
	CASE_CFN_SQRT_FN:
	  if (!boundary_p && MODE_COMPOSITE_P (mode))
	    return 1 + rnd;
	  break;
	CASE_CFN_COS:
	CASE_CFN_COS_FN:
	  if (!boundary_p && mode == SFmode)
	    return 3 + rnd;
	  if (!boundary_p && MODE_COMPOSITE_P (mode))
	    return 4 + rnd;
	  break;
	CASE_CFN_SIN:
	CASE_CFN_SIN_FN:
	  if (!boundary_p && MODE_COMPOSITE_P (mode))
	    return 1 + rnd;
	  break;
	default:
	  break;
	}
      return glibc_linux_libm_function_max_error (cfn, mode, boundary_p);
    }
  return default_libm_function_max_error (cfn, mode, boundary_p);
}
