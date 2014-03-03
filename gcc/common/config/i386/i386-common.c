/* IA-32 common hooks.
   Copyright (C) 1988-2014 Free Software Foundation, Inc.

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
#include "tm_p.h"
#include "common/common-target.h"
#include "common/common-target-def.h"
#include "opts.h"
#include "flags.h"

/* Define a set of ISAs which are available when a given ISA is
   enabled.  MMX and SSE ISAs are handled separately.  */

#define OPTION_MASK_ISA_MMX_SET OPTION_MASK_ISA_MMX
#define OPTION_MASK_ISA_3DNOW_SET \
  (OPTION_MASK_ISA_3DNOW | OPTION_MASK_ISA_MMX_SET)

#define OPTION_MASK_ISA_SSE_SET OPTION_MASK_ISA_SSE
#define OPTION_MASK_ISA_SSE2_SET \
  (OPTION_MASK_ISA_SSE2 | OPTION_MASK_ISA_SSE_SET)
#define OPTION_MASK_ISA_SSE3_SET \
  (OPTION_MASK_ISA_SSE3 | OPTION_MASK_ISA_SSE2_SET)
#define OPTION_MASK_ISA_SSSE3_SET \
  (OPTION_MASK_ISA_SSSE3 | OPTION_MASK_ISA_SSE3_SET)
#define OPTION_MASK_ISA_SSE4_1_SET \
  (OPTION_MASK_ISA_SSE4_1 | OPTION_MASK_ISA_SSSE3_SET)
#define OPTION_MASK_ISA_SSE4_2_SET \
  (OPTION_MASK_ISA_SSE4_2 | OPTION_MASK_ISA_SSE4_1_SET)
#define OPTION_MASK_ISA_AVX_SET \
  (OPTION_MASK_ISA_AVX | OPTION_MASK_ISA_SSE4_2_SET \
   | OPTION_MASK_ISA_XSAVE_SET)
#define OPTION_MASK_ISA_FMA_SET \
  (OPTION_MASK_ISA_FMA | OPTION_MASK_ISA_AVX_SET)
#define OPTION_MASK_ISA_AVX2_SET \
  (OPTION_MASK_ISA_AVX2 | OPTION_MASK_ISA_AVX_SET)
#define OPTION_MASK_ISA_FXSR_SET OPTION_MASK_ISA_FXSR
#define OPTION_MASK_ISA_XSAVE_SET OPTION_MASK_ISA_XSAVE
#define OPTION_MASK_ISA_XSAVEOPT_SET \
  (OPTION_MASK_ISA_XSAVEOPT | OPTION_MASK_ISA_XSAVE)
#define OPTION_MASK_ISA_AVX512F_SET \
  (OPTION_MASK_ISA_AVX512F | OPTION_MASK_ISA_AVX2_SET)
#define OPTION_MASK_ISA_AVX512CD_SET \
  (OPTION_MASK_ISA_AVX512CD | OPTION_MASK_ISA_AVX512F_SET)
#define OPTION_MASK_ISA_AVX512PF_SET \
  (OPTION_MASK_ISA_AVX512PF | OPTION_MASK_ISA_AVX512F_SET)
#define OPTION_MASK_ISA_AVX512ER_SET \
  (OPTION_MASK_ISA_AVX512ER | OPTION_MASK_ISA_AVX512F_SET)
#define OPTION_MASK_ISA_RTM_SET OPTION_MASK_ISA_RTM
#define OPTION_MASK_ISA_PRFCHW_SET OPTION_MASK_ISA_PRFCHW
#define OPTION_MASK_ISA_RDSEED_SET OPTION_MASK_ISA_RDSEED
#define OPTION_MASK_ISA_ADX_SET OPTION_MASK_ISA_ADX
#define OPTION_MASK_ISA_PREFETCHWT1_SET OPTION_MASK_ISA_PREFETCHWT1

/* SSE4 includes both SSE4.1 and SSE4.2. -msse4 should be the same
   as -msse4.2.  */
#define OPTION_MASK_ISA_SSE4_SET OPTION_MASK_ISA_SSE4_2_SET

#define OPTION_MASK_ISA_SSE4A_SET \
  (OPTION_MASK_ISA_SSE4A | OPTION_MASK_ISA_SSE3_SET)
#define OPTION_MASK_ISA_FMA4_SET \
  (OPTION_MASK_ISA_FMA4 | OPTION_MASK_ISA_SSE4A_SET \
   | OPTION_MASK_ISA_AVX_SET)
#define OPTION_MASK_ISA_XOP_SET \
  (OPTION_MASK_ISA_XOP | OPTION_MASK_ISA_FMA4_SET)
#define OPTION_MASK_ISA_LWP_SET \
  OPTION_MASK_ISA_LWP

/* AES, SHA and PCLMUL need SSE2 because they use xmm registers.  */
#define OPTION_MASK_ISA_AES_SET \
  (OPTION_MASK_ISA_AES | OPTION_MASK_ISA_SSE2_SET)
#define OPTION_MASK_ISA_SHA_SET \
  (OPTION_MASK_ISA_SHA | OPTION_MASK_ISA_SSE2_SET)
#define OPTION_MASK_ISA_PCLMUL_SET \
  (OPTION_MASK_ISA_PCLMUL | OPTION_MASK_ISA_SSE2_SET)

#define OPTION_MASK_ISA_ABM_SET \
  (OPTION_MASK_ISA_ABM | OPTION_MASK_ISA_POPCNT)

#define OPTION_MASK_ISA_BMI_SET OPTION_MASK_ISA_BMI
#define OPTION_MASK_ISA_BMI2_SET OPTION_MASK_ISA_BMI2
#define OPTION_MASK_ISA_LZCNT_SET OPTION_MASK_ISA_LZCNT
#define OPTION_MASK_ISA_TBM_SET OPTION_MASK_ISA_TBM
#define OPTION_MASK_ISA_POPCNT_SET OPTION_MASK_ISA_POPCNT
#define OPTION_MASK_ISA_CX16_SET OPTION_MASK_ISA_CX16
#define OPTION_MASK_ISA_SAHF_SET OPTION_MASK_ISA_SAHF
#define OPTION_MASK_ISA_MOVBE_SET OPTION_MASK_ISA_MOVBE
#define OPTION_MASK_ISA_CRC32_SET OPTION_MASK_ISA_CRC32

#define OPTION_MASK_ISA_FSGSBASE_SET OPTION_MASK_ISA_FSGSBASE
#define OPTION_MASK_ISA_RDRND_SET OPTION_MASK_ISA_RDRND
#define OPTION_MASK_ISA_F16C_SET \
  (OPTION_MASK_ISA_F16C | OPTION_MASK_ISA_AVX_SET)

/* Define a set of ISAs which aren't available when a given ISA is
   disabled.  MMX and SSE ISAs are handled separately.  */

#define OPTION_MASK_ISA_MMX_UNSET \
  (OPTION_MASK_ISA_MMX | OPTION_MASK_ISA_3DNOW_UNSET)
#define OPTION_MASK_ISA_3DNOW_UNSET \
  (OPTION_MASK_ISA_3DNOW | OPTION_MASK_ISA_3DNOW_A_UNSET)
#define OPTION_MASK_ISA_3DNOW_A_UNSET OPTION_MASK_ISA_3DNOW_A

#define OPTION_MASK_ISA_SSE_UNSET \
  (OPTION_MASK_ISA_SSE | OPTION_MASK_ISA_SSE2_UNSET)
#define OPTION_MASK_ISA_SSE2_UNSET \
  (OPTION_MASK_ISA_SSE2 | OPTION_MASK_ISA_SSE3_UNSET)
#define OPTION_MASK_ISA_SSE3_UNSET \
  (OPTION_MASK_ISA_SSE3 \
   | OPTION_MASK_ISA_SSSE3_UNSET \
   | OPTION_MASK_ISA_SSE4A_UNSET )
#define OPTION_MASK_ISA_SSSE3_UNSET \
  (OPTION_MASK_ISA_SSSE3 | OPTION_MASK_ISA_SSE4_1_UNSET)
#define OPTION_MASK_ISA_SSE4_1_UNSET \
  (OPTION_MASK_ISA_SSE4_1 | OPTION_MASK_ISA_SSE4_2_UNSET)
#define OPTION_MASK_ISA_SSE4_2_UNSET \
  (OPTION_MASK_ISA_SSE4_2 | OPTION_MASK_ISA_AVX_UNSET )
#define OPTION_MASK_ISA_AVX_UNSET \
  (OPTION_MASK_ISA_AVX | OPTION_MASK_ISA_FMA_UNSET \
   | OPTION_MASK_ISA_FMA4_UNSET | OPTION_MASK_ISA_F16C_UNSET \
   | OPTION_MASK_ISA_AVX2_UNSET | OPTION_MASK_ISA_XSAVE_UNSET)
#define OPTION_MASK_ISA_FMA_UNSET OPTION_MASK_ISA_FMA
#define OPTION_MASK_ISA_FXSR_UNSET OPTION_MASK_ISA_FXSR
#define OPTION_MASK_ISA_XSAVE_UNSET \
  (OPTION_MASK_ISA_XSAVE | OPTION_MASK_ISA_XSAVEOPT_UNSET)
#define OPTION_MASK_ISA_XSAVEOPT_UNSET OPTION_MASK_ISA_XSAVEOPT
#define OPTION_MASK_ISA_AVX2_UNSET \
  (OPTION_MASK_ISA_AVX2 | OPTION_MASK_ISA_AVX512F_UNSET)
#define OPTION_MASK_ISA_AVX512F_UNSET \
  (OPTION_MASK_ISA_AVX512F | OPTION_MASK_ISA_AVX512CD_UNSET \
   | OPTION_MASK_ISA_AVX512PF_UNSET | OPTION_MASK_ISA_AVX512ER_UNSET)
#define OPTION_MASK_ISA_AVX512CD_UNSET OPTION_MASK_ISA_AVX512CD
#define OPTION_MASK_ISA_AVX512PF_UNSET OPTION_MASK_ISA_AVX512PF
#define OPTION_MASK_ISA_AVX512ER_UNSET OPTION_MASK_ISA_AVX512ER
#define OPTION_MASK_ISA_RTM_UNSET OPTION_MASK_ISA_RTM
#define OPTION_MASK_ISA_PRFCHW_UNSET OPTION_MASK_ISA_PRFCHW
#define OPTION_MASK_ISA_RDSEED_UNSET OPTION_MASK_ISA_RDSEED
#define OPTION_MASK_ISA_ADX_UNSET OPTION_MASK_ISA_ADX
#define OPTION_MASK_ISA_PREFETCHWT1_UNSET OPTION_MASK_ISA_PREFETCHWT1

/* SSE4 includes both SSE4.1 and SSE4.2.  -mno-sse4 should the same
   as -mno-sse4.1. */
#define OPTION_MASK_ISA_SSE4_UNSET OPTION_MASK_ISA_SSE4_1_UNSET

#define OPTION_MASK_ISA_SSE4A_UNSET \
  (OPTION_MASK_ISA_SSE4A | OPTION_MASK_ISA_FMA4_UNSET)

#define OPTION_MASK_ISA_FMA4_UNSET \
  (OPTION_MASK_ISA_FMA4 | OPTION_MASK_ISA_XOP_UNSET)
#define OPTION_MASK_ISA_XOP_UNSET OPTION_MASK_ISA_XOP
#define OPTION_MASK_ISA_LWP_UNSET OPTION_MASK_ISA_LWP

#define OPTION_MASK_ISA_AES_UNSET OPTION_MASK_ISA_AES
#define OPTION_MASK_ISA_SHA_UNSET OPTION_MASK_ISA_SHA
#define OPTION_MASK_ISA_PCLMUL_UNSET OPTION_MASK_ISA_PCLMUL
#define OPTION_MASK_ISA_ABM_UNSET OPTION_MASK_ISA_ABM
#define OPTION_MASK_ISA_BMI_UNSET OPTION_MASK_ISA_BMI
#define OPTION_MASK_ISA_BMI2_UNSET OPTION_MASK_ISA_BMI2
#define OPTION_MASK_ISA_LZCNT_UNSET OPTION_MASK_ISA_LZCNT
#define OPTION_MASK_ISA_TBM_UNSET OPTION_MASK_ISA_TBM
#define OPTION_MASK_ISA_POPCNT_UNSET OPTION_MASK_ISA_POPCNT
#define OPTION_MASK_ISA_CX16_UNSET OPTION_MASK_ISA_CX16
#define OPTION_MASK_ISA_SAHF_UNSET OPTION_MASK_ISA_SAHF
#define OPTION_MASK_ISA_MOVBE_UNSET OPTION_MASK_ISA_MOVBE
#define OPTION_MASK_ISA_CRC32_UNSET OPTION_MASK_ISA_CRC32

#define OPTION_MASK_ISA_FSGSBASE_UNSET OPTION_MASK_ISA_FSGSBASE
#define OPTION_MASK_ISA_RDRND_UNSET OPTION_MASK_ISA_RDRND
#define OPTION_MASK_ISA_F16C_UNSET OPTION_MASK_ISA_F16C

/* Implement TARGET_HANDLE_OPTION.  */

bool
ix86_handle_option (struct gcc_options *opts,
		    struct gcc_options *opts_set ATTRIBUTE_UNUSED,
		    const struct cl_decoded_option *decoded,
		    location_t loc)
{
  size_t code = decoded->opt_index;
  int value = decoded->value;

  switch (code)
    {
    case OPT_mmmx:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_MMX_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_MMX_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_MMX_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_MMX_UNSET;
	}
      return true;

    case OPT_m3dnow:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_3DNOW_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_3DNOW_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_3DNOW_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_3DNOW_UNSET;
	}
      return true;

    case OPT_m3dnowa:
      return false;

    case OPT_msse:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_SSE_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_SSE_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE_UNSET;
	}
      return true;

    case OPT_msse2:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_SSE2_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE2_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_SSE2_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE2_UNSET;
	}
      return true;

    case OPT_msse3:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_SSE3_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE3_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_SSE3_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE3_UNSET;
	}
      return true;

    case OPT_mssse3:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_SSSE3_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSSE3_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_SSSE3_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSSE3_UNSET;
	}
      return true;

    case OPT_msse4_1:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_SSE4_1_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE4_1_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_SSE4_1_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE4_1_UNSET;
	}
      return true;

    case OPT_msse4_2:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_SSE4_2_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE4_2_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_SSE4_2_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE4_2_UNSET;
	}
      return true;

    case OPT_mavx:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_AVX_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX_UNSET;
	}
      return true;

    case OPT_mavx2:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX2_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX2_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_AVX2_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX2_UNSET;
	}
      return true;

    case OPT_mavx512f:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX512F_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX512F_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_AVX512F_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX512F_UNSET;
	}
      return true;

    case OPT_mavx512cd:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX512CD_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX512CD_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_AVX512CD_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX512CD_UNSET;
	}
      return true;

    case OPT_mavx512pf:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX512PF_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX512PF_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_AVX512PF_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX512PF_UNSET;
	}
      return true;

    case OPT_mavx512er:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX512ER_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX512ER_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_AVX512ER_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX512ER_UNSET;
	}
      return true;

    case OPT_mfma:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_FMA_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_FMA_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_FMA_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_FMA_UNSET;
	}
      return true;

    case OPT_mrtm:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_RTM_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_RTM_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_RTM_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_RTM_UNSET;
	}
      return true;

    case OPT_msse4:
      opts->x_ix86_isa_flags |= OPTION_MASK_ISA_SSE4_SET;
      opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE4_SET;
      return true;

    case OPT_mno_sse4:
      opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_SSE4_UNSET;
      opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE4_UNSET;
      return true;

    case OPT_msse4a:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_SSE4A_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE4A_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_SSE4A_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE4A_UNSET;
	}
      return true;

    case OPT_mfma4:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_FMA4_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_FMA4_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_FMA4_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_FMA4_UNSET;
	}
      return true;

   case OPT_mxop:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_XOP_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_XOP_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_XOP_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_XOP_UNSET;
	}
      return true;

   case OPT_mlwp:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_LWP_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_LWP_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_LWP_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_LWP_UNSET;
	}
      return true;

    case OPT_mabm:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_ABM_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_ABM_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_ABM_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_ABM_UNSET;
	}
      return true;

    case OPT_mbmi:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_BMI_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_BMI_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_BMI_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_BMI_UNSET;
	}
      return true;

    case OPT_mbmi2:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_BMI2_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_BMI2_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_BMI2_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_BMI2_UNSET;
	}
      return true;

    case OPT_mlzcnt:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_LZCNT_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_LZCNT_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_LZCNT_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_LZCNT_UNSET;
	}
      return true;

    case OPT_mtbm:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_TBM_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_TBM_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_TBM_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_TBM_UNSET;
	}
      return true;

    case OPT_mpopcnt:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_POPCNT_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_POPCNT_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_POPCNT_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_POPCNT_UNSET;
	}
      return true;

    case OPT_msahf:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_SAHF_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_SAHF_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_SAHF_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_SAHF_UNSET;
	}
      return true;

    case OPT_mcx16:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_CX16_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_CX16_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_CX16_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_CX16_UNSET;
	}
      return true;

    case OPT_mmovbe:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_MOVBE_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_MOVBE_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_MOVBE_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_MOVBE_UNSET;
	}
      return true;

    case OPT_mcrc32:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_CRC32_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_CRC32_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_CRC32_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_CRC32_UNSET;
	}
      return true;

    case OPT_maes:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AES_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AES_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_AES_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AES_UNSET;
	}
      return true;

    case OPT_msha:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_SHA_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_SHA_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_SHA_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_SHA_UNSET;
	}
      return true;

    case OPT_mpclmul:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_PCLMUL_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_PCLMUL_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_PCLMUL_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_PCLMUL_UNSET;
	}
      return true;

    case OPT_mfsgsbase:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_FSGSBASE_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_FSGSBASE_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_FSGSBASE_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_FSGSBASE_UNSET;
	}
      return true;

    case OPT_mrdrnd:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_RDRND_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_RDRND_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_RDRND_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_RDRND_UNSET;
	}
      return true;

    case OPT_mf16c:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_F16C_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_F16C_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_F16C_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_F16C_UNSET;
	}
      return true;

    case OPT_mfxsr:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_FXSR_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_FXSR_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_FXSR_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_FXSR_UNSET;
	}
      return true;

    case OPT_mxsave:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_XSAVE_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_XSAVE_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_XSAVE_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_XSAVE_UNSET;
	}
      return true;

    case OPT_mxsaveopt:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_XSAVEOPT_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_XSAVEOPT_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_XSAVEOPT_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_XSAVEOPT_UNSET;
	}
      return true;

    case OPT_mrdseed:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_RDSEED_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_RDSEED_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_RDSEED_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_RDSEED_UNSET;
	}
      return true;

    case OPT_mprfchw:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_PRFCHW_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_PRFCHW_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_PRFCHW_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_PRFCHW_UNSET;
	}
      return true;

    case OPT_madx:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_ADX_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_ADX_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_ADX_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_ADX_UNSET;
	}
      return true;

    case OPT_mprefetchwt1:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_PREFETCHWT1_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_PREFETCHWT1_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_PREFETCHWT1_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_PREFETCHWT1_UNSET;
	}
      return true;

  /* Comes from final.c -- no real reason to change it.  */
#define MAX_CODE_ALIGN 16

    case OPT_malign_loops_:
      warning_at (loc, 0, "-malign-loops is obsolete, use -falign-loops");
      if (value > MAX_CODE_ALIGN)
	error_at (loc, "-malign-loops=%d is not between 0 and %d",
		  value, MAX_CODE_ALIGN);
      else
	opts->x_align_loops = 1 << value;
      return true;

    case OPT_malign_jumps_:
      warning_at (loc, 0, "-malign-jumps is obsolete, use -falign-jumps");
      if (value > MAX_CODE_ALIGN)
	error_at (loc, "-malign-jumps=%d is not between 0 and %d",
		  value, MAX_CODE_ALIGN);
      else
	opts->x_align_jumps = 1 << value;
      return true;

    case OPT_malign_functions_:
      warning_at (loc, 0,
		  "-malign-functions is obsolete, use -falign-functions");
      if (value > MAX_CODE_ALIGN)
	error_at (loc, "-malign-functions=%d is not between 0 and %d",
		  value, MAX_CODE_ALIGN);
      else
	opts->x_align_functions = 1 << value;
      return true;

    case OPT_mbranch_cost_:
      if (value > 5)
	{
	  error_at (loc, "-mbranch-cost=%d is not between 0 and 5", value);
	  opts->x_ix86_branch_cost = 5;
	}
      return true;

    default:
      return true;
    }
}

static const struct default_options ix86_option_optimization_table[] =
  {
    /* Enable redundant extension instructions removal at -O2 and higher.  */
    { OPT_LEVELS_2_PLUS, OPT_free, NULL, 1 },
    /* Enable function splitting at -O2 and higher.  */
    { OPT_LEVELS_2_PLUS, OPT_freorder_blocks_and_partition, NULL, 1 },
    /* Turn off -fschedule-insns by default.  It tends to make the
       problem with not enough registers even worse.  */
    { OPT_LEVELS_ALL, OPT_fschedule_insns, NULL, 0 },

#ifdef SUBTARGET_OPTIMIZATION_OPTIONS
    SUBTARGET_OPTIMIZATION_OPTIONS,
#endif
    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

/* Implement TARGET_OPTION_INIT_STRUCT.  */

static void
ix86_option_init_struct (struct gcc_options *opts)
{
  if (TARGET_MACHO)
    /* The Darwin libraries never set errno, so we might as well
       avoid calling them when that's the only reason we would.  */
    opts->x_flag_errno_math = 0;

  opts->x_flag_pcc_struct_return = 2;
  opts->x_flag_asynchronous_unwind_tables = 2;
}

/* On the x86 -fsplit-stack and -fstack-protector both use the same
   field in the TCB, so they can not be used together.  */

static bool
ix86_supports_split_stack (bool report ATTRIBUTE_UNUSED,
			   struct gcc_options *opts ATTRIBUTE_UNUSED)
{
  bool ret = true;

#ifndef TARGET_THREAD_SPLIT_STACK_OFFSET
  if (report)
    error ("%<-fsplit-stack%> currently only supported on GNU/Linux");
  ret = false;
#else
  if (!HAVE_GAS_CFI_PERSONALITY_DIRECTIVE)
    {
      if (report)
	error ("%<-fsplit-stack%> requires "
	       "assembler support for CFI directives");
      ret = false;
    }
#endif

  return ret;
}

/* Implement TARGET_EXCEPT_UNWIND_INFO.  */

static enum unwind_info_type
i386_except_unwind_info (struct gcc_options *opts)
{
  /* Honor the --enable-sjlj-exceptions configure switch.  */
#ifdef CONFIG_SJLJ_EXCEPTIONS
  if (CONFIG_SJLJ_EXCEPTIONS)
    return UI_SJLJ;
#endif

  /* On windows 64, prefer SEH exceptions over anything else.  */
  if (TARGET_64BIT && DEFAULT_ABI == MS_ABI && opts->x_flag_unwind_tables)
    return UI_SEH;

  if (DWARF2_UNWIND_INFO)
    return UI_DWARF2;

  return UI_SJLJ;
}

#undef  TARGET_EXCEPT_UNWIND_INFO
#define TARGET_EXCEPT_UNWIND_INFO  i386_except_unwind_info

#undef TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS	\
  (TARGET_DEFAULT			\
   | TARGET_SUBTARGET_DEFAULT		\
   | TARGET_TLS_DIRECT_SEG_REFS_DEFAULT)

#undef TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION ix86_handle_option

#undef TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE ix86_option_optimization_table
#undef TARGET_OPTION_INIT_STRUCT
#define TARGET_OPTION_INIT_STRUCT ix86_option_init_struct

#undef TARGET_SUPPORTS_SPLIT_STACK
#define TARGET_SUPPORTS_SPLIT_STACK ix86_supports_split_stack

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
