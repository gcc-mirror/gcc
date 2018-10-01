/* IA-32 common hooks.
   Copyright (C) 1988-2018 Free Software Foundation, Inc.

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
#include "memmodel.h"
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
#define OPTION_MASK_ISA_3DNOW_A_SET \
  (OPTION_MASK_ISA_3DNOW_A | OPTION_MASK_ISA_3DNOW_SET)

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
  (OPTION_MASK_ISA_XSAVEOPT | OPTION_MASK_ISA_XSAVE_SET)
#define OPTION_MASK_ISA_AVX512F_SET \
  (OPTION_MASK_ISA_AVX512F | OPTION_MASK_ISA_AVX2_SET)
#define OPTION_MASK_ISA_AVX512CD_SET \
  (OPTION_MASK_ISA_AVX512CD | OPTION_MASK_ISA_AVX512F_SET)
#define OPTION_MASK_ISA_AVX512PF_SET \
  (OPTION_MASK_ISA_AVX512PF | OPTION_MASK_ISA_AVX512F_SET)
#define OPTION_MASK_ISA_AVX512ER_SET \
  (OPTION_MASK_ISA_AVX512ER | OPTION_MASK_ISA_AVX512F_SET)
#define OPTION_MASK_ISA_AVX512DQ_SET \
  (OPTION_MASK_ISA_AVX512DQ | OPTION_MASK_ISA_AVX512F_SET)
#define OPTION_MASK_ISA_AVX512BW_SET \
  (OPTION_MASK_ISA_AVX512BW | OPTION_MASK_ISA_AVX512F_SET)
#define OPTION_MASK_ISA_AVX512VL_SET \
  (OPTION_MASK_ISA_AVX512VL | OPTION_MASK_ISA_AVX512F_SET)
#define OPTION_MASK_ISA_AVX512IFMA_SET \
  (OPTION_MASK_ISA_AVX512IFMA | OPTION_MASK_ISA_AVX512F_SET)
#define OPTION_MASK_ISA_AVX512VBMI_SET \
  (OPTION_MASK_ISA_AVX512VBMI | OPTION_MASK_ISA_AVX512BW_SET)
#define OPTION_MASK_ISA_AVX5124FMAPS_SET OPTION_MASK_ISA_AVX5124FMAPS
#define OPTION_MASK_ISA_AVX5124VNNIW_SET OPTION_MASK_ISA_AVX5124VNNIW
#define OPTION_MASK_ISA_AVX512VBMI2_SET \
  (OPTION_MASK_ISA_AVX512VBMI2 | OPTION_MASK_ISA_AVX512F_SET)
#define OPTION_MASK_ISA_AVX512VNNI_SET \
  (OPTION_MASK_ISA_AVX512VNNI | OPTION_MASK_ISA_AVX512F_SET)
#define OPTION_MASK_ISA_AVX512VPOPCNTDQ_SET \
  (OPTION_MASK_ISA_AVX512VPOPCNTDQ | OPTION_MASK_ISA_AVX512F_SET)
#define OPTION_MASK_ISA_AVX512BITALG_SET \
  (OPTION_MASK_ISA_AVX512BITALG | OPTION_MASK_ISA_AVX512F_SET)
#define OPTION_MASK_ISA_RTM_SET OPTION_MASK_ISA_RTM
#define OPTION_MASK_ISA_PRFCHW_SET OPTION_MASK_ISA_PRFCHW
#define OPTION_MASK_ISA_RDSEED_SET OPTION_MASK_ISA_RDSEED
#define OPTION_MASK_ISA_ADX_SET OPTION_MASK_ISA_ADX
#define OPTION_MASK_ISA_PREFETCHWT1_SET OPTION_MASK_ISA_PREFETCHWT1
#define OPTION_MASK_ISA_CLFLUSHOPT_SET OPTION_MASK_ISA_CLFLUSHOPT
#define OPTION_MASK_ISA_XSAVES_SET \
  (OPTION_MASK_ISA_XSAVES | OPTION_MASK_ISA_XSAVE_SET)
#define OPTION_MASK_ISA_XSAVEC_SET \
  (OPTION_MASK_ISA_XSAVEC | OPTION_MASK_ISA_XSAVE_SET)
#define OPTION_MASK_ISA_CLWB_SET OPTION_MASK_ISA_CLWB

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

#define OPTION_MASK_ISA_PCONFIG_SET OPTION_MASK_ISA_PCONFIG
#define OPTION_MASK_ISA_WBNOINVD_SET OPTION_MASK_ISA_WBNOINVD
#define OPTION_MASK_ISA_SGX_SET OPTION_MASK_ISA_SGX
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
#define OPTION_MASK_ISA_MWAITX_SET OPTION_MASK_ISA_MWAITX
#define OPTION_MASK_ISA_CLZERO_SET OPTION_MASK_ISA_CLZERO
#define OPTION_MASK_ISA_PKU_SET OPTION_MASK_ISA_PKU
#define OPTION_MASK_ISA_RDPID_SET OPTION_MASK_ISA_RDPID
#define OPTION_MASK_ISA_GFNI_SET OPTION_MASK_ISA_GFNI
#define OPTION_MASK_ISA_SHSTK_SET OPTION_MASK_ISA_SHSTK
#define OPTION_MASK_ISA_VAES_SET OPTION_MASK_ISA_VAES
#define OPTION_MASK_ISA_VPCLMULQDQ_SET OPTION_MASK_ISA_VPCLMULQDQ
#define OPTION_MASK_ISA_MOVDIRI_SET OPTION_MASK_ISA_MOVDIRI
#define OPTION_MASK_ISA_MOVDIR64B_SET OPTION_MASK_ISA_MOVDIR64B
#define OPTION_MASK_ISA_WAITPKG_SET OPTION_MASK_ISA_WAITPKG
#define OPTION_MASK_ISA_CLDEMOTE_SET OPTION_MASK_ISA_CLDEMOTE

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
  (OPTION_MASK_ISA_XSAVE | OPTION_MASK_ISA_XSAVEOPT_UNSET \
   | OPTION_MASK_ISA_XSAVES_UNSET | OPTION_MASK_ISA_XSAVEC_UNSET)
#define OPTION_MASK_ISA_XSAVEOPT_UNSET OPTION_MASK_ISA_XSAVEOPT
#define OPTION_MASK_ISA_AVX2_UNSET \
  (OPTION_MASK_ISA_AVX2 | OPTION_MASK_ISA_AVX512F_UNSET)
#define OPTION_MASK_ISA_AVX512F_UNSET \
  (OPTION_MASK_ISA_AVX512F | OPTION_MASK_ISA_AVX512CD_UNSET \
   | OPTION_MASK_ISA_AVX512PF_UNSET | OPTION_MASK_ISA_AVX512ER_UNSET \
   | OPTION_MASK_ISA_AVX512DQ_UNSET | OPTION_MASK_ISA_AVX512BW_UNSET \
   | OPTION_MASK_ISA_AVX512VL_UNSET | OPTION_MASK_ISA_AVX512VBMI2_UNSET \
   | OPTION_MASK_ISA_AVX512VNNI_UNSET | OPTION_MASK_ISA_AVX512VPOPCNTDQ_UNSET \
   | OPTION_MASK_ISA_AVX512BITALG_UNSET)
#define OPTION_MASK_ISA_AVX512CD_UNSET OPTION_MASK_ISA_AVX512CD
#define OPTION_MASK_ISA_AVX512PF_UNSET OPTION_MASK_ISA_AVX512PF
#define OPTION_MASK_ISA_AVX512ER_UNSET OPTION_MASK_ISA_AVX512ER
#define OPTION_MASK_ISA_AVX512DQ_UNSET OPTION_MASK_ISA_AVX512DQ
#define OPTION_MASK_ISA_AVX512BW_UNSET \
  (OPTION_MASK_ISA_AVX512BW | OPTION_MASK_ISA_AVX512VBMI_UNSET)
#define OPTION_MASK_ISA_AVX512VL_UNSET OPTION_MASK_ISA_AVX512VL
#define OPTION_MASK_ISA_AVX512IFMA_UNSET OPTION_MASK_ISA_AVX512IFMA
#define OPTION_MASK_ISA_AVX512VBMI_UNSET OPTION_MASK_ISA_AVX512VBMI
#define OPTION_MASK_ISA_AVX5124FMAPS_UNSET OPTION_MASK_ISA_AVX5124FMAPS
#define OPTION_MASK_ISA_AVX5124VNNIW_UNSET OPTION_MASK_ISA_AVX5124VNNIW
#define OPTION_MASK_ISA_AVX512VBMI2_UNSET OPTION_MASK_ISA_AVX512VBMI2
#define OPTION_MASK_ISA_AVX512VNNI_UNSET OPTION_MASK_ISA_AVX512VNNI
#define OPTION_MASK_ISA_AVX512VPOPCNTDQ_UNSET OPTION_MASK_ISA_AVX512VPOPCNTDQ
#define OPTION_MASK_ISA_AVX512BITALG_UNSET OPTION_MASK_ISA_AVX512BITALG
#define OPTION_MASK_ISA_RTM_UNSET OPTION_MASK_ISA_RTM
#define OPTION_MASK_ISA_PRFCHW_UNSET OPTION_MASK_ISA_PRFCHW
#define OPTION_MASK_ISA_RDSEED_UNSET OPTION_MASK_ISA_RDSEED
#define OPTION_MASK_ISA_ADX_UNSET OPTION_MASK_ISA_ADX
#define OPTION_MASK_ISA_PREFETCHWT1_UNSET OPTION_MASK_ISA_PREFETCHWT1
#define OPTION_MASK_ISA_CLFLUSHOPT_UNSET OPTION_MASK_ISA_CLFLUSHOPT
#define OPTION_MASK_ISA_XSAVEC_UNSET OPTION_MASK_ISA_XSAVEC
#define OPTION_MASK_ISA_XSAVES_UNSET OPTION_MASK_ISA_XSAVES
#define OPTION_MASK_ISA_CLWB_UNSET OPTION_MASK_ISA_CLWB
#define OPTION_MASK_ISA_MWAITX_UNSET OPTION_MASK_ISA_MWAITX
#define OPTION_MASK_ISA_CLZERO_UNSET OPTION_MASK_ISA_CLZERO
#define OPTION_MASK_ISA_PKU_UNSET OPTION_MASK_ISA_PKU
#define OPTION_MASK_ISA_RDPID_UNSET OPTION_MASK_ISA_RDPID
#define OPTION_MASK_ISA_GFNI_UNSET OPTION_MASK_ISA_GFNI
#define OPTION_MASK_ISA_SHSTK_UNSET OPTION_MASK_ISA_SHSTK
#define OPTION_MASK_ISA_VAES_UNSET OPTION_MASK_ISA_VAES
#define OPTION_MASK_ISA_VPCLMULQDQ_UNSET OPTION_MASK_ISA_VPCLMULQDQ
#define OPTION_MASK_ISA_MOVDIRI_UNSET OPTION_MASK_ISA_MOVDIRI
#define OPTION_MASK_ISA_MOVDIR64B_UNSET OPTION_MASK_ISA_MOVDIR64B
#define OPTION_MASK_ISA_WAITPKG_UNSET OPTION_MASK_ISA_WAITPKG
#define OPTION_MASK_ISA_CLDEMOTE_UNSET OPTION_MASK_ISA_CLDEMOTE

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
#define OPTION_MASK_ISA_PCONFIG_UNSET OPTION_MASK_ISA_PCONFIG
#define OPTION_MASK_ISA_WBNOINVD_UNSET OPTION_MASK_ISA_WBNOINVD
#define OPTION_MASK_ISA_SGX_UNSET OPTION_MASK_ISA_SGX
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

#define OPTION_MASK_ISA_GENERAL_REGS_ONLY_UNSET \
  (OPTION_MASK_ISA_MMX_UNSET \
   | OPTION_MASK_ISA_SSE_UNSET)

#define OPTION_MASK_ISA2_AVX512F_UNSET \
  (OPTION_MASK_ISA_AVX5124FMAPS_UNSET | OPTION_MASK_ISA_AVX5124VNNIW_UNSET)
#define OPTION_MASK_ISA2_GENERAL_REGS_ONLY_UNSET \
  (OPTION_MASK_ISA2_AVX512F_UNSET)

/* Set 1 << value as value of -malign-FLAG option.  */

static void
set_malign_value (const char **flag, unsigned value)
{
  char *r = XNEWVEC (char, 6);
  sprintf (r, "%d", 1 << value);
  *flag = r;
}

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
    case OPT_mgeneral_regs_only:
      if (value)
	{
	  /* Disable MMX, SSE and x87 instructions if only
	     general registers are allowed.  */
	  opts->x_ix86_isa_flags
	    &= ~OPTION_MASK_ISA_GENERAL_REGS_ONLY_UNSET;
	  opts->x_ix86_isa_flags2
	    &= ~OPTION_MASK_ISA2_GENERAL_REGS_ONLY_UNSET;
	  opts->x_ix86_isa_flags_explicit
	    |= OPTION_MASK_ISA_GENERAL_REGS_ONLY_UNSET;
	  opts->x_ix86_isa_flags2_explicit
	    |= OPTION_MASK_ISA2_GENERAL_REGS_ONLY_UNSET;

	  opts->x_target_flags &= ~MASK_80387;
	}
      else
	gcc_unreachable ();
      return true;

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
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_3DNOW_A_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_3DNOW_A_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_3DNOW_A_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_3DNOW_A_UNSET;
	}
      return true;

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
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AVX512F_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVX512F_UNSET;
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
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AVX512F_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVX512F_UNSET;
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
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AVX512F_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVX512F_UNSET;
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
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AVX512F_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVX512F_UNSET;
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
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AVX512F_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVX512F_UNSET;
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
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AVX512F_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVX512F_UNSET;
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
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AVX512F_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVX512F_UNSET;
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
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AVX512F_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVX512F_UNSET;
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
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AVX512F_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVX512F_UNSET;
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

    case OPT_mrdpid:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA_RDPID_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA_RDPID_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA_RDPID_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA_RDPID_UNSET;
	}
      return true;

    case OPT_mgfni:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_GFNI_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_GFNI_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_GFNI_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_GFNI_UNSET;
	}
      return true;

    case OPT_mshstk:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_SHSTK_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_SHSTK_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_SHSTK_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_SHSTK_UNSET;
	}
      return true;

    case OPT_mvaes:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA_VAES_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA_VAES_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA_VAES_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA_VAES_UNSET;
	}
      return true;

    case OPT_mvpclmulqdq:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_VPCLMULQDQ_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_VPCLMULQDQ_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_VPCLMULQDQ_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_VPCLMULQDQ_UNSET;
	}
      return true;

    case OPT_mmovdiri:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_MOVDIRI_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_MOVDIRI_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_MOVDIRI_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_MOVDIRI_UNSET;
	}
      return true;

    case OPT_mmovdir64b:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA_MOVDIR64B_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA_MOVDIR64B_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA_MOVDIR64B_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA_MOVDIR64B_UNSET;
	}
	return true;

    case OPT_mcldemote:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA_CLDEMOTE_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA_CLDEMOTE_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA_CLDEMOTE_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA_CLDEMOTE_UNSET;
	}
      return true;

    case OPT_mwaitpkg:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA_WAITPKG_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA_WAITPKG_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA_WAITPKG_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA_WAITPKG_UNSET;
	}
      return true;

    case OPT_mavx5124fmaps:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA_AVX5124FMAPS_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA_AVX5124FMAPS_SET;
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX512F_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX512F_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA_AVX5124FMAPS_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA_AVX5124FMAPS_UNSET;
	}
      return true;

    case OPT_mavx5124vnniw:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA_AVX5124VNNIW_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA_AVX5124VNNIW_SET;
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX512F_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX512F_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA_AVX5124VNNIW_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA_AVX5124VNNIW_UNSET;
	}
      return true;

    case OPT_mavx512vbmi2:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX512VBMI2_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX512VBMI2_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_AVX512VBMI2_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX512VBMI2_UNSET;
	}
      return true;

    case OPT_mavx512vnni:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX512VNNI_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX512VNNI_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_AVX512VNNI_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX512VNNI_UNSET;
	}
      return true;

    case OPT_mavx512vpopcntdq:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX512VPOPCNTDQ_SET;
	  opts->x_ix86_isa_flags_explicit
	    |= OPTION_MASK_ISA_AVX512VPOPCNTDQ_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_AVX512VPOPCNTDQ_UNSET;
	  opts->x_ix86_isa_flags_explicit
	    |= OPTION_MASK_ISA_AVX512VPOPCNTDQ_UNSET;
	}
      return true;

    case OPT_mavx512bitalg:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX512BITALG_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX512BITALG_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_AVX512BITALG_UNSET;
	  opts->x_ix86_isa_flags_explicit
		|= OPTION_MASK_ISA_AVX512BITALG_UNSET;
	}
      return true;

    case OPT_msgx:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA_SGX_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA_SGX_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA_SGX_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA_SGX_UNSET;
	}
      return true;

    case OPT_mpconfig:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA_PCONFIG_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA_PCONFIG_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA_PCONFIG_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA_PCONFIG_UNSET;
	}
      return true;

    case OPT_mwbnoinvd:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA_WBNOINVD_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA_WBNOINVD_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA_WBNOINVD_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA_WBNOINVD_UNSET;
	}
      return true;

    case OPT_mavx512dq:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX512DQ_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX512DQ_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_AVX512DQ_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX512DQ_UNSET;
	}
      return true;

    case OPT_mavx512bw:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX512BW_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX512BW_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_AVX512BW_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX512BW_UNSET;
	}
      return true;

    case OPT_mavx512vl:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX512VL_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX512VL_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_AVX512VL_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX512VL_UNSET;
	}
      return true;

    case OPT_mavx512ifma:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX512IFMA_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX512IFMA_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_AVX512IFMA_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX512IFMA_UNSET;
	}
      return true;

    case OPT_mavx512vbmi:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX512VBMI_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX512VBMI_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_AVX512VBMI_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX512VBMI_UNSET;
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
      opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AVX512F_UNSET;
      opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVX512F_UNSET;
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
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA_CX16_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA_CX16_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA_CX16_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA_CX16_UNSET;
	}
      return true;

    case OPT_mmovbe:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA_MOVBE_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA_MOVBE_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA_MOVBE_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA_MOVBE_UNSET;
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

    case OPT_mxsavec:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_XSAVEC_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_XSAVEC_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_XSAVEC_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_XSAVEC_UNSET;
	}
      return true;

    case OPT_mxsaves:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_XSAVES_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_XSAVES_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_XSAVES_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_XSAVES_UNSET;
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

    case OPT_mclflushopt:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_CLFLUSHOPT_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_CLFLUSHOPT_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_CLFLUSHOPT_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_CLFLUSHOPT_UNSET;
	}
      return true;

    case OPT_mclwb:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_CLWB_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_CLWB_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_CLWB_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_CLWB_UNSET;
	}
      return true;

    case OPT_mmwaitx:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA_MWAITX_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA_MWAITX_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA_MWAITX_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA_MWAITX_UNSET;
	}
      return true;

    case OPT_mclzero:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA_CLZERO_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA_CLZERO_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA_CLZERO_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA_CLZERO_UNSET;
	}
      return true;

    case OPT_mpku:
      if (value)
	{
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_PKU_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_PKU_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags &= ~OPTION_MASK_ISA_PKU_UNSET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_PKU_UNSET;
	}
      return true;


    case OPT_malign_loops_:
      warning_at (loc, 0, "-malign-loops is obsolete, use -falign-loops");
      if (value > MAX_CODE_ALIGN)
	error_at (loc, "-malign-loops=%d is not between 0 and %d",
		  value, MAX_CODE_ALIGN);
      else
	set_malign_value (&opts->x_str_align_loops, value);
      return true;

    case OPT_malign_jumps_:
      warning_at (loc, 0, "-malign-jumps is obsolete, use -falign-jumps");
      if (value > MAX_CODE_ALIGN)
	error_at (loc, "-malign-jumps=%d is not between 0 and %d",
		  value, MAX_CODE_ALIGN);
      else
	set_malign_value (&opts->x_str_align_jumps, value);
      return true;

    case OPT_malign_functions_:
      warning_at (loc, 0,
		  "-malign-functions is obsolete, use -falign-functions");
      if (value > MAX_CODE_ALIGN)
	error_at (loc, "-malign-functions=%d is not between 0 and %d",
		  value, MAX_CODE_ALIGN);
      else
	set_malign_value (&opts->x_str_align_functions, value);
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
    /* The STC algorithm produces the smallest code at -Os, for x86.  */
    { OPT_LEVELS_2_PLUS, OPT_freorder_blocks_algorithm_, NULL,
      REORDER_BLOCKS_ALGORITHM_STC },
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

/* This table must be in sync with enum processor_type in i386.h.  */
const char *const processor_names[PROCESSOR_max] =
{
  "generic",
  "i386",
  "i486",
  "pentium",
  "lakemont",
  "pentiumpro",
  "pentium4",
  "nocona",
  "core2",
  "nehalem",
  "sandybridge",
  "haswell",
  "bonnell",
  "silvermont",
  "goldmont",
  "goldmont-plus",
  "tremont",
  "knl",
  "knm",
  "skylake",
  "skylake-avx512",
  "cannonlake",
  "icelake-client",
  "icelake-server",
  "intel",
  "geode",
  "k6",
  "athlon",
  "k8",
  "amdfam10",
  "bdver1",
  "bdver2",
  "bdver3",
  "bdver4",
  "btver1",
  "btver2",
  "znver1"
};

const pta processor_alias_table[] =
{
  {"i386", PROCESSOR_I386, CPU_NONE, 0},
  {"i486", PROCESSOR_I486, CPU_NONE, 0},
  {"i586", PROCESSOR_PENTIUM, CPU_PENTIUM, 0},
  {"pentium", PROCESSOR_PENTIUM, CPU_PENTIUM, 0},
  {"lakemont", PROCESSOR_LAKEMONT, CPU_PENTIUM, PTA_NO_80387},
  {"pentium-mmx", PROCESSOR_PENTIUM, CPU_PENTIUM, PTA_MMX},
  {"winchip-c6", PROCESSOR_I486, CPU_NONE, PTA_MMX},
  {"winchip2", PROCESSOR_I486, CPU_NONE, PTA_MMX | PTA_3DNOW},
  {"c3", PROCESSOR_I486, CPU_NONE, PTA_MMX | PTA_3DNOW},
  {"samuel-2", PROCESSOR_I486, CPU_NONE, PTA_MMX | PTA_3DNOW},
  {"c3-2", PROCESSOR_PENTIUMPRO, CPU_PENTIUMPRO,
    PTA_MMX | PTA_SSE | PTA_FXSR},
  {"nehemiah", PROCESSOR_PENTIUMPRO, CPU_PENTIUMPRO,
    PTA_MMX | PTA_SSE | PTA_FXSR},
  {"c7", PROCESSOR_PENTIUMPRO, CPU_PENTIUMPRO,
    PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3 | PTA_FXSR},
  {"esther", PROCESSOR_PENTIUMPRO, CPU_PENTIUMPRO,
    PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3 | PTA_FXSR},
  {"i686", PROCESSOR_PENTIUMPRO, CPU_PENTIUMPRO, 0},
  {"pentiumpro", PROCESSOR_PENTIUMPRO, CPU_PENTIUMPRO, 0},
  {"pentium2", PROCESSOR_PENTIUMPRO, CPU_PENTIUMPRO, PTA_MMX | PTA_FXSR},
  {"pentium3", PROCESSOR_PENTIUMPRO, CPU_PENTIUMPRO,
    PTA_MMX | PTA_SSE | PTA_FXSR},
  {"pentium3m", PROCESSOR_PENTIUMPRO, CPU_PENTIUMPRO,
    PTA_MMX | PTA_SSE | PTA_FXSR},
  {"pentium-m", PROCESSOR_PENTIUMPRO, CPU_PENTIUMPRO,
    PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_FXSR},
  {"pentium4", PROCESSOR_PENTIUM4, CPU_NONE,
    PTA_MMX |PTA_SSE | PTA_SSE2 | PTA_FXSR},
  {"pentium4m", PROCESSOR_PENTIUM4, CPU_NONE,
    PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_FXSR},
  {"prescott", PROCESSOR_NOCONA, CPU_NONE,
    PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3 | PTA_FXSR},
  {"nocona", PROCESSOR_NOCONA, CPU_NONE,
    PTA_64BIT | PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3
      | PTA_CX16 | PTA_NO_SAHF | PTA_FXSR},
  {"core2", PROCESSOR_CORE2, CPU_CORE2, PTA_CORE2},
  {"nehalem", PROCESSOR_NEHALEM, CPU_NEHALEM, PTA_NEHALEM},
  {"corei7", PROCESSOR_NEHALEM, CPU_NEHALEM, PTA_NEHALEM},
  {"westmere", PROCESSOR_NEHALEM, CPU_NEHALEM, PTA_WESTMERE},
  {"sandybridge", PROCESSOR_SANDYBRIDGE, CPU_NEHALEM,
    PTA_SANDYBRIDGE},
  {"corei7-avx", PROCESSOR_SANDYBRIDGE, CPU_NEHALEM,
    PTA_SANDYBRIDGE},
  {"ivybridge", PROCESSOR_SANDYBRIDGE, CPU_NEHALEM,
    PTA_IVYBRIDGE},
  {"core-avx-i", PROCESSOR_SANDYBRIDGE, CPU_NEHALEM,
    PTA_IVYBRIDGE},
  {"haswell", PROCESSOR_HASWELL, CPU_HASWELL, PTA_HASWELL},
  {"core-avx2", PROCESSOR_HASWELL, CPU_HASWELL, PTA_HASWELL},
  {"broadwell", PROCESSOR_HASWELL, CPU_HASWELL, PTA_BROADWELL},
  {"skylake", PROCESSOR_SKYLAKE, CPU_HASWELL, PTA_SKYLAKE},
  {"skylake-avx512", PROCESSOR_SKYLAKE_AVX512, CPU_HASWELL,
    PTA_SKYLAKE_AVX512},
  {"cannonlake", PROCESSOR_CANNONLAKE, CPU_HASWELL, PTA_CANNONLAKE},
  {"icelake-client", PROCESSOR_ICELAKE_CLIENT, CPU_HASWELL,
    PTA_ICELAKE_CLIENT},
  {"icelake-server", PROCESSOR_ICELAKE_SERVER, CPU_HASWELL,
    PTA_ICELAKE_SERVER},
  {"bonnell", PROCESSOR_BONNELL, CPU_ATOM, PTA_BONNELL},
  {"atom", PROCESSOR_BONNELL, CPU_ATOM, PTA_BONNELL},
  {"silvermont", PROCESSOR_SILVERMONT, CPU_SLM, PTA_SILVERMONT},
  {"slm", PROCESSOR_SILVERMONT, CPU_SLM, PTA_SILVERMONT},
  {"goldmont", PROCESSOR_GOLDMONT, CPU_GLM, PTA_GOLDMONT},
  {"goldmont-plus", PROCESSOR_GOLDMONT_PLUS, CPU_GLM, PTA_GOLDMONT_PLUS},
  {"tremont", PROCESSOR_TREMONT, CPU_GLM, PTA_TREMONT},
  {"knl", PROCESSOR_KNL, CPU_SLM, PTA_KNL},
  {"knm", PROCESSOR_KNM, CPU_SLM, PTA_KNM},
  {"intel", PROCESSOR_INTEL, CPU_SLM, PTA_NEHALEM},
  {"geode", PROCESSOR_GEODE, CPU_GEODE,
    PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_PREFETCH_SSE},
  {"k6", PROCESSOR_K6, CPU_K6, PTA_MMX},
  {"k6-2", PROCESSOR_K6, CPU_K6, PTA_MMX | PTA_3DNOW},
  {"k6-3", PROCESSOR_K6, CPU_K6, PTA_MMX | PTA_3DNOW},
  {"athlon", PROCESSOR_ATHLON, CPU_ATHLON,
    PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_PREFETCH_SSE},
  {"athlon-tbird", PROCESSOR_ATHLON, CPU_ATHLON,
    PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_PREFETCH_SSE},
  {"athlon-4", PROCESSOR_ATHLON, CPU_ATHLON,
    PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_SSE | PTA_FXSR},
  {"athlon-xp", PROCESSOR_ATHLON, CPU_ATHLON,
    PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_SSE | PTA_FXSR},
  {"athlon-mp", PROCESSOR_ATHLON, CPU_ATHLON,
    PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_SSE | PTA_FXSR},
  {"x86-64", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_NO_SAHF | PTA_FXSR},
  {"eden-x2", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3 | PTA_FXSR},
  {"nano", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3
      | PTA_SSSE3 | PTA_FXSR},
  {"nano-1000", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3
      | PTA_SSSE3 | PTA_FXSR},
  {"nano-2000", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3
      | PTA_SSSE3 | PTA_FXSR},
  {"nano-3000", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3
      | PTA_SSSE3 | PTA_SSE4_1 | PTA_FXSR},
  {"nano-x2", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3
      | PTA_SSSE3 | PTA_SSE4_1 | PTA_FXSR},
  {"eden-x4", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3
      | PTA_SSSE3 | PTA_SSE4_1 | PTA_FXSR},
  {"nano-x4", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3
      | PTA_SSSE3 | PTA_SSE4_1 | PTA_FXSR},
  {"k8", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_SSE
      | PTA_SSE2 | PTA_NO_SAHF | PTA_FXSR},
  {"k8-sse3", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_SSE
      | PTA_SSE2 | PTA_SSE3 | PTA_NO_SAHF | PTA_FXSR},
  {"opteron", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_SSE
      | PTA_SSE2 | PTA_NO_SAHF | PTA_FXSR},
  {"opteron-sse3", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_SSE
      | PTA_SSE2 | PTA_SSE3 | PTA_NO_SAHF | PTA_FXSR},
  {"athlon64", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_SSE
      | PTA_SSE2 | PTA_NO_SAHF | PTA_FXSR},
  {"athlon64-sse3", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_SSE
      | PTA_SSE2 | PTA_SSE3 | PTA_NO_SAHF | PTA_FXSR},
  {"athlon-fx", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_SSE
      | PTA_SSE2 | PTA_NO_SAHF | PTA_FXSR},
  {"amdfam10", PROCESSOR_AMDFAM10, CPU_AMDFAM10,
    PTA_64BIT | PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_SSE | PTA_SSE2
      | PTA_SSE3 | PTA_SSE4A | PTA_CX16 | PTA_ABM | PTA_PRFCHW | PTA_FXSR},
  {"barcelona", PROCESSOR_AMDFAM10, CPU_AMDFAM10,
    PTA_64BIT | PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_SSE | PTA_SSE2
      | PTA_SSE3 | PTA_SSE4A | PTA_CX16 | PTA_ABM | PTA_PRFCHW | PTA_FXSR},
  {"bdver1", PROCESSOR_BDVER1, CPU_BDVER1,
    PTA_64BIT | PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3
      | PTA_SSE4A | PTA_CX16 | PTA_ABM | PTA_SSSE3 | PTA_SSE4_1
      | PTA_SSE4_2 | PTA_AES | PTA_PCLMUL | PTA_AVX | PTA_FMA4
      | PTA_XOP | PTA_LWP | PTA_PRFCHW | PTA_FXSR | PTA_XSAVE},
  {"bdver2", PROCESSOR_BDVER2, CPU_BDVER2,
    PTA_64BIT | PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3
      | PTA_SSE4A | PTA_CX16 | PTA_ABM | PTA_SSSE3 | PTA_SSE4_1
      | PTA_SSE4_2 | PTA_AES | PTA_PCLMUL | PTA_AVX | PTA_FMA4
      | PTA_XOP | PTA_LWP | PTA_BMI | PTA_TBM | PTA_F16C
      | PTA_FMA | PTA_PRFCHW | PTA_FXSR | PTA_XSAVE},
  {"bdver3", PROCESSOR_BDVER3, CPU_BDVER3,
    PTA_64BIT | PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3
      | PTA_SSE4A | PTA_CX16 | PTA_ABM | PTA_SSSE3 | PTA_SSE4_1
      | PTA_SSE4_2 | PTA_AES | PTA_PCLMUL | PTA_AVX | PTA_FMA4
      | PTA_XOP | PTA_LWP | PTA_BMI | PTA_TBM | PTA_F16C
      | PTA_FMA | PTA_PRFCHW | PTA_FXSR | PTA_XSAVE
      | PTA_XSAVEOPT | PTA_FSGSBASE},
  {"bdver4", PROCESSOR_BDVER4, CPU_BDVER4,
    PTA_64BIT | PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3
      | PTA_SSE4A | PTA_CX16 | PTA_ABM | PTA_SSSE3 | PTA_SSE4_1
      | PTA_SSE4_2 | PTA_AES | PTA_PCLMUL | PTA_AVX | PTA_AVX2
      | PTA_FMA4 | PTA_XOP | PTA_LWP | PTA_BMI | PTA_BMI2
      | PTA_TBM | PTA_F16C | PTA_FMA | PTA_PRFCHW | PTA_FXSR
      | PTA_XSAVE | PTA_XSAVEOPT | PTA_FSGSBASE | PTA_RDRND
      | PTA_MOVBE | PTA_MWAITX},
  {"znver1", PROCESSOR_ZNVER1, CPU_ZNVER1,
    PTA_64BIT | PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3
      | PTA_SSE4A | PTA_CX16 | PTA_ABM | PTA_SSSE3 | PTA_SSE4_1
      | PTA_SSE4_2 | PTA_AES | PTA_PCLMUL | PTA_AVX | PTA_AVX2
      | PTA_BMI | PTA_BMI2 | PTA_F16C | PTA_FMA | PTA_PRFCHW
      | PTA_FXSR | PTA_XSAVE | PTA_XSAVEOPT | PTA_FSGSBASE
      | PTA_RDRND | PTA_MOVBE | PTA_MWAITX | PTA_ADX | PTA_RDSEED
      | PTA_CLZERO | PTA_CLFLUSHOPT | PTA_XSAVEC | PTA_XSAVES
      | PTA_SHA | PTA_LZCNT | PTA_POPCNT},
  {"btver1", PROCESSOR_BTVER1, CPU_GENERIC,
    PTA_64BIT | PTA_MMX |  PTA_SSE  | PTA_SSE2 | PTA_SSE3
      | PTA_SSSE3 | PTA_SSE4A |PTA_ABM | PTA_CX16 | PTA_PRFCHW
      | PTA_FXSR | PTA_XSAVE},
  {"btver2", PROCESSOR_BTVER2, CPU_BTVER2,
    PTA_64BIT | PTA_MMX |  PTA_SSE  | PTA_SSE2 | PTA_SSE3
      | PTA_SSSE3 | PTA_SSE4A |PTA_ABM | PTA_CX16 | PTA_SSE4_1
      | PTA_SSE4_2 | PTA_AES | PTA_PCLMUL | PTA_AVX
      | PTA_BMI | PTA_F16C | PTA_MOVBE | PTA_PRFCHW
      | PTA_FXSR | PTA_XSAVE | PTA_XSAVEOPT},

  {"generic", PROCESSOR_GENERIC, CPU_GENERIC,
    PTA_64BIT
      | PTA_HLE /* flags are only used for -march switch.  */ },
};

int const pta_size = ARRAY_SIZE (processor_alias_table);

/* Provide valid option values for -march and -mtune options.  */

vec<const char *>
ix86_get_valid_option_values (int option_code,
			      const char *prefix ATTRIBUTE_UNUSED)
{
  vec<const char *> v;
  v.create (0);
  opt_code opt = (opt_code) option_code;

  switch (opt)
    {
    case OPT_march_:
      for (unsigned i = 0; i < pta_size; i++)
	v.safe_push (processor_alias_table[i].name);
      break;
    case OPT_mtune_:
      for (unsigned i = 0; i < PROCESSOR_max; i++)
	v.safe_push (processor_names[i]);
      break;
    default:
      break;
    }

  return v;
}

#undef  TARGET_GET_VALID_OPTION_VALUES
#define TARGET_GET_VALID_OPTION_VALUES ix86_get_valid_option_values

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
