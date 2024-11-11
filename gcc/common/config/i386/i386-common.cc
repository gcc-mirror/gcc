/* IA-32 common hooks.
   Copyright (C) 1988-2024 Free Software Foundation, Inc.

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
#define OPTION_MASK_ISA_AVX512DQ_SET \
  (OPTION_MASK_ISA_AVX512DQ | OPTION_MASK_ISA_AVX512F_SET)
#define OPTION_MASK_ISA_AVX512BW_SET \
  (OPTION_MASK_ISA_AVX512BW | OPTION_MASK_ISA_AVX512F_SET)
#define OPTION_MASK_ISA_AVX512VL_SET \
  (OPTION_MASK_ISA_AVX512VL | OPTION_MASK_ISA_AVX512F_SET)
#define OPTION_MASK_ISA_AVX512IFMA_SET \
  (OPTION_MASK_ISA_AVX512IFMA | OPTION_MASK_ISA_AVX512F_SET)
#define OPTION_MASK_ISA2_AVXIFMA_SET OPTION_MASK_ISA2_AVXIFMA
#define OPTION_MASK_ISA_AVX512VBMI_SET \
  (OPTION_MASK_ISA_AVX512VBMI | OPTION_MASK_ISA_AVX512BW_SET)
#define OPTION_MASK_ISA_AVX512VBMI2_SET \
  (OPTION_MASK_ISA_AVX512VBMI2 | OPTION_MASK_ISA_AVX512BW_SET)
#define OPTION_MASK_ISA_AVX512FP16_SET OPTION_MASK_ISA_AVX512BW_SET
#define OPTION_MASK_ISA2_AVX512FP16_SET OPTION_MASK_ISA2_AVX512FP16
#define OPTION_MASK_ISA_AVX512VNNI_SET \
  (OPTION_MASK_ISA_AVX512VNNI | OPTION_MASK_ISA_AVX512F_SET)
#define OPTION_MASK_ISA2_AVXVNNI_SET OPTION_MASK_ISA2_AVXVNNI
#define OPTION_MASK_ISA_AVX512VPOPCNTDQ_SET \
  (OPTION_MASK_ISA_AVX512VPOPCNTDQ | OPTION_MASK_ISA_AVX512F_SET)
#define OPTION_MASK_ISA_AVX512BITALG_SET \
  (OPTION_MASK_ISA_AVX512BITALG | OPTION_MASK_ISA_AVX512BW_SET)
#define OPTION_MASK_ISA2_AVX512BF16_SET OPTION_MASK_ISA2_AVX512BF16
#define OPTION_MASK_ISA_RTM_SET OPTION_MASK_ISA_RTM
#define OPTION_MASK_ISA_PRFCHW_SET OPTION_MASK_ISA_PRFCHW
#define OPTION_MASK_ISA_RDSEED_SET OPTION_MASK_ISA_RDSEED
#define OPTION_MASK_ISA_ADX_SET OPTION_MASK_ISA_ADX
#define OPTION_MASK_ISA_CLFLUSHOPT_SET OPTION_MASK_ISA_CLFLUSHOPT
#define OPTION_MASK_ISA_XSAVES_SET \
  (OPTION_MASK_ISA_XSAVES | OPTION_MASK_ISA_XSAVE_SET)
#define OPTION_MASK_ISA_XSAVEC_SET \
  (OPTION_MASK_ISA_XSAVEC | OPTION_MASK_ISA_XSAVE_SET)
#define OPTION_MASK_ISA_CLWB_SET OPTION_MASK_ISA_CLWB
#define OPTION_MASK_ISA2_AVX512VP2INTERSECT_SET OPTION_MASK_ISA2_AVX512VP2INTERSECT
#define OPTION_MASK_ISA2_AMX_TILE_SET OPTION_MASK_ISA2_AMX_TILE
#define OPTION_MASK_ISA2_AMX_INT8_SET \
  (OPTION_MASK_ISA2_AMX_TILE_SET | OPTION_MASK_ISA2_AMX_INT8)
#define OPTION_MASK_ISA2_AMX_BF16_SET \
  (OPTION_MASK_ISA2_AMX_TILE_SET | OPTION_MASK_ISA2_AMX_BF16)
#define OPTION_MASK_ISA2_AVXVNNIINT8_SET OPTION_MASK_ISA2_AVXVNNIINT8
#define OPTION_MASK_ISA2_AVXNECONVERT_SET OPTION_MASK_ISA2_AVXNECONVERT
#define OPTION_MASK_ISA2_CMPCCXADD_SET OPTION_MASK_ISA2_CMPCCXADD
#define OPTION_MASK_ISA2_AMX_FP16_SET \
  (OPTION_MASK_ISA2_AMX_TILE_SET | OPTION_MASK_ISA2_AMX_FP16)
#define OPTION_MASK_ISA2_PREFETCHI_SET OPTION_MASK_ISA2_PREFETCHI
#define OPTION_MASK_ISA2_RAOINT_SET OPTION_MASK_ISA2_RAOINT
#define OPTION_MASK_ISA2_AMX_COMPLEX_SET \
  (OPTION_MASK_ISA2_AMX_TILE_SET | OPTION_MASK_ISA2_AMX_COMPLEX)
#define OPTION_MASK_ISA2_AVXVNNIINT16_SET OPTION_MASK_ISA2_AVXVNNIINT16
#define OPTION_MASK_ISA2_SM3_SET OPTION_MASK_ISA2_SM3
#define OPTION_MASK_ISA2_SHA512_SET OPTION_MASK_ISA2_SHA512
#define OPTION_MASK_ISA2_SM4_SET OPTION_MASK_ISA2_SM4
#define OPTION_MASK_ISA2_APX_F_SET OPTION_MASK_ISA2_APX_F
#define OPTION_MASK_ISA2_EVEX512_SET OPTION_MASK_ISA2_EVEX512
#define OPTION_MASK_ISA2_USER_MSR_SET OPTION_MASK_ISA2_USER_MSR
#define OPTION_MASK_ISA2_AVX10_1_256_SET OPTION_MASK_ISA2_AVX10_1_256
#define OPTION_MASK_ISA2_AVX10_1_512_SET \
  (OPTION_MASK_ISA2_AVX10_1_256_SET | OPTION_MASK_ISA2_AVX10_1_512)
#define OPTION_MASK_ISA2_AVX10_2_256_SET \
  (OPTION_MASK_ISA2_AVX10_1_256_SET | OPTION_MASK_ISA2_AVX10_2_256)
#define OPTION_MASK_ISA2_AVX10_2_512_SET \
  (OPTION_MASK_ISA2_AVX10_1_512_SET | OPTION_MASK_ISA2_AVX10_2_256_SET \
   | OPTION_MASK_ISA2_AVX10_2_512)
#define OPTION_MASK_ISA2_AMX_AVX512_SET \
  (OPTION_MASK_ISA2_AMX_TILE_SET | OPTION_MASK_ISA2_AVX10_2_512_SET \
   | OPTION_MASK_ISA2_AMX_AVX512)
#define OPTION_MASK_ISA2_AMX_TF32_SET \
  (OPTION_MASK_ISA2_AMX_TILE_SET | OPTION_MASK_ISA2_AMX_TF32)
#define OPTION_MASK_ISA2_AMX_TRANSPOSE_SET \
  (OPTION_MASK_ISA2_AMX_TILE_SET | OPTION_MASK_ISA2_AMX_TRANSPOSE)
#define OPTION_MASK_ISA2_AMX_FP8_SET \
  (OPTION_MASK_ISA2_AMX_TILE_SET | OPTION_MASK_ISA2_AMX_FP8)
#define OPTION_MASK_ISA2_MOVRS_SET OPTION_MASK_ISA2_MOVRS
#define OPTION_MASK_ISA2_AMX_MOVRS_SET \
  (OPTION_MASK_ISA2_AMX_TILE_SET | OPTION_MASK_ISA2_AMX_MOVRS)

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
  (OPTION_MASK_ISA_ABM | OPTION_MASK_ISA_POPCNT_SET)

#define OPTION_MASK_ISA2_PCONFIG_SET OPTION_MASK_ISA2_PCONFIG
#define OPTION_MASK_ISA2_WBNOINVD_SET OPTION_MASK_ISA2_WBNOINVD
#define OPTION_MASK_ISA2_SGX_SET OPTION_MASK_ISA2_SGX
#define OPTION_MASK_ISA_BMI_SET OPTION_MASK_ISA_BMI
#define OPTION_MASK_ISA_BMI2_SET OPTION_MASK_ISA_BMI2
#define OPTION_MASK_ISA_LZCNT_SET OPTION_MASK_ISA_LZCNT
#define OPTION_MASK_ISA_TBM_SET OPTION_MASK_ISA_TBM
#define OPTION_MASK_ISA_POPCNT_SET OPTION_MASK_ISA_POPCNT
#define OPTION_MASK_ISA2_CX16_SET OPTION_MASK_ISA2_CX16
#define OPTION_MASK_ISA_SAHF_SET OPTION_MASK_ISA_SAHF
#define OPTION_MASK_ISA2_MOVBE_SET OPTION_MASK_ISA2_MOVBE
#define OPTION_MASK_ISA_CRC32_SET OPTION_MASK_ISA_CRC32

#define OPTION_MASK_ISA_FSGSBASE_SET OPTION_MASK_ISA_FSGSBASE
#define OPTION_MASK_ISA_RDRND_SET OPTION_MASK_ISA_RDRND
#define OPTION_MASK_ISA2_PTWRITE_SET OPTION_MASK_ISA2_PTWRITE
#define OPTION_MASK_ISA_F16C_SET \
  (OPTION_MASK_ISA_F16C | OPTION_MASK_ISA_AVX_SET)
#define OPTION_MASK_ISA2_MWAITX_SET OPTION_MASK_ISA2_MWAITX
#define OPTION_MASK_ISA2_MWAIT_SET OPTION_MASK_ISA2_MWAIT
#define OPTION_MASK_ISA2_CLZERO_SET OPTION_MASK_ISA2_CLZERO
#define OPTION_MASK_ISA_PKU_SET OPTION_MASK_ISA_PKU
#define OPTION_MASK_ISA2_RDPID_SET OPTION_MASK_ISA2_RDPID
#define OPTION_MASK_ISA_GFNI_SET OPTION_MASK_ISA_GFNI
#define OPTION_MASK_ISA_SHSTK_SET OPTION_MASK_ISA_SHSTK
#define OPTION_MASK_ISA2_VAES_SET OPTION_MASK_ISA2_VAES
#define OPTION_MASK_ISA_VPCLMULQDQ_SET \
  (OPTION_MASK_ISA_VPCLMULQDQ | OPTION_MASK_ISA_PCLMUL_SET \
   | OPTION_MASK_ISA_AVX_SET)
#define OPTION_MASK_ISA_MOVDIRI_SET OPTION_MASK_ISA_MOVDIRI
#define OPTION_MASK_ISA2_MOVDIR64B_SET OPTION_MASK_ISA2_MOVDIR64B
#define OPTION_MASK_ISA2_WAITPKG_SET OPTION_MASK_ISA2_WAITPKG
#define OPTION_MASK_ISA2_CLDEMOTE_SET OPTION_MASK_ISA2_CLDEMOTE
#define OPTION_MASK_ISA2_ENQCMD_SET OPTION_MASK_ISA2_ENQCMD
#define OPTION_MASK_ISA2_SERIALIZE_SET OPTION_MASK_ISA2_SERIALIZE
#define OPTION_MASK_ISA2_TSXLDTRK_SET OPTION_MASK_ISA2_TSXLDTRK
#define OPTION_MASK_ISA2_UINTR_SET OPTION_MASK_ISA2_UINTR
#define OPTION_MASK_ISA2_HRESET_SET OPTION_MASK_ISA2_HRESET
#define OPTION_MASK_ISA2_KL_SET OPTION_MASK_ISA2_KL
#define OPTION_MASK_ISA2_WIDEKL_SET \
  (OPTION_MASK_ISA2_WIDEKL | OPTION_MASK_ISA2_KL_SET)

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
   | OPTION_MASK_ISA_AVX2_UNSET | OPTION_MASK_ISA_VPCLMULQDQ_UNSET)
#define OPTION_MASK_ISA_FMA_UNSET OPTION_MASK_ISA_FMA
#define OPTION_MASK_ISA_FXSR_UNSET OPTION_MASK_ISA_FXSR
#define OPTION_MASK_ISA_XSAVE_UNSET \
  (OPTION_MASK_ISA_XSAVE | OPTION_MASK_ISA_XSAVEOPT_UNSET \
   | OPTION_MASK_ISA_XSAVES_UNSET | OPTION_MASK_ISA_XSAVEC_UNSET \
   | OPTION_MASK_ISA_AVX_UNSET)
#define OPTION_MASK_ISA2_XSAVE_UNSET \
  (OPTION_MASK_ISA2_AVX2_UNSET | OPTION_MASK_ISA2_AMX_TILE_UNSET)
#define OPTION_MASK_ISA_XSAVEOPT_UNSET OPTION_MASK_ISA_XSAVEOPT
#define OPTION_MASK_ISA_AVX2_UNSET \
  (OPTION_MASK_ISA_AVX2 | OPTION_MASK_ISA_AVX512F_UNSET)
#define OPTION_MASK_ISA2_AVX2_UNSET \
  (OPTION_MASK_ISA2_AVXIFMA_UNSET | OPTION_MASK_ISA2_AVXVNNI_UNSET \
   | OPTION_MASK_ISA2_AVXVNNIINT8_UNSET | OPTION_MASK_ISA2_AVXNECONVERT_UNSET \
   | OPTION_MASK_ISA2_AVXVNNIINT16_UNSET | OPTION_MASK_ISA2_AVX512F_UNSET \
   | OPTION_MASK_ISA2_AVX10_1_256_UNSET)
#define OPTION_MASK_ISA_AVX512F_UNSET \
  (OPTION_MASK_ISA_AVX512F | OPTION_MASK_ISA_AVX512CD_UNSET \
   | OPTION_MASK_ISA_AVX512DQ_UNSET | OPTION_MASK_ISA_AVX512BW_UNSET \
   | OPTION_MASK_ISA_AVX512VL_UNSET | OPTION_MASK_ISA_AVX512IFMA_UNSET \
   | OPTION_MASK_ISA_AVX512VNNI_UNSET \
   | OPTION_MASK_ISA_AVX512VPOPCNTDQ_UNSET)
#define OPTION_MASK_ISA_AVX512CD_UNSET OPTION_MASK_ISA_AVX512CD
#define OPTION_MASK_ISA_AVX512DQ_UNSET OPTION_MASK_ISA_AVX512DQ
#define OPTION_MASK_ISA_AVX512BW_UNSET \
  (OPTION_MASK_ISA_AVX512BW | OPTION_MASK_ISA_AVX512VBMI_UNSET \
   | OPTION_MASK_ISA_AVX512VBMI2_UNSET | OPTION_MASK_ISA_AVX512BITALG_UNSET)
#define OPTION_MASK_ISA_AVX512VL_UNSET OPTION_MASK_ISA_AVX512VL
#define OPTION_MASK_ISA_AVX512IFMA_UNSET OPTION_MASK_ISA_AVX512IFMA
#define OPTION_MASK_ISA2_AVXIFMA_UNSET OPTION_MASK_ISA2_AVXIFMA
#define OPTION_MASK_ISA_AVX512VBMI_UNSET OPTION_MASK_ISA_AVX512VBMI
#define OPTION_MASK_ISA_AVX512VBMI2_UNSET OPTION_MASK_ISA_AVX512VBMI2
#define OPTION_MASK_ISA_AVX512FP16_UNSET OPTION_MASK_ISA_AVX512BW_UNSET
#define OPTION_MASK_ISA2_AVX512FP16_UNSET OPTION_MASK_ISA2_AVX512FP16
#define OPTION_MASK_ISA_AVX512VNNI_UNSET OPTION_MASK_ISA_AVX512VNNI
#define OPTION_MASK_ISA2_AVXVNNI_UNSET OPTION_MASK_ISA2_AVXVNNI
#define OPTION_MASK_ISA_AVX512VPOPCNTDQ_UNSET OPTION_MASK_ISA_AVX512VPOPCNTDQ
#define OPTION_MASK_ISA_AVX512BITALG_UNSET OPTION_MASK_ISA_AVX512BITALG
#define OPTION_MASK_ISA2_AVX512BF16_UNSET OPTION_MASK_ISA2_AVX512BF16
#define OPTION_MASK_ISA_RTM_UNSET OPTION_MASK_ISA_RTM
#define OPTION_MASK_ISA_PRFCHW_UNSET OPTION_MASK_ISA_PRFCHW
#define OPTION_MASK_ISA_RDSEED_UNSET OPTION_MASK_ISA_RDSEED
#define OPTION_MASK_ISA_ADX_UNSET OPTION_MASK_ISA_ADX
#define OPTION_MASK_ISA_CLFLUSHOPT_UNSET OPTION_MASK_ISA_CLFLUSHOPT
#define OPTION_MASK_ISA_XSAVEC_UNSET OPTION_MASK_ISA_XSAVEC
#define OPTION_MASK_ISA_XSAVES_UNSET OPTION_MASK_ISA_XSAVES
#define OPTION_MASK_ISA_CLWB_UNSET OPTION_MASK_ISA_CLWB
#define OPTION_MASK_ISA2_MWAITX_UNSET OPTION_MASK_ISA2_MWAITX
#define OPTION_MASK_ISA2_MWAIT_UNSET OPTION_MASK_ISA2_MWAIT
#define OPTION_MASK_ISA2_CLZERO_UNSET OPTION_MASK_ISA2_CLZERO
#define OPTION_MASK_ISA_PKU_UNSET OPTION_MASK_ISA_PKU
#define OPTION_MASK_ISA2_RDPID_UNSET OPTION_MASK_ISA2_RDPID
#define OPTION_MASK_ISA_GFNI_UNSET OPTION_MASK_ISA_GFNI
#define OPTION_MASK_ISA_SHSTK_UNSET OPTION_MASK_ISA_SHSTK
#define OPTION_MASK_ISA2_VAES_UNSET OPTION_MASK_ISA2_VAES
#define OPTION_MASK_ISA_VPCLMULQDQ_UNSET OPTION_MASK_ISA_VPCLMULQDQ
#define OPTION_MASK_ISA_MOVDIRI_UNSET OPTION_MASK_ISA_MOVDIRI
#define OPTION_MASK_ISA2_MOVDIR64B_UNSET OPTION_MASK_ISA2_MOVDIR64B
#define OPTION_MASK_ISA2_WAITPKG_UNSET OPTION_MASK_ISA2_WAITPKG
#define OPTION_MASK_ISA2_CLDEMOTE_UNSET OPTION_MASK_ISA2_CLDEMOTE
#define OPTION_MASK_ISA2_ENQCMD_UNSET OPTION_MASK_ISA2_ENQCMD
#define OPTION_MASK_ISA2_SERIALIZE_UNSET OPTION_MASK_ISA2_SERIALIZE
#define OPTION_MASK_ISA2_AVX512VP2INTERSECT_UNSET OPTION_MASK_ISA2_AVX512VP2INTERSECT
#define OPTION_MASK_ISA2_TSXLDTRK_UNSET OPTION_MASK_ISA2_TSXLDTRK
#define OPTION_MASK_ISA2_AMX_TILE_UNSET \
  (OPTION_MASK_ISA2_AMX_TILE | OPTION_MASK_ISA2_AMX_INT8_UNSET \
   | OPTION_MASK_ISA2_AMX_BF16_UNSET | OPTION_MASK_ISA2_AMX_FP16_UNSET \
   | OPTION_MASK_ISA2_AMX_COMPLEX_UNSET | OPTION_MASK_ISA2_AMX_AVX512_UNSET \
   | OPTION_MASK_ISA2_AMX_TF32_UNSET | OPTION_MASK_ISA2_AMX_TRANSPOSE_UNSET \
   | OPTION_MASK_ISA2_AMX_FP8_UNSET | OPTION_MASK_ISA2_AMX_MOVRS_UNSET)
#define OPTION_MASK_ISA2_AMX_INT8_UNSET OPTION_MASK_ISA2_AMX_INT8
#define OPTION_MASK_ISA2_AMX_BF16_UNSET OPTION_MASK_ISA2_AMX_BF16
#define OPTION_MASK_ISA2_UINTR_UNSET OPTION_MASK_ISA2_UINTR
#define OPTION_MASK_ISA2_HRESET_UNSET OPTION_MASK_ISA2_HRESET
#define OPTION_MASK_ISA2_KL_UNSET \
  (OPTION_MASK_ISA2_KL | OPTION_MASK_ISA2_WIDEKL_UNSET)
#define OPTION_MASK_ISA2_WIDEKL_UNSET OPTION_MASK_ISA2_WIDEKL
#define OPTION_MASK_ISA2_AVXVNNIINT8_UNSET OPTION_MASK_ISA2_AVXVNNIINT8
#define OPTION_MASK_ISA2_AVXNECONVERT_UNSET OPTION_MASK_ISA2_AVXNECONVERT
#define OPTION_MASK_ISA2_CMPCCXADD_UNSET OPTION_MASK_ISA2_CMPCCXADD
#define OPTION_MASK_ISA2_AMX_FP16_UNSET OPTION_MASK_ISA2_AMX_FP16
#define OPTION_MASK_ISA2_PREFETCHI_UNSET OPTION_MASK_ISA2_PREFETCHI
#define OPTION_MASK_ISA2_RAOINT_UNSET OPTION_MASK_ISA2_RAOINT
#define OPTION_MASK_ISA2_AMX_COMPLEX_UNSET OPTION_MASK_ISA2_AMX_COMPLEX
#define OPTION_MASK_ISA2_AVXVNNIINT16_UNSET OPTION_MASK_ISA2_AVXVNNIINT16
#define OPTION_MASK_ISA2_SM3_UNSET OPTION_MASK_ISA2_SM3
#define OPTION_MASK_ISA2_SHA512_UNSET OPTION_MASK_ISA2_SHA512
#define OPTION_MASK_ISA2_SM4_UNSET OPTION_MASK_ISA2_SM4
#define OPTION_MASK_ISA2_APX_F_UNSET OPTION_MASK_ISA2_APX_F
#define OPTION_MASK_ISA2_EVEX512_UNSET OPTION_MASK_ISA2_EVEX512
#define OPTION_MASK_ISA2_USER_MSR_UNSET OPTION_MASK_ISA2_USER_MSR
#define OPTION_MASK_ISA2_AVX10_1_256_UNSET \
  (OPTION_MASK_ISA2_AVX10_1_256 | OPTION_MASK_ISA2_AVX10_1_512_UNSET \
   | OPTION_MASK_ISA2_AVX10_2_256_UNSET)
#define OPTION_MASK_ISA2_AVX10_1_512_UNSET \
  (OPTION_MASK_ISA2_AVX10_1_512 | OPTION_MASK_ISA2_AVX10_2_512_UNSET)
#define OPTION_MASK_ISA2_AVX10_2_256_UNSET OPTION_MASK_ISA2_AVX10_2_256
#define OPTION_MASK_ISA2_AVX10_2_512_UNSET \
  (OPTION_MASK_ISA2_AVX10_2_512 | OPTION_MASK_ISA2_AMX_AVX512_UNSET)
#define OPTION_MASK_ISA2_AMX_AVX512_UNSET OPTION_MASK_ISA2_AMX_AVX512
#define OPTION_MASK_ISA2_AMX_TF32_UNSET OPTION_MASK_ISA2_AMX_TF32
#define OPTION_MASK_ISA2_AMX_TRANSPOSE_UNSET OPTION_MASK_ISA2_AMX_TRANSPOSE
#define OPTION_MASK_ISA2_AMX_FP8_UNSET OPTION_MASK_ISA2_AMX_FP8
#define OPTION_MASK_ISA2_MOVRS_UNSET OPTION_MASK_ISA2_MOVRS
#define OPTION_MASK_ISA2_AMX_MOVRS_UNSET OPTION_MASK_ISA2_AMX_MOVRS

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
#define OPTION_MASK_ISA_PCLMUL_UNSET \
  (OPTION_MASK_ISA_PCLMUL | OPTION_MASK_ISA_VPCLMULQDQ_UNSET)
#define OPTION_MASK_ISA_ABM_UNSET OPTION_MASK_ISA_ABM
#define OPTION_MASK_ISA2_PCONFIG_UNSET OPTION_MASK_ISA2_PCONFIG
#define OPTION_MASK_ISA2_WBNOINVD_UNSET OPTION_MASK_ISA2_WBNOINVD
#define OPTION_MASK_ISA2_SGX_UNSET OPTION_MASK_ISA2_SGX
#define OPTION_MASK_ISA_BMI_UNSET OPTION_MASK_ISA_BMI
#define OPTION_MASK_ISA_BMI2_UNSET OPTION_MASK_ISA_BMI2
#define OPTION_MASK_ISA_LZCNT_UNSET OPTION_MASK_ISA_LZCNT
#define OPTION_MASK_ISA_TBM_UNSET OPTION_MASK_ISA_TBM
#define OPTION_MASK_ISA_POPCNT_UNSET OPTION_MASK_ISA_POPCNT
#define OPTION_MASK_ISA2_CX16_UNSET OPTION_MASK_ISA2_CX16
#define OPTION_MASK_ISA_SAHF_UNSET OPTION_MASK_ISA_SAHF
#define OPTION_MASK_ISA2_MOVBE_UNSET OPTION_MASK_ISA2_MOVBE
#define OPTION_MASK_ISA_CRC32_UNSET OPTION_MASK_ISA_CRC32

#define OPTION_MASK_ISA_FSGSBASE_UNSET OPTION_MASK_ISA_FSGSBASE
#define OPTION_MASK_ISA_RDRND_UNSET OPTION_MASK_ISA_RDRND
#define OPTION_MASK_ISA2_PTWRITE_UNSET OPTION_MASK_ISA2_PTWRITE
#define OPTION_MASK_ISA_F16C_UNSET OPTION_MASK_ISA_F16C

#define OPTION_MASK_ISA_GENERAL_REGS_ONLY_UNSET \
  (OPTION_MASK_ISA_MMX_UNSET \
   | OPTION_MASK_ISA_SSE_UNSET)

#define OPTION_MASK_ISA2_AVX512F_UNSET \
  (OPTION_MASK_ISA2_AVX512BW_UNSET \
   | OPTION_MASK_ISA2_AVX512VP2INTERSECT_UNSET)
#define OPTION_MASK_ISA2_GENERAL_REGS_ONLY_UNSET \
  OPTION_MASK_ISA2_SSE_UNSET
#define OPTION_MASK_ISA2_AVX_UNSET \
  (OPTION_MASK_ISA2_AVX2_UNSET | OPTION_MASK_ISA2_VAES_UNSET \
   | OPTION_MASK_ISA2_SM3_UNSET | OPTION_MASK_ISA2_SHA512_UNSET \
   | OPTION_MASK_ISA2_SM4_UNSET)
#define OPTION_MASK_ISA2_SSE4_2_UNSET OPTION_MASK_ISA2_AVX_UNSET
#define OPTION_MASK_ISA2_SSE4_1_UNSET OPTION_MASK_ISA2_SSE4_2_UNSET
#define OPTION_MASK_ISA2_SSE4_UNSET OPTION_MASK_ISA2_SSE4_1_UNSET
#define OPTION_MASK_ISA2_SSSE3_UNSET OPTION_MASK_ISA2_SSE4_1_UNSET
#define OPTION_MASK_ISA2_SSE3_UNSET OPTION_MASK_ISA2_SSSE3_UNSET
#define OPTION_MASK_ISA2_SSE2_UNSET \
  (OPTION_MASK_ISA2_SSE3_UNSET | OPTION_MASK_ISA2_KL_UNSET)
#define OPTION_MASK_ISA2_SSE_UNSET OPTION_MASK_ISA2_SSE2_UNSET

#define OPTION_MASK_ISA2_AVX512BW_UNSET \
  (OPTION_MASK_ISA2_AVX512BF16_UNSET \
    | OPTION_MASK_ISA2_AVX512FP16_UNSET)

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
	  HOST_WIDE_INT general_regs_only_flags = 0;
	  HOST_WIDE_INT general_regs_only_flags2 = 0;

	  /* NB: Enable the GPR only instructions which are enabled
	     implicitly by SSE ISAs unless they have been disabled
	     explicitly.  */
	  if (TARGET_SSE4_2_P (opts->x_ix86_isa_flags))
	    {
	      if (!TARGET_EXPLICIT_CRC32_P (opts))
		general_regs_only_flags |= OPTION_MASK_ISA_CRC32;
	      if (!TARGET_EXPLICIT_POPCNT_P (opts))
		general_regs_only_flags |= OPTION_MASK_ISA_POPCNT;
	    }
	  if (TARGET_SSE3_P (opts->x_ix86_isa_flags))
	    {
	      if (!TARGET_EXPLICIT_MWAIT_P (opts))
		general_regs_only_flags2 |= OPTION_MASK_ISA2_MWAIT;
	    }

	  /* Disable MMX, SSE and x87 instructions if only
	     general registers are allowed.  */
	  opts->x_ix86_isa_flags
	    &= ~OPTION_MASK_ISA_GENERAL_REGS_ONLY_UNSET;
	  opts->x_ix86_isa_flags2
	    &= ~OPTION_MASK_ISA2_GENERAL_REGS_ONLY_UNSET;
	  opts->x_ix86_isa_flags |= general_regs_only_flags;
	  opts->x_ix86_isa_flags2 |= general_regs_only_flags2;
	  opts->x_ix86_isa_flags_explicit
	    |= (OPTION_MASK_ISA_GENERAL_REGS_ONLY_UNSET
		| general_regs_only_flags);
	  opts->x_ix86_isa_flags2_explicit
	    |= (OPTION_MASK_ISA2_GENERAL_REGS_ONLY_UNSET
		| general_regs_only_flags2);

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
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_SSE_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_SSE_UNSET;
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
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_SSE2_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_SSE2_UNSET;
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
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_SSE3_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_SSE3_UNSET;
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
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_SSSE3_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_SSSE3_UNSET;
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
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_SSE4_1_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_SSE4_1_UNSET;
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
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_SSE4_2_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_SSE4_2_UNSET;
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
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AVX_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVX_UNSET;
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
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AVX2_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVX2_UNSET;
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
	  opts->x_ix86_no_avx512_explicit = 1;
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
	  opts->x_ix86_no_avx512_explicit = 1;
	}
      return true;

    case OPT_mrdpid:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_RDPID_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_RDPID_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_RDPID_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_RDPID_UNSET;
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
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_VAES_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_VAES_SET;
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_VAES_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_VAES_UNSET;
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
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_MOVDIR64B_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_MOVDIR64B_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_MOVDIR64B_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_MOVDIR64B_UNSET;
	}
	return true;

    case OPT_mcldemote:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_CLDEMOTE_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_CLDEMOTE_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_CLDEMOTE_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_CLDEMOTE_UNSET;
	}
      return true;

    case OPT_mwaitpkg:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_WAITPKG_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_WAITPKG_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_WAITPKG_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_WAITPKG_UNSET;
	}
      return true;

    case OPT_menqcmd:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_ENQCMD_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_ENQCMD_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_ENQCMD_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_ENQCMD_UNSET;
	}
	return true;

    case OPT_mkl:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_KL_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_KL_SET;

	  /* The Keylocker instructions need XMM registers from SSE2.  */
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_SSE2_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE2_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_KL_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_KL_UNSET;
	}
      return true;

    case OPT_mwidekl:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_WIDEKL_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_WIDEKL_SET;

	  /* The Widekl instructions need XMM registers from SSE2.  */
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_SSE2_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_SSE2_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_WIDEKL_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_WIDEKL_UNSET;
	}
      return true;

    case OPT_mserialize:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_SERIALIZE_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_SERIALIZE_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_SERIALIZE_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_SERIALIZE_UNSET;
	}
      return true;

    case OPT_muintr:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_UINTR_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_UINTR_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_UINTR_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_UINTR_UNSET;
	}
      return true;

    case OPT_mhreset:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_HRESET_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_HRESET_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_HRESET_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_HRESET_UNSET;
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
	  opts->x_ix86_no_avx512_explicit = 1;
	}
      return true;

    case OPT_mavx512fp16:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_AVX512FP16_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVX512FP16_SET;
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX512FP16_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX512FP16_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AVX512FP16_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVX512FP16_UNSET;
	  opts->x_ix86_no_avx512_explicit = 1;
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
	  opts->x_ix86_no_avx512_explicit = 1;
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
	  opts->x_ix86_no_avx512_explicit = 1;
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
	  opts->x_ix86_no_avx512_explicit = 1;
	}
      return true;

    case OPT_mavx512bf16:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_AVX512BF16_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVX512BF16_SET;
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX512BW_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX512BW_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AVX512BF16_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVX512BF16_UNSET;
	  opts->x_ix86_no_avx512_explicit = 1;
	}
      return true;

    case OPT_mavxvnni:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_AVXVNNI_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVXVNNI_SET;
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX2_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX2_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AVXVNNI_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVXVNNI_UNSET;
	}
      return true;

    case OPT_msgx:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_SGX_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_SGX_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_SGX_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_SGX_UNSET;
	}
      return true;

    case OPT_mpconfig:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_PCONFIG_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_PCONFIG_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_PCONFIG_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_PCONFIG_UNSET;
	}
      return true;

    case OPT_mwbnoinvd:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_WBNOINVD_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_WBNOINVD_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_WBNOINVD_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_WBNOINVD_UNSET;
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
	  opts->x_ix86_no_avx512_explicit = 1;
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
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AVX512BW_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVX512BW_UNSET;
	  opts->x_ix86_no_avx512_explicit = 1;
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
	  opts->x_ix86_no_avx512_explicit = 1;
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
	  opts->x_ix86_no_avx512_explicit = 1;
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
	  opts->x_ix86_no_avx512_explicit = 1;
	}
      return true;

    case OPT_mavx512vp2intersect:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_AVX512VP2INTERSECT_SET;
	  opts->x_ix86_isa_flags2_explicit |=
	    OPTION_MASK_ISA2_AVX512VP2INTERSECT_SET;
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX512DQ_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX512DQ_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AVX512VP2INTERSECT_UNSET;
	  opts->x_ix86_isa_flags2_explicit |=
	    OPTION_MASK_ISA2_AVX512VP2INTERSECT_UNSET;
	}
      return true;

    case OPT_mtsxldtrk:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_TSXLDTRK_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_TSXLDTRK_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_TSXLDTRK_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_TSXLDTRK_UNSET;
	}
      return true;

    case OPT_mamx_tile:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_AMX_TILE_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AMX_TILE_SET;
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_XSAVE_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_XSAVE_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AMX_TILE_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AMX_TILE_UNSET;
	}
      return true;

    case OPT_mamx_int8:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_AMX_INT8_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AMX_INT8_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AMX_INT8_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AMX_INT8_UNSET;
	}
      return true;

    case OPT_mamx_bf16:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_AMX_BF16_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AMX_BF16_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AMX_BF16_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AMX_BF16_UNSET;
	}
      return true;

    case OPT_mavxifma:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_AVXIFMA_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVXIFMA_SET;
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX2_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX2_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AVXIFMA_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVXIFMA_UNSET;
	}
      return true;

    case OPT_mavxvnniint8:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_AVXVNNIINT8_SET;
	  opts->x_ix86_isa_flags2_explicit |=
	    OPTION_MASK_ISA2_AVXVNNIINT8_SET;
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX2_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX2_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &=
	    ~OPTION_MASK_ISA2_AVXVNNIINT8_UNSET;
	  opts->x_ix86_isa_flags2_explicit |=
	    OPTION_MASK_ISA2_AVXVNNIINT8_UNSET;
	}
      return true;

    case OPT_mavxneconvert:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_AVXNECONVERT_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVXNECONVERT_SET;
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX2_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX2_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AVXNECONVERT_UNSET;
	  opts->x_ix86_isa_flags2_explicit
	    |= OPTION_MASK_ISA2_AVXNECONVERT_UNSET;
	}
      return true;

    case OPT_mcmpccxadd:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_CMPCCXADD_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_CMPCCXADD_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_CMPCCXADD_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_CMPCCXADD_UNSET;
	}
      return true;

    case OPT_mamx_fp16:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_AMX_FP16_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AMX_FP16_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AMX_FP16_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AMX_FP16_UNSET;
	}
      return true;

    case OPT_mprefetchi:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_PREFETCHI_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_PREFETCHI_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_PREFETCHI_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_PREFETCHI_UNSET;
	}
      return true;

    case OPT_mraoint:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_RAOINT_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_RAOINT_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_RAOINT_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_RAOINT_UNSET;
	}
      return true;

    case OPT_mamx_complex:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_AMX_COMPLEX_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AMX_COMPLEX_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AMX_COMPLEX_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AMX_COMPLEX_UNSET;
	}
      return true;

    case OPT_mavxvnniint16:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_AVXVNNIINT16_SET;
	  opts->x_ix86_isa_flags2_explicit |=
	    OPTION_MASK_ISA2_AVXVNNIINT16_SET;
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX2_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX2_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &=
	    ~OPTION_MASK_ISA2_AVXVNNIINT16_UNSET;
	  opts->x_ix86_isa_flags2_explicit |=
	    OPTION_MASK_ISA2_AVXVNNIINT16_UNSET;
	}
      return true;

    case OPT_msm3:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_SM3_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_SM3_SET;
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_SM3_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_SM3_UNSET;
	}
      return true;

    case OPT_msha512:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_SHA512_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_SHA512_SET;
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_SHA512_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_SHA512_UNSET;
	}
      return true;

    case OPT_msm4:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_SM4_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_SM4_SET;
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_SM4_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_SM4_UNSET;
	}
      return true;

    case OPT_mapxf:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_APX_F_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_APX_F_SET;
	  opts->x_ix86_apx_features = apx_all;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_APX_F_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_APX_F_UNSET;
	  opts->x_ix86_apx_features = apx_none;
	}
      return true;

    case OPT_mevex512:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_EVEX512_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_EVEX512_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_EVEX512_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_EVEX512_UNSET;
	  opts->x_ix86_no_avx512_explicit = 1;
	}
      return true;

    case OPT_musermsr:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_USER_MSR_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_USER_MSR_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_USER_MSR_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_USER_MSR_UNSET;
	}
      return true;

    case OPT_mavx10_1_256:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_AVX10_1_256_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVX10_1_256_SET;
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX2_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX2_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AVX10_1_256_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVX10_1_256_UNSET;
	  opts->x_ix86_no_avx10_1_explicit = 1;
	}
      return true;

    case OPT_mavx10_1_512:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_AVX10_1_512_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVX10_1_512_SET;
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX2_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX2_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AVX10_1_512_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVX10_1_512_UNSET;
	  opts->x_ix86_no_avx10_1_explicit = 1;
	}
      return true;

    case OPT_mavx10_2_256:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_AVX10_2_256_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVX10_2_256_SET;
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX2_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX2_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AVX10_2_256_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVX10_2_256_UNSET;
	}
      return true;

    case OPT_mavx10_2_512:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_AVX10_2_512_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVX10_2_512_SET;
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX2_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX2_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AVX10_2_512_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AVX10_2_512_UNSET;
	}
      return true;

    case OPT_mamx_avx512:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_AMX_AVX512_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AMX_AVX512_SET;
	  opts->x_ix86_isa_flags |= OPTION_MASK_ISA_AVX2_SET;
	  opts->x_ix86_isa_flags_explicit |= OPTION_MASK_ISA_AVX2_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AMX_AVX512_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AMX_AVX512_UNSET;
	}
      return true;

    case OPT_mamx_tf32:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_AMX_TF32_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AMX_TF32_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AMX_TF32_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AMX_TF32_UNSET;
	}
      return true;

    case OPT_mamx_transpose:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_AMX_TRANSPOSE_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AMX_TRANSPOSE_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AMX_TRANSPOSE_UNSET;
	  opts->x_ix86_isa_flags2_explicit |=
	    OPTION_MASK_ISA2_AMX_TRANSPOSE_UNSET;
	}
      return true;

    case OPT_mamx_fp8:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_AMX_FP8_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AMX_FP8_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AMX_FP8_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AMX_FP8_UNSET;
	}
      return true;

    case OPT_mmovrs:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_MOVRS_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_MOVRS_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_MOVRS_UNSET;
	  opts->x_ix86_isa_flags2_explicit |=
	    OPTION_MASK_ISA2_MOVRS_UNSET;
	}
      return true;

    case OPT_mamx_movrs:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_AMX_MOVRS_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_AMX_MOVRS_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_AMX_MOVRS_UNSET;
	  opts->x_ix86_isa_flags2_explicit |=
	    OPTION_MASK_ISA2_AMX_MOVRS_UNSET;
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
      opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_SSE4_UNSET;
      opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_SSE4_UNSET;
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
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_CX16_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_CX16_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_CX16_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_CX16_UNSET;
	}
      return true;

    case OPT_mmovbe:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_MOVBE_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_MOVBE_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_MOVBE_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_MOVBE_UNSET;
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

    case OPT_mptwrite:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_PTWRITE_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_PTWRITE_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_PTWRITE_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_PTWRITE_UNSET;
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
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_XSAVE_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_XSAVE_UNSET;
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
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_MWAITX_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_MWAITX_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_MWAITX_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_MWAITX_UNSET;
	}
      return true;

    case OPT_mmwait:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_MWAIT_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_MWAIT_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_MWAIT_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_MWAIT_UNSET;
	}
      return true;

    case OPT_mclzero:
      if (value)
	{
	  opts->x_ix86_isa_flags2 |= OPTION_MASK_ISA2_CLZERO_SET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_CLZERO_SET;
	}
      else
	{
	  opts->x_ix86_isa_flags2 &= ~OPTION_MASK_ISA2_CLZERO_UNSET;
	  opts->x_ix86_isa_flags2_explicit |= OPTION_MASK_ISA2_CLZERO_UNSET;
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
      warning_at (loc, 0, "%<-malign-loops%> is obsolete, "
		  "use %<-falign-loops%>");
      if (value > MAX_CODE_ALIGN)
	error_at (loc, "%<-malign-loops=%d%> is not between 0 and %d",
		  value, MAX_CODE_ALIGN);
      else
	set_malign_value (&opts->x_str_align_loops, value);
      return true;

    case OPT_malign_jumps_:
      warning_at (loc, 0, "%<-malign-jumps%> is obsolete, "
		  "use %<-falign-jumps%>");
      if (value > MAX_CODE_ALIGN)
	error_at (loc, "%<-malign-jumps=%d%> is not between 0 and %d",
		  value, MAX_CODE_ALIGN);
      else
	set_malign_value (&opts->x_str_align_jumps, value);
      return true;

    case OPT_malign_functions_:
      warning_at (loc, 0,
		  "%<-malign-functions%> is obsolete, "
		  "use %<-falign-functions%>");
      if (value > MAX_CODE_ALIGN)
	error_at (loc, "%<-malign-functions=%d%> is not between 0 and %d",
		  value, MAX_CODE_ALIGN);
      else
	set_malign_value (&opts->x_str_align_functions, value);
      return true;

    case OPT_mbranch_cost_:
      if (value > 5)
	{
	  error_at (loc, "%<-mbranch-cost=%d%> is not between 0 and 5", value);
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

    /* Turn on -funroll-loops with -munroll-only-small-loops to enable small
       loop unrolling at -O2.  */
    { OPT_LEVELS_2_PLUS_SPEED_ONLY, OPT_funroll_loops, NULL, 1 },
    { OPT_LEVELS_2_PLUS_SPEED_ONLY, OPT_munroll_only_small_loops, NULL, 1 },
    /* Turns off -frename-registers and -fweb which are enabled by
       funroll-loops.  */
    { OPT_LEVELS_ALL, OPT_frename_registers, NULL, 0 },
    { OPT_LEVELS_ALL, OPT_fweb, NULL, 0 },
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
   field in the TCB, so they cannot be used together.  */

static bool
ix86_supports_split_stack (bool report,
			   struct gcc_options *opts ATTRIBUTE_UNUSED)
{
#if defined(TARGET_THREAD_SPLIT_STACK_OFFSET) && defined(OPTION_GLIBC_P)
  if (!OPTION_GLIBC_P (opts))
#endif
    {
      if (report)
	error ("%<-fsplit-stack%> currently only supported on GNU/Linux");
      return false;
    }

  bool ret = true;

#ifdef TARGET_THREAD_SPLIT_STACK_OFFSET
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
const char *const processor_names[] =
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
  "sierraforest",
  "grandridge",
  "clearwaterforest",
  "skylake",
  "skylake-avx512",
  "cannonlake",
  "icelake-client",
  "icelake-server",
  "cascadelake",
  "tigerlake",
  "cooperlake",
  "sapphirerapids",
  "alderlake",
  "rocketlake",
  "graniterapids",
  "graniterapids-d",
  "arrowlake",
  "arrowlake-s",
  "pantherlake",
  "diamondrapids",
  "intel",
  "lujiazui",
  "yongfeng",
  "shijidadao",
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
  "znver1",
  "znver2",
  "znver3",
  "znver4",
  "znver5"
};

/* Guarantee that the array is aligned with enum processor_type.  */
STATIC_ASSERT (ARRAY_SIZE (processor_names) == PROCESSOR_max);

const pta processor_alias_table[] =
{
  {"i386", PROCESSOR_I386, CPU_NONE, 0, 0, P_NONE},
  {"i486", PROCESSOR_I486, CPU_NONE, 0, 0, P_NONE},
  {"i586", PROCESSOR_PENTIUM, CPU_PENTIUM, 0, 0, P_NONE},
  {"pentium", PROCESSOR_PENTIUM, CPU_PENTIUM, 0, 0, P_NONE},
  {"lakemont", PROCESSOR_LAKEMONT, CPU_PENTIUM, PTA_NO_80387,
    0, P_NONE},
  {"pentium-mmx", PROCESSOR_PENTIUM, CPU_PENTIUM, PTA_MMX, 0, P_NONE},
  {"winchip-c6", PROCESSOR_I486, CPU_NONE, PTA_MMX, 0, P_NONE},
  {"winchip2", PROCESSOR_I486, CPU_NONE, PTA_MMX | PTA_3DNOW,
    0, P_NONE},
  {"c3", PROCESSOR_I486, CPU_NONE, PTA_MMX | PTA_3DNOW, 0, P_NONE},
  {"samuel-2", PROCESSOR_I486, CPU_NONE, PTA_MMX | PTA_3DNOW,
    0, P_NONE},
  {"c3-2", PROCESSOR_PENTIUMPRO, CPU_PENTIUMPRO,
    PTA_MMX | PTA_SSE | PTA_FXSR, 0, P_NONE},
  {"nehemiah", PROCESSOR_PENTIUMPRO, CPU_PENTIUMPRO,
    PTA_MMX | PTA_SSE | PTA_FXSR, 0, P_NONE},
  {"c7", PROCESSOR_PENTIUMPRO, CPU_PENTIUMPRO,
    PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3 | PTA_FXSR, 0, P_NONE},
  {"esther", PROCESSOR_PENTIUMPRO, CPU_PENTIUMPRO,
    PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3 | PTA_FXSR, 0, P_NONE},
  {"i686", PROCESSOR_PENTIUMPRO, CPU_PENTIUMPRO, 0, 0, P_NONE},
  {"pentiumpro", PROCESSOR_PENTIUMPRO, CPU_PENTIUMPRO, 0, 0, P_NONE},
  {"pentium2", PROCESSOR_PENTIUMPRO, CPU_PENTIUMPRO, PTA_MMX | PTA_FXSR,
    0, P_NONE},
  {"pentium3", PROCESSOR_PENTIUMPRO, CPU_PENTIUMPRO,
    PTA_MMX | PTA_SSE | PTA_FXSR, 0, P_NONE},
  {"pentium3m", PROCESSOR_PENTIUMPRO, CPU_PENTIUMPRO,
    PTA_MMX | PTA_SSE | PTA_FXSR, 0, P_NONE},
  {"pentium-m", PROCESSOR_PENTIUMPRO, CPU_PENTIUMPRO,
    PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_FXSR, 0, P_NONE},
  {"pentium4", PROCESSOR_PENTIUM4, CPU_NONE,
    PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_FXSR, 0, P_NONE},
  {"pentium4m", PROCESSOR_PENTIUM4, CPU_NONE,
    PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_FXSR, 0, P_NONE},
  {"prescott", PROCESSOR_NOCONA, CPU_NONE,
    PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3 | PTA_FXSR, 0, P_NONE},
  {"nocona", PROCESSOR_NOCONA, CPU_NONE,
    PTA_64BIT | PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3
      | PTA_CX16 | PTA_NO_SAHF | PTA_FXSR, 0, P_NONE},
  {"core2", PROCESSOR_CORE2, CPU_CORE2, PTA_CORE2,
   M_CPU_TYPE (INTEL_CORE2), P_PROC_SSSE3},
  {"nehalem", PROCESSOR_NEHALEM, CPU_NEHALEM, PTA_NEHALEM,
    M_CPU_SUBTYPE (INTEL_COREI7_NEHALEM), P_PROC_DYNAMIC},
  {"corei7", PROCESSOR_NEHALEM, CPU_NEHALEM, PTA_NEHALEM,
    M_CPU_TYPE (INTEL_COREI7), P_PROC_DYNAMIC},
  {"westmere", PROCESSOR_NEHALEM, CPU_NEHALEM, PTA_WESTMERE,
    M_CPU_SUBTYPE (INTEL_COREI7_WESTMERE), P_PROC_DYNAMIC},
  {"sandybridge", PROCESSOR_SANDYBRIDGE, CPU_NEHALEM,
    PTA_SANDYBRIDGE,
    M_CPU_SUBTYPE (INTEL_COREI7_SANDYBRIDGE), P_PROC_DYNAMIC},
  {"corei7-avx", PROCESSOR_SANDYBRIDGE, CPU_NEHALEM,
    PTA_SANDYBRIDGE, 0, P_PROC_DYNAMIC},
  {"ivybridge", PROCESSOR_SANDYBRIDGE, CPU_NEHALEM,
    PTA_IVYBRIDGE,
    M_CPU_SUBTYPE (INTEL_COREI7_IVYBRIDGE), P_PROC_DYNAMIC},
  {"core-avx-i", PROCESSOR_SANDYBRIDGE, CPU_NEHALEM,
    PTA_IVYBRIDGE, 0, P_PROC_DYNAMIC},
  {"haswell", PROCESSOR_HASWELL, CPU_HASWELL, PTA_HASWELL,
    M_CPU_SUBTYPE (INTEL_COREI7_HASWELL), P_PROC_DYNAMIC},
  {"core-avx2", PROCESSOR_HASWELL, CPU_HASWELL, PTA_HASWELL,
    0, P_PROC_DYNAMIC},
  {"broadwell", PROCESSOR_HASWELL, CPU_HASWELL, PTA_BROADWELL,
    M_CPU_SUBTYPE (INTEL_COREI7_BROADWELL), P_PROC_DYNAMIC},
  {"skylake", PROCESSOR_SKYLAKE, CPU_HASWELL, PTA_SKYLAKE,
    M_CPU_SUBTYPE (INTEL_COREI7_SKYLAKE), P_PROC_AVX2},
  {"skylake-avx512", PROCESSOR_SKYLAKE_AVX512, CPU_HASWELL,
    PTA_SKYLAKE_AVX512,
    M_CPU_SUBTYPE (INTEL_COREI7_SKYLAKE_AVX512), P_PROC_AVX512F},
  {"cannonlake", PROCESSOR_CANNONLAKE, CPU_HASWELL, PTA_CANNONLAKE,
    M_CPU_SUBTYPE (INTEL_COREI7_CANNONLAKE), P_PROC_AVX512F},
  {"icelake-client", PROCESSOR_ICELAKE_CLIENT, CPU_HASWELL,
    PTA_ICELAKE_CLIENT,
    M_CPU_SUBTYPE (INTEL_COREI7_ICELAKE_CLIENT), P_PROC_AVX512F},
  {"rocketlake", PROCESSOR_ROCKETLAKE, CPU_HASWELL,
    PTA_ROCKETLAKE,
    M_CPU_SUBTYPE (INTEL_COREI7_ROCKETLAKE), P_PROC_AVX512F},
  {"icelake-server", PROCESSOR_ICELAKE_SERVER, CPU_HASWELL,
    PTA_ICELAKE_SERVER,
    M_CPU_SUBTYPE (INTEL_COREI7_ICELAKE_SERVER), P_PROC_AVX512F},
  {"cascadelake", PROCESSOR_CASCADELAKE, CPU_HASWELL,
    PTA_CASCADELAKE,
    M_CPU_SUBTYPE (INTEL_COREI7_CASCADELAKE), P_PROC_AVX512F},
  {"tigerlake", PROCESSOR_TIGERLAKE, CPU_HASWELL, PTA_TIGERLAKE,
    M_CPU_SUBTYPE (INTEL_COREI7_TIGERLAKE), P_PROC_AVX512F},
  {"cooperlake", PROCESSOR_COOPERLAKE, CPU_HASWELL, PTA_COOPERLAKE,
    M_CPU_SUBTYPE (INTEL_COREI7_COOPERLAKE), P_PROC_AVX512F},
  {"sapphirerapids", PROCESSOR_SAPPHIRERAPIDS, CPU_HASWELL, PTA_SAPPHIRERAPIDS,
    M_CPU_SUBTYPE (INTEL_COREI7_SAPPHIRERAPIDS), P_PROC_AVX512F},
  {"emeraldrapids", PROCESSOR_SAPPHIRERAPIDS, CPU_HASWELL, PTA_SAPPHIRERAPIDS,
    M_CPU_SUBTYPE (INTEL_COREI7_SAPPHIRERAPIDS), P_PROC_AVX512F},
  {"alderlake", PROCESSOR_ALDERLAKE, CPU_HASWELL, PTA_ALDERLAKE,
    M_CPU_SUBTYPE (INTEL_COREI7_ALDERLAKE), P_PROC_AVX2},
  {"raptorlake", PROCESSOR_ALDERLAKE, CPU_HASWELL, PTA_ALDERLAKE,
    M_CPU_SUBTYPE (INTEL_COREI7_ALDERLAKE), P_PROC_AVX2},
  {"meteorlake", PROCESSOR_ALDERLAKE, CPU_HASWELL, PTA_ALDERLAKE,
    M_CPU_SUBTYPE (INTEL_COREI7_ALDERLAKE), P_PROC_AVX2},
  {"graniterapids", PROCESSOR_GRANITERAPIDS, CPU_HASWELL, PTA_GRANITERAPIDS,
    M_CPU_SUBTYPE (INTEL_COREI7_GRANITERAPIDS), P_PROC_AVX10_1_512},
  {"graniterapids-d", PROCESSOR_GRANITERAPIDS_D, CPU_HASWELL,
    PTA_GRANITERAPIDS_D, M_CPU_SUBTYPE (INTEL_COREI7_GRANITERAPIDS_D),
    P_PROC_AVX10_1_512},
  {"arrowlake", PROCESSOR_ARROWLAKE, CPU_HASWELL, PTA_ARROWLAKE,
    M_CPU_SUBTYPE (INTEL_COREI7_ARROWLAKE), P_PROC_AVX2},
  {"arrowlake-s", PROCESSOR_ARROWLAKE_S, CPU_HASWELL, PTA_ARROWLAKE_S,
    M_CPU_SUBTYPE (INTEL_COREI7_ARROWLAKE_S), P_PROC_AVX2},
  {"lunarlake", PROCESSOR_ARROWLAKE_S, CPU_HASWELL, PTA_ARROWLAKE_S,
    M_CPU_SUBTYPE (INTEL_COREI7_ARROWLAKE_S), P_PROC_AVX2},
  {"pantherlake", PROCESSOR_PANTHERLAKE, CPU_HASWELL, PTA_PANTHERLAKE,
    M_CPU_SUBTYPE (INTEL_COREI7_PANTHERLAKE), P_PROC_AVX2},
  {"diamondrapids", PROCESSOR_DIAMONDRAPIDS, CPU_HASWELL, PTA_DIAMONDRAPIDS,
    M_CPU_SUBTYPE (INTEL_COREI7_DIAMONDRAPIDS), P_PROC_AVX512F},
  {"bonnell", PROCESSOR_BONNELL, CPU_ATOM, PTA_BONNELL,
    M_CPU_TYPE (INTEL_BONNELL), P_PROC_SSSE3},
  {"atom", PROCESSOR_BONNELL, CPU_ATOM, PTA_BONNELL,
    M_CPU_TYPE (INTEL_BONNELL), P_PROC_SSSE3},
  {"silvermont", PROCESSOR_SILVERMONT, CPU_SLM, PTA_SILVERMONT,
    M_CPU_TYPE (INTEL_SILVERMONT), P_PROC_SSE4_2},
  {"slm", PROCESSOR_SILVERMONT, CPU_SLM, PTA_SILVERMONT,
    M_CPU_TYPE (INTEL_SILVERMONT), P_PROC_SSE4_2},
  {"goldmont", PROCESSOR_GOLDMONT, CPU_GLM, PTA_GOLDMONT,
    M_CPU_TYPE (INTEL_GOLDMONT), P_PROC_SSE4_2},
  {"goldmont-plus", PROCESSOR_GOLDMONT_PLUS, CPU_GLM, PTA_GOLDMONT_PLUS,
    M_CPU_TYPE (INTEL_GOLDMONT_PLUS), P_PROC_SSE4_2},
  {"tremont", PROCESSOR_TREMONT, CPU_HASWELL, PTA_TREMONT,
    M_CPU_TYPE (INTEL_TREMONT), P_PROC_SSE4_2},
  {"gracemont", PROCESSOR_ALDERLAKE, CPU_HASWELL, PTA_ALDERLAKE,
   M_CPU_SUBTYPE (INTEL_COREI7_ALDERLAKE), P_PROC_AVX2},
  {"sierraforest", PROCESSOR_SIERRAFOREST, CPU_HASWELL, PTA_SIERRAFOREST,
    M_CPU_TYPE (INTEL_SIERRAFOREST), P_PROC_AVX2},
  {"grandridge", PROCESSOR_GRANDRIDGE, CPU_HASWELL, PTA_GRANDRIDGE,
    M_CPU_TYPE (INTEL_GRANDRIDGE), P_PROC_AVX2},
  {"clearwaterforest", PROCESSOR_CLEARWATERFOREST, CPU_HASWELL,
    PTA_CLEARWATERFOREST, M_CPU_TYPE (INTEL_CLEARWATERFOREST), P_PROC_AVX2},
  {"intel", PROCESSOR_INTEL, CPU_SLM, PTA_NEHALEM,
    M_VENDOR (VENDOR_INTEL), P_NONE},
  {"geode", PROCESSOR_GEODE, CPU_GEODE,
    PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_PREFETCH_SSE, 0, P_NONE},
  {"k6", PROCESSOR_K6, CPU_K6, PTA_MMX, 0, P_NONE},
  {"k6-2", PROCESSOR_K6, CPU_K6, PTA_MMX | PTA_3DNOW, 0, P_NONE},
  {"k6-3", PROCESSOR_K6, CPU_K6, PTA_MMX | PTA_3DNOW, 0, P_NONE},
  {"athlon", PROCESSOR_ATHLON, CPU_ATHLON,
    PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_PREFETCH_SSE, 0, P_NONE},
  {"athlon-tbird", PROCESSOR_ATHLON, CPU_ATHLON,
    PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_PREFETCH_SSE, 0, P_NONE},
  {"athlon-4", PROCESSOR_ATHLON, CPU_ATHLON,
    PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_SSE | PTA_FXSR, 0, P_NONE},
  {"athlon-xp", PROCESSOR_ATHLON, CPU_ATHLON,
    PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_SSE | PTA_FXSR, 0, P_NONE},
  {"athlon-mp", PROCESSOR_ATHLON, CPU_ATHLON,
    PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_SSE | PTA_FXSR, 0, P_NONE},
  {"x86-64", PROCESSOR_K8, CPU_K8, PTA_X86_64_BASELINE, 0, P_NONE},
  {"x86-64-v2", PROCESSOR_K8, CPU_GENERIC, PTA_X86_64_V2 | PTA_NO_TUNE,
   0, P_NONE},
  {"x86-64-v3", PROCESSOR_K8, CPU_GENERIC, PTA_X86_64_V3 | PTA_NO_TUNE,
   0, P_NONE},
  {"x86-64-v4", PROCESSOR_K8, CPU_GENERIC, PTA_X86_64_V4 | PTA_NO_TUNE,
   0, P_NONE},
  {"eden-x2", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3 | PTA_FXSR,
    0, P_NONE},
  {"nano", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3
      | PTA_SSSE3 | PTA_FXSR, 0, P_NONE},
  {"nano-1000", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3
      | PTA_SSSE3 | PTA_FXSR, 0, P_NONE},
  {"nano-2000", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3
      | PTA_SSSE3 | PTA_FXSR, 0, P_NONE},
  {"nano-3000", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3
      | PTA_SSSE3 | PTA_SSE4_1 | PTA_FXSR, 0, P_NONE},
  {"nano-x2", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3
      | PTA_SSSE3 | PTA_SSE4_1 | PTA_FXSR, 0, P_NONE},
  {"eden-x4", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3
      | PTA_SSSE3 | PTA_SSE4_1 | PTA_FXSR, 0, P_NONE},
  {"nano-x4", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_SSE | PTA_SSE2 | PTA_SSE3
      | PTA_SSSE3 | PTA_SSE4_1 | PTA_FXSR, 0, P_NONE},
  {"lujiazui", PROCESSOR_LUJIAZUI, CPU_LUJIAZUI,
	PTA_LUJIAZUI,
	M_CPU_SUBTYPE (ZHAOXIN_FAM7H_LUJIAZUI), P_PROC_BMI},
  {"yongfeng", PROCESSOR_YONGFENG, CPU_YONGFENG,
	PTA_YONGFENG,
	M_CPU_SUBTYPE (ZHAOXIN_FAM7H_YONGFENG), P_PROC_AVX2},
  {"shijidadao", PROCESSOR_SHIJIDADAO, CPU_YONGFENG,
	PTA_YONGFENG,
	M_CPU_SUBTYPE (ZHAOXIN_FAM7H_SHIJIDADAO), P_PROC_AVX2},
  {"k8", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_SSE
      | PTA_SSE2 | PTA_NO_SAHF | PTA_FXSR, 0, P_NONE},
  {"k8-sse3", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_SSE
      | PTA_SSE2 | PTA_SSE3 | PTA_NO_SAHF | PTA_FXSR, 0, P_NONE},
  {"opteron", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_SSE
      | PTA_SSE2 | PTA_NO_SAHF | PTA_FXSR, 0, P_NONE},
  {"opteron-sse3", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_SSE
      | PTA_SSE2 | PTA_SSE3 | PTA_NO_SAHF | PTA_FXSR, 0, P_NONE},
  {"athlon64", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_SSE
      | PTA_SSE2 | PTA_NO_SAHF | PTA_FXSR, 0, P_NONE},
  {"athlon64-sse3", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_SSE
      | PTA_SSE2 | PTA_SSE3 | PTA_NO_SAHF | PTA_FXSR, 0, P_NONE},
  {"athlon-fx", PROCESSOR_K8, CPU_K8,
    PTA_64BIT | PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_SSE
      | PTA_SSE2 | PTA_NO_SAHF | PTA_FXSR, 0, P_NONE},
  {"amdfam10", PROCESSOR_AMDFAM10, CPU_AMDFAM10,
    PTA_64BIT | PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_SSE | PTA_SSE2
      | PTA_SSE3 | PTA_SSE4A | PTA_CX16 | PTA_ABM | PTA_PRFCHW | PTA_FXSR,
    0, P_PROC_DYNAMIC},
  {"barcelona", PROCESSOR_AMDFAM10, CPU_AMDFAM10,
    PTA_64BIT | PTA_MMX | PTA_3DNOW | PTA_3DNOW_A | PTA_SSE | PTA_SSE2
      | PTA_SSE3 | PTA_SSE4A | PTA_CX16 | PTA_ABM | PTA_PRFCHW | PTA_FXSR,
    M_CPU_SUBTYPE (AMDFAM10H_BARCELONA), P_PROC_DYNAMIC},
  {"bdver1", PROCESSOR_BDVER1, CPU_BDVER1,
    PTA_BDVER1,
    M_CPU_SUBTYPE (AMDFAM15H_BDVER1), P_PROC_XOP},
  {"bdver2", PROCESSOR_BDVER2, CPU_BDVER2,
    PTA_BDVER2,
    M_CPU_SUBTYPE (AMDFAM15H_BDVER2), P_PROC_FMA},
  {"bdver3", PROCESSOR_BDVER3, CPU_BDVER3,
    PTA_BDVER3,
    M_CPU_SUBTYPE (AMDFAM15H_BDVER3), P_PROC_FMA},
  {"bdver4", PROCESSOR_BDVER4, CPU_BDVER4,
    PTA_BDVER4,
    M_CPU_SUBTYPE (AMDFAM15H_BDVER4), P_PROC_AVX2},
  {"znver1", PROCESSOR_ZNVER1, CPU_ZNVER1,
    PTA_ZNVER1,
    M_CPU_SUBTYPE (AMDFAM17H_ZNVER1), P_PROC_AVX2},
  {"znver2", PROCESSOR_ZNVER2, CPU_ZNVER2,
    PTA_ZNVER2,
    M_CPU_SUBTYPE (AMDFAM17H_ZNVER2), P_PROC_AVX2},
  {"znver3", PROCESSOR_ZNVER3, CPU_ZNVER3,
    PTA_ZNVER3,
    M_CPU_SUBTYPE (AMDFAM19H_ZNVER3), P_PROC_AVX2},
  {"znver4", PROCESSOR_ZNVER4, CPU_ZNVER4,
    PTA_ZNVER4,
    M_CPU_SUBTYPE (AMDFAM19H_ZNVER4), P_PROC_AVX512F},
  {"znver5", PROCESSOR_ZNVER5, CPU_ZNVER5,
    PTA_ZNVER5,
    M_CPU_SUBTYPE (AMDFAM1AH_ZNVER5), P_PROC_AVX512F},
  {"btver1", PROCESSOR_BTVER1, CPU_GENERIC,
    PTA_BTVER1,
    M_CPU_TYPE (AMD_BTVER1), P_PROC_SSE4_A},
  {"btver2", PROCESSOR_BTVER2, CPU_BTVER2,
    PTA_BTVER2,
    M_CPU_TYPE (AMD_BTVER2), P_PROC_BMI},

  {"generic", PROCESSOR_GENERIC, CPU_GENERIC,
    PTA_64BIT
      | PTA_HLE /* flags are only used for -march switch.  */,
    0, P_NONE},

  {"amd", PROCESSOR_GENERIC, CPU_GENERIC, 0,
   M_VENDOR (VENDOR_AMD), P_NONE},
  {"amdfam10h", PROCESSOR_GENERIC, CPU_GENERIC, 0,
    M_CPU_TYPE (AMDFAM10H), P_NONE},
  {"amdfam15h", PROCESSOR_GENERIC, CPU_GENERIC, 0,
    M_CPU_TYPE (AMDFAM15H), P_NONE},
  {"amdfam17h", PROCESSOR_GENERIC, CPU_GENERIC, 0,
    M_CPU_TYPE (AMDFAM17H), P_NONE},
  {"amdfam19h", PROCESSOR_GENERIC, CPU_GENERIC, 0,
    M_CPU_TYPE (AMDFAM19H), P_NONE},
  {"shanghai", PROCESSOR_GENERIC, CPU_GENERIC, 0,
    M_CPU_SUBTYPE (AMDFAM10H_SHANGHAI), P_NONE},
  {"istanbul", PROCESSOR_GENERIC, CPU_GENERIC, 0,
    M_CPU_SUBTYPE (AMDFAM10H_ISTANBUL), P_NONE},
};

/* NB: processor_alias_table stops at the "generic" entry.  */
unsigned int const pta_size = ARRAY_SIZE (processor_alias_table) - 7;
unsigned int const num_arch_names = ARRAY_SIZE (processor_alias_table);

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
	{
	  const char *name = processor_alias_table[i].name;
	  gcc_checking_assert (name != NULL);
	  v.safe_push (name);
	}
#ifdef HAVE_LOCAL_CPU_DETECT
      /* Add also "native" as possible value.  */
      v.safe_push ("native");
#endif

      break;
    case OPT_mtune_:
      for (unsigned i = 0; i < PROCESSOR_max; i++)
	{
	  const char *name = processor_names[i];
	  gcc_checking_assert (name != NULL);
	  v.safe_push (name);
	}
      break;
    default:
      break;
    }

  return v;
}

#undef  TARGET_GET_VALID_OPTION_VALUES
#define TARGET_GET_VALID_OPTION_VALUES ix86_get_valid_option_values

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
