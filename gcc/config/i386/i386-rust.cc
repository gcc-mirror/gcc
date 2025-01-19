/* Subroutines for the Rust front end on the x86 architecture.
   Copyright (C) 2022-2025 Free Software Foundation, Inc.

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
#include "tm_rust.h"
#include "rust/rust-target.h"

/* Implement TARGET_RUST_CPU_INFO for x86 targets.  */

void
ix86_rust_target_cpu_info (void)
{
  if (TARGET_64BIT)
    rust_add_target_info ("target_arch", "x86_64");
  else
    rust_add_target_info ("target_arch", "x86");

  // features officially "stabilised" in rustc
  if (TARGET_MMX)
    rust_add_target_info ("target_feature", "mmx");
  if (TARGET_SSE)
    rust_add_target_info ("target_feature", "sse");
  if (TARGET_SSE2)
    rust_add_target_info ("target_feature", "sse2");
  if (TARGET_SSE3)
    rust_add_target_info ("target_feature", "sse3");
  if (TARGET_SSSE3)
    rust_add_target_info ("target_feature", "ssse3");
  if (TARGET_SSE4_1)
    rust_add_target_info ("target_feature", "sse4.1");
  if (TARGET_SSE4_2)
    rust_add_target_info ("target_feature", "sse4.2");
  if (TARGET_AES)
    rust_add_target_info ("target_feature", "aes");
  if (TARGET_SHA)
    rust_add_target_info ("target_feature", "sha");
  if (TARGET_AVX)
    rust_add_target_info ("target_feature", "avx");
  if (TARGET_AVX2)
    rust_add_target_info ("target_feature", "avx2");
  if (TARGET_AVX512F)
    rust_add_target_info ("target_feature", "avx512f");
  if (TARGET_AVX512CD)
    rust_add_target_info ("target_feature", "avx512cd");
  if (TARGET_AVX512DQ)
    rust_add_target_info ("target_feature", "avx512dq");
  if (TARGET_AVX512BW)
    rust_add_target_info ("target_feature", "avx512bw");
  if (TARGET_AVX512VL)
    rust_add_target_info ("target_feature", "avx512vl");
  if (TARGET_AVX512VBMI)
    rust_add_target_info ("target_feature", "avx512vbmi");
  if (TARGET_AVX512IFMA)
    rust_add_target_info ("target_feature", "avx512ifma");
  if (TARGET_AVX512VPOPCNTDQ)
    rust_add_target_info ("target_feature", "avx512vpopcntdq");
  if (TARGET_FMA)
    rust_add_target_info ("target_feature", "fma");
  if (TARGET_RTM)
    rust_add_target_info ("target_feature", "rtm");
  if (TARGET_SSE4A)
    rust_add_target_info ("target_feature", "sse4a");
  if (TARGET_BMI)
    {
      rust_add_target_info ("target_feature", "bmi1");
      rust_add_target_info ("target_feature", "bmi");
    }
  if (TARGET_BMI2)
    rust_add_target_info ("target_feature", "bmi2");
  if (TARGET_LZCNT)
    rust_add_target_info ("target_feature", "lzcnt");
  if (TARGET_TBM)
    rust_add_target_info ("target_feature", "tbm");
  if (TARGET_POPCNT)
    rust_add_target_info ("target_feature", "popcnt");
  if (TARGET_RDRND)
    {
      rust_add_target_info ("target_feature", "rdrand");
      rust_add_target_info ("target_feature", "rdrnd");
    }
  if (TARGET_F16C)
    rust_add_target_info ("target_feature", "f16c");
  if (TARGET_RDSEED)
    rust_add_target_info ("target_feature", "rdseed");
  if (TARGET_ADX)
    rust_add_target_info ("target_feature", "adx");
  if (TARGET_FXSR)
    rust_add_target_info ("target_feature", "fxsr");
  if (TARGET_XSAVE)
    rust_add_target_info ("target_feature", "xsave");
  if (TARGET_XSAVEOPT)
    rust_add_target_info ("target_feature", "xsaveopt");
  if (TARGET_XSAVEC)
    rust_add_target_info ("target_feature", "xsavec");
  if (TARGET_XSAVES)
    rust_add_target_info ("target_feature", "xsaves");
  if (TARGET_VPCLMULQDQ)
    {
      rust_add_target_info ("target_feature", "pclmulqdq");
      rust_add_target_info ("target_feature", "vpclmulqdq");
    }
  if (TARGET_CMPXCHG16B)
    rust_add_target_info ("target_feature", "cmpxchg16b");
  if (TARGET_MOVBE)
    rust_add_target_info ("target_feature", "movbe");
}
