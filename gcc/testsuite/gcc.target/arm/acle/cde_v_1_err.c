/* Test the CDE ACLE intrinsic.  */
/* { dg-do compile } */
/* { dg-require-effective-target arm_v8m_main_cde_fp_ok } */
/* { dg-add-options arm_v8m_main_cde_fp } */

#include "arm_cde.h"

uint64_t test_coproc_range (uint32_t a, uint64_t b)
{
  uint64_t res = 0;
  res += __arm_vcx1_u32 (8, 0); /* { dg-error {coproc must be a constant immediate in range \[0-7\]} } */
  res += __arm_vcx1a_u32 (8, a, 0); /* { dg-error {coproc must be a constant immediate in range \[0-7\]} } */
  res += __arm_vcx2_u32 (8, a, 0); /* { dg-error {coproc must be a constant immediate in range \[0-7\]} } */
  res += __arm_vcx2a_u32 (8, a, a, 0); /* { dg-error {coproc must be a constant immediate in range \[0-7\]} } */
  res += __arm_vcx3_u32 (8, a, a, 0); /* { dg-error {coproc must be a constant immediate in range \[0-7\]} } */
  res += __arm_vcx3a_u32 (8, a, a, a, 0); /* { dg-error {coproc must be a constant immediate in range \[0-7\]} } */
  res += __arm_vcx1d_u64 (8, 0); /* { dg-error {coproc must be a constant immediate in range \[0-7\]} } */
  res += __arm_vcx1da_u64 (8, a, 0); /* { dg-error {coproc must be a constant immediate in range \[0-7\]} } */
  res += __arm_vcx2d_u64 (8, a, 0); /* { dg-error {coproc must be a constant immediate in range \[0-7\]} } */
  res += __arm_vcx2da_u64 (8, a, a, 0); /* { dg-error {coproc must be a constant immediate in range \[0-7\]} } */
  res += __arm_vcx3d_u64 (8, a, a, 0); /* { dg-error {coproc must be a constant immediate in range \[0-7\]} } */
  res += __arm_vcx3da_u64 (8, a, a, a, 0); /* { dg-error {coproc must be a constant immediate in range \[0-7\]} } */
  return res;
}

uint64_t test_imm_range (uint32_t a, uint64_t b)
{
  uint64_t res = 0;
  res += __arm_vcx1_u32 (0, 2048);         /* { dg-error {argument [2-5] to '__builtin_arm_vcx1si' must be a constant immediate in range \[0-2047\]} } */
  res += __arm_vcx1a_u32 (0, a, 2048);     /* { dg-error {argument [2-5] to '__builtin_arm_vcx1asi' must be a constant immediate in range \[0-2047\]} } */
  res += __arm_vcx2_u32 (0, a, 64);        /* { dg-error {argument [2-5] to '__builtin_arm_vcx2si' must be a constant immediate in range \[0-63\]} } */
  res += __arm_vcx2a_u32 (0, a, a, 64);    /* { dg-error {argument [2-5] to '__builtin_arm_vcx2asi' must be a constant immediate in range \[0-63\]} } */
  res += __arm_vcx3_u32 (0, a, a, 8);      /* { dg-error {argument [2-5] to '__builtin_arm_vcx3si' must be a constant immediate in range \[0-7\]} } */
  res += __arm_vcx3a_u32 (0, a, a, a, 8);  /* { dg-error {argument [2-5] to '__builtin_arm_vcx3asi' must be a constant immediate in range \[0-7\]} } */
  res += __arm_vcx1d_u64 (0, 2048);        /* { dg-error {argument [2-5] to '__builtin_arm_vcx1di' must be a constant immediate in range \[0-2047\]} } */
  res += __arm_vcx1da_u64 (0, a, 2048);    /* { dg-error {argument [2-5] to '__builtin_arm_vcx1adi' must be a constant immediate in range \[0-2047\]} } */
  res += __arm_vcx2d_u64 (0, a, 64);       /* { dg-error {argument [2-5] to '__builtin_arm_vcx2di' must be a constant immediate in range \[0-63\]} } */
  res += __arm_vcx2da_u64 (0, a, a, 64);   /* { dg-error {argument [2-5] to '__builtin_arm_vcx2adi' must be a constant immediate in range \[0-63\]} } */
  res += __arm_vcx3d_u64 (0, a, a, 8);     /* { dg-error {argument [2-5] to '__builtin_arm_vcx3di' must be a constant immediate in range \[0-7\]} } */
  res += __arm_vcx3da_u64 (0, a, a, a, 8); /* { dg-error {argument [2-5] to '__builtin_arm_vcx3adi' must be a constant immediate in range \[0-7\]} } */
  return res;
}

#pragma GCC push_options
#pragma GCC target ("arch=armv8-m.main+cdecp1+fp")
uint64_t test_coproc_match_1 (uint32_t a, uint64_t b)
{
  uint64_t res = 0;
  res += __arm_vcx1_u32 (0, 0); /* { dg-error {coprocessor 0 is not enabled with \+cdecp0} } */
  res += __arm_vcx1a_u32 (0, a, 0); /* { dg-error {coprocessor 0 is not enabled with \+cdecp0} } */
  res += __arm_vcx2_u32 (0, a, 0); /* { dg-error {coprocessor 0 is not enabled with \+cdecp0} } */
  res += __arm_vcx2a_u32 (0, a, a, 0); /* { dg-error {coprocessor 0 is not enabled with \+cdecp0} } */
  res += __arm_vcx3_u32 (0, a, a, 0); /* { dg-error {coprocessor 0 is not enabled with \+cdecp0} } */
  res += __arm_vcx3a_u32 (0, a, a, a, 0); /* { dg-error {coprocessor 0 is not enabled with \+cdecp0} } */
  res += __arm_vcx1d_u64 (0, 0); /* { dg-error {coprocessor 0 is not enabled with \+cdecp0} } */
  res += __arm_vcx1da_u64 (0, a, 0); /* { dg-error {coprocessor 0 is not enabled with \+cdecp0} } */
  res += __arm_vcx2d_u64 (0, a, 0); /* { dg-error {coprocessor 0 is not enabled with \+cdecp0} } */
  res += __arm_vcx2da_u64 (0, a, a, 0); /* { dg-error {coprocessor 0 is not enabled with \+cdecp0} } */
  res += __arm_vcx3d_u64 (0, a, a, 0); /* { dg-error {coprocessor 0 is not enabled with \+cdecp0} } */
  res += __arm_vcx3da_u64 (0, a, a, a, 0); /* { dg-error {coprocessor 0 is not enabled with \+cdecp0} } */
  return res;
}
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8-m.main+cdecp2+fp")
uint32_t test_coproc_match_2 ()
{
  return __arm_vcx1_u32 (0, 0); /* { dg-error {coprocessor 0 is not enabled with \+cdecp0} } */
}
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8-m.main+cdecp3+fp")
uint32_t test_coproc_match_3 ()
{
  return __arm_vcx1_u32 (0, 0); /* { dg-error {coprocessor 0 is not enabled with \+cdecp0} } */
}
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8-m.main+cdecp4+fp")
uint32_t test_coproc_match_4 ()
{
  return __arm_vcx1_u32 (0, 0); /* { dg-error {coprocessor 0 is not enabled with \+cdecp0} } */
}
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8-m.main+cdecp5+fp")
uint32_t test_coproc_match_5 ()
{
  return __arm_vcx1_u32 (0, 0); /* { dg-error {coprocessor 0 is not enabled with \+cdecp0} } */
}
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8-m.main+cdecp6+fp")
uint32_t test_coproc_match_6 ()
{
  return __arm_vcx1_u32 (0, 0); /* { dg-error {coprocessor 0 is not enabled with \+cdecp0} } */
}
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8-m.main+cdecp7+fp")
uint32_t test_coproc_match_7 ()
{
  return __arm_vcx1_u32 (0, 0); /* { dg-error {coprocessor 0 is not enabled with \+cdecp0} } */
}
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8-m.main+cdecp0+fp")
uint32_t test_coproc_match_0 ()
{
  uint64_t res = 0;
  res += __arm_vcx1_u32 (1, 0); /* { dg-error {coprocessor 1 is not enabled with \+cdecp1} } */
  res += __arm_vcx1_u32 (2, 0); /* { dg-error {coprocessor 2 is not enabled with \+cdecp2} } */
  res += __arm_vcx1_u32 (3, 0); /* { dg-error {coprocessor 3 is not enabled with \+cdecp3} } */
  res += __arm_vcx1_u32 (4, 0); /* { dg-error {coprocessor 4 is not enabled with \+cdecp4} } */
  res += __arm_vcx1_u32 (5, 0); /* { dg-error {coprocessor 5 is not enabled with \+cdecp5} } */
  res += __arm_vcx1_u32 (6, 0); /* { dg-error {coprocessor 6 is not enabled with \+cdecp6} } */
  res += __arm_vcx1_u32 (7, 0); /* { dg-error {coprocessor 7 is not enabled with \+cdecp7} } */
  return res;
}
#pragma GCC pop_options
