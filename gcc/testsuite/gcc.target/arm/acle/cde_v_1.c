/* Test the CDE ACLE intrinsic.  */
/* { dg-do compile } */
/* { dg-require-effective-target arm_v8m_main_cde_fp_ok } */
/* { dg-options "-save-temps -O2" } */
/* { dg-add-options arm_v8m_main_cde_fp } */

#include "arm_cde.h"

#define TEST0(T, N, C, I) \
T test_arm_##N##_##C##_##I () { \
  return __arm_##N (C, I); \
}

#define TEST1(T, N, C, I) \
T test_arm_##N##_##C##_##I (T a) { \
  return __arm_##N (C, a, I); \
}

#define TEST2(T, N, C, I) \
T test_arm_##N##_##C##_##I (T a) { \
  return __arm_##N (C, a, a, I); \
}

#define TEST3(T, N, C, I) \
T test_arm_##N##_##C##_##I (T a) { \
  return __arm_##N (C, a, a, a, I); \
}

#define TEST_ALL(C) \
TEST0 (uint32_t, vcx1_u32,	C, 0) \
TEST1 (uint32_t, vcx1a_u32,	C, 0) \
TEST1 (uint32_t, vcx2_u32,	C, 0) \
TEST2 (uint32_t, vcx2a_u32,	C, 0) \
TEST2 (uint32_t, vcx3_u32,	C, 0) \
TEST3 (uint32_t, vcx3a_u32,	C, 0) \
TEST0 (uint64_t, vcx1d_u64,	C, 0) \
TEST1 (uint64_t, vcx1da_u64,	C, 0) \
TEST1 (uint64_t, vcx2d_u64,	C, 0) \
TEST2 (uint64_t, vcx2da_u64,	C, 0) \
TEST2 (uint64_t, vcx3d_u64,	C, 0) \
TEST3 (uint64_t, vcx3da_u64,	C, 0) \
TEST0 (uint32_t, vcx1_u32,	C, 2047) \
TEST1 (uint32_t, vcx1a_u32,	C, 2047) \
TEST1 (uint32_t, vcx2_u32,	C, 63) \
TEST2 (uint32_t, vcx2a_u32,	C, 63) \
TEST2 (uint32_t, vcx3_u32,	C, 7) \
TEST3 (uint32_t, vcx3a_u32,	C, 7) \
TEST0 (uint64_t, vcx1d_u64,	C, 2047) \
TEST1 (uint64_t, vcx1da_u64,	C, 2047) \
TEST1 (uint64_t, vcx2d_u64,	C, 63) \
TEST2 (uint64_t, vcx2da_u64,	C, 63) \
TEST2 (uint64_t, vcx3d_u64,	C, 7) \
TEST3 (uint64_t, vcx3da_u64,	C, 7)

#pragma GCC push_options
#pragma GCC target ("arch=armv8-m.main+cdecp0+fp")
TEST_ALL (0)
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8-m.main+cdecp1+fp")
TEST_ALL (1)
#pragma GCC pop_options

#pragma GCC push_options
#pragma GCC target ("arch=armv8-m.main+cdecp2+cdecp3+cdecp4+cdecp5+cdecp6+cdecp7+fp")
TEST_ALL (2)
TEST_ALL (3)
TEST_ALL (4)
TEST_ALL (5)
TEST_ALL (6)
TEST_ALL (7)
#pragma GCC pop_options

/* { dg-final { scan-assembler-times {\tvcx1\tp0, s[0-9]+, #0} 1 } } */
/* { dg-final { scan-assembler-times {\tvcx1\tp1, s[0-9]+, #0} 1 } } */
/* { dg-final { scan-assembler-times {\tvcx1\tp2, s[0-9]+, #0} 1 } } */
/* { dg-final { scan-assembler-times {\tvcx1\tp3, s[0-9]+, #0} 1 } } */
/* { dg-final { scan-assembler-times {\tvcx1\tp4, s[0-9]+, #0} 1 } } */
/* { dg-final { scan-assembler-times {\tvcx1\tp5, s[0-9]+, #0} 1 } } */
/* { dg-final { scan-assembler-times {\tvcx1\tp6, s[0-9]+, #0} 1 } } */
/* { dg-final { scan-assembler-times {\tvcx1\tp7, s[0-9]+, #0} 1 } } */
/* { dg-final { scan-assembler-times {\tvcx1\tp[0-7], s[0-9]+, #2047} 8 } } */
/* { dg-final { scan-assembler-times {\tvcx1a\tp[0-7], s[0-9]+, #[0,2047]} 16 } } */
/* { dg-final { scan-assembler-times {\tvcx2\tp[0-7], s[0-9]+, s[0-9]+, #[0,63]} 16 } } */
/* { dg-final { scan-assembler-times {\tvcx2a\tp[0-7], s[0-9]+, s[0-9]+, #[0,63]} 16 } } */
/* { dg-final { scan-assembler-times {\tvcx3\tp[0-7], s[0-9]+, s[0-9]+, s[0-9]+, #[0,7]} 16 } } */
/* { dg-final { scan-assembler-times {\tvcx3a\tp[0-7], s[0-9]+, s[0-9]+, s[0-9]+, #[0,7]} 16 } } */
/* { dg-final { scan-assembler-times {\tvcx1\tp[0-7], d[0-9]+, #[0,2047]} 16 } } */
/* { dg-final { scan-assembler-times {\tvcx1a\tp[0-7], d[0-9]+, #[0,2047]} 16 } } */
/* { dg-final { scan-assembler-times {\tvcx2\tp[0-7], d[0-9]+, d[0-9]+, #[0,63]} 16 } } */
/* { dg-final { scan-assembler-times {\tvcx2a\tp[0-7], d[0-9]+, d[0-9]+, #[0,63]} 16 } } */
/* { dg-final { scan-assembler-times {\tvcx3\tp[0-7], d[0-9]+, d[0-9]+, d[0-9]+, #[0,7]} 16 } } */
/* { dg-final { scan-assembler-times {\tvcx3a\tp[0-7], d[0-9]+, d[0-9]+, d[0-9]+, #[0,7]} 16 } } */
