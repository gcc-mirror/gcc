/* Test the CDE ACLE intrinsic.  */
/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_1m_main_cde_mve_ok } */
/* { dg-options "-save-temps -O2" } */
/* { dg-add-options arm_v8_1m_main_cde_mve } */

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
TEST3 (uint64_t, vcx3da_u64,	C, 0)

TEST_ALL (0)

/* { dg-final { scan-assembler-times {\tvcx1\tp0, s[0-9]+, #0} 1 } } */
/* { dg-final { scan-assembler-times {\tvcx1a\tp0, s[0-9]+, #0} 1 } } */
/* { dg-final { scan-assembler-times {\tvcx2\tp0, s[0-9]+, s[0-9]+, #0} 1 } } */
/* { dg-final { scan-assembler-times {\tvcx2a\tp0, s[0-9]+, s[0-9]+, #0} 1 } } */
/* { dg-final { scan-assembler-times {\tvcx3\tp0, s[0-9]+, s[0-9]+, s[0-9]+, #0} 1 } } */
/* { dg-final { scan-assembler-times {\tvcx3a\tp0, s[0-9]+, s[0-9]+, s[0-9]+, #0} 1 } } */
/* { dg-final { scan-assembler-times {\tvcx1\tp0, d[0-9]+, #0} 1 } } */
/* { dg-final { scan-assembler-times {\tvcx1a\tp0, d[0-9]+, #0} 1 } } */
/* { dg-final { scan-assembler-times {\tvcx2\tp0, d[0-9]+, d[0-9]+, #0} 1 } } */
/* { dg-final { scan-assembler-times {\tvcx2a\tp0, d[0-9]+, d[0-9]+, #0} 1 } } */
/* { dg-final { scan-assembler-times {\tvcx3\tp0, d[0-9]+, d[0-9]+, d[0-9]+, #0} 1 } } */
/* { dg-final { scan-assembler-times {\tvcx3a\tp0, d[0-9]+, d[0-9]+, d[0-9]+, #0} 1 } } */
