/* PR tree-optimization/123951.  Copying a lane between two vectors must
   remain a single INS (or ZIP) whichever lane pair is used, on both
   endiannesses.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_neon.h>

#define BUILD_TEST(TYPE, Q1, Q2, SUFFIX, INDEX1, INDEX2)		\
TYPE __attribute__((noinline,noclone))					\
test_copy##Q1##_lane##Q2##_##SUFFIX##_##INDEX1##INDEX2 (TYPE a, TYPE b) \
{									\
  return vcopy##Q1##_lane##Q2##_##SUFFIX (a, INDEX1, b, INDEX2);	\
}

BUILD_TEST (uint64x2_t,  q, q, u64, 0, 0)
BUILD_TEST (int64x2_t,   q, q, s64, 0, 0)
BUILD_TEST (float64x2_t, q, q, f64, 0, 0)
/* { dg-final { scan-assembler-times "ins\\tv0.d\\\[0\\\], v1.d\\\[0\\\]" 3 } } */
BUILD_TEST (uint64x2_t,  q, q, u64, 1, 1)
BUILD_TEST (int64x2_t,   q, q, s64, 1, 1)
BUILD_TEST (float64x2_t, q, q, f64, 1, 1)
/* { dg-final { scan-assembler-times "ins\\tv0.d\\\[1\\\], v1.d\\\[1\\\]" 3 } } */
BUILD_TEST (uint64x2_t,  q, q, u64, 1, 0)
BUILD_TEST (int64x2_t,   q, q, s64, 1, 0)
BUILD_TEST (float64x2_t, q, q, f64, 1, 0)
/* { dg-final { scan-assembler-times "zip1\\tv0.2d, v0.2d, v1.2d" 3 } } */
BUILD_TEST (uint64x2_t,  q, q, u64, 0, 1)
BUILD_TEST (int64x2_t,   q, q, s64, 0, 1)
BUILD_TEST (float64x2_t, q, q, f64, 0, 1)
/* { dg-final { scan-assembler-times "zip2\\tv0.2d, v1.2d, v0.2d" 3 } } */
BUILD_TEST (uint32x2_t,  , , u32, 0, 0)
BUILD_TEST (int32x2_t,   , , s32, 0, 0)
BUILD_TEST (float32x2_t, , , f32, 0, 0)
/* { dg-final { scan-assembler-times "ins\\tv0.s\\\[0\\\], v1.s\\\[0\\\]" 3 } } */
BUILD_TEST (uint32x2_t,  , , u32, 1, 1)
BUILD_TEST (int32x2_t,   , , s32, 1, 1)
BUILD_TEST (float32x2_t, , , f32, 1, 1)
/* { dg-final { scan-assembler-times "ins\\tv0.s\\\[1\\\], v1.s\\\[1\\\]" 3 } } */

/* { dg-final { scan-assembler-not "\\tmov\\t" } } */
