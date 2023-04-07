/* Test the vector comparison intrinsics when comparing to immediate zero.
   */

/* { dg-do assemble { target { arm_softfp_ok } } } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-save-temps -mfloat-abi=softfp -O3" } */
/* { dg-add-options arm_neon } */

#include <arm_neon.h>

#define GEN_TEST(T, D, C, R) \
  R test_##C##_##T (T a) { return C (a, D (0)); }

#define GEN_DOUBLE_TESTS(S, T, C) \
  GEN_TEST (T, vdup_n_s##S, C##_s##S, u##T) \
  GEN_TEST (u##T, vdup_n_u##S, C##_u##S, u##T) 

#define GEN_QUAD_TESTS(S, T, C) \
  GEN_TEST (T, vdupq_n_s##S, C##q_s##S, u##T) \
  GEN_TEST (u##T, vdupq_n_u##S, C##q_u##S, u##T) 

#define GEN_COND_TESTS(C) \
  GEN_DOUBLE_TESTS (8, int8x8_t, C) \
  GEN_DOUBLE_TESTS (16, int16x4_t, C) \
  GEN_DOUBLE_TESTS (32, int32x2_t, C) \
  GEN_QUAD_TESTS (8, int8x16_t, C) \
  GEN_QUAD_TESTS (16, int16x8_t, C) \
  GEN_QUAD_TESTS (32, int32x4_t, C)

GEN_COND_TESTS(vcgt)
GEN_COND_TESTS(vcge)
GEN_COND_TESTS(vclt)
GEN_COND_TESTS(vcle)
GEN_COND_TESTS(vceq)

/* Scan for expected outputs.  */
/* { dg-final { scan-assembler "vcgt\.s8\[ 	\]+\[dD\]\[0-9\]+, \[dD\]\[0-9\]+, #0" } } */
/* { dg-final { scan-assembler "vcgt\.s16\[ 	\]+\[dD\]\[0-9\]+, \[dD\]\[0-9\]+, #0" } } */
/* { dg-final { scan-assembler "vcgt\.s32\[ 	\]+\[dD\]\[0-9\]+, \[dD\]\[0-9\]+, #0" } } */
/* { dg-final { scan-assembler "vcgt\.s8\[ 	\]+\[qQ\]\[0-9\]+, \[qQ\]\[0-9\]+, #0" } } */
/* { dg-final { scan-assembler "vcgt\.s16\[ 	\]+\[qQ\]\[0-9\]+, \[qQ\]\[0-9\]+, #0" } } */
/* { dg-final { scan-assembler "vcgt\.s32\[ 	\]+\[qQ\]\[0-9\]+, \[qQ\]\[0-9\]+, #0" } } */
/* { dg-final { scan-assembler "vcge\.s8\[ 	\]+\[dD\]\[0-9\]+, \[dD\]\[0-9\]+, #0" } } */
/* { dg-final { scan-assembler "vcge\.s16\[ 	\]+\[dD\]\[0-9\]+, \[dD\]\[0-9\]+, #0" } } */
/* { dg-final { scan-assembler "vcge\.s32\[ 	\]+\[dD\]\[0-9\]+, \[dD\]\[0-9\]+, #0" } } */
/* { dg-final { scan-assembler "vcge\.s8\[ 	\]+\[qQ\]\[0-9\]+, \[qQ\]\[0-9\]+, #0" } } */
/* { dg-final { scan-assembler "vcge\.s16\[ 	\]+\[qQ\]\[0-9\]+, \[qQ\]\[0-9\]+, #0" } } */
/* { dg-final { scan-assembler "vcge\.s32\[ 	\]+\[qQ\]\[0-9\]+, \[qQ\]\[0-9\]+, #0" } } */
/* { dg-final { scan-assembler "vclt\.s8\[ 	\]+\[dD\]\[0-9\]+, \[dD\]\[0-9\]+, #0" } } */
/* { dg-final { scan-assembler "vclt\.s16\[ 	\]+\[dD\]\[0-9\]+, \[dD\]\[0-9\]+, #0" } } */
/* { dg-final { scan-assembler "vclt\.s32\[ 	\]+\[dD\]\[0-9\]+, \[dD\]\[0-9\]+, #0" } } */
/* { dg-final { scan-assembler "vclt\.s8\[ 	\]+\[qQ\]\[0-9\]+, \[qQ\]\[0-9\]+, #0" } } */
/* { dg-final { scan-assembler "vclt\.s16\[ 	\]+\[qQ\]\[0-9\]+, \[qQ\]\[0-9\]+, #0" } } */
/* { dg-final { scan-assembler "vclt\.s32\[ 	\]+\[qQ\]\[0-9\]+, \[qQ\]\[0-9\]+, #0" } } */
/* { dg-final { scan-assembler "vcle\.s8\[ 	\]+\[dD\]\[0-9\]+, \[dD\]\[0-9\]+, #0" } } */
/* { dg-final { scan-assembler "vcle\.s16\[ 	\]+\[dD\]\[0-9\]+, \[dD\]\[0-9\]+, #0" } } */
/* { dg-final { scan-assembler "vcle\.s32\[ 	\]+\[dD\]\[0-9\]+, \[dD\]\[0-9\]+, #0" } } */
/* { dg-final { scan-assembler "vcle\.s8\[ 	\]+\[qQ\]\[0-9\]+, \[qQ\]\[0-9\]+, #0" } } */
/* { dg-final { scan-assembler "vcle\.s16\[ 	\]+\[qQ\]\[0-9\]+, \[qQ\]\[0-9\]+, #0" } } */
/* { dg-final { scan-assembler "vcle\.s32\[ 	\]+\[qQ\]\[0-9\]+, \[qQ\]\[0-9\]+, #0" } } */
/* { dg-final { scan-assembler-times "vceq\.i8\[ 	\]+\[dD\]\[0-9\]+, \[dD\]\[0-9\]+, #0" 4 } } */
/* { dg-final { scan-assembler-times "vceq\.i16\[ 	\]+\[dD\]\[0-9\]+, \[dD\]\[0-9\]+, #0" 4 } } */
/* { dg-final { scan-assembler-times "vceq\.i32\[ 	\]+\[dD\]\[0-9\]+, \[dD\]\[0-9\]+, #0" 4 } } */
/* { dg-final { scan-assembler-times "vceq\.i8\[ 	\]+\[qQ\]\[0-9\]+, \[qQ\]\[0-9\]+, #0" 4 } } */
/* { dg-final { scan-assembler-times "vceq\.i16\[ 	\]+\[qQ\]\[0-9\]+, \[qQ\]\[0-9\]+, #0" 4 } } */
/* { dg-final { scan-assembler-times "vceq\.i32\[ 	\]+\[qQ\]\[0-9\]+, \[qQ\]\[0-9\]+, #0" 4 } } */
/* { dg-final { scan-assembler-times "mov\[ 	\]+r\[0-9\]+, #-1|mvn\[ 	\]+r\[0-9\]+, #0" 6 } } */

/* And ensure we don't have unexpected output too.  */
/* { dg-final { scan-assembler-not "vc\[gl\]\[te\]\.u\[0-9\]+\[ 	\]+\[qQdD\]\[0-9\]+, \[qQdD\]\[0-9\]+, #0" } } */

/* Tidy up.  */
