/* { dg-do run } */
/* { dg-options "-O3 --save-temps -ffast-math" } */

#include <arm_neon.h>

extern void abort (void);
extern double fabs (double);

#define NUM_TESTS 8
#define DELTA 0.000001

float input_f32[] = {0.1f, -0.1f, 0.4f, 10.3f,
		     200.0f, -800.0f, -13.0f, -0.5f};
double input_f64[] = {0.1, -0.1, 0.4, 10.3,
		      200.0, -800.0, -13.0, -0.5};

#define TEST(SUFFIX, Q, WIDTH, LANES, S, U, D)		     		\
int									\
test_vcvt##SUFFIX##_##S##WIDTH##_f##WIDTH##x##LANES##_t (void)		\
{									\
  int ret = 1;								\
  int i = 0;								\
  int nlanes = LANES;							\
  U##int##WIDTH##_t expected_out[NUM_TESTS];				\
  U##int##WIDTH##_t actual_out[NUM_TESTS];				\
									\
  for (i = 0; i < NUM_TESTS; i++)					\
    {									\
      expected_out[i]							\
	= vcvt##SUFFIX##D##_##S##WIDTH##_f##WIDTH (input_f##WIDTH[i]);	\
      /* Don't vectorize this.  */					\
      asm volatile ("" : : : "memory");					\
    }									\
									\
  for (i = 0; i < NUM_TESTS; i+=nlanes)					\
    {									\
      U##int##WIDTH##x##LANES##_t out =					\
	vcvt##SUFFIX##Q##_##S##WIDTH##_f##WIDTH				\
		(vld1##Q##_f##WIDTH (input_f##WIDTH + i));		\
      vst1##Q##_##S##WIDTH (actual_out + i, out);			\
    }									\
									\
  for (i = 0; i < NUM_TESTS; i++)					\
    ret &= fabs (expected_out[i] - actual_out[i]) < DELTA;		\
									\
  return ret;								\
}									\


#define BUILD_VARIANTS(SUFFIX)			\
TEST (SUFFIX,  , 32, 2, s, ,s)			\
TEST (SUFFIX, q, 32, 4, s, ,s)			\
TEST (SUFFIX, q, 64, 2, s, ,d)			\
TEST (SUFFIX,  , 32, 2, u,u,s)			\
TEST (SUFFIX, q, 32, 4, u,u,s)			\
TEST (SUFFIX, q, 64, 2, u,u,d)			\

BUILD_VARIANTS ( )
/* { dg-final { scan-assembler "fcvtzs\\t(w|s)\[0-9\]+, s\[0-9\]+" } } */
/* { dg-final { scan-assembler "fcvtzs\\t(x|d)\[0-9\]+, d\[0-9\]+" } } */
/* { dg-final { scan-assembler "fcvtzs\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s" } } */
/* { dg-final { scan-assembler "fcvtzs\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s" } } */
/* { dg-final { scan-assembler "fcvtzs\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d" } } */
/* { dg-final { scan-assembler "fcvtzu\\t(w|s)\[0-9\]+, s\[0-9\]+" } } */
/* { dg-final { scan-assembler "fcvtzu\\t(x|d)\[0-9\]+, d\[0-9\]+" } } */
/* { dg-final { scan-assembler "fcvtzu\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s" } } */
/* { dg-final { scan-assembler "fcvtzu\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s" } } */
/* { dg-final { scan-assembler "fcvtzu\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d" } } */
BUILD_VARIANTS (a)
/* { dg-final { scan-assembler "fcvtas\\tw\[0-9\]+, s\[0-9\]+" } } */
/* { dg-final { scan-assembler "fcvtas\\tx\[0-9\]+, d\[0-9\]+" } } */
/* { dg-final { scan-assembler "fcvtas\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s" } } */
/* { dg-final { scan-assembler "fcvtas\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s" } } */
/* { dg-final { scan-assembler "fcvtas\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d" } } */
/* { dg-final { scan-assembler "fcvtau\\tw\[0-9\]+, s\[0-9\]+" } } */
/* { dg-final { scan-assembler "fcvtau\\tx\[0-9\]+, d\[0-9\]+" } } */
/* { dg-final { scan-assembler "fcvtau\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s" } } */
/* { dg-final { scan-assembler "fcvtau\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s" } } */
/* { dg-final { scan-assembler "fcvtau\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d" } } */
BUILD_VARIANTS (m)
/* { dg-final { scan-assembler "fcvtms\\tw\[0-9\]+, s\[0-9\]+" } } */
/* { dg-final { scan-assembler "fcvtms\\tx\[0-9\]+, d\[0-9\]+" } } */
/* { dg-final { scan-assembler "fcvtms\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s" } } */
/* { dg-final { scan-assembler "fcvtms\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s" } } */
/* { dg-final { scan-assembler "fcvtms\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d" } } */
/* { dg-final { scan-assembler "fcvtmu\\tw\[0-9\]+, s\[0-9\]+" } } */
/* { dg-final { scan-assembler "fcvtmu\\tx\[0-9\]+, d\[0-9\]+" } } */
/* { dg-final { scan-assembler "fcvtmu\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s" } } */
/* { dg-final { scan-assembler "fcvtmu\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s" } } */
/* { dg-final { scan-assembler "fcvtmu\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d" } } */
BUILD_VARIANTS (n)
/* { dg-final { scan-assembler "fcvtns\\tw\[0-9\]+, s\[0-9\]+" } } */
/* { dg-final { scan-assembler "fcvtns\\tx\[0-9\]+, d\[0-9\]+" } } */
/* { dg-final { scan-assembler "fcvtns\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s" } } */
/* { dg-final { scan-assembler "fcvtns\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s" } } */
/* { dg-final { scan-assembler "fcvtns\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d" } } */
/* { dg-final { scan-assembler "fcvtnu\\tw\[0-9\]+, s\[0-9\]+" } } */
/* { dg-final { scan-assembler "fcvtnu\\tx\[0-9\]+, d\[0-9\]+" } } */
/* { dg-final { scan-assembler "fcvtnu\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s" } } */
/* { dg-final { scan-assembler "fcvtnu\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s" } } */
/* { dg-final { scan-assembler "fcvtnu\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d" } } */
BUILD_VARIANTS (p)
/* { dg-final { scan-assembler "fcvtps\\tw\[0-9\]+, s\[0-9\]+" } } */
/* { dg-final { scan-assembler "fcvtps\\tx\[0-9\]+, d\[0-9\]+" } } */
/* { dg-final { scan-assembler "fcvtps\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s" } } */
/* { dg-final { scan-assembler "fcvtps\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s" } } */
/* { dg-final { scan-assembler "fcvtps\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d" } } */
/* { dg-final { scan-assembler "fcvtpu\\tw\[0-9\]+, s\[0-9\]+" } } */
/* { dg-final { scan-assembler "fcvtpu\\tx\[0-9\]+, d\[0-9\]+" } } */
/* { dg-final { scan-assembler "fcvtpu\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s" } } */
/* { dg-final { scan-assembler "fcvtpu\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s" } } */
/* { dg-final { scan-assembler "fcvtpu\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d" } } */

#undef TEST
#define TEST(SUFFIX, Q, WIDTH, LANES, S, U, D)		     		\
{									\
  if (!test_vcvt##SUFFIX##_##S##WIDTH##_f##WIDTH##x##LANES##_t ())	\
    abort ();								\
}

int
main (int argc, char **argv)
{
  BUILD_VARIANTS ( )
  BUILD_VARIANTS (a)
  BUILD_VARIANTS (m)
  BUILD_VARIANTS (n)
  BUILD_VARIANTS (p)
  return 0;
}

