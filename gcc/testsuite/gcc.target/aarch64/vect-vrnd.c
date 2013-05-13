/* { dg-do run } */
/* { dg-options "-O3 --save-temps" } */

#include <arm_neon.h>

extern void abort (void);
extern float fabsf (float);
extern double fabs (double);

extern double trunc (double);
extern double round (double);
extern double nearbyint (double);
extern double floor (double);
extern double ceil (double);
extern double rint (double);

extern float truncf (float);
extern float roundf (float);
extern float nearbyintf (float);
extern float floorf (float);
extern float ceilf (float);
extern float rintf (float);

#define NUM_TESTS 8
#define DELTA 0.000001

float input_f32[] = {0.1f, -0.1f, 0.4f, 10.3f,
		     200.0f, -800.0f, -13.0f, -0.5f};
double input_f64[] = {0.1, -0.1, 0.4, 10.3,
		      200.0, -800.0, -13.0, -0.5};

#define TEST(SUFFIX, Q, WIDTH, LANES, C_FN, F)		     		\
int									\
test_vrnd##SUFFIX##_float##WIDTH##x##LANES##_t (void)			\
{									\
  int ret = 1;								\
  int i = 0;								\
  int nlanes = LANES;							\
  float##WIDTH##_t expected_out[NUM_TESTS];				\
  float##WIDTH##_t actual_out[NUM_TESTS];				\
									\
  for (i = 0; i < NUM_TESTS; i++)					\
    {									\
      expected_out[i] = C_FN##F (input_f##WIDTH[i]);			\
      /* Don't vectorize this.  */					\
      asm volatile ("" : : : "memory");					\
    }									\
									\
  /* Prevent the compiler from noticing these two loops do the same	\
     thing and optimizing away the comparison.  */			\
  asm volatile ("" : : : "memory");					\
									\
  for (i = 0; i < NUM_TESTS; i+=nlanes)					\
    {									\
      float##WIDTH##x##LANES##_t out =					\
	vrnd##SUFFIX##Q##_f##WIDTH					\
		(vld1##Q##_f##WIDTH (input_f##WIDTH + i));		\
      vst1##Q##_f##WIDTH (actual_out + i, out);				\
    }									\
									\
  for (i = 0; i < NUM_TESTS; i++)					\
    ret &= fabs##F (expected_out[i] - actual_out[i]) < DELTA;		\
									\
  return ret;								\
}									\


#define BUILD_VARIANTS(SUFFIX, C_FN)	\
TEST (SUFFIX,  , 32, 2, C_FN, f)	\
TEST (SUFFIX, q, 32, 4, C_FN, f)	\
TEST (SUFFIX, q, 64, 2, C_FN,  )	\

BUILD_VARIANTS ( , trunc)
/* { dg-final { scan-assembler "frintz\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s" } } */
/* { dg-final { scan-assembler "frintz\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s" } } */
/* { dg-final { scan-assembler "frintz\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d" } } */
BUILD_VARIANTS (a, round)
/* { dg-final { scan-assembler "frinta\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s" } } */
/* { dg-final { scan-assembler "frinta\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s" } } */
/* { dg-final { scan-assembler "frinta\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d" } } */
BUILD_VARIANTS (i, nearbyint)
/* { dg-final { scan-assembler "frinti\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s" } } */
/* { dg-final { scan-assembler "frinti\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s" } } */
/* { dg-final { scan-assembler "frinti\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d" } } */
BUILD_VARIANTS (m, floor)
/* { dg-final { scan-assembler "frintm\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s" } } */
/* { dg-final { scan-assembler "frintm\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s" } } */
/* { dg-final { scan-assembler "frintm\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d" } } */
BUILD_VARIANTS (p, ceil)
/* { dg-final { scan-assembler "frintp\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s" } } */
/* { dg-final { scan-assembler "frintp\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s" } } */
/* { dg-final { scan-assembler "frintp\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d" } } */
BUILD_VARIANTS (x, rint)
/* { dg-final { scan-assembler "frintx\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s" } } */
/* { dg-final { scan-assembler "frintx\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s" } } */
/* { dg-final { scan-assembler "frintx\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d" } } */

#undef TEST
#define TEST(SUFFIX, Q, WIDTH, LANES, C_FN, F)			\
{								\
  if (!test_vrnd##SUFFIX##_float##WIDTH##x##LANES##_t ())	\
    abort ();							\
}

int
main (int argc, char **argv)
{
  BUILD_VARIANTS ( , trunc)
  BUILD_VARIANTS (a, round)
  BUILD_VARIANTS (i, nearbyint)
  BUILD_VARIANTS (m, floor)
  BUILD_VARIANTS (p, ceil)
  BUILD_VARIANTS (x, rint)
  return 0;
}

/* { dg-final { cleanup-saved-temps } } */
