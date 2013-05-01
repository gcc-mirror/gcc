/* { dg-do run } */
/* { dg-options "-O3 --save-temps" } */

#include <arm_neon.h>

extern void abort (void);
extern float fabsf (float);
extern double fabs (double);

#define NUM_TESTS 8

float input_s1[] = {0.1f, -0.1f, 0.4f, 10.3f, 200.0f, -800.0f, -13.0f, -0.5f};
float input_s2[] = {-0.2f, 0.4f, 0.04f, -100.3f, 2.0f, -80.0f, 13.0f, -0.5f};
double input_d1[] = {0.1, -0.1, 0.4, 10.3, 200.0, -800.0, -13.0, -0.5};
double input_d2[] = {-0.2, 0.4, 0.04, -100.3, 2.0, -80.0, 13.0, -0.5};

#define TEST(T, CMP, SUFFIX, WIDTH, LANES, Q, F)			\
int									\
test_vca##T##_float##WIDTH##x##LANES##_t (void)				\
{									\
  int ret = 0;								\
  int i = 0;								\
  uint##WIDTH##_t output[NUM_TESTS];					\
									\
  for (i = 0; i < NUM_TESTS; i++)					\
    {									\
      float##WIDTH##_t f1 = fabs##F (input_##SUFFIX##1[i]);		\
      float##WIDTH##_t f2 = fabs##F (input_##SUFFIX##2[i]);		\
      /* Inhibit optimization of our linear test loop.  */		\
      asm volatile ("" : : : "memory");					\
      output[i] = f1 CMP f2 ? -1 : 0;					\
    }									\
									\
  for (i = 0; i < NUM_TESTS; i += LANES)				\
    {									\
      float##WIDTH##x##LANES##_t in1 =					\
	vld1##Q##_f##WIDTH (input_##SUFFIX##1 + i);			\
      float##WIDTH##x##LANES##_t in2 =					\
	vld1##Q##_f##WIDTH (input_##SUFFIX##2 + i);			\
      uint##WIDTH##x##LANES##_t expected_out =				\
	vld1##Q##_u##WIDTH (output + i);				\
      uint##WIDTH##x##LANES##_t out =					\
	veor##Q##_u##WIDTH (vca##T##Q##_f##WIDTH (in1, in2),		\
			    expected_out);				\
      vst1##Q##_u##WIDTH (output + i, out);				\
    }									\
									\
  for (i = 0; i < NUM_TESTS; i++)					\
    ret |= output[i];							\
									\
  return ret;								\
}

#define BUILD_VARIANTS(T, CMP)	\
TEST (T, CMP, s, 32, 2,  , f)	\
TEST (T, CMP, s, 32, 4, q, f)	\
TEST (T, CMP, d, 64, 2, q,  )

BUILD_VARIANTS (ge, >=)
/* { dg-final { scan-assembler "facge\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s, v\[0-9\]+\.2s" } } */
/* { dg-final { scan-assembler "facge\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s, v\[0-9\]+\.4s" } } */
/* { dg-final { scan-assembler "facge\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d, v\[0-9\]+\.2d" } } */

BUILD_VARIANTS (gt, >)
/* { dg-final { scan-assembler "facgt\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s, v\[0-9\]+\.2s" } } */
/* { dg-final { scan-assembler "facgt\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s, v\[0-9\]+\.4s" } } */
/* { dg-final { scan-assembler "facgt\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d, v\[0-9\]+\.2d" } } */

/* No need for another scan-assembler as these tests
   also generate facge, facgt instructions.  */
BUILD_VARIANTS (le, <=)
BUILD_VARIANTS (lt, <)

#undef TEST
#define TEST(T, CMP, SUFFIX, WIDTH, LANES, Q, F)	\
if (test_vca##T##_float##WIDTH##x##LANES##_t ())	\
  abort ();

int
main (int argc, char **argv)
{
BUILD_VARIANTS (ge, >=)
BUILD_VARIANTS (gt, >)
BUILD_VARIANTS (le, <=)
BUILD_VARIANTS (lt, <)
  return 0;
}

/* { dg-final { cleanup-saved-temps } } */
