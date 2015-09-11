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

#define TEST(TEST, CMP, SUFFIX, WIDTH, F)				\
int									\
test_fca##TEST##SUFFIX##_float##WIDTH##_t (void)			\
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
  for (i = 0; i < NUM_TESTS; i++)					\
    {									\
      output[i] = vca##TEST##SUFFIX##_f##WIDTH (input_##SUFFIX##1[i],	\
						input_##SUFFIX##2[i])	\
						  ^ output[i];		\
      /* Inhibit autovectorization of our scalar test loop.  */		\
      asm volatile ("" : : : "memory");					\
    }									\
									\
  for (i = 0; i < NUM_TESTS; i++)					\
    ret |= output[i];							\
									\
  return ret;								\
}

TEST (ge, >=, s, 32, f)
/* { dg-final { scan-assembler "facge\\ts\[0-9\]+, s\[0-9\]+, s\[0-9\]+" } } */
TEST (ge, >=, d, 64, )
/* { dg-final { scan-assembler "facge\\td\[0-9\]+, d\[0-9\]+, d\[0-9\]+" } } */
TEST (gt, >, s, 32, f)
/* { dg-final { scan-assembler "facgt\\ts\[0-9\]+, s\[0-9\]+, s\[0-9\]+" } } */
TEST (gt, >, d, 64, )
/* { dg-final { scan-assembler "facgt\\td\[0-9\]+, d\[0-9\]+, d\[0-9\]+" } } */

int
main (int argc, char **argv)
{
  if (test_fcages_float32_t ())
    abort ();
  if (test_fcaged_float64_t ())
    abort ();
  if (test_fcagts_float32_t ())
    abort ();
  if (test_fcagtd_float64_t ())
    abort ();
  return 0;
}

