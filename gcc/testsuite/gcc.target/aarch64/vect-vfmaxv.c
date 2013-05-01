/* { dg-do run } */
/* { dg-options "-O3 --save-temps -ffast-math" } */

#include <arm_neon.h>

extern void abort (void);

extern float fabsf (float);
extern double fabs (double);
extern int isnan (double);
extern float fmaxf (float, float);
extern float fminf (float, float);
extern double fmax (double, double);
extern double fmin (double, double);

#define NUM_TESTS 16
#define DELTA 0.000001
#define NAN (0.0 / 0.0)

float input_float32[] = {0.1f, -0.1f, 0.4f, 10.3f,
			 200.0f, -800.0f, -13.0f, -0.5f,
			 NAN, -870.0f, 10.4f, 310.11f,
			 0.0f, -865.0f, -2213.0f, -1.5f};

double input_float64[] = {0.1, -0.1, 0.4, 10.3,
			  200.0, -800.0, -13.0, -0.5,
			  NAN, -870.0, 10.4, 310.11,
			  0.0, -865.0, -2213.0, -1.5};

#define EQUALF(a, b) (fabsf (a - b) < DELTA)
#define EQUALD(a, b) (fabs (a - b) < DELTA)

/* Floating point 'unordered' variants.  */

#undef TEST
#define TEST(MAXMIN, CMP_OP, SUFFIX, Q, TYPE, LANES, FLOAT)		\
int									\
test_v##MAXMIN##v##SUFFIX##_##TYPE##x##LANES##_t (void)			\
{									\
  int i, j;								\
  int moves = (NUM_TESTS - LANES) + 1;					\
  TYPE##_t out_l[NUM_TESTS];						\
  TYPE##_t out_v[NUM_TESTS];						\
									\
  /* Calculate linearly.  */						\
  for (i = 0; i < moves; i++)						\
    {									\
      out_l[i] = input_##TYPE[i];					\
      for (j = 0; j < LANES; j++)					\
	{								\
	  if (isnan (out_l[i]))						\
	    continue;							\
	  if (isnan (input_##TYPE[i + j])				\
	      || input_##TYPE[i + j] CMP_OP out_l[i])			\
	    out_l[i] = input_##TYPE[i + j];				\
	}								\
    }									\
									\
  /* Calculate using vector reduction intrinsics.  */			\
  for (i = 0; i < moves; i++)						\
    {									\
      TYPE##x##LANES##_t t1 = vld1##Q##_##SUFFIX (input_##TYPE + i);	\
      out_v[i] = v##MAXMIN##v##Q##_##SUFFIX (t1);			\
    }									\
									\
  /* Compare.  */							\
  for (i = 0; i < moves; i++)						\
    {									\
      if (!EQUAL##FLOAT (out_v[i], out_l[i])				\
			 && !(isnan (out_v[i]) && isnan (out_l[i])))	\
	return 0;							\
    }									\
  return 1;								\
}

#define BUILD_VARIANTS(TYPE, STYPE, W32, W64, F)	\
TEST (max, >, STYPE,  , TYPE, W32, F)			\
TEST (max, >, STYPE, q, TYPE, W64, F)			\
TEST (min, <, STYPE,  , TYPE, W32, F)			\
TEST (min, <, STYPE, q, TYPE, W64, F)

BUILD_VARIANTS (float32, f32, 2, 4, F)
/* { dg-final { scan-assembler "fmaxp\\ts\[0-9\]+, v\[0-9\]+\.2s" } } */
/* { dg-final { scan-assembler "fminp\\ts\[0-9\]+, v\[0-9\]+\.2s" } } */
/* { dg-final { scan-assembler "fmaxv\\ts\[0-9\]+, v\[0-9\]+\.4s" } } */
/* { dg-final { scan-assembler "fminv\\ts\[0-9\]+, v\[0-9\]+\.4s" } } */
TEST (max, >, f64, q, float64, 2, D)
/* { dg-final { scan-assembler "fmaxp\\td\[0-9\]+, v\[0-9\]+\.2d" } } */
TEST (min, <, f64, q, float64, 2, D)
/* { dg-final { scan-assembler "fminp\\td\[0-9\]+, v\[0-9\]+\.2d" } } */

/* Floating point 'nm' variants.  */

#undef TEST
#define TEST(MAXMIN, F, SUFFIX, Q, TYPE, LANES, FLOAT)			\
int									\
test_v##MAXMIN##nmv##SUFFIX##_##TYPE##x##LANES##_t (void)		\
{									\
  int i, j;								\
  int moves = (NUM_TESTS - LANES) + 1;					\
  TYPE##_t out_l[NUM_TESTS];						\
  TYPE##_t out_v[NUM_TESTS];						\
									\
  /* Calculate linearly.  */						\
  for (i = 0; i < moves; i++)						\
    {									\
      out_l[i] = input_##TYPE[i];					\
      for (j = 0; j < LANES; j++)					\
	out_l[i] = f##MAXMIN##F (input_##TYPE[i + j],  out_l[i]);	\
    }									\
									\
  /* Calculate using vector reduction intrinsics.  */			\
  for (i = 0; i < moves; i++)						\
    {									\
      TYPE##x##LANES##_t t1 = vld1##Q##_##SUFFIX (input_##TYPE + i);	\
      out_v[i] = v##MAXMIN##nmv##Q##_##SUFFIX (t1);			\
    }									\
									\
  /* Compare.  */							\
  for (i = 0; i < moves; i++)						\
    {									\
      if (!EQUAL##FLOAT (out_v[i], out_l[i]))				\
	return 0;							\
    }									\
  return 1;								\
}

TEST (max, f, f32, , float32, 2, D)
/* { dg-final { scan-assembler "fmaxnmp\\ts\[0-9\]+, v\[0-9\]+\.2s" } } */
TEST (min, f, f32, , float32, 2, D)
/* { dg-final { scan-assembler "fminnmp\\ts\[0-9\]+, v\[0-9\]+\.2s" } } */
TEST (max, f, f32, q, float32, 4, D)
/* { dg-final { scan-assembler "fmaxnmv\\ts\[0-9\]+, v\[0-9\]+\.4s" } } */
TEST (min, f, f32, q, float32, 4, D)
/* { dg-final { scan-assembler "fminnmv\\ts\[0-9\]+, v\[0-9\]+\.4s" } } */
TEST (max, , f64, q, float64, 2, D)
/* { dg-final { scan-assembler "fmaxnmp\\td\[0-9\]+, v\[0-9\]+\.2d" } } */
TEST (min, , f64, q, float64, 2, D)
/* { dg-final { scan-assembler "fminnmp\\td\[0-9\]+, v\[0-9\]+\.2d" } } */

#undef TEST
#define TEST(MAXMIN, CMP_OP, SUFFIX, Q, TYPE, LANES, FLOAT)		\
{									\
  if (!test_v##MAXMIN##v##SUFFIX##_##TYPE##x##LANES##_t ())		\
    abort ();								\
}

int
main (int argc, char **argv)
{
  BUILD_VARIANTS (float32, f32, 2, 4, F)
  TEST (max, >, f64, q, float64, 2, D)
  TEST (min, <, f64, q, float64, 2, D)

#undef TEST
#define TEST(MAXMIN, CMP_OP, SUFFIX, Q, TYPE, LANES, FLOAT)		\
{									\
  if (!test_v##MAXMIN##nmv##SUFFIX##_##TYPE##x##LANES##_t ())		\
    abort ();								\
}

  BUILD_VARIANTS (float32, f32, 2, 4, F)
  TEST (max, >, f64, q, float64, 2, D)
  TEST (min, <, f64, q, float64, 2, D)

  return 0;
}

/* { dg-final { cleanup-saved-temps } } */
