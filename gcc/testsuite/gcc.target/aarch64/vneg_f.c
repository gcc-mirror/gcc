/* Test vneg works correctly.  */
/* { dg-do run } */
/* { dg-options "--save-temps" } */

#include <arm_neon.h>

#define FLT_EPSILON __FLT_EPSILON__
#define DBL_EPSILON __DBL_EPSILON__
#define FLT_MAX __FLT_MAX__
#define FLT_MIN __FLT_MIN__
#define DBL_MAX __DBL_MAX__
#define DBL_MIN __DBL_MIN__

#define TEST0 0
/* 6 digits of pi.  */
#define TEST1 3.14159
/* 6 digits of -e.  */
#define TEST2 -2.71828
/* 2^25, float has 24 significand bits
   according to Single-precision floating-point format.  */
#define TEST3_FLT 33554432
/* 2^54, double has 53 significand bits
   according to Double-precision floating-point format.  */
#define TEST3_DBL 18014398509481984

extern void abort (void);

#define FLT_INFINITY (__builtin_inff ())
#define DBL_INFINITY (__builtin_inf ())

#ifndef NAN
#define NAN (0.0 / 0.0)
#endif

#define CONCAT(a, b) a##b
#define CONCAT1(a, b) CONCAT (a, b)
#define REG_INFEX64 _
#define REG_INFEX128 q_
#define REG_INFEX(reg_len) REG_INFEX##reg_len
#define POSTFIX(reg_len, data_len) \
  CONCAT1 (REG_INFEX (reg_len), f##data_len)

#define DATA_TYPE_32 float
#define DATA_TYPE_64 double
#define DATA_TYPE(data_len) DATA_TYPE_##data_len

#define INDEX64_32 [i]
#define INDEX64_64
#define INDEX128_32 [i]
#define INDEX128_64 [i]
#define INDEX(reg_len, data_len) \
  CONCAT1 (INDEX, reg_len##_##data_len)

#define LOAD_INST(reg_len, data_len) \
  CONCAT1 (vld1, POSTFIX (reg_len, data_len))
#define NEG_INST(reg_len, data_len) \
  CONCAT1 (vneg, POSTFIX (reg_len, data_len))

#define INHIB_OPTIMIZATION asm volatile ("" : : : "memory")

#define RUN_TEST(test_set, reg_len, data_len, n, a, b) \
  {						       \
    int i;					       \
    (a) = LOAD_INST (reg_len, data_len) (test_set);    \
    (b) = NEG_INST (reg_len, data_len) (a);	       \
    for (i = 0; i < n; i++)			       \
      {						       \
	DATA_TYPE (data_len) diff;		       \
	INHIB_OPTIMIZATION;			       \
	diff					       \
	  = a INDEX (reg_len, data_len)		       \
	    + b INDEX (reg_len, data_len);	       \
	if (diff > EPSILON)			       \
	  return 1;				       \
      }						       \
  }

#define TEST3 TEST3_FLT
#define EPSILON FLT_EPSILON
#define VAR_MIN FLT_MIN
#define VAR_MAX FLT_MAX
#define INFINITY FLT_INFINITY

int
test_vneg_f32 ()
{
  float test_set0[2] = { TEST0, TEST1 };
  float test_set1[2] = { TEST2, TEST3 };
  float test_set2[2] = { VAR_MAX, VAR_MIN };
  float test_set3[2] = { INFINITY, NAN };

  float32x2_t a;
  float32x2_t b;

  RUN_TEST (test_set0, 64, 32, 2, a, b);
  RUN_TEST (test_set1, 64, 32, 2, a, b);
  RUN_TEST (test_set2, 64, 32, 2, a, b);
  RUN_TEST (test_set3, 64, 32, 0, a, b);

  /* Since last test cannot be checked in a uniform way by adding
     negation result to original value, the number of lanes to be
     checked in RUN_TEST is 0 (last argument).  Instead, result
     will be checked manually.  */

  if (b[0] != -INFINITY)
    return 1;

  if (!__builtin_isnan (b[1]))
    return 1;

  return 0;
}

/* { dg-final { scan-assembler-times "fneg\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s" 4 } } */

#undef TEST3
#undef EPSILON
#undef VAR_MIN
#undef VAR_MAX
#undef INFINITY

#define TEST3 TEST3_DBL
#define EPSILON DBL_EPSILON
#define VAR_MIN DBL_MIN
#define VAR_MAX DBL_MAX
#define INFINITY DBL_INFINITY

int
test_vneg_f64 ()
{
  float64x1_t a;
  float64x1_t b;

  double test_set0[1] = { TEST0 };
  double test_set1[1] = { TEST1 };
  double test_set2[1] = { TEST2 };
  double test_set3[1] = { TEST3 };
  double test_set4[1] = { VAR_MAX };
  double test_set5[1] = { VAR_MIN };
  double test_set6[1] = { INFINITY };
  double test_set7[1] = { NAN };

  RUN_TEST (test_set0, 64, 64, 1, a, b);
  RUN_TEST (test_set1, 64, 64, 1, a, b);
  RUN_TEST (test_set2, 64, 64, 1, a, b);
  RUN_TEST (test_set3, 64, 64, 1, a, b);
  RUN_TEST (test_set4, 64, 64, 1, a, b);
  RUN_TEST (test_set5, 64, 64, 1, a, b);
  RUN_TEST (test_set6, 64, 64, 0, a, b);

  /* Since last test cannot be checked in a uniform way by adding
     negation result to original value, the number of lanes to be
     checked in RUN_TEST is 0 (last argument).  Instead, result
     will be checked manually.  */

  if (b != -INFINITY)
    return 1;

  /* Same as above.  */

  RUN_TEST (test_set7, 64, 64, 0, a, b);

  if (!__builtin_isnan (b))
    return 1;

  return 0;
}

/* { dg-final { scan-assembler-times "fneg\\td\[0-9\]+, d\[0-9\]+" 8 } } */

#undef TEST3
#undef EPSILON
#undef VAR_MIN
#undef VAR_MAX
#undef INFINITY

#define TEST3 TEST3_FLT
#define EPSILON FLT_EPSILON
#define VAR_MIN FLT_MIN
#define VAR_MAX FLT_MAX
#define INFINITY FLT_INFINITY

int
test_vnegq_f32 ()
{
  float32x4_t a;
  float32x4_t b;

  float test_set0[4] = { TEST0, TEST1, TEST2, TEST3 };
  float test_set1[4] = { FLT_MAX, FLT_MIN, INFINITY, NAN };

  RUN_TEST (test_set0, 128, 32, 4, a, b);
  RUN_TEST (test_set1, 128, 32, 2, a, b);

  /* Since last test cannot be fully checked in a uniform way by
     adding negation result to original value, the number of lanes
     to be checked in RUN_TEST is 0 (last argument).  Instead, result
     will be checked manually.  */

  if (b[2] != -INFINITY)
    return 1;

  if (!__builtin_isnan (b[3]))
    return 1;

  return 0;
}

/* { dg-final { scan-assembler-times "fneg\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s" 2 } } */

#undef TEST3
#undef EPSILON
#undef VAR_MIN
#undef VAR_MAX
#undef INFINITY

#define TEST3 TEST3_DBL
#define EPSILON DBL_EPSILON
#define VAR_MIN DBL_MIN
#define VAR_MAX DBL_MAX
#define INFINITY DBL_INFINITY

int
test_vnegq_f64 ()
{
  float64x2_t a;
  float64x2_t b;

  double test_set0[2] = { TEST0, TEST1 };
  double test_set1[2] = { TEST2, TEST3 };
  double test_set2[2] = { FLT_MAX, FLT_MIN };
  double test_set3[2] = { INFINITY, NAN };

  RUN_TEST (test_set0, 128, 64, 2, a, b);
  RUN_TEST (test_set1, 128, 64, 2, a, b);
  RUN_TEST (test_set2, 128, 64, 2, a, b);
  RUN_TEST (test_set3, 128, 64, 0, a, b);

  /* Since last test cannot be checked in a uniform way by adding
     negation result to original value, the number of lanes to be
     checked in RUN_TEST is 0 (last argument).  Instead, result
     will be checked manually.  */

  if (b[0] != -INFINITY)
    return 1;

  if (!__builtin_isnan (b[1]))
    return 1;

  return 0;
}

/* { dg-final { scan-assembler-times "fneg\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d" 4 } } */

int
main (int argc, char **argv)
{
  if (test_vneg_f32 ())
    abort ();

  if (test_vneg_f64 ())
    abort ();

  if (test_vnegq_f32 ())
    abort ();

  if (test_vnegq_f64 ())
    abort ();

  return 0;
}

/* { dg-final { cleanup-saved-temps } } */
