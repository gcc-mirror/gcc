/* Test vdiv works correctly.  */
/* { dg-do run } */
/* { dg-options "-O3 --save-temps" } */

#include <arm_neon.h>

#define FLT_INFINITY (__builtin_inff ())
#define DBL_INFINITY (__builtin_inf ())

#define NAN (0.0 / 0.0)

#define PI 3.141592653589793
#define PI_4 0.7853981633974483
#define SQRT2 1.4142135623730951
#define SQRT1_2 0.7071067811865475

#define TESTA0 PI
#define TESTA1 -PI
#define TESTA2 PI
#define TESTA3 -PI
#define TESTA4 1.0
#define TESTA5 -1.0
#define TESTA6 1.0
#define TESTA7 -1.0
/* 2^25+1, float has 24 significand bits
   according to Single-precision floating-point format.  */
#define TESTA8_FLT 33554433
/* 2^54+1, double has 53 significand bits
   according to Double-precision floating-point format.  */
#define TESTA8_DBL 18014398509481985
#define TESTA9 -TESTA8
#define TESTA10 TESTA8
#define TESTA11 -TESTA8
#define TESTA12 NAN
#define TESTA13 1.0
#define TESTA14 INFINITY
#define TESTA15 -INFINITY
#define TESTA16 INFINITY
#define TESTA17 9.0
#define TESTA18 11.0
#define TESTA19 13.0

#define TESTB0 4.0
#define TESTB1 4.0
#define TESTB2 -4.0
#define TESTB3 -4.0
#define TESTB4 SQRT2
#define TESTB5 SQRT2
#define TESTB6 -SQRT2
#define TESTB7 -SQRT2
#define TESTB8 2.0
#define TESTB9 2.0
#define TESTB10 -2.0
#define TESTB11 -2.0
#define TESTB12 3.0
#define TESTB13 NAN
#define TESTB14 5.0
#define TESTB15 7.0
#define TESTB16 INFINITY
#define TESTB17 INFINITY
#define TESTB18 -INFINITY
#define TESTB19 0

#define ANSW0 PI_4
#define ANSW1 -PI_4
#define ANSW2 -PI_4
#define ANSW3 PI_4
#define ANSW4 SQRT1_2
#define ANSW5 -SQRT1_2
#define ANSW6 -SQRT1_2
#define ANSW7 SQRT1_2
#define ANSW8_FLT 16777216
#define ANSW8_DBL 9007199254740992
#define ANSW9 -ANSW8
#define ANSW10 -ANSW8
#define ANSW11 ANSW8
#define ANSW12 NAN
#define ANSW13 NAN
#define ANSW14 INFINITY
#define ANSW15 -INFINITY
#define ANSW16 NAN
#define ANSW17 0
#define ANSW18 0
#define ANSW19 INFINITY

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

#define EPSILON_32 __FLT_EPSILON__
#define EPSILON_64 __DBL_EPSILON__
#define EPSILON(data_len) EPSILON_##data_len

#define INDEX64_32 [i]
#define INDEX64_64
#define INDEX128_32 [i]
#define INDEX128_64 [i]
#define INDEX(reg_len, data_len) \
  CONCAT1 (INDEX, reg_len##_##data_len)

#define LOAD_INST(reg_len, data_len) \
  CONCAT1 (vld1, POSTFIX (reg_len, data_len))
#define DIV_INST(reg_len, data_len) \
  CONCAT1 (vdiv, POSTFIX (reg_len, data_len))

#define ABS(a) __builtin_fabs (a)
#define ISNAN(a) __builtin_isnan (a)
#define FP_equals(a, b, epsilon)			\
  (							\
   ((a) == (b))						\
    || (ISNAN (a) && ISNAN (b))				\
    || (ABS (a - b) < epsilon)				\
  )

#define INHIB_OPTIMIZATION asm volatile ("" : : : "memory")

#define RUN_TEST(a, b, c, testseta, testsetb, answset, count,		\
		 reg_len, data_len, n)					\
{									\
  int i;								\
  INHIB_OPTIMIZATION;							\
  (a) = LOAD_INST (reg_len, data_len) (testseta[count]);		\
  (b) = LOAD_INST (reg_len, data_len) (testsetb[count]);		\
  (c) = LOAD_INST (reg_len, data_len) (answset[count]);			\
  INHIB_OPTIMIZATION;							\
  (a) = DIV_INST (reg_len, data_len) (a, b);				\
  for (i = 0; i < n; i++)						\
  {									\
    INHIB_OPTIMIZATION;							\
    if (!FP_equals ((a) INDEX (reg_len, data_len),			\
		    (c) INDEX (reg_len, data_len),			\
		    EPSILON (data_len)))				\
      return 1;								\
  }									\
}

extern void abort (void);

#define TESTA8 TESTA8_FLT
#define ANSW8 ANSW8_FLT
#define INFINITY FLT_INFINITY

int
test_vdiv_f32 ()
{
  int count;
  float32x2_t a;
  float32x2_t b;
  float32x2_t c;

  float32_t testseta[10][2] = {
    { TESTA0, TESTA1 }, { TESTA2, TESTA3 },
    { TESTA4, TESTA5 }, { TESTA6, TESTA7 },
    { TESTA8, TESTA9 }, { TESTA10, TESTA11 },
    { TESTA12, TESTA13 }, { TESTA14, TESTA15 },
    { TESTA16, TESTA17 }, { TESTA18, TESTA19 }
  };

  float32_t testsetb[10][2] = {
    { TESTB0, TESTB1 }, { TESTB2, TESTB3 },
    { TESTB4, TESTB5 }, { TESTB6, TESTB7 },
    { TESTB8, TESTB9 }, { TESTB10, TESTB11 },
    { TESTB12, TESTB13 }, { TESTB14, TESTB15 },
    { TESTB16, TESTB17 }, { TESTB18, TESTB19 }
  };

  float32_t answset[10][2] = {
    { ANSW0, ANSW1 }, { ANSW2, ANSW3 },
    { ANSW4, ANSW5 }, { ANSW6, ANSW7 },
    { ANSW8, ANSW9 }, { ANSW10, ANSW11 },
    { ANSW12, ANSW13 }, { ANSW14, ANSW15 },
    { ANSW16, ANSW17 }, { ANSW18, ANSW19 }
  };

  for (count = 0; count < 10; count++)
    {
      RUN_TEST (a, b, c, testseta, testsetb, answset, count, 64, 32, 2);
    }

  return 0;
}

/* { dg-final { scan-assembler-times "fdiv\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s, v\[0-9\]+\.2s" 1 } } */

#undef TESTA8
#undef ANSW8
#undef INFINITY

#define TESTA8 TESTA8_DBL
#define ANSW8 ANSW8_DBL
#define INFINITY DBL_INFINITY

int
test_vdiv_f64 ()
{
  int count;
  float64x1_t a;
  float64x1_t b;
  float64x1_t c;

  float64_t testseta[20][1] = {
    { TESTA0 }, { TESTA1 }, { TESTA2 }, { TESTA3 },
    { TESTA4 }, { TESTA5 }, { TESTA6 }, { TESTA7 },
    { TESTA8 }, { TESTA9 }, { TESTA10 }, { TESTA11 },
    { TESTA12 }, { TESTA13 }, { TESTA14 }, { TESTA15 },
    { TESTA16 }, { TESTA17 }, { TESTA18 }, { TESTA19 }
  };

  float64_t testsetb[20][1] = {
    { TESTB0 }, { TESTB1 }, { TESTB2 }, { TESTB3 },
    { TESTB4 }, { TESTB5 }, { TESTB6 }, { TESTB7 },
    { TESTB8 }, { TESTB9 }, { TESTB10 }, { TESTB11 },
    { TESTB12 }, { TESTB13 }, { TESTB14 }, { TESTB15 },
    { TESTB16 }, { TESTB17 }, { TESTB18 }, { TESTB19 }
  };

  float64_t answset[20][1] = {
    { ANSW0 }, { ANSW1 }, { ANSW2 }, { ANSW3 },
    { ANSW4 }, { ANSW5 }, { ANSW6 }, { ANSW7 },
    { ANSW8 }, { ANSW9 }, { ANSW10 }, { ANSW11 },
    { ANSW12 }, { ANSW13 }, { ANSW14 }, { ANSW15 },
    { ANSW16 }, { ANSW17 }, { ANSW18 }, { ANSW19 }
  };

  for (count = 0; count < 20; count++)
    {
      RUN_TEST (a, b, c, testseta, testsetb, answset, count, 64, 64, 1);
    }
  return 0;
}

/* The following assembly should match 2 more times,
   in 64bit NAN generation.  */
/* { dg-final { scan-assembler-times "fdiv\\td\[0-9\]+, d\[0-9\]+, d\[0-9\]+" 3 } } */

#undef TESTA8
#undef ANSW8
#undef INFINITY

#define TESTA8 TESTA8_FLT
#define ANSW8 ANSW8_FLT
#define INFINITY FLT_INFINITY

int
test_vdivq_f32 ()
{
  int count;
  float32x4_t a;
  float32x4_t b;
  float32x4_t c;

  float32_t testseta[5][4] = {
    { TESTA0, TESTA1, TESTA2, TESTA3 },
    { TESTA4, TESTA5, TESTA6, TESTA7 },
    { TESTA8, TESTA9, TESTA10, TESTA11 },
    { TESTA12, TESTA13, TESTA14, TESTA15 },
    { TESTA16, TESTA17, TESTA18, TESTA19 }
  };

  float32_t testsetb[5][4] = {
    { TESTB0, TESTB1, TESTB2, TESTB3 },
    { TESTB4, TESTB5, TESTB6, TESTB7 },
    { TESTB8, TESTB9, TESTB10, TESTB11 },
    { TESTB12, TESTB13, TESTB14, TESTB15 },
    { TESTB16, TESTB17, TESTB18, TESTB19 }
  };

  float32_t answset[5][4] = {
    { ANSW0, ANSW1, ANSW2, ANSW3 },
    { ANSW4, ANSW5, ANSW6, ANSW7 },
    { ANSW8, ANSW9, ANSW10, ANSW11 },
    { ANSW12, ANSW13, ANSW14, ANSW15 },
    { ANSW16, ANSW17, ANSW18, ANSW19 }
  };

  for (count = 0; count < 5; count++)
    {
      RUN_TEST (a, b, c, testseta, testsetb, answset, count, 128, 32, 4);
    }
  return 0;
}

/* { dg-final { scan-assembler-times "fdiv\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s, v\[0-9\]+\.4s" 1 } } */

#undef TESTA8
#undef ANSW8
#undef INFINITY

#define TESTA8 TESTA8_DBL
#define ANSW8 ANSW8_DBL
#define INFINITY DBL_INFINITY

int
test_vdivq_f64 ()
{
  int count;
  float64x2_t a;
  float64x2_t b;
  float64x2_t c;

  float64_t testseta[10][2] = {
    { TESTA0, TESTA1 }, { TESTA2, TESTA3 },
    { TESTA4, TESTA5 }, { TESTA6, TESTA7 },
    { TESTA8, TESTA9 }, { TESTA10, TESTA11 },
    { TESTA12, TESTA13 }, { TESTA14, TESTA15 },
    { TESTA16, TESTA17 }, { TESTA18, TESTA19 }
  };

  float64_t testsetb[10][2] = {
    { TESTB0, TESTB1 }, { TESTB2, TESTB3 },
    { TESTB4, TESTB5 }, { TESTB6, TESTB7 },
    { TESTB8, TESTB9 }, { TESTB10, TESTB11 },
    { TESTB12, TESTB13 }, { TESTB14, TESTB15 },
    { TESTB16, TESTB17 }, { TESTB18, TESTB19 }
  };

  float64_t answset[10][2] = {
    { ANSW0, ANSW1 }, { ANSW2, ANSW3 },
    { ANSW4, ANSW5 }, { ANSW6, ANSW7 },
    { ANSW8, ANSW9 }, { ANSW10, ANSW11 },
    { ANSW12, ANSW13 }, { ANSW14, ANSW15 },
    { ANSW16, ANSW17 }, { ANSW18, ANSW19 }
  };

  for (count = 0; count < 10; count++)
    {
      RUN_TEST (a, b, c, testseta, testsetb, answset, count, 128, 64, 2);
    }

  return 0;
}

/* { dg-final { scan-assembler-times "fdiv\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d, v\[0-9\]+\.2d" 1 } } */

int
main (int argc, char **argv)
{
  if (test_vdiv_f32 ())
    abort ();

  if (test_vdiv_f64 ())
    abort ();

  if (test_vdivq_f32 ())
    abort ();

  if (test_vdivq_f64 ())
    abort ();

  return 0;
}

/* { dg-final { cleanup-saved-temps } } */
