/* Test vneg works correctly.  */
/* { dg-do run } */
/* { dg-options "-std=gnu99 -O3 -Wno-div-by-zero --save-temps" } */

#include <arm_neon.h>
#include <limits.h>

/* Used to force a variable to a SIMD register.  Also acts as a stronger
   inhibitor of optimization than the below - necessary for int64x1_t
   because more of the implementation is in terms of gcc vector extensions
   (which support constant propagation) than for other types.  */
#define force_simd(V1)   asm volatile ("mov %d0, %1.d[0]"	\
	   : "=w"(V1)						\
	   : "w"(V1)						\
	   : /* No clobbers */);
#define INHIB_OPTIMIZATION asm volatile ("" : : : "memory")

#define TEST0 0
#define TEST1 1
#define TEST2 -1
#define TEST3 10
#define TEST4 -10
#define TEST5 0

#define ANSW0 0
#define ANSW1 -1
#define ANSW2 1
#define ANSW3 -10
#define ANSW4 10
#define ANSW5 0

extern void abort (void);

#define BUILD_TEST(type, size, lanes)			   \
int __attribute__((noipa,noinline))			   \
run_test##type##size##x##lanes (int##size##_t* test_set,   \
		       int##size##_t* answ_set,		   \
		       int reg_len, int data_len, int n)   \
{							   \
  int i;						   \
  int##size##x##lanes##_t a = vld1##type##size (test_set); \
  int##size##x##lanes##_t b = vld1##type##size (answ_set); \
  a = vneg##type##size (a);				   \
  for (i = 0; i < n; i++)				   \
  {				    \
    INHIB_OPTIMIZATION;		    \
    if (a[i] != b[i])		    \
    return 1;			    \
  }				    \
  return 0;			    \
}				    \

#define RUN_TEST_SCALAR(test_val, answ_val, a, b)     \
  {                                                   \
    int64_t res;                                      \
    INHIB_OPTIMIZATION;                               \
    a = test_val;                                     \
    b = answ_val;                                     \
    force_simd (b);                                   \
    force_simd (a);                                   \
    res = vnegd_s64 (a);                              \
    force_simd (res);                                 \
  }

BUILD_TEST (_s, 8, 8)
BUILD_TEST (_s, 16, 4)
BUILD_TEST (_s, 32, 2)
BUILD_TEST (_s, 64, 1)

BUILD_TEST (q_s, 8, 16)
BUILD_TEST (q_s, 16, 8)
BUILD_TEST (q_s, 32, 4)
BUILD_TEST (q_s, 64, 2)

int __attribute__ ((noinline))
test_vneg_s8 ()
{
  int8_t test_set0[8] = {
    TEST0, TEST1, TEST2, TEST3, TEST4, TEST5, SCHAR_MAX, SCHAR_MIN
  };
  int8_t answ_set0[8] = {
    ANSW0, ANSW1, ANSW2, ANSW3, ANSW4, ANSW5, SCHAR_MIN + 1, SCHAR_MIN
  };

  int o1 = run_test_s8x8 (test_set0, answ_set0, 64, 8, 8);

  return o1;
}

/* { dg-final { scan-assembler-times "neg\\tv\[0-9\]+\.8b, v\[0-9\]+\.8b" 1 } } */

int __attribute__ ((noinline))
test_vneg_s16 ()
{
  int16_t test_set0[4] = { TEST0, TEST1, TEST2, TEST3 };
  int16_t test_set1[4] = { TEST4, TEST5, SHRT_MAX, SHRT_MIN };

  int16_t answ_set0[4] = { ANSW0, ANSW1, ANSW2, ANSW3 };
  int16_t answ_set1[4] = { ANSW4, ANSW5, SHRT_MIN + 1, SHRT_MIN };

  int o1 = run_test_s16x4 (test_set0, answ_set0, 64, 16, 4);
  int o2 = run_test_s16x4 (test_set1, answ_set1, 64, 16, 4);

  return o1||o2;
}

/* { dg-final { scan-assembler-times "neg\\tv\[0-9\]+\.4h, v\[0-9\]+\.4h" 1 } } */

int __attribute__ ((noinline))
test_vneg_s32 ()
{
  int32_t test_set0[2] = { TEST0, TEST1 };
  int32_t test_set1[2] = { TEST2, TEST3 };
  int32_t test_set2[2] = { TEST4, TEST5 };
  int32_t test_set3[2] = { INT_MAX, INT_MIN };

  int32_t answ_set0[2] = { ANSW0, ANSW1 };
  int32_t answ_set1[2] = { ANSW2, ANSW3 };
  int32_t answ_set2[2] = { ANSW4, ANSW5 };
  int32_t answ_set3[2] = { INT_MIN + 1, INT_MIN };

  int o1 = run_test_s32x2 (test_set0, answ_set0, 64, 32, 2);
  int o2 = run_test_s32x2 (test_set1, answ_set1, 64, 32, 2);
  int o3 = run_test_s32x2 (test_set2, answ_set2, 64, 32, 2);
  int o4 = run_test_s32x2 (test_set3, answ_set3, 64, 32, 2);

  return o1||o2||o3||o4;
}

/* { dg-final { scan-assembler-times "neg\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s" 1 } } */

int __attribute__ ((noinline))
test_vneg_s64 ()
{
  int64_t test_set0[1] = { TEST0 };
  int64_t test_set1[1] = { TEST1 };
  int64_t test_set2[1] = { TEST2 };
  int64_t test_set3[1] = { TEST3 };
  int64_t test_set4[1] = { TEST4 };
  int64_t test_set5[1] = { TEST5 };
  int64_t test_set6[1] = { LLONG_MAX };
  int64_t test_set7[1] = { LLONG_MIN };

  int64_t answ_set0[1] = { ANSW0 };
  int64_t answ_set1[1] = { ANSW1 };
  int64_t answ_set2[1] = { ANSW2 };
  int64_t answ_set3[1] = { ANSW3 };
  int64_t answ_set4[1] = { ANSW4 };
  int64_t answ_set5[1] = { ANSW5 };
  int64_t answ_set6[1] = { LLONG_MIN + 1 };
  int64_t answ_set7[1] = { LLONG_MIN };

  int o1 = run_test_s64x1 (test_set0, answ_set0, 64, 64, 1);
  int o2 = run_test_s64x1  (test_set1, answ_set1, 64, 64, 1);
  int o3 = run_test_s64x1 (test_set2, answ_set2, 64, 64, 1);
  int o4 = run_test_s64x1 (test_set3, answ_set3, 64, 64, 1);
  int o5 = run_test_s64x1 (test_set4, answ_set4, 64, 64, 1);
  int o6 = run_test_s64x1 (test_set5, answ_set5, 64, 64, 1);
  int o7 = run_test_s64x1 (test_set6, answ_set6, 64, 64, 1);
  int o8 = run_test_s64x1 (test_set7, answ_set7, 64, 64, 1);

  return o1||o2||o3||o4||o5||o6||o7||o8;
}

int __attribute__ ((noinline))
test_vnegd_s64 ()
{
  int64_t a, b;

  RUN_TEST_SCALAR (TEST0, ANSW0, a, b);
  RUN_TEST_SCALAR (TEST1, ANSW1, a, b);
  RUN_TEST_SCALAR (TEST2, ANSW2, a, b);
  RUN_TEST_SCALAR (TEST3, ANSW3, a, b);
  RUN_TEST_SCALAR (TEST4, ANSW4, a, b);
  RUN_TEST_SCALAR (TEST5, ANSW5, a, b);
  RUN_TEST_SCALAR (LLONG_MAX, LLONG_MIN + 1, a, b);
  RUN_TEST_SCALAR (LLONG_MIN, LLONG_MIN, a, b);

  return 0;
}

/* { dg-final { scan-assembler-times "neg\\td\[0-9\]+, d\[0-9\]+" 8 } } */

int __attribute__ ((noinline))
test_vnegq_s8 ()
{
  int8_t test_set0[16] = {
    TEST0, TEST1, TEST2, TEST3, TEST4, TEST5, SCHAR_MAX, SCHAR_MIN,
    4, 8, 15, 16, 23, 42, -1, -2
  };

  int8_t answ_set0[16] = {
    ANSW0, ANSW1, ANSW2, ANSW3, ANSW4, ANSW5, SCHAR_MIN + 1, SCHAR_MIN,
    -4, -8, -15, -16, -23, -42, 1, 2
  };

  int o1 = run_testq_s8x16 (test_set0, answ_set0, 128, 8, 8);

  return o1;
}

/* { dg-final { scan-assembler-times "neg\\tv\[0-9\]+\.16b, v\[0-9\]+\.16b" 1 } } */

int __attribute__ ((noinline))
test_vnegq_s16 ()
{
  int16_t test_set0[8] = {
    TEST0, TEST1, TEST2, TEST3, TEST4, TEST5, SHRT_MAX, SHRT_MIN
  };
  int16_t answ_set0[8] = {
    ANSW0, ANSW1, ANSW2, ANSW3, ANSW4, ANSW5, SHRT_MIN + 1, SHRT_MIN
  };

  int o1 = run_testq_s16x8 (test_set0, answ_set0, 128, 16, 8);

  return o1;
}

/* { dg-final { scan-assembler-times "neg\\tv\[0-9\]+\.8h, v\[0-9\]+\.8h" 1 } } */

int __attribute__ ((noinline))
test_vnegq_s32 ()
{
  int32_t test_set0[4] = { TEST0, TEST1, TEST2, TEST3 };
  int32_t test_set1[4] = { TEST4, TEST5, INT_MAX, INT_MIN };

  int32_t answ_set0[4] = { ANSW0, ANSW1, ANSW2, ANSW3 };
  int32_t answ_set1[4] = { ANSW4, ANSW5, INT_MIN + 1, INT_MIN };

  int o1 = run_testq_s32x4 (test_set0, answ_set0, 128, 32, 4);
  int o2 = run_testq_s32x4 (test_set1, answ_set1, 128, 32, 4);

  return o1||o2;
}

/* { dg-final { scan-assembler-times "neg\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s" 1 } } */

int __attribute__ ((noinline))
test_vnegq_s64 ()
{
  int64_t test_set0[2] = { TEST0, TEST1 };
  int64_t test_set1[2] = { TEST2, TEST3 };
  int64_t test_set2[2] = { TEST4, TEST5 };
  int64_t test_set3[2] = { LLONG_MAX, LLONG_MIN };

  int64_t answ_set0[2] = { ANSW0, ANSW1 };
  int64_t answ_set1[2] = { ANSW2, ANSW3 };
  int64_t answ_set2[2] = { ANSW4, ANSW5 };
  int64_t answ_set3[2] = { LLONG_MIN + 1, LLONG_MIN };

  int o1 = run_testq_s64x2 (test_set0, answ_set0, 128, 64, 2);
  int o2 = run_testq_s64x2 (test_set1, answ_set1, 128, 64, 2);
  int o3 = run_testq_s64x2 (test_set2, answ_set2, 128, 64, 2);
  int o4 = run_testq_s64x2 (test_set3, answ_set3, 128, 64, 2);

  return o1||o2||o2||o4;
}

/* { dg-final { scan-assembler-times "neg\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d" 1 } } */

int
main (int argc, char **argv)
{
  if (test_vneg_s8 ())
    abort ();

  if (test_vneg_s16 ())
    abort ();

  if (test_vneg_s32 ())
    abort ();

  if (test_vneg_s64 ())
    abort ();

  if (test_vnegd_s64 ())
    abort ();

  if (test_vnegq_s8 ())
    abort ();

  if (test_vnegq_s16 ())
    abort ();

  if (test_vnegq_s32 ())
    abort ();

  if (test_vnegq_s64 ())
    abort ();

  return 0;
}

