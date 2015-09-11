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

#define CONCAT(a, b) a##b
#define CONCAT1(a, b) CONCAT (a, b)
#define REG_INFEX64 _
#define REG_INFEX128 q_
#define REG_INFEX(reg_len) REG_INFEX##reg_len
#define POSTFIX(reg_len, data_len) \
  CONCAT1 (REG_INFEX (reg_len), s##data_len)
#define DATA_TYPE_32 float
#define DATA_TYPE_64 double
#define DATA_TYPE(data_len) DATA_TYPE_##data_len

#define FORCE_SIMD_INST64_8(data)
#define FORCE_SIMD_INST64_16(data)
#define FORCE_SIMD_INST64_32(data)
#define FORCE_SIMD_INST64_64(data) force_simd (data)
#define FORCE_SIMD_INST128_8(data)
#define FORCE_SIMD_INST128_16(data)
#define FORCE_SIMD_INST128_32(data)
#define FORCE_SIMD_INST128_64(data)

#define FORCE_SIMD_INST(reg_len, data_len, data) \
  CONCAT1 (FORCE_SIMD_INST, reg_len##_##data_len) (data)
#define LOAD_INST(reg_len, data_len) \
  CONCAT1 (vld1, POSTFIX (reg_len, data_len))
#define NEG_INST(reg_len, data_len) \
  CONCAT1 (vneg, POSTFIX (reg_len, data_len))

#define RUN_TEST(test_set, answ_set, reg_len, data_len, n, a, b)	\
  {									\
    int i;								\
    INHIB_OPTIMIZATION;							\
    (a) = LOAD_INST (reg_len, data_len) (test_set);			\
    (b) = LOAD_INST (reg_len, data_len) (answ_set);			\
    FORCE_SIMD_INST (reg_len, data_len, a)				\
    a = NEG_INST (reg_len, data_len) (a);				\
    FORCE_SIMD_INST (reg_len, data_len, a)				\
    for (i = 0; i < n; i++)						\
      {									\
        INHIB_OPTIMIZATION;						\
	if (a[i] != b[i])						\
	  return 1;							\
      }									\
  }

int
test_vneg_s8 ()
{
  int8x8_t a;
  int8x8_t b;

  int8_t test_set0[8] = {
    TEST0, TEST1, TEST2, TEST3, TEST4, TEST5, SCHAR_MAX, SCHAR_MIN
  };
  int8_t answ_set0[8] = {
    ANSW0, ANSW1, ANSW2, ANSW3, ANSW4, ANSW5, SCHAR_MIN + 1, SCHAR_MIN
  };

  RUN_TEST (test_set0, answ_set0, 64, 8, 8, a, b);

  return 0;
}

/* { dg-final { scan-assembler-times "neg\\tv\[0-9\]+\.8b, v\[0-9\]+\.8b" 1 } } */

int
test_vneg_s16 ()
{
  int16x4_t a;
  int16x4_t b;

  int16_t test_set0[4] = { TEST0, TEST1, TEST2, TEST3 };
  int16_t test_set1[4] = { TEST4, TEST5, SHRT_MAX, SHRT_MIN };

  int16_t answ_set0[4] = { ANSW0, ANSW1, ANSW2, ANSW3 };
  int16_t answ_set1[4] = { ANSW4, ANSW5, SHRT_MIN + 1, SHRT_MIN };

  RUN_TEST (test_set0, answ_set0, 64, 16, 4, a, b);
  RUN_TEST (test_set1, answ_set1, 64, 16, 4, a, b);

  return 0;
}

/* { dg-final { scan-assembler-times "neg\\tv\[0-9\]+\.4h, v\[0-9\]+\.4h" 2 } } */

int
test_vneg_s32 ()
{
  int32x2_t a;
  int32x2_t b;

  int32_t test_set0[2] = { TEST0, TEST1 };
  int32_t test_set1[2] = { TEST2, TEST3 };
  int32_t test_set2[2] = { TEST4, TEST5 };
  int32_t test_set3[2] = { INT_MAX, INT_MIN };

  int32_t answ_set0[2] = { ANSW0, ANSW1 };
  int32_t answ_set1[2] = { ANSW2, ANSW3 };
  int32_t answ_set2[2] = { ANSW4, ANSW5 };
  int32_t answ_set3[2] = { INT_MIN + 1, INT_MIN };

  RUN_TEST (test_set0, answ_set0, 64, 32, 2, a, b);
  RUN_TEST (test_set1, answ_set1, 64, 32, 2, a, b);
  RUN_TEST (test_set2, answ_set2, 64, 32, 2, a, b);
  RUN_TEST (test_set3, answ_set3, 64, 32, 2, a, b);

  return 0;
}

/* { dg-final { scan-assembler-times "neg\\tv\[0-9\]+\.2s, v\[0-9\]+\.2s" 4 } } */

int
test_vneg_s64 ()
{
  int64x1_t a;
  int64x1_t b;

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

  RUN_TEST (test_set0, answ_set0, 64, 64, 1, a, b);
  RUN_TEST (test_set1, answ_set1, 64, 64, 1, a, b);
  RUN_TEST (test_set2, answ_set2, 64, 64, 1, a, b);
  RUN_TEST (test_set3, answ_set3, 64, 64, 1, a, b);
  RUN_TEST (test_set4, answ_set4, 64, 64, 1, a, b);
  RUN_TEST (test_set5, answ_set5, 64, 64, 1, a, b);
  RUN_TEST (test_set6, answ_set6, 64, 64, 1, a, b);
  RUN_TEST (test_set7, answ_set7, 64, 64, 1, a, b);

  return 0;
}

/* { dg-final { scan-assembler-times "neg\\td\[0-9\]+, d\[0-9\]+" 8 } } */

int
test_vnegq_s8 ()
{
  int8x16_t a;
  int8x16_t b;

  int8_t test_set0[16] = {
    TEST0, TEST1, TEST2, TEST3, TEST4, TEST5, SCHAR_MAX, SCHAR_MIN,
    4, 8, 15, 16, 23, 42, -1, -2
  };

  int8_t answ_set0[16] = {
    ANSW0, ANSW1, ANSW2, ANSW3, ANSW4, ANSW5, SCHAR_MIN + 1, SCHAR_MIN,
    -4, -8, -15, -16, -23, -42, 1, 2
  };

  RUN_TEST (test_set0, answ_set0, 128, 8, 8, a, b);

  return 0;
}

/* { dg-final { scan-assembler-times "neg\\tv\[0-9\]+\.16b, v\[0-9\]+\.16b" 1 } } */

int
test_vnegq_s16 ()
{
  int16x8_t a;
  int16x8_t b;

  int16_t test_set0[8] = {
    TEST0, TEST1, TEST2, TEST3, TEST4, TEST5, SHRT_MAX, SHRT_MIN
  };
  int16_t answ_set0[8] = {
    ANSW0, ANSW1, ANSW2, ANSW3, ANSW4, ANSW5, SHRT_MIN + 1, SHRT_MIN
  };

  RUN_TEST (test_set0, answ_set0, 128, 16, 8, a, b);

  return 0;
}

/* { dg-final { scan-assembler-times "neg\\tv\[0-9\]+\.8h, v\[0-9\]+\.8h" 1 } } */

int
test_vnegq_s32 ()
{
  int32x4_t a;
  int32x4_t b;

  int32_t test_set0[4] = { TEST0, TEST1, TEST2, TEST3 };
  int32_t test_set1[4] = { TEST4, TEST5, INT_MAX, INT_MIN };

  int32_t answ_set0[4] = { ANSW0, ANSW1, ANSW2, ANSW3 };
  int32_t answ_set1[4] = { ANSW4, ANSW5, INT_MIN + 1, INT_MIN };

  RUN_TEST (test_set0, answ_set0, 128, 32, 4, a, b);
  RUN_TEST (test_set1, answ_set1, 128, 32, 4, a, b);

  return 0;
}

/* { dg-final { scan-assembler-times "neg\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s" 2 } } */

int
test_vnegq_s64 ()
{
  int64x2_t a;
  int64x2_t b;

  int64_t test_set0[2] = { TEST0, TEST1 };
  int64_t test_set1[2] = { TEST2, TEST3 };
  int64_t test_set2[2] = { TEST4, TEST5 };
  int64_t test_set3[2] = { LLONG_MAX, LLONG_MIN };

  int64_t answ_set0[2] = { ANSW0, ANSW1 };
  int64_t answ_set1[2] = { ANSW2, ANSW3 };
  int64_t answ_set2[2] = { ANSW4, ANSW5 };
  int64_t answ_set3[2] = { LLONG_MIN + 1, LLONG_MIN };

  RUN_TEST (test_set0, answ_set0, 128, 64, 2, a, b);
  RUN_TEST (test_set1, answ_set1, 128, 64, 2, a, b);
  RUN_TEST (test_set2, answ_set2, 128, 64, 2, a, b);
  RUN_TEST (test_set3, answ_set3, 128, 64, 2, a, b);

  return 0;
}

/* { dg-final { scan-assembler-times "neg\\tv\[0-9\]+\.2d, v\[0-9\]+\.2d" 4 } } */

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

