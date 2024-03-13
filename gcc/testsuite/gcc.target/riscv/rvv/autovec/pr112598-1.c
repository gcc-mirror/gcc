/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_zvfh_zfh_zvl512b -mabi=ilp32d -O3 -mrvv-max-lmul=m8 -O3 -fno-vect-cost-model -ffast-math" } */

#include <stdint-gcc.h>
#define TEST_UNARY_CALL_CVT(TYPE_IN, TYPE_OUT, CALL) \
  void test_##TYPE_IN##_##TYPE_OUT##_##CALL (        \
    TYPE_OUT *out, TYPE_IN *in, unsigned count)      \
  {                                                  \
    for (unsigned i = 0; i < count; i++)             \
      out[i] = CALL (in[i]);                         \
  }
#define TEST_ASSERT(TYPE)                                         \
  void test_##TYPE##_assert (TYPE *out, TYPE *ref, unsigned size) \
  {                                                               \
    for (unsigned i = 0; i < size; i++)                           \
      {                                                           \
	if (out[i] != ref[i])                                     \
	  __builtin_abort ();                                     \
      }                                                           \
  }
#define TEST_INIT_CVT(TYPE_IN, VAL_IN, TYPE_REF, VAL_REF, NUM) \
  void test_##TYPE_IN##_##TYPE_REF##_init_##NUM (              \
    TYPE_IN *in, TYPE_REF *ref, unsigned size)                 \
  {                                                            \
    for (unsigned i = 0; i < size; i++)                        \
      {                                                        \
	in[i] = VAL_IN;                                        \
	ref[i] = VAL_REF;                                      \
      }                                                        \
  }
#define RUN_TEST_CVT(TYPE_IN, TYPE_OUT, NUM, CALL, IN, OUT, REF, SIZE) \
  test_##TYPE_IN##_##TYPE_OUT##_init_##NUM (IN, REF, SIZE);            \
  test_##TYPE_IN##_##TYPE_OUT##_##CALL (OUT, IN, SIZE);                \
  test_##TYPE_OUT##_assert (OUT, REF, SIZE);

#define ARRAY_SIZE 128

float in[ARRAY_SIZE];
int64_t out[ARRAY_SIZE];
int64_t ref[ARRAY_SIZE];

TEST_UNARY_CALL_CVT (float, int64_t, __builtin_llceilf)

TEST_ASSERT (int64_t)


TEST_INIT_CVT (float, 9223372036854775808.0, int64_t, 0x7fffffffffffffff, 26)
TEST_INIT_CVT (float, __builtin_inf (), int64_t, __builtin_llceilf (__builtin_inf ()), 29)

int64_t
main ()
{
  RUN_TEST_CVT (float, int64_t, 26, __builtin_llceilf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 29, __builtin_llceilf, in, out, ref, ARRAY_SIZE);
  return 0;
}
