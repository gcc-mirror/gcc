/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv64gcv -mabi=lp64d -O3 -mrvv-max-lmul=m2 -fno-vect-cost-model" } */

#include <stdint-gcc.h>

#define DEF_LOOP(OLD_TYPE, NEW_TYPE)                                           \
  void __attribute__ ((noipa))                                                 \
  test_##OLD_TYPE##_2_##NEW_TYPE (NEW_TYPE *__restrict r,                      \
				  OLD_TYPE *__restrict a, NEW_TYPE b,          \
				  OLD_TYPE *__restrict pred, int n)            \
  {                                                                            \
    for (int i = 0; i < n; ++i)                                                \
      {                                                                        \
	r[i] = pred[i] ? (NEW_TYPE) a[i] : b;                                  \
      }                                                                        \
  }

/* INT -> narrower-INT */
#define TEST_ALL_X2X_NARROWER(T)                                               \
  T (int16_t, int8_t)

TEST_ALL_X2X_NARROWER (DEF_LOOP)

/* { dg-final { scan-assembler-not {\tvsetvli\t[a-x0-9]+,[a-x0-9]+,e[0-9]+,m[f0-9]+,t[au],m[au]\n\tvsetvli\t} } } */
