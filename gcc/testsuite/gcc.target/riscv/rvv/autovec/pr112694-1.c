/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zve64d_zvfh_zfh -mabi=ilp32d -mcmodel=medany -fdiagnostics-plain-output -ftree-vectorize -O2 -mrvv-max-lmul=m1 -std=c99 -fno-vect-cost-model -mrvv-vector-bits=zvl -ffast-math" } */

#include <stdint-gcc.h>

#define TEST_TYPE(TYPE) 				\
  __attribute__((noipa))				\
  void vmul_##TYPE (TYPE *dst, TYPE *a, TYPE *b, int n)	\
  {							\
    for (int i = 0; i < n; i++)				\
      dst[i] = a[i] * b[i];				\
  }

#define TEST_ALL()	\
 TEST_TYPE(_Float16)	\

TEST_ALL()

#define SZ 512

#define RUN(TYPE, VAL)                                                         \
  TYPE a##TYPE[SZ];                                                            \
  TYPE b##TYPE[SZ];                                                            \
  for (int i = 0; i < SZ; i++)                                                 \
    {                                                                          \
      a##TYPE[i] = 2;                                                          \
      b##TYPE[i] = VAL;                                                        \
    }                                                                          \
  vmul_##TYPE (a##TYPE, a##TYPE, b##TYPE, SZ);                                 \
  for (int i = 0; i < SZ; i++)                                                 \
    if (a##TYPE[i] != 2 * VAL) __builtin_abort ();

#define RUN_ALL()	\
 RUN(_Float16, 4)	\

int main ()
{
  RUN_ALL()
}
