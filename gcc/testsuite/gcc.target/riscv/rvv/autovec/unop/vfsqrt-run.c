/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -mrvv-vector-bits=zvl -ffast-math" } */

#include "vfsqrt-template.h"

#define SZ 255

#define EPS 1e-5

#define RUN(TYPE, SUFFIX)                                                      \
  TYPE a##TYPE[SZ];                                                            \
  for (int i = 0; i < SZ; i++)                                                 \
    a##TYPE[i] = (TYPE) i;                                                     \
  vsqrt_##TYPE (a##TYPE, a##TYPE, SZ);                                         \
  for (int i = 0; i < SZ; i++)                                                 \
    if (__builtin_fabs##SUFFIX (a##TYPE[i]                                     \
				- __builtin_sqrt##SUFFIX ((TYPE) i))           \
	> EPS)                                                                 \
      __builtin_abort ();

#define RUN_ALL()                                                              \
  RUN (float, f)                                                               \
  RUN (double, )

int main ()
{
  RUN_ALL ()
}
