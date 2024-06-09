/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -ffast-math" } */

#include <assert.h>
#include "widen-10.c"

#define SZ 512

#define RUN(TYPE1, TYPE2, LIMIT)                                               \
  TYPE2 a##TYPE2[SZ];                                                          \
  TYPE2 b##TYPE2[SZ];                                                          \
  TYPE1 dst##TYPE1[SZ];                                                        \
  TYPE1 dst2##TYPE1[SZ];                                                       \
  for (int i = 0; i < SZ; i++)                                                 \
    {                                                                          \
      a##TYPE2[i] = LIMIT + i % 8723;                                          \
      b##TYPE2[i] = LIMIT + i & 1964;                                          \
      dst##TYPE1[i] = LIMIT + i & 628;                                         \
      dst2##TYPE1[i] = LIMIT + i & 628;                                        \
    }                                                                          \
  vwmacc_##TYPE1_##TYPE2 (dst##TYPE1, a##TYPE2, b##TYPE2, SZ);                 \
  for (int i = 0; i < SZ; i++)                                                 \
    assert (dst##TYPE1[i]                                                      \
	    == -((TYPE1) a##TYPE2[i] * (TYPE1) b##TYPE2[i]) + dst2##TYPE1[i]);

#define RUN_ALL() RUN (double, float, -2147483648)

int
main ()
{
  RUN_ALL ()
}
