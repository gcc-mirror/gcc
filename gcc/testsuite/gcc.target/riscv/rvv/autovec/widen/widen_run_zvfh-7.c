/* { dg-do run { target { riscv_v && riscv_zvfh } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -ffast-math" } */

#include <assert.h>
#include "widen-7.c"

#define SZ 512

#define RUN(TYPE1, TYPE2, LIMIT)                                               \
  TYPE2 a##TYPE2[SZ];                                                          \
  TYPE1 b##TYPE1[SZ];                                                          \
  TYPE1 dst##TYPE1[SZ];                                                        \
  for (int i = 0; i < SZ; i++)                                                 \
    {                                                                          \
      a##TYPE2[i] = LIMIT + i % LIMIT;                                         \
      b##TYPE1[i] = LIMIT + i & LIMIT;                                         \
    }                                                                          \
  vwmul_##TYPE1_##TYPE2 (dst##TYPE1, a##TYPE2, b##TYPE1, SZ);                  \
  for (int i = 0; i < SZ; i++)                                                 \
    assert (dst##TYPE1[i] == (((TYPE1) a##TYPE2[i]) * b##TYPE1[i]));

#define RUN_ALL() RUN (float, _Float16, -32768)

int
main ()
{
  RUN_ALL ()
}
