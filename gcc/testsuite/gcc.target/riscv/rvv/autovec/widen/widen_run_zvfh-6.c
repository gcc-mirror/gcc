/* { dg-do run { target { riscv_v && riscv_zvfh } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -ffast-math" } */

#include <assert.h>
#include "widen-6.c"

#define SZ 512

#define RUN(TYPE1, TYPE2, LIMIT)                                               \
  TYPE1 a##TYPE1[SZ];                                                          \
  TYPE2 b##TYPE2[SZ];                                                          \
  TYPE1 dst##TYPE1[SZ];                                                        \
  for (int i = 0; i < SZ; i++)                                                 \
    {                                                                          \
      a##TYPE1[i] = LIMIT + i % 8723;                                          \
      b##TYPE2[i] = LIMIT + i & 1964;                                          \
    }                                                                          \
  vwsub_##TYPE1_##TYPE2 (dst##TYPE1, a##TYPE1, b##TYPE2, SZ);                  \
  for (int i = 0; i < SZ; i++)                                                 \
    assert (dst##TYPE1[i] == ((TYPE1) a##TYPE1[i] - (TYPE1) b##TYPE2[i]));

#define RUN_ALL() RUN (float, _Float16, -32768)

int
main ()
{
  RUN_ALL ()
}
