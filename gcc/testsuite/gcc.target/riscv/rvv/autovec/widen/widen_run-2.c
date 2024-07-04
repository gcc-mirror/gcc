/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -ffast-math" } */

#include <assert.h>
#include "widen-2.c"

#define SZ 512

#define RUN(TYPE1, TYPE2, LIMIT)                                               \
  TYPE2 a##TYPE2[SZ];                                                          \
  TYPE2 b##TYPE2[SZ];                                                          \
  TYPE1 dst##TYPE1[SZ];                                                        \
  for (int i = 0; i < SZ; i++)                                                 \
    {                                                                          \
      a##TYPE2[i] = LIMIT + i % 8723;                                          \
      b##TYPE2[i] = LIMIT + i & 1964;                                          \
    }                                                                          \
  vwsub_##TYPE1_##TYPE2 (dst##TYPE1, a##TYPE2, b##TYPE2, SZ);                  \
  for (int i = 0; i < SZ; i++)                                                 \
    assert (dst##TYPE1[i] == ((TYPE1) a##TYPE2[i] - (TYPE1) b##TYPE2[i]));

#define RUN_ALL()                                                              \
  RUN (int16_t, int8_t, -128)                                                  \
  RUN (uint16_t, uint8_t, 255)                                                 \
  RUN (int32_t, int16_t, -32768)                                               \
  RUN (uint32_t, uint16_t, 65535)                                              \
  RUN (int64_t, int32_t, -2147483648)                                          \
  RUN (uint64_t, uint32_t, 4294967295)                                         \
  RUN (double, float, -2147483648)

int
main ()
{
  RUN_ALL ()
}
