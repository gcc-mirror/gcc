/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable" } */

#include <assert.h>
#include "widen-9.c"

#define SZ 512

#define RUN(TYPE1, TYPE2, TYPE3, LIMIT)                                        \
  TYPE2 a##TYPE2[SZ];                                                          \
  TYPE3 b##TYPE3[SZ];                                                          \
  TYPE1 dst##TYPE1[SZ];                                                        \
  TYPE1 dst2##TYPE1[SZ];                                                       \
  for (int i = 0; i < SZ; i++)                                                 \
    {                                                                          \
      a##TYPE2[i] = LIMIT + i % 8723;                                          \
      b##TYPE3[i] = LIMIT + i & 1964;                                          \
      dst##TYPE1[i] = LIMIT + i & 728;                                         \
      dst2##TYPE1[i] = LIMIT + i & 728;                                        \
    }                                                                          \
  vwmacc_##TYPE1_##TYPE2 (dst##TYPE1, a##TYPE2, b##TYPE3, SZ);                 \
  for (int i = 0; i < SZ; i++)                                                 \
    assert (dst##TYPE1[i]                                                      \
	    == ((TYPE1) a##TYPE2[i] * (TYPE1) b##TYPE3[i]) + dst2##TYPE1[i]);

#define RUN_ALL()                                                              \
  RUN (int16_t, int8_t, uint8_t, -128)                                         \
  RUN (int32_t, int16_t, uint16_t, -32768)                                     \
  RUN (int64_t, int32_t, uint32_t, -2147483648)

int
main ()
{
  RUN_ALL ()
}
