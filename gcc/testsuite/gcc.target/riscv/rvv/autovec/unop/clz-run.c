/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -mrvv-vector-bits=zvl -ffast-math" } */

#include "clz-template.h"

#include <assert.h>
#include <stdio.h>

#define SZ 128

#define RUN(TYPE)                                                              \
  TYPE dst##TYPE[SZ];                                                          \
  TYPE a##TYPE[SZ];                                                            \
  for (int i = 0; i < SZ; i++)                                                 \
    {                                                                          \
      dst##TYPE[i] = 0;                                                        \
      a##TYPE[i] = i;                                                          \
    }                                                                          \
  vclz_##TYPE (dst##TYPE, a##TYPE, SZ);                                        \
  for (int i = 0; i < SZ; i++)                                                 \
    assert (dst##TYPE[i] == __builtin_clz (a##TYPE[i]));

#define RUN_ALL()                                                              \
  RUN (int8_t)                                                                 \
  RUN (uint8_t)                                                                \
  RUN (int16_t)                                                                \
  RUN (uint16_t)                                                               \
  RUN (int32_t)                                                                \
  RUN (uint32_t)                                                               \
  RUN (int64_t)                                                                \
  RUN (uint64_t)

int main ()
{
  RUN_ALL()
}
