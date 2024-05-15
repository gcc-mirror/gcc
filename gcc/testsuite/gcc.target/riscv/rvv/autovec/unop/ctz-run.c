/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -mrvv-vector-bits=zvl -ffast-math" } */

#include "ctz-template.h"

#include <assert.h>
#include <stdio.h>

#define SZ 126

#define RUN(TYPE)                                                              \
  TYPE dst##TYPE[SZ];                                                          \
  TYPE a##TYPE[SZ];                                                            \
  for (int i = 0; i < SZ; i++)                                                 \
    {                                                                          \
      dst##TYPE[i] = 0;                                                        \
      a##TYPE[i] = i + 1;                                                      \
    }                                                                          \
  vctz_##TYPE (dst##TYPE, a##TYPE, SZ);                                        \
  for (int i = 0; i < SZ; i++)                                                 \
    assert (dst##TYPE[i] == __builtin_ctz (a##TYPE[i]));\

#define RUN_ALL()                                                              \
  RUN (uint8_t)                                                                \
  RUN (int8_t)                                                                 \
  RUN (int16_t)                                                                \
  RUN (uint16_t)                                                               \
  RUN (int32_t)                                                                \
  RUN (uint32_t)                                                               \
  //RUN (int64_t)                                                                \
  //RUN (uint64_t)

int main ()
{
  RUN_ALL()
}
