/* { dg-do run } */
/* { dg-require-effective-target "riscv_zvbb_ok" } */
/* { dg-add-options "riscv_v" } */
/* { dg-add-options "riscv_zvbb" } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model" } */

#include "vwsll-template.h"

#include <assert.h>

#define SZ 512

#define RUN(TYPE1, TYPE2, VAL)                                                 \
  TYPE1 dst##TYPE1[SZ];                                                        \
  TYPE2 a##TYPE2[SZ];                                                          \
  TYPE2 b##TYPE2[SZ];                                                          \
  for (int i = 0; i < SZ; i++)                                                 \
    {                                                                          \
      dst##TYPE1[i] = 0;                                                       \
      a##TYPE2[i] = VAL;                                                       \
      b##TYPE2[i] = i % 4;                                                     \
    }                                                                          \
  vwsll_vv##TYPE1 (dst##TYPE1, a##TYPE2, b##TYPE2, SZ);                        \
  for (int i = 0; i < SZ; i++)                                                 \
    assert (dst##TYPE1[i] == (VAL << (i % 4)));

#define RUN2(TYPE1, TYPE2, VAL)                                                \
  TYPE1 dst2##TYPE1[SZ];                                                       \
  TYPE2 a2##TYPE2[SZ];                                                         \
  for (int i = 0; i < SZ; i++)                                                 \
    {                                                                          \
      dst2##TYPE1[i] = 0;                                                      \
      a2##TYPE2[i] = VAL;                                                      \
    }                                                                          \
  TYPE2 b2##TYPE2 = 7;                                                         \
  vwsll_vx##TYPE1 (dst2##TYPE1, a2##TYPE2, b2##TYPE2, SZ);                     \
  for (int i = 0; i < SZ; i++)                                                 \
    assert (dst2##TYPE1[i] == (VAL << b2##TYPE2));

#define RUN3(TYPE1, TYPE2, VAL)                                                \
  TYPE1 dst3##TYPE1[SZ];                                                       \
  TYPE2 a3##TYPE2[SZ];                                                         \
  for (int i = 0; i < SZ; i++)                                                 \
    {                                                                          \
      dst3##TYPE1[i] = 0;                                                      \
      a3##TYPE2[i] = VAL;                                                      \
    }                                                                          \
  vwsll_vi##TYPE1 (dst3##TYPE1, a3##TYPE2, SZ);                                \
  for (int i = 0; i < SZ; i++)                                                 \
    assert (dst3##TYPE1[i] == (VAL << 6));

#define RUN_ALL()                                                              \
  RUN (uint16_t, uint8_t, 2)                                                   \
  RUN (uint32_t, uint16_t, 2)                                                  \
  RUN (uint64_t, uint32_t, 4)                                                  \
  RUN2 (uint16_t, uint8_t, 8)                                                  \
  RUN2 (uint32_t, uint16_t, 8)                                                 \
  RUN2 (uint64_t, uint32_t, 10)                                                \
  RUN3 (uint16_t, uint8_t, 255)                                                \
  RUN3 (uint32_t, uint16_t, 34853)                                           \
  RUN3 (uint64_t, uint32_t, 1794394)

int
main ()
{
  RUN_ALL ()
}
