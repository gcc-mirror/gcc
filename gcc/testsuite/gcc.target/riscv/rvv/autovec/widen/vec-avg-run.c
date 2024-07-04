/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -mrvv-vector-bits=scalable -lm" } */

#include <limits.h>
#include <math.h>
#include <assert.h>

#include "vec-avg-template.h"

#define SZ 256

#define RUNS1(TYPE, SCALE)                                                     \
  TYPE a##TYPE[SZ + 1];                                                        \
  TYPE b##TYPE[SZ + 1];                                                        \
  TYPE dst##TYPE[SZ + 1];                                                      \
  for (int cnt = 0, i = -(SZ * SCALE) / 2; i < (SZ * SCALE) / 2; i += SCALE)   \
    {                                                                          \
      a##TYPE[cnt] = i;                                                        \
      b##TYPE[cnt] = i + 1;                                                    \
      dst##TYPE[cnt++] = 0;                                                    \
    }                                                                          \
  vavg_##TYPE (dst##TYPE, a##TYPE, b##TYPE, SZ);                               \
  for (int i = 0; i < SZ; i += SCALE)                                          \
    assert (dst##TYPE[i] == floor ((a##TYPE[i] + b##TYPE[i]) / 2.0));

#define RUNU1(TYPE, SCALE)                                                     \
  TYPE a##TYPE[SZ + 1];                                                        \
  TYPE b##TYPE[SZ + 1];                                                        \
  TYPE dst##TYPE[SZ + 1];                                                      \
  for (int cnt = 0, i = 0; i < (SZ * SCALE); i += SCALE)                       \
    {                                                                          \
      a##TYPE[cnt] = i;                                                        \
      b##TYPE[cnt] = i + 1;                                                    \
      dst##TYPE[cnt++] = 0;                                                    \
    }                                                                          \
  vavg_##TYPE (dst##TYPE, a##TYPE, b##TYPE, SZ);                               \
  for (int i = 0; i < SZ; i += SCALE)                                          \
    assert (dst##TYPE[i] == floor ((a##TYPE[i] + b##TYPE[i]) / 2.0));

#define RUNS2(TYPE, SCALE)                                                     \
  TYPE a2##TYPE[SZ + 1];                                                       \
  TYPE b2##TYPE[SZ + 1];                                                       \
  TYPE dst2##TYPE[SZ + 1];                                                     \
  for (int cnt = 0, i = -(SZ * SCALE) / 2; i < (SZ * SCALE) / 2; i += SCALE)   \
    {                                                                          \
      a2##TYPE[cnt] = i;                                                       \
      b2##TYPE[cnt] = i + 1;                                                   \
      dst2##TYPE[cnt++] = 0;                                                   \
    }                                                                          \
  vavg2_##TYPE (dst2##TYPE, a2##TYPE, b2##TYPE, SZ);                           \
  for (int i = 0; i < SZ; i += SCALE)                                          \
    assert (dst2##TYPE[i] == ceil ((a2##TYPE[i] + b2##TYPE[i]) / 2.0));

#define RUNU2(TYPE, SCALE)                                                     \
  TYPE a2##TYPE[SZ + 1];                                                       \
  TYPE b2##TYPE[SZ + 1];                                                       \
  TYPE dst2##TYPE[SZ + 1];                                                     \
  for (int cnt = 0, i = 0; i < (SZ * SCALE); i += SCALE)                       \
    {                                                                          \
      a2##TYPE[cnt] = i;                                                       \
      b2##TYPE[cnt] = i + 1;                                                   \
      dst2##TYPE[cnt++] = 0;                                                   \
    }                                                                          \
  vavg2_##TYPE (dst2##TYPE, a2##TYPE, b2##TYPE, SZ);                           \
  for (int i = 0; i < SZ; i += SCALE)                                          \
    assert (dst2##TYPE[i] == ceil ((a2##TYPE[i] + b2##TYPE[i]) / 2.0));

#define RUN_ALL()                                                              \
  RUNS1 (int8_t, 1)                                                            \
  RUNS1 (int16_t, 256)                                                         \
  RUNS1 (int32_t, 65536)                                                       \
  RUNU1 (uint8_t, 1)                                                           \
  RUNU1 (uint16_t, 256)                                                        \
  RUNU1 (uint32_t, 65536)                                                      \
  RUNS2 (int8_t, 1)                                                            \
  RUNS2 (int16_t, 256)                                                         \
  RUNS2 (int32_t, 65536)                                                       \
  RUNU2 (uint8_t, 1)                                                           \
  RUNU2 (uint16_t, 256)                                                        \
  RUNU2 (uint32_t, 65536)\

int main ()
{
  RUN_ALL ()
}
