/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -mrvv-vector-bits=zvl -ffast-math" } */

#include "cond_copysign-template.h"

#include <assert.h>

#define SZ 512

#define EPS 1e-6

#define INIT_PRED()                                                            \
  int pred[SZ];                                                                \
  for (int i = 0; i < SZ; i++)                                                 \
    {                                                                          \
      pred[i] = i % 3;                                                         \
    }

#define RUN(TYPE, VAL)                                                         \
  TYPE a##TYPE[SZ];                                                            \
  TYPE b##TYPE[SZ];                                                            \
  for (int i = 0; i < SZ; i++)                                                 \
    {                                                                          \
      a##TYPE[i] = i;                                                          \
      b##TYPE[i] = (i & 1) ? VAL : -VAL;                                       \
    }                                                                          \
  copysign_##TYPE (a##TYPE, a##TYPE, b##TYPE, pred, SZ);                       \
  for (int i = 0; i < SZ; i++)                                                 \
    assert (!pred[i] || __builtin_fabs (a##TYPE[i] - ((i & 1) ? i : -i)) < EPS);

#define RUN2(TYPE, VAL)                                                        \
  TYPE a2##TYPE[SZ];                                                           \
  for (int i = 0; i < SZ; i++)                                                 \
    a2##TYPE[i] = i;                                                           \
  copysigns_##TYPE (a2##TYPE, a2##TYPE, -VAL, pred, SZ);                       \
  for (int i = 0; i < SZ; i++)                                                 \
    assert (!pred[i] || __builtin_fabs (a2##TYPE[i] + i) < EPS);

#define RUN3(TYPE, VAL)                                                        \
  TYPE a3##TYPE[SZ];                                                           \
  TYPE b3##TYPE[SZ];                                                           \
  for (int i = 0; i < SZ; i++)                                                 \
    {                                                                          \
      a3##TYPE[i] = (i & 1) ? -i : i;                                          \
      b3##TYPE[i] = (i & 1) ? VAL : -VAL;                                      \
    }                                                                          \
  xorsign_##TYPE (a3##TYPE, a3##TYPE, b3##TYPE, pred, SZ);                     \
  for (int i = 0; i < SZ; i++)                                                 \
    assert (!pred[i] || __builtin_fabs (a3##TYPE[i] + i) < EPS);

#define RUN4(TYPE, VAL)                                                        \
  TYPE a4##TYPE[SZ];                                                           \
  for (int i = 0; i < SZ; i++)                                                 \
    a4##TYPE[i] = -i;                                                          \
  xorsigns_##TYPE (a4##TYPE, a4##TYPE, -VAL, pred, SZ);                        \
  for (int i = 0; i < SZ; i++)                                                 \
    assert (!pred[i] || __builtin_fabs (a4##TYPE[i] - i) < EPS);

#define RUN5(TYPE, VAL)                                                        \
  TYPE a5##TYPE[SZ];                                                           \
  TYPE b5##TYPE[SZ];                                                           \
  for (int i = 0; i < SZ; i++)                                                 \
    {                                                                          \
      a5##TYPE[i] = i;                                                         \
      b5##TYPE[i] = (i & 1) ? VAL : -VAL;                                      \
    }                                                                          \
  ncopysign_##TYPE (a5##TYPE, a5##TYPE, b##TYPE, pred, SZ);                    \
  for (int i = 0; i < SZ; i++)                                                 \
    assert (!pred[i]                                                           \
	    || __builtin_fabs (-a5##TYPE[i] - ((i & 1) ? i : -i)) < EPS);

#define RUN6(TYPE, VAL)                                                        \
  TYPE a6##TYPE[SZ];                                                           \
  for (int i = 0; i < SZ; i++)                                                 \
    a6##TYPE[i] = i;                                                           \
  ncopysigns_##TYPE (a6##TYPE, a6##TYPE, -VAL, pred, SZ);                      \
  for (int i = 0; i < SZ; i++)                                                 \
    assert (!pred[i] || __builtin_fabs (-a6##TYPE[i] + i) < EPS);

#define RUN_ALL()                                                              \
  RUN (float, 5)                                                               \
  RUN (double, 6)                                                              \
  RUN2 (float, 11)                                                             \
  RUN2 (double, 12)                                                            \
  RUN3 (float, 16)                                                             \
  RUN3 (double, 18)                                                            \
  RUN4 (float, 17)                                                             \
  RUN4 (double, 19)                                                            \
  RUN5 (float, 123)                                                            \
  RUN5 (double, 523)                                                           \
  RUN6 (float, 777)                                                            \
  RUN6 (double, 877)

int
main ()
{
  INIT_PRED ()
  RUN_ALL ()
}
