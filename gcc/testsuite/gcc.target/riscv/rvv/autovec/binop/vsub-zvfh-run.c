/* { dg-do run { target { riscv_v && riscv_zvfh } } } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -mrvv-vector-bits=zvl -ffast-math" } */

#include "vsub-template.h"

#include <assert.h>

#define SZ 512

#define RUN(TYPE, VAL)                                                         \
  TYPE a##TYPE[SZ];                                                            \
  TYPE b##TYPE[SZ];                                                            \
  for (int i = 0; i < SZ; i++)                                                 \
    {                                                                          \
      a##TYPE[i] = 123;                                                        \
      b##TYPE[i] = VAL;                                                        \
    }                                                                          \
  vsub_##TYPE (a##TYPE, a##TYPE, b##TYPE, SZ);                                 \
  for (int i = 0; i < SZ; i++)                                                 \
    assert (a##TYPE[i] == 123 - VAL);

#define RUN2(TYPE, VAL)                                                        \
  TYPE as##TYPE[SZ];                                                           \
  for (int i = 0; i < SZ; i++)                                                 \
    as##TYPE[i] = 234;                                                         \
  vsubs_##TYPE (as##TYPE, as##TYPE, VAL, SZ);                                  \
  for (int i = 0; i < SZ; i++)                                                 \
    assert (as##TYPE[i] == 234 - VAL);

#define RUN3(TYPE)                                                             \
  TYPE as2##TYPE[SZ];                                                          \
  for (int i = 0; i < SZ; i++)                                                 \
    as2##TYPE[i] = i * 33 - 779;                                               \
  vsubi_##TYPE (as2##TYPE, as2##TYPE, SZ);                                     \
  for (int i = 0; i < SZ; i++)                                                 \
    assert (as2##TYPE[i] == (TYPE) (-16 - (i * 33 - 779)));

#define RUN4(TYPE)                                                             \
  TYPE as3##TYPE[SZ];                                                          \
  for (int i = 0; i < SZ; i++)                                                 \
    as3##TYPE[i] = i * -3 + 267;                                               \
  vsubi2_##TYPE (as3##TYPE, as3##TYPE, SZ);                                    \
  for (int i = 0; i < SZ; i++)                                                 \
    assert (as3##TYPE[i] == (TYPE) (15 - (i * -3 + 267)));

#define RUN_ALL()	\
 RUN(_Float16, 4)	\
 RUN2(_Float16, 10)	\
 RUN3(_Float16)		\
 RUN4(_Float16)		\

int main ()
{
  RUN_ALL()
}
