/* { dg-do run { target { riscv_v && riscv_zvfh } } } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -mrvv-vector-bits=zvl -ffast-math" } */

#include "vneg-template.h"

#include <assert.h>

#define SZ 255

#define RUN(TYPE)				\
  TYPE a##TYPE[SZ];				\
  for (int i = 0; i < SZ; i++)			\
  {                             		\
    a##TYPE[i] = i - 127;             		\
  }                             		\
  vneg_##TYPE (a##TYPE, a##TYPE, SZ);		\
  for (int i = 0; i < SZ; i++)			\
    assert (a##TYPE[i] == -(i - 127));

#define RUN_ALL()	                        \
 RUN(_Float16)	                                \

int main ()
{
  RUN_ALL()
}
