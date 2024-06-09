/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -mrvv-vector-bits=zvl" } */

#include "shift-template.h"

#include <assert.h>

#define SZ 512

#define RUN(TYPE,VAL)					\
  TYPE a##TYPE[SZ];					\
  TYPE b##TYPE[SZ];	  				\
  for (int i = 0; i < SZ; i++)				\
  {                             			\
    a##TYPE[i] = VAL;             			\
    b##TYPE[i] = i % 4;           		       	\
  }                             			\
  vshl_##TYPE (a##TYPE, a##TYPE, b##TYPE, SZ);		\
  for (int i = 0; i < SZ; i++)				\
    assert (a##TYPE[i] == (VAL << (i % 4)));

#define RUN2(TYPE,VAL)					\
  TYPE as##TYPE[SZ];					\
  TYPE bs##TYPE[SZ];					\
  for (int i = 0; i < SZ; i++)				\
  {                             			\
    as##TYPE[i] = VAL;            			\
    bs##TYPE[i] = i % 4;				\
  }                             			\
  vshiftr_##TYPE (as##TYPE, as##TYPE, bs##TYPE, SZ);	\
  for (int i = 0; i < SZ; i++)				\
    assert (as##TYPE[i] == (VAL >> (i % 4)));

#define RUN_ALL()	\
 RUN(int8_t, 1)	\
 RUN(uint8_t, 2)	\
 RUN(int16_t, 1)	\
 RUN(uint16_t, 2)	\
 RUN(int32_t, 3)	\
 RUN(uint32_t, 4)	\
 RUN(int64_t, 5)	\
 RUN(uint64_t, 6)       \
 RUN2(int8_t, -7)	\
 RUN2(uint8_t, 8)	\
 RUN2(int16_t, -7)	\
 RUN2(uint16_t, 8)	\
 RUN2(int32_t, -9)	\
 RUN2(uint32_t, 10)	\
 RUN2(int64_t, -11)	\
 RUN2(uint64_t, 12)

int main ()
{
  RUN_ALL()
}
