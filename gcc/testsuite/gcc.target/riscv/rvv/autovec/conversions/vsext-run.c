/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -mrvv-vector-bits=zvl" } */

#include "vsext-template.h"

#include <assert.h>

#define SZ 256

#define RUN(TYPE1,TYPE2)				\
  TYPE1 src##TYPE1##TYPE2[SZ];				\
  TYPE2 dst##TYPE1##TYPE2[SZ];				\
  for (int i = 0; i < SZ; i++)				\
  {                             			\
    src##TYPE1##TYPE2[i] = i - 128;            		\
    dst##TYPE1##TYPE2[i] = 0;				\
  }                             			\
  vsext_##TYPE1##TYPE2 (dst##TYPE1##TYPE2,		\
			src##TYPE1##TYPE2, SZ);		\
  for (int i = 0; i < SZ; i++)				\
    assert (dst##TYPE1##TYPE2[i] == i - 128);


#define RUN_ALL()					\
 RUN(int8_t, int16_t)					\
 RUN(int8_t, int32_t)					\
 RUN(int8_t, int64_t)					\
 RUN(int16_t, int32_t)					\
 RUN(int16_t, int64_t)					\
 RUN(int32_t, int64_t)					\

int main ()
{
  RUN_ALL()
}
