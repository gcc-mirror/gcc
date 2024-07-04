/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -mrvv-vector-bits=zvl" } */

#include "vzext-template.h"

#include <assert.h>

#define SZ 256

#define RUN(TYPE1,TYPE2)				\
  TYPE1 src##TYPE1##TYPE2[SZ];				\
  TYPE2 dst##TYPE1##TYPE2[SZ];				\
  for (int i = 0; i < SZ; i++)				\
  {                             			\
    src##TYPE1##TYPE2[i] = i;             		\
    dst##TYPE1##TYPE2[i] = -1;				\
  }                             			\
  vzext_##TYPE1##TYPE2 (dst##TYPE1##TYPE2,		\
			src##TYPE1##TYPE2, SZ);		\
  for (int i = 0; i < SZ; i++)				\
    assert (dst##TYPE1##TYPE2[i] == i);


#define RUN_ALL()					\
 RUN(uint8_t, uint16_t)					\
 RUN(uint8_t, uint32_t)					\
 RUN(uint8_t, uint64_t)					\
 RUN(uint16_t, uint32_t)				\
 RUN(uint16_t, uint64_t)				\
 RUN(uint32_t, uint64_t)				\

int main ()
{
  RUN_ALL()
}
