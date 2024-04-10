/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -mrvv-vector-bits=zvl" } */

#include "vnot-template.h"

#include <assert.h>

#define SZ 255

#define RUN(TYPE)				\
  TYPE a##TYPE[SZ];				\
  for (int i = 0; i < SZ; i++)			\
  {                             		\
    a##TYPE[i] = i - 127;             		\
  }                             		\
  vnot_##TYPE (a##TYPE, a##TYPE, SZ);	        \
  for (int i = 0; i < SZ; i++)			\
    assert (a##TYPE[i] == (TYPE)~(i - 127));

#define RUN2(TYPE)				\
  TYPE a##TYPE[SZ];				\
  for (int i = 0; i < SZ; i++)			\
  {                             		\
    a##TYPE[i] = i;             		\
  }                             		\
  vnot_##TYPE (a##TYPE, a##TYPE, SZ);	        \
  for (int i = 0; i < SZ; i++)			\
    assert (a##TYPE[i] == (TYPE)~i);

#define RUN_ALL()	                        \
 RUN(int8_t)	                                \
 RUN(int16_t)	                                \
 RUN(int32_t)	                                \
 RUN(int64_t)	                                \
 RUN(uint8_t)	                                \
 RUN(uint16_t)	                                \
 RUN(uint32_t)	                                \
 RUN(uint64_t)

int main ()
{
  RUN_ALL()
}
