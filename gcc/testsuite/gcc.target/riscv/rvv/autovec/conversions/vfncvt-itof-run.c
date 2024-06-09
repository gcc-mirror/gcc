/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -mrvv-vector-bits=scalable" } */

#include "vfncvt-itof-template.h"

#define RUN(TYPE1, TYPE2, NUM)                                                 \
  TYPE1 src##TYPE1##TYPE2##NUM[NUM];                                           \
  TYPE2 dst##TYPE1##TYPE2##NUM[NUM];                                           \
  for (int i = 0; i < NUM; i++)                                                \
    {                                                                          \
      src##TYPE1##TYPE2##NUM[i] = i * 3 + 88932;                               \
    }                                                                          \
  vfncvt_##TYPE1##TYPE2 (dst##TYPE1##TYPE2##NUM, src##TYPE1##TYPE2##NUM, NUM); \
  for (int i = 0; i < NUM; i++)                                                \
    if (dst##TYPE1##TYPE2##NUM[i] != (TYPE2) src##TYPE1##TYPE2##NUM[i])        \
      __builtin_abort ();

int
main ()
{
  RUN (int64_t, float, 3)
  RUN (int64_t, float, 4)
  RUN (int64_t, float, 7)
  RUN (int64_t, float, 99)
  RUN (int64_t, float, 119)
  RUN (int64_t, float, 128)
  RUN (int64_t, float, 256)
  RUN (int64_t, float, 279)
  RUN (int64_t, float, 555)
  RUN (int64_t, float, 1024)
  RUN (int64_t, float, 1389)
  RUN (int64_t, float, 2048)
  RUN (int64_t, float, 3989)
  RUN (int64_t, float, 4096)
  RUN (int64_t, float, 5975)

  RUN (uint64_t, float, 3)
  RUN (uint64_t, float, 4)
  RUN (uint64_t, float, 7)
  RUN (uint64_t, float, 99)
  RUN (uint64_t, float, 119)
  RUN (uint64_t, float, 128)
  RUN (uint64_t, float, 256)
  RUN (uint64_t, float, 279)
  RUN (uint64_t, float, 555)
  RUN (uint64_t, float, 1024)
  RUN (uint64_t, float, 1389)
  RUN (uint64_t, float, 2048)
  RUN (uint64_t, float, 3989)
  RUN (uint64_t, float, 4096)
  RUN (uint64_t, float, 5975)
}
