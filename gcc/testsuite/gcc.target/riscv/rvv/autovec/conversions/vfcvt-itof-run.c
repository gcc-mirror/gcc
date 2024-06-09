/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -mrvv-vector-bits=scalable" } */

#include "vfcvt-itof-template.h"

#define RUN(TYPE1, TYPE2, NUM)                                                 \
  TYPE1 src##TYPE1##TYPE2##NUM[NUM];                                           \
  TYPE2 dst##TYPE1##TYPE2##NUM[NUM];                                           \
  for (int i = 0; i < NUM; i++)                                                \
    {                                                                          \
      src##TYPE1##TYPE2##NUM[i] = i * -3 - 88932;                              \
    }                                                                          \
  vfcvt_##TYPE1##TYPE2 (dst##TYPE1##TYPE2##NUM, src##TYPE1##TYPE2##NUM, NUM);  \
  for (int i = 0; i < NUM; i++)                                                \
    if (dst##TYPE1##TYPE2##NUM[i] != (TYPE2) src##TYPE1##TYPE2##NUM[i])        \
      __builtin_abort ();

#define RUN2(TYPE1, TYPE2, NUM)                                                \
  TYPE1 src##TYPE1##TYPE2##NUM[NUM];                                           \
  TYPE2 dst##TYPE1##TYPE2##NUM[NUM];                                           \
  for (int i = 0; i < NUM; i++)                                                \
    {                                                                          \
      src##TYPE1##TYPE2##NUM[i] = i * 3 + 88932;                               \
    }                                                                          \
  vfcvt_##TYPE1##TYPE2 (dst##TYPE1##TYPE2##NUM, src##TYPE1##TYPE2##NUM, NUM);  \
  for (int i = 0; i < NUM; i++)                                                \
    if (dst##TYPE1##TYPE2##NUM[i] != (TYPE2) src##TYPE1##TYPE2##NUM[i])        \
      __builtin_abort ();

int
main ()
{
  RUN (int32_t, float, 3)
  RUN (int32_t, float, 4)
  RUN (int32_t, float, 7)
  RUN (int32_t, float, 99)
  RUN (int32_t, float, 119)
  RUN (int32_t, float, 128)
  RUN (int32_t, float, 256)
  RUN (int32_t, float, 279)
  RUN (int32_t, float, 555)
  RUN (int32_t, float, 1024)
  RUN (int32_t, float, 1389)
  RUN (int32_t, float, 2048)
  RUN (int32_t, float, 3989)
  RUN (int32_t, float, 4096)
  RUN (int32_t, float, 5975)

  RUN2 (uint32_t, float, 3)
  RUN2 (uint32_t, float, 4)
  RUN2 (uint32_t, float, 7)
  RUN2 (uint32_t, float, 99)
  RUN2 (uint32_t, float, 119)
  RUN2 (uint32_t, float, 128)
  RUN2 (uint32_t, float, 256)
  RUN2 (uint32_t, float, 279)
  RUN2 (uint32_t, float, 555)
  RUN2 (uint32_t, float, 1024)
  RUN2 (uint32_t, float, 1389)
  RUN2 (uint32_t, float, 2048)
  RUN2 (uint32_t, float, 3989)
  RUN2 (uint32_t, float, 4096)
  RUN2 (uint32_t, float, 5975)

  RUN (int64_t, double, 3)
  RUN (int64_t, double, 4)
  RUN (int64_t, double, 7)
  RUN (int64_t, double, 99)
  RUN (int64_t, double, 119)
  RUN (int64_t, double, 128)
  RUN (int64_t, double, 256)
  RUN (int64_t, double, 279)
  RUN (int64_t, double, 555)
  RUN (int64_t, double, 1024)
  RUN (int64_t, double, 1389)
  RUN (int64_t, double, 2048)
  RUN (int64_t, double, 3989)
  RUN (int64_t, double, 4096)
  RUN (int64_t, double, 5975)

  RUN2 (uint64_t, double, 3)
  RUN2 (uint64_t, double, 4)
  RUN2 (uint64_t, double, 7)
  RUN2 (uint64_t, double, 99)
  RUN2 (uint64_t, double, 119)
  RUN2 (uint64_t, double, 128)
  RUN2 (uint64_t, double, 256)
  RUN2 (uint64_t, double, 279)
  RUN2 (uint64_t, double, 555)
  RUN2 (uint64_t, double, 1024)
  RUN2 (uint64_t, double, 1389)
  RUN2 (uint64_t, double, 2048)
  RUN2 (uint64_t, double, 3989)
  RUN2 (uint64_t, double, 4096)
  RUN2 (uint64_t, double, 5975)
}
