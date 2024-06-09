/* { dg-do run { target { riscv_v && riscv_zvfh } } } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -mrvv-vector-bits=scalable" } */

#include "vfcvt-itof-template.h"

#define RUN(TYPE1, TYPE2, NUM)                                                 \
  TYPE1 src##TYPE1##TYPE2##NUM[NUM];                                           \
  TYPE2 dst##TYPE1##TYPE2##NUM[NUM];                                           \
  for (int i = 0; i < NUM; i++)                                                \
    {                                                                          \
      src##TYPE1##TYPE2##NUM[i] = i * 3 - 8932;                                \
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
      src##TYPE1##TYPE2##NUM[i] = i * 3 + 8932;                                \
    }                                                                          \
  vfcvt_##TYPE1##TYPE2 (dst##TYPE1##TYPE2##NUM, src##TYPE1##TYPE2##NUM, NUM);  \
  for (int i = 0; i < NUM; i++)                                                \
    if (dst##TYPE1##TYPE2##NUM[i] != (TYPE2) src##TYPE1##TYPE2##NUM[i])        \
      __builtin_abort ();

int
main ()
{
  RUN (int16_t, _Float16, 3)
  RUN (int16_t, _Float16, 4)
  RUN (int16_t, _Float16, 7)
  RUN (int16_t, _Float16, 99)
  RUN (int16_t, _Float16, 119)
  RUN (int16_t, _Float16, 128)
  RUN (int16_t, _Float16, 256)
  RUN (int16_t, _Float16, 279)
  RUN (int16_t, _Float16, 555)
  RUN (int16_t, _Float16, 1024)
  RUN (int16_t, _Float16, 1389)
  RUN (int16_t, _Float16, 2048)
  RUN (int16_t, _Float16, 3989)
  RUN (int16_t, _Float16, 4096)
  RUN (int16_t, _Float16, 5975)

  RUN2 (uint16_t, _Float16, 3)
  RUN2 (uint16_t, _Float16, 4)
  RUN2 (uint16_t, _Float16, 7)
  RUN2 (uint16_t, _Float16, 99)
  RUN2 (uint16_t, _Float16, 119)
  RUN2 (uint16_t, _Float16, 128)
  RUN2 (uint16_t, _Float16, 256)
  RUN2 (uint16_t, _Float16, 279)
  RUN2 (uint16_t, _Float16, 555)
  RUN2 (uint16_t, _Float16, 1024)
  RUN2 (uint16_t, _Float16, 1389)
  RUN2 (uint16_t, _Float16, 2048)
  RUN2 (uint16_t, _Float16, 3989)
  RUN2 (uint16_t, _Float16, 4096)
  RUN2 (uint16_t, _Float16, 5975)
}
