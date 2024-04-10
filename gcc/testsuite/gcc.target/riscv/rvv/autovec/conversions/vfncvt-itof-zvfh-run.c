/* { dg-do run { target { riscv_zvfh && riscv_zfh} } } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -mrvv-vector-bits=scalable" } */

#include "vfncvt-itof-template.h"

#include <math.h>

TESTS (int64_t, _Float16)
TESTS (int32_t, _Float16)

#define EPS 1e-5

#define RUN(TYPE1, TYPE2, NUM)                                                 \
  TYPE1 src##TYPE1##TYPE2##NUM[NUM];                                           \
  TYPE2 dst##TYPE1##TYPE2##NUM[NUM];                                           \
  for (int i = 0; i < NUM; i++)                                                \
    src##TYPE1##TYPE2##NUM[i] = i * -3 + 833;                                  \
  vfncvts_##TYPE1##TYPE2 (dst##TYPE1##TYPE2##NUM, src##TYPE1##TYPE2##NUM,      \
			  NUM);                                                \
  for (int i = 0; i < NUM; i++)                                                \
    if (__builtin_fabsf16 (dst##TYPE1##TYPE2##NUM[i]                           \
			   - (TYPE2) src##TYPE1##TYPE2##NUM[i])                \
	> EPS)                                                                 \
      __builtin_abort ();

#define RUN2(TYPE1, TYPE2, NUM)                                                \
  TYPE1 src##TYPE1##TYPE2##NUM[NUM];                                           \
  TYPE2 dst##TYPE1##TYPE2##NUM[NUM];                                           \
  for (int i = 0; i < NUM; i++)                                                \
    src##TYPE1##TYPE2##NUM[i] = i * 3 + 892;                                   \
  vfncvt_##TYPE1##TYPE2 (dst##TYPE1##TYPE2##NUM, src##TYPE1##TYPE2##NUM, NUM); \
  for (int i = 0; i < NUM; i++)                                                \
    if (__builtin_fabsf16 (dst##TYPE1##TYPE2##NUM[i]                           \
			   - (TYPE2) src##TYPE1##TYPE2##NUM[i])                \
	> EPS)                                                                 \
      __builtin_abort ();
int
main ()
{
  RUN (int32_t, _Float16, 3)
  RUN (int32_t, _Float16, 4)
  RUN (int32_t, _Float16, 7)
  RUN (int32_t, _Float16, 99)
  RUN (int32_t, _Float16, 119)
  RUN (int32_t, _Float16, 128)
  RUN (int32_t, _Float16, 256)
  RUN (int32_t, _Float16, 279)
  RUN (int32_t, _Float16, 555)
  RUN (int32_t, _Float16, 1024)
  RUN (int32_t, _Float16, 1389)
  RUN (int32_t, _Float16, 2048)
  RUN (int32_t, _Float16, 3989)
  RUN (int32_t, _Float16, 4096)
  RUN (int32_t, _Float16, 5975)

  RUN2 (uint32_t, _Float16, 3)
  RUN2 (uint32_t, _Float16, 4)
  RUN2 (uint32_t, _Float16, 7)
  RUN2 (uint32_t, _Float16, 99)
  RUN2 (uint32_t, _Float16, 119)
  RUN2 (uint32_t, _Float16, 128)
  RUN2 (uint32_t, _Float16, 256)
  RUN2 (uint32_t, _Float16, 279)
  RUN2 (uint32_t, _Float16, 555)
  RUN2 (uint32_t, _Float16, 1024)
  RUN2 (uint32_t, _Float16, 1389)
  RUN2 (uint32_t, _Float16, 2048)
  RUN2 (uint32_t, _Float16, 3989)
  RUN2 (uint32_t, _Float16, 4096)
  RUN2 (uint32_t, _Float16, 5975)

  RUN (int64_t, _Float16, 3)
  RUN (int64_t, _Float16, 4)
  RUN (int64_t, _Float16, 7)
  RUN (int64_t, _Float16, 99)
  RUN (int64_t, _Float16, 119)
  RUN (int64_t, _Float16, 128)
  RUN (int64_t, _Float16, 256)
  RUN (int64_t, _Float16, 279)
  RUN (int64_t, _Float16, 555)
  RUN (int64_t, _Float16, 1024)
  RUN (int64_t, _Float16, 1389)
  RUN (int64_t, _Float16, 2048)
  RUN (int64_t, _Float16, 3989)
  RUN (int64_t, _Float16, 4096)
  RUN (int64_t, _Float16, 5975)

  RUN2 (uint64_t, _Float16, 3)
  RUN2 (uint64_t, _Float16, 4)
  RUN2 (uint64_t, _Float16, 7)
  RUN2 (uint64_t, _Float16, 99)
  RUN2 (uint64_t, _Float16, 119)
  RUN2 (uint64_t, _Float16, 128)
  RUN2 (uint64_t, _Float16, 256)
  RUN2 (uint64_t, _Float16, 279)
  RUN2 (uint64_t, _Float16, 555)
  RUN2 (uint64_t, _Float16, 1024)
  RUN2 (uint64_t, _Float16, 1389)
  RUN2 (uint64_t, _Float16, 2048)
  RUN2 (uint64_t, _Float16, 3989)
  RUN2 (uint64_t, _Float16, 4096)
  RUN2 (uint64_t, _Float16, 5975)
}
