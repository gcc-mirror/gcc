/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable" } */

#include <assert.h>
#include "narrow-3.c"

#define RUN(TYPE1, TYPE2, SZ)                                                  \
  TYPE1 a##TYPE1##_##TYPE2##_##SZ[SZ];                                         \
  TYPE2 dst##TYPE1##_##TYPE2##_##SZ[SZ];                                       \
  for (int i = 0; i < SZ; i++)                                                 \
    {                                                                          \
      a##TYPE1##_##TYPE2##_##SZ[i] = i % 8723;                                 \
    }                                                                          \
  vnshift_##TYPE1##_##TYPE2 (dst##TYPE1##_##TYPE2##_##SZ,                      \
			     a##TYPE1##_##TYPE2##_##SZ, 9, SZ);                \
  for (int i = 0; i < SZ; i++)                                                 \
    {                                                                          \
      assert (dst##TYPE1##_##TYPE2##_##SZ[i]                                   \
	      == (TYPE2) (a##TYPE1##_##TYPE2##_##SZ[i] >> 9));                 \
    }

#define RUN_ALL(SZ)                                                            \
  RUN (int16_t, int8_t, SZ)                                                    \
  RUN (int16_t, uint8_t, SZ)                                                   \
  RUN (uint16_t, int8_t, SZ)                                                   \
  RUN (uint16_t, uint8_t, SZ)                                                  \
  RUN (int32_t, int16_t, SZ)                                                   \
  RUN (int32_t, uint16_t, SZ)                                                  \
  RUN (uint32_t, int16_t, SZ)                                                  \
  RUN (uint32_t, uint16_t, SZ)                                                 \
  RUN (int64_t, int32_t, SZ)                                                   \
  RUN (int64_t, uint32_t, SZ)                                                  \
  RUN (uint64_t, int32_t, SZ)                                                  \
  RUN (uint64_t, uint32_t, SZ)

int
main ()
{
  RUN_ALL (15)
  RUN_ALL (16)
  RUN_ALL (17)
  RUN_ALL (127)
  RUN_ALL (128)
  RUN_ALL (129)
  RUN_ALL (512)
}
