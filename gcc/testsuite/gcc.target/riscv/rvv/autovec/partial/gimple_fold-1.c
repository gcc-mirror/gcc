/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=zvl -mrvv-max-lmul=m8 -O3 -fdump-tree-optimized-details" } */

#include <stdint-gcc.h>

#define SZ 255

#define DEF(TYPE) void fn_##TYPE (TYPE *__restrict a);

#define RUN(TYPE)                                                              \
  TYPE a##TYPE[SZ];                                                            \
  for (int i = 0; i < SZ; i++)                                                 \
    {                                                                          \
      a##TYPE[i] = 127;                                                        \
    }                                                                          \
  fn_##TYPE (a##TYPE);

#define RUN_ALL()                                                              \
  RUN (int8_t)                                                                 \
  RUN (int16_t)                                                                \
  RUN (int32_t)                                                                \
  RUN (int64_t)                                                                \
  RUN (uint8_t)                                                                \
  RUN (uint16_t)                                                               \
  RUN (uint32_t)                                                               \
  RUN (uint64_t)

DEF (int8_t)
DEF (int16_t)
DEF (int32_t)
DEF (int64_t)
DEF (uint8_t)
DEF (uint16_t)
DEF (uint32_t)
DEF (uint64_t)

int
main ()
{
  RUN_ALL ()
}

/* { dg-final { scan-tree-dump-times "\.MASK_LEN_STORE" 6 "optimized" } } */
