/* { dg-do compile }*/
/* { dg-options "-O2  -fno-trapping-math -msve-vector-bits=2048 " } */

#include <stdint.h>
#pragma GCC target "arch=armv9-a+sve-b16b16"

#define COND_BFMLA(TYPE, PRED_TYPE, MERGE)                               \
  TYPE test_bfmla_##TYPE##_##MERGE (TYPE a, TYPE b, TYPE c, PRED_TYPE p) \
  {return p ? a * b + c : MERGE; }

#define COND_BFMLS(TYPE, PRED_TYPE, MERGE)                               \
  TYPE test_bfmls_##TYPE##_##MERGE (TYPE a, TYPE b, TYPE c, PRED_TYPE p) \
  {return p ? a * -b + c : MERGE; }

#define TEST_OP(TYPE, PRED_TYPE, T) \
  T (TYPE, PRED_TYPE, c)            \
  T (TYPE, PRED_TYPE, 0)

#define TEST(TYPE, PTYPE, SIZE)                                   \
  typedef TYPE TYPE##SIZE __attribute__ ((vector_size (SIZE)));   \
  typedef PTYPE PTYPE##SIZE __attribute__ ((vector_size (SIZE))); \
  TEST_OP (TYPE##SIZE, PTYPE##SIZE, COND_BFMLA)                   \
  TEST_OP (TYPE##SIZE, PTYPE##SIZE, COND_BFMLS)

TEST (__bf16, uint16_t, 128)

TEST (__bf16, uint16_t, 64)

/* { dg-final { scan-assembler-times {\tptrue} 8 } } */

/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+\.h, p[0-7]/z, z[0-9]+\.h\n} 4 } } */
/* { dg-final { scan-assembler-times {\tbfmla\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 4 } } */
/* { dg-final { scan-assembler-times {\tbfmls\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 4 } } */

/* { dg-final { scan-assembler-not {\tsel\t} } } */
