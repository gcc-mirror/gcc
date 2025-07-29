/* { dg-do compile }*/
/* { dg-options "-O -ffinite-math-only -fno-signed-zeros -fno-trapping-math -msve-vector-bits=2048 " } */

#include <stdint.h>
#pragma GCC target "arch=armv9-a+sve-b16b16"

#define ADD(a, b) a + b
#define SUB(a, b) a - b
#define MUL(a, b) a * b
#define MAX(a, b) (a > b) ? a : b
#define MIN(a, b) (a > b) ? b : a

#define COND_OP(OP, TYPE, PRED_TYPE, ARG2, MERGE)                                  \
  TYPE test_##OP##_##TYPE##_##ARG2##_##MERGE (TYPE a, TYPE b, TYPE c, PRED_TYPE p) \
  {return p ? OP (a, ARG2) : MERGE; }

#define TEST_OP(OP, TYPE, PRED_TYPE, T)     \
  T (OP, TYPE, PRED_TYPE, b, a)             \
  T (OP, TYPE, PRED_TYPE, b, b)             \
  T (OP, TYPE, PRED_TYPE, b, c)

#define TEST_ALL(TYPE, PRED_TYPE, T) \
  TEST_OP (ADD, TYPE, PRED_TYPE, T)  \
  TEST_OP (SUB, TYPE, PRED_TYPE, T)  \
  TEST_OP (MUL, TYPE, PRED_TYPE, T)  \
  TEST_OP (MAX, TYPE, PRED_TYPE, T)  \
  TEST_OP (MIN, TYPE, PRED_TYPE, T)

#define TEST(TYPE, PTYPE, SIZE)                                   \
  typedef TYPE TYPE##SIZE __attribute__ ((vector_size (SIZE)));   \
  typedef PTYPE PTYPE##SIZE __attribute__ ((vector_size (SIZE))); \
  TEST_ALL (TYPE##SIZE, PTYPE##SIZE, COND_OP)

TEST (__bf16, uint16_t, 128)

TEST (__bf16, uint16_t, 64)

/* { dg-final { scan-assembler-times {\tbfadd\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 6 } } */
/* { dg-final { scan-assembler-times {\tbfsub\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 6 } } */
/* { dg-final { scan-assembler-times {\tbfmul\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 6 } } */

/* { dg-final { scan-assembler-times {\tbfminnm\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 6 } } */
/* { dg-final { scan-assembler-times {\tbfmaxnm\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 6 } } */

// There's no BFSUBR.
/* { dg-final { scan-assembler-times {\tsel\t} 2 } } */
