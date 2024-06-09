/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mrvv-vector-bits=zvl" } */

#include "riscv_vector.h"

typedef int8_t vnx2qi __attribute__ ((vector_size (2)));

__attribute__ ((noipa)) vnx2qi
f_vnx2qi (int8_t a, int8_t b, int8_t *out)
{
  vnx2qi v = {a, b};
  return v;
}

__attribute__ ((noipa)) vnx2qi
f_vnx2qi_2 (vnx2qi a, int8_t *out)
{
  return a;
}

__attribute__ ((noipa)) vint32m1_t
f_vint32m1 (int8_t *a, int8_t *out)
{
  vint32m1_t v = *(vint32m1_t *) a;
  return v;
}
