/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_zvl256b -mabi=ilp32d -O3 -mrvv-vector-bits=zvl" } */

#include <stdint-gcc.h>

typedef int64_t vnx2di __attribute__ ((vector_size (16)));

__attribute__ ((noipa)) void
f_vnx2di (int64_t a, int64_t b, int64_t *out)
{
  vnx2di v = {a, b};
  *(vnx2di *) out = v;
}
