/* { dg-do compile } */
/* { dg-options "-O3 -march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=zvl" } */

#include <stdint-gcc.h>

typedef int8_t vnx2qi __attribute__ ((vector_size (2)));

__attribute__ ((noipa)) void
f_vnx2qi (int8_t a, int8_t b, int8_t *out)
{
  vnx2qi v = {a, b};
  *(vnx2qi *) out = v;
}

/* { dg-final { scan-assembler {vsetivli\s+zero,\s*2,\s*e8,\s*mf8,\s*t[au],\s*m[au]} } } */
