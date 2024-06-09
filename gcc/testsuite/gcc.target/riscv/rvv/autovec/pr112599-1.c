/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zfh_zvl1024b -mabi=lp64d -O3 -mrvv-vector-bits=zvl" } */

#include "riscv_vector.h"

typedef int64_t v1024b __attribute__ ((vector_size (128)));

void foo (void *out, void *in, int64_t a, int64_t b)
{
  v1024b v = {a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a};
  v1024b v2 = {b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b};
  v1024b index = *(v1024b*)in;
  v1024b v3 = __builtin_shuffle (v, v2, index);
  __riscv_vse64_v_i64m1 (out, (vint64m1_t)v3, 10);
}

/* { dg-final { scan-assembler {vsetivli\s+zero,\s*16} } } */
