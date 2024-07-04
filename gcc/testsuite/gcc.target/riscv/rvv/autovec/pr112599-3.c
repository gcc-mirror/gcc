/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zfh_zvl1024b -mabi=lp64d -O3 -mrvv-vector-bits=zvl" } */

#include "riscv_vector.h"

typedef int64_t v1024b __attribute__ ((vector_size (128)));

void foo (void *out, void *in, int64_t a, int64_t b, int64_t c, int64_t d, int64_t e)
{
  v1024b v = {a,a,a,a,a,a,a,a,a,a,a,a,b,c,d,e};
  __riscv_vse64_v_i64m1 (out, (vint64m1_t)v, 12);
}

/* { dg-final { scan-assembler {vsetivli\s+zero,\s*16} } } */
