/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv_zvl256b -mabi=lp64d" } */

#include <stdint.h>
typedef uint16_t v16 __attribute__((vector_size(32)));

v16
foo (v16 x)
{
  return __builtin_shufflevector (x, x, 0, 1, 0, 1, 2, 3, 2, 3, 4, 5, 4, 5, 6,
				  7, 6, 7);
}

/* { dg-final { scan-assembler-not "vmv.v.i\\sv\[0-9\]+,0" } } */
/* { dg-final { scan-assembler-times "vle16" 2 } } */
