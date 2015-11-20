/* { dg-do compile } */
/* { dg-options "-std=c99" } */

#include <stdint.h>

typedef double float64x1_t __attribute__ ((vector_size (8)));
typedef uint64_t uint64x1_t;

void
foo (void)
{
  float64x1_t arg1 = (float64x1_t) 0x3fedf9d4343c7c80;
  float64x1_t arg2 = (float64x1_t) 0x3fcdc53742ea9c40;
  uint64x1_t result = (uint64x1_t) (arg1 == arg2);
  uint64_t got = result;
  uint64_t exp = 0;
  if (got != 0)
    __builtin_abort ();
}
