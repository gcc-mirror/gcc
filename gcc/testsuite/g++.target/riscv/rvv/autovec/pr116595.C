/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d" } */

#include <riscv_vector.h>

void
vsseg (float *a, vfloat32mf2_t b, vfloat32mf2_t c, unsigned long vl)
{
  vfloat32mf2x2_t foo = vfloat32mf2x2_t ();
}
