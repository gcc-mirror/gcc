/* PR middle-end/121105 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

svboolx2_t
ret2 (void)
{
  svboolx2_t x = {};
  return x;
}

svboolx4_t
ret4 (void)
{
  return (svboolx4_t) {};
}

void
mem2 (svboolx2_t *p)
{
  *p = (svboolx2_t) {};
}

void
mem4 (svboolx4_t *p)
{
  *p = (svboolx4_t) {};
}

/* { dg-final { scan-assembler-times {\tpfalse\tp[0-9]+\.b\n} 12 } } */
/* { dg-final { scan-assembler-times {\tstr\tp[0-9]+, \[x0(?:, #[1-3], mul vl)?\]\n} 6 } } */
