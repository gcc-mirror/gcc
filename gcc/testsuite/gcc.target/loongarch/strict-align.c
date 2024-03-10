/* { dg-do compile } */
/* { dg-options "-Ofast -mstrict-align -mlasx" } */
/* { dg-final { scan-assembler-not "vfadd.s" } } */

void
foo (float *restrict x, float *restrict y)
{
  x[0] = x[0] + y[0];
  x[1] = x[1] + y[1];
  x[2] = x[2] + y[2];
  x[3] = x[3] + y[3];
}
