/* { dg-do compile } */
/* { dg-options "-O2" } */

void
foo (double *output)
{
  *output = 0.0;
}

/* { dg-final { scan-assembler "fmov\\td\[0-9\]+, xzr" } } */
