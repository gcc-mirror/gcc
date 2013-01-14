/* { dg-do compile } */
/* { dg-options "-O2" } */

void
foo (double *output)
{
  *output = 4.25;
}

/* { dg-final { scan-assembler "fmov\\td\[0-9\]+, 4\\.25" } } */
