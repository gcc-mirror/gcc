/* { dg-do compile } */
/* { dg-options "-O2" } */

void
foo (float *output)
{
  *output = 4.25;
}

/* { dg-final { scan-assembler "fmov\\ts\[0-9\]+, 4\\.25" } } */
