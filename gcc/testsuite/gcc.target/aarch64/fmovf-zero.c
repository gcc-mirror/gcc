/* { dg-do compile } */
/* { dg-options "-O2" } */

void
foo (float *output)
{
  *output = 0.0;
}

/* { dg-final { scan-assembler "fmov\\ts\[0-9\]+, wzr" } } */
