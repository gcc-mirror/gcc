/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "sbbl" } } */

int test(unsigned long a, unsigned long b)
{
  return -(a < b);
}

