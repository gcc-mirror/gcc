/* { dg-do compile } */
/* { dg-require-effective-target arm_cpu_cortex_m3_ok } */
/* { dg-options "-O2" }  */
/* { dg-add-options arm_cpu_cortex_m3 } */

int test (int a, int b)
{
  int r;
  if (a > 10)
    {
      r = a - b;
      r += 10;
    }
  else
    {
      r = b - a;
      r -= 7;
    }
  if (r > 0)
    r -= 3;
  return r;
}
/* Ensure there is an IT block with at least 2 instructions.  */
/* { dg-final { scan-assembler "\\sit\[te\]{2}" } } */
