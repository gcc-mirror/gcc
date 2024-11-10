/* { dg-do compile } */
/* { dg-require-effective-target arm_cpu_cortex_m7_ok } */
/* { dg-options "-O2" }  */
/* { dg-add-options arm_cpu_cortex_m7 } */

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
/* Ensure there is no IT block with more than 2 instructions, ie. we only allow
   IT, ITT and ITE.  */
/* { dg-final { scan-assembler-not "\\sit\[te\]{2}" } } */
