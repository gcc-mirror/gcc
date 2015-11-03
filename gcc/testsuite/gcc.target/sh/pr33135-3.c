/* Check that fcmp/eq and fcmp/gt instructions are generated when specifying
   -ffinite-math-only and -mieee.  */
/* { dg-do compile { target { any_fpu } } }  */
/* { dg-options "-O1 -ffinite-math-only -mieee" } */
/* { dg-final { scan-assembler-times "fcmp/eq" 4 } } */
/* { dg-final { scan-assembler-times "fcmp/gt" 4 } } */

int
test_00 (float a, float b)
{
  return a <= b;
}

int
test_01 (float a, float b)
{
  return a >= b;
}

int
test_02 (double a, double b)
{
  return a <= b;
}

int
test_03 (double a, double b)
{
  return a >= b;
}
