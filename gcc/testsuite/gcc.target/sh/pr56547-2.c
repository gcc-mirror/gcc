/* Verify that the fmac insn is used for the expression 'a * b + a' and
   'a * a + a' when -ffast-math is specified.  */
/* { dg-do compile { target { any_fpu } } }  */
/* { dg-options "-O1 -ffast-math" } */
/* { dg-final { scan-assembler-times "fmac" 2 } } */

float
test_00 (float a, float b)
{
  return a * b + a;
}

float
test_01 (float a)
{
  return a * a + a;
}
