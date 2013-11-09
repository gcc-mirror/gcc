/* Check that only the fcmp/gt instruction is generated when specifying
   -ffinite-math-only (implicit -mno-ieee).  */
/* { dg-do compile }  */
/* { dg-options "-O1 -ffinite-math-only" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m1" "-m2" "-m3" "-m4al" "*nofpu" "-m4-340*" "-m4-400*" "-m4-500*" "-m5*" } { "" } }  */
/* { dg-final { scan-assembler-not "fcmp/eq" } } */
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

