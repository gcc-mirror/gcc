/* { dg-do compile } */
/* { dg-options "-mhard-float -fno-finite-math-only -march=mips32r6" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

/* Test MIN.D.  */

/* { dg-final { scan-assembler "\tmin\\.d\t" } } */
double
test01 (double x, double y)
{
  return __builtin_fmin (x, y);
}

/* Test MIN.S.  */

/* { dg-final { scan-assembler "\tmin\\.s\t" } } */
float
test02 (float x, float y)
{
  return __builtin_fminf (x, y);
}

/* Test MAX.D.  */

/* { dg-final { scan-assembler "\tmax\\.d\t" } } */
double
test03 (double x, double y)
{
  return __builtin_fmax (x, y);
}

/* Test MAX.S.  */

/* { dg-final { scan-assembler "\tmax\\.s\t" } } */
float
test04 (float x, float y)
{
  return __builtin_fmaxf (x, y);
}

