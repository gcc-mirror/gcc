/* { dg-do compile } */
/* { dg-options "-mhard-float -march=mips32r6" } */

NOMIPS16 float
test01 (float a, float b)
{
  return __builtin_mipsr6_min_a_s (a, b);
}

NOMIPS16 double
test02 (double a, double b)
{
  return __builtin_mipsr6_min_a_d (a, b);
}

NOMIPS16 float
test03 (float a, float b)
{
  return __builtin_mipsr6_max_a_s (a, b);
}

NOMIPS16 double
test04 (double a, double b)
{
  return __builtin_mipsr6_max_a_d (a, b);
}

/* { dg-final { scan-assembler "mina\\.s" } } */
/* { dg-final { scan-assembler "mina\\.d" } } */
/* { dg-final { scan-assembler "maxa\\.s" } } */
/* { dg-final { scan-assembler "maxa\\.d" } } */
