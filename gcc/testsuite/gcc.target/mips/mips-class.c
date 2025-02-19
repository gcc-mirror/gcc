/* { dg-do compile } */
/* { dg-options "-mhard-float -march=mips32r6" } */

NOMIPS16 float
test01 (float a)
{
  return __builtin_mipsr6_class_s (a);
}

NOMIPS16 double
test02 (double a)
{
  return __builtin_mipsr6_class_d (a);
}

/* { dg-final { scan-assembler "class\\.s" } } */
/* { dg-final { scan-assembler "class\\.d" } } */
