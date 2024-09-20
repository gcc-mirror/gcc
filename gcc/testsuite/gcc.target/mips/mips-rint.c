/* { dg-do compile } */
/* { dg-options "-mhard-float -march=mips32r6" } */

NOMIPS16 float 
test01 (float a)
{
  return __builtin_rintf (a);
}

NOMIPS16 double 
test02 (double a)
{
  return __builtin_rint (a);
}

/* { dg-final { scan-assembler "rint\\.s" } } */
/* { dg-final { scan-assembler "rint\\.d" } } */
