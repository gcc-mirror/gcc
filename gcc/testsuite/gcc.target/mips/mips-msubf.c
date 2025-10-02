/* { dg-do compile } */
/* { dg-options "-mhard-float -ffast-math -march=mips32r6" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" "-O1" } { "" } } */
/* { dg-final { scan-assembler-times "\tmsubf\\.s\t" 2 } } */
/* { dg-final { scan-assembler-times "\tmsubf\\.d\t" 2 } } */

NOMIPS16 float
test01 (float x, float y, float z)
{
  return x - (y * z);
}

NOMIPS16 double
test02 (double x, double y, double z)
{
  return x - (y * z);
}

NOMIPS16 float
test03 (float x, float y, float z)
{
  return (y * z) - x;
}

NOMIPS16 double
test04 (double x, double y, double z)
{
  return (y * z) - x;
}


