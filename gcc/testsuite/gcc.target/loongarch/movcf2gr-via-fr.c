/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64 -mtune=la464 -mabi=lp64d" } */
/* { dg-final { scan-assembler "movcf2fr\t\\\$f\[0-9\]+,\\\$fcc" } } */
/* { dg-final { scan-assembler "movfr2gr\\.s\t\\\$r4" } } */

int
t (float a, float b)
{
  return a > b;
}
