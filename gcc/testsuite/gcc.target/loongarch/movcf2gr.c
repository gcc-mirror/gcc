/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64 -mtune=la664 -mabi=lp64d" } */
/* { dg-final { scan-assembler "movcf2gr\t\\\$r4,\\\$fcc" } } */

int
t (float a, float b)
{
  return a > b;
}
