/* { dg-do compile { target { loongarch64*-*-* } } } */
/* { dg-options "-O3 -mno-lsx" } */
/* { dg-final { scan-assembler-not "stptr\.d" } } */
/* { dg-final { scan-assembler-not "fld\.d" } } */
/* { dg-final { scan-assembler-not "fst\.d" } } */
/* { dg-final { scan-assembler-not "ldptr\.d" } } */
/* { dg-final { scan-assembler "movgr2fr\.d" } } */
/* { dg-final { scan-assembler "movfr2gr\.d" } } */

typedef double vec __attribute__ ((vector_size(16)));

vec
foo (vec x, double a)
{
  x[0] -= a;
  return x;
}
