/* { dg-do compile { target { loongarch64*-*-* } } } */
/* { dg-options "-O3 -fno-strict-aliasing" } */

double
convert (long long in)
{
  double f;
  *((long long *)&f) = in;
  return f;
}

/* { dg-final { scan-assembler-not "st\\.d" } } */
/* { dg-final { scan-assembler-not "fld\\.d" } } */
/* { dg-final { scan-assembler "movgr2fr\\.d" } } */
