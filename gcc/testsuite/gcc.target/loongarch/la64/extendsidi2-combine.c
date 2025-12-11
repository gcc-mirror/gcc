/* { dg-do compile { target { loongarch64*-*-* } } } */
/* { dg-options "-O3 -fno-strict-aliasing" } */

int
test (double a)
{
  int z;

  *((double *)&z) = a;
  return z;
}

/* { dg-final { scan-assembler-not "slli\\.w" } } */
