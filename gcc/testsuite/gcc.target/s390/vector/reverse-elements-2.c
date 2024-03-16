/* { dg-compile } */
/* { dg-options "-O3 -mzarch -march=z14" } */
/* { dg-require-effective-target s390_vxe } */
/* { dg-final { scan-assembler-times {\tvpdi\t} 1 } } */
/* { dg-final { scan-assembler-not {\tvperm\t} } } */

typedef float __attribute__ ((vector_size (16))) V4SF;

V4SF
v4sf (V4SF x)
{
  V4SF y;
  for (int i = 0; i < 4; ++i)
    y[i] = x[3 - i];
  return y;
}
