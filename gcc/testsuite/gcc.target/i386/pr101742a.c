/* { dg-do compile } */
/* { dg-additional-options "-O3 -mtune=nano-x2" } */

int n2;

__attribute__ ((simd)) char
w7 (void)
{
  short int xb = n2;
  int qp;

  for (qp = 0; qp < 2; ++qp)
    xb = xb < 1;

  return xb;
}
