/* PR tree-optimization/39455 */
/* { dg-do compile } */
/* { dg-options "-O2 -fprefetch-loop-arrays -w" } */
/* { dg-additional-options "-march=i686 -msse" { target { { i?86-*-* x86_64-*-* } && ia32 } } } */

void
foo (char *x, unsigned long y, unsigned char *z)
{
  unsigned int c[256], *d;

  for (d = c + 1; d < c + 256; ++d)
    *d += d[-1];
  x[--c[z[y]]] = 0;
}
