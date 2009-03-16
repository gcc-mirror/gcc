/* PR tree-optimization/39455 */
/* { dg-do compile } */
/* { dg-options "-O2 -fprefetch-loop-arrays -w" } */

void
foo (char *x, unsigned long y, unsigned char *z)
{
  unsigned int c[256], *d;

  for (d = c + 1; d < c + 256; ++d)
    *d += d[-1];
  x[--c[z[y]]] = 0;
}
