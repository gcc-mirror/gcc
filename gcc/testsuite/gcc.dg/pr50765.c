/* PR tree-optimization/50765 */
/* { dg-do compile } */
/* { dg-options "-O3 -fno-tree-dce" } */

void
foo (long *w, long *x, unsigned char *y, int z)
{
  for (; z < 429; z++)
    w[z] = (-19 ^ y[z]) & x[z];
}
