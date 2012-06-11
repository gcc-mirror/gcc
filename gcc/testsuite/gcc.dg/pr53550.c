/* PR tree-optimization/53550 */
/* { dg-do compile } */
/* { dg-options "-O2 -fprefetch-loop-arrays -w" } */

int *
foo (int *x)
{
  int *a = x + 10, *b = x, *c = a;
  while (b != c)
    *--c = *b++;
  return x;
}
