/* PR tree-optimization/93953 */
/* { dg-do compile } */
/* { dg-options "-O3 --param=ggc-min-expand=0 --param=ggc-min-heapsize=0" } */

int *b, c, e;
float d, g, f;

void
foo (int l)
{
  for (; l; ++l)
    {
      float a = g > l;
      d += a * b[4 * (l + c * e)];
      f += a * b[4 * (l + c * e) + 1];
    }
}
