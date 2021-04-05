/* PR tree-optimization/99882 */
/* { dg-do compile } */
/* { dg-options "-O3" } */

void
foo (char *p, void *q)
{
  __INTPTR_TYPE__ i = (__INTPTR_TYPE__) q;
  p[2] = i;
  i >>= 8;
  p[3] = i;
  i >>= 8;
  p[4] = i;
  i >>= 8;
  p[5] = i;
  i >>= 8;
  p[6] = i;
  i >>= 8;
  p[7] = i;
  i >>= 8;
  p[8] = i;
  i >>= 8;
  p[9] = i;
}

void
bar (char *p, void *q)
{
  __INTPTR_TYPE__ i = (__INTPTR_TYPE__) q;
  p[2] = i;
  i >>= 8;
  p[3] = i;
  i >>= 8;
  p[4] = i;
  i >>= 8;
  p[5] = i;
}
