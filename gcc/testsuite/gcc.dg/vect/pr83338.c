/* PR tree-optimization/83338 */
/* { dg-do compile } */

void
foo (char **p, char **q, __PTRDIFF_TYPE__ *r)
{
  int i;
  for (i = 0; i < 1024; i++)
    r[i] = p[i] - q[i];
}
