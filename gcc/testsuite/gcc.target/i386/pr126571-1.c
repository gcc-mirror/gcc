/* { dg-do compile } */
/* { dg-options "-O2" } */
/* PR tree-optimization/126571 */

typedef __seg_fs  int *type1;

int
f (int c, type1 fp, int *q)
{
  int r;
  if (c)
    r = *fp;
  else
    r = *q;
  return r + 1;
}
