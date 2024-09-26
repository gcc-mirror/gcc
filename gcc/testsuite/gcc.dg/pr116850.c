/* { dg-do compile } */
/* { dg-options "-Os -w" } */

int a, b;
int *c()
{
  int d, *e = 0, *f = &d, *g = &a;
  if (b)
    g = 0;
  *e = *g;
  return f;
}
