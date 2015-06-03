/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1" } */

struct S { float a, b; };

float
foo (int x, float y)
{
  struct S z[1024];
  z[x].a = y;
  struct S *p = &z[x];
  float *q = (float *) p;
  return *q;
}

/* { dg-final { scan-tree-dump "return y_\\d\+\\(D\\);" "fre1" } } */
