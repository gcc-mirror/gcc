/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fvect-cost-model=dynamic" } */

void bar (int *);
int foo (int *p)
{
  int x[4];
  int tem0, tem1, tem2, tem3;
  tem0 = p[0] + 1;
  x[0] = tem0;
  tem1 = p[1] + 2;
  x[1] = tem1;
  tem2 = p[2] + 3;
  x[2] = tem2;
  tem3 = p[3] + 4;
  x[3] = tem3;
  bar (x);
  return tem0 + tem1 + tem2 + tem3;
}

/* { dg-final { scan-tree-dump "vectorization is not profitable" "slp" { xfail  vect_no_align } } } */
/* { dg-final { cleanup-tree-dump "slp" } } */
