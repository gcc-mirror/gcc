/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ccp1" } */

static const int x;
int foo()
{
  const int *p = &x;
  int y = *p;
  return y + 1;
}

static const int x2[3] = { 1, 0, 2 };
int bar()
{
  int i = 1;
  const int *p = &x2[i];
  int y = *p;
  return y + 1;
}

/* { dg-final { scan-tree-dump-times "return 1;" 2 "ccp1" } } */

