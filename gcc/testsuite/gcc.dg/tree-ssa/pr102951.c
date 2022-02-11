/* PR tree-optimization/102951 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ccp1" } */
/* { dg-final { scan-tree-dump-times "return \&a\\\[1\\\];" 2 "ccp1" } } */
/* { dg-final { scan-tree-dump-times "return \&a\\\[4\\\];" 2 "ccp1" } } */
/* { dg-final { scan-tree-dump-not "MIN_EXPR" "ccp1" } } */
/* { dg-final { scan-tree-dump-not "MAX_EXPR" "ccp1" } } */

extern int a[5];

int *
foo (void)
{
  int *p1 = &a[1];
  int *p2 = &a[2];
  return p1 < p2 ? p1 : p2;
}

int *
bar (void)
{
  int *p1 = &a[1];
  int *p2 = &a[2];
  return p1 <= p2 ? p1 : p2;
}

int *
baz (void)
{
  int *p1 = &a[3];
  int *p2 = &a[4];
  return p1 > p2 ? p1 : p2;
}

int *
qux (void)
{
  int *p1 = &a[3];
  int *p2 = &a[4];
  return p1 >= p2 ? p1 : p2;
}
