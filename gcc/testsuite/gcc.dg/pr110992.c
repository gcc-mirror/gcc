/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

void foo (int);

int f(unsigned b, short c)
{
  int bt = b;
  int bt1 = bt;
  int t = bt1 & -(c!=0);
 // int t = bt1 * (c!=0);

  if (!t) return 0;
  foo(bt == 0);
  return 0;
}

/* { dg-final { scan-tree-dump-times "foo \\(0\\)" 1 "evrp" } } */
