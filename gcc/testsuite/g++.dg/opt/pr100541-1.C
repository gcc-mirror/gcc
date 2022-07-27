// PR debug/100541
// { dg-do compile }
// { dg-options "-O3 -fno-expensive-optimizations -fno-tree-dce -fno-tree-dominator-opts -fcompare-debug" }

int a, b, i, x, y;

int
foo ()
{
  int rcmd = 0;
  switch (x) {
  case 0:
    if (i)
      rcmd = 6;
    if (y % 3)
      int &m1 = rcmd = rcmd | 5;
    break;
  case 1:
    rcmd = b;
  }
  if (rcmd != 7)
    return rcmd;
  return a;
}
