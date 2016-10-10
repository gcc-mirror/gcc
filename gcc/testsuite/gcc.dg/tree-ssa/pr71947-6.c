/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-vrp -fdump-tree-dom-details" } */


int f(int x, int y, int a, int b)
{
  int ret = 10;
  if (a == x
      && b == y
      && a == b)
    ret = x - y;

  return ret;
}

/* { dg-final { scan-tree-dump "Folded to: ret_\[0-9\]+ = 0;"  "dom2" { xfail *-*-* } } } */
