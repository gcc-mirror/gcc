/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int
f (int m1, int m2, int c)
{
  int d = m1 == m2;
  int e = d * c;
  return e;
}

/* { dg-final { scan-tree-dump-times "\\? c_\[0-9\]\\(D\\) : 0" 1 "optimized" } } */
