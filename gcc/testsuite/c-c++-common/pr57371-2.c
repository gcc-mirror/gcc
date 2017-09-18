/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

/* We can not get rid of comparison in tests below because of
   potential inexact exception.

   TODO: enable when -fno-trapping-math.  */

int foo1(int x) {
  return (float) x != 0;
  /* { dg-final { scan-tree-dump "\\(float\\)" "optimized" } } */
}

int foo2(long long x) {
  /* { dg-final { scan-tree-dump "\\(double\\)" "optimized" } } */
  return (double) x != 0;
}
