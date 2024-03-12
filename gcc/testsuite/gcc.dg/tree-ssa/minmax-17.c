/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-phiopt1-details" } */

static inline unsigned long long g(int t)
{
  unsigned t1 = t;
  return t1;
}
unsigned long long test_max(int c, int d, int e)
{
  unsigned long long t;
  if (c > d)
    t = g(c);
  else
    t = g(d);
  return t;
}

/* We should figure out that test_max has an MAX_EXPR in it. */
/* { dg-final { scan-tree-dump " = MAX_EXPR" "phiopt1"} } */
/* { dg-final { scan-tree-dump-times "changed to factor operation out from" 2 "phiopt1"} } */
