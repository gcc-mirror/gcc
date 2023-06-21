/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-phiopt1-details" } */

static inline unsigned long long g(int t)
{
  unsigned t1 = t;
  return t1;
}
static inline int abs1(int a)
{
  if (a < 0)
    a = -a;
  return a;
}
unsigned long long f(int c, int d, int e)
{
  unsigned long long t;
  if (d > e)
    t = g(abs1(d));
  else
    t = g(abs1(e));
  return t;
}

/* { dg-final { scan-tree-dump " = MAX_EXPR" "phiopt1"} } */
/* { dg-final { scan-tree-dump-times " = ABS_EXPR" 2 "phiopt1"} } */
/* { dg-final { scan-tree-dump-times "changed to factor operation out from" 3 "phiopt1"} } */
