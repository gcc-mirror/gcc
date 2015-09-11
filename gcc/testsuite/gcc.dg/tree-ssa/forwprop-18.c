/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-cddce1" } */

signed char f1(signed char n)
{
  return (long double)n;
}
unsigned long long f2(signed char n)
{
  return (long double)n;
}

unsigned long long g1(unsigned long long n)
{
  return (float)n;
}
signed char g2(unsigned long long n)
{
  return (float)n;
}

/* { dg-final { scan-tree-dump-times "\\\(float\\\)" 2 "cddce1" } } */
/* { dg-final { scan-tree-dump-not "\\\(long double\\\)" "cddce1" } } */
