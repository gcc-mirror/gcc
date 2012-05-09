/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-forwprop1" } */

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

/* { dg-final { scan-tree-dump-times "\\\(float\\\)" 2 "forwprop1" } } */
/* { dg-final { scan-tree-dump-not "\\\(long double\\\)" "forwprop1" } } */
/* { dg-final { cleanup-tree-dump "forwprop1" } } */
