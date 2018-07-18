/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-forwprop1-raw" } */

unsigned int
f1 (unsigned int a, unsigned int b)
{
  unsigned int x = 1U << b;
  return a / x;
}

unsigned long
f2 (unsigned long a, int b)
{
  unsigned long x = 1UL << b;
  return a / x;
}

unsigned long long
f3 (unsigned long long a, int b)
{
  unsigned long long x = 1ULL << b;
  return a / x;
}

/* { dg-final { scan-tree-dump-not "trunc_div_expr" "forwprop1" } } */
