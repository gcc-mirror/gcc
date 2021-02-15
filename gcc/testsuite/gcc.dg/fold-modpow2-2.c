/* PR tree-optimization/99079 */
/* { dg-do compile { target { lp64 || ilp32 } } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

unsigned int
foo1 (unsigned int a, unsigned int b)
{
  return a % (1 << b);
}

int
foo2 (int b)
{
  return 371 % (1U << b);
}

long long
foo3 (int b)
{
  return 371LL % (1U << b);
}

unsigned long long
foo4 (unsigned long long a, int b)
{
  return a % (1U << b);
}

unsigned
foo5 (unsigned a, int b)
{
  return a % (unsigned) (1ULL << b);
}

int
foo6 (int b)
{
  return 371 % (int) (1ULL << b);
}

long long
foo7 (int b)
{
  return 371LL % (1 << b);
}

/* { dg-final { scan-tree-dump-not " % " "optimized" } } */
