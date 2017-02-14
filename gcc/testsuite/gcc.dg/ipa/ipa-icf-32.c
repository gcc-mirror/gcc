/* { dg-do run } */
/* { dg-options "-O1 -fipa-icf -fdump-ipa-icf-details" } */

int
__attribute__((optimize("Os"), noinline, noclone))
foo(int a)
{
  return a * a;
}

__attribute__ ((noinline, noclone))
int bar(int b)
{
  return b * b;
}

int main()
{
  return foo (0) + bar (0);
}

/* { dg-final { scan-ipa-dump "Equal symbols: 0" "icf"  } } */
