/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf-optimized"  } */

__attribute__ ((noinline))
int zero()
{
  return 0;
}

__attribute__ ((noinline))
int nula()
{
  return 0;
}

__attribute__ ((noinline))
int foo()
{
  return zero();
}

__attribute__ ((noinline))
int bar()
{
  return nula();
}

int main()
{
  return foo() + bar();
}

/* { dg-final { scan-ipa-dump "Semantic equality hit:.*bar.*->.*foo.*" "icf"  } } */
/* { dg-final { scan-ipa-dump "Semantic equality hit:.*nula.*->.*zero.*" "icf"  } } */
/* { dg-final { scan-ipa-dump "Equal symbols: 2" "icf"  } } */
