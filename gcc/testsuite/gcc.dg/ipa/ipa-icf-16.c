/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf"  } */

#include <stdio.h>

__attribute__ ((noinline))
int foo()
{
  printf ("Hello world.\n");
  return 0;
}

__attribute__ ((noinline))
int bar()
{
  printf ("Hello world.\n");
  return 0;
}

int main()
{
  return foo() + bar();
}

/* { dg-final { scan-ipa-dump "Semantic equality hit:foo->bar" "icf"  } } */
/* { dg-final { scan-ipa-dump "Equal symbols: 1" "icf"  } } */
