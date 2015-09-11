/* { dg-do compile } */
/* { dg-options "-O0 -fipa-icf -fdump-ipa-icf"  } */

#include <stdio.h>

static int
__attribute__ ((no_icf))
foo()
{
  return 2;
}

static int
__attribute__ ((no_icf))
bar()
{
  return 2;
}

int main()
{
  return foo() - bar();
}

/* { dg-final { scan-ipa-dump "Equal symbols: 1" "icf"  } } */
