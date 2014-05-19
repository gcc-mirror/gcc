/* { dg-do compile { target *-*-linux* *-*-gnu* } } */
/* { dg-options "-O2 -fdump-ipa-comdats"  } */
#include <stdio.h>
__attribute__ ((noinline))
static int q(void)
{
  return printf ("test");
}
inline int t(void)
{
  return q();
}
int (*f)()=t;
/* { dg-final { scan-ipa-dump-times "Localizing symbol" 1 "comdats"  } } */
/* { dg-final { cleanup-ipa-dump "comdats" } } */
