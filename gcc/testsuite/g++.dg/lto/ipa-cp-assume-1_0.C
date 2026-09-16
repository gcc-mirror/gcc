/* { dg-lto-do link } */
/* { dg-lto-options {{ -O3 -fdump-ipa-cp-details -fdump-tree-optimized -flto }} } */

#include "../ipa/ipa-cp-assume.C"

int
main ()
{
  int x;
  __builtin_scanf ("%d", &x);
  foo (x);
  return 0;
}

/* { dg-final { scan-wpa-ipa-dump "replacing param #1 int with const 42" "cp" } } */
/* { dg-final { scan-ltrans-tree-dump-not "abort" "optimized" } } */
