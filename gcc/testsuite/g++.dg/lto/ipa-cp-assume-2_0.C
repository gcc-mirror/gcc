/* { dg-lto-do link } */
/* { dg-lto-options {{ -O3 -fdump-ipa-cp-details -fdump-tree-optimized -flto }} } */

#include "../ipa/ipa-cp-assume-2.C"

int
main ()
{
  int x;
  __builtin_scanf ("%d", &x);
  baz (x);
  return 0;
}

/* { dg-final { scan-wpa-ipa-dump-times "replacing param #1 int with const 42" 2 "cp" } } */
/* { dg-final { scan-ltrans-tree-dump-not "abort" "optimized" } } */
