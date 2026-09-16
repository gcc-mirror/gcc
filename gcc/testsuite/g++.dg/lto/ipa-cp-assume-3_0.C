/* { dg-lto-do link } */
/* { dg-lto-options  {{ -O3 -fdump-ipa-cp-details -fdump-ipa-sra-details -fdump-tree-optimized -flto }} } */

#include "../ipa/ipa-cp-assume-3.C"

int
main ()
{
  int x;
  __builtin_scanf ("%d", &x);
  baz (x);
  return 0;
}

/* { dg-final { scan-wpa-ipa-dump "Aggregate replacements: 0\\\[4]=42" "cp" } } */
/* { dg-final { scan-wpa-ipa-dump ".*assume.*constprop.* Will split parameter 0" "sra" } } */
/* { dg-final { scan-wpa-ipa-dump "assume.*constprop\\.isra" "sra" } } */
/* { dg-final { scan-ltrans-tree-dump-not "abort" "optimized" } } */
