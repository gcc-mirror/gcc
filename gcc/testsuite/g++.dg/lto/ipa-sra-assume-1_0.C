/* { dg-lto-do link } */
/* { dg-lto-options {{ -O2 -fno-inline -fipa-sra -fdump-ipa-sra-details -fdump-tree-optimized -flto }} } */

#include "../ipa/ipa-sra-assume.C"

int
main ()
{
  int x, y, z;
  __builtin_scanf ("%d %d %d", &x, &y, &z);

  struct S s = {.x = (bool) x, .y = y};
  bool a = z;

  split_struct (s);
  remove_ptr (&a);
  two_values (s);
  return 0;
}

/* { dg-final { scan-wpa-ipa-dump-times "Will split parameter 0" 6 "sra" } } */
/* { dg-final { scan-ltrans-tree-dump-not "abort" "optimized" } } */
