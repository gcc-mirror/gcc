/* { dg-options "-O3 -fdump-ipa-profile-details -fno-inline -fdump-tree-fixup_cfg3-details -fdump-tree-optimized-details" } */
char a[10000];
char b[10000];
int sz = 1000;

__attribute__((noipa))
     void test2 ()
{
  throw (sz);
}
void
test ()
{
  try
  {
    test2 ();
  }
  catch (int v)
  {
    __builtin_memcpy (b, a, v);
  }
}
int
main ()
{
  for (int i = 0; i < 100000; i++)
    test ();
}
/* { dg-final-use-not-autofdo { scan-ipa-dump-times "Average value sum:100000000" 2 "profile" } } */
/* 1 zero count for resx block.  */
/* { dg-final-use-not-autofdo { scan-tree-dump-times "count: 0" 1 "fixup_cfg3" } } */
/* 2 zero count for resx block and return block since return gets duplicated by tracer.  */
/* { dg-final-use-not-autofdo { scan-tree-dump-times "count: 0" 2 "optimized" } } */
/* { dg-final-use-not-autofdo { scan-tree-dump-times "Average value sum:100000000" 1 "optimized" } } */
