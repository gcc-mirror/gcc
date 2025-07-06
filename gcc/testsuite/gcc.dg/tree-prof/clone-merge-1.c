/* { dg-options "-O3 -fno-early-inlining -fdump-ipa-afdo_offline-all" } */
/* { dg-require-profiling "-fauto-profile" } */ 

__attribute__ ((used))
int a[1000];

__attribute__ ((noinline))
void
test2(int sz)
{
  a[sz]++;
  asm volatile (""::"m"(a));
}

__attribute__ ((noinline))
void
test1 (int sz)
{
  for (int i = 0; i < 1000; i++)
    if (i % 2)
      test2 (sz);
    else
      test2 (i);

}
int main()
{
  for (int i = 0; i < 1000; i++)
    test1 (1000);
  return 0;
}
/* We will have profiles for test2 and test2.constprop.0 that will have to be
   merged,  */
/* { dg-final-use-autofdo { scan-ipa-dump "Merging duplicate instance: test2" "afdo_offline"} } */
