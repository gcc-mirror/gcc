/* { dg-options "-O3 -fdump-ipa-cp" } */
__attribute__ ((used))
int a[1000];

__attribute__ ((noinline))
void
test2(int sz)
{
  for (int i = 0; i < sz; i++)
	  a[i]++;
  asm volatile (""::"m"(a));
}

__attribute__ ((noinline))
void
test1 (int sz)
{
  for (int i = 0; i < 1000; i++)
	  test2(sz);
}
int main()
{
	test1(1000);
	return 0;
}
/* We should clone test1 and test2 for constant 1000.
   In the past we did not do this since we did not clone for edges that are not hot
   and call main->test1 is not considered hot since it is executed just once.  */
/* { dg-final { scan-ipa-dump-times "Creating a specialized node of test1" 1 "cp"} } */
/* { dg-final { scan-ipa-dump-times "Creating a specialized node of test2" 1 "cp"} } */
