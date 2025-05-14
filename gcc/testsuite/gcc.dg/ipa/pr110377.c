/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-cp" } */
int test3(int);
__attribute__ ((noinline))
void test2(int a)
{
	test3(a);
}
void
test(int n)
{
        if (n > 5)
          __builtin_unreachable ();
        test2(n);
}
/* { dg-final { scan-ipa-dump "-INF, 5" "cp" } }  */
