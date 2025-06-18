/* { dg-options "-O2 -fdump-ipa-afdo-details -fdump-tree-einline-details --param early-inlining-insns=1" } */
/* { dg-require-profiling "-fauto-profile" } */ 

volatile int array[1000];
int reta (int i)
{
	return array[i];
}
struct wrapptr 
{
	int (*ret)(int);
};
int test (struct wrapptr *p)
{
	int s = 0;
	for (int pos = 0; pos < 1000; pos++)
	  s+p->ret(pos);
	if (s)
		__builtin_printf ("sum error\n");
}
int main()
{
	struct wrapptr p={reta};
	for (int i = 0; i < 10000; i++)
		test(&p);
	return 0;
}
/* { dg-final-use-autofdo { scan-tree-dump "Inlining using auto-profile test" "einline"} } */
/* { dg-final-use-autofdo { scan-ipa-dump "Checking indirect call -> direct call reta" "afdo"} } */
/* { dg-final-use-autofdo { scan-ipa-dump-times "looks good" 0 "afdo"} } */
/* If we inlined reta->test->main, it will contian array[pos].  */
/* { dg-final-use-autofdo { scan-ipa-dump "array.pos_" "afdo"} } */
