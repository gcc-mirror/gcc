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
	int (*ret)(int) = p->ret;
	for (int pos = 0; pos < 1000; pos++)
	  ret(pos);
	if (s)
		__builtin_printf ("sum error\n");
}
int main()
{
	for (int i = 0; i < 10000; i++)
	{
		struct wrapptr p={reta};



		test(&p);
	}
	return 0;
}
/* { dg-final-use-autofdo { scan-tree-dump "Inlining using auto-profile test" "einline"} } */
/* { dg-final-use-autofdo { scan-tree-dump "Checking indirect call -> direct call ret_" "einline"} } */
/* { dg-final-use-autofdo { scan-tree-dump "looks good" "einline"} } */
/* If we inlined reta->test->main, it will contian array[pos].  */
/* { dg-final-use-autofdo { scan-tree-dump "array.pos_" "einline"} } */
