/* { dg-options "-O2 -fdump-tree-einline-details --param early-inlining-insns=1" } */
/* { dg-require-profiling "-fauto-profile" } */ 
volatile int a[1000];

#define STR1(X) #X
#define STR2(X) STR1(X)

int reta (int i)
asm(STR2(__USER_LABEL_PREFIX__) "renamed_reta");
int test ()
asm(STR2(__USER_LABEL_PREFIX__) "renamed_test");

int reta (int i)
{
	if (a[i])
		__builtin_printf ("It is one\n");
	if (a[i] == 2)
		__builtin_printf ("It is two\n");
	return a[i];
}
int test ()
{
	int s = 0;
	for (int pos = 0; pos < 1000; pos++)
	  reta(pos);
	if (s)
		__builtin_printf ("sum error\n");
}
int main()
{
	for (int i = 0; i < 10000; i++)
		test();
	return 0;
}
/* { dg-final-use-autofdo { scan-tree-dump "Inlining using auto-profile test" "einline"} } */
/* { dg-final-use-autofdo { scan-tree-dump "Inlining using auto-profile reta.*transitively inlined to main" "einline"} } */
