/* { dg-require-effective-target lto } */
/* { dg-additional-sources "afdo-crossmodule-1b.c" } */
/* { dg-options "-O3 -flto -fdump-ipa-afdo_offline -fdump-tree-einline-details" } */
/* { dg-require-profiling "-fauto-profile" } */ 
volatile int c;

int foo2 ()
{
	c++;
	return 1;
}
int foo (int (*fooptr) ())
{
	return fooptr ();
}
extern int bar (int (*fooptr) (int (*)()));
	     
int
main()
{
	int n = 1000000;
	int s = 0;
	for (int i = 0; i < n; i++)
		s += bar (foo);
	return n != s;
}
/* { dg-final-use-autofdo { scan-ipa-dump "Removing external inline: main:5 bar" "afdo_offline"} } */
/* { dg-final-use-autofdo { scan-ipa-dump "Offlining function inlined to other module: bar:2 foo" "afdo_offline"} } */
/* { dg-final-use-autofdo { scan-tree-dump "Indirect call -> speculative call foo.. => foo2" "einline"} } */
