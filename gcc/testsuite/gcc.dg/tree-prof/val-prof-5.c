/* { dg-options "-O2 -fdump-tree-optimized -fdump-ipa-profile" } */
int a[1000];
int b=997;
int
main()
{
	int i;
	for (i = 0; i < 1000; i++)
		if (a[i] != 1)
			a[i]/=b;
		else
			a[i]/=b;
	return 0;
}
/* autofdo does not do value profiling so far */
/* { dg-final-use-not-autofdo { scan-ipa-dump "Transformation done: div.mod by constant 997" "profile" } } */
/* { dg-final-use { scan-tree-dump-not "Invalid sum" "optimized"} } */
