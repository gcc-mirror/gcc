/* { dg-options "-O2 -fdump-tree-optimized -fdump-ipa-profile" } */
int a[1000];
int b=997;
int
main()
{
	int i;
	for (i = 0; i < 1000; i++)
		if (a[i])
			a[i]/=b;
		else
			a[i]/=b;
	return 0;
}
/* autofdo does not do value profiling so far */
/* { dg-final-use-not-autofdo { scan-ipa-dump "Div.mod by constant b.*=997 transformation on insn" "profile" } } */
/* { dg-final-use { scan-tree-dump-not "Invalid sum" "optimized"} } */
