/* { dg-options "-O2 -fdump-tree-optimized -fdump-ipa-tree_profile_ipa" } */
int a[1000];
int b=997;
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
/* { dg-final-use { scan-ipa-dump "Div.mod by constant b.*=997 transformation on insn" "tree_profile_ipa" } } */
/* { dg-final-use { scan-tree-dump-not "Invalid sum" "optimized"} } */
/* { dg-final-use { cleanup-tree-dump "optimized" } } */
/* { dg-final-use { cleanup-ipa-dump "tree_profile_ipa" } } */
