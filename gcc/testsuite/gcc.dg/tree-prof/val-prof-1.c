/* { dg-options "-O2 -fdump-tree-optimized -fdump-ipa-tree_profile_ipa" } */
int a[1000];
int b = 256;
int c = 257;
main ()
{
  int i;
  int n;
  for (i = 0; i < 1000; i++)
    {
      if (i % 17)
	n = c;
      else n = b;
      a[i] /= n;
    }
  return 0;
}
/* { dg-final-use { scan-ipa-dump "Div.mod by constant n_\[0-9\]*=257 transformation on insn" "tree_profile_ipa"} } */
/* { dg-final-use { scan-tree-dump "if \\(n_\[0-9\]* != 257\\)" "optimized"} } */
/* { dg-final-use { scan-tree-dump-not "Invalid sum" "optimized"} } */
/* { dg-final-use { cleanup-tree-dump "optimized" } } */
/* { dg-final-use { cleanup-ipa-dump "tree_profile_ipa" } } */
