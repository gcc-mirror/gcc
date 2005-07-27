/* { dg-options "-O2 -fdump-tree-optimized -fdump-tree-tree_profile" } */
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
/* { dg-final-use { scan-tree-dump "Div.mod by constant n=257 transformation on insn" "tree_profile"} } */
/* { dg-final-use { scan-tree-dump "if \\(n != 257\\)" "optimized"} } */
/* { dg-final-use { scan-tree-dump-not "Invalid sum" "optimized"} } */
/* { dg-final-use { cleanup-tree-dump "optimized" } } */
/* { dg-final-use { cleanup-tree-dump "tree_profile" } } */
