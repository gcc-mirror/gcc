/* { dg-options "-O2 -fdump-tree-optimized -fdump-ipa-profile" } */
int a[1000];
int b = 256;
int c = 257;
int
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
/* autofdo does not do value profiling so far */
/* { dg-final-use-not-autofdo { scan-ipa-dump "Transformation done: div.mod by constant 257" "profile"} } */
/* { dg-final-use-not-autofdo { scan-tree-dump "if \\(n_\[0-9\]* != 257\\)" "optimized"} } */
/* { dg-final-use { scan-tree-dump-not "Invalid sum" "optimized"} } */
