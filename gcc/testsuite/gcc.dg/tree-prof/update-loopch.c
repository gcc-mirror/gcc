/* { dg-options "-O2 -fdump-ipa-profile-blocks-details -fdump-tree-switchlower-blocks-details" } */
int max = 33333;
int a[8];
int
main ()
{
  int i;
  for (i = 0; i < max; i++)
    {
      a[i % 8]++;
    }
  return 0;
}
/* Loop header copying will peel away the initial conditional, so the loop body
   is once reached directly from entry point of function, rest via loopback
   edge.  */
/* autofdo cannot do that precise counts */
/* { dg-final-use-not-autofdo { scan-ipa-dump "loop depth 1, count 33334" "profile"} } */
/* { dg-final-use-not-autofdo { scan-tree-dump "loop depth 1, count 33333" "switchlower"} } */
/* { dg-final-use-not-autofdo { scan-tree-dump-not "loop depth 1, count 33332" "switchlower"} } */
/* { dg-final-use-not-autofdo { scan-tree-dump "Removing basic block" "switchlower"} } */
/* { dg-final-use { scan-tree-dump-not "Invalid sum" "switchlower"} } */
