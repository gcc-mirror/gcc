/* { dg-options "-O2 -fdump-ipa-profile-blocks-details -fdump-tree-optimized-blocks-details" } */
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
/* { dg-final-use { scan-ipa-dump "loop depth 1, count 33334" "profile"} } */
/* { dg-final-use { scan-tree-dump "loop depth 1, count 33332" "optimized"} } */
/* { dg-final-use { scan-tree-dump-not "Invalid sum" "optimized"} } */
/* { dg-final-use { cleanup-ipa-dump "profile" } } */
/* { dg-final-use { cleanup-tree-dump "optimized" } } */
