/* copied from pr68431.c and adjusted for rvrp.
   PR tree-optimization/68431 */
/* { dg-options "-O2 -fdisable-tree-evrp -fdump-tree-rvrp-details" } */

unsigned int x = 1;
int
main (void)
{
  long long int a = -2LL;
  int t = 1 <= (a / x);
  if (t != 0)
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "Branch rewritten" 1 "rvrp" } } */
