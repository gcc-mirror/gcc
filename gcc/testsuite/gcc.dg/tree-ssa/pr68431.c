/* PR tree-optimization/68431 */
/* { dg-options "-O2 -fdisable-tree-evrp -fdump-tree-vrp1-details" } */

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

/* { dg-final { scan-tree-dump-times "Folding predicate .*to 0" 1 "vrp1" } } */
