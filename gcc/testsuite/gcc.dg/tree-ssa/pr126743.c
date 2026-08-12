/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

unsigned int
or_eq_zero (int a, int b)
{
  if (a == b)
    __builtin_unreachable ();
  // return 0;
  return (a | b) == 0;
}

unsigned int
or_ne_zero (int a, int b)
{
  if (a == b)
    __builtin_unreachable ();
  // return 1;
  return (a | b) != 0;
}

/* { dg-final { scan-tree-dump-times "return 0;" 1 "evrp" } } */
/* { dg-final { scan-tree-dump-times "return 1;" 1 "evrp" } } */
