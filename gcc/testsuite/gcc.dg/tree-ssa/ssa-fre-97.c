/* { dg-do compile } */
/* ethread threading does not yet catch this but it might at some point.  */
/* { dg-options "-O -fdump-tree-fre1-details -fno-thread-jumps" } */

int foo (int b, int x)
{
  int i, j;
  if (b)
    i = x;
  if (b)
    j = x;
  return j == i;
}

/* Even with different undefs we should CSE a PHI node with the
   same controlling condition.  */

/* { dg-final { scan-tree-dump "Replaced redundant PHI node" "fre1" } } */
/* { dg-final { scan-tree-dump "return 1;" "fre1" } } */
