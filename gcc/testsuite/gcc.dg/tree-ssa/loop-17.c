/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-sccp-details" } */

/* To determine the number of iterations in this loop we need to fold
   p_4 + 4B > p_4 + 8B to false.  This transformation has caused
   troubles in the past due to overflow issues.  */

int foo (int *p)
{
  int i = 0, *x;

  for (x = p; x < p + 2; x++)
    i++;

  return i;
}

/* { dg-final { scan-tree-dump "set_nb_iterations_in_loop = 1" "sccp" } } */
/* { dg-final { cleanup-tree-dump "sccp" } } */
