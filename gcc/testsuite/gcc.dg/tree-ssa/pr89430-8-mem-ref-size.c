/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cselim-details" } */

int *t;

int f1 (int tt)
{
  int *t1 = t;
  *t1 = -5;
  if (*t1 < tt)
    *((unsigned *) t1) = 5;
  return *t1;
}

/* { dg-final { scan-tree-dump "Conditional store replacement" "cselim" } } */
