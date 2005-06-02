/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp" } */

int
foo (int *p)
{
  int a = *p;
  int b = p != 0;

  *p = b;

  if (b)
    return a;
  else
    return 0;
}

/* { dg-final { scan-tree-dump-times "Folding predicate " 2 "vrp" } } */
/* { dg-final { cleanup-tree-dump "vrp" } } */
