/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

int foo(int i)
{
  if (i < 0 || i >= 5)
    return i == 1;
  return 1;
}

/* { dg-final { scan-tree-dump "Folding predicate i_.* == 1 to 0" "vrp1" } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
