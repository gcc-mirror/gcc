/* { dg-do compile } */
/* { dg-options "-O2 -fdisable-tree-evrp -fdump-tree-vrp1-details" } */

int foo(int i)
{
  if (i < 0 || i >= 5)
    return i == 1;
  return 1;
}

/* { dg-final { scan-tree-dump "Removing dead stmt \[^\r\n\]* = i_.* == 1" "vrp1" } } */
