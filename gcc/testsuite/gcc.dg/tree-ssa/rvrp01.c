/* Copy of pr21001.c for testing VRP adjusted for RVRP
 *
/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-fre -fdisable-tree-ethread -fdump-tree-rvrp-details" } */

int
foo (int a)
{
  int b = a != 0;
  if (b)
    if (a != 0)
      return 1;
  return 0;
}

/* { dg-final { scan-tree-dump-times "Branch rewritten" 1 "rvrp"} } */
