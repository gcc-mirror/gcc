/* { dg-do compile } */
/* { dg-options "-O2 -fdisable-tree-evrp -fdisable-tree-ethread -fdump-tree-rvrp-details" } */

extern int global;
extern int global2;
extern int global3;

void foo (int base)
{
  unsigned i;

  // rvrp should be able to remove the (i > 123) comparison
  for (i = base; i < 10; i++)
    if (i > 123)
      return;
}

/* { dg-final { scan-tree-dump-times "Branch rewritten" 1 "rvrp"} } */
