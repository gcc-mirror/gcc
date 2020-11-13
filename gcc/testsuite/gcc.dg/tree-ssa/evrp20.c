/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

void call (void);

void foo (int base)
{
  unsigned i;

  // Ranger should be able to remove the (i > 123) comparison.
  for (i = base; i < 10; i++)
    if (i > 123)
      {
        call ();
	return;
      }
}

/* { dg-final { scan-tree-dump-not "call" "evrp"} } */
