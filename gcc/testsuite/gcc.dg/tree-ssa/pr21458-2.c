/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp-details -fdisable-tree-ethread" } */

extern void g (void);
extern void bar (int);

int
foo (int a)
{
  int i;

  for (i = 1; i < 100; i++)
    {
      if (i)
	g ();
    }
}

/* { dg-final { scan-tree-dump-times "Folding predicate.* to 1" 1 "evrp" } } */
