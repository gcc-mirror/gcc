/* Confirm the ranger is picking up a relationship with equivalences.  */
/* { dg-do compile } */
/* { dg-options "-O2  -fdump-tree-evrp" } */

extern void foo ();

void f (unsigned int a, unsigned int b)
{
  if (a == b)
    for (unsigned i = 0; i < a; i++)
      if (i == b)   // Confirm  i < a also means i < b.
	foo (); /* Unreachable */
}

/* { dg-final { scan-tree-dump-times "foo\\(" 0 "evrp"} } */

