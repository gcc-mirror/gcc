/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-fno-fat-lto-objects" } { "" } } */
/* At -O1 DOM threads a jump in a non-optimal way which leads to
   the bogus propagation.  */
/* { dg-skip-if "" { *-*-* } { "-O1" } { "" } } */
/* { dg-options "-fdump-tree-ivcanon-details" } */

int a[199];

extern void abort (void);

int
main ()
{
  int i, x;
  for (i = 0; i < 199; i++)
    {
      x = a[i];
      if (x != i)
	abort ();
    }
  return 0;
}

/* Verify that we do not propagate the equivalence x == i into the
   induction variable increment.  */

/* { dg-final { scan-tree-dump "Added canonical iv" "ivcanon" } } */
/* { dg-final { cleanup-tree-dump "ivcanon" } } */
