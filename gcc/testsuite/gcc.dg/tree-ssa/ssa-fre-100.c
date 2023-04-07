/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1" } */

float bar, baz;
void foo (int *p, int n)
{
  *p = 0;
  do
    {
      bar = 1.;
      /* When iterating we should have optimistically value-numbered
	 *p to zero, on the second iteration we have to prove the
	 store below does not affect the value of this load though.
	 We can compare the stored value against the value from the
	 previous iteration instead relying on a non-walking lookup.  */
      if (*p)
        {
          baz = 2.;
          *p = 0;
        }
    }
  while (--n);
}

/* { dg-final { scan-tree-dump-not "baz" "fre1" { xfail { ! natural_alignment_32 } } } } */
