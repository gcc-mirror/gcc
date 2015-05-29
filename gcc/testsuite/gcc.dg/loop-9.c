/* { dg-do compile } */
/* { dg-options "-O1 -fdump-rtl-loop2_invariant" } */

void
f (double *a)
{
  int i;
  for (i = 0; i < 100; i++)
    a[i] = 18.4242;
}

/* Load of x is moved out of the loop.  */
/* { dg-final { scan-rtl-dump "Decided" "loop2_invariant" } } */
/* { dg-final { scan-rtl-dump "without introducing a new temporary register" "loop2_invariant" } } */

