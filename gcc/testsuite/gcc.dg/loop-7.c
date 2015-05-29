/* PR rtl-optimization/31360  */

/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-O1 -fdump-rtl-loop2_invariant" } */

void f(int *a)
{
  int i;
  for (i = 0;i<100;i++)
    a[i] = 0;
}

/* Load of 0 is moved out of the loop.  */
/* { dg-final { scan-rtl-dump-times "Decided" 1 "loop2_invariant" } } */

