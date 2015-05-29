/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-expand-details" } */

float total = 0.2;
void foo(int n)
{
  int i;
  for (i = 0; i < n; i++)
    total += i;
}

/* Verify that out-of-ssa coalescing did its job by verifying there are not
   any partition copies inserted.  */

/* { dg-final { scan-rtl-dump-not "partition copy" "expand"} } */
