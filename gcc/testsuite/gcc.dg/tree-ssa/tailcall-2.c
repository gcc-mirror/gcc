/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-tailc-details" } */
/* Test provided by Richard Earnshaw in PR 14312.  */

void bar (int i);
void baz (int *);

void
foo (int *x)
{
  if (*x < 0)
    {
      baz (x);
      return;
    }
  bar (*x);
}

/* The test has no local call-clobbered variables.  Only the memory
   tag for 'x' is call-clobbered.  And since tags are not real
   variables, they ought to be ignored.  There should be two tail
   calls here.  */
/* { dg-final { scan-tree-dump-times "Found tail call" 2 "tailc"} } */
/* { dg-final { cleanup-tree-dump "tailc" } } */
