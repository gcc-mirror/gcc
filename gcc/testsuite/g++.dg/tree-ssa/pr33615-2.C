/* { dg-do compile } */
/* { dg-options "-O2 -fnon-call-exceptions -fdump-tree-pre-details -w" } */

extern volatile int y;

double
foo (double a, int x)
{
  while (x--)
    {
      y++;
      a += 1.0 / 0.0;
    }
  return a;
}

// The expression 1.0 / 0.0 should not be treated as a loop invariant
// if it may throw an exception.
// { dg-final { scan-tree-dump-times "Replaced 1\\\.0e\\\+0 / 0\\\.0" 0 "pre" } }
