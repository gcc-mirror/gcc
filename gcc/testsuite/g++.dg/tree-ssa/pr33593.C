/* { dg-do compile } */
/* { dg-options "-O1 -fnon-call-exceptions -fdump-tree-optimized" } */

#include <stdio.h>

void foo (int) { printf ("Bar\n"); }

int
main (void)
{
  int a = 1 / 0;	// { dg-warning "division by zero" }
  printf ("Foo\n");
  foo (a);
}

// The expression 1 / 0 should not be propagated into the call to foo() if it
// may trap.
// { dg-final { scan-tree-dump-times "foo \\(1 \\/ 0\\)" 0 "optimized" } }
