// PR c++/5757
// Test that when a constructor throws in a new-expression, we pass the
// right pointer to operator delete.

// Split into pieces for binary compatibility testing October 2002

// Explicit { dg-require-effective-target exceptions_enabled } so that dependent tests don't turn UNRESOLVED for '-fno-exceptions'.

extern void new1_x (void);

int
main ()
{
  new1_x ();
}
