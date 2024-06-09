// PR 3719
// Test that an unexpected handler can rethrow to categorize.

// Split into pieces for binary compatibility testing October 2002

// Explicit { dg-require-effective-target exceptions_enabled } so that dependent tests don't turn UNRESOLVED for '-fno-exceptions'.

extern void unexpected1_x ();

int
main ()
{
  unexpected1_x ();
}
