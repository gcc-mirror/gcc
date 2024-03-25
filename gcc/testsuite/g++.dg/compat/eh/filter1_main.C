// Test that cleanups get run when a catch filter fails to match.

// Split into pieces for binary compatibility testing October 2002

// Explicit { dg-require-effective-target exceptions_enabled } so that dependent tests don't turn UNRESOLVED for '-fno-exceptions'.

extern void filter1_x (void);

int
main ()
{
  filter1_x ();
}
