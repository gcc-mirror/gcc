// Test that terminate gets run when a catch filter fails to match while
// running destructors.  Original bug depended on a::~a being inlined.

// Split into pieces for binary compatibility testing October 2002

// Explicit { dg-require-effective-target exceptions_enabled } so that dependent tests don't turn UNRESOLVED for '-fno-exceptions'.

extern void filter2_x (void);

int
main ()
{
  filter2_x ();
}
