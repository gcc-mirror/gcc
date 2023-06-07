// PR c++/4460
// Test that the cleanup for fully-constructed subobjects when a
// constructor throws gets the right address for a virtual base.

// Split into pieces for binary compatibility testing October 2002

// Explicit { dg-require-effective-target exceptions_enabled } so that dependent tests don't turn UNRESOLVED for '-fno-exceptions'.

extern void ctor2_x (void);

int main ()
{
  ctor2_x ();
}
