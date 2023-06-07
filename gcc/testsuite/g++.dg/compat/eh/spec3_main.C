// PR c++/4381
// Test that exception-specs work properly for classes with virtual bases.

// Split into pieces for binary compatibility testing October 2002

// Explicit { dg-require-effective-target exceptions_enabled } so that dependent tests don't turn UNRESOLVED for '-fno-exceptions'.

extern void spec3_x (void);

int
main ()
{
  spec3_x ();
}
