// PR c++/4381
// Test that exception-specs work properly for classes with virtual bases.

// Split into pieces for binary compatibility testing October 2002

extern void spec3_x (void);

int
main ()
{
  spec3_x ();
}
