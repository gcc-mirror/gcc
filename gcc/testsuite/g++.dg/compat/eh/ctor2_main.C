// PR c++/4460
// Test that the cleanup for fully-constructed subobjects when a
// constructor throws gets the right address for a virtual base.

// Split into pieces for binary compatibility testing October 2002

extern void ctor2_x (void);

int main ()
{
  ctor2_x ();
}
