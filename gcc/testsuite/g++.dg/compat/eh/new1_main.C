// PR c++/5757
// Test that when a constructor throws in a new-expression, we pass the
// right pointer to operator delete.

// Split into pieces for binary compatibility testing October 2002

extern void new1_x (void);

int
main ()
{
  new1_x ();
}
