// PR c++/3948
// Test that the destructor call for a value parameter gets the
// right address.
// Split into pieces for binary compatibility testing October 2002

extern void byval1_x (void);

int
main ()
{
  byval1_x ();
}
