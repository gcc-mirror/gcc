// Test whether exception specifier dependent on template parameter
// is accepted during template decl processing.

// Split into pieces for binary compatibility testing October 2002

extern void template1_x (void);

int
main ()
{
  template1_x ();
}
