// Test that terminate gets run when a catch filter fails to match while
// running destructors.  Original bug depended on a::~a being inlined.

// Split into pieces for binary compatibility testing October 2002

extern void filter2_x (void);

int
main ()
{
  filter2_x ();
}
