// Submitted by Jason Merrill <jason_merrill@redhat.com>
// Test for proper handling of local static references.
// Split into pieces for binary compatibility testing October 2002

extern void init_ref2_x (void);

int
main ()
{
  init_ref2_x ();
}
