// PR 3719
// Test that an unexpected handler can rethrow to categorize.

// Split into pieces for binary compatibility testing October 2002

extern void unexpected1_x ();

int
main ()
{
  unexpected1_x ();
}
