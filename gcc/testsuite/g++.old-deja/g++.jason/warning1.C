// Bug: g++ protests that foo was never defined.
// Build don't link:

static void foo ();
static void foo ();
static void foo () { }
void bar () { foo(); }		// gets bogus error - 
