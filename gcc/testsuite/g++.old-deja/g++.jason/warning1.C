// { dg-do assemble  }
// Bug: g++ protests that foo was never defined.

static void foo ();
static void foo ();
static void foo () { }
void bar () { foo(); }		// { dg-bogus "" } 
