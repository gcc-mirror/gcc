// Build don't link:

void f ();
void (&fr)() = f;		// gets bogus error - references to functions XFAIL *-*-*
