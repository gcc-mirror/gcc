// Bug; g++ binds a function definition to the line number of a later decl.
// Build don't link:

void foo () { }			// ERROR - redeclared
void foo ();			// gets bogus error - invalid binding
void foo () { }			// ERROR - redeclared
