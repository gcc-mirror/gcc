// { dg-do assemble  }
// Bug; g++ binds a function definition to the line number of a later decl.

void foo () { }			// { dg-error "" } redeclared
void foo ();			// { dg-bogus "" } invalid binding
void foo () { }			// { dg-error "" } redeclared
