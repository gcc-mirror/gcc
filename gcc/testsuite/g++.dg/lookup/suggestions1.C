// { dg-do compile }

namespace N { namespace M { int foo; } } // { dg-message "'N::M::foo' declared here" }
int f (void) { return N::foo; }		 // { dg-error "'foo' is not a member of 'N'; did you mean 'N::M::foo'\\?" }

int g (void) { return ::foo; }	// { dg-error "'::foo' has not been declared; did you mean 'N::M::foo'\\?" }
