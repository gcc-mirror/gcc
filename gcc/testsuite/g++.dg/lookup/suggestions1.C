// { dg-do compile }

namespace N { namespace M { int foo; } } // { dg-message "N::M::foo" }
int f (void) { return N::foo; }		 // { dg-error "not a member" }
// { dg-message "suggested alternative" "missing namespace" { target *-*-* } 4 }

int g (void) { return ::foo; }	// { dg-error "not been declared" }
// { dg-message "suggested alternative" "omitted namespace" { target *-*-* } 7 }
