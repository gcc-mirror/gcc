// PR c++/46538
// { dg-do compile }
// { dg-options "" }

S () : str(__PRETTY_FUNCTION__) { return S(); }	// { dg-error "forbids declaration" "decl" }
// { dg-error "only constructors" "constructor" { target *-*-* } .-1 }
