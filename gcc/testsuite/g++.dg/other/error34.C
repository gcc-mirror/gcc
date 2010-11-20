// PR c++/46538
// { dg-do compile }
// { dg-options "" }

S () : str(__PRETTY_FUNCTION__) {}	// { dg-error "forbids declaration" }
// { dg-error "only constructors" "" { target *-*-* } 5 }
