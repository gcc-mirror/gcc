// PR c++/84707
// { dg-do compile { target c++11 } }

inline namespace {
	 namespace {} // not this one
	 void a ();
}

namespace {
  int a (); // { dg-error "ambiguating" "" }
}
