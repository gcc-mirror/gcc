// { dg-do compile { target c++11 } }
// PR c++/87155 confused about which anon namespace

namespace {
  void a (); // this one
}

inline namespace n2 {
	 namespace {}
} 

namespace {
  int a ();  // { dg-error "ambiguating" "" }
}
