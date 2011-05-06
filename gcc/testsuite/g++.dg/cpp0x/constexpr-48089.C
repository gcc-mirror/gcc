// PR c++/48089
// { dg-options -std=c++0x }

// bang is ill-formed (diagnostic required) because its initializer is
// non-constant, because it uses the value of an uninitialized object.

// s() is ill-formed (no diagnostic required) because there is no set of
// arguments that would produce a constant expression.

// R() is well-formed because i is initialized before j.

struct s {
  constexpr s() : v(v) { }	// { dg-message "" }
  int v;
};

constexpr s bang;		// { dg-error "" }

struct R {
  int i,j;
  constexpr R() : i(42),j(i) { } // { dg-bogus "" "" { xfail *-*-* } }
};

constexpr R r;			// { dg-bogus "" "" { xfail *-*-* } }
