// PR c++/48089
// { dg-do compile { target c++11 } }

// bang is ill-formed (diagnostic required) because its initializer is
// non-constant, because it uses the value of an uninitialized object.

// s() is ill-formed (no diagnostic required) because there is no set of
// arguments that would produce a constant expression.

// R() is well-formed because i is initialized before j.

struct s {
  constexpr s() : v(v) { }
  int v;
};

constexpr s bang;		// { dg-error "" }

struct R {
  int i,j;
  constexpr R() : i(42),j(i) { } // { dg-bogus "" }
};

constexpr R r;			// { dg-bogus "" }

// Ill-formed (no diagnostic required)
struct T {
  int i;
  constexpr int f() { return i; }
  constexpr T(): i(0) { }
  constexpr T(const T& t) : i(f()) { } // { dg-message "" }
};

constexpr T t1;
// Ill-formed (diagnostic required)
constexpr T t2(t1);		// { dg-message "" }

// Well-formed
struct U {
  int i, j;
  constexpr int f(int _i) { return _i; }
  constexpr int g() { return i; }
  constexpr U(): i(0), j(0) { }
  constexpr U(const U& t) : i(f(t.i)),j(0) { } // { dg-bogus "" }
  constexpr U(int _i) : i(_i),j(g()) { } // { dg-bogus "" }
};

constexpr U u1;
constexpr U u2(u1);		// { dg-bogus "" }
constexpr U u3(1);		// { dg-bogus "" }
