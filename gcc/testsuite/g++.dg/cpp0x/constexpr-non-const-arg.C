// Example from issue 1125 drafting; D() and v were well-formed with the
// wording approved in Rapperswil, now seems they should be ill-formed.
// { dg-do compile { target c++11 } }

struct B {
  constexpr B(int x) : i(0) { }    // "x" is unused
  int i;
};

int global;			// { dg-message "not const" }

struct D : B {
  constexpr D() : B(global) { }   // { dg-error "global|argument" "" { target c++20_down } }
};

struct A2 {
  constexpr A2(bool b, int x) : m(b ? 42 : x) { }
  int m;
};

// ok, constructor call initializes m with the value 42 after substitution
constexpr int v = A2(true, global).m; // { dg-error "global" }
// error: initializer for m is "x", which is non-constant
constexpr int w = A2(false, global).m; // { dg-error "global" }
