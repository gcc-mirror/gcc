// PR c++/48296
// { dg-do compile { target c++11 } }

struct X
{
  constexpr X() { }
  constexpr X f(X x) { return x; }
  constexpr X g(X x);
};

constexpr X X::g(X x) { return x; }

struct Y
{
  Y() { }
  constexpr Y f(Y y) { return y; }  // { dg-error "constexpr" "" { target { { ! implicit_constexpr } && c++20_down } } }
  static constexpr Y g(Y y) { return y; } // { dg-error "constexpr" "" { target { { ! implicit_constexpr } && c++20_down } } }
};
