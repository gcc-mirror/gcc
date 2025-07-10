// { dg-do compile { target c++11 } }

struct A
{
  A();
  A(const A&);
  bool empty();
};

constexpr int ellipsis(...) { return 1; }

static_assert(ellipsis(A().empty()), "Error"); // { dg-error "non-constant condition" }
// { dg-error "call to non-'constexpr' function 'bool A::empty\\\(\\\)'" "" { target c++23_down } .-1 }
// { dg-error "temporary of non-literal type 'A' in a constant expression" "" { target c++26 } .-2 }
