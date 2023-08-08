// PR c++/100482
// { dg-do compile { target c++11 } }

namespace N {}
decltype(N) x;   // { dg-error "expected primary-expression" }

struct S {};
decltype(S) y;  // { dg-error "argument to .decltype. must be an expression" }

template <typename T>
struct U {};
decltype(U) z;  // { dg-error "missing template arguments" }
