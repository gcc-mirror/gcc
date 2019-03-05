// PR c++/89532
// { dg-do compile { target c++2a } }

struct tuple;

template <decltype(tuple {})> // { dg-error "invalid use of incomplete type" }
struct S { };

template<typename>
decltype(tuple {}) d; // { dg-error "invalid use of incomplete type" }
