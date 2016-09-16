// PR c++/77338
// { dg-do compile { target c++11 } }

struct S;

template <typename>
auto f (S s) -> decltype (s (s));	// { dg-error "no match for call to" }
