// PR c++/80864
// { dg-do compile { target c++11 } }
// { dg-options "-Wmissing-braces" }

struct S {};
struct A { S s[1]; };

template <typename>
struct R { static constexpr auto h = A{S{}}; }; // { dg-warning "missing braces" }

template <typename>
struct R2 { static constexpr auto h = A{{S{}}}; };

A foo = R<int>::h;
A foo2 = R2<int>::h;
