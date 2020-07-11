// PR c++/94885 - paren-init of aggregates accepts invalid code.
// { dg-do compile { target c++20 } }

template <typename T, typename = decltype(T(0))> // { dg-error "could not convert" }
void foo();

struct base {};
struct derived : base {};

void
bar()
{
  foo<derived>(); // { dg-error "no matching function" }
}
