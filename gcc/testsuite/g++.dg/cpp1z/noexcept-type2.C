// Test for function pointer conversion on template arguments.
// { dg-options -std=c++17 }

template <void (*P)()> struct A { };

void f() noexcept { };
constexpr void (*p)() noexcept = f;

A<f> a;
A<p> b;
