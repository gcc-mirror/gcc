// Build don't link:
// 
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 10 Aug 2000 <nathan@codesourcery.com>

// bug 381. We gave slightly different diagnostics, when binding an rvalue to
// a non-const reference, depending on the precise type to the rvalue.

namespace A { 
template <class T> void f(T) {}
};
void (* &h)(int) = A::f<int>;       // ERROR - rvalue to non-const
void (*const volatile &i)(int) = A::f<int>;  // ERROR - rvalue to volatile
void (*const &j)(int) = A::f<int>;

int &k = 1;                         // ERROR - rvalue to non-const
int &const volatile l = 1;          // ERROR - rvalue to volatile
int const &m = 1;
