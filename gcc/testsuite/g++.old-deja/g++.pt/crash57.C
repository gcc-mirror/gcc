// Build don't link:
// 
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 11 Aug 2000 <nathan@codesourcery.com>

// bug 382. We ICE'd rather than decay to an address.

struct A {
template <class T> static void f(T) {}
};
void (*h)(int) = A::f<int>;
void (*i)(int) = &A::f<int>;
