// Build don't link:
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 11 April 2001 <nathan@codesourcery.com>
// Origin: Theo Papadopoulo <Theodore.Papadopoulo@sophia.inria.fr>

// Bug 1917. We were considering thunks to clones to be clones. and
// hence getting confused.

struct A { virtual ~A(); };
struct B { virtual ~B(); };

struct C: public A,B {};

template <class TYPE>
inline TYPE
sqr(const TYPE& x) {
    return 1;
}
int f(const int t) { return sqr(t); }
