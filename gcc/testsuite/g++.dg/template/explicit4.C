// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 27 Jul 2003 <nathan@codesourcery.com>

// Failed to spot specialization using a template-id expr

template <typename n> class A {};
template <int m> class R {};

template <int n, int x> struct Trait { enum {m = n}; };

template <typename n, typename x> R<Trait<1,1>::m> f(A<x>);
template <> R<Trait<1,1>::m> f<int>(A<int>) {return R<1>();}
