// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 27 Jul 2003 <nathan@codesourcery.com>

// Failed to spot specialization using a template-id expr

template <typename n> class A {};
template <int m> class R {};

template <typename n, typename x> struct Trait { enum {m = sizeof (n)}; };

template <typename n, typename x> R<Trait<n,x>::m> f(A<x>);
template <> R<Trait<char,char>::m> f<char>(A<char>) {return R<1>();}
