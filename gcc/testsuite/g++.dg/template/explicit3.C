// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 27 Jul 2003 <nathan@codesourcery.com>

// Failed to spot specialization using a template-id expr

template <int n> class A {};
template <int m> class R {};

template <int n, int x> struct Trait { enum {m = n}; };

template <int n, int x> R<Trait<n,x>::m> f(A<x>);
template <> R<Trait<1,1>::m> f<1>(A<1>) {return R<1>();}

void Baz ()
{
  R<Trait<1,1>::m> (*ptr) (A<1>);

  ptr = &f<1>;
  
}
