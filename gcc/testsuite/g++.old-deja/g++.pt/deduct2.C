// { dg-do assemble  }
// 
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 5 Sept 2000 <nathan@codesourcery.com>

// bug 79 & 59. We failed to tsubst non-type template parms which used
// (previously deduced) type parms.

struct C {};  

template< class T, T *G > struct S {};
template< class T, T *G > void boz ( S<T,G> s1);

C c1Gen;

void foo ()
{
  S< C, &c1Gen > s1;

  boz (s1);
}
