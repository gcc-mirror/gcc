// { dg-do compile }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 29 Dec 2001 <nathan@codesourcery.com>

// PR 4361. Template conversion operators were not overloaded.

template <class T> struct Second;

template<class T> struct First
{
  int Foo ();
  
  template <class U> operator Second<U>();
  template <class U> operator First<U>();
};

template <class T> int First<T>::Foo ()
{} // This is here to make sure we didn't smash Foo's decl in the
   // method vector

struct B { };
struct D { };

void Foo ()
{
  First<B> (First<D>::*pf)() = &First<D>::operator First<B>;
}
