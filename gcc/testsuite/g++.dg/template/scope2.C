// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 15 Aug 2003 <nathan@codesourcery.com>

// checked instantiated bases in wrong scope.

class Helper {};

template<class T> struct X { };

template<class T> class Base
{
  protected:
  typedef Helper H;
};

template<class T >
struct Derived : Base<T>
{
  typedef Base<T> Parent;
  typedef typename Parent::H H;
  
  class Nested : public X<H> {};
  
  Nested m;
  
  void Foo ();
};

void Foo (Derived<char> &x)
{
  x.Foo ();
}
