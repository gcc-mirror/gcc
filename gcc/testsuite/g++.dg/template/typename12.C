// { dg-do compile }

// Copyright (C) 2007 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 21 Jul 2007 <nathan@codesourcery.com>

// Origin: sschunck@pdf.de
// PR 30818, failure to resolve typename typedef

template < typename T > 
class A 
{
  typedef int type;
  class B;
};

template < typename T >
class A<T>::B
{
  typedef typename A<T>::type type;
  type f();
};

template < typename T >
typename A<T>::B::type 
A<T>::B::f() { return 0; }
