// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 31 Jul 2003 <nathan@codesourcery.com>

// PR c++/11525 incorrect error about non-constant initalizer

template<typename> class X;
template<unsigned> class Y {};


template<typename T>
void Foo ()
{
  static const unsigned I = X<T>::I;
  
  Y<I> i;
  
  static const unsigned J = X<T>::J; // { dg-message "not initialized with a constant expression" }
  
  Y<J> j; // { dg-error "constant" "" }
}

struct A 
{
  operator unsigned () const;
};

template <typename> struct X 
{
  enum {I};
  static A const J;
};

void Baz ()
{
  Foo<int> (); // { dg-message "instantiated" "" }
}

  
