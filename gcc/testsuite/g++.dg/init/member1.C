// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 13 Jun 2005 <nathan@codesourcery.com>

// Origin:   Ivan Godard <igodard@pacbell.net>
// Bug 20789: ICE on invalid

template<typename> struct A;

template<int> struct B {};

template<typename T> struct C
{
  static const int i = A<T>::i;  // { dg-error "incomplete" }
  static const int j = i;      // { dg-error "initialized by a non-const" }
  B<j> b;  // { dg-error "not a valid template arg" }
};

C<int> c;
