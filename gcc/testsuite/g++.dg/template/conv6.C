// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 21 Mar 2003 <nathan@codesourcery.com>

// PR 9898, DR 322. Conversion to reference type.

template <typename> struct Ref {};
template <typename> struct Val {};

struct Wrapper
{
  template <typename U> operator Ref<U> & ();
  template <typename U> operator Val<U> ();
};

void Foo (Wrapper l)
{
  static_cast <Ref<int> &> (l);
  static_cast <Ref<int> const &> (l);
  static_cast <Ref<int> > (l);
  static_cast <Val<int> const &> (l);
  static_cast <Val<int> > (l);
} 
