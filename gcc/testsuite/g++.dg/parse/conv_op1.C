
// { dg-do compile }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 21 Dec 2002 <nathan@codesourcery.com>

// PR 8572. ICE with templated conversion operators.

template <typename T> struct A
{
  struct B { };
  operator B* () const;
  B *Foo ();
};

template <typename T> typename A<T>::B *A<T>::Foo ()
{
  return 0;
}

template <typename T> A<T>::operator typename A<T>::B* () const
{
  return 0;
}

void Foo (A<int> &p)
{
  p.Foo ();
  static_cast <A<int>::B *> (p);
}
