// { dg-do compile }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 24 Dec 2002 <nathan@codesourcery.com>

// PR 8702. Failure to match templates.

template <typename X> struct C1{};

template <typename X>
struct C2 {
  template<typename Y> operator C1<Y>();
  template<typename Y> operator C2<Y>();
};

template<typename X> template<typename Y>
C2<X>::operator C1<Y>()
{
  return C1<Y>();
}

struct A { // { dg-message "defined here" }
  operator int ();			// { dg-message "operator" }
  operator float ();			// { dg-message "operator" }
  operator float () const;		// { dg-message "operator" }
  template <typename T> operator T * (); // { dg-message "operator" }
};

A::operator short () { // { dg-error "no declaration matches" }
  return 0;
}
