// { dg-do compile }

// Copyright (C) 2001, 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 31 Dec 2001 <nathan@codesourcery.com>

// PR 4379. We created pointers to member references and pointers to
// member fields when we shouldn't have.

struct D {
  
  int &m;       // { dg-error "invalid use of non-static data member" "" }
  static int &s;
  
  int Foo ();
};

template<class T> int f1(T x);
template<class T> int f2(T x);

int D::Foo ()
{
  f1( &D::m);   // { dg-error "cannot create pointer to ref" "" }
  f1( &(D::m));	// ok
  f2( &D::s);   // ok
  f2( &(D::s)); // ok
  return 0;
}

int Foo ()
{
  f1( &D::m);    // { dg-error "cannot create pointer to ref" "" }
  f1( &(D::m));  // { dg-error "from this location" "" }
  f2( &D::s);    // ok
  f2( &(D::s));  // ok
  return 0;
}
