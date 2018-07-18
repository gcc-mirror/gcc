// { dg-do compile }
// { dg-options "-pedantic -pedantic-errors" }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 15 Dec 2001 <nathan@codesourcery.com>

// PR 2645

struct AS
{
  typedef void (myT) ();
  struct L {};
  
};


template <typename T> struct B1 : T
{
  typedef typename T::L __restrict__ r;// { dg-error "'__restrict__' qualifiers cannot" }
  typedef typename T::myT __restrict__ p;

  // The following are DR 295 dependent
  typedef typename T::myT volatile *myvolatile;
  typename T::myT volatile *a;
  myvolatile b;
};
template <typename T> struct B2 : T
{
  // The following are DR 295 dependent
  typedef typename T::myT const *myconst;
  typename T::myT const *a;
  myconst b;
};

B1<AS> b1;	// { dg-message "required" }
B2<AS> b2;
