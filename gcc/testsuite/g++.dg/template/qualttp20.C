// { dg-do compile }

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
  typedef typename T::L __restrict__ r;// { dg-error "`__restrict' qualifiers cannot" "" }
  typedef typename T::myT __restrict__ p;// { dg-warning "ignoring `__restrict'" "" { xfail *-*-* } }

  // The following are DR 295 dependent
  typedef typename T::myT volatile *myvolatile; // { dg-error "qualifiers" ""  }
  typename T::myT volatile *a;    // { dg-error "qualifiers" "" }
  myvolatile b;			 // { dg-error "qualifiers" "" }
};
template <typename T> struct B2 : T
{
  // The following are DR 295 dependent
  typedef typename T::myT const *myconst; // { dg-error "qualifiers" "" }
  typename T::myT const *a; // { dg-error "qualifiers" "" }
  myconst b; // { dg-error "qualifiers" "" }
};

B1<AS> b1;	// { dg-error "instantiated" "" }
B2<AS> b2;      // { dg-error "instantiated" "" }	
