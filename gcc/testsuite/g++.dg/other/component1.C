// { dg-do compile }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 28 Dec 2001 <nathan@codesourcery.com>

// PR 5123. ICE

struct C {
  template<class T> void f(T);
  void g ();
  void g (int);
};

void Foo () {
  C c;

  (c.g) ();
  (c.f) (1);
  
  (c.f<int>) (2);

  c.g;			// { dg-error "statement cannot resolve" "" }
  c.f;		        // { dg-error "statement cannot resolve" "" }
  c.f<int>;		// { dg-error "statement cannot resolve" "" }
  
  c.g == 1;		// { dg-error "invalid use of" "" }
  c.f == 1;		// { dg-error "invalid use of" "" }
  c.f<int> == 1;	// { dg-error "invalid use of" "" }
};
