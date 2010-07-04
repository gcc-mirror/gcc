// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 14 Aug 2003 <nathan@codesourcery.com>

// instantiated from did not indicate the nested class

template<class T> struct X
{
  T m;	// { dg-error "as type 'void'" "void" }
	// { dg-error "incomplete type" "incomplate" { target *-*-* } 10 }
	// { dg-error "invalid" "invalid" { target *-*-* } 10 }
};

template<class T >
struct Derived
{
  class Nested : public X<T> // { dg-message "instantiated" "" }
  {
  };
  
  Nested m; // { dg-message "instantiated" "" }
  
  void Foo ();
};

void Foo (Derived<void> &x)
{
  x.Foo (); // { dg-message "instantiated" "" }
}
