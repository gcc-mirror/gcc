// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 15 Oct 2004 <nathan@codesourcery.com>

// We forgot to ignore current context and friends when determing
// which exceptions shadowed eachother.

struct E;

struct B {};

struct D : private B
{
  friend class E;
  
  static B *baz (D *);
  virtual void V () throw (B);  // { dg-error "overriding" "" { target { ! c++1z } } }
};				// { dg-error "dynamic exception specification" "" { target c++1z } .-1 }
				// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } .-2 }
struct E : public D
{
  virtual void V () throw (D); // { dg-error "looser throw" "" { target { ! c++1z } } }
};			       // { dg-error "dynamic exception specification" "" { target c++1z } .-1 }
			       // { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } .-2 }
B* foo (D *);

B *D::baz (D *p)
{
  try {foo (p);}
  catch (B const &b) {}
  catch (D const &d) {}
  return p;
}
