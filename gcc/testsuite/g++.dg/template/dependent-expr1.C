// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 30 Jun 2003 <nathan@codesourcery.com>

// PR c++ 9779. ICE

struct I 
{
};

void Foo (int);
namespace std
{
  template <typename X>
  void Baz (I *x)
  {
    Foo (sizeof (I));
    Foo (sizeof (x));
    Foo (__alignof__ (I));
    Foo (__alignof__ (x));
    Foo (x->~I ()); // { dg-error "16:invalid" }
    //    Foo (typeid (I));
    Foo (delete x); // { dg-error "10:invalid" }
    Foo (delete[] x); // { dg-error "10:invalid" }
    Foo (throw x); // { dg-error "10:invalid" }
  }

}
