// { dg-do compile }

// Copyright (C) 2001, 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 13 Nov 2001 <nathan@codesourcery.com>

// Bug 3154

class A {};

struct B : A
{
  typedef A Parent;
  
  B () : Parent () {}
};

class T
{
  typedef int Foo;
  T () : Foo () {}	// { dg-error "T::Foo' is not" "" }
};

struct S : B
{
  int Parent;

  S () :Parent (1) {}
};
