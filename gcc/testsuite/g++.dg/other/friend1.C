// { dg-do compile }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 10 Oct 2001 <nathan@codesourcery.com>

// Bug 4476. We tangled up inline friends and pure virtuals during
// class definition.

struct A {
  friend void f () { }
  void g (A a) {  }
};
