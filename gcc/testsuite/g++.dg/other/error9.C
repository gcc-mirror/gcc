// { dg-options -Wall }

// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 27 Sept 2004 <nathan@codesourcery.com>

// Origin:   	 v.haisman@sh.cvut.cz
// Bug 17681: bad diagnostic text.

struct A
{ };

struct B
{ };

struct C : public B, public A
{
  C ()  // { dg-warning "when initialized" }
    : A(), B()  // { dg-warning "base .\[AB\]." }
  { }
};
