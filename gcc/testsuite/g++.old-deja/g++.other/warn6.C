// { dg-do assemble  }
// { dg-options "-W -Wall" }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 2 Mar 2001 <nathan@codesourcery.com>

// Bug 2139. We gave an erronous warning about an unused parm on a
// synthesized function

struct A
{
  virtual ~A ();
};
void foo (A const &a)
{
  A a1 = a;
}
