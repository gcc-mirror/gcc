// Build don't link:

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 14 Nov 2000 <nathan@codesourcery.com>

// We failed to spot a static member which overrode a virtual

struct A
{
  virtual int foo (char);
  static int foo ();
  virtual int foo (int);    // ERROR - this function
  static int foo (float);
  virtual int foo (double);
};

struct B : A
{
  static int foo (int);   // ERROR - cannot override
};
