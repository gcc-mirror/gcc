// Build don't link:

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 14 Nov 2000 <nathan@codesourcery.com>

// We failed to check virtual functions hidden by using declarations.

struct A
{
  virtual int foo ();
};

struct B
{
  virtual void foo ();  // ERROR - of this function
};

struct C : A , B
{
};

struct D : C
{
  void foo (short);
  using A::foo;
};

struct E : D
{
  virtual int foo ();   // ERROR - invalid override
};
