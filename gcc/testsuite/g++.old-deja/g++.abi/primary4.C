// { dg-do run  }
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 4 February 2001 <nathan@codesourcery.com>

// Check primary bases are chosen correctly.

struct A { virtual void Foo () {}};

struct B1 : virtual A {};
struct B2 : virtual A {};

struct C : virtual B1 {};

struct D : virtual B1, B2, C {};

int main ()
{
  D d;
  A *ap = &d;
  C *cp = &d;
  
#if __GXX_ABI_VERSION >= 100
  if (static_cast <void *> (ap) != static_cast <void *> (cp))
    return 1;
#endif

  return 0;
}
