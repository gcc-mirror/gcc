// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 4 February 2001 <nathan@codesourcery.com>

// Check primary bases are chosen correctly.

struct A {virtual void Foo () {}};
struct B1 : virtual A {};
struct B2 : virtual A {};
struct C : virtual B1, B2 {};
struct D : virtual C {};

int main ()
{
  C c;
  D d;
  
  A *apc = &c;
  B1 *b1pc = &c;
  B2 *b2pc = &c;
  
  A *apd = &d;
  B1 *b1pd = &d;
  B2 *b2pd = &d;
  
#if __GXX_ABI_VERSION >= 100
  if (static_cast <void *> (apc) != static_cast <void *> (b1pc))
    return 1;
  if (static_cast <void *> (apd) != static_cast <void *> (b1pd))
    return 2;
  if (static_cast <void *> (apd) != static_cast <void *> (&d))
    return 3;
#endif
  return 0;
}
