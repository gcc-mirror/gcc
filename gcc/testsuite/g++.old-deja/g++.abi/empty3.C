// Build don't link:

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 12 Apr 2001 <nathan@codesourcery.com>

// Check we deal with aligning virtual bases after a trailing empty
// base class properly

struct A {};
struct B1 : A {};
struct B2 : A {};
struct B3 : A {};

struct C : B1, B2, virtual B3 {};

int main ()
{
#if defined (__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100
  C c;
  if (((char *)static_cast <B3 *> (&c) - (char *)&c) % __alignof__ (C))
    return 1;
#endif
  return 0;
}
