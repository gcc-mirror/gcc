// Build don't link:
// 
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 24 April 2001 <nathan@codesourcery.com>

// Bug 2608. A default parameter introduced in the definition of a
// ctor never made it into the clones, leading to later overload
// resolution failures. This is related to bug 2356.

struct A
{
  A (int, int);
};

A::A (int d, int = 0)
{
  if (d)
    {
      A a (0);
    }
}

void get_width ()
{
  A a (1);
}

struct B : A
{
  B ();
};
B::B ()
  :A (1)
{
}

struct C : virtual A
{
  C (int, int);
};
C::C (int, int = 0)
  :A (1)
{
}
struct D: C
{
  D ();
};
D::D ()
  :A (0), C (0)
{
}
