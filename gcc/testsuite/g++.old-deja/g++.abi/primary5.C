// { dg-do run  }
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 4 February 2001 <nathan@codesourcery.com>

// Check primary bases are chosen correctly.

struct A1
{
   virtual void Foo () {}
};

struct A : A1
{
};

struct B : A
{
};

struct C : virtual B
{
};

struct D : virtual B, virtual C
{
};

int main()
{
  D d;
#if __GXX_ABI_VERSION >= 100
  if (sizeof (D) != sizeof (A))
    return 1;
#endif

  return 0;
}
