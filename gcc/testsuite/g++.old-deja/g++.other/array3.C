// Build don't link:
// 
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 10 Aug 2000 <nathan@codesourcery.com>

// bug 386.C We ICE'd before emitting a diagnostic when trying to
// initialize a constant non-pod array from something bogus.


struct A
{
  A(char);
};

class B
{
  const A ary[16];

  B (const A ary[]);
};

B::B (const A a[])
  : ary(a)
{        // ERROR - bad array initializer
}
