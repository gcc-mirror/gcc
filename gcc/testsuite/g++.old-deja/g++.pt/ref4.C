// Build don't link:
// 
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 29 Apr 2001 <nathan@codesourcery.com>

// Bug 2664. We failed to convert_from_reference for non-type
// template parms.

struct cow { };

cow c;

void func     (cow &c) {}
void operator-(cow &c) {}

template<cow &C> void test()
{
  func(C); //OK
  -C;      //bogus error
}

int main()
{
  test<c> ();
}
