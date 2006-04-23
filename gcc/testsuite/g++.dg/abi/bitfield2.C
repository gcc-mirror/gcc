// { dg-do run }
// { dg-options "-ansi -pedantic-errors -fsigned-bitfields" }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 15 Dec 2001 <nathan@codesourcery.com>

typedef int Int;
typedef signed int SInt;
typedef unsigned int UInt;

struct A
{
  SInt bitS : 1;	// signed
  UInt bitU : 1;	// unsigned
  Int bit : 1;		// signedness by -f{signed,unsigned}-bitfields
};

int main ()
{
  A a;

  a.bitS = 1; // { dg-warning "overflow" }
  a.bitU = 1;
  a.bit = 1;  // { dg-warning "overflow" }

  if (a.bitS != -1)
    return 1;
  if (a.bitU != 1)
    return 2;
  if (a.bit != -1)
    return 3;

  return 0;
}
