// Copyright (C) 2002  Free Software Foundation.
//
// Test that the nothrow attribute is working correctly.
//
// Written by Richard Henderson, 26 May 2002.

// { dg-do link { target c++11} }
extern void foo [[gnu::nothrow]] ();
extern void link_error();

int main()
{
  try {
    foo();
  } catch (...) {
    link_error();
  }
}

void foo() { }
