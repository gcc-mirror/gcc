// Build don't link:

// Copyright (C) 1999, 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 4 Jun 1999 <nathan@acm.org>
// Derived from a bug report from Stephan Riess <riess@bfw-online.de>
// http://gcc.gnu.org/ml/gcc-bugs/1999-06n/msg00107.html

// Builtin ops don't always need reference bindings, and we weren't removing
// them always after doing overload resolution. This caused us to try and take
// the address of a bitfield.

enum E {one};
struct A {
  E m1: 8;
  E m2: 8;
  unsigned m3 : 8;
  unsigned m4 : 8;
};

int fn (int f)
{
  A a;
  E m1, m2;
  E e;
  int i;
  
  e = f ? m1 : m2;
  e = f ? a.m1 : a.m2;
  i = f ? a.m3 : a.m4;
  (f ? m1 : m2) = e;
  (f ? a.m1 : a.m2) = e;
  (f ? a.m3 : a.m4) = i;
  return 0;
}
