// Build don't link:
// 
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 18 Jan 2001 <nathan@codesourcery.com>

// Bug 1639. We failed to have builtin relop candidates with enumeral type.

template <typename T1, typename T2> void operator == (T1, T2);

enum E {e1};
void operator != (E, E);

bool Foo (E e)
{
  return e == e1;
}
bool Baz (E e)
{
  return e != e1; // ERROR - void not ignored.
}
