// Build don't link:

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 10 Jan 2001 <nathan@codesourcery.com>

// Bug 1606. We sorry'd issuing an error.

struct A {};
template <class T = A> class Tpl {};

struct B {
  Tpl<int> s;
};

void foo (B *ptr)
{
  ptr->Tpl.t (); // ERROR - template as expression
}
