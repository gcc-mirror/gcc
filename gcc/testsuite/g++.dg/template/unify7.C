// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 1 Apr 2004 <nathan@codesourcery.com>

// PR c++/3518
template <typename T> void Foo (const T &);
template <typename T> void Baz (const T (*)()); // { dg-message "note" }

int &f ();

int main()
{
  Foo (f);
  Baz (f); // { dg-error "no matching function" "" }
  // { dg-message "(candidate|incompatible cv-qualifiers)" "candidate note" { target *-*-* } .-1 }
}
