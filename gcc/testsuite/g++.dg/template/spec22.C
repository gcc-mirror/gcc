// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 2 Apr 2005 <nathan@codesourcery.com>

// PR 20723
// Origin: Andrew Pinski <pinskia@gcc.gnu.org>
//         Nathan Sidwell <nathan@gcc.gnu.org>

template <typename T>
int operator+ (T const &, int); // { dg-error "T = Foo" "" }

struct Foo 
{
  template <typename T>
  int operator+ (T) const; // { dg-error "T = int" "" }
};

int main ()
{
  Foo f;

  return f + 0; // { dg-error "ambiguous overload" "" }
}
