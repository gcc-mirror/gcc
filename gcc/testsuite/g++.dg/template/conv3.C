// { dg-do run }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 29 Dec 2001 <nathan@codesourcery.com>

// PR 4361. Template conversion operators were not overloaded.

template <typename T> struct C
{
  operator T () 
  {
    return 0;
  }
  template <typename T2> operator T2 ()
  {
    return 1;
  }
  int Foo ()
  {
    return operator T ();
  }
  template <typename T2> int Baz ()
  {
    return static_cast <int> (operator T2 ());
  }
};

int main ()
{
  int r;
  C<int> c;

  r = c.Foo ();
  if (r)
    return 1;
  r = c.Baz<int> ();
  if (r)
    return 2;
  r = c.Baz<float> ();
  if (!r)
    return 3;
  return 0;
}
