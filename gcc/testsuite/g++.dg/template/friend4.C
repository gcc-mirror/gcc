// { dg-do compile }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 18 Dec 2001 <nathan@codesourcery.com>

// PR 109, dependent member friends

struct B
{
  static int foo ();
  struct N
  {
    static int bar ();
  };
};


template <class T>
class A
{
  friend int T::foo ();
  friend int T::N::bar ();
  
  private:
  static int m;
};

template <class T>
class C
{
  friend struct T::N;

  private:
  static int m;
};


int B::foo ()
{
  return A<B>::m;
}

int B::N::bar ()
{
  return A<B>::m + C<B>::m;
}
