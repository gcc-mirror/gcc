// { dg-do assemble }
// { dg-options "-ansi -pedantic-errors -O2" }
// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 29 Nov 1999 <nathan@acm.org>


struct Foo
{
  inline ~Foo ();
};

inline void Wibble (int) throw ()
{
}

inline Foo::~Foo ()
{
  Wibble (6);
}

int ExtendFoos ()
{
  Foo  tmp;
  return 0;
}
