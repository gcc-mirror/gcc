// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 29 Nov 1999 <nathan@acm.org>

// Build don't link:
// Special g++ Options: -ansi -pedantic-errors -O2
// crash test - XFAIL *-*-*

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
