// Build don't link:
// 
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 21 Jan 2001 <nathan@codesourcery.com>

// Bug 1728. We started sorting things when there were 7 fields. Our
// template_count algorithm was rather fragile ...

template <int dim> struct X
{
  struct Y
  {
    int x1;
    int x2;
    int x3;
    int x4;
    int x5;
    int x6;
    int x7;

    void Foo ();
  };
};

template <> void X<1>::Y::Foo () {}
