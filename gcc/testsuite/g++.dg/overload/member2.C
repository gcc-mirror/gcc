// { dg-do run }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 17 Oct 2002 <nathan@codesourcery.com>

// PR 7676. We didn't notice template members were different.

struct foo
{
  template<class T>
  int bar() {return 1;}
  
  template<int I>
  int bar() {return 2;}
    
};

struct baz : foo
{
  using foo::bar;
  template<int I>
  int bar () {return 3;}
};

int main ()
{
  baz b;
  foo f;

  if (f.bar<1> () != 2)
    return 1;
  if (f.bar<int> () != 1)
    return 2;
  
  if (b.bar<1> () != 3)
    return 1;
  if (b.bar<int> () != 1)
    return 2;

  return 0;
}
