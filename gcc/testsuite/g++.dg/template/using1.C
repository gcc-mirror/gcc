// { dg-do run }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 22 Jul 2003 <nathan@codesourcery.com>

// PR 9447. Using decls in template classes.

template <class T>
struct Foo {
  int i;
};

struct Baz 
{
  int j;
};

template <class T>
struct Bar : public Foo<T>, Baz {
  using Foo<T>::i;
  using Baz::j;
  
  int foo () { return i; }
  int baz () { return j; }
};

int main()
{
  Bar<int> bar;

  bar.i = 1;
  bar.j = 2;
  
  if (bar.foo() != 1)
    return 1;
  
  if (bar.baz() != 2)
    return 1;
  
  return 0;
}

