// { dg-do run }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 22 Jul 2003 <nathan@codesourcery.com>

// PR 9447. Using decls in template classes.

template <class T>
struct Foo {
  int i (int) {return 1;}
};

struct Baz 
{
  int k (int) {return 2;}
};

template <class T>
struct Bar : public Foo<T> , Baz {
  using Foo<T>::i;
  using Baz::k;

  int i (float) {return 3;}
  int k (float) {return 3;}
  
  int foo()
  {
    if (i (1) != 1)
      return 1;
    if (k (1) != 2)
      return 2;

    return 0;
  }
};

int main()
{
  Bar<int> bar;

  return bar.foo();
}
