// { dg-do run }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 22 Jul 2003 <nathan@codesourcery.com>

// PR 9447. Using decls in template classes.

template <class T>
struct Foo {
  int k (float) {return 1;}
};

struct Baz 
{
  int k (int) {return 2;}
};

template <class T>
struct Bar : public Foo<T> , Baz {
  using Foo<T>::k;
  using Baz::k;

  int foo()
  {
    if (k (1.0f) != 1)
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
