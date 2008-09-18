// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 22 Jul 2003 <nathan@codesourcery.com>

// PR 9447. Using decls in template classes.

template <class T>
struct Foo {
  int i; // { dg-error "Foo" }
};

struct Baz 
{
  int i; // { dg-error "Baz" }
};

template <class T>
struct Bar : public Foo<T>, Baz {
  using Foo<T>::i;
  using Baz::i;

  int foo () { return i; } // { dg-error "request for member" }
};

void foo (Bar<int> &bar)
{
  bar.foo(); // { dg-message "instantiated" }
}

