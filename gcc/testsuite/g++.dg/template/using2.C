// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 22 Jul 2003 <nathan@codesourcery.com>

// PR 9447. Using decls in template classes.

template <class T>
struct Foo {
  int i;
};

struct Baz 
{
  int i;
};

template <class T>
struct Bar : public Foo<T>, Baz
{
  using Foo<T>::i; // { dg-message "previous declaration" }
  using Baz::i; // { dg-error "redeclaration" }

  int foo () { return i; }
};

void foo (Bar<int> &bar)
{
  bar.foo();
}

