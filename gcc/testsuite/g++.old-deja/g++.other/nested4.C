// { dg-do assemble  }
// 
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 10 Aug 2000 <nathan@codesourcery.com>

// bug 372 We ICE'd on the out-of-class definition of a nested class of a
// class template.

struct Bar
{
};

template <class T>
struct Foo
{
  struct Baz;
  struct Biz;
  struct Boz
  : Bar
  {
  };
};

template <class T>
struct Foo<T>::Biz
{
};

template <class T>
struct Foo<T>::Baz
: Bar
{
};
