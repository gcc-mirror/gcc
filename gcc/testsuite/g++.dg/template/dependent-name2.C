// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 1 Aug 2003 <nathan@codesourcery.com>

// PR 10530. Thought a type was dependent.

template <typename T>
struct Foo {
  struct Inner {
    typedef int type;
  };
};

template <typename A> struct Bar {
  typedef typename Foo<int>::Inner::type type;
};

