// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 1 Aug 2003 <nathan@codesourcery.com>

// PR 9447. Using decls in reopened template classes.

template <typename> struct A { int i; };

template <typename T> struct B : public A<T>
{
    using A<T>::i;
    int foo() const;
};

struct C {};

template <typename T> int B<T>::foo() const
{
  return i;
}
