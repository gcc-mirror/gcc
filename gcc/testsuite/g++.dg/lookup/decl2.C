// { dg-do compile }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 24 Dec 2002 <nathan@codesourcery.com>
// Source Martin Buchholz martin@xemacs.org

// PR 9053. Failed to consider templates that are disambiguated by
// return type.

template <typename T> class bar;
template <> struct bar<const char*> { typedef void type; };
template <typename T> class qux;
template <> struct qux<int> { typedef void type; };

template <typename T>
typename bar<T>::type foo (T t) { }

template <typename T>
typename qux<T>::type foo (T t) { }


int
main (int argc, char *argv[])
{
  foo ("foo");
  foo (7);
}
