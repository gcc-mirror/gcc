// { dg-do assemble  }
// 
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 18 Jan 2001 <nathan@codesourcery.com>

// Bug 1617. We didn't resolve partial ordering properly. The std is rather
// vague about it anyway, DR 214 talks about this.

extern "C" int puts (char const *);

template <typename T> int Foo (T);    // { dg-error "" } candidate
template <typename T> int Foo (T &);  // { dg-error "" } candidate

template <typename T> int Qux (T);    // { dg-error "" } candidate
template <typename T> int Qux (T const &);  // { dg-error "" } candidate

template <typename T> int Bar (T const *const &); // { dg-error "" } candidate
template <typename T> int Bar (T *const &);       // { dg-error "" } candidate
template <typename T> int Bar (T *);              // { dg-error "" } candidate

template <typename T> int Baz (T *const &);       // { dg-error "" } candidate
template <typename T> int Baz (T *);              // { dg-error "" } candidate

int Baz (int const *ptr, int *ptr2)
{
  Baz (ptr2);   // { dg-error "" } ambiguous
  Bar (ptr2);   // { dg-error "" } ambiguous
  Foo (ptr2);   // { dg-error "" } ambiguous
  Qux (ptr2);   // { dg-error "" } ambiguous
  return 0;
}
