// Build don't link:
// 
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 18 Jan 2001 <nathan@codesourcery.com>

// Bug 1617. We didn't resolve partial ordering properly. The std is rather
// vague about it anyway, DR 214 talks about this.

extern "C" int puts (char const *);

template <typename T> int Foo (T);    // ERROR - candidate
template <typename T> int Foo (T &);  // ERROR - candidate

template <typename T> int Qux (T);    // ERROR - candidate
template <typename T> int Qux (T const &);  // ERROR - candidate

template <typename T> int Bar (T const *const &); // ERROR - candidate
template <typename T> int Bar (T *const &);       // ERROR - candidate
template <typename T> int Bar (T *);              // ERROR - candidate

template <typename T> int Baz (T *const &);       // ERROR - candidate
template <typename T> int Baz (T *);              // ERROR - candidate

int Baz (int const *ptr, int *ptr2)
{
  Baz (ptr2);   // ERROR - ambiguous
  Bar (ptr2);   // ERROR - ambiguous
  Foo (ptr2);   // ERROR - ambiguous
  Qux (ptr2);   // ERROR - ambiguous
  return 0;
}
