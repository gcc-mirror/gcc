// { dg-do compile }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 26 Dec 2002 <nathan@codesourcery.com>

// PR 4803. Used inline functions must have a definition.

inline void Foo1 ();  // { dg-warning "inline function" "" }
inline void Bar1 ();
template <typename T> inline void Foo2(T);   // { dg-warning "inline function" "" }
template <typename T> inline void Bar2(T);

void Baz ()
{
  Foo1 ();
  Foo2 (1);

  Bar1 ();
  Bar2 (1);
}

inline void Bar1 () {}
template <typename T> inline void Bar2(T) {}
