// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 22 Jul 2003 <nathan@codesourcery.com>

// PR c++ 11596

template <int V, bool F = V < 1> struct A { enum { value }; };
template <int V> struct B { enum { value = A<1>::value }; };
int ary[!B<1>::value ? 1 : -1];

template <int V, bool F = V < 1> struct A1 { enum { value = 1}; };
template <int V> struct A1<V,false> { enum { value}; };
template <int V> struct B1 { enum { value = A1<1>::value }; };

int ary1[!B1<1>::value ? 1 : -1];
