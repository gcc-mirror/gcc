// { dg-do compile }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 28 Feb 2002 <nathan@codesourcery.com>

// PR 775. Some mismatches which were broken.

template <class T> struct A {};
union A<int> a; // { dg-error "'union' tag" }

template <> union A<float> {}; // { dg-error "'union' tag" }

struct B {};
union B b;	// { dg-error "'union' tag" }

union C {};
class C c;	// { dg-error "'class' tag" }
