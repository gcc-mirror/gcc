// { dg-do assemble  }

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 28 Nov 2000 <nathan@codesourcery.com>

// Inspired by by 756. We'd ICE when trying to define a member of an
// incomplete template type.

template<class X> struct ObjCount;  // { dg-error "" } forward decl

template<class X> int ObjCount<X>::m; // { dg-error "" } undefined type
