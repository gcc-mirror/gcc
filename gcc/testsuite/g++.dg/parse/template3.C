// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 24 Jan 2003 <nathan@codesourcery.com>

// PR 9403. We failed to parse template keyword, and we accepted code
// which required one.

template<bool> struct Outer;

template <bool b, typename T>
struct X : Outer<b>::template Inner<T>
{};

template <bool b, typename T>
struct Y : Outer<b>::Inner<T> {}; // { dg-error "used as template" "temp" }
// { dg-error "expected" "exp" { target *-*-* } .-1 }
// { dg-message "note" "note" { target *-*-* } .-2 }

