
// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 1 Apr 2004 <nathan@codesourcery.com>
// Origin:Matt Austern <austern@apple.com>

// PR:c++/14007

template <typename T> struct X {};  // #1
template <typename T> struct X<const T>; //#2
template struct X<int&>; //#3
