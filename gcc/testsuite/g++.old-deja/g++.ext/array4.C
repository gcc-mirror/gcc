// { dg-do assemble  }
// { dg-options "" }


// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Mark Mitchell 19 Mar 2000 <mark@codesourcery.com>
//                Nathan Sidwell 19 Mar 2000 <nathan@codesourcery.com>

// [nathan] We have a zero sized array extension, and (unfortunately) allow it
// to be the sole member of a struct (rather than the trailing member of a
// non-empty struct as C99 is/will allow). Such a type will have a size of
// zero, but is not incomplete.

struct A
{
  int m[0];
};

void foo ()
{
  A a;
}

template <class T>
struct S
{
  int x[0];
};

template struct S<int>;

