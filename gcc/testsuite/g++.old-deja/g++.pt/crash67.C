// { dg-do assemble  }
// { dg-options "" }
// 
// Copyright (C) 2001, 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 6 May 2001 <nathan@codesourcery.com>

// Bug 2526. We ICE'd after diagnosing dependent name confusion in
// friendliness when not being pedantic.

template<typename T>
struct B
{
  typedef B<T> Mother;
};

template<typename T>
struct D : B<T>
{
  friend class Mother;
};
