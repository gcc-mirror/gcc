// { dg-do assemble  }
// 
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 9 Aug 2000 <nathan@codesourcery.com>

// bugs 173, 174 & 406 all ICE'd due to Koenig lookup involving
// typename T::t.

struct A
{
  typedef int type;
};

template<typename T> void same_key (T, typename T::type);

template <class T> void foo (T *, void (*) (T, int));

void baz (A *ptr)
{
  foo (ptr, same_key);
}
