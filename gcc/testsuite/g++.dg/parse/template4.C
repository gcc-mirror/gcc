// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 32 Jan 2003 <nathan@codesourcery.com>

// PR 795. fields are not necessarily a dependent type.

struct V
{
  template<typename T> T get ();
};

struct L
{
  V v;
  
  template<typename T> T at (int i)
  {
    return v.get<T> ();
  }
};
