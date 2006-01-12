// Copyright (C) 2006 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 6 Jan 2006 <nathan@codesourcery.com>

// PR 24824
// Origin:   	 wanderer@rsu.ru

// { dg-options "-feliminate-dwarf2-dups" }

namespace N
{
  struct Base
  {
    int m;
  };

  struct Derived : Base
  {
    using Base::m;
  };
}

N::Derived thing;
