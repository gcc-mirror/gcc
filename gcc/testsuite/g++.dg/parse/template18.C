// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 14 Oct 2005 <nathan@codesourcery.com>

// PR 22603: ICE
// Origin:  Flash Sheridan <flash@pobox.com>

struct A
{
  template<int> struct B
  {
    void foo(const struct C&);
  };
};
