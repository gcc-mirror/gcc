// { dg-do compile }

// Copyright (C) 2006 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 25 Aug 2006 <nathan@codesourcery.com>

// Origin: Tobias Schwinger <tschwinger@neoscientists.org>
// PR 27787. Too eager to resolve a typename

template<typename X>
struct x
{
  template<typename Y>
  struct y
  {
    typedef Y type;
  };
};

template<typename A>
struct a : x<A>
{
  template<typename B>
  typename a::template y<B>::type f(B);
};
