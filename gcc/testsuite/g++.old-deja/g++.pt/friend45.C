// Build don't link:
// 
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 14 Aug 2000 <nathan@codesourcery.com>

// bug 42. We ICE'd on instantiating a template with a bogus templated friend.

template<typename T> struct X
{
  template<typename D> friend X<D>;         // ERROR - friend must use aggr tag
};

X<int> g;
