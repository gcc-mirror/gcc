// Build don't link:
// 
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 7 May 2001 <nathan@codesourcery.com>

// Bug 2525. We ICEd when a namespace scope template was erroneously
// given as a base member init.

namespace N1
{
  template<typename T>
  struct B
  {
    B (T);
  };
  
  template<typename T>
  struct D : B<T>
  {
    D (T r)
      : B (r)  // ERROR - no field named B
    {}
  };
}

template<typename T>
struct D1 : N1::B<T>
{
  D1 (T r)
    : N1::B<T> (r)
  {}
};

template<typename T>
struct D2 : N1::B<T>
{
  D2 (T r)
    : N1::B (r) // ERROR - no field named N1::B
  {}
};
