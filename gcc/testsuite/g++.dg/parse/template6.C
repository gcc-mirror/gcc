// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Wolfgang Bangerth <bangerth@ticam.utexas.edu> 20 Feb 2003.

// PR c++/9778.  Ensure templated functions in other namespaces are
// correctly instantiated.

namespace NS {
  template <int N> void foo ();
}

template <int N> struct X {
  int m;
  void g () {
    NS::foo<sizeof(m)>();
  }
};

template class X<2>;
