// { dg-do assemble  }
// { dg-options "" }

// Copyright (C) 1999 Free Software Foundation

// by Alexandre Oliva <oliva@dcc.unicamp.br>
// based on bug report by Nick Rasmussen <nick@jive.org>

// This is slightly different from typename1.C.  This one tests
// whether the implicit typename extension works.  gcc 2.95 reports:

// warning: lookup of `foo' finds `struct foo'
// warning:   instead of `baz<T>::foo' from dependent base class
// warning:   (use `typename baz::foo' if that's what you meant)

// But baz<T>::foo is not a base class, and `foo' should have been
// found in baz's scope.

struct foo;

template <class T> struct bar {
  typedef int foo;
};

template <class T> struct baz {
  typedef bar<T>::foo foo; // { dg-error "need 'typename' before" "" { target c++17_down } }
  void m(foo); 
};
