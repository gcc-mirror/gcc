// Copyright (C) 2003 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>
// { dg-do compile }

// Template instantiate during deferred access check

template <void (*)(int)> struct C {
  typedef int Y;
};

template <class T> void f(typename T::X) {
}

class A {
  typedef int X;
  template <class T> friend void f(typename T::X);
};

C<&f<A> >::Y g(int);
