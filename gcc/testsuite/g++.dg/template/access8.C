// Copyright (C) 2003 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>
// { dg-do compile }

// Template instantiate during deferred access check

template <class T> struct C {
  typedef typename T::X Y;
};

class A {
  typedef int X;
  template <class T> friend struct C;
};

C<A>::Y f(int);
