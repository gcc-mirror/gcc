// Copyright (C) 2003 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>
// { dg-do compile }

// Elaborate type specifier of class template

template <class T> class A {
  class B;
};

template <class T> class A<T>::B {
  friend class A;
};
