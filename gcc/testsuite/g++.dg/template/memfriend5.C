// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>

// Member template function of member class template as friend

template <class T> struct A {
  template <class U> struct B {
    template <class V> void f(V);
  };
};

class X {
  int i;
  template <class T> template <class U> template <class V>
    friend void A<T>::B<U>::f(V);
};

template <class T> template <class U> template <class V>
  void A<T>::B<U>::f(V)
{
  X x;
  x.i = 0;
}

int main()
{
  A<char>::B<char> a1;
  a1.f(0);
}
