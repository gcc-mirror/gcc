// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>

// Member function template of class template as friend

template <class T> struct A
{
  template <class U> void f();
};

class C {
  int i;
  template <class T> template <class U> friend void A<T>::f();
};

template <class T> struct A<T*>
{
  template <class U> void f();
};

template <> struct A<char>
{
  template <class U> void f();
};

template <class T> template <class U> void A<T>::f()
{
  C c;
  c.i = 0;
}

template <class T> template <class U> void A<T*>::f()
{
  C c;
  c.i = 0;
}

template <class U> void A<char>::f()
{
  C c;
  c.i = 0;
}

template <> void A<char>::f<int>()
{
  C c;
  c.i = 0;
}

int main()
{
  A<int> a1;
  a1.f<char>();
  A<int *> a2;
  a2.f<char>();
  A<char> a3;
  a3.f<char>();
  a3.f<int>();
}
