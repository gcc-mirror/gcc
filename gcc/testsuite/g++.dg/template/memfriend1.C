// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>

// Member function of class template as friend

template<class T> struct A
{
  void f();
};

class C {
  int i;
  template<class T> friend void A<T>::f();
};

template<class T> struct A<T*>
{
  void f();
};

template<> struct A<char>
{
  void f();
};

template<class T> void A<T>::f()
{
  C c;
  c.i = 0;
}

template<class T> void A<T*>::f()
{
  C c;
  c.i = 0;
}

void A<char>::f()
{
  C c;
  c.i = 0;
}

int main()
{
  A<int> a1;
  a1.f();
  A<int *> a2;
  a2.f();
  A<char> a3;
  a3.f();
}
