// { dg-do compile }

// Copyright (C) 2004 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>

// Nested class of class template as friend

template<class T> struct A
{
  struct B
  {
    void f();
  };
};

class C {
  int i;
  template<class T> friend struct A<T>::B;
};

template<class T> struct A<T*>
{
  struct B
  {
    void f();
  };
};

template<> struct A<char>
{
  struct B
  {
    void f();
  };
};

template<class T> void A<T>::B::f()
{
  C c;
  c.i = 0;
}

template<class T> void A<T*>::B::f()
{
  C c;
  c.i = 0;
}

void A<char>::B::f()
{
  C c;
  c.i = 0;
}

int main()
{
  A<int>::B b1;
  b1.f();
  A<int *>::B b2;
  b2.f();
  A<char>::B b3;
  b3.f();
}
