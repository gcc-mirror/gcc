// { dg-do compile }

// Copyright (C) 2004 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>

// Nested class template of class template as friend

template<class T> struct A
{
  template <T t> struct B
  {
    void f();
  };
};

class C {
  int i;
  template<class T> template <T t> friend struct A<T>::B;
};

template<class T> struct A<T*>
{
  template <T* t> struct B
  {
    void f();
  };
};

template<> struct A<char>
{
  template <char t> struct B
  {
    void f();
  };
};

template<class T> template <T t> void A<T>::B<t>::f()
{
  C c;
  c.i = 0;
}

template<class T> template <T* t> void A<T*>::B<t>::f()
{
  C c;
  c.i = 0;
}

template <char t> void A<char>::B<t>::f()
{
  C c;
  c.i = 0;
}

template <> void A<char>::B<'b'>::f()
{
  C c;
  c.i = 0;
}

int d2 = 0;

int main()
{
  A<int>::B<0> b1;
  b1.f();
  A<int *>::B<&d2> b2;
  b2.f();
  A<char>::B<'a'> b3;
  b3.f();
  A<char>::B<'b'> b4;
  b4.f();
}
