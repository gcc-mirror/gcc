// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>

// Member function of class template as friend

template<class T> struct A
{
  template <T t> void f();
};

class C {
  int i;
  template<class T> template <T t> friend void A<T>::f();
};

template<class T> struct A<T*>
{
  template <T* t> void f();
};

template<> struct A<char>
{
  template <char t> void f();
};

template<class T> template <T t> void A<T>::f()
{
  C c;
  c.i = 0;
}

template<class T> template <T* t> void A<T*>::f()
{
  C c;
  c.i = 0;
}

template <char t> void A<char>::f()
{
  C c;
  c.i = 0;
}

template <> void A<char>::f<'b'>()
{
  C c;
  c.i = 0;
}

int d2 = 0;

int main()
{
  A<int> a1;
  a1.f<0>();
  A<int *> a2;
  a2.f<&d2>();
  A<char> a3;
  a3.f<'a'>();
  a3.f<'b'>();
}
