// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>

// Member function of class template as friend

template<class T> struct A
{
  void f(T);
};

class C {
  int i;
  template<class T> friend void A<T>::f(T);
};

template<class T> struct A<T*>
{
  void f(T*);
};

template<> struct A<char>
{
  void f(char);
};

template<class T> void A<T>::f(T)
{
  C c;
  c.i = 0;
}

template<class T> void A<T*>::f(T*)
{
  C c;
  c.i = 0;
}

void A<char>::f(char)
{
  C c;
  c.i = 0;
}

int main()
{
  A<int> a1;
  a1.f(0);
  A<int *> a2;
  int *p = 0;
  a2.f(p);
  A<char> a3;
  a3.f('a');
}
