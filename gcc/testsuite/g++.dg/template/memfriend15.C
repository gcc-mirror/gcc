// { dg-do compile }

// Copyright (C) 2004 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>

// Nested class of class template as friend

template<class T> struct A
{
  struct B1
  {
  };
  struct B2
  {
    void f();
  };
};

class C {
  int i;	// { dg-error "private" }
  template<class T> friend struct A<T>::B1;
};

template<class T> void A<T>::B2::f()
{
  C c;
  c.i = 0;	// { dg-error "context" }
}

int main()
{
  A<int>::B2 b1;
  b1.f();	// { dg-error "instantiated" }
}
