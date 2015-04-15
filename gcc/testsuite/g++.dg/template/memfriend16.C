// { dg-do compile }

// Copyright (C) 2004 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>

// Nested class of class template as friend

template<class T> struct A
{
  template <class U> struct B1
  {
  };
  template <class U> struct B2
  {
    void f();
  };
};

class C {
  int i;	// { dg-message "private" }
  template<class T> template <class U> friend struct A<T>::B1;
};

template<class T> template <class U> void A<T>::B2<U>::f()
{
  C c;
  c.i = 0;	// { dg-error "context" }
}

int main()
{
  A<int>::B2<int> b1;
  b1.f();	// { dg-message "required" }
}
