// { dg-do compile }

// Copyright (C) 2004 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>

// Nested class template of class template as friend

template <class T> struct A
{
  template <class U> struct B
  {
    void f();
  };
};

template <class V> class C {
  int i;
  template <class T> template <class U> friend struct A<T>::B;
};

template <class T> struct A<T*>
{
  template <class U> struct B
  {
    void f();
  };
};

template <> struct A<char>
{
  template <class U> struct B
  {
    void f();
  };
};

template <class T> template <class U> void A<T>::B<U>::f()
{
  C<int> c;
  c.i = 0;
}

template <class T> template <class U> void A<T*>::B<U>::f()
{
  C<int> c;
  c.i = 0;
}

template <class U> void A<char>::B<U>::f()
{
  C<int> c;
  c.i = 0;
}

template <> void A<char>::B<int>::f()
{
  C<int> c;
  c.i = 0;
}

int main()
{
  A<int>::B<int> b1;
  b1.f();
  A<int *>::B<int> b2;
  b2.f();
  A<char>::B<char> b3;
  b3.f();
  A<char>::B<int> b4;
  b4.f();
}
