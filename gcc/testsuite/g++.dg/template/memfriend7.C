// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>

// Member function of class template as friend
// Erroneous case: mismatch during specialization

template <class T> struct A {
  template <class U> void f(U);
  void g();
  void h();
  void i(int);
  template <T t> void j();
};

class C {
  int ii;				// { dg-error "private" }
  template <class U> template <class V>
    friend void A<U>::f(V);
  template <class U> friend void A<U>::g();
  template <class U> friend void A<U>::h();
  template <class U> friend void A<U>::i(int);
  template <class U> template <U t>
    friend void A<U>::j();
};

template <class T> struct A<T*> {
  void f(int);
  template <class U> void g();
  int h();
  void i(char);
  template <int> void j();
};

template <class T> void A<T*>::f(int)
{
  C c;
  c.ii = 0;				// { dg-error "context" }
}

template <class T> template <class U> void A<T*>::g()
{
  C c;
  c.ii = 0;				// { dg-error "context" }
}

template <class T> int A<T*>::h()
{
  C c;
  c.ii = 0;				// { dg-error "context" }
}

template <class T> void A<T*>::i(char)
{
  C c;
  c.ii = 0;				// { dg-error "context" }
}

template <class T> template <int> void A<T*>::j()
{
  C c;
  c.ii = 0;				// { dg-error "context" }
}

template <> struct A<char> {
  void f(int);
  template <class U> void g();
  int h();
  void i(char);
  template <int> void j();
};

void A<char>::f(int)
{
  C c;
  c.ii = 0;				// { dg-error "context" }
}

template <class U> void A<char>::g()
{
  C c;
  c.ii = 0;				// { dg-error "context" }
}

template <> void A<char>::g<int>()
{
  C c;
  c.ii = 0;				// { dg-error "context" }
}

int A<char>::h()
{
  C c;
  c.ii = 0;				// { dg-error "context" }
}

void A<char>::i(char)
{
  C c;
  c.ii = 0;				// { dg-error "context" }
}

template <int> void A<char>::j()
{
  C c;
  c.ii = 0;				// { dg-error "context" }
}

template <> void A<char>::j<0>()
{
  C c;
  c.ii = 0;				// { dg-error "context" }
}

int main()
{
  A<int *> a1;
  a1.f(0);				// { dg-message "required" }
  a1.g<char>();				// { dg-message "required" }
  a1.g<int>();				// { dg-message "required" }
  a1.h();				// { dg-message "required" }
  a1.i('a');				// { dg-message "required" }
  a1.j<1>();				// { dg-message "required" }
  A<char> a2;
  a2.f(0);
  a2.g<char>();				// { dg-message "required" }
  a2.g<int>();
  a2.h();
  a2.i('a');
  a2.j<1>();				// { dg-message "required" }
  a2.j<0>();
}
