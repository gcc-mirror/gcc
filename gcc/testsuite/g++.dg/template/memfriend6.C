// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>

// Member function of class template as friend
// Erroneous case: mismatch during declaration

template <class T> struct A {
  template <class U> void f(U);		// { dg-error "candidate" }
  void g();				// { dg-error "candidate" }
  void h();				// { dg-error "candidate" }
  void i(int);				// { dg-error "candidate" }
};

class C {
  int ii;
  template <class U> friend void A<U>::f(U);	// { dg-error "not match" }
  template <class U> template <class V>
    friend void A<U>::g();			// { dg-error "not match" }
  template <class U> friend int A<U>::h();	// { dg-error "not match" }
  template <class U> friend void A<U>::i(char);	// { dg-error "not match" }
};
