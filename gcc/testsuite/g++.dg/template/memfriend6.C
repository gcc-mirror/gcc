// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation
// Contributed by Kriang Lerdsuwanakij <lerdsuwa@users.sourceforge.net>

// Member function of class template as friend
// Erroneous case: mismatch during declaration

template <class T> struct A { // { dg-message "defined here" }
  template <class U> void f(U);		// { dg-message "candidate" }
  void g();				// { dg-message "candidate" }
  void h();				// { dg-message "candidate" }
  void i(int);				// { dg-message "candidate" }
};

class C {
  int ii;
  template <class U>
  friend void A<U>::f(U); // { dg-error "no declaration matches" }
  template <class U> template <class V>
    friend void A<U>::g();  // { dg-error "no declaration matches" }
  template <class U>
  friend int A<U>::h();	// { dg-error "no declaration matches" }
  template <class U>
  friend void A<U>::i(char);	// { dg-error "no declaration matches" }
};
