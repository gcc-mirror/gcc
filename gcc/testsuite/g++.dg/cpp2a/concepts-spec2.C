// { dg-do compile { target c++20 } }

template<class T, int> concept C = true;

template<class T> struct A {
  template<C<sizeof(T)> U> void f(); // #1
  template<C<0> U>         void f(); // #2
  template<C<-1> U>        void f(); // #3
};

constexpr int n = sizeof(int);
template<> template<C<n> U>  void A<int>::f() { } // matches #1
template<> template<C<0> U>  void A<int>::f() { } // matches #2
template<> template<C<-2> U> void A<int>::f() { } // no match { dg-error "match" }
template<> template<class U> void A<int>::f() requires C<U, -1> { } // shouldn't match #3
// { dg-error "match" "" { xfail *-*-* } .-1 }
