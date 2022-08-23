// PR c++/65396
// { dg-do compile { target c++11 } }
// Like defarg23.C, but for member functions.

struct A {
  template<class T> void f();
  template<class T=int> void g(); // { dg-message "original definition" }
  template<class T=char, class U> void h();
};

template<class T=int> void A::f() { }
template<class T=int> void A::g() { } // { dg-error "redefinition of default" }
template<class T, class U=bool>
void A::h() {
  static_assert(__is_same(T, char), "");
  static_assert(__is_same(U, bool), "");
}

int main() {
  A a;
  a.f();
  a.g();
  a.h();
}
