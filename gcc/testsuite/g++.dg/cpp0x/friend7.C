// PR c++/99032
// { dg-do compile { target c++11 } }

class X { };
template<typename T1, typename T2>
void foo (T1, T2);

struct S {
  [[deprecated]] friend void f(); // { dg-error "attribute appertains" }
  [[deprecated]] friend void f2() { }
  __attribute__((deprecated)) friend void f3(); // { dg-error "attribute appertains" }
  friend void f3 [[deprecated]] (); // { dg-error "attribute appertains" }
  friend void f4 [[deprecated]] () { }
  [[deprecated]] friend void; // { dg-error "attribute appertains" }
  __attribute__((deprecated)) friend int; // { dg-error "attribute appertains" }
  friend __attribute__((deprecated)) int; // { dg-error "attribute appertains" }
  friend int __attribute__((deprecated)); // { dg-error "attribute appertains" }
  [[deprecated]] friend X; // { dg-error "attribute appertains" }
  [[deprecated]] friend class N; // { dg-warning "attribute ignored" }
  friend class [[deprecated]] N2; // { dg-error "attribute appertains" }
  friend class __attribute__((deprecated)) N3; // { dg-error "attribute appertains" }
  [[deprecated]] friend void foo<>(int, int); // { dg-error "attribute appertains" }
  [[deprecated]] friend void ::foo(int, int); // { dg-error "attribute appertains" }
  // { dg-bogus "should have" "PR100339" { xfail *-*-* } .-1 }
};

template<typename T>
class node { };

template<typename T>
struct A {
  [[deprecated]] friend T; // { dg-error "attribute appertains" }
  [[deprecated]] friend class node<T>; // { dg-warning "attribute ignored" }
  template<typename>
  [[deprecated]] friend class A; // { dg-warning "attribute ignored" }
  template<typename>
  [[deprecated]] friend void bar () { }
  template<typename>
  [[deprecated]] friend void baz (); // { dg-error "attribute appertains" }
};
