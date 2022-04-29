// PR c++/99032
// { dg-do compile { target c++11 } }

class X { };
template<typename T1, typename T2>
void foo (T1, T2);

struct S {
  [[deprecated]] friend void f(); // { dg-warning "attribute ignored" }
  [[deprecated]] friend void f2() { }
  __attribute__((deprecated)) friend void f3();
  friend void f3 [[deprecated]] (); // { dg-warning "attribute ignored" }
  friend void f4 [[deprecated]] () { }
  [[deprecated]] friend void; // { dg-warning "attribute ignored" }
  __attribute__((deprecated)) friend int;
  friend __attribute__((deprecated)) int;
  friend int __attribute__((deprecated));
  [[deprecated]] friend X; // { dg-warning "attribute ignored" }
  [[deprecated]] friend class N; // { dg-warning "attribute ignored" }
  friend class [[deprecated]] N2; // { dg-warning "attribute ignored" }
  friend class __attribute__((deprecated)) N3;
  [[deprecated]] friend void foo<>(int, int); // { dg-warning "attribute ignored" }
  [[deprecated]] friend void ::foo(int, int); // { dg-warning "attribute ignored" }
  // { dg-bogus "should have" "PR100339" }
};

template<typename T>
class node { };

template<typename T>
struct A {
  [[deprecated]] friend T; // { dg-warning "attribute ignored" }
  [[deprecated]] friend class node<T>; // { dg-warning "attribute ignored" }
  template<typename>
  [[deprecated]] friend class A; // { dg-warning "attribute ignored" }
  template<typename>
  [[deprecated]] friend void bar () { }
  template<typename>
  [[deprecated]] friend void baz (); // { dg-warning "attribute ignored" }
};
