// PR c++/114974
// { dg-do compile { target c++20 } }

template<typename T1>
struct A {
  template<typename T2>
  struct B { T2 t; };
};

A<int>::B x{2}; // OK
A<int>::B y(2); // OK, previously rejected
