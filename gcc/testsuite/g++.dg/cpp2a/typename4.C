// P0634R3
// { dg-do compile { target c++20 } }

template<class T>
struct A {
  typedef int B;
  B b;
};
