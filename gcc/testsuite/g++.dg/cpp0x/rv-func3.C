// DR 1328
// { dg-do compile { target c++11 } }

template <class T> struct A {
  operator T&();  // #1
  operator T&&(); // #2
};
typedef int Fn();
A<Fn> a;
Fn&& f = a;
