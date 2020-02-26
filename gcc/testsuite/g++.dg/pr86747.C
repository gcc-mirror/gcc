// { dg-do compile }

template <typename T> class A {
  template <void (A::*p)()> class C; // #1
  template <void (A::*q)()> friend class C; // #2
};

A<double> a;
