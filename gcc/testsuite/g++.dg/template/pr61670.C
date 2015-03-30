// PR c++/61670
// { dg-do compile }

template <class>
class A {
  A: 0 // { dg-error "" }
};

A<int> a;
