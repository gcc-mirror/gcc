// PR c++/105982
// { dg-do compile { target c++17 } }

template<class T>
struct A {
  struct B { struct C { }; };
  A(T, typename B::C);
};

A a(0, {});
