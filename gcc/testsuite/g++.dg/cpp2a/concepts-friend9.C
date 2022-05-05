// PR c++/105064
// { dg-do compile { target c++20 } }

struct A {
  template<class T>
  friend void f(T) requires true;
};

struct B {
  template<class T>
  friend void f(T) requires true;
};
