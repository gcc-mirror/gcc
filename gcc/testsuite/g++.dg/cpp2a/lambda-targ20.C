// PR c++/122318
// { dg-do compile { target c++20 } }

template<class T, auto V>
struct A {
  A() { }
  A(T) { }
};

template<class T>
using AT = A<T, []{}>;

AT<int> a;
AT b = a;
