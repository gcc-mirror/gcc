// { dg-do compile { target c++11 } }

template <class T>
struct A {
  template <int I = 42, int J = (T)42> int f() { return I; }
  template <int I = 42> int g() { return f(); }
};
