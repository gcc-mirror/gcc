// { dg-do compile { target c++17 } }

template <class T> struct A;

template <class T> struct A {
  A(T&&);
};

int i;
A a = i;			// { dg-error "" }
