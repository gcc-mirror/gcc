// { dg-options -std=c++1z }

template <class T> struct A;

template <class T> struct A {
  A(T&&);
};

int i;
A a = i;			// { dg-error "" }
