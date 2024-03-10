// { dg-do compile { target c++11 } }

template<class T> using F = int(*)(T);
using G = int(*)(int*);

struct A {
  template<class T> operator F<T>(); // #1
  operator G() = delete; // #2
};

struct B {
  operator G() = delete; // #2
  template<class T> operator F<T>(); // #1
};

int i = A{}(0); // OK, selects #1
int j = B{}(0); // OK, selects #1
