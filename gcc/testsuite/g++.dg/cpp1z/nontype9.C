// PR c++/123529
// { dg-do compile { target c++17 } }

template<int* P>
void f();

template<class T>
struct A {
  static void g() {
    static int i;
    f<&i>();
  }
};

template struct A<char>;
template struct A<int>;
