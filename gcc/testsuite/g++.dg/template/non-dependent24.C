// PR c++/105758

struct A {
  void foo(int);
};

template<class>
struct Z : A {
  static Z *z;
  void bar();
};

template<class T>
Z<T> *Z<T>::z;

template<class T>
void Z<T>::bar() {
  z->foo(0);
}
