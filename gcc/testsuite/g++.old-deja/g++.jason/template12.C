// { dg-do assemble  }
// Testcase for 'this is a type' syntax.

struct B {
  typedef int A;
};

template <class T> struct Y {
  void f() {
    typename T::A *d;
  }
};

template class Y<B>;
