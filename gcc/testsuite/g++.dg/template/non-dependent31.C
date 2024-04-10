// PR c++/113908
// { dg-do compile { target c++11 } }

struct A {
  A();
private:
  A(const A&);
};

struct B {
  A a;

  template<class T>
  static void f() { new B(); }
};

template void B::f<int>();
static_assert(!__is_constructible(B, const B&), "");
