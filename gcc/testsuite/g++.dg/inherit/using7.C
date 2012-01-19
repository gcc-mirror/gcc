// PR c++/51889

struct A {
  void f();
};

template <class T>
struct B: A
{
  using A::f;
  void f();
};
