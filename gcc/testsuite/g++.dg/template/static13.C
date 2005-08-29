// PR c++/23099

struct Base {
  int x;
};

template <typename T>
struct A {
  static const int N = sizeof(static_cast<Base*>(T()));
};

struct Derived : Base {
  A<Derived*> a;
};
