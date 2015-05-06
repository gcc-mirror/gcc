// DR 1518
// { dg-do compile { target c++11 } }

struct A {
  explicit A() = default;
};

struct B : A {
  explicit B() = default;
};

struct C {
  explicit C();
};

struct D : A {
  C c;
  explicit D() = default;
};

template<typename T> void f() {
  T t = {};
}
template<typename T> void g() {
  void x(T t);
  x({});
}
