// PR c++/90320
// { dg-do compile { target c++11 } }

struct B { };

struct M : B {
  M() = default;
  template <typename T> explicit M(T&&) = delete;
};

struct V {
  V(B);
};

M m;
V v = m;
