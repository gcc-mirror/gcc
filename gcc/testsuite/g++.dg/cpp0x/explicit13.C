// PR c++/90320
// { dg-do compile { target c++11 } }

struct M {
  M() = default;
  template <typename T> explicit M(T&&) = delete;
};

struct V {
  V(M m);
};

M m;
V v = m;
