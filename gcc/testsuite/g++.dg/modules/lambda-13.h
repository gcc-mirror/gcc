// PR c++/123075

template <typename T>
concept r = []{ return true; }();

template <typename T>
inline void foo() {
  static_assert(r<T>);
}

template void foo<int>();

template <typename T>
struct S {
  static_assert(r<T>);
};

template struct S<double>;

enum E {
  X = r<E>,
};
