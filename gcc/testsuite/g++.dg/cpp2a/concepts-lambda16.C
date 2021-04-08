// PR c++/99874
// { dg-do compile { target c++20 } }

template <class T>
struct A {
  static inline auto a = [] <class U> (U) {
    return [] <class V> (V) requires requires (T t, U u, V v) { t + u + v; } { };
  };

  template <class W>
  static inline auto b = [] <class U> (U) {
    return [] <class V> (V) requires requires (T t, U u, V v, W w) { t + u + v + w; } { };
  };

  static auto f() {
    return [] <class U> (U) {
      return [] <class V> (V) requires requires (T t, U u, V v) { t + u + v; } { };
    };
  }

  template <class W>
  static auto g() {
    return [] <class U> (U) {
      return [] <class V> (V) requires requires (T t, U u, V v, W w) { t + u + v + w; } { };
    };
  }
};

template <class T>
auto a = [] <class U> (U) {
  return [] <class V> (V) requires requires (T t, U u, V v) { t + u + v; } { };
};

template <class T>
auto b = [] <class U> (U) {
  return [] <class V> (V) {
    return [] {
      return [] () requires requires (T t, U u, V v) { t + u + v; } { };
    };
  };
};

int main() {
  A<int>::a(0)(0);
  A<int>::a(0)(nullptr); // { dg-error "no match" }

  A<int>::b<int>(0)(0);
  A<int>::b<int>(0)(nullptr); // { dg-error "no match" }

  A<int>::f()(0)(0);
  A<int>::f()(0)(nullptr); // { dg-error "no match" }

  A<int>::g<int>()(0)(0);
  A<int>::g<int>()(0)(nullptr); // { dg-error "no match" }

  a<int>(0)(0);
  a<int>(0)(nullptr); // { dg-error "no match" }

  b<int>(0)(0)();
  b<int>(0)(nullptr)()(); // { dg-error "no match" }
}
