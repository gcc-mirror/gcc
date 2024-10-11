// { dg-additional-options "-fmodules-ts -Wtemplate-names-tu-local" }
// Test streaming and instantiations of various kinds of exposures

export module M;

namespace {
  int x;
  constexpr int y = 1;

  struct S { int m; void d(); };
  enum class E { e };

  template <typename T> int f(T t) { return (int)t; }
}

template <auto N> void g() {}

template <typename T>
int expose_1() {  // { dg-warning "TU-local" }
  return x;
}

template <typename T>
void expose_2() {  // { dg-warning "TU-local" }
  T t = &y;
}

template <typename T>
bool expose_3() {  // { dg-warning "TU-local" }
  return !(T{} * (x + 5) > 123);
}

template <typename T>
bool expose_4() {  // { dg-warning "TU-local" }
  return __is_same(S, T);
}

template <typename T>
void expose_5() {  // { dg-warning "TU-local" }
  static_assert(T{} == (int)E::e);
}

template <typename T>
void expose_6() {  // { dg-warning "TU-local" }
  f(T{});
}

template <typename T>
void expose_7() {  // { dg-warning "TU-local" }
  g<&y>();
}

template <typename T>
void expose_8() {  // { dg-warning "TU-local" }
  decltype(T{} .* &S::m)* (*x)[5][10];
};

template <typename T>
bool expose_9() {  // { dg-warning "TU-local" }
  return noexcept((T{} .* &S::d)());
}

template <typename T>
void expose_10() {  // { dg-warning "TU-local" }
  using U = decltype(f<T>());
}

template <typename T>
void expose_11() {  // { dg-warning "TU-local" }
  static thread_local E r;
}

template <typename T>
int expose_var  // { dg-warning "TU-local" }
  = f(sizeof(T));
