// PR c++/116567
// { dg-do compile { target c++20 } }

struct X { int n; };

template<auto N, auto F = []{ return N; }>
auto v1 = F;

template<auto N, auto F = [](auto y){ return decltype(N){N.n + y}; }>
auto v1g = F;

template<class T>
struct A {
  template<auto N, auto F = []{ return N; }>
  static inline auto v2 = F;

  template<auto N, auto F = [](auto y){ return decltype(N){N.n + y}; }>
  static inline auto v2g = F;

  template<class U>
  struct B {
    template<auto N, auto F = []{ return N; }>
    static inline auto v3 = F;

    template<auto N, auto F = [](auto y){ return decltype(N){N.n + y}; }>
    static inline auto v3g = F;

    template<class V>
    static void f() {
      static_assert(v1<X{1}>().n == 1);
      static_assert(v1g<X{1}>(42).n == 1 + 42);
      static_assert(v2<X{2}>().n == 2);
      static_assert(v2g<X{2}>(42).n == 2 + 42);
      static_assert(v3<X{3}>().n == 3);
      static_assert(v3g<X{3}>(42).n == 3 + 42);
    }
  };
};

int main() {
  A<int>::B<int>::f<int>();
}
