// { dg-do compile { target c++2a } }

template <auto N>
struct A {
  static constexpr auto n = N;
};

template <auto N>
constexpr auto g(A<[]{return N;}> a) {
  return a.n();
}

static_assert(g<42>({}) == 42);
