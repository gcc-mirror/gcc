// PR c++/83856
// { dg-do compile { target c++14 } }

namespace std {
template <typename _Tp> _Tp declval();
template <class _E> class initializer_list {
  _E *_M_len;

public:
  __SIZE_TYPE__ size;
  _E begin();
};
template <typename, unsigned> struct array { void operator[](long); };
} // namespace std

template <class> struct simd {
  static int size();
  template <class F> simd(F, decltype(std::declval<F>()(0)) * = nullptr) {}
};
template <class V, class... F>
void test_tuples(std::initializer_list<std::array<float, 1>> data,
                 F... fun_pack) {
  auto it = data.begin();
  const int remaining = V::size();
  [](...) {}((fun_pack([&](auto) { it[remaining]; }), 0)...);
}

void f() {
  test_tuples<simd<float>>({}, [](simd<float>) {});
}
