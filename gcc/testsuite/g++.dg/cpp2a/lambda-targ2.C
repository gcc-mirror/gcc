// PR c++/114393
// { dg-do compile { target c++20 } }

template <auto _DescriptorFn> struct c1 {};

template <class _Descriptor, auto t = [] { return _Descriptor(); }>
inline constexpr auto b_v = t;

template <class _Tag>
using c1_t = c1<b_v<int>>;

template <class _Data>
constexpr auto g(_Data __data) {
  return c1_t<_Data>{};
}

void f() {
  auto &&b = g(0);
}
