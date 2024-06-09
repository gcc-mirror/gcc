// PR c++/114393
// { dg-do compile { target c++20 } }

template <auto _DescriptorFn> struct c1 {};

template <class _Descriptor, auto F = [] { return _Descriptor(); }>
inline constexpr auto b_v = F;

template <class T, class U>
inline constexpr auto c_v = b_v<U>;

auto f = c_v<int, char>;
using type = decltype(f());
using type = char;
