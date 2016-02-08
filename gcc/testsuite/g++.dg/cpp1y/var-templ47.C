// PR c++/69009
// { dg-do compile { target c++14 } }

using _uchar = char;
using _size_t = decltype(sizeof(_uchar));
using size_t = _size_t;
template <class T, T> struct integral_constant;
template <bool b> using bool_constant = integral_constant<bool, b>;
template <class> constexpr auto tuple_size_v = 0;
template <class T> auto const tuple_size_v<T const volatile> = tuple_size_v<T>;
template <class T>
using tuple_size = integral_constant<size_t, tuple_size_v<T>>;
template <typename Base, typename Deriv>
using is_base_of = bool_constant<__is_base_of(Base, Deriv)>;
template <class T, size_t N> void test() {
  is_base_of<integral_constant<size_t, N>, tuple_size<T>> value(
      is_base_of<integral_constant<size_t, N>, tuple_size<const volatile T>>);
}
void foo() { test<int, 0>; }
