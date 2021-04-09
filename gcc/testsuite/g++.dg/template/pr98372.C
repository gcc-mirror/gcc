// PR 98372 ICE due to incorrect type compare
// { dg-do compile { target c++14 } }

template <typename _Tp> using remove_pointer_t = typename _Tp ::type;
template <bool> struct enable_if;
template <bool _Cond, typename>
using enable_if_t = typename enable_if<_Cond>::type;
template <typename> bool is_convertible_v;
template <typename, unsigned long = 0> class Span;
template <typename T, unsigned long> class Span {
  using element_type = T;
  template <unsigned long N>
  Span(element_type (&arr)[N],
       enable_if_t<is_convertible_v<remove_pointer_t<decltype(data(arr))>>,
                   decltype(nullptr)>);
};
template <typename T> class Span<T> {
  using element_type = T;
  template <unsigned long N>
  Span(element_type (&arr)[N],
       enable_if_t<is_convertible_v<remove_pointer_t<decltype(data(arr))>>,
                   decltype(nullptr)>);
};

struct aaa
{
  Span<char> data0;
};
