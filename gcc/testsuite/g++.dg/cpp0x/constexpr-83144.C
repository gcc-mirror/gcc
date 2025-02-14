// PR c++/83144
// { dg-do compile { target c++11 } }

template <typename...> class tuple;
struct _Head_base {
  int _M_head_impl;
};
template <unsigned long, typename...> struct _Tuple_impl;
template <unsigned long _Idx, typename _Head, typename... _Tail>
struct _Tuple_impl<_Idx, _Head, _Tail...> : _Head_base {};
template <typename _T1, typename _T2>
struct tuple<_T1, _T2> : _Tuple_impl<0, _T2> {
  template <typename _U1, typename _U2> tuple(_U1, _U2);
};
template <long, typename... _Elements> void get(tuple<_Elements...>);
template <class T> struct interval_t : tuple<T, T> {
  using tuple<T, T>::tuple;
  constexpr T last() { get<1>(*this); }
  auto size() -> decltype(last() - 0);
};
int main(int argc, char **) { interval_t<int>{2, argc}; }
