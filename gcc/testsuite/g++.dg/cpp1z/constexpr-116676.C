// PR c++/116676
// { dg-do compile { target c++17 } }

namespace std {
typedef __SIZE_TYPE__ size_t;

  template<typename _Tp>
    struct remove_reference
    { typedef _Tp type; };

  template<typename _Tp>
    struct remove_reference<_Tp&>
    { typedef _Tp type; };

  template<typename _Tp>
    struct remove_reference<_Tp&&>
    { typedef _Tp type; };

template <typename _Tp>
constexpr typename std::remove_reference<_Tp>::type &&
move(_Tp &&__t) noexcept {
  return static_cast<typename std::remove_reference<_Tp>::type &&>(__t);
}
template <typename _Tp> struct tuple_size;
template <size_t __i, typename _Tp> struct tuple_element;
template <typename _U1, typename _U2> class __pair_base {};
template <typename _T1, typename _T2>
struct pair {
  _T1 first;
  _T2 second;
  template <typename _U1 = _T1, typename _U2 = _T2>
  explicit constexpr pair(const _T1 &__a, const _T2 &__b)
      : first(__a), second(__b) {}
};
template <class _Tp1, class _Tp2>
struct tuple_size<pair<_Tp1, _Tp2>>
{
static constexpr size_t value = 2;
};
template <class _Tp1, class _Tp2>
struct tuple_element<0, pair<_Tp1, _Tp2>> {
  typedef _Tp1 type;
};
template <class _Tp1, class _Tp2>
struct tuple_element<1, pair<_Tp1, _Tp2>> {
  typedef _Tp2 type;
};

template <size_t _Int, class _Tp1, class _Tp2>
constexpr typename tuple_element<_Int, pair<_Tp1, _Tp2>>::type &
get(pair<_Tp1, _Tp2> &&__in) noexcept {
  return (std::move(__in).first);
}
int t;
auto [a, b] = std::pair<int&, int>{t, 1};
}

