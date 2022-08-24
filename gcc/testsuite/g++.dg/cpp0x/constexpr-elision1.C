// PR c++/105550
// { dg-do compile { target c++11 } }

template <typename, typename> struct pair {
  constexpr pair(int, int) {}
};
template <typename _Tp, typename _Compare>
pair<const _Tp &, const _Tp &> minmax(const _Tp &__a, const _Tp &__b,
                                      _Compare) {
  return 0 ? pair<const _Tp &, const _Tp &>(__b, __a)
           : pair<const _Tp &, const _Tp &>(__a, __b);
}
typedef int value_type;
typedef int compare_type;
template pair<const value_type &, const value_type &>
minmax(const value_type &, const value_type &, compare_type);
