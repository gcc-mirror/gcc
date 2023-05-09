// PR c++/109752
// { dg-do compile { target c++20 } }

template <typename _Tp, typename... _Args>
  inline constexpr bool is_constructible_v = __is_constructible(_Tp, _Args...);
    template<typename _Tp, typename _Up>
      concept __weakly_eq_cmp_with
 = requires(_Tp __t, _Up __u) {    { __u != __t } ; // { dg-error "changed from" }
 };
  template<typename _Tp>
    concept regular =  is_constructible_v<_Tp>  && __weakly_eq_cmp_with<_Tp, _Tp>;
  template<typename _Iter> concept incrementable = true
&& regular<_Iter>
&& requires(_Iter __i) { { __i++ } ;}
;
template<typename D>
struct iterator_interface
{
  friend constexpr bool operator>=(D lhs, D rhs) requires __weakly_eq_cmp_with<D, D> { return true; }
};
template<typename T>
struct iterator : iterator_interface<iterator<T>>
{
    bool operator==(iterator) const;
};
static_assert(incrementable<iterator<int>>); // { dg-error "assert" }
