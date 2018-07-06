// PR c++/81933
// { dg-do compile { target c++14 } }

namespace std {
template <typename _Tp> struct __decay_and_strip { typedef _Tp __type; };
template <int> struct enable_if { typedef int type; };
template <typename _Head> struct _Head_base {
  constexpr _Head_base(_Head) {}
};
template <unsigned long, typename...> struct _Tuple_impl;
template <unsigned long _Idx, typename _Head, typename... _Tail>
struct _Tuple_impl<_Idx, _Head, _Tail...> : _Tuple_impl<1, _Tail...>, // { dg-warning "direct base" }
                                            _Head_base<_Head> {
  typedef _Tuple_impl<1, _Tail...> _Inherited;
  typedef _Head_base<_Head> _Base;
  constexpr _Tuple_impl(_Head __head, _Tail... __tail)
      : _Inherited(__tail...), _Base(__head) {}
  _Tuple_impl(const _Tuple_impl &) = default;
  _Tuple_impl(_Tuple_impl &&);
};
template <unsigned long _Idx, typename _Head>
struct _Tuple_impl<_Idx, _Head> : _Head_base<_Head> {
  typedef _Head_base<_Head> _Base;
  constexpr _Tuple_impl(_Head __head) : _Base(__head) {}
};
template <int> struct _TC {
  static constexpr bool _NotSameTuple() { return true; }
};
template <typename... _Elements> class tuple : _Tuple_impl<0, _Elements...> {
  typedef _Tuple_impl<0, _Elements...> _Inherited;

public:
  template <typename... _UElements,
            enable_if<_TC<1>::_NotSameTuple()>::type = false>
  constexpr tuple(_UElements... __elements) : _Inherited(__elements...) {}
  tuple(const tuple &) = default;
};
template <typename... _Elements>
constexpr tuple<typename __decay_and_strip<_Elements>::__type...>
    make_tuple(_Elements... __args) {
  typedef tuple<typename __decay_and_strip<_Elements>::__type...> __result_type;
  return __result_type(__args...);
}
}
struct any_udt {};
template <typename... Tuples> constexpr auto flatten(Tuples... tuples) {
  auto all = std::make_tuple(tuples...);
  auto flat(all);
  return flat;
}
constexpr auto fail = flatten(any_udt{}, any_udt{});
