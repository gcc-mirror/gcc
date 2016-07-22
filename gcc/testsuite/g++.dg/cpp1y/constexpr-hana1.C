// PR c++/70685
// { dg-do compile { target c++14 } }

namespace std {
template <typename _Tp, _Tp __v> struct A { static constexpr _Tp value = __v;
};
typedef A<bool, false> false_type;
template <bool, typename _Iftrue, typename> using conditional_t = _Iftrue;
namespace hana {
template <typename> struct is_default : false_type {};
template <typename> struct tag_of;
struct deleted_implementation;
namespace detail {
namespace operators {
template <typename> struct adl {};
}
}
template <typename> struct B;
template <int v> struct G : std::A<int, v> {};
template <typename T, T v> G<v> integral_c;
template <int i> using int_ = G<i>;
template <int i> int_<i> int_c;
template <typename> struct C;
template <typename Tag> struct D {
  template <typename... X> auto operator()(X... x) {
    return C<Tag>::apply(x...);
  }
};
template <typename Tag> D<Tag> make;
template <typename> struct unpack_impl;
struct Foldable {
  using Tag = int;
  static constexpr int value = is_default<unpack_impl<Tag>>::value;
};
struct range_tag;
auto make_range = make<range_tag>;
template <typename> struct sum_impl;
template <typename> struct F;
template <typename M = B<int>> F<M> sum;
template <typename T, T, T To>
struct range : detail::operators::adl<range<T, 0, To>> {};
template <typename T, T From, T To> struct tag_of<range<T, From, To>> {
  using type = range_tag;
};
template <> struct C<range_tag> {
  template <typename From, typename To> static auto apply(From, To) {
    using T = int;
    constexpr T from(From::value);
    constexpr T to(To::value);
    return range<T, from, to>{};
  }
};
template <> struct sum_impl<range_tag> {
  template <typename I> static constexpr I sum_helper(I m, I n) {
    if (m == n)
      return 0;
    return sum_helper(0, 0);
  }
  template <typename T, T from, T to> static auto apply(range<T, from, to>) {
    integral_c<T, sum_helper(from, to - 1)>;
  }
};
template <typename> struct F {
  template <typename Xs> auto operator()(Xs xs) {
    using S = typename tag_of<Xs>::type;
    using Sum =
        conditional_t<Foldable::value, sum_impl<S>, deleted_implementation>;
    Sum::apply(xs);
  }
};
}
auto __hana_tmp_22 =
    (hana::sum<>(hana::make_range(hana::int_c<-3>, hana::int_c<-2>)),
     hana::sum<>(hana::make_range(hana::int_c<3>, hana::int_c<7>)),
     hana::int_c<6>);
}
