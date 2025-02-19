// PR c++/118846
// { dg-additional-options "-fmodules" }
// { dg-module-cmi M }

export module M;

template <int N> struct integral_constant { static constexpr int value = N; };
template <template <class> class> constexpr int cx_count_if() { return 0; }
template <template <class> class P> struct mp_count_if_impl {
  using type = integral_constant<cx_count_if<P>()>;
};

template <template <class> class> struct consume {
  static constexpr bool value = true;
};
template <class P> struct use {
  using type = consume<P::template fn>;
};
