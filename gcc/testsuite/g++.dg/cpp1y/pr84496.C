// PR c++/84496
// { dg-do compile { target c++14 } }

template <typename T, T n> struct C { static constexpr T D = n; };
struct E : C<bool, false> {};
template <typename> struct F : C<bool, false> {};
template <typename T> T foo ();
template <typename> struct H { typedef int G; };
template <typename> class I;
struct L;
template <typename, typename> struct J;
template <bool, bool, typename...> struct K;
struct R {
  template <typename M, typename... N>
  static J<decltype (foo<M> () (foo<N>...)), L> o;
};
template <typename P, typename... Q> struct K<false, false, P, Q...> : R {
  typedef decltype (o<P, Q...>) G;
};
template <typename P, typename... Q>
struct D : K<E::D, F<typename H<P>::G>::D, P, Q...> {};
template <typename P, typename... Q> struct I<P (Q...)> : D<P, Q...> {};
template <typename> class function;
template <typename S, typename... Q> struct function<S (Q...)> {
  template <typename T, typename = typename I<T (Q...)>::G> struct C;
  template <typename, typename> using U = int;
  template <typename P, typename = U<int, void>, typename = U<C<P>, void>>
  function (P);
};
template <typename S, typename... Q>
template <typename P, typename, typename>
function<S (Q...)>::function (P)
{
}
void bar (function<void (int)>);

void
baz ()
{
  auto a = [] {
    static int counter;
    bar ([] (auto) { counter++; });
  };
}
