// PR c++/88181
// { dg-do compile }
// { dg-options "-fpack-struct -g -std=c++11" }

template <typename T> struct A { typedef T B; };
template <typename...> class C;
template <typename e> struct D { constexpr D (e) {} };
template <int, typename...> struct E;
template <int N, typename T, typename... U>
struct E<N, T, U...> : E<1, U...>, D<T> {
  constexpr E (T x, U... y) : E<1, U...>(y...), D<T>(x) {}
};
template <int N, typename T> struct E<N, T> : D<T> {
  constexpr E (T x) : D<T>(x) {}
};
template <typename T, typename U> struct C<T, U> : E<0, T, U> {
  constexpr C (T x, U y) : E<0, T, U>(x, y) {}
  void operator= (typename A<const C>::B);
};
struct F {};
struct G {};

int
main ()
{
  F f;
  G g;
  constexpr C<F, G> c(f, g);
}
