// { dg-do compile }

template <int kBytes, typename From, typename To>
void CopyBytes(From from, To to) {
  __builtin_memcpy(to, from, kBytes);
}
template <typename From, typename To> void CopySameSize(From *from, To to) {
  CopyBytes<sizeof(From)>(from, to);
}
template <typename> using MakeUnsigned = char;
template <typename Lane, int N> struct Simd {
  using T = Lane;
  static constexpr int kPrivateLanes = N;
  template <typename NewT> using Rebind = Simd<NewT, 0>;
};
template <class D> using TFromD = D::T;
template <class T, class D> using Rebind = D::template Rebind<T>;
template <class D> using RebindToUnsigned = Rebind<MakeUnsigned<D>, D>;
template <typename T, int> struct Vec128 {
  using PrivateT = T;
  static constexpr int kPrivateN = 6;
  T raw[16];
};
template <class V> using DFromV = Simd<typename V::PrivateT, V::kPrivateN>;
template <class D> Vec128<TFromD<D>, D::kPrivateLanes> Zero(D);
template <class D> using VFromD = decltype(Zero(D()));
template <class D, class VFrom> VFromD<D> BitCast(D, VFrom v) {
  VFromD<D> to;
  CopySameSize(&v, to.raw);
  return to;
}
template <int N> Vec128<signed char, N> And(Vec128<signed char, N> b) {
  Vec128<signed char, N> a;
  DFromV<decltype(a)> d;
  RebindToUnsigned<decltype(d)> du;
  auto au(a);
  auto bu = BitCast(du, b);
  for (int i = 0; i < N; ++i)
    au.raw[i] &= bu.raw[i];
  return au;
}
void Or(Vec128<signed char, 16>);
template <int N> void IfVecThenElse(Vec128<signed char, N> yes) {
  Vec128 __trans_tmp_2 = And(yes);
  Or(__trans_tmp_2);
}
template <int N> void IfThenElseZero(Vec128<signed char, N> yes) {
  IfVecThenElse(yes);
}
Vec128<signed char, 16> Abs_a;
char MaskedAbs___trans_tmp_5;
void MaskedAbs() {
  Vec128<signed char, 16> __trans_tmp_4;
  for (int i = 0; i < 16; ++i) {
    MaskedAbs___trans_tmp_5 = Abs_a.raw[i] ? -Abs_a.raw[i] : 0;
    Abs_a.raw[i] = MaskedAbs___trans_tmp_5;
  }
  __trans_tmp_4 = Abs_a;
  Vec128 __trans_tmp_3 = __trans_tmp_4;
  IfThenElseZero(__trans_tmp_3);
}
