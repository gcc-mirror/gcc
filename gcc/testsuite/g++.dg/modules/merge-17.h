// PR c++/99426

inline auto f() {
  struct A { int m = 42; };
  return A{};
}

template<class T>
auto ft() {
  decltype(+T()) x;
  return [&x] { };
}

inline auto g() {
  enum E { e };
  return e;
}

template<class T>
auto gt() {
  enum E : T { e };
  return e;
}

inline auto h0() {
  struct { int m; } a0;
  struct { char n; } a1;
  return a0;
}

inline auto h1() {
  struct { int m; } a0;
  struct { char n; } a1;
  return a1;
}

template<class T>
inline auto h0t() {
  struct { int m; } a0;
  struct { char n; } a1;
  return a0;
}

template<class T>
inline auto h1t() {
  struct { int m; } a0;
  struct { char n; } a1;
  return a1;
}

using ty1 = decltype(f());
using ty2 = decltype(ft<int>());
using ty3 = decltype(g());
using ty4 = decltype(gt<int>());
using ty5 = decltype(h0());
using ty6 = decltype(h0t<int>());
using ty7 = decltype(h1());
using ty8 = decltype(h1t<int>());
