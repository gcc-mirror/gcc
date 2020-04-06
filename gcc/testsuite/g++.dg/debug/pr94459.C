// PR debug/94459
// { dg-do compile { target c++14 } }
// { dg-options "-g -dA" }

template <typename T>
struct S
{
  T v;
  T w[2];
  S () : v (0), w { 0, 0 } {}
  static auto baz () { return (T) 0; }
  auto m1 () { return v; }
  auto &m2 () { return v; }
  auto &&m3 () { return (T&&)v; }
  const auto m4 () { return v; }
  const auto &m5 () { return v; }
  const auto &&m6 () { return (T&&)v; }
  volatile auto m7 () { return v; }
  volatile auto &m8 () { return v; }
  volatile auto &&m9 () { return (T&&)v; }
  volatile const auto m10 () { return v; }
  volatile const auto &m11 () { return v; }
  volatile const auto &&m12 () { return (T&&)v; }
  const volatile auto m13 () { return v; }
  const volatile auto &m14 () { return v; }
  const volatile auto &&m15 () { return (T&&)v; }
#ifndef __STRICT_ANSI__
  __restrict const volatile auto &&m16 () { return (T&&)v; }
  const __restrict auto &m17 () { return v; }
#endif
  auto *m18 () { return &v; }
  auto (S::* (m19 ())) () { return &S::m1; }
  auto (S::* (m20 ())) { return &S::v; }
  auto (*m21 ()) () { return S::baz; }
};

S<long> s, u, v;

long
foo ()
{
  auto x = s.m19 ();
  auto y = s.m20 ();
  auto z = s.m21 ();
  return s.m1 () + s.m2 () + s.m3 () + s.m4 () + s.m5 ()
	 + s.m6 () + s.m7 () + s.m8 () + s.m9 () + s.m10 ()
	 + s.m11 () + s.m12 () + s.m13 () + s.m14 () + s.m15 ()
#ifndef __STRICT_ANSI__
	 + u.m16 () + v.m17 ()
#endif
	 + *s.m18 () + (s.*x) () + s.*y + z ();
}

int
main ()
{
  return foo ();
}
