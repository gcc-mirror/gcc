// PR c++/44148
// { dg-do compile }
// { dg-options "" }
// { dg-options "-fpic" { target fpic } }
// { dg-additional-options "-Wno-return-type" }

template <typename T> struct S2
{
  typedef const T &t2;
  t2 operator* () const {}
};
template <typename T> struct S3
{
  typedef S2 <T> t5;
};
template <typename T1, typename T2> T2 foo1 (T1 x, T2 y) { y (*x); }
template <class T> struct S4
{
  T &operator* () const;
};
struct S7 {};
struct S8
{
  typedef::S3 <S4 <S7> >::t5 t6;
  t6 m1 () const;
};
template <class T> struct S9
{
  typedef T t3;
  inline t3 &operator[] (unsigned int) {}
};
template <typename T1, typename T2, typename T3, void (&T4) (const T1 &, T3 &)> struct S10
{
  S10 (T2 &x, unsigned int y = 0) : u (x), v (y) {}
  void operator () (const S4 <T1> &x) { T4 (*x, u[v++]); }
  T2 &u;
  unsigned int v;
};
struct S15;
struct S11
{
  static void m3 (const S8 &, S15 &);
};
struct S16;
struct S12;
struct S13
{
  static void m4 (const S7 &,S16 &);
};
typedef S10 <S7, S12, S16, S13::m4> t10;
struct S12: S9 <S16>
{
};
struct S15
{
  S12 p;
};
void S11::m3 (const S8 &x, S15 &y)
{
  foo1 (x.m1 (), t10 (y.p));
}
