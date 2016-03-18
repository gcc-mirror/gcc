// PR c++/70139
// { dg-options "-fno-elide-constructors" }
// { dg-do compile { target c++11 } }

template<class T, class U>
struct A
{
  T a;
  U b;
  constexpr A () : a (), b () { }
  constexpr A (const T &x, const U &y) : a (x), b (y) { }
};
struct B
{
  constexpr B (const bool x) : c (x) {}
  constexpr bool operator!= (const B x) const { return c != x.c; }
  bool c;
};
constexpr static A<B, B*> d[] = { { B (true), nullptr }, { B (false), nullptr } };
static_assert (d[0].a != d[1].a, "");
