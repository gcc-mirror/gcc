// PR c++/127232
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

template<typename, typename> struct same_type;
template<typename T> struct same_type<T, T> { };

void
foo (int const& x)
{
  constexpr std::meta::info y = ^^x;
  constexpr auto e = extract<int const&>(y);    // { dg-error "value cannot be extracted" }
}

consteval bool
throws_p (std::meta::info r)
{
  try { extract<const int &> (r); }
  catch (std::meta::exception &) { return true; }
  return false;
}

int g;
constexpr const int &cr = g;

void
f (const int &x, int y)
{
  static_assert (throws_p (^^x));
  static_assert (throws_p (^^y));
  const int &lx = x;
  static_assert (throws_p (^^lx));
  int &lg = g;
  static_assert (!throws_p (^^lg));
  static_assert (throws_p (^^g));
  int a = 42;
  int &la = a;
  static_assert (!throws_p (^^la)); // { dg-bogus "" "P2686" { xfail *-*-* } }
}

consteval bool
lifetime ()
{
  int i = 0;
  int &ri = i;
  return &extract<int &> (^^ri) == &i;
}
static_assert (lifetime ());

consteval void
h (const int &p)
{
  int i = 42;
  const int &ri = i;
  extract<const int &> (^^i);
  extract<const int &> (^^ri);
  extract<const int &> (^^p);
}

consteval { h (g); }

consteval bool
erroneous ()
{
  int u;
  int &ru = u;
  return &extract<int &> (^^u) == &u && &extract<int &> (^^ru) == &u;
}
static_assert (erroneous ());

template <const int &> struct X { };
same_type<X<g>, X<extract<const int &> (object_of (^^g))>> s1;
same_type<X<g>, X<extract<int &> (object_of (^^g))>> s2;
