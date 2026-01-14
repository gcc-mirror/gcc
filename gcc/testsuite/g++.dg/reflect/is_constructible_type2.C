// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_constructible_type.

#include <array>
#include <list>
#include <meta>
#include <ranges>
#include <span>
#include <vector>

using namespace std::meta;

template <class... Args>
struct FromArgs { FromArgs (Args...); };

static_assert (!is_constructible_type (^^FromArgs <int &, long &, const double &>, {}));
static_assert (is_constructible_type (^^FromArgs <int &, long &, const double &>, { ^^int &, ^^long &, ^^const double & }));
constexpr info arr1[6] = { ^^double, ^^int &, ^^unsigned long long &, ^^int *, ^^short &, ^^char & };
static_assert (is_constructible_type (^^FromArgs <double, int &, unsigned long long &, int *, short &, char &>, arr1));
constexpr std::span <const info> sp1 (arr1);
static_assert (is_constructible_type (^^FromArgs <double, int &, unsigned long long &, int *, short &, char &>, sp1));
static_assert (is_constructible_type (^^FromArgs <int &, unsigned long long &, int *>, sp1.subspan (1, 3)));
static_assert (is_constructible_type (^^FromArgs <double, int &, unsigned long long &, int *, short &>, sp1.first (5)));
static_assert (is_constructible_type (^^FromArgs <int *, unsigned long long &, int &>, sp1.subspan (1, 3) | std::views::reverse));
static_assert (is_constructible_type (^^FromArgs <short &, int *, unsigned long long &, int &, double>, sp1.first (5) | std::views::reverse));
static_assert (!is_constructible_type (^^FromArgs <double, int &, unsigned long long &, int *, short &>, sp1.first (5) | std::views::reverse));

consteval bool
foo ()
{
  auto a = std::vector <info> { ^^int &, ^^long &, ^^long long & };
  if (!is_constructible_type (^^FromArgs <int &, long &, long long &>, a))
    return false;
  auto b = std::vector <info> { ^^short &, ^^double &, ^^float &, ^^char & };
  if (!is_constructible_type (^^FromArgs <short &, double &, float &, char &>, b))
    return false;
  auto c = std::array { ^^long &, ^^short &, ^^char &, ^^int };
  if (!is_constructible_type (^^FromArgs <long &, short &, char &, int>, c))
    return false;
  if (!is_constructible_type (^^FromArgs <long long &, long &, int &>, a | std::views::reverse))
    return false;
  if (!is_constructible_type (^^FromArgs <char &, float &, double &, short &>, b | std::views::reverse))
    return false;
  if (!is_constructible_type (^^FromArgs <int, char &, short &, long &>, c | std::views::reverse))
    return false;
  return true;
}
static_assert (foo ());

struct G
{
  consteval const info *begin () const { return &arr1[0]; }
  consteval const info *end () const { return &arr1[4]; }
};
static_assert (is_constructible_type (^^FromArgs <double, int &, unsigned long long &, int *>, G {}));

struct H
{
  consteval const info *begin () const { return &arr1[1]; }
  consteval const info *end () const { return &arr1[4]; }
};
static_assert (is_constructible_type (^^FromArgs <int &, unsigned long long &, int *>, H {}));

struct I
{
  using difference_type = std::ptrdiff_t;
  using value_type = info;
  consteval info operator * () const {
    if (p == 0) return arr1[2];
    else if (p == 1) return arr1[0];
    else if (p == 2) return arr1[1];
    else return arr1[3];
  }
  constexpr I &operator ++ () { ++p; return *this; }
  constexpr void operator ++ (int) { ++*this; }
  constexpr bool operator == (const I &x) const { return p == x.p; }
  int p;
};

struct J
{
  constexpr I begin () const { return I { 0 }; }
  constexpr I end () const { return I { 4 }; }
};
static_assert (is_constructible_type (^^FromArgs <unsigned long long &, double, int &, int *>, J {}));

struct K
{
  constexpr K () : q (new int) {}
  constexpr K (const K &) : q (new int) {}
  constexpr K &operator = (const K &) { return *this; }
  constexpr ~K () { delete q; }
  int *q;
};

template <typename T, T E>
struct L
{
  using difference_type = std::ptrdiff_t;
  using value_type = T;
  constexpr T &operator * () const { return *p; }
  constexpr L (T *x) : p (x), q (new int) {}
  constexpr L (const L &x) : p (x.p), q (new int) {}
  constexpr L &operator = (const L &x) { p = x.p; return *this; }
  constexpr ~L () { delete q; }
  constexpr L &operator ++ () { ++p; return *this; }
  constexpr void operator ++ (int) { ++*this; }
  constexpr bool operator == (const L &x) const { return p == x.p; }
  constexpr bool operator == (const K &) const { return *p == E; }
  T *p;
  int *q;
};

template <typename T, T E>
struct M
{
  constexpr M (T *x) : p (x), q (new int) {}
  constexpr M (const M &x) : p (x.p), q (new int) {}
  constexpr M &operator = (const M &x) { p = x.p; return *this; }
  constexpr ~M () { delete q; }
  constexpr L <T, E> begin () const { return L <T, E> (p); }
  constexpr K end () const { return K (); }
  T *p;
  int *q;
};

consteval bool
bar ()
{
  info a[] = { ^^int &, ^^short &, ^^long &, ^^unsigned &, ^^char &, ^^void, ^^int };
  auto b = M <info, ^^void> (&a[0]);
  if (!is_constructible_type (^^FromArgs <int &, short &, long &, unsigned &, char &>, b))
    return false;
  return true;
}
static_assert (bar ());
