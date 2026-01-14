// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::extract.

#include <meta>

using namespace std::meta;

template<class, class> struct same_type;
template<class T> struct same_type<T, T> {};

static constexpr int i = 0;
constexpr const int &r = i;
static_assert (extract<const int &>(^^i) == i);
static_assert (extract<const int &>(^^i) == 0);
static_assert (&extract<const int &>(^^i) == &i);
same_type<decltype(extract<const int &>(^^i)), const int &> st1;
same_type<decltype(&extract<const int &>(^^i)), const int *> st2;
const int& p = extract<const int &>(^^i);
static_assert (extract<const int &>(^^r) == 0);
static_assert (extract<const int &>(^^r) == i);

static const int j = 0;
const int &rj = j;
static_assert (extract<const int &>(^^j) == j);
static_assert (extract<const int &>(^^j) == 0);
static_assert (&extract<const int &>(^^j) == &j);
same_type<decltype(extract<const int &>(^^j)), const int &> st3;
same_type<decltype(&extract<const int &>(^^j)), const int *> st4;
const int &q = extract<const int &>(^^j);
static_assert (extract<const int &>(^^rj) == 0);
static_assert (extract<const int &>(^^rj) == j);
static_assert (&extract<const int &>(^^rj) == &j);

int k = 1;
static_assert (&extract<int &>(^^k) == &k);
static_assert (&extract<const int &>(^^k) == &k);

int arr[5] {};
static_assert (&extract<int (&)[5]>(^^arr) == &arr);
static_assert (&extract<const int (&)[5]>(^^arr) == &arr);

struct S { int m; };
constexpr S s{42};
constexpr const S &rs = s;
static_assert (extract<const S &>(^^s).m == 42);
static_assert (&extract<const S &>(^^s) == &s);
same_type<decltype(extract<const S &>(^^s)), const S &> st5;
same_type<decltype(&extract<const S &>(^^s)), const S *> st6;
const S &ps = extract<const S &>(^^s);
static_assert (extract<const S &>(^^rs).m == 42);
static_assert (extract<const S &>(reflect_constant (S{42})).m == 42);
same_type<decltype(extract<const S &>(reflect_constant (S{42}))), const S &> st7;
same_type<decltype(&extract<const S &>(reflect_constant (S{42}))), const S *> st8;

S sr{42};
static_assert (&extract<S&>(^^sr) == &sr);
static_assert (&extract<const S&>(^^sr) == &sr);
static_assert (&extract<S&>(^^sr).m == &sr.m);
int &srm = sr.m;
static_assert (&extract<int&>(^^srm) == &sr.m);

consteval bool foo() {
   int i = 5;
   return &extract<int&>(^^i) == &i;
}
static_assert (foo ());

consteval int
bar (int arg)
{
  int val = 3;
  int &ref = extract<int &>(^^val);
  ref = 4;
  return val + extract<int>(^^arg);
}
static_assert (bar (5) == 9);

consteval const int &
baz ()
{
  return j;
}

void
doit ()
{
  constexpr auto r = reflect_object (baz ());
  static_assert (extract<const int &>(r) == 0);
}
