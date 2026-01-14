// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_type_alias.

#include <meta>

using namespace std::meta;

typedef int I;
static_assert (is_type_alias (^^I));
static_assert (!is_type_alias (^^const I));
static_assert (!is_type_alias (^^I const));
static_assert (!is_type_alias (^^I &&));
static_assert (^^I != ^^int);
static_assert (^^const I == ^^const int);
static_assert (^^I const == ^^const int);
static_assert (^^I && == ^^int &&);

typedef const int J;
static_assert (is_type_alias (^^J));
static_assert (!is_type_alias (^^const J));
static_assert (!is_type_alias (^^J const));
static_assert (!is_type_alias (^^J &));
static_assert (^^J != ^^const int);
static_assert (^^const J == ^^const int);
static_assert (^^J const == ^^const int);
static_assert (^^J & == ^^const int &);

template <typename> struct cls_tmpl {};
template <typename T> using cls_tmpl_alias = const cls_tmpl <T>;

static_assert (is_type_alias (^^cls_tmpl_alias <int>));
static_assert (!is_type_alias (^^const cls_tmpl_alias <int>));
static_assert (!is_type_alias (^^cls_tmpl_alias <int> const));
static_assert (!is_type_alias (^^cls_tmpl_alias <int> &&));
static_assert (^^cls_tmpl_alias <int> != ^^const cls_tmpl <int>);
static_assert (^^cls_tmpl_alias <int> != ^^const cls_tmpl_alias <int>);
static_assert (^^const cls_tmpl_alias <int> == ^^const cls_tmpl <int>);
static_assert (^^cls_tmpl_alias <int> const == ^^const cls_tmpl <int>);
static_assert (^^cls_tmpl_alias <int> && == ^^const cls_tmpl <int> &&);

namespace N1
{
  template <typename> struct cls_tmpl {};
  template <typename T> using cls_tmpl_alias = const cls_tmpl <T>;

  static_assert (is_type_alias (^^N1::cls_tmpl_alias <int>));
  static_assert (!is_type_alias (^^const N1::cls_tmpl_alias <int>));
  static_assert (!is_type_alias (^^N1::cls_tmpl_alias <int> const));
  static_assert (!is_type_alias (^^N1::cls_tmpl_alias <int> &&));
  static_assert (^^N1::cls_tmpl_alias <int> != ^^const N1::cls_tmpl <int>);
  static_assert (^^N1::cls_tmpl_alias <int> != ^^const N1::cls_tmpl_alias <int>);
  static_assert (^^const N1::cls_tmpl_alias <int> == ^^const N1::cls_tmpl <int>);
  static_assert (^^N1::cls_tmpl_alias <int> const == ^^const N1::cls_tmpl <int>);
  static_assert (^^N1::cls_tmpl_alias <int> && == ^^const N1::cls_tmpl <int> &&);
  static_assert (is_type_alias (^^N1:: template cls_tmpl_alias <int>));
  static_assert (!is_type_alias (^^const N1:: template cls_tmpl_alias <int>));
  static_assert (!is_type_alias (^^N1:: template cls_tmpl_alias <int> const));
  static_assert (!is_type_alias (^^N1:: template cls_tmpl_alias <int> &&));
  static_assert (^^N1:: template cls_tmpl_alias <int> != ^^const N1:: template cls_tmpl <int>);
  static_assert (^^N1:: template cls_tmpl_alias <int> != ^^const N1:: template cls_tmpl_alias <int>);
  static_assert (^^const N1:: template cls_tmpl_alias <int> == ^^const N1:: template cls_tmpl <int>);
  static_assert (^^N1:: template cls_tmpl_alias <int> const == ^^const N1:: template cls_tmpl <int>);
  static_assert (^^N1:: template cls_tmpl_alias <int> && == ^^const N1:: template cls_tmpl <int> &&);
}

const I a = 42;
static_assert (!is_type_alias (type_of (^^a)));
static_assert (type_of (^^a) == ^^const int);

J b = 42;
static_assert (!is_type_alias (type_of (^^b)));
static_assert (type_of (^^b) == ^^const int);

cls_tmpl_alias <long> c;
static_assert (!is_type_alias (type_of (^^c)));
static_assert (type_of (^^c) == ^^const cls_tmpl <long>);
