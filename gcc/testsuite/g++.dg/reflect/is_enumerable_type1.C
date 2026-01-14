// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_enumerable_type.

#include <meta>

using namespace std::meta;

constexpr info null_reflection;
void foo ();

static_assert (!is_enumerable_type (null_reflection));
static_assert (!is_enumerable_type (^^int));
static_assert (!is_enumerable_type (^^::));
static_assert (!is_enumerable_type (^^foo));

class S;
enum class E;
typedef E TE;
static_assert (!is_enumerable_type (^^S));
static_assert (!is_enumerable_type (^^E));
static_assert (!is_enumerable_type (^^TE));

template<typename> struct cls_tmpl {};
template<typename T> using cls_tmpl_alias = cls_tmpl<T>;
static_assert (!is_enumerable_type (^^cls_tmpl));
static_assert (is_enumerable_type (^^cls_tmpl<int>));
static_assert (!is_enumerable_type (^^cls_tmpl_alias));
static_assert (is_enumerable_type (^^cls_tmpl_alias<int>));

class S {
  void foo ()
  {
    static_assert (is_enumerable_type (^^S));
  }
  static_assert (!is_enumerable_type (^^S));
};
static_assert (is_enumerable_type (^^S));

enum class E {
  A = (is_enumerable_type (^^E) || is_enumerable_type (^^TE)) ? 1 : 2
};
static_assert (is_enumerable_type (^^E));
static_assert (static_cast <int> (E::A) == 2);
static_assert (is_enumerable_type (^^TE));

enum F : int;
using TF = F;
static_assert (!is_enumerable_type (^^F));
static_assert (!is_enumerable_type (^^TF));
enum F : int {
  B = (is_enumerable_type (^^F) || is_enumerable_type (^^TF)) ? 3 : 4
};
static_assert (is_enumerable_type (^^F));
static_assert (B == 4);
static_assert (is_enumerable_type (^^TF));

enum G {
  C = is_enumerable_type (^^G) ? 5 : 6
};
static_assert (is_enumerable_type (^^G));
static_assert (C == 6);

enum H : int;
typedef H TH;
static_assert (!is_enumerable_type (^^H));
static_assert (!is_enumerable_type (^^TH));

enum H : int {};
static_assert (is_enumerable_type (^^H));
static_assert (is_enumerable_type (^^TH));

enum I : short;
using TI = I;
static_assert (!is_enumerable_type (^^I));
static_assert (!is_enumerable_type (^^TI));
enum I : short {};
static_assert (is_enumerable_type (^^I));
static_assert (is_enumerable_type (^^TI));

template <typename T>
void
qux ()
{
  enum J : T;
  using K = J;
  // FIXME: No idea if this is supposed to be false or true.
  // We certainly during instantiation don't differentiate between
  // forward enum declarations and later definitions.
  //  static_assert (!is_enumerable_type (^^J));
  //  static_assert (!is_enumerable_type (^^K));
  enum J : T {
    D = (is_enumerable_type (^^J) || is_enumerable_type (^^K))
	? sizeof (T) * 2 : sizeof (T)
  };
  static_assert (is_enumerable_type (^^J));
  static_assert (D == sizeof (T));
  static_assert (is_enumerable_type (^^K));
}

void
corge ()
{
  qux <int> ();
  qux <unsigned char> ();
}
