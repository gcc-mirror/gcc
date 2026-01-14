// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::enumerators_of.

#include <meta>

using namespace std::meta;

constexpr info null_reflection;
void foo ();

consteval bool
has_enumerators_of (info r)
{
  try { enumerators_of (r); }
  catch (std::meta::exception &) { return false; }
  return true;
}

static_assert (!has_enumerators_of (null_reflection));
static_assert (!has_enumerators_of (^^int));
static_assert (!has_enumerators_of (^^::));
static_assert (!has_enumerators_of (^^foo));

class S;
enum class E;
typedef E TE;
static_assert (!has_enumerators_of (^^S));
static_assert (!has_enumerators_of (^^E));
static_assert (!has_enumerators_of (^^TE));

template<typename> struct cls_tmpl {};
template<typename T> using cls_tmpl_alias = cls_tmpl<T>;
static_assert (!has_enumerators_of (^^cls_tmpl));
static_assert (!has_enumerators_of (^^cls_tmpl<int>));
static_assert (!has_enumerators_of (^^cls_tmpl_alias));
static_assert (!has_enumerators_of (^^cls_tmpl_alias<int>));

class S {
  void foo ()
  {
    static_assert (!has_enumerators_of (^^S));
  }
  static_assert (!has_enumerators_of (^^S));
};
static_assert (!has_enumerators_of (^^S));

enum class E {
//  A1 = (has_enumerators_of (^^E) || has_enumerators_of (^^TE)) ? 1 : 2,
  A2 = has_enumerators_of (^^E) ? 10 : 20,
  A3 = has_enumerators_of (^^TE) ? 20 : 30
};
static_assert (has_enumerators_of (^^E));
//static_assert (static_cast <int> (E::A1) == 2);
static_assert (static_cast <int> (E::A2) == 20);
static_assert (static_cast <int> (E::A3) == 30);
static_assert (has_enumerators_of (^^TE));

enum F : int;
using TF = F;
static_assert (!has_enumerators_of (^^F));
static_assert (!has_enumerators_of (^^TF));
enum F : int {
//  B1 = (has_enumerators_of (^^F) || has_enumerators_of (^^TF)) ? 3 : 4,
  B2 = has_enumerators_of (^^F) ? 30 : 40,
  B3 = has_enumerators_of (^^TF) ? 40 : 42
};
static_assert (has_enumerators_of (^^F));
//static_assert (B1 == 4);
static_assert (B2 == 40);
static_assert (B3 == 42);
static_assert (has_enumerators_of (^^TF));

enum G {
  C = has_enumerators_of (^^G) ? 5 : 6
};
static_assert (has_enumerators_of (^^G));
static_assert (C == 6);

enum H : int;
typedef H TH;
static_assert (!has_enumerators_of (^^H));
static_assert (!has_enumerators_of (^^TH));

enum H : int {};
static_assert (has_enumerators_of (^^H));
static_assert (has_enumerators_of (^^TH));

enum I : short;
using TI = I;
static_assert (!has_enumerators_of (^^I));
static_assert (!has_enumerators_of (^^TI));
enum I : short {};
static_assert (has_enumerators_of (^^I));
static_assert (has_enumerators_of (^^TI));

template <typename T>
void
qux ()
{
  enum J : T;
  using K = J;
  // FIXME: No idea if this is supposed to be false or true.
  // We certainly during instantiation don't differentiate between
  // forward enum declarations and later definitions.
  //  static_assert (!has_enumerators_of (^^J));
  //  static_assert (!has_enumerators_of (dealias(^^K)));
  enum J : T {
    D = (has_enumerators_of (^^J) || has_enumerators_of (^^K))
	? sizeof (T) * 2 : sizeof (T)
  };
  static_assert (has_enumerators_of (^^J));
  static_assert (D == sizeof (T));
  static_assert (has_enumerators_of (^^K));
}

void
corge ()
{
  qux <int> ();
  qux <unsigned char> ();
}

enum L : int;
using TL = L;
static_assert (!has_enumerators_of (^^L));
static_assert (!has_enumerators_of (^^TL));
enum L : int {
  L0, L1 = 42, L2, L3 = 14, L4 = 16, L6 = 18, L5 = 24
};
static_assert (enumerators_of (^^L).size () == 7);
static_assert (enumerators_of (^^L)[0] == ^^L0);
static_assert (enumerators_of (^^L)[1] == ^^L1);
static_assert (enumerators_of (^^L)[2] == ^^L2);
static_assert (enumerators_of (^^L)[3] == ^^L3);
static_assert (enumerators_of (^^L)[4] == ^^L4);
static_assert (enumerators_of (^^L)[5] == ^^L6);
static_assert (enumerators_of (^^L)[6] == ^^L5);
static_assert (enumerators_of (^^TL).size () == 7);
static_assert (enumerators_of (^^TL)[0] == ^^L0);
static_assert (enumerators_of (^^TL)[1] == ^^L1);
static_assert (enumerators_of (^^TL)[2] == ^^L2);
static_assert (enumerators_of (^^TL)[3] == ^^L3);
static_assert (enumerators_of (^^TL)[4] == ^^L4);
static_assert (enumerators_of (^^TL)[5] == ^^L6);
static_assert (enumerators_of (^^TL)[6] == ^^L5);

enum M {
  M5, M13, M2 = 8, M0, M17
};
static_assert (enumerators_of (^^M).size () == 5);
static_assert (enumerators_of (^^M)[0] == ^^M5);
static_assert (enumerators_of (^^M)[1] == ^^M13);
static_assert (enumerators_of (^^M)[2] == ^^M2);
static_assert (enumerators_of (^^M)[3] == ^^M0);
static_assert (enumerators_of (^^M)[4] == ^^M17);
static_assert (enumerators_of (parent_of (^^M5)).size () == 5);

enum N : int;
typedef N TN;
static_assert (!has_enumerators_of (^^N));
static_assert (!has_enumerators_of (^^TN));

enum N : int { N1 = 5 };
static_assert (enumerators_of (^^N).size () == 1);
static_assert (enumerators_of (^^N)[0] == ^^N1);
static_assert (enumerators_of (^^TN).size () == 1);
static_assert (enumerators_of (^^TN)[0] == ^^N1);

enum O : short;
using TO = O;
static_assert (!has_enumerators_of (^^O));
static_assert (!has_enumerators_of (^^TO));
enum O : short { O1 = 2, O2 = 3, O3 = 4};
static_assert (enumerators_of (^^O).size () == 3);
static_assert (enumerators_of (^^O)[0] == ^^O1);
static_assert (enumerators_of (^^O)[1] == ^^O2);
static_assert (enumerators_of (^^O)[2] == ^^O3);
static_assert (enumerators_of (^^TO).size () == 3);
static_assert (enumerators_of (^^TO)[0] == ^^O1);
static_assert (enumerators_of (^^TO)[1] == ^^O2);
static_assert (enumerators_of (^^TO)[2] == ^^O3);

enum { P1, P2, P3 };
static_assert (enumerators_of (parent_of (^^P1)).size() == 3);
static_assert (enumerators_of (parent_of (^^P1))[0] == ^^P1);
static_assert (enumerators_of (parent_of (^^P1))[1] == ^^P2);
static_assert (enumerators_of (parent_of (^^P1))[2] == ^^P3);

