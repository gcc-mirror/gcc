// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::{,{,non}static_data_}members_of and
// has_inaccessible_nonstatic_data_members.

#include <meta>
#include <ranges>

using namespace std::meta;
using std::views::filter;
using std::ranges::to;
using std::vector;
using std::string_view;

consteval bool
check_special_members (const vector <info> &vec, int cnt, bool dct, bool cct, bool cas, bool mct, bool mas, bool dt)
{
  if ((vec | filter (is_special_member_function) | filter (is_defaulted) | to <vector> ()).size () != cnt)
    throw 1;
  if ((vec | filter (is_default_constructor) | to <vector> ()).size () != dct)
    throw 2;
  if ((vec | filter (is_copy_constructor) | to <vector> ()).size () != cct)
    throw 3;
  if ((vec | filter (is_copy_assignment) | to <vector> ()).size () != cas)
    throw 4;
  if ((vec | filter (is_move_constructor) | to <vector> ()).size () != mct)
    throw 5;
  if ((vec | filter (is_move_assignment) | to <vector> ()).size () != mas)
    throw 6;
  if ((vec | filter (is_destructor) | to <vector> ()).size () != dt)
    throw 7;
  return true;
}

namespace N1 {
  struct A {
    int a;
    static int b;
    enum E { E0 };
    using c = int;
    typedef long d;
    consteval {
    }
  };
  int A::b = 24;
  consteval {
  }
}
namespace N2 {
  struct B {};
  struct C { int : 5; };
  struct D { D (int = 42); int d; };
  struct E { E (int); int e; };
  struct F { int f; F (const F &); };
  struct G { G &operator = (const G &); int g; };
  struct H { int h; H (H &&); };
  struct I { I &operator = (I &&); int i; };
  struct J { ~J (); };
  struct K {
    int a;
    static int b;
    enum E { E0 };
    using c = int;
    typedef long d;
    template <typename T>
    struct A {};
    template <typename T>
    static consteval bool foo (const T &) { return true; }
    template <int N>
    static constexpr int e = N;
    template <typename T>
    using f = const T &;
    void bar () {}
    auto baz ();
    int g : 3;
    long : 2;
    int : 0;
    consteval {
      A <int> z = {};
      static_assert (e <42> == 42);
      f <int> w = 42;
    }
  protected:
    int Xa;
    static int Xb;
    enum XE { XE0 };
    using Xc = int;
    typedef long Xd;
    template <typename T>
    struct XA {};
    template <typename T>
    static consteval bool Xfoo (const T &) { return true; }
    template <int N>
    static constexpr int Xe = N;
    template <typename T>
    using Xf = const T &;
    void Xbar () {}
    auto Xbaz ();
    int Xg : 5;
    long : 4;
    int : 0;
  private:
    int Ya;
    static int Yb;
    enum YE { YE0 };
    using Yc = int;
    typedef long Yd;
    template <typename T>
    struct YA {};
    template <typename T>
    static consteval bool Yfoo (const T &) { return true; }
    template <int N>
    static constexpr int Ye = N;
    template <typename T>
    using Yf = const T &;
    void Ybar () {}
    auto Ybaz ();
    int Yg : 7;
    long : 6;
    int : 0;
  public:
    static constexpr info rXa = ^^Xa;
    static constexpr info rXb = ^^Xb;
    static constexpr info rXE = ^^XE;
    static constexpr info rXg = ^^Xg;
    static constexpr info rYa = ^^Ya;
    static constexpr info rYb = ^^Yb;
    static constexpr info rYE = ^^YE;
    static constexpr info rYg = ^^Yg;
  };
  namespace W {}
  namespace X = W;
  namespace W {}
  namespace W {}
  inline namespace Y { int a; }
  template <typename T>
  concept Z = requires { true; };
}
static_assert (N2::K::foo (42) && N2::K::foo (42UL));

constexpr access_context gctx = access_context::current ();
constexpr access_context uctx = access_context::unchecked ();

static_assert (members_of (^^N1::A, gctx).size () == 11);
static_assert (members_of (^^N1::A, gctx)[0] == ^^N1::A::a);
static_assert (members_of (^^N1::A, gctx)[1] == ^^N1::A::b);
static_assert (members_of (^^N1::A, gctx)[2] == ^^N1::A::E);
static_assert (members_of (^^N1::A, gctx)[3] == ^^N1::A::c);
static_assert (members_of (^^N1::A, gctx)[4] == ^^N1::A::d);
static_assert (check_special_members (members_of (^^N1::A, gctx), 6, true, true, true, true, true, true));
static_assert (static_data_members_of (^^N1::A, gctx).size () == 1);
static_assert (static_data_members_of (^^N1::A, gctx)[0] == ^^N1::A::b);
static_assert (nonstatic_data_members_of (^^N1::A, gctx).size () == 1);
static_assert (nonstatic_data_members_of (^^N1::A, gctx)[0] == ^^N1::A::a);
static_assert (!has_inaccessible_nonstatic_data_members (^^N1::A, gctx));

static_assert (members_of (^^N1, gctx).size () == 1);
static_assert (members_of (^^N1, gctx)[0] == ^^N1::A);

static_assert (members_of (^^N2::B, gctx).size () == 6);
static_assert (check_special_members (members_of (^^N2::B, gctx), 6, true, true, true, true, true, true));
static_assert (static_data_members_of (^^N2::B, gctx).size () == 0);
static_assert (nonstatic_data_members_of (^^N2::B, gctx).size () == 0);
static_assert (!has_inaccessible_nonstatic_data_members (^^N2::B, gctx));

static_assert (members_of (^^N2::C, gctx).size () == 7);
static_assert (is_bit_field (members_of (^^N2::C, gctx)[0]));
static_assert (!has_identifier (members_of (^^N2::C, gctx)[0]));
static_assert (bit_size_of (members_of (^^N2::C, gctx)[0]) == 5);
static_assert (type_of (members_of (^^N2::C, gctx)[0]) == ^^int);
static_assert (check_special_members (members_of (^^N2::C, gctx), 6, true, true, true, true, true, true));
static_assert (static_data_members_of (^^N2::C, gctx).size () == 0);
static_assert (nonstatic_data_members_of (^^N2::C, gctx).size () == 0);
static_assert (!has_inaccessible_nonstatic_data_members (^^N2::C, gctx));

static_assert (members_of (^^N2::D, gctx).size () == 7);
static_assert (is_default_constructor (members_of (^^N2::D, gctx)[0]));
static_assert (members_of (^^N2::D, gctx)[1] == ^^N2::D::d);
static_assert (check_special_members (members_of (^^N2::D, gctx), 5, true, true, true, true, true, true));
static_assert (static_data_members_of (^^N2::D, gctx).size () == 0);
static_assert (nonstatic_data_members_of (^^N2::D, gctx).size () == 1);
static_assert (nonstatic_data_members_of (^^N2::D, gctx)[0] == ^^N2::D::d);
static_assert (!has_inaccessible_nonstatic_data_members (^^N2::D, gctx));

static_assert (members_of (^^N2::E, gctx).size () == 7);
static_assert (is_constructor (members_of (^^N2::E, gctx)[0]));
static_assert (members_of (^^N2::E, gctx)[1] == ^^N2::E::e);
static_assert (check_special_members (members_of (^^N2::E, gctx), 5, false, true, true, true, true, true));
static_assert (static_data_members_of (^^N2::E, gctx).size () == 0);
static_assert (nonstatic_data_members_of (^^N2::E, gctx).size () == 1);
static_assert (nonstatic_data_members_of (^^N2::E, gctx)[0] == ^^N2::E::e);
static_assert (!has_inaccessible_nonstatic_data_members (^^N2::E, gctx));

static_assert (members_of (^^N2::F, gctx).size () == 4);
static_assert (members_of (^^N2::F, gctx)[0] == ^^N2::F::f);
static_assert (is_copy_constructor (members_of (^^N2::F, gctx)[1]));
static_assert (check_special_members (members_of (^^N2::F, gctx), 2, false, true, true, false, false, true));
static_assert (static_data_members_of (^^N2::F, gctx).size () == 0);
static_assert (nonstatic_data_members_of (^^N2::F, gctx).size () == 1);
static_assert (nonstatic_data_members_of (^^N2::F, gctx)[0] == ^^N2::F::f);
static_assert (!has_inaccessible_nonstatic_data_members (^^N2::F, gctx));

static_assert (members_of (^^N2::G, gctx).size () == 5);
static_assert (is_copy_assignment (members_of (^^N2::G, gctx)[0]));
static_assert (members_of (^^N2::G, gctx)[1] == ^^N2::G::g);
static_assert (check_special_members (members_of (^^N2::G, gctx), 3, true, true, true, false, false, true));
static_assert (static_data_members_of (^^N2::G, gctx).size () == 0);
static_assert (nonstatic_data_members_of (^^N2::G, gctx).size () == 1);
static_assert (nonstatic_data_members_of (^^N2::G, gctx)[0] == ^^N2::G::g);
static_assert (!has_inaccessible_nonstatic_data_members (^^N2::G, gctx));

static_assert (members_of (^^N2::H, gctx).size () == 5);
static_assert (members_of (^^N2::H, gctx)[0] == ^^N2::H::h);
static_assert (is_move_constructor (members_of (^^N2::H, gctx)[1]));
static_assert (check_special_members (members_of (^^N2::H, gctx), 3, false, true, true, true, false, true));
static_assert (static_data_members_of (^^N2::H, gctx).size () == 0);
static_assert (nonstatic_data_members_of (^^N2::H, gctx).size () == 1);
static_assert (nonstatic_data_members_of (^^N2::H, gctx)[0] == ^^N2::H::h);
static_assert (!has_inaccessible_nonstatic_data_members (^^N2::H, gctx));

static_assert (members_of (^^N2::I, gctx).size () == 6);
static_assert (is_move_assignment (members_of (^^N2::I, gctx)[0]));
static_assert (members_of (^^N2::I, gctx)[1] == ^^N2::I::i);
static_assert (check_special_members (members_of (^^N2::I, gctx), 4, true, true, true, false, true, true));
static_assert (static_data_members_of (^^N2::I, gctx).size () == 0);
static_assert (nonstatic_data_members_of (^^N2::I, gctx).size () == 1);
static_assert (nonstatic_data_members_of (^^N2::I, gctx)[0] == ^^N2::I::i);
static_assert (!has_inaccessible_nonstatic_data_members (^^N2::I, gctx));

static_assert (members_of (^^N2::J, gctx).size () == 4);
static_assert (is_destructor (members_of (^^N2::J, gctx)[0]));
static_assert (check_special_members (members_of (^^N2::J, gctx), 3, true, true, true, false, false, true));
static_assert (static_data_members_of (^^N2::J, gctx).size () == 0);
static_assert (nonstatic_data_members_of (^^N2::J, gctx).size () == 0);
static_assert (!has_inaccessible_nonstatic_data_members (^^N2::J, gctx));

static_assert (members_of (^^N2::K, gctx).size () == 27);
static_assert (members_of (^^N2::K, gctx)[0] == ^^N2::K::a);
static_assert (members_of (^^N2::K, gctx)[1] == ^^N2::K::b);
static_assert (members_of (^^N2::K, gctx)[2] == ^^N2::K::E);
static_assert (members_of (^^N2::K, gctx)[3] == ^^N2::K::c);
static_assert (members_of (^^N2::K, gctx)[4] == ^^N2::K::d);
static_assert (members_of (^^N2::K, gctx)[5] == ^^N2::K::A);
static_assert (members_of (^^N2::K, gctx)[6] == ^^N2::K::foo);
static_assert (members_of (^^N2::K, gctx)[7] == ^^N2::K::e);
static_assert (members_of (^^N2::K, gctx)[8] == ^^N2::K::f);
static_assert (members_of (^^N2::K, gctx)[9] == ^^N2::K::bar);
static_assert (members_of (^^N2::K, gctx)[10] == ^^N2::K::g);
static_assert (is_bit_field (members_of (^^N2::K, gctx)[11]));
static_assert (!has_identifier (members_of (^^N2::K, gctx)[11]));
static_assert (bit_size_of (members_of (^^N2::K, gctx)[11]) == 2);
static_assert (type_of (members_of (^^N2::K, gctx)[11]) == ^^long);
static_assert (is_bit_field (members_of (^^N2::K, gctx)[12]));
static_assert (!has_identifier (members_of (^^N2::K, gctx)[12]));
static_assert (bit_size_of (members_of (^^N2::K, gctx)[12]) == 0);
static_assert (type_of (members_of (^^N2::K, gctx)[12]) == ^^int);
static_assert (members_of (^^N2::K, gctx)[13] == ^^N2::K::rXa);
static_assert (members_of (^^N2::K, gctx)[14] == ^^N2::K::rXb);
static_assert (members_of (^^N2::K, gctx)[15] == ^^N2::K::rXE);
static_assert (members_of (^^N2::K, gctx)[16] == ^^N2::K::rXg);
static_assert (members_of (^^N2::K, gctx)[17] == ^^N2::K::rYa);
static_assert (members_of (^^N2::K, gctx)[18] == ^^N2::K::rYb);
static_assert (members_of (^^N2::K, gctx)[19] == ^^N2::K::rYE);
static_assert (members_of (^^N2::K, gctx)[20] == ^^N2::K::rYg);
static_assert (check_special_members (members_of (^^N2::K, gctx), 6, true, true, true, true, true, true));
static_assert (static_data_members_of (^^N2::K, gctx).size () == 9);
static_assert (static_data_members_of (^^N2::K, gctx)[0] == ^^N2::K::b);
static_assert (static_data_members_of (^^N2::K, gctx)[1] == ^^N2::K::rXa);
static_assert (static_data_members_of (^^N2::K, gctx)[2] == ^^N2::K::rXb);
static_assert (static_data_members_of (^^N2::K, gctx)[3] == ^^N2::K::rXE);
static_assert (static_data_members_of (^^N2::K, gctx)[4] == ^^N2::K::rXg);
static_assert (static_data_members_of (^^N2::K, gctx)[5] == ^^N2::K::rYa);
static_assert (static_data_members_of (^^N2::K, gctx)[6] == ^^N2::K::rYb);
static_assert (static_data_members_of (^^N2::K, gctx)[7] == ^^N2::K::rYE);
static_assert (static_data_members_of (^^N2::K, gctx)[8] == ^^N2::K::rYg);
static_assert (nonstatic_data_members_of (^^N2::K, gctx).size () == 2);
static_assert (nonstatic_data_members_of (^^N2::K, gctx)[0] == ^^N2::K::a);
static_assert (nonstatic_data_members_of (^^N2::K, gctx)[1] == ^^N2::K::g);
static_assert (has_inaccessible_nonstatic_data_members (^^N2::K, gctx));
static_assert (members_of (^^N2::K, uctx).size () == 53);
static_assert (members_of (^^N2::K, uctx)[0] == ^^N2::K::a);
static_assert (members_of (^^N2::K, uctx)[1] == ^^N2::K::b);
static_assert (members_of (^^N2::K, uctx)[2] == ^^N2::K::E);
static_assert (members_of (^^N2::K, uctx)[3] == ^^N2::K::c);
static_assert (members_of (^^N2::K, uctx)[4] == ^^N2::K::d);
static_assert (members_of (^^N2::K, uctx)[5] == ^^N2::K::A);
static_assert (members_of (^^N2::K, uctx)[6] == ^^N2::K::foo);
static_assert (members_of (^^N2::K, uctx)[7] == ^^N2::K::e);
static_assert (members_of (^^N2::K, uctx)[8] == ^^N2::K::f);
static_assert (members_of (^^N2::K, uctx)[9] == ^^N2::K::bar);
static_assert (members_of (^^N2::K, uctx)[10] == ^^N2::K::g);
static_assert (is_bit_field (members_of (^^N2::K, uctx)[11]));
static_assert (!has_identifier (members_of (^^N2::K, uctx)[11]));
static_assert (bit_size_of (members_of (^^N2::K, uctx)[11]) == 2);
static_assert (type_of (members_of (^^N2::K, uctx)[11]) == ^^long);
static_assert (is_bit_field (members_of (^^N2::K, uctx)[12]));
static_assert (!has_identifier (members_of (^^N2::K, uctx)[12]));
static_assert (bit_size_of (members_of (^^N2::K, uctx)[12]) == 0);
static_assert (type_of (members_of (^^N2::K, uctx)[12]) == ^^int);
static_assert (members_of (^^N2::K, uctx)[13] == N2::K::rXa);
static_assert (members_of (^^N2::K, uctx)[14] == N2::K::rXb);
static_assert (members_of (^^N2::K, uctx)[15] == N2::K::rXE);
static_assert (identifier_of (members_of (^^N2::K, uctx)[16]) == string_view ("Xc"));
static_assert (identifier_of (members_of (^^N2::K, uctx)[17]) == string_view ("Xd"));
static_assert (identifier_of (members_of (^^N2::K, uctx)[18]) == string_view ("XA"));
static_assert (identifier_of (members_of (^^N2::K, uctx)[19]) == string_view ("Xfoo"));
static_assert (identifier_of (members_of (^^N2::K, uctx)[20]) == string_view ("Xe"));
static_assert (identifier_of (members_of (^^N2::K, uctx)[21]) == string_view ("Xf"));
static_assert (identifier_of (members_of (^^N2::K, uctx)[22]) == string_view ("Xbar"));
static_assert (members_of (^^N2::K, uctx)[23] == N2::K::rXg);
static_assert (is_bit_field (members_of (^^N2::K, uctx)[24]));
static_assert (!has_identifier (members_of (^^N2::K, uctx)[24]));
static_assert (bit_size_of (members_of (^^N2::K, uctx)[24]) == 4);
static_assert (type_of (members_of (^^N2::K, uctx)[24]) == ^^long);
static_assert (is_bit_field (members_of (^^N2::K, uctx)[25]));
static_assert (!has_identifier (members_of (^^N2::K, uctx)[25]));
static_assert (bit_size_of (members_of (^^N2::K, uctx)[25]) == 0);
static_assert (type_of (members_of (^^N2::K, uctx)[25]) == ^^int);
static_assert (members_of (^^N2::K, uctx)[26] == N2::K::rYa);
static_assert (members_of (^^N2::K, uctx)[27] == N2::K::rYb);
static_assert (members_of (^^N2::K, uctx)[28] == N2::K::rYE);
static_assert (identifier_of (members_of (^^N2::K, uctx)[29]) == string_view ("Yc"));
static_assert (identifier_of (members_of (^^N2::K, uctx)[30]) == string_view ("Yd"));
static_assert (identifier_of (members_of (^^N2::K, uctx)[31]) == string_view ("YA"));
static_assert (identifier_of (members_of (^^N2::K, uctx)[32]) == string_view ("Yfoo"));
static_assert (identifier_of (members_of (^^N2::K, uctx)[33]) == string_view ("Ye"));
static_assert (identifier_of (members_of (^^N2::K, uctx)[34]) == string_view ("Yf"));
static_assert (identifier_of (members_of (^^N2::K, uctx)[35]) == string_view ("Ybar"));
static_assert (members_of (^^N2::K, uctx)[36] == N2::K::rYg);
static_assert (is_bit_field (members_of (^^N2::K, uctx)[37]));
static_assert (!has_identifier (members_of (^^N2::K, uctx)[37]));
static_assert (bit_size_of (members_of (^^N2::K, uctx)[37]) == 6);
static_assert (type_of (members_of (^^N2::K, uctx)[37]) == ^^long);
static_assert (is_bit_field (members_of (^^N2::K, uctx)[38]));
static_assert (!has_identifier (members_of (^^N2::K, uctx)[38]));
static_assert (bit_size_of (members_of (^^N2::K, uctx)[38]) == 0);
static_assert (type_of (members_of (^^N2::K, uctx)[38]) == ^^int);
static_assert (members_of (^^N2::K, uctx)[39] == ^^N2::K::rXa);
static_assert (members_of (^^N2::K, uctx)[40] == ^^N2::K::rXb);
static_assert (members_of (^^N2::K, uctx)[41] == ^^N2::K::rXE);
static_assert (members_of (^^N2::K, uctx)[42] == ^^N2::K::rXg);
static_assert (members_of (^^N2::K, uctx)[43] == ^^N2::K::rYa);
static_assert (members_of (^^N2::K, uctx)[44] == ^^N2::K::rYb);
static_assert (members_of (^^N2::K, uctx)[45] == ^^N2::K::rYE);
static_assert (members_of (^^N2::K, uctx)[46] == ^^N2::K::rYg);
static_assert (check_special_members (members_of (^^N2::K, uctx), 6, true, true, true, true, true, true));
static_assert (static_data_members_of (^^N2::K, uctx).size () == 11);
static_assert (static_data_members_of (^^N2::K, uctx)[0] == ^^N2::K::b);
static_assert (static_data_members_of (^^N2::K, uctx)[1] == N2::K::rXb);
static_assert (static_data_members_of (^^N2::K, uctx)[2] == N2::K::rYb);
static_assert (static_data_members_of (^^N2::K, uctx)[3] == ^^N2::K::rXa);
static_assert (static_data_members_of (^^N2::K, uctx)[4] == ^^N2::K::rXb);
static_assert (static_data_members_of (^^N2::K, uctx)[5] == ^^N2::K::rXE);
static_assert (static_data_members_of (^^N2::K, uctx)[6] == ^^N2::K::rXg);
static_assert (static_data_members_of (^^N2::K, uctx)[7] == ^^N2::K::rYa);
static_assert (static_data_members_of (^^N2::K, uctx)[8] == ^^N2::K::rYb);
static_assert (static_data_members_of (^^N2::K, uctx)[9] == ^^N2::K::rYE);
static_assert (static_data_members_of (^^N2::K, uctx)[10] == ^^N2::K::rYg);
static_assert (nonstatic_data_members_of (^^N2::K, uctx).size () == 6);
static_assert (nonstatic_data_members_of (^^N2::K, uctx)[0] == ^^N2::K::a);
static_assert (nonstatic_data_members_of (^^N2::K, uctx)[1] == ^^N2::K::g);
static_assert (nonstatic_data_members_of (^^N2::K, uctx)[2] == N2::K::rXa);
static_assert (nonstatic_data_members_of (^^N2::K, uctx)[3] == N2::K::rXg);
static_assert (nonstatic_data_members_of (^^N2::K, uctx)[4] == N2::K::rYa);
static_assert (nonstatic_data_members_of (^^N2::K, uctx)[5] == N2::K::rYg);
static_assert (!has_inaccessible_nonstatic_data_members (^^N2::K, uctx));

static_assert (members_of (^^N2, gctx).size () == 14);
static_assert ((members_of (^^N2, gctx) | filter (is_type) | filter (is_class_type) | to <vector> ()).size () == 10);
static_assert ((members_of (^^N2, gctx) | filter (is_namespace) | to <vector> ()).size () == 3);
static_assert ((members_of (^^N2, gctx) | filter (is_namespace_alias) | to <vector> ()).size () == 1);
static_assert ((members_of (^^N2, gctx) | filter (is_concept) | to <vector> ()).size () == 1);

namespace N2 {
  struct L : public K {
    static constexpr auto ctx = access_context::current ();
  };
  static constexpr auto ctx = L::ctx.via (^^L);
}

static_assert (members_of (^^N2::K, N2::ctx).size () == 40);
static_assert (members_of (^^N2::K, N2::ctx)[0] == ^^N2::K::a);
static_assert (members_of (^^N2::K, N2::ctx)[1] == ^^N2::K::b);
static_assert (members_of (^^N2::K, N2::ctx)[2] == ^^N2::K::E);
static_assert (members_of (^^N2::K, N2::ctx)[3] == ^^N2::K::c);
static_assert (members_of (^^N2::K, N2::ctx)[4] == ^^N2::K::d);
static_assert (members_of (^^N2::K, N2::ctx)[5] == ^^N2::K::A);
static_assert (members_of (^^N2::K, N2::ctx)[6] == ^^N2::K::foo);
static_assert (members_of (^^N2::K, N2::ctx)[7] == ^^N2::K::e);
static_assert (members_of (^^N2::K, N2::ctx)[8] == ^^N2::K::f);
static_assert (members_of (^^N2::K, N2::ctx)[9] == ^^N2::K::bar);
static_assert (members_of (^^N2::K, N2::ctx)[10] == ^^N2::K::g);
static_assert (is_bit_field (members_of (^^N2::K, N2::ctx)[11]));
static_assert (!has_identifier (members_of (^^N2::K, N2::ctx)[11]));
static_assert (bit_size_of (members_of (^^N2::K, N2::ctx)[11]) == 2);
static_assert (type_of (members_of (^^N2::K, N2::ctx)[11]) == ^^long);
static_assert (is_bit_field (members_of (^^N2::K, N2::ctx)[12]));
static_assert (!has_identifier (members_of (^^N2::K, N2::ctx)[12]));
static_assert (bit_size_of (members_of (^^N2::K, N2::ctx)[12]) == 0);
static_assert (type_of (members_of (^^N2::K, N2::ctx)[12]) == ^^int);
static_assert (members_of (^^N2::K, N2::ctx)[13] == N2::K::rXa);
static_assert (members_of (^^N2::K, N2::ctx)[14] == N2::K::rXb);
static_assert (members_of (^^N2::K, N2::ctx)[15] == N2::K::rXE);
static_assert (identifier_of (members_of (^^N2::K, N2::ctx)[16]) == string_view ("Xc"));
static_assert (identifier_of (members_of (^^N2::K, N2::ctx)[17]) == string_view ("Xd"));
static_assert (identifier_of (members_of (^^N2::K, N2::ctx)[18]) == string_view ("XA"));
static_assert (identifier_of (members_of (^^N2::K, N2::ctx)[19]) == string_view ("Xfoo"));
static_assert (identifier_of (members_of (^^N2::K, N2::ctx)[20]) == string_view ("Xe"));
static_assert (identifier_of (members_of (^^N2::K, N2::ctx)[21]) == string_view ("Xf"));
static_assert (identifier_of (members_of (^^N2::K, N2::ctx)[22]) == string_view ("Xbar"));
static_assert (members_of (^^N2::K, N2::ctx)[23] == N2::K::rXg);
static_assert (is_bit_field (members_of (^^N2::K, N2::ctx)[24]));
static_assert (!has_identifier (members_of (^^N2::K, N2::ctx)[24]));
static_assert (bit_size_of (members_of (^^N2::K, N2::ctx)[24]) == 4);
static_assert (type_of (members_of (^^N2::K, N2::ctx)[24]) == ^^long);
static_assert (is_bit_field (members_of (^^N2::K, N2::ctx)[25]));
static_assert (!has_identifier (members_of (^^N2::K, N2::ctx)[25]));
static_assert (bit_size_of (members_of (^^N2::K, N2::ctx)[25]) == 0);
static_assert (type_of (members_of (^^N2::K, N2::ctx)[25]) == ^^int);
static_assert (members_of (^^N2::K, N2::ctx)[26] == ^^N2::K::rXa);
static_assert (members_of (^^N2::K, N2::ctx)[27] == ^^N2::K::rXb);
static_assert (members_of (^^N2::K, N2::ctx)[28] == ^^N2::K::rXE);
static_assert (members_of (^^N2::K, N2::ctx)[29] == ^^N2::K::rXg);
static_assert (members_of (^^N2::K, N2::ctx)[30] == ^^N2::K::rYa);
static_assert (members_of (^^N2::K, N2::ctx)[31] == ^^N2::K::rYb);
static_assert (members_of (^^N2::K, N2::ctx)[32] == ^^N2::K::rYE);
static_assert (members_of (^^N2::K, N2::ctx)[33] == ^^N2::K::rYg);
static_assert (check_special_members (members_of (^^N2::K, N2::ctx), 6, true, true, true, true, true, true));
static_assert (static_data_members_of (^^N2::K, N2::ctx).size () == 10);
static_assert (static_data_members_of (^^N2::K, N2::ctx)[0] == ^^N2::K::b);
static_assert (static_data_members_of (^^N2::K, N2::ctx)[1] == N2::K::rXb);
static_assert (static_data_members_of (^^N2::K, N2::ctx)[2] == ^^N2::K::rXa);
static_assert (static_data_members_of (^^N2::K, N2::ctx)[3] == ^^N2::K::rXb);
static_assert (static_data_members_of (^^N2::K, N2::ctx)[4] == ^^N2::K::rXE);
static_assert (static_data_members_of (^^N2::K, N2::ctx)[5] == ^^N2::K::rXg);
static_assert (static_data_members_of (^^N2::K, N2::ctx)[6] == ^^N2::K::rYa);
static_assert (static_data_members_of (^^N2::K, N2::ctx)[7] == ^^N2::K::rYb);
static_assert (static_data_members_of (^^N2::K, N2::ctx)[8] == ^^N2::K::rYE);
static_assert (static_data_members_of (^^N2::K, N2::ctx)[9] == ^^N2::K::rYg);
static_assert (nonstatic_data_members_of (^^N2::K, N2::ctx).size () == 4);
static_assert (nonstatic_data_members_of (^^N2::K, N2::ctx)[0] == ^^N2::K::a);
static_assert (nonstatic_data_members_of (^^N2::K, N2::ctx)[1] == ^^N2::K::g);
static_assert (nonstatic_data_members_of (^^N2::K, N2::ctx)[2] == N2::K::rXa);
static_assert (nonstatic_data_members_of (^^N2::K, N2::ctx)[3] == N2::K::rXg);
static_assert (has_inaccessible_nonstatic_data_members (^^N2::K, N2::ctx));
static_assert (members_of (^^N2::L, gctx).size () == 7);
static_assert (members_of (^^N2::L, gctx)[0] == ^^N2::L::ctx);
static_assert (check_special_members (members_of (^^N2::L, gctx), 6, true, true, true, true, true, true));
static_assert (static_data_members_of (^^N2::L, gctx).size () == 1);
static_assert (static_data_members_of (^^N2::L, gctx)[0] == ^^N2::L::ctx);
static_assert (nonstatic_data_members_of (^^N2::L, gctx).size () == 0);
static_assert (!has_inaccessible_nonstatic_data_members (^^N2::L, gctx));

static_assert (members_of (^^N2, gctx).size () == 16);
static_assert ((members_of (^^N2, gctx) | filter (is_type) | filter (is_class_type) | to <vector> ()).size () == 11);
static_assert ((members_of (^^N2, gctx) | filter (is_namespace) | to <vector> ()).size () == 3);
static_assert ((members_of (^^N2, gctx) | filter (is_namespace_alias) | to <vector> ()).size () == 1);
static_assert ((members_of (^^N2, gctx) | filter (is_concept) | to <vector> ()).size () == 1);
static_assert ((members_of (^^N2, gctx) | filter (is_variable) | to <vector> ()).size () == 1);

namespace N3 {
  struct A {
    union { int a; };
  };

  struct B {
    static int foo (int);
    static long foo (long);
    static short foo (short);
    static int foo (int, int);
    static double foo (double, float);
  };

  template <typename, typename>
  constexpr bool c = false;

  template <typename T>
  constexpr bool c <T, T> = true;

  template <typename T>
  struct C {
    void foo () requires (c <T, int>);
    void bar () requires (c <T, long>);
    void baz () requires (c <T, long>);
  };
}

static_assert (members_of (^^N3::A, gctx).size () == 8);
static_assert (is_union_type (members_of (^^N3::A, gctx)[0]));
static_assert (!has_identifier (members_of (^^N3::A, gctx)[0]));
static_assert (is_nonstatic_data_member (members_of (^^N3::A, gctx)[1]));
static_assert (!has_identifier (members_of (^^N3::A, gctx)[1]));
static_assert (check_special_members (members_of (^^N3::A, gctx), 6, true, true, true, true, true, true));

static_assert (members_of (^^N3::B, gctx).size () == 11);
static_assert (is_function (members_of (^^N3::B, gctx)[0]));
static_assert (identifier_of (members_of (^^N3::B, gctx)[0]) == "foo");
static_assert (type_of (members_of (^^N3::B, gctx)[0]) == ^^int (int));
static_assert (is_function (members_of (^^N3::B, gctx)[1]));
static_assert (identifier_of (members_of (^^N3::B, gctx)[1]) == "foo");
static_assert (type_of (members_of (^^N3::B, gctx)[1]) == ^^long (long));
static_assert (is_function (members_of (^^N3::B, gctx)[2]));
static_assert (identifier_of (members_of (^^N3::B, gctx)[2]) == "foo");
static_assert (type_of (members_of (^^N3::B, gctx)[2]) == ^^short (short));
static_assert (is_function (members_of (^^N3::B, gctx)[3]));
static_assert (identifier_of (members_of (^^N3::B, gctx)[3]) == "foo");
static_assert (type_of (members_of (^^N3::B, gctx)[3]) == ^^int (int, int));
static_assert (is_function (members_of (^^N3::B, gctx)[4]));
static_assert (identifier_of (members_of (^^N3::B, gctx)[4]) == "foo");
static_assert (type_of (members_of (^^N3::B, gctx)[4]) == ^^double (double, float));
static_assert (check_special_members (members_of (^^N3::B, gctx), 6, true, true, true, true, true, true));

static_assert (members_of (^^N3::C <int>, gctx).size () == 7);
static_assert (is_function (members_of (^^N3::C <int>, gctx)[0]));
static_assert (identifier_of (members_of (^^N3::C <int>, gctx)[0]) == "foo");
static_assert (check_special_members (members_of (^^N3::C <int>, gctx), 6, true, true, true, true, true, true));
static_assert (members_of (^^N3::C <long>, gctx).size () == 8);
static_assert (is_function (members_of (^^N3::C <long>, gctx)[0]));
static_assert (identifier_of (members_of (^^N3::C <long>, gctx)[0]) == "bar");
static_assert (is_function (members_of (^^N3::C <long>, gctx)[1]));
static_assert (identifier_of (members_of (^^N3::C <long>, gctx)[1]) == "baz");
static_assert (check_special_members (members_of (^^N3::C <long>, gctx), 6, true, true, true, true, true, true));
static_assert (members_of (^^N3::C <short>, gctx).size () == 6);
static_assert (check_special_members (members_of (^^N3::C <short>, gctx), 6, true, true, true, true, true, true));
