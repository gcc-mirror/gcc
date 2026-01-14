// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::subobjects_of and has_inaccessible_subobjects.

#include <meta>

using namespace std::meta;

struct A { int a; };
struct B { int b; };
struct C { int c; };
struct D { int d; };
struct E { int e; };
struct F { int f; };
struct G { int g; };
struct H { int h; };
struct I : public A, protected B, private C { int i; };
struct J : protected D, private E, public F { int j; };
struct K : public G, protected I, private H, public J {
public:
  int Za;
  static int Zb;
  enum ZE { ZE0 };
  using Zc = int;
  typedef long Zd;
  template <typename T>
  struct ZA {};
  template <typename T>
  static consteval bool Zfoo (const T &) { return true; }
  template <int N>
  static constexpr int Ze = N;
  template <typename T>
  using Zf = const T &;
  void Zbar () {}
  auto Zbaz ();
  int Zg : 5;
  long : 4;
  int : 0;
  consteval {
    ZA <int> z = {};
    static_assert (Ze <42> == 42);
    Zf <int> w = 42;
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
};
struct L : virtual A { int l; };
struct M : virtual A { int m; };
struct N : public L, protected M, public virtual A { int n; };
struct O : public L, public M, public virtual A { int o; };
struct P : public L, public M, public virtual A { protected: int p; };

constexpr access_context gctx = access_context::current ();
constexpr access_context uctx = access_context::unchecked ();

static_assert (subobjects_of (^^A, uctx).size () == 1);
static_assert (subobjects_of (^^A, uctx)[0] == nonstatic_data_members_of (^^A, uctx)[0]);
static_assert (subobjects_of (^^B, uctx).size () == 1);
static_assert (subobjects_of (^^B, uctx)[0] == nonstatic_data_members_of (^^B, uctx)[0]);
static_assert (subobjects_of (^^C, uctx).size () == 1);
static_assert (subobjects_of (^^C, uctx)[0] == nonstatic_data_members_of (^^C, uctx)[0]);
static_assert (!has_inaccessible_subobjects (^^C, uctx));
static_assert (subobjects_of (^^D, uctx).size () == 1);
static_assert (subobjects_of (^^D, uctx)[0] == nonstatic_data_members_of (^^D, uctx)[0]);
static_assert (subobjects_of (^^E, uctx).size () == 1);
static_assert (subobjects_of (^^E, uctx)[0] == nonstatic_data_members_of (^^E, uctx)[0]);
static_assert (subobjects_of (^^F, uctx).size () == 1);
static_assert (subobjects_of (^^F, uctx)[0] == nonstatic_data_members_of (^^F, uctx)[0]);
static_assert (subobjects_of (^^G, uctx).size () == 1);
static_assert (subobjects_of (^^G, uctx)[0] == nonstatic_data_members_of (^^G, uctx)[0]);
static_assert (subobjects_of (^^H, uctx).size () == 1);
static_assert (subobjects_of (^^H, uctx)[0] == nonstatic_data_members_of (^^H, uctx)[0]);
static_assert (subobjects_of (^^I, uctx).size () == 4);
static_assert (subobjects_of (^^I, uctx)[0] == bases_of (^^I, uctx)[0]);
static_assert (subobjects_of (^^I, uctx)[1] == bases_of (^^I, uctx)[1]);
static_assert (subobjects_of (^^I, uctx)[2] == bases_of (^^I, uctx)[2]);
static_assert (subobjects_of (^^I, uctx)[3] == nonstatic_data_members_of (^^I, uctx)[0]);
static_assert (!has_inaccessible_subobjects (^^I, uctx));
static_assert (subobjects_of (^^J, uctx).size () == 4);
static_assert (subobjects_of (^^J, uctx)[0] == bases_of (^^J, uctx)[0]);
static_assert (subobjects_of (^^J, uctx)[1] == bases_of (^^J, uctx)[1]);
static_assert (subobjects_of (^^J, uctx)[2] == bases_of (^^J, uctx)[2]);
static_assert (subobjects_of (^^J, uctx)[3] == nonstatic_data_members_of (^^J, uctx)[0]);
static_assert (!has_inaccessible_subobjects (^^J, uctx));
static_assert (subobjects_of (^^K, uctx).size () == 10);
static_assert (subobjects_of (^^K, uctx)[0] == bases_of (^^K, uctx)[0]);
static_assert (subobjects_of (^^K, uctx)[1] == bases_of (^^K, uctx)[1]);
static_assert (subobjects_of (^^K, uctx)[2] == bases_of (^^K, uctx)[2]);
static_assert (subobjects_of (^^K, uctx)[3] == bases_of (^^K, uctx)[3]);
static_assert (subobjects_of (^^K, uctx)[4] == nonstatic_data_members_of (^^K, uctx)[0]);
static_assert (subobjects_of (^^K, uctx)[5] == nonstatic_data_members_of (^^K, uctx)[1]);
static_assert (subobjects_of (^^K, uctx)[6] == nonstatic_data_members_of (^^K, uctx)[2]);
static_assert (subobjects_of (^^K, uctx)[7] == nonstatic_data_members_of (^^K, uctx)[3]);
static_assert (subobjects_of (^^K, uctx)[8] == nonstatic_data_members_of (^^K, uctx)[4]);
static_assert (subobjects_of (^^K, uctx)[9] == nonstatic_data_members_of (^^K, uctx)[5]);
static_assert (!has_inaccessible_subobjects (^^K, uctx));
static_assert (subobjects_of (^^L, uctx).size () == 2);
static_assert (subobjects_of (^^L, uctx)[0] == bases_of (^^L, uctx)[0]);
static_assert (subobjects_of (^^L, uctx)[1] == nonstatic_data_members_of (^^L, uctx)[0]);
static_assert (!has_inaccessible_subobjects (^^L, uctx));
static_assert (subobjects_of (^^M, uctx).size () == 2);
static_assert (subobjects_of (^^M, uctx)[0] == bases_of (^^M, uctx)[0]);
static_assert (subobjects_of (^^M, uctx)[1] == nonstatic_data_members_of (^^M, uctx)[0]);
static_assert (!has_inaccessible_subobjects (^^M, uctx));
static_assert (subobjects_of (^^N, uctx).size () == 4);
static_assert (subobjects_of (^^N, uctx)[0] == bases_of (^^N, uctx)[0]);
static_assert (subobjects_of (^^N, uctx)[1] == bases_of (^^N, uctx)[1]);
static_assert (subobjects_of (^^N, uctx)[2] == bases_of (^^N, uctx)[2]);
static_assert (subobjects_of (^^N, uctx)[3] == nonstatic_data_members_of (^^N, uctx)[0]);
static_assert (!has_inaccessible_subobjects (^^N, uctx));
static_assert (subobjects_of (^^O, uctx).size () == 4);
static_assert (subobjects_of (^^O, uctx)[0] == bases_of (^^O, uctx)[0]);
static_assert (subobjects_of (^^O, uctx)[1] == bases_of (^^O, uctx)[1]);
static_assert (subobjects_of (^^O, uctx)[2] == bases_of (^^O, uctx)[2]);
static_assert (subobjects_of (^^O, uctx)[3] == nonstatic_data_members_of (^^O, uctx)[0]);
static_assert (!has_inaccessible_subobjects (^^O, uctx));
static_assert (subobjects_of (^^P, uctx).size () == 4);
static_assert (subobjects_of (^^P, uctx)[0] == bases_of (^^P, uctx)[0]);
static_assert (subobjects_of (^^P, uctx)[1] == bases_of (^^P, uctx)[1]);
static_assert (subobjects_of (^^P, uctx)[2] == bases_of (^^P, uctx)[2]);
static_assert (subobjects_of (^^P, uctx)[3] == nonstatic_data_members_of (^^P, uctx)[0]);
static_assert (!has_inaccessible_subobjects (^^P, uctx));

static_assert (subobjects_of (^^A, gctx).size () == 1);
static_assert (subobjects_of (^^A, gctx)[0] == nonstatic_data_members_of (^^A, uctx)[0]);
static_assert (subobjects_of (^^B, gctx).size () == 1);
static_assert (subobjects_of (^^B, gctx)[0] == nonstatic_data_members_of (^^B, uctx)[0]);
static_assert (subobjects_of (^^C, gctx).size () == 1);
static_assert (subobjects_of (^^C, gctx)[0] == nonstatic_data_members_of (^^C, uctx)[0]);
static_assert (!has_inaccessible_subobjects (^^C, gctx));
static_assert (subobjects_of (^^D, gctx).size () == 1);
static_assert (subobjects_of (^^D, gctx)[0] == nonstatic_data_members_of (^^D, uctx)[0]);
static_assert (subobjects_of (^^E, gctx).size () == 1);
static_assert (subobjects_of (^^E, gctx)[0] == nonstatic_data_members_of (^^E, uctx)[0]);
static_assert (subobjects_of (^^F, gctx).size () == 1);
static_assert (subobjects_of (^^F, gctx)[0] == nonstatic_data_members_of (^^F, uctx)[0]);
static_assert (subobjects_of (^^G, gctx).size () == 1);
static_assert (subobjects_of (^^G, gctx)[0] == nonstatic_data_members_of (^^G, uctx)[0]);
static_assert (subobjects_of (^^H, gctx).size () == 1);
static_assert (subobjects_of (^^H, gctx)[0] == nonstatic_data_members_of (^^H, uctx)[0]);
static_assert (subobjects_of (^^I, gctx).size () == 2);
static_assert (subobjects_of (^^I, gctx)[0] == bases_of (^^I, uctx)[0]);
static_assert (subobjects_of (^^I, gctx)[1] == nonstatic_data_members_of (^^I, uctx)[0]);
static_assert (has_inaccessible_subobjects (^^I, gctx));
static_assert (subobjects_of (^^J, gctx).size () == 2);
static_assert (subobjects_of (^^J, gctx)[0] == bases_of (^^J, uctx)[2]);
static_assert (subobjects_of (^^J, gctx)[1] == nonstatic_data_members_of (^^J, uctx)[0]);
static_assert (has_inaccessible_subobjects (^^J, gctx));
static_assert (subobjects_of (^^K, gctx).size () == 4);
static_assert (subobjects_of (^^K, gctx)[0] == bases_of (^^K, uctx)[0]);
static_assert (subobjects_of (^^K, gctx)[1] == bases_of (^^K, uctx)[3]);
static_assert (subobjects_of (^^K, gctx)[2] == ^^K::Za);
static_assert (subobjects_of (^^K, gctx)[3] == ^^K::Zg);
static_assert (has_inaccessible_subobjects (^^K, gctx));
static_assert (subobjects_of (^^L, gctx).size () == 2);
static_assert (subobjects_of (^^L, gctx)[0] == bases_of (^^L, uctx)[0]);
static_assert (subobjects_of (^^L, gctx)[1] == nonstatic_data_members_of (^^L, uctx)[0]);
static_assert (!has_inaccessible_subobjects (^^L, uctx));
static_assert (subobjects_of (^^M, gctx).size () == 2);
static_assert (subobjects_of (^^M, gctx)[0] == bases_of (^^M, uctx)[0]);
static_assert (subobjects_of (^^M, gctx)[1] == nonstatic_data_members_of (^^M, uctx)[0]);
static_assert (!has_inaccessible_subobjects (^^M, gctx));
static_assert (subobjects_of (^^N, gctx).size () == 3);
static_assert (subobjects_of (^^N, gctx)[0] == bases_of (^^N, uctx)[0]);
static_assert (subobjects_of (^^N, gctx)[1] == bases_of (^^N, uctx)[2]);
static_assert (subobjects_of (^^N, gctx)[2] == nonstatic_data_members_of (^^N, uctx)[0]);
static_assert (has_inaccessible_subobjects (^^N, gctx));
static_assert (subobjects_of (^^O, gctx).size () == 4);
static_assert (subobjects_of (^^O, gctx)[0] == bases_of (^^O, uctx)[0]);
static_assert (subobjects_of (^^O, gctx)[1] == bases_of (^^O, uctx)[1]);
static_assert (subobjects_of (^^O, gctx)[2] == bases_of (^^O, uctx)[2]);
static_assert (subobjects_of (^^O, gctx)[3] == nonstatic_data_members_of (^^O, uctx)[0]);
static_assert (!has_inaccessible_subobjects (^^O, gctx));
static_assert (subobjects_of (^^P, gctx).size () == 3);
static_assert (subobjects_of (^^P, gctx)[0] == bases_of (^^P, uctx)[0]);
static_assert (subobjects_of (^^P, gctx)[1] == bases_of (^^P, uctx)[1]);
static_assert (subobjects_of (^^P, gctx)[2] == bases_of (^^P, uctx)[2]);
static_assert (has_inaccessible_subobjects (^^P, gctx));
