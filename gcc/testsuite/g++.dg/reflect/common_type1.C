// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::common_type.

#include <meta>
using namespace std::meta;

template <reflection_range R = std::initializer_list <info>>
consteval bool
has_common_type (R &&args)
{
  try { common_type (args); }
  catch (std::meta::exception &) { return false; }
  return true;
}

struct S {};
struct B {};
struct D : B {};
struct F1 { operator void * (); };
struct F2 { operator void * (); };
struct G1 { operator const void * (); };
struct G2 { operator volatile void * (); };
template <typename T>
struct ImplicitTo { operator T (); };
template <typename T>
struct ExplicitTo { explicit operator T (); };
template <typename T>
struct PrivateImplicitTo { private: operator T (); };
auto lmd1 = [] (int, double) {};
auto lmd2 = [] (int, double) {};
struct Abstract { virtual ~Abstract () = 0; };
enum class ScEn;
enum UnscEn : int;
struct Ukn;
union U { int i; };
union U2 { long i; };
union UConv1 { operator Abstract * (); };
union UConv2 { operator Abstract * (); };
struct X1 {};
struct X2 {};
struct RX12 {};
struct RX21 {};
struct Y1 {};
struct Y2 {};
struct Y3 {};
struct Y4 {};

namespace std {
  template <>
  struct common_type <X1, X2> { typedef RX12 type; };
  template <>
  struct common_type <X2, X1> { typedef RX21 type; };
  template <>
  struct common_type <RX12, X1> { typedef Y1 type; };
  template <>
  struct common_type <X1, RX12> { typedef Y2 type; };
  template <>
  struct common_type <RX21, X1> { typedef Y3 type; };
  template <>
  struct common_type <X1, RX21> { typedef Y4 type; };
}

using A = int;
static_assert (common_type ({ ^^int }) == ^^int);
static_assert (common_type ({ ^^A }) == ^^int);
static_assert (common_type ({ ^^const int }) == ^^int);
static_assert (common_type ({ ^^int, ^^int }) == ^^int);
static_assert (common_type ({ ^^A, ^^int }) == ^^int);
static_assert (common_type ({ ^^A, ^^A }) == ^^int);
static_assert (common_type ({ ^^const int, ^^int }) == ^^int);
static_assert (common_type ({ ^^ScEn, ^^ScEn }) == ^^ScEn);
static_assert (common_type ({ ^^UnscEn, ^^UnscEn }) == ^^UnscEn);
static_assert (common_type ({ ^^UnscEn, ^^int }) == ^^int);
static_assert (common_type ({ ^^int, ^^int, ^^int }) == ^^int);
static_assert (common_type ({ ^^int, ^^int, ^^int, ^^int }) == ^^int);
static_assert (common_type ({ ^^int, ^^int, ^^int, ^^int, ^^int }) == ^^int);
static_assert (common_type ({ ^^S, ^^S }) == ^^S);
static_assert (common_type ({ ^^const S, ^^const S }) == ^^S);
static_assert (common_type ({ ^^std::initializer_list<int>, ^^std::initializer_list<int> }) == ^^std::initializer_list<int>);
static_assert (common_type ({ ^^B, ^^D }) == ^^B);
static_assert (common_type ({ ^^D, ^^B }) == ^^B);
static_assert (common_type ({ ^^F1, ^^F2 }) == ^^void *);
static_assert (common_type ({ ^^F2, ^^F1 }) == ^^void *);
static_assert (common_type ({ ^^G1, ^^G2 }) == ^^const volatile void *);
static_assert (common_type ({ ^^G2, ^^G1 }) == ^^const volatile void *);
static_assert (common_type ({ ^^int *, ^^const volatile int * }) == ^^const volatile int *);
static_assert (common_type ({ ^^void *, ^^const volatile int * }) == ^^const volatile void *);
static_assert (common_type ({ ^^void }) == ^^void);
static_assert (common_type ({ ^^const void }) == ^^void);
static_assert (common_type ({ ^^void, ^^void }) == ^^void);
static_assert (common_type ({ ^^const void, ^^const void }) == ^^void);
static_assert (common_type ({ ^^int &, ^^int && }) == ^^int);
static_assert (common_type ({ ^^int &, ^^int & }) == ^^int);
static_assert (common_type ({ ^^int &&, ^^int && }) == ^^int);
static_assert (common_type ({ ^^int &&, ^^const int && }) == ^^int);
static_assert (common_type ({ ^^U &, ^^const U && }) == ^^U);
static_assert (common_type ({ ^^U &, ^^U & }) == ^^U);
static_assert (common_type ({ ^^U &&, ^^U && }) == ^^U);
static_assert (common_type ({ ^^int B::*, ^^int D::* }) == ^^int D::*);
static_assert (common_type ({ ^^int D::*, ^^int B::* }) == ^^int D::*);
static_assert (common_type ({ ^^const int B::*, ^^volatile int D::* }) == ^^const volatile int D::*);
static_assert (common_type ({ ^^int (B::*) (), ^^int (D::*) () }) == ^^int (D::*) ());
static_assert (common_type ({ ^^int (B::*) () const, ^^int (D::*) () const }) == ^^int (D::*) () const);
static_assert (common_type ({ ^^int [3], ^^int [3] }) == ^^int *);
static_assert (common_type ({ ^^int [1], ^^const int [3] }) == ^^const int *);
static_assert (common_type ({ ^^void (), ^^void () }) == ^^void (*) ());
static_assert (common_type ({ ^^void (&) (), ^^void (&) () }) == ^^void (*) ());
static_assert (common_type ({ ^^void (&) (), ^^void (&&) () }) == ^^void (*) ());
static_assert (common_type ({ ^^void (&&) (), ^^void (&) () }) == ^^void (*) ());
static_assert (common_type ({ ^^void (&&) (), ^^void (&&) () }) == ^^void (*) ());
static_assert (common_type ({ ^^ImplicitTo<int>, ^^int }) == ^^int);
static_assert (common_type ({ ^^const ImplicitTo<int>, ^^int }) == ^^int);
static_assert (common_type ({ ^^ImplicitTo<int>, ^^ImplicitTo<int> }) == ^^ImplicitTo<int>);
static_assert (common_type ({ ^^ImplicitTo<int>, ^^int, ^^ImplicitTo<int> }) == ^^int);
static_assert (common_type ({ ^^ExplicitTo<int>, ^^ExplicitTo<int> }) == ^^ExplicitTo<int>);
static_assert (common_type ({ ^^decltype (lmd1), ^^decltype (lmd1) }) == ^^decltype (lmd1));
static_assert (common_type ({ ^^decltype (lmd1) &, ^^decltype (lmd1) & }) == ^^decltype (lmd1));
static_assert (common_type ({ ^^decltype (lmd1) &, ^^decltype (lmd2) & }) == ^^void (*) (int, double));
static_assert (common_type ({ ^^decltype (nullptr), ^^void * }) == ^^void *);
static_assert (common_type ({ ^^decltype (nullptr), ^^int * }) == ^^int *);
static_assert (common_type ({ ^^const decltype (nullptr) &, ^^int * }) == ^^int *);
static_assert (common_type ({ ^^decltype (nullptr), ^^const volatile int * }) == ^^const volatile int *);
static_assert (common_type ({ ^^decltype (nullptr), ^^int (B::*) () }) == ^^int (B::*) ());
static_assert (common_type ({ ^^decltype (nullptr), ^^int (B::*) () const }) == ^^int (B::*) () const);
static_assert (common_type ({ ^^decltype (nullptr), ^^const int B::* }) == ^^const int B::*);
static_assert (common_type ({ ^^Abstract &, ^^Abstract & }) == ^^Abstract);
static_assert (common_type ({ ^^Ukn &, ^^Ukn & }) == ^^Ukn);
static_assert (common_type ({ ^^ImplicitTo<B &>, ^^B & }) == ^^B);
static_assert (common_type ({ ^^ImplicitTo<B &> &, ^^B && }) == ^^B);
static_assert (common_type ({ ^^UConv1, ^^const Abstract * & }) == ^^const Abstract *);
static_assert (common_type ({ ^^UConv1, ^^UConv2 }) == ^^Abstract *);
static_assert (common_type ({ ^^UConv1 &, ^^UConv2 & }) == ^^Abstract *);
static_assert (common_type ({ ^^Abstract &&, ^^Abstract && }) == ^^Abstract);
static_assert (common_type ({ ^^const Abstract &&, ^^const Abstract && }) == ^^Abstract);
static_assert (common_type ({ ^^volatile Abstract &&, ^^volatile Abstract && }) == ^^Abstract);
static_assert (common_type ({ ^^Ukn &&, ^^Ukn && }) == ^^Ukn);
static_assert (common_type ({ ^^const Ukn &&, ^^const Ukn && }) == ^^Ukn);
static_assert (common_type ({ ^^volatile Ukn &&, ^^volatile Ukn && }) == ^^Ukn);
static_assert (common_type ({ ^^X1, ^^X2 }) == ^^RX12);
static_assert (common_type ({ ^^const X1, ^^X2 }) == ^^RX12);
static_assert (common_type ({ ^^X1 &, ^^const X2 }) == ^^RX12);
static_assert (common_type ({ ^^const X1 &, ^^const X2 & }) == ^^RX12);
static_assert (common_type ({ ^^X2, ^^X1 }) == ^^RX21);
static_assert (common_type ({ ^^X1, ^^X2, ^^X1 }) == ^^Y1);
static_assert (common_type ({ ^^X2, ^^X1, ^^X1 }) == ^^Y3);
static_assert (common_type ({ ^^X1, ^^X1, ^^X2 }) == ^^RX12);
static_assert (common_type ({ ^^X1 &, ^^const X1, ^^const X2 && }) == ^^RX12);
static_assert (common_type ({ ^^X1, ^^X1, ^^X2, ^^X1 }) == ^^Y1);
static_assert (!has_common_type ({ ^^::, ^^int }));
static_assert (!has_common_type ({ ^^int, ^^int, ^^std }));
