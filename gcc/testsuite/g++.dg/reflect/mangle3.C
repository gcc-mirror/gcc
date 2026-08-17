// PR c++/123237
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test mangling dependent splices.

#include <meta>
using namespace std::meta;

int i;
int arr[] = {1, 2, 3};
auto [a1, a2, a3] = arr;
enum Enum { A };
using Alias = int;

template<auto>
struct TCls { };

template <auto> int TVar;

template<typename T>
struct B {
  T t;
  constexpr T fn () { return 42; }
  template<typename>
  struct C { };
  template<typename U>
  using U = C<U>;
};

struct Y {
  int i;
  using type = int;
};

struct S : Y { };

namespace N {
  struct NY {
    int i;
  };
}

namespace NSAlias = N;
template <auto> concept Concept = requires { true; };
constexpr auto ctx = std::meta::access_context::current ();

template<typename T>
using US = [:^^T:];

template<info R>
constexpr auto f1 (typename [:R:] x) { return x; }

template<info R>
constexpr auto f2 (typename [:R:]::NY x) { return x; }

template<info R>
constexpr auto f3 () -> TCls<sizeof(typename [:R:])> { return {}; }

template<info R>
constexpr auto f4 () -> TCls<sizeof([:R:])> { return {}; }

template<info R>
constexpr auto f5 () -> decltype([:R:]) { return {}; }

template<typename T>
constexpr auto f6 (US<T> y) { return y; }

template<info R>
constexpr auto f7 (typename [:R:]<0> x) { return x; }

template<typename T>
constexpr auto f8 (typename [:^^T:] x) { return x; }

template<info R>
constexpr typename [:R:] f9 (int i) { return i; }

template<typename T>
constexpr T foo (T t) { return t; }

template<info R>
constexpr auto f10 () -> decltype([:R:](42)) { return {}; }

template<info R>
constexpr auto f11 (TCls<[:R:](42)> a) { return a; }

template<typename T>
consteval info id () { return ^^T; }

template<typename T>
constexpr typename [:id<T>():] f12 () { return 42; }

template<info R>
constexpr auto f13 (typename [:R:]::type x) { return x; }

template<info R>
constexpr auto f14 () -> [:R:] { return {}; }

template<typename T, info R>
constexpr auto f15 () -> decltype(T{}.[:R:]) { return {}; }

template<typename T, info R>
constexpr auto f16 () -> decltype(T{}.[:R:]()) { return {}; }

template<info O, info M>
constexpr auto f17 () -> decltype(typename [:O:]{}.[:M:]) { return {}; }

template<info M>
constexpr auto f18 () -> decltype(Y{}.[:M:]) { return {}; }

template<template<typename> class T>
constexpr auto f19 () -> [:^^T:]<int> { return {}; }

template<typename T>
constexpr auto f20 () -> [:^^typename T::type:] { return {}; }

template<typename T>
constexpr auto f21 () -> decltype(&[:^^T::i:]) { return {}; }

template<typename T>
constexpr auto f22 () -> decltype(&[:^^T::fn:]) { return {}; }

template<typename T>
constexpr auto f23 () -> [:^^typename T::C<int>:] { return {}; }

template<typename T>
constexpr auto f24 () -> [:^^typename T::U<int>:] { return {}; }

template<info R>
constexpr auto f25 () -> decltype([:R:])* { return {}; }

template<typename T>
consteval auto f26 (typename [:^^T:] x) { return x; }

template<info R>
constexpr auto f27 (typename [:R:]::Alias x) { return x; }

template<info M>
constexpr auto f28 () -> decltype(S{}.[:M:].i) { return 42; }

void
g (int p)
{
  f1<^^Y>(Y{ 42 });
// { dg-final { scan-assembler "_Z2f1ILDmty1YEEDaDST_E" } }
  f1<^^Enum>(A);
// { dg-final { scan-assembler "_Z2f1ILDmty4EnumEEDaDST_E" } }
  f2<^^N>(N::NY{ 42 });
// { dg-final { scan-assembler "_Z2f2ILDmns1NEEDaNDST_E2NYE" } }
  f2<^^NSAlias>(NSAlias::NY{ 42 });
// { dg-final { scan-assembler "_Z2f2ILDmna7NSAliasEEDaNDST_E2NYE" } }
  f3<^^int>();
// { dg-final { scan-assembler "_Z2f3ILDmtyiEE4TClsIXstDST_EEEv" } }
  f3<^^Alias>();
// { dg-final { scan-assembler "_Z2f3ILDmtyiEE4TClsIXstDST_EEEv" } }
  f4<^^i>();
// { dg-final { scan-assembler "_Z2f4ILDmvr1iEE4TClsIXszDST_EEEv" } }
  f4<^^arr>();
// { dg-final { scan-assembler "_Z2f4ILDmvr3arrEE4TClsIXszDST_EEEv" } }
  f4<^^a1>();
// { dg-final { scan-assembler "_Z2f4ILDmsb2a1EE4TClsIXszDST_EEEv" } }
  f4<^^A>();
// { dg-final { scan-assembler "_Z2f4ILDmen4Enum1AEE4TClsIXszDST_EEEv" } }
  f4<^^TVar>();
// { dg-final { scan-assembler "_Z2f4ILDmvt4TVarEE4TClsIXszDST_EEEv" } }
  f5<^^i>();
// { dg-final { scan-assembler "_Z2f5ILDmvr1iEEDtDST_EEv" } }
  f5<^^a1>();
// { dg-final { scan-assembler "_Z2f5ILDmsb2a1EEDtDST_EEv" } }
  f5<^^A>();
// { dg-final { scan-assembler "_Z2f5ILDmen4Enum1AEEDtDST_EEv" } }
  f5<^^TVar>();
// { dg-final { scan-assembler "_Z2f5ILDmvt4TVarEEDtDST_EEv" } }
  f5<^^Concept>();
// { dg-final { scan-assembler "_Z2f5ILDmco7ConceptEEDtDST_EEv" } }
  f5<^^p>();
// { dg-final { scan-assembler "_Z2f5ILDmvrZ1giE1pEEDtDST_EEv" } }
  f5<std::meta::reflect_constant (42)>();
// { dg-final { scan-assembler "_Z2f5ILDmvlLi42EEEDtDST_EEv" } }
  f5<std::meta::reflect_object (arr[1])>();
// { dg-final { scan-assembler "_Z2f5ILDmobixL_Z3arrEL\[ilx]1EEEDtDST_EEv" } }
  f6<Y>(Y{42});
// { dg-final { scan-assembler "_Z2f6I1YEDaDSLDmtyT_EE" } }
  f7<^^TCls>(TCls<0>{});
// { dg-final { scan-assembler "_Z2f7ILDmct4TClsEEDaDST_ILi0EEE" } }
  f8<int>(42);
// { dg-final { scan-assembler "_Z2f8IiEDaDSLDmtyT_EE" } }
  f8<Alias>(42);
// { dg-final { scan-assembler "_Z2f8IiEDaDSLDmtyT_EE" } }
  f9<^^int>(42);
// { dg-final { scan-assembler "_Z2f9ILDmtyiEEDST_Ei" } }
  f9<^^Alias>(42);
// { dg-final { scan-assembler "_Z2f9ILDmtyiEEDST_Ei" } }
  f10<^^foo<int>>();
// { dg-final { scan-assembler "_Z3f10ILDmfn3fooIiET_S1_EEDTclDST_ELi42EEEv" } }
  f10<^^foo<Alias>>();
// { dg-final { scan-assembler "_Z3f10ILDmfn3fooIiET_S1_EEDTclDST_ELi42EEEv" } }
  f11<^^foo<int>>(TCls<42>{});
// { dg-final { scan-assembler "_Z3f11ILDmfn3fooIiET_S1_EEDa4TClsIXclDST_ELi42EEEE" } }
  f11<^^foo<Alias>>(TCls<42>{});
// { dg-final { scan-assembler "_Z3f11ILDmfn3fooIiET_S1_EEDa4TClsIXclDST_ELi42EEEE" } }
  f12<int>();
// { dg-final { scan-assembler "_Z3f12IiEDScl2idIT_EEEv" } }
  f12<Alias>();
// { dg-final { scan-assembler "_Z3f12IiEDScl2idIT_EEEv" } }
  f13<^^Y>(42);
// { dg-final { scan-assembler "_Z3f13ILDmty1YEEDaNDST_E4typeE" } }
  f14<^^int>();
// { dg-final { scan-assembler "_Z3f14ILDmtyiEEDST_Ev" } }
  f14<^^TCls<1>>();
// { dg-final { scan-assembler "_Z3f14ILDmty4TClsILi1EEEEDST_Ev" } }
  f15<Y, ^^Y::i>();
// { dg-final { scan-assembler "_Z3f15I1YLDmdmS0_1iEEDtdttlT_EDST0_EEv" } }
  f15<B<int>, ^^B<int>::t>();
// { dg-final { scan-assembler "_Z3f15I1BIiELDmdmS1_1tEEDtdttlT_EDST0_EEv" } }
  f16<B<int>, ^^B<int>::fn>();
// { dg-final { scan-assembler "_Z3f16I1BIiELDmfnNS1_2fnEvEEDTcldttlT_EDST0_EEEv" } }
  f17<^^Y, ^^Y::i>();
// { dg-final { scan-assembler "_Z3f17ILDmty1YELDmdmS0_1iEEDtdttlDST_EEDST0_EEv" } }
  f18<^^Y::i>();
// { dg-final { scan-assembler "_Z3f18ILDmdm1Y1iEEDtdttlS0_EDST_EEv" } }
  f19<B>();
// { dg-final { scan-assembler "_Z3f19I1BEDSLDmttT_EIiEEv" } }
  f20<Y>();
// { dg-final { scan-assembler "_Z3f20I1YEDSLDmtyNT_4typeEEEv" } }
  f21<Y>();
// { dg-final { scan-assembler "_Z3f21I1YEDTadDSLDmdesrT_1iEEEv" } }
  f22<B<int>>();
// { dg-final { scan-assembler "_Z3f22I1BIiEEDTadDSLDmdesrT_2fnEEEv" } }
  f23<B<int>>();
// { dg-final { scan-assembler "_Z3f23I1BIiEEDSLDmtyNT_1CIiEEEEv" } }
  f24<B<int>>();
// { dg-final { scan-assembler "_Z3f24I1BIiEEDSLDmtyNT_1UIiEEEEv" } }
  f24<B<Alias>>();
// { dg-final { scan-assembler "_Z3f24I1BIiEEDSLDmtyNT_1UIiEEEEv" } }
  f25<^^arr>();
// { dg-final { scan-assembler "_Z3f25ILDmvr3arrEEPDtDST_EEv" } }
  constexpr auto r = f26<std::meta::info>({});
  f27<^^::>(42);
// { dg-final { scan-assembler "_Z3f27ILDmgsEEDaNDST_E5AliasE" } }
  f28<bases_of (^^S, ctx)[0]>();
// { dg-final { scan-assembler "_Z3f28ILDmba_1SEEDtdtdttlS0_EDST_E1iEv" } }
}
