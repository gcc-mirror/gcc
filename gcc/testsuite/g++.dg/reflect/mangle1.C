// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection -O0" }

#include <meta>
#include <ranges>

int arr[] = {1, 2, 3};
auto [a1, a2, a3] = arr;
[[=1]] void fn (int n);
enum Enum { A };
using Alias = int;
struct B {};
struct S : B {
  int mem;
  int : 0;
  static int var;
};
template <auto> struct TCls {};
template <auto> void TFn ();
template <auto> int TVar;
template <auto N> using TAlias = TCls <N>;
template <auto> concept Concept = requires { true; };
namespace NS {};
namespace NSAlias = NS;
namespace NS2 {
  int arr[] = {1, 2, 3};
  auto [a1, a2, a3] = arr;
  [[=1]] void fn (int n, long m);
  enum Enum { A };
  using Alias = decltype (^^int);
  struct B {};
  struct S : B {
    int mem;
    int : 0;
    int : 3;
    int : 5;
    static int var;
    template <typename T> static T TVar;
  };
  template <auto> struct TCls {};
  template <auto> void TFn ();
  template <auto> int TVar;
  template <auto N> using TAlias = TCls <N>;
  template <auto> concept Concept = requires { true; };
  struct V { int a, b, c; };
  S s;
  namespace NS3 {
    auto [a1, a2, a3] = arr;
  }
  namespace NSAlias = NS3;
  enum { C };
  template <typename T>
  struct W : B, ::B {};
  static union { int au; };
  struct X {
    union { int a; };
    X &operator + (const X &);
    operator int ();
  };
  template <typename T>
  struct Y {
    union { T a; };
  };
  struct Z {
  };
}

constexpr auto ctx = std::meta::access_context::current ();

template <int N, typename T>
void
foo ()
{
}

template <int N, std::meta::info I>
void
bar ()
{
}

void
baz (int x)
{
  int v = 42;
  foo <1, std::meta::info> ();
  foo <2, const std::meta::info> ();
  bar <110, std::meta::info {}> (); // null reflection
  bar <120, std::meta::reflect_constant (42)> (); // value
  bar <121, std::meta::reflect_constant (42.0)> (); // value
  bar <122, std::meta::reflect_constant (NS2::V { 1, 2, 3 })> (); // value
  bar <130, std::meta::reflect_object (arr[1])> (); // object
  bar <131, std::meta::reflect_object (NS2::arr[1])> (); // object
  bar <132, std::meta::reflect_object (NS2::s.mem)> (); // object
  bar <140, ^^arr> (); // variable
  bar <141, ^^NS2::arr> (); // variable
  bar <142, ^^x> (); // variable
  bar <143, ^^v> (); // variable
  bar <144, members_of (^^NS2::NS3, ctx)[0]> (); // variable
  bar <145, ^^S::var> (); // variable
  bar <146, ^^NS2::S::var> (); // variable
  bar <147, ^^TVar <42>> (); // variable
  bar <148, ^^TVar <42>> (); // variable
  bar <149, ^^NS2::S::TVar <int>> (); // variable
  bar <150, ^^a3> (); // structured binding
  bar <151, ^^NS2::a3> (); // structured binding
  bar <160, ^^fn> (); // function
  bar <161, ^^NS2::fn> (); // function
  bar <162, ^^TFn <42>> (); // function
  bar <163, ^^NS2::TFn <0U>> (); // function
  bar <164, ^^NS2::TFn <^^int>> (); // function
  bar <165, ^^NS2::X::operator+> (); // function
  bar <166, (members_of (^^NS2::Z, ctx) | std::views::filter (std::meta::is_default_constructor) | std::ranges::to <std::vector> ())[0]> (); // function
  bar <167, (members_of (^^NS2::Z, ctx) | std::views::filter (std::meta::is_copy_constructor) | std::ranges::to <std::vector> ())[0]> (); // function
  bar <168, (members_of (^^NS2::Z, ctx) | std::views::filter (std::meta::is_copy_assignment) | std::ranges::to <std::vector> ())[0]> (); // function
  bar <169, (members_of (^^NS2::Z, ctx) | std::views::filter (std::meta::is_destructor) | std::ranges::to <std::vector> ())[0]> (); // function
  bar <170, parameters_of (^^fn)[0]> (); // function parameter
  bar <171, parameters_of (^^NS2::fn)[0]> (); // function parameter
  bar <172, parameters_of (^^NS2::fn)[1]> (); // function parameter
  bar <180, ^^Enum::A> (); // enumerator
  bar <181, ^^NS2::Enum::A> (); // enumerator
  bar <182, ^^NS2::C> (); // enumerator
  bar <190, annotations_of (^^fn)[0]> (); // annotation
  bar <200, ^^Alias> (); // type alias
  bar <201, ^^NS2::Alias> (); // type alias
  bar <202, ^^TAlias <42>> (); // type alias
  bar <203, ^^NS2::TAlias <0U>> (); // type alias
  bar <210, ^^S> (); // type
  bar <211, ^^NS2::S> (); // type
  bar <212, ^^const int> (); // type
  bar <213, ^^decltype (nullptr)> (); // type
  bar <214, ^^decltype (^^::)> (); // type
  bar <215, ^^TCls <42>> (); // type
  bar <216, ^^NS2::TCls <42>> (); // type
  bar <220, ^^S::mem> (); // non-static data member
  bar <221, ^^NS2::S::mem> (); // non-static data member
  bar <222, ^^NS2::au> (); // non-static data member
  bar <223, ^^NS2::X::a> (); // non-static data member
  bar <224, ^^NS2::Y <int>::a> (); // non-static data member
  bar <230, members_of (^^S, ctx)[1]> (); // unnamed bit-field
  bar <231, members_of (^^NS2::S, ctx)[1]> (); // unnamed bit-field
  bar <232, members_of (^^NS2::S, ctx)[2]> (); // unnamed bit-field
  bar <233, members_of (^^NS2::S, ctx)[3]> (); // unnamed bit-field
  bar <240, ^^TCls> (); // class template
  bar <241, ^^NS2::TCls> (); // class template
  bar <250, ^^TFn> (); // function template
  bar <251, ^^NS2::TFn> (); // function template
  bar <260, ^^TVar> (); // variable template
  bar <261, ^^NS2::TVar> (); // variable template
  bar <270, ^^TAlias> (); // alias template
  bar <271, ^^NS2::TAlias> (); // alias template
  bar <280, ^^Concept> (); // concept
  bar <281, ^^NS2::Concept> (); // concept
  bar <290, ^^NSAlias> (); // namespace alias
  bar <291, ^^NS2::NSAlias> (); // namespace alias
  bar <300, ^^NS> (); // namespace
  bar <301, ^^NS2::NS3> (); // namespace
  bar <310, ^^::> (); // global namespace
  bar <320, bases_of (^^S, ctx)[0]> (); // direct base class relationship
  bar <321, bases_of (^^NS2::S, ctx)[0]> (); // direct base class relationship
  bar <322, bases_of (^^NS2::W <int>, ctx)[0]> (); // direct base class relationship
  bar <323, bases_of (^^NS2::W <float>, ctx)[1]> (); // direct base class relationship
  bar <330, data_member_spec (^^S, { .name = "member" })> (); // data member description
  bar <331, data_member_spec (^^NS2::S, { .name = "m", .alignment = 16, .no_unique_address = true })> (); // data member description
  bar <332, data_member_spec (^^unsigned short, { .name = "b", .bit_width = 5 })> (); // data member description
  bar <333, data_member_spec (^^long, { .bit_width = 3 })> (); // data member description
  bar <334, data_member_spec (^^int, { .bit_width = 0 })> (); // data member description
  bar <340, ^^NS2::X::~X> (); // function
}

// { dg-final { scan-assembler "_Z3fooILi1EDmEvv" } }
// { dg-final { scan-assembler "_Z3fooILi2EKDmEvv" } }
// { dg-final { scan-assembler "_Z3barILi110ELDmnuEEvv" } }
// { dg-final { scan-assembler "_Z3barILi120ELDmvlLi42EEEvv" } }
// { dg-final { scan-assembler "_Z3barILi121ELDmvlLd4045000000000000EEEvv" } }
// { dg-final { scan-assembler "_Z3barILi122ELDmobL_Z29_ZTAXtlN3NS21VELi1ELi2ELi3EEEEEEvv" } }
// { dg-final { scan-assembler "_Z3barILi130ELDmobixL_Z3arrEL\[ilx]1EEEvv" } }
// { dg-final { scan-assembler "_Z3barILi131ELDmobixL_ZN3NS23arrEEL\[ilx]1EEEvv" } }
// { dg-final { scan-assembler "_Z3barILi132ELDmobdtL_ZN3NS21sEE3memEEvv" } }
// { dg-final { scan-assembler "_Z3barILi140ELDmvr3arrEEvv" } }
// { dg-final { scan-assembler "_Z3barILi141ELDmvrN3NS23arrEEEvv" } }
// { dg-final { scan-assembler "_Z3barILi142ELDmvrZ3baziE1xEEvv" } }
// { dg-final { scan-assembler "_Z3barILi143ELDmvrZ3baziE1vEEvv" } }
// { dg-final { scan-assembler "_Z3barILi144ELDmvrN3NS23NS324_ZN3NS23NS3DC2a12a22a3EEEEEvv" } }
// { dg-final { scan-assembler "_Z3barILi145ELDmvrN1S3varEEEvv" } }
// { dg-final { scan-assembler "_Z3barILi146ELDmvrN3NS21S3varEEEvv" } }
// { dg-final { scan-assembler "_Z3barILi147ELDmvr4TVarILi42EEEEvv" } }
// { dg-final { scan-assembler "_Z3barILi148ELDmvr4TVarILi42EEEEvv" } }
// { dg-final { scan-assembler "_Z3barILi149ELDmvrN3NS21S4TVarIiEEEEvv" } }
// { dg-final { scan-assembler "_Z3barILi150ELDmsb2a3EEvv" } }
// { dg-final { scan-assembler "_Z3barILi151ELDmsbN3NS22a3EEEvv" } }
// { dg-final { scan-assembler "_Z3barILi160ELDmfn2fniEEvv" } }
// { dg-final { scan-assembler "_Z3barILi161ELDmfnN3NS22fnEilEEvv" } }
// { dg-final { scan-assembler "_Z3barILi162ELDmfn3TFnITnDaLi42EEvvEEvv" } }
// { dg-final { scan-assembler "_Z3barILi163ELDmfnN3NS23TFnILj0EEEvvEEvv" } }
// { dg-final { scan-assembler "_Z3barILi164ELDmfnN3NS23TFnILDmtyiEEEvvEEvv" } }
// { dg-final { scan-assembler "_Z3barILi165ELDmfnN3NS21XplERKS1_EEvv" } }
// { dg-final { scan-assembler "_Z3barILi166ELDmfnN3NS21ZC4EvEEvv" } }
// { dg-final { scan-assembler "_Z3barILi167ELDmfnN3NS21ZC4ERKS1_EEvv" } }
// { dg-final { scan-assembler "_Z3barILi168ELDmfnN3NS21ZaSERKS1_EEvv" } }
// { dg-final { scan-assembler "_Z3barILi169ELDmfnN3NS21ZD4EvEEvv" } }
// { dg-final { scan-assembler "_Z3barILi170ELDmpa_2fniEEvv" } }
// { dg-final { scan-assembler "_Z3barILi171ELDmpa_N3NS22fnEilEEvv" } }
// { dg-final { scan-assembler "_Z3barILi172ELDmpa0_N3NS22fnEilEEvv" } }
// { dg-final { scan-assembler "_Z3barILi180ELDmen4Enum1AEEvv" } }
// { dg-final { scan-assembler "_Z3barILi181ELDmen3NS24Enum1AEEvv" } }
// { dg-final { scan-assembler "_Z3barILi182ELDmen3NS2Uej1C1CEEvv" } }
// { dg-final { scan-assembler "_Z3barILi190ELDman_EEvv" } }
// { dg-final { scan-assembler "_Z3barILi200ELDmta5AliasEEvv" } }
// { dg-final { scan-assembler "_Z3barILi201ELDmta3NS25AliasEEvv" } }
// { dg-final { scan-assembler "_Z3barILi202ELDmta6TAliasILi42EEEEvv" } }
// { dg-final { scan-assembler "_Z3barILi203ELDmta3NS26TAliasILj0EEEEvv" } }
// { dg-final { scan-assembler "_Z3barILi210ELDmty1SEEvv" } }
// { dg-final { scan-assembler "_Z3barILi211ELDmtyN3NS21SEEEvv" } }
// { dg-final { scan-assembler "_Z3barILi212ELDmtyKiEEvv" } }
// { dg-final { scan-assembler "_Z3barILi213ELDmtyDnEEvv" } }
// { dg-final { scan-assembler "_Z3barILi214ELDmtyDmEEvv" } }
// { dg-final { scan-assembler "_Z3barILi215ELDmty4TClsILi42EEEEvv" } }
// { dg-final { scan-assembler "_Z3barILi216ELDmtyN3NS24TClsILi42EEEEEvv" } }
// { dg-final { scan-assembler "_Z3barILi220ELDmdm1S3memEEvv" } }
// { dg-final { scan-assembler "_Z3barILi221ELDmdm3NS21S3memEEvv" } }
// { dg-final { scan-assembler "_Z3barILi222ELDmdm3NS22auEEvv" } }
// { dg-final { scan-assembler "_Z3barILi223ELDmdm3NS21X1aEEvv" } }
// { dg-final { scan-assembler "_Z3barILi224ELDmdm3NS21YIiE1aEEvv" } }
// { dg-final { scan-assembler "_Z3barILi230ELDmun1S_EEvv" } }
// { dg-final { scan-assembler "_Z3barILi231ELDmun3NS21S_EEvv" } }
// { dg-final { scan-assembler "_Z3barILi232ELDmun3NS21S0_EEvv" } }
// { dg-final { scan-assembler "_Z3barILi233ELDmun3NS21S1_EEvv" } }
// { dg-final { scan-assembler "_Z3barILi240ELDmct4TClsEEvv" } }
// { dg-final { scan-assembler "_Z3barILi241ELDmct3NS24TClsEEvv" } }
// { dg-final { scan-assembler "_Z3barILi250ELDmft3TFnEEvv" } }
// { dg-final { scan-assembler "_Z3barILi251ELDmft3NS23TFnEEvv" } }
// { dg-final { scan-assembler "_Z3barILi260ELDmvt4TVarEEvv" } }
// { dg-final { scan-assembler "_Z3barILi261ELDmvt3NS24TVarEEvv" } }
// { dg-final { scan-assembler "_Z3barILi270ELDmat6TAliasEEvv" } }
// { dg-final { scan-assembler "_Z3barILi271ELDmat3NS26TAliasEEvv" } }
// { dg-final { scan-assembler "_Z3barILi280ELDmco7ConceptEEvv" } }
// { dg-final { scan-assembler "_Z3barILi281ELDmco3NS27ConceptEEvv" } }
// { dg-final { scan-assembler "_Z3barILi290ELDmna7NSAliasEEvv" } }
// { dg-final { scan-assembler "_Z3barILi291ELDmna3NS27NSAliasEEvv" } }
// { dg-final { scan-assembler "_Z3barILi300ELDmns2NSEEvv" } }
// { dg-final { scan-assembler "_Z3barILi301ELDmns3NS23NS3EEvv" } }
// { dg-final { scan-assembler "_Z3barILi310ELDmngEEvv" } }
// { dg-final { scan-assembler "_Z3barILi320ELDmba_1SEEvv" } }
// { dg-final { scan-assembler "_Z3barILi321ELDmba_N3NS21SEEEvv" } }
// { dg-final { scan-assembler "_Z3barILi322ELDmba_N3NS21WIiEEEEvv" } }
// { dg-final { scan-assembler "_Z3barILi323ELDmba0_N3NS21WIfEEEEvv" } }
// { dg-final { scan-assembler "_Z3barILi330ELDmds1S_6member___EEvv" } }
// { dg-final { scan-assembler "_Z3barILi331ELDmdsN3NS21SE_1m_16__nEEvv" } }
// { dg-final { scan-assembler "_Z3barILi332ELDmdst_1b__5_EEvv" } }
// { dg-final { scan-assembler "_Z3barILi333ELDmdsl___3_EEvv" } }
// { dg-final { scan-assembler "_Z3barILi334ELDmdsi___0_EEvv" } }
// { dg-final { scan-assembler "_Z3barILi340ELDmfnN3NS21XD4EvEEvv" } }
