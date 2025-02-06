// C++ 26 P2552R3 - On the ignorability of standard attributes
// { dg-do compile { target c++11 } }

int arr[2];
struct S { int a, b; };
S arr2[2];

struct S2 {
  [[no_unique_address]] struct {} a;
  [[no_unique_address ("foobar")]] struct {} b;		// { dg-error "'no_unique_address' attribute does not take any arguments" }
  [[no_unique_address (0)]] struct {} c;		// { dg-error "'no_unique_address' attribute does not take any arguments" }
  struct {} d [[no_unique_address]];
};

void
foo (int n)
{
  auto a = [] [[no_unique_address]] () { };		// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
  auto b = [] constexpr [[no_unique_address]] {};	// { dg-warning "'no_unique_address' attribute does not apply to types" }
							// { dg-error "parameter declaration before lambda declaration specifiers only optional with" "" { target c++20_down } .-1 }
							// { dg-error "'constexpr' lambda only available with" "" { target c++14_down } .-2 }
  auto c = [] noexcept [[no_unique_address]] {};	// { dg-warning "'no_unique_address' attribute does not apply to types" }
							// { dg-error "parameter declaration before lambda exception specification only optional with" "" { target c++20_down } .-1 }
  auto d = [] () [[no_unique_address]] {};		// { dg-warning "'no_unique_address' attribute does not apply to types" }
  auto e = new int [n] [[no_unique_address]];		// { dg-warning "attributes ignored on outermost array type in new expression" }
  auto e2 = new int [n] [[no_unique_address]] [42];	// { dg-warning "attributes ignored on outermost array type in new expression" }
  auto f = new int [n][42] [[no_unique_address]];	// { dg-warning "'no_unique_address' attribute does not apply to types" }
  [[no_unique_address]];				// { dg-warning "attributes at the beginning of statement are ignored" }
  [[no_unique_address]] {}				// { dg-warning "attributes at the beginning of statement are ignored" }
  [[no_unique_address]] if (true) {}			// { dg-warning "attributes at the beginning of statement are ignored" }
  [[no_unique_address]] while (false) {}		// { dg-warning "attributes at the beginning of statement are ignored" }
  [[no_unique_address]] goto lab;			// { dg-warning "attributes at the beginning of statement are ignored" }
  [[no_unique_address]] lab:;				// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
  [[no_unique_address]] try {} catch (int) {}		// { dg-warning "attributes at the beginning of statement are ignored" }
  if ([[no_unique_address]] int x = 0) {}		// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
  switch (n)
    {
    [[no_unique_address]] case 1:			// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
    [[no_unique_address]] break;			// { dg-warning "attributes at the beginning of statement are ignored" }
    [[no_unique_address]] default:			// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
	 break;
    }
  for ([[no_unique_address]] auto a : arr) {}		// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
  for ([[no_unique_address]] auto [a, b] : arr2) {}	// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
							// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
  [[no_unique_address]] asm ("");			// { dg-warning "attributes ignored on 'asm' declaration" }
  try {} catch ([[no_unique_address]] int x) {}		// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
  try {} catch ([[no_unique_address]] int) {}		// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
  try {} catch (int [[no_unique_address]] x) {}		// { dg-warning "attribute ignored" }
  try {} catch (int [[no_unique_address]]) {}		// { dg-warning "attribute ignored" }
  try {} catch (int x [[no_unique_address]]) {}		// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
}

[[no_unique_address]] int bar ();			// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
using foobar [[no_unique_address]] = int;		// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
[[no_unique_address]] int a;				// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
[[no_unique_address]] auto [b, c] = arr;		// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
							// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
[[no_unique_address]];					// { dg-warning "attribute ignored" }
inline [[no_unique_address]] void baz () {}		// { dg-warning "attribute ignored" }
							// { dg-error "standard attributes in middle of decl-specifiers" "" { target *-*-* } .-1 }
constexpr [[no_unique_address]] int qux () { return 0; }	// { dg-warning "attribute ignored" }
							// { dg-error "standard attributes in middle of decl-specifiers" "" { target *-*-* } .-1 }
int [[no_unique_address]] d;				// { dg-warning "attribute ignored" }
int const [[no_unique_address]] e = 1;			// { dg-warning "attribute ignored" }
struct A {} [[no_unique_address]];			// { dg-warning "attribute ignored in declaration of 'struct A'" }
struct A [[no_unique_address]];				// { dg-warning "attribute ignored" }
struct A [[no_unique_address]] a1;			// { dg-warning "attribute ignored" }
A [[no_unique_address]] a2;				// { dg-warning "attribute ignored" }
enum B { B0 } [[no_unique_address]];			// { dg-warning "attribute ignored in declaration of 'enum B'" }
enum B [[no_unique_address]];				// { dg-warning "attribute ignored" }
enum B [[no_unique_address]] b1;			// { dg-warning "attribute ignored" }
B [[no_unique_address]] b2;				// { dg-warning "attribute ignored" }
struct [[no_unique_address]] C {};			// { dg-warning "'no_unique_address' attribute does not apply to types" }
int f [[no_unique_address]];				// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
int g[2] [[no_unique_address]];				// { dg-warning "'no_unique_address' attribute does not apply to types" }
int g2 [[no_unique_address]] [2];			// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
int corge () [[no_unique_address]];			// { dg-warning "'no_unique_address' attribute does not apply to types" }
int *[[no_unique_address]] h;				// { dg-warning "'no_unique_address' attribute does not apply to types" }
int & [[no_unique_address]] i = f;			// { dg-warning "'no_unique_address' attribute does not apply to types" }
int && [[no_unique_address]] j = 0;			// { dg-warning "'no_unique_address' attribute does not apply to types" }
int S::* [[no_unique_address]] k;			// { dg-warning "'no_unique_address' attribute does not apply to types" }
auto l = sizeof (int [2] [[no_unique_address]]);	// { dg-warning "'no_unique_address' attribute does not apply to types" }
int freddy ([[no_unique_address]] int a,		// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
	    [[no_unique_address]] int,			// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
	    [[no_unique_address]] int c = 0,		// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
	    [[no_unique_address]] int = 0);		// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
void
corge ([[no_unique_address]] int a,			// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
       [[no_unique_address]] int,			// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
       [[no_unique_address]] int c = 0,			// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
       [[no_unique_address]] int = 0)			// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
{
}
[[no_unique_address]] void
garply ()						// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
{
}
int grault (int [[no_unique_address]] a,		// { dg-warning "attribute ignored" }
	    int [[no_unique_address]],			// { dg-warning "attribute ignored" }
	    int [[no_unique_address]] c = 0,		// { dg-warning "attribute ignored" }
	    int [[no_unique_address]] = 0);		// { dg-warning "attribute ignored" }
void
waldo (int [[no_unique_address]] a,			// { dg-warning "attribute ignored" }
       int [[no_unique_address]],			// { dg-warning "attribute ignored" }
       int [[no_unique_address]] c = 0,			// { dg-warning "attribute ignored" }
       int [[no_unique_address]] = 0)			// { dg-warning "attribute ignored" }
{
}
int plugh (int a [[no_unique_address]],			// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
	    int b [[no_unique_address]] = 0);		// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
void
thud (int a [[no_unique_address]],			// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
      int b [[no_unique_address]] = 0)			// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
{
}
enum [[no_unique_address]] D { D0 };			// { dg-warning "'no_unique_address' attribute does not apply to types" }
enum class [[no_unique_address]] E { E0 };		// { dg-warning "'no_unique_address' attribute does not apply to types" }
enum F {};
enum [[no_unique_address]] F;				// { dg-warning "'no_unique_address' attribute does not apply to types" }
enum G {
  G0 [[no_unique_address]],				// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
  G1 [[no_unique_address]] = 2				// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
};
namespace [[no_unique_address]] H { using H0 = int; }	// { dg-warning "'no_unique_address' attribute directive ignored" }
namespace [[no_unique_address]] {}			// { dg-warning "'no_unique_address' attribute directive ignored" }
[[no_unique_address]] using namespace H;		// { dg-warning "'no_unique_address' attribute directive ignored" }
struct [[no_unique_address]] I				// { dg-warning "'no_unique_address' attribute does not apply to types" }
{
  [[no_unique_address]];				// { dg-error "declaration does not declare anything" }
  [[no_unique_address]] int i;
  [[no_unique_address]] int foo ();			// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
  [[no_unique_address]] int bar () { return 1; }	// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
  [[no_unique_address]] int : 0;			// { dg-warning "'no_unique_address' attribute cannot be applied to a bit-field" }
  [[no_unique_address]] int i2 : 5;			// { dg-warning "'no_unique_address' attribute cannot be applied to a bit-field" }
  [[no_unique_address]] static int i3;			// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
  static int i4;
};
[[no_unique_address]] int I::i4 = 0;			// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
struct J : [[no_unique_address]] C {};			// { dg-warning "attributes on base specifiers are ignored" }
#if __cpp_concepts >= 201907L
template <typename T>
concept K [[no_unique_address]] = requires { true; };	// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" "" { target c++20 } }
#endif
typedef int L [[no_unique_address]];			// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
template <typename T>
struct M {};
template <>
struct [[no_unique_address]] M<int> { int m; };		// { dg-warning "'no_unique_address' attribute does not apply to types" }
typedef int N[2] [[no_unique_address]];			// { dg-warning "'no_unique_address' attribute does not apply to types" }
typedef int O [[no_unique_address]] [2];		// { dg-warning "'no_unique_address' attribute can only be applied to non-static data members" }
