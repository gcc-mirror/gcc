// C++ 26 P2552R3 - On the ignorability of standard attributes
// { dg-do compile { target c++11 } }

int arr[2];
struct S { int a, b; };
S arr2[2];

void
xyzzy (int *a [[carries_dependency]],			// { dg-warning "'carries_dependency' attribute ignored" }
       int *b [[carries_dependency (1)]])		// { dg-error "'carries_dependency' attribute does not take any arguments" }
{							// { dg-error "expected ',' or '...' before 'b'" "" { target *-*-* } .-1 }
}

void
foo (int n)
{
  [[carries_dependency]] int x1;			// { dg-warning "'carries_dependency' attribute can only be applied to functions or parameters" }

  auto a = [] [[carries_dependency]] () {};		// { dg-warning "'carries_dependency' attribute ignored" }
  auto b = [] constexpr [[carries_dependency]] {};	// { dg-warning "'carries_dependency' attribute does not apply to types" }
							// { dg-error "parameter declaration before lambda declaration specifiers only optional with" "" { target c++20_down } .-1 }
							// { dg-error "'constexpr' lambda only available with" "" { target c++14_down } .-2 }
  auto c = [] noexcept [[carries_dependency]] {};	// { dg-warning "'carries_dependency' attribute does not apply to types" }
							// { dg-error "parameter declaration before lambda exception specification only optional with" "" { target c++20_down } .-1 }
  auto d = [] () [[carries_dependency]] {};		// { dg-warning "'carries_dependency' attribute does not apply to types" }
  auto e = new int [n] [[carries_dependency]];		// { dg-warning "attributes ignored on outermost array type in new expression" }
  auto e2 = new int [n] [[carries_dependency]] [42];	// { dg-warning "attributes ignored on outermost array type in new expression" }
  auto f = new int [n][42] [[carries_dependency]];	// { dg-warning "'carries_dependency' attribute does not apply to types" }
  [[carries_dependency]];				// { dg-warning "attributes at the beginning of statement are ignored" }
  [[carries_dependency]] {}				// { dg-warning "attributes at the beginning of statement are ignored" }
  [[carries_dependency]] if (true) {}			// { dg-warning "attributes at the beginning of statement are ignored" }
  [[carries_dependency]] while (false) {}		// { dg-warning "attributes at the beginning of statement are ignored" }
  [[carries_dependency]] goto lab;			// { dg-warning "attributes at the beginning of statement are ignored" }
  [[carries_dependency]] lab:;				// { dg-warning "'carries_dependency' attribute can only be applied to functions or parameters" }
  [[carries_dependency]] try {} catch (int) {}		// { dg-warning "attributes at the beginning of statement are ignored" }
  if ([[carries_dependency]] int x = 0) {}		// { dg-warning "'carries_dependency' attribute can only be applied to functions or parameters" }
  switch (n)
    {
    [[carries_dependency]] case 1:			// { dg-warning "'carries_dependency' attribute can only be applied to functions or parameters" }
    [[carries_dependency]] break;			// { dg-warning "attributes at the beginning of statement are ignored" }
    [[carries_dependency]] default:			// { dg-warning "'carries_dependency' attribute can only be applied to functions or parameters" }
	 break;
    }
  for ([[carries_dependency]] auto a : arr) {}		// { dg-warning "'carries_dependency' attribute can only be applied to functions or parameters" }
  for ([[carries_dependency]] auto [a, b] : arr2) {}	// { dg-warning "'carries_dependency' attribute can only be applied to functions or parameters" }
							// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
  [[carries_dependency]] asm ("");			// { dg-warning "attributes ignored on 'asm' declaration" }
  try {} catch ([[carries_dependency]] int x) {}	// { dg-warning "'carries_dependency' attribute can only be applied to functions or parameters" }
  try {} catch ([[carries_dependency]] int) {}		// { dg-warning "'carries_dependency' attribute can only be applied to functions or parameters" }
  try {} catch (int [[carries_dependency]] x) {}	// { dg-warning "attribute ignored" }
  try {} catch (int [[carries_dependency]]) {}		// { dg-warning "attribute ignored" }
  try {} catch (int x [[carries_dependency]]) {}	// { dg-warning "'carries_dependency' attribute can only be applied to functions or parameters" }
}

[[carries_dependency]] int bar ();			// { dg-warning "'carries_dependency' attribute ignored" }
using foobar [[carries_dependency]] = int;		// { dg-warning "'carries_dependency' attribute can only be applied to functions or parameters" }
[[carries_dependency]] int a;				// { dg-warning "'carries_dependency' attribute can only be applied to functions or parameters" }
[[carries_dependency]] auto [b, c] = arr;		// { dg-warning "'carries_dependency' attribute can only be applied to functions or parameters" }
							// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
[[carries_dependency]];					// { dg-warning "attribute ignored" }
inline [[carries_dependency]] void baz () {}		// { dg-warning "attribute ignored" }
							// { dg-error "standard attributes in middle of decl-specifiers" "" { target *-*-* } .-1 }
constexpr [[carries_dependency]] int qux () { return 0; }	// { dg-warning "attribute ignored" }
							// { dg-error "standard attributes in middle of decl-specifiers" "" { target *-*-* } .-1 }
int [[carries_dependency]] d;				// { dg-warning "attribute ignored" }
int const [[carries_dependency]] e = 1;			// { dg-warning "attribute ignored" }
struct A {} [[carries_dependency]];			// { dg-warning "attribute ignored in declaration of 'struct A'" }
struct A [[carries_dependency]];			// { dg-warning "attribute ignored" }
struct A [[carries_dependency]] a1;			// { dg-warning "attribute ignored" }
A [[carries_dependency]] a2;				// { dg-warning "attribute ignored" }
enum B { B0 } [[carries_dependency]];			// { dg-warning "attribute ignored in declaration of 'enum B'" }
enum B [[carries_dependency]];				// { dg-warning "attribute ignored" }
enum B [[carries_dependency]] b1;			// { dg-warning "attribute ignored" }
B [[carries_dependency]] b2;				// { dg-warning "attribute ignored" }
struct [[carries_dependency]] C {};			// { dg-warning "'carries_dependency' attribute does not apply to types" }
int f [[carries_dependency]];				// { dg-warning "'carries_dependency' attribute can only be applied to functions or parameters" }
int g[2] [[carries_dependency]];			// { dg-warning "'carries_dependency' attribute does not apply to types" }
int g2 [[carries_dependency]] [2];			// { dg-warning "'carries_dependency' attribute can only be applied to functions or parameters" }
int corge () [[carries_dependency]];			// { dg-warning "'carries_dependency' attribute does not apply to types" }
int *[[carries_dependency]] h;				// { dg-warning "'carries_dependency' attribute does not apply to types" }
int & [[carries_dependency]] i = f;			// { dg-warning "'carries_dependency' attribute does not apply to types" }
int && [[carries_dependency]] j = 0;			// { dg-warning "'carries_dependency' attribute does not apply to types" }
int S::* [[carries_dependency]] k;			// { dg-warning "'carries_dependency' attribute does not apply to types" }
auto l = sizeof (int [2] [[carries_dependency]]);	// { dg-warning "'carries_dependency' attribute does not apply to types" }
int freddy ([[carries_dependency]] int a,		// { dg-warning "'carries_dependency' attribute ignored" }
	    [[carries_dependency]] int,			// { dg-warning "'carries_dependency' attribute ignored" }
	    [[carries_dependency]] int c = 0,		// { dg-warning "'carries_dependency' attribute ignored" }
	    [[carries_dependency]] int = 0);		// { dg-warning "'carries_dependency' attribute ignored" }
void
corge ([[carries_dependency]] int a,			// { dg-warning "'carries_dependency' attribute ignored" }
       [[carries_dependency]] int,			// { dg-warning "'carries_dependency' attribute ignored" }
       [[carries_dependency]] int c = 0,		// { dg-warning "'carries_dependency' attribute ignored" }
       [[carries_dependency]] int = 0)			// { dg-warning "'carries_dependency' attribute ignored" }
{
}
[[carries_dependency]] void
garply ()						// { dg-warning "'carries_dependency' attribute ignored" }
{
}
int grault (int [[carries_dependency]] a,		// { dg-warning "attribute ignored" }
	    int [[carries_dependency]],			// { dg-warning "attribute ignored" }
	    int [[carries_dependency]] c = 0,		// { dg-warning "attribute ignored" }
	    int [[carries_dependency]] = 0);		// { dg-warning "attribute ignored" }
void
waldo (int [[carries_dependency]] a,			// { dg-warning "attribute ignored" }
       int [[carries_dependency]],			// { dg-warning "attribute ignored" }
       int [[carries_dependency]] c = 0,		// { dg-warning "attribute ignored" }
       int [[carries_dependency]] = 0)			// { dg-warning "attribute ignored" }
{
}
int plugh (int a [[carries_dependency]],		// { dg-warning "'carries_dependency' attribute ignored" }
	    int b [[carries_dependency]] = 0);		// { dg-warning "'carries_dependency' attribute ignored" }
void
thud (int a [[carries_dependency]],			// { dg-warning "'carries_dependency' attribute ignored" }
      int b [[carries_dependency]] = 0)			// { dg-warning "'carries_dependency' attribute ignored" }
{
}
enum [[carries_dependency]] D { D0 };			// { dg-warning "'carries_dependency' attribute does not apply to types" }
enum class [[carries_dependency]] E { E0 };		// { dg-warning "'carries_dependency' attribute does not apply to types" }
enum F {};
enum [[carries_dependency]] F;				// { dg-warning "'carries_dependency' attribute does not apply to types" }
enum G {
  G0 [[carries_dependency]],				// { dg-warning "'carries_dependency' attribute can only be applied to functions or parameters" }
  G1 [[carries_dependency]] = 2				// { dg-warning "'carries_dependency' attribute can only be applied to functions or parameters" }
};
namespace [[carries_dependency]] H { using H0 = int; }	// { dg-warning "'carries_dependency' attribute directive ignored" } */
namespace [[carries_dependency]] {}			// { dg-warning "'carries_dependency' attribute directive ignored" }
[[carries_dependency]] using namespace H;		// { dg-warning "'carries_dependency' attribute directive ignored" }
struct [[carries_dependency]] I				// { dg-warning "'carries_dependency' attribute does not apply to types" }
{
  [[carries_dependency]];				// { dg-error "declaration does not declare anything" }
  [[carries_dependency]] int i;				// { dg-warning "'carries_dependency' attribute can only be applied to functions or parameters" }
  [[carries_dependency]] int foo ();			// { dg-warning "'carries_dependency' attribute ignored" }
  [[carries_dependency]] int bar () { return 1; }	// { dg-warning "'carries_dependency' attribute ignored" }
  [[carries_dependency]] int : 0;			// { dg-warning "'carries_dependency' attribute can only be applied to functions or parameters" }
  [[carries_dependency]] int i2 : 5;			// { dg-warning "'carries_dependency' attribute can only be applied to functions or parameters" }
  [[carries_dependency]] static int i3;			// { dg-warning "'carries_dependency' attribute can only be applied to functions or parameters" }
  static int i4;
};
[[carries_dependency]] int I::i4 = 0;			// { dg-warning "'carries_dependency' attribute can only be applied to functions or parameters" }
struct J : [[carries_dependency]] C {};			// { dg-warning "attributes on base specifiers are ignored" }
#if __cpp_concepts >= 201907L
template <typename T>
concept K [[carries_dependency]] = requires { true; };	// { dg-warning "'carries_dependency' attribute can only be applied to functions or parameters" "" { target c++20 } }
#endif
typedef int L [[carries_dependency]];			// { dg-warning "'carries_dependency' attribute can only be applied to functions or parameters" }
template <typename T>
struct M {};
template <>
struct [[carries_dependency]] M<int> { int m; };	// { dg-warning "'carries_dependency' attribute does not apply to types" }
typedef int N[2] [[carries_dependency]];		// { dg-warning "'carries_dependency' attribute does not apply to types" }
typedef int O [[carries_dependency]] [2];		// { dg-warning "'carries_dependency' attribute can only be applied to functions or parameters" }
