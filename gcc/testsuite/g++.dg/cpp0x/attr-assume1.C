// C++ 26 P2552R3 - On the ignorability of standard attributes
// { dg-do compile { target c++11 } }

int arr[2];
struct S { int a, b; };
S arr2[2];

void
foo (int n)
{
  [[assume (n > 0)]];
  [[assume]];					// { dg-error "wrong number of arguments specified for 'assume' attribute" }
  [[assume ("abc")]];
  [[assume (1, 2, 3)]];				// { dg-error "wrong number of arguments specified for 'assume' attribute" }

  [[assume (true)]] int x1;			// { dg-error "'assume' attribute ignored" }

  auto a = [] [[assume (true)]] () {};		// { dg-error "'assume' attribute ignored" }
  auto b = [] constexpr [[assume (true)]] {};	// { dg-error "'assume' attribute ignored" }
						// { dg-error "parameter declaration before lambda declaration specifiers only optional with" "" { target c++20_down } .-1 }
						// { dg-error "'constexpr' lambda only available with" "" { target c++14_down } .-2 }
  auto c = [] noexcept [[assume (true)]] {};	// { dg-error "'assume' attribute ignored" }
						// { dg-error "parameter declaration before lambda exception specification only optional with" "" { target c++20_down } .-1 }
  auto d = [] () [[assume (true)]] {};		// { dg-error "'assume' attribute ignored" }
  auto e = new int [n] [[assume (true)]];	// { dg-warning "attributes ignored on outermost array type in new expression" }
  auto e2 = new int [n] [[assume (true)]] [42];	// { dg-warning "attributes ignored on outermost array type in new expression" }
  auto f = new int [n][42] [[assume (true)]];	// { dg-error "'assume' attribute ignored" }
  [[assume (true)]];
  [[assume (true)]] {}				// { dg-warning "attributes at the beginning of statement are ignored" }
  [[assume (true)]] if (true) {}		// { dg-warning "attributes at the beginning of statement are ignored" }
  [[assume (true)]] while (false) {}		// { dg-warning "attributes at the beginning of statement are ignored" }
  [[assume (true)]] goto lab;			// { dg-warning "attributes at the beginning of statement are ignored" }
  [[assume (true)]] lab:;			// { dg-error "'assume' attribute ignored" }
  [[assume (true)]] try {} catch (int) {}	// { dg-warning "attributes at the beginning of statement are ignored" }
  if ([[assume (true)]] int x = 0) {}		// { dg-error "'assume' attribute ignored" }
  switch (n)
    {
    [[assume (true)]] case 1:			// { dg-error "'assume' attribute ignored" }
    [[assume (true)]] break;			// { dg-warning "attributes at the beginning of statement are ignored" }
    [[assume (true)]] default:			// { dg-error "'assume' attribute ignored" }
	 break;
    }
  for ([[assume (true)]] auto a : arr) {}	// { dg-error "'assume' attribute ignored" }
  for ([[assume (true)]] auto [a, b] : arr2) {}	// { dg-error "'assume' attribute ignored" }
						// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
  [[assume (true)]] asm ("");			// { dg-warning "attributes ignored on 'asm' declaration" }
  try {} catch ([[assume (true)]] int x) {}	// { dg-error "'assume' attribute ignored" }
  try {} catch ([[assume (true)]] int) {}	// { dg-error "'assume' attribute ignored" }
  try {} catch (int [[assume (true)]] x) {}	// { dg-warning "attribute ignored" }
  try {} catch (int [[assume (true)]]) {}	// { dg-warning "attribute ignored" }
  try {} catch (int x [[assume (true)]]) {}	// { dg-error "'assume' attribute ignored" }
}

[[assume (true)]] int bar ();			// { dg-error "'assume' attribute ignored" }
using foobar [[assume (true)]] = int;		// { dg-error "'assume' attribute ignored" }
[[assume (true)]] int a;			// { dg-error "'assume' attribute ignored" }
[[assume (true)]] auto [b, c] = arr;		// { dg-error "'assume' attribute ignored" }
						// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
[[assume (true)]];				// { dg-warning "attribute ignored" }
inline [[assume (true)]] void baz () {}		// { dg-warning "attribute ignored" }
						// { dg-error "standard attributes in middle of decl-specifiers" "" { target *-*-* } .-1 }
constexpr [[assume (true)]] int qux () { return 0; }	// { dg-warning "attribute ignored" }
						// { dg-error "standard attributes in middle of decl-specifiers" "" { target *-*-* } .-1 }
int [[assume (true)]] d;			// { dg-warning "attribute ignored" }
int const [[assume (true)]] e = 1;		// { dg-warning "attribute ignored" }
struct A {} [[assume (true)]];			// { dg-warning "attribute ignored in declaration of 'struct A'" }
struct A [[assume (true)]];			// { dg-warning "attribute ignored" }
struct A [[assume (true)]] a1;			// { dg-warning "attribute ignored" }
A [[assume (true)]] a2;				// { dg-warning "attribute ignored" }
enum B { B0 } [[assume (true)]];		// { dg-warning "attribute ignored in declaration of 'enum B'" }
enum B [[assume (true)]];			// { dg-warning "attribute ignored" }
enum B [[assume (true)]] b1;			// { dg-warning "attribute ignored" }
B [[assume (true)]] b2;				// { dg-warning "attribute ignored" }
struct [[assume (true)]] C {};			// { dg-error "'assume' attribute ignored" }
int f [[assume (true)]];			// { dg-error "'assume' attribute ignored" }
int g[2] [[assume (true)]];			// { dg-error "'assume' attribute ignored" }
int g2 [[assume (true)]] [2];			// { dg-error "'assume' attribute ignored" }
int corge () [[assume (true)]];			// { dg-error "'assume' attribute ignored" }
int *[[assume (true)]] h;			// { dg-error "'assume' attribute ignored" }
int & [[assume (true)]] i = f;			// { dg-error "'assume' attribute ignored" }
int && [[assume (true)]] j = 0;			// { dg-error "'assume' attribute ignored" }
int S::* [[assume (true)]] k;			// { dg-error "'assume' attribute ignored" }
auto l = sizeof (int [2] [[assume (true)]]);	// { dg-error "'assume' attribute ignored" }
int freddy ([[assume (true)]] int a,		// { dg-error "'assume' attribute ignored" }
	    [[assume (true)]] int,		// { dg-error "'assume' attribute ignored" }
	    [[assume (true)]] int c = 0,	// { dg-error "'assume' attribute ignored" }
	    [[assume (true)]] int = 0);		// { dg-error "'assume' attribute ignored" }
void
corge ([[assume (true)]] int a,			// { dg-error "'assume' attribute ignored" }
       [[assume (true)]] int,			// { dg-error "'assume' attribute ignored" }
       [[assume (true)]] int c = 0,		// { dg-error "'assume' attribute ignored" }
       [[assume (true)]] int = 0)		// { dg-error "'assume' attribute ignored" }
{
}
[[assume (true)]] void
garply ()					// { dg-error "'assume' attribute ignored" }
{
}
int grault (int [[assume (true)]] a,		// { dg-warning "attribute ignored" }
	    int [[assume (true)]],		// { dg-warning "attribute ignored" }
	    int [[assume (true)]] c = 0,	// { dg-warning "attribute ignored" }
	    int [[assume (true)]] = 0);		// { dg-warning "attribute ignored" }
void
waldo (int [[assume (true)]] a,			// { dg-warning "attribute ignored" }
       int [[assume (true)]],			// { dg-warning "attribute ignored" }
       int [[assume (true)]] c = 0,		// { dg-warning "attribute ignored" }
       int [[assume (true)]] = 0)		// { dg-warning "attribute ignored" }
{
}
int plugh (int a [[assume (true)]],		// { dg-error "'assume' attribute ignored" }
	    int b [[assume (true)]] = 0);	// { dg-error "'assume' attribute ignored" }
void
thud (int a [[assume (true)]],			// { dg-error "'assume' attribute ignored" }
      int b [[assume (true)]] = 0)		// { dg-error "'assume' attribute ignored" }
{
}
enum [[assume (true)]] D { D0 };		// { dg-error "'assume' attribute ignored" }
enum class [[assume (true)]] E { E0 };		// { dg-error "'assume' attribute ignored" }
enum F {};
enum [[assume (true)]] F;			// { dg-warning "type attributes ignored after type is already defined" }
enum G {
  G0 [[assume (true)]],				// { dg-error "'assume' attribute ignored" }
  G1 [[assume (true)]] = 2			// { dg-error "'assume' attribute ignored" }
};
namespace [[assume (true)]] H { using H0 = int; }	// { dg-warning "'assume' attribute directive ignored" } */
namespace [[assume (true)]] {}			// { dg-warning "'assume' attribute directive ignored" }
[[assume (true)]] using namespace H;		// { dg-warning "'assume' attribute directive ignored" }
struct [[assume (true)]] I			// { dg-error "'assume' attribute ignored" }
{
  [[assume (true)]];				// { dg-error "declaration does not declare anything" }
  [[assume (true)]] int i;			// { dg-error "'assume' attribute ignored" }
  [[assume (true)]] int foo ();			// { dg-error "'assume' attribute ignored" }
  [[assume (true)]] int bar () { return 1; }	// { dg-error "'assume' attribute ignored" }
  [[assume (true)]] int : 0;			// { dg-error "'assume' attribute ignored" }
  [[assume (true)]] int i2 : 5;			// { dg-error "'assume' attribute ignored" }
  [[assume (true)]] static int i3;		// { dg-error "'assume' attribute ignored" }
  static int i4;
};
[[assume (true)]] int I::i4 = 0;		// { dg-error "'assume' attribute ignored" }
struct J : [[assume (true)]] C {};		// { dg-warning "attributes on base specifiers are ignored" }
#if __cpp_concepts >= 201907L
template <typename T>
concept K [[assume (true)]] = requires { true; };	// { dg-error "'assume' attribute ignored" "" { target c++20 } }
#endif
typedef int L [[assume (true)]];		// { dg-error "'assume' attribute ignored" }
template <typename T>
struct M {};
template <>
struct [[assume (true)]] M<int> { int m; };	// { dg-error "'assume' attribute ignored" }
typedef int N[2] [[assume (true)]];		// { dg-error "'assume' attribute ignored" }
typedef int O [[assume (true)]] [2];		// { dg-error "'assume' attribute ignored" }
