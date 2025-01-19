// C++ 26 P2552R3 - On the ignorability of standard attributes
// { dg-do compile { target c++11 } }

int arr[2];
struct S { int a, b; };
S arr2[2];

void
foo (int n)
{
  switch (n)
    {
    case 1:
      [[fallthrough (n > 0)]];			// { dg-error "'fallthrough' attribute does not take any arguments" }
    case 2:
      break;
    case 3:
      [[fallthrough]];
    case 4:
      break;
    case 5:
      [[fallthrough ("abc")]];			// { dg-error "'fallthrough' attribute does not take any arguments" }
    case 6:
      break;
    case 7:
      [[fallthrough (1, 2, 3)]];		// { dg-error "'fallthrough' attribute does not take any arguments" }
    case 8:
      [[fallthrough]];				// { dg-error "attribute 'fallthrough' not preceding a case label or default label" }
      foo (n - 1);
      break;
    default:
      break;
    }

  [[fallthrough]] int x1;			// { dg-error "'fallthrough' attribute ignored" }

  auto a = [] [[fallthrough]] () {};		// { dg-error "'fallthrough' attribute ignored" }
  auto b = [] constexpr [[fallthrough]] {};	// { dg-error "'fallthrough' attribute ignored" }
						// { dg-error "parameter declaration before lambda declaration specifiers only optional with" "" { target c++20_down } .-1 }
						// { dg-error "'constexpr' lambda only available with" "" { target c++14_down } .-2 }
  auto c = [] noexcept [[fallthrough]] {};	// { dg-error "'fallthrough' attribute ignored" }
						// { dg-error "parameter declaration before lambda exception specification only optional with" "" { target c++20_down } .-1 }
  auto d = [] () [[fallthrough]] {};		// { dg-error "'fallthrough' attribute ignored" }
  auto e = new int [n] [[fallthrough]];		// { dg-warning "attributes ignored on outermost array type in new expression" }
  auto e2 = new int [n] [[fallthrough]] [42];	// { dg-warning "attributes ignored on outermost array type in new expression" }
  auto f = new int [n][42] [[fallthrough]];	// { dg-error "'fallthrough' attribute ignored" }
  [[fallthrough]] {}				// { dg-warning "attributes at the beginning of statement are ignored" }
  [[fallthrough]] if (true) {}			// { dg-warning "attributes at the beginning of statement are ignored" }
  [[fallthrough]] while (false) {}		// { dg-warning "attributes at the beginning of statement are ignored" }
  [[fallthrough]] goto lab;			// { dg-warning "attributes at the beginning of statement are ignored" }
  [[fallthrough]] lab:;				// { dg-error "'fallthrough' attribute ignored" }
  [[fallthrough]] try {} catch (int) {}		// { dg-warning "attributes at the beginning of statement are ignored" }
  if ([[fallthrough]] int x = 0) {}		// { dg-error "'fallthrough' attribute ignored" }
  switch (n)
    {
    [[fallthrough]] case 1:			// { dg-error "'fallthrough' attribute ignored" }
    [[fallthrough]] break;			// { dg-warning "attributes at the beginning of statement are ignored" }
    [[fallthrough]] default:			// { dg-error "'fallthrough' attribute ignored" }
	 break;
    }
  for ([[fallthrough]] auto a : arr) {}		// { dg-error "'fallthrough' attribute ignored" }
  for ([[fallthrough]] auto [a, b] : arr2) {}	// { dg-error "'fallthrough' attribute ignored" }
						// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
  [[fallthrough]] asm ("");			// { dg-warning "attributes ignored on 'asm' declaration" }
  try {} catch ([[fallthrough]] int x) {}	// { dg-error "'fallthrough' attribute ignored" }
  try {} catch ([[fallthrough]] int) {}		// { dg-error "'fallthrough' attribute ignored" }
  try {} catch (int [[fallthrough]] x) {}	// { dg-warning "attribute ignored" }
  try {} catch (int [[fallthrough]]) {}		// { dg-warning "attribute ignored" }
  try {} catch (int x [[fallthrough]]) {}	// { dg-error "'fallthrough' attribute ignored" }
}

[[fallthrough]] int bar ();			// { dg-error "'fallthrough' attribute ignored" }
using foobar [[fallthrough]] = int;		// { dg-error "'fallthrough' attribute ignored" }
[[fallthrough]] int a;				// { dg-error "'fallthrough' attribute ignored" }
[[fallthrough]] auto [b, c] = arr;		// { dg-error "'fallthrough' attribute ignored" }
						// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
[[fallthrough]];				// { dg-warning "attribute ignored" }
inline [[fallthrough]] void baz () {}		// { dg-warning "attribute ignored" }
						// { dg-error "standard attributes in middle of decl-specifiers" "" { target *-*-* } .-1 }
constexpr [[fallthrough]] int qux () { return 0; }	// { dg-warning "attribute ignored" }
						// { dg-error "standard attributes in middle of decl-specifiers" "" { target *-*-* } .-1 }
int [[fallthrough]] d;				// { dg-warning "attribute ignored" }
int const [[fallthrough]] e = 1;		// { dg-warning "attribute ignored" }
struct A {} [[fallthrough]];			// { dg-warning "attribute ignored in declaration of 'struct A'" }
struct A [[fallthrough]];			// { dg-warning "attribute ignored" }
struct A [[fallthrough]] a1;			// { dg-warning "attribute ignored" }
A [[fallthrough]] a2;				// { dg-warning "attribute ignored" }
enum B { B0 } [[fallthrough]];			// { dg-warning "attribute ignored in declaration of 'enum B'" }
enum B [[fallthrough]];				// { dg-warning "attribute ignored" }
enum B [[fallthrough]] b1;			// { dg-warning "attribute ignored" }
B [[fallthrough]] b2;				// { dg-warning "attribute ignored" }
struct [[fallthrough]] C {};			// { dg-error "'fallthrough' attribute ignored" }
int f [[fallthrough]];				// { dg-error "'fallthrough' attribute ignored" }
int g[2] [[fallthrough]];			// { dg-error "'fallthrough' attribute ignored" }
int g2 [[fallthrough]] [2];			// { dg-error "'fallthrough' attribute ignored" }
int corge () [[fallthrough]];			// { dg-error "'fallthrough' attribute ignored" }
int *[[fallthrough]] h;				// { dg-error "'fallthrough' attribute ignored" }
int & [[fallthrough]] i = f;			// { dg-error "'fallthrough' attribute ignored" }
int && [[fallthrough]] j = 0;			// { dg-error "'fallthrough' attribute ignored" }
int S::* [[fallthrough]] k;			// { dg-error "'fallthrough' attribute ignored" }
auto l = sizeof (int [2] [[fallthrough]]);	// { dg-error "'fallthrough' attribute ignored" }
int freddy ([[fallthrough]] int a,		// { dg-error "'fallthrough' attribute ignored" }
	    [[fallthrough]] int,		// { dg-error "'fallthrough' attribute ignored" }
	    [[fallthrough]] int c = 0,		// { dg-error "'fallthrough' attribute ignored" }
	    [[fallthrough]] int = 0);		// { dg-error "'fallthrough' attribute ignored" }
void
corge ([[fallthrough]] int a,			// { dg-error "'fallthrough' attribute ignored" }
       [[fallthrough]] int,			// { dg-error "'fallthrough' attribute ignored" }
       [[fallthrough]] int c = 0,		// { dg-error "'fallthrough' attribute ignored" }
       [[fallthrough]] int = 0)			// { dg-error "'fallthrough' attribute ignored" }
{
}
[[fallthrough]] void
garply ()					// { dg-error "'fallthrough' attribute ignored" }
{
}
int grault (int [[fallthrough]] a,		// { dg-warning "attribute ignored" }
	    int [[fallthrough]],		// { dg-warning "attribute ignored" }
	    int [[fallthrough]] c = 0,		// { dg-warning "attribute ignored" }
	    int [[fallthrough]] = 0);		// { dg-warning "attribute ignored" }
void
waldo (int [[fallthrough]] a,			// { dg-warning "attribute ignored" }
       int [[fallthrough]],			// { dg-warning "attribute ignored" }
       int [[fallthrough]] c = 0,		// { dg-warning "attribute ignored" }
       int [[fallthrough]] = 0)			// { dg-warning "attribute ignored" }
{
}
int plugh (int a [[fallthrough]],		// { dg-error "'fallthrough' attribute ignored" }
	    int b [[fallthrough]] = 0);		// { dg-error "'fallthrough' attribute ignored" }
void
thud (int a [[fallthrough]],			// { dg-error "'fallthrough' attribute ignored" }
      int b [[fallthrough]] = 0)		// { dg-error "'fallthrough' attribute ignored" }
{
}
enum [[fallthrough]] D { D0 };			// { dg-error "'fallthrough' attribute ignored" }
enum class [[fallthrough]] E { E0 };		// { dg-error "'fallthrough' attribute ignored" }
enum F {};
enum [[fallthrough]] F;				// { dg-warning "type attributes ignored after type is already defined" }
enum G {
  G0 [[fallthrough]],				// { dg-error "'fallthrough' attribute ignored" }
  G1 [[fallthrough]] = 2			// { dg-error "'fallthrough' attribute ignored" }
};
namespace [[fallthrough]] H { using H0 = int; }	// { dg-warning "'fallthrough' attribute directive ignored" } */
namespace [[fallthrough]] {}			// { dg-warning "'fallthrough' attribute directive ignored" }
[[fallthrough]] using namespace H;		// { dg-warning "'fallthrough' attribute directive ignored" }
struct [[fallthrough]] I			// { dg-error "'fallthrough' attribute ignored" }
{
  [[fallthrough]];				// { dg-error "declaration does not declare anything" }
  [[fallthrough]] int i;			// { dg-error "'fallthrough' attribute ignored" }
  [[fallthrough]] int foo ();			// { dg-error "'fallthrough' attribute ignored" }
  [[fallthrough]] int bar () { return 1; }	// { dg-error "'fallthrough' attribute ignored" }
  [[fallthrough]] int : 0;			// { dg-error "'fallthrough' attribute ignored" }
  [[fallthrough]] int i2 : 5;			// { dg-error "'fallthrough' attribute ignored" }
  [[fallthrough]] static int i3;		// { dg-error "'fallthrough' attribute ignored" }
  static int i4;
};
[[fallthrough]] int I::i4 = 0;			// { dg-error "'fallthrough' attribute ignored" }
struct J : [[fallthrough]] C {};		// { dg-warning "attributes on base specifiers are ignored" }
#if __cpp_concepts >= 201907L
template <typename T>
concept K [[fallthrough]] = requires { true; };	// { dg-error "'fallthrough' attribute ignored" "" { target c++20 } }
#endif
typedef int L [[fallthrough]];			// { dg-error "'fallthrough' attribute ignored" }
template <typename T>
struct M {};
template <>
struct [[fallthrough]] M<int> { int m; };	// { dg-error "'fallthrough' attribute ignored" }
typedef int N[2] [[fallthrough]];		// { dg-error "'fallthrough' attribute ignored" }
typedef int O [[fallthrough]] [2];		// { dg-error "'fallthrough' attribute ignored" }
