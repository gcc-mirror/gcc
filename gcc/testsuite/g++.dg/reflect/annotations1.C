// C++ 26 P3394R4 - Annotations for Reflection
// { dg-do compile { target c++26 } }

int arr[2];
struct S { int a, b; };
S arr2[2];

void
foo (int n)
{
  [[=1]] int x1;
  auto a = [] [[=1]] () {};
  auto b = [] constexpr [[=1]] {};	// { dg-error "annotation on a type other than class or enumeration definition" }
  auto c = [] noexcept [[=1]] {};	// { dg-error "annotation on a type other than class or enumeration definition" }
  auto d = [] () [[=1]] {};		// { dg-error "annotation on a type other than class or enumeration definition" }
  auto e = new int [n] [[=1]];		// { dg-warning "attributes ignored on outermost array type in new expression" }
  auto e2 = new int [n] [[=1]] [42];	// { dg-warning "attributes ignored on outermost array type in new expression" }
  auto f = new int [n][42] [[=1]];	// { dg-error "annotation on a type other than class or enumeration definition" }
  [[=1]];				// { dg-warning "attributes at the beginning of statement are ignored" }
  [[=1]] {}				// { dg-warning "attributes at the beginning of statement are ignored" }
  [[=1]] if (true) {}			// { dg-warning "attributes at the beginning of statement are ignored" }
  [[=1]] while (false) {}		// { dg-warning "attributes at the beginning of statement are ignored" }
  [[=1]] goto lab;			// { dg-warning "attributes at the beginning of statement are ignored" }
  [[=1]] lab:;				// { dg-error "annotation applied to a label" }
  [[=1]] try {} catch (int) {}		// { dg-warning "attributes at the beginning of statement are ignored" }
  if ([[=1]] int x = 0) {}
  switch (n)
    {
    [[=1]] case 1:			// { dg-error "annotation applied to a label" }
    [[=1]] break;			// { dg-warning "attributes at the beginning of statement are ignored" }
    [[=1]] default:			// { dg-error "annotation applied to a label" }
	   break;
    }
  for ([[=1]] auto a : arr) {}
  for ([[=1]] auto [a, b] : arr2) {}
  [[=1]] asm ("");			// { dg-warning "attributes ignored on 'asm' declaration" }
  try {} catch ([[=1]] int x) {}
  try {} catch ([[=1]] int) {}
  try {} catch (int [[=1]] x) {}	// { dg-warning "attribute ignored" }
  try {} catch (int [[=1]]) {}		// { dg-warning "attribute ignored" }
  try {} catch (int x [[=1]]) {}
}

[[=1]] int bar ();
using foobar [[=1]] = int;
[[=1]] int a;
[[=1]] auto [b, c] = arr;
auto [b3 [[=1]], c3 [[=1]]] = arr;	// { dg-error "annotation on structured binding" }
[[=1]];					// { dg-warning "attribute ignored" }
inline [[=1]] void baz () {}		// { dg-warning "attribute ignored" }
						// { dg-error "standard attributes in middle of decl-specifiers" "" { target *-*-* } .-1 }
constexpr [[=1]] int qux () { return 0; }	// { dg-warning "attribute ignored" }
						// { dg-error "standard attributes in middle of decl-specifiers" "" { target *-*-* } .-1 }
int [[=1]] d;				// { dg-warning "attribute ignored" }
int const [[=1]] e = 1;			// { dg-warning "attribute ignored" }
struct A {} [[=1]];			// { dg-warning "attribute ignored in declaration of 'struct A'" }
struct A [[=1]];			// { dg-warning "attribute ignored" }
struct A [[=1]] a1;			// { dg-warning "attribute ignored" }
A [[=1]] a2;				// { dg-warning "attribute ignored" }
enum B { B0 } [[=1]];			// { dg-warning "attribute ignored in declaration of 'enum B'" }
enum B [[=1]];				// { dg-warning "attribute ignored" }
enum B [[=1]] b1;			// { dg-warning "attribute ignored" }
B [[=1]] b2;				// { dg-warning "attribute ignored" }
struct [[=1]] C {};
int f [[=1]];
int g[2] [[=1]];			// { dg-error "annotation on a type other than class or enumeration definition" }
int g2 [[=1]] [2];
int corge () [[=1]];			// { dg-error "annotation on a type other than class or enumeration definition" }
int *[[=1]] h;				// { dg-error "annotation on a type other than class or enumeration definition" }
int & [[=1]] i = f;			// { dg-error "annotation on a type other than class or enumeration definition" }
int && [[=1]] j = 0;			// { dg-error "annotation on a type other than class or enumeration definition" }
int S::* [[=1]] k;			// { dg-error "annotation on a type other than class or enumeration definition" }
auto l = sizeof (int [2] [[=1]]);	// { dg-error "annotation on a type other than class or enumeration definition" }
int freddy ([[=1]] int a,
	    [[=1]] int,
	    [[=1]] int c = 0,
	    [[=1]] int = 0);
void
corge ([[=1]] int a,
       [[=1]] int,
       [[=1]] int c = 0,
       [[=1]] int = 0)
{
}
[[=1]] void
garply ()
{
}
int grault (int [[=1]] a,		// { dg-warning "attribute ignored" }
	    int [[=1]],			// { dg-warning "attribute ignored" }
	    int [[=1]] c = 0,		// { dg-warning "attribute ignored" }
	    int [[=1]] = 0);		// { dg-warning "attribute ignored" }
void
waldo (int [[=1]] a,			// { dg-warning "attribute ignored" }
       int [[=1]],			// { dg-warning "attribute ignored" }
       int [[=1]] c = 0,		// { dg-warning "attribute ignored" }
       int [[=1]] = 0)			// { dg-warning "attribute ignored" }
{
}
int plugh (int a [[=1]],
	    int b [[=1]] = 0);
void
thud (int a [[=1]],
      int b [[=1]] = 0)
{
}
enum [[=1]] D { D0 };
enum class [[=1]] E { E0 };
enum F {};
enum [[=1]] F;				// { dg-warning "type attributes ignored after type is already defined" }
enum G {
  G0 [[=1]],
  G1 [[=1]] = 2
};
namespace [[=1]] H { using H0 = int; }
namespace [[=1]] {}
[[=1]] using namespace H;		// { dg-error "annotation on using directive" }
struct [[=1]] I
{
  [[=1]];				// { dg-error "declaration does not declare anything" }
  [[=1]] int i;
  [[=1]] int foo ();
  [[=1]] int bar () { return 1; }
  [[=1]] int : 0;			// { dg-error "annotation on unnamed bit-field" }
  [[=1]] int i2 : 5;
  [[=1]] static int i3;
  static int i4;
};
[[=1]] int I::i4 = 0;
struct J : [[=1]] C {};
template <typename T>
concept K [[=1]] = requires { true; };
typedef int L [[=1]];
template <typename T>
struct M {};
template <>
struct [[=1]] M<int> { int m; };
typedef int N[2] [[=1]];		// { dg-error "annotation on a type other than class or enumeration definition" }
typedef int O [[=1]] [2];
