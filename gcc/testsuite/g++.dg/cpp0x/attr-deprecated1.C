// C++ 26 P2552R3 - On the ignorability of standard attributes
// { dg-do compile { target c++11 } }

int arr[2];
struct S { int a, b; };
S arr2[2];

void
foo (int n)
{
  [[deprecated]] int x1;
  [[deprecated ("foobar")]] int x2;
  [[deprecated (0)]] int x3;			// { dg-error "deprecated message is not a string" }
						// { dg-error "expected string-literal before numeric constant" "" { target c++26 } .-1 }
  [[deprecated ("foo", "bar", "baz")]] int x4;	// { dg-error "wrong number of arguments specified for 'deprecated' attribute" }
  [[deprecated (0, 1, 2)]] int x5;		// { dg-error "wrong number of arguments specified for 'deprecated' attribute" }
						// { dg-error "expected string-literal before numeric constant" "" { target c++26 } .-1 }

  auto a = [] [[deprecated]] () {};
  auto b = [] constexpr [[deprecated]] {};	// { dg-error "'deprecated' on a type other than class or enumeration definition" }
						// { dg-error "parameter declaration before lambda declaration specifiers only optional with" "" { target c++20_down } .-1 }
						// { dg-error "'constexpr' lambda only available with" "" { target c++14_down } .-2 }
  auto c = [] noexcept [[deprecated]] {};	// { dg-error "'deprecated' on a type other than class or enumeration definition" }
						// { dg-error "parameter declaration before lambda exception specification only optional with" "" { target c++20_down } .-1 }
  auto d = [] () [[deprecated]] {};		// { dg-error "'deprecated' on a type other than class or enumeration definition" }
  auto e = new int [n] [[deprecated]];		// { dg-warning "attributes ignored on outermost array type in new expression" }
  auto e2 = new int [n] [[deprecated]] [42];	// { dg-warning "attributes ignored on outermost array type in new expression" }
  auto f = new int [n][42] [[deprecated]];	// { dg-error "'deprecated' on a type other than class or enumeration definition" }
  [[deprecated]];				// { dg-warning "attributes at the beginning of statement are ignored" }
  [[deprecated]] {}				// { dg-warning "attributes at the beginning of statement are ignored" }
  [[deprecated]] if (true) {}			// { dg-warning "attributes at the beginning of statement are ignored" }
  [[deprecated]] while (false) {}		// { dg-warning "attributes at the beginning of statement are ignored" }
  [[deprecated]] goto lab;			// { dg-warning "attributes at the beginning of statement are ignored" }
  [[deprecated]] lab:;				// { dg-error "'deprecated' attribute ignored" }
  [[deprecated]] try {} catch (int) {}		// { dg-warning "attributes at the beginning of statement are ignored" }
  if ([[deprecated]] int x = 0) {}
  switch (n)
    {
    [[deprecated]] case 1:			// { dg-error "'deprecated' attribute ignored" }
    [[deprecated]] break;			// { dg-warning "attributes at the beginning of statement are ignored" }
    [[deprecated]] default:			// { dg-error "'deprecated' attribute ignored" }
	 break;
    }
  for ([[deprecated]] auto a : arr) {}
  for ([[deprecated]] auto [a, b] : arr2) {}	// { dg-error "structured bindings only available with" "" { target c++14_down } }
  [[deprecated]] asm ("");			// { dg-warning "attributes ignored on 'asm' declaration" }
  try {} catch ([[deprecated]] int x) {}
  try {} catch ([[deprecated]] int) {}
  try {} catch (int [[deprecated]] x) {}	// { dg-warning "attribute ignored" }
  try {} catch (int [[deprecated]]) {}		// { dg-warning "attribute ignored" }
  try {} catch (int x [[deprecated]]) {}
}

[[deprecated]] int bar ();
using foobar [[deprecated]] = int;
[[deprecated]] int a;
[[deprecated]] auto [b, c] = arr;		// { dg-error "structured bindings only available with" "" { target c++14_down } }
[[deprecated]];					// { dg-warning "attribute ignored" }
inline [[deprecated]] void baz () {}		// { dg-warning "attribute ignored" }
						// { dg-error "standard attributes in middle of decl-specifiers" "" { target *-*-* } .-1 }
constexpr [[deprecated]] int qux () { return 0; }	// { dg-warning "attribute ignored" }
						// { dg-error "standard attributes in middle of decl-specifiers" "" { target *-*-* } .-1 }
int [[deprecated]] d;				// { dg-warning "attribute ignored" }
int const [[deprecated]] e = 1;			// { dg-warning "attribute ignored" }
struct A {} [[deprecated]];			// { dg-warning "attribute ignored in declaration of 'struct A'" }
struct A [[deprecated]];			// { dg-warning "attribute ignored" }
struct A [[deprecated]] a1;			// { dg-warning "attribute ignored" }
A [[deprecated]] a2;				// { dg-warning "attribute ignored" }
enum B { B0 } [[deprecated]];			// { dg-warning "attribute ignored in declaration of 'enum B'" }
enum B [[deprecated]];				// { dg-warning "attribute ignored" }
enum B [[deprecated]] b1;			// { dg-warning "attribute ignored" }
B [[deprecated]] b2;				// { dg-warning "attribute ignored" }
struct [[deprecated]] C {};
int f [[deprecated]];
int g[2] [[deprecated]];			// { dg-error "'deprecated' on a type other than class or enumeration definition" }
int g2 [[deprecated]] [2];
int corge () [[deprecated]];			// { dg-error "'deprecated' on a type other than class or enumeration definition" }
int *[[deprecated]] h;				// { dg-error "'deprecated' on a type other than class or enumeration definition" }
int & [[deprecated]] i = f;			// { dg-error "'deprecated' on a type other than class or enumeration definition" }
						// { dg-warning "'f' is deprecated" "" { target *-*-* } .-1 }
int && [[deprecated]] j = 0;			// { dg-error "'deprecated' on a type other than class or enumeration definition" }
int S::* [[deprecated]] k;			// { dg-error "'deprecated' on a type other than class or enumeration definition" }
auto l = sizeof (int [2] [[deprecated]]);	// { dg-error "'deprecated' on a type other than class or enumeration definition" }
int freddy ([[deprecated]] int a,
	    [[deprecated]] int,
	    [[deprecated]] int c = 0,
	    [[deprecated]] int = 0);
void
corge ([[deprecated]] int a,
       [[deprecated]] int,
       [[deprecated]] int c = 0,
       [[deprecated]] int = 0)
{
}
[[deprecated]] void
garply ()
{
}
int grault (int [[deprecated]] a,		// { dg-warning "attribute ignored" }
	    int [[deprecated]],			// { dg-warning "attribute ignored" }
	    int [[deprecated]] c = 0,		// { dg-warning "attribute ignored" }
	    int [[deprecated]] = 0);		// { dg-warning "attribute ignored" }
void
waldo (int [[deprecated]] a,			// { dg-warning "attribute ignored" }
       int [[deprecated]],			// { dg-warning "attribute ignored" }
       int [[deprecated]] c = 0,		// { dg-warning "attribute ignored" }
       int [[deprecated]] = 0)			// { dg-warning "attribute ignored" }
{
}
int plugh (int a [[deprecated]],
	    int b [[deprecated]] = 0);
void
thud (int a [[deprecated]],
      int b [[deprecated]] = 0)
{
}
enum [[deprecated]] D { D0 };
enum class [[deprecated]] E { E0 };
enum F {};
enum [[deprecated]] F;				// { dg-warning "type attributes ignored after type is already defined" }
enum G {
  G0 [[deprecated]],
  G1 [[deprecated]] = 2
};
namespace [[deprecated]] H { using H0 = int; }
namespace [[deprecated]] {}			// { dg-warning "ignoring 'deprecated' attribute on anonymous namespace" }
[[deprecated]] using namespace H;		// { dg-warning "'deprecated' attribute directive ignored" }
						// { dg-warning "'H' is deprecated" "" { target *-*-* } .-1 }
struct [[deprecated]] I
{
  [[deprecated]];				// { dg-error "declaration does not declare anything" }
  [[deprecated]] int i;
  [[deprecated]] int foo ();
  [[deprecated]] int bar () { return 1; }
  [[deprecated]] int : 0;			// { dg-error "'deprecated' on unnamed bit-field" }
  [[deprecated]] int i2 : 5;
  [[deprecated]] static int i3;
  static int i4;
};
[[deprecated]] int I::i4 = 0;
struct J : [[deprecated]] C {};			// { dg-warning "attributes on base specifiers are ignored" }
#if __cpp_concepts >= 201907L
template <typename T>
concept K [[deprecated]] = requires { true; };
#endif
typedef int L [[deprecated]];
template <typename T>
struct M {};
template <>
struct [[deprecated]] M<int> { int m; };
typedef int N[2] [[deprecated]];		// { dg-error "'deprecated' on a type other than class or enumeration definition" }
typedef int O [[deprecated]] [2];
