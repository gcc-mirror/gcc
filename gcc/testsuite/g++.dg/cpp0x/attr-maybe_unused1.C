// C++ 26 P2552R3 - On the ignorability of standard attributes
// { dg-do compile { target c++11 } }

int arr[2];
struct S { int a, b; };
S arr2[2];

void
foo (int n)
{
  [[maybe_unused]] int x1;
  [[maybe_unused ("foobar")]] int x2;		// { dg-error "'maybe_unused' attribute does not take any arguments" }
						// { dg-error "expected primary-expression before 'int'" "" { target *-*-* } .-1 }
  [[maybe_unused (0)]] int x3;			// { dg-error "'maybe_unused' attribute does not take any arguments" }
						// { dg-error "expected primary-expression before 'int'" "" { target *-*-* } .-1 }

  auto a = [] [[maybe_unused]] () {};
  auto b = [] constexpr [[maybe_unused]] {};	// { dg-error "'maybe_unused' on a type other than class or enumeration definition" }
						// { dg-error "parameter declaration before lambda declaration specifiers only optional with" "" { target c++20_down } .-1 }
						// { dg-error "'constexpr' lambda only available with" "" { target c++14_down } .-2 }
  auto c = [] noexcept [[maybe_unused]] {};	// { dg-error "'maybe_unused' on a type other than class or enumeration definition" }
						// { dg-error "parameter declaration before lambda exception specification only optional with" "" { target c++20_down } .-1 }
  auto d = [] () [[maybe_unused]] {};		// { dg-error "'maybe_unused' on a type other than class or enumeration definition" }
  auto e = new int [n] [[maybe_unused]];	// { dg-warning "attributes ignored on outermost array type in new expression" }
  auto e2 = new int [n] [[maybe_unused]] [42];	// { dg-warning "attributes ignored on outermost array type in new expression" }
  auto f = new int [n][42] [[maybe_unused]];	// { dg-error "'maybe_unused' on a type other than class or enumeration definition" }
  [[maybe_unused]];				// { dg-warning "attributes at the beginning of statement are ignored" }
  [[maybe_unused]] {}				// { dg-warning "attributes at the beginning of statement are ignored" }
  [[maybe_unused]] if (true) {}			// { dg-warning "attributes at the beginning of statement are ignored" }
  [[maybe_unused]] while (false) {}		// { dg-warning "attributes at the beginning of statement are ignored" }
  [[maybe_unused]] goto lab;			// { dg-warning "attributes at the beginning of statement are ignored" }
  [[maybe_unused]] lab:;
  [[maybe_unused]] try {} catch (int) {}	// { dg-warning "attributes at the beginning of statement are ignored" }
  if ([[maybe_unused]] int x = 0) {}
  switch (n)
    {
    [[maybe_unused]] case 1:			// { dg-error "'maybe_unused' on 'case' or 'default' label" }
    [[maybe_unused]] break;			// { dg-warning "attributes at the beginning of statement are ignored" }
    [[maybe_unused]] default:			// { dg-error "'maybe_unused' on 'case' or 'default' label" }
	 break;
    }
  for ([[maybe_unused]] auto a : arr) {}
  for ([[maybe_unused]] auto [a, b] : arr2) {}	// { dg-error "structured bindings only available with" "" { target c++14_down } }
  [[maybe_unused]] asm ("");			// { dg-warning "attributes ignored on 'asm' declaration" }
  try {} catch ([[maybe_unused]] int x) {}
  try {} catch ([[maybe_unused]] int) {}
  try {} catch (int [[maybe_unused]] x) {}	// { dg-warning "attribute ignored" }
  try {} catch (int [[maybe_unused]]) {}	// { dg-warning "attribute ignored" }
  try {} catch (int x [[maybe_unused]]) {}
}

[[maybe_unused]] int bar ();
using foobar [[maybe_unused]] = int;
[[maybe_unused]] int a;
[[maybe_unused]] auto [b, c] = arr;		// { dg-error "structured bindings only available with" "" { target c++14_down } }
[[maybe_unused]];				// { dg-warning "attribute ignored" }
inline [[maybe_unused]] void baz () {}		// { dg-warning "attribute ignored" }
						// { dg-error "standard attributes in middle of decl-specifiers" "" { target *-*-* } .-1 }
constexpr [[maybe_unused]] int qux () { return 0; }	// { dg-warning "attribute ignored" }
						// { dg-error "standard attributes in middle of decl-specifiers" "" { target *-*-* } .-1 }
int [[maybe_unused]] d;				// { dg-warning "attribute ignored" }
int const [[maybe_unused]] e = 1;		// { dg-warning "attribute ignored" }
struct A {} [[maybe_unused]];			// { dg-warning "attribute ignored in declaration of 'struct A'" }
struct A [[maybe_unused]];			// { dg-warning "attribute ignored" }
struct A [[maybe_unused]] a1;			// { dg-warning "attribute ignored" }
A [[maybe_unused]] a2;				// { dg-warning "attribute ignored" }
enum B { B0 } [[maybe_unused]];			// { dg-warning "attribute ignored in declaration of 'enum B'" }
enum B [[maybe_unused]];			// { dg-warning "attribute ignored" }
enum B [[maybe_unused]] b1;			// { dg-warning "attribute ignored" }
B [[maybe_unused]] b2;				// { dg-warning "attribute ignored" }
struct [[maybe_unused]] C {};
int f [[maybe_unused]];
int g[2] [[maybe_unused]];			// { dg-error "'maybe_unused' on a type other than class or enumeration definition" }
int g2 [[maybe_unused]] [2];
int corge () [[maybe_unused]];			// { dg-error "'maybe_unused' on a type other than class or enumeration definition" }
int *[[maybe_unused]] h;			// { dg-error "'maybe_unused' on a type other than class or enumeration definition" }
int & [[maybe_unused]] i = f;			// { dg-error "'maybe_unused' on a type other than class or enumeration definition" }
int && [[maybe_unused]] j = 0;			// { dg-error "'maybe_unused' on a type other than class or enumeration definition" }
int S::* [[maybe_unused]] k;			// { dg-error "'maybe_unused' on a type other than class or enumeration definition" }
auto l = sizeof (int [2] [[maybe_unused]]);	// { dg-error "'maybe_unused' on a type other than class or enumeration definition" }
int freddy ([[maybe_unused]] int a,
	    [[maybe_unused]] int,
	    [[maybe_unused]] int c = 0,
	    [[maybe_unused]] int = 0);
void
corge ([[maybe_unused]] int a,
       [[maybe_unused]] int,
       [[maybe_unused]] int c = 0,
       [[maybe_unused]] int = 0)
{
}
[[maybe_unused]] void
garply ()
{
}
int grault (int [[maybe_unused]] a,		// { dg-warning "attribute ignored" }
	    int [[maybe_unused]],		// { dg-warning "attribute ignored" }
	    int [[maybe_unused]] c = 0,		// { dg-warning "attribute ignored" }
	    int [[maybe_unused]] = 0);		// { dg-warning "attribute ignored" }
void
waldo (int [[maybe_unused]] a,			// { dg-warning "attribute ignored" }
       int [[maybe_unused]],			// { dg-warning "attribute ignored" }
       int [[maybe_unused]] c = 0,		// { dg-warning "attribute ignored" }
       int [[maybe_unused]] = 0)		// { dg-warning "attribute ignored" }
{
}
int plugh (int a [[maybe_unused]],
	    int b [[maybe_unused]] = 0);
void
thud (int a [[maybe_unused]],
      int b [[maybe_unused]] = 0)
{
}
enum [[maybe_unused]] D { D0 };
enum class [[maybe_unused]] E { E0 };
enum F {};
enum [[maybe_unused]] F;			// { dg-warning "type attributes ignored after type is already defined" }
enum G {
  G0 [[maybe_unused]],
  G1 [[maybe_unused]] = 2
};
namespace [[maybe_unused]] H { using H0 = int; }// { dg-warning "'maybe_unused' attribute directive ignored" }
namespace [[maybe_unused]] {}			// { dg-warning "'maybe_unused' attribute directive ignored" }
[[maybe_unused]] using namespace H;		// { dg-warning "'maybe_unused' attribute directive ignored" }
struct [[maybe_unused]] I
{
  [[maybe_unused]];				// { dg-error "declaration does not declare anything" }
  [[maybe_unused]] int i;
  [[maybe_unused]] int foo ();
  [[maybe_unused]] int bar () { return 1; }
  [[maybe_unused]] int : 0;			// { dg-error "'maybe_unused' on unnamed bit-field" }
  [[maybe_unused]] int i2 : 5;
  [[maybe_unused]] static int i3;
  static int i4;
};
[[maybe_unused]] int I::i4 = 0;
struct J : [[maybe_unused]] C {};		// { dg-warning "attributes on base specifiers are ignored" }
#if __cpp_concepts >= 201907L
template <typename T>
concept K [[maybe_unused]] = requires { true; };// { dg-warning "'maybe_unused' attribute ignored" "" { target c++20 } }
#endif
typedef int L [[maybe_unused]];
template <typename T>
struct M {};
template <>
struct [[maybe_unused]] M<int> { int m; };
typedef int N[2] [[maybe_unused]];		// { dg-error "'maybe_unused' on a type other than class or enumeration definition" }
typedef int O [[maybe_unused]] [2];
