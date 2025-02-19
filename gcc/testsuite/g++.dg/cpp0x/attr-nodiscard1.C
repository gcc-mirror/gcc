// C++ 26 P2552R3 - On the ignorability of standard attributes
// { dg-do compile { target c++11 } }

int arr[2];
struct S { int a, b; };
S arr2[2];

void
foo (int n)
{
  struct [[nodiscard]] S1 {};
  struct [[nodiscard ("foobar")]] S2 {};
  struct [[nodiscard (0)]] S3 {};		// { dg-error "'nodiscard' attribute argument must be a string constant" }
  struct [[nodiscard ("foo", "bar", "baz")]] S4 {};	// { dg-error "wrong number of arguments specified for 'nodiscard' attribute" }
  struct [[nodiscard (0, 1, 2)]] S5 {};		// { dg-error "wrong number of arguments specified for 'nodiscard' attribute" }

  auto a = [] [[nodiscard]] () {};
  auto b = [] constexpr [[nodiscard]] {};	// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
						// { dg-error "parameter declaration before lambda declaration specifiers only optional with" "" { target c++20_down } .-1 }
						// { dg-error "'constexpr' lambda only available with" "" { target c++14_down } .-2 }
  auto c = [] noexcept [[nodiscard]] {};	// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
						// { dg-error "parameter declaration before lambda exception specification only optional with" "" { target c++20_down } .-1 }
  auto d = [] () [[nodiscard]] {};		// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
  auto e = new int [n] [[nodiscard]];		// { dg-warning "attributes ignored on outermost array type in new expression" }
  auto e2 = new int [n] [[nodiscard]] [42];	// { dg-warning "attributes ignored on outermost array type in new expression" }
  auto f = new int [n][42] [[nodiscard]];	// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
  [[nodiscard]];				// { dg-warning "attributes at the beginning of statement are ignored" }
  [[nodiscard]] {}				// { dg-warning "attributes at the beginning of statement are ignored" }
  [[nodiscard]] if (true) {}			// { dg-warning "attributes at the beginning of statement are ignored" }
  [[nodiscard]] while (false) {}		// { dg-warning "attributes at the beginning of statement are ignored" }
  [[nodiscard]] goto lab;			// { dg-warning "attributes at the beginning of statement are ignored" }
  [[nodiscard]] lab:;				// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
  [[nodiscard]] try {} catch (int) {}		// { dg-warning "attributes at the beginning of statement are ignored" }
  if ([[nodiscard]] int x = 0) {}		// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
  switch (n)
    {
    [[nodiscard]] case 1:			// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
    [[nodiscard]] break;			// { dg-warning "attributes at the beginning of statement are ignored" }
    [[nodiscard]] default:			// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
	 break;
    }
  for ([[nodiscard]] auto a : arr) {}		// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
  for ([[nodiscard]] auto [a, b] : arr2) {}	// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
						// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
  [[nodiscard]] asm ("");			// { dg-warning "attributes ignored on 'asm' declaration" }
  try {} catch ([[nodiscard]] int x) {}		// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
  try {} catch ([[nodiscard]] int) {}		// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
  try {} catch (int [[nodiscard]] x) {}		// { dg-warning "attribute ignored" }
  try {} catch (int [[nodiscard]]) {}		// { dg-warning "attribute ignored" }
  try {} catch (int x [[nodiscard]]) {}		// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
}

[[nodiscard]] int bar ();
using foobar [[nodiscard]] = int;		// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
[[nodiscard]] int a;				// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
[[nodiscard]] auto [b, c] = arr;		// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
						// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
[[nodiscard]];					// { dg-warning "attribute ignored" }
inline [[nodiscard]] void baz () {}		// { dg-warning "attribute ignored" }
						// { dg-error "standard attributes in middle of decl-specifiers" "" { target *-*-* } .-1 }
constexpr [[nodiscard]] int qux () { return 0; }	// { dg-warning "attribute ignored" }
						// { dg-error "standard attributes in middle of decl-specifiers" "" { target *-*-* } .-1 }
int [[nodiscard]] d;				// { dg-warning "attribute ignored" }
int const [[nodiscard]] e = 1;			// { dg-warning "attribute ignored" }
struct A {} [[nodiscard]];			// { dg-warning "attribute ignored in declaration of 'struct A'" }
struct A [[nodiscard]];				// { dg-warning "attribute ignored" }
struct A [[nodiscard]] a1;			// { dg-warning "attribute ignored" }
A [[nodiscard]] a2;				// { dg-warning "attribute ignored" }
enum B { B0 } [[nodiscard]];			// { dg-warning "attribute ignored in declaration of 'enum B'" }
enum B [[nodiscard]];				// { dg-warning "attribute ignored" }
enum B [[nodiscard]] b1;			// { dg-warning "attribute ignored" }
B [[nodiscard]] b2;				// { dg-warning "attribute ignored" }
struct [[nodiscard]] C {};
int f [[nodiscard]];				// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
int g[2] [[nodiscard]];				// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
int g2 [[nodiscard]] [2];			// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
int corge () [[nodiscard]];			// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
int *[[nodiscard]] h;				// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
int & [[nodiscard]] i = f;			// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
int && [[nodiscard]] j = 0;			// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
int S::* [[nodiscard]] k;			// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
auto l = sizeof (int [2] [[nodiscard]]);	// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
int freddy ([[nodiscard]] int a,		// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
	    [[nodiscard]] int,			// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
	    [[nodiscard]] int c = 0,		// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
	    [[nodiscard]] int = 0);		// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
void
corge ([[nodiscard]] int a,			// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
       [[nodiscard]] int,			// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
       [[nodiscard]] int c = 0,			// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
       [[nodiscard]] int = 0)			// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
{
}
[[nodiscard]] void
garply ()					// { dg-warning "'nodiscard' attribute applied to 'void garply\\\(\\\)' with void return type" }
{
}
[[nodiscard]] int
xyzzyy ()
{
  return 0;
}
int grault (int [[nodiscard]] a,		// { dg-warning "attribute ignored" }
	    int [[nodiscard]],			// { dg-warning "attribute ignored" }
	    int [[nodiscard]] c = 0,		// { dg-warning "attribute ignored" }
	    int [[nodiscard]] = 0);		// { dg-warning "attribute ignored" }
void
waldo (int [[nodiscard]] a,			// { dg-warning "attribute ignored" }
       int [[nodiscard]],			// { dg-warning "attribute ignored" }
       int [[nodiscard]] c = 0,			// { dg-warning "attribute ignored" }
       int [[nodiscard]] = 0)			// { dg-warning "attribute ignored" }
{
}
int plugh (int a [[nodiscard]],			// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
	    int b [[nodiscard]] = 0);		// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
void
thud (int a [[nodiscard]],			// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
      int b [[nodiscard]] = 0)			// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
{
}
enum [[nodiscard]] D { D0 };
enum class [[nodiscard]] E { E0 };
enum F {};
enum [[nodiscard]] F;				// { dg-warning "type attributes ignored after type is already defined" }
enum G {
  G0 [[nodiscard]],				// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
  G1 [[nodiscard]] = 2				// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
};
namespace [[nodiscard]] H { using H0 = int; }	// { dg-warning "'nodiscard' attribute directive ignored" }
namespace [[nodiscard]] {}			// { dg-warning "'nodiscard' attribute directive ignored" }
[[nodiscard]] using namespace H;		// { dg-warning "'nodiscard' attribute directive ignored" }
struct [[nodiscard]] I
{
  [[nodiscard]];				// { dg-error "declaration does not declare anything" }
  [[nodiscard]] int i;				// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
  [[nodiscard]] int foo ();
  [[nodiscard]] int bar () { return 1; }
  [[nodiscard]] int : 0;			// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
  [[nodiscard]] int i2 : 5;			// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
  [[nodiscard]] static int i3;			// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
  static int i4;
};
[[nodiscard]] int I::i4 = 0;			// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
struct J : [[nodiscard]] C {};			// { dg-warning "attributes on base specifiers are ignored" }
#if __cpp_concepts >= 201907L
template <typename T>
concept K [[nodiscard]] = requires { true; };	// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" "" { target c++20 } }
#endif
typedef int L [[nodiscard]];			// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
template <typename T>
struct M {};
template <>
struct [[nodiscard]] M<int> { int m; };
typedef int N[2] [[nodiscard]];			// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
typedef int O [[nodiscard]] [2];		// { dg-warning "'nodiscard' attribute can only be applied to functions or to class or enumeration types" }
