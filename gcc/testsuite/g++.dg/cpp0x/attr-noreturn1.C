// C++ 26 P2552R3 - On the ignorability of standard attributes
// { dg-do compile { target c++11 } }

int arr[2];
struct S { int a, b; };
S arr2[2];

[[noreturn]] void foo1 ();
[[noreturn ("foobar")]] void foo2 ();		// { dg-error "'noreturn' attribute does not take any arguments" }
[[noreturn (0)]] void foo3 ();			// { dg-error "'noreturn' attribute does not take any arguments" }

void
foo (int n)
{
  auto a = [] [[noreturn]] () { do { } while (true); };
  auto b = [] constexpr [[noreturn]] {};	// { dg-warning "'noreturn' attribute does not apply to types" }
						// { dg-error "parameter declaration before lambda declaration specifiers only optional with" "" { target c++20_down } .-1 }
						// { dg-error "'constexpr' lambda only available with" "" { target c++14_down } .-2 }
  auto c = [] noexcept [[noreturn]] {};		// { dg-warning "'noreturn' attribute does not apply to types" }
						// { dg-error "parameter declaration before lambda exception specification only optional with" "" { target c++20_down } .-1 }
  auto d = [] () [[noreturn]] {};		// { dg-warning "'noreturn' attribute does not apply to types" }
  auto e = new int [n] [[noreturn]];		// { dg-warning "attributes ignored on outermost array type in new expression" }
  auto e2 = new int [n] [[noreturn]] [42];	// { dg-warning "attributes ignored on outermost array type in new expression" }
  auto f = new int [n][42] [[noreturn]];	// { dg-warning "'noreturn' attribute does not apply to types" }
  [[noreturn]];					// { dg-warning "attributes at the beginning of statement are ignored" }
  [[noreturn]] {}				// { dg-warning "attributes at the beginning of statement are ignored" }
  [[noreturn]] if (true) {}			// { dg-warning "attributes at the beginning of statement are ignored" }
  [[noreturn]] while (false) {}			// { dg-warning "attributes at the beginning of statement are ignored" }
  [[noreturn]] goto lab;			// { dg-warning "attributes at the beginning of statement are ignored" }
  [[noreturn]] lab:;				// { dg-warning "'noreturn' attribute ignored" }
  [[noreturn]] try {} catch (int) {}		// { dg-warning "attributes at the beginning of statement are ignored" }
  if ([[noreturn]] int x = 0) {}		// { dg-warning "'noreturn' attribute ignored" }
  switch (n)
    {
    [[noreturn]] case 1:			// { dg-warning "'noreturn' attribute ignored" }
    [[noreturn]] break;				// { dg-warning "attributes at the beginning of statement are ignored" }
    [[noreturn]] default:			// { dg-warning "'noreturn' attribute ignored" }
	 break;
    }
  for ([[noreturn]] auto a : arr) {}		// { dg-warning "'noreturn' attribute ignored" }
  for ([[noreturn]] auto [a, b] : arr2) {}	// { dg-warning "'noreturn' attribute ignored" }
						// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
  [[noreturn]] asm ("");			// { dg-warning "attributes ignored on 'asm' declaration" }
  try {} catch ([[noreturn]] int x) {}		// { dg-warning "'noreturn' attribute ignored" }
  try {} catch ([[noreturn]] int) {}		// { dg-warning "'noreturn' attribute ignored" }
  try {} catch (int [[noreturn]] x) {}		// { dg-warning "attribute ignored" }
  try {} catch (int [[noreturn]]) {}		// { dg-warning "attribute ignored" }
  try {} catch (int x [[noreturn]]) {}		// { dg-warning "'noreturn' attribute ignored" }
}

[[noreturn]] int bar ();
using foobar [[noreturn]] = int;		// { dg-warning "'noreturn' attribute ignored" }
[[noreturn]] int a;				// { dg-warning "'noreturn' attribute ignored" }
[[noreturn]] auto [b, c] = arr;			// { dg-warning "'noreturn' attribute ignored" }
						// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
[[noreturn]];					// { dg-warning "attribute ignored" }
inline [[noreturn]] void baz () {}		// { dg-warning "attribute ignored" }
						// { dg-error "standard attributes in middle of decl-specifiers" "" { target *-*-* } .-1 }
constexpr [[noreturn]] int qux () { return 0; }	// { dg-warning "attribute ignored" }
						// { dg-error "standard attributes in middle of decl-specifiers" "" { target *-*-* } .-1 }
int [[noreturn]] d;				// { dg-warning "attribute ignored" }
int const [[noreturn]] e = 1;			// { dg-warning "attribute ignored" }
struct A {} [[noreturn]];			// { dg-warning "attribute ignored in declaration of 'struct A'" }
struct A [[noreturn]];				// { dg-warning "attribute ignored" }
struct A [[noreturn]] a1;			// { dg-warning "attribute ignored" }
A [[noreturn]] a2;				// { dg-warning "attribute ignored" }
enum B { B0 } [[noreturn]];			// { dg-warning "attribute ignored in declaration of 'enum B'" }
enum B [[noreturn]];				// { dg-warning "attribute ignored" }
enum B [[noreturn]] b1;				// { dg-warning "attribute ignored" }
B [[noreturn]] b2;				// { dg-warning "attribute ignored" }
struct [[noreturn]] C {};			// { dg-warning "'noreturn' attribute does not apply to types" }
int f [[noreturn]];				// { dg-warning "'noreturn' attribute ignored" }
int g[2] [[noreturn]];				// { dg-warning "'noreturn' attribute does not apply to types" }
int g2 [[noreturn]] [2];			// { dg-warning "'noreturn' attribute ignored" }
int corge () [[noreturn]];			// { dg-warning "'noreturn' attribute does not apply to types" }
int *[[noreturn]] h;				// { dg-warning "'noreturn' attribute does not apply to types" }
int & [[noreturn]] i = f;			// { dg-warning "'noreturn' attribute does not apply to types" }
int && [[noreturn]] j = 0;			// { dg-warning "'noreturn' attribute does not apply to types" }
int S::* [[noreturn]] k;			// { dg-warning "'noreturn' attribute does not apply to types" }
auto l = sizeof (int [2] [[noreturn]]);		// { dg-warning "'noreturn' attribute does not apply to types" }
int freddy ([[noreturn]] int a,			// { dg-warning "'noreturn' attribute ignored" }
	    [[noreturn]] int,			// { dg-warning "'noreturn' attribute ignored" }
	    [[noreturn]] int c = 0,		// { dg-warning "'noreturn' attribute ignored" }
	    [[noreturn]] int = 0);		// { dg-warning "'noreturn' attribute ignored" }
void
corge ([[noreturn]] int a,			// { dg-warning "'noreturn' attribute ignored" }
       [[noreturn]] int,			// { dg-warning "'noreturn' attribute ignored" }
       [[noreturn]] int c = 0,			// { dg-warning "'noreturn' attribute ignored" }
       [[noreturn]] int = 0)			// { dg-warning "'noreturn' attribute ignored" }
{
}
[[noreturn]] void
garply ()
{
  for (;;)
    ;
}
[[noreturn]] int
xyzzyy ()
{
  return 0;					// { dg-warning "function declared 'noreturn' has a 'return' statement" }
}						// { dg-warning "'noreturn' function does return" "" { target *-*-* } .-1 }
[[noreturn]] void
xyzzyy2 ()
{
}						// { dg-warning "'noreturn' function does return" }
int grault (int [[noreturn]] a,			// { dg-warning "attribute ignored" }
	    int [[noreturn]],			// { dg-warning "attribute ignored" }
	    int [[noreturn]] c = 0,		// { dg-warning "attribute ignored" }
	    int [[noreturn]] = 0);		// { dg-warning "attribute ignored" }
void
waldo (int [[noreturn]] a,			// { dg-warning "attribute ignored" }
       int [[noreturn]],			// { dg-warning "attribute ignored" }
       int [[noreturn]] c = 0,			// { dg-warning "attribute ignored" }
       int [[noreturn]] = 0)			// { dg-warning "attribute ignored" }
{
}
int plugh (int a [[noreturn]],			// { dg-warning "'noreturn' attribute ignored" }
	    int b [[noreturn]] = 0);		// { dg-warning "'noreturn' attribute ignored" }
void
thud (int a [[noreturn]],			// { dg-warning "'noreturn' attribute ignored" }
      int b [[noreturn]] = 0)			// { dg-warning "'noreturn' attribute ignored" }
{
}
enum [[noreturn]] D { D0 };			// { dg-warning "'noreturn' attribute does not apply to types" }
enum class [[noreturn]] E { E0 };		// { dg-warning "'noreturn' attribute does not apply to types" }
enum F {};
enum [[noreturn]] F;				// { dg-warning "'noreturn' attribute does not apply to types" }
enum G {
  G0 [[noreturn]],				// { dg-warning "'noreturn' attribute ignored" }
  G1 [[noreturn]] = 2				// { dg-warning "'noreturn' attribute ignored" }
};
namespace [[noreturn]] H { using H0 = int; }	// { dg-warning "'noreturn' attribute directive ignored" }
namespace [[noreturn]] {}			// { dg-warning "'noreturn' attribute directive ignored" }
[[noreturn]] using namespace H;			// { dg-warning "'noreturn' attribute directive ignored" }
struct [[noreturn]] I				// { dg-warning "'noreturn' attribute does not apply to types" }
{
  [[noreturn]];					// { dg-error "declaration does not declare anything" }
  [[noreturn]] int i;				// { dg-warning "'noreturn' attribute ignored" }
  [[noreturn]] int foo ();
  [[noreturn]] int bar () { return 1; }		// { dg-warning "function declared 'noreturn' has a 'return' statement" }
  [[noreturn]] int baz () { for (;;) ; }
  [[noreturn]] int : 0;				// { dg-warning "'noreturn' attribute ignored" }
  [[noreturn]] int i2 : 5;			// { dg-warning "'noreturn' attribute ignored" }
  [[noreturn]] static int i3;			// { dg-warning "'noreturn' attribute ignored" }
  static int i4;
};
[[noreturn]] int I::i4 = 0;			// { dg-warning "'noreturn' attribute ignored" }
struct J : [[noreturn]] C {};			// { dg-warning "attributes on base specifiers are ignored" }
#if __cpp_concepts >= 201907L
template <typename T>
concept K [[noreturn]] = requires { true; };	// { dg-warning "'noreturn' attribute ignored" "" { target c++20 } }
#endif
typedef int L [[noreturn]];			// { dg-warning "'noreturn' attribute ignored" }
template <typename T>
struct M {};
template <>
struct [[noreturn]] M<int> { int m; };		// { dg-warning "'noreturn' attribute does not apply to types" }
typedef int N[2] [[noreturn]];			// { dg-warning "'noreturn' attribute does not apply to types" }
typedef int O [[noreturn]] [2];			// { dg-warning "'noreturn' attribute ignored" }
