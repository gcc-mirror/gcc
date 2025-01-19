// C++ 26 P2552R3 - On the ignorability of standard attributes
// { dg-do compile { target c++11 } }

int arr[2];
struct S { int a, b; };
S arr2[2];

void
foo (int n)
{
  [[unlikely]];
  [[unlikely (1)]];				// { dg-error "'unlikely' attribute does not take any arguments" }
  [[unlikely]] ++n;
  [[unlikely]] int x1;				// { dg-warning "'unlikely' attribute ignored" }

  auto a = [] [[unlikely]] () {};		// { dg-warning "ISO C\\\+\\\+ 'unlikely' attribute does not apply to functions; treating as '\\\[\\\[gnu::cold\\\]\\\]'" }
  auto b = [] constexpr [[unlikely]] {};	// { dg-warning "'unlikely' attribute ignored" }
						// { dg-error "parameter declaration before lambda declaration specifiers only optional with" "" { target c++20_down } .-1 }
						// { dg-error "'constexpr' lambda only available with" "" { target c++14_down } .-2 }
  auto c = [] noexcept [[unlikely]] {};		// { dg-warning "'unlikely' attribute ignored" }
						// { dg-error "parameter declaration before lambda exception specification only optional with" "" { target c++20_down } .-1 }
  auto d = [] () [[unlikely]] {};		// { dg-warning "'unlikely' attribute ignored" }
  auto e = new int [n] [[unlikely]];		// { dg-warning "attributes ignored on outermost array type in new expression" }
  auto e2 = new int [n] [[unlikely]] [42];	// { dg-warning "attributes ignored on outermost array type in new expression" }
  auto f = new int [n][42] [[unlikely]];	// { dg-warning "'unlikely' attribute ignored" }
  [[unlikely]];
  [[unlikely]] {}
  [[unlikely]] if (true) {}
  [[unlikely]] while (false) {}
  [[unlikely]] goto lab;
  [[unlikely]] lab:;
  [[unlikely]] try {} catch (int) {}
  if ([[unlikely]] int x = 0) {}		// { dg-warning "'unlikely' attribute ignored" }
  switch (n)
    {
    [[unlikely]] case 1:
    [[unlikely]] break;
    [[unlikely]] default:
	 break;
    }
  for ([[unlikely]] auto a : arr) {}		// { dg-warning "'unlikely' attribute ignored" }
  for ([[unlikely]] auto [a, b] : arr2) {}	// { dg-warning "'unlikely' attribute ignored" }
						// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
  [[unlikely]] asm ("");			// { dg-warning "attributes ignored on 'asm' declaration" }
  try {} catch ([[unlikely]] int x) {}		// { dg-warning "'unlikely' attribute ignored" }
  try {} catch ([[unlikely]] int) {}		// { dg-warning "'unlikely' attribute ignored" }
  try {} catch (int [[unlikely]] x) {}		// { dg-warning "attribute ignored" }
  try {} catch (int [[unlikely]]) {}		// { dg-warning "attribute ignored" }
  try {} catch (int x [[unlikely]]) {}		// { dg-warning "'unlikely' attribute ignored" }
}

[[unlikely]] int bar ();			// { dg-warning "ISO C\\\+\\\+ 'unlikely' attribute does not apply to functions; treating as '\\\[\\\[gnu::cold\\\]\\\]'" }
using foobar [[unlikely]] = int;		// { dg-warning "'unlikely' attribute ignored" }
[[unlikely]] int a;				// { dg-warning "'unlikely' attribute ignored" }
[[unlikely]] auto [b, c] = arr;			// { dg-warning "'unlikely' attribute ignored" }
						// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
[[unlikely]];					// { dg-warning "attribute ignored" }
inline [[unlikely]] void baz () {}		// { dg-warning "attribute ignored" }
						// { dg-error "standard attributes in middle of decl-specifiers" "" { target *-*-* } .-1 }
constexpr [[unlikely]] int qux () { return 0; }	// { dg-warning "attribute ignored" }
						// { dg-error "standard attributes in middle of decl-specifiers" "" { target *-*-* } .-1 }
int [[unlikely]] d;				// { dg-warning "attribute ignored" }
int const [[unlikely]] e = 1;			// { dg-warning "attribute ignored" }
struct A {} [[unlikely]];			// { dg-warning "attribute ignored in declaration of 'struct A'" }
struct A [[unlikely]];				// { dg-warning "attribute ignored" }
struct A [[unlikely]] a1;			// { dg-warning "attribute ignored" }
A [[unlikely]] a2;				// { dg-warning "attribute ignored" }
enum B { B0 } [[unlikely]];			// { dg-warning "attribute ignored in declaration of 'enum B'" }
enum B [[unlikely]];				// { dg-warning "attribute ignored" }
enum B [[unlikely]] b1;				// { dg-warning "attribute ignored" }
B [[unlikely]] b2;				// { dg-warning "attribute ignored" }
struct [[unlikely]] C {};			// { dg-warning "'unlikely' attribute ignored" }
int f [[unlikely]];				// { dg-warning "'unlikely' attribute ignored" }
int g[2] [[unlikely]];				// { dg-warning "'unlikely' attribute ignored" }
int g2 [[unlikely]] [2];			// { dg-warning "'unlikely' attribute ignored" }
int corge () [[unlikely]];			// { dg-warning "'unlikely' attribute ignored" }
int *[[unlikely]] h;				// { dg-warning "'unlikely' attribute ignored" }
int & [[unlikely]] i = f;			// { dg-warning "'unlikely' attribute ignored" }
int && [[unlikely]] j = 0;			// { dg-warning "'unlikely' attribute ignored" }
int S::* [[unlikely]] k;			// { dg-warning "'unlikely' attribute ignored" }
auto l = sizeof (int [2] [[unlikely]]);		// { dg-warning "'unlikely' attribute ignored" }
int freddy ([[unlikely]] int a,			// { dg-warning "'unlikely' attribute ignored" }
	    [[unlikely]] int,			// { dg-warning "'unlikely' attribute ignored" }
	    [[unlikely]] int c = 0,		// { dg-warning "'unlikely' attribute ignored" }
	    [[unlikely]] int = 0);		// { dg-warning "'unlikely' attribute ignored" }
void
corge ([[unlikely]] int a,			// { dg-warning "'unlikely' attribute ignored" }
       [[unlikely]] int,			// { dg-warning "'unlikely' attribute ignored" }
       [[unlikely]] int c = 0,			// { dg-warning "'unlikely' attribute ignored" }
       [[unlikely]] int = 0)			// { dg-warning "'unlikely' attribute ignored" }
{
}
[[unlikely]] void
garply ()					// { dg-warning "ISO C\\\+\\\+ 'unlikely' attribute does not apply to functions; treating as '\\\[\\\[gnu::cold\\\]\\\]'" }
{
}
int grault (int [[unlikely]] a,			// { dg-warning "attribute ignored" }
	    int [[unlikely]],			// { dg-warning "attribute ignored" }
	    int [[unlikely]] c = 0,		// { dg-warning "attribute ignored" }
	    int [[unlikely]] = 0);		// { dg-warning "attribute ignored" }
void
waldo (int [[unlikely]] a,			// { dg-warning "attribute ignored" }
       int [[unlikely]],			// { dg-warning "attribute ignored" }
       int [[unlikely]] c = 0,			// { dg-warning "attribute ignored" }
       int [[unlikely]] = 0)			// { dg-warning "attribute ignored" }
{
}
int plugh (int a [[unlikely]],			// { dg-warning "'unlikely' attribute ignored" }
	    int b [[unlikely]] = 0);		// { dg-warning "'unlikely' attribute ignored" }
void
thud (int a [[unlikely]],			// { dg-warning "'unlikely' attribute ignored" }
      int b [[unlikely]] = 0)			// { dg-warning "'unlikely' attribute ignored" }
{
}
enum [[unlikely]] D { D0 };			// { dg-warning "'unlikely' attribute ignored" }
enum class [[unlikely]] E { E0 };		// { dg-warning "'unlikely' attribute ignored" }
enum F {};
enum [[unlikely]] F;				// { dg-warning "type attributes ignored after type is already defined" }
enum G {
  G0 [[unlikely]],				// { dg-warning "'unlikely' attribute ignored" }
  G1 [[unlikely]] = 2				// { dg-warning "'unlikely' attribute ignored" }
};
namespace [[unlikely]] H { using H0 = int; }	// { dg-warning "'unlikely' attribute directive ignored" } */
namespace [[unlikely]] {}			// { dg-warning "'unlikely' attribute directive ignored" }
[[unlikely]] using namespace H;			// { dg-warning "'unlikely' attribute directive ignored" }
struct [[unlikely]] I				// { dg-warning "'unlikely' attribute ignored" }
{
  [[unlikely]];					// { dg-error "declaration does not declare anything" }
  [[unlikely]] int i;				// { dg-warning "'unlikely' attribute ignored" }
  [[unlikely]] int foo ();			// { dg-warning "ISO C\\\+\\\+ 'unlikely' attribute does not apply to functions; treating as '\\\[\\\[gnu::cold\\\]\\\]'" }
  [[unlikely]] int bar () { return 1; }		// { dg-warning "ISO C\\\+\\\+ 'unlikely' attribute does not apply to functions; treating as '\\\[\\\[gnu::cold\\\]\\\]'" }
  [[unlikely]] int : 0;				// { dg-warning "'unlikely' attribute ignored" }
  [[unlikely]] int i2 : 5;			// { dg-warning "'unlikely' attribute ignored" }
  [[unlikely]] static int i3;			// { dg-warning "'unlikely' attribute ignored" }
  static int i4;
};
[[unlikely]] int I::i4 = 0;			// { dg-warning "'unlikely' attribute ignored" }
struct J : [[unlikely]] C {};			// { dg-warning "attributes on base specifiers are ignored" }
#if __cpp_concepts >= 201907L
template <typename T>
concept K [[unlikely]] = requires { true; };	// { dg-warning "'unlikely' attribute ignored" "" { target c++20 } }
#endif
typedef int L [[unlikely]];			// { dg-warning "'unlikely' attribute ignored" }
template <typename T>
struct M {};
template <>
struct [[unlikely]] M<int> { int m; };		// { dg-warning "'unlikely' attribute ignored" }
typedef int N[2] [[unlikely]];			// { dg-warning "'unlikely' attribute ignored" }
typedef int O [[unlikely]] [2];			// { dg-warning "'unlikely' attribute ignored" }
