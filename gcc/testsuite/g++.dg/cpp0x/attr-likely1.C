// C++ 26 P2552R3 - On the ignorability of standard attributes
// { dg-do compile { target c++11 } }

int arr[2];
struct S { int a, b; };
S arr2[2];

void
foo (int n)
{
  [[likely]];
  [[likely (1)]];			// { dg-error "'likely' attribute does not take any arguments" }
  [[likely]] ++n;
  [[likely]] int x1;			// { dg-warning "'likely' attribute ignored" }

  auto a = [] [[likely]] () {};		// { dg-warning "ISO C\\\+\\\+ 'likely' attribute does not apply to functions; treating as '\\\[\\\[gnu::hot\\\]\\\]'" }
  auto b = [] constexpr [[likely]] {};	// { dg-warning "'likely' attribute ignored" }
					// { dg-error "parameter declaration before lambda declaration specifiers only optional with" "" { target c++20_down } .-1 }
					// { dg-error "'constexpr' lambda only available with" "" { target c++14_down } .-2 }
  auto c = [] noexcept [[likely]] {};	// { dg-warning "'likely' attribute ignored" }
					// { dg-error "parameter declaration before lambda exception specification only optional with" "" { target c++20_down } .-1 }
  auto d = [] () [[likely]] {};		// { dg-warning "'likely' attribute ignored" }
  auto e = new int [n] [[likely]];	// { dg-warning "attributes ignored on outermost array type in new expression" }
  auto e2 = new int [n] [[likely]] [42];// { dg-warning "attributes ignored on outermost array type in new expression" }
  auto f = new int [n][42] [[likely]];	// { dg-warning "'likely' attribute ignored" }
  [[likely]];
  [[likely]] {}
  [[likely]] if (true) {}
  [[likely]] while (false) {}
  [[likely]] goto lab;
  [[likely]] lab:;
  [[likely]] try {} catch (int) {}
  if ([[likely]] int x = 0) {}		// { dg-warning "'likely' attribute ignored" }
  switch (n)
    {
    [[likely]] case 1:
    [[likely]] break;
    [[likely]] default:
	 break;
    }
  for ([[likely]] auto a : arr) {}	// { dg-warning "'likely' attribute ignored" }
  for ([[likely]] auto [a, b] : arr2) {}// { dg-warning "'likely' attribute ignored" }
					// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
  [[likely]] asm ("");			// { dg-warning "attributes ignored on 'asm' declaration" }
  try {} catch ([[likely]] int x) {}	// { dg-warning "'likely' attribute ignored" }
  try {} catch ([[likely]] int) {}	// { dg-warning "'likely' attribute ignored" }
  try {} catch (int [[likely]] x) {}	// { dg-warning "attribute ignored" }
  try {} catch (int [[likely]]) {}	// { dg-warning "attribute ignored" }
  try {} catch (int x [[likely]]) {}	// { dg-warning "'likely' attribute ignored" }
}

[[likely]] int bar ();			// { dg-warning "ISO C\\\+\\\+ 'likely' attribute does not apply to functions; treating as '\\\[\\\[gnu::hot\\\]\\\]'" }
using foobar [[likely]] = int;		// { dg-warning "'likely' attribute ignored" }
[[likely]] int a;			// { dg-warning "'likely' attribute ignored" }
[[likely]] auto [b, c] = arr;		// { dg-warning "'likely' attribute ignored" }
					// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
[[likely]];				// { dg-warning "attribute ignored" }
inline [[likely]] void baz () {}	// { dg-warning "attribute ignored" }
					// { dg-error "standard attributes in middle of decl-specifiers" "" { target *-*-* } .-1 }
constexpr [[likely]] int qux () { return 0; }	// { dg-warning "attribute ignored" }
					// { dg-error "standard attributes in middle of decl-specifiers" "" { target *-*-* } .-1 }
int [[likely]] d;			// { dg-warning "attribute ignored" }
int const [[likely]] e = 1;		// { dg-warning "attribute ignored" }
struct A {} [[likely]];			// { dg-warning "attribute ignored in declaration of 'struct A'" }
struct A [[likely]];			// { dg-warning "attribute ignored" }
struct A [[likely]] a1;			// { dg-warning "attribute ignored" }
A [[likely]] a2;			// { dg-warning "attribute ignored" }
enum B { B0 } [[likely]];		// { dg-warning "attribute ignored in declaration of 'enum B'" }
enum B [[likely]];			// { dg-warning "attribute ignored" }
enum B [[likely]] b1;			// { dg-warning "attribute ignored" }
B [[likely]] b2;			// { dg-warning "attribute ignored" }
struct [[likely]] C {};			// { dg-warning "'likely' attribute ignored" }
int f [[likely]];			// { dg-warning "'likely' attribute ignored" }
int g[2] [[likely]];			// { dg-warning "'likely' attribute ignored" }
int g2 [[likely]] [2];			// { dg-warning "'likely' attribute ignored" }
int corge () [[likely]];		// { dg-warning "'likely' attribute ignored" }
int *[[likely]] h;			// { dg-warning "'likely' attribute ignored" }
int & [[likely]] i = f;			// { dg-warning "'likely' attribute ignored" }
int && [[likely]] j = 0;		// { dg-warning "'likely' attribute ignored" }
int S::* [[likely]] k;			// { dg-warning "'likely' attribute ignored" }
auto l = sizeof (int [2] [[likely]]);	// { dg-warning "'likely' attribute ignored" }
int freddy ([[likely]] int a,		// { dg-warning "'likely' attribute ignored" }
	    [[likely]] int,		// { dg-warning "'likely' attribute ignored" }
	    [[likely]] int c = 0,	// { dg-warning "'likely' attribute ignored" }
	    [[likely]] int = 0);	// { dg-warning "'likely' attribute ignored" }
void
corge ([[likely]] int a,		// { dg-warning "'likely' attribute ignored" }
       [[likely]] int,			// { dg-warning "'likely' attribute ignored" }
       [[likely]] int c = 0,		// { dg-warning "'likely' attribute ignored" }
       [[likely]] int = 0)		// { dg-warning "'likely' attribute ignored" }
{
}
[[likely]] void
garply ()				// { dg-warning "ISO C\\\+\\\+ 'likely' attribute does not apply to functions; treating as '\\\[\\\[gnu::hot\\\]\\\]'" }
{
}
int grault (int [[likely]] a,		// { dg-warning "attribute ignored" }
	    int [[likely]],		// { dg-warning "attribute ignored" }
	    int [[likely]] c = 0,	// { dg-warning "attribute ignored" }
	    int [[likely]] = 0);	// { dg-warning "attribute ignored" }
void
waldo (int [[likely]] a,		// { dg-warning "attribute ignored" }
       int [[likely]],			// { dg-warning "attribute ignored" }
       int [[likely]] c = 0,		// { dg-warning "attribute ignored" }
       int [[likely]] = 0)		// { dg-warning "attribute ignored" }
{
}
int plugh (int a [[likely]],		// { dg-warning "'likely' attribute ignored" }
	    int b [[likely]] = 0);	// { dg-warning "'likely' attribute ignored" }
void
thud (int a [[likely]],			// { dg-warning "'likely' attribute ignored" }
      int b [[likely]] = 0)		// { dg-warning "'likely' attribute ignored" }
{
}
enum [[likely]] D { D0 };		// { dg-warning "'likely' attribute ignored" }
enum class [[likely]] E { E0 };		// { dg-warning "'likely' attribute ignored" }
enum F {};
enum [[likely]] F;			// { dg-warning "type attributes ignored after type is already defined" }
enum G {
  G0 [[likely]],			// { dg-warning "'likely' attribute ignored" }
  G1 [[likely]] = 2			// { dg-warning "'likely' attribute ignored" }
};
namespace [[likely]] H { using H0 = int; }	// { dg-warning "'likely' attribute directive ignored" } */
namespace [[likely]] {}			// { dg-warning "'likely' attribute directive ignored" }
[[likely]] using namespace H;		// { dg-warning "'likely' attribute directive ignored" }
struct [[likely]] I			// { dg-warning "'likely' attribute ignored" }
{
  [[likely]];				// { dg-error "declaration does not declare anything" }
  [[likely]] int i;			// { dg-warning "'likely' attribute ignored" }
  [[likely]] int foo ();		// { dg-warning "ISO C\\\+\\\+ 'likely' attribute does not apply to functions; treating as '\\\[\\\[gnu::hot\\\]\\\]'" }
  [[likely]] int bar () { return 1; }	// { dg-warning "ISO C\\\+\\\+ 'likely' attribute does not apply to functions; treating as '\\\[\\\[gnu::hot\\\]\\\]'" }
  [[likely]] int : 0;			// { dg-warning "'likely' attribute ignored" }
  [[likely]] int i2 : 5;		// { dg-warning "'likely' attribute ignored" }
  [[likely]] static int i3;		// { dg-warning "'likely' attribute ignored" }
  static int i4;
};
[[likely]] int I::i4 = 0;		// { dg-warning "'likely' attribute ignored" }
struct J : [[likely]] C {};		// { dg-warning "attributes on base specifiers are ignored" }
#if __cpp_concepts >= 201907L
template <typename T>
concept K [[likely]] = requires { true; };	// { dg-warning "'likely' attribute ignored" "" { target c++20 } }
#endif
typedef int L [[likely]];		// { dg-warning "'likely' attribute ignored" }
template <typename T>
struct M {};
template <>
struct [[likely]] M<int> { int m; };	// { dg-warning "'likely' attribute ignored" }
typedef int N[2] [[likely]];		// { dg-warning "'likely' attribute ignored" }
typedef int O [[likely]] [2];		// { dg-warning "'likely' attribute ignored" }
