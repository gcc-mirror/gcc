// C++ 26 P2795R5 - Erroneous behaviour for uninitialized reads
// { dg-do compile { target c++11 } }

int arr[2];
struct S { int a, b; };
S arr2[2];

void
foo ([[indeterminate]] int n, int n2 [[indeterminate]], int n3 [[indeterminate]] [2])
{
  [[indeterminate]] int x1, x11, x12, x13;
  int x14, x15 [[indeterminate]];
  [[indeterminate ("foobar")]] int x2;		// { dg-error "'indeterminate' attribute does not take any arguments" }
						// { dg-error "expected primary-expression before 'int'" "" { target *-*-* } .-1 }
  [[indeterminate (0)]] int x3;			// { dg-error "'indeterminate' attribute does not take any arguments" }
						// { dg-error "expected primary-expression before 'int'" "" { target *-*-* } .-1 }
  [[indeterminate ("foo", "bar", "baz")]] int x4;// { dg-error "'indeterminate' attribute does not take any arguments" }
						// { dg-error "expected primary-expression before 'int'" "" { target *-*-* } .-1 }
  [[indeterminate (0, 1, 2)]] int x5;		// { dg-error "'indeterminate' attribute does not take any arguments" }
						// { dg-error "expected primary-expression before 'int'" "" { target *-*-* } .-1 }

  auto a = [] [[indeterminate]] () {};		// { dg-error "'indeterminate' on declaration other than parameter or automatic variable" }
  auto b = [] constexpr [[indeterminate]] {};	// { dg-warning "'indeterminate' attribute does not apply to types" }
						// { dg-error "parameter declaration before lambda declaration specifiers only optional with" "" { target c++20_down } .-1 }
						// { dg-error "'constexpr' lambda only available with" "" { target c++14_down } .-2 }
  auto c = [] noexcept [[indeterminate]] {};	// { dg-warning "'indeterminate' attribute does not apply to types" }
						// { dg-error "parameter declaration before lambda exception specification only optional with" "" { target c++20_down } .-1 }
  auto d = [] () [[indeterminate]] {};		// { dg-warning "'indeterminate' attribute does not apply to types" }
  auto e = new int [n] [[indeterminate]];	// { dg-warning "attributes ignored on outermost array type in new expression" }
  auto e2 = new int [n] [[indeterminate]] [42];	// { dg-warning "attributes ignored on outermost array type in new expression" }
  auto f = new int [n][42] [[indeterminate]];	// { dg-warning "'indeterminate' attribute does not apply to types" }
  [[indeterminate]];				// { dg-warning "attributes at the beginning of statement are ignored" }
  [[indeterminate]] {}				// { dg-warning "attributes at the beginning of statement are ignored" }
  [[indeterminate]] if (true) {}		// { dg-warning "attributes at the beginning of statement are ignored" }
  [[indeterminate]] while (false) {}		// { dg-warning "attributes at the beginning of statement are ignored" }
  [[indeterminate]] goto lab;			// { dg-warning "attributes at the beginning of statement are ignored" }
  [[indeterminate]] lab:;			// { dg-error "'indeterminate' on declaration other than parameter or automatic variable" }
  [[indeterminate]] try {} catch (int) {}	// { dg-warning "attributes at the beginning of statement are ignored" }
  if ([[indeterminate]] int x = 0) {}
  switch (n)
    {
    [[indeterminate]] case 1:			// { dg-error "'indeterminate' on declaration other than parameter or automatic variable" }
    [[indeterminate]] break;			// { dg-warning "attributes at the beginning of statement are ignored" }
    [[indeterminate]] default:			// { dg-error "'indeterminate' on declaration other than parameter or automatic variable" }
	 break;
    }
  for ([[indeterminate]] auto a : arr) {}
  for ([[indeterminate]] auto [a, b] : arr2) {}	// { dg-error "structured bindings only available with" "" { target c++14_down } }
  [[indeterminate]] asm ("");			// { dg-warning "attributes ignored on 'asm' declaration" }
  try {} catch ([[indeterminate]] int x) {}
  try {} catch ([[indeterminate]] int) {}
  try {} catch (int [[indeterminate]] x) {}	// { dg-warning "attribute ignored" }
  try {} catch (int [[indeterminate]]) {}	// { dg-warning "attribute ignored" }
  try {} catch (int x [[indeterminate]]) {}
}

[[indeterminate]] int bar ();			// { dg-error "'indeterminate' on declaration other than parameter or automatic variable" }
using foobar [[indeterminate]] = int;		// { dg-error "'indeterminate' on declaration other than parameter or automatic variable" }
[[indeterminate]] int a;			// { dg-error "'indeterminate' on declaration other than parameter or automatic variable" }
[[indeterminate]] auto [b, c] = arr;		// { dg-error "'indeterminate' on declaration other than parameter or automatic variable" }
						// { dg-error "structured bindings only available with" "" { target c++14_down } .-1 }
[[indeterminate]];				// { dg-warning "attribute ignored" }
inline [[indeterminate]] void baz () {}		// { dg-warning "attribute ignored" }
						// { dg-error "standard attributes in middle of decl-specifiers" "" { target *-*-* } .-1 }
constexpr [[indeterminate]] int qux () { return 0; }	// { dg-warning "attribute ignored" }
						// { dg-error "standard attributes in middle of decl-specifiers" "" { target *-*-* } .-1 }
int [[indeterminate]] d;			// { dg-warning "attribute ignored" }
int const [[indeterminate]] e = 1;		// { dg-warning "attribute ignored" }
struct A {} [[indeterminate]];			// { dg-warning "attribute ignored in declaration of 'struct A'" }
struct A [[indeterminate]];			// { dg-warning "attribute ignored" }
struct A [[indeterminate]] a1;			// { dg-warning "attribute ignored" }
A [[indeterminate]] a2;				// { dg-warning "attribute ignored" }
enum B { B0 } [[indeterminate]];		// { dg-warning "attribute ignored in declaration of 'enum B'" }
enum B [[indeterminate]];			// { dg-warning "attribute ignored" }
enum B [[indeterminate]] b1;			// { dg-warning "attribute ignored" }
B [[indeterminate]] b2;				// { dg-warning "attribute ignored" }
struct [[indeterminate]] C {};			// { dg-warning "'indeterminate' attribute does not apply to types" }
int f [[indeterminate]];			// { dg-error "'indeterminate' on declaration other than parameter or automatic variable" }
int g[2] [[indeterminate]];			// { dg-warning "'indeterminate' attribute does not apply to types" }
int g2 [[indeterminate]] [2];			// { dg-error "'indeterminate' on declaration other than parameter or automatic variable" }
int corge () [[indeterminate]];			// { dg-warning "'indeterminate' attribute does not apply to types" }
int *[[indeterminate]] h;			// { dg-warning "'indeterminate' attribute does not apply to types" }
int & [[indeterminate]] i = f;			// { dg-warning "'indeterminate' attribute does not apply to types" }
int && [[indeterminate]] j = 0;			// { dg-warning "'indeterminate' attribute does not apply to types" }
int S::* [[indeterminate]] k;			// { dg-warning "'indeterminate' attribute does not apply to types" }
auto l = sizeof (int [2] [[indeterminate]]);	// { dg-warning "'indeterminate' attribute does not apply to types" }
int freddy ([[indeterminate]] int a,
	    [[indeterminate]] int,
	    [[indeterminate]] int c = 0,
	    [[indeterminate]] int = 0);
void
corge ([[indeterminate]] int a,
       [[indeterminate]] int,
       [[indeterminate]] int c = 0,
       [[indeterminate]] int = 0)
{
}
[[indeterminate]] void
garply ()					// { dg-error "'indeterminate' on declaration other than parameter or automatic variable" }
{
}
int grault (int [[indeterminate]] a,		// { dg-warning "attribute ignored" }
	    int [[indeterminate]],		// { dg-warning "attribute ignored" }
	    int [[indeterminate]] c = 0,	// { dg-warning "attribute ignored" }
	    int [[indeterminate]] = 0);		// { dg-warning "attribute ignored" }
void
waldo (int [[indeterminate]] a,			// { dg-warning "attribute ignored" }
       int [[indeterminate]],			// { dg-warning "attribute ignored" }
       int [[indeterminate]] c = 0,		// { dg-warning "attribute ignored" }
       int [[indeterminate]] = 0)		// { dg-warning "attribute ignored" }
{
}
int plugh (int a [[indeterminate]],
	    int b [[indeterminate]] = 0);
void
thud (int a [[indeterminate]],
      int b [[indeterminate]] = 0)
{
}
enum [[indeterminate]] D { D0 };		// { dg-warning "'indeterminate' attribute does not apply to types" }
enum class [[indeterminate]] E { E0 };		// { dg-warning "'indeterminate' attribute does not apply to types" }
enum F {};
enum [[indeterminate]] F;			// { dg-warning "'indeterminate' attribute does not apply to types" }
enum G {
  G0 [[indeterminate]],				// { dg-error "'indeterminate' on declaration other than parameter or automatic variable" }
  G1 [[indeterminate]] = 2			// { dg-error "'indeterminate' on declaration other than parameter or automatic variable" }
};
namespace [[indeterminate]] H { using H0 = int; }// { dg-warning "'indeterminate' attribute directive ignored" }
namespace [[indeterminate]] {}			// { dg-warning "'indeterminate' attribute directive ignored" }
[[indeterminate]] using namespace H;		// { dg-warning "'indeterminate' attribute directive ignored" }
struct [[indeterminate]] I			// { dg-warning "'indeterminate' attribute does not apply to types" }
{
  [[indeterminate]];				// { dg-error "declaration does not declare anything" }
  [[indeterminate]] int i;			// { dg-error "'indeterminate' on declaration other than parameter or automatic variable" }
  [[indeterminate]] int foo ();			// { dg-error "'indeterminate' on declaration other than parameter or automatic variable" }
  [[indeterminate]] int bar () { return 1; }	// { dg-error "'indeterminate' on declaration other than parameter or automatic variable" }
  [[indeterminate]] int : 0;			// { dg-error "'indeterminate' on declaration other than parameter or automatic variable" }
  [[indeterminate]] int i2 : 5;			// { dg-error "'indeterminate' on declaration other than parameter or automatic variable" }
  [[indeterminate]] static int i3;		// { dg-error "'indeterminate' on declaration other than parameter or automatic variable" }
  static int i4;
};
[[indeterminate]] int I::i4 = 0;		// { dg-error "'indeterminate' on declaration other than parameter or automatic variable" }
struct J : [[indeterminate]] C {};		// { dg-warning "attributes on base specifiers are ignored" }
#if __cpp_concepts >= 201907L
template <typename T>
concept K [[indeterminate]] = requires { true; };// { dg-error "'indeterminate' on declaration other than parameter or automatic variable" "" { target c++20 } }
#endif
typedef int L [[indeterminate]];		// { dg-error "'indeterminate' on declaration other than parameter or automatic variable" }
template <typename T>
struct M {};
template <>
struct [[indeterminate]] M<int> { int m; };	// { dg-warning "'indeterminate' attribute does not apply to types" }
typedef int N[2] [[indeterminate]];		// { dg-warning "'indeterminate' attribute does not apply to types" }
typedef int O [[indeterminate]] [2];		// { dg-error "'indeterminate' on declaration other than parameter or automatic variable" }
