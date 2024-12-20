// C++ 26 P2552R3 - On the ignorability of standard attributes
// { dg-do compile { target c++11 } }

int arr[2];
struct S { int a, b; };
S arr2[2];

void
foo (int n)
{
  alignas (int) int x1;
  alignas ("foobar") int x2;			// { dg-error "'alignas' argument has non-integral type 'const char \\\[7\\\]'" }
  alignas (0) int x3;				// { dg-warning "requested alignment '0' is not a positive power of 2" }
  alignas ("foo", "bar", "baz") int x4;		// { dg-error "'alignas' argument has non-integral type 'const char \\\[4\\\]'" }
						// { dg-error "expected '\\\)' before ',' token" "" { target *-*-* } .-1 }
						// { dg-error "expected declaration before ',' token" "" { target *-*-* } .-2 }
						// { dg-error "expected primary-expression before ',' token" "" { target *-*-* } .-3 }
  alignas (0, 1, 2) int x5;			// { dg-error "expected '\\\)' before ',' token" }
						// { dg-error "expected declaration before ',' token" "" { target *-*-* } .-1 }
						// { dg-error "expected primary-expression before ',' token" "" { target *-*-* } .-2 }

  auto a = [] alignas (int) () {};		// { dg-error "'alignas' on function declaration" }
  auto b = [] constexpr alignas (int) {};	// { dg-error "'alignas' on a type other than class" }
						// { dg-error "parameter declaration before lambda declaration specifiers only optional with" "" { target c++20_down } .-1 }
						// { dg-error "'constexpr' lambda only available with" "" { target c++14_down } .-2 }
  auto c = [] noexcept alignas (int) {};	// { dg-error "'alignas' on a type other than class" }
						// { dg-error "parameter declaration before lambda exception specification only optional with" "" { target c++20_down } .-1 }
  auto d = [] () alignas (int) {};		// { dg-error "'alignas' on a type other than class" }
  auto e = new int [n] alignas (int);		// { dg-warning "attributes ignored on outermost array type in new expression" }
  auto e2 = new int [n] alignas (int) [42];	// { dg-warning "attributes ignored on outermost array type in new expression" }
  auto f = new int [n][42] alignas (int);	// { dg-error "'alignas' on a type other than class" }
  alignas (int);				// { dg-warning "attributes at the beginning of statement are ignored" }
  alignas (int) {}				// { dg-warning "attributes at the beginning of statement are ignored" }
  alignas (int) if (true) {}			// { dg-warning "attributes at the beginning of statement are ignored" }
  alignas (int) while (false) {}		// { dg-warning "attributes at the beginning of statement are ignored" }
  alignas (int) goto lab;			// { dg-warning "attributes at the beginning of statement are ignored" }
  alignas (int) lab:;				// { dg-error "alignment may not be specified for 'lab'" }
  alignas (int) try {} catch (int) {}		// { dg-warning "attributes at the beginning of statement are ignored" }
  if (alignas (int) int x = 0) {}
  switch (n)
    {
    alignas (int) case 1:			// { dg-error "alignment may not be specified for" }
    alignas (int) break;			// { dg-warning "attributes at the beginning of statement are ignored" }
    alignas (int) default:			// { dg-error "alignment may not be specified for" }
	 break;
    }
  for (alignas (int) auto a : arr) {}
  for (alignas (int) auto [a, b] : arr2) {}	// { dg-error "structured bindings only available with" "" { target c++14_down } }
  alignas (int) asm ("");			// { dg-warning "attributes ignored on 'asm' declaration" }
  try {} catch (alignas (int) int x) {}		// { dg-error "'alignas' on exception declaration" }
  try {} catch (alignas (int) int) {}		// { dg-error "'alignas' on exception declaration" }
  try {} catch (int alignas (int) x) {}		// { dg-warning "attribute ignored" }
  try {} catch (int alignas (int)) {}		// { dg-warning "attribute ignored" }
  try {} catch (int x alignas (int)) {}		// { dg-error "'alignas' on exception declaration" }
}

alignas (int) int bar ();			// { dg-error "'alignas' on function declaration" }
using foobar alignas (int) = int;		// { dg-error "'alignas' on a type alias" }
alignas (int) int a;
alignas (int) auto [b, c] = arr;		// { dg-error "structured bindings only available with" "" { target c++14_down } }
alignas (int);					// { dg-warning "attribute ignored" }
inline alignas (int) void baz () {}		// { dg-warning "attribute ignored" }
						// { dg-error "standard attributes in middle of decl-specifiers" "" { target *-*-* } .-1 }
constexpr alignas (int) int qux () { return 0; }	// { dg-warning "attribute ignored" }
						// { dg-error "standard attributes in middle of decl-specifiers" "" { target *-*-* } .-1 }
int alignas (int) d;				// { dg-warning "attribute ignored" }
int const alignas (int) e = 1;			// { dg-warning "attribute ignored" }
struct A {} alignas (int);			// { dg-warning "attribute ignored in declaration of 'struct A'" }
struct A alignas (int);				// { dg-warning "attribute ignored" }
struct A alignas (int) a1;			// { dg-warning "attribute ignored" }
A alignas (int) a2;				// { dg-warning "attribute ignored" }
enum B { B0 } alignas (int);			// { dg-warning "attribute ignored in declaration of 'enum B'" }
enum B alignas (int);				// { dg-warning "attribute ignored" }
enum B alignas (int) b1;			// { dg-warning "attribute ignored" }
B alignas (int) b2;				// { dg-warning "attribute ignored" }
struct alignas (int) C {};
int f alignas (int);
struct alignas (int) Y;
int g[2] alignas (int);				// { dg-error "'alignas' on a type other than class" }
int g2 alignas (int) [2];
int corge () alignas (int);			// { dg-error "'alignas' on a type other than class" }
int *alignas (int) h;				// { dg-error "'alignas' on a type other than class" }
int & alignas (int) i = f;			// { dg-error "'alignas' on a type other than class" }
int && alignas (int) j = 0;			// { dg-error "'alignas' on a type other than class" }
int S::* alignas (int) k;			// { dg-error "'alignas' on a type other than class" }
auto l = sizeof (int [2] alignas (int));	// { dg-error "'alignas' on a type other than class" }
int freddy (alignas (int) int a,		// { dg-error "alignment may not be specified for 'a'" }
	    alignas (int) int,			// { dg-error "alignment may not be specified for '<anonymous>'" }
	    alignas (int) int c = 0,		// { dg-error "alignment may not be specified for 'c'" }
	    alignas (int) int = 0);		// { dg-error "alignment may not be specified for '<anonymous>'" }
void
corge (alignas (int) int a,			// { dg-error "alignment may not be specified for 'a'" }
       alignas (int) int,			// { dg-error "alignment may not be specified for '<anonymous>'" }
       alignas (int) int c = 0,			// { dg-error "alignment may not be specified for 'c'" }
       alignas (int) int = 0)			// { dg-error "alignment may not be specified for '<anonymous>'" }
{
}
alignas (int) void
garply ()					// { dg-error "'alignas' on function declaration" }
{
}
int grault (int alignas (int) a,		// { dg-warning "attribute ignored" }
	    int alignas (int),			// { dg-warning "attribute ignored" }
	    int alignas (int) c = 0,		// { dg-warning "attribute ignored" }
	    int alignas (int) = 0);		// { dg-warning "attribute ignored" }
void
waldo (int alignas (int) a,			// { dg-warning "attribute ignored" }
       int alignas (int),			// { dg-warning "attribute ignored" }
       int alignas (int) c = 0,			// { dg-warning "attribute ignored" }
       int alignas (int) = 0)			// { dg-warning "attribute ignored" }
{
}
int plugh (int a alignas (int),			// { dg-error "alignment may not be specified for 'a'" }
	    int b alignas (int) = 0);		// { dg-error "alignment may not be specified for 'b'" }
void
thud (int a alignas (int),			// { dg-error "alignment may not be specified for 'a'" }
      int b alignas (int) = 0)			// { dg-error "alignment may not be specified for 'b'" }
{
}
enum alignas (int) D { D0 };			// { dg-error "'alignas' on enumerated type" }
enum class alignas (int) E { E0 };		// { dg-error "'alignas' on enumerated type" }
enum F {};
enum alignas (int) F;				// { dg-warning "type attributes ignored after type is already defined" }
enum G {
  G0 alignas (int),				// { dg-error "alignment may not be specified for 'G0'" }
  G1 alignas (int) = 2				// { dg-error "alignment may not be specified for 'G1'" }
};
namespace alignas (int) H { using H0 = int; }	// { dg-error "expected identifier before 'alignas'" }
						// { dg-error "H' does not name a type" "" { target *-*-* } .-1 }
namespace alignas (int) {}			// { dg-error "expected identifier before 'alignas'" }
						// { dg-error "expected unqualified-id before '\\\{' token" "" { target *-*-* } .-1 }
alignas (int) using namespace H;
						// { dg-error "'H' is not a namespace-name" "" { target *-*-* } .-1 }
struct alignas (int) I
{
  alignas (int);				// { dg-error "declaration does not declare anything" }
  alignas (int) int i;
  alignas (int) int foo ();			// { dg-error "'alignas' on function declaration" }
  alignas (int) int bar () { return 1; }	// { dg-error "'alignas' on function declaration" }
  alignas (int) int : 0;			// { dg-error "'alignas' on bit-field" }
  alignas (int) int i2 : 5;			// { dg-error "'alignas' on bit-field" }
  alignas (int) static int i3;
  static int i4;
};
alignas (int) int I::i4 = 0;
struct J : alignas (int) C {};			// { dg-warning "attributes on base specifiers are ignored" }
#if __cpp_concepts >= 201907L
template <typename T>
concept K alignas (int) = requires { true; };	// { dg-error "alignment may not be specified for 'K'" "" { target c++20 } }
#endif
typedef int L alignas (int);			// { dg-error "'alignas' on a type alias" }
template <typename T>
struct M {};
template <>
struct alignas (int) M<int> { int m; };
typedef int N[2] alignas (int);			// { dg-error "'alignas' on a type other than class" }
typedef int O alignas (int) [2];		// { dg-error "'alignas' on a type alias" }
