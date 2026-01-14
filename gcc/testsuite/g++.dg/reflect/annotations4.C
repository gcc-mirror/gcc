// C++ 26 P3394R4 - Annotations for Reflection
// { dg-do compile { target c++26 } }

struct E { constexpr E () : e (42) {} private: int e; };
int arr[2];
struct S { int a, b; };
S arr2[2];

void
foo (int n)
{
  [[=E ()]] int x1;				// { dg-error "annotation does not have structural type" }
  auto a = [] [[=E ()]] () {};			// { dg-error "annotation does not have structural type" }
  if ([[=E ()]] int x = 0) {}			// { dg-error "annotation does not have structural type" }
  for ([[=E ()]] auto a : arr) {}		// { dg-error "annotation does not have structural type" }
  for ([[=E ()]] auto [a, b] : arr2) {}		// { dg-error "annotation does not have structural type" }
  try {} catch ([[=E ()]] int x) {}		// { dg-error "annotation does not have structural type" }
  try {} catch ([[=E ()]] int) {}		// { dg-error "annotation does not have structural type" }
  try {} catch (int x [[=E ()]]) {}		// { dg-error "annotation does not have structural type" }
}

[[=E ()]] int bar ();				// { dg-error "annotation does not have structural type" }
using foobar [[=E ()]] = int;			// { dg-error "annotation does not have structural type" }
[[=E ()]] int a;				// { dg-error "annotation does not have structural type" }
[[=E ()]] auto [b, c] = arr;			// { dg-error "annotation does not have structural type" }
struct [[=E ()]] C {};				// { dg-error "annotation does not have structural type" }
int f [[=E ()]];				// { dg-error "annotation does not have structural type" }
int g2 [[=E ()]] [2];				// { dg-error "annotation does not have structural type" }
int freddy ([[=E ()]] int a,			// { dg-error "annotation does not have structural type" }
	    [[=E ()]] int,			// { dg-error "annotation does not have structural type" }
	    [[=E ()]] int c = 0,		// { dg-error "annotation does not have structural type" }
	    [[=E ()]] int = 0);			// { dg-error "annotation does not have structural type" }
void
corge ([[=E ()]] int a,				// { dg-error "annotation does not have structural type" }
       [[=E ()]] int,				// { dg-error "annotation does not have structural type" }
       [[=E ()]] int c = 0,			// { dg-error "annotation does not have structural type" }
       [[=E ()]] int = 0)			// { dg-error "annotation does not have structural type" }
{
}
[[=E ()]] void
garply ()					// { dg-error "annotation does not have structural type" }
{
}
int plugh (int a [[=E ()]],			// { dg-error "annotation does not have structural type" }
	   int b [[=E ()]] = 0);		// { dg-error "annotation does not have structural type" }
void
thud (int a [[=E ()]],				// { dg-error "annotation does not have structural type" }
      int b [[=E ()]] = 0)			// { dg-error "annotation does not have structural type" }
{
}
enum [[=E ()]] D { D0 };			// { dg-error "annotation does not have structural type" }
enum class [[=E ()]] Z { Z0 };			// { dg-error "annotation does not have structural type" }
enum F {};
enum G {
  G0 [[=E ()]],					// { dg-error "annotation does not have structural type" }
  G1 [[=E ()]] = 2				// { dg-error "annotation does not have structural type" }
};
namespace [[=E ()]] H { using H0 = int; }	// { dg-error "annotation does not have structural type" }
namespace [[=E ()]] {}				// { dg-error "annotation does not have structural type" }
struct [[=E ()]] I				// { dg-error "annotation does not have structural type" }
{
  [[=E ()]] int i;				// { dg-error "annotation does not have structural type" }
  [[=E ()]] int foo ();				// { dg-error "annotation does not have structural type" }
  [[=E ()]] int bar () { return 1; }		// { dg-error "annotation does not have structural type" }
  [[=E ()]] int i2 : 5;				// { dg-error "annotation does not have structural type" }
  [[=E ()]] static int i3;			// { dg-error "annotation does not have structural type" }
  static int i4;
};
[[=E ()]] int I::i4 = 0;			// { dg-error "annotation does not have structural type" }
struct J : [[=E ()]] C {};			// { dg-error "annotation does not have structural type" }
template <typename T>
concept K [[=E ()]] = requires { true; };	// { dg-error "annotation does not have structural type" "" { xfail *-*-* } }
constexpr bool k = K <int>;
typedef int L [[=E ()]];			// { dg-error "annotation does not have structural type" }
template <typename T>
struct M {};
template <>
struct [[=E ()]] M<int> { int m; };		// { dg-error "annotation does not have structural type" }
typedef int O [[=E ()]] [2];			// { dg-error "annotation does not have structural type" }
