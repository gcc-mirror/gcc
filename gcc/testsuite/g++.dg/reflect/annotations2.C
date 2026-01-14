// C++ 26 P3394R4 - Annotations for Reflection
// { dg-do compile { target c++26 } }

struct [[=0]] A;
struct [[=1, =2]] A {
  [[=1]] [[=1]] [[=2]] int a;
  [[=3, =4, =5]] static int b; 
};
namespace [[=6, =6]] [[=6]] N
{
};
namespace [[=7]] [[= 7 + 1]] [[= 7 + 2]] N
{
};
[[=9.5L]] int c;
struct B { constexpr B () : b (42) {} int b; };
[[=B ()]] int d;
struct C { constexpr C () : c (42) {} int c; ~C () {} };
[[=C ()]] int e;						// { dg-error "temporary of non-literal type 'C' in a constant expression" }
								// { dg-error "annotation does not have structural type" "" { target *-*-* } .-1 }
struct D { constexpr D () : d (42) {} volatile int d; };
[[=D ()]] int f;						// { dg-error "temporary of non-literal type 'D' in a constant expression" }
								// { dg-error "annotation does not have structural type" "" { target *-*-* } .-1 }
struct E { constexpr E () : e (42) {} private: int e; };
[[=E ()]] int g;						// { dg-error "annotation does not have structural type" }
struct F { constexpr F () : f (42) {} protected: int f; };
[[=F ()]] int h;						// { dg-error "annotation does not have structural type" }
struct G { constexpr G () : g (42) {} mutable int g; };
[[=G ()]] int i;						// { dg-error "annotation does not have structural type" }
struct H { constexpr H () : h (42) {} int h; G g; };
[[=H ()]] int j;						// { dg-error "annotation does not have structural type" }
[[=c]] int k;							// { dg-error "the value of 'c' is not usable in a constant expression" }
[[=(throw 1, 0)]] int l;					// { dg-error "uncaught exception '1'" }
struct I { int a, b; long c; };
constexpr B m = B ();
constexpr I n = { 1, 2, 3 };
template <auto ...A>
[[=1, =A...]] int o;
int p = o<1, m, n> + o<2, 42>;
[[maybe_unused]] [[=3]] [[gnu::unused]] int q;
[[=3, gnu::unused, gnu::aligned(16), =4]] int r;		// { dg-error "mixing annotations and attributes in the same list" }
[[gnu::unused, =5, gnu::aligned(16), =6]] int s;		// { dg-error "mixing annotations and attributes in the same list" }
[[using gnu:unused, =7, aligned(16), =8]] int t;		// { dg-error "mixing annotations and attributes in the same list" }
[[using gnu:=9]] int u;						// { dg-error "mixing annotations and attributes in the same list" }
