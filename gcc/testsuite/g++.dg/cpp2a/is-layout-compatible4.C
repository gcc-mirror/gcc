// Test for diagnostics on failed is_layout_compatible.
// { dg-do compile { target c++20 } }

template <typename T, typename U>
constexpr bool is_layout_compatible_v = __is_layout_compatible (T, U);

static_assert(is_layout_compatible_v<int, unsigned>);  // { dg-error "assert" }
// { dg-message "is not layout compatible" "" { target *-*-* } .-1 }
// { dg-message "same type" "" { target *-*-* } .-2 }

struct S {};
static_assert(is_layout_compatible_v<const S, volatile int>);  // { dg-error "assert" }
// { dg-message "is not layout compatible" "" { target *-*-* } .-1 }
// { dg-message "same type" "" { target *-*-* } .-2 }

struct A {
  int a;
  char b;  // { dg-message "'A::b' and 'B::b' do not correspond" }
};
struct B {
  int a;
  signed char b;  // { dg-message "declared here" }
};
static_assert(is_layout_compatible_v<A, B>);  // { dg-error "assert" }

struct C {
  int : 1;
  int c : 7;
  int : 0;  // { dg-message "'C::<anonymous>' and 'D::g' do not correspond" }
  int : 2;
};
struct D {
  int f : 1;
  int : 7;
  int g : 2;  // { dg-message "declared here" }
};
static_assert(is_layout_compatible_v<C, D>);  // { dg-error "assert" }

struct E {  // { dg-message "'E' is not a standard-layout type" }
  int a;
private:
  int b;
};
struct F {
  int a;
private:
  int b;
};
static_assert(is_layout_compatible_v<E, F>);  // { dg-error "assert" }

union G {
  int a;
  long long b;
  signed char c;  // { dg-message "'H' has no member corresponding to 'G::c'" }
};
union H {  // { dg-message "declared here" }
  char x;
  int y;
  long long z;
};
static_assert(is_layout_compatible_v<G, H>);  // { dg-error "assert" }

union I {  // { dg-message "'I' has 2 fields, but" }
  int a;
  double b;
};
union J {  // { dg-message "'J' has 1 field" }
  int c;
};
static_assert(is_layout_compatible_v<I, J>);  // { dg-error "assert" }

enum K : int {  // { dg-message "the underlying type of 'K' is 'int'" }
  K0, K1
};
enum L : long int {  // { dg-message "the underlying type of 'L' is 'long int'" }
  L0, L1
};
static_assert(is_layout_compatible_v<K, L>);  // { dg-error "assert" }
